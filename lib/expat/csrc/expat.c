#include <unistd.h>
#include <sys/types.h>
#include <stdio.h>
#include <string.h>
#include <arpa/inet.h>
#include <expat.h>
#include <erl_interface.h>
#include <ei.h>

#define ERL_IN  3
#define ERL_OUT 4

#define CMD_PARSE	0

#define RESP_ERROR	-1
#define RESP_START	0 /* start of tag */
#define RESP_END	1 /* end of tag */
#define RESP_CHAR_DATA	2


#define RESP_HEAD_LEN 5

#define EXPAT_BUF_SZ 1<<18 /* 256k */
#define REQ_HEAD_LEN 5

#define error(str) {							\
	fprintf(stderr, "Error on line %d: %s\r\n", __LINE__, str);	\
	exit(1);							\
}

char resp_error[RESP_HEAD_LEN];
char resp_start[RESP_HEAD_LEN];
char resp_end[RESP_HEAD_LEN];
char resp_char_data[RESP_HEAD_LEN];


/*
 * Protocol from erlang:
 *   {CMD_PARSE, <<"Str...'>>}  - parser string of length NNNN
 *
 * Protocol to erlang:
 *   {RESP_ERROR, <<Str>>}
 *   {RESP_SOT, <<NS>>, <<Tag>>, [<<Attrs>>]}
 *   {RESP_EOT, NS, Tag}
 *   {RESP_CHAR_DATA, Char_Data}
 */

typedef struct {
	XML_Parser p;
} state;

void write_head(const char *head, uint32_t bytes) {
	bytes = htonl(bytes);
	if (write(ERL_OUT, &bytes, 4) == -1) error("writing");
	if (write(ERL_OUT, head, RESP_HEAD_LEN) == -1) error("writing");
}

void write_bin(const char *s, uint32_t len) {
	uint32_t nlen = htonl(len);

	/* BINARY_EXT */
	if (write(ERL_OUT, "m", 1) == -1) error("writing");
	if (write(ERL_OUT, &nlen, 4) == -1) error("writing");
	if (write(ERL_OUT, s, len) == -1) error("writing");
}

/* return the number of bytes needed to encode the attributes */
int attr_bytes(const char **attr) {
	int len = 0;
	int i;

	for (i = 0; attr[i]; i += 2) {
		len += 2 + 5 + 5 + strlen(attr[i]) + strlen(attr[i + 1]);
	}

	if (i == 0)
		/* size of NIL_EXT */
		return 1;
	else
		/* size of LIST_EXT and trailing NIL_EXT */
		return len + 6;
}

/* return the number of attributes */
int num_attrs(const char **attrs) {
	int i;

	for (i = 0; attrs[i]; i += 2) ;

	return i / 2;
}

void write_attrs(const char **attrs) {
	int i;
	uint32_t nattrs;
	char tuple_head[] = {'h', 2}; /* SMALL_TUPLE_EXT */

	nattrs = num_attrs(attrs);
	if (nattrs == 0) {
		/* NIL_EXT */
		if (write(ERL_OUT, "j", 1) == -1) error("writing");
		return;
	}

	nattrs = htonl(nattrs);

	/* LIST_EXT */
	if (write(ERL_OUT, "l", 1) == -1) error("writing");
	if (write(ERL_OUT, &nattrs, 4) == -1) error("writing");

	for (i = 0; attrs[i]; i += 2) {
		/* SMALL_TUPLE_EXT */
		if (write(ERL_OUT, &tuple_head, 2) == -1) error("writing");
		write_bin(attrs[i], strlen(attrs[i]));
		write_bin(attrs[i+1], strlen(attrs[i+1]));
	}

	/* NIL_EXT */
	if (write(ERL_OUT, "j", 1) == -1) error("writing");
}

void XMLCALL start(void *data, const char *el, const char **attrs) {
	char *space;

	if ((space = strchr(el, ' ')) == NULL) { /* no namespace */
		write_head(resp_start,
			   RESP_HEAD_LEN + 5 + 5 +
			   strlen(el) + attr_bytes(attrs));
		write_bin("", 0);
		write_bin(el, strlen(el));
	} else {
		write_head(resp_start,
			   RESP_HEAD_LEN + 5 + 5 +
			   strlen(el) - 1 + attr_bytes(attrs));
		write_bin(el, space - el);
		write_bin(space + 1, strlen(space + 1));
	}
	write_attrs(attrs);
}

void XMLCALL end(void *data, const char *el) {
	char *space;
	int el_len = strlen(el);

	if ((space = strchr(el, ' ')) == NULL) { /* no namespace */
		write_head(resp_end, RESP_HEAD_LEN + 5 + 5 + el_len);
		write_bin("", 0);
		write_bin(el, el_len);
	} else {
		write_head(resp_end, RESP_HEAD_LEN + 5 + 5 + el_len - 1);
		write_bin(el, space - el);
		write_bin(space + 1, el_len - (space - el + 1));
	}
}

void XMLCALL char_data(void *data, const XML_Char *s, int len) {
	write_head(resp_char_data, RESP_HEAD_LEN + 5 + len);
	write_bin(s, len);
}

void new_parser(state *st) {
	if ((st->p = XML_ParserCreateNS(NULL, ' ')) == NULL)
		error("Failed creating parser");
	XML_SetElementHandler(st->p, &start, &end);
	XML_SetCharacterDataHandler(st->p, &char_data);
}


void read_exact(char *buf, int left) {
	int l;

	for (left = left; left > 0; left -= l, buf += l) {
		if ((l = read(ERL_IN, buf, left)) <= 0) error("Failed reading");
	}
}

int read_bin_head() {
	char bin_head[5];
	int idx = 0;
	int type, len;

	read_exact(bin_head, 5);

	if (ei_get_type(bin_head, &idx, &type, &len))
		error("get_type");
	if (type != ERL_BINARY_EXT)
		error("'parse' was expecting binary");

	return len;
}

void parse(state *st, int arity) {
	int len, done;

	if (arity != 1)
		error("Wrong arity for 'parse'");

	if (st->p == NULL)
		new_parser(st);

	len = read_bin_head();
	done = len == 0;
	while (len > 0) {
		char *buf;
		int rsz;
		int sz = MIN(len, EXPAT_BUF_SZ);

		if (! (buf = XML_GetBuffer(st->p, sz)))
			error("Failed allocating buffer");

		if ((rsz = read(ERL_IN, buf, sz)) < 0)
			error("reading");

		if (! XML_ParseBuffer(st->p, rsz, done)) {
			fprintf(stderr, "Parse error at line %d:\r\n%s\r\n",
				XML_GetCurrentLineNumber(st->p),
				XML_ErrorString(XML_GetErrorCode(st->p)));
			exit(1);
		}
		len -= rsz;
	}

	if (done)
		XML_ParserReset(st->p, NULL);
}

void build_resp(char *buf, char resp, int arity) {
	int idx = 0;

	if (ei_encode_version(buf, &idx))
		error("encoding_verison");
	if (ei_encode_tuple_header(buf, &idx, arity + 1))
		error("encoding_tuple");
	if (ei_encode_char(buf, &idx, resp))
		error("encoding_resp");
}


int main(int argc, char **argv) {
	state _st = {NULL};
	state *st = &_st;

	/* prebuild common headers so we can just write them straight out */
	build_resp(resp_error, RESP_ERROR, 1);
	build_resp(resp_start, RESP_START, 3);
	build_resp(resp_end, RESP_END, 2);
	build_resp(resp_char_data, RESP_CHAR_DATA, 1);

	while(1) {
		char buf[REQ_HEAD_LEN];
		int arity;
		char cmd;
		int idx = 0;

		read_exact(buf, 4);
		read_exact(buf, REQ_HEAD_LEN);

		if (ei_decode_version(buf, &idx, NULL))
			error("version");
		if (ei_decode_tuple_header(buf, &idx, &arity))
			error("decoding_tuple");
		arity--;
		if (ei_decode_char(buf, &idx, &cmd))
			error("decoding_cmd");

		switch (cmd) {
		case CMD_PARSE:
			parse(st, arity);
			break;
		default: error("unknown_command");
		}
	}
}
