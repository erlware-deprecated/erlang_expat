all: expat

clean:
	rm ../priv/expat

expat: expat.c
	gcc -g -o ../priv/expat -O2 -Wall expat.c -I/sw/include -lexpat -L/sw/lib -I/usr/local/stow/otp-r11b2/lib/erlang/lib/erl_interface-3.5.5.2/include -L/usr/local/stow/otp-r11b2/lib/erlang/lib/erl_interface-3.5.5.2/lib -lei
