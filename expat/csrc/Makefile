BUILD_DIR=$(echo ../../../_build/development/apps/expat-*/)

all: ${BUILD_DIR}/priv/expat

${BUILD_DIR}/priv:
	mkdir ${BUILD_DIR}/priv

${BUILD_DIR}/priv/expat: expat.c ${BUILD_DIR}/priv
	gcc -g -o ${BUILD_DIR}/priv/expat -O2 -Wall expat.c -I/sw/include -I/usr/local/stow/otp-r11b2/lib/erlang/lib/erl_interface-3.5.5.2/include -L/usr/local/stow/otp-r11b2/lib/erlang/lib/erl_interface-3.5.5.2/lib -lei /sw/lib/libexpat.a
