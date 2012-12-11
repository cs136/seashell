all: aodb.so ss_env

aodb.so: aodb.c
	gcc -Wall -fPIC -shared -O2 -g aodb.c -o aodb.so -lrt

ss_env: ss_env.c
	gcc -Wall -O2 -D_FORTIFY_SOURCE=2 -g ss_env.c -o ss_env

clean:
	rm -fr aodb.so compiled ss_env

.PHONY: all clean
