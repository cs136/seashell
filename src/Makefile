all: aodb.so

aodb.so: aodb.c
	gcc -Wall -fPIC -shared -O2 -g aodb.c -o aodb.so -lrt

clean:
	rm -fr aodb.so compiled

.PHONY: all clean
