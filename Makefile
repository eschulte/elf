CC=gcc
HERE=$(shell pwd)
LD_LIBRARY_PATH:=$(HERE)

all: elf.c

libelf.so: elf.lisp ecl-build.lisp
	ecl -norc -shell ecl-build.lisp

run-test: run-test.c libelf.so
	gcc -o run-test run-test.c -L/usr/local/lib -L. -lelf -lecl

test: run-test
	./run-test|egrep -v "^;;;"

clean:
	rm -f *.fas *.fasl *.lx32fsl *.lx64fsl \
		*.a *.o *.h *.so *.eclh *.data \
		elf.html elf.pdf elf.tex \
		run-test
