CC=gcc

all: elf.c

libelf.so: elf.lisp ecl-build.lisp
	ecl -norc -shell ecl-build.lisp

run-test: run-test.c libelf.so
	gcc -o run-test run-test.c -L/usr/local/lib -L. -lelf -lecl

test: run-test
	export export LD_LIBRARY_PATH=`pwd` && ./run-test

clean:
	rm -f *.fas *.fasl *.lx32fsl *.lx64fsl \
		*.a *.o *.h *.so *.eclh *.data \
		elf.html elf.pdf elf.tex
