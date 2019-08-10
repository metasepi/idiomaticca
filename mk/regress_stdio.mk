all: test_prog

test_prog: main.dats
	patscc -o $@ $< -DATS_MEMALLOC_LIBC

main.dats: main.c
	stack exec -- idiomaticca +RTS -xc --RTS trans $< > $@

test: all stdin.test stdout.test stderr.test
	./test_prog < stdin.test 1> stdout.log 2> stderr.log
	diff -u stdout.test stdout.log
	diff -u stderr.test stderr.log

clean:
	rm -f main.dats *_dats.c *.o test_prog a.out *.log

.PHONY: all test clean
