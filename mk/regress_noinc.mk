all: test_prog

test_prog: main.dats
	patscc -o $@ $< -DATS_MEMALLOC_LIBC

main.dats: main.c
	stack run idiomaticca trans $< > $@

test: all
	./test_prog

clean:
	rm -f main.dats *_dats.c *.o test_prog

.PHONY: all test clean
