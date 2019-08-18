# Idiomaticca: translate IDIOMATIC C into readable ATS

[![Build Status](https://travis-ci.org/metasepi/idiomaticca.svg)](https://travis-ci.org/metasepi/idiomaticca)

## What's this?

[ATS](http://www.ats-lang.org/) is a statically typed programming language that unifies implementation with formal specification. It can safely use pointer and powerfully capture invariant in program.

However, there are already many [C language](https://en.wikipedia.org/wiki/C_(programming_language)) codes, which shape unsafe application. Should we manually rewrite the code with ATS to get safe application? No, it needs so much human resource.

This program is a translator from C code, which is written by idiomatic manner, to ATS code, which is human-readable. The translated code should be maintained by human, not only for C compiler. You may manually add some formal specifications to the code after translating.

## What's IDIOMATIC C code?

It means the C code written by idiomatic manner.
Following will be supported.

* [NetBSD](https://www.netbsd.org/) kernel
* [MISRA C](https://www.misra.org.uk/)

## Requirements

* [ATS-Postiats](http://www.ats-lang.org/) to compile ATS code translated by idiomaticca
* [GCC](https://gcc.gnu.org/) to parse C language header
* [Stack](https://www.haskellstack.org/) to build [Haskell](https://www.haskell.org/) code

We are testing this program on [Debian GNU/Linux](https://www.debian.org/).

## Install

Get source code of the idiomaticca:

```
$ git clone https://github.com/metasepi/idiomaticca.git
$ cd idiomaticca
```

Then install it using stack:

```
$ make install
$ which idiomaticca
/home/YourName/.local/bin/idiomaticca
```

## Usage

Start from [a simple example](./regress/noinc/loop_for/):

```
$ cat regress/noinc/loop_for/main.c
```
```c
int sum1(int n) {
	int i, sum = 0;

	for (i = 1; i <= n; i = i + 1) {
		sum = sum + i;
	}

	return sum;
}

int sum2(int n) {
	int i, sum = 0;

	for (i = 1; i <= n; i++) {
		sum = sum + i;
	}

	return sum;
}

int main() {
	return sum1(5) - sum2(5);
}
```

Translate the C code into ATS:

```
$ idiomaticca trans regress/noinc/loop_for/main.c > main.dats
$ cat main.dats
```

```ats
#include "share/atspre_staload.hats"

staload UN = "prelude/SATS/unsafe.sats"

fun sum1(n : int) : int =
  let
    var n: int = n
  in
    let
      var i: int
      var sum: int = 0

      fun loop_for(i : int, n : int, sum : int) : (int, int, int) =
        let
          var i: int = i
          var n: int = n
          var sum: int = sum
        in
          if i <= n then
            let
              val () = sum := sum + i
              val () = i := i + 1
            in
              loop_for(i, n, sum)
            end
          else
            (i, n, sum)
        end

      val () = i := 1
      val (i9a_i, i9a_n, i9a_sum) = loop_for(i, n, sum)
      val () = i := i9a_i
      val () = n := i9a_n
      val () = sum := i9a_sum
    in
      sum
    end
  end

fun sum2(n : int) : int =
  let
    var n: int = n
  in
    let
      var i: int
      var sum: int = 0

      fun loop_for(i : int, n : int, sum : int) : (int, int, int) =
        let
          var i: int = i
          var n: int = n
          var sum: int = sum
        in
          if i <= n then
            let
              val () = sum := sum + i
              val () = i := i + 1
            in
              loop_for(i, n, sum)
            end
          else
            (i, n, sum)
        end

      val () = i := 1
      val (i9a_i, i9a_n, i9a_sum) = loop_for(i, n, sum)
      val () = i := i9a_i
      val () = n := i9a_n
      val () = sum := i9a_sum
    in
      sum
    end
  end

implement main () =
  sum1(5) - sum2(5)
```

Compile the translated ATS code and run it:

```
$ patscc main.dats
$ ls a.out main*
a.out*  main.dats  main_dats.c
$ ./a.out
$ echo $?
0
```

`idiomaticca` command line option will be changed, and not stable today.

The other examples are found at [regress](./regress/) directory.

## Contributing

Please send pull request, and write your name and reachable e-mail address in [CONTRIBUTORS.md](./CONTRIBUTORS.md).

You also feel free to post [some issues](https://github.com/metasepi/idiomaticca/issues) or [mailing list](http://groups.google.com/group/metasepi).

## Acknowledgements

The idiomaticca translator is inspired by Jamey Sharp's [Corrode](https://github.com/jameysharp/corrode) translator.

## License

[AGPL-3](./COPYING) or later.

Copyright (c) 2019 Metasepi team.

## FAQ

### What license will be applied to my ATS code translated by idiomaticca?

You can apply your own license to your ATS code.

Example: If you translate your C code published under BSD license, you can publish your translated ATS code under BSD license.
