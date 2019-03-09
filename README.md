# Idiomaticca: translate IDIOMATIC C into readable ATS

## What's this?

[ATS](http://www.ats-lang.org/) is a statically typed programming language that unifies implementation with formal specification. It can safely use pointer and powerfully capture invariant in program.

However, there are already many [C language](https://en.wikipedia.org/wiki/C_(programming_language)) codes, which shape unsafe application. Should we manually rewrite the code with ATS to get safe application? No, it needs so much human resource.

This program is a translator from C code, which is written by idiomatic manner, to ATS code, which is human-readable. The translated code should be maintained by human, not only for C compiler. You may manually add some formal specifications to the code after translating.

## What's IDIOMATIC C code?

It means the C code written by idiomatic manner such as following:

* xxx TBD: show list of translatable example

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

Start from [a simple example](./regress/noinc/arithmetic_op/):

```
$ cat regress/noinc/arithmetic_op/main.c
int main() {
        return 1 + 2 - 3 * 4 / 4;
}
```

Translate the C code into ATS:

```
$ idiomaticca regress/noinc/arithmetic_op/main.c > main.dats
$ cat main.dats
#include "share/atspre_staload.hats"

implement main () =
  1 + 2 - 3 * 4 / 4
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

Not yet acceptable. You can feel free to post [some issues](https://github.com/metasepi/idiomaticca/issues) or [mailing list](http://groups.google.com/group/metasepi).

## Acknowledgements

The idiomaticca translator is inspired by Jamey Sharp's [Corrode](https://github.com/jameysharp/corrode) translator.

## License

[AGPL-3](./COPYING) or later.

Copyright (c) 2019 Metasepi Team.

## FAQ

### What license will be applied to my ATS code translated by idiomaticca?

AGPL-3 with your copyright should be applied to your ATS code.

Example: If you translate your C code published under BSD license, your translated ATS code should be published under AGPL-3.

### Are there another option to apply a license other than AGPL-3?

Yes. You can apply your own license to your translated ATS code by purchasing idiomaticca. Please contact Metasepi Team <metasepi@gmail.com>.
