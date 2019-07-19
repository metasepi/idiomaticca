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

Start from [a simple example](./regress/noinc/arithmetic_op/):

```
$ cat regress/noinc/arithmetic_op/main.c
int main() {
        return 1 + 2 - 3 * 4 / 4;
}
```

Translate the C code into ATS:

```
$ idiomaticca trans regress/noinc/arithmetic_op/main.c > main.dats
$ cat main.dats
```

```ats
(*
 * Copyright (c) 2019 YOUR NAME
 * All rights reserved.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public
 * License along with this program.
 * If not, see <http://www.gnu.org/licenses/>.
 *)

#include "share/atspre_staload.hats"

staload UN = "prelude/SATS/unsafe.sats"

implement main () =
  let
    
  in
    1 + 2 - 3 * 4 / 4
  end
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

AGPL-3 with your copyright should be applied to your ATS code.

Example: If you translate your C code published under BSD license, your translated ATS code should be published under AGPL-3.

### Are there another option to apply a license other than AGPL-3?

Yes. You can apply your own license to your translated ATS code by purchasing idiomaticca. Please contact Metasepi team <metasepi@gmail.com>.
