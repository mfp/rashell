### Makefile -- Project Rashell

# Rashell (https://github.com/michipili/rashell)
# This file is part of Rashell
#
# Copyright © 2015—2016 Michael Grünewald
#
# This file must be used under the terms of the MIT license.
# This source file is licensed as described in the file LICENSE, which
# you should have received as part of this distribution. The terms
# are also available at
# https://opensource.org/licenses/MIT

PACKAGE=		rashell
VERSION=		0.2.1-current
OFFICER=		michipili@gmail.com

MODULE=			ocaml.lib:src
MODULE+=		ocaml.prog:example
MODULE+=		ocaml.meta:meta
MODULE+=		ocaml.manual:manual

SUBDIR=			testsuite

EXTERNAL=		ocaml.findlib:broken
EXTERNAL+=		ocaml.findlib:lemonade
EXTERNAL+=		ocaml.findlib:lwt.unix
EXTERNAL+=		ocaml.findlib:mixture
EXTERNAL+=		ocaml.findlib:str
EXTERNAL+=		ocaml.findlib:atdgen

CONFIGURE+=		Makefile.config.in
CONFIGURE+=		src/rashell_Configuration.ml.in

.include "generic.project.mk"

### End of file `Makefile'
