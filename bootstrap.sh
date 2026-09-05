#!/bin/sh -e

mkdir -p build
cd build
git clone https://codeberg.org/guile/guile --revision=26200b67055e51f12f47f03fba50971b7ae4fae0 --depth=1
cd guile
# git reset
# The older version differs slightly from the newly generated one
echo 'c575d13b0388cd3be07282f733ffadca7bbadff247185c9ac40a361057c05a53  module/ice-9/psyntax-pp.scm' >psyntax-pp.sha256
rm module/ice-9/psyntax-pp.scm

## now let us prepare to rebuild it
echo '(primitive-load-path "psyntax-bootstrap/allsteps")' >module/ice-9/psyntax-pp.scm
mkdir -p module/psyntax-bootstrap
cp ../../psyntax-bootstrap/*.scm module/psyntax-bootstrap
cd module/ice-9
cp psyntax.scm psyntax-patched.scm
patch <../../../../stage2.patch
cd ../..

## rebuild it
./autogen.sh
./configure --prefix=/tmp
make config.h
make libguile/scmconfig.h
make .version
cd lib
make all
cd ../meta
make all
cd ../libguile
make all
cd ../module
make ice-9/psyntax-pp.scm.gen
cd ..

## verify it
sha256sum module/ice-9/psyntax-pp.scm
sha256sum -c psyntax-pp.sha256

## DONE ##
