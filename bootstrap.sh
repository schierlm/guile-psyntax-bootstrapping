#!/bin/sh -e

mkdir -p build
cd build
wget -nc https://ftp.gnu.org/gnu/guile/guile-3.0.2.tar.xz
rm -rf guile-3.0.2
tar xfvJ guile-3.0.2.tar.xz
cd guile-3.0.2
sha256sum module/ice-9/psyntax-pp.scm | tee psyntax-pp.sha256
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
