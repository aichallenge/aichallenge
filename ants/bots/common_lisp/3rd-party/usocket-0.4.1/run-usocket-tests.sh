#!/bin/sh

# Test script to be run from the usocket source root
#
# Unfortunately, it currently works only with SBCL
# in my setup...
#
# I need to figure out how to setup ASDF with the other lisps
# I have installed: cmucl, ABCL, clisp, allegro and lispworks

cd `dirname $0`/test
rm tests.log

if test -z "$1" ; then
  lisps=*.conf
else
  lisps=$1
fi

for my_lisp_conf in $lisps ; do


args=
lisp_bin=
lisp_name=
lisp_exit="(quit result)"

. $my_lisp_conf

if test -z "$lisp_bin" ; then
  echo "YOU NEED TO SET A LISP BINARY IN YOUR CONF FILE"
  exit 1
fi

if test -z "$lisp_name" ; then
  lisp_name="`basename \"$lisp_bin\"`"
fi

echo "
#-sbcl (load \"asdf.lisp\")

(asdf:operate #-sbcl 'asdf:load-source-op
              #+sbcl 'asdf:load-op :usocket-test)

(let ((result (if (usocket-test:do-tests) 1 0)))
  $lisp_exit)
" | $lisp_bin $args

if test $? -eq 1 ; then
  echo "PASS: $lisp_name" >> tests.log
else
  echo "FAIL: $lisp_name" >> tests.log
fi

echo "Above the test results gathered for $lisp_name."

done
