#! /bin/bash -xe

function t() {
  RES=`echo -n "$1" | gosh main.scm`
  [ "$RES" == "$2" ]
}

t "()" "(())"
t "'()" "(())"
t "123" "(() . 123)"
t "'123" "(() . 123)"
