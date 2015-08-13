#!/usr/bin/env perl

while (<>) {
    s/\(\*/QWERT/g;
    s/\*\)/TREWQ/g;
    s/&/  ASDFASDF  /g;
    s/\|/  +||  /g;
    s/->/  OPERATOR  /g;
    s/\/-/  ^^-->  /g;
    s/!/  !!!  /g;
    s/%/  !@@  /g;
    s/\?/  !??  /g;
    s/\+/  +++  /g;
    s/\^/  !++  /g;
    s/\*/  %**  /g;
    s/==/   @===  /g;
    s#/=#  ^^-->  #g;
    s/OPERATOR/^->/g;
    s/ASDFASDF/%&&/g;
    s/0/  Zero  /g;
    s/QWERT/(*/g;
    s/TREWQ/*)/g;
    print;
}
