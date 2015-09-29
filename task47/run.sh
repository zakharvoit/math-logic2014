(echo "|- A"; ((for x in $@ ; do echo $x ; done) | ./task7.native)) >proof.txt
echo >/dev/stderr "Checking generated proof"
./task4.native <proof.txt
