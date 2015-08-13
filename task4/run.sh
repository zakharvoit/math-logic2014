(echo "|- A"; ((for x in $@ ; do echo $x ; done) | ./task7.native)) >proof.txt
./task4.native <proof.txt
