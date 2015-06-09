mkdir -p _build
rm -f _build/*.ml
cp src/*.ml _build
cd _build
for i in *.ml; do
	ocamlc -c -g $i -o ${i%%o}cmo
done	
cd ..
ocamlc _build/*.cmo -o task8.byte
