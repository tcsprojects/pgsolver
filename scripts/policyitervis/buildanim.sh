#!/bin/sh

# 1st parameter = call to game generator
# 2nd parameter = argument to policyitervis
# 3rd parameter = target files base name
# 4th parameter = Metapost template
# 5th parameter = Latex template

BASEDIR=$(dirname $0)/../..

$BASEDIR/bin/$1 | $BASEDIR/bin/policyitervis -pi "$2" > $3_animation.mp

cp $4 $3_main.mp

echo "input ./$3_animation;" >> $3_main.mp
echo "end;" >> $3_main.mp

cp $5 $3_main.tex

for line in `cat $3_animation.mp | grep "beginfig" | sed s/beginfig\(// | sed s/\)\;//`; do
 echo "\\\\animationgraphic{$3_main.$line}" >> $3_main.tex
 echo "" >> $3_main.tex
done

echo "\\\\end{document}" >> $3_main.tex

mpost $3_main.mp
mpost $3_main.mp
pdflatex $3_main.tex
