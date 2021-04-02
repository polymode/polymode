#!/bin/bash

OUTDIR=$1
MODULE=$(basename $OUTDIR)

CP=cp
T_ARG=T
SEDI=(sed -i)
$CP --version >/dev/null 2>&1 || { type -t gcp >/dev/null 2>&1 && CP=gcp; } || T_ARG=
sed --version >/dev/null 2>&1 || SEDI+=('')
POLYMODE_VERSION=$(grep Version polymode.el | sed 's/.*Version: *\(.*\) */\1/')

echo "-- Copying template to $OUTDIR"
$CP -${T_ARG}fr template/ $OUTDIR/

echo "-- Replacing __MODULE__ with $MODULE"
for f in "$OUTDIR"/* ;
do
    if [ ! -d "$f" ]; then
	  "${SEDI[@]}" "s/__MODULE__/$MODULE/g" $f;
    fi
done

for f in "$OUTDIR"/targets/* ;
do
    if [ ! -d "$f" ]; then
	  "${SEDI[@]}" "s/__MODULE__/$MODULE/g" $f;
    fi
done

for f in "$OUTDIR"/tests/* ;
do
    if [ ! -d "$f" ]; then
	  "${SEDI[@]}" "s/__MODULE__/$MODULE/g" $f;
    fi
done

echo "-- Creating $OUTDIR/$MODULE.el"
"${SEDI[@]}" "s/__POLYMODE_VERSION__/$POLYMODE_VERSION/g" $OUTDIR/poly-xyz.el
mv -n $OUTDIR/poly-xyz.el $OUTDIR/$MODULE.el
mv -n $OUTDIR/README-xyz.md $OUTDIR/README.md
mv -n $OUTDIR/tests/xyz-tests.el $OUTDIR/tests/$MODULE-tests.el
rm -f $OUTDIR/*xyz*
rm -f $OUTDIR/tests/xyz*

echo "-- Done!"
