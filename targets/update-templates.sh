#!/bin/bash
## Update templates of all poly-xyz packages in the parent directory

set -e

CURDIR=$(pwd)
# DIRS=( "../poly-erb")
DIRS=( "../poly-*" )

for D in ${DIRS[@]} ;
do
    git add -A .
    cd $D
    if ! git diff-index --cached --quiet HEAD -- ; then
        echo "Project $D has uncommitted changes" >&2
        cd $CURDIR
        exit 1
    fi
    cd $CURDIR
    echo $CURDIR
    make template$D
    cd $D
    git add -A .
    git diff-index --cached --quiet HEAD -- || git commit -m "Update from template"
    cd $CURDIR
done
