#!/bin/bash
## Update version of polymode and all poly-xyz packages in the parent directory
## NB: Modify Version: x.y.z in polymode.el then make update-versions.
## NB: !!! Don't commit here !!!

set -e

SED=sed
SED_I_ARG=(-i)
$SED --version >/dev/null 2>&1 || { type -t gsed >/dev/null 2>&1 && SED=gsed; } || SED_I_ARG+=('')
# DIRS=( "../poly-erb")
DIRS=( "../polymode" "../poly-*" )
VERSION=$(grep Version polymode.el | $SED 's/.*Version: *\(.*\) */\1/')

for d in ${DIRS[@]} ;
do
    cd "$d"
    pkg=$(basename "$d").el
    echo "** TAGGING $pkg with v$VERSION ..."
    if git rev-parse "v$VERSION" >/dev/null 2>&1; then
        echo "** TAG EXISTS";
    else
        $SED "${SED_I_ARG[@]}" "s/\(;; Version: .\+\)/;; Version: $VERSION/g" $pkg
        $SED "${SED_I_ARG[@]}" "s/(\(poly-\?[a-z]\+\) \"[0-9.]\+\")/(\1 \"$VERSION\")/g" $pkg
        git add $pkg
        git commit -m "Version $VERSION"
        git fetch --tags
        echo '** Pushing to upstream ...'
	    git tag "v$VERSION"
	    git push --set-upstream origin master
        git push --tags
        echo '** DONE'
    fi
    cd -
done
