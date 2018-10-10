#!/bin/bash
## Update version of polymode and all poly-xyz packages in the parent directory
## NB: Modify Version: x.y.z in polymode.el then make update-versions

set -e

DIRS=( "../polymode" "../poly-*" )
VERSION=$(grep Version polymode.el | sed 's/.*Version: *\(.*\) */\1/')

for d in ${DIRS[@]} ;
do
    pkg=$(basename "$d").el
    echo "** TAGGING $pkg with v$VERSION ..."
    if git rev-parse "v$VERSION" >/dev/null 2>&1; then
        echo "** TAG EXISTS";
    else
        cd "$d"
        echo '** Updating version ...'
        sed -i "s/(\(poly-\?[a-z]\+\) \"[0-9.]\+\")/(\1 \"$VERSION\")/g" $pkg
        git add $pkg
        git commit -m "Version $VERSION"
        git fetch --tags
        echo '** Pushing to upstream ...'
	    git tag "v$VERSION"
	    # git push --set-upstream origin master
        # git push --tags
        cd -
        echo '** DONE'
    fi
done



