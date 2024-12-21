#!/usr/bin/env bash

git submodule update --remote --merge

list=$(git submodule | grep '^+');

M="The following Emacs MELPA packages have been updated

"
PKG=""
while IFS= read -r line; do
    x=$line

    pkg_repo=$(echo $x | cut -d " " -f 2)
    repo=$(echo $pkg_repo | cut -d "/" -f 2)
    git add $pkg_repo
    version=$(echo $x | cut -d " " -f 3 | sed 's/(//g' | sed 's/)//g')

    PKG+="  * ${repo} was updated to ${version}
"
done <<< "$list"

git commit -m "$M$PKG"
