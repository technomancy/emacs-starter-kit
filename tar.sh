#!/bin/bash

version=$1

if [ "$version" = "" ]; then
    echo "Usage: tar.sh \$VERSION"
    exit 1
fi

dir="starter-kit-${version}"

mkdir $dir

cp starter-kit*.el $dir

tar cf starter-kit-${version}.tar $dir
