#!/bin/bash

set -eo pipefail

mkdir tmp
cd tmp

git clone git@github.com:polytypic/f-omega-mu.git
cd f-omega-mu

git checkout -b gh-pages

script/travis.sh

rm docs/.gitignore

script/docs.sh

git add docs
git commit -m 'Built GitHub pages'
git push -f -u origin gh-pages

cd ../..
rm -rf tmp
