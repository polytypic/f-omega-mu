#!/bin/bash

set -eo pipefail

git branch -D gh-pages
git checkout -b gh-pages

rm docs/.gitignore

git add docs
git commit -m 'Built GitHub pages'
git push -f -u origin gh-pages

git checkout main
