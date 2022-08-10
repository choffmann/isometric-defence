#!/bin/bash

set -euxo pipefail

git clone --no-checkout --depth 1 https://github.com/hsfl-pltp/elm-review-pltp.git

cd elm-review-pltp/

git sparse-checkout set --no-cone review/elm.json review/src/
git checkout

cd ../

ln -s elm-review-pltp/review/ ./review
