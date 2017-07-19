#!/bin/bash
set -e

cd client && stack build
cp -f `stack path --local-install-root`/bin/taiji-viz-client.jsexe/*.js ../server/data/ && cd ..

cd server && stack build
mv `stack path --local-install-root`/bin/taiji-viz ../ && cd ..
