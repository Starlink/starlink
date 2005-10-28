#!/bin/sh

cd PREFIX/build/public_html
rsync -avz --delete -e ssh . ADD_YOUR_PATH_TO_WEB_DIR

