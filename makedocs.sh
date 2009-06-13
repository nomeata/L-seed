#!/bin/bash

set -e 

cd $(dirname $0)

haddock -o doc/ -h $(find src/Lseed -name \*.hs)

if [ "$USER" = "jojo" ]
then
	rsync -rva --delete doc/ bl0rg:public_html/L-seed-doc/
fi
