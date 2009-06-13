#!/bin/bash

set -e 

cd $(dirname $0)

runhaskell ./Setup.hs configure
runhaskell ./Setup.hs haddock --hyperlink-source

if [ "$USER" = "jojo" ]
then
	rsync -rva --delete dist/doc/html/L-seed/ bl0rg:public_html/L-seed-doc/
fi
