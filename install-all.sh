#!/bin/sh

cd ~/Projects/FruitTart && \
cd FruitTartInterface && \
cabal install && \
cd ../FruitTart && \
cabal install && \
cd ../Base && \
cabal install && \
cd ../Captcha && \
cabal install && \
cd ../Buglist && \
cabal install && \
cd ../Blog && \
cabal install && \
cd ../Adventure && \
cabal install && \
echo "\n\nDONE!!!\n"
