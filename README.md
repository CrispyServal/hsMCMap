# hsMCMap
read mc region file and output world map, including NBT parser.

# Installation
an easy way to install this is using cabal

``` bash
# clone from git
git clone https://github.com/DFLY-Entertainment/hsMCMap.git
cd hsMCMap
# if you only want to build an executable, install only dependencies
cabal install --only-dependencies hsMCMap.cabal
# if you want to install hsMCMap with library then:
# cabal install hsMCMap.cabal
# configure and build
cabal configure
cabal build
```

# Usage

``` bash
# replace 2 with the number of cores of your PC
./main YOUR_REGION_DIR OUTPUT.png + RTS -N2
```
