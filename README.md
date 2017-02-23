# hsMCMap
Read mc region file and output world map, including general NBT parser.


# Installation
An easy way to install this is using cabal

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
# 255 is default value for y_slicing height
./main YOUR_REGION_DIR OUTPUT.png [Y_SLICING_HEIGHT] + RTS -N2
```

# references
This is the final project for my Haskell course.

I got many cool ideas about Haskell from other projects. Some of my code is almost the same as theirs, but I have understanded how these code works.

The referenced works are listed blow:

[1] https://github.com/FalconNL/mc2obj (IntMap, BNT data struct, how to parse compoundElements in NBT, and the elegant way to navigate the NBT)

[2] https://github.com/acfoltzer/nbt (NBT data struct )

The gamepedia records information about the region file format, NBT format, and the Block IDs.

[3] http://minecraft.gamepedia.com/Data_values#Block_IDs

[4] http://minecraft.gamepedia.com/Chunk_format

and I also refer to this blog:

[5] http://nathanwilliams.github.io/2013/04/16/minecraft-region-files/
