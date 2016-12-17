module Game.Minecraft.Map.Block (
              blocks
            , ifDraw
            , getBlockColor
            ) where
import           Data.Bits
import qualified Data.IntMap.Strict as I
import           Data.Word
data BlockInfo = BlockInfo {    toDrawBlock :: Bool
                            ,   nameBlock   :: String
                            ,   colorBblock :: [Word8]
                            } deriving Show

ifDraw :: (Word8,Word8) -> Bool
ifDraw (id, add) = toDrawBlock $ (blocks I.! fromIntegral id ) add

getBlockColor :: (Word8,Word8) -> [Word8]
getBlockColor (id,add) = colorBblock $ (blocks I.! fromIntegral id) add

bugBlock :: BlockInfo
bugBlock = BlockInfo True "bug" [244, 47, 224, 255]

blocks :: I.IntMap (Word8 -> BlockInfo)
blocks = I.fromDistinctAscList
    [( 0, \_     -> BlockInfo False "air" [0, 0, 0, 0] )
    ,( 1, \_     -> BlockInfo True "stone" [80, 80, 80, 255] )
    ,( 2, \_     -> BlockInfo True "grass" [76, 104, 38, 255] )
    ,( 3, \_     -> BlockInfo True "dirt" [118, 82, 55, 255] )
    ,( 4, \_     -> BlockInfo True "cobblestone" [135, 135, 135, 255] )
    ,( 5, \i     -> case i of
                    0 -> BlockInfo True "oak_wood_planks" [186, 150, 97, 255]
                    1 -> BlockInfo True "spruce_wood_planks" [119, 87, 53, 255]
                    2 -> BlockInfo True "birch_wood_planks" [212, 201, 139, 255]
                    3 -> BlockInfo True "jungle_wood_planks" [113, 79, 52, 255]
                    4 -> BlockInfo True "acacia_wood_planks" [185, 102, 54, 255]
                    5 -> BlockInfo True "dark_oak_wood_planks" [55, 33, 8, 255]
                    _ -> bugBlock
    )
    ,( 6, \_     -> BlockInfo False "sapling" [0, 0, 0, 255] )
    ,( 7, \_     -> BlockInfo True "bedrock" [39, 39, 39, 255] )
    ,( 8, \_     -> BlockInfo True "flowing_water" [105, 113, 188, 255] )
    ,( 9, \_     -> BlockInfo True "water" [105, 113, 188, 255] )
    ,( 10, \_     -> BlockInfo True "flowing_lava" [192, 65, 8, 255] )
    ,( 11, \_     -> BlockInfo True "lava" [192, 65, 8, 255] )
    ,( 12, \_     -> BlockInfo True "sand" [209, 202, 151, 255] )
    ,( 13, \_     -> BlockInfo True "gravel" [121, 117, 115, 255] )
    ,( 14, \_     -> BlockInfo True "gold_ore" [150, 133, 102, 255] )
    ,( 15, \_     -> BlockInfo True "iron_ore" [159, 137, 121, 255] )
    ,( 16, \_     -> BlockInfo True "coal_ore" [54, 54, 54, 255] )
    ,( 17, \i     -> case i .&. 0x03 of
                    0 -> BlockInfo True "oak_wood" [186, 150, 97, 255]
                    1 -> BlockInfo True "spruce_wood" [119, 87, 53, 255]
                    2 -> BlockInfo True "birch_wood" [113, 79, 52, 255]
                    3 -> BlockInfo True "jungle_wood" [55, 33, 8, 255]
                    _ -> bugBlock
    )
    ,( 18, \_     -> BlockInfo True "leaves" [51, 126, 17, 255] )
    ,( 19, \_     -> BlockInfo True "sponge" [188, 188, 85, 255] )
    ,( 20, \_     -> BlockInfo False "glass" [0, 0, 0, 255] )
    ,( 21, \_     -> BlockInfo True "lapis_ore" [114, 119, 130, 255] )
    ,( 22, \_     -> BlockInfo True "lapis_block" [35, 61, 127, 255] )
    ,( 23, \_     -> BlockInfo True "dispenser" [80, 80, 80, 255] )
    ,( 24, \_     -> BlockInfo True "sandstone" [141, 136, 107, 255] )
    ,( 25, \_     -> BlockInfo True "noteblock" [120, 75, 53, 255] )
    ,( 26, \_     -> BlockInfo True "bed" [131, 19, 20, 255] )
    ,( 27, \_     -> BlockInfo False "golden_rail" [0, 0, 0, 255] )
    ,( 28, \_     -> BlockInfo False "detector_rail" [0, 0, 0, 255] )
    ,( 29, \_     -> BlockInfo True "sticky_piston" [80, 80, 80, 255] )
    ,( 30, \_     -> BlockInfo True "web" [212, 212, 212, 255] )
    ,( 31, \_     -> BlockInfo False "tallgrass" [0, 0, 0, 255] )
    ,( 32, \_     -> BlockInfo False "deadbush" [0, 0, 0, 255] )
    ,( 33, \_     -> BlockInfo True "piston" [80, 80, 80, 255] )
    ,( 34, \_     -> BlockInfo True "piston_head" [186, 150, 97, 255] )
    ,( 35, \i     -> case i of
                    0 -> BlockInfo True "white_wool" [221, 221, 221, 255]
                    1 -> BlockInfo True "orange_wool" [219, 125, 62, 255]
                    2 -> BlockInfo True "magenta_wool" [179, 80, 188, 255]
                    3 -> BlockInfo True "light_blue_wool" [107, 138, 201, 255]
                    4 -> BlockInfo True "yellow_wool" [177, 166, 39, 255]
                    5 -> BlockInfo True "lime_wool" [65, 174, 56, 255]
                    6 -> BlockInfo True "pink_wool" [208, 132, 153, 255]
                    7 -> BlockInfo True "gray_wool" [64, 64, 64, 255]
                    8 -> BlockInfo True "light_gray_wool" [154, 161, 161, 255]
                    9 -> BlockInfo True "cyan_wool" [46, 110, 137, 255]
                    10 -> BlockInfo True "purple_wool" [126, 61, 181, 255]
                    11 -> BlockInfo True "blue_wool" [46, 56, 141, 255]
                    12 -> BlockInfo True "brown_wool" [79, 50, 31, 255]
                    13 -> BlockInfo True "green_wool" [53, 70, 27, 255]
                    14 -> BlockInfo True "red_wool" [150, 52, 48, 255]
                    15 -> BlockInfo True "black_wool" [25, 22, 22, 255]
                    _ -> bugBlock
    )
    ,( 36, \_     -> BlockInfo False "piston_extension" [0, 0, 0, 255] )
    ,( 37, \_     -> BlockInfo False "yellow_flower" [0, 0, 0, 255] )
    ,( 38, \_     -> BlockInfo False "red_flower" [0, 0, 0, 255] )
    ,( 39, \_     -> BlockInfo False "brown_mushroom" [0, 0, 0, 255] )
    ,( 40, \_     -> BlockInfo False "red_mushroom" [0, 0, 0, 255] )
    ,( 41, \_     -> BlockInfo True "gold_block" [237, 233, 95, 255] )
    ,( 42, \_     -> BlockInfo True "iron_block" [144, 144, 144, 255] )
    ,( 43, \_     -> BlockInfo True "double_stone_slab" [160, 160, 160, 255] )
    ,( 44, \_     -> BlockInfo True "stone_slab" [160, 160, 160, 255] )
    ,( 45, \_     -> BlockInfo True "brick_block" [116, 67, 53, 255] )
    ,( 46, \_     -> BlockInfo True "tnt" [82, 25, 10, 255] )
    ,( 47, \_     -> BlockInfo True "bookshelf" [186, 150, 97, 255] )
    ,( 48, \_     -> BlockInfo True "mossy_cobblestone" [76, 96, 76, 255] )
    ,( 49, \_     -> BlockInfo True "obsidian" [26, 21, 38, 255] )
    ,( 50, \_     -> BlockInfo False "torch" [0, 0, 0, 255] )
    ,( 51, \_     -> BlockInfo False "fire" [0, 0, 0, 255] )
    ,( 52, \_     -> BlockInfo True "mob_spawner" [26, 39, 47, 255] )
    ,( 53, \_     -> BlockInfo True "oak_stairs" [186, 150, 97, 255] )
    ,( 54, \_     -> BlockInfo True "chest" [148, 101, 28, 255] )
    ,( 55, \_     -> BlockInfo False "redstone_wire" [0, 0, 0, 255] )
    ,( 56, \_     -> BlockInfo True "diamond_ore" [143, 166, 175, 255] )
    ,( 57, \_     -> BlockInfo True "diamond_block" [74, 203, 197, 255] )
    ,( 58, \_     -> BlockInfo True "crafting_table" [186, 150, 97, 255] )
    ,( 59, \_     -> BlockInfo True "wheat" [124, 144, 11, 255] )
    ,( 60, \_     -> BlockInfo True "farmland" [118, 82, 55, 255] )
    ,( 61, \_     -> BlockInfo True "furnace" [80, 80, 80, 255] )
    ,( 62, \_     -> BlockInfo True "lit_furnace" [80, 80, 80, 255] )
    ,( 63, \_     -> BlockInfo False "standing_sign" [0, 0, 0, 255] )
    ,( 64, \_     -> BlockInfo False "wooden_door" [0, 0, 0, 255] )
    ,( 65, \_     -> BlockInfo False "ladder" [0, 0, 0, 255] )
    ,( 66, \_     -> BlockInfo False "rail" [0, 0, 0, 255] )
    ,( 67, \_     -> BlockInfo True "stone_stairs" [80, 80, 80, 255] )
    ,( 68, \_     -> BlockInfo False "wall_sign" [0, 0, 0, 255] )
    ,( 69, \_     -> BlockInfo False "lever" [0, 0, 0, 255] )
    ,( 70, \_     -> BlockInfo True "stone_pressure_plate" [80, 80, 80, 255] )
    ,( 71, \_     -> BlockInfo False "iron_door" [0, 0, 0, 255] )
    ,( 72, \_     -> BlockInfo True "wooden_pressure_plate" [186, 150, 97, 255] )
    ,( 73, \_     -> BlockInfo True "redstone_ore" [136, 55, 55, 255] )
    ,( 74, \_     -> BlockInfo False "lit_redstone_ore" [136, 55, 55, 255] )
    ,( 75, \_     -> BlockInfo False "unlit_redstone_torch" [0, 0, 0, 255] )
    ,( 76, \_     -> BlockInfo False "redstone_torch" [0, 0, 0, 255] )
    ,( 77, \_     -> BlockInfo False "stone_button" [0, 0, 0, 255] )
    ,( 78, \_     -> BlockInfo True "snow_layer" [230, 239, 239, 255] )
    ,( 79, \_     -> BlockInfo True "ice" [165, 195, 245, 255] )
    ,( 80, \_     -> BlockInfo True "snow" [230, 239, 239, 255] )
    ,( 81, \_     -> BlockInfo True "cactus" [15, 114, 27, 255] )
    ,( 82, \_     -> BlockInfo True "clay" [100, 104, 112, 255] )
    ,( 83, \_     -> BlockInfo True "reeds" [77, 157, 33, 255] )
    ,( 84, \_     -> BlockInfo True "jukebox" [118, 77, 54, 255] )
    ,( 85, \_     -> BlockInfo True "fence" [186, 150, 97, 255] )
    ,( 86, \_     -> BlockInfo True "pumpkin" [179, 108, 17, 255] )
    ,( 87, \_     -> BlockInfo True "netherrack" [84, 51, 43, 255] )
    ,( 88, \_     -> BlockInfo True "soul_sand" [44, 33, 26, 255] )
    ,( 89, \_     -> BlockInfo True "glowstone" [219, 180, 124, 255] )
    ,( 90, \_     -> BlockInfo True "portal" [59, 37, 100, 255] )
    ,( 91, \_     -> BlockInfo True "lit_pumpkin" [189, 114, 19, 255] )
    ,( 92, \_     -> BlockInfo True "cake" [216, 216, 217, 255] )
    ,( 93, \_     -> BlockInfo True "unpowered_repeater" [80, 80, 80, 255] )
    ,( 94, \_     -> BlockInfo True "powered_repeater" [80, 80, 80, 255] )
    ,( 95, \i     -> case i of
                    0 -> BlockInfo True "white_stained_glass" [221, 221, 221, 255]
                    1 -> BlockInfo True "orange_stained_glass" [219, 125, 62, 255]
                    2 -> BlockInfo True "magenta_stained_glass" [179, 80, 188, 255]
                    3 -> BlockInfo True "light_blue_stained_glass" [107, 138, 201, 255]
                    4 -> BlockInfo True "yellow_stained_glass" [177, 166, 39, 255]
                    5 -> BlockInfo True "lime_stained_glass" [65, 174, 56, 255]
                    6 -> BlockInfo True "pink_stained_glass" [208, 132, 153, 255]
                    7 -> BlockInfo True "gray_stained_glass" [64, 64, 64, 255]
                    8 -> BlockInfo True "light_gray_stained_glass" [154, 161, 161, 255]
                    9 -> BlockInfo True "cyan_stained_glass" [46, 110, 137, 255]
                    10 -> BlockInfo True "purple_stained_glass" [126, 61, 181, 255]
                    11 -> BlockInfo True "blue_stained_glass" [46, 56, 141, 255]
                    12 -> BlockInfo True "brown_stained_glass" [79, 50, 31, 255]
                    13 -> BlockInfo True "green_stained_glass" [53, 70, 27, 255]
                    14 -> BlockInfo True "red_stained_glass" [150, 52, 48, 255]
                    15 -> BlockInfo True "black_stained_glass" [25, 22, 22, 255]
                    _ -> bugBlock
    )
    ,( 96, \_     -> BlockInfo True "trapdoor" [186, 150, 97, 255] )
    ,( 97, \_     -> BlockInfo True "monster_egg" [80, 80, 80, 255] )
    ,( 98, \_     -> BlockInfo True "stonebrick" [75, 75, 75, 255] )
    ,( 99, \_     -> BlockInfo True "brown_mushroom_block" [88, 66, 51, 255] )
    ,( 100, \_     -> BlockInfo True "red_mushroom_block" [112, 17, 16, 255] )
    ,( 101, \_     -> BlockInfo True "iron_bars" [114, 114, 114, 255] )
    ,( 102, \_     -> BlockInfo False "glass_pane" [0, 0, 0, 255] )
    ,( 103, \_     -> BlockInfo True "melon_block" [164, 164, 38, 255] )
    ,( 104, \_     -> BlockInfo False "pumpkin_stem" [0, 0, 0, 255] )
    ,( 105, \_     -> BlockInfo False "melon_stem" [0, 0, 0, 255] )
    ,( 106, \_     -> BlockInfo False "vine" [0, 0, 0, 255] )
    ,( 107, \_     -> BlockInfo True "fence_gate" [196, 150, 97, 255] )
    ,( 108, \_     -> BlockInfo True "brick_stairs" [116, 67, 53, 255] )
    ,( 109, \_     -> BlockInfo True "stone_brick_stairs" [75, 75, 75, 255] )
    ,( 110, \_     -> BlockInfo True "mycelium" [107, 97, 104, 255] )
    ,( 111, \_     -> BlockInfo True "waterlily" [26, 112, 40, 255] )
    ,( 112, \_     -> BlockInfo True "nether_brick" [41, 21, 25, 255] )
    ,( 113, \_     -> BlockInfo True "nether_brick_fence" [41, 21, 25, 255] )
    ,( 114, \_     -> BlockInfo True "nether_brick_stairs" [41, 21, 25, 255] )
    ,( 115, \_     -> BlockInfo True "nether_wart" [130, 0, 0, 255] )
    ,( 116, \_     -> BlockInfo True "enchanting_table" [161, 212, 203, 255] )
    ,( 117, \_     -> BlockInfo True "brewing_stand" [85, 85, 85, 255] )
    ,( 118, \_     -> BlockInfo True "cauldron" [52, 52, 52, 255] )
    ,( 119, \_     -> BlockInfo True "end_portal" [11, 10, 11, 255] )
    ,( 120, \_     -> BlockInfo True "end_portal_frame" [198, 198, 146, 255] )
    ,( 121, \_     -> BlockInfo True "end_stone" [202, 203, 145, 255] )
    ,( 122, \_     -> BlockInfo True "dragon_egg" [24, 1, 28, 255] )
    ,( 123, \_     -> BlockInfo True "redstone_lamp" [144, 96, 65, 255] )
    ,( 124, \_     -> BlockInfo True "lit_redstone_lamp" [197, 171, 130, 255] )
    ,( 125, \_     -> BlockInfo True "double_wooden_slab" [186, 150, 97, 255] )
    ,( 126, \_     -> BlockInfo True "wooden_slab" [186, 150, 97, 255] )
    ,( 127, \_     -> BlockInfo True "cocoa" [178, 116, 59, 255] )
    ,( 128, \_     -> BlockInfo True "sandstone_stairs" [141, 136, 107, 255] )
    ,( 129, \_     -> BlockInfo True "emerald_ore" [47, 130, 78, 255] )
    ,( 130, \_     -> BlockInfo True "ender_chest" [2, 10, 14, 255] )
    ,( 131, \_     -> BlockInfo False "tripwire_hook" [0, 0, 0, 255] )
    ,( 132, \_     -> BlockInfo False "tripwire" [0, 0, 0, 255] )
    ,( 133, \_     -> BlockInfo True "emerald_block" [43, 134, 67, 255] )
    ,( 134, \_     -> BlockInfo True "spruce_stairs" [119, 87, 53, 255] )
    ,( 135, \_     -> BlockInfo True "birch_stairs" [212, 201, 139, 255] )
    ,( 136, \_     -> BlockInfo True "jungle_stairs" [113, 79, 52, 255] )
    ,( 137, \_     -> BlockInfo True "command_block" [190, 152, 126, 255] )
    ,( 138, \_     -> BlockInfo True "beacon" [91, 207, 200, 255] )
    ,( 139, \_     -> BlockInfo True "cobblestone_wall" [135, 135, 135, 255] )
    ,( 140, \_     -> BlockInfo True "flower_pot" [75, 42, 32, 255] )
    ,( 141, \_     -> BlockInfo True "carrots" [1, 157, 0, 255] )
    ,( 142, \_     -> BlockInfo True "potatoes" [0, 224, 26, 255] )
    ,( 143, \_     -> BlockInfo False "wooden_button" [0, 0, 0, 255] )
    ,( 144, \_     -> BlockInfo True "skull" [142, 142, 142, 255] )
    ,( 145, \_     -> BlockInfo True "anvil" [57, 54, 54, 255] )
    ,( 146, \_     -> BlockInfo True "trapped_chest" [148, 101, 28, 255] )
    ,( 147, \_     -> BlockInfo True "light_weighted_pressure_plate" [237, 233, 95, 255] )
    ,( 148, \_     -> BlockInfo True "heavy_weighted_pressure_plate" [144, 144, 144, 255] )
    ,( 149, \_     -> BlockInfo True "unpowered_comparator" [80, 80, 80, 255] )
    ,( 150, \_     -> BlockInfo True "powered_comparator" [80, 80, 80, 255] )
    ,( 151, \_     -> BlockInfo True "daylight_detector" [165, 147, 124, 255] )
    ,( 152, \_     -> BlockInfo True "redstone_block" [114, 4, 6, 255] )
    ,( 153, \_     -> BlockInfo True "quartz_ore" [145, 107, 99, 255] )
    ,( 154, \_     -> BlockInfo True "hopper" [47, 47, 47, 255] )
    ,( 155, \_     -> BlockInfo True "quartz_block" [148, 147, 143, 255] )
    ,( 156, \_     -> BlockInfo True "quartz_stairs" [148, 147, 143, 255] )
    ,( 157, \_     -> BlockInfo False "activator_rail" [0, 0, 0, 255] )
    ,( 158, \_     -> BlockInfo True "dropper" [80, 80, 80, 255] )
    ,( 159, \i     -> case i of
                    0 -> BlockInfo True "white_hardened_clay" [204, 171, 156, 255]
                    1 -> BlockInfo True "orange_hardened_clay" [156, 79, 33, 255]
                    2 -> BlockInfo True "magenta_hardened_clay" [146, 85, 106, 255]
                    3 -> BlockInfo True "light_blue_hardened_clay" [109, 105, 135, 255]
                    4 -> BlockInfo True "yellow_hardened_clay" [186, 133, 33, 255]
                    5 -> BlockInfo True "lime_hardened_clay" [100, 114, 48, 255]
                    6 -> BlockInfo True "pink_hardened_clay" [157, 72, 72, 255]
                    7 -> BlockInfo True "gray_hardened_clay" [56, 42, 34, 255]
                    8 -> BlockInfo True "light_gray_hardened_clay" [133, 105, 95, 255]
                    9 -> BlockInfo True "cyan_hardened_clay" [88, 91, 90, 255]
                    10 -> BlockInfo True "purple_hardened_clay" [113, 65, 82, 255]
                    11 -> BlockInfo True "blue_hardened_clay" [71, 56, 87, 255]
                    12 -> BlockInfo True "brown_hardened_clay" [74, 46, 31, 255]
                    13 -> BlockInfo True "green_hardened_clay" [72, 79, 38, 255]
                    14 -> BlockInfo True "red_hardened_clay" [146, 60, 47, 255]
                    15 -> BlockInfo True "black_hardened_clay" [33, 18, 13, 255]
                    _ -> bugBlock
    )
    ,( 160, \i     -> case i of
                    0 -> BlockInfo True "white_stained_glass_pane" [221, 221, 221, 255]
                    1 -> BlockInfo True "orange_stained_glass_pane" [219, 125, 62, 255]
                    2 -> BlockInfo True "magenta_stained_glass_pane" [179, 80, 188, 255]
                    3 -> BlockInfo True "light_blue_stained_glass_pane" [107, 138, 201, 255]
                    4 -> BlockInfo True "yellow_stained_glass_pane" [177, 166, 39, 255]
                    5 -> BlockInfo True "lime_stained_glass_pane" [65, 174, 56, 255]
                    6 -> BlockInfo True "pink_stained_glass_pane" [208, 132, 153, 255]
                    7 -> BlockInfo True "gray_stained_glass_pane" [64, 64, 64, 255]
                    8 -> BlockInfo True "light_gray_stained_glass_pane" [154, 161, 161, 255]
                    9 -> BlockInfo True "cyan_stained_glass_pane" [46, 110, 137, 255]
                    10 -> BlockInfo True "purple_stained_glass_pane" [126, 61, 181, 255]
                    11 -> BlockInfo True "blue_stained_glass_pane" [46, 56, 141, 255]
                    12 -> BlockInfo True "brown_stained_glass_pane" [79, 50, 31, 255]
                    13 -> BlockInfo True "green_stained_glass_pane" [53, 70, 27, 255]
                    14 -> BlockInfo True "red_stained_glass_pane" [150, 52, 48, 255]
                    15 -> BlockInfo True "black_stained_glass_pane" [25, 22, 22, 255]
                    _ -> bugBlock
    )
    ,( 161, \_     -> BlockInfo True "leaves2" [51, 126, 17, 255] )
    ,( 162, \i     -> case i .&. 0x03 of
                    0 -> BlockInfo True "acacia_wood" [185, 102, 54, 255]
                    1 -> BlockInfo True "dark_oak_wood" [55, 33, 8, 255]
                    _ -> bugBlock
    )
    ,( 163, \_     -> BlockInfo True "acacia_stairs" [185, 102, 54, 255] )
    ,( 164, \_     -> BlockInfo True "dark_oak_stairs" [55, 33, 8, 255] )
    ,( 165, \_     -> BlockInfo True "slime" [138, 225, 117, 255] )
    ,( 166, \_     -> BlockInfo True "barrier" [219, 0, 0, 255] )
    ,( 167, \_     -> BlockInfo True "iron_trapdoor" [187, 187, 187, 255] )
    ,( 168, \_     -> BlockInfo True "prismarine" [84, 145, 118, 255] )
    ,( 169, \_     -> BlockInfo True "sea_lantern" [198, 213, 207, 255] )
    ,( 170, \_     -> BlockInfo True "hay_block" [139, 115, 14, 255] )
    ,( 171, \i     -> case i of
                    0 -> BlockInfo True "white_carpet" [221, 221, 221, 255]
                    1 -> BlockInfo True "orange_carpet" [219, 125, 62, 255]
                    2 -> BlockInfo True "magenta_carpet" [179, 80, 188, 255]
                    3 -> BlockInfo True "light_blue_carpet" [107, 138, 201, 255]
                    4 -> BlockInfo True "yellow_carpet" [177, 166, 39, 255]
                    5 -> BlockInfo True "lime_carpet" [65, 174, 56, 255]
                    6 -> BlockInfo True "pink_carpet" [208, 132, 153, 255]
                    7 -> BlockInfo True "gray_carpet" [64, 64, 64, 255]
                    8 -> BlockInfo True "light_gray_carpet" [154, 161, 161, 255]
                    9 -> BlockInfo True "cyan_carpet" [46, 110, 137, 255]
                    10 -> BlockInfo True "purple_carpet" [126, 61, 181, 255]
                    11 -> BlockInfo True "blue_carpet" [46, 56, 141, 255]
                    12 -> BlockInfo True "brown_carpet" [79, 50, 31, 255]
                    13 -> BlockInfo True "green_carpet" [53, 70, 27, 255]
                    14 -> BlockInfo True "red_carpet" [150, 52, 48, 255]
                    15 -> BlockInfo True "black_carpet" [25, 22, 22, 255]
                    _ -> bugBlock
    )
    ,( 172, \_     -> BlockInfo True "hardened_clay" [139, 84, 60, 255] )
    ,( 173, \_     -> BlockInfo True "coal_block" [23, 23, 23, 255] )
    ,( 174, \_     -> BlockInfo True "packed_ice" [157, 186, 236, 255] )
    ,( 175, \_     -> BlockInfo False "double_plant" [0, 0, 0, 255] )
    ,( 176, \_     -> BlockInfo False "standing_banner" [0, 0, 0, 255] )
    ,( 177, \_     -> BlockInfo False "wall_banner" [0, 0, 0, 255] )
    ,( 178, \_     -> BlockInfo True "daylight_detector_inverted" [165, 147, 124, 255] )
    ,( 179, \_     -> BlockInfo True "red_sandstone" [157, 80, 28, 255] )
    ,( 180, \_     -> BlockInfo True "red_sandstone_stairs" [157, 80, 28, 255] )
    ,( 181, \_     -> BlockInfo True "double_stone_slab2" [157, 80, 28, 255] )
    ,( 182, \_     -> BlockInfo True "stone_slab2" [157, 80, 28, 255] )
    ,( 183, \_     -> BlockInfo True "spruce_fence_gate" [119, 87, 53, 255] )
    ,( 184, \_     -> BlockInfo True "birch_fence_gate" [212, 201, 139, 255] )
    ,( 185, \_     -> BlockInfo True "jungle_fence_gate" [113, 79, 52, 255] )
    ,( 186, \_     -> BlockInfo True "dark_oak_fence_gate" [55, 33, 8, 255] )
    ,( 187, \_     -> BlockInfo True "acacia_fence_gate" [185, 102, 54, 255] )
    ,( 188, \_     -> BlockInfo True "spruce_fence" [119, 87, 53, 255] )
    ,( 189, \_     -> BlockInfo True "birch_fence" [212, 201, 139, 255] )
    ,( 190, \_     -> BlockInfo True "jungle_fence" [113, 79, 52, 255] )
    ,( 191, \_     -> BlockInfo True "dark_oak_fence" [55, 33, 8, 255] )
    ,( 192, \_     -> BlockInfo True "acacia_fence" [185, 102, 54, 255] )
    ,( 193, \_     -> BlockInfo True "spruce_door" [119, 87, 53, 255] )
    ,( 194, \_     -> BlockInfo False "birch_door" [0, 0, 0, 255] )
    ,( 195, \_     -> BlockInfo False "jungle_door" [0, 0, 0, 255] )
    ,( 196, \_     -> BlockInfo False "acacia_door" [0, 0, 0, 255] )
    ,( 197, \_     -> BlockInfo False "dark_oak_door" [0, 0, 0, 255] )
    ,( 198, \_     -> BlockInfo False "end_rod" [0, 0, 0, 255] )
    ,( 199, \_     -> BlockInfo True "chorus_plant" [116, 86, 116, 255] )
    ,( 200, \_     -> BlockInfo True "chorus_flower" [144, 113, 144, 255] )
    ,( 201, \_     -> BlockInfo True "purpur_block" [103, 74, 103, 255] )
    ,( 202, \_     -> BlockInfo True "purpur_pillar" [103, 74, 103, 255] )
    ,( 203, \_     -> BlockInfo True "purpur_stairs" [103, 74, 103, 255] )
    ,( 204, \_     -> BlockInfo True "purpur_double_slab" [103, 74, 103, 255] )
    ,( 205, \_     -> BlockInfo True "purpur_slab" [103, 74, 103, 255] )
    ,( 206, \_     -> BlockInfo True "end_bricks" [202, 203, 145, 255] )
    ,( 207, \_     -> BlockInfo True "beetroots" [38, 154, 60, 255] )
    ,( 208, \_     -> BlockInfo True "grass_path" [142, 116, 63, 255] )
    ,( 209, \_     -> BlockInfo True "end_gateway" [17, 15, 25, 255] )
    ,( 210, \_     -> BlockInfo True "repeating_command_block" [137, 129, 163, 255] )
    ,( 211, \_     -> BlockInfo True "chain_command_block" [154, 125, 131, 255] )
    ,( 212, \_     -> BlockInfo True "frosted_ice" [165, 195, 245, 255] )
    ,( 213, \_     -> BlockInfo True "magma" [141, 61, 20, 255] )
    ,( 214, \_     -> BlockInfo True "nether_wart_block" [92, 3, 3, 255] )
    ,( 215, \_     -> BlockInfo True "red_nether_brick" [68, 0, 0, 255] )
    ,( 216, \_     -> BlockInfo True "bone_block" [175, 170, 146, 255] )
    ,( 217, \_     -> BlockInfo True "structure_void" [18, 94, 100, 255] )
    ,( 218, \_     -> BlockInfo True "observer" [80, 80, 80, 255] )
    ,( 219, \_     -> BlockInfo True "white_shulker_box" [210, 207, 207, 255] )
    ,( 220, \_     -> BlockInfo True "orange_shulker_box" [196, 111, 54, 255] )
    ,( 221, \_     -> BlockInfo True "magenta_shulker_box" [177, 95, 184, 255] )
    ,( 222, \_     -> BlockInfo True "light_blue_shulker_box" [95, 133, 192, 255] )
    ,( 223, \_     -> BlockInfo True "yellow_shulker_box" [182, 173, 58, 255] )
    ,( 224, \_     -> BlockInfo True "lime_shulker_box" [68, 174, 57, 255] )
    ,( 225, \_     -> BlockInfo True "pink_shulker_box" [198, 135, 155, 255] )
    ,( 226, \_     -> BlockInfo True "gray_shulker_box" [78, 77, 77, 255] )
    ,( 227, \_     -> BlockInfo True "light_gray_shulker_box" [154, 152, 152, 255] )
    ,( 228, \_     -> BlockInfo True "cyan_shulker_box" [65, 130, 156, 255] )
    ,( 229, \_     -> BlockInfo True "purple_shulker_box" [143, 97, 143, 255] )
    ,( 230, \_     -> BlockInfo True "blue_shulker_box" [95, 107, 190, 255] )
    ,( 231, \_     -> BlockInfo True "brown_shulker_box" [134, 107, 89, 255] )
    ,( 232, \_     -> BlockInfo True "green_shulker_box" [105, 123, 79, 255] )
    ,( 233, \_     -> BlockInfo True "red_shulker_box" [184, 84, 81, 255] )
    ,( 234, \_     -> BlockInfo True "black_shulker_box" [54, 52, 52, 255] )
    ,( 235, \_     -> BlockInfo False "unkown" [0, 0, 0, 255] )
    ,( 236, \_     -> BlockInfo False "unkown" [0, 0, 0, 255] )
    ,( 237, \_     -> BlockInfo False "unkown" [0, 0, 0, 255] )
    ,( 238, \_     -> BlockInfo False "unkown" [0, 0, 0, 255] )
    ,( 239, \_     -> BlockInfo False "unkown" [0, 0, 0, 255] )
    ,( 240, \_     -> BlockInfo False "unkown" [0, 0, 0, 255] )
    ,( 241, \_     -> BlockInfo False "unkown" [0, 0, 0, 255] )
    ,( 242, \_     -> BlockInfo False "unkown" [0, 0, 0, 255] )
    ,( 243, \_     -> BlockInfo False "unkown" [0, 0, 0, 255] )
    ,( 244, \_     -> BlockInfo False "unkown" [0, 0, 0, 255] )
    ,( 245, \_     -> BlockInfo False "unkown" [0, 0, 0, 255] )
    ,( 246, \_     -> BlockInfo False "unkown" [0, 0, 0, 255] )
    ,( 247, \_     -> BlockInfo False "unkown" [0, 0, 0, 255] )
    ,( 248, \_     -> BlockInfo False "unkown" [0, 0, 0, 255] )
    ,( 249, \_     -> BlockInfo False "unkown" [0, 0, 0, 255] )
    ,( 250, \_     -> BlockInfo False "unkown" [0, 0, 0, 255] )
    ,( 251, \_     -> BlockInfo False "unkown" [0, 0, 0, 255] )
    ,( 252, \_     -> BlockInfo False "unkown" [0, 0, 0, 255] )
    ,( 253, \_     -> BlockInfo False "unkown" [0, 0, 0, 255] )
    ,( 254, \_     -> BlockInfo False "unkown" [0, 0, 0, 255] )
    ,( 255, \_     -> BlockInfo True "structure_block" [0, 0, 0, 255] )
    ]
