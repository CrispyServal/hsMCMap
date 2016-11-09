module Game.Minecraft.Map.Block where
import qualified Data.IntMap as I
import Data.Bits
import Data.Word
import Codec.Picture

data BlockInfo = BlockInfo {    toDrawBlock :: Bool
                            ,   nameBlock :: String
                            ,   colorBlock :: PixelRGB8
                            } deriving Show

ifDraw :: (Word8,Word8) -> Bool
ifDraw (id, add) = toDrawBlock $ (blocks I.! (fromIntegral id) ) add

blocks :: I.IntMap (Word8 -> BlockInfo)
blocks = I.fromList
    [( 0, \_	 -> BlockInfo False "air" (PixelRGB8 0 0 0) )
	,( 1, \_	 -> BlockInfo True "stone" (PixelRGB8 80 80 80) )
	,( 2, \_	 -> BlockInfo True "grass" (PixelRGB8 76 104 38) )
	,( 3, \_	 -> BlockInfo True "dirt" (PixelRGB8 118 82 55) )
	,( 4, \_	 -> BlockInfo True "cobblestone" (PixelRGB8 135 135 135) )
	,( 5, \i	 -> case i of
                    0 -> BlockInfo True "oak_wood_planks" (PixelRGB8 186 150 97)
                    1 -> BlockInfo True "spruce_wood_planks" (PixelRGB8 119 87 53)
                    2 -> BlockInfo True "birch_wood_planks" (PixelRGB8 212 201 139)
                    3 -> BlockInfo True "jungle_wood_planks" (PixelRGB8 113 79 52)
                    4 -> BlockInfo True "acacia_wood_planks" (PixelRGB8 185 102 54)
                    5 -> BlockInfo True "dark_oak_wood_planks" (PixelRGB8 55 33 8)
    )
	,( 6, \_	 -> BlockInfo False "sapling" (PixelRGB8 0 0 0) )
	,( 7, \_	 -> BlockInfo True "bedrock" (PixelRGB8 39 39 39) )
	,( 8, \_	 -> BlockInfo True "flowing_water" (PixelRGB8 105 113 188) )
	,( 9, \_	 -> BlockInfo True "water" (PixelRGB8 105 113 188) )
	,( 10, \_	 -> BlockInfo True "flowing_lava" (PixelRGB8 192 65 8) )
	,( 11, \_	 -> BlockInfo True "lava" (PixelRGB8 192 65 8) )
	,( 12, \_	 -> BlockInfo True "sand" (PixelRGB8 209 202 151) )
	,( 13, \_	 -> BlockInfo True "gravel" (PixelRGB8 121 117 115) )
	,( 14, \_	 -> BlockInfo True "gold_ore" (PixelRGB8 150 133 102) )
	,( 15, \_	 -> BlockInfo True "iron_ore" (PixelRGB8 159 137 121) )
	,( 16, \_	 -> BlockInfo True "coal_ore" (PixelRGB8 54 54 54) )
	,( 17, \i	 -> case ( i .&. 0x03) of
                    0 -> BlockInfo True "oak_wood" (PixelRGB8 186 150 97)
                    1 -> BlockInfo True "spruce_wood" (PixelRGB8 119 87 53)
                    2 -> BlockInfo True "birch_wood" (PixelRGB8 113 79 52)
                    3 -> BlockInfo True "jungle_wood" (PixelRGB8 55 33 8)
    )
	,( 18, \_	 -> BlockInfo True "leaves" (PixelRGB8 51 126 17) )
	,( 19, \_	 -> BlockInfo True "sponge" (PixelRGB8 188 188 85) )
	,( 20, \_	 -> BlockInfo False "glass" (PixelRGB8 0 0 0) )
	,( 21, \_	 -> BlockInfo True "lapis_ore" (PixelRGB8 114 119 130) )
	,( 22, \_	 -> BlockInfo True "lapis_block" (PixelRGB8 35 61 127) )
	,( 23, \_	 -> BlockInfo True "dispenser" (PixelRGB8 80 80 80) )
	,( 24, \_	 -> BlockInfo True "sandstone" (PixelRGB8 141 136 107) )
	,( 25, \_	 -> BlockInfo True "noteblock" (PixelRGB8 120 75 53) )
	,( 26, \_	 -> BlockInfo True "bed" (PixelRGB8 131 19 20) )
	,( 27, \_	 -> BlockInfo False "golden_rail" (PixelRGB8 0 0 0) )
	,( 28, \_	 -> BlockInfo False "detector_rail" (PixelRGB8 0 0 0) )
	,( 29, \_	 -> BlockInfo True "sticky_piston" (PixelRGB8 80 80 80) )
	,( 30, \_	 -> BlockInfo True "web" (PixelRGB8 212 212 212) )
	,( 31, \_	 -> BlockInfo False "tallgrass" (PixelRGB8 0 0 0) )
	,( 32, \_	 -> BlockInfo False "deadbush" (PixelRGB8 0 0 0) )
	,( 33, \_	 -> BlockInfo True "piston" (PixelRGB8 80 80 80) )
	,( 34, \_	 -> BlockInfo True "piston_head" (PixelRGB8 186 150 97) )
	,( 35, \i	 -> case i of
                    0 -> BlockInfo True "white_wool" (PixelRGB8 221 221 221)
                    1 -> BlockInfo True "orange_wool" (PixelRGB8 219 125 62)
                    2 -> BlockInfo True "magenta_wool" (PixelRGB8 179 80 188)
                    3 -> BlockInfo True "light_blue_wool" (PixelRGB8 107 138 201)
                    4 -> BlockInfo True "yellow_wool" (PixelRGB8 177 166 39)
                    5 -> BlockInfo True "lime_wool" (PixelRGB8 65 174 56)
                    6 -> BlockInfo True "pink_wool" (PixelRGB8 208 132 153)
                    7 -> BlockInfo True "gray_wool" (PixelRGB8 64 64 64)
                    8 -> BlockInfo True "light_gray_wool" (PixelRGB8 154 161 161)
                    9 -> BlockInfo True "cyan_wool" (PixelRGB8 46 110 137)
                    10 -> BlockInfo True "purple_wool" (PixelRGB8 126 61 181)
                    11 -> BlockInfo True "blue_wool" (PixelRGB8 46 56 141)
                    12 -> BlockInfo True "brown_wool" (PixelRGB8 79 50 31)
                    13 -> BlockInfo True "green_wool" (PixelRGB8 53 70 27)
                    14 -> BlockInfo True "red_wool" (PixelRGB8 150 52 48)
                    15 -> BlockInfo True "black_wool" (PixelRGB8 25 22 22)
    )
    ,( 36, \_	 -> BlockInfo False "piston_extension" (PixelRGB8 0 0 0) )
	,( 37, \_	 -> BlockInfo False "yellow_flower" (PixelRGB8 0 0 0) )
	,( 38, \_	 -> BlockInfo False "red_flower" (PixelRGB8 0 0 0) )
	,( 39, \_	 -> BlockInfo False "brown_mushroom" (PixelRGB8 0 0 0) )
	,( 40, \_	 -> BlockInfo False "red_mushroom" (PixelRGB8 0 0 0) )
	,( 41, \_	 -> BlockInfo True "gold_block" (PixelRGB8 237 233 95) )
	,( 42, \_	 -> BlockInfo True "iron_block" (PixelRGB8 144 144 144) )
	,( 43, \_	 -> BlockInfo True "double_stone_slab" (PixelRGB8 160 160 160) )
	,( 44, \_	 -> BlockInfo True "stone_slab" (PixelRGB8 160 160 160) )
	,( 45, \_	 -> BlockInfo True "brick_block" (PixelRGB8 116 67 53) )
	,( 46, \_	 -> BlockInfo True "tnt" (PixelRGB8 82 25 10) )
	,( 47, \_	 -> BlockInfo True "bookshelf" (PixelRGB8 186 150 97) )
	,( 48, \_	 -> BlockInfo True "mossy_cobblestone" (PixelRGB8 76 96 76) )
	,( 49, \_	 -> BlockInfo True "obsidian" (PixelRGB8 26 21 38) )
	,( 50, \_	 -> BlockInfo False "torch" (PixelRGB8 0 0 0) )
	,( 51, \_	 -> BlockInfo False "fire" (PixelRGB8 0 0 0) )
	,( 52, \_	 -> BlockInfo True "mob_spawner" (PixelRGB8 26 39 47) )
	,( 53, \_	 -> BlockInfo True "oak_stairs" (PixelRGB8 186 150 97) )
	,( 54, \_	 -> BlockInfo True "chest" (PixelRGB8 148 101 28) )
	,( 55, \_	 -> BlockInfo False "redstone_wire" (PixelRGB8 0 0 0) )
	,( 56, \_	 -> BlockInfo True "diamond_ore" (PixelRGB8 143 166 175) )
	,( 57, \_	 -> BlockInfo True "diamond_block" (PixelRGB8 74 203 197) )
	,( 58, \_	 -> BlockInfo True "crafting_table" (PixelRGB8 186 150 97) )
	,( 59, \_	 -> BlockInfo True "wheat" (PixelRGB8 124 144 11) )
	,( 60, \_	 -> BlockInfo True "farmland" (PixelRGB8 118 82 55) )
	,( 61, \_	 -> BlockInfo True "furnace" (PixelRGB8 80 80 80) )
	,( 62, \_	 -> BlockInfo True "lit_furnace" (PixelRGB8 80 80 80) )
	,( 63, \_	 -> BlockInfo False "standing_sign" (PixelRGB8 0 0 0) )
	,( 64, \_	 -> BlockInfo False "wooden_door" (PixelRGB8 0 0 0) )
	,( 65, \_	 -> BlockInfo False "ladder" (PixelRGB8 0 0 0) )
	,( 66, \_	 -> BlockInfo False "rail" (PixelRGB8 0 0 0) )
	,( 67, \_	 -> BlockInfo True "stone_stairs" (PixelRGB8 80 80 80) )
	,( 68, \_	 -> BlockInfo False "wall_sign" (PixelRGB8 0 0 0) )
	,( 69, \_	 -> BlockInfo False "lever" (PixelRGB8 0 0 0) )
	,( 70, \_	 -> BlockInfo True "stone_pressure_plate" (PixelRGB8 80 80 80) )
	,( 71, \_	 -> BlockInfo False "iron_door" (PixelRGB8 0 0 0) )
	,( 72, \_	 -> BlockInfo True "wooden_pressure_plate" (PixelRGB8 186 150 97) )
	,( 73, \_	 -> BlockInfo True "redstone_ore" (PixelRGB8 136 55 55) )
	,( 74, \_	 -> BlockInfo False "lit_redstone_ore" (PixelRGB8 136 55 55) )
	,( 75, \_	 -> BlockInfo False "unlit_redstone_torch" (PixelRGB8 0 0 0) )
	,( 76, \_	 -> BlockInfo False "redstone_torch" (PixelRGB8 0 0 0) )
	,( 77, \_	 -> BlockInfo False "stone_button" (PixelRGB8 0 0 0) )
	,( 78, \_	 -> BlockInfo True "snow_layer" (PixelRGB8 230 239 239) )
	,( 79, \_	 -> BlockInfo True "ice" (PixelRGB8 165 195 245) )
	,( 80, \_	 -> BlockInfo True "snow" (PixelRGB8 230 239 239) )
	,( 81, \_	 -> BlockInfo True "cactus" (PixelRGB8 15 114 27) )
	,( 82, \_	 -> BlockInfo True "clay" (PixelRGB8 100 104 112) )
	,( 83, \_	 -> BlockInfo True "reeds" (PixelRGB8 77 157 33) )
	,( 84, \_	 -> BlockInfo True "jukebox" (PixelRGB8 118 77 54) )
	,( 85, \_	 -> BlockInfo True "fence" (PixelRGB8 186 150 97) )
	,( 86, \_	 -> BlockInfo True "pumpkin" (PixelRGB8 179 108 17) )
	,( 87, \_	 -> BlockInfo True "netherrack" (PixelRGB8 84 51 43) )
	,( 88, \_	 -> BlockInfo True "soul_sand" (PixelRGB8 44 33 26) )
	,( 89, \_	 -> BlockInfo True "glowstone" (PixelRGB8 219 180 124) )
	,( 90, \_	 -> BlockInfo True "portal" (PixelRGB8 59 37 100) )
	,( 91, \_	 -> BlockInfo True "lit_pumpkin" (PixelRGB8 189 114 19) )
	,( 92, \_	 -> BlockInfo True "cake" (PixelRGB8 216 216 217) )
	,( 93, \_	 -> BlockInfo True "unpowered_repeater" (PixelRGB8 80 80 80) )
	,( 94, \_	 -> BlockInfo True "powered_repeater" (PixelRGB8 80 80 80) )
	,( 95, \i	 -> case i of
                    0 -> BlockInfo True "white_stained_glass" (PixelRGB8 221 221 221)
                    1 -> BlockInfo True "orange_stained_glass" (PixelRGB8 219 125 62)
                    2 -> BlockInfo True "magenta_stained_glass" (PixelRGB8 179 80 188)
                    3 -> BlockInfo True "light_blue_stained_glass" (PixelRGB8 107 138 201)
                    4 -> BlockInfo True "yellow_stained_glass" (PixelRGB8 177 166 39)
                    5 -> BlockInfo True "lime_stained_glass" (PixelRGB8 65 174 56)
                    6 -> BlockInfo True "pink_stained_glass" (PixelRGB8 208 132 153)
                    7 -> BlockInfo True "gray_stained_glass" (PixelRGB8 64 64 64)
                    8 -> BlockInfo True "light_gray_stained_glass" (PixelRGB8 154 161 161)
                    9 -> BlockInfo True "cyan_stained_glass" (PixelRGB8 46 110 137)
                    10 -> BlockInfo True "purple_stained_glass" (PixelRGB8 126 61 181)
                    11 -> BlockInfo True "blue_stained_glass" (PixelRGB8 46 56 141)
                    12 -> BlockInfo True "brown_stained_glass" (PixelRGB8 79 50 31)
                    13 -> BlockInfo True "green_stained_glass" (PixelRGB8 53 70 27)
                    14 -> BlockInfo True "red_stained_glass" (PixelRGB8 150 52 48)
                    15 -> BlockInfo True "black_stained_glass" (PixelRGB8 25 22 22)
    )
    ,( 96, \_	 -> BlockInfo True "trapdoor" (PixelRGB8 186 150 97) )
	,( 97, \_	 -> BlockInfo True "monster_egg" (PixelRGB8 80 80 80) )
	,( 98, \_	 -> BlockInfo True "stonebrick" (PixelRGB8 75 75 75) )
	,( 99, \_	 -> BlockInfo True "brown_mushroom_block" (PixelRGB8 88 66 51) )
	,( 100, \_	 -> BlockInfo True "red_mushroom_block" (PixelRGB8 112 17 16) )
	,( 101, \_	 -> BlockInfo True "iron_bars" (PixelRGB8 114 114 114) )
	,( 102, \_	 -> BlockInfo False "glass_pane" (PixelRGB8 0 0 0) )
	,( 103, \_	 -> BlockInfo True "melon_block" (PixelRGB8 164 164 38) )
	,( 104, \_	 -> BlockInfo False "pumpkin_stem" (PixelRGB8 0 0 0) )
	,( 105, \_	 -> BlockInfo False "melon_stem" (PixelRGB8 0 0 0) )
	,( 106, \_	 -> BlockInfo False "vine" (PixelRGB8 0 0 0) )
	,( 107, \_	 -> BlockInfo True "fence_gate" (PixelRGB8 196 150 97) )
	,( 108, \_	 -> BlockInfo True "brick_stairs" (PixelRGB8 116 67 53) )
	,( 109, \_	 -> BlockInfo True "stone_brick_stairs" (PixelRGB8 75 75 75) )
	,( 110, \_	 -> BlockInfo True "mycelium" (PixelRGB8 107 97 104) )
	,( 111, \_	 -> BlockInfo True "waterlily" (PixelRGB8 26 112 40) )
	,( 112, \_	 -> BlockInfo True "nether_brick" (PixelRGB8 41 21 25) )
	,( 113, \_	 -> BlockInfo True "nether_brick_fence" (PixelRGB8 41 21 25) )
	,( 114, \_	 -> BlockInfo True "nether_brick_stairs" (PixelRGB8 41 21 25) )
	,( 115, \_	 -> BlockInfo True "nether_wart" (PixelRGB8 130 0 0) )
	,( 116, \_	 -> BlockInfo True "enchanting_table" (PixelRGB8 161 212 203) )
	,( 117, \_	 -> BlockInfo True "brewing_stand" (PixelRGB8 85 85 85) )
	,( 118, \_	 -> BlockInfo True "cauldron" (PixelRGB8 52 52 52) )
	,( 119, \_	 -> BlockInfo True "end_portal" (PixelRGB8 11 10 11) )
	,( 120, \_	 -> BlockInfo True "end_portal_frame" (PixelRGB8 198 198 146) )
	,( 121, \_	 -> BlockInfo True "end_stone" (PixelRGB8 202 203 145) )
	,( 122, \_	 -> BlockInfo True "dragon_egg" (PixelRGB8 24 1 28) )
	,( 123, \_	 -> BlockInfo True "redstone_lamp" (PixelRGB8 144 96 65) )
	,( 124, \_	 -> BlockInfo True "lit_redstone_lamp" (PixelRGB8 197 171 130) )
	,( 125, \_	 -> BlockInfo True "double_wooden_slab" (PixelRGB8 186 150 97) )
	,( 126, \_	 -> BlockInfo True "wooden_slab" (PixelRGB8 186 150 97) )
	,( 127, \_	 -> BlockInfo True "cocoa" (PixelRGB8 178 116 59) )
	,( 128, \_	 -> BlockInfo True "sandstone_stairs" (PixelRGB8 141 136 107) )
	,( 129, \_	 -> BlockInfo True "emerald_ore" (PixelRGB8 47 130 78) )
	,( 130, \_	 -> BlockInfo True "ender_chest" (PixelRGB8 2 10 14) )
	,( 131, \_	 -> BlockInfo False "tripwire_hook" (PixelRGB8 0 0 0) )
	,( 132, \_	 -> BlockInfo False "tripwire" (PixelRGB8 0 0 0) )
	,( 133, \_	 -> BlockInfo True "emerald_block" (PixelRGB8 43 134 67) )
	,( 134, \_	 -> BlockInfo True "spruce_stairs" (PixelRGB8 119 87 53) )
	,( 135, \_	 -> BlockInfo True "birch_stairs" (PixelRGB8 212 201 139) )
	,( 136, \_	 -> BlockInfo True "jungle_stairs" (PixelRGB8 113 79 52) )
	,( 137, \_	 -> BlockInfo True "command_block" (PixelRGB8 190 152 126) )
	,( 138, \_	 -> BlockInfo True "beacon" (PixelRGB8 91 207 200) )
	,( 139, \_	 -> BlockInfo True "cobblestone_wall" (PixelRGB8 135 135 135) )
	,( 140, \_	 -> BlockInfo True "flower_pot" (PixelRGB8 75 42 32) )
	,( 141, \_	 -> BlockInfo True "carrots" (PixelRGB8 1 157 0) )
	,( 142, \_	 -> BlockInfo True "potatoes" (PixelRGB8 0 224 26) )
	,( 143, \_	 -> BlockInfo False "wooden_button" (PixelRGB8 0 0 0) )
	,( 144, \_	 -> BlockInfo True "skull" (PixelRGB8 142 142 142) )
	,( 145, \_	 -> BlockInfo True "anvil" (PixelRGB8 57 54 54) )
	,( 146, \_	 -> BlockInfo True "trapped_chest" (PixelRGB8 148 101 28) )
	,( 147, \_	 -> BlockInfo True "light_weighted_pressure_plate" (PixelRGB8 237 233 95) )
	,( 148, \_	 -> BlockInfo True "heavy_weighted_pressure_plate" (PixelRGB8 144 144 144) )
	,( 149, \_	 -> BlockInfo True "unpowered_comparator" (PixelRGB8 80 80 80) )
	,( 150, \_	 -> BlockInfo True "powered_comparator" (PixelRGB8 80 80 80) )
	,( 151, \_	 -> BlockInfo True "daylight_detector" (PixelRGB8 165 147 124) )
	,( 152, \_	 -> BlockInfo True "redstone_block" (PixelRGB8 114 4 6) )
	,( 153, \_	 -> BlockInfo True "quartz_ore" (PixelRGB8 145 107 99) )
	,( 154, \_	 -> BlockInfo True "hopper" (PixelRGB8 47 47 47) )
	,( 155, \_	 -> BlockInfo True "quartz_block" (PixelRGB8 148 147 143) )
	,( 156, \_	 -> BlockInfo True "quartz_stairs" (PixelRGB8 148 147 143) )
	,( 157, \_	 -> BlockInfo False "activator_rail" (PixelRGB8 0 0 0) )
	,( 158, \_	 -> BlockInfo True "dropper" (PixelRGB8 80 80 80) )
	,( 159, \i	 -> case i of
                    0 -> BlockInfo True "white_hardened_clay" (PixelRGB8 204 171 156)
                    1 -> BlockInfo True "orange_hardened_clay" (PixelRGB8 156 79 33)
                    2 -> BlockInfo True "magenta_hardened_clay" (PixelRGB8 146 85 106)
                    3 -> BlockInfo True "light_blue_hardened_clay" (PixelRGB8 109 105 135)
                    4 -> BlockInfo True "yellow_hardened_clay" (PixelRGB8 186 133 33)
                    5 -> BlockInfo True "lime_hardened_clay" (PixelRGB8 100 114 48)
                    6 -> BlockInfo True "pink_hardened_clay" (PixelRGB8 157 72 72)
                    7 -> BlockInfo True "gray_hardened_clay" (PixelRGB8 56 42 34)
                    8 -> BlockInfo True "light_gray_hardened_clay" (PixelRGB8 133 105 95)
                    9 -> BlockInfo True "cyan_hardened_clay" (PixelRGB8 88 91 90)
                    10 -> BlockInfo True "purple_hardened_clay" (PixelRGB8 113 65 82)
                    11 -> BlockInfo True "blue_hardened_clay" (PixelRGB8 71 56 87)
                    12 -> BlockInfo True "brown_hardened_clay" (PixelRGB8 74 46 31)
                    13 -> BlockInfo True "green_hardened_clay" (PixelRGB8 72 79 38)
                    14 -> BlockInfo True "red_hardened_clay" (PixelRGB8 146 60 47)
                    15 -> BlockInfo True "black_hardened_clay" (PixelRGB8 33 18 13)
    )
    --,( 160, \_	 -> BlockInfo True "stained_glass_pane" (PixelRGB8 0 0 0) )
	,( 160, \i	 -> case i of
                    0 -> BlockInfo True "white_stained_glass_pane" (PixelRGB8 221 221 221)
                    1 -> BlockInfo True "orange_stained_glass_pane" (PixelRGB8 219 125 62)
                    2 -> BlockInfo True "magenta_stained_glass_pane" (PixelRGB8 179 80 188)
                    3 -> BlockInfo True "light_blue_stained_glass_pane" (PixelRGB8 107 138 201)
                    4 -> BlockInfo True "yellow_stained_glass_pane" (PixelRGB8 177 166 39)
                    5 -> BlockInfo True "lime_stained_glass_pane" (PixelRGB8 65 174 56)
                    6 -> BlockInfo True "pink_stained_glass_pane" (PixelRGB8 208 132 153)
                    7 -> BlockInfo True "gray_stained_glass_pane" (PixelRGB8 64 64 64)
                    8 -> BlockInfo True "light_gray_stained_glass_pane" (PixelRGB8 154 161 161)
                    9 -> BlockInfo True "cyan_stained_glass_pane" (PixelRGB8 46 110 137)
                    10 -> BlockInfo True "purple_stained_glass_pane" (PixelRGB8 126 61 181)
                    11 -> BlockInfo True "blue_stained_glass_pane" (PixelRGB8 46 56 141)
                    12 -> BlockInfo True "brown_stained_glass_pane" (PixelRGB8 79 50 31)
                    13 -> BlockInfo True "green_stained_glass_pane" (PixelRGB8 53 70 27)
                    14 -> BlockInfo True "red_stained_glass_pane" (PixelRGB8 150 52 48)
                    15 -> BlockInfo True "black_stained_glass_pane" (PixelRGB8 25 22 22)
    )
	,( 161, \_	 -> BlockInfo True "leaves2" (PixelRGB8 51 126 17) )
	,( 162, \i	 -> case ( i .&. 0x03) of
                    0 -> BlockInfo True "acacia_wood" (PixelRGB8 185 102 54)
                    1 -> BlockInfo True "dark_oak_wood" (PixelRGB8 55 33 8)
    )
	,( 163, \_	 -> BlockInfo True "acacia_stairs" (PixelRGB8 185 102 54) )
	,( 164, \_	 -> BlockInfo True "dark_oak_stairs" (PixelRGB8 55 33 8) )
	,( 165, \_	 -> BlockInfo True "slime" (PixelRGB8 138 225 117) )
	,( 166, \_	 -> BlockInfo True "barrier" (PixelRGB8 219 0 0) )
	,( 167, \_	 -> BlockInfo True "iron_trapdoor" (PixelRGB8 187 187 187) )
	,( 168, \_	 -> BlockInfo True "prismarine" (PixelRGB8 84 145 118) )
	,( 169, \_	 -> BlockInfo True "sea_lantern" (PixelRGB8 198 213 207) )
	,( 170, \_	 -> BlockInfo True "hay_block" (PixelRGB8 139 115 14) )
	,( 171, \i	 -> case i of
                    0 -> BlockInfo True "white_carpet" (PixelRGB8 221 221 221)
                    1 -> BlockInfo True "orange_carpet" (PixelRGB8 219 125 62)
                    2 -> BlockInfo True "magenta_carpet" (PixelRGB8 179 80 188)
                    3 -> BlockInfo True "light_blue_carpet" (PixelRGB8 107 138 201)
                    4 -> BlockInfo True "yellow_carpet" (PixelRGB8 177 166 39)
                    5 -> BlockInfo True "lime_carpet" (PixelRGB8 65 174 56)
                    6 -> BlockInfo True "pink_carpet" (PixelRGB8 208 132 153)
                    7 -> BlockInfo True "gray_carpet" (PixelRGB8 64 64 64)
                    8 -> BlockInfo True "light_gray_carpet" (PixelRGB8 154 161 161)
                    9 -> BlockInfo True "cyan_carpet" (PixelRGB8 46 110 137)
                    10 -> BlockInfo True "purple_carpet" (PixelRGB8 126 61 181)
                    11 -> BlockInfo True "blue_carpet" (PixelRGB8 46 56 141)
                    12 -> BlockInfo True "brown_carpet" (PixelRGB8 79 50 31)
                    13 -> BlockInfo True "green_carpet" (PixelRGB8 53 70 27)
                    14 -> BlockInfo True "red_carpet" (PixelRGB8 150 52 48)
                    15 -> BlockInfo True "black_carpet" (PixelRGB8 25 22 22)
    )
	,( 172, \_	 -> BlockInfo True "hardened_clay" (PixelRGB8 139 84 60) )
	,( 173, \_	 -> BlockInfo True "coal_block" (PixelRGB8 23 23 23) )
	,( 174, \_	 -> BlockInfo True "packed_ice" (PixelRGB8 157 186 236) )
	,( 175, \_	 -> BlockInfo False "double_plant" (PixelRGB8 0 0 0) )
	,( 176, \_	 -> BlockInfo False "standing_banner" (PixelRGB8 0 0 0) )
	,( 177, \_	 -> BlockInfo False "wall_banner" (PixelRGB8 0 0 0) )
	,( 178, \_	 -> BlockInfo True "daylight_detector_inverted" (PixelRGB8 165 147 124) )
	,( 179, \_	 -> BlockInfo True "red_sandstone" (PixelRGB8 157 80 28) )
	,( 180, \_	 -> BlockInfo True "red_sandstone_stairs" (PixelRGB8 157 80 28) )
	,( 181, \_	 -> BlockInfo True "double_stone_slab2" (PixelRGB8 157 80 28) )
	,( 182, \_	 -> BlockInfo True "stone_slab2" (PixelRGB8 157 80 28) )
	,( 183, \_	 -> BlockInfo True "spruce_fence_gate" (PixelRGB8 119 87 53) )
	,( 184, \_	 -> BlockInfo True "birch_fence_gate" (PixelRGB8 212 201 139) )
	,( 185, \_	 -> BlockInfo True "jungle_fence_gate" (PixelRGB8 113 79 52) )
	,( 186, \_	 -> BlockInfo True "dark_oak_fence_gate" (PixelRGB8 55 33 8) )
	,( 187, \_	 -> BlockInfo True "acacia_fence_gate" (PixelRGB8 185 102 54) )
	,( 188, \_	 -> BlockInfo True "spruce_fence" (PixelRGB8 119 87 53) )
	,( 189, \_	 -> BlockInfo True "birch_fence" (PixelRGB8 212 201 139) )
	,( 190, \_	 -> BlockInfo True "jungle_fence" (PixelRGB8 113 79 52) )
	,( 191, \_	 -> BlockInfo True "dark_oak_fence" (PixelRGB8 55 33 8) )
	,( 192, \_	 -> BlockInfo True "acacia_fence" (PixelRGB8 185 102 54) )
	,( 193, \_	 -> BlockInfo True "spruce_door" (PixelRGB8 119 87 53) )
	,( 194, \_	 -> BlockInfo False "birch_door" (PixelRGB8 0 0 0) )
	,( 195, \_	 -> BlockInfo False "jungle_door" (PixelRGB8 0 0 0) )
	,( 196, \_	 -> BlockInfo False "acacia_door" (PixelRGB8 0 0 0) )
	,( 197, \_	 -> BlockInfo False "dark_oak_door" (PixelRGB8 0 0 0) )
	,( 198, \_	 -> BlockInfo False "end_rod" (PixelRGB8 0 0 0) )
	,( 199, \_	 -> BlockInfo True "chorus_plant" (PixelRGB8 116 86 116) )
	,( 200, \_	 -> BlockInfo True "chorus_flower" (PixelRGB8 144 113 144) )
	,( 201, \_	 -> BlockInfo True "purpur_block" (PixelRGB8 103 74 103) )
	,( 202, \_	 -> BlockInfo True "purpur_pillar" (PixelRGB8 103 74 103) )
	,( 203, \_	 -> BlockInfo True "purpur_stairs" (PixelRGB8 103 74 103) )
	,( 204, \_	 -> BlockInfo True "purpur_double_slab" (PixelRGB8 103 74 103) )
	,( 205, \_	 -> BlockInfo True "purpur_slab" (PixelRGB8 103 74 103) )
	,( 206, \_	 -> BlockInfo True "end_bricks" (PixelRGB8 202 203 145) )
	,( 207, \_	 -> BlockInfo True "beetroots" (PixelRGB8 38 154 60) )
    ,( 208, \_	 -> BlockInfo True "grass_path" (PixelRGB8 142 116 63) )
    ,( 209, \_	 -> BlockInfo True "end_gateway" (PixelRGB8 17 15 25) )
    ,( 210, \_	 -> BlockInfo True "repeating_command_block" (PixelRGB8 137 129 163) )
    ,( 211, \_	 -> BlockInfo True "chain_command_block" (PixelRGB8 154 125 131) )
    ,( 212, \_	 -> BlockInfo True "frosted_ice" (PixelRGB8 165 195 245) )
    ,( 213, \_	 -> BlockInfo True "magma" (PixelRGB8 141 61 20) )
    ,( 214, \_	 -> BlockInfo True "nether_wart_block" (PixelRGB8 92 3 3) )
    ,( 215, \_	 -> BlockInfo True "red_nether_brick" (PixelRGB8 68 0 0) )
    ,( 216, \_	 -> BlockInfo True "bone_block" (PixelRGB8 175 170 146) )
    ,( 217, \_	 -> BlockInfo True "structure_void" (PixelRGB8 18 94 100) )
    ,( 218, \_	 -> BlockInfo True "observer" (PixelRGB8 80 80 80) )
    ,( 219, \_	 -> BlockInfo True "white_shulker_box" (PixelRGB8 210 207 207) )
    ,( 220, \_	 -> BlockInfo True "orange_shulker_box" (PixelRGB8 196 111 54) )
    ,( 221, \_	 -> BlockInfo True "magenta_shulker_box" (PixelRGB8 177 95 184) )
    ,( 222, \_	 -> BlockInfo True "light_blue_shulker_box" (PixelRGB8 95 133 192) )
    ,( 223, \_	 -> BlockInfo True "yellow_shulker_box" (PixelRGB8 182 173 58) )
    ,( 224, \_	 -> BlockInfo True "lime_shulker_box" (PixelRGB8 68 174 57) )
	,( 225, \_	 -> BlockInfo True "pink_shulker_box" (PixelRGB8 198 135 155) )
	,( 226, \_	 -> BlockInfo True "gray_shulker_box" (PixelRGB8 78 77 77) )
	,( 227, \_	 -> BlockInfo True "light_gray_shulker_box" (PixelRGB8 154 152 152) )
	,( 228, \_	 -> BlockInfo True "cyan_shulker_box" (PixelRGB8 65 130 156) )
	,( 229, \_	 -> BlockInfo True "purple_shulker_box" (PixelRGB8 143 97 143) )
	,( 230, \_	 -> BlockInfo True "blue_shulker_box" (PixelRGB8 95 107 190) )
	,( 231, \_	 -> BlockInfo True "brown_shulker_box" (PixelRGB8 134 107 89) )
	,( 232, \_	 -> BlockInfo True "green_shulker_box" (PixelRGB8 105 123 79) )
	,( 233, \_	 -> BlockInfo True "red_shulker_box" (PixelRGB8 184 84 81) )
	,( 234, \_	 -> BlockInfo True "black_shulker_box" (PixelRGB8 54 52 52) )
	,( 235, \_	 -> BlockInfo False "unkown" (PixelRGB8 0 0 0) )
	,( 236, \_	 -> BlockInfo False "unkown" (PixelRGB8 0 0 0) )
	,( 237, \_	 -> BlockInfo False "unkown" (PixelRGB8 0 0 0) )
	,( 238, \_	 -> BlockInfo False "unkown" (PixelRGB8 0 0 0) )
	,( 239, \_	 -> BlockInfo False "unkown" (PixelRGB8 0 0 0) )
	,( 240, \_	 -> BlockInfo False "unkown" (PixelRGB8 0 0 0) )
	,( 241, \_	 -> BlockInfo False "unkown" (PixelRGB8 0 0 0) )
	,( 242, \_	 -> BlockInfo False "unkown" (PixelRGB8 0 0 0) )
	,( 243, \_	 -> BlockInfo False "unkown" (PixelRGB8 0 0 0) )
	,( 244, \_	 -> BlockInfo False "unkown" (PixelRGB8 0 0 0) )
	,( 245, \_	 -> BlockInfo False "unkown" (PixelRGB8 0 0 0) )
	,( 246, \_	 -> BlockInfo False "unkown" (PixelRGB8 0 0 0) )
	,( 247, \_	 -> BlockInfo False "unkown" (PixelRGB8 0 0 0) )
	,( 248, \_	 -> BlockInfo False "unkown" (PixelRGB8 0 0 0) )
	,( 249, \_	 -> BlockInfo False "unkown" (PixelRGB8 0 0 0) )
	,( 250, \_	 -> BlockInfo False "unkown" (PixelRGB8 0 0 0) )
	,( 251, \_	 -> BlockInfo False "unkown" (PixelRGB8 0 0 0) )
	,( 252, \_	 -> BlockInfo False "unkown" (PixelRGB8 0 0 0) )
	,( 253, \_	 -> BlockInfo False "unkown" (PixelRGB8 0 0 0) )
	,( 254, \_	 -> BlockInfo False "unkown" (PixelRGB8 0 0 0) )
	,( 255, \_	 -> BlockInfo True "structure_block" (PixelRGB8 0 0 0) )
    ]
{--



--}
