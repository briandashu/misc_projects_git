#!/usr/local/bin/ruby

require "minitest/autorun"
require_relative "maze.rb"

$MAZE1 = "inputs/maze1"
$MAZE2 = "inputs/maze2"
$MAZE3 = "inputs/maze3"
$MYMAZE = "mymaze"

$MAZE1_STD = "inputs/maze1-std"
$MAZE1B_STD = "inputs/maze1b-std"
$MAZE2_STD = "inputs/maze2-std"
$MAZE3_STD = "inputs/maze3-std"
$MAZE4_STD = "inputs/maze4-std"
$OUTPUT_MAZE1 = File.read("outputs/public_print_maze1.out")
$OUTPUT_MAZE2 = File.read("outputs/public_print_maze2.out")
$OUTPUT_MAZE3 = File.read("outputs/public_print_maze3.out")

class PublicTests < Minitest::Test
    #puts main("open", $MAZE1)
    #puts main("bridge", $MAZE1)
    #puts main("sortcells",$MAZE1)

    #puts main("open", $MAZE2)
    #puts main("bridge", $MAZE2)
    #puts main("sortcells",$MAZE2)

    #puts main("open", $MAZE3)
    #puts main("bridge", $MAZE3)
    #puts main("sortcells",$MAZE1).join("\n")

    #puts main("print",$MYMAZE)
    #puts main("distance", $MAZE1)
    puts main("paths", $MAZE3)
end