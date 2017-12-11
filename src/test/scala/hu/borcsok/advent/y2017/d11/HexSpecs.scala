package hu.borcsok.advent.y2017.d11

import org.specs2.Specification
import hu.borcsok.advent.y2017.Helpers._

class HexSpecs extends Specification {

  val input: String = readResource("2017/hex.txt")

  def is =
s2"""
# Day 11

## Part I

Crossing the bridge, you've barely reached the other side of the stream when a program comes up to you, clearly in distress.
"It's my child process," she says, "he's gotten lost in an infinite grid!"

Fortunately for her, you have plenty of experience with infinite grids.

Unfortunately for you, it's a hex grid.

The hexagons ("hexes") in this grid are aligned such that adjacent hexes can be found to the north, northeast,
southeast, south, southwest, and northwest:

  \ n  /
nw +--+ ne
  /    \
-+      +-
  \    /
sw +--+ se
  / s  \
You have the path the child process took. Starting where he started, you need to determine the fewest number of steps
required to reach him. (A "step" means to move from the hex you are in to any adjacent hex.)

For example:

${Hex.distance("ne,ne,ne") === 3} steps away.
${Hex.distance("ne,ne,sw,sw") === 0} steps away (back where you started).
${Hex.distance("ne,ne,s,s") === 2} steps away (se,se).
${Hex.distance("se,sw,se,sw,sw") === 3} steps away (s,s,sw).
${Hex.distance("n,se,s,sw,nw,n,se") === 0} steps away, take a circle around zero then go back.

${Hex.distance(input) === 687}

## Part II

How many steps away is the furthest he ever got from his starting position?

${Hex.furthestDistance(input) === 1483}
"""
}
