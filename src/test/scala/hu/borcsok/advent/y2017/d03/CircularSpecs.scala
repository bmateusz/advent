package hu.borcsok.advent.y2017.d03

import org.specs2.Specification
import hu.borcsok.advent.y2017.d03.Circular.Position

class CircularSpecs extends Specification {

  def is =
s2"""
# Day 03

## Part I
Each square on the grid is allocated in a spiral pattern starting at a location marked 1 and then counting up while
spiraling outward. For example, the first few squares are allocated like this:

37  36  35  34  33  32  31
38  17  16  15  14  13  30
39  18   5   4   3  12  29
40  19   6   1   2  11  28
41  20   7   8   9  10  27
42  21  22  23  24  25  26
43  44  45  46  47  48  49
While this is very space-efficient (no squares are skipped), requested data must be carried back to square 1
(the location of the only access port for this memory system) by programs that can only move up, down, left, or right.
They always take the shortest path: the Manhattan Distance between the location of the data and square 1.

For example:

${Circular.manhattan(1) === 0} steps, since it's at the access port.
${Circular.manhattan(2) === 1}
${Circular.manhattan(3) === 2}
${Circular.manhattan(4) === 1}
${Circular.manhattan(5) === 2}
${Circular.manhattan(6) === 1}
${Circular.manhattan(7) === 2}
${Circular.manhattan(8) === 1}
${Circular.manhattan(9) === 2}
${Circular.manhattan(10) === 3}
${Circular.manhattan(11) === 2}
${Circular.manhattan(12) === 3} steps, such as: down, left, left.
${Circular.manhattan(13) === 4}
${Circular.manhattan(14) === 3}
${Circular.manhattan(15) === 2}
${Circular.manhattan(16) === 3}
${Circular.manhattan(17) === 4}
${Circular.manhattan(18) === 3}
${Circular.manhattan(19) === 2}
${Circular.manhattan(20) === 3}
${Circular.manhattan(21) === 4}
${Circular.manhattan(22) === 3}
${Circular.manhattan(23) === 2} steps: up twice.
${Circular.manhattan(24) === 3}

${Circular.manhattan(1024) === 31} steps.
${Circular.manhattan(368078) === 371} steps.

${Circular.positionFrom(1) === Position(0, 0)}
${Circular.positionFrom(2) === Position(1, 0)}
${Circular.positionFrom(3) === Position(1, -1)}
${Circular.positionFrom(4) === Position(0, -1)}
${Circular.positionFrom(5) === Position(-1, -1)}
${Circular.positionFrom(6) === Position(-1, 0)}
${Circular.positionFrom(7) === Position(-1, 1)}
${Circular.positionFrom(8) === Position(0, 1)}
${Circular.positionFrom(9) === Position(1, 1)}
${Circular.positionFrom(10) === Position(2, 1)}
${Circular.positionFrom(11) === Position(2, 0)}
${Circular.positionFrom(12) === Position(2, -1)}
${Circular.positionFrom(13) === Position(2, -2)}
${Circular.positionFrom(14) === Position(1, -2)}
${Circular.positionFrom(15) === Position(0, -2)}
${Circular.positionFrom(16) === Position(-1, -2)}
${Circular.positionFrom(17) === Position(-2, -2)}
${Circular.positionFrom(18) === Position(-2, -1)}
${Circular.positionFrom(19) === Position(-2, 0)}
${Circular.positionFrom(20) === Position(-2, 1)}
${Circular.positionFrom(21) === Position(-2, 2)}
${Circular.positionFrom(22) === Position(-1, 2)}
${Circular.positionFrom(23) === Position(0, 2)}
${Circular.positionFrom(24) === Position(1, 2)}

## Part II

As a stress test on the system, the programs here clear the grid and then store the value 1 in square 1.
Then, in the same allocation order as shown above, they store the sum of the values in all adjacent squares,
including diagonals.

So, the first few squares' values are chosen as follows:

Square 1 starts with the value 1.
Square 2 has only one adjacent filled square (with value 1), so it also stores 1.
Square 3 has both of the above squares as neighbors and stores the sum of their values, 2.
Square 4 has all three of the aforementioned squares as neighbors and stores the sum of their values, 4.
Square 5 only has the first and fourth squares as neighbors, so it gets the value 5.
Once a square is written, its value does not change. Therefore, the first few squares would receive the following values:

147  142  133  122   59
304    5    4    2   57
330   10    1    1   54
351   11   23   25   26
362  747  806--->   ...

${Circular.stress(1) === 1}
${Circular.stress(2) === 1}
${Circular.stress(3) === 2}
${Circular.stress(4) === 4}
${Circular.stress(5) === 5}
${Circular.stress(6) === 10}
${Circular.stress(7) === 11}
${Circular.stress(8) === 23}
${Circular.stress(9) === 25}
${Circular.stress(10) === 26}
${Circular.stress(11) === 54}
${Circular.stress(12) === 57}
${Circular.stress(13) === 59}
${Circular.stress(14) === 122}
${Circular.stress(15) === 133}
${Circular.stress(16) === 142}
${Circular.stress(17) === 147}
${Circular.stress(18) === 304}
${Circular.stress(19) === 330}
${Circular.stress(20) === 351}
${Circular.stress(21) === 362}
${Circular.stress(22) === 747}
${Circular.stress(23) === 806}
${Circular.stress(24) === 880}

${Circular.stressFirstLargerThan(368078) === Some(65)}
${Circular.stress(65) === 369601}
"""
}
