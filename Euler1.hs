module Euler1 where

import qualified Euler001
import qualified Euler002
import qualified Euler003
import qualified Euler004
import qualified Euler005
import qualified Euler006
import qualified Euler007
import qualified Euler008
import qualified Euler009
import qualified Euler010
import qualified Euler011
import qualified Euler012
import qualified Euler013
import qualified Euler014
import qualified Euler015
import qualified Euler016
import qualified Euler017
import qualified Euler018
import qualified Euler019
import qualified Euler020
import qualified Euler021
import qualified Euler022
import qualified Euler023
import qualified Euler024
import qualified Euler025
import qualified Euler026
import qualified Euler027
import qualified Euler028
import qualified Euler029
import qualified Euler030
import qualified Euler031
import qualified Euler032
import qualified Euler033
import qualified Euler034
import qualified Euler035
import qualified Euler036
import qualified Euler037
import qualified Euler038
import qualified Euler039
import qualified Euler040
import qualified Euler041
import qualified Euler042
import qualified Euler043
import qualified Euler044
import qualified Euler045
import qualified Euler046
import qualified Euler047
import qualified Euler048
import qualified Euler049
import qualified Euler050
import qualified Euler051
import qualified Euler052
import qualified Euler053
import qualified Euler054
import qualified Euler055
import qualified Euler056
import qualified Euler057
import qualified Euler058
import qualified Euler059
import qualified Euler060
import qualified Euler061
import qualified Euler062
import qualified Euler063
import qualified Euler064
import qualified Euler065
import qualified Euler066
import qualified Euler067
import qualified Euler068
import qualified Euler069
import qualified Euler070
import qualified Euler071
import qualified Euler072
import qualified Euler073
import qualified Euler074
import qualified Euler075
import qualified Euler076
import qualified Euler077
import qualified Euler078
import qualified Euler079
import qualified Euler080
import qualified Euler081
import qualified Euler082
import qualified Euler083
import qualified Euler084
import qualified Euler085
import qualified Euler086
import qualified Euler087
import qualified Euler088
import qualified Euler089
import qualified Euler090
import qualified Euler091
import qualified Euler092
import qualified Euler093
import qualified Euler094
import qualified Euler095
import qualified Euler096
import qualified Euler097
import qualified Euler098
import qualified Euler099
import qualified Euler100

checks :: [(Int, Double, Int, IO String, String, String)]
checks = [
  (  1, 0.0,  1, Euler001.main, "233168", "Add multiples of 3 or 5"),
  (  2, 0.0,  1, Euler002.main, "4613732", "Sum of even-valued Fibonacci numbers"),
  (  3, 0.0,  1, Euler003.main, "6857", "Find the largest prime factor"),
  (  4, 0.0,  1, Euler004.main, "906609", "Largest palindrome made from a product"),
  (  5, 0.0,  1, Euler005.main, "232792560", "Smallest number divisible by 1 to 20"),
  (  6, 0.0,  1, Euler006.main, "25164150", "Sum of squares - square of sums"),
  (  7, 0.1,  1, Euler007.main, "104743", "Find the 10001st prime"),
  (  8, 0.0,  1, Euler008.main, "40824", "Largest product of 5 consecutive digits"),
  (  9, 0.0,  1, Euler009.main, "31875000", "Pythagorean triplet with a+b+c=1000"),
  ( 10, 0.1,  1, Euler010.main, "142913828922", "Sum of primes below 2 million"),
  ( 11, 0.0,  1, Euler011.main, "70600674", "Greatest product of 4 numbers"),
  ( 12, 0.2,  1, Euler012.main, "76576500", "Triangle number with 500 divisors"),
  ( 13, 0.0,  1, Euler013.main, "5537376230", "Sum of one-hundred 50-digit numbers"),
  ( 14, 1.9,  5, Euler014.main, "837799", "Longest Collatz Sequence"),
  ( 15, 0.0,  1, Euler015.main, "137846528820", "How many routes to the bottom corner"),
  ( 16, 0.0,  1, Euler016.main, "1366", "Sum of the digits of 2^1000"),
  ( 17, 0.0,  1, Euler017.main, "21124", "How many letters to write 1 to 1000"),
  ( 18, 0.0,  1, Euler018.main, "1074", "Maximum sum travelling down the triangle"),
  ( 19, 0.0,  1, Euler019.main, "171", "Sundays on the first of the month"),
  ( 20, 0.0,  1, Euler020.main, "648", "Sum of the digits in 100 factorial"),
  ( 21, 0.2,  1, Euler021.main, "31626", "Sum of amicable pairs under 10000"),
  ( 22, 0.2,  3, Euler022.main, "871198282", "Total of all name scores in file"),
  ( 23, 1.1,  1, Euler023.main, "4179871", "Sums of two abundant numbers"),
  ( 24, 0.0,  1, Euler024.main, "2783915460", "Millionth lexicographic permutation"),
  ( 25, 0.0,  1, Euler025.main, "4782", "First Fibonacci number with 1000 digits"),
  ( 26, 0.0,  1, Euler026.main, "983", "1/d containing the longest cycle"),
  ( 27, 0.2,  1, Euler027.main, "-59231", "Quadratic formula that produces primes"),
  ( 28, 0.0,  1, Euler028.main, "669171001", "Sum of diagonals in a spiral"),
  ( 29, 0.1,  1, Euler029.main, "9183", "Distinct terms generated by a^b"),
  ( 30, 0.1,  2, Euler030.main, "443839", "Sums of fifth powers of digits"),
  ( 31, 0.0,  1, Euler031.main, "73682", "Combinations of currency denominations"),
  ( 32, 0.1,  1, Euler032.main, "45228", "Sum of pandigital products"),
  ( 33, 0.0,  1, Euler033.main, "100", "Fractions with unorthodox cancelling"),
  ( 34, 0.1,  2, Euler034.main, "40730", "Sums of factorials of digits"),
  ( 35, 0.3,  1, Euler035.main, "55", "Circular primes"),
  ( 36, 0.1,  1, Euler036.main, "872187", "Palindromes in base 10 and base 2"),
  ( 37, 0.4,  3, Euler037.main, "748317", "Truncatable primes"),
  ( 38, 0.0,  1, Euler038.main, "932718654", "Largest pandigital"),
  ( 39, 0.0,  1, Euler039.main, "840", "Perimeter with most right triangles"),
  ( 40, 0.0,  1, Euler040.main, Euler040.answer, "Finding the nth digit"),
  ( 41, 0.0,  1, Euler041.main, "7652413", "Largest pandigital prime"),
  ( 42, 0.1,  2, Euler042.main, "162", "Triangle words"),
  ( 43, 0.0,  1, Euler043.main, "16695334890", "Sub-string divisibility"),
  ( 44, 0.1,  1, Euler044.main, "5482660", "Pentagonal sum and difference"),
  ( 45, 0.1,  2, Euler045.main, "1533776805", "Pentagonal, hexagonal triangle number"),
  ( 46, 0.0,  1, Euler046.main, "5777", "Sum of a prime and twice a square"),
  ( 47, 0.3,  1, Euler047.main, "134043", "Consecutive integers with 4 prime factors"),
  ( 48, 0.0,  1, Euler048.main, "9110846700", "1^1 + 2^2 + ... + 1000^1000"),
  ( 49, 0.2,  1, Euler049.main, "296962999629", "Prime permutation arithmetic sequences"),
  ( 50, 0.0,  1, Euler050.main, "997651", "Prime as the sum of consecutive primes"),
  ( 51, 0.2,  1, Euler051.main, "121313", "Changing a part to form 8 primes"),
  ( 52, 0.0,  1, Euler052.main, "142857", "2x, 3x, 4x, 5x, 6x with the same digits"),
  ( 53, 0.0,  1, Euler053.main, "4075", "C(n,r) exceeding 1 million"),
  ( 54, 0.2,  1, Euler054.main, "376", "Poker Hands"),
  ( 55, 0.6,  1, Euler055.main, "249", "Lychrel Numbers"),
  ( 56, 0.2,  1, Euler056.main, "972", "Maximum digital sum of a^b"),
  ( 57, 0.1,  1, Euler057.main, "153", "Continued fraction for square root of 2"),
  ( 58, 0.6,  1, Euler058.main, "26241", "Primes on diagonals of spiral grid"),
  ( 59, 0.0,  1, Euler059.main, "107359" , "Decrypt XOR cipher"),
  ( 60, 3.1,  2, Euler060.main, Euler060.answer, "Concatenating a set of five primes"),
  ( 61, 0.0,  1, Euler061.main, "28684", "Figurate numbers with a cyclic property"),
  ( 62, 0.3,  7, Euler062.main, "127035954683", "Digit permutations are cubes"),
  ( 63, 0.0,  1, Euler063.main, "49", "n-digit integers which are nth powers"),
  ( 64, 0.4,  1, Euler064.main, "1322", "Continued fractions with odd period"),
  ( 65, 0.0,  1, Euler065.main, "272", "Convergents of continued fraction for e"),
  ( 66, 0.1,  1, Euler066.main, "661", "Diophantine equation x^2 - Dy^2 = 1"),
  ( 67, 0.1,  2, Euler067.main, "7273", "Maximal sum in the triangle"),
  ( 68, 0.0,  1, Euler068.main, "6531031914842725", "Magic 5-gon ring"),
  ( 69, 0.0,  1, Euler069.main, "510510", "Maximize n / phi(n)"),
  ( 70, 0.3,  5, Euler070.main, "8319823", "phi(n) as a permutation of n"),
  ( 71, 0.0,  1, Euler071.main, "428570", "Listing proper fractions in order"),
  ( 72, 0.9,  9, Euler072.main, "303963552391", "Counting proper fractions"),
  ( 73, 0.3,  1, Euler073.main, "5066251", "Proper fractions between 1/3 and 1/2"),
  ( 74, 1.2,  6, Euler074.main, "402", "Factorial Chains"),
  ( 75, 1.5, 18, Euler075.main, "214954", "Lengths forming a unique right triangle"),
  ( 76, 0.0,  2, Euler076.main, "190569291", "100 as sum of at least two integers"),
  ( 77, 0.1,  1, Euler077.main, "71", "Value as sum of primes in 5000 ways"),
  ( 78, 1.2,  3, Euler078.main, "55374", "Separating coins into piles"),
  ( 79, 0.0,  1, Euler079.main, "73162890", "Determine the secret passcode"),
  ( 80, 0.2,  1, Euler080.main, "40886", "Sum of digits of square roots"),
  ( 81, 0.2,  3, Euler081.main, "427337", "Minimal Path Sum I"),
  ( 82, 0.2,  3, Euler082.main, "260324", "Minimal Path Sum II"),
  ( 83, 1.1,  7, Euler083.main, "425185", "Minimal Path Sum III"),
  ( 84, 0.1,  1, Euler084.main, "101524", "Most Popular Monopoly Squares"),
  ( 85, 0.0,  1, Euler085.main, "2772", "Number of Rectangles in a Grid"),
  ( 86, 0.1,  1, Euler086.main, "1818", "Shortest Path between Corners of a Cuboid"),
  ( 87, 1.3,  5, Euler087.main, "1097343", "Sum of Prime Square, Cube, and 4th Power"),
  ( 88, 1.3, 16, Euler088.main, "7587457", "Minimal Product-Sum Numbers"),
  ( 89, 0.0,  1, Euler089.main, "743", "Minimizing Roman Numerals"),
  ( 90, 0.0,  1, Euler090.main, "1217", "Two Cubes to Make a Square"),
  ( 91, 0.4,  2, Euler091.main, "14234", "Right Angle Triangles in the Quadrant"),
  ( 92, 0.0,  1, Euler092.main, "8581146", "Square Digits Number Chain"),
  ( 93, 0.9,  2, Euler093.main, "1258", "Arithmetic with Four Digits"),
  ( 94, 0.0,  1, Euler094.main, "518408346", "Almost Equilateral Triangles"),
  ( 95, 1.1,  5, Euler095.main, Euler095.answer, "The longest amicable chain"),
  ( 96, 3.6,  2, Euler096.main, "24702", "Solving Su Doku puzzles"),
  ( 97, 0.0,  1, Euler097.main, "8739992577", "28433 * 2^7830457 + 1"),
  ( 98, 0.2,  2, Euler098.main, "18769", "Anagrams Representing Square Numbers"),
  ( 99, 0.1,  1, Euler099.main, "709", "Greatest Base/Exponent Pair"),
  (100, 0.0,  1, Euler100.main, "756872327473", "50% Chance of Two Blue Discs")
  ]