/*
 * Copyright 2021 Arman Bilge
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/*
 * math.js
 * https://github.com/josdejong/mathjs
 * Copyright (C) 2013-2022 Jos de Jong <wjosdejong@gmail.com>
 */

package schrodinger.math.special

def logGamma(x: Double): Double =
  if x < 0 then Double.NaN
  else if x == 0 then Double.PositiveInfinity
  else if !x.isFinite then x
  else if x < 0.5 then
    // Use Euler's reflection formula:
    // gamma(z) = PI / (sin(PI * z) * gamma(1 - z))
    Math.log(Math.PI / Math.sin(Math.PI * x)) - logGamma(1 - x)
  else

    // Compute the logarithm of the Gamma function using the Lanczos method

    val y = x - 1
    val base = y + 5.5 // Base of the Lanczos exponential

    inline def series(i: Int) =
      inline i match
        case 0 => 1.000000000190015
        case 1 => 76.18009172947146
        case 2 => -86.50532032941677
        case 3 => 24.01409824083091
        case 4 => -1.231739572450155
        case 5 => 0.1208650973866179e-2
        case 6 => -0.5395239384953e-5

    inline def go(i: Int, sum: Double): Double =
      inline if i >= 1 then go(i - 1, sum + series(i) / (y + i))
      else sum

    val sum = go(6, series(0))

    val logSqrt2Pi = 0.91893853320467274178
    logSqrt2Pi + (y + 0.5) * Math.log(base) - base + Math.log(sum)
