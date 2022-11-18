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

package schrodinger.unsafe

import munit.FunSuite
import scodec.bits.*

class ThreefishSuite extends FunSuite:

  test("test vector") {
    val out = new Array[Long](4)
    Threefish.processBlock(new Array(4), new Array(4), out)
    assertEquals(
      out.map(ByteVector.fromLong(_)).reduce(_ ++ _),
      hex"84da2a1f8beaee947066ae3e3103f1ad536db1f4a1192495116b9f3ce6133fd8",
    )
  }
