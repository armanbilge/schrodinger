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

package schrodinger.kernel.testkit

import org.scalacheck.Prop
import org.scalacheck.Prop.Result
import org.typelevel.discipline.Laws

class EqUndecidableException extends Exception

extension (ruleSet: Laws#RuleSet) def random: Laws#RuleSet = RandomRuleSet(ruleSet)

object RandomRuleSet extends Laws:
  def apply(ruleSet: Laws#RuleSet): RuleSet =
    new RuleSet:
      val bases = ruleSet.bases
      val name = ruleSet.name
      val parents = ruleSet.parents.map(apply)

      def props = ruleSet.props.map { (name, prop) =>
        name -> prop.map {
          case result @ Result(Prop.Exception(ex: EqUndecidableException), _, _, _) =>
            result.copy(status = Prop.Undecided)
          case result => result
        }
      }
