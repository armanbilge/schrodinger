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
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package schrodinger.random;

import org.apache.commons.rng.UniformRandomProvider;
import org.apache.commons.rng.sampling.distribution.ContinuousSampler;
import org.apache.commons.rng.sampling.distribution.NormalizedGaussianSampler;

class MarsagliaTsangGammaSampler implements ContinuousSampler {

    /** 1/3. */
    private static final double ONE_THIRD = 1d / 3;

    private final UniformRandomProvider rng;
    private final double alpha;
    private final double theta;

    /** Optimization (see code). */
    private final double dOptim;
    /** Optimization (see code). */
    private final double cOptim;
    /** Gaussian sampling. */
    private final NormalizedGaussianSampler gaussian;

    /**
     * @param rng   Generator of uniformly distributed random numbers.
     * @param alpha Alpha parameter of the distribution.
     * @param theta Theta parameter of the distribution.
     * @throws IllegalArgumentException if {@code alpha <= 0} or {@code theta <= 0}
     */
    MarsagliaTsangGammaSampler(UniformRandomProvider rng, NormalizedGaussianSampler gaussian, double alpha, double theta) {
        this.rng = rng;
        this.gaussian = gaussian;
        this.alpha = alpha;
        this.theta = theta;
        dOptim = alpha - ONE_THIRD;
        cOptim = ONE_THIRD / Math.sqrt(dOptim);
    }

    public double sample() {
        while (true) {
            final double x = gaussian.sample();
            final double oPcTx = 1 + cOptim * x;
            final double v = oPcTx * oPcTx * oPcTx;

            if (v <= 0) {
                continue;
            }

            final double x2 = x * x;
            final double u = rng.nextDouble();

            // Squeeze.
            if (u < 1 - 0.0331 * x2 * x2) {
                return theta * dOptim * v;
            }

            if (Math.log(u) < 0.5 * x2 + dOptim * (1 - v + Math.log(v))) {
                return theta * dOptim * v;
            }
        }
    }

}