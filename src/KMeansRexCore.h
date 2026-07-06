/* KMeansRexCore.cpp
This file includes code modfied under the following license:
BSD 3-clause license for open-source software.

Copyright (c) 2013-2015, Michael C. Hughes
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are
permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this list of
conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice, this list of
conditions and the following disclaimer in the documentation and/or other materials
provided with the distribution.

3. Neither the name of the copyright holder nor the names of its contributors may be used
to endorse or promote products derived from this software without specific prior written
permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY
EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. LICENSED UNDER THE BSD
*/
#pragma once

#include <iostream>
#include "cpp11eigen.hpp"

namespace kmeans_rex {
using namespace Eigen;
using namespace std;

// DEFINE Custom Type Names to make code more readable.
typedef Map<ArrayXXd> ExtMat;
typedef ArrayXXd Mat;
typedef ArrayXd Vec;

// KMeans Interface.
void RunKMeans(const Eigen::Ref<const Eigen::MatrixXd> X_IN, int K, int Niter,
               Eigen::Ref<Eigen::MatrixXd> Mu_OUT, Eigen::Ref<Eigen::VectorXd> Z_OUT);
}  // namespace kmeans_rex
