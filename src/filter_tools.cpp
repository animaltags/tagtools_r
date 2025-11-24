// filter_cpp() code from: SigPack - the C++ signal processing library
// https://sigpack.sourceforge.net/index.html
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

#include <RcppArmadillo.h>
#include <string>
#include <stdexcept>
using namespace Rcpp;

//' @title C++ convolution
//' @description Performs 1D convolution of two vectors.
//' @param a The first numeric vector.
//' @param b The second numeric vector.
//' @param shape (optional) The shape of the output: "full", "same", or "valid". Default is "full," matching \code{\link[signal]{conv}} and the Armadillo default, which means that the output array is \code{length(a)} + \code{length(b)} - 1.
//' @return A numeric vector containing the convolution result.
//' @export
// [[Rcpp::export]]
 arma::vec conv_cpp(arma::vec a, arma::vec b, std::string shape = "full") {
   
   if (shape!= "full" && shape!= "same" && shape!= "valid") {
     Rcpp::stop("Shape must be 'full', 'same', or 'valid'.");
   }
   return arma::conv(a, b, shape.c_str());
 }