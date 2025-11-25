#include <RcppArmadillo.h>
#include <string>
#include <stdexcept>
using namespace Rcpp;

//' @title Convolution (in C++)
//' @description Performs 1D convolution of two vectors in C++ via FFT.
//' @param a The first numeric vector.
//' @param b The second numeric vector.
//' @param shape (optional) The shape of the output: "full", "same", or "valid". Default is "full," matching \code{\link[signal]{conv}} and the Armadillo default, which means that the output array is \code{length(a)} + \code{length(b)} - 1.
//' @return A numeric vector containing the convolution result.
//' @export
// [[Rcpp::export]]
 arma::vec conv_cpp(const arma::vec& a, const arma::vec& b, std::string shape = "full") {
   // note the const ...& is to pass inputs in by reference so copies are not made (help w/speed)
   // only really matters for the big vectors/matrices but we don't need to mod copies anyway
   if (shape!= "full" && shape!= "same" && shape!= "valid") {
     Rcpp::stop("Shape must be 'full', 'same', or 'valid'.");
   }
   return arma::conv(a, b, shape.c_str());
 }