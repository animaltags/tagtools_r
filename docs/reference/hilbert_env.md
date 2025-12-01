# Compute the envelope of X using Hilbert transform. Compute the envelope of the signal matrix X using the Hilbert transform. To avoid long transforms, this function uses the overlap and add method.

Compute the envelope of X using Hilbert transform.

Compute the envelope of the signal matrix X using the Hilbert transform.
To avoid long transforms, this function uses the overlap and add method.

## Usage

``` r
hilbert_env(X, N = 1024)
```

## Arguments

- X:

  a vector or matrix of signals. If X is a matrix, each column is
  treated as a separate signal. The signals must be regularly sampled
  for the result to be correctly interpretable as the envelope.

- N:

  (optional) specifies the transform length used. The default value is
  1024 and this may be fine for most situations.

## Value

E, the envelope of X. E is the same size as X: it has the same number of
columns and the same number of samples per signal. It has the same units
as X but being an envelope, all values are \>=0.

## Examples

``` r
s <- matrix(sin(0.1 * c(1:10000)), ncol = 1) *
 matrix(sin(0.001 * c(1:10000)), ncol = 1)
E <- hilbert_env(s)
plot(c(1:length(s)), s, col = 'grey34')
lines(c(1:length(E)), E, col = 'black')

```
