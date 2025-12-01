# Buffers a signal vector into matrix

This function is used to buffer a signal vector into a matrix of data
frames. If the input for nodelay is TRUE, the the signal is buffered
with no delay. If nodelay is FALSE, and specifies a vector of samples to
precede x\[1\] in an overlapping buffer.

## Usage

``` r
buffer(x, n, overlap, opt, nodelay = FALSE)
```

## Arguments

- x:

  The signal vector to be buffered

- n:

  The desired length of data segments (rows).

- overlap:

  The desired amount of overlap between consecutive frames (columns) in
  the output matrix

- opt:

  The vector of samples specified to precede x\[1\] in an overlapping
  buffer

- nodelay:

  A logical statement to determine if the vector should be buffered with
  or without delay. Default is FALSE (with delay)

## Value

A list with 3 elements is returned if nodelay = FALSE:

- **X:** A matrix of the buffered signal vector "vec" with "n" data
  segments and an overlap between consecutive frames specified by "p".
  The matrix starts with "opt" values if nodelay is FALSE.

- **z:** The remainder of the vector which was not included in the
  matrix if the last column did not have a full number of rows.

- **opt:** The last values, length of "p", of the matrix "X".

If nodelay = TRUE, then a matrix of the buffered signal vector "vec"
with "n" data segments and an overlap between consecutive frames
specified by "overlap". The matrix starts with "opt" values if nodelay
is FALSE.

## Examples

``` r
x <- c(1:10)
n <- 3
overlap <- 2
opt <- c(2, 1)
list1 <- buffer(x, n, overlap, opt)
list2 <- buffer(x, n, overlap, nodelay = TRUE)
```
