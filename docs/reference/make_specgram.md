# Plot a spectrogram with default settings

This is a wrapper function for
[specgram](https://rdrr.io/pkg/signal/man/specgram.html) to draw a
spectrogram with the same input argument names and defaults as the tag
tools Matlab/Octave function make_specgram.

## Usage

``` r
make_specgram(
  x,
  nfft = 256,
  fs = 2,
  window = gsignal::hanning(nfft),
  noverlap = length(window)/2,
  draw_plot = TRUE
)
```

## Arguments

- x:

  The input signal

- nfft:

  specifies the number of frequency points used to calculate the
  discrete Fourier transforms.

- fs:

  The sampling frequency in Hz

- window:

  If you specify a scalar for `window`, make_specgram uses a Hanning
  window of that length. `window` must have length smaller than or equal
  to `nfft` and greater than `noverlap`.

- noverlap:

  The number of samples the sections of `x` overlap.

- draw_plot:

  (logical) Should a plot be drawn? Defaults to TRUE.

## Value

if `draw_plot` is TRUE, a plot is produced. If it is FALSE, a list is
returned, with as follows. Each element is a matrix and all three
matrices are the same size.

- `s, ` A matrix of spectrogram values of signal x in dB.

- `f, ` Frequencies (Hz) corresponding to the rows of s

- `t, ` Time indices corresponding to the columns of s

## Examples

``` r
x <- gsignal::chirp(seq(from = 0, by = 0.001, to = 2),
  f0 = 0,
  t1 = 2,
  f1 = 500
)
S <- make_specgram(x, nfft = 256, fs = 2, 
noverlap = 128, draw_plot = FALSE)
```
