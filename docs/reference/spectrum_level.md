# Compute the spectrum level of a signal x.

This function is used to compute the spectrum level of a signal x.

## Usage

``` r
spectrum_level(x, nfft = 512, sampling_rate = 1, w = nfft, nov = round(w/2))
```

## Arguments

- x:

  A vector containing the signal to be processed. For signals with
  multiple channels, each channel should be in a column of x.

- nfft:

  The length of the fft to use. Choose a power of two for fastest
  operation. Default value is 512.

- sampling_rate:

  The sampling rate of x in Hz. Default value is 1. sampling_rate is the
  vector of frequencies at which SL is calculated.

- w:

  The window length. The default value is nfft. If w\<nfft, each segment
  of w samples is zero-padded to nfft.

- nov:

  The number of samples to overlap each segment. The default value is
  half of the window length.

## Value

A list with 2 elements:

- **SL:** The spectrum level at each frequency in dB RMS re root-Hz. The
  spectrum is single-sided and extends to sampling_rate/2. The reference
  level is 1.0 (i.e., white noise with unit variance will have a
  spectrum level of 3-10\*log10(sampling_rate). The 3dB is because both
  the negative and positive spectra are added together so that the total
  power in the signal is the same as the total power in the spectrum.

- **freq:** The vector of frequencies at which SL is calculated.

## Note

The spectrum is single-sided and extends to sampling_rate/2. The
reference level is 1.0 (i.e., white noise with unit variance will have a
spectrum level of 3-10\*log10(sampling_rate). The 3dB is because both
the negative and positive spectra are added together so that the total
power in the signal is the same as the total power in the spectrum.

## Examples

``` r
list <- spectrum_level(x = beaked_whale$P$data, 
nfft = 4, sampling_rate = beaked_whale$P$sampling_rate)
```
