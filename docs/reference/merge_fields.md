# Merge the fields of two lists

This function is used to merge the fields of two lists. If there are
duplicate fields, the fields in s1 are taken.

## Usage

``` r
merge_fields(s1, s2)
```

## Arguments

- s1:

  Arbitrary list e.g., containing metadata or settings.

- s2:

  Arbitrary list e.g., containing metadata or settings.

## Value

A list containing all of the fields in s1 and s2

## Examples

``` r
s1 <- list(a = 1, b = c(2, 3, 4))
s2 <- list(b = 3, c = "cat")
s <- merge_fields(s1, s2)
```
