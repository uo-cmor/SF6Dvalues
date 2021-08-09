
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SF6Dvalues

<!-- badges: start -->
<!-- badges: end -->

The goal of SF6Dvalues is to provide calculators and tools for SF-6D
value sets.

## Installation

You can install the most recent version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("uo-cmor/SF6Dvalues")
```

## Example

### S3 vector classes

The `SF12` and `SF6D` classes provide S3 vectors to store SF-12
responses and SF-6D profiles, respectively.

`SF12` vectors are created with `SF12()`:

``` r
library(SF6Dvalues)
SF12(Q1 = 3:5, Q2 = 1:3, Q3 = 1:3, Q4 = 1:3, Q5 = 1:3, Q6 = 1:3,
     Q7 = 1:3, Q8 = 1:3, Q9 = 1:3, Q10 = 1:3, Q11 = 1:3, Q12 = 1:3)
#> <SF12[3]>
#> [1] 311111111111 422222222222 533333333333
```

SF-12 questions can be labelled either Q1, Q2, …, Q12 as above, or Q1,
Q2a, Q2b, …, Q7:

``` r
SF12(Q1 = 3:5, Q2a = 1:3, Q2b = 1:3, Q3a = 1:3, Q3b = 1:3, Q4a = 1:3,
     Q4b = 1:3, Q5 = 1:3, Q6a = 1:3, Q6b = 1:3, Q6c = 1:3, Q7 = 1:3)
#> <SF12[3]>
#> [1] 311111111111 422222222222 533333333333
```

`SF6D` vectors can be constructed from SF-6D dimension levels with
`SF6D()`:

``` r
SF6D(PF = 1:3, RL = 1:3, SF = 1:3, PAIN = 1:3, MH = 1:3, VIT = 1:3)
#> <SF6D[3]>
#> [1] 111111 222222 333333
```

Or from an `SF12` vector with `as_SF6D()`:

``` r
sf12 <- SF12(Q1 = 3:5, Q2 = 1:3, Q3 = 1:3, Q4 = 1:3, Q5 = 1:3, Q6 = 1:3,
             Q7 = 1:3, Q8 = 1:3, Q9 = 1:3, Q10 = 1:3, Q11 = 1:3, Q12 = 1:3)
as_SF6D(sf12)
#> <SF6D[3]>
#> [1] 345151 244242 143333
```

Or directly from SF-12 responses with `sf6d_profile()`:

``` r
sf6d_profile(Q1 = 3:5, Q2 = 1:3, Q3 = 1:3, Q4 = 1:3, Q5 = 1:3, Q6 = 1:3,
             Q7 = 1:3, Q8 = 1:3, Q9 = 1:3, Q10 = 1:3, Q11 = 1:3, Q12 = 1:3)
#> <SF6D[3]>
#> [1] 345151 244242 143333
```

### SF-6D utility values

Utility values can be calculated either from an `SF6D` vector or
directly from SF-12 responses. At this stage, only the original Brazier
& Roberts UK population value set is available.

``` r
sf6d <- SF6D(PF = 1:3, RL = 1:3, SF = 1:3, PAIN = 1:3, MH = 1:3, VIT = 1:3)
utility(sf6d)
#> [1] 1.000 0.737 0.570
```

``` r
sf6d_utility(Q1 = 3:5, Q2 = 1:3, Q3 = 1:3, Q4 = 1:3, Q5 = 1:3, Q6 = 1:3,
             Q7 = 1:3, Q8 = 1:3, Q9 = 1:3, Q10 = 1:3, Q11 = 1:3, Q12 = 1:3)
#> [1] 0.588 0.588 0.615
```

### SF-12 Component Summary scores

The SF-12 PCS and MCS scores can be calculated from an `SF12` vector
with `PCS()` and `MCS()`:

``` r
sf12 <- SF12(Q1 = 3:5, Q2 = 1:3, Q3 = 1:3, Q4 = 1:3, Q5 = 1:3, Q6 = 1:3,
             Q7 = 1:3, Q8 = 1:3, Q9 = 1:3, Q10 = 1:3, Q11 = 1:3, Q12 = 1:3)
PCS(sf12)
#> [1] 39.16695 40.17094 42.25097
MCS(sf12)
#> [1] 32.98882 34.33071 35.60485
```

Or directly from SF-12 responses with `sf12_PCS()` and `sf12_MCS()`:

``` r
sf12_PCS(Q1 = 3:5, Q2 = 1:3, Q3 = 1:3, Q4 = 1:3, Q5 = 1:3, Q6 = 1:3,
         Q7 = 1:3, Q8 = 1:3, Q9 = 1:3, Q10 = 1:3, Q11 = 1:3, Q12 = 1:3)
#> [1] 39.16695 40.17094 42.25097
sf12_MCS(Q1 = 3:5, Q2 = 1:3, Q3 = 1:3, Q4 = 1:3, Q5 = 1:3, Q6 = 1:3,
         Q7 = 1:3, Q8 = 1:3, Q9 = 1:3, Q10 = 1:3, Q11 = 1:3, Q12 = 1:3)
#> [1] 32.98882 34.33071 35.60485
```
