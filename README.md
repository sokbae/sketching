
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sketching

<!-- badges: start -->

[![R-CMD-check](https://github.com/sokbae/sketching/workflows/R-CMD-check/badge.svg)](https://github.com/sokbae/sketching/actions)
[![codecov](https://codecov.io/gh/sokbae/sketching/branch/main/graph/badge.svg?token=D6RNLQZUJO)](https://app.codecov.io/gh/sokbae/sketching)
<!-- badges: end -->

The package “sketching” is an R package that provides a variety of
random sketching methods via random subspace embeddings Researchers may
perform regressions using a sketch of data of size *m* instead of the
full sample of size *n* for a variety of reasons. Lee and Ng (2022)
considers the case when the regression errors do not have constant
variance and heteroskedasticity robust standard errors would normally be
needed for test statistics to provide accurate inference. It is shown in
Lee and Ng (2022) that estimates using data sketched by random
projections will behave *as if* the errors were homoskedastic.
Estimation by random sampling would not have this property.

For more details, see the following papers.

-   Lee, S. and Ng, S. (2022). “Least Squares Estimation Using Sketched
    Data with Heteroskedastic Errors,” arXiv:2007.07781, accepted for
    presentation at the Thirty-ninth International Conference on Machine
    Learning (ICML 2022).
-   Lee, S. and Ng, S. (2020). “An Econometric Perspective on
    Algorithmic Subsampling,” Annual Review of Economics, 12(1): 45–80.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools") if devtools is not installed
devtools::install_github("https://github.com/sokbae/sketching")
```

## Numerical Illustration

We begin by calling the sketching package and fix the seed for
reproducibility.

``` r
library(sketching)
seed <- 220526  
set.seed(seed)  
```

### Estimating the Return to Education

To illustrate the usefulness of the package, we use the well-known
Angrist and Krueger (1991) dataset. A particular extract of their
dataset is included in the package. Specifically, we look at the
ordinary least squares (OLS) and two stage least squares (2SLS)
estimates of the return to education in columns (1) and (2) of Table IV
in their paper. The dependent variable *y* is the log weekly wages, the
covariates *X* include years of education, the intercept term and 9
year-of-birth dummies (*p*=11). Following Angrist and Krueger (1991),
the instruments *Z* are a full set of quarter-of-birth times
year-of-birth interactions (*q*=30). Their idea was that season of birth
is unlikely to be correlated with workers’ ability but can affect
educational attainment because of compulsory schooling laws. The full
sample size is *n* = 247, 199.

We now define the variables accordingly.

``` r
Y <- AK$LWKLYWGE
intercept <- AK$CNST
X_end <- AK$EDUC
X_exg <- AK[,3:11]
X <- cbind(X_exg, X_end)
Z_inst <- AK[,12:(ncol(AK)-1)]
Z <- cbind(X_exg, Z_inst)
fullsample <- cbind(Y,intercept,X)
n <- nrow(fullsample)
d <- ncol(X)
```

### How to Choose *m*

We start with how to choose *m* in this application. Lee and Ng (2020)
highlights the tension between a large *m* required for accurate
inference, and a small *m* for computation efficiency. From the
algorithmic perspective, *m* needs to be chosen as small as possible to
achieve computational efficiency. However, statistical analysis often
cares about the variability of the estimates in repeated sampling and a
larger *m* may be desirable from the perspective of statistical
efficiency. An *inference-conscious* guide *m*<sub>2</sub> can be
obtained as in Lee and Ng (2020) by targeting the power at *γ̄* of a
one-sided *t*-test for given nominal size. Alternatively, a
data-oblivious sketch size *m*<sub>3</sub> for a pre-specified
*τ*<sub>2</sub>(∞) can be used.

We focus on the data-oblivious sketch size *m*<sub>3</sub>, as it is
simpler to use. We set the target size *α* = 0.05 and the target power
*γ* = 0.8. Then, *S*<sup>2</sup>(*ᾱ*,*γ̄*) = 6.18. It remains to specify
*τ*<sub>2</sub>(∞), which can be interpreted as the value of
*t*-statistic when the sample size is really large.

### OLS Estimation Results

For OLS, we take *τ*<sub>2</sub>(∞) = 10, resulting in *m* = 15, 283
(about 6% of *n*).

``` r
# choice of m (data-oblivious sketch size)
target_size <- 0.05
target_power <- 0.8
S_constant <- (stats::qnorm(1-target_size) + stats::qnorm(target_power))^2
tau_limit <- 10
m_ols <- floor(n*S_constant/tau_limit^2) 
print(m_ols)
#> [1] 15283
```

As a benchmark, we first obtain the OLS estimate using the full sample.

``` r
ys <- fullsample[,1]
reg <- as.matrix(fullsample[,-1])
fullmodel <- lm(ys ~ reg - 1)
# use homoskedasticity-only asymptotic variance
ztest <- lmtest::coeftest(fullmodel, df = Inf)
est <- ztest[(d+1),1] 
se <- ztest[(d+1),2]
print(c(est,se))
#> [1] 0.0801594610 0.0003552066
# use heteroskedasticity-robust asymptotic variance
ztest_hc <- lmtest::coeftest(fullmodel, df = Inf, 
            vcov = sandwich::vcovHC, type = "HC0")
est_hc <- ztest_hc[(d+1),1] 
se_hc <- ztest_hc[(d+1),2]
print(c(est_hc,se_hc))
#> [1] 0.0801594610 0.0003946747
```

We now obtain the OLS estimates using a Bernoulli subsampling.

``` r
subsample <- sketch(fullsample, m_ols, method = "bernoulli")
ys <- subsample[,1]
reg <- subsample[,-1]
submodel <- lm(ys ~ reg - 1) 
# use homoskedasticity-only asymptotic variance
ztest <- lmtest::coeftest(submodel, df = Inf)
est <- ztest[(d+1),1] 
se <- ztest[(d+1),2]
print(c(est,se))
#> [1] 0.080631991 0.001443075
# use heteroskedasticity-robust asymptotic variance
ztest_hc <- lmtest::coeftest(submodel, df = Inf, 
            vcov = sandwich::vcovHC, type = "HC0")
est_hc <- ztest_hc[(d+1),1] 
se_hc <- ztest_hc[(d+1),2]
print(c(est_hc,se_hc))
#> [1] 0.080631991 0.001603487
```

As another example of random sampling, we now consider uniform sampling.

``` r
subsample <- sketch(fullsample, m_ols, method = "unif")
ys <- subsample[,1]
reg <- subsample[,-1]
submodel <- lm(ys ~ reg - 1) 
# use homoskedasticity-only asymptotic variance
ztest <- lmtest::coeftest(submodel, df = Inf)
est <- ztest[(d+1),1] 
se <- ztest[(d+1),2]
print(c(est,se))
#> [1] 0.078158594 0.001467223
# use heteroskedasticity-robust asymptotic variance
ztest_hc <- lmtest::coeftest(submodel, df = Inf, 
            vcov = sandwich::vcovHC, type = "HC0")
est_hc <- ztest_hc[(d+1),1] 
se_hc <- ztest_hc[(d+1),2]
print(c(est_hc,se_hc))
#> [1] 0.078158594 0.001630221
```

We now move to random projection schemes. First, we consider
countsketch.

``` r
subsample <- sketch(fullsample, m_ols, method = "countsketch")
ys <- subsample[,1]
reg <- subsample[,-1]
submodel <- lm(ys ~ reg - 1) 
# use homoskedasticity-only asymptotic variance
ztest <- lmtest::coeftest(submodel, df = Inf)
est <- ztest[(d+1),1] 
se <- ztest[(d+1),2]
print(c(est,se))
#> [1] 0.07818373 0.00142449
# use heteroskedasticity-robust asymptotic variance
ztest_hc <- lmtest::coeftest(submodel, df = Inf, 
            vcov = sandwich::vcovHC, type = "HC0")
est_hc <- ztest_hc[(d+1),1] 
se_hc <- ztest_hc[(d+1),2]
print(c(est_hc,se_hc))
#> [1] 0.07818373 0.00146307
```

Next, we consider Subsampled Randomized Hadamard Transform (SRHT).

``` r
subsample <- sketch(fullsample, m_ols, method = "srht")
ys <- subsample[,1]
reg <- subsample[,-1]
submodel <- lm(ys ~ reg - 1) 
# use homoskedasticity-only asymptotic variance
ztest <- lmtest::coeftest(submodel, df = Inf)
est <- ztest[(d+1),1] 
se <- ztest[(d+1),2]
print(c(est,se))
#> [1] 0.077694934 0.001417144
# use heteroskedasticity-robust asymptotic variance
ztest_hc <- lmtest::coeftest(submodel, df = Inf, 
            vcov = sandwich::vcovHC, type = "HC0")
est_hc <- ztest_hc[(d+1),1] 
se_hc <- ztest_hc[(d+1),2]
print(c(est_hc,se_hc))
#> [1] 0.077694934 0.001420585
```

For each sketching scheme, only one random sketch is drawn; hence, the
results can change if we redraw sketches. Note that the intercept term
is included in the full sample data before applying sketching methods.
This is important for random sketching schemes as the observations
across different rows are randomly combined.

Remarkably, all sketched estimates are 0.08, reproducing the full sample
estimate up to the second digit. The sketched homoskedasticity-only
standard errors are also very much the same across different methods.
The Eicker-Huber-White standard error (i.e., heteroskedasticity-robust
standard error) is a bit larger than the homoskedastic standard error
with the full sample. As expected, the same pattern is observed for
Bernoulli and uniform sampling, as these sampling schemes preserve
conditional heteroskedasticity.

### 2SLS Estimation Results

We now move to 2SLS estimation. For 2SLS, as it is more demanding to
achieve good precision, we take *τ*<sub>2</sub>(∞) = 5, resulting in
*m* = 61, 132 (about 25% of *n*).

``` r
fullsample <- cbind(Y,intercept,X,intercept,Z)
n <- nrow(fullsample)
p <- ncol(X)
q <- ncol(Z)
# choice of m (data-oblivious sketch size)
target_size <- 0.05
target_power <- 0.8
S_constant <- (qnorm(1-target_size) + qnorm(target_power))^2
tau_limit <- 5
m_2sls <- floor(n*S_constant/tau_limit^2) 
print(m_2sls)
#> [1] 61132
```

As before, we first obtain the 2SLS estimate using the full sample.

``` r
ys <- fullsample[,1]
reg <- as.matrix(fullsample[,2:(p+2)])
inst <- as.matrix(fullsample[,(p+3):ncol(fullsample)]) 
fullmodel <- ivreg::ivreg(ys ~ reg - 1 | inst - 1) 
# use homoskedasticity-only asymptotic variance
ztest <- lmtest::coeftest(fullmodel, df = Inf)
est <- ztest[(d+1),1] 
se <- ztest[(d+1),2]
print(c(est,se))
#> [1] 0.07685568 0.01504165
# use heteroskedasticity-robust asymptotic variance
ztest_hc <- lmtest::coeftest(fullmodel, df = Inf, 
            vcov = sandwich::vcovHC, type = "HC0")
est_hc <- ztest_hc[(d+1),1] 
se_hc <- ztest_hc[(d+1),2]
print(c(est_hc,se_hc))
#> [1] 0.07685568 0.01512252
```

The 2SLS estimate of the return to education is 0.769 and both types of
standard errors are almost the same.

We now consider a variety of sketching schemes.

``` r
# sketching methods for 2SLS
methods <- c("bernoulli","unif","countsketch","srht")
results_2sls <- array(NA, dim = c(length(methods),3))
for (met in 1:length(methods)){
  method <- methods[met]
    # generate a sketch
    subsample <- sketch(fullsample, m_2sls, method = method)
    ys <- subsample[,1]
    reg <- as.matrix(subsample[,2:(p+2)])
    inst <- as.matrix(subsample[,(p+3):ncol(subsample)]) 
    submodel <- ivreg::ivreg(ys ~ reg - 1 | inst - 1) 
    # use homoskedasticity-only asymptotic variance
    ztest <- lmtest::coeftest(submodel, df = Inf)
    est <- ztest[(d+1),1] 
    se <- ztest[(d+1),2]
    # use heteroskedasticity-robust asymptotic variance
    ztest_hc <- lmtest::coeftest(submodel, df = Inf, 
            vcov = sandwich::vcovHC, type = "HC0")
    est_hc <- ztest_hc[(d+1),1] 
    se_hc <- ztest_hc[(d+1),2]
  results_2sls[met,] <- c(est, se, se_hc)
}
rownames(results_2sls) <- methods
colnames(results_2sls) <- c("est", "non-robust se","robust se")
print(results_2sls)
#>                    est non-robust se  robust se
#> bernoulli   0.08090466    0.02358229 0.02362023
#> unif        0.05960437    0.02412623 0.02467241
#> countsketch 0.10289271    0.02297033 0.02615567
#> srht        0.08491405    0.02403900 0.02412490
```

The sketched 2SLS estimates vary more than the sketched OLS estimates,
reflecting that the 2SLS estimates are less precisely estimated than the
OLS estimates. As in the full sample case, both types of standard errors
are similar across all sketches for 2SLS.

## References

-   Angrist, J. D., and A. B. Krueger (1991). “Does Compulsory School
    Attendance Affect Schooling and Earnings?” Quarterly Journal of
    Economics 106, no. 4 (1991): 979–1014.
    <https://doi.org/10.2307/2937954>.

-   Lee, S. and Ng, S. (2022). “Least Squares Estimation Using Sketched
    Data with Heteroskedastic Errors,”
    [arXiv:2007.07781](https://arxiv.org/abs/2007.07781), accepted for
    presentation at the Thirty-ninth International Conference on Machine
    Learning (ICML 2022).

-   Lee, S. and Ng, S. (2020). “An Econometric Perspective on
    Algorithmic Subsampling,” Annual Review of Economics, 12(1): 45–80.
    <https://doi.org/10.1146/annurev-economics-022720-114138>
