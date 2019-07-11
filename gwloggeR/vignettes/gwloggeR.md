``` r
library(gwloggeR)
```

Outliers
========

Outliers are single measurements that are considered very unlikely to occur.

`detect_outliers(x)`
--------------------

This function take in a vector of datapoints and returns a boolean vector of outlier indicators:

``` r
x <- c(1000:1005, 975)
x
#> [1] 1000 1001 1002 1003 1004 1005  975
detect_outliers(x)
#> [1] FALSE FALSE FALSE FALSE FALSE FALSE  TRUE
```

So why is 975 considered an outlier? We can understand the decision procedure better if we add some comprehensive plots and extra output:

``` r
detect_outliers(x, plot = TRUE, verbose = TRUE)
```

![](C:/Users/Davor/AppData/Local/Temp/Rtmpia4G5c/preview-4ab85df12be8.dir/gwloggeR_files/figure-markdown_github/unnamed-chunk-3-1.png)

    #> [1] FALSE FALSE FALSE FALSE FALSE FALSE  TRUE
    #> attr(,"class")
    #> [1] "logical"  "Outliers"
    #> attr(,"x.mean")
    #> [1] 1002
    #> attr(,"x.sd")
    #> [1] 2.9652
    #> attr(,"sigma.reject")
    #> [1] 3.971425
    #> attr(,"alpha")
    #> [1] 5e-04
    #> attr(,"type")
    #> [1] "two.sided"
    #> attr(,"fun.density")
    #> function (x) 
    #> dnorm(x, x.mean, x.sd)
    #> <bytecode: 0x000000001461d708>
    #> <environment: 0x000000001477fae0>
    #> attr(,"cutpoints")
    #> [1]  990.2239 1013.7761

The underlying assumption for outlier detection is normality of *x*<sub>*i*</sub>. Top left plot shows the histogram of the data points. The green curve is the best-fit normal distribution, based on robust estimates of *μ* and *σ*. The cutoff points are signified by red vertical lines. The top right is the QQ-plot with cutoff points as horizontal lines. The bottom plot is the sequential data, with outliers in red.

### Estimation of cutoff lines

How do we determine where to place the red cutoff lines? Well, for start, we want to minimize false positives. Suppose that we take 100 random points from a standard normal distribution. If we place the cutoff lines at *c* = ±1.96 then we expect to find 5 outliers on average. But these are not real outliers! Remeber our assumption that all the points in this set are from the standard normal distribution. So we want to set the cutoff lines at the optimal place: not too small, so we don't flag points as outliers incorrectly, but also not too big either, because in that case we might miss real outliers.

Let us formalize the above intuition. Assume that **x** consists of *n* independent and identically distributed datapoints (*x*<sub>1</sub>, *x*<sub>2</sub>, …, *x*<sub>*n*</sub>) taken from a standard normal distribution with fixed *μ* = 0 and *σ* = 1. Now we choose some *c* as the cutoff line. We can then calculate the probability of at least one outlier detected in the `detect_outliers(x)` process:

*P*(|*x*<sub>1</sub>|&gt;*c* ∨ |*x*<sub>2</sub>|&gt;*c* ∨  …  ∨ |*x*<sub>*n*</sub>|&gt;*c*)=*α*

That probability is *α*. We want to set *c* such that *α* is low. How low? Well, if we set it to 1/2000 then it means that we will detect one or more outliers in 1 out of 2000 times we run `detect_outliers(x)`. Obviously, on average, this 1 time we will be wrong, but in 1999 of the other cases we will not. This seems a good value for a production setting.

Ok, so now that we know our optimal *α*, how do we compute *c*? We first massage a bit the above equation:

$$
P(|x\_1| &gt; c \\lor |x\_2| &gt; c \\lor \\; \\dots \\; \\lor |x\_n| &gt; c) = \\alpha \\\\
1 - P(|x\_1| \\le c \\land |x\_2| \\le c \\land \\; \\dots \\; \\land |x\_n| \\le c) = \\alpha \\\\
1 - \\prod\_i \\Phi(|x\_i| \\le c) = \\alpha \\\\
1 - \\left\[ 1 - 2\\Phi(x &lt; -c) \\right\]^n = \\alpha
$$

Now solving for *c* is easy:

$$
c = -\\Phi^{-1} \\left( \\frac{1-(1-\\alpha)^\\frac{1}{n}}{2} \\right)
$$

where *Φ*<sup>−1</sup>(⋅) is the standard normal quantile function.

#### Example

This is how *c* behaves in function of *n* with fixed *α* = 1/2000.

``` r
ggplot2::ggplot(data = data.frame(n = 5:10000), mapping = ggplot2::aes(x = n)) + 
  ggplot2::stat_function(fun = function(n) -qnorm((1-(1-1/2000)^(1/n))/2), col = 'black') + 
  ggplot2::theme_light() + ggplot2::ylab('c')
```

![](C:/Users/Davor/AppData/Local/Temp/Rtmpia4G5c/preview-4ab85df12be8.dir/gwloggeR_files/figure-markdown_github/unnamed-chunk-4-1.png)

Note that this function is implemented in `gwloggeR:::c.optimal(alpha, n)`. So as long as we set *c* to the optimal value we make sure that we will make a wrong `detect_outliers(x)` run (i.e. detect falsely one or more ouliers) in 1/2000 of time.

``` r
# e.g. optimal c for 5000 points:
gwloggeR:::c.optimal(alpha = 1/2000, n = 5000, type = "two.sided")
#> [1] 5.326678
```

#### Simulation

TODO....

### Estimation of *μ* and *σ*

In calculating *c* we assumed *x*<sub>*i*</sub> being normal with *μ* = 0 and *σ* = 1. To make the above also work for *y*<sub>*i*</sub> from any normal distribution, we need to estimate *μ* and *σ*. Once we have the estimates, then we can standardize *y*<sub>*i*</sub> using (*y*<sub>*i*</sub> − *μ*)/*σ* = *x*<sub>*i*</sub> and use the previous results on *x*<sub>*i*</sub>.

Mean and square root of variance give the most efficient estimators for *μ* and *σ* as long as ∀*i* : *y*<sub>*i*</sub> ∼ 𝒩(*μ*, *σ*<sup>2</sup>) and mutually independent. Under influence of real outliers, these estimators get easily biased. (cf. Leys et al. 2013) So we need more robust estimators for *μ* and *σ*.

In case of outliers, a way to measure robustness is the breakdown point. The maximum attainable breakdown point is 50%, meaning that 50% of observations can be replaced by arbitrary large numbers, without breaking the estimator. For *μ*, the obvious choice is the median. For *σ* it is the median absolute deviation (MAD). They both have a 50% breakdown point. A problem with the latter is its efficiency (cf. simuation). There exist more efficient *σ*-estimators than the MAD. For example the Q-estimator (cf. Rousseeuw and Croux 1993). Currently, MAD suffices because we usualy have lots of data (*n* &gt; 5000) so efficiency suffers less.

`detect_outliers(x, apriori("air pressure", units = "cmH2O"))`
--------------------------------------------------------------

We can improve the outlier detection by providing *a-priori* information about **x**. For example:

``` r
x <- c(990:999)
detect_outliers(x, apriori = apriori("air pressure", "cmH2O"), plot = TRUE)
```

![](C:/Users/Davor/AppData/Local/Temp/Rtmpia4G5c/preview-4ab85df12be8.dir/gwloggeR_files/figure-markdown_github/unnamed-chunk-6-1.png)

    #>  [1]  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE

Top left is again the histogram of **x**. But the green density this time is not a robust normal estimate based on **x**. Instead it is the hardcoded *a-priori* density distribution of air pressure (*c**m**H*<sub>2</sub>*O*) in Belgium. Given the 10 points and assuming that we want to detect falsely one or more outliers in 1 of 2000 tests, we set the red cutoffs appropriately. This results in first 4 points being identified as outliers.

`detect_outliers(x, apriori("hydrostatic pressure", units = "cmH2O"))`
----------------------------------------------------------------------

Hydrostatic pressure incorporates *a-priori* information about air pressure as the lower limit. The upper limit is determined with the `detect_outliers(x)` approach (i.e. without *a-priori* information). The following example explains:

``` r
x <- c(985, 1070:1077, 1100)
detect_outliers(x, apriori = apriori("hydrostatic pressure", "cmH2O"), plot = TRUE)
```

![](C:/Users/Davor/AppData/Local/Temp/Rtmpia4G5c/preview-4ab85df12be8.dir/gwloggeR_files/figure-markdown_github/unnamed-chunk-7-1.png)

    #>  [1]  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE

The top left density is now bimodal. The first mode is just the *a-priori* air pressure, and the second is the robust estimate of the datapoints themselves, excluding the left outliers, using `detect_outliers(x)`. The reasoning is that hydrostatic pressure should never be higher than air pressure. Thus, since 985 *c**m**H*<sub>2</sub>*O* is very unlikely given our *a-priori* air pressure information, it is considered an outlier. Subsequently, robust estimates of *μ* and *σ* are made based on remaining **x**, from which 1100 *c**m**H*<sub>2</sub>*O* also seems very unlikely on the right side.

Levelshifts
===========

To be continued...

References
==========

Leys, Christophe, Christophe Ley, Olivier Klein, Philippe Bernard, and Laurent Licata. 2013. “Detecting Outliers: Do Not Use Standard Deviation Around the Mean, Use Absolute Deviation Around the Median.” *Journal of Experimental Social Psychology* 49 (4): 764–66. doi:[10.1016/j.jesp.2013.03.013](https://doi.org/10.1016/j.jesp.2013.03.013).

Rousseeuw, Peter J., and Christophe Croux. 1993. “Alternatives to the Median Absolute Deviation.” *Journal of the American Statistical Association* 88 (424). Taylor & Francis: 1273–83. doi:[10.1080/01621459.1993.10476408](https://doi.org/10.1080/01621459.1993.10476408).
