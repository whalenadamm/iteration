Writing functions
================
Adam Whalen
11/9/2020

## Do something simple

First: z-scores

``` r
x_vec = rnorm(30, mean = 5, sd = 3)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1]  0.93943285  1.99693824  0.84119084  0.07245942 -0.13989071  0.94467568
    ##  [7] -0.75474134 -3.38750838 -0.86631513  0.69560962 -1.07289551  0.70884693
    ## [13] -0.38329763  0.17438418 -0.71541267 -0.22358754 -0.26316999 -0.13438563
    ## [19]  0.64009940 -0.56823853 -0.40347761 -0.54282908 -1.10919852  0.35654669
    ## [25] -0.43270397  1.05976417  1.07330531 -0.05745919  0.26194950  1.28990860

I want a function that computes z-scores.

``` r
z_scores = function(x) {
  
  z = (x - mean(x)) / sd(x)
  
  return(z)
  
}

z_scores(x_vec)
```

    ##  [1]  0.93943285  1.99693824  0.84119084  0.07245942 -0.13989071  0.94467568
    ##  [7] -0.75474134 -3.38750838 -0.86631513  0.69560962 -1.07289551  0.70884693
    ## [13] -0.38329763  0.17438418 -0.71541267 -0.22358754 -0.26316999 -0.13438563
    ## [19]  0.64009940 -0.56823853 -0.40347761 -0.54282908 -1.10919852  0.35654669
    ## [25] -0.43270397  1.05976417  1.07330531 -0.05745919  0.26194950  1.28990860

Try my function on some other things\!

``` r
z_scores(3)
```

    ## [1] NA

``` r
z_scores("my name is adam")
```

    ## Warning in mean.default(x): argument is not numeric or logical: returning NA

    ## Error in x - mean(x): non-numeric argument to binary operator

``` r
z_scores(mtcars)
```

    ## Warning in mean.default(x): argument is not numeric or logical: returning NA

    ## Error in is.data.frame(x): 'list' object cannot be coerced to type 'double'

``` r
z_scores(c(TRUE, TRUE, FALSE, TRUE))
```

    ## [1]  0.5  0.5 -1.5  0.5

Better make some adjustments to my function.

``` r
z_scores = function(x) {
  
  if (!is.numeric(x)) {
    stop("Input must be numeric")
  }
  
  if (length(x) < 3) {
    stop("Input must have at least 3 numbers")
  }
  
  z = (x - mean(x)) / sd(x)
  
  return(z)
  
}

z_scores(x_vec)
```

    ##  [1]  0.93943285  1.99693824  0.84119084  0.07245942 -0.13989071  0.94467568
    ##  [7] -0.75474134 -3.38750838 -0.86631513  0.69560962 -1.07289551  0.70884693
    ## [13] -0.38329763  0.17438418 -0.71541267 -0.22358754 -0.26316999 -0.13438563
    ## [19]  0.64009940 -0.56823853 -0.40347761 -0.54282908 -1.10919852  0.35654669
    ## [25] -0.43270397  1.05976417  1.07330531 -0.05745919  0.26194950  1.28990860
