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

    ##  [1] -0.38662304  1.33022659  0.35151034 -0.15778345  1.12036695 -0.09094710
    ##  [7] -0.03376817  1.07061102 -0.05448081 -0.18096117 -0.41499840  0.19982652
    ## [13]  0.20523421 -0.32780575  0.42647077 -0.31740003 -1.55091927 -2.05000911
    ## [19] -0.49248821 -0.41367225 -0.98836635 -0.20877041 -0.97403179  0.99827646
    ## [25]  3.02442403  0.16366768 -0.97528682  1.19789491  0.80982821 -1.28002554

I want a function that computes z-scores.

``` r
z_scores = function(x) {
  
  z = (x - mean(x)) / sd(x)
  
  return(z)
  
}

z_scores(x_vec)
```

    ##  [1] -0.38662304  1.33022659  0.35151034 -0.15778345  1.12036695 -0.09094710
    ##  [7] -0.03376817  1.07061102 -0.05448081 -0.18096117 -0.41499840  0.19982652
    ## [13]  0.20523421 -0.32780575  0.42647077 -0.31740003 -1.55091927 -2.05000911
    ## [19] -0.49248821 -0.41367225 -0.98836635 -0.20877041 -0.97403179  0.99827646
    ## [25]  3.02442403  0.16366768 -0.97528682  1.19789491  0.80982821 -1.28002554

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

    ##  [1] -0.38662304  1.33022659  0.35151034 -0.15778345  1.12036695 -0.09094710
    ##  [7] -0.03376817  1.07061102 -0.05448081 -0.18096117 -0.41499840  0.19982652
    ## [13]  0.20523421 -0.32780575  0.42647077 -0.31740003 -1.55091927 -2.05000911
    ## [19] -0.49248821 -0.41367225 -0.98836635 -0.20877041 -0.97403179  0.99827646
    ## [25]  3.02442403  0.16366768 -0.97528682  1.19789491  0.80982821 -1.28002554

## Multiple Outputs

``` r
mean_and_sd = function(x) {
  
  if (!is.numeric(x)) {
    stop("Input must be numeric")
  }
  
  if (length(x) < 3) {
    stop("Input must have at least 3 numbers")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)
  
  tibble(
    mean = mean_x,
    sd = sd_x
  )
  
}
```

Check that the function works.

``` r
x_vec = rnorm(1000)
mean_and_sd(x_vec)
```

    ## # A tibble: 1 x 2
    ##      mean    sd
    ##     <dbl> <dbl>
    ## 1 -0.0157  1.00

## Multiple inputs

I’d like to do this with a function:

``` r
sim_data = 
    tibble(
      x = rnorm(n = 100, mean = 4, sd = 3)
    )

sim_data %>% 
  summarize(
    mean = mean(x),
    sd = sd(x)
  )
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.45  2.80

``` r
sim_mean_sd = function(samp_size, mu = 3, sigma = 4) {
  
  sim_data = 
    tibble(
      x = rnorm(n = samp_size, mean = mu, sd = sigma)
    )

sim_data %>% 
  summarize(
    mean = mean(x),
    sd = sd(x)
  )
  
}

sim_mean_sd(samp_size = 100, mu = 6, sigma = 3)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  6.61  3.35

``` r
sim_mean_sd(samp_size = 100, sigma = 3, mu = 6)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  6.15  3.18

``` r
sim_mean_sd(samp_size = 100)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.08  3.86
