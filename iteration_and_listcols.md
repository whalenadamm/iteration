Iteration and List Columns
================
Adam Whalen

## Lists

You can put anything in a list.

``` r
l = list(
  vec_numeric = 5:8,
  vec_logical = c(TRUE, TRUE, FALSE, TRUE, FALSE, FALSE),
  mat = matrix(1:8, nrow = 2, ncol = 4),
  summary = summary(rnorm(100))
)
```

``` r
l
```

    ## $vec_numeric
    ## [1] 5 6 7 8
    ## 
    ## $vec_logical
    ## [1]  TRUE  TRUE FALSE  TRUE FALSE FALSE
    ## 
    ## $mat
    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    3    5    7
    ## [2,]    2    4    6    8
    ## 
    ## $summary
    ##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
    ## -2.052636 -0.680654 -0.022991 -0.007248  0.645792  2.217677

``` r
l$vec_numeric
```

    ## [1] 5 6 7 8

``` r
l[[1]]
```

    ## [1] 5 6 7 8

``` r
l[["vec_numeric"]]
```

    ## [1] 5 6 7 8

``` r
mean(l[["vec_numeric"]])
```

    ## [1] 6.5

``` r
l[["vec_numeric"]][1:3]
```

    ## [1] 5 6 7

## `for` loop

Create a new list.

``` r
list_norm = 
  list(
    a = rnorm(20, mean = 3, sd = 1),
    a = rnorm(30, mean = 0, sd = 5),
    a = rnorm(40, mean = 10, sd = .2),
    a = rnorm(20, mean = -3, sd = 1)
)
```

``` r
list_norm[[1]]
```

    ##  [1] 3.262730 1.799779 3.296531 1.683651 3.608943 2.711505 1.706336 3.381548
    ##  [9] 2.962376 3.097830 3.079163 3.530446 2.130167 4.402541 2.038219 3.434114
    ## [17] 3.726566 2.622195 3.187205 3.372945

Pause and get my old function.

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

I can apply that function to each list element.

``` r
mean_and_sd(list_norm[[1]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.95 0.745

``` r
mean_and_sd(list_norm[[2]])
```

    ## # A tibble: 1 x 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.279  4.61

``` r
mean_and_sd(list_norm[[3]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.1 0.176

``` r
mean_and_sd(list_norm[[4]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.70 0.932

That’s a little tedious. Let’s use a `for` loop instead.

``` r
output = vector("list", length = 4)

for(i in 1:4) {
  
  output[[i]] = mean_and_sd(list_norm[[i]])
  
}
```
