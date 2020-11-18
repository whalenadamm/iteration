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
    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -2.20633 -0.78383 -0.14466 -0.08359  0.57176  2.04266

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
    b = rnorm(30, mean = 0, sd = 5),
    c = rnorm(40, mean = 10, sd = .2),
    d = rnorm(20, mean = -3, sd = 1)
)
```

``` r
list_norm[[1]]
```

    ##  [1] 2.986981 1.784826 3.239378 3.147403 2.533434 3.423381 4.022170 2.550963
    ##  [9] 1.443680 3.700050 2.808279 3.068093 2.203805 1.735714 4.085275 3.073507
    ## [17] 1.575012 2.453860 4.397533 4.000998

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
    ## 1  2.91 0.879

``` r
mean_and_sd(list_norm[[2]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.317  5.45

``` r
mean_and_sd(list_norm[[3]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.0 0.234

``` r
mean_and_sd(list_norm[[4]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.96  1.09

That’s a little tedious. Let’s use a `for` loop instead.

``` r
output = vector("list", length = 4)

for(i in 1:4) {
  
  output[[i]] = mean_and_sd(list_norm[[i]])
  
}
```

## Let’s try `map`\!

``` r
map(list_norm, mean_and_sd)
```

    ## $a
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.91 0.879
    ## 
    ## $b
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.317  5.45
    ## 
    ## $c
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.0 0.234
    ## 
    ## $d
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.96  1.09

What about a different function?

``` r
output = map(list_norm, IQR)
```

``` r
output = map_dbl(list_norm, median)
```

``` r
output = map_df(list_norm, mean_and_sd, .id = "input")
```

## List columns\!

``` r
listcol_df = 
  tibble(
    name = c("a", "b", "c", "d"),
    samp = list_norm
  )
```

``` r
listcol_df %>% pull(name)
```

    ## [1] "a" "b" "c" "d"

``` r
listcol_df %>% pull(samp)
```

    ## $a
    ##  [1] 2.986981 1.784826 3.239378 3.147403 2.533434 3.423381 4.022170 2.550963
    ##  [9] 1.443680 3.700050 2.808279 3.068093 2.203805 1.735714 4.085275 3.073507
    ## [17] 1.575012 2.453860 4.397533 4.000998
    ## 
    ## $b
    ##  [1]   4.8052343   5.7130127  -2.9822278  -7.2101019  -0.1318959   4.2715681
    ##  [7] -10.0653495   9.2527991  -6.6450118   5.3291652  -6.0410388  -7.0956594
    ## [13]  -5.1421221   0.2716523   0.6958943  -3.1744882  10.5934596   5.1204295
    ## [19]   3.5406922  -0.1154294   1.1228586   3.4621150   5.9549839   3.9120804
    ## [25]  -2.5985104   4.0804394  -0.5824596   5.4961236  -5.6922127  -6.6419170
    ## 
    ## $c
    ##  [1] 10.103753 10.263443 10.240986  9.674510  9.682267  9.935034 10.158941
    ##  [8]  9.894427 10.041814 10.186451  9.990405 10.514586 10.018662 10.207303
    ## [15]  9.690079  9.988674 10.235246  9.823556  9.967659  9.934815  9.995643
    ## [22] 10.155007  9.957073 10.115922 10.543598  9.992993  9.445701 10.033790
    ## [29] 10.235670 10.055247 10.193792  9.944707 10.044772 10.190118  9.787210
    ## [36] 10.419628  9.634349  9.976663  9.674015 10.041855
    ## 
    ## $d
    ##  [1] -3.8019766 -2.1118662 -0.8227156 -3.1409468 -3.3090724 -0.7448663
    ##  [7] -4.4329720 -3.1217332 -3.9521741 -3.6592205 -4.3478897 -1.7983170
    ## [13] -3.0818453 -3.5230548 -1.5779577 -2.6247455 -2.3803175 -3.6885683
    ## [19] -4.0445332 -3.0055741

``` r
listcol_df %>% 
  filter(name == "a")
```

    ## # A tibble: 1 x 2
    ##   name  samp        
    ##   <chr> <named list>
    ## 1 a     <dbl [20]>

Let’s try some operations.

``` r
listcol_df$samp[[1]]
```

    ##  [1] 2.986981 1.784826 3.239378 3.147403 2.533434 3.423381 4.022170 2.550963
    ##  [9] 1.443680 3.700050 2.808279 3.068093 2.203805 1.735714 4.085275 3.073507
    ## [17] 1.575012 2.453860 4.397533 4.000998

``` r
mean_and_sd(listcol_df$samp[[1]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.91 0.879

``` r
mean_and_sd(listcol_df$samp[[2]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.317  5.45

Can I just… map?

``` r
map(listcol_df$samp, mean_and_sd)
```

    ## $a
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.91 0.879
    ## 
    ## $b
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.317  5.45
    ## 
    ## $c
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.0 0.234
    ## 
    ## $d
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.96  1.09

So…can I add a list column?

``` r
listcol_df %>% 
  mutate(
    summary = map(samp, mean_and_sd),
    medians = map_dbl(samp, median)
  )
```

    ## # A tibble: 4 x 4
    ##   name  samp         summary          medians
    ##   <chr> <named list> <named list>       <dbl>
    ## 1 a     <dbl [20]>   <tibble [1 × 2]>   3.03 
    ## 2 b     <dbl [30]>   <tibble [1 × 2]>   0.484
    ## 3 c     <dbl [40]>   <tibble [1 × 2]>  10.0  
    ## 4 d     <dbl [20]>   <tibble [1 × 2]>  -3.13
