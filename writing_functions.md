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

    ##  [1]  0.76934110  0.33210125  1.10342523 -1.06536849  0.48487970 -0.74566613
    ##  [7] -0.07644958 -1.32922307 -0.23279051 -1.11288816  1.01724189 -0.65472391
    ## [13]  0.18117389  0.46678567 -0.85832195 -1.31759346 -1.06212031  1.62540166
    ## [19] -0.52346930 -1.68789663  1.03900569 -0.47238679  0.60819404 -1.15560682
    ## [25]  1.13053747 -0.17250758 -0.06165290  1.07769035  2.32159901  0.37128865

I want a function that computes z-scores.

``` r
z_scores = function(x) {
  
  z = (x - mean(x)) / sd(x)
  
  return(z)
  
}

z_scores(x_vec)
```

    ##  [1]  0.76934110  0.33210125  1.10342523 -1.06536849  0.48487970 -0.74566613
    ##  [7] -0.07644958 -1.32922307 -0.23279051 -1.11288816  1.01724189 -0.65472391
    ## [13]  0.18117389  0.46678567 -0.85832195 -1.31759346 -1.06212031  1.62540166
    ## [19] -0.52346930 -1.68789663  1.03900569 -0.47238679  0.60819404 -1.15560682
    ## [25]  1.13053747 -0.17250758 -0.06165290  1.07769035  2.32159901  0.37128865

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

    ##  [1]  0.76934110  0.33210125  1.10342523 -1.06536849  0.48487970 -0.74566613
    ##  [7] -0.07644958 -1.32922307 -0.23279051 -1.11288816  1.01724189 -0.65472391
    ## [13]  0.18117389  0.46678567 -0.85832195 -1.31759346 -1.06212031  1.62540166
    ## [19] -0.52346930 -1.68789663  1.03900569 -0.47238679  0.60819404 -1.15560682
    ## [25]  1.13053747 -0.17250758 -0.06165290  1.07769035  2.32159901  0.37128865

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
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 0.0233  1.01

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
    ## 1  3.61  3.01

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
    ## 1  6.26  2.43

``` r
sim_mean_sd(samp_size = 100, sigma = 3, mu = 6)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  6.28  2.92

``` r
sim_mean_sd(samp_size = 100)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.34  3.78

## Let’s review Napoleon Dynamite

``` r
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

dynamite_html = read_html(url)

review_titles = 
  dynamite_html %>%
  html_nodes(".a-text-bold span") %>%
  html_text()

review_stars = 
  dynamite_html %>%
  html_nodes("#cm_cr-review_list .review-rating") %>%
  html_text() %>%
  str_extract("^\\d") %>%
  as.numeric()

review_text = 
  dynamite_html %>%
  html_nodes(".review-text-content span") %>%
  html_text() %>% 
  str_replace_all("\n", "") %>% 
  str_trim()

reviews_p1 = tibble(
  title = review_titles,
  stars = review_stars,
  text = review_text
)
```

What about the next page of reviews?

``` r
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=2"

dynamite_html = read_html(url)

review_titles = 
  dynamite_html %>%
  html_nodes(".a-text-bold span") %>%
  html_text()

review_stars = 
  dynamite_html %>%
  html_nodes("#cm_cr-review_list .review-rating") %>%
  html_text() %>%
  str_extract("^\\d") %>%
  as.numeric()

review_text = 
  dynamite_html %>%
  html_nodes(".review-text-content span") %>%
  html_text() %>% 
  str_replace_all("\n", "") %>% 
  str_trim()

reviews_p2 = tibble(
  title = review_titles,
  stars = review_stars,
  text = review_text
)
```

…that sucks. Let’s make a function instead.

``` r
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=2"

read_page_reviews = function(url) {
  
  html = read_html(url)

  review_titles = 
    html %>%
    html_nodes(".a-text-bold span") %>%
    html_text()
  
  review_stars = 
    html %>%
    html_nodes("#cm_cr-review_list .review-rating") %>%
    html_text() %>%
    str_extract("^\\d") %>%
    as.numeric()
  
  review_text = 
    html %>%
    html_nodes(".review-text-content span") %>%
    html_text() %>% 
    str_replace_all("\n", "") %>% 
    str_trim()
  
  reviews = 
    tibble(
      title = review_titles,
      stars = review_stars,
      text = review_text
    )
  reviews
}
```

Let’s try the new function.

``` r
dynamite_url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=2"

read_page_reviews(dynamite_url)
```

    ## # A tibble: 10 x 3
    ##    title                               stars text                               
    ##    <chr>                               <dbl> <chr>                              
    ##  1 Boo                                     1 "We rented this movie because our …
    ##  2 Movie is still silly fun....amazon…     1 "We are getting really frustrated …
    ##  3 Brilliant and awkwardly funny.          5 "I've watched this movie repeatedl…
    ##  4 Great purchase price for great mov…     5 "Great movie and real good digital…
    ##  5 Movie for memories                      5 "I've been looking for this movie …
    ##  6 Love!                                   5 "Love this movie. Great quality"   
    ##  7 Hilarious!                              5 "Such a funny movie, definitely br…
    ##  8 napoleon dynamite                       5 "cool movie"                       
    ##  9 Top 5                                   5 "Best MOVIE ever! Funny one liners…
    ## 10 👍                                      5 "Exactly as described and came on …

Let’s read a few pages of reviews.

``` r
dynamite_url_base = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="

dynamite_urls = str_c(dynamite_url_base, 1:5)

dynamite_urls[1]
```

    ## [1] "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

``` r
all_reviews = 
  bind_rows(
    read_page_reviews(dynamite_urls[1]),
    read_page_reviews(dynamite_urls[2]),
    read_page_reviews(dynamite_urls[3]),
    read_page_reviews(dynamite_urls[4]),
    read_page_reviews(dynamite_urls[5])
  )
```

## Mean scoping example

``` r
f = function(x) {
  z = x + y
  z
}

x = 1
y = 2

f(x = y)
```

    ## [1] 4

## Functions as arguments

Passing a function into a function

``` r
my_summary = function(x, summ_fxn) {
  
  summ_fxn(x)
}

x_vec = rnorm(100, 3, 7)

mean(x_vec)
```

    ## [1] 2.532146

``` r
median(x_vec)
```

    ## [1] 2.713348

``` r
my_summary(x_vec, mean)
```

    ## [1] 2.532146
