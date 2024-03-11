library(ggplot2)
library(tibble)
library(dplyr)

plot_theme <- theme_minimal() +
  theme(
    plot.title = element_text(size = 20),
    plot.subtitle = element_text(size = 14),
    axis.title = element_text(size = 16),
    axis.text = element_text(size=14)
  )

theme_set(plot_theme)

distribution_options <- 
  c(
    "Normal (5,10)"="norm",
    "Beta (4,9)"="beta",
    "Chi-squared (1)" = "chisq",  
    "Uniform (1,4)"="unif",
    "Gamma(2,4)"="gamma"
  )

replicate_count <- c(
  "30"=30,
  "100"=100,
  "500"=500,
  "1000"=1000
)

ci_levels <- c(
  "80%"=1.282,
  "90%"=1.64,
  "95%"=1.96,
  "99%"=2.576
)


salt_samples <- tibble(ID = 1:32, na_ppm = c(
  13.0, 18.5, 16.4, 14.8, 19.4, 17.3, 23.2, 24.9,
  20.8, 19.3, 18.8, 23.1, 15.2, 19.9, 19.1, 18.1,
  25.1, 16.8, 20.4, 17.4, 25.2, 23.1, 15.3, 19.4, 
  16.0, 21.7, 15.2, 21.3, 21.5, 16.8, 15.6, 17.6
))

# generate the bowl with 900 red and 1500 white balls
bowl <- rep(c("Red", "White"), times=c(900, 1500)) |> 
  # randomise the bowl
  sample() |>
  # create a tibble that will be easier to work with using tidyverse functions.
  tibble(ball_ID = 1:2400, colour=_)

################################################################################
### FUNCTION DEFINIITIONS
################################################################################

generate_samples <- function(data, size=50, reps=1) {
  # coerce the tibble to have specific fields in the specified order.
  sample_set <- tibble(ball_ID=c(0), colour=c(''), replicate=c(0))
  
  for(i in 1:reps) {
    # draw of new sample
    sample_draw <- data |> 
      sample_n(size = size) |> 
      bind_cols(replicate=i)
    
    # add new sample to the master tibble  
    sample_set <- bind_rows(sample_set, sample_draw)
  }
  
  sample_set |>
    # remove the dummy row and then rearrange and tibble and add grouping.
    filter(replicate != 0) |> 
    relocate(replicate, before=ball_ID) |> 
    group_by(replicate)
}

generate_dist_pdf <- function(dist = "norm") {
  X <- Y <- NULL
  if (dist == "beta") {
    X <- seq(from=0, to=1, by=0.005)
    Y <- dbeta(X, 4, 9)
  } else if (dist == "chisq") {
    X <- seq(from=0, to=5, by=0.01)
    Y <- dchisq(X, df=1)
  } else if (dist == "gamma") {
    X <- seq(from=0, to=16, by=0.04)
    Y <- dgamma(X, shape=2,scale=2)
  } else if (dist == "unif") {
    X <- seq(from=0, to=5, by=0.02)
    Y <- dunif(X, min=1, max=4)
  } else {
    X <- seq(from=-25, to=35, by=0.15)
    Y <- dnorm(X, mean=5, sd=10)
  }
  
  tibble(x=X, y=Y)
}

generate_sample_dist <- function(dist="norm", size=50, reps=1) {
  
  # coerce the tibble to have specific fields in the specified order.
  sample_set <- tibble(replicate=c(0), x=c(0))
  
  for(i in 1:reps) {
    # draw of new sample
    if (dist == "beta") {
      data <- rbeta(size, 4, 9)
    } else if (dist == "chisq") {
      data <- rchisq(size, df=1)
    } else if (dist == "gamma") {
      data <- rgamma(size, shape=2,scale=2)
    } else if (dist == "unif") {
      data <- runif(size, min=1, max=4)
    } else {
      data <- rnorm(size, mean=5, sd=10)
    }
    
    sample_draw <- tibble(replicate=i, x=data)
    
    # add new sample to the master tibble  
    sample_set <- bind_rows(sample_set, sample_draw)
  }
  
  sample_set |>
    # remove the dummy row and then rearrange and tibble and add grouping.
    filter(replicate != 0) |> 
    group_by(replicate)
}

generate_boot_dist <- function(data, reps=1) {
  # coerce the tibble to have specific fields in the specified order.
  sample_set <- tibble(ID=c(0), na_ppm=c(0),replicate=c(0))
  size <- nrow(data)
  for(i in 1:reps) {
    # draw of new sample
    sample_draw <- data |> 
      sample_n(size = size, replace = TRUE) |> 
      bind_cols(replicate=i)
    
    # add new sample to the master tibble  
    sample_set <- bind_rows(sample_set, sample_draw)
  }
  
  sample_set |>
    # remove the dummy row and then rearrange and tibble and add grouping.
    filter(replicate != 0) |> 
    relocate(replicate, before=ID) |> 
    group_by(replicate)
}
