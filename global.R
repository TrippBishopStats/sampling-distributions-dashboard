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
  "10"=10,
  "100"=100,
  "500"=500,
  "1000"=1000
)

# generate the bowl with 900 red and 1500 white balls
bowl <- rep(c("Red", "White"), times=c(900, 1500)) |> 
  # randomise the bowl
  sample() |>
  # create a tibble that will be easier to work with using tidyverse functions.
  tibble(ball_ID = 1:2400, colour=_)