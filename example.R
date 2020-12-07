
# This script produces a simulated simplified Labour Force Survey (LFS).
#
# First a population and their answers are simulated.
#
# Hereafter missing values are introduced randomly to the answers of employed and hours
# in order to simulate a partial non-response scenario.
#
# Lastly the result is saved in a csv file.


library(tidyverse)

N = 500

set.seed(500)

x <- tibble::tibble(
  id = seq_len(length.out = N),
  age = sample(x = 18:90, size = N, replace = TRUE, prob = c(rep(0.7/40, 40), rep(0.3/33, 33))),
  gender = sample(x = c("M","F"), size = N, replace = TRUE),
  region = sample(x = c("N","W","S","E"), size = N, replace = TRUE, prob = c(0.2,0.4,0.3,0.3))
) %>%
  mutate(
    employed = if_else(age < 70,
                       sample(x = 0:1, size = N, replace = TRUE, prob = c(0.8,0.2)),
                       sample(x = 0:1, size = N, replace = TRUE, prob = c(0.1,0.9))),
    hours = if_else(employed == 1,
                    sample(x = 20:40, size = N, replace = TRUE, prob = c(rep(0.3/20,20),0.7)),
                    NA_integer_)
  )

x[c("employed","hours")] <- missForest::prodNA(x[c("employed","hours")], noNA = 0.1)

readr::write_csv(x = x, file = "example.csv", na = "")

# Check file is correct

readr::read_csv("example.csv") %>%
  mutate_all(is.na) %>%
  summary()
