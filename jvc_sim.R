library("tidyverse")
library("brms")
library("tidybayes")

dat <- read_csv("./data/merged_results.csv") |> 
  filter(delta_f != 0) |> 
  mutate(response_f = as.ordered(response_f))

glimpse(dat)

dat$response_f |> unique()


dat |> 
  ggplot() + 
  aes(x = delta_t, y = response_f) + 
  facet_grid(. ~ delta_f) + 
  geom_point() 

