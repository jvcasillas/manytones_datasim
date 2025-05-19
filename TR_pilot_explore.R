## Title: Exploring ManyTones Pilot Data
## Description: Exploring patterns and modelling attempts with GAMs
## Authors: Roettger
## Date: 18th May 2025

## Set up
# Nifty code using the pacman package
# it checks if the packages specified below are installed, if not, they will be installed, if yes, they will be loaded
if (!require("pacman")) install.packages("pacman")
pacman::p_load(brms, ggplot2, tidybayes, tidyverse, dplyr, truncnorm, lme4, faux, scam)

# Set the current working directory to the one where this file is
current_working_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(current_working_dir)

# Load pilot data
xdata <- read_csv("data/merged_results.csv")


## Wrangle 

# Change variables
xdata <- xdata |> 
  filter(stimuli_direction != 0) |> 
         # make delta_f positive / negative
  mutate(delta_f_raw = ifelse(stimuli_direction == "p", delta_f, delta_f * -1),
         # make response_f_rel relative to baseline 150hz      
         response_f_rel = response_f - 150,
         # relative to target
         response_f_prop = response_f_rel / delta_f
         )


## Visualize

xdata_agg <- xdata |> 
  group_by(delta_f, delta_t, participant, stimuli_type) |> 
  summarise(response_f_prop = mean(response_f_prop, na.rm = TRUE))
  

# Plot perceived f0 relative to presented f0
ggplot(data = xdata, 
       aes(x = delta_t,
           y = response_f_prop,
           color = delta_f)) + 
  geom_jitter(data = xdata_agg,
              width = 0.1,
              height = 0.1,
              alpha = 0.2) +
  facet_grid(. ~ delta_f) + 
  # add a quick and dirty monotonic spline smooth (increasing)
  geom_smooth(method = "scam",
              lwd = 2,
              formula = y ~ s(x, k = 5, bs = "mpi")) +
  scale_y_continuous(limits = c(-0.5,1)) +
  theme_minimal()


## Model
# Trying a simple gam without constraints on shape
priors_gam = c(prior(normal(0, 0.5), class = Intercept),
               prior(normal(0, 1), class = b),
               prior(student_t(3, 0, 2), class = sds),
               prior(normal(0, 1), class = sigma))

xmdl_gam <- brm(response_f_prop ~ delta_f + s(delta_t, k = 4, by = delta_f),
                prior = priors_gam,
                chains = 4,
                cores = 4,
                seed = 1234,
                file  = "models/xmdl_gam.RDS",  
                control = list(adapt_delta = 0.9, max_treedepth = 13),
                backend = "cmdstanr",
                data = xdata)

# quick and dirty look
conditional_effects(xmdl_gam)

# pp_check()
pp_check(xmdl_gam) # not too bad

# prepare model predictions
new <- expand.grid(delta_t = seq(40,120,5),
                   delta_f = unique(xdata$delta_f))

new <- cbind(new, fitted(xmdl_gam, newdata = new))

# plot model predictions
new |> 
ggplot(aes(x = delta_t,
           y = Estimate,
           color = delta_f)) + 
  geom_ribbon(aes(ymin = Q2.5, 
                  ymax = Q97.5),
              fill = "grey",
              color = NA,
              alpha = 0.5) +
  geom_line(lwd = 2) +
  facet_grid(. ~ delta_f) + 
  # add a quick and dirty monotonic spline smooth (increasing & concave)
  scale_y_continuous(limits = c(-0.5,1)) +
  theme_minimal()

# they are rather off, assuming ordinal relationships

## scam
# using scam for monotonic splines?
xmdl_mpi <- brm(response_f_prop ~ delta_f + s(delta_t, bs = "mpi", k = 4, by = delta_f),
                prior = priors_gam,
                chains = 4,
                cores = 4,
                seed = 1234,
                file  = "models/xmdl_mpi.RDS",  
                control = list(adapt_delta = 0.9, max_treedepth = 13),
                backend = "cmdstanr",
                data = xdata)
  
# prepare model predictions
new_mpi <- expand.grid(delta_t = seq(40,120,5),
                       delta_f = unique(xdata$delta_f))

# breaks down here "Error in X %*% object$diagRP : non-conformable arguments"
new_mpi <- cbind(new_mpi, fitted(xmdl_mpi, newdata = new))

# plot model predictions
new_mpi |> 
  ggplot(aes(x = delta_t,
             y = Estimate,
             color = delta_f)) + 
  geom_ribbon(aes(ymin = Q2.5, 
                  ymax = Q97.5),
              fill = "grey",
              color = NA,
              alpha = 0.5) +
  geom_line(lwd = 2) +
  facet_grid(. ~ delta_f) + 
  # add a quick and dirty monotonic spline smooth (increasing & concave)
  scale_y_continuous(limits = c(0,1)) +
  theme_minimal()
