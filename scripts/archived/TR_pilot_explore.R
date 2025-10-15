## Title: Exploring ManyTones Pilot Data
## Description: Exploring patterns and modelling attempts with GAMs/scams
## Authors: Timo B. Roettger
## Date: 18th May 2025

## Set up
# Nifty code using the pacman package
# it checks if the packages specified below are installed, if not, they will be installed, if yes, they will be loaded
if (!require("pacman")) install.packages("pacman")
pacman::p_load(brms, ggplot2, tidybayes, viridis, tidyverse, dplyr, truncnorm, lme4, faux, scam)

# Set the current working directory to the one where this file is
current_working_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(current_working_dir)

# Load pilot data
xdata <- read_csv("../data/merged_results.csv")

## Wrangle 

# Change variables to few variations
xdata <- xdata |> 
  filter(stimuli_direction != 0) |> 
         # make delta_f positive / negative
  mutate(delta_f_raw = ifelse(stimuli_direction == "p", delta_f, delta_f * -1),
         # make response_f relative to baseline 150hz      
         response_f_rel = response_f - 150,
         # make response_f absolute without sign differences
         response_f_rel_abs = abs(response_f_rel),
         # relative to target
         response_f_prop = response_f_rel / delta_f,
         # calculate actual f0 of presented tonal onset
         presented_f = 150 + delta_f_raw
         )


## Visualize

xdata_agg <- xdata |> 
  group_by(delta_f_raw, delta_t, participant, stimuli_type) |> 
  summarise(response_f_rel = mean(response_f_rel, na.rm = TRUE))
  

# Plot perceived f0 relative to presented f0
ggplot(data = xdata, 
       aes(x = delta_t,
           y = response_f_rel,
           color = delta_f_raw)) + 
  geom_jitter(data = xdata_agg,
              width = 1,
              height = 1,
              alpha = 0.2) +
  facet_grid(. ~ delta_f_raw) + 
  # add a quick and dirty monotonic spline smooth (increasing)
  geom_smooth(data = xdata ,
              method = "gam",
              lwd = 2,
              formula = y ~ s(x, k = 5)) +
  # geom_smooth(data = xdata |> filter(delta_f_raw > 0),
  #             method = "scam",
  #             lwd = 2,
  #             formula = y ~ s(x, k = 5, bs = "mpi")) +
  # geom_smooth(data = xdata |> filter(delta_f_raw < 0),
  #             method = "scam",
  #             lwd = 2,
  #             formula = y ~ s(x, k = 5, bs = "mpd")) +
  theme_minimal()


## Model
# Trying a simple gam without constraints on shape
priors_gam = c(prior(normal(150, 20), class = Intercept),
               prior(normal(0, 10), class = b),
               prior(student_t(3, 0, 10), class = sds),
               prior(normal(0, 10), class = sigma))

xmdl_gam <- brm(response_f ~ delta_f_raw + s(delta_t, k = 5, by = delta_f_raw),
                prior = priors_gam,
                chains = 4,
                cores = 4,
                seed = 1234,
                file  = "../models/xmdl_gam.RDS",  
                control = list(adapt_delta = 0.9, max_treedepth = 13),
                backend = "cmdstanr",
                data = xdata)

# quick and dirty look
conditional_effects(xmdl_gam)
# look as expected

# pp_check()
pp_check(xmdl_gam) # not too bad - symmetric and "unimodal", but due to response categories not smooth

# prepare model predictions
new <- expand.grid(delta_t = seq(40,120,5),
                   delta_f_raw = unique(xdata$delta_f_raw))

new <- cbind(new, fitted(xmdl_gam, newdata = new))

# plot model predictions
new |> 
ggplot(aes(x = delta_t,
           y = Estimate,
           color = delta_f_raw)) + 
  geom_hline(yintercept = unique(xdata$presented_f),
             col = c("#55AA84", "#79C66E", "#3F5187", "#FAD155",
                     "#3F718B", "#432E77",  "#B5DB54", "#3F1250"),
             lty = "dashed",
             alpha = 0.5) +
  geom_ribbon(aes(ymin = Q2.5, 
                  ymax = Q97.5,
                  group = delta_f_raw),
              fill = "grey",
              color = NA,
              alpha = 0.5) +
  geom_line(aes(group = delta_f_raw),
            lwd = 1) +
  scale_color_viridis() +
  scale_y_continuous(breaks = unique(xdata$presented_f)) +
  theme_minimal()

# almost linear with a slight bend towards asymptote 

## scam

# please run predict-mpi.r before trying to predict

# using scam for monotonic splines?
xmdl_mpi <- brm(response_f_prop_abs ~ delta_f + s(delta_t, bs = "mpi", k = 4, by = delta_f),
                prior = priors_gam,
                chains = 4,
                cores = 4,
                seed = 1234,
                file  = "models/xmdl_mpi.RDS",  
                control = list(adapt_delta = 0.99, max_treedepth = 13),
                backend = "cmdstanr",
                data = xdata)
  
# prepare model predictions
new_mpi <- expand.grid(delta_t = seq(40,120,5),
                       delta_f = unique(xdata$delta_f))

# breaks down here "Error in X %*% object$diagRP : non-conformable arguments"
new_mpi <- cbind(new_mpi, fitted(xmdl_mpi, newdata = new_mpi))
# breaks down here "Error in object$m + 1 : non-numeric argument to binary operator"
new_mpi <- cbind(new_mpi, Predict.matrix.mpi.smooth(xmdl_mpi, data = new_mpi))

