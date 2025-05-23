# Try to run the models Timo ran in the TR_pilot_explore.R script but with the gam function
# NOTE: My GAM knowledge is very limited, so probably lots of errors in the script

if (!require("pacman")) install.packages("pacman")
pacman::p_load(mgcv, ggplot2, tidybayes, tidyverse, dplyr, truncnorm, lme4, faux, scam)

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


# Try GAM (not brms) 

xmdl_gam <- gam(response_f_prop ~ delta_f_raw + s(delta_t, k = 5, by = delta_f_raw),
                method = "REML",
                data = xdata)

summary(xmdl_gam)


# Try to predict data from the model
pred_dat <- predict(xmdl_gam, type = 'lpmatrix')
beta <- coef(xmdl_gam)
preds <- as.vector(pred_dat %*% beta)

# Store the predicted values in data
xdata$pred <- preds

# Plot predicted data
# 
xdata_agg <- xdata |> 
  group_by(delta_f, delta_t, participant, stimuli_type) |> 
  summarise(pred = mean(pred, na.rm = TRUE))

ggplot(data = xdata, 
       aes(x = delta_t,
           y = pred,
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

# Doesn't look as nice as the plot with brms though

# Now try this for GAM w. splines

xmdl_mpi <- gam(response_f_prop ~ delta_f_raw + s(delta_t, bs = "mpi", k = 4, by = delta_f_raw),
                method = "REML",
                data = xdata)

summary(xmdl_mpi)
plot(xmdl_mpi)
# This gives the same error Timo got using brm....

# Predict data
# But immediately get an error here, why??
pred_dat <- predict(xmdl_mpi, type = 'lpmatrix')
beta <- coef(xmdl_gam)
preds <- as.vector(pred_dat %*% beta)

xdata$pred_mpi <- preds


