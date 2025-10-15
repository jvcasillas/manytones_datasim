# ------------- Initialization ----------------------####
# load required packages
library("lme4")        # model specification / estimation
library("afex")        # anova and deriving p-values from lmer
library("broom.mixed") # extracting data from model fits 
library("faux")        # data simulation
library("tidyverse")   # data wrangling and visualisation

# ensure this script returns the same results on each run
set.seed(8675309)
faux_options(verbose = FALSE)

# We're using a dependent binomial variable, so we need to enter the expected effect size as beta
# These functions below involve conversion between probability and the logit function of probability
logit <- function(x) { log(x / (1 - x)) }
inv_logit <- function(x) { 1 / (1 + exp(-x)) }




# ------------- Data simulation ----------------------####
# Set up the custom data simulation function. This will create a single simulated dataset

# Note, these values will be altered in step 3 of the script, so for now just define them with arbitrary values

my_bin_data <- function(
    n_subj     = 100, # number of subjects, 
    n_110    =  15,  # number of items per f0 onset condition
    n_120    =  15,
    n_130    =  15,  
    n_140    =  15,  
    n_150    =  15,  
    n_160    =  15,  
    n_170    =  15,  
    n_180    =  15,  
    n_190    =  15,  
    beta_0   =  150, # intercept
    beta_1   =  10,
    beta_2   =   0,
    beta_int =   5,
    omega_0  =   1, # by-item random intercept sd
    tau_0    =   1, # by-subject random intercept sd
    tau_1    =   2, # by-subject random slope sd
    rho      =   0 # correlation between intercept and slope
) {
  
  
  # simulate a sample of items
  # This makes a dataframe, and assigns a random intercept for each item, sampled from a normal distribution with mean zero and std of omega_0
  items <- data.frame(
    item_id = 1:(n_110 + n_120 + n_130 + n_140 + n_150 + n_160 + n_170 + n_180 + n_190),
    f0Onset = rep(c("110", "120", "130", "140", "150", "160", "170", "180", "190"), c(n_110 + n_120 + n_130 + n_140 + n_150 + n_160 + n_170 + n_180 + n_190)),
    X1_i = scale(as.numeric(rep(c("110", "120", "130", "140", "150", "160", "170", "180", "190"), c(n_110 + n_120 + n_130 + n_140 + n_150 + n_160 + n_170 + n_180 + n_190)))),
    slope = rep(c("40", "60", "80", "100", "120"), each = 9, times = 3),
    X2_i = scale(as.numeric(rep(c("40", "60", "80", "100", "120"), each = 9, times = 3))),
    O_0i = rnorm(n = n_cond1 + n_cond2 + n_cond3 + n_cond4 + n_cond5 + n_cond6 + n_cond7 + n_cond8 + n_cond9, mean = 0, sd = omega_0)
  )
  
  # simulate a sample of subjects
  # This makes a dataframe which specifies for each subject the random intercept and the random slope
  subjects <- faux::rnorm_multi(
    n = n_subj, mu = 0, sd = c(tau_0, tau_1), r = rho, 
    varnames = c("T_0s", "T_1s")
  )
  subjects$subj_id <- 1:n_subj
  
  # cross subject and item IDs to simulate the data
  crossing(subjects, items)  %>%
    mutate(
      # calculate gaussian DV
      Y = beta_0 + T_0s + O_0i + ((beta_1 + T_1s) * X1_i) + ((beta_2 + T_1s) * X2_i) + ((beta_int + T_1s) * X1_i * X2_i),
      Y = plyr::round_any(Y,5)
      # So you add: beta intercept + random intercepts for item and subj + (beta of main effect + random slope) * condition
      ) %>%
    
    select(subj_id, item_id, f0Onset, slope, X1_i, X2_i, Y)
}


dat <- my_bin_data()



# Plot data
aggr <-  dat %>% 
  group_by(slope, f0Onset) %>% 
  dplyr::summarise(f0_resp = mean(Y),se=sd(Y)/sqrt(n()),
                          N = n()) %>% 
  mutate(ci = qt(.95/2 + .5, N-1) * se) %>% 
  mutate(slope = as.numeric(slope))

ggplot(data = aggr, mapping = aes(x = slope, y = f0_resp, group = f0Onset))+
  geom_line(aes(color = f0Onset))

# Ok, so this does seem to mimic the Hombert data to some extent

# Run quick n dirty LMER

m1 <- lmer(Y ~ X1_i + X2_i + (1 | subj_id) + (1 | item_id), data = dat, REML = F)
summary(m1)

m2 <- lmer(Y ~ X1_i * X2_i + (1 | subj_id) + (1 | item_id), data = dat, REML = F)
summary(m2)
