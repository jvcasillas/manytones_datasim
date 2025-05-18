library(tidyverse)

generate_simulated_experiment <- function(n_participants) {
  # Define possible frequency values (110 to 190 in increments of 10)
  frequency_values <- seq(110, 190, by = 10)
  
  # Define possible time values (40 to 120 in increments of 20)
  time_values <- seq(40, 120, by = 20)
  
  # Define possible response values (100 to 200 in increments of 5)
  response_values <- seq(100, 200, by = 5)
  
  # Create all possible (Frequency, Time) combinations
  stimulus_set <- expand.grid(Frequency = frequency_values, Time = time_values)
  
  # Number of trials per participant (40 unique stimuli)
  n_trials <- nrow(stimulus_set)
  
  # Create an empty list to store responses
  data_list <- list()
  
  # Generate responses for each participant
  for (participant in seq_len(n_participants)) {
    # Shuffle stimulus order for each participant
    participant_stimuli <- stimulus_set[sample(n_trials), ]
    
    # Generate a response for each trial
    participant_stimuli$Response <- sample(response_values, size = n_trials, replace = TRUE)
    
    # Add participant label
    participant_stimuli$Participant <- paste0("Participant_", LETTERS[participant])
    
    # Add trial numbers
    participant_stimuli$Trial <- 1:n_trials
    
    # Reorder columns
    participant_df <- participant_stimuli[, c("Participant", "Trial", "Frequency", "Time", "Response")]
    
    # Append to list
    data_list[[participant]] <- participant_df
  }
  
  # Combine all participants' data into one data frame
  simulated_df <- do.call(rbind, data_list)
  
  return(simulated_df)
}

# Example: Generate data for 2 participants
set.seed(123)  # For reproducibility

dat <- generate_simulated_experiment(n_participants = 10) |> 
  arrange(Participant, Frequency, Time)

dat |> 
  mutate(deviation = Response - Frequency) |> 
  ggplot() + 
  aes(x = Time, y = Frequency, fill = deviation) + 
  geom_tile()

library(brms)

formula <- bf(Response ~ Time * Frequency + (1 + Time * Frequency | Participant))

get_prior(
  formula = formula, 
  data = dat
)



hold <- brm(
  formula = formula, 
  prior =  prior(normal(0, 10), class = 'b') + 
           prior(lkj(1), class = 'cor') + 
           prior(student_t(3, 0, 37.1), class = 'sd') + 
           prior(student_t(3, 0, 37.1), class = 'sigma'),
  #sample_prior = "only", 
  data = dat
)

conditional_effects(hold)

as_tibble(hold)
