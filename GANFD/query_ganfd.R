
## Anonymous
# Skin Tone Homogeneity Bias in Vision Language Models

## Script date: 20 Nov 2024

# Install and/or load packages -------------------------------------------------

if(!require("tidyverse")){install.packages("tidyverse", dependencies = TRUE); require("tidyverse")}

# Load data --------------------------------------------------------------------

ganfd = read.csv('GANFD_Data_V1.csv')
lookup = read.csv('image_lookup.csv')

# Query artificial ratings of the stimuli used ----------------------------------

lookup %>% 
  left_join(ganfd %>% select(c('full_ID', 'artificial')), by = 'full_ID') %>% 
  pull(artificial) %>% mean()
