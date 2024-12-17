
## Anonymous
# Skin Tone Homogeneity Bias in Vision Language Models

## Script date: 20 Nov 2024

# Install and/or load packages -------------------------------------------------

if(!require("tidyverse")){install.packages("tidyverse", dependencies = TRUE); require("tidyverse")}
if(!require("lme4")){install.packages("lme4", dependencies = TRUE); require("lme4")}
if(!require("lmerTest")){install.packages("lmerTest", dependencies = TRUE); require("lmerTest")}
if(!require("effsize")){install.packages("effsize", dependencies = TRUE); require("effsize")}
if(!require("afex")){install.packages("afex", dependencies = TRUE); require("afex")}
if(!require("emmeans")){install.packages("emmeans", dependencies = TRUE); require("emmeans")}

# Load data --------------------------------------------------------------------

llama = read.csv('llama3.2_cosines.csv') %>%
  mutate(gender = substr(condition, 1, 1)) %>% 
  mutate(skintone = substr(condition, 2, 2)) %>%
  mutate(gender = as.factor(gender), 
         skintone = as.factor(skintone)) %>% 
  mutate(pair_id_1 = sapply(str_split(image_1, "-"), `[`, 1)) %>% 
  mutate(pair_id_2 = sapply(str_split(image_2, "-"), `[`, 1)) %>% 
  mutate(pair_id = paste(pair_id_1, pair_id_2, sep = "-")) %>% 
  mutate(pair_id = as.factor(pair_id)) %>%
  mutate(cosine = as.numeric(scale(cosine)))

# Fit three fixed-effects models -----------------------------------------------

llama.skintone <- lmer(cosine ~ 1 + skintone + (1 | pair_id), 
                           data = llama,
                           control = lmerControl(optimizer = "nmkbw", calc.derivs = FALSE))

summary(llama.skintone)
logLik(llama.skintone)

llama.gender <- lmer(cosine ~ 1 + gender + (1 | pair_id), 
                     data = llama,
                     control = lmerControl(optimizer = "nmkbw", calc.derivs = FALSE))

summary(llama.gender)
logLik(llama.gender)

llama.skintone.gender <- lmer(cosine ~ 1 + skintone * gender + (1 | pair_id), 
                                  data = llama,
                                  control = lmerControl(optimizer = "nmkbw", calc.derivs = FALSE))

summary(llama.skintone.gender)
logLik(llama.skintone.gender)

# Simple slopes ----------------------------------------------------------------

summary(emmeans(llama.skintone.gender, ~ skintone))

# Likelihood ratio tests -------------------------------------------------------

mixed(cosine ~ 1 + skintone * gender + (1 | pair_id), 
      data = llama, 
      control = lmerControl(optimizer = "nmkbw", calc.derivs = FALSE),
      method = "LRT")

