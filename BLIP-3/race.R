
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

blip3 = read.csv('blip3_cosines.csv') %>%
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

blip3.skintone <- lmer(cosine ~ 1 + skintone + (1 | pair_id), 
                       data = blip3,
                       control = lmerControl(optimizer = "nmkbw", calc.derivs = FALSE))

summary(blip3.skintone)
logLik(blip3.skintone)

blip3.gender <- lmer(cosine ~ 1 + gender + (1 | pair_id), 
                     data = blip3,
                     control = lmerControl(optimizer = "nmkbw", calc.derivs = FALSE))

summary(blip3.gender)
logLik(blip3.gender)

blip3.skintone.gender <- lmer(cosine ~ 1 + skintone * gender + (1 | pair_id), 
                                  data = blip3,
                                  control = lmerControl(optimizer = "nmkbw", calc.derivs = FALSE))

summary(blip3.skintone.gender)
logLik(blip3.skintone.gender)

# Simple slopes ----------------------------------------------------------------

summary(emmeans(blip3.skintone.gender, ~ skintone))

# Likelihood ratio tests -------------------------------------------------------

mixed(cosine ~ 1 + skintone * gender + (1 | pair_id), 
      data = blip3,
      control = lmerControl(optimizer = "nmkbw", calc.derivs = FALSE),
      method = "LRT")

