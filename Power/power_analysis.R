
## Anonymous
# Skin Tone Homogeneity Bias in Vision Language Models

## Script date: 8 Dec 2024

# Install and/or load packages -------------------------------------------------

if(!require("tidyverse")){install.packages("tidyverse", dependencies = TRUE); require("tidyverse")}
if(!require("simr")){install.packages("simr", dependencies = TRUE); require("simr")}
if(!require("lme4")){install.packages("lme4", dependencies = TRUE); require("lme4")}
if(!require("lmerTest")){install.packages("lmerTest", dependencies = TRUE); require("lmerTest")}

# Load GPT-4o mini data --------------------------------------------------------

gpt4omini = read.csv('../GPT4omini/gpt4omini_cosines.csv') %>%
  mutate(gender = substr(condition, 1, 1)) %>% 
  mutate(skintone = substr(condition, 2, 2)) %>%
  mutate(gender = as.factor(gender), 
         skintone = as.factor(skintone)) %>% 
  mutate(pair_id_1 = sapply(str_split(image_1, "-"), `[`, 1)) %>% 
  mutate(pair_id_2 = sapply(str_split(image_2, "-"), `[`, 1)) %>% 
  mutate(pair_id = paste(pair_id_1, pair_id_2, sep = "-")) %>% 
  mutate(pair_id = as.factor(pair_id)) %>%
  mutate(cosine = as.numeric(scale(cosine)))

# Fit interaction model --------------------------------------------------------

gpt4omini.skintone.gender <- lmer(cosine ~ 1 + skintone * gender + (1 | pair_id), 
                                  data = gpt4omini,
                                  control = lmerControl(optimizer = "nmkbw", calc.derivs = FALSE))

# Power analysis on the interaction effect -------------------------------------

# All power analyses were performed to see if we could detect d = 0.30
# for the interaction effect (A * B), which generally requires more 
# statistical power than detecting main effects (A or B)
fixef(gpt4omini.skintone.gender)['skintone1:genderw'] <- 0.3

# Check how many cosine similarity measurements associated with each pair_id
# The minimum number of counts associated with a pair_id was 1128
gpt4omini %>% group_by(pair_id) %>%
  summarise(count = n(), .groups = 'drop') %>% 
  slice_min(order_by = count, n = 1)

# Do we have enough cosine similarity measurements for each condition? Yes.
power.curve = powerCurve(gpt4omini.skintone.gender, 
                         test = fixed("skintone1:genderw", method = "t"), 
                         within = "pair_id",
                         breaks = c(1240, 1244, 1245), 
                         nsim = 1000) # 1244: 90.40% power

plot(power.curve)

# Power analysis (Interaction) -------------------------------------------------

fixef(mpnetbase.interaction)['raceBlack:mean_proto'] <- 0.3
powerSim(mpnetbase.interaction, 
         test = fixed("raceBlack:mean_proto", method = "t"), 
         nsim = 100)
