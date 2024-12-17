
## Anonymous
# Skin Tone Homogeneity Bias in Vision Language Models

## Script date: 7 Dec 2024

# Install and/or load packages -------------------------------------------------

if(!require("tidyverse")){install.packages("tidyverse", dependencies = TRUE); require("tidyverse")}
if(!require("ggplot2")){install.packages("ggplot2", dependencies = TRUE); require("ggplot2")}
if(!require("ggsci")){install.packages("ggsci", dependencies = TRUE); require("ggsci")}
if(!require("showtext")){install.packages("showtext", dependencies = TRUE); require("showtext")}
if(!require("Cairo")){install.packages("Cairo", dependencies = TRUE); require("Cairo")}

font_add_google("Public Sans", "publicsans")
showtext_auto()

# Define preprocessing ---------------------------------------------------------

preprocess = function(file_name){
  
  df = read.csv(file_name) %>%
    mutate(gender = substr(condition, 1, 1)) %>% 
    mutate(skintone = substr(condition, 2, 2)) %>%
    mutate(skintone = case_when(
      skintone == 0 ~ "Lighter",
      skintone == 1 ~ "Darker"
    )) %>% 
    mutate(gender = case_when(
      gender == 'm' ~ "Men",
      gender == 'w' ~ "Women"
    )) %>% 
    mutate(gender = factor(gender, levels = c("Men", "Women")), 
           skintone = factor(skintone, levels = c("Lighter", "Darker"))) %>% 
    mutate(pair_id = paste(image_1, image_2, sep = "-")) %>% 
    mutate(pair_id = as.factor(pair_id)) %>%
    mutate(s_cosine = as.numeric(scale(cosine)))
  
  return(df)
  
}

# Load all data ----------------------------------------------------------------

gpt4omini = preprocess('../GPT4omini/gpt4omini_cosines.csv') %>% mutate(model = 'GPT-4o mini')
gpt4turbo = preprocess('../GPT4Turbo/gpt4turbo_cosines.csv') %>% mutate(model = 'GPT-4 Turbo')
llama3.2 = preprocess('../Llama-3.2/llama3.2_cosines.csv') %>% mutate(model = 'Llama-3.2')
blip3 = preprocess('../BLIP-3/blip3_cosines.csv') %>% mutate(model = 'BLIP-3')

cosines_df = rbind(gpt4omini, gpt4turbo, llama3.2, blip3) %>% 
  mutate(model = factor(model, levels = c('GPT-4o mini', 'GPT-4 Turbo', 'Llama-3.2', 'BLIP-3')))

# Visualize skin tone effects --------------------------------------------------

skintone_summary <- cosines_df %>% 
  group_by(model, skintone) %>%
  summarize(
    mean_cosine = mean(s_cosine, na.rm = TRUE),
    se_cosine = sd(s_cosine, na.rm = TRUE) / sqrt(n())
  )

# Bar plot with standard error bars
ggplot(skintone_summary, aes(x = skintone, y = mean_cosine, fill = skintone)) + 
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  geom_errorbar(aes(ymin = mean_cosine - se_cosine, ymax = mean_cosine + se_cosine), 
                width = 0.2, position = position_dodge(0.7)) +
  geom_hline(yintercept = 0) +
  theme_bw() + 
  ylim(c(-0.10, 0.10)) + 
  labs(y = "Standardized Cosine Similarity", x = "Skin Tone", fill = "Skin Tone") + 
  theme(
    legend.position = "top",
    axis.title.x = element_blank()
  ) +
  facet_wrap(vars(model), nrow = 1) + 
  scale_fill_jama()

ggsave(filename = "skintone.pdf", width = 10, height = 4, 
       dpi = "retina", device = cairo_pdf)

# Visualize gender effect ------------------------------------------------------

gender_summary <- cosines_df %>% 
  group_by(model, gender) %>%
  summarize(
    mean_cosine = mean(s_cosine, na.rm = TRUE),
    se_cosine = sd(s_cosine, na.rm = TRUE) / sqrt(n())
  )

ggplot(gender_summary, aes(x = gender, y = mean_cosine, fill = gender)) + 
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  geom_errorbar(aes(ymin = mean_cosine - se_cosine, ymax = mean_cosine + se_cosine), 
                width = 0.2, position = position_dodge(0.7)) +
  geom_hline(yintercept = 0) +
  theme_bw() + 
  ylim(c(-0.50, 0.50)) + 
  labs(y = "Standardized Cosine Similarity", x = "Gender", fill = "Gender") + 
  theme(
    legend.position = "top",
    axis.title.x = element_blank()
  ) +
  facet_wrap(vars(model), nrow = 1) + 
  scale_fill_jama()

ggsave(filename = "gender.pdf", width = 10, height = 4, 
       dpi = "retina", device = cairo_pdf)

# Visualize Interaction Effects ------------------------------------------------

int_summary <- cosines_df %>% 
  group_by(model, gender, skintone) %>%
  summarize(
    mean_cosine = mean(s_cosine, na.rm = TRUE),
    se_cosine = sd(s_cosine, na.rm = TRUE) / sqrt(n())
  )

ggplot(int_summary, aes(x = gender, y = mean_cosine, fill = skintone)) + 
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  geom_errorbar(aes(ymin = mean_cosine - se_cosine, ymax = mean_cosine + se_cosine), 
                width = 0.2, position = position_dodge(0.7)) +
  geom_hline(yintercept = 0) +
  theme_bw() + 
  ylim(c(-0.50, 0.50)) + 
  labs(y = "Standardized Cosine Similarity", x = "Gender", fill = "Skin Tone") + 
  theme(
    legend.position = "top",
    axis.title.x = element_blank()
  ) +
  facet_wrap(vars(model), nrow = 1) + 
  scale_fill_jama()

ggsave(filename = "interaction.pdf", width = 10, height = 4, 
       dpi = "retina", device = cairo_pdf)

