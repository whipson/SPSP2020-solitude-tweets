# Hipson, Kiritchenko, Mohammad, & Coplan (2020). Distinguishing Positive and Negative Aspects of Solitude in Tweets

# Load tidyverse library for data manipulation and plotting functions
# install.packages("tidyverse")
library(tidyverse)
# Load ggpubr for arranging plots
# install.packages("ggpubr")
library(ggpubr)

# Load data. Each .csv file is gives the comparative PMI scores between pairs of solitude, lonely, and alone.
# Note: These files must be in your working directory for the code below to run properly!

lonely_v_alone <- read_csv("lonely_v_alone.csv")
solitude_v_alone <- read_csv("solitude_v_alone.csv")
solitude_v_lonely <- read_csv("solitude_v_lonely.csv")

# Filter for words that occurred in the set of tweets at least 500 times

lonely_v_alone_filt <- lonely_v_alone %>%
  filter(frequency >= 500)

solitude_v_alone_filt <- solitude_v_alone %>%
  filter(frequency >= 500)

solitude_v_lonely_filt <- solitude_v_lonely %>%
  filter(frequency >= 500)

# --- Linear regressions --- #
# There are 9 linear regressions in total

# solitude versus loneliness

sol_lon_val_fit <- lm(valence ~ pmi, data = solitude_v_lonely_filt)
summary(sol_lon_val_fit)

sol_lon_aro_fit <- lm(arousal ~ pmi, data = solitude_v_lonely_filt)
summary(sol_lon_aro_fit)

sol_lon_dom_fit <- lm(dominance ~ pmi, data = solitude_v_lonely_filt)
summary(sol_lon_dom_fit)

# solitude versus alone

sol_alo_val_fit <- lm(valence ~ pmi, data = solitude_v_alone_filt)
summary(sol_alo_val_fit)

sol_alo_aro_fit <- lm(arousal ~ pmi, data = solitude_v_alone_filt)
summary(sol_alo_aro_fit)

sol_alo_dom_fit <- lm(dominance ~ pmi, data = solitude_v_alone_filt)
summary(sol_alo_dom_fit)

# lonely versus alone

lon_alo_val_fit <- lm(valence ~ pmi, data = lonely_v_alone_filt)
summary(lon_alo_val_fit)

lon_alo_aro_fit <- lm(arousal ~ pmi, data = lonely_v_alone_filt)
summary(lon_alo_aro_fit)

lon_alo_dom_fit <- lm(dominance ~ pmi, data = lonely_v_alone_filt)
summary(lon_alo_dom_fit)

# --- Plotting --- #

# Figure 1

p1 <- solitude_v_lonely_filt %>%
  ggplot(aes(x = valence, y = pmi, label = Term)) +
  geom_point(alpha = .25, color = "dodgerblue2", position = "jitter") +
  geom_smooth(method = 'lm', color = "black") +
  scale_y_continuous(breaks = c(-3, 0,  6),
                     labels = c("Lonely", 0, "Solitude")) +
  labs(x = "Valence",
       y = "Tendency of Co-occurrence") +
  theme_minimal(base_size = 14)

p2 <- solitude_v_lonely_filt %>%
  ggplot(aes(x = arousal, y = pmi, label = Term)) +
  geom_point(alpha = .25, color = "#00CD66", position = "jitter") +
  geom_smooth(method = 'lm', color = "black") +
  scale_y_continuous(breaks = c(-3, 0,  6),
                     labels = c("Lonely", 0, "Solitude")) +
  labs(x = "Arousal",
       y = "Tendency of Co-occurrence") +
  theme_minimal(base_size = 14)

p3 <- solitude_v_lonely_filt %>%
  ggplot(aes(x = dominance, y = pmi, label = Term)) +
  geom_point(alpha = .25, color = "#EE3B3B", position = "jitter") +
  geom_smooth(method = 'lm', color = "black") +
  scale_y_continuous(breaks = c(-3, 0,  6),
                     labels = c("Lonely", 0, "Solitude")) +
  labs(x = "Dominance",
       y = "Tendency of Co-occurrence") +
  theme_minimal(base_size = 14)

fig1 <- ggarrange(p1, p2, p3, nrow = 1)
annotate_figure(fig1,
                top = text_grob("Solitude vs. Lonely", face = "bold", size = 18),
                bottom = text_grob("Words with higher PMI difference co-occur more with 'solitude'. Words with lower PMI difference co-occur more with 'lonely'\nFive highest and lowest PMI difference scores are highlighted", hjust = 0, x = .01))

# Figure 2

p4 <- solitude_v_alone_filt %>%
  ggplot(aes(x = valence, y = pmi)) +
  geom_point(alpha = .25, color = "dodgerblue2", position = "jitter") +
  geom_smooth(method = 'lm', color = "black") +
  scale_y_continuous(breaks = c(-4, 0,  5),
                     labels = c("Alone", 0, "Solitude")) +
  labs(x = "Valence",
       y = "Tendency of Co-occurrence") +
  theme_minimal(base_size = 14)

p5 <- solitude_v_alone_filt %>%
  ggplot(aes(x = arousal, y = pmi)) +
  geom_point(alpha = .25, color = "#00CD66", position = "jitter") +
  geom_smooth(method = 'lm', color = "black") +
  scale_y_continuous(breaks = c(-4, 0,  5),
                     labels = c("Alone", 0, "Solitude")) +
  labs(x = "Arousal",
       y = "Tendency of Co-occurrence") +
  theme_minimal(base_size = 14)

p6 <- solitude_v_alone_filt %>%
  ggplot(aes(x = dominance, y = pmi)) +
  geom_point(alpha = .25, color = "#EE3B3B", position = "jitter") +
  geom_smooth(method = 'lm', color = "black") +
  scale_y_continuous(breaks = c(-4, 0,  5),
                     labels = c("Alone", 0, "Solitude")) +
  labs(x = "Dominance",
       y = "Tendency of Co-occurrence") +
  theme_minimal(base_size = 14)

fig2 <- ggarrange(p4, p5, p6, nrow = 1)
annotate_figure(fig2,
                top = text_grob("Solitude vs. Alone", face = "bold", size = 18),
                bottom = text_grob("Words with higher PMI difference co-occur more with 'solitude'. Words with lower PMI difference co-occur more with 'alone'\nFive highest and lowest PMI difference scores are highlighted", hjust = 0, x = .01))

# Figure 3

p7 <- lonely_v_alone_filt %>%
  ggplot(aes(x = valence, y = pmi)) +
  geom_point(alpha = .25, color = "dodgerblue2", position = "jitter") +
  geom_smooth(method = 'lm', color = "black") +
  scale_y_continuous(breaks = c(-4, 0,  3.5),
                     labels = c("Alone", 0, "Lonely")) +
  labs(x = "Valence",
       y = "Tendency of Co-occurrence") +
  theme_minimal(base_size = 14)

p8 <- lonely_v_alone_filt %>%
  ggplot(aes(x = arousal, y = pmi)) +
  geom_point(alpha = .25, color = "#00CD66", position = "jitter") +
  geom_smooth(method = 'lm', color = "black") +
  scale_y_continuous(breaks = c(-4, 0,  3.5),
                     labels = c("Alone", 0, "Lonely")) +
  labs(x = "Arousal",
       y = "Tendency of Co-occurrence") +
  theme_minimal(base_size = 14)

p9 <- lonely_v_alone_filt %>%
  ggplot(aes(x = dominance, y = pmi)) +
  geom_point(alpha = .25, color = "#EE3B3B", position = "jitter") +
  geom_smooth(method = 'lm', color = "black") +
  scale_y_continuous(breaks = c(-4, 0,  3.5),
                     labels = c("Alone", 0, "Lonely")) +
  labs(x = "Dominance",
       y = "Tendency of Co-occurrence") +
  theme_minimal(base_size = 14)

fig3 <- ggarrange(p7, p8, p9, nrow = 1)
annotate_figure(fig3,
                top = text_grob("Lonely vs. Alone", face = "bold", size = 18),
                bottom = text_grob("Words with higher PMI difference co-occur more with 'lonely'. Words with lower PMI difference co-occur more with 'alone'\nFive highest and lowest PMI difference scores are highlighted", hjust = 0, x = .01))