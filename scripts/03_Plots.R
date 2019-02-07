## Project: Final Lengthening in Italian
## Description: Plot results
## Author: Timo Roettger 
## Contact: timo.b.roettger@gmail.com
## Date: 07/02/19

## Code book:

# processed_Italian_QS.csv
# Speaker:  Unique Speaker IDs (n = 16)
# Rand:     Randomisation list (rand1, rand2, rand3, rand4)
# Pros:     Prosodic condition (Query vs. Stat(ement))
# Word:     Lexical item in phrase-final position (n = 32)
# Durw:     Word duration in seconds
# Fvowel:   Quality of final vowel (' indicated stress)
# DurFvowel: Duration of final vowel
# Stress:   Whether final syllable is stressed or not (1 vs. 0)

# processed_Italian_list.csv
# Speaker:  Unique Speaker IDs (n = 16)
# Rand:     Randomisation list (rand1, rand2, rand3, rand4)
# Position: Position in list (prefinal vs. final)
# Word:     Lexical item in phrase-final position (n = 32)
# Durw:     Word duration in seconds
# vowel:    Quality of final vowel
# DurFvowel: Duration of final vowel
# stress:   Whether final syllable is stressed or not (1 vs. 0)


## load in packages
library(tidyverse)
library(rstudioapi)
library(brms)
library(bayesplot)
library(ggbeeswarm)

## Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))
setwd("../data/")

xdata_qs <- read_csv("processed_Italian_QS.csv")
xdata_list <- read_csv("processed_Italian_list.csv")

load("Bayesian_models_Italian.RData")
load("Posteriors_Italian.RData")


## plot results

# Preprocess
xdata_qs$stress <- as.factor(as.character(xdata_qs$stress))
xdata_list$stress <- as.factor(as.character(xdata_list$stress))

posteriors_qs$Stress <- as.factor(as.character(posteriors_qs$Stress))
posteriors_list$Stress <- as.factor(as.character(posteriors_list$Stress))

levels(xdata_qs$stress) <- c("trochee", "iambus")
levels(posteriors_qs$Stress) <- c("iambus", "trochee")
levels(xdata_list$stress) <- c("trochee", "iambus")
levels(posteriors_list$Stress) <- c("iambus", "trochee")

xdata_qs$Pros <- as.factor(xdata_qs$Pros)
levels(xdata_qs$Pros) <- c("Question", "Statement")
levels(posteriors_qs$Function) <- c("Question", "Statement")

xdata_list$Position <- as.factor(xdata_list$Position)
levels(xdata_list$Position) <- c("Final", "Prefinal")

# aggregate
xagg_qs <- xdata_qs %>%
  group_by(Pros, Speaker, stress) %>%
  summarise(mean_dur = mean(DurFvowel)) %>%
  rename(Stress = stress, Function = Pros)

xagg_qs$Function <- as.factor(xagg_qs$Function)
levels(xagg_qs$Function) <- c("Question", "Statement")

xagg_list <- xdata_list %>%
  group_by(Position, Speaker, stress) %>%
  summarise(mean_dur = mean(Durvowel)) %>%
  rename(Stress = stress, Function = Position)

xagg_list$Function <- as.factor(xagg_list$Function)
levels(xagg_list$Function) <- c("Final", "Prefinal")

xagg_qs$Stress <- factor(xagg_qs$Stress,levels(xagg_qs$Stress)[c(2,1)])
xagg_list$Stress <- factor(xagg_list$Stress,levels(xagg_list$Stress)[c(2,1)])


# plot Figure_qs
results_plot_qs <- 
  ggplot(xagg_qs) +
  #geom_hline(aes(yintercept = 0.5), colour = "grey", lty = "dashed") +
  geom_errorbar(data = posteriors_qs, aes(x = Function, ymin = lci, ymax = uci, 
                                       group = interaction(Stress, Function)), 
                colour = "black", position = position_dodge(width = 1), width = 0.2) +
  geom_point(data = posteriors_qs, aes(x = Function, y = mean, fill = Stress), colour = "black", 
             size = 4, pch = 21,
             position = position_dodge(width = 1)) +
  geom_quasirandom(aes(x = Function, y = mean_dur, colour = Stress), 
                   alpha = 0.3, size = 3, dodge.width = 1) + 
  ylab("Vowel duration in seconds\n") +
  xlab("\n") +
  scale_colour_manual(values = c("#0072B2", "#D55E00")) +
  scale_fill_manual(values = c("#0072B2", "#D55E00")) +
  scale_y_continuous(expand = c(0, 0), breaks = (c(0,0.1,0.2,0.3,0.35)), limits = c(0,0.35)) +
  labs(title = "Duration of final vowel",
       subtitle = "posterior means and 95% credible intervals\nsemitransparent dots are speaker averages") +
  theme_bw() +
  theme(legend.position = "right",
        legend.key.height = unit(2,"line"),
        legend.title = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 16),
        legend.background = element_rect(fill = "transparent"),
        strip.background = element_blank(),
        strip.text = element_text(size = 18, face = "bold"),
        panel.spacing = unit(2, "lines"),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent"),
        axis.line.x = element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18, face = "bold"),
        plot.title = element_text(size = 18, face = "bold"),
        plot.margin = unit(c(0.2,0.1,0.2,0.1),"cm"))

# plot Figure_list
results_plot_list <- 
  ggplot(xagg_list) +
  #geom_hline(aes(yintercept = 0.5), colour = "grey", lty = "dashed") +
  geom_errorbar(data = posteriors_list, aes(x = Function, ymin = lci, ymax = uci, 
                                          group = interaction(Stress, Function)), 
                colour = "black", position = position_dodge(width = 1), width = 0.2) +
  geom_point(data = posteriors_list, aes(x = Function, y = mean, fill = Stress), colour = "black", 
             size = 4, pch = 21,
             position = position_dodge(width = 1)) +
  geom_quasirandom(aes(x = Function, y = mean_dur, colour = Stress), 
                   alpha = 0.3, size = 3, dodge.width = 1) + 
  ylab("Vowel duration in seconds\n") +
  xlab("\n") +
  scale_colour_manual(values = c("#0072B2", "#D55E00")) +
  scale_fill_manual(values = c("#0072B2", "#D55E00")) +
  scale_y_continuous(expand = c(0, 0), breaks = (c(0,0.1,0.2,0.25)), limits = c(0,0.25)) +
  labs(title = "Duration of final vowel",
       subtitle = "posterior means and 95% credible intervals\nsemitransparent dots are speaker averages") +
  theme_bw() +
  theme(legend.position = "right",
        legend.key.height = unit(2,"line"),
        legend.title = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 16),
        legend.background = element_rect(fill = "transparent"),
        strip.background = element_blank(),
        strip.text = element_text(size = 18, face = "bold"),
        panel.spacing = unit(2, "lines"),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent"),
        axis.line.x = element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18, face = "bold"),
        plot.title = element_text(size = 18, face = "bold"),
        plot.margin = unit(c(0.2,0.1,0.2,0.1),"cm"))

setwd("../plots/")
ggsave(filename = "results_plot_qs.png", 
       plot = results_plot_qs,
       width = 150, 
       height = 150,
       units = "mm", 
       #bg = "transparent",
       dpi = 500)

ggsave(filename = "results_plot_list.png", 
       plot = results_plot_list,
       width = 150, 
       height = 150,
       units = "mm", 
       #bg = "transparent",
       dpi = 500)

