## Project: Final Lengthening in Italian
## Description: Bayesian inference
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
# stress:   Whether final syllable is stressed or not (1 vs. 0)

# processed_Italian_list.csv
# Speaker:  Unique Speaker IDs (n = 16)
# Rand:     Randomisation list (rand1, rand2, rand3, rand4)
# Position: Position in list (prefinal vs. final)
# Word:     Lexical item in phrase-final position (n = 32)
# Durw:     Word duration in seconds
# vowel:    Quality of final vowel
# Durvowel: Duration of final vowel
# stress:   Whether final syllable is stressed or not (1 vs. 0)


## load in packages
library(tidyverse)
library(rstudioapi)
library(brms)
library(bayesplot)

# Run on multiple cores for Bayesian regressions
options(mc.cores = parallel::detectCores ()) 

## Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))
setwd("../data/")

xdata_qs <- read_csv("processed_Italian_QS.csv")
xdata_list <- read_csv("processed_Italian_list.csv")


## Bayesian model: confirmatory
# set priors for predictors and intercept
prior_qs <- c(prior(normal(0,0.05), class = b), prior(normal(0.16, 0.08), class = Intercept))

xmdl_1_qs <- brm(DurFvowel ~ stress * Pros + Fvowel +
                     (1 + stress * Pros + Fvowel | Speaker) +
                     (1 + Pros | Word),
                   family = gaussian,
                   prior = prior_qs, data = xdata_qs,
              control = list(adapt_delta = 0.90))

prior_list <- c(prior(normal(0,0.05), class = b), prior(normal(0.14, 0.8), class = Intercept))

xmdl_1_list <- brm(Durvowel ~ stress * Position + Vowel +
                   (1 + stress * Position + Vowel | Speaker) +
                   (1 + Position | Word),
                 family = gaussian,
                 prior = prior_list, data = xdata_list,
                 control = list(adapt_delta = 0.90))

# store model output
setwd("../data/")
save(xmdl_1_qs, xmdl_1_list, file = "Bayesian_models_Italian.RData")

# prediction check
pp_check(xmdl_1_qs) # heavy right tale but modelled well
pp_check(xmdl_1_list) # looks good


## extract posterior values
# qs
# posterior distribution
psamples_qs = posterior_samples(xmdl_1_qs, pars = rownames(brms:::fixef.brmsfit(xmdl_1_qs))) %>% 
  mutate(unstressed_Q = b_Intercept,
         unstressed_S = b_Intercept + b_ProsStat,
         stressed_Q = b_Intercept + b_stress,
         stressed_S = b_Intercept + b_ProsStat + b_stress + `b_stress:ProsStat`,
         diff_unstressed_QS = unstressed_Q - unstressed_S,
         diff_stressed_QS = stressed_Q - stressed_S,
         diff_Q = unstressed_Q - stressed_Q,
         diff_S = unstressed_S - stressed_S)


# 95% HDI of difference
Stress <- c(rep("trochee",2),rep("iambus",2))
Function <- rep(c("Query","Stat"),2)
Difference <- c("Query - Statement (trochee)", "Query - Statement (iambus)", 
                "Query (trochee) - Query (iambus)", "Statement (trochee) - Statement (iambus)")

parameters <- c("unstressed_Q","unstressed_S","stressed_Q","stressed_S")
parameters_diff <- c("diff_unstressed_QS","diff_stressed_QS","diff_Q","diff_S")

name <- c()
lci <- c()
uci <- c()
mean <- c()

for (i in 1:length(parameters)) {
  lci <- c(lci, coda::HPDinterval(as.mcmc(psamples_qs[[parameters[i]]]))[1])
  uci <- c(uci, coda::HPDinterval(as.mcmc(psamples_qs[[parameters[i]]]))[2])
  mean <- c(mean, mean(psamples_qs[[parameters[i]]]))
  name <- c(name, parameters[i])
}

posteriors_qs = data.frame(name, Function, Stress, mean, lci, uci)

name <- c()
lci <- c()
uci <- c()
mean <- c()
probs <- c()

for (i in 1:length(parameters_diff)) {
  lci <- c(lci, coda::HPDinterval(as.mcmc(psamples_qs[[parameters_diff[i]]]))[1])
  uci <- c(uci, coda::HPDinterval(as.mcmc(psamples_qs[[parameters_diff[i]]]))[2])
  mean <- c(mean, mean(psamples_qs[[parameters_diff[i]]]))
  name <- c(name, parameters_diff[i])
  probs <- c(probs, length(which(psamples_qs[[parameters_diff[i]]] < 0)) / length(psamples_qs[[parameters_diff[i]]]))
}

posteriors_diff_qs = data.frame(Difference, mean, lci, uci, probs)


# list
# posterior distribution
psamples_list = posterior_samples(xmdl_1_list, pars = rownames(brms:::fixef.brmsfit(xmdl_1_list))) %>% 
  mutate(unstressed_F = b_Intercept,
         unstressed_PF = b_Intercept + b_Positionprefinal,
         stressed_F = b_Intercept + b_stress,
         stressed_PF = b_Intercept + b_Positionprefinal + b_stress + `b_stress:Positionprefinal`,
         diff_unstressed = unstressed_PF - unstressed_F,
         diff_stressed = stressed_PF - stressed_F,
         diff_PF = unstressed_PF - stressed_PF,
         diff_F = unstressed_F - stressed_F)


# 95% HDI of difference
Stress <- c(rep("trochee",2),rep("iambus",2))
Function <- rep(c("Final","Prefinal"),2)
Difference <- c("Prefinal - Final (trochee)", "Prefinal - Final (iambus)", 
                "Final (trochee) - Final (iambus)", "Prefinal (trochee) - Prefinal (iambus)")

parameters <- c("unstressed_F","unstressed_PF","stressed_F","stressed_PF")
parameters_diff <- c("diff_unstressed","diff_stressed","diff_F","diff_PF")

name <- c()
lci <- c()
uci <- c()
mean <- c()

for (i in 1:length(parameters)) {
  lci <- c(lci, coda::HPDinterval(as.mcmc(psamples_list[[parameters[i]]]))[1])
  uci <- c(uci, coda::HPDinterval(as.mcmc(psamples_list[[parameters[i]]]))[2])
  mean <- c(mean, mean(psamples_list[[parameters[i]]]))
  name <- c(name, parameters[i])
}

posteriors_list = data.frame(name, Function, Stress, mean, lci, uci)

name <- c()
lci <- c()
uci <- c()
mean <- c()
probs <- c()

for (i in 1:length(parameters_diff)) {
  lci <- c(lci, coda::HPDinterval(as.mcmc(psamples_list[[parameters_diff[i]]]))[1])
  uci <- c(uci, coda::HPDinterval(as.mcmc(psamples_list[[parameters_diff[i]]]))[2])
  mean <- c(mean, mean(psamples_list[[parameters_diff[i]]]))
  name <- c(name, parameters_diff[i])
  probs <- c(probs, length(which(psamples_list[[parameters_diff[i]]] < 0)) / length(psamples_list[[parameters_diff[i]]]))
}

posteriors_diff_list = data.frame(Difference, mean, lci, uci, probs)

# store posteriors
setwd("../data/")
save(posteriors_qs, posteriors_diff_qs, 
     posteriors_list, posteriors_diff_list, 
     file = "Posteriors_Italian.RData")


