## Project: Final Lengthening in Italian
## Description: Preprocess list data
## Author: Timo Roettger 
## Contact: timo.b.roettger@gmail.com
## Date: 07/02/19

# CodeBook:

# Speaker:    Unique Speaker IDs (n = 16)
# Rand:       Randomisation list (rand1, rand2, rand3, rand4)
# Pros:       (redundant = list)
# WordPF:     Lexical item in phrase-final position in prefinal list position (n = 32)
# DurwPF:     Word duration in seconds
# VowelPF:    Quality of final vowel in prefinal list position (' indicated stress)
# DurFvowelPF: Duration of final vowel in prefinal list position 
# WordF:      Lexical item in phrase-final position in final list position  (n = 32)
# DurwF:      Word duration in seconds in final list position
# VowelF:     Quality of final vowel in final list position (' indicated stress)
# DurFvowelF: Duration of final vowel in final list position


# load in packages
library(tidyverse)
library(rstudioapi)

# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))
setwd("../raw/")

xdata <- read_csv("PF-F_inLists_results.csv")

# melt data frame and make tidy
xdata <- xdata %>%
  gather(Position, Word, c(WordPF, WordF)) %>%
  mutate(Position = fct_recode(Position, "prefinal" = "WordPF",
                               "final" = "WordF"))

xdata$Durw <- ifelse(xdata$Position == "prefinal", xdata$DurwPF, xdata$DurwF) 
xdata$Vowel <- ifelse(xdata$Position == "prefinal", xdata$VowelPF, xdata$VowelF) 
xdata$Durvowel <- ifelse(xdata$Position == "prefinal", xdata$DurvowelPF, xdata$DurvowelPF) 

xdata <- xdata %>%
  select(-DurwPF,- VowelPF, -DurvowelPF, -DurwF, -VowelF, -DurvowelF, -Pros)

# fix mali' -> mali
xdata[xdata$Word == "mali'",]$Vowel <- "i"
xdata[xdata$Word == "mali'",]$Word <- "mali"

# fix pupi' -> pupi
xdata[xdata$Word == "pupi'",]$Vowel <- "i"
xdata[xdata$Word == "pupi'",]$Word <- "pupi"

# add stress vector
xdata$stress <- ifelse(xdata$Vowel %in% c("a'","e'", "i'", "o'"), 1, 0)

# remove diacritc from Fvowel
xdata$Vowel <- str_replace(xdata$Vowel, "[']", "")

# save processed data
setwd("../data/")
write.csv(xdata, file = "processed_Italian_list.csv", row.names = FALSE)

