## Project: Final Lengthening in Italian
## Description: Preprocess question-statement data
## Author: Timo Roettger 
## Contact: timo.b.roettger@gmail.com
## Date: 07/02/19

# CodeBook:

# Speaker:  Unique Speaker IDs (n = 16)
# Rand:     Randomisation list (rand1, rand2, rand3, rand4)
# Pros:     Prosodic condition (Query vs. Stat(ement))
# Word:     Lexical item in phrase-final position (n = 32)
# Durw:     Word duration in seconds
# Fvowel:   Quality of final vowel (' indicated stress)
# DurFvowel: Duration of final vowel


# load in packages
library(tidyverse)
library(rstudioapi)

# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))
setwd("../raw/")

xdata <- read_csv("Queries_Statements_duration-data.csv")

# check for consistency in vowel specification
xtabs(~Word + Fvowel, xdata)

# fix bide -> bide'
xdata[xdata$Word == "bide",]$Fvowel <- "e'"
xdata[xdata$Word == "bide",]$Word <- "bide'"

# fix nana -> nana'
xdata[xdata$Word == "nana",]$Fvowel <- "a'"
xdata[xdata$Word == "nana",]$Word <- "nana'"

# add stress vector
xdata$stress <- ifelse(xdata$Fvowel %in% c("a'","e'", "i'", "o'"), 1, 0)

# remove diacritc from Fvowel
xdata$Fvowel <- str_replace(xdata$Fvowel, "[']", "")

# save processed data
setwd("../data/")
write.csv(xdata, file = "processed_Italian_QS.csv", row.names = FALSE)

