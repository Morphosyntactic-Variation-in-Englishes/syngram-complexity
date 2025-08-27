### R code to generate the dataset containing the complexity indices for
### individual texts and demographic information, as presented in Ehret,
### Katharina. Contact and complexity in English varieties: The number of
### native speakers partially negatively influences morphosyntactic complexity
### but the proportion of non-native speakers does not. Submitted to: PLOS One.


## load packages

library(data.table)
library(tidyverse)


## prepare frequency counts for further analysis

# read csv
mydata <- fread("data/POS_feature_counts_sorted_21July2025.csv")


# add language IDs

mydata <- mydata |> 
mutate (ID = as.factor(area), .before = 1) |>
mutate (
	#ICE
	ID = ifelse(area == "AU", "59", ID),
	ID = ifelse(area == "CAN", "79", ID),
	ID = ifelse(area == "GB", "78", ID), 
	ID = ifelse(area == "GH", "39", ID),
	ID = ifelse(area == "HK", "56", ID),
	ID = ifelse(area == "IND", "52", ID),
	ID = ifelse(area == "IRE", "3", ID),
	ID = ifelse(area == "JA", "26", ID), 
	ID = ifelse(area == "NG", "41", ID),
	ID = ifelse(area == "NZ", "64", ID),
	ID = ifelse(area == "PHI", "75", ID),
	ID = ifelse(area == "SIN", "57", ID),
	ID = ifelse(area == "SL", "55", ID),
	ID = ifelse(area == "TT", "81", ID),
	ID = ifelse(area == "UG", "47", ID),
	ID = ifelse(area == "KE", "46", ID), #only Kenya (removed EA_018, 19, 20)
	
	#SBCSAE	
	ID = ifelse(area == "SBC", "14", ID), 

 	#FRED
	ID = ifelse(area == "Mid", "80", ID),
	ID = ifelse(area == "N", "6", ID),
	ID = ifelse(area == "ScL", "2", ID), 
	ID = ifelse(area == "ScH", "2", ID),
	ID = ifelse(area == "SE", "9", ID),
	ID = ifelse(area == "SW", "7", ID),
	ID = ifelse(area == "Wal", "5", ID),
	ID = ifelse(area == "Man", "4", ID),
	ID = ifelse(area == "Heb", "82", ID) #Hebrides

	)


## create dataset with demographic data

# load demographic data; taken from https://github.com/Morphosyntactic-Variation-in-Englishes/DOVE; consult DOVE for the original copyright of the individual demographic data!

langs <- fread("data/languages_DOVE_v1.0.csv")


# select demographic variables used in this analysis

langs_subset <- langs |> select(ID, name, macro_region, language_type, abbr, natives, non_natives)

# add corpus information for languages; currently not in DOVE

langs_subset$corpus <- c("FRED", "ICE", "FRED", "FRED", "FRED", "FRED", "FRED", "SBCSAE", "ICE", "ICE", "ICE", "ICE", "ICE", "ICE", "ICE", "ICE", "ICE", "ICE", "ICE", "ICE", "ICE", "ICE", "FRED", "ICE", "FRED")


# join demographic data with complexity data

mydata[, ID := as.integer(ID)] #because ID is an integer in langs; required for join

mydata <- full_join(mydata, langs_subset, by= "ID", multiple="all", relationship="many-to-many")

# rename columns to avoid confusion

mydata <- rename(mydata, language_ID = ID)

mydata <- rename(mydata, language_name = name)

# create tidy order of columns; drop area

setcolorder(mydata, c("text", "language_ID", "language_name", "abbr", "language_type", "macro_region", "natives", "non_natives", "SI", "GI", "total_no_tokens", "corpus"))

mydata <- mydata[, area := NULL]

# save final dataset for further analyses

fwrite(mydata, "data/syngram_complexity_data.csv")



























