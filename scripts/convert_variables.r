#### R code to load data and implement modelling transformations for the #
###subsequent regression analyses as described in Ehret, Katharina. Contact and
###complexity in English varieties: The number of native speakers partially
###negatively influences morphosyntactic complexity but the proportion of
###non-native speakers does not. Submitted to: PLOS One.



## load dataset with raw values

df = read_csv("data/syngram_complexity_data.csv") #adjust paths if necessary


## get corpus statistics

# get number of texts per variety type

# length(which(df$language_type == "L1t")) # 359

# length(which(df$language_type == "L1c")) # 773

# length(which(df$language_type == "L2")) # 795


### apply transformations for modelling

# log transform total number of tokens as exposure variable

df <- df %>% 
mutate(log_tokens = log(total_no_tokens), .after ="total_no_tokens")  


# change character strings to factors; relevel language type with L1t as reference level

df <- df %>% 
  mutate_at(c("macro_region", "language_type", "corpus"), as.factor)

df <- df %>% 
  mutate(language_type = relevel(language_type, ref = "L1t"))


# add log transformed numeric predictor natives; use log10 following Sinnemäki and Di Garbo (2018)

df <- df %>% 
mutate(log10_natives = log10(natives), .after ="non_natives")  


# calculate the proportion of non-native speakers following Bentz and Winter (2013), as the proportion of non-native speakers in the total population of native and non-native speakers.

df <- df |> mutate(prop_non_natives = non_natives/(natives+non_natives), .after = non_natives)


#################################################################################################

## function to measure overdispersion as implemented in check_overdispersion but with more explicit output. Adapted from Bolker et al. (2017): GLMM FAQ, http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html

overdisp_fun <- function(model) {
    rdf <- df.residual(model)
    rp <- residuals(model,type="pearson")
    Pearson.chisq <- sum(rp^2)
    prat <- Pearson.chisq/rdf
    pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
    c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}




