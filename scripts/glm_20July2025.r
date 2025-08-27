### R code to conduct regression analyses to assess the influence of non-native
### acquisition on morphological complexity operationalised as the number of
### bound grammatical markers (SI = syntheticity index) and the overall number
### of explicit grammatical markers (GI = grammaticitiy index) as described in
### Ehret, Katharina. Contact and complexity in English varieties: The number
### of native speakers partially negatively influences morphosyntactic
### complexity but the proportion of non-native speakers does not. Submitted
### to: PLOS One.


### getting started

# load packages

library(tidyverse)
library(lme4)
library(ggeffects)
library(effects) # for ggeffect()
library(performance)
library(broom.mixed)
library(viridis) #colour-blind friendly palette
library(ggpubr) #arrange plots


### load data and apply modelling transformations

source("scripts/convert_variables.r")


#######################################################################################


### model syntheticity (SI) and speaker numbers

### build model with Poisson -- including all theoretically motivated triggers and varying intercept and slopes for region; intercept for corpus

SI_max <- glmer(SI ~ log10_natives + prop_non_natives +
			    language_type + (1|corpus) +
			    (1 + log10_natives|macro_region) + 
			    (1 + prop_non_natives|macro_region) +
			     offset(log(total_no_tokens)), data = df, family = 'poisson'
		)


## address singularity

summary(SI_max)

# decorrelate varying slopes as correlated slopes result in singular fit and are perfectly correlated

SI_max_nocorr <- glmer(SI ~ log10_natives + prop_non_natives +
			    language_type +  
			    (1|corpus) +
			    (1|macro_region) + 
			    (0 + log10_natives|macro_region) + 
			    (0 + prop_non_natives|macro_region) +
			     offset(log(total_no_tokens)), data = df, family = 'poisson'
		)


summary(SI_max_nocorr)

# check if slopes have been fitted

coef(SI_max_nocorr) 


## check model assumptions

# check overdispersion

overdisp_fun(SI_max_nocorr)  #using code from Bolker et al. (2017): GLMM FAQ

# check other model assumptions

check_zeroinflation(SI_max_nocorr)

check_collinearity(SI_max_nocorr)


### fit negative binomial to address overdispersion

SI_max_nocorr.nb <- glmer.nb(SI ~ log10_natives + prop_non_natives +
			    language_type +  
			    (1|corpus) +
			    (1|macro_region) + 
			    (0 + log10_natives|macro_region) + 
			    (0 + prop_non_natives|macro_region) +
			     offset(log_tokens), data = df
		)


## check model assumptions

# check overdispersion

overdisp_fun(SI_max_nocorr.nb) # using code from Bolker et al. (2017): GLMM FAQ

# check other model assumptions

check_zeroinflation(SI_max_nocorr.nb)

check_collinearity(SI_max_nocorr.nb)


## check variance of coefficients

coef(SI_max_nocorr.nb) 

## view summary

summary(SI_max_nocorr.nb) 


### get significance of varying intercepts

# remove varying intercept for region

SI_max_nocorr_m0.nb <- glmer.nb(SI ~ log10_natives + prop_non_natives +
			    language_type +  
			    (1|corpus) +
			    #(1|macro_region) + 
			    (0 + log10_natives|macro_region) + 
			    (0 + prop_non_natives|macro_region) +
			     offset(log_tokens), data = df
		)

anova(SI_max_nocorr.nb, SI_max_nocorr_m0.nb) #non-significant


# remove varying intercept for corpus -> results in singular fit

SI_max_nocorr_m1.nb <- glmer.nb(SI ~ log10_natives + prop_non_natives +
			    language_type +  
			    #(1|corpus) +
			    (1|macro_region) + 
			    (0 + log10_natives|macro_region) + 
			    (0 + prop_non_natives|macro_region) +
			     offset(log_tokens), data = df
		)

#anova(SI_max_nocorr.nb, SI_max_nocorr_m1.nb)



### check significance of fixed effects via LRT

#drop1(SI_max_nocorr.nb, test="Chisq") #singular fits

## check separately to see which models result in singular fit

# remove log10 natives

SI_max_nocorr_m2.nb <- glmer.nb(SI ~  prop_non_natives +
			    language_type +  
			    (1|corpus) +
			    (1|macro_region) + 
			    (0 + log10_natives|macro_region) + 
			   (0 + prop_non_natives|macro_region) +
			     offset(log(total_no_tokens)), data = df
		)

anova(SI_max_nocorr.nb, SI_max_nocorr_m2.nb) # significant *

# remove proportion of non-natives -> singular fit 

SI_max_nocorr_m3.nb <- glmer.nb(SI ~ log10_natives + 
			    language_type +  
			    (1|corpus) +
			    (1|macro_region) + 
			    (0 + log10_natives|macro_region) + 
			    (0 + prop_non_natives|macro_region) +
			     offset(log(total_no_tokens)), data = df
		)

#anova(SI_max_nocorr.nb, SI_max_nocorr_m3.nb) #unreliable

# remove language type -> singular fit

SI_max_nocorr_m4.nb <- glmer.nb(SI ~ log10_natives + prop_non_natives +
			    (1|corpus) +
			    (1|macro_region) + 
			    (0 + log10_natives|macro_region) + 
			    (0 + prop_non_natives|macro_region) +
			     offset(log(total_no_tokens)), data = df
		)


#anova(SI_max_nocorr.nb, SI_max_nocorr_m4.nb) #unreliable


# calculate profile confidence intervals instead/additionally

SI_confints <- confint(SI_max_nocorr.nb, oldNames=F)

write.csv(as.data.frame(SI_confints), "stats/SI_confints.csv", row.names=T)

# format for later plotting 

SI_ci <- as.data.frame(SI_confints)

SI_ciFixed <- SI_ci[6:9,]

colnames(SI_ciFixed) <- c("conf.low", "conf.high")



######################### graphs

### plot random effects

## extract random effect variances for region

#for log 10 natives and intercept

SI_ranefn <-  as.data.frame(ranef(SI_max_nocorr.nb))[7:18,]

SI_ranefn$region <- c("Africa", "America", "Asia", "British Isles", "Caribbean", "Oceania")

# nice labels 

SI_ranefn$term <- c( "log10 natives", "log10 natives", "log10 natives", "log10 natives", "log10 natives", "log10 natives","Intercept", "Intercept", "Intercept", "Intercept",
		    "Intercept", "Intercept")

# plot (not used in paper)

SI_randomVar_natives <- SI_ranefn |> ggplot(aes(x = region, y = condval)) +
		geom_point(aes(colour = factor(term)), shape=1, size=3, stroke=1, show.legend=T) +
	 	geom_hline(yintercept = 0, linetype = 3) +
	 	coord_flip() + 
	 	labs(title = NULL, y = "random effect variance for log10 native speakers", x="region") +
		scale_color_viridis(discrete = TRUE) +
 	 	theme_bw() +  #base_family("Arial")
		theme(legend.position = "top", legend.title=element_blank())

ggsave("pics/SI_randomVar_natives.tiff", dpi=300, unit="mm", width=160)

#ggsave("pics/SI_randomVar_natives.png", dpi=400, unit="mm", width=160)


# for prop non-natives and intercept

SI_ranef <-  as.data.frame(ranef(SI_max_nocorr.nb))

SI_ranefnn <- SI_ranef %>%  filter(term != "log10_natives" & grpvar != "corpus")

SI_ranefnn$region <- c("Africa", "America", "Asia", "British Isles", "Caribbean", "Oceania")

SI_ranefnn$term <- c( "prop non-natives", "prop non-natives", "prop non-natives", "prop non-natives", "prop non-natives", "prop non-natives" ,"Intercept", "Intercept", "Intercept", "Intercept",
		     "Intercept", "Intercept")

# plot (not used in paper)

SI_randomVar_prop <- SI_ranefnn |> ggplot(aes(x = region, y = condval)) +
		geom_point(aes(colour = factor(term)), shape=1, size=3, stroke=1, show.legend=T) +
	 	geom_hline(yintercept = 0, linetype = 3) +
	 	coord_flip() + 
	 	labs(title = NULL, y = "random effect variance for proportion of non-native speakers", x="region") + 
	 	#ylim(-0.5, .5) +
		scale_color_viridis(discrete = TRUE) +
 	 	theme_bw() + 
		theme(legend.position = "top", legend.title=element_blank())

ggsave("pics/SI_randomVar_prop.tiff", dpi=300, unit="mm", width=160)

#ggsave("pics/SI_randomVar_prop.png", dpi=400, unit="mm", width=160)



# create combined plot with random variances for both demographic triggers

SI_randomVar_nativesc <- SI_ranefn |> ggplot(aes(x = region, y = condval)) +
		geom_point(aes(colour = factor(term)), shape=1, size=3, stroke=1, show.legend=T) +
	 	geom_hline(yintercept = 0, linetype = 3) +
	 	coord_flip() + 
	 	labs(title = NULL, y = "random effect variances", x="region") +
		scale_color_viridis(discrete = TRUE) +
 	 	theme_bw() +  #base_family("Arial")
		theme(legend.position = "top", legend.title=element_blank())


SI_randomVar_propc <- SI_ranefnn |> ggplot(aes(x = region, y = condval)) +
		geom_point(aes(colour = factor(term)), shape=1, size=3, stroke=1, show.legend=T) +
	 	geom_hline(yintercept = 0, linetype = 3) +
	 	coord_flip() + 
	 	labs(title = NULL, y = "random effect variances", x="") + 
	 	#ylim(-0.5, .5) +
		scale_color_viridis(discrete = TRUE) +
 	 	theme_bw() + 
		theme(legend.position = "top", legend.title=element_blank())


ggarrange(SI_randomVar_nativesc, SI_randomVar_propc, ncol=2, common.legend=F)


ggsave("pics/SI_randomVarEstimates.tiff", dpi=300, unit="mm", width=160)


# for corpus (not used in paper)

SI_ranefc <- SI_ranef %>%  filter(grpvar == "corpus")

SI_ranefc$term <- c("Intercept", "Intercept", "Intercept")

# plot 

SI_randomVar_corp <- SI_ranefc |> ggplot(aes(x = grp, y = condval)) +
		geom_point(aes(colour = factor(term)), shape=1, size=3, stroke=1, show.legend=T) +
	 	geom_hline(yintercept = 0, linetype = 3) +
	 	coord_flip() + 
	 	labs(title = NULL, y = "random effect variance for corpus", x="corpus") +
	 	#ylim(-0.5, .5) +
		scale_color_viridis(discrete = TRUE) +
 	 	theme_bw() + 
		theme(legend.position = "top", legend.title=element_blank())

ggsave("pics/SI_randomVar_corp.tiff", dpi=300, unit="mm", width=160)

#ggsave("pics/SI_randomVar_corp.png", dpi=400, unit="mm", width=160)


### visualise fixed effects

# dots and whisker plot with confidence intervals based on code from Winter (2020: 183)

SIfixefs <-  c("language_typeL2", "language_typeL1c", "prop_non_natives", "log10_natives")

# get estimates and confidence intervals

SIcoefs <- tidy(SI_max_nocorr.nb, conf.int = F) |>
		filter(term %in% SIfixefs)

# attach profile confidence intervals calculated above

SIcoefs <- cbind(SIcoefs, SI_ciFixed)

# order coefficients by estimate and relevel

SIpred_order <- arrange(SIcoefs, estimate)$term

SIcoefs <- mutate(SIcoefs, term = factor(term, levels = SIpred_order))

# plot 

SIcoefs |> ggplot(aes(x = term, y = estimate)) +
	 geom_point(color="#1fa187", size=2, show.legend=F) + #viridis green
	 geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
	 width = 0.3, color="#1fa187") +
	 geom_hline(yintercept = 0, linetype = 2) +
	 coord_flip() + labs(title = NULL, x = "fixed effects", y="coefficient estimates") +
	 ylim(-.4, .4) +
 	 theme_bw(base_family="Arial")

ggsave("pics/SI_dotswhiskers.tiff", dpi=300, unit="mm", width=160) # width: min 6.68cm ; max 19.05cm; height: max 22.23cm

#ggsave("pics/SI_dotswhiskers.png", dpi=400, unit="mm", width=160)



#######################################################################################


### model grammaticity (GI) and speaker numbers

### build model with Poisson -- including all theoretically motivated triggers and varying intercept and slopes for region; and intercept for corpus

GI_max <- glmer(GI ~ log10_natives + prop_non_natives +
			    language_type + 
			    (1|corpus) +
			    (1 + log10_natives|macro_region) + 
			    (1 + prop_non_natives|macro_region) +
			     offset(log_tokens), data = df, family = 'poisson')

# view model

summary(GI_max)

## address singularity

# decorrelate varying slopes

GI_max_nocorr <- glmer(GI ~ log10_natives + prop_non_natives +
			    language_type +  
			    (1|corpus) +
			    (1|macro_region) + 
			    (0 + log10_natives|macro_region) + 
			    (0 + prop_non_natives|macro_region) +
			     offset(log_tokens), data = df, family = 'poisson')


# check if slopes have been fitted

coef(GI_max_nocorr)

## check model assumptions

# check overdispersion

overdisp_fun(GI_max_nocorr)  # using standard Pearson Chi-square (code from Bolker et al. (2017): GLMM FAQ)


# check other model assumptions

check_zeroinflation(GI_max_nocorr)

check_collinearity(GI_max_nocorr)


### fit negative binomial to address overdispersion

GI_max_nocorr.nb <- glmer.nb(GI ~ log10_natives + prop_non_natives +
			    language_type +  
			    (1|corpus) +
			    (1|macro_region) + 
			    (0 + log10_natives|macro_region) + 
			    (0 + prop_non_natives|macro_region) +
			     offset(log_tokens), data = df
			)

summary(GI_max_nocorr.nb)


### check model assumptions

## check overdispersion

# using standard Pearson Chi-square (code from Bolker et al. (2017): GLMM FAQ)

overdisp_fun(GI_max_nocorr.nb)

## check other model assumptions

check_zeroinflation(GI_max_nocorr.nb)

check_collinearity(GI_max_nocorr.nb)


### check variance of coefficients

coef(GI_max_nocorr.nb)


# look at the model

summary(GI_max_nocorr.nb)


### check significance of varying intercepts

# remove varying intercept for region

GI_max_nocorr_m0.nb <- glmer.nb(GI ~ log10_natives + prop_non_natives +
			    language_type +  
			    (1|corpus) +
			    #(1|macro_region) + 
			    (0 + log10_natives|macro_region) + 
			    (0 + prop_non_natives|macro_region) +
			     offset(log_tokens), data = df
		)

anova(GI_max_nocorr.nb, GI_max_nocorr_m0.nb) # significant


# remove varying intercept for corpus

GI_max_nocorr_m1.nb <- glmer.nb(GI ~ log10_natives + prop_non_natives +
			    language_type +  
			    #(1|corpus) +
			    (1|macro_region) + 
			    (0 + log10_natives|macro_region) + 
			    (0 + prop_non_natives|macro_region) +
			     offset(log_tokens), data = df
		)

anova(GI_max_nocorr.nb, GI_max_nocorr_m1.nb) #not significant


### check significance of fixed effects via LRT

drop1(GI_max_nocorr.nb, test="Chisq")


# additionally calculate profiled confidence intervals 

GI_confints <- confint(GI_max_nocorr.nb, oldNames=F)

write.csv(as.data.frame(GI_confints), "stats/GI_confints.csv", row.names=T)

# format for later plotting 

GI_ci <- as.data.frame(GI_confints)

GI_ciFixed <- GI_ci[6:9,]

colnames(GI_ciFixed) <- c("conf.low", "conf.high")



###################################### graphs

### visualise fixed effects

## dots and whisker plot with confidence intervals based on code from Winter (2020: 183)

# only plot fixed effects

GIfixefs <-  c("language_typeL2", "language_typeL1c", "prop_non_natives", "log10_natives")

# get estimates; filter for fixed effects

GIcoefs <- tidy(GI_max_nocorr.nb, conf.int = F) |>
		filter(term %in% GIfixefs)

# attach profile confidence intervals; calculated with confint()

GIcoefs <- cbind(GIcoefs, GI_ciFixed)

# order coefficients by estimate and relevel

GIpred_order <- arrange(GIcoefs, estimate)$term

GIcoefs <- mutate(GIcoefs, term = factor(term, levels = GIpred_order))

# plot 

GIcoefs |> ggplot(aes(x = term, y = estimate)) +
	 geom_point(color="#1fa187", size=2, show.legend=F) + #viridis green
	 geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
	 width = 0.3, color="#1fa187") +
	 geom_hline(yintercept = 0, linetype = 2) +
	 coord_flip() + labs(title = NULL, x = "fixed effects", y="coefficient estimates") +
 	 theme_bw(base_family="Arial")


ggsave("pics/GI_dotswhiskers.tiff", dpi=300, unit="mm", width=160)

#ggsave("pics/GI_dotswhiskers.png", dpi=400, unit="mm", width=160)


### plot random effects

## extract random effect variances for region

#for log 10 natives and intercept

GI_ranefn <-  as.data.frame(ranef(GI_max_nocorr.nb))[7:18,]

GI_ranefn$region <- c("Africa", "America", "Asia", "British Isles", "Caribbean", "Oceania")

# nice labels 

GI_ranefn$term <- c( "log10 natives", "log10 natives", "log10 natives", "log10 natives", "log10 natives", "log10 natives","Intercept", "Intercept", "Intercept", "Intercept",
		    "Intercept", "Intercept")

# plot

GI_randomVar_natives <- GI_ranefn |> ggplot(aes(x = region, y = condval)) +
		geom_point(aes(colour = factor(term)), shape=1, size=3, stroke=1, show.legend=T) +
	 	geom_hline(yintercept = 0, linetype = 3) +
	 	coord_flip() + 
	 	labs(title = NULL, y = "random effect variance for log10 native speakers", x="region") +
		scale_color_viridis(discrete = TRUE) +
 	 	theme_bw() +  #base_family("Arial")
		theme(legend.position = "top", legend.title=element_blank())

ggsave("pics/GI_randomVar_natives.tiff", dpi=300, unit="mm", width=160)

#ggsave("pics/GI_randomVar_natives.png", dpi=400, unit="mm", width=160)


# for prop non-natives and intercept

GI_ranef <-  as.data.frame(ranef(GI_max_nocorr.nb))

GI_ranefnn <- GI_ranef %>%  filter(term != "log10_natives" & grpvar != "corpus")

GI_ranefnn$region <- c("Africa", "America", "Asia", "British Isles", "Caribbean", "Oceania")

GI_ranefnn$term <- c( "prop non-natives", "prop non-natives", "prop non-natives", "prop non-natives", "prop non-natives", "prop non-natives" ,"Intercept", "Intercept", "Intercept", "Intercept",
		     "Intercept", "Intercept")

# plot 

GI_randomVar_prop <- GI_ranefnn |> ggplot(aes(x = region, y = condval)) +
		geom_point(aes(colour = factor(term)), shape=1, size=3, stroke=1, show.legend=T) +
	 	geom_hline(yintercept = 0, linetype = 3) +
	 	coord_flip() + 
	 	labs(title = NULL, y = "random effect variance for proportion of non-native speakers", x="region") +
	 	#ylim(-0.5, .5) +
		scale_color_viridis(discrete = TRUE) +
 	 	theme_bw() + 
		theme(legend.position = "top", legend.title=element_blank())

ggsave("pics/GI_randomVar_prop.tiff", dpi=300, unit="mm", width=160)

#ggsave("pics/GI_randomVar_prop.png", dpi=400, unit="mm", width=160)


# create combined plot with random variances for both demographic triggers

GI_randomVar_nativesc <- GI_ranefn |> ggplot(aes(x = region, y = condval)) +
		geom_point(aes(colour = factor(term)), shape=1, size=3, stroke=1, show.legend=T) +
	 	geom_hline(yintercept = 0, linetype = 3) +
	 	coord_flip() + 
	 	labs(title = NULL, y = "random effect variance", x="region") +
		scale_color_viridis(discrete = TRUE) +
 	 	theme_bw() +  #base_family("Arial")
		theme(legend.position = "top", legend.title=element_blank())


GI_randomVar_propc <- GI_ranefnn |> ggplot(aes(x = region, y = condval)) +
		geom_point(aes(colour = factor(term)), shape=1, size=3, stroke=1, show.legend=T) +
	 	geom_hline(yintercept = 0, linetype = 3) +
	 	coord_flip() + 
	 	labs(title = NULL, y = "random effect variance", x="") +
	 	#ylim(-0.5, .5) +
		scale_color_viridis(discrete = TRUE) +
 	 	theme_bw() + 
		theme(legend.position = "top", legend.title=element_blank())


ggarrange(GI_randomVar_nativesc, GI_randomVar_propc, ncol=2, common.legend=F)


ggsave("pics/GI_randomVarEstimates.tiff", dpi=300, unit="mm", width=160)


# for corpus

GI_ranefc <- GI_ranef %>%  filter(grpvar == "corpus")

GI_ranefc$term <- c("Intercept", "Intercept", "Intercept")

# plot 

GI_randomVar_corp <- GI_ranefc |> ggplot(aes(x = grp, y = condval)) +
		geom_point(aes(colour = factor(term)), shape=1, size=3, stroke=1, show.legend=T) +
	 	geom_hline(yintercept = 0, linetype = 3) +
	 	coord_flip() + 
	 	labs(title = NULL, y = "random effect variance for corpus", x="corpus") +
	 	#ylim(-0.5, .5) +
		scale_color_viridis(discrete = TRUE) +
 	 	theme_bw() + 
		theme(legend.position = "top", legend.title=element_blank())

ggsave("pics/GI_randomVar_corp.tiff", dpi=300, unit="mm", width=160)

#ggsave("pics/GI_randomVar_corp.png", dpi=400, unit="mm", width=160)







