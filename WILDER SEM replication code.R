#######################################################################################################
#                                                                                                     #
# SEM replication code for Wilder, Matt. (2021). "Can we afford to be more like Scandinavians?"       #
# Available at SSRN: http://dx.doi.org/10.2139/ssrn.3808057                                           #
#                                                                                                     #
# Working paper -- please report errors/bugs, or make suggestions: matt@mattwilder.net                #
#                                                                                                     #
#######################################################################################################

#######################
# read in patent data #
#######################

data <- read.delim("./output_data.csv",  header=TRUE, sep="\t")


# required packages (run install.packages("package name") if necessary)
library(lavaan)
library(semPlot)
library(tidyverse)
library(ggplot2)
library(plotly)

# convert variable names to the case of naming conventions
data <- data %>% 
  dplyr::rename(CORP_GOVA = corp_gova, CORP_GOVI = corp_govi, IFM_RELA = ifm_rela, IFM_RELI = ifm_reli,
                FRM_HIERA = frm_hiera, FRM_HIERI = frm_hieri,  EMP_RELA = emp_rela, EMP_RELI = emp_reli,
                OCC_TRAINA = occ_traina, OCC_TRAINI = occ_traini)

# run the line below to test whether bi-gram and tri-gram radicality corresponds with advantage in incremental innovation 
# data$total_radicality <- data$b_radicality + data$t_radicality


##################################
# text-based radicality measures #
##################################

hist(data$total_radicality)

datasub <- subset(data, total_radicality > 0)
datasub$log_total_radicality <- log(datasub$total_radicality)

hist(datasub$log_total_radicality)

# fit SEM
# =~ denotes latent variable, ~~ denotes covariance and correlation, ~ denotes direct prediction
model1.fit <- cfa(model ='configuration =~ CORP_GOVA + IFM_RELA + FRM_HIERA + EMP_RELA + OCC_TRAINA 
                     log_total_radicality ~ configuration', data = datasub)
# configuration is a latent variable that causes scores on institutional indicators
# configuration is the predictor for radicality

summary(model1.fit, standardized = TRUE, fit.measures = TRUE)
# estimate coefficients are in the scale of the data, std.all is a standardized loading of the manifest variables on the latent variable 0.3 is the threshold for poor loading/model fit
# CFI and TLI above 0.9 is very good model fit
# RMSEA and SRMR should be below 0.1 is very good model fit



# remove US to assess robustness of Taylor's (2004) findings
dataNUS = filter(datasub, assignee_country != "US" & inventor_country != "US")


model2.fit <- cfa(model ='configuration =~ CORP_GOVA + IFM_RELA + FRM_HIERA + EMP_RELA + OCC_TRAINA 
                     log_total_radicality ~ configuration',
                   data = dataNUS)
summary(model2.fit, standardized = TRUE, fit.measures = TRUE)




######################################
# citation-based radicality measures #
######################################


# raw citation counts as DV

hist(data$forward_citations)
hist(log(data$forward_citations))

# add a small constant to 0 scores so they aren't lost when transformed to a log 
data$temp <- data$forward_citations + 0.1

data$log_forward_citations <- log(data$temp)

hist(data$log_forward_citations)

# =~ denotes latent variable, ~~ denotes covariance and correlation, ~ denotes direct prediction
model3.fit <- cfa(model ='configuration =~ CORP_GOVA + IFM_RELA + FRM_HIERA + EMP_RELA + OCC_TRAINA 
                     log_forward_citations ~ configuration', data = data)
# configuration is a latent variable that causes scores on institutional indicators
# configuration is the predictor for radicality

summary(model3.fit, standardized = TRUE, fit.measures = TRUE)
# estimate coefficients are in the scale of the data, std.all is a standardized loading of the manifest variables on the latent variable 0.3 is the threshold for poor loading/model fit
# CFI and TLI above 0.9 is very good model fit
# RMSEA and SRMR should be below 0.1 is very good model fit



# remove US to assess robustness of Taylor's (2004) findings
dataNUS = filter(data, assignee_country != "US" & inventor_country != "US")


hist(dataNUS$log_forward_citations)

model4.fit <- cfa(model ='configuration =~ CORP_GOVA + IFM_RELA + FRM_HIERA + EMP_RELA + OCC_TRAINA 
                     log_forward_citations ~ configuration',
                   data = dataNUS)
summary(model4.fit, standardized = TRUE, fit.measures = TRUE)




# backward citations 

hist(data$backward_citations)
hist(log(data$backward_citations))

# add a small constant to 0 scores so they aren't lost when transformed to a log 
data$temp <- data$backward_citations + 0.1

data$log_backward_citations <- log(data$temp)

hist(data$log_backward_citations)

# =~ denotes latent variable, ~~ denotes covariance and correlation, ~ denotes direct prediction
model5.fit <- cfa(model ='configuration =~ CORP_GOVA + IFM_RELA + FRM_HIERA + EMP_RELA + OCC_TRAINA 
                     log_backward_citations ~ configuration', data = data)
# configuration is a latent variable that causes scores on institutional indicators
# configuration is the predictor for radicality

summary(model5.fit, standardized = TRUE, fit.measures = TRUE)
# estimate coefficients are in the scale of the data, std.all is a standardized loading of the manifest variables on the latent variable 0.3 is the threshold for poor loading/model fit

# CFI and TLI above 0.9 is very good model fit
# RMSEA and SRMR should be below 0.1 is very good model fit





######################################
#            generality              #
######################################

hist(data$generality_hhi)
hist(log(data$generality_hhi))

# add a small constant to 0 scores so they aren't lost when transformed to a log 
data$temp <- data$generality_hhi + 0.1

data$log_generality_hhi <- log(data$temp)

hist(data$log_generality_hhi)

# =~ denotes latent variable, ~~ denotes covariance and correlation, ~ denotes direct prediction
model6.fit <- cfa(model ='configuration =~ CORP_GOVA + IFM_RELA + FRM_HIERA + EMP_RELA + OCC_TRAINA 
                     log_generality_hhi ~ configuration', data = data)
# configuration is a latent variable that causes scores on institutional indicators
# configuration is the predictor for radicality

summary(model6.fit, standardized = TRUE, fit.measures = TRUE)
# estimate coefficients are in the scale of the data, std.all is a standardized loading of the manifest variables on the latent variable 0.3 is the threshold for poor loading/model fit

# CFI and TLI above 0.9 is very good model fit
# RMSEA and SRMR should be below 0.1 is very good model fit




######################################
#            originality             #
######################################

hist(data$originality_hhi)
hist(log(data$originality_hhi))

# add a small constant to 0 scores so they aren't lost when transformed to a log 
data$temp <- data$originality_hhi + 0.1

data$log_originality_hhi <- log(data$temp)

hist(data$log_originality_hhi)

# =~ denotes latent variable, ~~ denotes covariance and correlation, ~ denotes direct prediction
model7.fit <- cfa(model ='configuration =~ CORP_GOVA + IFM_RELA + FRM_HIERA + EMP_RELA + OCC_TRAINA 
                     log_originality_hhi ~ configuration', data = data)
# configuration is a latent variable that causes scores on institutional indicators
# configuration is the predictor for radicality

summary(model7.fit, standardized = TRUE, fit.measures = TRUE)
# estimate coefficients are in the scale of the data, std.all is a standardized loading of the manifest variables on the latent variable 0.3 is the threshold for poor loading/model fit

# CFI and TLI above 0.9 is very good model fit
# RMSEA and SRMR should be below 0.1 is very good model fit





############################################
#            citation deflators            #
############################################ 
#add a small constant to 0 scores so they aren't lost when transformed to a log 
data$temp <- data$forward_citations + 0.1


data$deflate_1 <- data$temp/data$weight_1*0.009



data$log_deflate_1 <- log(data$deflate_1)

hist(data$log_deflate_1)


model8.fit <- cfa(model ='configuration =~ CORP_GOVA + IFM_RELA + FRM_HIERA + EMP_RELA + OCC_TRAINA 
log_deflate_1 ~ configuration', data = data)

summary(model8.fit, standardized = TRUE, fit.measures = TRUE)



# and with the other weight, weight_2
data$deflate_2 <- data$temp/data$weight_2*0.197 

data$log_deflate_2 <- log(data$deflate_2)

hist(data$log_deflate_2)


model9.fit <- cfa(model ='configuration =~ CORP_GOVA + IFM_RELA + FRM_HIERA + EMP_RELA + OCC_TRAINA 
log_deflate_2 ~ configuration', data = data)

summary(model9.fit, standardized = TRUE, fit.measures = TRUE)
