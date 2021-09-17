#######################################################################################################
#                                                                                                     #
# fuzzy-set replication code for Wilder, Matt. (2021). "Can we afford to be more like Scandinavians?" #
# Available at SSRN: http://dx.doi.org/10.2139/ssrn.3808057                                           #
#                                                                                                     #
# Working paper -- please report errors/bugs, or make suggestions: matt@mattwilder.net                #
#                                                                                                     #
#######################################################################################################


# required packages (run install.packages("package name") if necessary)
library(psych); library(base); library(dplyr); library(QCA); library(SetMethods)


#######################
# read in patent data #
#######################

data <- read.delim("./output_data.csv",  header=TRUE, sep="\t", stringsAsFactors=FALSE)


data$date <- as.Date(data$date) # format date column as date type


# remove duplicate patents 
data = filter(data, patent != "5693762" & patent != "5585089" & patent != "6573273")


# remove US cases if desired
#data = filter(data, assignee_country != "US")
#data = filter(data, inventor_country != "US")



################################################
#                                              #
#      fsQCA CALIBRATION (direct method)       #
#                                              #
################################################

# in QCA it is conventional to use lower case or ~ for negation, + for logical OR and * for logical AND
# rename institutional variables upper case to respect QCA conventions


data <- data %>% 
        dplyr::rename(CORP_GOVA = corp_gova, CORP_GOVI = corp_govi, IFM_RELA = ifm_rela, IFM_RELI = ifm_reli,
                FRM_HIERA = frm_hiera, FRM_HIERI = frm_hieri,  EMP_RELA = emp_rela, EMP_RELI = emp_reli,
                OCC_TRAINA = occ_traina, OCC_TRAINI = occ_traini)

# subset patent data to 30 year overlapping periods
data6090 <- subset(data, date > "1959-12-31" & date < "1990-01-01")
data7000 <- subset(data, date > "1969-12-31" & date < "2000-01-01")
data8010 <- subset(data, date > "1979-12-31" & date < "2010-01-01")
data9020 <- subset(data, date > "1989-12-31" & date < "2021-01-01")



########################################
#####        full period          ######
########################################

# get patent counts by country
data %>% dplyr::count(assignee_country)

# get radical patent counts by country
quantile(data$total_radicality, c(.25, .50, .75, .90, .95, .99)) 


data$FSRADICALITY <- calibrate(data$u_radicality, type = "fuzzy", 
                               thresholds = "e=  0, c= 145.0, i= 6057.8", ecdf = TRUE)


keepsa <- c("FSRADICALITY", "CORP_GOVA", "IFM_RELA", "FRM_HIERA", "EMP_RELA",
           "OCC_TRAINA") # assignee

keepsi <- c("FSRADICALITY",  "CORP_GOVI", "IFM_RELI", "FRM_HIERI", "EMP_RELI",
            "OCC_TRAINI") # inventor

data_a <- data[keepsa]
data_i <- data[keepsi]


######################
#  US configuration  #
######################

# assignee
pof("~OCC_TRAINA*~FRM_HIERA*~IFM_RELA*~EMP_RELA*~CORP_GOVA <= FSRADICALITY", data = data_a) # <= denotes necessity, => denotes sufficiency, + = logical OR, * = logical AND ~ = negation

# inventor
pof("~OCC_TRAINI*~FRM_HIERI*~IFM_RELI*~EMP_RELI*~CORP_GOVI <= FSRADICALITY", data = data_i) 


###########################
#  UK, Canada, Australia  #
###########################

pof("~OCC_TRAINA*~FRM_HIERA*~IFM_RELA*~CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*~IFM_RELI*~CORP_GOVI <= FSRADICALITY", data = data_i)



##############
#  Ireland   #
##############

pof("~OCC_TRAINA*~FRM_HIERA*~IFM_RELA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*~IFM_RELI <= FSRADICALITY", data = data_i)  


##################
#  New Zealand   #
##################
pof("~OCC_TRAINA*~FRM_HIERA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*CORP_GOVI <= FSRADICALITY", data = data_i) 



############
#  Spain   #
############
pof("~OCC_TRAINA*IFM_RELA*~EMP_RELA <= FSRADICALITY", data = data_a)
pof("~OCC_TRAINI*IFM_RELI*~EMP_RELI <= FSRADICALITY", data = data_i)



############
#  France  #
############
pof("FRM_HIERA <= FSRADICALITY", data = data_a)
pof("FRM_HIERI <= FSRADICALITY", data = data_i) 


#########################
#  Portugal and Greece  #
#########################

pof("~OCC_TRAINA*~FRM_HIERA*IFM_RELA*EMP_RELA*CORP_GOVA <=  FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*IFM_RELI*EMP_RELI*CORP_GOVI <=  FSRADICALITY", data = data_i) 


############
#  Italy   #
############

pof("OCC_TRAINA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 


############
#  Japan   #
############

pof("~OCC_TRAINA*~FRM_HIERA*IFM_RELA*EMP_RELA*~CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*IFM_RELI*EMP_RELI*~CORP_GOVI <= FSRADICALITY", data = data_i)


###############
#  S. Korea   #
###############
 
pof("~OCC_TRAINA*~FRM_HIERA*IFM_RELA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*IFM_RELI <= FSRADICALITY", data = data_i) 


#############
#  Finland  #
#############

pof("~OCC_TRAINA*FRM_HIERA*~IFM_RELA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*FRM_HIERI*~IFM_RELI <= FSRADICALITY", data = data_i)

##################
#  Switzerland   #
##################

pof("OCC_TRAINA*EMP_RELA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*EMP_RELI <= FSRADICALITY", data = data_i) 

##################
#    Denmark     #
##################
pof("~OCC_TRAINA*FRM_HIERA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*FRM_HIERI*CORP_GOVI <= FSRADICALITY", data = data_i)


###############################
#    Sweden & Netherlands     #
###############################

pof("FRM_HIERA*EMP_RELA <= FSRADICALITY", data = data_a) 
pof("FRM_HIERI*EMP_RELI <= FSRADICALITY", data = data_i) 


##################
#    Norway      #
##################
pof("~OCC_TRAINA*FRM_HIERA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*FRM_HIERI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 


##################
#    Belgium     #
##################
pof("OCC_TRAINA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 


##################
#    Germany     #
##################
pof("OCC_TRAINA*FRM_HIERA*IFM_RELA*EMP_RELA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*FRM_HIERI*IFM_RELI*EMP_RELI <= FSRADICALITY", data = data_i) 


##################
#    Austria     #
##################
pof("OCC_TRAINA*FRM_HIERA*IFM_RELA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*FRM_HIERI*IFM_RELI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 




#########################################
############### 60-90 ###################
#########################################

# get patent counts by country
data6090 %>% dplyr::count(assignee_country)

# get radical patent counts by country
quantile(data6090$total_radicality, c(.25, .50, .75, .90, .95, .99)) 

    
# e = complete nonmembership value' c = the crossover value (re: more/less in than out) i=complete membership
data6090$FSRADICALITY<- calibrate(data6090$total_radicality, type = "fuzzy", 
                                   thresholds = "e=  0,  c= 6844.0 , i=77771.6 ", ecdf = TRUE)


keepsa <- c("FSRADICALITY", "CORP_GOVA", "IFM_RELA", "FRM_HIERA", "EMP_RELA",
           "OCC_TRAINA")

keepsi <- c("FSRADICALITY","CORP_GOVI", "IFM_RELI", "FRM_HIERI", "EMP_RELI",
            "OCC_TRAINI")

data_a <- data6090[keepsa]
data_i <- data6090[keepsi]



######################
#  US configuration  #
######################

# assignee
pof("~OCC_TRAINA*~FRM_HIERA*~IFM_RELA*~EMP_RELA*~CORP_GOVA <= FSRADICALITY", data = data_a) # <= denotes necessity, => denotes sufficiency, + = logical OR, * = logical AND ~ = negation

# inventor
pof("~OCC_TRAINI*~FRM_HIERI*~IFM_RELI*~EMP_RELI*~CORP_GOVI <= FSRADICALITY", data = data_i) 


###########################
#  UK, Canada, Australia  #
###########################

pof("~OCC_TRAINA*~FRM_HIERA*~IFM_RELA*~CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*~IFM_RELI*~CORP_GOVI <= FSRADICALITY", data = data_i)


##############
#  Ireland   #
##############

pof("~OCC_TRAINA*~FRM_HIERA*~IFM_RELA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*~IFM_RELI <= FSRADICALITY", data = data_i)  


##################
#  New Zealand   #
##################
pof("~OCC_TRAINA*~FRM_HIERA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*CORP_GOVI <= FSRADICALITY", data = data_i) 



############
#  Spain   #
############
pof("~OCC_TRAINA*IFM_RELA*~EMP_RELA <= FSRADICALITY", data = data_a)
pof("~OCC_TRAINI*IFM_RELI*~EMP_RELI <= FSRADICALITY", data = data_i)



############
#  France  #
############
pof("FRM_HIERA <= FSRADICALITY", data = data_a)
pof("FRM_HIERI <= FSRADICALITY", data = data_i) 


#########################
#  Portugal and Greece  #
#########################

pof("~OCC_TRAINA*~FRM_HIERA*IFM_RELA*EMP_RELA*CORP_GOVA <=  FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*IFM_RELI*EMP_RELI*CORP_GOVI <=  FSRADICALITY", data = data_i) 


############
#  Italy   #
############

pof("OCC_TRAINA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 


############
#  Japan   #
############

pof("~OCC_TRAINA*~FRM_HIERA*IFM_RELA*EMP_RELA*~CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*IFM_RELI*EMP_RELI*~CORP_GOVI <= FSRADICALITY", data = data_i)


###############
#  S. Korea   #
###############

pof("~OCC_TRAINA*~FRM_HIERA*IFM_RELA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*IFM_RELI <= FSRADICALITY", data = data_i) 


#############
#  Finland  #
#############

pof("~OCC_TRAINA*FRM_HIERA*~IFM_RELA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*FRM_HIERI*~IFM_RELI <= FSRADICALITY", data = data_i)


##################
#  Switzerland   #
##################

pof("OCC_TRAINA*EMP_RELA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*EMP_RELI <= FSRADICALITY", data = data_i) 


##################
#    Denmark     #
##################
pof("~OCC_TRAINA*FRM_HIERA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*FRM_HIERI*CORP_GOVI <= FSRADICALITY", data = data_i)


###############################
#    Sweden & Netherlands     #
###############################

pof("FRM_HIERA*EMP_RELA <= FSRADICALITY", data = data_a) 
pof("FRM_HIERI*EMP_RELI <= FSRADICALITY", data = data_i) 


##################
#    Norway      #
##################
pof("~OCC_TRAINA*FRM_HIERA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*FRM_HIERI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 


##################
#    Belgium     #
##################
pof("OCC_TRAINA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 


##################
#    Germany     #
##################
pof("OCC_TRAINA*FRM_HIERA*IFM_RELA*EMP_RELA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*FRM_HIERI*IFM_RELI*EMP_RELI <= FSRADICALITY", data = data_i) 


##################
#    Austria     #
##################
pof("OCC_TRAINA*FRM_HIERA*IFM_RELA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*FRM_HIERI*IFM_RELI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 



#########################################
############### 70-00 ###################
#########################################


# get patent counts by country
data7000 %>% dplyr::count(assignee_country)


quantile(data7000$total_radicality, c(.25, .50, .75, .90, .95, .99))    

# e = complete nonmembership value' c = the crossover value (re: more/less in than out) i=complete membership
data7000$FSRADICALITY <- calibrate(data7000$total_radicality, type = "fuzzy", 
                                     thresholds = "e=  0,  c=  2350.0, i=24534.2", ecdf = TRUE)


# keep only needed columns 
keepsa <- c("FSRADICALITY","CORP_GOVA", "IFM_RELA", "FRM_HIERA", "EMP_RELA",
            "OCC_TRAINA")

keepsi <- c("FSRADICALITY", "CORP_GOVI", "IFM_RELI", "FRM_HIERI", "EMP_RELI",
            "OCC_TRAINI")

data_a <- data7000[keepsa]
data_i <- data7000[keepsi]



######################
#  US configuration  #
######################

# assignee
pof("~OCC_TRAINA*~FRM_HIERA*~IFM_RELA*~EMP_RELA*~CORP_GOVA <= FSRADICALITY", data = data_a) # <= denotes necessity, => denotes sufficiency, + = logical OR, * = logical AND ~ = negation

# inventor
pof("~OCC_TRAINI*~FRM_HIERI*~IFM_RELI*~EMP_RELI*~CORP_GOVI <= FSRADICALITY", data = data_i) 


###########################
#  UK, Canada, Australia  #
###########################

pof("~OCC_TRAINA*~FRM_HIERA*~IFM_RELA*~CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*~IFM_RELI*~CORP_GOVI <= FSRADICALITY", data = data_i)


##############
#  Ireland   #
##############

pof("~OCC_TRAINA*~FRM_HIERA*~IFM_RELA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*~IFM_RELI <= FSRADICALITY", data = data_i)  


##################
#  New Zealand   #
##################
pof("~OCC_TRAINA*~FRM_HIERA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*CORP_GOVI <= FSRADICALITY", data = data_i) 



############
#  Spain   #
############
pof("~OCC_TRAINA*IFM_RELA*~EMP_RELA <= FSRADICALITY", data = data_a)
pof("~OCC_TRAINI*IFM_RELI*~EMP_RELI <= FSRADICALITY", data = data_i)



############
#  France  #
############
pof("FRM_HIERA <= FSRADICALITY", data = data_a)
pof("FRM_HIERI <= FSRADICALITY", data = data_i) 


#########################
#  Portugal and Greece  #
#########################

pof("~OCC_TRAINA*~FRM_HIERA*IFM_RELA*EMP_RELA*CORP_GOVA <=  FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*IFM_RELI*EMP_RELI*CORP_GOVI <=  FSRADICALITY", data = data_i) 


############
#  Italy   #
############

pof("OCC_TRAINA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 


############
#  Japan   #
############

pof("~OCC_TRAINA*~FRM_HIERA*IFM_RELA*EMP_RELA*~CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*IFM_RELI*EMP_RELI*~CORP_GOVI <= FSRADICALITY", data = data_i)


###############
#  S. Korea   #
###############

pof("~OCC_TRAINA*~FRM_HIERA*IFM_RELA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*IFM_RELI <= FSRADICALITY", data = data_i) 


#############
#  Finland  #
#############

pof("~OCC_TRAINA*FRM_HIERA*~IFM_RELA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*FRM_HIERI*~IFM_RELI <= FSRADICALITY", data = data_i)


##################
#  Switzerland   #
##################

pof("OCC_TRAINA*EMP_RELA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*EMP_RELI <= FSRADICALITY", data = data_i) 


##################
#    Denmark     #
##################
pof("~OCC_TRAINA*FRM_HIERA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*FRM_HIERI*CORP_GOVI <= FSRADICALITY", data = data_i)


###############################
#    Sweden & Netherlands     #
###############################

pof("FRM_HIERA*EMP_RELA <= FSRADICALITY", data = data_a) 
pof("FRM_HIERI*EMP_RELI <= FSRADICALITY", data = data_i) 


##################
#    Norway      #
##################
pof("~OCC_TRAINA*FRM_HIERA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*FRM_HIERI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 


##################
#    Belgium     #
##################
pof("OCC_TRAINA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 


##################
#    Germany     #
##################
pof("OCC_TRAINA*FRM_HIERA*IFM_RELA*EMP_RELA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*FRM_HIERI*IFM_RELI*EMP_RELI <= FSRADICALITY", data = data_i) 


##################
#    Austria     #
##################
pof("OCC_TRAINA*FRM_HIERA*IFM_RELA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*FRM_HIERI*IFM_RELI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 




#######################################
############## 80-10 ##################
#######################################


# get patent counts by country
data8010 %>% dplyr::count(assignee_country)


# making the fuzzy set variable for radicality for 1960-1990 
quantile(data8010$total_radicality, c(.25, .50, .75, .90, .95, .99))    

# e = complete nonmembership value' c = the crossover value (re: more/less in than out) i=complete membership
data8010$FSRADICALITY <- calibrate(data8010$total_radicality, type = "fuzzy", 
                                     thresholds = "e=  0,  c=  473.0, i=9076.0", ecdf = TRUE)

# keep only needed columns 
keepsa <- c("FSRADICALITY","CORP_GOVA", "IFM_RELA", "FRM_HIERA", "EMP_RELA",
            "OCC_TRAINA")

keepsi <- c("FSRADICALITY", "CORP_GOVI", "IFM_RELI", "FRM_HIERI", "EMP_RELI",
            "OCC_TRAINI")

data_a <- data8010[keepsa]
data_i <- data8010[keepsi]



######################
#  US configuration  #
######################

# assignee
pof("~OCC_TRAINA*~FRM_HIERA*~IFM_RELA*~EMP_RELA*~CORP_GOVA <= FSRADICALITY", data = data_a) # <= denotes necessity, => denotes sufficiency, + = logical OR, * = logical AND ~ = negation

# inventor
pof("~OCC_TRAINI*~FRM_HIERI*~IFM_RELI*~EMP_RELI*~CORP_GOVI <= FSRADICALITY", data = data_i) 


###########################
#  UK, Canada, Australia  #
###########################

pof("~OCC_TRAINA*~FRM_HIERA*~IFM_RELA*~CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*~IFM_RELI*~CORP_GOVI <= FSRADICALITY", data = data_i)


##############
#  Ireland   #
##############

pof("~OCC_TRAINA*~FRM_HIERA*~IFM_RELA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*~IFM_RELI <= FSRADICALITY", data = data_i)  


##################
#  New Zealand   #
##################
pof("~OCC_TRAINA*~FRM_HIERA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*CORP_GOVI <= FSRADICALITY", data = data_i) 


############
#  Spain   #
############
pof("~OCC_TRAINA*IFM_RELA*~EMP_RELA <= FSRADICALITY", data = data_a)
pof("~OCC_TRAINI*IFM_RELI*~EMP_RELI <= FSRADICALITY", data = data_i)



############
#  France  #
############
pof("FRM_HIERA <= FSRADICALITY", data = data_a)
pof("FRM_HIERI <= FSRADICALITY", data = data_i) 


#########################
#  Portugal and Greece  #
#########################

pof("~OCC_TRAINA*~FRM_HIERA*IFM_RELA*EMP_RELA*CORP_GOVA <=  FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*IFM_RELI*EMP_RELI*CORP_GOVI <=  FSRADICALITY", data = data_i) 


############
#  Italy   #
############

pof("OCC_TRAINA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 


############
#  Japan   #
############

pof("~OCC_TRAINA*~FRM_HIERA*IFM_RELA*EMP_RELA*~CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*IFM_RELI*EMP_RELI*~CORP_GOVI <= FSRADICALITY", data = data_i)


###############
#  S. Korea   #
###############

pof("~OCC_TRAINA*~FRM_HIERA*IFM_RELA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*IFM_RELI <= FSRADICALITY", data = data_i) 


#############
#  Finland  #
#############

pof("~OCC_TRAINA*FRM_HIERA*~IFM_RELA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*FRM_HIERI*~IFM_RELI <= FSRADICALITY", data = data_i)


##################
#  Switzerland   #
##################

pof("OCC_TRAINA*EMP_RELA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*EMP_RELI <= FSRADICALITY", data = data_i) 


##################
#    Denmark     #
##################
pof("~OCC_TRAINA*FRM_HIERA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*FRM_HIERI*CORP_GOVI <= FSRADICALITY", data = data_i)


###############################
#    Sweden & Netherlands     #
###############################

pof("FRM_HIERA*EMP_RELA <= FSRADICALITY", data = data_a) 
pof("FRM_HIERI*EMP_RELI <= FSRADICALITY", data = data_i) 


##################
#    Norway      #
##################
pof("~OCC_TRAINA*FRM_HIERA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*FRM_HIERI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 


##################
#    Belgium     #
##################
pof("OCC_TRAINA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 


##################
#    Germany     #
##################
pof("OCC_TRAINA*FRM_HIERA*IFM_RELA*EMP_RELA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*FRM_HIERI*IFM_RELI*EMP_RELI <= FSRADICALITY", data = data_i) 


##################
#    Austria     #
##################
pof("OCC_TRAINA*FRM_HIERA*IFM_RELA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*FRM_HIERI*IFM_RELI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 





#########################################
############### 90-20 ###################
#########################################


# get patent counts by country
data9020 %>% dplyr::count(assignee_country)


quantile(data9020$total_radicality, c(.25, .50, .75, .90, .95, .99))    


data9020$FSRADICALITY <- calibrate(data9020$total_radicality, type = "fuzzy", 
                                     thresholds = "e=  0,  c= 125.00 , i=4877.70 ", ecdf = TRUE)


# keep only needed columns 
keepsa <- c("FSRADICALITY","CORP_GOVA", "IFM_RELA", "FRM_HIERA", "EMP_RELA",
            "OCC_TRAINA")

keepsi <- c("FSRADICALITY", "CORP_GOVI", "IFM_RELI", "FRM_HIERI", "EMP_RELI",
            "OCC_TRAINI")

data_a <- data9020[keepsa]
data_i <- data9020[keepsi]



######################
#  US configuration  #
######################

# assignee
pof("~OCC_TRAINA*~FRM_HIERA*~IFM_RELA*~EMP_RELA*~CORP_GOVA <= FSRADICALITY", data = data_a) # <= denotes necessity, => denotes sufficiency, + = logical OR, * = logical AND ~ = negation

# inventor
pof("~OCC_TRAINI*~FRM_HIERI*~IFM_RELI*~EMP_RELI*~CORP_GOVI <= FSRADICALITY", data = data_i) 


###########################
#  UK, Canada, Australia  #
###########################

pof("~OCC_TRAINA*~FRM_HIERA*~IFM_RELA*~CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*~IFM_RELI*~CORP_GOVI <= FSRADICALITY", data = data_i)


##############
#  Ireland   #
##############

pof("~OCC_TRAINA*~FRM_HIERA*~IFM_RELA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*~IFM_RELI <= FSRADICALITY", data = data_i)  


##################
#  New Zealand   #
##################
pof("~OCC_TRAINA*~FRM_HIERA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*CORP_GOVI <= FSRADICALITY", data = data_i) 


############
#  Spain   #
############
pof("~OCC_TRAINA*IFM_RELA*~EMP_RELA <= FSRADICALITY", data = data_a)
pof("~OCC_TRAINI*IFM_RELI*~EMP_RELI <= FSRADICALITY", data = data_i)



############
#  France  #
############
pof("FRM_HIERA <= FSRADICALITY", data = data_a)
pof("FRM_HIERI <= FSRADICALITY", data = data_i) 


#########################
#  Portugal and Greece  #
#########################

pof("~OCC_TRAINA*~FRM_HIERA*IFM_RELA*EMP_RELA*CORP_GOVA <=  FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*IFM_RELI*EMP_RELI*CORP_GOVI <=  FSRADICALITY", data = data_i) 


############
#  Italy   #
############

pof("OCC_TRAINA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 


############
#  Japan   #
############

pof("~OCC_TRAINA*~FRM_HIERA*IFM_RELA*EMP_RELA*~CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*IFM_RELI*EMP_RELI*~CORP_GOVI <= FSRADICALITY", data = data_i)


###############
#  S. Korea   #
###############

pof("~OCC_TRAINA*~FRM_HIERA*IFM_RELA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*IFM_RELI <= FSRADICALITY", data = data_i) 


#############
#  Finland  #
#############

pof("~OCC_TRAINA*FRM_HIERA*~IFM_RELA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*FRM_HIERI*~IFM_RELI <= FSRADICALITY", data = data_i)


##################
#  Switzerland   #
##################

pof("OCC_TRAINA*EMP_RELA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*EMP_RELI <= FSRADICALITY", data = data_i) 


##################
#    Denmark     #
##################
pof("~OCC_TRAINA*FRM_HIERA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*FRM_HIERI*CORP_GOVI <= FSRADICALITY", data = data_i)


###############################
#    Sweden & Netherlands     #
###############################

pof("FRM_HIERA*EMP_RELA <= FSRADICALITY", data = data_a) 
pof("FRM_HIERI*EMP_RELI <= FSRADICALITY", data = data_i) 


##################
#    Norway      #
##################
pof("~OCC_TRAINA*FRM_HIERA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*FRM_HIERI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 


##################
#    Belgium     #
##################
pof("OCC_TRAINA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 


##################
#    Germany     #
##################
pof("OCC_TRAINA*FRM_HIERA*IFM_RELA*EMP_RELA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*FRM_HIERI*IFM_RELI*EMP_RELI <= FSRADICALITY", data = data_i) 


##################
#    Austria     #
##################
pof("OCC_TRAINA*FRM_HIERA*IFM_RELA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*FRM_HIERI*IFM_RELI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 



##############################################
#                                            #
#         citation-based indicators          #
#                                            #
##############################################


########################################
#####        full period          ######
########################################

# get patent counts by country
data %>% dplyr::count(assignee_country)

# get radical patent counts by country
quantile(data$forward_citations, c(.25, .50, .75, .90, .95, .99), na.rm=TRUE) 


data$FSRADICALITY <- calibrate(data$u_radicality, type = "fuzzy", 
                                 thresholds = "e=  0, c= 1, i= 7", ecdf = TRUE)


keepsa <- c("FSRADICALITY", "CORP_GOVA", "IFM_RELA", "FRM_HIERA", "EMP_RELA",
            "OCC_TRAINA")

keepsi <- c("FSRADICALITY", "CORP_GOVI", "IFM_RELI", "FRM_HIERI", "EMP_RELI",
            "OCC_TRAINI")


data_a <- data[keepsa]
data_i <- data[keepsi]


######################
#  US configuration  #
######################

# assignee
pof("~OCC_TRAINA*~FRM_HIERA*~IFM_RELA*~EMP_RELA*~CORP_GOVA <= FSRADICALITY", data = data_a) # <= denotes necessity, => denotes sufficiency, + = logical OR, * = logical AND ~ = negation

# inventor
pof("~OCC_TRAINI*~FRM_HIERI*~IFM_RELI*~EMP_RELI*~CORP_GOVI <= FSRADICALITY", data = data_i) 


###########################
#  UK, Canada, Australia  #
###########################

pof("~OCC_TRAINA*~FRM_HIERA*~IFM_RELA*~CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*~IFM_RELI*~CORP_GOVI <= FSRADICALITY", data = data_i)


##############
#  Ireland   #
##############

pof("~OCC_TRAINA*~FRM_HIERA*~IFM_RELA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*~IFM_RELI <= FSRADICALITY", data = data_i)  


##################
#  New Zealand   #
##################
pof("~OCC_TRAINA*~FRM_HIERA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*CORP_GOVI <= FSRADICALITY", data = data_i) 


############
#  Spain   #
############
pof("~OCC_TRAINA*IFM_RELA*~EMP_RELA <= FSRADICALITY", data = data_a)
pof("~OCC_TRAINI*IFM_RELI*~EMP_RELI <= FSRADICALITY", data = data_i)



############
#  France  #
############
pof("FRM_HIERA <= FSRADICALITY", data = data_a)
pof("FRM_HIERI <= FSRADICALITY", data = data_i) 


#########################
#  Portugal and Greece  #
#########################

pof("~OCC_TRAINA*~FRM_HIERA*IFM_RELA*EMP_RELA*CORP_GOVA <=  FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*IFM_RELI*EMP_RELI*CORP_GOVI <=  FSRADICALITY", data = data_i) 


############
#  Italy   #
############

pof("OCC_TRAINA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 


############
#  Japan   #
############

pof("~OCC_TRAINA*~FRM_HIERA*IFM_RELA*EMP_RELA*~CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*IFM_RELI*EMP_RELI*~CORP_GOVI <= FSRADICALITY", data = data_i)


###############
#  S. Korea   #
###############

pof("~OCC_TRAINA*~FRM_HIERA*IFM_RELA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*IFM_RELI <= FSRADICALITY", data = data_i) 


#############
#  Finland  #
#############

pof("~OCC_TRAINA*FRM_HIERA*~IFM_RELA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*FRM_HIERI*~IFM_RELI <= FSRADICALITY", data = data_i)


##################
#  Switzerland   #
##################

pof("OCC_TRAINA*EMP_RELA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*EMP_RELI <= FSRADICALITY", data = data_i) 


##################
#    Denmark     #
##################
pof("~OCC_TRAINA*FRM_HIERA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*FRM_HIERI*CORP_GOVI <= FSRADICALITY", data = data_i)


###############################
#    Sweden & Netherlands     #
###############################

pof("FRM_HIERA*EMP_RELA <= FSRADICALITY", data = data_a) 
pof("FRM_HIERI*EMP_RELI <= FSRADICALITY", data = data_i) 


##################
#    Norway      #
##################
pof("~OCC_TRAINA*FRM_HIERA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*FRM_HIERI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 


##################
#    Belgium     #
##################
pof("OCC_TRAINA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 


##################
#    Germany     #
##################
pof("OCC_TRAINA*FRM_HIERA*IFM_RELA*EMP_RELA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*FRM_HIERI*IFM_RELI*EMP_RELI <= FSRADICALITY", data = data_i) 


##################
#    Austria     #
##################
pof("OCC_TRAINA*FRM_HIERA*IFM_RELA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*FRM_HIERI*IFM_RELI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 



#########################################
############### 60-90 ###################
#########################################

# get patent counts by country
data6090 %>% dplyr::count(assignee_country)


quantile(data6090$forward_citations, c(.25, .50, .75, .90, .95, .99), na.rm=TRUE) 


data6090$FSRADICALITY <- calibrate(data6090$total_radicality, type = "fuzzy", 
                                     thresholds = "e=  0,  c= 1, i=8.00 ", ecdf = TRUE)



# keep only needed columns 
keepsa <- c("FSRADICALITY","CORP_GOVA", "IFM_RELA", "FRM_HIERA", "EMP_RELA",
            "OCC_TRAINA")

keepsi <- c("FSRADICALITY","CORP_GOVI", "IFM_RELI", "FRM_HIERI", "EMP_RELI",
            "OCC_TRAINI")

data_a <- data6090[keepsa]
data_i <- data6090[keepsi]


######################
#  US configuration  #
######################

# assignee
pof("~OCC_TRAINA*~FRM_HIERA*~IFM_RELA*~EMP_RELA*~CORP_GOVA <= FSRADICALITY", data = data_a) # <= denotes necessity, => denotes sufficiency, + = logical OR, * = logical AND ~ = negation

# inventor
pof("~OCC_TRAINI*~FRM_HIERI*~IFM_RELI*~EMP_RELI*~CORP_GOVI <= FSRADICALITY", data = data_i) 


###########################
#  UK, Canada, Australia  #
###########################

pof("~OCC_TRAINA*~FRM_HIERA*~IFM_RELA*~CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*~IFM_RELI*~CORP_GOVI <= FSRADICALITY", data = data_i)


##############
#  Ireland   #
##############

pof("~OCC_TRAINA*~FRM_HIERA*~IFM_RELA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*~IFM_RELI <= FSRADICALITY", data = data_i)  


##################
#  New Zealand   #
##################
pof("~OCC_TRAINA*~FRM_HIERA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*CORP_GOVI <= FSRADICALITY", data = data_i) 


############
#  Spain   #
############
pof("~OCC_TRAINA*IFM_RELA*~EMP_RELA <= FSRADICALITY", data = data_a)
pof("~OCC_TRAINI*IFM_RELI*~EMP_RELI <= FSRADICALITY", data = data_i)



############
#  France  #
############
pof("FRM_HIERA <= FSRADICALITY", data = data_a)
pof("FRM_HIERI <= FSRADICALITY", data = data_i) 


#########################
#  Portugal and Greece  #
#########################

pof("~OCC_TRAINA*~FRM_HIERA*IFM_RELA*EMP_RELA*CORP_GOVA <=  FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*IFM_RELI*EMP_RELI*CORP_GOVI <=  FSRADICALITY", data = data_i) 


############
#  Italy   #
############

pof("OCC_TRAINA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 


############
#  Japan   #
############

pof("~OCC_TRAINA*~FRM_HIERA*IFM_RELA*EMP_RELA*~CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*IFM_RELI*EMP_RELI*~CORP_GOVI <= FSRADICALITY", data = data_i)


###############
#  S. Korea   #
###############

pof("~OCC_TRAINA*~FRM_HIERA*IFM_RELA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*IFM_RELI <= FSRADICALITY", data = data_i) 


#############
#  Finland  #
#############

pof("~OCC_TRAINA*FRM_HIERA*~IFM_RELA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*FRM_HIERI*~IFM_RELI <= FSRADICALITY", data = data_i)


##################
#  Switzerland   #
##################

pof("OCC_TRAINA*EMP_RELA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*EMP_RELI <= FSRADICALITY", data = data_i) 


##################
#    Denmark     #
##################
pof("~OCC_TRAINA*FRM_HIERA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*FRM_HIERI*CORP_GOVI <= FSRADICALITY", data = data_i)


###############################
#    Sweden & Netherlands     #
###############################

pof("FRM_HIERA*EMP_RELA <= FSRADICALITY", data = data_a) 
pof("FRM_HIERI*EMP_RELI <= FSRADICALITY", data = data_i) 


##################
#    Norway      #
##################
pof("~OCC_TRAINA*FRM_HIERA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*FRM_HIERI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 


##################
#    Belgium     #
##################
pof("OCC_TRAINA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 


##################
#    Germany     #
##################
pof("OCC_TRAINA*FRM_HIERA*IFM_RELA*EMP_RELA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*FRM_HIERI*IFM_RELI*EMP_RELI <= FSRADICALITY", data = data_i) 


##################
#    Austria     #
##################
pof("OCC_TRAINA*FRM_HIERA*IFM_RELA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*FRM_HIERI*IFM_RELI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 




#########################################
############### 70-00 ###################
#########################################


# get patent counts by country
data7000 %>% dplyr::count(assignee_country)


quantile(data7000$forward_citations, c(.25, .50, .75, .90, .95, .99), na.rm =TRUE) 


data7000$FSRADICALITY <- calibrate(data7000$forward_citations, type = "fuzzy", 
                                     thresholds = "e=  0,  c=  1, i=13 ", ecdf = TRUE)



keepsa <- c("FSRADICALITY","CORP_GOVA", "IFM_RELA", "FRM_HIERA", "EMP_RELA",
            "OCC_TRAINA")

keepsi <- c("FSRADICALITY","CORP_GOVI", "IFM_RELI", "FRM_HIERI", "EMP_RELI",
            "OCC_TRAINI")

data_a <- data7000[keepsa]
data_i <- data7000[keepsi]

######################
#  US configuration  #
######################
# assignee
pof("~OCC_TRAINA*~FRM_HIERA*~IFM_RELA*~EMP_RELA*~CORP_GOVA <= FSRADICALITY", data = data_a) # <= denotes necessity, => denotes sufficiency, + = logical OR, * = logical AND ~ = negation

# inventor
pof("~OCC_TRAINI*~FRM_HIERI*~IFM_RELI*~EMP_RELI*~CORP_GOVI <= FSRADICALITY", data = data_i) 


###########################
#  UK, Canada, Australia  #
###########################
pof("~OCC_TRAINA*~FRM_HIERA*~IFM_RELA*~CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*~IFM_RELI*~CORP_GOVI <= FSRADICALITY", data = data_i)


##############
#  Ireland   #
##############
pof("~OCC_TRAINA*~FRM_HIERA*~IFM_RELA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*~IFM_RELI <= FSRADICALITY", data = data_i)  


##################
#  New Zealand   #
##################
pof("~OCC_TRAINA*~FRM_HIERA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*CORP_GOVI <= FSRADICALITY", data = data_i) 


############
#  Spain   #
############
pof("~OCC_TRAINA*IFM_RELA*~EMP_RELA <= FSRADICALITY", data = data_a)
pof("~OCC_TRAINI*IFM_RELI*~EMP_RELI <= FSRADICALITY", data = data_i)



############
#  France  #
############
pof("FRM_HIERA <= FSRADICALITY", data = data_a)
pof("FRM_HIERI <= FSRADICALITY", data = data_i) 


#########################
#  Portugal and Greece  #
#########################
pof("~OCC_TRAINA*~FRM_HIERA*IFM_RELA*EMP_RELA*CORP_GOVA <=  FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*IFM_RELI*EMP_RELI*CORP_GOVI <=  FSRADICALITY", data = data_i) 


############
#  Italy   #
############
pof("OCC_TRAINA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 


############
#  Japan   #
############
pof("~OCC_TRAINA*~FRM_HIERA*IFM_RELA*EMP_RELA*~CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*IFM_RELI*EMP_RELI*~CORP_GOVI <= FSRADICALITY", data = data_i)


###############
#  S. Korea   #
###############
pof("~OCC_TRAINA*~FRM_HIERA*IFM_RELA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*IFM_RELI <= FSRADICALITY", data = data_i) 


#############
#  Finland  #
#############
pof("~OCC_TRAINA*FRM_HIERA*~IFM_RELA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*FRM_HIERI*~IFM_RELI <= FSRADICALITY", data = data_i)


##################
#  Switzerland   #
##################
pof("OCC_TRAINA*EMP_RELA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*EMP_RELI <= FSRADICALITY", data = data_i) 


##################
#    Denmark     #
##################
pof("~OCC_TRAINA*FRM_HIERA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*FRM_HIERI*CORP_GOVI <= FSRADICALITY", data = data_i)


###############################
#    Sweden & Netherlands     #
###############################
pof("FRM_HIERA*EMP_RELA <= FSRADICALITY", data = data_a) 
pof("FRM_HIERI*EMP_RELI <= FSRADICALITY", data = data_i) 


##################
#    Norway      #
##################
pof("~OCC_TRAINA*FRM_HIERA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*FRM_HIERI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 


##################
#    Belgium     #
##################
pof("OCC_TRAINA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 


##################
#    Germany     #
##################
pof("OCC_TRAINA*FRM_HIERA*IFM_RELA*EMP_RELA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*FRM_HIERI*IFM_RELI*EMP_RELI <= FSRADICALITY", data = data_i) 


##################
#    Austria     #
##################
pof("OCC_TRAINA*FRM_HIERA*IFM_RELA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*FRM_HIERI*IFM_RELI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 



#######################################
############## 80-10 ##################
#######################################


# get patent counts by country
data8010 %>% dplyr::count(assignee_country)


quantile(data8010$forward_citations, c(.25, .50, .75, .90, .95, .99),  na.rm = TRUE)    


data8010$FSRADICALITY <- calibrate(data8010$forward_citations, type = "fuzzy", 
                                     thresholds = "e=  0,  c=  1 , i=10", ecdf = TRUE)

keepsa <- c("FSRADICALITY","CORP_GOVA", "IFM_RELA", "FRM_HIERA", "EMP_RELA",
            "OCC_TRAINA")

keepsi <- c("FSRADICALITY","CORP_GOVI", "IFM_RELI", "FRM_HIERI", "EMP_RELI",
            "OCC_TRAINI")

data_a <- data8010[keepsa]
data_i <- data8010[keepsi]


######################
#  US configuration  #
######################
# assignee
pof("~OCC_TRAINA*~FRM_HIERA*~IFM_RELA*~EMP_RELA*~CORP_GOVA <= FSRADICALITY", data = data_a) # <= denotes necessity, => denotes sufficiency, + = logical OR, * = logical AND ~ = negation

# inventor
pof("~OCC_TRAINI*~FRM_HIERI*~IFM_RELI*~EMP_RELI*~CORP_GOVI <= FSRADICALITY", data = data_i) 


###########################
#  UK, Canada, Australia  #
###########################
pof("~OCC_TRAINA*~FRM_HIERA*~IFM_RELA*~CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*~IFM_RELI*~CORP_GOVI <= FSRADICALITY", data = data_i)


##############
#  Ireland   #
##############
pof("~OCC_TRAINA*~FRM_HIERA*~IFM_RELA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*~IFM_RELI <= FSRADICALITY", data = data_i)  


##################
#  New Zealand   #
##################
pof("~OCC_TRAINA*~FRM_HIERA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*CORP_GOVI <= FSRADICALITY", data = data_i) 


############
#  Spain   #
############
pof("~OCC_TRAINA*IFM_RELA*~EMP_RELA <= FSRADICALITY", data = data_a)
pof("~OCC_TRAINI*IFM_RELI*~EMP_RELI <= FSRADICALITY", data = data_i)



############
#  France  #
############
pof("FRM_HIERA <= FSRADICALITY", data = data_a)
pof("FRM_HIERI <= FSRADICALITY", data = data_i) 


#########################
#  Portugal and Greece  #
#########################
pof("~OCC_TRAINA*~FRM_HIERA*IFM_RELA*EMP_RELA*CORP_GOVA <=  FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*IFM_RELI*EMP_RELI*CORP_GOVI <=  FSRADICALITY", data = data_i) 


############
#  Italy   #
############
pof("OCC_TRAINA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 


############
#  Japan   #
############
pof("~OCC_TRAINA*~FRM_HIERA*IFM_RELA*EMP_RELA*~CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*IFM_RELI*EMP_RELI*~CORP_GOVI <= FSRADICALITY", data = data_i)


###############
#  S. Korea   #
###############
pof("~OCC_TRAINA*~FRM_HIERA*IFM_RELA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*IFM_RELI <= FSRADICALITY", data = data_i) 


#############
#  Finland  #
#############
pof("~OCC_TRAINA*FRM_HIERA*~IFM_RELA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*FRM_HIERI*~IFM_RELI <= FSRADICALITY", data = data_i)


##################
#  Switzerland   #
##################
pof("OCC_TRAINA*EMP_RELA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*EMP_RELI <= FSRADICALITY", data = data_i) 


##################
#    Denmark     #
##################
pof("~OCC_TRAINA*FRM_HIERA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*FRM_HIERI*CORP_GOVI <= FSRADICALITY", data = data_i)


###############################
#    Sweden & Netherlands     #
###############################
pof("FRM_HIERA*EMP_RELA <= FSRADICALITY", data = data_a) 
pof("FRM_HIERI*EMP_RELI <= FSRADICALITY", data = data_i) 


##################
#    Norway      #
##################
pof("~OCC_TRAINA*FRM_HIERA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*FRM_HIERI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 


##################
#    Belgium     #
##################
pof("OCC_TRAINA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 


##################
#    Germany     #
##################
pof("OCC_TRAINA*FRM_HIERA*IFM_RELA*EMP_RELA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*FRM_HIERI*IFM_RELI*EMP_RELI <= FSRADICALITY", data = data_i) 


##################
#    Austria     #
##################
pof("OCC_TRAINA*FRM_HIERA*IFM_RELA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*FRM_HIERI*IFM_RELI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 



#########################################
############### 90-20 ###################
#########################################

# get patent counts by country
data9020 %>% dplyr::count(assignee_country)


quantile(data9020$forward_citations, c(.25, .50, .75, .90, .95, .99), na.rm=TRUE)    


data9020$FSRADICALITY <- calibrate(data9020$total_radicality, type = "fuzzy", 
                                     thresholds = "e=  0,  c= 1 , i=7 ", ecdf = TRUE)


keepsa <- c("FSRADICALITY","CORP_GOVA", "IFM_RELA", "FRM_HIERA", "EMP_RELA",
            "OCC_TRAINA")

keepsi <- c("FSRADICALITY","CORP_GOVI", "IFM_RELI", "FRM_HIERI", "EMP_RELI",
            "OCC_TRAINI")

data_a <- data9020[keepsa]
data_i <- data9020[keepsi]

######################
#  US configuration  #
######################
# assignee
pof("~OCC_TRAINA*~FRM_HIERA*~IFM_RELA*~EMP_RELA*~CORP_GOVA <= FSRADICALITY", data = data_a) # <= denotes necessity, => denotes sufficiency, + = logical OR, * = logical AND ~ = negation

# inventor
pof("~OCC_TRAINI*~FRM_HIERI*~IFM_RELI*~EMP_RELI*~CORP_GOVI <= FSRADICALITY", data = data_i) 



###########################
#  UK, Canada, Australia  #
###########################
pof("~OCC_TRAINA*~FRM_HIERA*~IFM_RELA*~CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*~IFM_RELI*~CORP_GOVI <= FSRADICALITY", data = data_i)


##############
#  Ireland   #
##############
pof("~OCC_TRAINA*~FRM_HIERA*~IFM_RELA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*~IFM_RELI <= FSRADICALITY", data = data_i)  


##################
#  New Zealand   #
##################
pof("~OCC_TRAINA*~FRM_HIERA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*CORP_GOVI <= FSRADICALITY", data = data_i) 


############
#  Spain   #
############
pof("~OCC_TRAINA*IFM_RELA*~EMP_RELA <= FSRADICALITY", data = data_a)
pof("~OCC_TRAINI*IFM_RELI*~EMP_RELI <= FSRADICALITY", data = data_i)


############
#  France  #
############
pof("FRM_HIERA <= FSRADICALITY", data = data_a)
pof("FRM_HIERI <= FSRADICALITY", data = data_i) 


#########################
#  Portugal and Greece  #
#########################
pof("~OCC_TRAINA*~FRM_HIERA*IFM_RELA*EMP_RELA*CORP_GOVA <=  FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*IFM_RELI*EMP_RELI*CORP_GOVI <=  FSRADICALITY", data = data_i) 


############
#  Italy   #
############
pof("OCC_TRAINA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 


############
#  Japan   #
############
pof("~OCC_TRAINA*~FRM_HIERA*IFM_RELA*EMP_RELA*~CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*IFM_RELI*EMP_RELI*~CORP_GOVI <= FSRADICALITY", data = data_i)


###############
#  S. Korea   #
###############
pof("~OCC_TRAINA*~FRM_HIERA*IFM_RELA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*IFM_RELI <= FSRADICALITY", data = data_i) 


#############
#  Finland  #
#############
pof("~OCC_TRAINA*FRM_HIERA*~IFM_RELA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*FRM_HIERI*~IFM_RELI <= FSRADICALITY", data = data_i)


##################
#  Switzerland   #
##################
pof("OCC_TRAINA*EMP_RELA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*EMP_RELI <= FSRADICALITY", data = data_i) 


##################
#    Denmark     #
##################
pof("~OCC_TRAINA*FRM_HIERA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*FRM_HIERI*CORP_GOVI <= FSRADICALITY", data = data_i)


###############################
#    Sweden & Netherlands     #
###############################
pof("FRM_HIERA*EMP_RELA <= FSRADICALITY", data = data_a) 
pof("FRM_HIERI*EMP_RELI <= FSRADICALITY", data = data_i) 


##################
#    Norway      #
##################
pof("~OCC_TRAINA*FRM_HIERA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*FRM_HIERI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 


##################
#    Belgium     #
##################
pof("OCC_TRAINA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 


##################
#    Germany     #
##################
pof("OCC_TRAINA*FRM_HIERA*IFM_RELA*EMP_RELA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*FRM_HIERI*IFM_RELI*EMP_RELI <= FSRADICALITY", data = data_i) 


##################
#    Austria     #
##################
pof("OCC_TRAINA*FRM_HIERA*IFM_RELA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*FRM_HIERI*IFM_RELI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 



#########################################################################
#                                                                       #
# assess the effect of citation deflators courtesy of Hall et al (2001) #
#                                                                       #
#########################################################################


data2 <- subset(data, year > 1976 & year < 2013)


data2$deflate_2 <- data2$forward_citations/data2$weight_2*0.197


quantile(data2$deflate_2, c(.25, .50, .75, .90, .95, .99), na.rm=T)    


data2$FSRADICALITY <- calibrate(data2$forward_citations, type = "fuzzy", 
                                     thresholds = "e=  0,  c=  1, i = 11.588235", ecdf = TRUE)


# keep only needed columns 
keepsa <- c("FSRADICALITY","CORP_GOVA", "IFM_RELA", "FRM_HIERA", "EMP_RELA",
            "OCC_TRAINA")

keepsi <- c("FSRADICALITY","CORP_GOVI", "IFM_RELI", "FRM_HIERI", "EMP_RELI",
            "OCC_TRAINI")

data_a <- data2[keepsa]
data_i <- data2[keepsi]


######################
#  US configuration  #
######################
# assignee
pof("~OCC_TRAINA*~FRM_HIERA*~IFM_RELA*~EMP_RELA*~CORP_GOVA <= FSRADICALITY", data = data_a) # <= denotes necessity, => denotes sufficiency, + = logical OR, * = logical AND ~ = negation

# inventor
pof("~OCC_TRAINI*~FRM_HIERI*~IFM_RELI*~EMP_RELI*~CORP_GOVI <= FSRADICALITY", data = data_i) 



###########################
#  UK, Canada, Australia  #
###########################
pof("~OCC_TRAINA*~FRM_HIERA*~IFM_RELA*~CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*~IFM_RELI*~CORP_GOVI <= FSRADICALITY", data = data_i)


##############
#  Ireland   #
##############
pof("~OCC_TRAINA*~FRM_HIERA*~IFM_RELA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*~IFM_RELI <= FSRADICALITY", data = data_i)  


##################
#  New Zealand   #
##################
pof("~OCC_TRAINA*~FRM_HIERA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*CORP_GOVI <= FSRADICALITY", data = data_i) 


############
#  Spain   #
############
pof("~OCC_TRAINA*IFM_RELA*~EMP_RELA <= FSRADICALITY", data = data_a)
pof("~OCC_TRAINI*IFM_RELI*~EMP_RELI <= FSRADICALITY", data = data_i)


############
#  France  #
############
pof("FRM_HIERA <= FSRADICALITY", data = data_a)
pof("FRM_HIERI <= FSRADICALITY", data = data_i) 


#########################
#  Portugal and Greece  #
#########################
pof("~OCC_TRAINA*~FRM_HIERA*IFM_RELA*EMP_RELA*CORP_GOVA <=  FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*IFM_RELI*EMP_RELI*CORP_GOVI <=  FSRADICALITY", data = data_i) 


############
#  Italy   #
############
pof("OCC_TRAINA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 


############
#  Japan   #
############
pof("~OCC_TRAINA*~FRM_HIERA*IFM_RELA*EMP_RELA*~CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*IFM_RELI*EMP_RELI*~CORP_GOVI <= FSRADICALITY", data = data_i)


###############
#  S. Korea   #
###############
pof("~OCC_TRAINA*~FRM_HIERA*IFM_RELA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*IFM_RELI <= FSRADICALITY", data = data_i) 


#############
#  Finland  #
#############
pof("~OCC_TRAINA*FRM_HIERA*~IFM_RELA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*FRM_HIERI*~IFM_RELI <= FSRADICALITY", data = data_i)


##################
#  Switzerland   #
##################
pof("OCC_TRAINA*EMP_RELA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*EMP_RELI <= FSRADICALITY", data = data_i) 


##################
#    Denmark     #
##################
pof("~OCC_TRAINA*FRM_HIERA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*FRM_HIERI*CORP_GOVI <= FSRADICALITY", data = data_i)


###############################
#    Sweden & Netherlands     #
###############################
pof("FRM_HIERA*EMP_RELA <= FSRADICALITY", data = data_a) 
pof("FRM_HIERI*EMP_RELI <= FSRADICALITY", data = data_i) 


##################
#    Norway      #
##################
pof("~OCC_TRAINA*FRM_HIERA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*FRM_HIERI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 


##################
#    Belgium     #
##################
pof("OCC_TRAINA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 


##################
#    Germany     #
##################
pof("OCC_TRAINA*FRM_HIERA*IFM_RELA*EMP_RELA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*FRM_HIERI*IFM_RELI*EMP_RELI <= FSRADICALITY", data = data_i) 


##################
#    Austria     #
##################
pof("OCC_TRAINA*FRM_HIERA*IFM_RELA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*FRM_HIERI*IFM_RELI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 



#########################################################################
#                                                                       #
# assess the effect of originality scores courtesy of Hall et al (2001) #
#                                                                       #
#########################################################################


########################################
#####        full period          ######
########################################


# get radical patent counts by country
quantile(data$originality_hhi, c(.25, .50, .75, .90, .95, .99), na.rm=T) 


data$FSRADICALITY <- calibrate(data$originality_hhi, type = "fuzzy", 
                               thresholds = "e=  0, c= 0.02984389, i= 0.72022960", ecdf = TRUE)


keepsa <- c("FSRADICALITY", "CORP_GOVA", "IFM_RELA", "FRM_HIERA", "EMP_RELA",
            "OCC_TRAINA") # assignee

keepsi <- c("FSRADICALITY",  "CORP_GOVI", "IFM_RELI", "FRM_HIERI", "EMP_RELI",
            "OCC_TRAINI") # inventor

data_a <- data[keepsa]
data_i <- data[keepsi]


######################
#  US configuration  #
######################

# assignee
pof("~OCC_TRAINA*~FRM_HIERA*~IFM_RELA*~EMP_RELA*~CORP_GOVA <= FSRADICALITY", data = data_a) # <= denotes necessity, => denotes sufficiency, + = logical OR, * = logical AND ~ = negation

# inventor
pof("~OCC_TRAINI*~FRM_HIERI*~IFM_RELI*~EMP_RELI*~CORP_GOVI <= FSRADICALITY", data = data_i) 


###########################
#  UK, Canada, Australia  #
###########################
pof("~OCC_TRAINA*~FRM_HIERA*~IFM_RELA*~CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*~IFM_RELI*~CORP_GOVI <= FSRADICALITY", data = data_i)


##############
#  Ireland   #
##############
pof("~OCC_TRAINA*~FRM_HIERA*~IFM_RELA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*~IFM_RELI <= FSRADICALITY", data = data_i)  


##################
#  New Zealand   #
##################
pof("~OCC_TRAINA*~FRM_HIERA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*CORP_GOVI <= FSRADICALITY", data = data_i) 



############
#  Spain   #
############
pof("~OCC_TRAINA*IFM_RELA*~EMP_RELA <= FSRADICALITY", data = data_a)
pof("~OCC_TRAINI*IFM_RELI*~EMP_RELI <= FSRADICALITY", data = data_i)


############
#  France  #
############
pof("FRM_HIERA <= FSRADICALITY", data = data_a)
pof("FRM_HIERI <= FSRADICALITY", data = data_i) 


#########################
#  Portugal and Greece  #
#########################
pof("~OCC_TRAINA*~FRM_HIERA*IFM_RELA*EMP_RELA*CORP_GOVA <=  FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*IFM_RELI*EMP_RELI*CORP_GOVI <=  FSRADICALITY", data = data_i) 


############
#  Italy   #
############
pof("OCC_TRAINA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 


############
#  Japan   #
############
pof("~OCC_TRAINA*~FRM_HIERA*IFM_RELA*EMP_RELA*~CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*IFM_RELI*EMP_RELI*~CORP_GOVI <= FSRADICALITY", data = data_i)


###############
#  S. Korea   #
###############
pof("~OCC_TRAINA*~FRM_HIERA*IFM_RELA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*IFM_RELI <= FSRADICALITY", data = data_i) 


#############
#  Finland  #
#############
pof("~OCC_TRAINA*FRM_HIERA*~IFM_RELA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*FRM_HIERI*~IFM_RELI <= FSRADICALITY", data = data_i)


##################
#  Switzerland   #
##################
pof("OCC_TRAINA*EMP_RELA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*EMP_RELI <= FSRADICALITY", data = data_i) 


##################
#    Denmark     #
##################
pof("~OCC_TRAINA*FRM_HIERA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*FRM_HIERI*CORP_GOVI <= FSRADICALITY", data = data_i)


###############################
#    Sweden & Netherlands     #
###############################

pof("FRM_HIERA*EMP_RELA <= FSRADICALITY", data = data_a) 
pof("FRM_HIERI*EMP_RELI <= FSRADICALITY", data = data_i) 


##################
#    Norway      #
##################
pof("~OCC_TRAINA*FRM_HIERA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*FRM_HIERI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 


##################
#    Belgium     #
##################
pof("OCC_TRAINA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 


##################
#    Germany     #
##################
pof("OCC_TRAINA*FRM_HIERA*IFM_RELA*EMP_RELA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*FRM_HIERI*IFM_RELI*EMP_RELI <= FSRADICALITY", data = data_i) 


##################
#    Austria     #
##################
pof("OCC_TRAINA*FRM_HIERA*IFM_RELA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*FRM_HIERI*IFM_RELI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 




#########################################
############### 60-90 ###################
#########################################


quantile(data6090$originality_hhi, c(.25, .50, .75, .90, .95, .99), na.rm=T) 


# e = complete nonmembership value' c = the crossover value (re: more/less in than out) i=complete membership
data6090$FSRADICALITY<- calibrate(data6090$originality_hhi, type = "fuzzy", 
                                  thresholds = "e=  0,  c= 0 , i=0.5000000 ", ecdf = TRUE)


keepsa <- c("FSRADICALITY", "CORP_GOVA", "IFM_RELA", "FRM_HIERA", "EMP_RELA",
            "OCC_TRAINA")

keepsi <- c("FSRADICALITY","CORP_GOVI", "IFM_RELI", "FRM_HIERI", "EMP_RELI",
            "OCC_TRAINI")

data_a <- data6090[keepsa]
data_i <- data6090[keepsi]



######################
#  US configuration  #
######################

# assignee
pof("~OCC_TRAINA*~FRM_HIERA*~IFM_RELA*~EMP_RELA*~CORP_GOVA <= FSRADICALITY", data = data_a) # <= denotes necessity, => denotes sufficiency, + = logical OR, * = logical AND ~ = negation

# inventor
pof("~OCC_TRAINI*~FRM_HIERI*~IFM_RELI*~EMP_RELI*~CORP_GOVI <= FSRADICALITY", data = data_i) 


###########################
#  UK, Canada, Australia  #
###########################

pof("~OCC_TRAINA*~FRM_HIERA*~IFM_RELA*~CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*~IFM_RELI*~CORP_GOVI <= FSRADICALITY", data = data_i)


##############
#  Ireland   #
##############

pof("~OCC_TRAINA*~FRM_HIERA*~IFM_RELA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*~IFM_RELI <= FSRADICALITY", data = data_i)  


##################
#  New Zealand   #
##################
pof("~OCC_TRAINA*~FRM_HIERA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*CORP_GOVI <= FSRADICALITY", data = data_i) 



############
#  Spain   #
############
pof("~OCC_TRAINA*IFM_RELA*~EMP_RELA <= FSRADICALITY", data = data_a)
pof("~OCC_TRAINI*IFM_RELI*~EMP_RELI <= FSRADICALITY", data = data_i)



############
#  France  #
############
pof("FRM_HIERA <= FSRADICALITY", data = data_a)
pof("FRM_HIERI <= FSRADICALITY", data = data_i) 


#########################
#  Portugal and Greece  #
#########################

pof("~OCC_TRAINA*~FRM_HIERA*IFM_RELA*EMP_RELA*CORP_GOVA <=  FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*IFM_RELI*EMP_RELI*CORP_GOVI <=  FSRADICALITY", data = data_i) 


############
#  Italy   #
############

pof("OCC_TRAINA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 


############
#  Japan   #
############

pof("~OCC_TRAINA*~FRM_HIERA*IFM_RELA*EMP_RELA*~CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*IFM_RELI*EMP_RELI*~CORP_GOVI <= FSRADICALITY", data = data_i)


###############
#  S. Korea   #
###############

pof("~OCC_TRAINA*~FRM_HIERA*IFM_RELA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*IFM_RELI <= FSRADICALITY", data = data_i) 


#############
#  Finland  #
#############

pof("~OCC_TRAINA*FRM_HIERA*~IFM_RELA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*FRM_HIERI*~IFM_RELI <= FSRADICALITY", data = data_i)


##################
#  Switzerland   #
##################

pof("OCC_TRAINA*EMP_RELA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*EMP_RELI <= FSRADICALITY", data = data_i) 


##################
#    Denmark     #
##################
pof("~OCC_TRAINA*FRM_HIERA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*FRM_HIERI*CORP_GOVI <= FSRADICALITY", data = data_i)


###############################
#    Sweden & Netherlands     #
###############################

pof("FRM_HIERA*EMP_RELA <= FSRADICALITY", data = data_a) 
pof("FRM_HIERI*EMP_RELI <= FSRADICALITY", data = data_i) 


##################
#    Norway      #
##################
pof("~OCC_TRAINA*FRM_HIERA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*FRM_HIERI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 


##################
#    Belgium     #
##################
pof("OCC_TRAINA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 


##################
#    Germany     #
##################
pof("OCC_TRAINA*FRM_HIERA*IFM_RELA*EMP_RELA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*FRM_HIERI*IFM_RELI*EMP_RELI <= FSRADICALITY", data = data_i) 


##################
#    Austria     #
##################
pof("OCC_TRAINA*FRM_HIERA*IFM_RELA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*FRM_HIERI*IFM_RELI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 



#########################################
############### 70-00 ###################
#########################################


# get patent counts by country
data7000 %>% dplyr::count(assignee_country)


quantile(data7000$originality_hhi, c(.25, .50, .75, .90, .95, .99), na.rm=T)    

# e = complete nonmembership value' c = the crossover value (re: more/less in than out) i=complete membership
data7000$FSRADICALITY <- calibrate(data7000$originality_hhi, type = "fuzzy", 
                                   thresholds = "e=  0,  c=  0, i=0.625", ecdf = TRUE)


# keep only needed columns 
keepsa <- c("FSRADICALITY","CORP_GOVA", "IFM_RELA", "FRM_HIERA", "EMP_RELA",
            "OCC_TRAINA")

keepsi <- c("FSRADICALITY", "CORP_GOVI", "IFM_RELI", "FRM_HIERI", "EMP_RELI",
            "OCC_TRAINI")

data_a <- data7000[keepsa]
data_i <- data7000[keepsi]



######################
#  US configuration  #
######################

# assignee
pof("~OCC_TRAINA*~FRM_HIERA*~IFM_RELA*~EMP_RELA*~CORP_GOVA <= FSRADICALITY", data = data_a) # <= denotes necessity, => denotes sufficiency, + = logical OR, * = logical AND ~ = negation

# inventor
pof("~OCC_TRAINI*~FRM_HIERI*~IFM_RELI*~EMP_RELI*~CORP_GOVI <= FSRADICALITY", data = data_i) 


###########################
#  UK, Canada, Australia  #
###########################
pof("~OCC_TRAINA*~FRM_HIERA*~IFM_RELA*~CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*~IFM_RELI*~CORP_GOVI <= FSRADICALITY", data = data_i)


##############
#  Ireland   #
##############
pof("~OCC_TRAINA*~FRM_HIERA*~IFM_RELA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*~IFM_RELI <= FSRADICALITY", data = data_i)  


##################
#  New Zealand   #
##################
pof("~OCC_TRAINA*~FRM_HIERA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*CORP_GOVI <= FSRADICALITY", data = data_i) 


############
#  Spain   #
############
pof("~OCC_TRAINA*IFM_RELA*~EMP_RELA <= FSRADICALITY", data = data_a)
pof("~OCC_TRAINI*IFM_RELI*~EMP_RELI <= FSRADICALITY", data = data_i)


############
#  France  #
############
pof("FRM_HIERA <= FSRADICALITY", data = data_a)
pof("FRM_HIERI <= FSRADICALITY", data = data_i) 


#########################
#  Portugal and Greece  #
#########################
pof("~OCC_TRAINA*~FRM_HIERA*IFM_RELA*EMP_RELA*CORP_GOVA <=  FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*IFM_RELI*EMP_RELI*CORP_GOVI <=  FSRADICALITY", data = data_i) 


############
#  Italy   #
############
pof("OCC_TRAINA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 


############
#  Japan   #
############
pof("~OCC_TRAINA*~FRM_HIERA*IFM_RELA*EMP_RELA*~CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*IFM_RELI*EMP_RELI*~CORP_GOVI <= FSRADICALITY", data = data_i)


###############
#  S. Korea   #
###############
pof("~OCC_TRAINA*~FRM_HIERA*IFM_RELA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*IFM_RELI <= FSRADICALITY", data = data_i) 


#############
#  Finland  #
#############
pof("~OCC_TRAINA*FRM_HIERA*~IFM_RELA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*FRM_HIERI*~IFM_RELI <= FSRADICALITY", data = data_i)


##################
#  Switzerland   #
##################
pof("OCC_TRAINA*EMP_RELA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*EMP_RELI <= FSRADICALITY", data = data_i) 


##################
#    Denmark     #
##################
pof("~OCC_TRAINA*FRM_HIERA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*FRM_HIERI*CORP_GOVI <= FSRADICALITY", data = data_i)


###############################
#    Sweden & Netherlands     #
###############################
pof("FRM_HIERA*EMP_RELA <= FSRADICALITY", data = data_a) 
pof("FRM_HIERI*EMP_RELI <= FSRADICALITY", data = data_i) 


##################
#    Norway      #
##################
pof("~OCC_TRAINA*FRM_HIERA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*FRM_HIERI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 


##################
#    Belgium     #
##################
pof("OCC_TRAINA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 


##################
#    Germany     #
##################
pof("OCC_TRAINA*FRM_HIERA*IFM_RELA*EMP_RELA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*FRM_HIERI*IFM_RELI*EMP_RELI <= FSRADICALITY", data = data_i) 


##################
#    Austria     #
##################
pof("OCC_TRAINA*FRM_HIERA*IFM_RELA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*FRM_HIERI*IFM_RELI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 




#######################################
############## 80-10 ##################
#######################################


quantile(data8010$originality_hhi, c(.25, .50, .75, .90, .95, .99), na.rm=T)    

# e = complete nonmembership value' c = the crossover value (re: more/less in than out) i=complete membership
data8010$FSRADICALITY <- calibrate(data8010$originality_hhi, type = "fuzzy", 
                                   thresholds = "e=  0,  c=  0, i=0.6873961", ecdf = TRUE)

# keep only needed columns 
keepsa <- c("FSRADICALITY","CORP_GOVA", "IFM_RELA", "FRM_HIERA", "EMP_RELA",
            "OCC_TRAINA")

keepsi <- c("FSRADICALITY", "CORP_GOVI", "IFM_RELI", "FRM_HIERI", "EMP_RELI",
            "OCC_TRAINI")

data_a <- data8010[keepsa]
data_i <- data8010[keepsi]



######################
#  US configuration  #
######################

# assignee
pof("~OCC_TRAINA*~FRM_HIERA*~IFM_RELA*~EMP_RELA*~CORP_GOVA <= FSRADICALITY", data = data_a) # <= denotes necessity, => denotes sufficiency, + = logical OR, * = logical AND ~ = negation

# inventor
pof("~OCC_TRAINI*~FRM_HIERI*~IFM_RELI*~EMP_RELI*~CORP_GOVI <= FSRADICALITY", data = data_i) 


###########################
#  UK, Canada, Australia  #
###########################
pof("~OCC_TRAINA*~FRM_HIERA*~IFM_RELA*~CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*~IFM_RELI*~CORP_GOVI <= FSRADICALITY", data = data_i)


##############
#  Ireland   #
##############
pof("~OCC_TRAINA*~FRM_HIERA*~IFM_RELA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*~IFM_RELI <= FSRADICALITY", data = data_i)  


##################
#  New Zealand   #
##################
pof("~OCC_TRAINA*~FRM_HIERA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*CORP_GOVI <= FSRADICALITY", data = data_i) 


############
#  Spain   #
############
pof("~OCC_TRAINA*IFM_RELA*~EMP_RELA <= FSRADICALITY", data = data_a)
pof("~OCC_TRAINI*IFM_RELI*~EMP_RELI <= FSRADICALITY", data = data_i)



############
#  France  #
############
pof("FRM_HIERA <= FSRADICALITY", data = data_a)
pof("FRM_HIERI <= FSRADICALITY", data = data_i) 


#########################
#  Portugal and Greece  #
#########################
pof("~OCC_TRAINA*~FRM_HIERA*IFM_RELA*EMP_RELA*CORP_GOVA <=  FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*IFM_RELI*EMP_RELI*CORP_GOVI <=  FSRADICALITY", data = data_i) 


############
#  Italy   #
############
pof("OCC_TRAINA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 


############
#  Japan   #
############
pof("~OCC_TRAINA*~FRM_HIERA*IFM_RELA*EMP_RELA*~CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*IFM_RELI*EMP_RELI*~CORP_GOVI <= FSRADICALITY", data = data_i)


###############
#  S. Korea   #
###############
pof("~OCC_TRAINA*~FRM_HIERA*IFM_RELA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*IFM_RELI <= FSRADICALITY", data = data_i) 


#############
#  Finland  #
#############
pof("~OCC_TRAINA*FRM_HIERA*~IFM_RELA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*FRM_HIERI*~IFM_RELI <= FSRADICALITY", data = data_i)


##################
#  Switzerland   #
##################
pof("OCC_TRAINA*EMP_RELA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*EMP_RELI <= FSRADICALITY", data = data_i) 


##################
#    Denmark     #
##################
pof("~OCC_TRAINA*FRM_HIERA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*FRM_HIERI*CORP_GOVI <= FSRADICALITY", data = data_i)


###############################
#    Sweden & Netherlands     #
###############################
pof("FRM_HIERA*EMP_RELA <= FSRADICALITY", data = data_a) 
pof("FRM_HIERI*EMP_RELI <= FSRADICALITY", data = data_i) 


##################
#    Norway      #
##################
pof("~OCC_TRAINA*FRM_HIERA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*FRM_HIERI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 


##################
#    Belgium     #
##################
pof("OCC_TRAINA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 


##################
#    Germany     #
##################
pof("OCC_TRAINA*FRM_HIERA*IFM_RELA*EMP_RELA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*FRM_HIERI*IFM_RELI*EMP_RELI <= FSRADICALITY", data = data_i) 


##################
#    Austria     #
##################
pof("OCC_TRAINA*FRM_HIERA*IFM_RELA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*FRM_HIERI*IFM_RELI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 





#########################################
############### 90-20 ###################
#########################################


quantile(data9020$originality_hhi, c(.25, .50, .75, .90, .95, .99), na.rm = T)    


data9020$FSRADICALITY <- calibrate(data9020$originality_hhi, type = "fuzzy", 
                                   thresholds = "e=  0,  c= 0.1124567 , i=0.7242798 ", ecdf = TRUE)


# keep only needed columns 
keepsa <- c("FSRADICALITY","CORP_GOVA", "IFM_RELA", "FRM_HIERA", "EMP_RELA",
            "OCC_TRAINA")

keepsi <- c("FSRADICALITY", "CORP_GOVI", "IFM_RELI", "FRM_HIERI", "EMP_RELI",
            "OCC_TRAINI")

data_a <- data9020[keepsa]
data_i <- data9020[keepsi]



######################
#  US configuration  #
######################
# assignee
pof("~OCC_TRAINA*~FRM_HIERA*~IFM_RELA*~EMP_RELA*~CORP_GOVA <= FSRADICALITY", data = data_a) # <= denotes necessity, => denotes sufficiency, + = logical OR, * = logical AND ~ = negation

# inventor
pof("~OCC_TRAINI*~FRM_HIERI*~IFM_RELI*~EMP_RELI*~CORP_GOVI <= FSRADICALITY", data = data_i) 


###########################
#  UK, Canada, Australia  #
###########################
pof("~OCC_TRAINA*~FRM_HIERA*~IFM_RELA*~CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*~IFM_RELI*~CORP_GOVI <= FSRADICALITY", data = data_i)


##############
#  Ireland   #
##############
pof("~OCC_TRAINA*~FRM_HIERA*~IFM_RELA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*~IFM_RELI <= FSRADICALITY", data = data_i)  


##################
#  New Zealand   #
##################
pof("~OCC_TRAINA*~FRM_HIERA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*CORP_GOVI <= FSRADICALITY", data = data_i) 


############
#  Spain   #
############
pof("~OCC_TRAINA*IFM_RELA*~EMP_RELA <= FSRADICALITY", data = data_a)
pof("~OCC_TRAINI*IFM_RELI*~EMP_RELI <= FSRADICALITY", data = data_i)


############
#  France  #
############
pof("FRM_HIERA <= FSRADICALITY", data = data_a)
pof("FRM_HIERI <= FSRADICALITY", data = data_i) 


#########################
#  Portugal and Greece  #
#########################
pof("~OCC_TRAINA*~FRM_HIERA*IFM_RELA*EMP_RELA*CORP_GOVA <=  FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*IFM_RELI*EMP_RELI*CORP_GOVI <=  FSRADICALITY", data = data_i) 


############
#  Italy   #
############
pof("OCC_TRAINA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 


############
#  Japan   #
############
pof("~OCC_TRAINA*~FRM_HIERA*IFM_RELA*EMP_RELA*~CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*IFM_RELI*EMP_RELI*~CORP_GOVI <= FSRADICALITY", data = data_i)


###############
#  S. Korea   #
###############
pof("~OCC_TRAINA*~FRM_HIERA*IFM_RELA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*IFM_RELI <= FSRADICALITY", data = data_i) 


#############
#  Finland  #
#############
pof("~OCC_TRAINA*FRM_HIERA*~IFM_RELA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*FRM_HIERI*~IFM_RELI <= FSRADICALITY", data = data_i)


##################
#  Switzerland   #
##################
pof("OCC_TRAINA*EMP_RELA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*EMP_RELI <= FSRADICALITY", data = data_i) 


##################
#    Denmark     #
##################
pof("~OCC_TRAINA*FRM_HIERA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*FRM_HIERI*CORP_GOVI <= FSRADICALITY", data = data_i)


###############################
#    Sweden & Netherlands     #
###############################
pof("FRM_HIERA*EMP_RELA <= FSRADICALITY", data = data_a) 
pof("FRM_HIERI*EMP_RELI <= FSRADICALITY", data = data_i) 


##################
#    Norway      #
##################
pof("~OCC_TRAINA*FRM_HIERA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*FRM_HIERI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 


##################
#    Belgium     #
##################
pof("OCC_TRAINA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 


##################
#    Germany     #
##################
pof("OCC_TRAINA*FRM_HIERA*IFM_RELA*EMP_RELA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*FRM_HIERI*IFM_RELI*EMP_RELI <= FSRADICALITY", data = data_i) 


##################
#    Austria     #
##################
pof("OCC_TRAINA*FRM_HIERA*IFM_RELA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*FRM_HIERI*IFM_RELI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 



########################################################################
#                                                                      #
# assess the effect of generality scores courtesy of Hall et al (2001) #
#                                                                      #
########################################################################


########################################
#####        full period          ######
########################################


# get radical patent counts by country
quantile(data$generality_hhi, c(.25, .50, .75, .90, .95, .99), na.rm=T) 


data$FSRADICALITY <- calibrate(data$generality_hhi, type = "fuzzy", 
                               thresholds = "e=  0, c= 0, i= 0.7222222", ecdf = TRUE)


keepsa <- c("FSRADICALITY", "CORP_GOVA", "IFM_RELA", "FRM_HIERA", "EMP_RELA",
            "OCC_TRAINA") # assignee

keepsi <- c("FSRADICALITY",  "CORP_GOVI", "IFM_RELI", "FRM_HIERI", "EMP_RELI",
            "OCC_TRAINI") # inventor

data_a <- data[keepsa]
data_i <- data[keepsi]


######################
#  US configuration  #
######################

# assignee
pof("~OCC_TRAINA*~FRM_HIERA*~IFM_RELA*~EMP_RELA*~CORP_GOVA <= FSRADICALITY", data = data_a) # <= denotes necessity, => denotes sufficiency, + = logical OR, * = logical AND ~ = negation

# inventor
pof("~OCC_TRAINI*~FRM_HIERI*~IFM_RELI*~EMP_RELI*~CORP_GOVI <= FSRADICALITY", data = data_i) 


###########################
#  UK, Canada, Australia  #
###########################
pof("~OCC_TRAINA*~FRM_HIERA*~IFM_RELA*~CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*~IFM_RELI*~CORP_GOVI <= FSRADICALITY", data = data_i)


##############
#  Ireland   #
##############
pof("~OCC_TRAINA*~FRM_HIERA*~IFM_RELA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*~IFM_RELI <= FSRADICALITY", data = data_i)  


##################
#  New Zealand   #
##################
pof("~OCC_TRAINA*~FRM_HIERA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*CORP_GOVI <= FSRADICALITY", data = data_i) 


############
#  Spain   #
############
pof("~OCC_TRAINA*IFM_RELA*~EMP_RELA <= FSRADICALITY", data = data_a)
pof("~OCC_TRAINI*IFM_RELI*~EMP_RELI <= FSRADICALITY", data = data_i)


############
#  France  #
############
pof("FRM_HIERA <= FSRADICALITY", data = data_a)
pof("FRM_HIERI <= FSRADICALITY", data = data_i) 


#########################
#  Portugal and Greece  #
#########################
pof("~OCC_TRAINA*~FRM_HIERA*IFM_RELA*EMP_RELA*CORP_GOVA <=  FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*IFM_RELI*EMP_RELI*CORP_GOVI <=  FSRADICALITY", data = data_i) 


############
#  Italy   #
############
pof("OCC_TRAINA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 


############
#  Japan   #
############
pof("~OCC_TRAINA*~FRM_HIERA*IFM_RELA*EMP_RELA*~CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*IFM_RELI*EMP_RELI*~CORP_GOVI <= FSRADICALITY", data = data_i)


###############
#  S. Korea   #
###############
pof("~OCC_TRAINA*~FRM_HIERA*IFM_RELA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*IFM_RELI <= FSRADICALITY", data = data_i) 


#############
#  Finland  #
#############
pof("~OCC_TRAINA*FRM_HIERA*~IFM_RELA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*FRM_HIERI*~IFM_RELI <= FSRADICALITY", data = data_i)


##################
#  Switzerland   #
##################
pof("OCC_TRAINA*EMP_RELA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*EMP_RELI <= FSRADICALITY", data = data_i) 


##################
#    Denmark     #
##################
pof("~OCC_TRAINA*FRM_HIERA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*FRM_HIERI*CORP_GOVI <= FSRADICALITY", data = data_i)


###############################
#    Sweden & Netherlands     #
###############################
pof("FRM_HIERA*EMP_RELA <= FSRADICALITY", data = data_a) 
pof("FRM_HIERI*EMP_RELI <= FSRADICALITY", data = data_i) 


##################
#    Norway      #
##################
pof("~OCC_TRAINA*FRM_HIERA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*FRM_HIERI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 


##################
#    Belgium     #
##################
pof("OCC_TRAINA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 


##################
#    Germany     #
##################
pof("OCC_TRAINA*FRM_HIERA*IFM_RELA*EMP_RELA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*FRM_HIERI*IFM_RELI*EMP_RELI <= FSRADICALITY", data = data_i) 


##################
#    Austria     #
##################
pof("OCC_TRAINA*FRM_HIERA*IFM_RELA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*FRM_HIERI*IFM_RELI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 




#########################################
############### 60-90 ###################
#########################################


quantile(data6090$generality_hhi, c(.25, .50, .75, .90, .95, .99), na.rm=T) 


# e = complete nonmembership value' c = the crossover value (re: more/less in than out) i=complete membership
data6090$FSRADICALITY<- calibrate(data6090$generality_hhi, type = "fuzzy", 
                                  thresholds = "e=  0,  c= 0 , i=0.7187500 ", ecdf = TRUE)


keepsa <- c("FSRADICALITY", "CORP_GOVA", "IFM_RELA", "FRM_HIERA", "EMP_RELA",
            "OCC_TRAINA")

keepsi <- c("FSRADICALITY","CORP_GOVI", "IFM_RELI", "FRM_HIERI", "EMP_RELI",
            "OCC_TRAINI")

data_a <- data6090[keepsa]
data_i <- data6090[keepsi]



######################
#  US configuration  #
######################

# assignee
pof("~OCC_TRAINA*~FRM_HIERA*~IFM_RELA*~EMP_RELA*~CORP_GOVA <= FSRADICALITY", data = data_a) # <= denotes necessity, => denotes sufficiency, + = logical OR, * = logical AND ~ = negation

# inventor
pof("~OCC_TRAINI*~FRM_HIERI*~IFM_RELI*~EMP_RELI*~CORP_GOVI <= FSRADICALITY", data = data_i) 


###########################
#  UK, Canada, Australia  #
###########################
pof("~OCC_TRAINA*~FRM_HIERA*~IFM_RELA*~CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*~IFM_RELI*~CORP_GOVI <= FSRADICALITY", data = data_i)


##############
#  Ireland   #
##############
pof("~OCC_TRAINA*~FRM_HIERA*~IFM_RELA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*~IFM_RELI <= FSRADICALITY", data = data_i)  


##################
#  New Zealand   #
##################
pof("~OCC_TRAINA*~FRM_HIERA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*CORP_GOVI <= FSRADICALITY", data = data_i) 


############
#  Spain   #
############
pof("~OCC_TRAINA*IFM_RELA*~EMP_RELA <= FSRADICALITY", data = data_a)
pof("~OCC_TRAINI*IFM_RELI*~EMP_RELI <= FSRADICALITY", data = data_i)


############
#  France  #
############
pof("FRM_HIERA <= FSRADICALITY", data = data_a)
pof("FRM_HIERI <= FSRADICALITY", data = data_i) 


#########################
#  Portugal and Greece  #
#########################
pof("~OCC_TRAINA*~FRM_HIERA*IFM_RELA*EMP_RELA*CORP_GOVA <=  FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*IFM_RELI*EMP_RELI*CORP_GOVI <=  FSRADICALITY", data = data_i) 


############
#  Italy   #
############
pof("OCC_TRAINA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 


############
#  Japan   #
############
pof("~OCC_TRAINA*~FRM_HIERA*IFM_RELA*EMP_RELA*~CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*IFM_RELI*EMP_RELI*~CORP_GOVI <= FSRADICALITY", data = data_i)


###############
#  S. Korea   #
###############
pof("~OCC_TRAINA*~FRM_HIERA*IFM_RELA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*IFM_RELI <= FSRADICALITY", data = data_i) 


#############
#  Finland  #
#############
pof("~OCC_TRAINA*FRM_HIERA*~IFM_RELA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*FRM_HIERI*~IFM_RELI <= FSRADICALITY", data = data_i)


##################
#  Switzerland   #
##################
pof("OCC_TRAINA*EMP_RELA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*EMP_RELI <= FSRADICALITY", data = data_i) 


##################
#    Denmark     #
##################
pof("~OCC_TRAINA*FRM_HIERA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*FRM_HIERI*CORP_GOVI <= FSRADICALITY", data = data_i)


###############################
#    Sweden & Netherlands     #
###############################
pof("FRM_HIERA*EMP_RELA <= FSRADICALITY", data = data_a) 
pof("FRM_HIERI*EMP_RELI <= FSRADICALITY", data = data_i) 


##################
#    Norway      #
##################
pof("~OCC_TRAINA*FRM_HIERA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*FRM_HIERI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 


##################
#    Belgium     #
##################
pof("OCC_TRAINA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 


##################
#    Germany     #
##################
pof("OCC_TRAINA*FRM_HIERA*IFM_RELA*EMP_RELA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*FRM_HIERI*IFM_RELI*EMP_RELI <= FSRADICALITY", data = data_i) 


##################
#    Austria     #
##################
pof("OCC_TRAINA*FRM_HIERA*IFM_RELA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*FRM_HIERI*IFM_RELI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 



#########################################
############### 70-00 ###################
#########################################


quantile(data7000$generality_hhi, c(.25, .50, .75, .90, .95, .99), na.rm=T)    

# e = complete nonmembership value' c = the crossover value (re: more/less in than out) i=complete membership
data7000$FSRADICALITY <- calibrate(data7000$generality_hhi, type = "fuzzy", 
                                   thresholds = "e=  0,  c=  0, i=0.7585161", ecdf = TRUE)


# keep only needed columns 
keepsa <- c("FSRADICALITY","CORP_GOVA", "IFM_RELA", "FRM_HIERA", "EMP_RELA",
            "OCC_TRAINA")

keepsi <- c("FSRADICALITY", "CORP_GOVI", "IFM_RELI", "FRM_HIERI", "EMP_RELI",
            "OCC_TRAINI")

data_a <- data7000[keepsa]
data_i <- data7000[keepsi]



######################
#  US configuration  #
######################

# assignee
pof("~OCC_TRAINA*~FRM_HIERA*~IFM_RELA*~EMP_RELA*~CORP_GOVA <= FSRADICALITY", data = data_a) # <= denotes necessity, => denotes sufficiency, + = logical OR, * = logical AND ~ = negation

# inventor
pof("~OCC_TRAINI*~FRM_HIERI*~IFM_RELI*~EMP_RELI*~CORP_GOVI <= FSRADICALITY", data = data_i) 


###########################
#  UK, Canada, Australia  #
###########################
pof("~OCC_TRAINA*~FRM_HIERA*~IFM_RELA*~CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*~IFM_RELI*~CORP_GOVI <= FSRADICALITY", data = data_i)


##############
#  Ireland   #
##############
pof("~OCC_TRAINA*~FRM_HIERA*~IFM_RELA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*~IFM_RELI <= FSRADICALITY", data = data_i)  


##################
#  New Zealand   #
##################
pof("~OCC_TRAINA*~FRM_HIERA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*CORP_GOVI <= FSRADICALITY", data = data_i) 


############
#  Spain   #
############
pof("~OCC_TRAINA*IFM_RELA*~EMP_RELA <= FSRADICALITY", data = data_a)
pof("~OCC_TRAINI*IFM_RELI*~EMP_RELI <= FSRADICALITY", data = data_i)


############
#  France  #
############
pof("FRM_HIERA <= FSRADICALITY", data = data_a)
pof("FRM_HIERI <= FSRADICALITY", data = data_i) 


#########################
#  Portugal and Greece  #
#########################
pof("~OCC_TRAINA*~FRM_HIERA*IFM_RELA*EMP_RELA*CORP_GOVA <=  FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*IFM_RELI*EMP_RELI*CORP_GOVI <=  FSRADICALITY", data = data_i) 


############
#  Italy   #
############
pof("OCC_TRAINA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 


############
#  Japan   #
############
pof("~OCC_TRAINA*~FRM_HIERA*IFM_RELA*EMP_RELA*~CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*IFM_RELI*EMP_RELI*~CORP_GOVI <= FSRADICALITY", data = data_i)


###############
#  S. Korea   #
###############
pof("~OCC_TRAINA*~FRM_HIERA*IFM_RELA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*IFM_RELI <= FSRADICALITY", data = data_i) 


#############
#  Finland  #
#############
pof("~OCC_TRAINA*FRM_HIERA*~IFM_RELA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*FRM_HIERI*~IFM_RELI <= FSRADICALITY", data = data_i)


##################
#  Switzerland   #
##################
pof("OCC_TRAINA*EMP_RELA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*EMP_RELI <= FSRADICALITY", data = data_i) 


##################
#    Denmark     #
##################
pof("~OCC_TRAINA*FRM_HIERA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*FRM_HIERI*CORP_GOVI <= FSRADICALITY", data = data_i)


###############################
#    Sweden & Netherlands     #
###############################
pof("FRM_HIERA*EMP_RELA <= FSRADICALITY", data = data_a) 
pof("FRM_HIERI*EMP_RELI <= FSRADICALITY", data = data_i) 


##################
#    Norway      #
##################
pof("~OCC_TRAINA*FRM_HIERA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*FRM_HIERI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 


##################
#    Belgium     #
##################
pof("OCC_TRAINA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 


##################
#    Germany     #
##################
pof("OCC_TRAINA*FRM_HIERA*IFM_RELA*EMP_RELA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*FRM_HIERI*IFM_RELI*EMP_RELI <= FSRADICALITY", data = data_i) 


##################
#    Austria     #
##################
pof("OCC_TRAINA*FRM_HIERA*IFM_RELA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*FRM_HIERI*IFM_RELI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 




#######################################
############## 80-10 ##################
#######################################


quantile(data8010$generality_hhi, c(.25, .50, .75, .90, .95, .99), na.rm=T)    

# e = complete nonmembership value' c = the crossover value (re: more/less in than out) i=complete membership
data8010$FSRADICALITY <- calibrate(data8010$generality_hhi, type = "fuzzy", 
                                   thresholds = "e=  0,  c=  0, i=0.7466667", ecdf = TRUE)

# keep only needed columns 
keepsa <- c("FSRADICALITY","CORP_GOVA", "IFM_RELA", "FRM_HIERA", "EMP_RELA",
            "OCC_TRAINA")

keepsi <- c("FSRADICALITY", "CORP_GOVI", "IFM_RELI", "FRM_HIERI", "EMP_RELI",
            "OCC_TRAINI")

data_a <- data8010[keepsa]
data_i <- data8010[keepsi]



######################
#  US configuration  #
######################

# assignee
pof("~OCC_TRAINA*~FRM_HIERA*~IFM_RELA*~EMP_RELA*~CORP_GOVA <= FSRADICALITY", data = data_a) # <= denotes necessity, => denotes sufficiency, + = logical OR, * = logical AND ~ = negation

# inventor
pof("~OCC_TRAINI*~FRM_HIERI*~IFM_RELI*~EMP_RELI*~CORP_GOVI <= FSRADICALITY", data = data_i) 


###########################
#  UK, Canada, Australia  #
###########################
pof("~OCC_TRAINA*~FRM_HIERA*~IFM_RELA*~CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*~IFM_RELI*~CORP_GOVI <= FSRADICALITY", data = data_i)


##############
#  Ireland   #
##############
pof("~OCC_TRAINA*~FRM_HIERA*~IFM_RELA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*~IFM_RELI <= FSRADICALITY", data = data_i)  


##################
#  New Zealand   #
##################
pof("~OCC_TRAINA*~FRM_HIERA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*CORP_GOVI <= FSRADICALITY", data = data_i) 


############
#  Spain   #
############
pof("~OCC_TRAINA*IFM_RELA*~EMP_RELA <= FSRADICALITY", data = data_a)
pof("~OCC_TRAINI*IFM_RELI*~EMP_RELI <= FSRADICALITY", data = data_i)


############
#  France  #
############
pof("FRM_HIERA <= FSRADICALITY", data = data_a)
pof("FRM_HIERI <= FSRADICALITY", data = data_i) 


#########################
#  Portugal and Greece  #
#########################
pof("~OCC_TRAINA*~FRM_HIERA*IFM_RELA*EMP_RELA*CORP_GOVA <=  FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*IFM_RELI*EMP_RELI*CORP_GOVI <=  FSRADICALITY", data = data_i) 


############
#  Italy   #
############
pof("OCC_TRAINA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 


############
#  Japan   #
############
pof("~OCC_TRAINA*~FRM_HIERA*IFM_RELA*EMP_RELA*~CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*IFM_RELI*EMP_RELI*~CORP_GOVI <= FSRADICALITY", data = data_i)


###############
#  S. Korea   #
###############
pof("~OCC_TRAINA*~FRM_HIERA*IFM_RELA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*IFM_RELI <= FSRADICALITY", data = data_i) 


#############
#  Finland  #
#############
pof("~OCC_TRAINA*FRM_HIERA*~IFM_RELA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*FRM_HIERI*~IFM_RELI <= FSRADICALITY", data = data_i)


##################
#  Switzerland   #
##################
pof("OCC_TRAINA*EMP_RELA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*EMP_RELI <= FSRADICALITY", data = data_i) 


##################
#    Denmark     #
##################
pof("~OCC_TRAINA*FRM_HIERA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*FRM_HIERI*CORP_GOVI <= FSRADICALITY", data = data_i)


###############################
#    Sweden & Netherlands     #
###############################
pof("FRM_HIERA*EMP_RELA <= FSRADICALITY", data = data_a) 
pof("FRM_HIERI*EMP_RELI <= FSRADICALITY", data = data_i) 


##################
#    Norway      #
##################
pof("~OCC_TRAINA*FRM_HIERA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*FRM_HIERI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 


##################
#    Belgium     #
##################
pof("OCC_TRAINA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 


##################
#    Germany     #
##################
pof("OCC_TRAINA*FRM_HIERA*IFM_RELA*EMP_RELA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*FRM_HIERI*IFM_RELI*EMP_RELI <= FSRADICALITY", data = data_i) 


##################
#    Austria     #
##################
pof("OCC_TRAINA*FRM_HIERA*IFM_RELA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*FRM_HIERI*IFM_RELI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 





#########################################
############### 90-20 ###################
#########################################


quantile(data9020$generality_hhi, c(.25, .50, .75, .90, .95, .99), na.rm = T)    


data9020$FSRADICALITY <- calibrate(data9020$generality_hhi, type = "fuzzy", 
                                   thresholds = "e=  0,  c= 0 , i=0.7222222  ", ecdf = TRUE)


# keep only needed columns 
keepsa <- c("FSRADICALITY","CORP_GOVA", "IFM_RELA", "FRM_HIERA", "EMP_RELA",
            "OCC_TRAINA")

keepsi <- c("FSRADICALITY", "CORP_GOVI", "IFM_RELI", "FRM_HIERI", "EMP_RELI",
            "OCC_TRAINI")

data_a <- data9020[keepsa]
data_i <- data9020[keepsi]



######################
#  US configuration  #
######################
# assignee
pof("~OCC_TRAINA*~FRM_HIERA*~IFM_RELA*~EMP_RELA*~CORP_GOVA <= FSRADICALITY", data = data_a) # <= denotes necessity, => denotes sufficiency, + = logical OR, * = logical AND ~ = negation

# inventor
pof("~OCC_TRAINI*~FRM_HIERI*~IFM_RELI*~EMP_RELI*~CORP_GOVI <= FSRADICALITY", data = data_i) 


###########################
#  UK, Canada, Australia  #
###########################
pof("~OCC_TRAINA*~FRM_HIERA*~IFM_RELA*~CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*~IFM_RELI*~CORP_GOVI <= FSRADICALITY", data = data_i)


##############
#  Ireland   #
##############
pof("~OCC_TRAINA*~FRM_HIERA*~IFM_RELA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*~IFM_RELI <= FSRADICALITY", data = data_i)  


##################
#  New Zealand   #
##################
pof("~OCC_TRAINA*~FRM_HIERA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*CORP_GOVI <= FSRADICALITY", data = data_i) 


############
#  Spain   #
############
pof("~OCC_TRAINA*IFM_RELA*~EMP_RELA <= FSRADICALITY", data = data_a)
pof("~OCC_TRAINI*IFM_RELI*~EMP_RELI <= FSRADICALITY", data = data_i)


############
#  France  #
############
pof("FRM_HIERA <= FSRADICALITY", data = data_a)
pof("FRM_HIERI <= FSRADICALITY", data = data_i) 


#########################
#  Portugal and Greece  #
#########################
pof("~OCC_TRAINA*~FRM_HIERA*IFM_RELA*EMP_RELA*CORP_GOVA <=  FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*IFM_RELI*EMP_RELI*CORP_GOVI <=  FSRADICALITY", data = data_i) 


############
#  Italy   #
############
pof("OCC_TRAINA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 


############
#  Japan   #
############
pof("~OCC_TRAINA*~FRM_HIERA*IFM_RELA*EMP_RELA*~CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*IFM_RELI*EMP_RELI*~CORP_GOVI <= FSRADICALITY", data = data_i)


###############
#  S. Korea   #
###############
pof("~OCC_TRAINA*~FRM_HIERA*IFM_RELA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*~FRM_HIERI*IFM_RELI <= FSRADICALITY", data = data_i) 


#############
#  Finland  #
#############
pof("~OCC_TRAINA*FRM_HIERA*~IFM_RELA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*FRM_HIERI*~IFM_RELI <= FSRADICALITY", data = data_i)


##################
#  Switzerland   #
##################
pof("OCC_TRAINA*EMP_RELA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*EMP_RELI <= FSRADICALITY", data = data_i) 


##################
#    Denmark     #
##################
pof("~OCC_TRAINA*FRM_HIERA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*FRM_HIERI*CORP_GOVI <= FSRADICALITY", data = data_i)


###############################
#    Sweden & Netherlands     #
###############################
pof("FRM_HIERA*EMP_RELA <= FSRADICALITY", data = data_a) 
pof("FRM_HIERI*EMP_RELI <= FSRADICALITY", data = data_i) 


##################
#    Norway      #
##################
pof("~OCC_TRAINA*FRM_HIERA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("~OCC_TRAINI*FRM_HIERI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 


##################
#    Belgium     #
##################
pof("OCC_TRAINA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 


##################
#    Germany     #
##################
pof("OCC_TRAINA*FRM_HIERA*IFM_RELA*EMP_RELA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*FRM_HIERI*IFM_RELI*EMP_RELI <= FSRADICALITY", data = data_i) 


##################
#    Austria     #
##################
pof("OCC_TRAINA*FRM_HIERA*IFM_RELA*EMP_RELA*CORP_GOVA <= FSRADICALITY", data = data_a) 
pof("OCC_TRAINI*FRM_HIERI*IFM_RELI*EMP_RELI*CORP_GOVI <= FSRADICALITY", data = data_i) 


