################################################################
######### Main effect socio-economic covariates ################
################################################################

#### Loading libraries
library(glmmTMB)
library(dlnm)
library(splines)

###############################################
###############################################
###############################################

# Importing data and climatic variables (some of them are not necessary for the analysis),
# and merging them.

malaria_1 <- read.csv("DHS_infants2.csv")
malaria_1$HV006[malaria_1$HV006 == "3" | malaria_1$HV006 == "March"] <- "3"
malaria_1$HV006[malaria_1$HV006 == "4" | malaria_1$HV006 == "April"] <- "4"
malaria_1$HV006[malaria_1$HV006 == "5" | malaria_1$HV006 == "May"] <- "5"
malaria_1$HV006[malaria_1$HV006 == "6" | malaria_1$HV006 == "June"] <- "6"
malaria_1$HV006[malaria_1$HV006 == "10" | malaria_1$HV006 == "October"] <- "10"
malaria_1$HV006[malaria_1$HV006 == "11" | malaria_1$HV006 == "November"] <- "11"
malaria_1$HV006[malaria_1$HV006 == "12" | malaria_1$HV006 == "December"] <- "12"
malaria_1$HV006 <- as.integer(malaria_1$HV006)
malaria_1$ID <- 1:nrow(malaria_1)

# merging spei data
spei <- read.csv("spei_values_merged.csv")
merged_1 <- merge(malaria_1, spei, by = c("HV006", "HV007", "LATNUM", "LONGNUM"), all.x = TRUE)

# merging socio-economic data
smod <- read.csv("DHS_urbanization_smod.csv")
merged_2 <- merge(merged_1, smod, by = c("HV007", "LATNUM", "LONGNUM"), all.x = TRUE)

rwi <- read.csv("DHS_infants2_rwi.csv")
rwi$ID <- 1:nrow(rwi)
rwi <- rwi[, -c(1, 2)]
merged_3 <- merge(merged_2, rwi, by = "ID")

# changing longitude and latitude
# longnum_replacements = {-13.66651: -13.66671, -11.375967: -11.373967, 13.832946: 13.842946}
merged_3$LONGNUM[merged_3$LONGNUM == -13.66651] <- -13.66671
merged_3$LONGNUM[merged_3$LONGNUM == -11.375967] <- -11.373967
merged_3$LONGNUM[merged_3$LONGNUM == 13.832946] <- 13.842946
# latnum_replacements = {4.367325: 4.377325, 4.373476: 4.375476, 5.080720: 5.083720}
merged_3$LATNUM[merged_3$LATNUM == 4.367325] <- 4.377325
merged_3$LATNUM[merged_3$LATNUM == 4.373476] <- 4.375476
merged_3$LATNUM[merged_3$LATNUM == 5.080720] <- 5.083720

# merging temperature, actual evapo, precipitation and soil moisture

terra_clim <- read.csv("DHS_infants_terraclimate_pml_merged.csv")
merged_4 <- merge(merged_3, terra_clim, by = c("HV006", "HV007", "LATNUM", "LONGNUM"), all.x = TRUE)

# merging specific humidity

humidity <- read.csv("DHS_infants_fldas_merged.csv")

malaria <- merge(merged_4, humidity, by = c("HV006", "HV007", "LATNUM", "LONGNUM"), all.x = TRUE)

malaria <- malaria[order(malaria$ID), ]

###############################################
###############################################
###############################################

malaria <- malaria[!((malaria$LONGNUM > -0.01 & malaria$LONGNUM < 0.01) & (malaria$LATNUM > -0.01 & malaria$LATNUM < 0.01)), ]

#### Cleaning variables

### Survey variables

# Variable Country
malaria$Country <- factor(malaria$Country)

# Creating variable unique_cluster

cluster_coords <- unique(malaria[, c("DHSID", "LATNUM", "LONGNUM")])

cluster_coords$coord <- paste(cluster_coords$LATNUM, cluster_coords$LONGNUM, sep = "_")

double_coord <- which(table(cluster_coords$coord) > 1)

for(i in 1:length(double_coord)){
  clust_id <- cluster_coords[cluster_coords$coord == names(double_coord)[i], "DHSID"]
  new <- paste(substr(clust_id[1], 1, 6), "/", substr(clust_id[2], 3, 14), sep = "")
  malaria$DHSID[malaria$DHSID == clust_id[1] | malaria$DHSID == clust_id[2]] <- new
}

malaria$unique_cluster <- factor(malaria$DHSID)

# Variable month of the interview
malaria$month <- malaria$HV006

# Variable year of the interview
malaria$year <- malaria$HV007 - 2005

# Variable main source of drinking water
water <- data.frame(type = unique(malaria$HV201))
water$improved <- c(1, 1, 1, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 9999, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 0, 0, 0, 1, 1, 1, 0, 1, 0)
water$improved[43] <- 1

malaria$source_water <- 1*(malaria$HV201 %in% water[water$improved == 1, "type"])
malaria$source_water[is.na(malaria$HV201)] <- NA
malaria$source_water <- factor(malaria$source_water, levels = c("0", "1"), labels = c("Unimproved", "Improved"))

# Variable type of toilet facility
toilet <- data.frame(type = unique(malaria$HV205))
toilet$improved <- c(1, 1, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 1, 1, 1, 1, 1, 9999, 0, 0, 1, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1)
toilet$improved[47] <- 0
toilet$improved[c(51, 58, 68, 70)] <- 1

malaria$toilet_type <- 1*(malaria$HV205 %in% toilet[toilet$improved == 1, "type"])
malaria$toilet_type[is.na(malaria$HV205)] <- NA
malaria$toilet_type <- factor(malaria$toilet_type, levels = c("0", "1"), labels = c("Unimproved", "Improved"))

# Variable age of head of household
malaria$HV220[malaria$HV220 == "DK" | malaria$HV220 == "Don't know"] <- NA
malaria$HV220[malaria$HV220 == "95+"] <- "95"
malaria$HV220[malaria$HV220 == "97+"] <- "97"
malaria$age_head <- as.integer(malaria$HV220)

# Variable type of cooking fuel
fuel <- data.frame(type = unique(malaria$HV226))
fuel$improved <- c(1, 1, 0, 0, 0, 0, 1, 0, 1, 0, 1, 1, 0, 0, 0, 1, 0, 9999, 0, 1, 1, 1, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1)
fuel$improved[c(2, 12, 25, 34, 37, 40, 41)] <- 0

malaria$cook_fuel <- 1*(malaria$HV226 %in% fuel[fuel$improved == 1, "type"])
malaria$cook_fuel[is.na(malaria$HV226)] <- NA
malaria$cook_fuel <- factor(malaria$cook_fuel, levels = c("0", "1"), labels = c("Unimproved", "Improved"))

# Variable wealth index
malaria$HV270[malaria$HV270 == "Fourth"] <- "Richer"
malaria$HV270[malaria$HV270 == "Highest"] <- "Richest"
malaria$HV270[malaria$HV270 == "Lowest"] <- "Poorest"
malaria$HV270[malaria$HV270 == "Second"] <- "Poorer"
malaria$wealth_cat <- factor(malaria$HV270, levels = c("Poorest", "Poorer", "Middle", "Richer", "Richest")) 

# Variable sex
malaria$sex <- factor(malaria$HV104)

# Variable age in months
malaria$age_months <- malaria$HC1

# Variable mother education
malaria$mother_education <- malaria$HC61
malaria$mother_education[malaria$mother_education == "DK" | 
                           malaria$mother_education == "Don't know" | 
                           malaria$mother_education == "Others"] <- NA
malaria$mother_education[malaria$mother_education == "No education / pre-primary"] <- "No education"
malaria$mother_education[malaria$mother_education == "Primary" | 
                           malaria$mother_education == "Middle/JSS/JHS" | malaria$mother_education == "Elementary (1-6)" | malaria$mother_education == "Junior High (7-9)"] <- "Primary/Middle"
malaria$mother_education[malaria$mother_education == "Secondary" | 
                           malaria$mother_education == "Secondary/SSS/SHS" | malaria$mother_education == "Senior High (10-12)" | malaria$mother_education == "Higher"] <- "Secondary/Higher"
malaria$mother_education[is.na(malaria$mother_education)] <- "Missing"
malaria$mother_education <- factor(malaria$mother_education, 
                                   levels = c("No education", "Primary/Middle", "Secondary/Higher", "Missing"))

# Variable net indicator
malaria$HML12[malaria$HML12 == "Did not sleep under a net" | 
                malaria$HML12 == "No bednet"] <- "No"

malaria$HML12[malaria$HML12 == "Both treated (ITN) and untreated nets" | 
                malaria$HML12 == "Only treated (ITN) nets" | 
                malaria$HML12 == "Only treated bednets (ITN)" | 
                malaria$HML12 == "Both treated and untreated bednets" | 
                malaria$HML12 == "Only treated bednets" | 
                malaria$HML12 == "Only untreated bednets" | 
                malaria$HML12 == "Only untreated nets"] <- "Yes"

malaria$ITN_ind <- factor(malaria$HML12)

# Variable malaria test result
malaria$HML32[malaria$HML32 != "Positive" & malaria$HML32 != "Negative"] <- NA

malaria$HML35[malaria$HML35 == "Mixed Positive" | 
                malaria$HML35 == "Falciparum Positive" | 
                malaria$HML35 == "Other Positive" | 
                malaria$HML35 == "Positive falciparum" | 
                malaria$HML35 == "Positive falciparum and pan" | 
                malaria$HML35 == "Positive pan"] <- "Positive"

malaria$HML35[malaria$HML35 != "Positive" & malaria$HML35 != "Negative"] <- NA

malaria$test_result <- malaria$HML32
malaria$test_result[is.na(malaria$test_result)] <- malaria$HML35[is.na(malaria$test_result)]
malaria$test_result <- factor(malaria$test_result)



### Climatic variables

# Variable Actual Evapotranspiration (aet)
for(i in 1:12){
  malaria[, paste("aet_lag", i, sep = "")] <- malaria[, paste("aet_lag", i, sep = "")] * 0.1
}

# Variable Soil moisture (soil)
for(i in 1:12){
  malaria[, paste("soil_lag", i, sep = "")] <- malaria[, paste("soil_lag", i, sep = "")] * 0.1
}

# Creating average temperature variable (avg_temp)
for(i in 1:12){
  malaria[, paste("avg_temp_lag", i, sep = "")] <- (malaria[, paste("tmmn_lag", i, sep = "")] + malaria[, paste("tmmx_lag", i, sep = "")])/2 * 0.1
}

# Variable Specific Humidity (Qair_f_tavg)
for(i in 1:12){
  names(malaria)[names(malaria) == paste("Qair_f_tavg_lag", i, sep = "")] <- paste("hum_lag", i, sep = "")
}

for(i in 1:12){
  malaria[, paste("hum_lag", i, sep = "")] <- malaria[, paste("hum_lag", i, sep = "")] * 1000
}



# Variable Relative Wealth Index at/near to that location (nearest_rwi)
malaria$nearest_rwi[((malaria$LONGNUM > -0.01 & malaria$LONGNUM < 0.01) & (malaria$LATNUM > -0.01 & malaria$LATNUM < 0.01))] <- NA


# Variable smod_code (smod_code)
malaria$smod_code[((malaria$LONGNUM > -0.01 & malaria$LONGNUM < 0.01) & (malaria$LATNUM > -0.01 & malaria$LATNUM < 0.01))] <- NA

malaria$smod_code[malaria$smod_code == 10] <- NA
malaria$smod_code[malaria$smod_code == 11 | malaria$smod_code == 12 | malaria$smod_code == 13 ] <- 0
malaria$smod_code[malaria$smod_code == 21 | malaria$smod_code == 22 | malaria$smod_code == 23 | malaria$smod_code == 30] <- 1

malaria$smod_code <- factor(malaria$smod_code, levels = c("0", "1"), labels = c("Rural", "Urban"))



###############################################
###############################################
###############################################

##### Fitting models

names_climate <- c()
for(i in 1:12){
  names_climate <- c(names_climate,paste("aet_lag", i, sep = ""),
                     paste("pr_lag", i, sep = ""),
                     paste("soil_lag", i, sep = ""),
                     paste("avg_temp_lag", i, sep = ""),
                     paste("hum_lag", i, sep = "")
  )
}

group <- c("unique_cluster", "Country")

response <- "test_result"

period <- c("month", "year")

effect_modif <- c("smod_code", "source_water",
                  "toilet_type", "age_head", "sex", "age_months", 
                  "mother_education", "ITN_ind", "nearest_rwi")

data <- malaria

malaria <- na.omit(data[, c(group, names_climate, response, period, effect_modif)])

# climatic variables

temp <- rowMeans(malaria[, names_climate[grepl("avg_temp", names_climate)]][, 1:6])
  
prec <- rowMeans(malaria[, names_climate[grepl("pr", names_climate)]][, 1:6])

soil <- rowMeans(malaria[, names_climate[grepl("soil", names_climate)]][, 1:6])

aet <- rowMeans(malaria[, names_climate[grepl("aet", names_climate)]][, 1:6])

hum <- rowMeans(malaria[, names_climate[grepl("hum", names_climate)]][, 1:6])


# Fitting Model

model <- glmmTMB(test_result ~ temp + prec + soil + aet + hum + ITN_ind + smod_code + source_water + sex + toilet_type + nearest_rwi + mother_education + age_head + age_months + ns(month, df = 4) + year + (1 | unique_cluster) + (1 | Country),
                  data = malaria, family = binomial(link = "logit"), control = glmmTMBControl(optCtrl = list(iter.max=1000, eval.max=1000)))

summary(model)







