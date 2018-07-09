# Load Libraries
install.packages("psych")
install.packages("jsonlite")
library(readr)
library(plyr)
library(psych)
library(jsonlite)

# Read in 'raw' admissions file- transcriptions from the Bellevue Almshouse admissions ledgers
admissions <- read.csv(file.choose(), na.strings="")

# Clean admissions data
admissions <- rename(admissions, c(`Reason_cleaned.1`="reason_cleaned"))
admissions$Age_standardized <- as.integer(admissions$Age_standardized)
admissions$Date <- as.Date(admissions$Date, "%m/%d/%Y")
admissions <- rename(admissions, c("Date"="date_in", "Sex"="gender", "Age_standardized"="age_standard", "Occupation"="occupation", "ByWhomSent_1"="admittor_1","ByWhomSent_2"="admittor_2", "SentTo_cleaned"="sent_to_cleaned"))

# Load and clean sites data - dataset constructed from the places referenced in the Bellevue Almshouse admissions ledgers
sites <-read.csv(file.choose(), na.strings="")
bellevue_sites_variables <- c("code", "public_health_site", "public_health_site_locale")
sites <- sites[bellevue_sites_variables]
sites <- rename(sites, c("public_health_site"="site", "public_health_site_locale"="institution"))
sites$sent_to <- paste0(as.character(sites$site),"_",as.character(sites$institution))
sites$sent_to <- sub("_$", "", sites$sent_to)

# Load and clean admittors data - dataset constructed from the people mentioned in the "By Whom Sent" column of the Bellevue Almshouse admissions ledgers
admittors <- read.csv(file.choose(), na.strings="")

# Merge for admittor 1, 2 and site
bellevue_admittors1 <- merge(admissions, admittors, by.x="admittor_1", by.y="admittors_from_almshouse_register", all.x=TRUE, all.y=FALSE)
bellevue_admittors1_variables <- c("date_in", "age_standard", "gender", "occupation", "reason_cleaned", "admittor_1", "identified_admittors", "admittor_2", "sent_to_cleaned")
bellevue_admittors1 <- bellevue_admittors1[bellevue_admittors1_variables]
bellevue_admittors1 <- rename(bellevue_admittors1, c("identified_admittors"="admittor_1_cleaned"))
bellevue_admittors2 <- merge(bellevue_admittors1, admittors, by.x="admittor_2", by.y="admittors_from_almshouse_register", all.x=TRUE, all.y=FALSE)
bellevue_admittors2_variables <- c("date_in", "age_standard", "gender", "occupation", "reason_cleaned", "admittor_1", "admittor_1_cleaned", "admittor_2","identified_admittors", "sent_to_cleaned")
bellevue_admittors2 <- bellevue_admittors2[bellevue_admittors2_variables]
bellevue_admittors2 <- rename(bellevue_admittors2, c("identified_admittors"="admittor_2_cleaned"))
bellevue_admittors_sites <- merge(bellevue_admittors2, sites, by.x="sent_to_cleaned", by.y="code", all.x=TRUE, all.y=FALSE)
bellevue_admittors_sites_variables <- c("date_in", "age_standard", "gender", "occupation", "reason_cleaned", "admittor_1_cleaned", "admittor_2_cleaned", "site","institution","sent_to")
bellevue_admittors_sites <- bellevue_admittors_sites[bellevue_admittors_sites_variables]
data <- bellevue_admittors_sites
data$occupation <- as.character(data$occupation)
data$gender <- as.character(data$gender)
data$reason_cleaned <- as.character(data$reason_cleaned)
data$admittor_1_cleaned <- as.character(data$admittor_1_cleaned)
data$admittor_2_cleaned <- as.character(data$admittor_2_cleaned)
data$site <- as.character(data$site)
data$institution <- as.character(data$institution)

# Subset the data to keep only the entries that include "sent_to", removes illegible entries
data <- data[ which(data$reason_cleaned!="(illegible)"), ]
data <- data[ which(data$reason_cleaned!="for say"), ]

write.csv(bellevue_admittors_sites, file="bellevue_for_R.csv")

# Creates JSON for path visualization
parallel_vars <- c("gender", "age_category", "occupation", "admittor_1_cleaned", "admittor_2_cleaned", "reason_cleaned", "site", "institution")
data$age_category <- cut(data$age_standard, c(0,3,10,18,30,100), labels = c("infant", "child", "teen", "young adult", "adult"))
parallel_data <- data[parallel_vars]
parallel_data <- rename(parallel_data, c("age_category"="age", "reason_cleaned"="reason", "admittor_1_cleaned"="By_Whom_Sent_1", "admittor_2_cleaned"="By_Whom_Sent_2"))
parallel_data <- parallel_data[ which(parallel_data$site!="NA"), ]
parallel_data <- parallel_data[ which(parallel_data$age!="NA"), ]
parallel_data <- parallel_data[ which(parallel_data$gender!="?"), ]
parallel_data <- parallel_data[ which(parallel_data$By_Whom_Sent_1!="NA"), ]
parallel_data[is.na(parallel_data)] <- "not specificed"

Bellevue_JSON <- toJSON(parallel_data, pretty=TRUE)
write(Bellevue_JSON, file="Bellevue.json")

write.csv(parallel_data, file="Bellevue.csv")
