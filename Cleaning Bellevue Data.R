# Load Libraries
library(readr)
library(plyr)
install.packages("psych")
library(psych)

# Load and clean admissions data - transcriptions from the Bellevue Almshouse admissions ledgers
admissions <- read_csv("~/Dropbox (Personal)/Publications/In Progress/Digital Almshouse/Digital almshouse dataset/Bellevue Admissions Records/bellevue_admissions_2018_05_28.csv")
admissions <- rename(admissions, c(`Reason_cleaned 1`="reason_cleaned"))
admissions$Age_standardized <- as.integer(admissions$Age_standardized)
admissions$Date <- as.Date(admissions$Date, "%m/%d/%Y")
admissions <- rename(admissions, c("Date"="date_in", "Sex"="gender", "Age_standardized"="age_standard", "Occupation"="occupation", "ByWhomSent_1"="admittor_1","ByWhomSent_2"="admittor_2","Remarks"="obligations", "SentTo_cleaned"="sent_to_cleaned"))

# Load and clean sites data - dataset constructed from the places referenced in the Bellevue Almshouse admissions ledgers
sites <- read_csv("~/Dropbox (Personal)/Publications/In Progress/Digital Almshouse/Digital almshouse dataset/Bellevue sites only/sites.csv")
bellevue_sites_variables <- c("code", "public_health_site", "public_health_site_locale")
sites <- sites[bellevue_sites_variables]
sites <- rename(sites, c("public_health_site"="site", "public_health_site_locale"="sub_site"))
sites$sent_to <- paste0(as.character(sites$site),"_",as.character(sites$sub_site))

# Load and clean admittors data - dataset constructed from the people mentioned in the "By Whom Sent" column of the Bellevue Almshouse admissions ledgers
admittors <- read_csv("~/Dropbox (Personal)/Publications/In Progress/Digital Almshouse/Digital almshouse dataset/Bellevue admittors only/admittors.csv")

# Merge for admittor 1, 2 and site
bellevue_admittors1 <- merge(admissions, admittors, by.x="admittor_1", by.y="admittors_from_almshouse_register", all.x=TRUE, all.y=FALSE)
bellevue_admittors1_variables <- c("LastName", "FirstName", "date_in", "age_standard", "gender", "occupation", "reason_cleaned", "admittor_1", "identified_admittors", "admittor_2", "sent_to_cleaned", "obligations")
bellevue_admittors1 <- bellevue_admittors1[bellevue_admittors1_variables]
bellevue_admittors1 <- rename(bellevue_admittors1, c("identified_admittors"="admittor_1_cleaned"))
bellevue_admittors2 <- merge(bellevue_admittors1, admittors, by.x="admittor_2", by.y="admittors_from_almshouse_register", all.x=TRUE, all.y=FALSE)
bellevue_admittors2_variables <- c("LastName", "FirstName", "date_in", "age_standard", "gender", "occupation", "reason_cleaned", "admittor_1", "admittor_1_cleaned", "admittor_2","identified_admittors", "sent_to_cleaned", "obligations")
bellevue_admittors2 <- bellevue_admittors2[bellevue_admittors2_variables]
bellevue_admittors2 <- rename(bellevue_admittors2, c("identified_admittors"="admittor_2_cleaned"))
bellevue_admittors_sites <- merge(bellevue_admittors2, sites, by.x="sent_to_cleaned", by.y="code", all.x=TRUE, all.y=FALSE)
bellevue_admittors_sites_variables <- c("LastName", "FirstName", "date_in", "age_standard", "gender", "occupation", "reason_cleaned", "admittor_1", "admittor_1_cleaned", "admittor_2","admittor_2_cleaned","obligations","sent_to_cleaned", "site","sub_site","sent_to")
bellevue_admittors_sites <- bellevue_admittors_sites[bellevue_admittors_sites_variables]

write.csv(bellevue_admittors_sites, file="bellevue_for_R.csv")

# Subset the data to keep only the entries that include "sent_to"
data <- bellevue_admittors_sites[ which(bellevue_admittors_sites$sent_to>0), ]