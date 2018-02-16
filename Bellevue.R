# Some R scripts for playing around with the Bellevue Almshouse Data.

# Loads necessary libraries
library("psych")
library("dplyr")

# Call Data Sets - admittors_biography, admittors, incarceration, inmates, medical_terms, site_codes, site_description
incarceration <- read_csv("~/Dropbox (Personal)/Publications/In Progress/Digital Almshouse/Digital almshouse dataset/R scripts/Bellevue Analysis/incarceration.csv")

inmates <- read_csv("~/Dropbox (Personal)/Publications/In Progress/Digital Almshouse/Digital almshouse dataset/R scripts/Bellevue Analysis/inmates.csv")

site_codes <- read_csv("~/Dropbox (Personal)/Publications/In Progress/Digital Almshouse/Digital almshouse dataset/R scripts/Bellevue Analysis/site_codes.csv")

site_description <- read_csv("~/Dropbox (Personal)/Publications/In Progress/Digital Almshouse/Digital almshouse dataset/R scripts/Bellevue Analysis/site_description.csv")

# Synthesize dataset for analysis
metadata <- c("first_name", "last_name", "age", "ageord", "profession", "gender","disease", "site_general_description")
data <- merge(inmates, incarceration, by="incarceration_key")
data <- merge(data, site_codes, by.x="sent_to", by.y="sent_to_cleaned", all=FALSE)
data <- merge(data, site_description, by="public_health_site_code", all=FALSE)

# Create age categories
data$ageord <- cut(data$age, c(0,3,10,18,30,100), labels = c(1:5))
data$ageord <- as.integer(data$ageord)

# Cull data
data <- as.data.frame(data)
data <- data[metadata]

# Creates a dummy variable for the attribute I am interested in, and merges or subsets categories if needed.
sent_to_dummy <- as.data.frame(dummy.code(data$site_general_description))
disease_dummy <- as.data.frame(dummy.code(data$disease))
gender_dummy <- as.data.frame(dummy.code(data$gender))

# Cleans gender_dummy
gender_dummy$f <- gender_dummy$f + gender_dummy$g + gender_dummy$h

# Creates xtab
xtab <- data.frame(data$first_name, data$last_name, data$ageord, data$profession, data$gender, gender_dummy$f, gender_dummy$m, data$disease, disease_dummy, data$site_general_description, sent_to_dummy)

# Creates a lumping dummy variable for the Shanty
xtab$shanty_other <- xtab$abandonment + xtab$abscess + xtab$ague + xtab$ascites + xtab$asthma + xtab$bleeding + xtab$blind + xtab$broken.bone + xtab$bronchitis + xtab$bruise + xtab$burn + xtab$colic + xtab$congested.head + xtab$contusion + xtab$cripple + xtab$deaf + xtab$debility + xtab$del.femur + xtab$delusion.dreams + xtab$diarrhea + xtab$disabled + xtab$dropsy + xtab$drunkenness + xtab$dysentery + xtab$eczema + xtab$emotional + xtab$erysipelas + xtab$fits + xtab$from.trial + xtab$horrors + xtab$hypochondria + xtab$injuries + xtab$illegible + xtab$insane + xtab$intemperance + xtab$jaundice + xtab$lame + xtab$measles + xtab$neuralgia + xtab$old.age + xtab$ophthalmia + xtab$paralysis + xtab$phagadaena + xtab$phthisis + xtab$piles + xtab$poorly + xtab$pregnant + xtab$rheumatism + xtab$rickets + xtab$scarletina + xtab$scrofula + xtab$seizure + xtab$severed.limb + xtab$sore + xtab$spinal.disease + xtab$sprain + xtab$syphilis + xtab$throat.cut + xtab$tuberculosis + xtab$tumor + xtab$typhus + xtab$ulcers + xtab$ungovernable + xtab$vagrant

# Creates a table with age categories and entry into the Shanty
shanty_age <- xtabs(~ data.ageord + Shanty, data = xtab)
shanty_age_disease <- xtabs(~ data.ageord + Shanty + data.disease, data = xtab)
prop.table(shanty_age)
prop.table(shanty_age_disease)
summary(shanty_age_disease)

# Creates a crosstab in the style of SPSS, to determine (a) which diseases seem highly correlated with the Shanty and (b) which diseases might be productively lumped
CrossTable(xtab$data.disease, xtab$Shanty, format="SPSS")

# Creates a crosstab in the style of SPSS, to determine whether particular age cohorts seem correlated with the Shanty
CrossTable(xtab$data.ageord, xtab$Shanty, format="SPSS")

# Logistic regression - treating the diagnosis recent emigrant as my reference category
shanty_disease_logit <- glm(Shanty ~ shanty_other + sickness + destitution + fever + data.ageord, data=xtab, family="binomial")
summary(shanty_disease_logit)

# Coefficients and log odds
shanty_disease_coefficients <- shanty_disease_logit$coefficients

exp(shanty_disease_coefficients)
