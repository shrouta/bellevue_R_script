# Some R scripts for playing around with the Bellevue Almshouse Data.

# Loads necessary libraries
library("psych")
library("dplyr")
library("gmodels")


# Create age categories
data$ageord <- cut(data$age_standard, c(0,3,10,18,30,100), labels = c(0:4))
data$ageord <- as.integer(data$ageord)

# Creates a dummy variable for the attribute I am interested in, and merges or subsets categories if needed.
sent_to_dummy <- as.data.frame(dummy.code(data$sent_to))
reason_dummy <- as.data.frame(dummy.code(data$reason_cleaned))
gender_dummy <- as.data.frame(dummy.code(data$gender))
by_whom_sent_dummy <- as.data.frame(dummy.code(data$admittor_1_cleaned))

# Creates xtab
dummies <- data.frame(data$ageord, data$occupation, data$gender, gender_dummy$f, gender_dummy$m, data$reason, data$admittor_1_cleaned, by_whom_sent_dummy, reason_dummy, data$sent_to, sent_to_dummy)

dummies$specific <- dummies$rheumatism + dummies$consumption + dummies$ophthalmia + dummies$rheumanation + dummies$dysentery + dummies$typhus + dummies$typhus.fever + dummies$rhumatic + dummies$erysipelas + dummies$measles + dummies$sciatica + dummies$bronchitis + dummies$scrofula + dummies$rickets + dummies$syphilis + dummies$ulcers + dummies$piles + dummies$rhumanic + dummies$phthisis + dummies$sec.syphilis + dummies$dropsy + dummies$phehius + dummies$trontial + dummies$ustio + dummies$pneumonia + dummies$ague + dummies$exzema + dummies$asthma + dummies$hyperchondria + dummies$abscess + dummies$colic + dummies$opthalamia + dummies$icterus + dummies$diarrhea + dummies$phagadenic.ulcers + dummies$neuralgia + dummies$ascites + dummies$cognetis.cretin + dummies$phentitis + dummies$tumor + dummies$pritchins + dummies$rever + dummies$scarletina + dummies$opthalia + dummies$congested.head + dummies$sea.hove + dummies$coup.de.soleil + dummies$small.pox + dummies$spinal.disease

dummies$injuries <- dummies$lame + dummies$same + dummies$sore.limb + dummies$injury + dummies$spinal.disease + dummies$cripple + dummies$contusion + dummies$throat.cut + dummies$fracture + dummies$sprained.ankle + dummies$fractured.leg + dummies$broken.bone + dummies$severed.limb + dummies$sprained.jaw + dummies$bleeding

dummies$mental <- dummies$fits + dummies$ungovernable + dummies$emotional + dummies$delusion.dreams + dummies$horrors + dummies$insane

# Creates a table with age categories and entry into the Shanty
shanty_age <- xtabs(~ data.ageord + Shanty, data = dummies)
shanty_age_disease <- xtabs(~ data.ageord + Shanty + data.disease, data = xtab)
prop.tablsue(shanty_age)
prop.table(shanty_age_disease)
summary(shanty_age_disease)

# Crosstabs
CrossTable(dummies$data.disease, dummies$Shanty, format="SPSS")
CrossTable(dummies$data.ageord, dummies$Shanty, format="SPSS")
CrossTable(dummies$data.ageord, dummies$recent.emigrant, format="SPSS")
CrossTable(dummies$data.by_whom_sent2, dummies$Shanty, format="SPSS")

# Logistic regression - treating the missing category reference category - intercept stands in as log odds
shanty_logit <- glm(Shanty ~ disease_other + sickness + destitution + fever + data.ageord, data=dummies, family="binomial")
summary(shanty_logit)
shanty_coef <- shanty_logit$coefficients
exp(shanty_coef)

garret_logit <- glm(Garret ~ disease_other + sickness + destitution + fever + data.ageord, data=dummies, family="binomial")
summary(garret_logit)
garret_coef <- garret_logit$coefficients
exp(garret_coef)

almshouse_logit <- glm(Bellevue.Almshouse ~ disease_other + sickness + destitution + fever + data.ageord, data=dummies, family="binomial")
summary(almshouse_logit)
almshouse_coef <- almshouse_logit$coefficients
exp(almshouse_coef)

blackwell_logit <- glm(Blackwell.s.Island ~ disease_other + sickness + destitution + fever + data.ageord, data=dummies, family="binomial")
summary(blackwell_logit)
blackwell_coef <- blackwell_logit$coefficients
exp(blackwell_coef)

randall_logit <- glm(Randall.s.Island ~ disease_other + sickness + destitution + fever + data.ageord, data=dummies, family="binomial")
summary(randall_logit)
randall_coef <- randall_logit$coefficients
exp(randall_coef)

hospital_logit <- glm(Hospital ~ disease_other + sickness + destitution + fever + data.ageord, data=dummies, family="binomial")
summary(hospital_logit)
hospital_coef <- hospital_logit$coefficients
exp(hospital_coef)

recent_emigrant_logit <- glm(recent.emigrant ~ Shanty + Garret + Blackwell.s.Island + Randall.s.Island + Hospital + Long.Island + Lunatic.Asylum + data.ageord, data=dummies, family="binomial")
summary(recent_emigrant_logit)
recent_emigrant_coef <- recent_emigrant_logit$coefficients
exp(recent_emigrant_coef)
