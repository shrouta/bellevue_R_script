# Some R scripts for playing around with the Bellevue Almshouse Data.

# Loads necessary libraries
install.packages("gmodels")
library(psych)
library(dplyr)
library(gmodels)

# Read in CSV file from Cleaning Bellevue Data.R
data <- read.csv(file.choose())

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

# Groups diseases according to type

dummies$disability <- dummies$blind + dummies$cripple + dummies$deaf + dummies$disabled + dummies$lame + dummies$same

dummies$fever <- dummies$fever + dummies$rever

dummies$injury <- dummies$bleeding + dummies$broken.bone + dummies$contusion + dummies$fracture + dummies$fractured.leg + dummies$injury + dummies$severed.limb + dummies$sore.limb + dummies$sprained.ankle + dummies$sprained.jaw + dummies$throat.cut

dummies$mental <- dummies$delusion.dreams + dummies$emotional + dummies$horrors + dummies$hyperchondria + dummies$insane + dummies$non.comps

dummies$social_general <- dummies$abandonment + dummies$debility + dummies$drunkenness + dummies$fits + dummies$from.trial + dummies$intemperance + dummies$old.age + dummies$poorly + dummies$ungovernable

dummies$specific <- dummies$icterus + dummies$abscess + dummies$ague + dummies$ascites + dummies$asthma + dummies$bronchitis + dummies$cognetis.cretin + dummies$colic + dummies$congested.head + dummies$consumption + dummies$diarrhea + dummies$dropsy + dummies$dysentery + dummies$erysipelas + dummies$exzema + dummies$lomehan + dummies$measles + dummies$neuralgia + dummies$ophthalmia + dummies$opthalamia + dummies$opthalia + dummies$phagadenic.ulcers + dummies$phehius + dummies$phentitis + dummies$phthisis + dummies$piles + dummies$pneumonia + dummies$pritchins + dummies$rheumanation + dummies$rheumatism + dummies$rhumanic + dummies$rhumatic + dummies$rickets + dummies$scarletina + dummies$sciatica + dummies$scrofula + dummies$sea.hove + dummies$sec.syphilis + dummies$spinal.disease + dummies$tile + dummies$trontial + dummies$tumor + dummies$typhus + dummies$typhus.fever + dummies$ulcers + dummies$ustio

# Creates a table with age categories and entry into the garret
garret_age <- xtabs(~ data.ageord + Bellevue_Garret, data = dummies)
garret_age_reason <- xtabs(~ data.ageord + Bellevue_Garret + data.reason, data = dummies)
prop.table(garret_age)
prop.table(garret_age_reason)
summary(garret_age)
summary(garret_age_reason)

# Crosstabs
CrossTable(dummies$data.reason, dummies$Bellevue_Garret, format="SPSS")
CrossTable(dummies$data.ageord, dummies$Bellevue_Garret, format="SPSS")
CrossTable(dummies$data.ageord, dummies$recent.emigrant, format="SPSS")
CrossTable(dummies$data.admittor_1_cleaned, dummies$Bellevue_Garret, format="SPSS")

# Crosstabs
CrossTable(dummies$data.reason, dummies$Bellevue_Shanty, format="SPSS")
CrossTable(dummies$data.ageord, dummies$Bellevue_Shanty, format="SPSS")
CrossTable(dummies$data.admittor_1_cleaned, dummies$Bellevue_Shanty, format="SPSS")

# Logistic regression - treating the missing category reference category - intercept stands in as log odds. Second pass includes only significant variables
sink(file="recent_emigrant_logistic_output.txt", type="output")
shanty_logit <- glm(Bellevue_Shanty ~ recent.emigrant + disability + fever + injury + mental + pregnant + sickness + social_general + specific + data.ageord, data=dummies, family="binomial")
summary(shanty_logit)
shanty_coef <- shanty_logit$coefficients
exp(shanty_coef)

chapel_logit <- glm(Bellevue_Chapel ~ recent.emigrant + disability + fever + injury + mental + pregnant + sickness + social_general + specific + data.ageord, data=dummies, family="binomial")
summary(chapel_logit)
chapel_coef <- chapel_logit$coefficients
exp(chapel_coef)

garret_logit <- glm(Bellevue_Garret ~ recent.emigrant + disability + fever + injury + mental + pregnant + sickness + social_general + specific + data.ageord, data=dummies, family="binomial")
summary(garret_logit)
garret_coef <- garret_logit$coefficients
exp(garret_coef)

almshouse_logit <- glm(Bellevue_Almshouse ~ recent.emigrant + disability + fever + injury + mental + pregnant + sickness + social_general + specific + data.ageord, data=dummies, family="binomial")
summary(almshouse_logit)
almshouse_coef <- almshouse_logit$coefficients
exp(almshouse_coef)

blackwell_logit <- glm(Blackwell_NA ~ recent.emigrant + disability + fever + injury + mental + pregnant + sickness + social_general + specific + data.ageord, data=dummies, family="binomial")
summary(blackwell_logit)
blackwell_coef <- blackwell_logit$coefficients
exp(blackwell_coef)

randall_logit <- glm(Randall_NA ~ recent.emigrant + disability + fever + injury + mental + pregnant + sickness + social_general + specific + data.ageord, data=dummies, family="binomial")
summary(randall_logit)
randall_coef <- randall_logit$coefficients
exp(randall_coef)

hospital_logit <- glm(Bellevue_Hospital ~ recent.emigrant + disability + fever + injury + mental + pregnant + sickness + social_general + specific + data.ageord, data=dummies, family="binomial")
summary(hospital_logit)
hospital_coef <- hospital_logit$coefficients
exp(hospital_coef)

lunatic_logit <- glm(Lunatic_Asylum_NA ~ recent.emigrant + disability + fever + injury + mental + pregnant + sickness + social_general + specific + data.ageord, data=dummies, family="binomial")
summary(lunatic_logit)
lunatic_coef <- lunatic_logit$coefficients
exp(lunatic_coef)

long_island_logit <- glm(Long_Island_NA ~ recent.emigrant + disability + fever + injury + mental + pregnant + sickness + social_general + specific + data.ageord, data=dummies, family="binomial")
summary(long_island_logit)
long_island_coef <- long_island_logit$coefficients
exp(long_island_coef)

shanty_admittor_logit <- glm(Bellevue_Shanty ~ a.b.roberts + a.g..schratty + abraham.r..lawrence + ald..pan + alexander.h..schultz + benson.s..hopkins + bernard.j..mersole + clarkson.crolius + commissioners.of.emigration + edward.witherell + g.w..anderson + g.w..hansen.4th.ward + george.h..purser + illegibile + j..blackwell + jacob.l..dodge + james.d..oliver + james.donnelly + james.kelly + james.walsh + john.p..cumming + leonard.l..johnson + mcgavin + morris.franklin + moses.g..leonard + moses.m..s..jackson + moses.maynard + oscar.s..field + peter.greg + police + resident.physician + richard.t..compton + superintendent + theodore.r..deforest + thomas.conner + thomas.spofford + washington.smith + william.a..walker + william.adams + william.f..jackson + william.w..fream + data.ageord, data=dummies, family="binomial")
summary(shanty_admittor_logit)
shanty_admittor_coef <- shanty_admittor_logit$coefficients
exp(shanty_admittor_coef)

chapel_logit <- glm(Bellevue_Chapel ~ a.b.roberts + a.g..schratty + abraham.r..lawrence + ald..pan + alexander.h..schultz + benson.s..hopkins + bernard.j..mersole + clarkson.crolius + commissioners.of.emigration + edward.witherell + g.w..anderson + g.w..hansen.4th.ward + george.h..purser + illegibile + j..blackwell + jacob.l..dodge + james.d..oliver + james.donnelly + james.kelly + james.walsh + john.p..cumming + leonard.l..johnson + mcgavin + morris.franklin + moses.g..leonard + moses.m..s..jackson + moses.maynard + oscar.s..field + peter.greg + police + resident.physician + richard.t..compton + superintendent + theodore.r..deforest + thomas.conner + thomas.spofford + washington.smith + william.a..walker + william.adams + william.f..jackson + william.w..fream + data.ageord, data=dummies, family="binomial")
summary(chapel_logit)
chapel_coef <- chapel_logit$coefficients
exp(chapel_coef)

garret_admittor_logit <- glm(Bellevue_Garret ~ a.b.roberts + a.g..schratty + abraham.r..lawrence + ald..pan + alexander.h..schultz + benson.s..hopkins + bernard.j..mersole + clarkson.crolius + commissioners.of.emigration + edward.witherell + g.w..anderson + g.w..hansen.4th.ward + george.h..purser + illegibile + j..blackwell + jacob.l..dodge + james.d..oliver + james.donnelly + james.kelly + james.walsh + john.p..cumming + leonard.l..johnson + mcgavin + morris.franklin + moses.g..leonard + moses.m..s..jackson + moses.maynard + oscar.s..field + peter.greg + police + resident.physician + richard.t..compton + superintendent + theodore.r..deforest + thomas.conner + thomas.spofford + washington.smith + william.a..walker + william.adams + william.f..jackson + william.w..fream + data.ageord, data=dummies, family="binomial")
summary(garret_admittor_logit)
garret_admittor_coef <- garret_admittor_logit$coefficients
exp(garret_admittor_coef)

almshouse_admittor_logit <- glm(Bellevue_Almshouse ~ a.b.roberts + a.g..schratty + abraham.r..lawrence + ald..pan + alexander.h..schultz + benson.s..hopkins + bernard.j..mersole + clarkson.crolius + commissioners.of.emigration + edward.witherell + g.w..anderson + g.w..hansen.4th.ward + george.h..purser + illegibile + j..blackwell + jacob.l..dodge + james.d..oliver + james.donnelly + james.kelly + james.walsh + john.p..cumming + leonard.l..johnson + mcgavin + morris.franklin + moses.g..leonard + moses.m..s..jackson + moses.maynard + oscar.s..field + peter.greg + police + resident.physician + richard.t..compton + superintendent + theodore.r..deforest + thomas.conner + thomas.spofford + washington.smith + william.a..walker + william.adams + william.f..jackson + william.w..fream + data.ageord, data=dummies, family="binomial")
summary(almshouse_admittor_logit)
almshouse_admittor_coef <- almshouse_admittor_logit$coefficients
exp(almshouse_admittor_coef)

blackwell_admittor_logit <- glm(Blackwell_NA ~ a.b.roberts + a.g..schratty + abraham.r..lawrence + ald..pan + alexander.h..schultz + benson.s..hopkins + bernard.j..mersole + clarkson.crolius + commissioners.of.emigration + edward.witherell + g.w..anderson + g.w..hansen.4th.ward + george.h..purser + illegibile + j..blackwell + jacob.l..dodge + james.d..oliver + james.donnelly + james.kelly + james.walsh + john.p..cumming + leonard.l..johnson + mcgavin + morris.franklin + moses.g..leonard + moses.m..s..jackson + moses.maynard + oscar.s..field + peter.greg + police + resident.physician + richard.t..compton + superintendent + theodore.r..deforest + thomas.conner + thomas.spofford + washington.smith + william.a..walker + william.adams + william.f..jackson + william.w..fream + data.ageord, data=dummies, family="binomial")
summary(blackwell_admittor_logit)
blackwell_admittor_coef <- blackwell_admittor_logit$coefficients
exp(blackwell_admittor_coef)

randall_admittor_logit <- glm(Randall_NA ~ a.b.roberts + a.g..schratty + abraham.r..lawrence + ald..pan + alexander.h..schultz + benson.s..hopkins + bernard.j..mersole + clarkson.crolius + commissioners.of.emigration + edward.witherell + g.w..anderson + g.w..hansen.4th.ward + george.h..purser + illegibile + j..blackwell + jacob.l..dodge + james.d..oliver + james.donnelly + james.kelly + james.walsh + john.p..cumming + leonard.l..johnson + mcgavin + morris.franklin + moses.g..leonard + moses.m..s..jackson + moses.maynard + oscar.s..field + peter.greg + police + resident.physician + richard.t..compton + superintendent + theodore.r..deforest + thomas.conner + thomas.spofford + washington.smith + william.a..walker + william.adams + william.f..jackson + william.w..fream + data.ageord, data=dummies, family="binomial")
summary(randall_admittor_logit)
randall_admittor_coef <- randall_admittor_logit$coefficients
exp(randall_admittor_coef)

hospital_admittor_logit <- glm(Bellevue_Hospital ~ a.b.roberts + a.g..schratty + abraham.r..lawrence + ald..pan + alexander.h..schultz + benson.s..hopkins + bernard.j..mersole + clarkson.crolius + commissioners.of.emigration + edward.witherell + g.w..anderson + g.w..hansen.4th.ward + george.h..purser + illegibile + j..blackwell + jacob.l..dodge + james.d..oliver + james.donnelly + james.kelly + james.walsh + john.p..cumming + leonard.l..johnson + mcgavin + morris.franklin + moses.g..leonard + moses.m..s..jackson + moses.maynard + oscar.s..field + peter.greg + police + resident.physician + richard.t..compton + superintendent + theodore.r..deforest + thomas.conner + thomas.spofford + washington.smith + william.a..walker + william.adams + william.f..jackson + william.w..fream + data.ageord, data=dummies, family="binomial")
summary(hospital_admittor_logit)
hospital_admittor_coef <- hospital_admittor_logit$coefficients
exp(hospital_admittor_coef)

lunatic_admittor_logit <- glm(Lunatic_Asylum_NA ~ a.b.roberts + a.g..schratty + abraham.r..lawrence + ald..pan + alexander.h..schultz + benson.s..hopkins + bernard.j..mersole + clarkson.crolius + commissioners.of.emigration + edward.witherell + g.w..anderson + g.w..hansen.4th.ward + george.h..purser + illegibile + j..blackwell + jacob.l..dodge + james.d..oliver + james.donnelly + james.kelly + james.walsh + john.p..cumming + leonard.l..johnson + mcgavin + morris.franklin + moses.g..leonard + moses.m..s..jackson + moses.maynard + oscar.s..field + peter.greg + police + resident.physician + richard.t..compton + superintendent + theodore.r..deforest + thomas.conner + thomas.spofford + washington.smith + william.a..walker + william.adams + william.f..jackson + william.w..fream + data.ageord, data=dummies, family="binomial")
summary(lunatic_admittor_logit)
lunatic_admittor_coef <- lunatic_admittor_logit$coefficients
exp(lunatic_admittor_coef)

long_island_admittor_logit <- glm(Long_Island_NA ~ a.b.roberts + a.g..schratty + abraham.r..lawrence + ald..pan + alexander.h..schultz + benson.s..hopkins + bernard.j..mersole + clarkson.crolius + commissioners.of.emigration + edward.witherell + g.w..anderson + g.w..hansen.4th.ward + george.h..purser + illegibile + j..blackwell + jacob.l..dodge + james.d..oliver + james.donnelly + james.kelly + james.walsh + john.p..cumming + leonard.l..johnson + mcgavin + morris.franklin + moses.g..leonard + moses.m..s..jackson + moses.maynard + oscar.s..field + peter.greg + police + resident.physician + richard.t..compton + superintendent + theodore.r..deforest + thomas.conner + thomas.spofford + washington.smith + william.a..walker + william.adams + william.f..jackson + william.w..fream + data.ageord, data=dummies, family="binomial")
summary(long_island_admittor_logit)
long_island_admittor_coef <- long_island_admittor_logit$coefficients
exp(long_island_admittor_coef)

recent_emigrant_site_logit <- glm(recent.emigrant ~ Bellevue_Shanty + Bellevue_Garret + Bellevue_Hospital + Long_Island_NA + Lunatic_Asylum_NA + Randall_NA + Blackwell_NA + data.ageord, data=dummies, family="binomial")
summary(recent_emigrant_site_logit)
recent_emigrant_site_coef <- recent_emigrant_site_logit$coefficients
exp(recent_emigrant_site_coef)

recent_emigrant_admittor_logit <- glm(recent.emigrant ~ a.b.roberts + a.g..schratty + abraham.r..lawrence + ald..pan + alexander.h..schultz + benson.s..hopkins + bernard.j..mersole + clarkson.crolius + commissioners.of.emigration + edward.witherell + g.w..anderson + g.w..hansen.4th.ward + george.h..purser + illegibile + j..blackwell + jacob.l..dodge + james.d..oliver + james.donnelly + james.kelly + james.walsh + john.p..cumming + leonard.l..johnson + mcgavin + morris.franklin + moses.g..leonard + moses.m..s..jackson + moses.maynard + oscar.s..field + peter.greg + police + resident.physician + richard.t..compton + superintendent + theodore.r..deforest + thomas.conner + thomas.spofford + washington.smith + william.a..walker + william.adams + william.f..jackson + william.w..fream + data.ageord, data=dummies, family="binomial")
summary(recent_emigrant_admittor_logit)
recent_emigrant_admittor_coef <- recent_emigrant_admittor_logit$coefficients
exp(recent_emigrant_admittor_coef)

destitution_admittor_logit <- glm(destitution ~ a.b.roberts + a.g..schratty + abraham.r..lawrence + ald..pan + alexander.h..schultz + benson.s..hopkins + bernard.j..mersole + clarkson.crolius + commissioners.of.emigration + edward.witherell + g.w..anderson + g.w..hansen.4th.ward + george.h..purser + illegibile + j..blackwell + jacob.l..dodge + james.d..oliver + james.donnelly + james.kelly + james.walsh + john.p..cumming + leonard.l..johnson + mcgavin + morris.franklin + moses.g..leonard + moses.m..s..jackson + moses.maynard + oscar.s..field + peter.greg + police + resident.physician + richard.t..compton + superintendent + theodore.r..deforest + thomas.conner + thomas.spofford + washington.smith + william.a..walker + william.adams + william.f..jackson + william.w..fream + data.ageord, data=dummies, family="binomial")
summary(destitution_admittor_logit)
destitution_admittor_coef <- destitution_admittor_logit$coefficients
exp(destitution_admittor_coef)
sink()
