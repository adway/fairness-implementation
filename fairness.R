library(tidyverse)
library(tidymodels)
library(fauxnaif)
library(fairness)

nsduh <- read_tsv("data/NSDUH_2018_Tab.tsv")
youth <- nsduh %>% 
  subset(CATAG2 == 1)

set.seed(331)

# Training and testing data
youth_split <- initial_split(youth)
youth_train <- training(youth_split)
youth_test <- testing(youth_split)

#Recipe for data
youth_rec <- recipe(youth_train)
youth_rec <- youth_rec %>%
  step_mutate(
    # Health Recode
    health = HEALTH, 
    health = na_if_in(health, 94, 98),
    RSKYFQDGR = na_if_in(RSKYFQDGR, 85, 94, 97, 98),
    RSKYFQTES = na_if_in(RSKYFQTES, 85, 94, 97, 98),
    risk_taking = RSKYFQDGR + RSKYFQTES,
    peer_drug_use = STNDSCIG + STNDSMJ + STNDDNK + STNDALC,
    parenting_style = PARCHKHW + PARHLPHW + PRCHORE2 + PRLMTTV2 + PARLMTSN + PRGDJOB2 + PRPROUD2 + ARGUPAR + PRTALK3,
    school_environment = SCHFELT + TCHGJOB,
    grades = AVGGRADE,
    perc_approval_by_close_friends = FRDPCIG2 + FRDMEVR2 + FRDMJMON + FRDADLY2,
    parental_approval = PRPKCIG2 + PRMJEVR2 + PRMJMO + PRALDLY2,
    inherent_attitudes = YFLPKCG2 + YFLMJMO + YFLTMRJ2 + YFLADLY2,
    violent_behavior = YOFIGHT2 + YOGRPFT2 + YOHGUN2 + YOSTOLE2 + YOSELL2 + YOATTAK2,
    religiosity = RLGATTD + RLGIMPT + RLGDCSN + RLGFRND,
    monthly_risk_mj_coc = GRSKCOCMON + GRSKMRJMON,
    weekly_risk = GRSKBNGWK + GRSKCOCWK + GRSKHERWK + GRSKLSDWK + GRSKMRJWK,
    lifetime_risk = GRSKHERTRY + GRSKLSDTRY,
    daily_risk = GRSKCIGPKD + GRSKBNGDLY,
    drug_easy = DIFOBTCOC + DIFOBTCRK + DIFOBTHER + DIFOBTLSD + DIFOBTMRJ,
    drug_education = ANYEDUC3,
    outside_drug_education = DRPRVME3,
    support_system = TALKPROB,
    extracurricular_activities = YTHACT2,
    mde = YMDELT,
    sex = IRSEX,
    race = NEWRACE2,
    poverty = POVERTY3,
    approached = APPDRGMON2,
    govt_prog = GOVTPROG,
    county = COUTYP4,
    insured = IRINSUR4,
    bmi = BMI2,
    # state_med_mj = MEDMJST2,
    father = IFATHER,
    mother = IMOTHER,
    family_size = IRHHSIZ2,
    role = "predictor"
  ) %>%
  step_mutate(SUD = UDPYILAL, role = "outcome") %>%
  step_impute_knn(all_predictors(), neighbors = 3)

youth_rec
  

youth_recepie_trained <- prep(youth_rec)
imputed <- bake(youth_recepie_trained, youth_train)
