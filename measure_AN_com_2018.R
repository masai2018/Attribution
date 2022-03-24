################################################################################
### entitywise measure calculation
### version controlled by git
################################################################################

## prerequisites
library(methods)
source("E:/CT_APCD/gitrepo/ct-apcd/R/utils.R")
need_pkgs <- c("data.table", "bit64", "tools", "dplyr",
               "lubridate", "ggplot2", "microbenchmark",
               "tidyverse", "zoo")
need.packages(need_pkgs)
source('E:/CT_APCD/Sai/2019spring/data-organization/R/utils.R')
dir_code <- input_dir('E:/CT_APCD/Sai/lds2021/lds2021code/Attribution_2018_2019/')
dir.in <- input_dir('E:/CT_APCD/Sai/lds2021/lds2021file/Attribution_result/output/Double-Attribution_2018/')
# dir.out <- 'E:/CT_APCD/Hongfei/output/measure_AN/'
dir.out <- output_dir('E:/CT_APCD/Sai/lds2021/lds2021file/Attribution_result/output/measure_AN_2018/detail/')
dir.measure <- input_dir('E:/CT_APCD/Sai/lds2021/lds2021file/Attribution_result/measure_in/')
## nppes <- 'E:/CT_APCD/Hongfei/files/nppes0.csv'
source(paste0(dir_code, 'myDoc.R'))
source(paste0(dir_code, 'myFun.R'))

## read in pre files.
yr <- 2018
Attri2_final <- fread(paste0(dir.in, 'Attri2_final.csv'),
                      header = TRUE, colClasses = 'character', encoding = 'UTF-8')
## Attri2_toAN <- fread(paste0(dir.in, 'Attri2_toAN.csv'),
##                      header = TRUE, colClasses = 'character', encoding = 'UTF-8')
dim(Attri2_final)
table(Attri2_final$Total)
mc.patient <- rbindlist(lapply(paste0("E:/CT_APCD/Sai/lds2021/lds2021file/medicare_medicare_ad_patients/medicare_medicare_ad_patients_", (yr - 1):yr, ".csv"),
                               function(x){
                                 fread(x, colClasses = "character")
                               })) %>% unique()
names(mc.patient)
entityNameList <- names(Attri2_final)[9:27]
measurelist <- paste0(yr, 
                      c(NULL,
                        # 'BCS',
                        # 'CCS',
                        # "CHL",
                        # "NCS",
                        # "AWC",
                        # "ImAdo1",
                        # "ImAdo2",
                        # "ImAdo3",
                        # "ImAdo4",
                        # 'FUH_30d',
                        # 'FUH_7d',
                        # "IET_opioid_Enga", "IET_opioid_IESD", "IET_opioid_Init", "IET_opioid_Neg",
                        # "IET_drug_Enga", "IET_drug_IESD", "IET_drug_Init", "IET_drug_Neg",
                        # "IET_alcohol_Enga", "IET_alcohol_IESD", "IET_alcohol_Init", "IET_alcohol_Neg",
                        # "IET_Enga", "IET_IESD", "IET_Init", "IET_Neg",
                        # "LBP",
                        # "Nephropathy",
                        # "HbA1c",
                        "Diabetes",
                        NULL))


ANlist <- entityNameList
FQHClist <- NULL
Attri2_final[, `:=`(Att_InAN = 0,
                    # Att_InFQHC = 0,
                    Att_OutAN = 0
                    # Att_OutFQHC = 0,
                    )]
Attri2_final[, (entityNameList) := lapply(.SD, as.numeric), 
             .SDcols = entityNameList]
Attri2_final[, Att_InAN := rowSums(.SD, na.rm = TRUE),
             .SDcols = ANlist]
# Attri2_final[, Att_InFQHC := rowSums(.SD, na.rm = TRUE),
#              .SDcols = FQHClist]
Attri2_final[!is.na(Total) & Att_InAN == 0, Att_OutAN := 1]
# Attri2_final[!is.na(Total) & Att_InFQHC == 0, Att_OutFQHC := 1]
elig.patient <- fread("E:/CT_APCD/Beth/data6/Eligibility/By_Calendar_Year/MEDICAL_ELIGIBILITY_ALLRECORDS14_19_6b.csv", 
                      select = c("INTERNAL_MEMBER_ID",
                                 "GENDER_CODE",
                                 "birth_dt"),
                      colClasses = "character") %>% unique()
elig.patient[, `:=`(birth_year = year(as.yearmon(birth_dt, "%Y%m")))]
elig.patient[, age:= yr - as.integer(birth_year)]
elig.patient <- elig.patient[age < 65]
names(elig.patient) <- tolower(names(elig.patient))
elig.patient <- elig.patient[, .(internal_member_id, gender_code, age)] %>%
  unique(use.key = FALSE)

for (measureName in measurelist){
  measure <- fread(paste0(dir.measure, measureName, '_summary.csv'),
                   header = TRUE, colClasses = 'character', encoding = 'UTF-8')
  colnames(measure) <- tolower(colnames(measure))
  measure <- measure[!internal_member_id %in% mc.patient$internal_member_id]
  ## subset infomation ##
  ## measure_AN <- Attri2valid[measure, on = 'internal_member_id']
  measure_AN_demo <- Attri2_final[measure, on = c('internal_member_id')]
  measure_AN_demo <- measure_AN_demo[, -c("birth_dt",
                                          "gender_code",
                                          "age")]
  measure_AN_demo <- elig.patient[measure_AN_demo, on = "internal_member_id"]
  measure_AN_demo[, age := as.integer(age)]
  measure_AN_demo <- measure_AN_demo[age >= 0]
  ## measure_AN_demo <- elig[measure_AN, on = c('internal_member_id' = 'internal_member_id')] ## merge patient GENDER, age info
  measure_AN_valid <- measure_AN_demo[!Total == "" & !is.na(Total)]
  overall_rate <- dim(measure[num == '1'])[1] / dim(measure[denom == '1'])[1]
  overall_AN_rate <- dim(measure_AN_valid[num == '1'])[1] / dim(measure_AN_valid[denom == '1'])[1]
  if (measureName == measurelist[1]) {
    info.measure <- data.table(matrix(c(measureName, dim(measure)[1], dim(measure_AN_valid)[1], overall_rate, overall_AN_rate), nrow = 1))
  } else info.measure <- rbind(info.measure, data.table(matrix(c(measureName, dim(measure)[1], dim(measure_AN_valid)[1], overall_rate, overall_AN_rate), nrow = 1)))
  ## for each entity ##
  ANrate.out <- data.table()
  AN_demo_summary <- data.table()
  for (entityName in c(entityNameList, 
                       # 'Att_InANFQHC',
                       # 'Att_OutANFQHC', 
                       "Att_InAN",
                       # "Att_InFQHC",
                       "Att_OutAN",
                       # "Att_OutFQHC",
                       'Attributed', 'Unattributed', 'Overall')){
    if (entityName %in% ANlist){
      entityType <- "AN"
    } else if (entityName %in% FQHClist) {
      entityType <- "FQHC"
    } else {     
      entityType <- "summary"
    }
      if (entityName %in% entityNameList){
      eval(parse(text = paste0('measure_entity <- measure_AN_valid[`', entityName, '` == "1"]')))
    } else if (entityName == 'Att_OutANFQHC'){
      measure_entity <- measure_AN_demo[Total == ""]
    } else if (entityName == 'Att_InANFQHC'){
      measure_entity <- measure_AN_demo[!Total == "" & !is.na(Total)]
    } else if (entityName == 'Attributed'){
      measure_entity <- measure_AN_demo[!is.na(Total)]
    } else if (entityName == 'Unattributed'){
      measure_entity <- measure_AN_demo[is.na(Total)]
    } else if (entityName == 'Overall'){
      measure_entity <- measure_AN_demo
    } else if (entityName == "Att_InAN") {
      measure_entity <- measure_AN_demo[Att_InAN > 0]
    } else if (entityName == "Att_InFQHC") {
      measure_entity <- measure_AN_demo[Att_InFQHC > 0]
    } else if (entityName == "Att_OutAN") {
      measure_entity <- measure_AN_demo[Att_OutAN > 0]
    } else if (entityName == "Att_OutFQHC") {
      measure_entity <- measure_AN_demo[Att_OutFQHC > 0]
    }
    ## calculate entitywise rate
    entity_num <- measure_entity[num == '1']
    entity_denom <- measure_entity[denom == '1']
    entity_rate <- dim(entity_num)[1] / dim(entity_denom)[1]
    ## if (dim(entity_denom)[1] >= 30){
    ##     CI_95lb <- entity_rate - qnorm(.975) * sqrt(entity_rate * (1 - entity_rate) / dim(entity_denom)[1])
    ##     CI_95rb <- entity_rate + qnorm(.975) * sqrt(entity_rate * (1 - entity_rate) / dim(entity_denom)[1])
    ##     CI_90lb <- entity_rate - qnorm(.95) * sqrt(entity_rate * (1 - entity_rate) / dim(entity_denom)[1])
    ##     CI_90rb <- entity_rate + qnorm(.95) * sqrt(entity_rate * (1 - entity_rate) / dim(entity_denom)[1])
    ## } else CI_95lb <- CI_95rb <- CI_90lb <- CI_90rb <- '*'
    if (measureName %in% c('2018FUH_30d',
                           '2018FUH_7d')){
      ANrate.out <- rbind(
        ANrate.out,
        data.table(matrix(c(
          measureName, entityName, entityType, 
          dim(entity_num)[1], 
          dim(entity_denom)[1], 
          dim(entity_num)[1]/dim(entity_denom)[1]#,
          ## CI_95lb, CI_95rb, CI_90lb, CI_90rb# , overall_rate, overall_AN_rate
        ), nrow = 1)))
    }else{
      ANrate.out <- rbind(
        ANrate.out,
        data.table(matrix(c(
          measureName, entityName, entityType, 
          uniqueN(entity_num$internal_member_id), 
          uniqueN(entity_denom$internal_member_id), 
          uniqueN(entity_num$internal_member_id)/uniqueN(entity_denom$internal_member_id)#,
          ## CI_95lb, CI_95rb, CI_90lb, CI_90rb# , overall_rate, overall_AN_rate
        ), nrow = 1)))
    }
    ## calculate entitywise demographic
    pat.total <- length(unique(entity_denom$internal_member_id))
    pat.male <- length(unique(entity_denom[gender_code == "M"]$internal_member_id))
    pat.female <- length(unique(entity_denom[gender_code == "F"]$internal_member_id))
    pat.age0017 <- length(unique(entity_denom[age >=0 & age <= 17]$internal_member_id))
    pat.age1834 <- length(unique(entity_denom[age >=18 & age <= 34]$internal_member_id))
    pat.age3549 <- length(unique(entity_denom[age >=35 & age <= 49]$internal_member_id))
    pat.age5064 <- length(unique(entity_denom[age >=50 & age <= 64]$internal_member_id))
    # pat.age65_ <- length(unique(entity_denom[age >= 65]$internal_member_id))
    if (entityName %in% c(entityNameList, 'Att_InANFQHC',
                          "Att_InAN", "Att_InFQHC", "Att_OutAN", "Att_OutFQHC")){
      pvd <- unique(entity_denom[, .(NPI, Taxonomy1)])
      pvd.total <- length(unique(entity_denom$NPI))
      pvd.NP <- length(pvd[Taxonomy1 %in% taxo_NP]$NPI)
      pvd.PA <- length(pvd[Taxonomy1 %in% taxo_PA]$NPI)
      pvd.PCP <- length(pvd[Taxonomy1 %in% taxo_PCP]$NPI)
      pvd.PedM <- length(pvd[Taxonomy1 %in% taxo_PedM]$NPI)
      pvd.ObGyn <- length(pvd[Taxonomy1 %in% taxo_OBGYN]$NPI)
    } else {
      pvd.total <- pvd.NP <- pvd.PA <- pvd.PCP <- pvd.PedM <- pvd.ObGyn <- NA
    }
    AN_demo_summary <- rbind(
      AN_demo_summary,
      data.table(
        entityName,
        entityType,
        pvd.total,
        pvd.NP,
        pvd.PA,
        pvd.PCP,
        pvd.PedM,
        pvd.ObGyn,
        pat.total,
        pat.male,
        pat.female,
        pat.age0017,
        pat.age1834,
        pat.age3549,
        pat.age5064
        # pat.age65_
      )
    )
  }
  colnames(ANrate.out) <- c(
    'Measure', 'Entity', "entityType", 'num', 'denom', 'Rate'## ,
    ## 'CI_95lb', 'CI_95rb', 'CI_90lb', 'CI_90rb'
  )# , 'OverallRate', 'OverallANRate')
  # fwrite(ANrate.out, paste0(dir.out, 'detail/', measureName, '.csv'))
  setcolorder(ANrate.out, c('Measure', "entityType", 'Entity',  
                            'num', 'denom', 'Rate'))
  fwrite(ANrate.out, paste0(dir.out, measureName, '.csv'))
  cat(measureName, '\n')
  # fwrite(AN_demo_summary, paste0(dir.out, 'detail/', measureName, '_demo.csv'))
  fwrite(AN_demo_summary, paste0(dir.out, measureName, '_demo.csv'))
  
}
# colnames(info.measure) <- c('MeasureName', 'denom_pat_total', 'num_pat_AN', 'rate_total', 'rate_AN')
# ## colnames(AN_demo_summary) <- c(
# ##     "pvd.total",
# ##     "pvd.NP",
# ##     "pvd.PA",
# ##     "pvd.PCP",
# ##     "pvd.PedM",
# ##     "pvd.ObGyn",
# ##     "pat.total",
# ##     "pat.male",
# ##     "pat.female",
# ##     "pat.age0017",
# ##     "pat.age1834",
# ##     "pat.age3549",
# ##     "pat.age5064"
# ## )
# fwrite(info.measure, paste0(dir.out, 'summary_measure.csv'))
