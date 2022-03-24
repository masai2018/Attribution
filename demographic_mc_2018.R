################################################################################
### demographic information for attribution for entity
################################################################################

### prerequisites
library(data.table)
source('E:/CT_APCD/Sai/2019spring/data-organization/R/utils.R')
dir_code <- input_dir('E:/CT_APCD/Sai/2019spring/attribution/')
dir.in <- input_dir('E:/CT_APCD/Sai/Attribution_2018_2019/output/Double-Attribution_mc_2018/')
dir.demo <- output_dir('E:/CT_APCD/Sai/Attribution_2018_2019/output/demo_mc_2018/')
source(paste0(dir_code, 'myDoc.R'))
source(paste0(dir_code, 'myFun.R'))


### read in files
Attri2_final <- fread(paste0(dir.in, 'Attri2_final.csv'),
                      header = TRUE, colClasses = 'character', encoding = 'UTF-8')
## elig <- fread("E:/CT_APCD/shared/intermediate_data/APCD_modified/eligibility/eligibility_patient.csv",
##             colClasses = 'character')
## elig[, Age := myear - as.integer(birth_dt)]
## Attri2_final <- elig[Attri2_final, on = c('INTERNAL_MEMBER_ID' = 'internal_member_id')]
## fwrite(Attri2_final, file.path(dir.in, 'Attri2_final.csv'))
Attri2_final[, Age := as.integer(Age)]

entityNameList <- names(Attri2_final)[10:43]
AN_demo_pat <- data.table()
for (entityName in c(entityNameList, 'outAN', "toAN")){
    if (entityName %in% entityNameList){
        eval(parse(text = paste0(
                       'entity <- Attri2_final[', entityName, ' == "1"]'
                   )))
    } else if (entityName == 'outAN'){
        entity <- Attri2_final[Total == ""]
    } else if (entityName == 'toAN'){
        entity <- Attri2_final[Total == "1" |
                                 Total == "2" |
                               Total == "3" |
                                 Total == "4" |
                                 Total == "5" |
                                 Total == "6"]
    }
    ## entity <- entity[Age >= 0 & Age <= 64]
    # entity <- entity[Age >= 0]
    ## calculate entitywise demographic
    ## pat.total <- length(unique(entity[Age <= 64 & Age >= 0]$INTERNAL_MEMBER_ID))
    pat.total <- length(unique(entity$INTERNAL_MEMBER_ID))
    ## pat.male <- length(unique(entity[Age <= 64 & Age >= 0 & GENDER_CODE == "M"]$INTERNAL_MEMBER_ID))
    pat.male <- length(unique(entity[GENDER_CODE == "M"]$INTERNAL_MEMBER_ID))
    ## pat.female <- length(unique(entity[Age <= 64 & Age >= 0 & GENDER_CODE == "F"]$INTERNAL_MEMBER_ID))
    pat.female <- length(unique(entity[GENDER_CODE == "F"]$INTERNAL_MEMBER_ID))
    pat.age0017 <- length(unique(entity[Age >=0 & Age <= 17]$INTERNAL_MEMBER_ID))
    pat.age1834 <- length(unique(entity[Age >=18 & Age <= 34]$INTERNAL_MEMBER_ID))
    pat.age3549 <- length(unique(entity[Age >=35 & Age <= 49]$INTERNAL_MEMBER_ID))
    pat.age5064 <- length(unique(entity[Age >=50 & Age <= 64]$INTERNAL_MEMBER_ID))
    pat.age65_ <- length(unique(entity[Age >= 65]$INTERNAL_MEMBER_ID))
    pat.age0minus <- length(unique(entity[Age < 0]$INTERNAL_MEMBER_ID))
    AN_demo_pat <- rbind(AN_demo_pat,
                         data.table(
                             entityName,
                             pat.total,
                             pat.male,
                             pat.female,
                             pat.age0017,
                             pat.age1834,
                             pat.age3549,
                             pat.age5064,
                             pat.age65_#,
                             # pat.age65plus,
                             # pat.age0minus
                         ))
    fwrite(AN_demo_pat,
           file.path(dir.demo, 'demo_patient.csv'))
    print(entityName)
}

