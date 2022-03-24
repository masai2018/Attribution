################################################################################
### demographic information for attribution for entity
################################################################################

### prerequisites
library(data.table)
source('E:/CT_APCD/Sai/2019spring/data-organization/R/utils.R')
dir_code <- input_dir('E:/CT_APCD/Sai/2019spring/attribution/')
dir.in <- input_dir('E:/CT_APCD/Sai/lds2021/lds2021file/Attribution_result/output/Double-Attribution_2019/')
dir.demo <- output_dir('E:/CT_APCD/Sai/lds2021/lds2021file/Attribution_result/output/demo_2019/')
source(paste0(dir_code, 'myDoc.R'))
source(paste0(dir_code, 'myFun.R'))


### read in files
Attri2_final <- fread(paste0(dir.in, 'Attri2_final.csv'),
                      header = TRUE, colClasses = 'character', encoding = 'UTF-8')
## elig <- fread("E:/CT_APCD/shared/intermediate_data/APCD_modified/eligibility/eligibility_patient.csv",
##             colClasses = 'character')
## elig[, age := myear - as.integer(birth_dt)]
## Attri2_final <- elig[Attri2_final, on = c('internal_member_id' = 'internal_member_id')]
## fwrite(Attri2_final, file.path(dir.in, 'Attri2_final.csv'))
Attri2_final[, age := as.integer(age)]

entityNameList <- names(Attri2_final)[9:27]
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
    ## entity <- entity[age >= 0 & age <= 64]
    # entity <- entity[age >= 0]
    ## calculate entitywise demographic
    ## pat.total <- length(unique(entity[age <= 64 & age >= 0]$internal_member_id))
    pat.total <- length(unique(entity$internal_member_id))
    ## pat.male <- length(unique(entity[age <= 64 & age >= 0 & gender_code == "M"]$internal_member_id))
    pat.male <- length(unique(entity[gender_code == "M"]$internal_member_id))
    ## pat.female <- length(unique(entity[age <= 64 & age >= 0 & gender_code == "F"]$internal_member_id))
    pat.female <- length(unique(entity[gender_code == "F"]$internal_member_id))
    pat.age0017 <- length(unique(entity[age >=0 & age <= 17]$internal_member_id))
    pat.age1834 <- length(unique(entity[age >=18 & age <= 34]$internal_member_id))
    pat.age3549 <- length(unique(entity[age >=35 & age <= 49]$internal_member_id))
    pat.age5064 <- length(unique(entity[age >=50 & age <= 64]$internal_member_id))
    # pat.age65_ <- length(unique(entity[age >= 65]$internal_member_id))
    pat.age0minus <- length(unique(entity[age < 0]$internal_member_id))
    AN_demo_pat <- rbind(AN_demo_pat,
                         data.table(
                             entityName,
                             pat.total,
                             pat.male,
                             pat.female,
                             pat.age0017,
                             pat.age1834,
                             pat.age3549,
                             pat.age5064
                             # pat.age65_,
                             # pat.age65plus,
                             # pat.age0minus
                         ))
    fwrite(AN_demo_pat,
           file.path(dir.demo, 'demo_patient.csv'))
    print(entityName)
}

