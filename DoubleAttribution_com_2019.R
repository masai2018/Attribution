################################################################################
#### merge attributions (patientAttribution & provider attribution)
################################################################################

### prerequisite
library(data.table)
library(lubridate)
library(tidyverse)
library(zoo)## server <- (Sys.info()[4] == 'LAZ-DEID1') ## logical value
## if (!server) stop('please run in LAZ-DEID1 server!')
source('E:/CT_APCD/Sai/2019spring/data-organization/R/utils.R')
dir_code <- input_dir('E:/CT_APCD/Sai/lds2021/lds2021code/Attribution_2018_2019/')
dir.out <- output_dir('E:/CT_APCD/Sai/lds2021/lds2021file/Attribution_result/output/Double-Attribution_2019/')
dir.in.patAttri <- input_dir('E:/CT_APCD/Sai/lds2021/lds2021file/Attribution_result/InProcess_2019/')
dir.in.NPIAttri <- input_dir("E:/CT_APCD/Sai/lds2021/lds2021code/Attribution_2018_2019/NPI-Attribution/")
dir.demo <- output_dir('E:/CT_APCD/Sai/lds2021/lds2021file/Attribution_result/output/demo_2019/')
source(file.path(dir_code, "myDoc.R"))

date_attribution <- Sys.Date()
overlap <- TRUE # TRUE: consider overlap between ANs; or FALSE: consider only NPI who serves for unique AN.

## start from here ##
patAttri <- fread(paste0(dir.in.patAttri, 'PatAttri_detail.csv'),
                  header = TRUE, colClasses = 'character', encoding = 'UTF-8')

uniqueN(patAttri$internal_member_id) # 642219
patAttriValid <- patAttri[!Identify %in% c('Unattributed', '')] # '' refers to Tie
uniqueN(patAttriValid$internal_member_id) #634209
uniqueN(patAttri[Identify %in% c('Unattributed')]$internal_member_id) #39755
uniqueN(patAttri[Identify %in% c('')]$internal_member_id) #689
npiAttri <- fread(paste0(dir.in.NPIAttri, 'NPI_WholeSet_2019_com.csv'),
                  header = TRUE, colClasses = 'character', encoding = 'UTF-8')
if (overlap) {
  npiAttrivalid = npiAttri
} else npiAttrivalid <- npiAttri[Total == 1] # if consider only NPI who is assigned to unique AN.

# Attri2 <- npiAttrivalid[patAttri[, -c("Specialty", "Taxonomy1")], on = 'NPI']
Attri2 <- npiAttrivalid[patAttri, on = 'NPI']
uniqueN(Attri2$internal_member_id)  #642219
Attri2valid <- Attri2[!is.na(Total)]
uniqueN(Attri2valid$internal_member_id) # 351734

fwrite(Attri2, paste0(dir.out, 'Attri2.csv'))
fwrite(Attri2valid, paste0(dir.out, 'Attri2valid.csv'))
## Attri2valid <- fread(file.path(dir.out, "Attri2valid.csv"), colClasses = 'character')


entityNameList <- names(npiAttri)[4:22]
### provider info (only include those providers who are assigned to at least 1 patient)
npiAttrivalid <- unique(
  Attri2valid[, Npat := .N, by = NPI][, eval(
    parse(
      text = paste0('.(NPI, Taxonomy1, ', paste0(entityNameList, collapse = ", "), ', Npat)')
    )
  )]
)
uniqueN(npiAttrivalid$NPI) # 2602 providers are attributed to at least 1 patients
fwrite(npiAttrivalid, file.path(dir.out, 'npiAttrivalid.csv'))

### NPI tables
npitable <- data.table(entityName = entityNameList,
                       provider.total = apply(apply(npiAttri[, eval(parse(text = paste0('.(', paste0(entityNameList, collapse = ", "), ')')))],
                                                    2, as.numeric), 2, sum),
                       provider.PCP = apply(apply(npiAttri[Taxonomy1 %in% taxo_PCP,
                                                           eval(parse(text = paste0('.(', paste0(entityNameList, collapse = ", "), ')')))],
                                                  2, as.numeric), 2, sum),
                       provider.ObGyn = apply(apply(npiAttri[Taxonomy1 %in% taxo_OBGYN,
                                                             eval(parse(text = paste0('.(', paste0(entityNameList, collapse = ", "), ')')))],
                                                    2, as.numeric), 2, sum),
                       provider.NP = apply(apply(npiAttri[Taxonomy1 %in% taxo_NP,
                                                          eval(parse(text = paste0('.(', paste0(entityNameList, collapse = ", "), ')')))],
                                                 2, as.numeric), 2, sum),
                       provier.PA = apply(apply(npiAttri[Taxonomy1 %in% taxo_PA,
                                                         eval(parse(text = paste0('.(', paste0(entityNameList, collapse = ", "), ')')))],
                                                2, as.numeric), 2, sum),
                       provider.PedM = apply(apply(npiAttri[Taxonomy1 %in% taxo_PedM,
                                                            eval(parse(text = paste0('.(', paste0(entityNameList, collapse = ", "), ')')))],
                                                   2, as.numeric), 2, sum),
                       provider.ineffect.total = apply(apply(npiAttrivalid[, eval(parse(text = paste0('.(', paste0(entityNameList, collapse = ", "), ')')))],
                                                             2, as.numeric), 2, sum),
                       provider.ineffect.PCP = apply(apply(npiAttrivalid[Taxonomy1 %in% taxo_PCP,
                                                                         eval(parse(text = paste0('.(', paste0(entityNameList, collapse = ", "), ')')))],
                                                           2, as.numeric), 2, sum),
                       provider.ineffect.ObGyn = apply(apply(npiAttrivalid[Taxonomy1 %in% taxo_OBGYN,
                                                                           eval(parse(text = paste0('.(', paste0(entityNameList, collapse = ", "), ')')))],
                                                             2, as.numeric), 2, sum),
                       provider.ineffect.NP = apply(apply(npiAttrivalid[Taxonomy1 %in% taxo_NP,
                                                                        eval(parse(text = paste0('.(', paste0(entityNameList, collapse = ", "), ')')))],
                                                          2, as.numeric), 2, sum),
                       provider.ineffect.PA = apply(apply(npiAttrivalid[Taxonomy1 %in% taxo_PA,
                                                                        eval(parse(text = paste0('.(', paste0(entityNameList, collapse = ", "), ')')))],
                                                          2, as.numeric), 2, sum),
                       provider.ineffect.PedM = apply(apply(npiAttrivalid[Taxonomy1 %in% taxo_PedM,
                                                                          eval(parse(text = paste0('.(', paste0(entityNameList, collapse = ", "), ')')))],
                                                            2, as.numeric), 2, sum),
                       provider.ineffect.CCNS = apply(apply(npiAttrivalid[Taxonomy1 %in% taxo_CCNS,
                                                                          eval(parse(text = paste0('.(', paste0(entityNameList, collapse = ", "), ')')))],
                                                            2, as.numeric), 2, sum),
                       provider.ineffect.FP = apply(apply(npiAttrivalid[Taxonomy1 %in% taxo_FP,
                                                                        eval(parse(text = paste0('.(', paste0(entityNameList, collapse = ", "), ')')))],
                                                          2, as.numeric), 2, sum),
                       provider.ineffect.IM = apply(apply(npiAttrivalid[Taxonomy1 %in% taxo_IM,
                                                                        eval(parse(text = paste0('.(', paste0(entityNameList, collapse = ", "), ')')))],
                                                          2, as.numeric), 2, sum),
                       provider.ineffect.GP = apply(apply(npiAttrivalid[Taxonomy1 %in% taxo_GP,
                                                                        eval(parse(text = paste0('.(', paste0(entityNameList, collapse = ", "), ')')))],
                                                          2, as.numeric), 2, sum)
)
fwrite(npitable,
       file.path(dir.demo, 'demo_provider.csv'))

### NPI lists and ineffect indicator for each entity
dir.out2 <- output_dir(paste0(dir.out, 'ANLists_final', date_attribution))
for (entityName in entityNameList){
  npi_AN <- npiAttri[eval(parse(text = entityName)) == 1,
                     .(NPI, Taxonomy1)]
  ## npi_AN.ineffect <- npiAttrivalid[eval(parse(text = entityName)) == 1]$NPI
  npi_AN.ineffect <- npiAttrivalid[eval(parse(text = entityName)) == 1][, .N, by = NPI]
  npi_AN <- npiAttrivalid[, .(NPI, Npat)][npi_AN, on = 'NPI']
  npi_AN[is.na(Npat), Npat := 0]
  ## table(npi_AN.ineffect$Npat)
  fwrite(npi_AN,
         paste0(dir.out2, '/', entityName, '.csv'))
  cat(entityName, dim(npi_AN)[1], dim(npi_AN[Npat > 0])[1], '\n')
}


pattable <- data.table(entityName = entityNameList,
                       pat.total = apply(apply(Attri2valid[, eval(parse(text = paste0('.(', paste0(entityNameList, collapse = ", "), ')')))],
                                               2, as.numeric), 2, sum),
                       pat.to.pcp = apply(apply(Attri2valid[Taxonomy1 %in% taxo_PCP, eval(parse(text = paste0('.(', paste0(entityNameList, collapse = ", "), ')')))],
                                                2, as.numeric), 2, sum),
                       pat.to.obgyn = apply(apply(Attri2valid[Taxonomy1 %in% taxo_OBGYN, eval(parse(text = paste0('.(', paste0(entityNameList, collapse = ", "), ')')))],
                                                  2, as.numeric), 2, sum))
fwrite(pattable,
       paste0(dir.out, 'PATtable.csv'))

##### Tie cases
Patient_Tie <- fread(paste0(dir.in.patAttri, 'PatientTie.csv'),
                     header = TRUE, colClasses = "character", encoding = 'UTF-8')
NPI_WholeSet <- fread(paste0(dir.in.NPIAttri, 'NPI_WholeSet_2019_com.csv'),
                      header = TRUE, colClasses = "character", encoding = 'UTF-8')
Patient_Tie <- NPI_WholeSet[Patient_Tie, on = "NPI"]

Patient_Tie2 <- Patient_Tie[
  , c(4:22, # corresponds to AN indicators
      24 # internal_member_id
  )][
    , lapply(.SD, function(x) sum(as.numeric(x), na.rm = T)), by = .(internal_member_id)
    ]

Patient_Tie2[, AN_max := do.call(pmax, .SD), .SDcols = 2:20]
fwrite(Patient_Tie2, paste0(dir.out, 'PatientTie2.csv'))

## indicator of whether the ANs have most No. of NPI who are assigned for each tie patient.
Patient_Tie3 <- cbind(
  data.table(internal_member_id = Patient_Tie2[!AN_max == 0]$internal_member_id),
  Patient_Tie2[!AN_max == 0, ifelse(.SD == AN_max, 1, 0), .SDcols = 2:20]
)
uniqueN(Patient_Tie2$internal_member_id)[1]    # 652   
uniqueN(Patient_Tie3$internal_member_id)   #435 have AN

Attri2_toAN <- rbind(
  Attri2valid,
  Patient_Tie3,
  fill = TRUE
)
Attri2_toAN <- Attri2_toAN[, Total := eval(parse(text = paste(paste0('as.numeric(', entityNameList, ')'), collapse = "+")))]
uniqueN(Attri2_toAN$internal_member_id) # 352169
fwrite(Attri2_toAN, paste0(dir.out, "Attri2_toAN.csv"))
## discovery on providers's specialty


## npitable2 <- data.table(entityName = entityNameList,
##                        provider.total = apply(apply(npiAttri[, eval(parse(text = paste0('.(', paste0(entityNameList, collapse = ", "), ')')))],
##                                                     2, as.numeric), 2, sum),
##                        provider.pcp = apply(apply(npiAttri[taxo %in% taxo_PCP,
##                                                            eval(parse(text = paste0('.(', paste0(entityNameList, collapse = ", "), ')')))],
##                                                   2, as.numeric), 2, sum),
##                        provider.obgyn = apply(apply(npiAttri[taxo %in% taxo_OBGYN,
##                                                              eval(parse(text = paste0('.(', paste0(entityNameList, collapse = ", "), ')')))],
##                                                     2, as.numeric), 2, sum),
##                        provider.ineffect.total = apply(apply(npiAttrivalid[, eval(parse(text = paste0('.(', paste0(entityNameList, collapse = ", "), ')')))],
##                                                           2, as.numeric), 2, sum),
##                        provider.ineffect.pcp = apply(apply(npiAttrivalid[taxo %in% taxo_PCP,
##                                                                       eval(parse(text = paste0('.(', paste0(entityNameList, collapse = ", "), ')')))],
##                                                         2, as.numeric), 2, sum),
##                        provider.ineffect.obgyn = apply(apply(npiAttrivalid[taxo %in% taxo_OBGYN,
##                                                                         eval(parse(text = paste0('.(', paste0(entityNameList, collapse = ", "), ')')))],
##                                                           2, as.numeric), 2, sum))


### discovery on patients who get attributed to NPI but not ANs
outAN_part1 <- Attri2[!Identify %in% 'Unattributed' & is.na(Total) & !internal_member_id %in% Patient_Tie$internal_member_id]
outAN_part2 <- Patient_Tie2[AN_max == 0][, AN_max := NULL]
Attri2_outAN <- rbind(
  outAN_part1,
  outAN_part2,
  fill = TRUE
)

uniqueN(Attri2_outAN$internal_member_id) # 250295
fwrite(outAN_part1, paste0(dir.out, 'Attri2_outAN_part1.csv'))
fwrite(Attri2_outAN, paste0(dir.out, 'Attri2_outAN.csv'))

### Attri2_final
Attri2_final <- rbind(
  Attri2_toAN,
  Attri2_outAN,
  fill = T
)
uniqueN(Attri2_final$internal_member_id) # 602464


elig <- fread("E:/CT_APCD/Beth/data6/Eligibility/By_Calendar_Year/MEDICAL_ELIGIBILITY_ALLRECORDS14_19_6b.csv", 
              select = c("INTERNAL_MEMBER_ID",
                         "GENDER_CODE",
                         "birth_dt"),
              colClasses = "character") %>% unique()
elig[, `:=`(birth_year = year(as.yearmon(birth_dt, "%Y%m")))]
elig[, age:= 2019 - as.integer(birth_year)]
elig <- elig[age < 65]
names(elig) <- tolower(names(elig))
elig.patient <- elig[, .(internal_member_id, gender_code, age)] %>%
  unique(use.key = FALSE)
sum(!Attri2_final$internal_member_id %in% elig$internal_member_id)
Attri2_final <- elig[Attri2_final, on = c('internal_member_id')]
fwrite(Attri2_final, paste0(dir.out, 'Attri2_final.csv'))

### summary table for patientwise AN v.s. Specialty
# nppes <- fread("E:/CT_APCD/Sai/lds2021/lds2021file/nppes/nppes_lds_com_2019_short.csv",
#                colClasses = "character",
#                header = TRUE,
#                encoding = "UTF-8")
# outAN_part1 <- nppes[outAN_part1, on = c("NPI")]

# spec.order <- c("NP", "CCNS", "PA", "OBGYN", "IM", "FP", "GP", "PedM")
# split_by_specialty <- function(dat, # data.table format with `NPI` column
#                                row.name){
#   dat1 <- nppes[dat[!is.na(NPI), .(NPI, internal_member_id)], on = "NPI"]
#   dat2 <- taxo_info[, .(taxo, spec1)][dat1, on = c("taxo" = "Taxonomy1")]
#   freq <- table(factor(dat2$spec1, levels = spec.order))
#   freq1 <- matrix(freq, nrow = 1)
#   freq.tb <- data.table(AN = row.name, freq1)
#   colnames(freq.tb) <- c("AN", names(freq))
#   return(freq.tb)
# }
# AN_spec <- data.table()
# for (entityName in entityNameList){
#   AN_spec <- rbind(
#     AN_spec,
#     split_by_specialty(
#       Attri2_toAN[eval(parse(
#         text = paste0(entityName, " == 1")
#       ))],
#       entityName
#     )
#   )
#   cat(entityName, '\n')
# }
# AN_spec <- rbind(
#   AN_spec,
#   split_by_specialty(outAN_part1, "Non Attributed")
# )
# fwrite(AN_spec, paste0(dir.out, "AN_spec.csv"))

### discover those
Attri2valid <- fread(file.path(dir.out, "Attri2valid.csv"), colClasses = 'character')
length(unique(Attri2valid$NPI)) # 2674
npiAttri <- fread(paste0(dir.in.NPIAttri, 'NPI_WholeSet_2019_com.csv'),
                  header = TRUE, colClasses = 'character', encoding = 'UTF-8')
uniqueN(npiAttri$NPI) # 3288
npiAttri[, get.patient := as.numeric(NPI %in% unique(Attri2valid$NPI))]
npi.no.patient <- npiAttri[get.patient == 0]$NPI

## source("E:/CT_APCD/Yaqiong/myfunction.R")
## MC_2016_OutP <- get_medical_vs(
##     vs.list = 'Outpatient',
##     year.list = 2016
## )
## MC_2019_OutP <- get_medical_vs(
##     vs.list = 'Outpatient',
##     ## code.sys = c("procedure_code"), #, "revenue_code"),
##     year.list = 2019,
##     # select = c(
##     #     'INTERNAL_MEMBER_ID',
##     #     'RENDERING_PROVIDER_ID',
##     #     'submitter_id',
##     #     'PROCEDURE_CODE'
##     # )
## )
## MC_2019_OutP <- MC_2019_OutP[, .(internal_member_id, submitter_id, procedure_code, rendering_provider_id)]
## MC_2016_OutP <- MC_2016_OutP[, .(internal_member_id, submitter_id, procedure_code, rendering_provider_id)]
## ## modified provider file
## provider <- fread('E:/CT_APCD/shared/intermediate_data/APCD_modified/provider/provider_new.csv',
##                   header = TRUE,
##                   colClasses = 'character',
##                   encoding = 'UTF-8')
## ## modified eligibility file
## elig.patient <- fread("E:/CT_APCD/shared/intermediate_data/APCD_modified/eligibility/eligibility_patient.csv",
##                       header = TRUE, colClasses = 'character', encoding = 'UTF-8',
##                       select = c(
##                           "INTERNAL_MEMBER_ID",
##                           "birth_dt"
##                           )
##                       ) ## need MEDICAL COVERAGE?????
##
## ## identify patients whose ages more than 64
## patient_age0064 <- elig.patient[birth_dt >= 2019 - 64]$INTERNAL_MEMBER_ID
##
## ## Add NPI to raw dataset
## MC_2019_OutP <- provider[MC_2019_OutP, on = c(PROVIDER_ID = 'rendering_provider_id')]
## MC_2016_OutP <- provider[MC_2016_OutP, on = c(PROVIDER_ID = 'rendering_provider_id')]
##
## ## Exclude patients with age 65 more
## MC_2019_OutP <- MC_2019_OutP[internal_member_id %in% patient_age0064]
## MC_2016_OutP <- MC_2016_OutP[internal_member_id %in% patient_age0064]
##
## ## Exclude Medicare claims
## MC_2019_OutP <- MC_2019_OutP[!submitter_id == 15227]
## MC_2016_OutP <- MC_2016_OutP[!submitter_id == 15227]
##
## ## MC to MC_EM & MC_NotEM datasets
## MC_2019_OutP_EM <- MC_2019_OutP[procedure_code %in% CPTList]
## MC_2016_OutP_EM <- MC_2016_OutP[procedure_code %in% CPTList]
##
## MC_2019_npi.no.patient <- MC_2019_OutP_EM[NPI %in% npi.no.patient]
## dim(MC_2019_npi.no.patient)
##                                         # claims that associate with those NPI who get no patient attributed to: 51659 (dt: 2019-04-14)
## length(unique(MC_2019_npi.no.patient$NPI))
##                                         # npi who associate with at least 1 claim in 2019: 364 (dt: 2019-04-14)
## length(unique(MC_2019_npi.no.patient$internal_member_id))
##                                         # patients who associate with those NPI who get no patient attributed to: 29010 (dt: 2019-04-14)
## MC_2016_npi.no.patient <- MC_2016_OutP_EM[NPI %in% npi.no.patient]
## dim(MC_2016_npi.no.patient)
## # claims that associate with those NPI who get no patient attributed to:  42581 (dt: 2019-04-14)
## length(unique(MC_2016_npi.no.patient$NPI))
## # npi who associate with at least 1 claim in 2016: 276 (dt: 2019-04-14)
## length(unique(MC_2016_npi.no.patient$internal_member_id))
## # patients who associate with those NPI who get no patient attributed to: 23625 (dt: 2019-04-14)
##
## ## 16-17 overlap
## length(unique(c(MC_2019_npi.no.patient$NPI, MC_2016_npi.no.patient)))
## # 370 (dt: 2019-04-14)
## length(unique(c(MC_2019_npi.no.patient$internal_member_id, MC_2016_npi.no.patient$internal_member_id)))
## # 41825 (dt: 2019-04-14)
## patient.target <- unique(MC_2019_npi.no.patient$internal_member_id)
## table(Attri2_final[INTERNAL_MEMBER_ID %in% patient.target]$Total)

Attri2_final[, Total := as.numeric(Total)]
Attri2_final_tmp <- Attri2_final[, lapply(.SD, sum), 
                                 by = .(internal_member_id),
                                 .SDcol = "Total"]
table(Attri2_final_tmp$Total)    # patients with npi to 1,2,3 AN

# 1      2      3
#336193  14911   1065

Attri2valid[, Total := as.numeric(Total)]
Attri2valid_tmp <- Attri2valid[, lapply(.SD, sum), 
                               by = .(internal_member_id),
                               .SDcol = "Total"]
table(Attri2valid_tmp$Total)   # patients (with npi and no tie) to 1,2,3 AN
# 1      2      3 
# 335784  14885   1065  
Patient_Tie4 <- Patient_Tie3
Patient_Tie4[, Total := rowSums(.SD, na.rm = T), 
             .SDcols = names(Patient_Tie4)[-1]]
table(Patient_Tie4$Total)   # patients (with npi and tie) to 1,2,3 AN
# 1    2 
# 409  26 
