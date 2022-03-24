########## Patient Attribution ##########


#### packages #####
library(data.table)
library(lubridate)
library(tidyverse)
library(zoo)
source('E:/CT_APCD/Sai/2019spring/data-organization/R/utils.R')
dir_code <- input_dir('E:/CT_APCD/Sai/lds2021/lds2021code/Attribution_2018_2019/')
source(file.path(dir_code, 'myDoc.R'))
source(file.path(dir_code, 'myFun.R'))
source('E:/CT_APCD/Sai/2019spring/data-organization/R/functions.R')
dir_out <- output_dir('E:/CT_APCD/Sai/lds2021/lds2021file/Attribution_result/InProcess_2018/')
## READ MC and NPPES
nppes0 <- fread("E:/CT_APCD/Sai/lds2021/lds2021file/nppes/nppes_lds_com_2018_short.csv",
                colClasses = "character",
                header = TRUE,
                encoding = "UTF-8")
col.select <-  c('medical_claim_service_line_id',
                 'internal_member_id',
                 'first_service_dt',
                 'procedure_code',
                 'diagnosis_code',
                 'icd_procedure_code',
                 'medical_claim_header_id',
                 'rendering_provider_id',
                 # 'billing_provider_id',
                 'submitter_id',
                 "product_code",
                 NULL)
## Outpatient value set ### coomentted by Sai 05/25/2018
MC_2018_OutP <- get_medical_vs(
  vs.list = 'Outpatient',
  dir.raw = "E:/CT_APCD/Sai/lds2021/lds2021file/value_sets/medical_claim/",
  year.list = 2018,
  select = col.select
)
MC_2017_OutP <- get_medical_vs(
  vs.list = 'Outpatient',
  dir.raw = "E:/CT_APCD/Sai/lds2021/lds2021file/value_sets/medical_claim/",
  year.list = 2017,
  select = col.select
)

## modified provider file
elig.patient <- fread("E:/CT_APCD/Beth/data6/Eligibility/By_Calendar_Year/MEDICAL_ELIGIBILITY_ALLRECORDS14_19_6b.csv", 
                      select = c("INTERNAL_MEMBER_ID",
                                 "GENDER_CODE",
                                 "birth_dt"),
                      colClasses = "character") %>% unique()
elig.patient[, `:=`(birth_year = year(as.yearmon(birth_dt, "%Y%m")))]
elig.patient[, age:= 2018 - as.integer(birth_year)]
elig.patient <- elig.patient[age < 65]
names(elig.patient) <- tolower(names(elig.patient))
elig.patient <- elig.patient[, .(internal_member_id, gender_code, age)] %>%
  unique(use.key = FALSE)

provider <- fread("E:/CT_APCD/Sai/lds2021/lds2021file/nppes/provider_small.csv",
                  colClasses  = "character")

## identify patients whose ages more than 64
# patient_age0064 <- elig.patient[birth_dt >= 2018 - 64]$INTERNAL_MEMBER_ID  # by Sai

## Get NPI list
realNPI_pcp <- unique(nppes0[spec0 == "PCP"]$NPI)
realNPI_obgyn <- unique(nppes0[spec0 == "OBGYN"]$NPI)

## remove Medicare AD claims
mcpt <- rbindlist(lapply(paste0("E:/CT_APCD/Sai/lds2021/lds2021file/medicare_medicare_ad_patients/medicare_medicare_ad_patients_", 2017:2018, ".csv"),
                         function(x){
                           fread(x, colClasses = "character")
                         })) %>% unique()
MC_2017_OutP <- MC_2017_OutP[!internal_member_id %in% mcpt$internal_member_id]
MC_2018_OutP <- MC_2018_OutP[!internal_member_id %in% mcpt$internal_member_id]

## Add NPI to raw dataset
MC_2018_OutP <- provider[MC_2018_OutP, on = c(provider_id = 'rendering_provider_id')]
MC_2017_OutP <- provider[MC_2017_OutP, on = c(provider_id = 'rendering_provider_id')]

## Add birth_dt to raw dataset
MC_2018_OutP <- elig.patient[MC_2018_OutP, on = c("internal_member_id"),
                             nomatch = 0]
MC_2017_OutP <- elig.patient[MC_2017_OutP, on = c("internal_member_id"),
                             nomatch = 0]

## remove facility provider
# MC_2018_OutP <- MC_2018_OutP[!provider_facility, on = 'PROVIDER_ID']
# MC_2017_OutP <- MC_2017_OutP[!provider_facility, on = 'PROVIDER_ID']

## MC to MC_EM & MC_NotEM datasets
MC_2018_OutP_EM <- MC_2018_OutP[procedure_code %in% CPTList]
MC_2018_OutP_NonEM <- MC_2018_OutP[!procedure_code %in% CPTList]
MC_2017_OutP_EM <- MC_2017_OutP[procedure_code %in% CPTList]
MC_2017_OutP_NonEM <- MC_2017_OutP[!procedure_code %in% CPTList]

## to MC_year_EM_class datasets
MC_2018_OutP_EM_pcp <- MC_2018_OutP_EM[NPI %in% realNPI_pcp]
MC_2018_OutP_EM_obgyn <- MC_2018_OutP_EM[NPI %in% realNPI_obgyn]
MC_2017_OutP_EM_pcp <- MC_2017_OutP_EM[NPI %in% realNPI_pcp]
MC_2017_OutP_EM_obgyn <- MC_2017_OutP_EM[NPI %in% realNPI_obgyn]
MC_2018_OutP_EM_other <- MC_2018_OutP_EM[!NPI %in% c(realNPI_pcp, realNPI_obgyn)]


## Patient_Attribution matrix
Patient_classification <- data.table(internal_member_id =
                                       unique(MC_2018_OutP_EM$internal_member_id))
Patient_classification[,
                       `:=` (In_2018_OutP_EM_pcp =
                               as.numeric(internal_member_id %in%
                                            unique(MC_2018_OutP_EM_pcp$internal_member_id)),
                             In_2018_OutP_EM_obgyn =
                               as.numeric(internal_member_id %in%
                                            unique(MC_2018_OutP_EM_obgyn$internal_member_id)),
                             In_2017_OutP_EM_pcp =
                               as.numeric(internal_member_id %in%
                                            unique(MC_2017_OutP_EM_pcp$internal_member_id)),
                             In_2017_OutP_EM_obgyn =
                               as.numeric(internal_member_id %in%
                                            unique(MC_2017_OutP_EM_obgyn$internal_member_id)))]
## patient attribution flow chart step1
Patient_classification[In_2018_OutP_EM_pcp == 1,
                       Attribution := 'PCP_2018']
## patient attribution flow chart step2
Patient_classification[In_2018_OutP_EM_pcp == 0 & In_2017_OutP_EM_pcp == 1,
                       Attribution := 'PCP_2017']
## patient attribution flow chart step3
Patient_classification[In_2018_OutP_EM_pcp == 0 & In_2017_OutP_EM_pcp == 0 &
                         In_2018_OutP_EM_obgyn == 1,
                       Attribution := 'OBGYN_2018']
## patient attribution flow chart step4
Patient_classification[In_2018_OutP_EM_pcp == 0 & In_2017_OutP_EM_pcp == 0 &
                         In_2018_OutP_EM_obgyn == 0 & In_2017_OutP_EM_obgyn == 1,
                       Attribution := 'OBGYN_2017']
## patient attribution flow chart step5
Patient_classification[In_2018_OutP_EM_pcp == 0 & In_2017_OutP_EM_pcp == 0 &
                         In_2018_OutP_EM_obgyn == 0 & In_2017_OutP_EM_obgyn == 0,
                       Attribution := 'Unattributed']

Patient_classification[!internal_member_id %in% c(unique(MC_2018_OutP_EM$internal_member_id),
                                                  unique(MC_2017_OutP_EM$internal_member_id)),
                       Attribution := 'No E&M service Record']
## frequency table for 6 attribution categories.
tb.pat_class <- as.data.table(table(Patient_classification$Attribution))
tb.pat_class[, percent := N / sum(tb.pat_class$N)]
colnames(tb.pat_class) <- c('AttriClass', 'Count', 'percent')

### save tables
fwrite(Patient_classification,  ## actual patient attribution table
       paste0(dir_out, 'PatientClass.csv'))
fwrite(tb.pat_class,  ## frequency table for attribution categories
       paste0(dir_out, 'tb_PatientClass.csv'))
## assign patient_id lists for each attribution categories into 5 variable names.
patient_to_pcp_2018 <- Patient_classification[Attribution == 'PCP_2018']$internal_member_id
patient_to_pcp_2017 <- Patient_classification[Attribution == 'PCP_2017']$internal_member_id
patient_to_obgyn_2018 <- Patient_classification[Attribution == 'OBGYN_2018']$internal_member_id
patient_to_obgyn_2017 <- Patient_classification[Attribution == 'OBGYN_2017']$internal_member_id
patient_unattr <- Patient_classification[Attribution == 'Unattributed']$internal_member_id

###################### FIND NPI for each Patient #############################
#### Part 1.1
MC_pcp_2018 <- MC_2018_OutP_EM_pcp[internal_member_id %in% patient_to_pcp_2018]
MC_pcp_2017 <- MC_2017_OutP_EM_pcp[internal_member_id %in% patient_to_pcp_2017]
MC_obgyn_2018 <- MC_2018_OutP_EM_obgyn[internal_member_id %in% patient_to_obgyn_2018]
MC_obgyn_2017 <- MC_2017_OutP_EM_obgyn[internal_member_id %in% patient_to_obgyn_2017]
MC_unattr_2018 <- MC_2018_OutP_EM[internal_member_id %in% patient_unattr]

fwrite(MC_pcp_2018,
       paste0(dir_out, 'MC_pcp_2018.csv'))
fwrite(MC_pcp_2017,
       paste0(dir_out, 'MC_pcp_2017.csv'))
fwrite(MC_obgyn_2018,
       paste0(dir_out, 'MC_obgyn_2018.csv'))
fwrite(MC_obgyn_2017,
       paste0(dir_out, 'MC_obgyn_2017.csv'))
fwrite(MC_unattr_2018,
       paste0(dir_out, 'MC_unattributed_2018.csv'))


##### TieBreaker SS1 ####
## SS1.1 ##
group <- MC_pcp_2018
{
  cat('start at ', as.character(Sys.time()), '\n')
  tmp1 <- group[,
                list(Freq = length(unique(medical_claim_header_id))),
                by = list(internal_member_id, NPI)]
  tmp2 <- tmp1[tmp1[, .I[Freq == max(Freq)], by = internal_member_id]$V1]
  out_unique <- tmp2[tmp2[, .I[length(unique(NPI))==1], by = internal_member_id]$V1]
  out_tie <- tmp2[tmp2[, .I[length(unique(NPI))>1], by = internal_member_id]$V1]
  cat('finish at', as.character(Sys.time()))
}
Patient_PCP_2018_UniqueVisits_EM <- out_unique[, Identify := 'Visits_EM']
Patient_PCP_2018_TieVisits_EM <- out_tie
fwrite(out_unique[, Identify := 'Visits_EM'],
       paste0(dir_out, 'Patient_PCP_2018_UniqueVisits_EM.csv'))
fwrite(out_tie,
       paste0(dir_out, 'Patient_PCP_2018_TieVisits_EM.csv'))
info.summary <- data.table(matrix(c('MC_PCP_2018_Visits_EM',
                                    length(unique(group$internal_member_id)),
                                    dim(out_unique)[1],
                                    dim(out_tie)[1],
                                    length(unique(out_tie$internal_member_id)),
                                    ratio <- dim(out_unique)[1]/
                                      (dim(out_unique)[1] +
                                         length(unique(out_tie$internal_member_id))),
                                    max(out_tie$Freq)), nrow = 1))

## SS1.2 ##
group <- MC_pcp_2017
{ # from MC_xx_yyyy get out_unique, out_tie
  tmp1 <- group[,
                list(Freq = length(unique(medical_claim_header_id))),
                by = list(internal_member_id, NPI)]
  tmp2 <- tmp1[tmp1[, .I[Freq == max(Freq)], by = internal_member_id]$V1]
  out_unique <- tmp2[tmp2[, .I[length(unique(NPI))==1], by = internal_member_id]$V1]
  out_tie <- tmp2[tmp2[, .I[length(unique(NPI))>1], by = internal_member_id]$V1]
}
Patient_PCP_2017_UniqueVisits_EM <- out_unique[, Identify := 'Visits_EM']
fwrite(out_unique[, Identify := 'Visits_EM'],
       paste0(dir_out, 'Patient_PCP_2017_UniqueVisits_EM.csv'))
fwrite(out_tie,
       paste0(dir_out, 'Patient_PCP_2017_TieVisits_EM.csv'))
info.summary <- rbind(info.summary,
                      data.table(matrix(c('MC_PCP_2017_Visits_EM',
                                          length(unique(group$internal_member_id)),
                                          dim(out_unique)[1],
                                          dim(out_tie)[1],
                                          length(unique(out_tie$internal_member_id)),
                                          ratio <- dim(out_unique)[1]/
                                            (dim(out_unique)[1] +
                                               length(unique(out_tie$internal_member_id))),
                                          max(out_tie$Freq)), nrow = 1)))

## SS1.3 ##
group <- MC_obgyn_2018
{ # from MC_xx_yyyy get out_unique, out_tie
  tmp1 <- group[,
                list(Freq = length(unique(medical_claim_header_id))),
                by = list(internal_member_id, NPI)]
  tmp2 <- tmp1[tmp1[, .I[Freq == max(Freq)], by = internal_member_id]$V1]
  out_unique <- tmp2[tmp2[, .I[length(unique(NPI))==1], by = internal_member_id]$V1]
  out_tie <- tmp2[tmp2[, .I[length(unique(NPI))>1], by = internal_member_id]$V1]
}
Patient_OBGYN_2018_UniqueVisits_EM <- out_unique[, Identify := 'Visits_EM']
fwrite(out_unique[, Identify := 'Visits_EM'],
       paste0(dir_out, 'Patient_OBGYN_2018_UniqueVisits_EM.csv'))
fwrite(out_tie,
       paste0(dir_out, 'Patient_OBGYN_2018_TieVisits_EM.csv'))
info.summary <- rbind(info.summary,
                      data.table(matrix(c('MC_OBGYN_2018_Visits_EM',
                                          length(unique(group$internal_member_id)),
                                          dim(out_unique)[1],
                                          dim(out_tie)[1],
                                          length(unique(out_tie$internal_member_id)),
                                          ratio <- dim(out_unique)[1]/
                                            (dim(out_unique)[1] +
                                               length(unique(out_tie$internal_member_id))),
                                          max(out_tie$Freq)), nrow = 1)))

## SS1.4 ##
group <- MC_obgyn_2017
{ # from MC_xx_yyyy get out_unique, out_tie
  tmp1 <- group[,
                list(Freq = length(unique(medical_claim_header_id))),
                by = list(internal_member_id, NPI)]
  tmp2 <- tmp1[tmp1[, .I[Freq == max(Freq)], by = internal_member_id]$V1]
  out_unique <- tmp2[tmp2[, .I[length(unique(NPI))==1], by = internal_member_id]$V1]
  out_tie <- tmp2[tmp2[, .I[length(unique(NPI))>1], by = internal_member_id]$V1]
}
Patient_OBGYN_2017_UniqueVisits_EM <- out_unique[, Identify := 'Visits_EM']
fwrite(out_unique[, Identify := 'Visits_EM'],
       paste0(dir_out, 'Patient_OBGYN_2017_UniqueVisits_EM.csv'))
fwrite(out_tie,
       paste0(dir_out, 'Patient_OBGYN_2017_TieVisits_EM.csv'))
info.summary <- rbind(info.summary,
                      data.table(matrix(c('MC_OBGYN_2017_Visits_EM',
                                          length(unique(group$internal_member_id)),
                                          dim(out_unique)[1],
                                          dim(out_tie)[1],
                                          length(unique(out_tie$internal_member_id)),
                                          ratio <- dim(out_unique)[1]/
                                            (dim(out_unique)[1] +
                                               length(unique(out_tie$internal_member_id))),
                                          max(out_tie$Freq)), nrow = 1)))

###### document summary ####
names(info.summary) <- c("visit",
                         "number_of_member",
                         "number_of_member_with_unique_npi",
                         "number_of_npi_tie",
                         "number_of_member_with_npi_tie",
                         "ratio_of_member_with_unique_npi",
                         "max_freq_in_npi_tie"
)
fwrite(info.summary,
       paste0(dir_out, 'summarySS1.csv'))

######## SS1 complete #############

##### TieBreaker SS2 ####
info.summary <- fread(paste0(dir_out, 'summarySS1.csv'), header = TRUE,
                      colClasses = 'character', encoding = 'UTF-8')

## SS2.1 ##
Patient_PCP_2018_TieVisits_EM <- fread(paste0(dir_out, 'Patient_PCP_2018_TieVisits_EM.csv'),
                                       header = TRUE,
                                       colClasses = 'character',
                                       encoding = 'UTF-8')
MC_PCP_2018_OutP_SS1 <- Patient_PCP_2018_TieVisits_EM[MC_2018_OutP,
                                                      on = c(internal_member_id = 'internal_member_id',
                                                             NPI = 'NPI')][Freq > 0]
group <- MC_PCP_2018_OutP_SS1
{
  tmp1 <- group[,
                list(Freq = length(unique(medical_claim_header_id))),
                by = list(internal_member_id, NPI)]
  tmp2 <- tmp1[tmp1[, .I[Freq == max(Freq)], by = internal_member_id]$V1]
  out_unique <- tmp2[tmp2[, .I[length(unique(NPI))==1], by = internal_member_id]$V1]
  out_tie <- tmp2[tmp2[, .I[length(unique(NPI))>1], by = internal_member_id]$V1]
}
Patient_PCP_2018_UniqueVisits_OutP <- out_unique[, Identify := 'Visits_OutP']
fwrite(out_unique[, Identify := 'Visits_OutP'],
       paste0(dir_out, 'Patient_PCP_2018_UniqueVisits_OutP.csv'))
fwrite(out_tie,
       paste0(dir_out, 'Patient_PCP_2018_TieVisits_OutP.csv'))
tmp <- data.table(matrix(c('MC_PCP_2018_Visits_OutP',
                           length(unique(group$internal_member_id)),
                           dim(out_unique)[1],
                           dim(out_tie)[1],
                           length(unique(out_tie$internal_member_id)),
                           ratio <- dim(out_unique)[1]/
                             (dim(out_unique)[1] +
                                length(unique(out_tie$internal_member_id))),
                           max(out_tie$Freq)), nrow = 1))
names(tmp) <- names(info.summary)
info.summary <- rbind(info.summary, tmp)


## SS2.2 ##
Patient_PCP_2017_TieVisits_EM <- fread(paste0(dir_out, 'Patient_PCP_2017_TieVisits_EM.csv'),
                                       header = TRUE,
                                       colClasses = 'character',
                                       encoding = 'UTF-8')
MC_PCP_2017_OutP_SS1 <- Patient_PCP_2017_TieVisits_EM[MC_2017_OutP,
                                                      on = c(internal_member_id = 'internal_member_id',
                                                             NPI = 'NPI')][Freq > 0]
group <- MC_PCP_2017_OutP_SS1
{
  tmp1 <- group[,
                list(Freq = length(unique(medical_claim_header_id))),
                by = list(internal_member_id, NPI)]
  tmp2 <- tmp1[tmp1[, .I[Freq == max(Freq)], by = internal_member_id]$V1]
  out_unique <- tmp2[tmp2[, .I[length(unique(NPI))==1], by = internal_member_id]$V1]
  out_tie <- tmp2[tmp2[, .I[length(unique(NPI))>1], by = internal_member_id]$V1]
}
Patient_PCP_2017_UniqueVisits_OutP <- out_unique[, Identify := 'Visits_OutP']
fwrite(out_unique[, Identify := 'Visits_OutP'],
       paste0(dir_out, 'Patient_PCP_2017_UniqueVisits_OutP.csv'))
fwrite(out_tie,
       paste0(dir_out, 'Patient_PCP_2017_TieVisits_OutP.csv'))
tmp <- data.table(matrix(c('MC_PCP_2017_Visits_OutP',
                           length(unique(group$internal_member_id)),
                           dim(out_unique)[1],
                           dim(out_tie)[1],
                           length(unique(out_tie$internal_member_id)),
                           ratio <- dim(out_unique)[1]/
                             (dim(out_unique)[1] +
                                length(unique(out_tie$internal_member_id))),
                           max(out_tie$Freq)), nrow = 1))
names(tmp) <- names(info.summary)
info.summary <- rbind(info.summary, tmp)

## SS2.3 ##
Patient_OBGYN_2018_TieVisits_EM <- fread(paste0(dir_out, 'Patient_OBGYN_2018_TieVisits_EM.csv'),
                                         header = TRUE,
                                         colClasses = 'character',
                                         encoding = 'UTF-8')
MC_OBGYN_2018_OutP_SS1 <- Patient_OBGYN_2018_TieVisits_EM[MC_2018_OutP,
                                                          on = c(internal_member_id = 'internal_member_id',
                                                                 NPI = 'NPI')][Freq > 0]
group <- MC_OBGYN_2018_OutP_SS1
{
  tmp1 <- group[,
                list(Freq = length(unique(medical_claim_header_id))),
                by = list(internal_member_id, NPI)]
  tmp2 <- tmp1[tmp1[, .I[Freq == max(Freq)], by = internal_member_id]$V1]
  out_unique <- tmp2[tmp2[, .I[length(unique(NPI))==1], by = internal_member_id]$V1]
  out_tie <- tmp2[tmp2[, .I[length(unique(NPI))>1], by = internal_member_id]$V1]
}
Patient_OBGYN_2018_UniqueVisits_OutP <- out_unique[, Identify := 'Visits_OutP']
fwrite(out_unique[, Identify := 'Visits_OutP'],
       paste0(dir_out, 'Patient_OBGYN_2018_UniqueVisits_OutP.csv'))
fwrite(out_tie,
       paste0(dir_out, 'Patient_OBGYN_2018_TieVisits_OutP.csv'))
tmp <- data.table(matrix(c('MC_OBGYN_2018_Visits_OutP',
                           length(unique(group$internal_member_id)),
                           dim(out_unique)[1],
                           dim(out_tie)[1],
                           length(unique(out_tie$internal_member_id)),
                           ratio <- dim(out_unique)[1]/
                             (dim(out_unique)[1] +
                                length(unique(out_tie$internal_member_id))),
                           max(out_tie$Freq)), nrow = 1))
names(tmp) <- names(info.summary)
info.summary <- rbind(info.summary, tmp)

## SS2.4 ##
Patient_OBGYN_2017_TieVisits_EM <- fread(paste0(dir_out, 'Patient_OBGYN_2017_TieVisits_EM.csv'),
                                         header = TRUE,
                                         colClasses = 'character',
                                         encoding = 'UTF-8')
MC_OBGYN_2017_OutP_SS1 <- Patient_OBGYN_2017_TieVisits_EM[MC_2017_OutP,
                                                          on = c(internal_member_id = 'internal_member_id',
                                                                 NPI = 'NPI')][Freq > 0]
group <- MC_OBGYN_2017_OutP_SS1
{
  tmp1 <- group[,
                list(Freq = length(unique(medical_claim_header_id))),
                by = list(internal_member_id, NPI)]
  tmp2 <- tmp1[tmp1[, .I[Freq == max(Freq)], by = internal_member_id]$V1]
  out_unique <- tmp2[tmp2[, .I[length(unique(NPI))==1], by = internal_member_id]$V1]
  out_tie <- tmp2[tmp2[, .I[length(unique(NPI))>1], by = internal_member_id]$V1]
}
Patient_OBGYN_2017_UniqueVisits_OutP <- out_unique[, Identify := 'Visits_OutP']
fwrite(out_unique[, Identify := 'Visits_OutP'],
       paste0(dir_out, 'Patient_OBGYN_2017_UniqueVisits_OutP.csv'))
fwrite(out_tie,
       paste0(dir_out, 'Patient_OBGYN_2017_TieVisits_OutP.csv'))
tmp <- data.table(matrix(c('MC_OBGYN_2017_Visits_OutP',
                           length(unique(group$internal_member_id)),
                           dim(out_unique)[1],
                           dim(out_tie)[1],
                           length(unique(out_tie$internal_member_id)),
                           ratio <- dim(out_unique)[1]/
                             (dim(out_unique)[1] +
                                length(unique(out_tie$internal_member_id))),
                           max(out_tie$Freq)), nrow = 1))
names(tmp) <- names(info.summary)
info.summary <- rbind(info.summary, tmp)
###### document summary ####
fwrite(info.summary,
       paste0(dir_out, 'summarySS2.csv'))


###### SS3 #####
info.summary <- fread(paste0(dir_out, 'summarySS2.csv'), header = TRUE,
                      colClasses = 'character', encoding = 'UTF-8')

## SS3.1 ##
Patient_PCP_2018_TieVisits_OutP <- fread(paste0(dir_out, 'Patient_PCP_2018_TieVisits_OutP.csv'),
                                         header = TRUE,
                                         colClasses = 'character',
                                         encoding = 'UTF-8')
MC_PCP_2018_OutP_SS2 <- Patient_PCP_2018_TieVisits_OutP[MC_2018_OutP,
                                                        on = c(internal_member_id = 'internal_member_id',
                                                               NPI = 'NPI')][Freq > 0]
group <- MC_PCP_2018_OutP_SS2
{
  tmp1 <- group[,
                list(Freq = length(unique(first_service_dt))),
                by = list(internal_member_id, NPI)]
  tmp2 <- tmp1[tmp1[, .I[Freq == max(Freq)], by = internal_member_id]$V1]
  out_unique <- tmp2[tmp2[, .I[length(unique(NPI))==1], by = internal_member_id]$V1]
  out_tie <- tmp2[tmp2[, .I[length(unique(NPI))>1], by = internal_member_id]$V1]
}
Patient_PCP_2018_UniqueDays_OutP <- out_unique[, Identify := 'MostDays_OutP']
fwrite(out_unique[, Identify := 'MostDays_OutP'],
       paste0(dir_out, 'Patient_PCP_2018_UniqueDays_OutP.csv'))
fwrite(out_tie,
       paste0(dir_out, 'Patient_PCP_2018_TieDays_OutP.csv'))
tmp <- data.table(matrix(c('MC_PCP_2018_Days_OutP',
                           length(unique(group$internal_member_id)),
                           dim(out_unique)[1],
                           dim(out_tie)[1],
                           length(unique(out_tie$internal_member_id)),
                           ratio <- dim(out_unique)[1]/
                             (dim(out_unique)[1] +
                                length(unique(out_tie$internal_member_id))),
                           max(out_tie$Freq)), nrow = 1))
names(tmp) <- names(info.summary)
info.summary <- rbind(info.summary, tmp)

## SS3.2 ##
Patient_PCP_2017_TieVisits_OutP <- fread(paste0(dir_out, 'Patient_PCP_2017_TieVisits_OutP.csv'),
                                         header = TRUE,
                                         colClasses = 'character',
                                         encoding = 'UTF-8')
MC_PCP_2017_OutP_SS2 <- Patient_PCP_2017_TieVisits_OutP[MC_2017_OutP,
                                                        on = c(internal_member_id = 'internal_member_id',
                                                               NPI = 'NPI')][Freq > 0]
group <- MC_PCP_2017_OutP_SS2
{
  tmp1 <- group[,
                list(Freq = length(unique(first_service_dt))),
                by = list(internal_member_id, NPI)]
  tmp2 <- tmp1[tmp1[, .I[Freq == max(Freq)], by = internal_member_id]$V1]
  out_unique <- tmp2[tmp2[, .I[length(unique(NPI))==1], by = internal_member_id]$V1]
  out_tie <- tmp2[tmp2[, .I[length(unique(NPI))>1], by = internal_member_id]$V1]
}
Patient_PCP_2017_UniqueDays_OutP <- out_unique[, Identify := 'MostDays_OutP']
fwrite(out_unique[, Identify := 'MostDays_OutP'],
       paste0(dir_out, 'Patient_PCP_2017_UniqueDays_OutP.csv'))
fwrite(out_tie,
       paste0(dir_out, 'Patient_PCP_2017_TieDays_OutP.csv'))
tmp <- data.table(matrix(c('MC_PCP_2017_Days_OutP',
                           length(unique(group$internal_member_id)),
                           dim(out_unique)[1],
                           dim(out_tie)[1],
                           length(unique(out_tie$internal_member_id)),
                           ratio <- dim(out_unique)[1]/
                             (dim(out_unique)[1] +
                                length(unique(out_tie$internal_member_id))),
                           max(out_tie$Freq)), nrow = 1))
names(tmp) <- names(info.summary)
info.summary <- rbind(info.summary, tmp)

## SS3.3 ##
Patient_OBGYN_2018_TieVisits_OutP <- fread(paste0(dir_out, 'Patient_OBGYN_2018_TieVisits_OutP.csv'),
                                           header = TRUE,
                                           colClasses = 'character',
                                           encoding = 'UTF-8')
MC_OBGYN_2018_OutP_SS2 <- Patient_OBGYN_2018_TieVisits_OutP[MC_2018_OutP,
                                                            on = c(internal_member_id = 'internal_member_id',
                                                                   NPI = 'NPI')][Freq > 0]
group <- MC_OBGYN_2018_OutP_SS2
{
  tmp1 <- group[,
                list(Freq = length(unique(first_service_dt))),
                by = list(internal_member_id, NPI)]
  tmp2 <- tmp1[tmp1[, .I[Freq == max(Freq)], by = internal_member_id]$V1]
  out_unique <- tmp2[tmp2[, .I[length(unique(NPI))==1], by = internal_member_id]$V1]
  out_tie <- tmp2[tmp2[, .I[length(unique(NPI))>1], by = internal_member_id]$V1]
}
Patient_OBGYN_2018_UniqueDays_OutP <- out_unique[, Identify := 'MostDays_OutP']
fwrite(out_unique[, Identify := 'MostDays_OutP'],
       paste0(dir_out, 'Patient_OBGYN_2018_UniqueDays_OutP.csv'))
fwrite(out_tie,
       paste0(dir_out, 'Patient_OBGYN_2018_TieDays_OutP.csv'))
tmp <- data.table(matrix(c('MC_OBGYN_2018_Days_OutP',
                           length(unique(group$internal_member_id)),
                           dim(out_unique)[1],
                           dim(out_tie)[1],
                           length(unique(out_tie$internal_member_id)),
                           ratio <- dim(out_unique)[1]/
                             (dim(out_unique)[1] +
                                length(unique(out_tie$internal_member_id))),
                           max(out_tie$Freq)), nrow = 1))
names(tmp) <- names(info.summary)
info.summary <- rbind(info.summary, tmp)

## SS3.4 ##
Patient_OBGYN_2017_TieVisits_OutP <- fread(paste0(dir_out, 'Patient_OBGYN_2017_TieVisits_OutP.csv'),
                                           header = TRUE,
                                           colClasses = 'character',
                                           encoding = 'UTF-8')
MC_OBGYN_2017_OutP_SS2 <- Patient_OBGYN_2017_TieVisits_OutP[MC_2017_OutP,
                                                            on = c(internal_member_id = 'internal_member_id',
                                                                   NPI = 'NPI')][Freq > 0]
group <- MC_OBGYN_2017_OutP_SS2
{
  tmp1 <- group[,
                list(Freq = length(unique(first_service_dt))),
                by = list(internal_member_id, NPI)]
  tmp2 <- tmp1[tmp1[, .I[Freq == max(Freq)], by = internal_member_id]$V1]
  out_unique <- tmp2[tmp2[, .I[length(unique(NPI))==1], by = internal_member_id]$V1]
  out_tie <- tmp2[tmp2[, .I[length(unique(NPI))>1], by = internal_member_id]$V1]
}
Patient_OBGYN_2017_UniqueDays_OutP <- out_unique[, Identify := 'MostDays_OutP']
fwrite(out_unique[, Identify := 'MostDays_OutP'],
       paste0(dir_out, 'Patient_OBGYN_2017_UniqueDays_OutP.csv'))
fwrite(out_tie,
       paste0(dir_out, 'Patient_OBGYN_2017_TieDays_OutP.csv'))
tmp <- data.table(matrix(c('MC_OBGYN_2017_Days_OutP',
                           length(unique(group$internal_member_id)),
                           dim(out_unique)[1],
                           dim(out_tie)[1],
                           length(unique(out_tie$internal_member_id)),
                           ratio <- dim(out_unique)[1]/
                             (dim(out_unique)[1] +
                                length(unique(out_tie$internal_member_id))),
                           max(out_tie$Freq)), nrow = 1))
names(tmp) <- names(info.summary)
info.summary <- rbind(info.summary, tmp)
###### document summary ####
fwrite(info.summary,
       paste0(dir_out, 'summarySS3.csv'))


########## SS4 ##########
info.summary <- fread(paste0(dir_out, 'summarySS3.csv'), header = TRUE,
                      colClasses = 'character', encoding = 'UTF-8')

## SS4.1 ##
Patient_PCP_2018_TieDays_OutP <- fread(paste0(dir_out, 'Patient_PCP_2018_TieDays_OutP.csv'),
                                       header = TRUE,
                                       colClasses = 'character',
                                       encoding = 'UTF-8')
MC_PCP_2018_OutP_SS3 <- Patient_PCP_2018_TieDays_OutP[MC_2018_OutP,
                                                      on = c(internal_member_id = 'internal_member_id',
                                                             NPI = 'NPI')][Freq > 0]
group <- MC_PCP_2018_OutP_SS3
{
  tmp1 <- group[,
                list(RecentDate = max(ymd(first_service_dt))),
                by = list(internal_member_id, NPI)]
  tmp2 <- tmp1[tmp1[, .I[RecentDate == max(RecentDate)], by = internal_member_id]$V1]
  out_unique <- tmp2[tmp2[, .I[length(unique(NPI))==1], by = internal_member_id]$V1]
  out_tie <- tmp2[tmp2[, .I[length(unique(NPI))>1], by = internal_member_id]$V1]
}
Patient_PCP_2018_UniqueRecentDate_OutP <- out_unique[, Identify := 'RecentDate_OutP']
fwrite(out_unique[, Identify := 'RecentDate_OutP'],
       paste0(dir_out, 'Patient_PCP_2018_UniqueRecentDate_OutP.csv'))
Patient_PCP_2018_TieRecentDate_OutP <- out_tie
fwrite(out_tie,
       paste0(dir_out, 'Patient_PCP_2018_TieRecentDate_OutP.csv'))
tmp <- data.table(matrix(c('MC_PCP_2018_RecentDate_OutP',
                           length(unique(group$internal_member_id)),
                           dim(out_unique)[1],
                           dim(out_tie)[1],
                           length(unique(out_tie$internal_member_id)),
                           ratio <- dim(out_unique)[1]
                           /(dim(out_unique)[1] +
                               length(unique(out_tie$internal_member_id))),
                           as.character(max(out_tie$RecentDate))), nrow = 1))
names(tmp) <- names(info.summary)
info.summary <- rbind(info.summary, tmp)

## SS4.2 ##
Patient_PCP_2017_TieDays_OutP <- fread(paste0(dir_out, 'Patient_PCP_2017_TieDays_OutP.csv'),
                                       header = TRUE,
                                       colClasses = 'character',
                                       encoding = 'UTF-8')
MC_PCP_2017_OutP_SS3 <- Patient_PCP_2017_TieDays_OutP[MC_2017_OutP,
                                                      on = c(internal_member_id = 'internal_member_id',
                                                             NPI = 'NPI')][Freq > 0]
group <- MC_PCP_2017_OutP_SS3
{
  tmp1 <- group[,
                list(RecentDate = max(ymd(first_service_dt))),
                by = list(internal_member_id, NPI)]
  tmp2 <- tmp1[tmp1[, .I[RecentDate == max(RecentDate)], by = internal_member_id]$V1]
  out_unique <- tmp2[tmp2[, .I[length(unique(NPI))==1], by = internal_member_id]$V1]
  out_tie <- tmp2[tmp2[, .I[length(unique(NPI))>1], by = internal_member_id]$V1]
}
Patient_PCP_2017_UniqueRecentDate_OutP <- out_unique[, Identify := 'RecentDate_OutP']
fwrite(out_unique[, Identify := 'RecentDate_OutP'],
       paste0(dir_out, 'Patient_PCP_2017_UniqueRecentDate_OutP.csv'))
Patient_PCP_2017_TieRecentDate_OutP <- out_tie
fwrite(out_tie,
       paste0(dir_out, 'Patient_PCP_2017_TieRecentDate_OutP.csv'))
tmp <- data.table(matrix(c('MC_PCP_2017_RecentDate_OutP',
                           length(unique(group$internal_member_id)),
                           dim(out_unique)[1],
                           dim(out_tie)[1],
                           length(unique(out_tie$internal_member_id)),
                           ratio <- dim(out_unique)[1]/
                             (dim(out_unique)[1] +
                                length(unique(out_tie$internal_member_id))),
                           as.character(max(out_tie$RecentDate))), nrow = 1))
names(tmp) <- names(info.summary)
info.summary <- rbind(info.summary, tmp)

## SS4.3 ##
Patient_OBGYN_2018_TieDays_OutP <- fread(paste0(dir_out, 'Patient_OBGYN_2018_TieDays_OutP.csv'),
                                         header = TRUE,
                                         colClasses = 'character',
                                         encoding = 'UTF-8')
MC_OBGYN_2018_OutP_SS3 <- Patient_OBGYN_2018_TieDays_OutP[MC_2018_OutP,
                                                          on = c(internal_member_id = 'internal_member_id',
                                                                 NPI = 'NPI')][Freq > 0]
group <- MC_OBGYN_2018_OutP_SS3
{
  tmp1 <- group[,
                list(RecentDate = max(ymd(first_service_dt))),
                by = list(internal_member_id, NPI)]
  tmp2 <- tmp1[tmp1[, .I[RecentDate == max(RecentDate)], by = internal_member_id]$V1]
  out_unique <- tmp2[tmp2[, .I[length(unique(NPI))==1], by = internal_member_id]$V1]
  out_tie <- tmp2[tmp2[, .I[length(unique(NPI))>1], by = internal_member_id]$V1]
}
Patient_OBGYN_2018_UniqueRecentDate_OutP <- out_unique[, Identify := 'RecentDate_OutP']
fwrite(out_unique[, Identify := 'RecentDate_OutP'],
       paste0(dir_out, 'Patient_OBGYN_2018_UniqueRecentDate_OutP.csv'))
Patient_OBGYN_2018_TieRecentDate_OutP <- out_tie
fwrite(out_tie,
       paste0(dir_out, 'Patient_OBGYN_2018_TieRecentDate_OutP.csv'))
tmp <- data.table(matrix(c('MC_OBGYN_2018_RecentDate_OutP',
                           length(unique(group$internal_member_id)),
                           dim(out_unique)[1],
                           dim(out_tie)[1],
                           length(unique(out_tie$internal_member_id)),
                           ratio <- dim(out_unique)[1]/
                             (dim(out_unique)[1] +
                                length(unique(out_tie$internal_member_id))),
                           as.character(max(out_tie$RecentDate))), nrow = 1))
names(tmp) <- names(info.summary)

info.summary <- rbind(info.summary, tmp)

## SS4.4 ##
Patient_OBGYN_2017_TieDays_OutP <- fread(paste0(dir_out, 'Patient_OBGYN_2017_TieDays_OutP.csv'),
                                         header = TRUE,
                                         colClasses = 'character',
                                         encoding = 'UTF-8')
MC_OBGYN_2017_OutP_SS3 <- Patient_OBGYN_2017_TieDays_OutP[MC_2017_OutP,
                                                          on = c(internal_member_id = 'internal_member_id',
                                                                 NPI = 'NPI')][Freq > 0]
group <- MC_OBGYN_2017_OutP_SS3
{
  tmp1 <- group[,
                list(RecentDate = max(ymd(first_service_dt))),
                by = list(internal_member_id, NPI)]
  tmp2 <- tmp1[tmp1[, .I[RecentDate == max(RecentDate)], by = internal_member_id]$V1]
  out_unique <- tmp2[tmp2[, .I[length(unique(NPI))==1], by = internal_member_id]$V1]
  out_tie <- tmp2[tmp2[, .I[length(unique(NPI))>1], by = internal_member_id]$V1]
}
Patient_OBGYN_2017_UniqueRecentDate_OutP <- out_unique[, Identify := 'RecentDate_OutP']
fwrite(out_unique[, Identify := 'RecentDate_OutP'],
       paste0(dir_out, 'Patient_OBGYN_2017_UniqueRecentDate_OutP.csv'))
Patient_OBGYN_2017_TieRecentDate_OutP <- out_tie
fwrite(out_tie,
       paste0(dir_out, 'Patient_OBGYN_2017_TieRecentDate_OutP.csv'))
tmp <- data.table(matrix(c('MC_OBGYN_2017_RecentDate_OutP',
                           length(unique(group$internal_member_id)),
                           dim(out_unique)[1],
                           dim(out_tie)[1],
                           length(unique(out_tie$internal_member_id)),
                           ratio <- dim(out_unique)[1]/
                             (dim(out_unique)[1] +
                                length(unique(out_tie$internal_member_id))),
                           as.character(max(out_tie$RecentDate))), nrow = 1))
names(tmp) <- names(info.summary)

info.summary <- rbind(info.summary, tmp)

###### document summary ####
fwrite(info.summary,
       paste0(dir_out, 'summarySS4.csv'))



### finish Patient Attribution Matrix
Patient_PCP_2018_UniqueRecentDate_OutP <- fread(paste0(dir_out, 'Patient_PCP_2018_UniqueRecentDate_OutP.csv'),
                                                header = TRUE,
                                                colClasses = 'character',
                                                encoding = 'UTF-8')
Patient_PCP_2017_UniqueRecentDate_OutP <- fread(paste0(dir_out, 'Patient_PCP_2017_UniqueRecentDate_OutP.csv'),
                                                header = TRUE,
                                                colClasses = 'character',
                                                encoding = 'UTF-8')
Patient_OBGYN_2018_UniqueRecentDate_OutP <- fread(paste0(dir_out, 'Patient_OBGYN_2018_UniqueRecentDate_OutP.csv'),
                                                  header = TRUE,
                                                  colClasses = 'character',
                                                  encoding = 'UTF-8')
Patient_OBGYN_2017_UniqueRecentDate_OutP <- fread(paste0(dir_out, 'Patient_OBGYN_2017_UniqueRecentDate_OutP.csv'),
                                                  header = TRUE,
                                                  colClasses = 'character',
                                                  encoding = 'UTF-8')

Patient_Unique <- rbind(Patient_PCP_2018_UniqueVisits_EM, Patient_PCP_2018_UniqueVisits_OutP, Patient_PCP_2018_UniqueDays_OutP, Patient_PCP_2018_UniqueRecentDate_OutP,
                        Patient_PCP_2017_UniqueVisits_EM, Patient_PCP_2017_UniqueVisits_OutP, Patient_PCP_2017_UniqueDays_OutP, Patient_PCP_2017_UniqueRecentDate_OutP,
                        Patient_OBGYN_2018_UniqueVisits_EM, Patient_OBGYN_2018_UniqueVisits_OutP, Patient_OBGYN_2018_UniqueDays_OutP, Patient_OBGYN_2018_UniqueRecentDate_OutP,
                        Patient_OBGYN_2017_UniqueVisits_EM, Patient_OBGYN_2017_UniqueVisits_OutP, Patient_OBGYN_2017_UniqueDays_OutP, Patient_OBGYN_2017_UniqueRecentDate_OutP,
                        fill = TRUE)
Patient_classification1 <- Patient_Unique[Patient_classification, on = c(internal_member_id = 'internal_member_id')]
fwrite(Patient_classification1,
       paste0(dir_out, 'PatientAttri_short.csv'))

### Ties left
Patient_Tie <- rbind(
  Patient_PCP_2018_TieRecentDate_OutP,
  Patient_PCP_2017_TieRecentDate_OutP,
  Patient_OBGYN_2018_TieRecentDate_OutP,
  Patient_OBGYN_2017_TieRecentDate_OutP,
  fill = TRUE
)
fwrite(Patient_Tie,
       paste0(dir_out, 'PatientTie.csv'))


#### Taxonomy/Specialty Freq Research ####
Patient_classification2 <- Patient_classification1
# Patient_classification2 <- nppes0[Patient_classification1, on = c(NPI = 'NPI')]
# fwrite(as.data.table(sort(table(Patient_classification2$Taxonomy1), decreasing = TRUE)),
#        paste0(dir_out, 'PatientToTaxonomy.csv'))
# Patient_classification2[Taxonomy1 %in% taxo_FP, Specialty := 'FP']
# Patient_classification2[Taxonomy1 %in% taxo_GP, Specialty := 'GP']
# # Patient_classification2[Taxonomy1 %in% taxo_GM, Specialty := 'GM'] # by Sai
# Patient_classification2[Taxonomy1 %in% taxo_IM, Specialty := 'IM']
# Patient_classification2[Taxonomy1 %in% taxo_PedM, Specialty := 'PedM']
# Patient_classification2[Taxonomy1 %in% taxo_NP, Specialty := 'NP']
# # Patient_classification2[Taxonomy1 %in% taxo_PreM, Specialty := 'PreM'] # by Sai
# Patient_classification2[Taxonomy1 %in% taxo_CCNS, Specialty := 'CCNS']
# Patient_classification2[Taxonomy1 %in% taxo_PA, Specialty := 'PA']
# Patient_classification2[Taxonomy1 %in% taxo_OBGYN, Specialty := 'OBGYN']
# Patient_classification2[(!Taxonomy1 %in% c(taxo_PCP, taxo_OBGYN)) &
#                           (!is.na(Taxonomy1)), Specialty := 'Others']
setcolorder(Patient_classification2,
            c('internal_member_id', 'Attribution', 'NPI',
              'Identify', 
              # 'Specialty', 
              # 'Taxonomy1',
              'Freq', 'RecentDate'))
fwrite(as.data.table(sort(table(Patient_classification2$Specialty), decreasing = TRUE)),
       paste0(dir_out, 'PatientToSpecialty.csv'))

fwrite(Patient_classification2,
       paste0(dir_out, 'PatAttri_detail.csv'))

fwrite(Patient_classification2[Attribution == 'Unattributed',
                               Identify := 'Unattributed'][Identify == "", Identify := 'Tie'],
       paste0(dir_out, 'PatAttri_detail.csv'))



#### Research for the Unattributed Par
## taxoClass <- fread('E:/CT_APCD/Hongfei/files/taxonomy classification.csv',
##                    header = TRUE,
##                    colClasses = 'character',
##                    encoding = 'UTF-8')

## NPI_freq_unattr <- head(sort(table(MC_unattr_2018$NPI), decreasing = TRUE), n = 50)
## NPI_freq_unattr <- nppes0[as.data.table(NPI_freq_unattr), on = c(NPI = 'V1')][, .(NPI, Taxonomy1, N)]
## NPI_freq_unattr <- taxoClass[NPI_freq_unattr, on = c(taxonomy1 = 'Taxonomy1')][, .(NPI, N, taxonomy1, Grouping, Classification, Specialization, Definition)]
## write.csv(NPI_freq_unattr,
##           'file:///E:/CT_APCD/Hongfei/PatientAttribution/NPI_Freq_Unattr.csv')

## NPI_unattr <- data.table(NPI = MC_unattr_2018$NPI)
## taxo_freq_unattr_NPI <- head(sort(table(nppes0[NPI_unattr, on = c(NPI = 'NPI')]$Taxonomy1), decreasing = T), n = 50)
## taxo_freq_unattr_NPI <- taxoClass[as.data.table(taxo_freq_unattr_NPI), on = c(taxonomy1 = 'V1')][, .(taxonomy1, N, Grouping, Classification, Specialization, Definition)]
## write.csv(taxo_freq_unattr_NPI,
##           'file:///E:/CT_APCD/Hongfei/PatientAttribution/TaxoFreq_Unattr_NPI.csv')

## uniqueNPI_unattr <- data.table(NPI = unique(MC_unattr_2018$NPI))
## taxo_freq_unattr_uniqueNPI <- head(sort(table(nppes0[uniqueNPI_unattr, on = c(NPI = 'NPI')]$Taxonomy1), decreasing = T), n = 50)
## taxo_freq_unattr_uniqueNPI <- taxoClass[as.data.table(taxo_freq_unattr_uniqueNPI), on = c(taxonomy1 = 'V1')][, .(taxonomy1, N, Grouping, Classification, Specialization, Definition)]
## write.csv(taxo_freq_unattr_uniqueNPI,
##           'file:///E:/CT_APCD/Hongfei/PatientAttribution/TaxoFreq_Unattr_uniqueNPI.csv')
