library(methods)
source("e:/CT_APCD/Sai/data-organization/R/utils.R")
pkgs <- c("data.table", "tidyverse", "reshape2", "readxl",
          NULL)
need.packages(pkgs)
outdir <- output_dir("NPI-Attribution/")
cols <- c("NPI",
          # "LastName",
          # "FirstName",
          "Taxonomy1",
          "spec0",
          # "Specialty",
          "entityName")

## FQHC
fqhcdir <- input_dir("E:/CT_APCD/Sai/MedicareProvider_ Lists_2018_2019/2019Lists_ForAttribution/2019Lists_ForAttribution/FQHC_Commercial_Medicare_2019/")
fqhcfiles <- list.files(fqhcdir)

## 1
fqhcfiles[1]
fqhc1 <- data.table(readxl::read_excel(paste0(fqhcdir, 
                                            "CHCINC_2018_2019_FinalForAttribution.xlsx")))
names(fqhc1)
fqhc1[, `:=` (NPI = as.character(`Clinician NPI (Type 1) (Required)`),
            entityName = "CHCINC",
            Taxonomy1 = taxo)]
fqhc1 <- fqhc1[, cols, with = FALSE]
head(fqhc1)

## 2
fqhcfiles[2]
fqhc2 <-data.table(readxl::read_excel(paste0(fqhcdir, 
                                           "CHSHartford_2019_FinalForAttribution.xlsx")))
names(fqhc2)
fqhc2[, `:=` (NPI = as.character(NPI),
            entityName = "CHSHartford",
            Taxonomy1 = taxo)]
fqhc2 <- fqhc2[, cols, with = FALSE]
head(fqhc2)

## 3
fqhcfiles[3]
fqhc3 <-data.table(readxl::read_excel(paste0(fqhcdir, 
                                           "CHWTorrington_Provider Listing 2019_FinalForAttribution.xls")))
names(fqhc3)
fqhc3[, `:=` (NPI = as.character(NPI),
            entityName = "CHWTorrington",
            Taxonomy1 = taxo)]
fqhc3 <- fqhc3[, cols, with = FALSE]
head(fqhc3)

## 4
fqhcfiles[4]
fqhc4 <-data.table(readxl::read_excel(paste0(fqhcdir, 
                                           "CIFC Danbury_HealthCare Quality Scorecard_2019_FinalForAttribution.xlsx")))
names(fqhc4)
fqhc4[, `:=` (NPI = as.character(`Provider NPI`),
            entityName = "CIFC",
            Taxonomy1 = taxo)]
fqhc4 <- fqhc4[, cols, with = FALSE]
head(fqhc4)

## 5
fqhcfiles[5]
fqhc5 <-data.table(readxl::read_excel(paste0(fqhcdir, 
                                             "CornellScottHC Medical Providers 2019_FinalForAttribution.xlsx")))

names(fqhc5)
fqhc5[, `:=` (NPI = as.character(NPI),
            entityName = "CornellScottHC",
            Taxonomy1 = taxo)]
fqhc5 <- fqhc5[, cols, with = FALSE]
head(fqhc5)

## 6
fqhcfiles[6]
fqhc6 <-data.table(readxl::read_excel(paste0(fqhcdir, 
                                           "Fair Haven Provider List 2019_FinalForAttributio.xlsx")))
names(fqhc6)
fqhc6[, `:=` (NPI = as.character(`Clinician NPI (Type 1) (Required)`),
            entityName = "Fairhaven",
            Taxonomy1 = taxo)]
fqhc6 <- fqhc6[, cols, with = FALSE]
head(fqhc6)

## 7
fqhcfiles[7]
fqhc7 <-data.table(readxl::read_excel(paste0(fqhcdir, 
                                           "FQHC Provider NPI Lists by Entity_CharterOak_FirstChoice_2017_UseFor_2018_2019 (1).xlsx")))
names(fqhc7)
fqhc7[, `:=` (NPI = as.character(NPI),
            entityName = "CharterOak",
            Taxonomy1 = taxo, 
            spec0 = new.spec0)]
fqhc7 <- fqhc7[, cols, with = FALSE]
head(fqhc7)

## 8
fqhcfiles[8]
fqhc8 <-data.table(readxl::read_excel(paste0(fqhcdir, 
                                           "Generations_2019_FinalForAttribution.xlsx")))
names(fqhc8)
fqhc8[, `:=` (NPI = as.character(`Clinician NPI (Type 1) (Required)`),
            entityName = "Generations",
            Taxonomy1 = taxo)]
fqhc8 <- fqhc8[, cols, with = FALSE]
head(fqhc8)

## 9
fqhcfiles[9]
fqhc9 <-data.table(read_excel(paste0(fqhcdir, 
                                   "Intercommunity_primary providers for 2018 2019_FinalForAttribution.xlsx")))
names(fqhc9)
fqhc9[, `:=` (NPI = as.character(NPI),
            entityName = "Intercommunity",
            Taxonomy1 = taxo)]
fqhc9 <- fqhc9[, cols, with = FALSE]
head(fqhc9)

## 10
fqhcfiles[10]
fqhc10 <-data.table(read_excel(paste0(fqhcdir, 
                                    "MPS_2019_FinalForAttribution.xlsx")))
names(fqhc10)
fqhc10[, `:=` (NPI = as.character(`NPI #`),
             entityName = "MPS",
             Taxonomy1 = taxo)]
fqhc10 <- fqhc10[, cols, with = FALSE]
head(fqhc10)

## 11
fqhcfiles[11]
fqhc11 <-data.table(read_excel(paste0(fqhcdir, 
                                    "Norwalk_2019_FinalForAttribution.xlsx")))
names(fqhc11)
fqhc11[, `:=` (NPI = as.character(`Clinician NPI (Type 1) (Required)`),
             entityName = "Norwalk",
             Taxonomy1 = taxo)]
fqhc11 <- fqhc11[, cols, with = FALSE]
head(fqhc11)

## 12
fqhcfiles[12]
fqhc12 <-data.table(read_excel(paste0(fqhcdir, 
                                    "OptimusProvider List_2019_FinalForAttribution.xlsx")))
names(fqhc12)
fqhc12[, `:=` (NPI = as.character(`Clinician NPI (Type 1) (Required)`),
             entityName = "Optimus",
             Taxonomy1 = taxo)]
fqhc12 <- fqhc12[, cols, with = FALSE]
head(fqhc12)

## 13
fqhcfiles[13]
fqhc13 <-data.table(read_excel(paste0(fqhcdir, 
                                    "SouthwestCHC providers List_2019_FinalForAttribution.xlsx")))
names(fqhc13)
fqhc13[, `:=` (NPI = as.character(`Clinician NPI (Type 1) (Required)`),
             entityName = "SouthwestCHC",
             Taxonomy1 = taxo)]
fqhc13 <- fqhc13[, cols, with = FALSE]
head(fqhc13)

## 14
fqhcfiles[14]
fqhc14 <-data.table(read_excel(paste0(fqhcdir, 
                                    "StaywellOnline Healthcare Quality Scorecard Provider Lists_2019_FinalForAttribution.xlsx")))
names(fqhc14)
fqhc14[, `:=` (NPI = as.character(NPI),
             entityName = "Staywell",
             Taxonomy1 = taxo)]
fqhc14 <- fqhc14[, cols, with = FALSE]
head(fqhc14)

## 15
fqhcfiles[15]
fqhc15 <-data.table(read_excel(paste0(fqhcdir, 
                                    "UCFSHealthcare_2019_FinalForAttribution.xlsx")))
names(fqhc15)
fqhc15[, `:=` (NPI = as.character(`Clinician NPI (Type 1) (Required)`),
             entityName = "UCFS",
             Taxonomy1 = taxo)]
fqhc15 <- fqhc15[, cols, with = FALSE]
head(fqhc15)

## 16
fqhcfiles[16]
fqhc16 <-data.table(read_excel(paste0(fqhcdir, 
                                    "Wheeler_2019_FinalForAttribution.xlsx")))
names(fqhc16)
fqhc16[, `:=` (NPI = as.character(npi),
             entityName = "Wheele",
             Taxonomy1 = taxo)]
fqhc16 <- fqhc16[, cols, with = FALSE]
head(fqhc16)

## all fqhc
fqhc <- rbind(fqhc1, fqhc2, fqhc3, fqhc4, fqhc5, fqhc6, fqhc7, fqhc8, fqhc9, fqhc10, 
            fqhc11, fqhc12, fqhc13, fqhc14, fqhc15, fqhc16)
fqhc[, entityName := paste0("FQHC_", entityName)]


## AN
andir <- input_dir("E:/CT_APCD/Sai/MedicareProvider_ Lists_2018_2019/2019Lists_ForAttribution/2019Lists_ForAttribution/AN_Medicare_2019/")
anfiles <- list.files(andir)
length(anfiles)

## 1
anfiles[1]
an1 <- data.table(readxl::read_excel(paste0(andir, 
                                              "2019 Middlesex Providers_FinalForAttribution.xlsx")))
names(an1)
an1[, `:=` (NPI = as.character(NPI),
              entityName = "Middlesex",
              Taxonomy1 = taxo)]
an1 <- an1[, cols, with = FALSE]
head(an1)

## 2
anfiles[2]
an2 <-data.table(readxl::read_excel(paste0(andir, 
                                             "AllianceWaterbury_2019_FinalForAttribution.xlsx")))
names(an2)
an2[, `:=` (NPI = as.character(NPI),
              entityName = "AllianceWaterbury",
              Taxonomy1 = taxo)]
an2 <- an2[, cols, with = FALSE]
head(an2)

## 3
anfiles[3]
an3 <-data.table(readxl::read_excel(paste0(andir, 
                                             "CMG_2019_Commercial_Medicare_FinalForAttribution.xls")))
names(an3)
an3[, `:=` (NPI = as.character(NPI),
              entityName = "CMG",
              Taxonomy1 = taxo)]
an3 <- an3[, cols, with = FALSE]
head(an3)

## 4
anfiles[4]
an4 <-data.table(readxl::read_excel(paste0(andir, 
                                             "DAYKIMBALL_ Provider NPI Lists_CommericalMedicare2019_FinalForAttribution.xlsx")))
names(an4)
an4[, `:=` (NPI = as.character(NPI),
              entityName = "DAYKIMBAL",
              Taxonomy1 = taxo)]
an4 <- an4[, cols, with = FALSE]
head(an4)

## 5
anfiles[5]
an5 <-data.table(readxl::read_excel(paste0(andir, 
                                             "EasternCTHealthNetwork_2019_FinalForAttribution.xlsx")))

names(an5)
an5[, `:=` (NPI = as.character(NPI),
              entityName = "ECHN",
              Taxonomy1 = taxo)]
an5 <- an5[, cols, with = FALSE]
head(an5)

## 6
anfiles[6]
an6 <-data.table(readxl::read_excel(paste0(andir, 
                                             "GriffinProviders_2018_2019_FinalForAttribution.xlsx")))
names(an6)
an6[, `:=` (NPI = as.character(`NPI #`),
              entityName = "Griffin",
              Taxonomy1 = taxo_Uconn,
            spec0 = spec0_Uconn)]
an6 <- an6[, cols, with = FALSE]
head(an6)

## 7
anfiles[7]
an7 <-data.table(readxl::read_excel(paste0(andir, 
                                             "HHCICP_C_MC_2019_FinalForAttribution.xlsx")))
names(an7)
an7[, `:=` (NPI = as.character(NPI),
              entityName = "HHCIC",
              Taxonomy1 = taxo)]
an7 <- an7[, cols, with = FALSE]
head(an7)

## 8
anfiles[8]
an8 <-data.table(readxl::read_excel(paste0(andir, 
                                             "MPS_2019_FinalForAttribution.xlsx")))
names(an8)
an8[, `:=` (NPI = as.character(`NPI #`),
              entityName = "MPS",
              Taxonomy1 = taxo)]
an8 <- an8[, cols, with = FALSE]
head(an8)

## 9
anfiles[9]
an9 <-data.table(read_excel(paste0(andir, 
                                     "NEMedicalGroup_C_MC_2019_FinalForAttribution.xlsx")))
names(an9)
an9[, `:=` (NPI = as.character(NPI),
              entityName = "NEMG",
              Taxonomy1 = taxo)]
an9 <- an9[, cols, with = FALSE]
head(an9)

## 10   ## Practitioner NPI
anfiles[10]
an10 <-data.table(read_excel(paste0(andir, 
                                      "ProHealth_PCPs as of 12.31.2019_FinalForAttribution.xlsx")))
names(an10)
an10[, `:=` (NPI = as.character(`Practitioner NPI`),
               entityName = "ProHealth",
               Taxonomy1 = taxo)]
an10 <- an10[, cols, with = FALSE]
head(an10)

## 11
anfiles[11]
an11 <-data.table(read_excel(paste0(andir, 
                                      "Soundview__CommericalMedicare_2019_FinalForAttribution.xlsx")))
names(an11)
an11[, `:=` (NPI = as.character(NPI),
               entityName = "Soundview",
               Taxonomy1 = taxo)]
an11 <- an11[, cols, with = FALSE]
head(an11)

## 12  # No NPI
anfiles[12]
an12 <-data.table(read_excel(paste0(andir, 
                                      "Stamford_2019_FinalForAttribution.xlsx")))
names(an12)
an12[, `:=` (NPI = as.character(NPI),
               entityName = "Stamford",
               Taxonomy1 = taxo_Uconn)]
an12 <- an12[, cols, with = FALSE]
head(an12)

## 13
anfiles[13]
an13 <-data.table(read_excel(paste0(andir, 
                                      "Starling_2019_FinalForAttribution.xlsx")))
names(an13)
an13[, `:=` (NPI = as.character(NPI),
               entityName = "Starling",
               Taxonomy1 = taxo)]
an13 <- an13[, cols, with = FALSE]
head(an13)

## 14
anfiles[14]
an14 <-data.table(read_excel(paste0(andir, 
                                      "STFrancis Provider List 2018_19_FinalForAttribution.xlsx")))
names(an14)
an14[, `:=` (NPI = as.character(NPI),
               entityName = "STFrancis",
               Taxonomy1 = taxo)]
an14 <- an14[, cols, with = FALSE]
head(an14)

## 15
anfiles[15]
an15 <-data.table(read_excel(paste0(andir, 
                                      "StMary_2018_2019_Medicare_FinalForAttribution.xlsx")))
names(an15)
an15[, `:=` (NPI = as.character(NPI),
               entityName = "StMary",
               Taxonomy1 = taxo)]
an15 <- an15[, cols, with = FALSE]
head(an15)

## 16
anfiles[16]
an16 <-data.table(read_excel(paste0(andir, 
                                      "STVincent_C_MC_2019_FinalForAttribution.xlsx")))
names(an16)
an16[, `:=` (NPI = as.character(NPI),
               entityName = "STVincen",
               Taxonomy1 = taxo)]
an16 <- an16[, cols, with = FALSE]
head(an16)

## 17
anfiles[17]
an17 <-data.table(read_excel(paste0(andir, 
                                    "WesternCTHealthNetwork_Medicare_2019_FinalForAttibution.xlsx")))
names(an17)
an17[, `:=` (NPI = as.character(NPI),
             entityName = "WCHN",
             Taxonomy1 = taxo)]
an17 <- an17[, cols, with = FALSE]
head(an17)

## 18
anfiles[18]
an18 <-data.table(read_excel(paste0(andir, 
                                    "WestMedHistorical Roster 2019_FinalForAttribution.xlsx")))
names(an18)
an18[, `:=` (NPI = as.character(`NPI #`),
             entityName = "WestMedHistorical",
             Taxonomy1 = taxo)]
an18 <- an18[, cols, with = FALSE]
head(an18)

## 19
anfiles[19]
an19 <-data.table(read_excel(paste0(andir, 
                                    "Yale_Commerical_Medicare_2019_FinalForAttribution.xlsx")))
names(an19)
an19[, `:=` (NPI = as.character(NPI_NUMBER),
             entityName = "Yale",
             Taxonomy1 = taxo, spec0 = Spec0)]
an19 <- an19[, cols, with = FALSE]
head(an19)

## all an
an <- rbind(an1, an2, an3, an4, an5, an6, an7, an8, an9, an10, 
              an11, an12, an13, an14, an15, an16, an17, an18, an19)
an[, entityName := paste0("AN_", entityName)]

## combine two tables
datall <- rbind(an, fqhc)[, value := 1]
datall <- datall[!is.na(NPI)]
datall <- datall[NPI != 0]
entitynames <- as.character(datall[, unique(entityName)])
othercolnames <- c("NPI",
                   "Taxonomy1")
dat_wide <- dcast(datall, 
                  NPI + Taxonomy1 + spec0 ~ entityName) %>%
  data.table
dat_wide[, Total := rowSums(.SD, na.rm = TRUE),
         .SDcols = entitynames,
         by = othercolnames]
dat_wide[is.na(dat_wide)] <- 0
# nps <- fread("E:/CT_APCD/shared/intermediate_data/APCD_modified/provider/nppes_1129.csv",
#             colClasses = "character",
#             select = c("NPI",
#                        "Taxonomy1"))
# tmp <- nps[NPI %in% dat_wide[Taxonomy1 == 0]$NPI]
# dat_wide[Taxonomy1 == 0] <- nps[dat_wide[Taxonomy1 == 0], on = "NPI"]
fwrite(dat_wide, 
       file = paste0(outdir, "NPI_WholeSet_2019_mc.csv"))



## modify nppes
nppes <- fread("E:/CT_APCD/shared/intermediate_data/APCD_modified/provider/nppes_0613.csv",
               colClasses = "character",
               header = TRUE,
               encoding = "UTF-8")
add1 <- datall[!NPI %in% nppes$NPI]
add_names1 <- names(add1)[names(add1) %in% names(nppes)]
add1 <- add1[, add_names1, with = FALSE]
nppes_new <- rbind(nppes, add1, fill = TRUE)
fwrite(nppes_new,
       file = "E:/CT_APCD/shared/intermediate_data/APCD_modified/provider/nppes_1129.csv")


## modify nppes
provider <- fread("E:/CT_APCD/shared/intermediate_data/APCD_modified/provider/provider_0613.csv",
               colClasses = "character",
               header = TRUE,
               encoding = "UTF-8")
add2 <- datall[!NPI %in% provider$NPI]
add_names2 <- names(add2)[names(add2) %in% names(provider)]
add2 <- add2[, add_names2, with = FALSE]
provider_new <- rbind(provider, add2, fill = TRUE)
fwrite(provider_new,
       file = "E:/CT_APCD/shared/intermediate_data/APCD_modified/provider/provider_1129.csv")
