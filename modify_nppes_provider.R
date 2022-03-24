library(methods)
source("E:/CT_APCD/Sai/data-organization/R/utils.R")
need_pkgs <- c("data.table", "bit64", "tools",
               "dplyr", "lubridate", "tidyverse", "haven",
               "readxl")
need.packages(need_pkgs)
taxo_info <- rbind(data.table('taxo' = '207Q00000X', 'spec1' = 'FP', 'reference' = 'CMS08',
                              'description' = 'Allopathic & Osteopathic Physicians/Family Medicine'),
                   data.table('taxo' = '207QA0000X', 'spec1' = 'FP', 'reference' = 'CMS08',
                              'description' = 'Allopathic & Osteopathic Physicians/Family Medicine, Adolescent Medicine'),
                   data.table('taxo' = '207QA0505X', 'spec1' = 'FP', 'reference' = 'CMS08',
                              'description' = 'Allopathic & Osteopathic Physicians/Family Medicine, Adult Medicine'),
                   data.table('taxo' = '207QG0300X', 'spec1' = 'FP', 'reference' = 'CMS08',
                              'description' = 'Allopathic & Osteopathic Physicians/Family Medicine, Geriatric Medicine'),
                   data.table('taxo' = '208D00000X', 'spec1' = 'GP', 'reference' = 'CMS01',
                              'description' = 'Allopathic & Osteopathic Physicians/General Practice'),
                   data.table('taxo' = '207R00000X', 'spec1' = 'IM', 'reference' = 'CMS11',
                              'description' = 'Allopathic & Osteopathic Physicians/Internal Medicine'),
                   data.table('taxo' = '207RA0000X', 'spec1' = 'IM', 'reference' = 'CMS11',
                              'description' = 'Allopathic & Osteopathic Physicians/Internal Medicine, Adolescent Medicine'),
                   data.table('taxo' = '207RG0300X', 'spec1' = 'IM', 'reference' = 'CMS11',
                              'description' = 'Allopathic & Osteopathic Physicians/Internal Medicine, Geriatric Medicine'),
                   data.table('taxo' = '208000000X', 'spec1' = 'PedM', 'reference' = 'CMS37',
                              'description' = 'Allopathic & Osteopathic Physicians/Pediatrics'),
                   data.table('taxo' = '2080A0000X', 'spec1' = 'PedM', 'reference' = 'CMS37',
                              'description' = 'Allopathic & Osteopathic Physicians/Pediatrics, Adolescent Medicine'),
                   data.table('taxo' = '363L00000X', 'spec1' = 'NP', 'reference' = 'CMS50',
                              'description' = 'Physician Assistants & Advanced Practice Nursing Providers/Nurse Practitioner'),
                   data.table('taxo' = '363LA2200X', 'spec1' = 'NP', 'reference' = 'CMS50',
                              'description' = 'Physician Assistants & Advanced Practice Nursing Providers/Nurse Practitioner, Adult Health'),
                   data.table('taxo' = '363LC1500X', 'spec1' = 'NP', 'reference' = 'CMS50',
                              'description' = 'Physician Assistants & Advanced Practice Nursing Providers/Nurse Practitioner, Community Health'),
                   data.table('taxo' = '363LF0000X', 'spec1' = 'NP', 'reference' = 'CMS50',
                              'description' = 'Physician Assistants & Advanced Practice Nursing Providers/Nurse Practitioner, Family'),
                   data.table('taxo' = '363LG0600X', 'spec1' = 'NP', 'reference' = 'CMS50',
                              'description' = 'Physician Assistants & Advanced Practice Nursing Providers/Nurse Practitioner, Gerontology'),
                   data.table('taxo' = '363LP0200X', 'spec1' = 'NP', 'reference' = 'CMS50',
                              'description' = 'Physician Assistants & Advanced Practice Nursing Providers/Nurse Practitioner, Pediatrics'),
                   data.table('taxo' = '363LP2300X', 'spec1' = 'NP', 'reference' = 'CMS50',
                              'description' = 'Physician Assistants & Advanced Practice Nursing Providers/Nurse Practitioner, Primary Care'),
                   data.table('taxo' = '364S00000X', 'spec1' = 'CCNS', 'reference' = 'CMS89',
                              'description' = 'Physician Assistants & Advanced Practice Nursing Providers/Clinical Nurse Specialist'),
                   data.table('taxo' = '364SA2200X', 'spec1' = 'CCNS', 'reference' = 'CMS89',
                              'description' = 'Physician Assistants & Advanced Practice Nursing Providers/Clinical Nurse Specialist, Adult Health'),
                   data.table('taxo' = '364SC1501X', 'spec1' = 'CCNS', 'reference' = 'CMS89',
                              'description' = 'Physician Assistants & Advanced Practice Nursing Providers/Clinical Nurse Specialist, Community Health/Public Health'),
                   data.table('taxo' = '364SF0001X', 'spec1' = 'CCNS', 'reference' = 'CMS89',
                              'description' = 'Physician Assistants & Advanced Practice Nursing Providers/Clinical Nurse Specialist, Family Health'),
                   data.table('taxo' = '364SG0600X', 'spec1' = 'CCNS', 'reference' = 'CMS89',
                              'description' = 'Physician Assistants & Advanced Practice Nursing Providers/Clinical Nurse Specialist, Gerontology'),
                   data.table('taxo' = '364SP0200X', 'spec1' = 'CCNS', 'reference' = 'CMS89',
                              'description' = 'Physician Assistants & Advanced Practice Nursing Providers/Clinical Nurse Specialist, Pediatrics'),
                   data.table('taxo' = '363A00000X', 'spec1' = 'PA', 'reference' = 'CMS97',
                              'description' = 'Physician Assistants & Advanced Practice Nursing Providers/Physician Assistant'),
                   data.table('taxo' = '363AM0700X', 'spec1' = 'PA', 'reference' = 'CMS97',
                              'description' = 'Physician Assistants & Advanced Practice Nursing Providers/Physician Assistant, Medical'),
                   data.table('taxo' = '207V00000X', 'spec1' = 'OBGYN', 'reference' = 'CMS16',
                              'description' = 'Allopathic & Osteopathic Physicians/Obstetrics & Gynecology'),
                   data.table('taxo' = '207VG0400X', 'spec1' = 'OBGYN', 'reference' = 'CMS16',
                              'description' = 'Allopathic & Osteopathic Physicians/Obstetrics & Gynecology, Gynecology'),
                   data.table('taxo' = '207VX0000X', 'spec1' = 'OBGYN', 'reference' = 'CMS16',
                              'description' = 'Allopathic & Osteopathic Physicians/Obstetrics & Gynecology, Obstetrics'),
                   data.table('taxo' = '363LX0001X', 'spec1' = 'OBGYN', 'reference' = 'sup',
                              'description' = 'Physician Assistants & Advanced Practice Nursing Providers/Nurse Practitioner, Obstetrics & Gynecology'),
                   data.table('taxo' = '363LW0102X', 'spec1' = 'OBGYN', 'reference' = 'sup',
                              'description' = 'Physician Assistants & Advanced Practice Nursing Providers/Nurse Practitioner, Women’s Health'),
                   data.table('taxo' = '364SW0102X', 'spec1' = 'OBGYN', 'reference' = 'sup',
                              'description' = 'Physician Assistants & Advanced Practice Nursing Providers/Clinical Nurse Specialist, Women’s Health'),
                   NULL # placeholder for comments
)
taxo_info[spec1 %in% c('FP',
                       'GP',
                       'IM',
                       'PedM',
                       'NP',
                       'CCNS',
                       'PA'),
          spec0 := 'PCP'][spec1 %in% c('OBGYN'),
                          spec0 := 'OBGYN']

pvd <- rbindlist(lapply(list.files("E:/CT_APCD/LDS_4_22_21/Downloaded_061021/268_provider_1/",
                                   full.names = TRUE)[1:4], function(x){
                                     fread(x, colClasses = "character", 
                                           select = c("provider_id","orig_npi"))
                                   })) %>% unique(use.key = FALSE)

setnames(pvd, "primary_taxonomy", "taxo")
setnames(pvd, "orig_npi", "NPI")
fwrite(pvd, 
       file = "E:/CT_APCD/Sai/lds2021/lds2021file/nppes/provider_small.csv")
nppes2017 <- fread("E:/CT_APCD/Sai/lds2021/lds2021file/nppes/nppes_lds_com_2017.csv",
                colClasses = "character",
                header = TRUE,
                encoding = "UTF-8")
sum(!unique(pvd$NPI) %in% unique(nppes2017$NPI))
pvd2017_1 <- pvd[NPI %in% unique(nppes2017$NPI)]
pvd2017_2 <- pvd[!NPI %in% unique(nppes2017$NPI)]
pvd2017_2 <- taxo_info[pvd2017_2, on = "taxo"]
pvd2017_2 <- pvd2017_2[, -"provider_id"]
pvd2017_2[, `:=`(Type = 1, primary = 1, 
                 Taxonomy_order = 1)]
nppes2017 <- rbind(nppes2017, pvd2017_2)
fwrite(nppes2017,
       file = "E:/CT_APCD/Sai/lds2021/lds2021file/nppes/nppes_lds_com_2017.csv")


nppes2018 <- fread("E:/CT_APCD/Sai/lds2021/lds2021file/nppes/nppes_lds_com_2018.csv",
                   colClasses = "character",
                   header = TRUE,
                   encoding = "UTF-8")
sum(!unique(pvd$NPI) %in% unique(nppes2018$NPI))
pvd2018_2 <- pvd[!NPI %in% unique(nppes2018$NPI)]
pvd2018_2 <- taxo_info[pvd2018_2, on = "taxo"]
pvd2018_2 <- pvd2018_2[, -"provider_id"]
pvd2018_2[, `:=`(Type = 1, primary = 1, 
                 Taxonomy_order = 1)]
nppes2018 <- rbind(nppes2018, pvd2018_2)
fwrite(nppes2018,
       file = "E:/CT_APCD/Sai/lds2021/lds2021file/nppes/nppes_lds_com_2018.csv")

nppes2019 <- fread("E:/CT_APCD/Sai/lds2021/lds2021file/nppes/nppes_lds_com_2019.csv",
                   colClasses = "character",
                   header = TRUE,
                   encoding = "UTF-8")
sum(!unique(pvd$NPI) %in% unique(nppes2019$NPI))
pvd2019_2 <- pvd[!NPI %in% unique(nppes2019$NPI)]
pvd2019_2 <- taxo_info[pvd2019_2, on = "taxo"]
pvd2019_2 <- pvd2019_2[, -"provider_id"]
pvd2019_2[, `:=`(Type = 1, primary = 1, 
                 Taxonomy_order = 1)]
nppes2019 <- rbind(nppes2019, pvd2019_2)
nppes2019 <- fread("E:/CT_APCD/Sai/lds2021/lds2021file/nppes/nppes_lds_com_2019.csv",
                   colClasses = "character")
nppes2019_a <- nppes2019[primary == 1]
fwrite(nppes2019,
       file = "E:/CT_APCD/Sai/lds2021/lds2021file/nppes/nppes_lds_com_2019.csv")
