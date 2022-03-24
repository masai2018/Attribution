## Taxo Dict
{
  taxo_FP <- c('207Q00000X', '207QA0000X', '207QA0505X', '207QG0300X') # CMS08, FamilyPractice
  taxo_GP <- c('208D00000X') # CMS01, GeneralPractice
  taxo_IM <- c('207R00000X', '207RA0000X', '207RG0300X') # CMS 11, InternalMedicine
  taxo_PedM <- c('208000000X', '2080A0000X') # CMS37, PediatricMedicine
  taxo_NP <- c('363L00000X', '363LA2200X', '363LC1500X', '363LF0000X', '363LG0600X',
               '363LP0200X', '363LP2300X') # CMS50, NursePractitioner
  # taxo_PreM <-  c() # CMS84, PreventiveMedicine
  taxo_CCNS <- c('364S00000X', '364SA2200X', '364SC1501X', '364SF0001X', '364SG0600X',
                 '364SP0200X') # CMS89, Certified Clinical Nurse Specialist
  taxo_PA <- c('363A00000X', '363AM0700X') # CMS97, PhysicianAssistant
  taxo_OBGYN_org <- c('207V00000X', '207VG0400X', '207VX0000X') # CMS16, OBGYN
  taxo_OBGYN_sup <- c('363LX0001X', '363LW0102X', '364SW0102X')
}
taxo_PCP <- c(taxo_FP, taxo_GP, taxo_IM, taxo_PedM, taxo_NP, taxo_CCNS, taxo_PA) # 8 categories for PCP.
taxo_OBGYN <- c(taxo_OBGYN_org, taxo_OBGYN_sup)

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

## EM code: CPT list
CPTList <- c(99201:99215,
             99241:99255,
             99381:99429) # modified (2019-02-11)

## Entity List
entityNameList <- c(
    'CMG',
    'DayKimbal',
    'ECHN',
    'Grinffin',
    'HHC',
    'Middlesex',
    ## 'PHA',
    'ProHealth',
    'SaintFrancis',
    'SaintMary',
    'Soundview',
    'StamfordHealth',
    'Starling',
    'StVincent',
    'Waterbury',
    'WCHN',
    'WestMed',
    'YaleMedicine',
    'YaleNewHaven'
)
## returned AN lists.
entityNameReturned <- c(
    'CMG',
    ## 'DayKimbal',
    ## 'ECHN',
    'Grinffin',
    'HHC',
    'Middlesex',
    ## 'PHA', # join into CMG 20190216
    'ProHealth',
    'SaintFrancis',
    'SaintMary',
    'Soundview',
    'StamfordHealth',
    'Starling',
    'StVincent',
    ## 'Waterbury',
    'WCHN',
    'WestMed',
    'YaleMedicine',
    'YaleNewHaven'
)


## MeasureList for each AN
# measurelist <- c(
#     '2017AAB',
#     '2017AMM_12w',
#     '2017AMM_6m',
#     '2017BCS',
#     '2017CCS',
#     '2017CHL',
#     ## 'Diabetes',
#     '2017HbA1c',
#     ## 'HbA1c2016',
#     ## 'HbA1c2015',
#     '2017IET_Enga',
#     '2017IET_Init',
#     '2017MMA_50',
#     '2017MMA_75',
#     '2017NCS',
#     NULL
# )

measurelist <- c(
                 ## "2017LARC",
                 # "2017CHL",
                 # "2017HbA1c_eye",
                 # "2017HbA1c_kidney",
                 # "2017FUH_30d",
                 # "2017FUH_7d",
                 # "2016FUH_30d",
                 # "2016FUH_7d",
                 # "2015FUH_30d",
                 # "2015FUH_7d",
                 # "2014FUH_30d",
                 # "2014FUH_7d",
                 # "2013FUH_30d",
                 # "2013FUH_7d",
                 # "2017ImAdo1",
                 # "2017ImAdo2",
                 # "2017ImAdo3",
                 # "2017ImAdo4",
                 # "2017ADHD_30d",
                 # "2017ADHD_10m",
                 # "2017WcvAdo",
                 # "2017skip1",
                 # "2017skip3",
                 # "2017ReAd",
                 # "2017LBP",
                 # "2017ReAd_obs",
                 # "2017ReAd_exp",
                 # "2017ReAd_oe",
  "2017BCS",
  "2017CCS",
                 # '2016BCS',
                 # '2015BCS',
                 # '2014BCS',
                 # '2017IET_Init',
                 # '2017IET_Enga',
                 # '2017IET_Init_alcohol',
                 # '2017IET_Enga_alcohol',
                 # '2017IET_Init_opioid',
                 # '2017IET_Enga_opioid',
                 # '2017IET_Init_drug',
                 # '2017IET_Enga_drug',
                 # '2017AMM_6m',
                 # '2017AMM_12w',  
                 # '2016AMM_6m',
                 # '2016AMM_12w',
                 # '2015AMM_6m',
                 # '2015AMM_12w',
                 # '2014AMM_6m',
                 # '2014AMM_12w',
                 # '2013AMM_6m',
                 # '2013AMM_12w',
                 NULL)

measurelist_lowerbetter <- c(
    '2017NCS',
    ##  "2017LBP",
    NULL
)

## edit version for running NPI attribution
## edit_dt <- '20190217' # apply several selecting criteria from Martha's email
# edit_dt <- '20190218' # joint PHA into CMG
edit_dt <- '20191129' # add some medicare entities


spec.order <- c("NP", "CCNS", "PA", "OBGYN", "IM", "FP", "GP", "PedM")

## measure year
myear <- 2016

## org_crosswork for Shiny group
name_dat_samp <- readRDS(file = "name_dat_samp.rds")
org_crosswalk <- unique(as.data.table(name_dat_samp)[, .(org_name, org_url, insurance)])
org_crosswalk[, org_abbr := c(
  'Waterbury',
  'CMG',
  'DayKimbal',
  'ECHN',
  'Grinffin',
  'HHC',
  'Middlesex',
  'ProHealth',
  'SaintFrancis',
  'SaintMary',
  'Soundview',
  'StVincent',
  'StamfordHealth',
  'Starling',
  'WCHN',
  'WestMed',
  'YaleMedicine',
  'YaleNewHaven'
)]
org_crosswalk <- rbind(org_crosswalk,
                       list('Out Of Organizations', NA, 'Commercial', 'outAN'))

## measure_info for score_dat.rds for Shiny group
score_dat_samp <- as.data.table(readRDS('score_dat_samp.rds'))
measure_info <- unique(score_dat_samp[, .(
  measure_abbr
)])
measure_info[, measure_label := c('LARC',
                                  'ADHD_30d',
                                  'ADHD_10m',
                                  'skip',
                                  'AMM_12w',
                                  'AMM_6m',
                                  'AAB',
                                  'LBP',
                                  'MMA_50',
                                  'MMA_75',
                                  'BCS',
                                  'MDD',
                                  'CCS',
                                  'CHL',
                                  'HbA1c',
                                  'HbA1c_eye',
                                  'HbA1c_kidney',
                                  'IET_Enga',
                                  'FUH_30d',
                                  'FUH_7d',
                                  'ReAd',
                                  'ImAdo',
                                  'IET_Init',
                                  'NCS',
                                  'CAHPS_courteous',
                                  'CAHPS_Comm',
                                  'CAHPS_overall',
                                  'CAHPS_timely',
                                  'WcvAdo',
                                  NULL)]
# measure_info <- measure_info[data.table(measure_label = c(
#   'LARC',
#   'ADHD_30d',
#   'ADHD_10m',
#   'skip',
#   'AMM_12w',
#   'AMM_6m',
#   'AAB',
#   'LBP',
#   'MMA_50',
#   'MMA_75',
#   'BCS',
#   'MDD',
#   'CCS',
#   'CHL',
#   'HbA1c',
#   'HbA1c_eye',
#   'HbA1c_kidney',
#   'IET_Enga',
#   'FUH_30d',
#   'FUH_7d',
#   'ReAd',
#   'ImAdo',
#   'IET_Init',
#   "IET_Init_drug",
#   "IET_Init_alcohol",
#   "IET_Init_opioid", 
#   "IET_Enga_drug",
#   "IET_Enga_alcohol",
#   "IET_Enga_opioid",
#   'NCS',
#   'CAHPS_courteous',
#   'CAHPS_Comm',
#   'CAHPS_overall',
#   'CAHPS_timely',
#   'WcvAdo',
#   NULL
# )), on = "measure_label"]
