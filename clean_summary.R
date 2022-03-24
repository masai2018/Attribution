library(data.table)
indir <- "E:/CT_APCD/Sai/lds2021/lds2021file/Attribution_result/raw_measure/"
outdir <- "E:/CT_APCD/Sai/lds2021/lds2021file/Attribution_result/measure_in/"
## BCS
for(yr in 2017:2019){
  bcs <- fread(paste0(indir, "BCS_summary_", yr,".csv"),
               header = TRUE,
               colClasses = "character",
               encoding = "UTF-8")[Denom == 1, 
                                   .(INTERNAL_MEMBER_ID, Denom, Num)]
  fwrite(bcs, 
         file = paste0(outdir, yr, "BCS", "_summary.csv"))

}

## CCS
for(yr in 2017:2019){
  ccs <- fread(paste0(indir, "CCS_summary_", yr,".csv"),
               header = TRUE,
               colClasses = "character",
               encoding = "UTF-8")[Denom == 1, 
                                   .(INTERNAL_MEMBER_ID, Denom, Numerator)]
  setnames(ccs, "Numerator", "Num")
  fwrite(ccs, 
         file = paste0(outdir, yr, "CCS", "_summary.csv"))
  
}

## NCS
for(yr in c(2017:2019)){
  ncs <- fread(paste0(indir, "NCS_summary_", yr,".csv"),
               header = TRUE,
               colClasses = "character",
               encoding = "UTF-8")[Denom == 1, 
                                   .(INTERNAL_MEMBER_ID, Denom, Numerator)]
  setnames(ncs, "Numerator", "Num")
  fwrite(ncs, 
         file = paste0(outdir, yr, "NCS", "_summary.csv"))
  
}

## CHL
for(yr in 2017:2019){
  chl <- fread(paste0(indir, yr, "CHL_Summary", ".csv"),
               header = TRUE,
               colClasses = "character",
               encoding = "UTF-8")[Denom == 1, 
                                   .(INTERNAL_MEMBER_ID, Denom, Num)]
  fwrite(chl, 
         file = paste0(outdir, yr, "CHL", "_summary.csv"))
  
}

## AWC
for(yr in 2017:2019){
  awc <- fread(paste0(indir, yr, "AWC_com_summary", ".csv"),
               header = TRUE,
               colClasses = "character",
               encoding = "UTF-8")
  setnames(awc, c("num", "denom"), c("Num", "Denom"))
  fwrite(awc, 
         file = paste0(outdir, yr, "AWC", "_summary.csv"))
  
}

# IMA
for(yr in 2017:2019){
  ima <- fread(paste0(indir, "ima_summary_", yr, ".csv"),
               header = TRUE,
               colClasses = "character",
               encoding = "UTF-8")[, Denom := 1]
  ima4 <- ima[, list(INTERNAL_MEMBER_ID, Denom, numerator_four)]
  setnames(ima4, "numerator_four", "Num")
  fwrite(ima4, 
         file = paste0(outdir, yr, "ImAdo4", "_summary.csv"))
  ima3 <- ima[, list(INTERNAL_MEMBER_ID, Denom, numerator_three)]
  setnames(ima3, "numerator_three", "Num")
  fwrite(ima3, 
         file = paste0(outdir, yr, "ImAdo3", "_summary.csv"))
  ima2 <- ima[, list(INTERNAL_MEMBER_ID, Denom, numerator_two)]
  setnames(ima2, "numerator_two", "Num")
  fwrite(ima2, 
         file = paste0(outdir, yr, "ImAdo2", "_summary.csv"))
  ima1 <- ima[, list(INTERNAL_MEMBER_ID, Denom, numerator_one)]
  setnames(ima1, "numerator_one", "Num")
  fwrite(ima1, 
         file = paste0(outdir, yr, "ImAdo1", "_summary.csv"))
  
}

## FUH

for(yr in 2017:2019){
  fuh <- fread(paste0(indir, "FUH_m_summary_", yr, ".csv"),
               header = TRUE,
               colClasses = "character",
               encoding = "UTF-8")
  fuh[, Denom := 1]
  fuh7d <- fuh[, .(INTERNAL_MEMBER_ID, Denom, numerator_one)]
  setnames(fuh7d, "numerator_one", "Num")
  fwrite(fuh7d, 
         file = paste0(outdir, yr, "FUH_7d_summary.csv"))
  fuh30d <- fuh[,.(INTERNAL_MEMBER_ID, Denom, numerator_two)]
  setnames(fuh30d, "numerator_two", "Num")
  fwrite(fuh30d, 
         file = paste0(outdir, yr, "FUH_30d_summary.csv"))
}



## IET
for(yr in 2018:2019){
  iet <- fread(paste0(indir, "IET_Summary_", yr, ".csv"),
               header = TRUE,
               colClasses = "character",
               encoding = "UTF-8")
  iet_iesd <- iet[, .(INTERNAL_MEMBER_ID, Denom, IESD)]
  setnames(iet_iesd, "IESD", "num")
  fwrite(iet_iesd, 
         file = paste0(outdir, yr, "IET_IESD_summary.csv"))
  iet_neg <- iet[, .(INTERNAL_MEMBER_ID, Denom, Neg)]
  setnames(iet_neg, "Neg", "num")
  fwrite(iet_neg, 
         file = paste0(outdir, yr, "IET_Neg_summary.csv"))
  iet_init <- iet[, .(INTERNAL_MEMBER_ID, Denom, Init)]
  setnames(iet_init, "Init", "num")
  fwrite(iet_init, 
         file = paste0(outdir, yr, "IET_Init_summary.csv"))
  iet_enga <- iet[, .(INTERNAL_MEMBER_ID, Denom, Enga)]
  setnames(iet_enga, "Enga", "num")
  fwrite(iet_enga, 
         file = paste0(outdir, yr, "IET_Enga_summary.csv"))
  rm(iet, iet_enga, iet_iesd, iet_init, iet_neg)
  iet_alcohol <- fread(paste0(indir, "IET_alcohol_Summary_", yr, ".csv"),
               header = TRUE,
               colClasses = "character",
               encoding = "UTF-8")
  iet_alcohol_iesd <- iet_alcohol[, .(INTERNAL_MEMBER_ID, Denom, IESD)]
  setnames(iet_alcohol_iesd, "IESD", "num")
  fwrite(iet_alcohol_iesd, 
         file = paste0(outdir, yr, "IET_alcohol_IESD_summary.csv"))
  iet_alcohol_neg <- iet_alcohol[, .(INTERNAL_MEMBER_ID, Denom, Neg)]
  setnames(iet_alcohol_neg, "Neg", "num")
  fwrite(iet_alcohol_neg, 
         file = paste0(outdir, yr, "IET_alcohol_Neg_summary.csv"))
  iet_alcohol_init <- iet_alcohol[, .(INTERNAL_MEMBER_ID, Denom, Init)]
  setnames(iet_alcohol_init, "Init", "num")
  fwrite(iet_alcohol_init, 
         file = paste0(outdir, yr, "IET_alcohol_Init_summary.csv"))
  iet_alcohol_enga <- iet_alcohol[, .(INTERNAL_MEMBER_ID, Denom, Enga)]
  setnames(iet_alcohol_enga, "Enga", "num")
  fwrite(iet_alcohol_enga, 
         file = paste0(outdir, yr, "IET_alcohol_Enga_summary.csv"))
  rm(iet_alcohol, iet_alcohol_enga, iet_alcohol_iesd, iet_alcohol_init, iet_alcohol_neg)
  
  iet_drug <- fread(paste0(indir, "IET_drug_Summary_", yr, ".csv"),
                       header = TRUE,
                       colClasses = "character",
                       encoding = "UTF-8")
  iet_drug_iesd <- iet_drug[, .(INTERNAL_MEMBER_ID, Denom, IESD)]
  setnames(iet_drug_iesd, "IESD", "num")
  fwrite(iet_drug_iesd, 
         file = paste0(outdir, yr, "IET_drug_IESD_summary.csv"))
  iet_drug_neg <- iet_drug[, .(INTERNAL_MEMBER_ID, Denom, Neg)]
  setnames(iet_drug_neg, "Neg", "num")
  fwrite(iet_drug_neg, 
         file = paste0(outdir, yr, "IET_drug_Neg_summary.csv"))
  iet_drug_init <- iet_drug[, .(INTERNAL_MEMBER_ID, Denom, Init)]
  setnames(iet_drug_init, "Init", "num")
  fwrite(iet_drug_init, 
         file = paste0(outdir, yr, "IET_drug_Init_summary.csv"))
  iet_drug_enga <- iet_drug[, .(INTERNAL_MEMBER_ID, Denom, Enga)]
  setnames(iet_drug_enga, "Enga", "num")
  fwrite(iet_drug_enga, 
         file = paste0(outdir, yr, "IET_drug_Enga_summary.csv"))
  rm(iet_drug, iet_drug_enga, iet_drug_iesd, iet_drug_init, iet_drug_neg)
  
  iet_opioid <- fread(paste0(indir, "IET_opioid_Summary_", yr, ".csv"),
                    header = TRUE,
                    colClasses = "character",
                    encoding = "UTF-8")
  iet_opioid_iesd <- iet_opioid[, .(INTERNAL_MEMBER_ID, Denom, IESD)]
  setnames(iet_opioid_iesd, "IESD", "num")
  fwrite(iet_opioid_iesd, 
         file = paste0(outdir, yr, "IET_opioid_IESD_summary.csv"))
  iet_opioid_neg <- iet_opioid[, .(INTERNAL_MEMBER_ID, Denom, Neg)]
  setnames(iet_opioid_neg, "Neg", "num")
  fwrite(iet_opioid_neg, 
         file = paste0(outdir, yr, "IET_opioid_Neg_summary.csv"))
  iet_opioid_init <- iet_opioid[, .(INTERNAL_MEMBER_ID, Denom, Init)]
  setnames(iet_opioid_init, "Init", "num")
  fwrite(iet_opioid_init, 
         file = paste0(outdir, yr, "IET_opioid_Init_summary.csv"))
  iet_opioid_enga <- iet_opioid[, .(INTERNAL_MEMBER_ID, Denom, Enga)]
  setnames(iet_opioid_enga, "Enga", "num")
  fwrite(iet_opioid_enga, 
         file = paste0(outdir, yr, "IET_opioid_Enga_summary.csv"))
  rm(iet_opioid, iet_opioid_enga, iet_opioid_iesd, iet_opioid_init, iet_opioid_neg)
  
}

# LBP
for(yr in 2018:2019){
  lbp <- fread(paste0(indir, "LBP_Summary_", yr, ".csv"),
               header = TRUE,
               colClasses = "character",
               encoding = "UTF-8", 
               select = c("INTERNAL_MEMBER_ID",
                         "Denom",
                         "Num"))
  fwrite(lbp, 
         file = paste0(outdir, yr, "LBP", "_summary.csv"))
  
}

# Nephropathy
for(yr in 2017:2017){
  Nephropathy <- fread(paste0(indir, "Nephropathy_summary_", yr, ".csv"),
               header = TRUE,
               colClasses = "character",
               encoding = "UTF-8")
  setnames(Nephropathy, c("denominator", "numerator"), c("Denom", "Num"))
  fwrite(Nephropathy, 
         file = paste0(outdir, yr, "Nephropathy", "_summary.csv"))
  
}

#Hab1c

for(yr in 2017:2019){
  HbA1c <- fread(paste0(indir, yr, "HbA1c_com_summary", ".csv"),
                       header = TRUE,
                       colClasses = "character",
                       encoding = "UTF-8")
  fwrite(HbA1c, 
         file = paste0(outdir, yr, "HbA1c", "_summary.csv"))
  
}

# Diabetes eye exam
for(yr in 2017:2019){
  Diabetes <- fread(paste0(indir, "Diabetes_summary_file_", yr, ".csv"),
                 header = TRUE,
                 colClasses = "character",
                 encoding = "UTF-8",
                 select = c("internal_member_id",
                            "eye_exam"))[, Denom := 1]
  setnames(Diabetes, "eye_exam", "Num")
  fwrite(Diabetes, 
         file = paste0(outdir, yr, "Diabetes", "_summary.csv"))
  
}

# nephropathy
for(yr in 2017:2019){
  Nephropathy <- fread(paste0(indir, "Nephropathy_summary_file_", yr, ".csv"),
                    header = TRUE,
                    colClasses = "character",
                    encoding = "UTF-8",
                    select = c("internal_member_id",
                               "eye_exam"))[, Denom := 1]
  setnames(Diabetes, "eye_exam", "Num")
  fwrite(Diabetes, 
         file = paste0(outdir, yr, "Diabetes", "_summary.csv"))
  
}



IET_2016 <- fread("E:/CT_APCD/Yaqiong/IET/output_medicare/IET_summary_2016.csv",
                  colClasses = "character")
IET_IESD_2016 <- IET_2016[, .(INTERNAL_MEMBER_ID, Denom, IESD)]
setnames(IET_IESD_2016, c("Denom", "IESD"), c("denom", "num"))
fwrite(IET_IESD_2016, paste0(outdir, "2016IET_IESD_summary.csv"))

IET_Neg_2016 <- IET_2016[, .(INTERNAL_MEMBER_ID, Denom, Neg)]
setnames(IET_Neg_2016, c("Denom", "Neg"), c("denom", "num"))
fwrite(IET_Neg_2016, paste0(outdir, "2016IET_Neg_summary.csv"))

IET_Init_2016 <- IET_2016[, .(INTERNAL_MEMBER_ID, Denom, Init)]
setnames(IET_Init_2016, c("Denom", "Init"), c("denom", "num"))
fwrite(IET_Init_2016, paste0(outdir, "2016IET_Init_summary.csv"))

IET_Enga_2016 <- IET_2016[, .(INTERNAL_MEMBER_ID, Denom, Enga)]
setnames(IET_Enga_2016, c("Denom", "Enga"), c("denom", "num"))
fwrite(IET_Enga_2016, paste0(outdir, "2016IET_Enga_summary.csv"))


## IET_alcohol
IET_alcohol_2016 <- fread("E:/CT_APCD/Yaqiong/IET/output_medicare/IET_alcohol_summary_2016.csv",
                          colClasses = "character")
IET_alcohol_IESD_2016 <- IET_alcohol_2016[, .(INTERNAL_MEMBER_ID, Denom, IESD)]
setnames(IET_alcohol_IESD_2016, c("Denom", "IESD"), c("denom", "num"))
fwrite(IET_alcohol_IESD_2016, paste0(outdir, "2016IET_alcohol_IESD_summary.csv"))

IET_alcohol_Neg_2016 <- IET_alcohol_2016[, .(INTERNAL_MEMBER_ID, Denom, Neg)]
setnames(IET_alcohol_Neg_2016, c("Denom", "Neg"), c("denom", "num"))
fwrite(IET_alcohol_Neg_2016, paste0(outdir, "2016IET_alcohol_Neg_summary.csv"))

IET_alcohol_Init_2016 <- IET_alcohol_2016[, .(INTERNAL_MEMBER_ID, Denom, Init)]
setnames(IET_alcohol_Init_2016, c("Denom", "Init"), c("denom", "num"))
fwrite(IET_alcohol_Init_2016, paste0(outdir, "2016IET_alcohol_Init_summary.csv"))

IET_alcohol_Enga_2016 <- IET_alcohol_2016[, .(INTERNAL_MEMBER_ID, Denom, Enga)]
setnames(IET_alcohol_Enga_2016, c("Denom", "Enga"), c("denom", "num"))
fwrite(IET_alcohol_Enga_2016, paste0(outdir, "2016IET_alcohol_Enga_summary.csv"))

## IET_drug
IET_drug_2016 <- fread("E:/CT_APCD/Yaqiong/IET/output_medicare/IET_drug_summary_2016.csv",
                       colClasses = "character")
IET_drug_IESD_2016 <- IET_drug_2016[, .(INTERNAL_MEMBER_ID, Denom, IESD)]
setnames(IET_drug_IESD_2016, c("Denom", "IESD"), c("denom", "num"))
fwrite(IET_drug_IESD_2016, paste0(outdir, "2016IET_drug_IESD_summary.csv"))

IET_drug_Neg_2016 <- IET_drug_2016[, .(INTERNAL_MEMBER_ID, Denom, Neg)]
setnames(IET_drug_Neg_2016, c("Denom", "Neg"), c("denom", "num"))
fwrite(IET_drug_Neg_2016, paste0(outdir, "2016IET_drug_Neg_summary.csv"))

IET_drug_Init_2016 <- IET_drug_2016[, .(INTERNAL_MEMBER_ID, Denom, Init)]
setnames(IET_drug_Init_2016, c("Denom", "Init"), c("denom", "num"))
fwrite(IET_drug_Init_2016, paste0(outdir, "2016IET_drug_Init_summary.csv"))

IET_drug_Enga_2016 <- IET_drug_2016[, .(INTERNAL_MEMBER_ID, Denom, Enga)]
setnames(IET_drug_Enga_2016, c("Denom", "Enga"), c("denom", "num"))
fwrite(IET_drug_Enga_2016, paste0(outdir, "2016IET_drug_Enga_summary.csv"))

## IET_opioid
IET_opioid_2016 <- fread("E:/CT_APCD/Yaqiong/IET/output_medicare/IET_opioid_summary_2016.csv",
                         colClasses = "character")
IET_opioid_IESD_2016 <- IET_opioid_2016[, .(INTERNAL_MEMBER_ID, Denom, IESD)]
setnames(IET_opioid_IESD_2016, c("Denom", "IESD"), c("denom", "num"))
fwrite(IET_opioid_IESD_2016, paste0(outdir, "2016IET_opioid_IESD_summary.csv"))

IET_opioid_Neg_2016 <- IET_opioid_2016[, .(INTERNAL_MEMBER_ID, Denom, Neg)]
setnames(IET_opioid_Neg_2016, c("Denom", "Neg"), c("denom", "num"))
fwrite(IET_opioid_Neg_2016, paste0(outdir, "2016IET_opioid_Neg_summary.csv"))

IET_opioid_Init_2016 <- IET_opioid_2016[, .(INTERNAL_MEMBER_ID, Denom, Init)]
setnames(IET_opioid_Init_2016, c("Denom", "Init"), c("denom", "num"))
fwrite(IET_opioid_Init_2016, paste0(outdir, "2016IET_opioid_Init_summary.csv"))

IET_opioid_Enga_2016 <- IET_opioid_2016[, .(INTERNAL_MEMBER_ID, Denom, Enga)]
setnames(IET_opioid_Enga_2016, c("Denom", "Enga"), c("denom", "num"))
fwrite(IET_opioid_Enga_2016, paste0(outdir, "2016IET_opioid_Enga_summary.csv"))
}
## AMM
ammdir <- 'E:/CT_APCD/Sai/measure_in/backup/AMM/'
ammfiles <- list.files(ammdir, patter = 'AMM')
for(i in ammfiles){
  aaa <- fread(paste0(ammdir, i),
               header = TRUE,
               colClasses = "character",
               encoding = "UTF-8")
  aaa <- aaa[, list(INTERNAL_MEMBER_ID, 
                    Denom, Num_EAPT, Num_ECPT)]
  aaa[, Denom := '1']
  bbb <- aaa[,list(INTERNAL_MEMBER_ID, Denom, Num_ECPT)]
  colnames(bbb) <- c('INTERNAL_MEMBER_ID', 'denom', 'num')
  fwrite(bbb, paste0(outdir, gsub('AMM_Summary.csv', 'AMM_6m_summary.csv', i)))
  ccc <- aaa[,list(INTERNAL_MEMBER_ID, Denom, Num_EAPT)]
  colnames(ccc) <- c('INTERNAL_MEMBER_ID', 'denom', 'num')
  fwrite(ccc, paste0(outdir, gsub('AMM_Summary.csv', 'AMM_12w_summary.csv', i)))
}


## AMM 2017
aaa <- fread(paste0(ammdir, "2017AMM_Summary.csv"),
                        header = TRUE,
                        colClasses = "character",
                        encoding = "UTF-8")
aaa <- aaa[Denom == '1']
bbb <- aaa[,list(INTERNAL_MEMBER_ID, Denom, Num_ECPT)]
colnames(bbb) <- c('INTERNAL_MEMBER_ID', 'denom', 'num')
fwrite(bbb, paste0(outdir, gsub('AMM_Summary.csv', 'AMM_6m_summary.csv', "2017AMM_Summary.csv")))
ccc <- aaa[,list(INTERNAL_MEMBER_ID, Denom, Num_EAPT)]
colnames(ccc) <- c('INTERNAL_MEMBER_ID', 'denom', 'num')
fwrite(ccc, paste0(outdir, gsub('AMM_Summary.csv', 'AMM_12w_summary.csv', "2017AMM_Summary.csv")))



colnames(aaa) <- c("MEDICAL_CLAIM_HEADER_ID",
                   "INTERNAL_MEMBER_ID", "num1", "num2")
aaa[, denom := 1]
bbb <- aaa[,list(MEDICAL_CLAIM_HEADER_ID,
                 INTERNAL_MEMBER_ID, denom, num1)]
head(bbb)
colnames(bbb)[4] <- c("num")
fwrite(bbb, "E:/CT_APCD/Sai/measure_in/2017FUH_7d_summary.csv")
ccc <- aaa[,list(MEDICAL_CLAIM_HEADER_ID,
                 INTERNAL_MEMBER_ID, denom, num2)]
head(ccc)
colnames(ccc)[4] <- c("num")
fwrite(ccc, "E:/CT_APCD/Sai/measure_in/2017FUH_30d_Summary.csv")
head(ccc)
aa <- fread("E:/CT_APCD/Sai/measure_in/ima_summary.csv",
            colClasses = "character",
            header = TRUE,
            encoding = "UTF-8")
bb <- fread("E:/CT_APCD/Sai/measure_in/2017HbA1c_eye_summary.csv",
            # colClasses = "character",
            header = TRUE,
            encoding = "UTF-8")
head(aa)
aa[, denom := 1]
a4 <- aa[, list(INTERNAL_MEMBER_ID, denom, numerator_four)]
head(a4)
colnames(a4) <- c("INTERNAL_MEMBER_ID", "denom", "num")
head(a4)
fwrite(a4, "E:/CT_APCD/Sai/measure_in/2017ImAdo4_Summary.csv")

sm <- fread("E:/CT_APCD/Yaqiong/PCR/entity_summary.txt",
            colClasses = "character",
            encoding = "UTF-8")

## MPM
infl_MPM_2016 <- fread("E:/CT_APCD/Yaqiong/MPM/output_medicare/Summary_2016.csv",
                  colClasses = "character")
names(infl_MPM_2016)
infl_MPM_2016_T <- infl_MPM_2016[, .(INTERNAL_MEMBER_ID,
                                     RateTDenom,
                                     RateTNum)]
setnames(infl_MPM_2016_T,
         c("RateTDenom", "RateTNum"),
         c("denom", "num"))
fwrite(infl_MPM_2016_T, paste0(outdir, "2016MPM_T_summary.csv"))

infl_MPM_2016_1 <- infl_MPM_2016[, .(INTERNAL_MEMBER_ID,
                                     Rate1Denom,
                                     Rate1Num)]
setnames(infl_MPM_2016_1,
         c("Rate1Denom", "Rate1Num"),
         c("denom", "num"))
fwrite(infl_MPM_2016_1, paste0(outdir, "2016MPM_1_summary.csv"))

infl_MPM_2016_3 <- infl_MPM_2016[, .(INTERNAL_MEMBER_ID,
                                     Rate3Denom,
                                     Rate3Num)]
setnames(infl_MPM_2016_3,
         c("Rate3Denom", "Rate3Num"),
         c("denom", "num"))
fwrite(infl_MPM_2016_3, paste0(outdir, "2016MPM_3_summary.csv"))

## IET
IET_2016 <- fread("E:/CT_APCD/Yaqiong/IET/output_medicare/IET_summary_2016.csv",
             colClasses = "character")
IET_IESD_2016 <- IET_2016[, .(INTERNAL_MEMBER_ID, Denom, IESD)]
setnames(IET_IESD_2016, c("Denom", "IESD"), c("denom", "num"))
fwrite(IET_IESD_2016, paste0(outdir, "2016IET_IESD_summary.csv"))

IET_Neg_2016 <- IET_2016[, .(INTERNAL_MEMBER_ID, Denom, Neg)]
setnames(IET_Neg_2016, c("Denom", "Neg"), c("denom", "num"))
fwrite(IET_Neg_2016, paste0(outdir, "2016IET_Neg_summary.csv"))

IET_Init_2016 <- IET_2016[, .(INTERNAL_MEMBER_ID, Denom, Init)]
setnames(IET_Init_2016, c("Denom", "Init"), c("denom", "num"))
fwrite(IET_Init_2016, paste0(outdir, "2016IET_Init_summary.csv"))

IET_Enga_2016 <- IET_2016[, .(INTERNAL_MEMBER_ID, Denom, Enga)]
setnames(IET_Enga_2016, c("Denom", "Enga"), c("denom", "num"))
fwrite(IET_Enga_2016, paste0(outdir, "2016IET_Enga_summary.csv"))


## IET_alcohol
IET_alcohol_2016 <- fread("E:/CT_APCD/Yaqiong/IET/output_medicare/IET_alcohol_summary_2016.csv",
                          colClasses = "character")
IET_alcohol_IESD_2016 <- IET_alcohol_2016[, .(INTERNAL_MEMBER_ID, Denom, IESD)]
setnames(IET_alcohol_IESD_2016, c("Denom", "IESD"), c("denom", "num"))
fwrite(IET_alcohol_IESD_2016, paste0(outdir, "2016IET_alcohol_IESD_summary.csv"))

IET_alcohol_Neg_2016 <- IET_alcohol_2016[, .(INTERNAL_MEMBER_ID, Denom, Neg)]
setnames(IET_alcohol_Neg_2016, c("Denom", "Neg"), c("denom", "num"))
fwrite(IET_alcohol_Neg_2016, paste0(outdir, "2016IET_alcohol_Neg_summary.csv"))

IET_alcohol_Init_2016 <- IET_alcohol_2016[, .(INTERNAL_MEMBER_ID, Denom, Init)]
setnames(IET_alcohol_Init_2016, c("Denom", "Init"), c("denom", "num"))
fwrite(IET_alcohol_Init_2016, paste0(outdir, "2016IET_alcohol_Init_summary.csv"))

IET_alcohol_Enga_2016 <- IET_alcohol_2016[, .(INTERNAL_MEMBER_ID, Denom, Enga)]
setnames(IET_alcohol_Enga_2016, c("Denom", "Enga"), c("denom", "num"))
fwrite(IET_alcohol_Enga_2016, paste0(outdir, "2016IET_alcohol_Enga_summary.csv"))

## IET_drug
IET_drug_2016 <- fread("E:/CT_APCD/Yaqiong/IET/output_medicare/IET_drug_summary_2016.csv",
                          colClasses = "character")
IET_drug_IESD_2016 <- IET_drug_2016[, .(INTERNAL_MEMBER_ID, Denom, IESD)]
setnames(IET_drug_IESD_2016, c("Denom", "IESD"), c("denom", "num"))
fwrite(IET_drug_IESD_2016, paste0(outdir, "2016IET_drug_IESD_summary.csv"))

IET_drug_Neg_2016 <- IET_drug_2016[, .(INTERNAL_MEMBER_ID, Denom, Neg)]
setnames(IET_drug_Neg_2016, c("Denom", "Neg"), c("denom", "num"))
fwrite(IET_drug_Neg_2016, paste0(outdir, "2016IET_drug_Neg_summary.csv"))

IET_drug_Init_2016 <- IET_drug_2016[, .(INTERNAL_MEMBER_ID, Denom, Init)]
setnames(IET_drug_Init_2016, c("Denom", "Init"), c("denom", "num"))
fwrite(IET_drug_Init_2016, paste0(outdir, "2016IET_drug_Init_summary.csv"))

IET_drug_Enga_2016 <- IET_drug_2016[, .(INTERNAL_MEMBER_ID, Denom, Enga)]
setnames(IET_drug_Enga_2016, c("Denom", "Enga"), c("denom", "num"))
fwrite(IET_drug_Enga_2016, paste0(outdir, "2016IET_drug_Enga_summary.csv"))

## IET_opioid
IET_opioid_2016 <- fread("E:/CT_APCD/Yaqiong/IET/output_medicare/IET_opioid_summary_2016.csv",
                       colClasses = "character")
IET_opioid_IESD_2016 <- IET_opioid_2016[, .(INTERNAL_MEMBER_ID, Denom, IESD)]
setnames(IET_opioid_IESD_2016, c("Denom", "IESD"), c("denom", "num"))
fwrite(IET_opioid_IESD_2016, paste0(outdir, "2016IET_opioid_IESD_summary.csv"))

IET_opioid_Neg_2016 <- IET_opioid_2016[, .(INTERNAL_MEMBER_ID, Denom, Neg)]
setnames(IET_opioid_Neg_2016, c("Denom", "Neg"), c("denom", "num"))
fwrite(IET_opioid_Neg_2016, paste0(outdir, "2016IET_opioid_Neg_summary.csv"))

IET_opioid_Init_2016 <- IET_opioid_2016[, .(INTERNAL_MEMBER_ID, Denom, Init)]
setnames(IET_opioid_Init_2016, c("Denom", "Init"), c("denom", "num"))
fwrite(IET_opioid_Init_2016, paste0(outdir, "2016IET_opioid_Init_summary.csv"))

IET_opioid_Enga_2016 <- IET_opioid_2016[, .(INTERNAL_MEMBER_ID, Denom, Enga)]
setnames(IET_opioid_Enga_2016, c("Denom", "Enga"), c("denom", "num"))
fwrite(IET_opioid_Enga_2016, paste0(outdir, "2016IET_opioid_Enga_summary.csv"))

## HbA1c
HbA1c <- fread("E:/CT_APCD/junwang/measure by me/2020/Diabetes/Medicare_2016/HbA1C_medicare_and_advantage/HbA1c.csv",
               colClasses = "character")
names(HbA1c)
HbA1c[, Denom := 1]
fwrite(HbA1c, file = paste0(outdir, "2016HbA1c_summary.csv"))


# MPM 08202020
# has medicare claims in this measure
infl_MPM_2016 <- fread("E:/CT_APCD/Yaqiong/MPM/output_medicare/Summary_2016_medicare.csv",
                       colClasses = "character")
names(infl_MPM_2016)
infl_MPM_2016_com <- infl_MPM_2016[medicare == 1]
infl_MPM_2016_com_T <- infl_MPM_2016_com[, .(INTERNAL_MEMBER_ID,
                                     RateTDenom,
                                     RateTNum)]
setnames(infl_MPM_2016_com_T,
         c("RateTDenom", "RateTNum"),
         c("denom", "num"))
fwrite(infl_MPM_2016_com_T, paste0(outdir, "2016MPM_com_T_summary.csv"))

infl_MPM_2016_com_1 <- infl_MPM_2016_com[, .(INTERNAL_MEMBER_ID,
                                     Rate1Denom,
                                     Rate1Num)]
setnames(infl_MPM_2016_com_1,
         c("Rate1Denom", "Rate1Num"),
         c("denom", "num"))
fwrite(infl_MPM_2016_com_1, paste0(outdir, "2016MPM_com_1_summary.csv"))

infl_MPM_2016_com_3 <- infl_MPM_2016_com[, .(INTERNAL_MEMBER_ID,
                                     Rate3Denom,
                                     Rate3Num)]
setnames(infl_MPM_2016_com_3,
         c("Rate3Denom", "Rate3Num"),
         c("denom", "num"))
fwrite(infl_MPM_2016_com_3, paste0(outdir, "2016MPM_com_3_summary.csv"))

#  only has commercial claims
infl_MPM_2016_mc <- infl_MPM_2016[medicare == 0]
infl_MPM_2016_mc_T <- infl_MPM_2016_mc[, .(INTERNAL_MEMBER_ID,
                                             RateTDenom,
                                             RateTNum)]
setnames(infl_MPM_2016_mc_T,
         c("RateTDenom", "RateTNum"),
         c("denom", "num"))
fwrite(infl_MPM_2016_mc_T, paste0(outdir, "2016MPM_mc_T_summary.csv"))

infl_MPM_2016_mc_1 <- infl_MPM_2016_mc[, .(INTERNAL_MEMBER_ID,
                                             Rate1Denom,
                                             Rate1Num)]
setnames(infl_MPM_2016_mc_1,
         c("Rate1Denom", "Rate1Num"),
         c("denom", "num"))
fwrite(infl_MPM_2016_mc_1, paste0(outdir, "2016MPM_mc_1_summary.csv"))

infl_MPM_2016_mc_3 <- infl_MPM_2016_mc[, .(INTERNAL_MEMBER_ID,
                                             Rate3Denom,
                                             Rate3Num)]
setnames(infl_MPM_2016_mc_3,
         c("Rate3Denom", "Rate3Num"),
         c("denom", "num"))
fwrite(infl_MPM_2016_mc_3, paste0(outdir, "2016MPM_mc_3_summary.csv"))

# all 
infl_MPM_2016 <- fread("E:/CT_APCD/Yaqiong/MPM/output_medicare/Summary_2016_medicare.csv",
                       colClasses = "character")
names(infl_MPM_2016)
infl_MPM_2016_T <- infl_MPM_2016[, .(INTERNAL_MEMBER_ID,
                                     RateTDenom,
                                     RateTNum)]
setnames(infl_MPM_2016_T,
         c("RateTDenom", "RateTNum"),
         c("denom", "num"))
fwrite(infl_MPM_2016_T, paste0(outdir, "2016MPM_T_summary.csv"))

infl_MPM_2016_1 <- infl_MPM_2016[, .(INTERNAL_MEMBER_ID,
                                     Rate1Denom,
                                     Rate1Num)]
setnames(infl_MPM_2016_1,
         c("Rate1Denom", "Rate1Num"),
         c("denom", "num"))
fwrite(infl_MPM_2016_1, paste0(outdir, "2016MPM_1_summary.csv"))

infl_MPM_2016_3 <- infl_MPM_2016[, .(INTERNAL_MEMBER_ID,
                                     Rate3Denom,
                                     Rate3Num)]
setnames(infl_MPM_2016_3,
         c("Rate3Denom", "Rate3Num"),
         c("denom", "num"))
fwrite(infl_MPM_2016_3, paste0(outdir, "2016MPM_3_summary.csv"))


## HbA1c
HbA1c_all <- fread("E:/CT_APCD/Abby/hba1c/hba1c_summary_2016.csv",
               colClasses = "character")
setnames(HbA1c_all, "denominator", "denom")

HbA1c <- HbA1c_all[, .(INTERNAL_MEMBER_ID, hbA1c, denom)]
setnames(HbA1c, "hbA1c", "num")
fwrite(HbA1c, paste0(outdir, "2016HbA1c_summary.csv"))

HbA1c_eye <- HbA1c_all[, .(INTERNAL_MEMBER_ID, eye_exam, denom)]
setnames(HbA1c_eye, "eye_exam", "num")
fwrite(HbA1c_eye, paste0(outdir, "2016HbA1c_eye_summary.csv"))

HbA1c_kidney <- HbA1c_all[, .(INTERNAL_MEMBER_ID, nephropathy, denom)]
setnames(HbA1c_kidney, "nephropathy", "num")
fwrite(HbA1c_kidney, paste0(outdir, "2016HbA1c_kidney_summary.csv"))
