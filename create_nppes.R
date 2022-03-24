library(methods)
source("E:/CT_APCD/Sai/data-organization/R/utils.R")
need_pkgs <- c("data.table", "bit64", "tools",
               "dplyr", "lubridate", "tidyverse", "haven",
               "readxl")
need.packages(need_pkgs)

cols1 <- c("NPI", "Entity Type Code",
           paste0("Healthcare Provider Taxonomy Code_", 1:15),
           paste0("Healthcare Provider Primary Taxonomy Switch_", 1:15))
# nppes2018 <- fread(paste0("E:/CT_APCD/Sai/medicaid/medicaid_files/", 
#                       "nppes_raw_file/npidata_20050523-20180213.csv"), 
#                colClasses = "character",
#                select = cols1)
# 
# nppes2018_new <- data.table()
# for(i in 1:15){
#   cols2 <- c("NPI", "Entity Type Code",
#              paste0("Healthcare Provider Taxonomy Code_", i),
#              paste0("Healthcare Provider Primary Taxonomy Switch_", i))
#   tmp <- nppes2018[, cols2, with = FALSE]
#   tmp[, Taxonomy_order := i]
#   names(tmp)[2:4] <- c("Type", "Taxonomy", "primary")
#   tmp2 <- tmp[!Taxonomy == ""]
#   nppes2018_new <- rbind(nppes2018_new, tmp2)
# }
# rm(nppes2018, tmp, tmp2)
# gc()
# nppes2018_new <- unique(nppes2018_new, use.key = FALSE)
# keycol <- c("Type", "Taxonomy", "Taxonomy_order")
# setorderv(nppes2018_new, keycol)
# fwrite(nppes2018_new, "E:/CT_APCD/Sai/lds2021/lds2021file/nppes/nppes2018.csv")
# rm(nppes2018_new)
# gc()
# 
# 
nppes2019 <- fread("E:/CT_APCD/Abby/HPV/NPPES_Data_Dissemination_November_2019/npidata_pfile_20050523-20191110.csv",
                   colClasses = "character",
                   select = cols1)
nppes2019_new <- data.table()
for(i in 1:15){
  cols2 <- c("NPI", "Entity Type Code",
             paste0("Healthcare Provider Taxonomy Code_", i),
             paste0("Healthcare Provider Primary Taxonomy Switch_", i))
  tmp <- nppes2019[, cols2, with = FALSE]
  tmp[, Taxonomy_order := i]
  names(tmp)[2:4] <- c("Type", "Taxonomy", "primary")
  tmp2 <- tmp[!Taxonomy == ""]
  nppes2019_new <- rbind(nppes2019_new, tmp2)
}
rm(nppes2019, tmp, tmp2)
gc()
nppes2019_new <- unique(nppes2019_new, use.key = FALSE)
keycol <- c("Type", "Taxonomy", "Taxonomy_order")
setorderv(nppes2019_new, keycol)
uniqueN(nppes2019_new$NPI)
dim(nppes2019_new)
pvd <- fread("E:/CT_APCD/Sai/lds2021/lds2021file/nppes/provider_small.csv",
             colClasses = "character")
com2019 <- fread("E:/CT_APCD/Sai/lds2021/lds2021code/Attribution_2018_2019/NPI-Attribution/NPI_WholeSet_2019_com.csv",
                 colClasses = "character", 
                 select = c("NPI","Taxonomy1", "spec0")) %>% unique(use.key = FALSE)
com2018 <- fread("E:/CT_APCD/Sai/lds2021/lds2021code/Attribution_2018_2019/NPI-Attribution/NPI_WholeSet_2019_com.csv",
                 colClasses = "character", 
                 select = c("NPI","Taxonomy1", "spec0")) %>% unique(use.key = FALSE)
com2017 <- fread("E:/CT_APCD/Sai/output/Attribution/NPI-Attribution/NPI_WholeSet_0613.csv",
                 colClasses = "character", 
                 select = c("NPI","Taxonomy1")) %>% unique(use.key = FALSE)
nppes2019_new <- nppes2019_new[NPI %in% unique(c(pvd$NPI, 
                                               com2017$NPI,
                                               com2018$NPI,
                                               com2019$NPI))]
pvd2 <- pvd[!NPI %in% nppes2019_new$NPI][, c("NPI", "taxo")]
setnames(pvd2, "taxo", "Taxonomy")
pvd2[, `:=`(Type = 1, 
            primary = 1,
            Taxonomy_order = 1)]
nppes2019_new <- rbind(nppes2019_new, pvd2)



com2019 <- com2019[, -'spec0']
com2019[, `:=`(Type = 1, 
               primary = 1,
               Taxonomy_order = 1)]
setnames(com2019, "Taxonomy1", "Taxonomy")
nppes2019_new_2019_2 <- nppes2019_new[!NPI %in% unique(com2019$NPI)]
nppes2019_new_2019 <- rbind(com2019, nppes2019_new_2019_2)
sum(!com2019$NPI %in% nppes2019_new_2019$NPI)
sum(!pvd$NPI %in% nppes2019_new_2019$NPI)
# 0
nppes2019_new_2019 <- taxo_info[nppes2019_new_2019, on = c(taxo = "Taxonomy")]
table(nppes2019_new_2019[NPI %in% com2019$NPI]$spec0)
fwrite(nppes2019_new_2019, "E:/CT_APCD/Sai/lds2021/lds2021file/nppes/nppes_lds_com_2019.csv")
nppes2019_new_2019_short1 <- nppes2019_new_2019[primary == 1] %>% unique(use.key = FALSE)
nppes2019_new_2019[, Taxonomy_order := as.numeric(Taxonomy_order)]
nppes2019_new_2019_short2_npi <- nppes2019_new_2019[primary != 1,
                                                    lapply(.SD, min),
                                                    .SDcols = "Taxonomy_order",
                                                    by = "NPI"] %>% unique(use.key = FALSE)
nppes2019_new_2019_short2 <- nppes2019_new_2019[nppes2019_new_2019_short2_npi,
                                                on = c("NPI",
                                                       "Taxonomy_order")]
nppes2019_new_2019_short <- rbind(nppes2019_new_2019_short1, nppes2019_new_2019_short2)
fwrite(nppes2019_new_2019_short, "E:/CT_APCD/Sai/lds2021/lds2021file/nppes/nppes_lds_com_2019_short.csv")


com2018 <- com2018[, -'spec0']
com2018[, `:=`(Type = 1, 
               primary = 1,
               Taxonomy_order = 1)]
setnames(com2018, "Taxonomy1", "Taxonomy")
nppes2019_new_2018_2 <- nppes2019_new[!NPI %in% unique(com2018$NPI)]
nppes2019_new_2018 <- rbind(com2018, nppes2019_new_2018_2)
sum(!com2018$NPI %in% nppes2019_new_2018$NPI)
sum(!pvd$NPI %in% nppes2019_new_2018$NPI)
# 0
nppes2019_new_2018 <- taxo_info[nppes2019_new_2018, on = c(taxo = "Taxonomy")]
table(nppes2019_new_2018[NPI %in% com2018$NPI]$spec0)
fwrite(nppes2019_new_2018, "E:/CT_APCD/Sai/lds2021/lds2021file/nppes/nppes_lds_com_2018.csv")
nppes2019_new_2018_short1 <- nppes2019_new_2018[primary == 1] %>% unique(use.key = FALSE)
nppes2019_new_2018[, Taxonomy_order := as.numeric(Taxonomy_order)]
nppes2019_new_2018_short2_npi <- nppes2019_new_2018[primary != 1,
                                                    lapply(.SD, min),
                                                    .SDcols = "Taxonomy_order",
                                                    by = "NPI"] %>% unique(use.key = FALSE)
nppes2019_new_2018_short2 <- nppes2019_new_2018[nppes2019_new_2018_short2_npi,
                                                on = c("NPI",
                                                       "Taxonomy_order")]
nppes2019_new_2018_short <- rbind(nppes2019_new_2018_short1, nppes2019_new_2018_short2)
fwrite(nppes2019_new_2018_short, "E:/CT_APCD/Sai/lds2021/lds2021file/nppes/nppes_lds_com_2018_short.csv")



com2017 <- com2017
com2017[, `:=`(Type = 1, 
               primary = 1,
               Taxonomy_order = 1)]
setnames(com2017, "Taxonomy1", "Taxonomy")
nppes2019_new_2017_2 <- nppes2019_new[!NPI %in% unique(com2017$NPI)]
nppes2019_new_2017 <- rbind(com2017, nppes2019_new_2017_2)
sum(!com2017$NPI %in% nppes2019_new_2017$NPI)
sum(!pvd$NPI %in% nppes2019_new_2017$NPI)
# 0
nppes2019_new_2017 <- taxo_info[nppes2019_new_2017, on = c(taxo = "Taxonomy")]
table(nppes2019_new_2017[NPI %in% com2017$NPI]$spec0)
fwrite(nppes2019_new_2017, "E:/CT_APCD/Sai/lds2021/lds2021file/nppes/nppes_lds_com_2017.csv")

nppes2019_new_2017_short1 <- nppes2019_new_2017[primary == 1] %>% unique(use.key = FALSE)
nppes2019_new_2017[, Taxonomy_order := as.numeric(Taxonomy_order)]
nppes2019_new_2017_short2_npi <- nppes2019_new_2017[primary != 1,
                                                    lapply(.SD, min),
                                                    .SDcols = "Taxonomy_order",
                                                    by = "NPI"] %>% unique(use.key = FALSE)
nppes2019_new_2017_short2 <- nppes2019_new_2017[nppes2019_new_2017_short2_npi,
                                                on = c("NPI",
                                                       "Taxonomy_order")]
nppes2019_new_2017_short <- rbind(nppes2019_new_2017_short1, nppes2019_new_2017_short2)
fwrite(nppes2019_new_2017_short, "E:/CT_APCD/Sai/lds2021/lds2021file/nppes/nppes_lds_com_2017_short.csv")