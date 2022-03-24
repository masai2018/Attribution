################################################################################
### entitywise measure calculation
### version controlled by git
################################################################################

## prerequisites
source('E:/CT_APCD/Sai/2019spring/data-organization/R/utils.R')
library(knitr)
library(scales)
library(data.table)
dir_code <- input_dir('E:/CT_APCD/Sai/lds2021/lds2021code/Attribution_2018_2019/')
dir.in <- input_dir('E:/CT_APCD/Sai/lds2021/lds2021file/Attribution_result/output/measure_AN_2018/detail/')
dir.in2 <- input_dir("E:/CT_APCD/Sai/lds2021/lds2021file/Attribution_result/output/Double-Attribution_2018/")
dir.out.tb <- output_dir('E:/CT_APCD/Sai/lds2021/lds2021file/Attribution_result/output/measure_AN_2018/table/')
## dir.out <- 'E:/CT_APCD/Hongfei/output/toShiny/'
# dir.out <- output_dir('E:/CT_APCD/Sai/Attribution_result/output/measure_AN_0_64_com/shiny-apps-data/')
source(paste0(dir_code, 'myDoc.R'))
source(paste0(dir_code, 'myFun.R'))
myear <- 2018

## read in output format sample
name_dat_samp <- readRDS(paste0(dir_code, 'name_dat_samp.rds'))
## measure_info from myDoc.R
measure_info[, measure_label := paste0(myear, measure_label)]

### entitywise rate output
Attri2_final <- fread(paste0(dir.in2, 'Attri2_final.csv'),
                      header = TRUE, colClasses = 'character', encoding = 'UTF-8',
                      nrows = 1)
entityNameList <- names(Attri2_final)[9:27]
LETTERS702 <- c(LETTERS, sapply(LETTERS, function(x) paste0(x, LETTERS)))
measurelist <- paste0(myear, 
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
for (measureName in measurelist){
  measure <- fread(paste0(dir.in, measureName, '.csv'))
  measure[, Rate := as.numeric(Rate)*100]
  measure[, `:=` (score = Rate)]
  ## measure[, `:=` (score = round(as.numeric(Rate) * 100, 2))]
  ## measure[as.numeric(denom) >= 30, `:=` (CI_95lb = as.character(round(as.numeric(CI_95lb)*100, 2)),
  ##                                        CI_95rb = as.character(round(as.numeric(CI_95rb)*100, 2)),
  ##                                        CI_90lb = as.character(round(as.numeric(CI_90lb)*100, 2)),
  ##                                        CI_90rb = as.character(round(as.numeric(CI_90rb)*100, 2)))]
  measure[1:length(entityNameList),
          EntityID := LETTERS702[1:length(entityNameList)]]
  measure[Entity == "Att_OutAN", EntityID := 'ZZ']
  if (measureName %in% measurelist_lowerbetter){
    measure[, score := 1 - score]
  }
  measure.mean <- measure[Entity == 'Attributed']$score
  measure.top_performer <- max(measure[!is.na(EntityID) & !is.na(score) 
                                       & denom >= 30]$score) ## higher rate, better
  measure.top_performer_AN <- max(measure[entityType == "AN" & !is.na(score) 
                                          & denom >= 30]$score)
  # measure.top_performer_FQHC <- max(measure[entityType == "FQHC" & !is.na(score) 
  #                                         & denom >= 30]$score)
  measure.sd <- sd(measure[!is.na(EntityID) & !is.na(score) & denom >= 30]$score)
  measure.sd.AN <- sd(measure[Entity %in% c(measure[entityType == "AN"]$Entity, "Att_OutAN") &
                               denom >= 30 &
                             !is.na(score)]$score)
  LLSDM1_AN <- measure.mean - measure.sd.AN
  LLHSDM1_AN <- measure.mean - 0.5*measure.sd.AN
  ULHSDM1_AN <- measure.mean + 0.5*measure.sd.AN
  ULSDM1_AN <- measure.mean + measure.sd.AN
  # measure.sd.FQHC <- sd(measure[Entity %in% c(measure[entityType == "FQHC"]$Entity, "Att_OutFQHC") & 
  #                                 denom >= 30 & 
  #                              !is.na(score)]$score)
  # LLSDM1_FQHC <- measure.mean - measure.sd.FQHC
  # LLHSDM1_FQHC <- measure.mean - 0.5*measure.sd.FQHC
  # ULHSDM1_FQHC <- measure.mean + 0.5*measure.sd.FQHC
  # ULSDM1_FQHC <- measure.mean + measure.sd.FQHC
  measure[!is.na(EntityID) & EntityID != "ZZ",
          `:=` (state_average = measure.mean)]
  measure[, rate_in_SD := 0]
  measure[!is.na(EntityID) & denom >= 30 & EntityID != "ZZ",
          `:=` (sd = measure.sd,
                rate_in_SD = 1)]
  measure[denom >= 30 & entityType == "AN",
          `:=` (top_performer_AN = measure.top_performer_AN,
                sd.AN = measure.sd.AN,
                LLSDM1_AN = LLSDM1_AN,
                LLHSDM1_AN = LLHSDM1_AN,
                ULHSDM1_AN = ULHSDM1_AN,
                ULSDM1_AN = ULSDM1_AN)]
  measure[denom >= 30 & entityType == "AN" & score < LLSDM1_AN, quality_care_rating_AN := 1
          ][denom >= 30 & entityType == "AN" & score >= LLSDM1_AN & score <= LLHSDM1_AN, quality_care_rating_AN := 2
            ][denom >= 30 & entityType == "AN" & score >= LLHSDM1_AN & score <= ULHSDM1_AN, quality_care_rating_AN := 3
              ][denom >= 30 & entityType == "AN" & score >= ULHSDM1_AN & score <= ULSDM1_AN, quality_care_rating_AN := 4
                ][denom >= 30 & entityType == "AN" & score >= ULSDM1_AN, quality_care_rating_AN := 5
                  ]
  # measure[denom >= 30 & entityType == "FQHC",
  #         `:=` (top_performer_FQHC = measure.top_performer_FQHC,
  #               sd.FQHC = measure.sd.FQHC,
  #               LLSDM1_FQHC = LLSDM1_FQHC,
  #               LLHSDM1_FQHC = LLHSDM1_FQHC,
  #               ULHSDM1_FQHC = ULHSDM1_FQHC,
  #               ULSDM1_FQHC = ULSDM1_FQHC)]
  # measure[denom >= 30 & entityType == "FQHC" & score < LLSDM1_FQHC, quality_care_rating_FQHC := 1
  #         ][denom >= 30 & entityType == "FQHC" & score >= LLSDM1_FQHC & score <= LLHSDM1_FQHC, quality_care_rating_FQHC := 2
  #           ][denom >= 30 & entityType == "FQHC" & score >= LLHSDM1_FQHC & score <= ULHSDM1_FQHC, quality_care_rating_FQHC := 3
  #             ][denom >= 30 & entityType == "FQHC" & score >= ULHSDM1_FQHC & score <= ULSDM1_FQHC, quality_care_rating_FQHC := 4
  #               ][denom >= 30 & entityType == "FQHC" & score >= ULSDM1_FQHC, quality_care_rating_FQHC := 5
  #                 ]
  # measure[
  #   !is.na(Entity) & abs(score - measure.mean) < 1/2 * measure.sd & 
  #     denom >= 30,
  #   quality_care_rating := 3][
  #     !is.na(Entity) & (score - measure.mean) > -1 * measure.sd & (score - measure.mean) < -1/2 * measure.sd & 
  #       denom >= 30,
  #     quality_care_rating := 2][
  #       !is.na(Entity) & (score - measure.mean) < -1 * measure.sd &
  #         denom >= 30,
  #       quality_care_rating := 1][
  #         !is.na(Entity) & (score - measure.mean) > 1/2 * measure.sd & (score - measure.mean) < 1 * measure.sd & 
  #           denom >= 30,
  #         quality_care_rating := 4][
  #           !is.na(Entity) & (score - measure.mean) > 1 * measure.sd & 
  #             denom >= 30,
  #           quality_care_rating := 5]
  measure[!Entity %in% entityNameList, quality_care_rating := NA]
  if (measureName %in% measurelist_lowerbetter){
    measure[!is.na(EntityID) & denom >= 30 & EntityID != "ZZ", `:=`(score = 1 - score,
                                   state_average = 1 - state_average,
                                   top_performer = 1 - top_performer)]
  }
  # score_dat <- rbind(score_dat, measure)
  measure_tb <- measure[, .(Measure, entityType, EntityID, Entity,
                            num, denom, Rate, state_average,
                            top_performer_AN,
                            rate_in_SD,
                            sd, sd.AN,
                            LLSDM1_AN,
                            LLHSDM1_AN,
                            ULHSDM1_AN,
                            ULSDM1_AN,
                            quality_care_rating_AN,
                            # top_performer_FQHC,
                            # sd.FQHC,
                            # LLSDM1_FQHC,
                            # LLHSDM1_FQHC,
                            # ULHSDM1_FQHC,
                            # ULSDM1_FQHC,
                            # top_performer, 
                            # quality_care_rating_FQHC,
                            NULL)] ## modify output csv format
  # measure_tb[, Rate := as.numeric(Rate)*100]
  # measure_tb[!is.na(EntityID), `:=`(
  #   state_average = round(state_average*100, 2),
  #   sd = round(sd*100, 2),
  #   top_performer = round(top_performer*100, 2)
  # )]
  measure_tb[EntityID == "ZZ", EntityID := ""]
  fwrite(measure_tb, paste0(dir.out.tb, measureName, '_table.csv'))
}
## modify output rds format
# score_dat1 <- score_dat[
#   , .(Measure, Entity, score, state_average, top_performer, quality_care_rating)]
# score_dat1 <- score_dat1[Entity %in% c(entityNameList, 'outAN')]
# score_dat2 <- org_crosswalk[score_dat1, on = c('org_abbr' = 'Entity')]
# ## merge 'measure_label'
# score_dat3 <- measure_info[score_dat2, on = c('measure_label' = 'Measure')]
# 
# ## add 'outAN' to template
# score_dat_samp[org_name == "Farmington Valley Medical Associates",
#                `:=`(org_name = 'Yale New Haven Health',
#                     org_url = 'https://www.ynhh.org/')]
# score_dat_samp1 <- unique(score_dat_samp[, .(
#   ## insurance, ## 1
#   measure_name, ## 2
#   measure_category, ## 3
#   measure_def, ## 4
#   measure_hover_col, ## 5
#   measure_hover_row, ## 6
#   measure_abbr,  ## 7
#   measure_link1, ## 8
#   measure_link2, ## 9
#   measure_link3, ## 10
#   measure_link4 ## 11
# )])[, org_name := 'Other Healthcare Providers']
# score_dat_samp2 <- rbind(score_dat_samp[, .(
#   ## insurance, ## 1
#   measure_name, ## 2
#   measure_category, ## 3
#   measure_def, ## 4
#   measure_hover_col, ## 5
#   measure_hover_row, ## 6
#   measure_abbr, ## 7
#   measure_link1, ## 8
#   measure_link2, ## 9
#   measure_link3, ## 10
#   measure_link4, ## 11
#   org_name ## 12
# )], score_dat_samp1)
# 
# ## merge 'score'
# score_dat4 <- score_dat3[score_dat_samp2, on = c('measure_abbr', 'org_name')]
# dim(score_dat4) ## [1] 1273   19
# ## merge `quality_care_rating3`
# rating_info <- unique(score_dat_samp[, .(quality_care_rating, quality_care_rating3)])
# score_dat4 <- rating_info[score_dat4, on = 'quality_care_rating']
# corder <- colnames(score_dat_samp)
# setcolorder(score_dat4, corder)
# 
# ## simplify template
# score_simp <- unique(score_dat4[measure_label %in% measurelist, .(
#   org_name,
#   measure_name,
#   insurance,
#   score,
#   state_average,
#   top_performer,
#   quality_care_rating
# )])
# 
# ## save
# fwrite(score_simp, file.path(dir.out, "score_dat.csv"))

## saveRDS(as.data.frame(score_dat4[, `:=`(measure_label = NULL,
##                                         org_abbr = NULL)]),
##         file = paste0(dir.out, "score_dat.rds"))
