#### My Functions
####
## Hongfei Li ####
## hongfei.2.li@uconn.edu ####
## Script create date: 7/29/2018
##
## Total time spent on writing this script: 3 hour 00 min
##


########## Get Medical Claim ##########
#### Please notice, read all column for both 2016 and 2017 year will EXCEED MEMORY!!!!
#### Please notice, read all column for both 2016 and 2017 year will EXCEED MEMORY!!!!
#### Please notice, read all column for both 2016 and 2017 year will EXCEED MEMORY!!!!

GetMC <- function(year = 2017,
                  select = NULL,
                  class = 'measure',
                  dir.raw = 'E:/CT APCD/Hongfei/output/APCD by month/'){
  ## get MC for measurement/calendar year
  ## e.g. measurement year: 2017 refers to Oct.2016-Sep.2017
  ## e.g. calendar year: 2017 refers to Jan.2017-Dec.2017
  ##
  require(data.table)
  if (class == 'measure'){
    pathchr <- paste0(dir.raw, 'MEDICAL_',
                      c(paste0(year-1, "_", sprintf("%02d", 10:12)), paste0(year, "_", sprintf("%02d", 1:9))),
                      ".csv")
  } else if (class == 'calendar'){
    pathchr <- paste0(dir.raw, 'MEDICAL_',
                      c(paste0(year, "_", sprintf("%02d", 1:12))),
                      ".csv")
  } else if (!class %in% 'calendar'){
    stop ('"class" can only be "measure" or "calendar"')
  }
  data <- data.table()
  cat ('start reading data: total', num.chunk <- length(pathchr), 'chunks. \n', as.character(Sys.time()), '\n')
  for (i in 1:num.chunk) {
    data <- rbind(data, datachunk <- fread(pathchr[i],
                                           header = TRUE,
                                           select = select,
                                           colClasses = 'character',
                                           encoding = 'UTF-8'))
    cat(i, '/', num.chunk, 'finished,' , as.character(Sys.time()), '\n')
  }
  return( data )
}


### Example ###
# col.keep <- c('MEDICAL_CLAIM_SERVICE_LINE_ID',
#               'INTERNAL_MEMBER_ID',
#               'PROCEDURE_CODE',
#               'MEDICAL_CLAIM_HEADER_ID',
#               'ORIG_NPI')
# MC_2016 <- GetMC(year = 2016, select = col.keep) #### Please notice, read all column for both 2016 and 2017 year will EXCEED MEMORY!!!!
# MC_2017 <- GetMC(year = 2017, select = col.keep) #### Please notice, read all column for both 2016 and 2017 year will EXCEED MEMORY!!!!
### End of Example ###

## aggreate MC and PC

GetClaim <- function(year = 2017,
                     type = NULL,
                     select = NULL,
                     class = 'measure',
                     dir.raw = 'E:/CT APCD/Hongfei/output/APCD by month'){
  ## get Claim data for measurement/calendar year
  ## e.g. measurement year: 2017 refers to Oct.2016-Sep.2017
  ## e.g. calendar year: 2017 refers to Jan.2017-Dec.2017
  ##
  require(data.table)
  if (! type %in% c('medical', 'pharmacy')) stop('"type" argument can only be "medical" or "pharmacy"')
  type <- toupper(type)
  if (class == 'measure'){
    pathchr <- paste0(dir.raw, '/', type, '_',
                      c(paste0(year-1, "_", sprintf("%02d", 10:12)), paste0(year, "_", sprintf("%02d", 1:9))),
                      ".csv")
  } else if (class == 'calendar'){
    pathchr <- paste0(dir.raw, '/', type, '_',
                      c(paste0(year, "_", sprintf("%02d", 1:12))),
                      ".csv")
  } else if (!class %in% 'calendar'){
    stop ('"class" can only be "measure" or "calendar"')
  }
  data <- data.table()
  cat ('start reading data: total', num.chunk <- length(pathchr), 'chunks. \n', as.character(Sys.time()), '\n')
  for (i in 1:num.chunk) {
    data <- rbind(data, datachunk <- fread(pathchr[i],
                                           header = TRUE,
                                           select = select,
                                           colClasses = 'character',
                                           encoding = 'UTF-8'))
    cat(i, '/', num.chunk, 'finished,' , as.character(Sys.time()), '\n')
  }
  return(data)
}






################## Get Value Set ######################
GetMC_VS <- function(vsname,
                     year = 2017,
                     select = NULL,
                     class = 'measure',
                     dir.raw = 'E:/CT APCD/Hongfei/output/value_set/',
                     takeUnique = TRUE){
  ## get MC for specific Value Set for measurement/calendar year
  ## e.g. measurement year: 2017 refers to Oct.2016-Sep.2017
  ## e.g. calendar year: 2017 refers to Jan.2017-Dec.2017
  ##
  dir.vs <- paste0(dir.raw, '#', vsname, '/')
  if (!dir.exists(dir.vs)){
    stop('"vsname" does not exists, please check it!! ')
  }
  dir.vs.procedure <- paste0(dir.vs, '#PROCEDURE_CODE/')
  dir.vs.revenue <- paste0(dir.vs, '#REVENUE_CODE/')
  pathchr <- character()
  if (class == 'measure'){
    if (dir.exists(dir.vs.procedure)){
      pathchr <- c(pathchr, paste0(dir.vs.procedure, 'MEDICAL_',
                                   c(paste0(year-1, "_", sprintf("%02d", 10:12)), paste0(year, "_", sprintf("%02d", 1:9))),
                                   ".csv"))
    }
    if (dir.exists(dir.vs.revenue)){
      pathchr <- c(pathchr, paste0(dir.vs.revenue, 'MEDICAL_',
                                   c(paste0(year-1, "_", sprintf("%02d", 10:12)), paste0(year, "_", sprintf("%02d", 1:9))),
                                   ".csv"))
    }
    if (length(pathchr) == 0){
      stop('The "pathchr" is empty, please examine')
    }
  } else if (class == 'calendar'){
    if (dir.exists(dir.vs.procedure)){
      pathchr <- c(pathchr, paste0(dir.vs.procedure, 'MEDICAL_',
                                   c(paste0(year, "_", sprintf("%02d", 1:12))),
                                   ".csv"))
    }
    if (dir.exists(dir.vs.revenue)){
      pathchr <- c(pathchr, paste0(dir.vs.revenue, 'MEDICAL_',
                                   c(paste0(year, "_", sprintf("%02d", 1:12))),
                                   ".csv"))
    }
  }
  if (length(pathchr) == 0){
    stop('The "pathchr" is empty, please examine')
  } else if (!class %in% c('measure', 'calendar')){
    stop ('"class" can only be "measure" or "calendar"')
  }
  data <- data.table()
  cat ('start reading data: total', num.chunk <- length(pathchr), 'chunks. \n', as.character(Sys.time()), '\n')
  if (takeUnique == TRUE){
    for (i in 1:num.chunk) {
      data <- unique(rbind(data, datachunk <- fread(pathchr[i],
                                                    header = TRUE,
                                                    select = select,
                                                    colClasses = 'character',
                                                    encoding = 'UTF-8')))
      cat(i, '/', num.chunk, 'finished,' , as.character(Sys.time()), '\n')
    }
  } else if (takeUnique == FALSE){
    for (i in 1:num.chunk) {
      data <- rbind(data, datachunk <- fread(pathchr[i],
                                             header = TRUE,
                                             select = select,
                                             colClasses = 'character',
                                             encoding = 'UTF-8'))
      cat(i, '/', num.chunk, 'finished,' , as.character(Sys.time()), '\n')
    }
  } else if (!takeUnique %in% c(TRUE, FALSE)){
    stop('"takeUnique" option can only be TRUE or FALSE!! Please check!! ')
  }
  return(data)
}


### split NPI by specialty and give the number ###
#' nppes needs to get loaded
split_by_specialty <- function(dat, # data.table format with `NPI` column
                               row.name){
    dat1 <- nppes[dat[!is.na(NPI), .(NPI, internal_member_id)], on = "NPI"]
    dat2 <- taxo_info[, .(taxo, spec1)][dat1, on = c("taxo" = "Taxonomy1")]
    freq <- table(factor(dat2$spec1, levels = spec.order))
    freq1 <- matrix(freq, nrow = 1)
    freq.tb <- data.table(AN = row.name, freq1)
    colnames(freq.tb) <- c("AN", names(freq))
    return(freq.tb)
}

