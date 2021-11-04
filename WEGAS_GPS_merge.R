##################################################
###   WEGAS and GPS file merge and filtering   ###
##################################################

# Author: Hannes Reinwald 
# Contact: hannes-reinwald@web.de

# README: ----------------------------------------------------
# This script is designed to extract, filter and reformat .dat files obtained
# from ..... The .dat file format is a format which ....
# The provided .dat file will be converted into a csv file which can be more 
# easily used for downstream analysis.
# The script is written to support multiple .dat files per project directory.
# 
# Input:
#  - file1.dat (at least one .dat file)
#  
# Run this script by navigating to your project directory containing your dat files
# and executing it via R
# > source("path/to/Script.R")
########

require(dplyr)
require(stringr)

### Check input data ### -------------------------------------------------------
HOME = getwd()
message(paste0("Searching for WEGAS.dat files in:\n",HOME,"\n"))
files = list.files(pattern = "WEGAS.dat")
gpsF  = sub("WEGAS.dat","GPS.csv",files)

# Quick sum check
if(length(files) == 0) {
  warning("No '.dat' files were found!\nPlease check your working directory.\n")
  } else {message("Number of provided 'WEGAS.dat' files:\t",paste(length(files)))}
stopifnot(length(files) > 0)
if(length(gpsF) > length(files)){warning("More GPS files than WEGAS files provided!\n")}
if(length(gpsF) < length(files)){warning("More WEGAS files than GPS files provided!\n")}


### Read-in & merge WEGAS & GPS data function ### ------------------------------

## This function reads in the complete WEGAS file ##
read.WEGAS <- function(wegasFile, rmvDupRecords = F) {
  message(paste("\nReading in file:\t",wegasFile))
  dat = read.csv(wegasFile, na.strings = c("","NAN"), skip = 1, header = T, dec = ".")
  # rmv the first two rows 
  if(dat[1,1:2] == c("TS","RN") && dat[2,3] == "Min"){
    message("First rows containing only units were removed from WEGAS files.")
    dat = dat[-c(1,2),]}
  dat$TIMESTAMP = as.POSIXct(dat$TIMESTAMP, format = "%Y-%m-%d %H:%M:%OS")
  # Columns to convert to numeric values; skip Pic_Time columns!
  num.col = grep("[pP]ic_[tT]ime", colnames(dat), value = T, invert = T)[-1]
  dat[,num.col] = sapply(dat[,num.col],as.numeric) # set rest of variables as numeric
  dat = dat[order(dat$TIMESTAMP), ] # order df after time stamp
  row.names(dat) = NULL
  if(rmvDupRecords == T){# Remove duplicated run records
    message(nrow(dat[duplicated(dat$RECORD),])," duplicate RECORDS removed.")
    return(dat[!duplicated(dat$RECORD),])
  } else {
    return(dat)
  }
}
#x = read.WEGAS(files[1])

## combining WEGAS and GPS with time correction
combineWEGAS.GPS <- function(wegasFile, gpsFile=sub("WEGAS.dat","GPS.csv",wegasFile), corMin=4, ...){
  # wegasFile: path to corresponding WEGAS.dat file
  # gpsFile: path to corresponding GPS file. No need to specify if file names prior WEGAS extension are identical.
  # corMin: correction minutes to substract from WEGAS file time stamp prior merge.
  
  ### WEGAS data import ### ----------------------------------------------------
  dat = read.WEGAS(wegasFile = wegasFile, ...)[,-c(18,35)]
  
  ## TIME Correction in WEGAS ##
  # It is better to correct the time stamp in the WEGAS file as the time in GPS location
  # and future Wind data will correspond directly to time point the sample was taken.
  # MS measurement has a 4 min delay. Easiest way to correct POSIXct time is in seconds, 
  # since they rely on seconds as the major unit of time management. 
  # See: https://www.geeksforgeeks.org/how-to-add-or-subtract-time-span-to-a-datetime-in-r/
  # subtracting sd ds sdsa 
  message(paste("Subtracting",corMin,"min from WEGAS timestamp ..."))
  sec = 60 * corMin
  dat$TIMESTAMP_GPS <- dat$TIMESTAMP - sec # TIMESTAMP_GPS column now contains the corrected time
  
  ### GPS data ### ---------------------------------------------------------------
  message(paste("Reading in file:\t",gpsFile,"..."))
  gps = read.csv2(gpsFile, header = F)[,c(1,6,8)]
  if(ncol(gps) == 1) { gps = read.csv(gpsFile, header = F)[,c(1,6,8)] }
  colnames(gps) <- c("TIMESTAMP_GPS", "Lat_raw", "Long_raw")
  gps$TIMESTAMP_GPS <- as.POSIXct(gps$TIMESTAMP_GPS, format = "%d/%m/%Y %H:%M:%OS") # time stamp formatting
  
  ## NMEA to DD coordinates conversion ##
  NMEA2DD <- function(x){
    # Get coordinates prior "."
    tmp = sub("..[.].+$",".",x)
    # get coordinates around the "."
    tmp1 = round(as.numeric(sub("[.]","",stringr::str_extract(x,"..[.].+$")))/60,2)
    tmp1 = sub("[.]","",as.character(tmp1))
    # bring both together
    tmp2 <- c()
    for(i in 1:length(tmp)){
      tmp2 = append(tmp2, paste0(tmp[i],tmp1[i]))
    }
    return(tmp2)
  }
  
  message("GPS coordinates conversion from NMEA to DD ...")
  gps$Lat <- NMEA2DD(gps$Lat_raw)
  gps$Long <- NMEA2DD(gps$Long_raw)
  
  ### Merge WEGAS & GPS ### ------------------------------------------------------
  # both datasets can by merged by "TIMESTAMP_GPS" column
  message("Merging data frames by WEGAS corrected timestamp")
  comb = merge(x = dat, y = gps, by = "TIMESTAMP_GPS", all.x = T)
  stopifnot(nrow(comb) == nrow(dat)) # Quick Sum check
  message("Done!\n")
  return(comb)
}
#x = combineWEGAS.GPS(files[1], rmvDupRecords = T)


### Import & Merge Data ### ----------------------------------------------------
dat.ls <- list()
wegas.ls <- list()
for(i in files){
  n = sub("_WEGAS.dat$","",i)
  # Check if corresponding GPS file is provided
  if(file.exists(sub("WEGAS.dat","GPS.csv",i))) {
    dat.ls[[n]] = combineWEGAS.GPS(i)
  } else {
    message("No GPS file provided for:\t",i,"\nOnly reading in WEGAS file.")
    wegas.ls[[n]] = read.WEGAS(i)[,-c(18,35)]
  }
}

### Filter out post transition timepoints ### ----------------------------------
filterPostTransi <- function(df, rmvMin=3, sampSec=10, transCol="MeasEQ"){
  mEQ <- data.frame()
  for(i in 1:(nrow(df)-1)){
    tmp <- c( df[i,transCol], df[i+1,transCol] )
    stopifnot(length(tmp) == 2) #SumCheck
    mEQ[i,1:2] <- tmp
  }
  stopifnot(nrow(mEQ) == nrow(df)-1) #SumCheck
  # Get rows in which transition occurs
  Trow <- which(apply(mEQ,1,function(x){ if(x[1] == x[2]){FALSE}else{TRUE} }) == T)
  message("\n",length(Trow)," transition timepoints detected.")
  # Compute rows to be removed after each transition
  n = ceiling((rmvMin * 60)/sampSec)
  message(n," sample timepoints post transition will be removed (",round((n*sampSec/60),1)," min).")
  cutOut <- c()
  for(i in Trow){
    rmv = seq(i, i+n, 1)
    if(max(rmv) > nrow(df)){rmv = seq(i, nrow(df), 1)}
    cutOut <- append(cutOut, rmv)
  }
  # Safety check
  rmvMinMAX = round((min(diff(Trow))*sampSec/60),2)
  stopifnot(rmvMin < rmvMinMAX)
  # clean out df
  df.cl <- df[-cutOut,]
  stopifnot(nrow(df.cl) == nrow(df)-length(cutOut)) #Final Sumcheck
  message(length(cutOut)," timepoints were removed in total.")
  return(df.cl)
}

m = 3 # minutes to be removed post transition timepoint
message("\nRemoving transition timepoints:\t",m," min post transition")
dat2.ls = lapply(dat.ls, filterPostTransi, rmvMin = m)
wegas.ls = lapply(wegas.ls, filterPostTransi, rmvMin = m)
message("Done!\n")

### Exporting merged files ### -------------------------------------------------

# Only transition filtered WEGAS files 
if(length(names(wegas.ls)) > 0){
  out = "wegas.Rout" #output dir
  dir.create(out, showWarnings = F)
  for(i in names(wegas.ls)){
    message(paste("\nExporting transition timepoints filtered WEGAS files for measurement:",i,"..."))
    write.csv(wegas.ls[[i]], file = paste0(out,"/",i,"_WEGAS.",m,"minPTrm.csv"),row.names = F)
    message("Done!\n")
  }
}

# WEGAS GPS merged files & transition filtered tables
if(length(names(dat.ls)) > 0){
  out = "wegas.gps.Rout" #output dir
  dir.create(out, showWarnings = F)
  for(i in names(dat.ls)){
    message(paste("\nExporting merged files for measurement:",i,"..."))
    write.csv(dat.ls[[i]], file = paste0(out,"/",i,"_WEGAS.GPS.csv"),row.names = F)
    write.csv(dat2.ls[[i]], file = paste0(out,"/",i,"_WEGAS.GPS.",m,"minPTrm.csv"),row.names = F) #PT = post transition
    message("Done!\n")
  }
}

rm(i,n)
message("Your output files are stored in:\n",HOME,"/",out,"\n\nJ.A.R.V.I.S. over and out ;)")
##############################