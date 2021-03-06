files = list.files(pattern = "WEGAS.dat")

### WEGAS data ### -------------------------------------------------------------
dat = read.csv(files, na.strings = c("","NAN"), skip = 1, header = T, dec = ".")[-c(1,2), -c(18,35)]
dat[,3:ncol(dat)] <- sapply(dat[,3:ncol(dat)],as.numeric) # set variables as numeric

## TIME Correction in WEGAS ##
# It is better to correct the time stamp in the WEGAS file as the time in GPS location
# and future Wind data will correspond directly to time point the sample was taken.
# MS measurement has a 4 min delay. Easiest way to correct POSIXct time is in seconds, 
# since they rely on seconds as the major unit of time management. 
# See: https://www.geeksforgeeks.org/how-to-add-or-subtract-time-span-to-a-datetime-in-r/ 
dat$TIMESTAMP <- as.POSIXct(dat$TIMESTAMP, format = "%Y-%m-%d %H:%M:%OS")
corMin = 4 #correction minutes = corMin; potential variable for function.
sec = 60 * corMin
dat$TIMESTAMP_GPS <- dat$TIMESTAMP - sec # TIME.adj column now contains the corrected time

### GPS data ### ---------------------------------------------------------------
gps = read.csv2(sub("WEGAS.dat","GPS.csv",files), header = F)[,c(1,6,8)] # 1,6,8
colnames(gps) <- c("TIMESTAMP_GPS", "Lat_raw", "Long_raw")
gps$TIMESTAMP_GPS <- as.POSIXct(gps$TIMESTAMP_GPS, format = "%d/%m/%Y %H:%M:%OS") # time stamp formatting

## NMEA coordinate conversion ##
NMEA2DD <- function(x){
  #x = gps$Lat_raw
  # Get coordinates prior "."
  tmp = sub("..[.].+$",".",x)
  # get coordinates around the "."
  require(stringr)
  tmp1 = round(as.numeric(sub("[.]","",str_extract(x,"..[.].+$")))/60,2)
  tmp1 = sub("[.]","",as.character(tmp1))
  
  tmp2 <- c()
  for(i in 1:length(tmp)){
    tmp2 <- append(tmp2, paste0(tmp[i],tmp1[i]))
  }
  return(tmp2)
}
gps$Lat <- NMEA2DD(gps$Lat_raw)
gps$Long <- NMEA2DD(gps$Long_raw)

### Merge WEGAS & GPS ### ------------------------------------------------------
# both datasets can by merged by "TIMESTAMP_GPS" column
comb = merge(dat, gps, by = "TIMESTAMP_GPS", all.x = T)
stopifnot(nrow(comb) == nrow(dat)) # Quick Sum check

dir.create("merged", showWarnings = F)
n = sub(".dat$",".GPS",files)
#write.table(comb, file = paste0("fileMerge.out/",n,".tab),row.names = F)
write.csv(comb, file = paste0("merged/",n,".csv"), row.names = F)

################################################################################


#' Now let's try to establish the filter function Flo was talking about.
#' The idea is that the next 3 min or 18 measurements (10sec / measurement = 3 min) after 
#' transition from water to air and from air to water should be removed as the system wasn't quite 
#' stable during those measurements. Find a nice way to do that.
#' 
#' MeasEQ column defines if measurement was conducted in water (0) or in air (-1).
#' Hence, find a way to specify the rows in which the values shift from 0 to -1
#' or from -1 to 0 and the remove the following 18 (= 3 min) measurements. 
#df = dat.ls[[1]]

# PARAMETERS 
rmvMin = 3 #remove minutes; removes all measurements after a transition conducted within those minutes
sampSec = 10 # sample seconds; time between each measurement timepoint

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
  message(length(Trow)," transition timepoints detected.")
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
comb2 = filterPostTransi(comb)

n = sub(".dat$",".GPS.clean",files)
write.csv(comb2, file = paste0("merged/",n,".csv"), row.names = F)
### DONE! :) ###


### FINAL NOTE ###
#' It's better to execute the above jobs via a stable Rscript.
#' Rscript can be easily called in R via the command source("path/to/script.R")
#' Here is an easy loop function to run the script on multiple project directories: 

script = "~/DATA/Flo_data/WEGAS_GPS/WEGAS_GPS_merge.R" #This might differ for you
PDIR = c("testRun1","testRun2","testRun3")# Names / Paths for the project directories from your pwd
PWD = getwd()
# Run script in loop for testRun project dir 1 to 3
for(i in PDIR){
  setwd(PDIR)
  source(script)
  setwd(PWD)
}
# Easy ;) 