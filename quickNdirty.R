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
comb = merge(dat, gps, by = "TIMESTAMP_GPS", all.x = T, all.y = F)
stopifnot(nrow(comb) == nrow(dat)) # Quick Sum check

dir.create("merged", showWarnings = F)
n = sub(".dat$",".GPS",files)
#write.table(comb, file = paste0("fileMerge.out/",n,".tab),row.names = F)
write.csv(comb, file = paste0("merged/",n,".csv"),row.names = F)

################################################################################

#comb = dat.ls[[1]]
#' Now let's try to establish the filter function Flo was talking about.
#' The idea is that the next 18 measurements (10sec / measurement) after transission
#' from water to air and from air to water should be removed as the system wasn't quite 
#' stable during those measurements. Find a nice way to do that.


