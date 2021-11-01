# WEGAS_GPS
This repo contains scripts for the automated timestamp correction (- 4 min), GPS coordinate conversion and integration of WEGAS and GPS files from on site field measurements

Required Input files:
* NAME_WEGAS.dat
* NAME_GPS.csv

The script is designed to convert and merge multiple WEGAS / GPS files if provided. 

Navigate to your project directory in R (```setwd("path/to/PDIR")```) containing the respective files and run stable version of the script via: ```source("path/to/WEGAS_GPS_merge.R")```

The merged output csv files will be stored in a new directory called ```merge```.
