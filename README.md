# WEGAS_GPS
This repo contains scripts for the automated timestamp correction (- 4 min), GPS coordinate conversion and integration of WEGAS and GPS files from on site field measurements

Required Input files:
* NAME_WEGAS.dat

Optional Input files:
* NAME_GPS.csv

The NAME variable must be consitent between WEGAS.dat and GPS.csv. Multiple WEGAS / GPS files can be provided as the script is designed to convert and merge multiple files. If no GPS file is provided, the script will only read in the WEGAS file and will remove all measurements within 3 min after each transition (post transition = PT) from water to air or air to water.

Navigate to your project directory in R (```setwd("path/to/PDIR")```) containing the respective files. Then run stable version of the script via: ```source("path/to/WEGAS_GPS/WEGAS_GPS_merge.R")```

The merged output csv files will be stored in a new directory called ```wegas.gps.Rout```. For WEGAS files without GPS file the PT filtered files are stored in ```wegas.Rout```. 
