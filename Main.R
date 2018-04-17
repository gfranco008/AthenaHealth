
###########################################################################################*
# Athena Health
# Gustavo Franco Reynoso
# 04/17/2018
# 
# This script will run the two user stories seleceted and save the output.
#
#
###########################################################################################*

#---------------------------------------------------------------------------#
############################# Library Calls #################################
#---------------------------------------------------------------------------#
library(dplyr, warn.conflicts=F)
# library(readr, warn.conflicts=F) for read_csv()
# library(readxl, warn.conflicts=F) for read_excel() xlsx and xls files


#---------------------------------------------------------------------------#
############################# Import ########################################
#---------------------------------------------------------------------------#
# Source al files 
files.choose <- list.files(pattern="*.R")
files.choose <- files.choose[-6]
for (f in files.choose) {
  source(f)
}

indata <- "/Users/gustavo/Desktop/Athena Health Project/MDS_Quality_Measures.csv"

# Read in raw data
data_raw <- tools::file_ext(indata)
if (data_raw == "csv") {
  data_raw <- readr::read_csv(indata, col_types = readr::cols())
} else if (data_raw == "xlsx" | "xls") {
  data_raw <- readxl::read_excel(indata)
} else {
  stop("indata must be a csv, xlsx, or xls file")
} 

# Removes unneeded columns
data <- data_raw[, -c(3, 4, 8, seq(9, 19, 2), 20:24, 26)] 
levels = c("provider", "zip", "state", "national")
for (level in levels){
  change <-  pro_change(data, 406, 1, 2, level = level) # Will ask for a level to print the table.
  write.csv(change, paste("proportional_change", level, ".csv",sep="_"))
  
  pred_values <- Forcast(data, 1) # will forcast however many quarters you put in.
  write.csv(pred_values, paste("predicted_vals", level, ".csv",sep="_"))
}

