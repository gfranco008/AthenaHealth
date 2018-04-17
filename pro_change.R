###########################################################################################*
# Athena Health
# Gustavo Franco Reynoso
# 04/09/2018
# 
# This script will calculate the proportional change in a given measure.
#
# 
# Inputs:
#      data: dataframe containing needed information
#      measure: the name of the desired measure code
#      Qa: Initial quarter to select [Q1, Q2, Q3, Q4]
#      Qb: Final quarter to select [Q1, Q2, Q3, Q4]
#      level: Result level to select, default is provider [provider, zip, state, national]
#      extra: Refers to extra columns you want in the output
# Ouputs:
#       You obtain a table with the names of the providers, and proportional change.
#
###########################################################################################*

pro_change <- function(data, measure, Qa, Qb, level = "provider") {
  
  #-----------------------------------------------------------------------------#
  ############################# Library Calls ###################################
  #-----------------------------------------------------------------------------#
  library(assertr)
  library(assertive, warn.conflicts=F)
  library(dplyr, warn.conflicts=F)
  
  #-----------------------------------------------------------------------------#
  ######################### Unit Test & Pre format ##############################
  #-----------------------------------------------------------------------------#
  #options(warn=-1)
  measure %>% assert_is_a_number()
  Qa %>% assert_is_a_number() 
  quarters <- in_set(1:4)
  assert_all_are_true(quarters(Qa))
  Qb %>% assert_is_a_number()
  assert_all_are_true(quarters(Qb))
  level %>% assert_is_character()
  lev <- in_set("provider", "zip", "state", "national")
  if (lev(level) != TRUE){
    level <- switch(m <- menu(c("provider", "zip", "state", "national"), 
                              title = "Enter an acceptable method:"), 
                    "1" = "provider", 
                    "2" = "zip", 
                    "3" = "state", 
                    "4" = "national")}
  
  # Select the appropriate quarter
  QA <- switch(Qa %>% as.character(),
               "1" = "Q1 Measure Score",
               "2" = "Q2 Measure Score",
               "3" = "Q3 Measure Score",
               "4" = "Q4 Measure Score")
  QB <- switch(Qb %>% as.character(),
               "1" = "Q1 Measure Score",
               "2" = "Q2 Measure Score",
               "3" = "Q3 Measure Score",
               "4" = "Q4 Measure Score")
  
  # Removes unneeded columns
  # data <- data[, -c(3, 4, 8, seq(9, 19, 2), 20:24, 26)]  
  
  
  #-----------------------------------------------------------------------------#
  ############################ Proportional Change  #############################
  #-----------------------------------------------------------------------------#
  # Filter by measure
  datap <-  data %>% as.data.frame() %>% filter(`Measure Code` == measure) 
  # Change na to 0
  datap[is.na(datap)] <- 0
  
  if(level == "provider"){
    datap <- datap %>% aggregate( by = list(datap$`Provider Name`), mean)
    # Calculate change and percent
    datap["Difference"] <- (datap[QB]-datap[QA])
    datap["Percent"] <- ((datap[QB]-datap[QA])/datap[QA])* 100
    results <- datap %>% rename(Provider_Name = Group.1) %>% select_(1, 13, 14) %>% na.omit() 
    results <- results[is.finite(rowSums(results["Percent"])),]
    results["Percent"] <- paste(round(results$Percent, 2), "%", sep="") # Aesthetically more pleasing
  }
  else if(level == "zip"){
    datap <- datap %>% aggregate( by = list(datap$`Provider Zip Code`), mean)
    # Calculate change and percent
    datap["Difference"] <- (datap[QB]-datap[QA])
    datap["Percent"] <- ((datap[QB]-datap[QA])/datap[QA])* 100
    results <- datap %>% rename(Provider_Zip = Group.1) %>% select_(1,13,14) %>% na.omit() 
    results <- results[is.finite(rowSums(results["Percent"])),]
    results["Percent"] <- paste(round(results$Percent, 2), "%", sep="")
    
  }
  else if(level == "state"){
    datap <- datap %>% aggregate( by = list(datap$`Provider State`), mean)
    # Calculate change and percent
    datap["Difference"] <- (datap[QB]-datap[QA])
    datap["Percent"] <- ((datap[QB]-datap[QA])/datap[QA])* 100  
    results <- datap %>% rename(Provider_State = Group.1) %>%  select_(1,13,14) %>% na.omit()
    results["Percent"] <- paste(round(results$Percent, 2), "%", sep="")
  }
  else{
    m <- datap[QB] %>% dim()
    datap["Difference"] <- (sum(datap[QB])-sum(datap[QA]))/m[1]
    datap["Percent"] <- ((sum(datap[QB])-sum(datap[QA]))/sum(datap[QA]))* 100
    results <- datap %>% rename(Provider_Country = Group.1) %>% select_(12,13) %>% na.omit()
    results["Percent"] <- paste(round(results$Percent, 2), "%", sep="") 
    results <-  results %>% slice(1)
     
  }
  
  rownames(results) <- NULL 
  #print(results)
  options(warn=0)
  return(results)
  
}