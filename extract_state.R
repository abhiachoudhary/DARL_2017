#This script reads the data from all the sheets of master.xlsx which was provided
#the output is four different lists corresponding to each state.

#Install readxl if you have never installed it before
#install.packages("readxl")
#install.packages("Hmisc")

library(readxl)
#library(Hmisc)

readMasterData <- function() {
  
  master <- "~/data/DARL_Fall2017/GREENWICH/all_sheets/master_sheets/a0004_stg_table_2_slim.xlsx"
  
  filePaths <- c(
    "~/data/DARL_Fall2017/GREENWICH//all_sheets/master_sheets/a0004_stg_table_2_slim(2).xlsx",
    "~/data/DARL_Fall2017/GREENWICH//all_sheets/master_sheets/a0004_stg_table_2_slim(3).xlsx",
    "~/data/DARL_Fall2017/GREENWICH//all_sheets/master_sheets/a0004_stg_table_2_slim(4).xlsx",
    "~/data/DARL_Fall2017/GREENWICH//all_sheets/master_sheets/a0004_stg_table_2_slim(5).xlsx",
    "~/data/DARL_Fall2017/GREENWICH//all_sheets/master_sheets/a0004_stg_table_2_slim(6).xlsx",
    "~/data/DARL_Fall2017/GREENWICH//all_sheets/master_sheets/a0004_stg_table_2_slim(7).xlsx",
    "~/data/DARL_Fall2017/GREENWICH//all_sheets/master_sheets/a0004_stg_table_2_slim(8).xlsx",
    "~/data/DARL_Fall2017/GREENWICH//all_sheets/master_sheets/a0004_stg_table_2_slim(9).xlsx",
    "~/data/DARL_Fall2017/GREENWICH//all_sheets/master_sheets/a0004_stg_table_2_slim(10).xlsx")
  
  return(readAnyData(master,filePaths))
}

#This function takes in a string filePath for the file with the headers. It then takes a list
#of the the file paths for all other files without headers.
readAnyData <- function(firstFilePath, otherFilePaths) {
  
  first <- read_excel(firstFilePath, skip=0)
  
  other_files <- lapply(otherFilePaths, read_excel, skip=1)
  
  #fix the column names so that the lists can be rbinded
  for (i in 1:length(other_files)) {
    colnames(other_files[[i]]) <- colnames(first)
  }
  
  #bind all the lists together
  all_data <- Reduce(rbind,other_files,first)
  return(all_data)
}

extractState <- function(all_data, m_state) {
  return(all_data[all_data$state == m_state,])
}

#MAIN
all_data <- readMasterData()
va_data <- extractState(all_data, "VA")
tx_data <- extractState(all_data, "TX")
il_data <- extractState(all_data, "IL")
fl_data <- extractState(all_data, "FL")



