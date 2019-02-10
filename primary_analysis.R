#this file does some primary analysis on the data corresponding to state FL

#save.image("~/data/DARL_Fall2017/GREENWICH/abhi/abhi_data.RData")
#load("~/data/DARL_Fall2017/GREENWICH/abhi/abhi_data.RData")
#savehistory(file = "~/data/DARL_Fall2017/GREENWICH/abhi/abhi_history.Rhistory")
#loadhistory(file = "~/data/DARL_Fall2017/GREENWICH/abhi/abhi_history.Rhistory")

#source("extract_state.R")

#Install readxl if you have never installed it before
#install.packages("readxl")
#install.packages("Hmisc")

library(readxl)
#library(Hmisc)

#Automatically reads the master data and returns a single array.
readMasterData <- function() {
  
  master <- "~/data/DARL_Fall2017/GREENWICH/all_sheets/master_sheets/a0004_stg_table_2_slim.xlsx"
  
  filePaths <- c(
    "~/data/DARL_Fall2017/GREENWICH/all_sheets/master_sheets/a0004_stg_table_2_slim(2).xlsx",
    "~/data/DARL_Fall2017/GREENWICH/all_sheets/master_sheets/a0004_stg_table_2_slim(3).xlsx",
    "~/data/DARL_Fall2017/GREENWICH/all_sheets/master_sheets/a0004_stg_table_2_slim(4).xlsx",
    "~/data/DARL_Fall2017/GREENWICH/all_sheets/master_sheets/a0004_stg_table_2_slim(5).xlsx",
    "~/data/DARL_Fall2017/GREENWICH/all_sheets/master_sheets/a0004_stg_table_2_slim(6).xlsx",
    "~/data/DARL_Fall2017/GREENWICH/all_sheets/master_sheets/a0004_stg_table_2_slim(7).xlsx",
    "~/data/DARL_Fall2017/GREENWICH/all_sheets/master_sheets/a0004_stg_table_2_slim(8).xlsx",
    "~/data/DARL_Fall2017/GREENWICH/all_sheets/master_sheets/a0004_stg_table_2_slim(9).xlsx",
    "~/data/DARL_Fall2017/GREENWICH/all_sheets/master_sheets/a0004_stg_table_2_slim(10).xlsx")
  
  return(readAnyData(master,filePaths))
}

#Automatically reads the role data and returns a single array.
readRoleData <- function() {
  
  role <- "~/data/DARL_Fall2017/GREENWICH/all_sheets/role_sheets/a0004_stg_3_role.xlsx"
  
  filePaths <- c(
    "~/data/DARL_Fall2017/GREENWICH/all_sheets/role_sheets/a0004_stg_3_role(2).xlsx",
    "~/data/DARL_Fall2017/GREENWICH/all_sheets/role_sheets/a0004_stg_3_role(3).xlsx",
    "~/data/DARL_Fall2017/GREENWICH/all_sheets/role_sheets/a0004_stg_3_role(4).xlsx",
    "~/data/DARL_Fall2017/GREENWICH/all_sheets/role_sheets/a0004_stg_3_role(5).xlsx",
    "~/data/DARL_Fall2017/GREENWICH/all_sheets/role_sheets/a0004_stg_3_role(6).xlsx",
    "~/data/DARL_Fall2017/GREENWICH/all_sheets/role_sheets/a0004_stg_3_role(7).xlsx",
    "~/data/DARL_Fall2017/GREENWICH/all_sheets/role_sheets/a0004_stg_3_role(8).xlsx",
    "~/data/DARL_Fall2017/GREENWICH/all_sheets/role_sheets/a0004_stg_3_role(9).xlsx",
    "~/data/DARL_Fall2017/GREENWICH/all_sheets/role_sheets/a0004_stg_3_role(10).xlsx",
    "~/data/DARL_Fall2017/GREENWICH/all_sheets/role_sheets/a0004_stg_3_role(11).xlsx",
    "~/data/DARL_Fall2017/GREENWICH/all_sheets/role_sheets/a0004_stg_3_role(12).xlsx",
    "~/data/DARL_Fall2017/GREENWICH/all_sheets/role_sheets/a0004_stg_3_role(13).xlsx")
  
  return(readAnyData(role,filePaths))
}

#Automatically reads the filter data and returns a single array.
readFilterData <- function() {
  
  filter <- "~/data/DARL_Fall2017/GREENWICH/all_sheets/filter_sheets/a0004_stg_3_skill.xlsx"
  
  filePaths <- c(
    "~/data/DARL_Fall2017/GREENWICH/all_sheets/filter_sheets/a0004_stg_3_skill(2).xlsx",
    "~/data/DARL_Fall2017/GREENWICH/all_sheets/filter_sheets/a0004_stg_3_skill(3).xlsx",
    "~/data/DARL_Fall2017/GREENWICH/all_sheets/filter_sheets/a0004_stg_3_skill(4).xlsx",
    "~/data/DARL_Fall2017/GREENWICH/all_sheets/filter_sheets/a0004_stg_3_skill(5).xlsx",
    "~/data/DARL_Fall2017/GREENWICH/all_sheets/filter_sheets/a0004_stg_3_skill(6).xlsx",
    "~/data/DARL_Fall2017/GREENWICH/all_sheets/filter_sheets/a0004_stg_3_skill(7).xlsx",
    "~/data/DARL_Fall2017/GREENWICH/all_sheets/filter_sheets/a0004_stg_3_skill(8).xlsx",
    "~/data/DARL_Fall2017/GREENWICH/all_sheets/filter_sheets/a0004_stg_3_skill(9).xlsx",
    "~/data/DARL_Fall2017/GREENWICH/all_sheets/filter_sheets/a0004_stg_3_skill(10).xlsx",
    "~/data/DARL_Fall2017/GREENWICH/all_sheets/filter_sheets/a0004_stg_3_skill(11).xlsx",
    "~/data/DARL_Fall2017/GREENWICH/all_sheets/filter_sheets/a0004_stg_3_skill(12).xlsx",
    "~/data/DARL_Fall2017/GREENWICH/all_sheets/filter_sheets/a0004_stg_3_skill(13).xlsx",
    "~/data/DARL_Fall2017/GREENWICH/all_sheets/filter_sheets/a0004_stg_3_skill(14).xlsx",
    "~/data/DARL_Fall2017/GREENWICH/all_sheets/filter_sheets/a0004_stg_3_skill(15).xlsx",
    "~/data/DARL_Fall2017/GREENWICH/all_sheets/filter_sheets/a0004_stg_3_skill(16).xlsx",
    "~/data/DARL_Fall2017/GREENWICH/all_sheets/filter_sheets/a0004_stg_3_skill(17).xlsx",
    "~/data/DARL_Fall2017/GREENWICH/all_sheets/filter_sheets/a0004_stg_3_skill(18).xlsx",
    "~/data/DARL_Fall2017/GREENWICH/all_sheets/filter_sheets/a0004_stg_3_skill(19).xlsx",
    "~/data/DARL_Fall2017/GREENWICH/all_sheets/filter_sheets/a0004_stg_3_skill(20).xlsx",
    "~/data/DARL_Fall2017/GREENWICH/all_sheets/filter_sheets/a0004_stg_3_skill(21).xlsx",
    "~/data/DARL_Fall2017/GREENWICH/all_sheets/filter_sheets/a0004_stg_3_skill(22).xlsx",
    "~/data/DARL_Fall2017/GREENWICH/all_sheets/filter_sheets/a0004_stg_3_skill(23).xlsx",
    "~/data/DARL_Fall2017/GREENWICH/all_sheets/filter_sheets/a0004_stg_3_skill(24).xlsx",
    "~/data/DARL_Fall2017/GREENWICH/all_sheets/filter_sheets/a0004_stg_3_skill(25).xlsx",
    "~/data/DARL_Fall2017/GREENWICH/all_sheets/filter_sheets/a0004_stg_3_skill(26).xlsx",
    "~/data/DARL_Fall2017/GREENWICH/all_sheets/filter_sheets/a0004_stg_3_skill(27).xlsx",
    "~/data/DARL_Fall2017/GREENWICH/all_sheets/filter_sheets/a0004_stg_3_skill(28).xlsx",
    "~/data/DARL_Fall2017/GREENWICH/all_sheets/filter_sheets/a0004_stg_3_skill(29).xlsx",
    "~/data/DARL_Fall2017/GREENWICH/all_sheets/filter_sheets/a0004_stg_3_skill(30).xlsx",
    "~/data/DARL_Fall2017/GREENWICH/all_sheets/filter_sheets/a0004_stg_3_skill(31).xlsx",
    "~/data/DARL_Fall2017/GREENWICH/all_sheets/filter_sheets/a0004_stg_3_skill(32).xlsx",
    "~/data/DARL_Fall2017/GREENWICH/all_sheets/filter_sheets/a0004_stg_3_skill(33).xlsx",
    "~/data/DARL_Fall2017/GREENWICH/all_sheets/filter_sheets/a0004_stg_3_skill(34).xlsx",
    "~/data/DARL_Fall2017/GREENWICH/all_sheets/filter_sheets/a0004_stg_3_skill(35).xlsx",
    "~/data/DARL_Fall2017/GREENWICH/all_sheets/filter_sheets/a0004_stg_3_skill(36).xlsx",
    "~/data/DARL_Fall2017/GREENWICH/all_sheets/filter_sheets/a0004_stg_3_skill(37).xlsx")
  
  return(readAnyData(filter,filePaths))
}

#This function takes in a string filePath for the file with the headers. 
# It then takes a list of the the file paths 
# for all other files without headers.
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

#written with Abhishek's help
extractState <- function(all_data, m_state) {
  return(all_data[all_data$state == m_state,])
}

print("hi1")
#MAIN
all_data <- readMasterData()
role_data <- readRoleData()
filter_data <- readFilterData()
role_H_data <- read_excel("~/data/DARL_Fall2017/GREENWICH/role_hierarchy_hc.xls",skip=0)
va_data <- extractState(all_data, "VA")

print("hi2")
all_roles <- unique(role_data$role)
num_roles <- length(all_roles)
all_skills <- unique(filter_data$skill)
num_skills <- length(all_skills)

all_data_DT <- data.table(all_data)
role_data_DT <- data.table(role_data)
filter_data_DT <- data.table(filter_data)
role_H_data_DT<- data.table(role_H_data)

#adding details about role and skill in the master table
role_data_unique <- role_data_DT[ , lapply(.SD, list), by = job_id]
filter_data_unique <- filter_data_DT[ , lapply(.SD, list), by = job_id]
filter_data_unique$source <- NULL

all_role_data <- merge(all_data,role_data_unique,by="job_id",all.x=TRUE)
all_filter_data <- merge(all_data,filter_data_unique,by="job_id",all.x=TRUE)

all_role_filter_data <- merge(all_role_data,filter_data_unique,by="job_id",all.x=TRUE)
arfd <- all_role_filter_data
num_roles <- length(all_roles)

#sorting of roles and skills lists
#removing the empty role lists to properly apply sapply function later on
arfd <- all_role_filter_data
temp <- arfd[-which(is.na(arfd$role)),]
arfd$role <- NULL
temp <- temp[,c(1,17)] #keeping only the job_id and role columns for merging later
temp$role <- sapply(temp$role,sort)
arfd <- merge(arfd,temp,by="job_id",all.x=TRUE)

temp <- arfd[-which(is.na(arfd$skill)),]
arfd$skill <- NULL
temp <- temp[,c(1,17)] #keeping only the job_id and skill columns for merging later
temp$skill <- sapply(temp$skill,sort)
arfd <- merge(arfd,temp,by="job_id",all.x=TRUE)

# merging of role_sub_family and role_family
#everytime you call a function in r, run the corresponding script file
source('/data/DARL_Fall2017/GREENWICH/abhi/create_role_lists.R')
arfd$job_sub_family <- sapply(arfd$role,crl_sub_family)
arfd$job_family <- sapply(arfd$role,crl_family)


print("hi3")
#idea taken from Zuhao's work, count the occurences of roles and skills

# frequency of roles
temp <- data.frame(matrix(0,1,num_roles))
colnames(temp) <- all_roles
freq_roles <- temp
freq_roles_FL <- temp
freq_roles_VA <- temp
freq_roles_IL <- temp
freq_roles_TX <- temp

# for (i in 1:num_roles){
#   print(i)
#   temp <- arfd[which(arfd$role %in% all_roles[i]),]
#   freq_roles[i] <- nrow(temp)
#   freq_roles_FL[i] <- nrow(temp[which(temp$state=="FL"),])
#   freq_roles_VA[i] <- nrow(temp[which(temp$state=="VA"),])
#   freq_roles_IL[i] <- nrow(temp[which(temp$state=="IL"),])
#   freq_roles_TX[i] <- nrow(temp[which(temp$state=="TX"),])
# }
#the code above doesn't give correct answer as R list is weird and it doesn't search for 
#the role in each list. the idea for the code below was taken from 
#https://stackoverflow.com/questions/9981224/search-for-index-of-a-list-entry-in-r
#and here the inner lapply function coverts the role list arfd$role into separable entries
#from (i think) and then the function sapply actually searches for the corresponding role
#in that seperable list
for (i in 1:num_roles){
  print(i)
  temp <- arfd[which(sapply(lapply(arfd$role,as.character),
                            FUN=function(X) all_roles[i]  %in% X)),]
  freq_roles[i] <- nrow(temp)
  freq_roles_FL[i] <- nrow(temp[which(temp$state=="FL"),])
  freq_roles_VA[i] <- nrow(temp[which(temp$state=="VA"),])
  freq_roles_IL[i] <- nrow(temp[which(temp$state=="IL"),])
  freq_roles_TX[i] <- nrow(temp[which(temp$state=="TX"),])
}
print("hi4")
#for (i in 1:num_roles){
#freq_roles[i] <- nrow(arfd[which(arfd$role %in% all_roles[i]),]) #is very slow
#freq_roles[i] <- nrow(role_data[which(all_roles[i] == role_data$role),])
#}

freq_roles_sorted <- sort(freq_roles,decreasing=TRUE)
temp <- order(freq_roles,decreasing=TRUE)
freq_roles_sorted_FL <- freq_roles_FL[temp]
freq_roles_sorted_VA <- freq_roles_VA[temp]
freq_roles_sorted_IL <- freq_roles_IL[temp]
freq_roles_sorted_TX <- freq_roles_TX[temp]

top_role_freqs = as.vector(freq_roles_sorted,mode="numeric")

op <- par(mar=c(11,4,4,1)) # the 10 allows the names.arg below the barplot
labs <- colnames(freq_roles_sorted)[1:20]
barplot(top_role_freqs[1:20],main="Roles Frequencies",
        names.arg = labs,
        ylab="Frequencies",
        col="skyblue",
        las=2)
rm(op)

# frequency of skills
print("hi5")
freq_skills <- data.frame(matrix(0,1,num_skills))
colnames(freq_skills) <- all_skills
#older way directly from skill dataset, doesn't correspond to actual
#jobs
#for (i in 1:num_skills){
#  freq_skills[i] <- nrow(filter_data[which(all_skills[i] == filter_data$skill),])
#}

for (i in 1:num_skills){
  print(i)
  temp <- arfd[which(sapply(lapply(arfd$skill,as.character),
                            FUN=function(X) all_skills[i]  %in% X)),]
  freq_skills[i] <- nrow(temp)
}

freq_skills_sorted = sort(freq_skills,decreasing=TRUE)
top_skill_freqs = as.vector(freq_skills_sorted,mode="numeric")

op <- par(mar=c(11,4,4,1)) # the 10 allows the names.arg below the barplot
labs <- colnames(freq_skills_sorted)[1:20]
barplot(top_skill_freqs[1:20],main="Skills Frequencies",
        names.arg = labs,
        ylab="Frequencies",
        col="skyblue",
        las=2)
rm(op)

print("hi6")
#average salary for roles
avsal_roles <- freq_roles
for (i in 1:num_roles){
  print(i)
  #the line below is incorrect, it doesn't search through the list as you want to
  temp <- arfd[which(arfd$role %in% all_roles[i]),]
  avsal_roles[i] <- mean(temp$salary)
}

avsal_roles_new <- freq_roles
for (i in 1:num_roles){
  print(i)
  #the line below is incorrect, it doesn't search through the list as you want to
  temp <- arfd[which(sapply(lapply(arfd$role,as.character),
                            FUN=function(X) all_roles[i]  %in% X)),]
  avsal_roles_new[i] <- mean(temp$salary)
}


avsal_roles_sorted = sort(avsal_roles,decreasing=TRUE)
top_role_avsals = as.vector(avsal_roles_sorted,mode="numeric")

op <- par(mar=c(11,4,4,1)) # the 10 allows the names.arg below the barplot
labs <- colnames(avsal_roles_sorted)[1:20]
barplot(top_role_avsals[1:20],main="Roles Average Slaries",
        names.arg = labs,
        ylab="Pays",
        col="skyblue",
        las=2)
rm(op)

print("hi7")
#average salary for roles in individual states
avsal_roles_FL <- freq_roles
avsal_roles_VA <- freq_roles
avsal_roles_IL <- freq_roles
avsal_roles_TX <- freq_roles
for (i in 1:num_roles){
  print(i)
  temp <- arfd[which(arfd$role %in% all_roles[i]),]
  temp_FL <- temp[which(temp$state=="FL"),]
  temp_VA <- temp[which(temp$state=="VA"),]
  temp_IL <- temp[which(temp$state=="IL"),]
  temp_TX <- temp[which(temp$state=="TX"),]
  avsal_roles_FL[i] <- mean(temp_FL$salary)
  avsal_roles_VA[i] <- mean(temp_VA$salary)
  avsal_roles_IL[i] <- mean(temp_IL$salary)
  avsal_roles_TX[i] <- mean(temp_TX$salary)
}

#ordering of avsal for each states w.r.t. in the order of the decreasing 
#overall frequency
#temp <- order(freq_roles,decreasing=TRUE)
#avsal_roles_sorted_FL <- avsal_roles_FL[temp]
#avsal_roles_sorted_VA <- avsal_roles_VA[temp]
#avsal_roles_sorted_IL <- avsal_roles_IL[temp]
#avsal_roles_sorted_TX <- avsal_roles_TX[temp]

temp <- order(avsal_roles_FL,decreasing=TRUE)
avsal_roles_sorted_FL <- avsal_roles_FL[temp]
temp <- order(avsal_roles_VA,decreasing=TRUE)
avsal_roles_sorted_VA <- avsal_roles_VA[temp]
temp <- order(avsal_roles_IL,decreasing=TRUE)
avsal_roles_sorted_IL <- avsal_roles_IL[temp]
temp <- order(avsal_roles_TX,decreasing=TRUE)
avsal_roles_sorted_TX <- avsal_roles_TX[temp]

top_role_avsals_FL <- as.vector(avsal_roles_sorted_FL,mode="numeric")
top_role_avsals_VA <- as.vector(avsal_roles_sorted_VA,mode="numeric")
top_role_avsals_IL <- as.vector(avsal_roles_sorted_IL,mode="numeric")
top_role_avsals_TX <- as.vector(avsal_roles_sorted_TX,mode="numeric")

png(filename="avsal_roles_SW.png")
par(mfrow=c(2,2))
op <- par(mar=c(10,4,4,1))
labs <- colnames(avsal_roles_sorted_FL)[1:10]
barplot(top_role_avsals_FL[1:10],main="Roles Average Salaries in FL",
        names.arg = labs, col="deepskyblue", las=2)
op <- par(mar=c(10,4,4,1))
labs <- colnames(avsal_roles_sorted_VA)[1:10]
barplot(top_role_avsals_VA[1:10],main="Roles Average Salaries in VA",
        names.arg = labs, col="firebrick1", las=2)
op <- par(mar=c(10,4,4,1))
labs <- colnames(avsal_roles_sorted_IL)[1:10]
barplot(top_role_avsals_IL[1:10],main="Roles Average Salaries in IL",
        names.arg = labs, col="orangered1",las=2)
op <- par(mar=c(10,4,4,1))
labs <- colnames(avsal_roles_sorted_TX)[1:10]
barplot(top_role_avsals_TX[1:10],main="Roles Average Salaries in TX",
        names.arg = labs, col="green", las=2)
dev.off()

print("hi8")
#average salary for skills
avsal_skills <- freq_skills
for (i in 1:num_skills){
  print(i)
  #the line below is incorrect, it doesn't search through the list as you want to
  temp <- arfd[which(arfd$skill %in% all_skills[i]),]
  avsal_skills[i] <- mean(temp$salary)
}

avsal_skills_new <- freq_skills
for (i in 1:num_skills){
  print(i)
  temp <- arfd[which(sapply(lapply(arfd$skill,as.character),
                            FUN=function(X) all_skills[i]  %in% X)),]
  avsal_skills_new[i] <- mean(temp$salary)
}

avsal_skills_sorted = sort(avsal_skills,decreasing=TRUE)
top_skill_avsals = as.vector(avsal_skills_sorted,mode="numeric")

op <- par(mar=c(11,4,4,1)) # the 10 allows the names.arg below the barplot
labs <- colnames(avsal_skills_sorted)[1:20]
barplot(top_skill_avsals[1:20],main="Skills Average Slaries",
        names.arg = labs,
        ylab="Pays",
        col="skyblue",
        las=2)
rm(op)

print("hi9")
#average salary for skills in individual states
avsal_skills_FL <- freq_skills
avsal_skills_VA <- freq_skills
avsal_skills_IL <- freq_skills
avsal_skills_TX <- freq_skills

for (i in 1:num_skills){
  print(i)
  temp <- arfd[which(arfd$skill %in% all_skills[i]),]
  temp_FL <- temp[which(temp$state=="FL"),]
  temp_VA <- temp[which(temp$state=="VA"),]
  temp_IL <- temp[which(temp$state=="IL"),]
  temp_TX <- temp[which(temp$state=="TX"),]
  avsal_skills_FL[i] <- mean(temp_FL$salary)
  avsal_skills_VA[i] <- mean(temp_VA$salary)
  avsal_skills_IL[i] <- mean(temp_IL$salary)
  avsal_skills_TX[i] <- mean(temp_TX$salary)
}

print("hi10")
# average time to fill for roles
avttf_roles <- freq_roles
for (i in 1:num_roles){
  print(i)
  temp <- arfd[which(arfd$role %in% all_roles[i] & !is.na(arfd$time_to_fill)),]
  avttf_roles[i] <- mean(temp$time_to_fill)
}

avttf_roles_sorted = sort(avttf_roles,decreasing=TRUE)

#average time to fill for skills
avttf_skills <- freq_skills
for (i in 1:num_skills){
  print(i)
  temp <- arfd[which(arfd$skill %in% all_skills[i] & is.na(arfd$time_to_fill)),]
  avttf_skills[i] <- mean(temp$time_to_fill)
}

avttf_skills_sorted = sort(avttf_skills,decreasing=TRUE)

print("hi11")
#rural-urban analysis
#source of data https://www.census.gov/geo/reference/ua/urban-rural-2010.html
temp <- read_excel("~/data/DARL_Fall2017/GREENWICH/abhi/County_Rural_Lookup_formatted.xlsx",
                   skip=0)
temp$`2015 GEOID` <- NULL
temp$Note <- NULL
temp <- temp[which(temp$State %in% c('VA','TX','IL','FL')),]
county_ur_scale <- temp

#population-density analyis
#source of data https://www.census.gov/population/www/censusdata/density.html
#another thing https://www2.census.gov/geo/
temp <- read.csv("~/data/DARL_Fall2017/GREENWICH/abhi/county_population_densities.csv")
temp$GEO.id <- NULL
temp$GEO.id2 <- NULL
temp$GEO.display.label <- NULL
temp$GCT_STUB.target.geo.id <- NULL
temp$GCT_STUB.target.geo.id2 <- NULL
temp$GCT_STUB.display.label <- as.character(temp$GCT_STUB.display.label)
temp$GCT_STUB.display.label.1 <- as.character(temp$GCT_STUB.display.label.1)
colnames(temp) <- c("area","county","population","housing_units",
                    "total_area_sq_mile","water_area_sq_mile","land_area_sq_mile",
                    "density_population_per_land_sq_mile",
                    "density_housing_units_per_land_sq_mile")
#doing separately as together doesn't work
temp_FL <- temp[which(grepl("United States - Florida",temp$area)),]
temp_IL <- temp[which(grepl("United States - Illinois",temp$area)),]
temp_TX <- temp[which(grepl("United States - Texas",temp$area)),]
temp_VA <- temp[which(grepl("United States - Virginia",temp$area)),]
county_pop_density <- rbind(temp_FL,temp_IL,temp_TX,temp_VA)


print("hi12")
#time analysis
arfd_new <- arfd
arfd_new["post_year"] <- NA
arfd_new["post_month"] <- NA
arfd_new["post_day"] <- NA
arfd_new["post_weekday"] <- NA
# to change the order of columns
arfd_new <- arfd_new[, colnames(arfd_new)[c(1:2,21:23,3:20)]]
temp <- data.frame(date = arfd_new$post_date)
temp <- separate(temp, "date", c("Year", "Month", "Day"), sep = "-")
temp$Year <- as.numeric(temp$Year)
temp$Month <- as.numeric(temp$Month)
temp$Day <- as.numeric(temp$Day)
temp$Month <- month.abb[temp$Month] 
arfd_new$post_year <- temp$Year
arfd_new$post_month <- temp$Month
arfd_new$post_day <- temp$Day
arfd_new$post_weekday <- weekdays(as.Date(arfd_new$post_date))

arfd <- arfd_new

#missing entries
# count how many missing entries from each month of posting date
temp <- arfd[which(is.na(arfd$time_to_fill)),]
temp <- table(temp$post_year,temp$post_month)
temp <- temp[, colnames(temp)[c(5,4,8,1,9,7,6,2,12,11,10,3)]] # for reordering
#output is
#       Jan   Feb   Mar   Apr   May   Jun   Jul   Aug   Sep   Oct   Nov   Dec
#2016     0     0     0     0     0     0     0     0   318  1011  1172  1378
#2017  2574  4975    84   139  2681  6388 12157 37791     0     0     0     0

# handling missing time_to_fill (takes too much time for now)
#indices to the rows for missing time_to_fill
missing_ttf_ind <- which(is.na(arfd$time_to_fill)) 
num_missing_ttfl <- length(missing_ttf_ind)
print(paste0("No. of missing time_to_fill entries in arfd: ", num_missing_ttfl))
print(paste0("No. of missing states in missing time_to_fill rows in arfd: ",
             length(which(is.na(arfd[missing_ttf_ind,]$state)))))
print(paste0("No. of missing cities in missing time_to_fill rows in arfd: ",
             length(which(is.na(arfd[missing_ttf_ind,]$city)))))
# that means we can use the state and city information to 
# fill the missing time_to_fill entries
# a <- 0
# arfd_new <- arfd
# for (i in missing_ttf_ind){
#   a <- a+1
#   print(a)
#   missing_entry_jobid <- arfd[i,]$job_id
#   missing_entry_state <- arfd[i,]$state
#   missing_entry_city <- arfd[i,]$city
#   missing_entry_role <- arfd[i,]$role #verified that all these entries have
#   #just one role
#   temp <- arfd[which((arfd$role %in% missing_entry_role) &
#                        (arfd$city == missing_entry_city) &
#                        (arfd$state == missing_entry_state) &
#                        (!is.na(arfd$time_to_fill)) &
#                        (arfd$job_id != missing_entry_jobid)),]
#   print(length(temp$job_id))
#   if(length(temp$job_id)==0){
#     # if city filter is too much, just do the state average for that role
#     temp <- arfd[which((arfd$role %in% missing_entry_role) &
#                          (arfd$state == missing_entry_state) &
#                          (!is.na(arfd$time_to_fill)) &
#                          (arfd$job_id != missing_entry_jobid)),]
#   }
#   if(length(temp$job_id)!=0){ #only replace if there is something to replace with
#     arfd_new[i,]$time_to_fill <- ceiling(mean(temp$time_to_fill))
#   }
# }

masterFL <- fl_data

id_sar <- masterFL[,c(1,8)]
#hist(id_sar$salary)

hist(id_sar$salary, 
     main="Histogram for salaries in Florida", 
     xlab="Salaries", 
     ylab="# of jobs",
     border="blue", 
     col="green",
     las=1, 
     breaks=200)

slr <- masterFL[,8] #salaries
ttf <- masterFL[,4] #time to fill
slrvar <- slr$salary
ttfvar <- ttf$time_to_fill
plot(slrvar,ttfvar,
     xlab="Salary",
     ylab="Time to fill (in days)",
     pch=21,
     bg=c("gray"),
     main="Slary v/s time to fill in Florida")



