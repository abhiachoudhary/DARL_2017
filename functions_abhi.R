#VERY VERY VERY important to clear the argument of any function
#in R before sourcing the function
if(exists("role_list")){ rm(role_list) }
crl_sub_family <- function(role_list) {
  #role_list is the list of roles for a particular row in arfd
  #if(is.na(role_list)){ #this command doesnt' work
  if(length(role_list[!is.na(role_list)])==0){ #counting the non NA elements
    role_sub_family_list <- NA
    return(role_sub_family_list)
  }
  role_sub_family_list <- NULL
  for (i in 1:length(role_list[[1]])){ #lengths is necessary instead of length
    this_role <- lapply(role_list, `[[`, i)
    index_this_role <- match(this_role,role_H_data$role)
    role_sub_family_list <- c(role_sub_family_list,role_H_data[index_this_role,]$job_sub_family)
  }
  role_sub_family_list <- unique(role_sub_family_list)
  role_sub_family_list <- sort(role_sub_family_list) #already sorting here
  return(role_sub_family_list)
}

if(exists("role_list")){ rm(role_list) }
crl_family <- function(role_list) {
  #role_list is the list of roles for a particular row in arfd
  #if(is.na(role_list)){ #this command doesnt' work
  if(length(role_list[!is.na(role_list)])==0){ #counting the non NA elements
    role_family_list <- NA
    return(role_family_list)
  }
  role_family_list <- NULL
  for (i in 1:length(role_list[[1]])){ #lengths is necessary instead of length
    this_role <- lapply(role_list, `[[`, i)
    index_this_role <- match(this_role,role_H_data$role)
    role_family_list <- c(role_family_list,role_H_data[index_this_role,]$job_family)
  }
  role_family_list <- unique(role_family_list)
  role_family_list <- sort(role_family_list) #already sorting here
  return(role_family_list)
}

if(exists("role_list")){ rm(role_list) }
find_div_index <- function(role_list){
  #print(role_list)
  #this function finds the diversity index as the maximum of the distances
  #between any two roles defining a job
  #using the matrix is not efficient (basically doesn't even run)
  #tempMat <- matrix(0,nrow=length_role_list,ncol=length_role_list)
  div_index <- 0 #just keep track of max
  length_role_list <- length(role_list[[1]])
  for (i in 1:(length_role_list-1)){ #lengths is necessary instead of length
    for (j in (i+1):length_role_list){
      this_role_first <- lapply(role_list, `[[`, i)
      this_role_second <- lapply(role_list, `[[`,j)
      index_this_role_first <- match(this_role_first,all_roles)
      index_this_role_second <- match(this_role_second,all_roles)
      #tempMat[i,j] <- roles_relation_table[index_this_role_first,
       #                                    index_this_role_second]
      temp <- roles_relation_table[cbind(index_this_role_first,
                                   index_this_role_second)]
      if(temp>div_index){
        div_index <- temp
      }
    }
  }
  #div_index <- max(tempMat)
  return(div_index)
}

if(exists("skill_list")){ rm(skill_list) }
find_dex_index <- function(skill_list){
  #this function finds the dexeterity index as the maximum of the distances
  #between any two skills defining a job
  dex_index <- 0 #just keep track of max
  length_skill_list <- length(skill_list[[1]])
  for (i in 1:(length_skill_list-1)){
    for (j in (i+1):length_skill_list){
      this_skill_first <- lapply(skill_list, `[[`, i)
      this_skill_second <- lapply(skill_list, `[[`,j)
      index_this_skill_first <- match(this_skill_first,all_skills)
      index_this_skill_second <- match(this_skill_second,all_skills)
      temp <- skills_relation_table[cbind(index_this_skill_first,
                                         index_this_skill_second)]
      if(temp>dex_index){
        dex_index <- temp
      }
    }
  }
  return(dex_index)
}

if(exists("mylist")){ rm(mylist) }
if(exists("div_index")) { rm(div_index) }
put_zeros <- function(mylist,div_index){
  if(length(mylist[!is.na(mylist)])!=0){ #counting the non NA elements
    if(length(mylist[[1]])==1){
      return(0)
    }
  }
  return(div_index)
}

if(exists("main_role")){rm(main_role)}
if(exists("chosen_state")){rm(chosen_state)}
if(exists("chosen_city")){rm(chosen_city)}
compare_combo_roles <- function(main_role,chosen_state,chosen_city){
  #browser()
  index_main_role <- as.numeric(which(closest_roles_table$roles==main_role))
  #extract entires from arfd which contain this role
  temp <- arfd[-which(is.na(arfd$role)),] #first clear the NA roles rows
  #note that it is VERY VERY VERY VERY IMPORTANT to write the next two lines separately
  #everytime you search something in a list, the function behave weirdly and it doesn't apply
  #all the conditions as it should so they MUST BE SEPERATED.
  temp <- temp[which(sapply(lapply(temp$role,as.character),
                            FUN=function(X) main_role  %in% X)),]
  temp <- temp[which(temp$state==chosen_state),]
  if(missing(chosen_city)){
    chosen_city <- "(All)"
  }
  else{
    temp <- temp[which(temp$city==chosen_city),]
  }
  
  #just the main role
  temp_0 <- temp[which(temp$role==main_role),]
  #include the first closest role
  crole_1 <- as.character(closest_roles_table[index_main_role,]$mc_first.role)
  temp_1 <- temp[which(sapply(lapply(temp$role,as.character),
                              FUN=function(X) crole_1  %in% X)),]
  crole_2 <- as.character(closest_roles_table[index_main_role,]$mc_second.role)
  temp_2 <- temp[which(sapply(lapply(temp$role,as.character),
                              FUN=function(X) crole_2  %in% X)),]
  crole_3 <- as.character(closest_roles_table[index_main_role,]$mc_third.role)
  temp_3 <- temp[which(sapply(lapply(temp$role,as.character),
                              FUN=function(X) crole_3  %in% X)),]
  crole_4 <- as.character(closest_roles_table[index_main_role,]$mc_fourth.role)
  temp_4 <- temp[which(sapply(lapply(temp$role,as.character),
                              FUN=function(X) crole_4  %in% X)),]
  crole_5 <- as.character(closest_roles_table[index_main_role,]$mc_fifth.role)
  temp_5 <- temp[which(sapply(lapply(temp$role,as.character),
                              FUN=function(X) crole_5  %in% X)),]
  y <- c(mean(temp_0$salary),mean(temp_1$salary),
         mean(temp_2$salary),mean(temp_3$salary),
         mean(temp_4$salary),mean(temp_5$salary))
  #plot(y)
  p <- plot_ly(x = ~c(0,1,2,3,4,5), y = ~y, mode = "markers", type = "scatter",
          text = c(main_role,paste("+",crole_1),paste("+",crole_2),paste("+",crole_3),
                   paste("+",crole_4),paste("+",crole_5)), showlegend=FALSE) %>%
    add_text(textfont = list(
      family = "sans serif",
      size = 14,
      color = toRGB("grey50")),
      textposition = "bottom left") %>%
    layout(title = paste('Average salary comparison for',main_role,
                         '\n State-',chosen_state,', City-',chosen_city),
           yaxis = list(title = "Average Salary", zeroline = FALSE),
           xaxis = list(title = paste(main_role,"+"),zeroline = FALSE))
  return(p)
}

if(exists("main_skill")){rm(main_skill)}
if(exists("chosen_state")){rm(chosen_state)}
if(exists("chosen_city")){rm(chosen_city)}
compare_combo_skills <- function(main_skill,chosen_state,chosen_city){
  index_main_skill <- as.numeric(which(closest_skills_table$skills==main_skill))
  #extract entires from arfd which contain this skill
  temp <- arfd[-which(is.na(arfd$skill)),] #first clear the NA skills rows
  #note that it is VERY VERY VERY VERY IMPORTANT to write the next two lines separately
  #everytime you search something in a list, the function behave weirdly and it doesn't apply
  #all the conditions as it should so they MUST BE SEPERATED.
  temp <- temp[which(sapply(lapply(temp$skill,as.character),
                            FUN=function(X) main_skill  %in% X)),]
  temp <- temp[which(temp$state==chosen_state),]
  if(missing(chosen_city)){
    chosen_city <- "(All)"
  }
  else{
    temp <- temp[which(temp$city==chosen_city),]
  }
  
  #just the main skill
  temp_0 <- temp[which(temp$skill==main_skill),]
  #include the first closest skill
  cskill_1 <- as.character(closest_skills_table[index_main_skill,]$mc_first.skill)
  temp_1 <- temp[which(sapply(lapply(temp$skill,as.character),
                              FUN=function(X) cskill_1  %in% X)),]
  cskill_2 <- as.character(closest_skills_table[index_main_skill,]$mc_second.skill)
  temp_2 <- temp[which(sapply(lapply(temp$skill,as.character),
                              FUN=function(X) cskill_2  %in% X)),]
  cskill_3 <- as.character(closest_skills_table[index_main_skill,]$mc_third.skill)
  temp_3 <- temp[which(sapply(lapply(temp$skill,as.character),
                              FUN=function(X) cskill_3  %in% X)),]
  cskill_4 <- as.character(closest_skills_table[index_main_skill,]$mc_fourth.skill)
  temp_4 <- temp[which(sapply(lapply(temp$skill,as.character),
                              FUN=function(X) cskill_4  %in% X)),]
  cskill_5 <- as.character(closest_skills_table[index_main_skill,]$mc_fifth.skill)
  temp_5 <- temp[which(sapply(lapply(temp$skill,as.character),
                              FUN=function(X) cskill_5  %in% X)),]
  y <- c(mean(temp_0$salary),mean(temp_1$salary),
         mean(temp_2$salary),mean(temp_3$salary),
         mean(temp_4$salary),mean(temp_5$salary))
  #plot(y)
  p <- plot_ly(x = ~c(0,1,2,3,4,5), y = ~y, mode = "markers", type = "scatter",
               text = c(main_skill,paste("+",cskill_1),paste("+",cskill_2),paste("+",cskill_3),
                        paste("+",cskill_4),paste("+",cskill_5)), showlegend=FALSE) %>%
    add_text(textfont = list(
      family = "sans serif",
      size = 14,
      color = toRGB("grey50")),
      textposition = "bottom left") %>%
    layout(title = paste('Average salary comparison for',main_skill,
                         '\n State-',chosen_state,', City-',chosen_city),
           yaxis = list(title = "Average Salary", zeroline = FALSE),
           xaxis = list(title = paste(main_skill,"+"),zeroline = FALSE))
  return(p)
}

if(exists("main_role")){rm(main_role)}
if(exists("chosen_state")){rm(chosen_state)}
if(exists("chosen_city")){rm(chosen_city)}
compare_close_roles <- function(main_role,chosen_state,chosen_city){
  #browser()
  #extract entires from arfd which contain this role
  temp <- arfd[-which(is.na(arfd$role)),] #first clear the NA roles rows
  temp <- temp[which(temp$state==chosen_state),]
  if(missing(chosen_city)){
    chosen_city <- "(All)"
  }
  else{
    temp <- temp[which(temp$city==chosen_city),]
  }
  
  index_main_role <- as.numeric(which(closest_roles_table$roles==main_role))
  #roles_relation_table <- 1-roles_relation_table #the diagonal of roles_relation_table should be zero
  #the diagonal should be zero for roles_relation_table after above line
  rrt <- roles_relation_table+diag(271)
  indices_nearest <- t(apply(rrt, 1, order)[ 1:num_roles, ])
  nghbrs_main_role <- indices_nearest[index_main_role,]
  nghbrs_main_role <- nghbrs_main_role[!(nghbrs_main_role==index_main_role)]
  nghbrs_av_sal <- matrix(0, nrow=1, ncol=num_roles)
  #temp1 <- temp[which(sapply(lapply(temp$role,as.character),
   #                         FUN=function(X) main_role %in% X)),]
  temp1 <- temp[which(temp$role==main_role),]
  nghbrs_av_sal[1] <- mean(temp1$salary)
  #note that it is VERY VERY VERY VERY IMPORTANT to write the next two lines separately
  #everytime you search something in a list, the function behave weirdly and it doesn't apply
  #all the conditions as it should so they MUST BE SEPERATED.
  
  for(i in 1:50){
    this_role <- as.character(all_roles[nghbrs_main_role[i]])
    #temp1 <- temp[which(sapply(lapply(temp$role,as.character),
     #                         FUN=function(X) this_role %in% X)),]
    temp1 <- temp[which(temp$role==this_role),]
    nghbrs_av_sal[i+1] <- mean(temp1$salary)
  }
  
  p <- plot_ly(x = ~(0:50), y = ~nghbrs_av_sal[1:51], mode = "markers", type = "scatter") %>%
    layout(title = paste('Average salary of neighbours for',main_role,
                         '\n State-',chosen_state,', City-',chosen_city),
           yaxis = list(title = "Average Salary", zeroline = FALSE),
           xaxis = list(title = paste(main_role),zeroline = FALSE))
  return(p)
}

if(exists("skill_list")){ rm(skill_list) }
avsal_BO_skills <- function(skill_list){
  #this function compute 
  #average salary (avsal) of a job posting based on (BO) skills 
  if(length(skill_list[!is.na(skill_list)])==0){ #if there are no skills
    return(0)
  }
  total_sal <- 0 #just keep track of total salary
  length_skill_list <- length(skill_list[[1]])
  for (i in 1:(length_skill_list)){
      this_skill <- lapply(skill_list, `[[`, i)
      sal_this_skill <- as.numeric(avsal_skills_new[as.character(this_skill)])
      if(sal_this_skill>total_sal){
        total_sal <- sal_this_skill
      }
      #total_sal <- total_sal + sal_this_skill
  }
  avsal <- total_sal
  #avsal <- total_sal/length_skill_list
  return(avsal)
}

if(exists("role_list")){ rm(role_list) }
if(exists("skill_list")){ rm(skill_list) }
give_rows_unique_rs <- function(role_list,skill_list){
  #this function gives the rows which have just one role and one skill
  #by returning true for them
  length_role_list <- length(role_list[[1]])
  length_skill_list <- length(skill_list[[1]])
  if(length_role_list==1){ #if there is one role
    if(length_skill_list==1){ #if there is one skill
      return(TRUE)
    }
  }
  return(FALSE)
}

if(exists("role_list")){ rm(role_list) }
give_rows_two_rs <- function(role_list){
  #this function gives the rows which have exactly two roles
  #by returning true for them
  length_role_list <- length(role_list[[1]])
  if(length_role_list==2){ #if there is one role
    return(TRUE)
  }
  return(FALSE)
}

if(exists("main_role")){rm(main_role)}
if(exists("num_close")){rm(num_close)}
give_corr_mat_role <- function(main_role,num_close){
  #this function computes the correlation matrix for a role w.r.t. the closest
  #num_close roles
  rrt <- roles_relation_table+diag(271)
  indices_nearest <- t(apply(rrt, 1, order)[ 1:num_roles, ])
  nghbrs_main_role <- indices_nearest[index_main_role,]
  nghbrs_main_role <- nghbrs_main_role[!(nghbrs_main_role==index_main_role)]
  nghbrs_main_role <- nghbrs_main_role[]
  corr_mat <- matrix()
  nghbrs_av_sal <- matrix(0, nrow=1, ncol=num_roles)
  #temp1 <- temp[which(sapply(lapply(temp$role,as.character),
  #                         FUN=function(X) main_role %in% X)),]
  temp1 <- temp[which(temp$role==main_role),]
  nghbrs_av_sal[1] <- mean(temp1$salary)
  #note that it is VERY VERY VERY VERY IMPORTANT to write the next two lines separately
  #everytime you search something in a list, the function behave weirdly and it doesn't apply
  #all the conditions as it should so they MUST BE SEPERATED.
  
  for(i in 1:50){
    this_role <- as.character(all_roles[nghbrs_main_role[i]])
    #temp1 <- temp[which(sapply(lapply(temp$role,as.character),
    #                         FUN=function(X) this_role %in% X)),]
    temp1 <- temp[which(temp$role==this_role),]
    nghbrs_av_sal[i+1] <- mean(temp1$salary)
  }
  
  p <- plot_ly(x = ~(0:50), y = ~nghbrs_av_sal[1:51], mode = "markers", type = "scatter") %>%
    layout(title = paste('Average salary of neighbours for',main_role,
                         '\n State-',chosen_state,', City-',chosen_city),
           yaxis = list(title = "Average Salary", zeroline = FALSE),
           xaxis = list(title = paste(main_role),zeroline = FALSE))
  return(p)
}

if(exists("given_state")){rm(given_state)}
if(exists("given_city")){rm(given_city)}
if(exists("given_role_index")){rm(given_role_index)}
extract_city_for_role <- function(given_state,given_city,given_role_index){
  given_role <- colnames(freq_roles_sorted)[given_role_index]
  temp <- arfd[which(arfd$state==given_state & arfd$city==given_city & arfd$role==given_role),
                     c("company","salary","city","state","role","skill","dexterity_index")]
  return(temp)
}

if(exists("given_df")){rm(given_df)}
give_skills_in_df <- function(given_df){
  #this function takes in a data frame and returns the list of all the skills in that data frame
  out <- NA #starting with list() fucks the whole thing up
  for(i in 1:as.numeric(lengths(given_df)[1])){
    this_list <- given_df[i,]$skill
    for (j in 1:length(this_list[[1]])){ #lengths is necessary instead of length
      this_skill <- as.character(lapply(this_list, `[[`, j))
      out <- c(out,this_skill)
    }
  }
  return(out)
}

if(exists("given_role_type")){rm(given_role_type)}
if(exists("custom_roles")){rm(custom_roles)}
corrmat_for_role_type <- function(given_role_type,custom_roles){
  if(missing(custom_roles)){
    temp <- role_H_data[which(role_H_data$job_sub_family==given_role_type),"role"]
    temp <- unique(temp$role)
    custom_roles <- temp[which(temp %in% all_roles)]
  }
  temp <- 1-roles_relation_table[custom_roles,custom_roles]
  #we put the option roles_common_table[...] in above line
  #another option for another line is temp <- 1-roles_relation_table[...] (much lighter)
  dtemp <- diag(temp)
  temp <- temp/dtemp[row(temp)] #dividing each row by diagonal entry
  return(temp)
}

if(exists("custom_skills")){rm(custom_skills)}
corrmat_for_skills <- function(custom_skills){
  temp <- 1-skills_relation_table[custom_skills,custom_skills]
  #we put the option skills_common_table[...] in above line
  #another option for another line is temp <- 1-skills_relation_table[...] (much lighter)
  dtemp <- diag(temp)
  temp <- temp/dtemp[row(temp)] #dividing each row by diagonal entry
  return(temp)
}
