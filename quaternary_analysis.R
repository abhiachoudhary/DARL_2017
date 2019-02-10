#------------------------------------------------------------------------------------
#-----------------------------------THIS PART DOESN'T NEED-------------------------------------------------
#-------------------------------------- TO BE RUN AGAIN----------------------------------------------
#------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------
#roles relaionship table
temp <- arfd[which(!is.na(arfd$role)),]
roles_relation_table <- matrix(0,nrow=num_roles,ncol=num_roles)
roles_common_table <- matrix(0,nrow=num_roles,ncol=num_roles)
for (i in 1:num_roles){
  for (j in 1:num_roles){
    #Jaccard coefficient is defined by |S1 INTERSECTION S2|/|S1 UNION S2|
    #to count S1 INTERSECTION S2
    print(paste(as.character(i),"->",as.character(j)))
    temp <- arfd[which(sapply(lapply(arfd$role,as.character),
                              FUN=function(X) (all_roles[i]  %in% X) &&
                                (all_roles[j]  %in% X))),]
    num_s1ANDs2 <- length(temp$role)
    num_s1ORs2 <- as.numeric(freq_roles[i])+as.numeric(freq_roles[j])-num_s1ANDs2
    if(num_s1ORs2!=0){
      roles_relation_table[i,j] = num_s1ANDs2/num_s1ORs2
    }
    roles_common_table[i,j] <- num_s1ANDs2
  }
}
#to govern the distsance
roles_relation_table <- 1-roles_relation_table
#the diagonal should be zero for roles_relation_table after above line
rrt <- roles_relation_table+diag(271)
#the indices of 5 closest neighbors in each row
indices_nearest <- t(apply(rrt, 1, order)[ 1:5, ])
closest_roles_table <- data.frame(
  roles = all_roles, 
  mc_first = list(index=indices_nearest[,1],role=all_roles[indices_nearest[,1]],dist=0),
  mc_second = list(index=indices_nearest[,2],role=all_roles[indices_nearest[,2]],dist=0),
  mc_third = list(index=indices_nearest[,3],role=all_roles[indices_nearest[,3]],dist=0),
  mc_fourth = list(index=indices_nearest[,4],role=all_roles[indices_nearest[,4]],dist=0),
  mc_fifth = list(index=indices_nearest[,5],role=all_roles[indices_nearest[,5]],dist=0))
for(i in 1:num_roles){
  closest_roles_table[i,]$mc_first.dist =
    roles_relation_table[i,indices_nearest[i,1]]
  closest_roles_table[i,]$mc_second.dist =
    roles_relation_table[i,indices_nearest[i,2]]
  closest_roles_table[i,]$mc_third.dist =
    roles_relation_table[i,indices_nearest[i,3]]
  closest_roles_table[i,]$mc_fourth.dist =
    roles_relation_table[i,indices_nearest[i,4]]
  closest_roles_table[i,]$mc_fifth.dist =
    roles_relation_table[i,indices_nearest[i,5]]
}

#skills relaionship table
temp <- arfd[which(!is.na(arfd$skill)),]
skills_relation_table <- matrix(0,nrow=num_skills,ncol=num_skills)
skills_common_table <- matrix(0,nrow=num_skills,ncol=num_skills)
for (i in 1:num_skills){
  for (j in 1:num_skills){
    #Jaccard coefficient is defined by |S1 INTERSECTION S2|/|S1 UNION S2|
    #to count S1 INTERSECTION S2
    print(paste(as.character(i),"->",as.character(j)))
    temp <- arfd[which(sapply(lapply(arfd$skill,as.character),
                              FUN=function(X) (all_skills[i] %in% X) &&
                                (all_skills[j]  %in% X))),]
    num_s1ANDs2 <- length(temp$skill)
    num_s1ORs2 <- as.numeric(freq_skills[i])+as.numeric(freq_skills[j])-num_s1ANDs2
    if(num_s1ORs2!=0){
      skills_relation_table[i,j] = num_s1ANDs2/num_s1ORs2
    }
    skills_common_table[i,j] <- num_s1ANDs2
  }
}

#to make the colnames and rownames descriptive
colnames(roles_relation_table) <- all_roles
rownames(roles_relation_table) <- all_roles
colnames(roles_common_table) <- all_roles
rownames(roles_common_table) <- all_roles
colnames(skills_relation_table) <- all_skills
rownames(skills_relation_table) <- all_skills
colnames(skills_common_table) <- all_skills
rownames(skills_common_table) <- all_skills

#to govern the distance
skills_relation_table <- 1-skills_relation_table #do it only once
#the diagonal should be zero for skills_relation_table after above line
srt <- skills_relation_table+diag(341)
#the indices of 5 closest neighbors in each row
indices_nearest <- t(apply(srt, 1, order)[ 1:5, ])
closest_skills_table <- data.frame(
  skills = all_skills, 
  mc_first = list(index=indices_nearest[,1],skill=all_skills[indices_nearest[,1]],dist=0),
  mc_second = list(index=indices_nearest[,2],skill=all_skills[indices_nearest[,2]],dist=0),
  mc_third = list(index=indices_nearest[,3],skill=all_skills[indices_nearest[,3]],dist=0),
  mc_fourth = list(index=indices_nearest[,4],skill=all_skills[indices_nearest[,4]],dist=0),
  mc_fifth = list(index=indices_nearest[,5],skill=all_skills[indices_nearest[,5]],dist=0))
for(i in 1:num_skills){
  closest_skills_table[i,]$mc_first.dist =
    skills_relation_table[i,indices_nearest[i,1]]
  closest_skills_table[i,]$mc_second.dist =
    skills_relation_table[i,indices_nearest[i,2]]
  closest_skills_table[i,]$mc_third.dist =
    skills_relation_table[i,indices_nearest[i,3]]
  closest_skills_table[i,]$mc_fourth.dist =
    skills_relation_table[i,indices_nearest[i,4]]
  closest_skills_table[i,]$mc_fifth.dist =
    skills_relation_table[i,indices_nearest[i,5]]
}

#now we have a quantitative relationships between all roles and all skills
#but because these distances are very biased in their distribution, it's
#of not much use.
#next we translate the roles and skills columns into a numeric value by
#making use of these distances. we define two new columns, 
#"diversity-index" and "dexterity-index", as the maximum distance between
#any pair of roles (and skills respectively) which define a job
temp_nozero <- arfd[which(!is.na(arfd$role)),]
#the line below doesn't work
#temp$diversity_index <- sapply(list(temp$role),find_div_index) 
#so will have to do in the loop
temp_nozero$diversity_index <- 0
temp_nozero_noone <- temp_nozero[which(lengths(temp_nozero$role)>1),]

lentemp <- length(temp_nozero_noone$job_id)
for (i in 1:lentemp){
  if(ceiling(i %% 100 ==0)) {print(i)}
  temp_nozero_noone[i,]$diversity_index <- 
    find_div_index(temp_nozero_noone[i,]$role)
}

temp_nozero_noone <- 
  temp_nozero_noone[,c("job_id","diversity_index")]
temp_nozero <- 
  merge(temp_nozero,temp_nozero_noone,by="job_id",all.x=TRUE)
temp_nozero$diversity_index <- 
  rowSums(temp_nozero[, c("diversity_index.x","diversity_index.y")], 
          na.rm = TRUE)
temp_nozero <- temp_nozero[,c("job_id","diversity_index")]
mytemp <- merge(arfd,temp_nozero,by="job_id",all.x=TRUE)
arfd <- mytemp

temp_nozero <- arfd[which(!is.na(arfd$skill)),]
temp_nozero$dexterity_index <- 0
temp_nozero_noone <- 
  temp_nozero[which(lengths(temp_nozero$skill)>1),]

lentemp <- length(temp_nozero_noone$job_id)
for (i in 1:lentemp){
  if(ceiling(i %% 100 ==0)) {print(i)}
  temp_nozero_noone[i,]$dexterity_index <- 
    find_dex_index(temp_nozero_noone[i,]$skill)
}

temp_nozero_noone <- 
  temp_nozero_noone[,c("job_id","dexterity_index")]
temp_nozero <- 
  merge(temp_nozero,temp_nozero_noone,by="job_id",all.x=TRUE)
temp_nozero$dexterity_index <- 
  rowSums(temp_nozero[, c("dexterity_index.x",
                          "dexterity_index.y")], 
          na.rm = TRUE)
temp_nozero <- temp_nozero[,c("job_id","dexterity_index")]
mytemp <- merge(arfd,temp_nozero,by="job_id",all.x=TRUE)
arfd <- mytemp

#VERY DANGEROUS LINE, TAKES FOREVER (LITERALLY, never ended)
#useless thing do not run the line below
#temp$diversity_index <- sapply(temp$role,put_zeros,div_index=temp$diversity_index)

#------------------------------------------------------------------------------------
#-----------------------------------THE PART ABOVE DOESN'T NEED-------------------------------------------------
#---------------------------------------- TO BE RUN AGAIN----------------------------------------------
#------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------

##plotting with dexeterity or diversity index (don't see any meaningful patterns)
temp <- arfd[which(arfd$role=="RN" & arfd$state=="FL" &
                     arfd$city=="Hudson" & !is.na(arfd$dexterity_index)),]
temp <- arfd[which(arfd$role=="Facilities" & arfd$state=="FL" &
                     arfd$city=="Jacksonville" & !is.na(arfd$dexterity_index)),]

## we now look at the salary variation as you include more and more closest roles (upto 5)
#just to see the big cities
table_cities <- as.data.frame(table(arfd$state,arfd$city))
table_cities <- table_cities[-which(table_cities$Freq==0),]
colnames(table_cities) <- c("state","city","freq")
table_cities <- table_cities[order(-table_cities$freq),]
source('~/data/DARL_Fall2017/GREENWICH/abhi/functions_abhi.R')
#define the main role
main_role <- "RN" 
chosen_state <- "FL"
chosen_city <- "Hudson"
#compare_combo_roles(main_role,chosen_state,chosen_city)
compare_combo_roles(colnames(freq_roles_sorted[1]),"FL","Hudson")

##same thing with skill
#define the main skill
compare_combo_skills(colnames(freq_skills_sorted[1]),"FL","Tampa")

##comparing Chicago,IL with Houston,TX (two biggest cities in the dataset)
temp_chicago <- arfd[which(arfd$state=="IL" & arfd$city=="Chicago"),]
temp_houston <- arfd[which(arfd$state=="TX" & arfd$city=="Houston"),]
common_thing <- colnames(freq_roles_sorted[1])
chosen_state1 <- "IL"
chosen_city1 <- "Chicago"
p1 <- compare_combo_roles(common_thing,chosen_state1,chosen_city1)
chosen_state2 <- "TX"
chosen_city2 <- "Houston"
p2 <- compare_combo_roles(common_thing,chosen_state2,chosen_city2)
par(mfrow=c(1,2))
subplot(p1,p2, nrows = 1, margin = 0.1, titleX = FALSE, titleY = FALSE)


compare_close_roles("RN","IL","Chicago")

##none of the things agove worked, so we try to now just focus on the two biggest cities
#and try to see if we see any pattern
temp_chicago <- arfd[which(arfd$state=="IL" & arfd$city=="Chicago" & !is.na(arfd$role)),]

temp <- temp_chicago[which(sapply(lapply(temp_chicago$role,as.character),
                      FUN=function(X) "RN"  %in% X)),
                      c("company","salary","role","skill","dexterity_index")]
temp <- temp[which(temp$role=="RN"),]
RN_chicago <- temp[which(!is.na(temp$skill)),]

RN_chicago_SS <- RN_chicago[which(sapply(lapply(RN_chicago$skill,as.character),
                                  FUN=function(X) "Education"  %in% X)),
                     c("company","salary","role","skill","dexterity_index")]
  

RN_chicago_SS <- RN_chicago_SS[which(RN_chicago_SS$skill=="Education"),]

####THIS IS HOW TO EXTRACT ROWS WITH EXACTLY TWO ROLES THAT WE WANT
temp <- arfd #or whichever dataframe you want to start with
temp2 <- temp[1,] #temporarily start with first row
for(i in 1:length(temp$company)){
  if(give_rows_two_rs(temp[i,]$role)==TRUE){
    temp2 <- rbind(temp2,temp[i,])
  }
}
temp <- temp[2:length(temp2$company),] #remove the first row now 
temp <- temp[which(sapply(lapply(temp$role,as.character),
                          FUN=function(X) "RN"  %in% X)), ]
temp <- temp[which(sapply(lapply(temp$role,as.character),
                          FUN=function(X) "CNA"  %in% X)), ]
####THIS IS HOW TO EXTRACT ROWS WITH EXACTLY TWO ROLES THAT WE WANT ENDED ENDED ENDED

#Afte above line, what we have are the jobs of "RN" with skill "Education" and in Chicago, IL and yet we see a range of 
#salary covereing more than 100k. so that tells us that the data is seriously screwed up.
#other things we see are: if we just take a combination o two roles, say RN and CNA, we see a range
#of salaries, sometimes they come from RN (which is >60000 in chicago) and sometimes they 
#come from CNA (which is <30000 in chicago)

#because we have established that the data is lacking at such finer scales, i.e. we can't even
#narrow down on single role and single frequency and give a predicted salary, we need to constrain
#ourselves to broader views for our analysis. we can start with taking the same role and two different
#(preferrably big) cities and plot the distribution of salaries for that role. quantitatively speaking,
#we could compute the mean, variance etc. among the cities to highlight the differences between them
RN_chicago <- arfd[which(arfd$state=="IL" & arfd$city=="Chicago" & arfd$role=="RN"),
                   c("company","salary","city","state","role","skill","dexterity_index")]
RN_houston <- arfd[which(arfd$state=="TX" & arfd$city=="Houston" & arfd$role=="RN"),
                   c("company","salary","city","state","role","skill","dexterity_index")]
RN_dallas <- arfd[which(arfd$state=="TX" & arfd$city=="Dallas" & arfd$role=="RN"),
                   c("company","salary","city","state","role","skill","dexterity_index")]
temp <- rbind(RN_chicago,RN_houston,RN_dallas)
ggplot(temp, aes(x=salary, colour=city)) + geom_density()

cat('\014')
role_chosen_index <- 7
chosen_cities <- c(2,91)
temp <- data.frame()
temp1 <- extract_city_for_role(table_cities[chosen_cities[[1]],]$state,
                               table_cities[chosen_cities[[1]],]$city,
                               role_chosen_index)
temp2 <- extract_city_for_role(table_cities[chosen_cities[[2]],]$state,
                               table_cities[chosen_cities[[2]],]$city,
                               role_chosen_index)
# do this loop if there are more than two cities
# for(i in chosen_cities){
#   print(i)
#   #temp <- extract_city_for_role(table_cities[i,]$state,table_cities[2,]$city,role_chosen_index)
#   temp1 <- extract_city_for_role(table_cities[i,]$state,table_cities[i,]$city,role_chosen_index)
#   temp <- rbind(temp,temp1)
# }
temp <- rbind(temp1,temp2)
ggplot(temp, aes(x=salary, colour=city)) + geom_density() + 
  ggtitle(paste("Salary Distribution for ",colnames(freq_roles_sorted)[role_chosen_index]))

#to check high paying skills etc.
cat('\014')
temp1_low <- temp1[which(temp1$salary>60000 & temp1$salary<150000 & !is.na(temp1$skill)),]
temp1_high <- temp1[which(temp1$salary>40000 & temp1$salary<130000 & !is.na(temp1$skill)),]
low_skills <- give_skills_in_df(temp1_low)
high_skills <- give_skills_in_df(temp1_high)
sort(table(low_skills),decreasing=TRUE)
#sort(table(high_skills),decreasing=TRUE)
temp2_low <- temp2[which(temp2$salary>60000 & temp2$salary<125000 & !is.na(temp2$skill)),]
temp2_high <- temp2[which(temp2$salary>20000 & temp2$salary<130000 & !is.na(temp2$skill)),]
low_skills <- give_skills_in_df(temp2_low)
high_skills <- give_skills_in_df(temp2_high)
sort(table(low_skills),decreasing=TRUE)
#sort(table(high_skills),decreasing=TRUE)
#low_skills <- c(give_skills_in_df(temp1_low),give_skills_in_df(temp2_low))
#high_skills <- c(give_skills_in_df(temp1_high),give_skills_in_df(temp2_high))


#(number 1 and 2) chicago (avsal = 69046) and houston (avsal = 59332). Here only general 
# observation we would give is this: the average salary difference between the two cities 
# is ~10k, and most of this comes from roles which are non-patient facing. In other words,
# the jobs with parimary roles in patient care offer almost the same salary in Chicago 
# and Houston even though the per capita income in two cities is significantly different.

#(number 2 and 3) houston (avsal = 59332) and dallas (avsal = 60110) are pretty similar 
#in all roles except in 1(RN), 6(LPN), 7(executive), 11 (similar but interetsting), 
# 12 (business services), 19 (logisctics), 22 (audit)

#(number 6 and 7) tampa (avsal = 57624) and orlando (avsal=57180) are good basis for comparison
#as they are pretty similar in number of total postings. what we see is that for most roles
# there salary distributions coincide, except 10, 11, 12 (somewhat), 13 and 15 
#(in a different way), 16, 19, 20, 21, 22, 23, 24 (among top 15)

#(number 2 and 42) houston (avsal = 59332) and texas city (avsal = 78118) are good examples
#of cities which aren't that close on the number of jobs. Here, 4 (CNA), 6 (LPN), 7, 10

#(numer 2 and 91) houston (avsal = 59332) and midland (avsal = 49033.15). 
#Here, 7, 9, 14, 17, 18, 19 (very), 

#Because the skills_relationship_table and skill_relationship_table have very 
#uneven entries, and we don't see any strong relationship other than any role (and skill) to itself
#we use anohter way we look at the roles_common_table (and skills_common_table) and for each row
#and consider each row as containing it's "commonality" with other roles
#we obtain these commonalities by dividing each row by the diagonal entry because diagonal 
#entry shows the number of rows in which that role appears.

#just doing with first 10 roles
temp <- 1-roles_relation_table
dtemp <- diag(temp)
temp <- temp/dtemp[row(temp)] #dividing each row by diagonal entry
corrplot(temp[1:10,1:10])

#doing with respect to one category of roles
corrplot(corrmat_for_role_type("Administration"))
corrplot(corrmat_for_role_type("Healthcare Management"))
corrplot(corrmat_for_role_type("Lab/Research"))
corrplot(corrmat_for_role_type("Pharmacy"))
corrplot(corrmat_for_role_type("Other"))
corrplot(corrmat_for_role_type("Executive"))
corrplot(corrmat_for_role_type("Nursing"))
corrplot(corrmat_for_role_type("IT"))
corrplot(corrmat_for_role_type("Dental"))
corrplot(corrmat_for_role_type("Business Services"))

#custom list of roles
temp <- c("RN","Facilities","LPN","CNA","Case Manager")
corrplot(corrmat_for_role_type("",temp))
temp <- c("Business Analyst","Healthcare Data Analyst","Medicare Analyst","Data Specialist","Business Services")
corrplot(corrmat_for_role_type("",temp))
temp <- all_roles[which(grepl("Coordinator", all_roles))]
corrplot(corrmat_for_role_type("",temp))
temp <- all_roles[which(grepl("Manager", all_roles))]
corrplot(corrmat_for_role_type("",temp))
temp <- all_roles[which(grepl("Therapist", all_roles))]
corrplot(corrmat_for_role_type("",temp))
temp <- all_roles[which(grepl("Specialist", all_roles))] #not much useful
corrplot(corrmat_for_role_type("",temp))
temp <- all_roles[which(grepl("Data", all_roles))] #not much useful
corrplot(corrmat_for_role_type("",temp))
temp <- all_roles[which(grepl("Surg", all_roles))]
corrplot(corrmat_for_role_type("",temp))
temp <- all_roles[which(grepl("Technician", all_roles))] #not much useful
corrplot(corrmat_for_role_type("",temp))
temp <- all_roles[which(grepl("Nurse", all_roles))] #not much useful
corrplot(corrmat_for_role_type("",temp))
temp <- all_roles[which(grepl("Medical", all_roles))] #not much useful
corrplot(corrmat_for_role_type("",temp))
temp <- all_roles[which(grepl("Analyst", all_roles))]
corrplot(corrmat_for_role_type("",temp))
temp <- all_roles[which(grepl("Lab", all_roles))] #not much useful
corrplot(corrmat_for_role_type("",temp)) 

#custom list of skills
temp <- c("Healthcare Law","Medical Law")
corrplot(corrmat_for_skills(temp))
temp <- all_skills[which(grepl("Care", all_skills))]
corrplot(corrmat_for_skills(temp))
temp <- all_skills[which(grepl("Communica", all_skills))]
corrplot(corrmat_for_skills(temp))
temp <- all_skills[which(grepl("Medical", all_skills))]
corrplot(corrmat_for_skills(temp))
temp <- all_skills[which(grepl("Manage", all_skills))] #does not help
corrplot(corrmat_for_skills(temp))
temp <- all_skills[which(grepl("Health", all_skills))]
corrplot(corrmat_for_skills(temp))
temp <- all_skills[which(grepl("Thera", all_skills))]
corrplot(corrmat_for_skills(temp))
temp <- c("Education","Training","Patient Care","Hospital","Compliance",
          "Customer Service","Acute Care","Billing","Clinic","Security")
corrplot(corrmat_for_skills(temp))
temp <- all_skills[which(grepl("Medicine", all_skills))] #doesn't help much
corrplot(corrmat_for_skills(temp))

#fill the missing counties
temp <- arfd[which(is.na(arfd$county)),]
temp_df <- as.data.frame(table(temp$state,temp$city))
temp_df$Freq <- NULL
colnames(temp_df) <- c("state","city")
#use of external dataset to get information about cities and counties
df1 <- read.csv("~/data/DARL_Fall2017/GREENWICH/abhi/zip_codes_states.csv")
df1 <- df1[which(df1$state %in% c('VA','TX','IL','FL')),
           c("city","state","county")]
df1 <- df1[!duplicated(df1), ]
temp_df <- merge(temp_df,df1,all.x="TRUE") #still more than half missing
#actual updating of ardf
temp <- arfd
df1 <- read.csv("~/data/DARL_Fall2017/GREENWICH/abhi/zip_codes_states.csv")
df1 <- df1[which(df1$state %in% c('VA','TX','IL','FL')),
           c("city","state","county")]
df1 <- df1[!duplicated(df1), ]
nrow(temp[which(is.na(temp$county)),]) #gives 100474
temp <- merge(temp,df1,by=c("state","county","city"),
              all.x = TRUE,all.y = FALSE)
temp <- temp[,colnames(arfd)] #to reorder the columns again
nrow(temp[which(is.na(temp$county)),]) #still gives 100474 

#PCA PCA PCA PCA
temp <- arfd
temp$diversity_index <- sample(roles_relation_table,length(arfd$job_family),
                               replace = TRUE)
temp$diversity_index <- (temp$salary)/50000
temp1 <- temp[,c("time_to_fill","salary","latitude","longitude","diversity_index")]
temp1$latitude <- as.numeric(temp1$latitude)
temp1$longitude <- as.numeric(temp1$longitude)
temp1 <- temp1[which(!is.na(temp1$time_to_fill) & 
                       !is.na(temp1$latitude) & !is.na(temp1$longitude)),]
