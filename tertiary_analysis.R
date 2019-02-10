# Plot the # of jobs posting with month
temp<-as.data.frame(table(arfd$post_month,arfd$post_year))
temp <- temp[c(12,11,10,3,17,16,20,13,21,19,18,14),] #to order the months
temp1 <- temp
temp1$Var2 <- NULL
temp1$Var1 <- paste(temp$Var1,temp$Var2,sep = " ")
temp <- temp1
colnames(temp) <- c("month","freq")
temp$month <- factor(temp$month, levels=temp$month)
plot_ly(x = ~temp$month, y = ~temp$freq, type = 'bar',) %>%
  layout(yaxis = list(title = 'Count'), margin = list(b = 100),
         xaxis = list(title='Day',ticks = "outside",tickangle=45),
         barmode = 'group', title = '# of Jobs posted with month')

# Plot the # of jobs posting with weekdate 
jobs_with_date_plot <- function(cmpname,mdb){
  #mdb is main database, cmp is company name
  temp <- as.data.frame(table(mdb$post_day))
  colnames(temp) <- c("date","freq")
  p <- plot_ly(x = ~temp$date, y = ~temp$freq, type = 'bar',) %>%
    layout(yaxis = list(title = 'Count'), margin = list(b = 100),
           xaxis = list(title='Date',ticks = "outside",tickangle=45),
           barmode = 'group', 
           title = paste('# of Jobs posted with date of the month \n Company:'
                         ,cmpname))
  return(p)
}
jobs_with_date_plot("All",arfd) #overall plot for month date variations
# same thing for biggest 4 companies (no. of postings wise)
temp <- as.data.frame(table(arfd$company))
colnames(temp) <- c("company","total_freq")
temp <- temp[with(temp, order(-total_freq)), ]
temp <- temp[1:80,] #only keep top 80
#temp[1,]$company #gives Sentara Healthcare
#we see that top 80 companies have >1000 jobs
#mfc_main1 <- arfd[which(arfd$company==temp[1,]$company),]
# assigning to the top 80 companies
for(i in 1:80){
  assign(paste("mfc_main",i,sep = ""), 
         arfd[which(arfd$company==temp[i,]$company),])
}
# not much we can do programmatically with above since the names 
# each time need to be done with paste
# another direct approach
temp2 <- as.data.frame(table(arfd$company,arfd$post_day))
colnames(temp2) <- c("company","date","freq")
#only keep top 80 companies, the ones which are in temp
temp2 <- temp2[which(temp2$company %in% temp$company),] 
temp2 <- merge(temp2,temp,by = "company",all.x = TRUE)
mfctop80 <- temp2[with(temp2,order(-total_freq,date)),]
# mfctop80 now has 80*31 entries in the sorted order of total frequencies
# we want to find out which one of these companies have spikes in the graphs
# when plotted the dates with respect to no. of postings

spikey_companies <- NULL
for (i in 1:80){
  table_this_cmpny <- mfctop80[(31*(i-1)+1):(31*(i-1)+31),]
  y <- as.numeric(table_this_cmpny$freq)
  num_real_spikes <- length(which(y>5*sqrt(var(y))))
  #if(any(y>7*sqrt(var(y)))){
  if(num_real_spikes>0 && num_real_spikes<3){
    spikey_companies <- c(spikey_companies, 
                          as.character(i),
                          as.character(table_this_cmpny[1,]$company))
  }
}

#nothing was observed with the pattern before because companies post in bulk
#on specific day sometime, nothing to do with a particular date pattern

#plotting number of posting with time in each month
temp<-as.data.frame(table(arfd$post_month,arfd$post_day))
colnames(temp) <- c("month","date","freq")
temp <- temp[with(temp,order(month,date)),]
#reordering with time (no pattern)
temp <- temp[c(342:372,311:341,280:310,63:93,125:155,94:124,
               218:248,1:31,249:279,187:217,156:186,32:62),]
plot_ly(x = 1:31, y = ~temp[1:31,]$freq, name="Sep 16",
        type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~temp[32:62,]$freq, name="Oct 16", 
            type='scatter', mode='lines') %>%
  add_trace(y = ~temp[63:93,]$freq, name="Nov 16", 
            type='scatter', mode='lines') %>%
  add_trace(y = ~temp[94:124,]$freq, name="Dec 16", 
            type='scatter', mode='lines') %>%
  add_trace(y = ~temp[125:155,]$freq, name="Jan 17", 
            type='scatter', mode='lines') %>%
  add_trace(y = ~temp[156:186,]$freq, name="Feb 17", 
            type='scatter', mode='lines') %>%
  add_trace(y = ~temp[187:217,]$freq, name="Mar 17", 
            type='scatter', mode='lines') %>%
  add_trace(y = ~temp[218:248,]$freq, name="Apr 17", 
            type='scatter', mode='lines') %>%
  add_trace(y = ~temp[249:279,]$freq, name="May 17", 
            type='scatter', mode='lines') %>%
  add_trace(y = ~temp[280:310,]$freq, name="Jun 17", 
            type='scatter', mode='lines') %>%
  add_trace(y = ~temp[311:341,]$freq, name="July 17", 
            type='scatter', mode='lines') %>%
  add_trace(y = ~temp[342:372,]$freq, name="Aug 17", 
            type='scatter', mode='lines') %>%
  layout(title = "Postings with time",
         xaxis = list(title = "Dates"),
         yaxis = list (title = "# of postings"))


# starting with Monday for each month and truncating in the end
plot_ly(x = 1:28, y = ~temp[c(5:31,4),]$freq, name="Sep 16",
        type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~temp[c(34:61),]$freq, name="Oct 16", 
            type='scatter', mode='lines') %>%
  add_trace(y = ~temp[c(69:93,66:68),]$freq, name="Nov 16", 
            type='scatter', mode='lines') %>%
  add_trace(y = ~temp[c(98:124,97),]$freq, name="Dec 16", 
            type='scatter', mode='lines') %>%
  add_trace(y = ~temp[c(126:153),]$freq, name="Jan 17", 
            type='scatter', mode='lines') %>%
  add_trace(y = ~temp[c(161:186,159:160),]$freq, name="Feb 17", 
            type='scatter', mode='lines') %>%
  add_trace(y = ~temp[c(192:217,190:191),]$freq, name="Mar 17", 
            type='scatter', mode='lines') %>%
  add_trace(y = ~temp[c(220:247),]$freq, name="Apr 17", 
            type='scatter', mode='lines') %>%
  add_trace(y = ~temp[249:276,]$freq, name="May 17", 
            type='scatter', mode='lines') %>%
  add_trace(y = ~temp[c(284:310,283),]$freq, name="Jun 17", 
            type='scatter', mode='lines') %>%
  add_trace(y = ~temp[c(313:340),]$freq, name="July 17", 
            type='scatter', mode='lines') %>%
  add_trace(y = ~temp[c(348:372,345:347),]$freq, name="Aug 17", 
            type='scatter', mode='lines') %>%
  layout(title = "Postings with time \n (months shifted)",
         xaxis = list(title = "Dates"),
         yaxis = list (title = "# of postings"))

# Plot the # of jobs posting with weekday
temp <- as.data.frame(table(arfd$post_weekday))
colnames(temp) <- c("weekday","freq")
temp <- temp[c(2,6,7,5,1,3,4),] #to put weekdays in order
temp$weekday <- factor(temp$weekday, levels=temp$weekday)
plot_ly(x = ~temp$weekday, y = ~temp$freq, type = 'bar',) %>%
  layout(yaxis = list(title = 'Count'), margin = list(b = 100),
         xaxis = list(title='Day',ticks = "outside",tickangle=45),
         barmode = 'group', title = '# of Jobs posted with day of the week')

# Plot the # of jobs filling with weekday
temp <- as.data.frame(table(arfd$fill_weekday))
colnames(temp) <- c("weekday","freq")
temp <- temp[c(2,6,7,5,1,3,4),] #to put weekdays in order
temp$weekday <- factor(temp$weekday, levels=temp$weekday)
plot_ly(x = ~temp$weekday, y = ~temp$freq, type = 'bar',) %>%
  layout(yaxis = list(title = 'Count'), margin = list(b = 100),
         xaxis = list(title='Day',ticks = "outside",tickangle=45),
         barmode = 'group', title = '# of Jobs filled with day of the week')

# Plot the # of jobs filling with date
temp <- arfd[which(!is.na(arfd$fill_date)),]
temp1 <- data.frame(date = temp$fill_date)
temp1 <- separate(temp1, "date", c("Year", "Month", "Day"), sep = "-")
temp$fill_date <- as.numeric(temp1$Day)
temp <- as.data.frame(table(temp$fill_date))
colnames(temp) <- c("date","freq")
plot_ly(x = ~temp$date, y = ~temp$freq, type = 'bar',) %>%
  layout(yaxis = list(title = 'Count'), margin = list(b = 100),
         xaxis = list(title='Day',ticks = "outside",tickangle=45),
         barmode = 'group', title = '# of Jobs filled with date')

#bulk posting analysis  (no behavior here too)
library(dplyr)
library(tidyr)
temp <- as.data.frame(table(arfd$company,arfd$post_date))
colnames(temp) <- c("company","date","freq")
temp <- temp[-which(temp$freq<51),]
temp["day"] <- NA
temp2 <- data.frame(date = temp$date)
temp2 <- separate(temp2, "date", c("Year", "Month", "Day"), sep = "-")
temp2$Day <- as.numeric(temp2$Day)
temp$day <- temp2$Day
temp1 <- summarise(group_by(temp,day), count=sum(freq))

#to see what percent of postings is in bulk by a company
#temp will contain total frequencies for top 200 companies 
temp <- as.data.frame(table(arfd$company))
colnames(temp) <- c("company","total_freq")
temp <- temp[with(temp, order(-total_freq)), ]
temp <- temp[1:200,] #only keep top 200
#temp2 contains date with frequency for all companies
temp2 <- as.data.frame(table(arfd$company,arfd$post_date))
colnames(temp2) <- c("company","date","freq")
#selecting only the ones which are in temp
temp2 <- temp2[which(temp2$company %in% temp$company),] 
#merging for later purposes (basically to keep the total as well)
temp3 <- merge(temp2, temp, all.x = "TRUE")
#summing over the number of postings which are more than 5% of total
#postings on one day
temp3 <- summarise(group_by(temp3,company), 
                   count=sum(freq[which(freq>(total_freq*0.05))]))
colnames(temp3) <- c("company","bulk_freq")
#merge again to put total frequency side by side
temp3 <- merge(temp3, temp, all.x = "TRUE")
#calculate percent bulk
temp3$percent_bulk = temp3$bulk_freq*100/temp3$total_freq
#ordering with respect to total frequency
temp3 <- temp3[with(temp3,order(total_freq)),]
#the command below is dangerous. it doesn't work when there is no entry with bulk_freq=0. 
#in such a situation, it just returns an empty data set instead of the all the rows of original as it should.
#temp3 <- temp3[-which(temp3$bulk_freq==0),] 
temp3 <- temp3[which(temp3$bulk_freq>0),] #use this instead
#plot
#x = ~(1:length(temp3$company))
plot_ly(data = temp3, x = ~temp3$total_freq, 
             y = ~temp3$percent_bulk,
             text = ~paste(temp3$company,
                           "\n Total postings:",temp3$total_freq),
             marker = list(size = 10,
                           color = 'rgba(255, 182, 193, .9)',
                           line = list(color = 'rgba(152, 0, 0, .8)',
                                       width = 2))) %>%
      layout(title = paste('Companies postings in bulk',
                          '\n (among top 200 companies)'),
         yaxis = list(title="Bulk posting percent",
                      zeroline = FALSE),
         xaxis = list(title="Company size (based on # of postings)", 
                      zeroline = FALSE))

# we plot the jobs on time scale based on their time to fill
# the scatter plot will be in two categories, one for the jobs
# which were posted in bulk and second which weren't
#use temp and temp2 from above
#remove the rows with no time_to_fill
mytemp <- arfd[which(!is.na(arfd$time_to_fill)),
               c("job_id","post_date","time_to_fill","company")]
temp <- as.data.frame(table(mytemp$company))
colnames(temp) <- c("company","total_freq")
temp <- temp[with(temp, order(-total_freq)), ]
#nice things are observed when only 1 company is taken say 1st 1:1 or 
#second bigest 2:2 and so on
temp <- temp[1:10,] #only keep top 200
#temp2 contains date with frequency for all companies from mytemp
temp2 <- as.data.frame(table(mytemp$company,mytemp$post_date))
colnames(temp2) <- c("company","post_date","freq")
#selecting only the ones which are in temp
temp2 <- temp2[which(temp2$company %in% temp$company &
                       temp2$freq > 0),] 
#merging for later purposes (basically to keep the total as well)
temp3 <- merge(temp2, temp, all.x = "TRUE")

temp3$post_date <- as.character(temp3$post_date)
big_temp <- merge(mytemp,temp3,all.x = FALSE)
jobs_bulk <- big_temp[which(big_temp$freq>=big_temp$total_freq*0.05),
                      c("company","time_to_fill","total_freq")]
jobs_nonbulk <- big_temp[which(big_temp$freq<big_temp$total_freq*0.05),
                         c("company","time_to_fill","total_freq")]
jobs_bulk <- jobs_bulk[with(jobs_bulk,order(-total_freq)),]
jobs_nonbulk <- jobs_nonbulk[with(jobs_nonbulk,order(-total_freq)),]
#reduce the points in this file to same as bulk one
jobs_nonbulk <- jobs_nonbulk[1:length(jobs_bulk$company),] 

jobs_bulk <- big_temp[which(big_temp$freq>=big_temp$total_freq*0.00),]
jobs_bulk <- jobs_bulk[with(jobs_bulk,order(freq)),]

plot_ly(data = big_temp, x = ~freq, y = ~time_to_fill, 
        color = ~company, showlegend=FALSE,type = 'scatter',
        size=0.012, mode='markers', marker=list(size=~freq/80))%>%
  layout(title = paste('Time of fill for jobs',
                       '\n (Effects of bulk postings)'),
         yaxis = list(title="Time to fill",
                      zeroline = FALSE),
         xaxis = list(title="# of jobs posted on a day", 
                      zeroline = FALSE))

#useless code don't plot (only for reference)
# plot_ly(data = jobs_bulk, x = ~(1:length(jobs_bulk$company)), 
#         y = ~jobs_bulk$time_to_fill,  type = 'scatter',  
#         mode = 'markers', name = "B",
#         marker = list(size = 1.2,
#                       color = 'rgba(255, 82, 93, .9)')) %>%
#   add_trace(data=jobs_nonbulk, x= ~(1:length(jobs_nonbulk$company)), 
#             y = ~jobs_nonbulk$time_to_fill,  mode = 'markers',
#             name="NB",
#             marker = list(size = 1.2,
#                           color = 'rgba(39, 68, 145, .9)'))

#to see what happens with the jobs played in bulk
temp <- as.data.frame(table(arfd$company,arfd$post_date))
colnames(temp) <- c("company","date","freq")
temp <- temp[-which(temp$freq<51),]
#the below corresponds to highest entry
temp1 <- arfd[which(arfd$company=="Baylor Scott & White" & arfd$post_date=="2017-08-24"),]

temp <- as.data.frame(table(arfd$company))
colnames(temp) <- c("company","total_freq")
temp <- temp[with(temp, order(-total_freq)), ]
top_companies <- temp[1:200,] #only keep top 200
temp1 <- arfd[which(arfd$company %in% top_companies$company),] 
temp1 <- temp1[-which(is.na(temp1$time_to_fill)),]
temp <- as.data.frame(table(temp1$company,temp1$post_date,temp1$time_to_fill))
colnames(temp) <- c("company","post_date","time_to_fill","freq")
temp <- temp[-which(temp$freq==0),]

#classification of job w.r.t. job_family
#removing the rows with no job_family
temp <- arfd[-which(is.na(arfd$job_family)),]
print(length(unique(temp$job_family))) #comes out to be 102 different 
#role_families combination

#making the dataframe with average salary of roles and avearge salary in
#individual states for all roles
roles <- all_roles
avsal <- as.numeric(avsal_roles)
avsal_FL <- as.numeric(avsal_roles_FL)
avsal_VA <- as.numeric(avsal_roles_VA)
avsal_IL <- as.numeric(avsal_roles_IL)
avsal_TX <- as.numeric(avsal_roles_TX)
all_avsal_roles_table <- data.frame(roles,avsal,avsal_FL,
                                    avsal_VA,avsal_IL,avsal_TX)

#comapny classification
temp <- unique(arfd$company) 
mygrepl <- function(mysubstrings){
  myoutput <- FALSE
  print("hi")
  for (i in 1:length(mysubstrings)){
    print(i)
    myoutput <- myoutput || grepl(mysubstrings[i],arfd$company,fixed = TRUE)
  }
  return(myoutput)
}
mygrepl1 <- function(mysubstrings){
  myoutput <- grepl(mysubstrings,arfd$company,fixed = TRUE)
  return(myoutput)
}

temp_1 <- arfd[which(sapply(c("Hospital"),mygrepl1)==1),]  
temp_1 <- arfd[which(sapply(c("Hospital"),mygrepl)==1),]  #doesn'work
temp_2 <- arfd[which(sapply("Healthcare",mygrepl1)==1),] 

