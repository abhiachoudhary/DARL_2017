#contribution of frequency for roles from each state
Roles <- colnames(freq_roles_sorted)
Roles_FL <- as.vector(freq_roles_sorted_FL,mode="numeric")
Roles_VA <- as.vector(freq_roles_sorted_VA,mode="numeric")
Roles_IL <- as.vector(freq_roles_sorted_IL,mode="numeric")
Roles_TX <- as.vector(freq_roles_sorted_TX,mode="numeric")
Roles <- Roles[2:15]
Roles_FL <- Roles_FL[2:15]
Roles_VA <- Roles_VA[2:15]
Roles_IL <- Roles_IL[2:15]
Roles_TX <- Roles_TX[2:15]

#using https://plot.ly/r/bar-charts/
library(plotly)
data <- data.frame(Roles, Roles_FL, Roles_VA, Roles_IL, Roles_TX)
#to keep the same order instead of deafault alphabetical, we do
data$Roles <- factor(data$Roles, levels=Roles) 

plot_ly(data, x = ~Roles, y = ~Roles_FL, type = 'bar', 
        name = 'FL portion') %>%
  add_trace(y = ~Roles_VA, name = 'VA portion') %>%
  add_trace(y = ~Roles_IL, name = 'IL portion') %>%
  add_trace(y = ~Roles_TX, name = 'TX portion') %>%
  layout(yaxis = list(title = 'Count'), margin = list(b = 100),
         xaxis = list(title='',ticks = "outside",tickangle=45),
         barmode = 'stack')

# comparison of salaries for roles in different states
Roles <- colnames(freq_roles_sorted)
Salaries_FL <- as.vector(avsal_roles_sorted_FL,mode="numeric")
Salaries_VA <- as.vector(avsal_roles_sorted_VA,mode="numeric")
Salaries_IL <- as.vector(avsal_roles_sorted_IL,mode="numeric")
Salaries_TX <- as.vector(avsal_roles_sorted_TX,mode="numeric")
Roles <- Roles[1:10]
Salaries_FL <- Salaries_FL[1:10]
Salaries_VA <- Salaries_VA[1:10]
Salaries_IL <- Salaries_IL[1:10]
Salaries_TX <- Salaries_TX[1:10]

library(plotly)
data <- data.frame(Roles, Salaries_FL, Salaries_VA, Salaries_IL, Salaries_TX)
#to keep the same order instead of deafault alphabetical, we do
data$Roles <- factor(data$Roles, levels=Roles) 

plot_ly(data, x = ~Roles, y = ~Salaries_FL, type = 'bar',
        name = 'FL Salary') %>%
  add_trace(y = ~Salaries_VA, name = 'VA Salary') %>%
  add_trace(y = ~Salaries_IL, name = 'IL Salary') %>%
  add_trace(y = ~Salaries_TX, name = 'TX Salary') %>%
  layout(yaxis = list(title = 'Count'), margin = list(b = 100),
         xaxis = list(title='',ticks = "outside",tickangle=45),
         barmode = 'group')

#clustering of roles with respect to frequencies and average salaries
#decreasing frequency (no pattern)
#png(filename="clustering_roles_frequency.png")
temp <- freq_avsal_roles[order(freq_avsal_roles$frequency,
                               decreasing = TRUE),]
temp <- temp[1:30,]
set.seed(123456789) ## to fix the random starting clusters
grpRoles <- kmeans(temp[,c("frequency","average_salary")], 
                   centers=5, nstart=10)
# to see the cluster assignments
o=order(grpRoles$cluster)
data.frame(temp$role[o],grpRoles$cluster[o])
plot(temp$average_salary, temp$frequency,
     type="n", xlab="Average Salary", ylab="Frequency")
text(x=temp$average_salary, y=temp$frequency, col=grpRoles$cluster+1)
#dev.off()

#decreasing salaries (pattern)
png(filename="clustering_roles_average_salary.png")
temp <- freq_avsal_roles[order(freq_avsal_roles$average_salary,
                               decreasing = TRUE),]
temp <- temp[1:20,]
set.seed(123456789) ## to fix the random starting clusters
grpRoles <- kmeans(temp[,c("frequency","average_salary")], 
                   centers=4, nstart=10)
# to see the cluster assignments
o=order(grpRoles$cluster)
data.frame(temp$role[o],grpRoles$cluster[o])
plot(temp$average_salary, temp$frequency,
     type="n", xlab="Average Salary", ylab="Frequency")
text(x=temp$average_salary, y=temp$frequency, col=grpRoles$cluster+1)
dev.off()

#plotting of avearge time to fill with respect to average salaray
role <- all_roles
average_ttf <- as.vector(avttf_roles,mode="numeric")
average_salary <- as.vector(avsal_roles,mode="numeric")
frequency <- as.vector(freq_roles,mode="numeric")
temp = data.frame(role, average_ttf, average_salary, frequency)
avttf_avsal_roles <- temp[which(!is.na(temp$average_ttf) 
                                & !is.na(temp$average_salary)
                                & !is.na(temp$frequency)),]
#plot(temp$average_ttf,temp$average_salary)

#clustering of roles with respect to frequencies on average salary 
#and average time to fill scale
temp <- avttf_avsal_roles[order(avttf_avsal_roles$frequency,
                               decreasing = TRUE),]
temp <- temp[2:lengths(temp)[1],]
#temp <- temp[1:30,]
set.seed(123456789) ## to fix the random starting clusters
#grpRoles <- kmeans(temp[,c("average_ttf","average_salary")], 
 #                  centers=5, nstart=10)
grpRoles <- kmeans(temp[,c("frequency")], 
                   centers=5, nstart=5000)
#the labeling of clusters by default is not sorted so we change that

grpRoles$cluster[1:2] <- c(1,1)
grpRoles$cluster[3:5] <- c(2,2,2)
grpRoles$cluster[6:13] <- rep(3,8)
#grpRoles$cluster[39:103] <- rep(6,65)
grpRoles$cluster[104:244] <- rep(6,141)

# to see the cluster assignments
o=order(grpRoles$cluster)
data.frame(temp$role[o],grpRoles$cluster[o], temp$frequency[o])

plot(temp$average_ttf, temp$average_salary, 
     col =(grpRoles$cluster),
     main="Clustering with respect to frequency", 
     pch=20, cex=temp$frequency/10000,
     ylab = "Average Sarlay",
     xlab = "Average time to Fill")
legend("bottomright", 
       legend = c(">15000","8000-10000", "3000-8000", 
                  "1000-3000", "100-1000","1-100"), 
       pch=20, col=1:6, pt.cex=1, cex = 0.7)

#the same plot using plot_ly (to do)
#https://plot.ly/r/line-and-scatter/
 plot_ly(
   x = ~temp$average_ttf, y = ~temp$average_salary,
   color = ~grpRoles$cluster, size = ~temp$frequency/10000,
   mode = "markers"
 ) %>%
   layout(title = "Clustering with respect to Frequency",
          xaxis=list(title="Average time to fill"),
          yaxis=list(title="Average Salary"))

#putting the jobs on US-map
#city and state wise frequency table
temp <- as.data.frame(table(arfd$city,arfd$state))
#the above command works but what is SUPER WEIRD is that the two commands 
#below which should do exactly the same thing don't work in the place of 
#the above command
#temp <- table(arfd$city,arfd$state)
#temp <- as.data.frame(temp)
colnames(temp)[1] <- "city"
colnames(temp)[2] <- "state"
colnames(temp)[3] <- "freq"
#removing the rows corresponding to non existent cities in the all states
temp <- temp[-which(temp$freq==0),]

#downloaded from
#https://www.gaslampmedia.com/download-zip-code-latitude-
#     longitude-city-state-county-csv/
df1 <- read.csv("~/data/DARL_Fall2017/GREENWICH/abhi/zip_codes_states.csv")
df1 <- df1[which(df1$state %in% c('VA','TX','IL','FL')),]
df1$zip_code <- NULL
df1$county <- NULL
df1 <- aggregate(.~city+state, df1, mean)

df1 <- merge(df1,temp,by=c("city","state"))
colnames(df1)[3] <- "lat"
colnames(df1)[4] <- "lon"

#doesn't list all the cities in the arfd file but then we never had all the lat
#and lon in the first place in master file.

library(plotly)
#using https://plot.ly/r/bubble-maps/ downloaded csv file is below
#df <- read.csv("~/data/DARL_Fall2017/GREENWICH/abhi/2014_us_cities.csv")

df1$q <- with(df1, cut(freq, quantile(freq)))
levels(df1$q) <- paste(c("1st", "2nd", "3rd", "4th", "5th"), "Quantile")
df1$q <- as.ordered(df1$q)

g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showland = TRUE,
  landcolor = toRGB("gray85"),
  subunitwidth = 1,
  countrywidth = 1,
  subunitcolor = toRGB("white"),
  countrycolor = toRGB("white")
)
t <- list(
  family = "sans serif",
  size = 12,
  color = 'blue')

plot_geo(df1, locationmode = 'USA-states', sizes = c(1,75)) %>%
  add_markers(
    x = ~lon, y = ~lat, size = ~freq, hoverinfo = "text",
    text = ~paste(df1$city, "<br />", df1$freq, " jobs")
  ) %>%
  layout(title=paste('# of healthcare jobs-citywise',
                     '<br>   '), font=t, geo = g)

#to do: put color/shade (read the reference) in above based on urban rural thing

# role specific salary in different states (at different locations) 
#(for overall comparison with average salary of those locations)

temp <- arfd[,c("city","state","salary")] #for all roles
temp_sp <- arfd[which(arfd$role %in% "CNA"),] #for one particular role
temp_sp <- temp_sp[,c("city","state","salary")]

#contains the mean of salries for all cities
temp_fn <- function(temp) { #input would be temp or temp_sp
  temp <- aggregate(.~city+state, temp, mean)
  df1 <- read.csv("~/data/DARL_Fall2017/GREENWICH/abhi/zip_codes_states.csv")
  df1 <- df1[which(df1$state %in% c('VA','TX','IL','FL')),]
  df1$zip_code <- NULL
  df1$county <- NULL
  #contains the mean of latitudes and longitudes 
  #(taken from outside data source as our data is not 
  # complete with all location information)
  df1 <- aggregate(.~city+state, df1, mean)
  df1 <- merge(df1,temp,by=c("city","state"))
  df1$salary <- round(df1$salary, digits = 2)
  colnames(df1)[3] <- "lat"
  colnames(df1)[4] <- "lon"
  return(df1)
}
df1 <- temp_fn(temp)
df1_sp <- temp_fn(temp_sp)

library(plotly)
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showland = TRUE,
  landcolor = toRGB("gray85"),
  subunitwidth = 1,
  countrywidth = 1,
  subunitcolor = toRGB("white"),
  countrycolor = toRGB("white")
)
t <- list(
  family = "sans serif",
  size = 12,
  color = 'blue')

par(mfrow=c(2,1))

p1 <- plot_geo(df1, locationmode = 'USA-states', sizes = c(1,75)) %>%
  add_markers(
    x = ~lon, y = ~lat, size = ~salary/1e17,
    marker=list(color = ~salary, showscale=FALSE),
    colors = 'Reds', hoverinfo = "text", 
    showlegend=FALSE, 
    text = ~paste(df1$city, "<br />$", df1$salary, ".")
  ) %>%
  layout(title=paste('Average Salaries of different cities',
                     '<br>   '), font=t, geo = g)

p2 <- plot_geo(df1_sp, locationmode = 'USA-states', sizes = c(1,75)) %>%
  add_markers(
    x = ~lon, y = ~lat, size = ~salary/1e17,
    marker=list(color = ~salary, showscale=FALSE),
    colors = 'Reds', hoverinfo = "text",
    showlegend=FALSE, 
    text = ~paste(df1_sp$city, "<br />$", df1_sp$salary, ".")
  ) %>%
  layout(title=paste('Overall average salary and CNA average salary',
                     '<br>   '), font=t, geo = g)

subplot(p1,p2, nrows = 2, margin = 0.05, titleX = TRUE, titleY = TRUE)

# shading of states with respect to no. of jobs
#reference https://plot.ly/r/county-level-choropleth/ read data here using
#df <- read.csv("~/data/DARL_Fall2017/GREENWICH/abhi/californiaPopulation.csv")
library(tidyverse)
library(plotly)

df1 <- arfd[which(arfd$state=="FL"),12:18]
df2 <- arfd[which(arfd$state=="IL"),12:18]
df3 <- arfd[which(arfd$state=="VA"),12:18]
df4 <- arfd[which(arfd$state=="TX"),12:18]

cali1 <- map_data("county") %>% filter(region == 'florida')
cali2 <- map_data("county") %>% filter(region == 'illinois')
cali3 <- map_data("county") %>% filter(region == 'virginia')
cali4 <- map_data("county") %>% filter(region == 'texas')

pop1 <- summarize(group_by(df1,county), count=n())
pop2 <- summarize(group_by(df2,county), count=n())
pop3 <- summarize(group_by(df3,county), count=n())
pop4 <- summarize(group_by(df4,county), count=n())

pop1$county <- tolower(pop1$county) #matching string
pop2$county <- tolower(pop2$county) #matching string
pop3$county <- tolower(pop3$county) #matching string
pop4$county <- tolower(pop4$county) #matching string

cali_pop1 <- merge(cali1, pop1, by.x = "subregion", 
                   by.y = "county",all.x=FALSE, all.y=TRUE)
cali_pop2 <- merge(cali2, pop2, by.x = "subregion", 
                   by.y = "county",all.x=FALSE, all.y=TRUE)
cali_pop3 <- merge(cali3, pop3, by.x = "subregion", 
                   by.y = "county",all.x=FALSE, all.y=TRUE)
cali_pop4 <- merge(cali4, pop4, by.x = "subregion", 
                   by.y = "county",all.x=FALSE, all.y=TRUE)

cali_pop1$pop_cat <- cut(cali_pop1$count, 
                         breaks = c(seq(0, 30000, by = 3000)), labels=1:10)
cali_pop2$pop_cat <- cut(cali_pop2$count, 
                         breaks = c(seq(0, 35000, by = 5000)), labels=1:7)
cali_pop3$pop_cat <- cut(cali_pop3$count, 
                         breaks = c(seq(0, 25000, by = 5000)), labels=1:5)
cali_pop4$pop_cat <- cut(cali_pop4$count, 
                         breaks = c(seq(0, 40000, by = 4000)), labels=1:10)

#to order the table according to proper order column (very important)
cali_pop1 <- cali_pop1[order(cali_pop1$order),]
cali_pop2 <- cali_pop2[order(cali_pop2$order),]
cali_pop3 <- cali_pop3[order(cali_pop3$order),]
cali_pop4 <- cali_pop4[order(cali_pop4$order),]

state_plot <- function(cali_pop,stname){
  p <- cali_pop %>%
    group_by(group) %>%
    plot_ly(x = ~long, y = ~lat, color = ~pop_cat, colors = 'Reds',
            text = ~subregion, hoverinfo = 'text', showlegend= FALSE) %>%
    add_polygons(line = list(width = 0.4)) %>%
    add_polygons(
      fillcolor = 'transparent',
      line = list(color = 'black', width = 0.5),
      showlegend = FALSE, hoverinfo = 'none'
    ) %>%
    layout(
      title = paste(stname, ": # of jobs by County", sep=" "),
      titlefont = list(size = 15),
      xaxis = list(title = "", showgrid = FALSE,
                   zeroline = FALSE, showticklabels = FALSE),
      yaxis = list(title = "", showgrid = FALSE,
                   zeroline = FALSE, showticklabels = FALSE)
    )
  return(p)
}

state_plot(cali_pop1,"FL")
state_plot(cali_pop2,"IL")
state_plot(cali_pop3,"VA")
state_plot(cali_pop4,"TX")
