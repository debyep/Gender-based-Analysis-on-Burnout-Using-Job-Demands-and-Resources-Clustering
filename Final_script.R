###################################################
################### Library  ######################
###################################################
library(ranger)
library(dplyr)
library(purrr)
library(caret)
library(glmnet)
library(ggplot2)
library(flipTime)
library(tidyverse)
library(reshape2)
library(corrplot)
library(dendextend)
library(factoextra)
library(vtable)

###################################################
################ DATA PREPARATION  ################
###################################################

#################### READ DATA ####################
# read data
setwd("C:/Users/debye/ESMT/Thesis")
emails <- read.csv("Data/emails.csv")
employees <- read.csv("Data/employees.csv")

#set seed to ensure that results can be replicated
set.seed(1309)

############### EMPLOYEES DATASET  ################
#formatting
employees$Birth.Date <- format(as.Date(employees$Birth.Date, "%d.%m.%y"), "19%y-%m-%d")
employees$Date.of.joining.the.company <- as.Date(employees$Date.of.joining.the.company, "%d.%m.%y")
employees$Gender <- factor(employees$Gender)
employees$Education.Level <- as.numeric(employees$Education.Level)

#stay duration and age, in weeks
employees$stay <- as.numeric(difftime("2018-01-02", employees$Date.of.joining.the.company, units = "weeks"))/52
employees$age <- as.numeric(difftime("2018-01-02", employees$Birth.Date, units = "weeks"))/52

#cleaning NA values(2 NA rows have all NA values except ID, can be excl., stay > 0 in line with analysis scope)
employees <- na.omit(employees)
employees <- employees %>% 
  filter(stay > 0)

################## EMAILS DATASET  ################
#formatting
emails$datetime <- AsDateTime(emails$datetimesent, us.format =FALSE)
emails$date <- as.Date(emails$datetime)
emails$time <- format(as.POSIXct(emails$datetime), "%H:%M")

#unique employee sent to from ID
sentto <- emails %>% 
  group_by(date, senderaddress) %>% 
  summarise(sentto = as.integer(length(unique(recipient))))
colnames(sentto)[2] <- "ID"

#unique employee received from ID
receivedfrom <- emails %>% 
  group_by(date, recipient) %>% 
  summarise(receivedfrom = length(unique(senderaddress)))
colnames(receivedfrom)[2] <- "ID"

#volume sent/day
sent <- emails %>% 
  group_by(date, senderaddress) %>%  
  summarise(sent = n())
colnames(sent)[2] <- "ID"

#volume received/day
received <- emails %>% 
  group_by(date, recipient) %>% 
  summarise(received = n())
colnames(received)[2] <- "ID"

#checking if its OOO
sat_sun <- data.frame(Date = c("2018-01-06", "2018-01-07", "2018-01-13", "2018-01-14", "2018-01-20", "2018-01-21", "2018-01-27",
                               "2018-01-28","2018-02-03", "2018-02-04", "2018-02-10", "2018-02-11", "2018-02-17", "2018-02-18",
                               "2018-02-24", "2018-02-25", "2018-03-03","2018-03-04", "2018-03-10", "2018-03-11", "2018-03-17",
                               "2018-03-18", "2018-03-24", "2018-03-25", "2018-03-31", "2018-04-01", "2018-04-07", "2018-04-08",
                               "2018-04-14", "2018-04-15", "2018-04-21", "2018-04-22", "2018-04-28", "2018-04-29", "2018-05-05",
                               "2018-05-06", "2018-05-12", "2018-05-13", "2018-05-19", "2018-05-20","2018-05-26", "2018-05-27"))
sat_sun <- as.Date(sat_sun$Date)             
emails <- emails %>% 
  mutate(OOOhours = ifelse(time > "18:00:00",1,ifelse(time < "09:00:00",1,0)),
         weekend = ifelse((date %in% sat_sun), 1, 0),
         OOO = ifelse(OOOhours == 1 | weekend == 1, 1, 0))

#volume sent OOO/day
sentOOO <- emails %>% 
  filter(OOO == 1) %>% 
  group_by(date, senderaddress) %>% 
  summarise(sentOOO = n())
colnames(sentOOO)[2] <- "ID"
summary(sentOOO)

#volume by day per ID
volumes_byday <- list(sent, received, sentOOO, sentto, receivedfrom) %>%
  reduce(left_join, by = c("date", "ID"))
volumes_byday <- volumes_byday %>% 
  mutate(difference = sent - received,
         differenceID = sentto - receivedfrom)

#average volume per ID
volumes_byID <- volumes_byday %>% 
  group_by(ID) %>% 
  summarise(mean_sent = mean(sent, na.rm = TRUE),
            mean_received = mean(received, na.rm = TRUE),
            mean_sentOOO = mean(sentOOO, na.rm = TRUE),
            mean_difference = mean(difference, na.rm = TRUE),
            mean_sentto = mean(sentto, na.rm = TRUE),
            mean_receivedfrom = mean(receivedfrom, na.rm = TRUE),
            mean_differenceID = mean(differenceID, na.rm = TRUE),
            ratio = mean_sent/mean_received,
            ratioID = mean_sentto/mean_receivedfrom
  )
volumes_byID <- na.omit(volumes_byID)



##################  CONSOLIDATION  ################
#joining all variables
consol_df <- left_join(employees, volumes_byID, by = "ID")
consol_df <- na.omit(consol_df)
consol_df <- consol_df %>% 
  select(c(4,6,9,10,13,14,15,16,17,18,19,20,21,22,23))

#remove outliers
outliers <- function(x) {
  Q1 <- quantile(x, probs=.25)
  Q3 <- quantile(x, probs=.75)
  iqr = Q3-Q1
  upper_limit = Q3 + iqr*1.5
  lower_limit = Q1 - iqr*1.5
  x > upper_limit | x < lower_limit
}

remove_outliers <- function(df, cols = names(df)) {
  for (col in cols) {
    df <- df[!outliers(df[[col]]),]
  }
  df
}
consol_df <- remove_outliers(consol_df, c('mean_sent', 'mean_received','mean_sentOOO','mean_sentto'))
summary(consol_df)

#scaling data for analysis
df_scaled <- consol_df %>%
  mutate_at(c('mean_sentOOO', 'mean_difference', 'ratioID'), funs(c(scale(.))))

#correlation for used variables
cor_df <- df_scaled %>%
  select(c('mean_difference','mean_sentOOO','ratioID'))
upper <- round(cor(cor_df, use = "complete.obs") , 3)
upper[upper.tri(cor)] <-""
upper <- as.data.frame(upper)
upper

#correlation matrix with heat map
corrplot(cor(cor_df, use = "complete.obs"), 
         method = "number", 
         type = "upper", 
         order = "hclust", # reorder by the size of the correlation coefficient
)

#density plot
par(mfrow=c(2,2))

plot(density(consol_df$mean_difference), main = "Mean Difference")
plot(density(consol_df$mean_sentOOO), main = "Mean SentOOO")
plot(density(consol_df$ratioID), main = "RatioID")


###################################################
##################### CLUSTERING ##################
###################################################

################## K DETERMINATION ################

#cluster analysis on a distance matrix using Ward method
#(compare clustering result with single, same K, check change in cluster center)
df_k <- df_scaled
df_k_ward <- hclust(dist(df_k[c('mean_difference', 'mean_sentOOO','ratioID')]), method="ward.D2") 

#Scree plot
plot(rev(df_k_ward$height), 
     type = "b",
     ylab = "Dissimilarity measure",
     xlab = "Number of clusters",
     main = "Scree plot",
     col = "orange",
     pch = 16) 
abline(v = 4, lty = 2, col = "darkred") # draw a vertical line at v = 5

k_value <- 4

#Dendrogram 
plot(set(as.dendrogram(df_k_ward),  
         "branches_k_color", # to highlight the cluster solution with a color
         k = k_value),
     ylab = "Distance",
     main = "Dendrogram",
     cex = 0.2)             # Size of labels
rect.hclust(df_k_ward, k = k_value, border = "orange")

#K Value for analysis
memb <- cutree(df_k_ward, k = k_value)

#################### K CLUSTERING #################

#K-means clustering method
cent <- NULL
for(k in 1:k_value){
  cent <- rbind(cent, colMeans(df_k[c('mean_difference', 'mean_sentOOO','ratioID')][memb == k, , drop = FALSE]))
}
df_k_means <- kmeans(df_k[c('mean_difference', 'mean_sentOOO','ratioID')], centers = cent[], iter.max = 10)
df_k_means$size

#check other K value, maximize between ss and minimize within ss
str(df_k_means)

#Calculate change in each cluster from K-means clustering
change <- NULL
for (i in 1:4){
  change <- rbind(change, df_k_means$centers[i,]-cent[i,])
}
round(change, 3)

#Distances between cluster centers
dist(df_k_means$centers)


################## K INTERPRETATION ###############

#Binding result
df_k <- cbind(consol_df, cluster = df_k_means$cluster)

#Average for each cluster
agg_value <- aggregate(df_k[, 2:6],
                       by = list(cluster = df_k$cluster), 
                       FUN = median)

agg_value <- agg_value %>% 
  mutate(female = c(91,17,16,41), male = c(33,14,15,27))

group_by <- df_k %>% group_by(cluster, Gender) %>%
  summarise(Exhaustion = median(Exhaustion),
            Disengagement = median(Disenchantment))

#Visualization of average
dt.cluster = aggregate(df_k[, c('mean_difference', 'mean_sentOOO','ratioID')],
                       by = list(cluster = df_k$cluster), 
                       FUN = median)
dt.cluster %>% 
  gather(tes, value, -cluster) %>% 
  mutate(tes = fct_rev(factor(tes))) %>% 
  ggplot(aes(x = factor(cluster), y = tes)) +
  geom_tile(aes(fill = round(value, digits = 5))) +
  geom_text(aes(label = round(value, digits = 5)), color="white") +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  scale_fill_gradient("Average\n value", low = "lightgrey", high = "orange") +
  theme_minimal() +
  labs(title = "Average Email Communication Behavior",
       x = "Cluster",
       y = " ") + 
  theme(legend.position="right", 
        plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
        axis.ticks = element_blank()) 


#Cluster plot
fviz_cluster(df_k_means, data = df_k[c('mean_difference','ratioID', 'mean_sentOOO')]) + 
  theme_bw()