

#reading data
data<-read.csv("C:/Users/Sandra George/Desktop/project stat/StudentsPerformance.csv",na.strings="")
print(data)

print(nrow(data))
print(ncol(data))
dim(data)
names(data)

#Data cleaning
duplicated(data)
sum(duplicated(data))    #no dupes

colSums(is.na(data))    #Removed null values(removed 25 rows)
sum(is.na(data))
data<-na.omit(data)
sum(is.na(data))
colSums(is.na(data))
print(nrow(data))

print(any(data$age<0 ))  #no negative values
print(any(data$G1<0 ))
print(any(data$G2<0 ))
print(any(data$G3<0 ))
       
unique(data$sex)        #Removed Inconsistent data
data$sex[data$sex=="F"]<-'Female'
data$sex[data$sex=="M"]<-'Male'
unique(data$sex)
print(data)

unique(data$internet)   #no inconsistency
unique(data$romantic)   

#visualization
summary(data)

plot(data)

pie(table(data$sex),main = "sex piechart")
barplot(table(data$sex),main = "sex barplot")
table(data$sex)

pie(table(data$internet),main = "Internet piechart")
barplot(table(data$internet),main = "Internet barplot")
table(data$internet)

pie(table(data$romantic),main = "Romantic piechart")
barplot(table(data$romantic),main = "Romantic barplot")
table(data$romantic)

pie(table (data$Fjob),main = "Fjob piechart")
barplot(table(data$Fjob),main = "Fjob barplot")
table(data$Fjob)

pie(table (data$Mjob),main = "Mjob piechart")
barplot(table(data$Mjob),main = "Mjob barplot")
table(data$Mjob)

boxplot(table(data$romantic),main ="Romantic Boxplot")
boxplot(table (data$Fjob),main ="Fjob Boxplot")
boxplot(table(data$Mjob),main ="Mjob Boxplot")
boxplot(table(data$G1),main ="G1 Boxplot")
boxplot(table(data$goout),main ="goout Boxplot")

########################Removing outliers(351 row , remove 19 rows)
 boxplot(table(data$G2),main ="G2 Boxplot")
 Q1 <- quantile(data$G2, 0.25)
 Q3 <- quantile(data$G2, 0.75)
 IQR <- Q3 - Q1
 lower_bound <- Q1 - 1.5 * IQR
 upper_bound <- Q3 + 1.5 * IQR
data_no_outliers <- subset(data, G2 >= lower_bound & G2 <= upper_bound)
 print(data_no_outliers)
 print (nrow(data_no_outliers))
 boxplot(table(data_no_outliers$G2),main ="G2 Boxplot without outliers")
 summary(data$G2)
 summary(data_no_outliers$G2)

 boxplot(table(data$G3),main ="G3 Boxplot")
 Q1 <- quantile(data$G3, 0.25)
 Q3 <- quantile(data$G3, 0.75)
 IQR <- Q3 - Q1
 lower_bound <- Q1 - 1.5 * IQR
 upper_bound <- Q3 + 1.5 * IQR
  data_no_outliers <- subset(data, G3 >= lower_bound & G3 <= upper_bound)
 print(data_no_outliers)
 print (nrow(data_no_outliers))
 boxplot(table(data_no_outliers$G3),main ="G3 Boxplot without outliers")
 summary(data$G3)
summary(data_no_outliers$G3)
#######################CLUSTERING using K-means 
install.packages("cluster")       
library(cluster)

unique(data$sex)
data$sex[data$sex=="Female"]<-0
data$sex[data$sex=="Male"]<-1
data$sex<-as.numeric(data$sex)
unique(data$sex)

unique(data$internet)
data$internet[data$internet=="no"]<-0
data$internet[data$internet=="yes"]<-1
data$internet<-as.numeric(data$internet)
unique(data$internet)

unique(data$romantic)
data$romantic[data$romantic=="no"]<-0
data$romantic[data$romantic=="yes"]<-1
data$romantic<-as.numeric(data$romantic)
unique(data$romantic)

unique(data$Fjob)
data$Fjob[data$Fjob=="other"]<-0
data$Fjob[data$Fjob=="services"]<-1
data$Fjob[data$Fjob=="teacher"]<-2
data$Fjob[data$Fjob=="health"]<-3
data$Fjob[data$Fjob=="at_home"]<-4
data$Fjob<-as.numeric(data$Fjob)
unique(data$Fjob)

unique(data$Mjob)
data$Mjob[data$Mjob=="other"]<-0
data$Mjob[data$Mjob=="services"]<-1
data$Mjob[data$Mjob=="teacher"]<-2
data$Mjob[data$Mjob=="health"]<-3
data$Mjob[data$Mjob=="at_home"]<-4
data$Mjob<-as.numeric(data$Mjob)
unique(data$Mjob)

unique(data$school)
data$school[data$school=="GP"]<-0
data$school[data$school=="MS"]<-1
data$school<-as.numeric(data$school)
unique(data$school)

install.packages("factoextra")  #To choose the optimal number of clusters
library(cluster)
library(factoextra)
fviz_nbclust(data,kmeans,method = "wss")
kmeans(data,3) #accuracy=88%
km<-kmeans(data,3)  
km$cluster
km$centers
km$size

 
######################CLASSIFICATION using Naiive bayes
install.packages("e1071")
library(e1071)
NB<-naiveBayes(data$school~. , data=data)
v=predict(NB, as.data.frame(data))
table(v)
#325 school GP  (87.83%)
#45 school MS   (12.16%)

#######################CLASSIFICATION using Decision tree(according to school)
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
fit = rpart(data$school~., data=data ,method = "class")
rpart.plot(fit)

#####################Testing data using decision tree
#new <- data.frame(x=350 ,sex = 0,age=13 ,Fjob=2,Mjob=4 ,goout=2,internet=0 ,romantic=1,studytime=2 ,failures=1 ,health=3 ,absences=7 ,G1=10 ,G2=15 ,G3=5)
#predict(fit,new)
#predict(fit,data)
#Summary(fit)
