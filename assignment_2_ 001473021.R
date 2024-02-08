rm(list = ls())
library(datasets)
library(ggplot2)
library(dplyr)
library(corrplot)
library(tsoutliers)
#read the csv file
housing=read.csv("housing_new.csv",header =TRUE,sep = ";")
#using str to observe the data
str(housing)

#summary of the variables 5
summary(housing$MEDV)   
summary(housing$PCCR)   
summary(housing$INDUS)  
summary(housing$NOX)    
summary(housing$PRLZ)    
summary(housing$AVR)  

#Explanatory Analysis
#Histogram of dpendent variable data
ggplot(housing, aes(x = MEDV)) +
  geom_histogram(binwidth = 1, fill = "gold", color = "red") +
  labs(x = "MEDV", y = "Frequency") +
  ggtitle("Histogram of dependent variable MEDV")
#Histogram of explanatory variable
ggplot(housing, aes(x = PCCR)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "blue") +
  labs(x = "PCCR", y = "Frequency") +
  ggtitle("Histogram of explanatory variable PCCR")
#Histogram PRLZ
ggplot(housing, aes(x = PRLZ)) +
  geom_histogram(binwidth = 1, fill = "gold", color = "red") +
  labs(x = "PRLZ", y = "Frequency") +
  ggtitle("Histogram of dependent variable PRLZ")


#Histogram INDUS
#Histogram
ggplot(housing, aes(x = INDUS)) +
  geom_histogram(binwidth = 1, fill = "white", color = "red") +
  labs(x = "INDUS", y = "Frequency") +
  ggtitle("Histogram of dependent variable INDUS")

#Histogram NOX
ggplot(housing, aes(x = NOX)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "white") +
  labs(x = "NOX", y = "Frequency") +
  ggtitle("Histogram of dependent variable NOX")

#Histogram AVR
ggplot(housing, aes(x = AVR)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "red") +
  labs(x = "AVR", y = "Frequency") +
  ggtitle("Histogram of dependent variable AVR")


#correlation between all variables

#plot correlation
corrplot(cor(housing),"number")

#Now corelation with linear associaciation without MEDV
depvar=housing$MEDV
exvar=housing[,1:11]

corrplot(cor(exvar),"number")


#Remove explanatory variable with highest absolute value
#remove absolute or values higher than 0.8

cormat= abs(cor(exvar))
diag(cormat)=0

#while loop
while (max(cormat)>=0.8) {
  #find explanatory variables with highest corelations
  maxvar= which(cormat==max(cormat),arr.ind = TRUE)
  
  #select variable with highest average corelation
  maxavg=which.max(rowMeans(cormat[maxvar[,1],]))
  #removal
  exvar=exvar[,-maxvar[maxavg,1]]
  cormat=cormat[-maxvar[maxavg,1],-maxvar[maxavg,1]]
}

#Make our model
my_data=cbind('MEDEV'=depvar,exvar)
linearregmodel=lm(MEDEV~PCCR+ PRLZ + INDUS + NOX +AVR + AGE +DIS +RAD, data =my_data)

summary(linearregmodel)

#let's plot our linear regression model
predictedvalues = predict(linearregmodel)
observedvalues = my_data$MEDEV


ggplot(data = data.frame(Predicted = predictedvalues, Observed = observed_values)) +
  geom_point(aes(x = Predicted, y = Observed)) +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(title = "Observed vs. Predicted Values", x = "Predicted Values", y = "Observed Values")


#let's remove RAD for high p value
linearregmodel1=lm(MEDEV~PCCR+ PRLZ + INDUS + NOX +AVR + AGE +DIS, data =my_data)

summary(linearregmodel1)



#Residuals
#mean of the residuals
mean(residuals(linearregmodel))

#Homoskedasticity & residual linear independent
plot(residuals(linearregmodel), type="p", col="blue",ylim =c(-200,200),pch=16,
     ylab ="residuals",main = "residual over time")
abline(a=5*sd(residuals(linearregmodel)), b=0,col="red", lty=2)
abline(a=-5*sd(residuals(linearregmodel)), b=0,col="red", lty=2)
abline(a=0,b=0,col="red",lty=2)


#corelation between residuals and independent variables
cor(residuals(linearregmodel),housing[,c("INDUS","AVR","PRLZ","NOX","PCCR")])



######PART 2



rm(list = ls())
library(dplyr)
library(corrplot)
library(scales)
library(NbClust)
library(purrr)
library(ggplot2)
# Read the CSV file with the correct delimiter (e.g., assuming a comma as the delimiter)
data=read.csv("wholesale_2023.csv", header = TRUE, sep = ";")

# Check the structure and summary of the dataset
str(data)
summary(data)
#missing value
any(is.na(data)
    
    #Explanatory data analysis
    #correlation
    # Install and load the corrplot package if not already installed
    # install.packages("corrplot")
    library(corrplot)
    
    correlation1 =cor(data[c("Fresh", "Milk", "Grocery", "Frozen", "Detergents_Paper", "Delicassen", "Beverages")])
    corrplot(correlation1, "number")
    
    #plot some variables
    ggplot(data, aes(x = Fresh, y = Milk)) +
      geom_point() +
      labs(title = "Scatterplot: Fresh vs. Milk", x = "Fresh", y = "Milk")
    #plot channel and milk
    ggplot(data, aes(x = Channel, y = Milk)) +
      geom_point() +
      labs(title = "Scatterplot: channel vs. Milk", x = "channel", y = "Milk")
    #box plot
    ggplot(data, aes(y = Fresh)) +
      geom_boxplot(fill = "goldenrod") +
      labs(title = "Box Plot: Fresh Products", y = "Fresh Products")
    
    #Grocery plot
    ggplot(data, aes(y = Grocery)) +
      geom_boxplot(fill = "blue") +
      labs(title = "Grocery:Box plot", y = "Grocery")
    
    #Beverages
    ggplot(data, aes(y = Beverages)) +
      geom_boxplot(fill = "yellow") +
      labs(title = "Beverages: Box Plot", y = "Baverages")
    
    
    #plot some variables
    ggplot(data, aes(x = Frozen, y = Grocery)) +
      geom_point() +
      labs(title = "Frozen vs Grocery", x = "Frozen", y = "Grocery")
    
    
    #normalization
    #scaling with min max Normalization
    
    data= apply(data,2,rescale, to=c(0,1))
    
    #plot data
    ggplot(data.frame(data),aes(x=Region,y=Milk))+
      geom_point()+ 
      ggtitle("clustering")
    
    
    #hierarchical matrix
    distmat=dist(data, method ="euclidean")
    model= hclust(distmat, method="complete")
    
    #dendogram
    dendmodel1= as.dendrogram(model)
    plot(dendmodel1, main="Dendogram", panel.first=grid())
    
    #cluster membership
    clustermemeber= cutree(model,k=2)
    
    #plot the membership
    plot(data[,1],data[,2], col=clustermemeber, pch=16,panel.first = grid())
    
    
    #kmeans clustering
    kmeansmod= kmeans(data, centers=2, nstart = 25)
    
    kmeansmod$cluster
    kmeansmod$centers
    plot(data[,1],data[,2],col=kmeansmod$cluster, pch=16, main = "kmeans cluster")
    kmeansmod$size
    kmeansmod$withinss
    kmeansmod$tot.withinss
    
    
    rm(list = ls())
    library(dplyr)
    library(corrplot)
    library(scales)
    library(NbClust)
    library(purrr)
    library(ggplot2)
    # Read the CSV file with the correct delimiter (e.g., assuming a comma as the delimiter)
    data=read.csv("wholesale_2023.csv", header = TRUE, sep = ";")
    
    # Check the structure and summary of the dataset
    str(data)
    
    #Determine number of clusters
    #Elbow method
    tot_within_ss= map_dbl(1:10, function(k){
      model=kmeans(data,centers = k,nstart = 25)
      model$tot.withinss
      
    })
    
    plot(1:10,tot_within_ss,type="o", xlab = "number of clusters",
         ylab = "total wss", main="elbow method",panel.first = grid())
    
    
    #silclustering method
    silclust=NbClust(data,distance = "euclidean",min.nc = 2,max.nc = 10,
                     method = "kmeans",index = "silhouette")
    
    #Gap clustering
    
    Gap_clust= NbClust(data,distance = "euclidean",min.nc = 2,max.nc = 10,
                       method = "kmeans",index = "gap")
    
    #CHclust
    
    CH_clust= NbClust(data,distance = "euclidean",min.nc = 2,max.nc = 10,
                      method = "kmeans",index = "ch")
    
    
    #plot the methods
    par(mfrow=c(1,3))
    plot(2:10,silclust$All.index,type = "o", xlab = "Number of clusters",
         ylab = "silhouette Score", panel.first = grid())
    plot(2:10,Gap_clust$All.index,type = "o", xlab = "Number of clusters",
         ylab = "Gap statistics", panel.first = grid())
    
    plot(2:10,CH_clust$All.index,type = "o", xlab = "Number of clusters",
         ylab = "Calinksi Harabasz", panel.first = grid())
    
    #optimal number of clusters
    par(mfrow=c(1,1))
    kmeansmod=kmeans(data,centers = 3,nstart = 25)
    
    #let's add cluster membership to the data set
    datanew= data %>%mutate(member= factor(kmeansmod$cluster))
    
    datanew %>% 
      group_by(member) %>%
      summarise_all(list(avg= mean))
    
    datanew %>% 
      group_by(member) %>%
      summarise_all(list(avg= mean, std= sd))
    
    #plot data
    par(mfrow=c(1,1))
    ggplot(datanew,aes(x=Region,y=Milk, col=member))+
      geom_point()+ 
      ggtitle("clustering: region vs milk")
    
    par(mfrow=c(1,1))
    ggplot(datanew,aes(x=Frozen,y=Grocery, col=member))+
      geom_point()+ 
      ggtitle("clustering: Frozen vs grocery")
    ##plot
    
    ggplot(datanew,aes(x=Channel,y=Grocery, col=member))+
      geom_point()+ 
      ggtitle("clustering, channel vs grocery")
    
    #plot
    ggplot(datanew,aes(x=Region,y=Grocery, col=member))+
      geom_point()+ 
      ggtitle("clustering, Region vs grocery")
    
    #plot
    
    ggplot(datanew,aes(x=Milk,y=Beverages, col=member))+
      geom_point()+ 
      ggtitle("cluster: Milk vs Beverages")
    
    
    #Plot
    ggplot(datanew,aes(x=Region,y=Delicassen , col=member))+
      geom_point()+ 
      ggtitle("cluster: Region vs Delicassen ")
    
    
    
    
