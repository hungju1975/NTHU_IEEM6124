# calculate speed and traffic per hour
# North from HsinChu System to ChuBei
GatesName <- c("01F0979N", "01F0956N", "01F0928N","01F0880N","01F0750N","01F0681N")
#selholidays <- c("0402", "0403", "0404", "0405", "0406", "0407")
holidays <-c()
hourSpeed <-c()
hourTraffic <- c()
hourTime <- c()
avgSpeed <- c()
totalTraffic <-c()
totalTime <- c()
hourData <-c()
hoursPerDay <- c()
GateData <- c()
TimePerGate <- c()
setwd("~/MyProject/dataMining")


# given date, hour, and gate name to get the speed & traffic per hour
FetchGateData <- function(givenPath, givenDay, givenHour, whichGate){
  array5min<-c("00", "05", "10", "15", "20", "25", "30", "35", "40", "45", "50", "55")
  selDir <- paste(givenPath, "/2015",givenDay,"/", givenHour,"/", sep="")
  tselDir <- paste("~/myProject/dataMining/2015/M08A_src/M08A", "/2015",givenDay,"/", givenHour,"/", sep="")

  perHourData <- c()

  for(i in (1:12)) {
    selFile <- paste(selDir,"TDCS_M05A_2015",givenDay,"_",givenHour,array5min[i],"00.csv", sep="")
    tselFile <- paste(tselDir,"TDCS_M08A_2015",givenDay,"_",givenHour,array5min[i],"00.csv", sep="")
    #print(tselFile);
    if (file.exists(selFile)){ 
        #print(selFile)
        #print(array5min[i])
        fetchData <- read.csv(selFile, header=FALSE)
        #print(tselFile)
        fetchTime <- read.csv(tselFile, header=FALSE)
        
        Gate <- fetchData[fetchData$V2 == whichGate,]
        GateTime <- fetchTime[fetchTime$V2==whichGate,]
        
        if(whichGate=="01F0750N")
        {
          Gate <- Gate[Gate$V3=="01F0681N",]
          GateTime <- GateTime[GateTime$V3=="01F0681N",]
        }

        avgSpeed <-c(avgSpeed,round(sum(Gate[,"V5"]*Gate[,"V6"])/sum(Gate[,"V6"])))

        totalTraffic <- c(totalTraffic ,sum(Gate[,"V6"]))

        if((sum(GateTime[,"V5"]) == 0)|| is.null(sum(GateTime[,"V5"])))
        {
          if(length(totalTime)==0){
            smTime=1;
            totalTime <- c(totalTime, smTime)
          }
          else {
          #print(totalTime[length(totalTime)])
          totalTime <- c(totalTime, totalTime[length(totalTime)])
          }
        }
        else
        {
          #print(mean(GateTime[,"V5"]))
          totalTime <- c(totalTime, mean(GateTime[,"V5"]))
        }
    }
  }
  
  perHourData <- list(avgSpeed, totalTraffic, totalTime)
  
  return (perHourData) 
}

#for(gateIndex in (1:length(GatesName)))
GateDataOp <- function(GateName, givenMonth)
{
  monLen <- 0
  
  if((givenMonth=="01")||(givenMonth=="03")||(givenMonth=="05")||(givenMonth=="07")||(givenMonth=="08")||(givenMonth=="10")||(givenMonth=="12"))
  {
    monLen <- 31
  }
  else 
  {
    monLen <- 30
  }
  
  for (len in (1:monLen))
  #for (len in (1:3))
  {
    dayString <- toString(len, digits=2)
    if(len <10)
    {
      dayString <- paste0("0", dayString)
    }
    dayString <-paste0(givenMonth,dayString)
    
    print(dayString)
  
    holidays[len] <- dayString
  
    for (perHour in (16:21))
    {
      hourString <- toString(perHour, digits=2)
      if(perHour < 10)
        hourString <- paste0("0",hourString)

      hoursPerDay[perHour+1] <- hourString
      hourData <- FetchGateData("~/myProject/dataMining/2015/M05A/", holidays[len], hourString,GateName)
      #print(hourData)
      #for (index in hourData)
      #{
      #  print(index)
      #}
      hourSpeed <- c(hourSpeed, hourData[1])
      hourTraffic <- c(hourTraffic, hourData[2])
      hourTime <-c(hourTime, hourData[3])
    }
  }
  
  totalGate <- list(hourSpeed, hourTraffic,hourTime)
  
  return (totalGate)
}


#Gate 1 start here.
trainGate1 <- GateDataOp(GatesName[1],"04")

# empty data.frame
GateData <- data.frame((matrix(0, ncol=2, nrow=length(unlist(trainGate1[1])))))

TimePerGate <- unlist(trainGate1[3])

# assign varibles' name
x <- c("speed", "traffic")
colnames(GateData) <- x


# given the speed and traffic
GateData$speed <- unlist(trainGate1[1])
GateData$traffic <-unlist(trainGate1[2])

kc <- kmeans(GateData$speed, 3)
GateData$cluster <- kc$cluster

GateData$TravelTime <- TimePerGate
GateData <- ddply(GateData, "speed", transform, set = sample(c("train", "test"), length(speed),replace = TRUE, prob = c(2, 1)))

#partition data as training set and testing set
train.data <- subset(GateData, set == "train")
test.data <- subset(GateData, set == "test")

#extract observations by clusters
data_cluster1 <- train.data[train.data$cluster==1,]
data_cluster2 <- train.data[train.data$cluster==2,]
data_cluster3 <- train.data[train.data$cluster==3,]

# fit by linear regression for each cluster
fit1 <- lm(TravelTime ~ speed+traffic, data = data_cluster1)
estimated1 <- predict(fit1, test.data[test.data$cluster==1,])
print(length(estimated1))

fit2 <- lm(TravelTime~speed+traffic, data=data_cluster2)
estimated2 <- predict(fit2, test.data[test.data$cluster==2,])

fit3 <- lm(data_cluster3$TravelTime~data_cluster3$speed+data_cluster3$traffic)
print(length(test.data[test.data$cluster==3,]))
estimated3 <- predict(fit3, test.data[test.data$cluster==3,])
print(length(estimated2))

obs1 <- test.data[test.data$cluster==1,]$TravelTime
obs1 <- test.data[test.data$cluster==1,]$TravelTime
obs2 <- test.data[test.data$cluster==2,]$TravelTime
obs3 <- test.data[test.data$cluster==3,]$TravelTime


rmse1 <- sqrt(mean((obs1-estimated1)^2+(obs2-estimated2)^2+(obs3-estimated3)^2))

# plot for each cluster with linear regression
g1<- ggplot(GateData, aes(traffic, speed, color=kc$cluster)) + geom_point()
print(g1)
cluster1 <- ggplot(data_cluster1, aes(traffic, speed))+geom_point()+geom_smooth(method = "lm")
print(cluster1)

cluster2 <- ggplot(data_cluster2, aes(traffic, speed))+geom_point()+geom_smooth(method = "lm")
print(cluster2)

cluster3 <- ggplot(data_cluster3, aes(traffic, speed))+geom_point()+geom_smooth(method = "lm")
print(cluster3)

# end of Gate 1

# start Gate 2
trainGate2 <- GateDataOp(GatesName[2],"04")

# empty data.frame
GateData <- data.frame((matrix(0, ncol=2, nrow=length(unlist(trainGate1[1])))))

TimePerGate <- unlist(trainGate1[3])

# assign varibles' name
x <- c("speed", "traffic")
colnames(GateData) <- x


# given the speed and traffic
GateData$speed <- unlist(trainGate1[1])
GateData$traffic <-unlist(trainGate1[2])

kc <- kmeans(GateData$speed, 3)
GateData$cluster <- kc$cluster

GateData$TravelTime <- TimePerGate
GateData <- ddply(GateData, "speed", transform, set = sample(c("train", "test"), length(speed),replace = TRUE, prob = c(2, 1)))

#partition data as training set and testing set
train.data <- subset(GateData, set == "train")
test.data <- subset(GateData, set == "test")

#extract observations by clusters
data_cluster1 <- train.data[train.data$cluster==1,]
data_cluster2 <- train.data[train.data$cluster==2,]
data_cluster3 <- train.data[train.data$cluster==3,]

# fit by linear regression for each cluster
fit1 <- lm(TravelTime ~ speed+traffic, data = data_cluster1)
estimated1 <- predict(fit1, test.data[test.data$cluster==1,])
print(length(estimated1))

fit2 <- lm(TravelTime~speed+traffic, data=data_cluster2)
estimated2 <- predict(fit2, test.data[test.data$cluster==2,])

fit3 <- lm(data_cluster3$TravelTime~data_cluster3$speed+data_cluster3$traffic)
print(length(test.data[test.data$cluster==3,]))
estimated3 <- predict(fit3, test.data[test.data$cluster==3,])
print(length(estimated2))

obs1 <- test.data[test.data$cluster==1,]$TravelTime
obs2 <- test.data[test.data$cluster==2,]$TravelTime
obs3 <- test.data[test.data$cluster==3,]$TravelTime

rmse2 <- sqrt(mean((obs1-estimated1)^2+(obs2-estimated2)^2+(obs3-estimated3)^2))
#rmse2 <- sqrt(mean((obs2-estimated2)^2))
#rmse3 <- sqrt(mean((obs3-estimated3)^2))

# plot for each cluster with linear regression
g1<- ggplot(GateData, aes(traffic, speed, color=kc$cluster)) + geom_point()
print(g1)
cluster1 <- ggplot(data_cluster1, aes(traffic, speed))+geom_point()+geom_smooth(method = "lm")
print(cluster1)

cluster2 <- ggplot(data_cluster2, aes(traffic, speed))+geom_point()+geom_smooth(method = "lm")
print(cluster2)

cluster3 <- ggplot(data_cluster3, aes(traffic, speed))+geom_point()+geom_smooth(method = "lm")
print(cluster3)
# end of Gate 2

# start Gate 3
trainGate3 <- GateDataOp(GatesName[3],"04")

# empty data.frame
GateData <- data.frame((matrix(0, ncol=2, nrow=length(unlist(trainGate1[1])))))

TimePerGate <- unlist(trainGate1[3])

# assign varibles' name
x <- c("speed", "traffic")
colnames(GateData) <- x


# given the speed and traffic
GateData$speed <- unlist(trainGate1[1])
GateData$traffic <-unlist(trainGate1[2])

kc <- kmeans(GateData$speed, 3)
GateData$cluster <- kc$cluster

GateData$TravelTime <- TimePerGate
GateData <- ddply(GateData, "speed", transform, set = sample(c("train", "test"), length(speed),replace = TRUE, prob = c(2, 1)))

#partition data as training set and testing set
train.data <- subset(GateData, set == "train")
test.data <- subset(GateData, set == "test")

#extract observations by clusters
data_cluster1 <- train.data[train.data$cluster==1,]
data_cluster2 <- train.data[train.data$cluster==2,]
data_cluster3 <- train.data[train.data$cluster==3,]

# fit by linear regression for each cluster
fit1 <- lm(TravelTime ~ speed+traffic, data = data_cluster1)
estimated1 <- predict(fit1, test.data[test.data$cluster==1,])
print(length(estimated1))

fit2 <- lm(TravelTime~speed+traffic, data=data_cluster2)
estimated2 <- predict(fit2, test.data[test.data$cluster==2,])

fit3 <- lm(TravelTime~speed+traffic,data = data_cluster3)
print(length(test.data[test.data$cluster==3,]))
estimated3 <- predict(fit3, test.data[test.data$cluster==3,])
print(length(estimated2))

obs1 <- test.data[test.data$cluster==1,]$TravelTime
obs2 <- test.data[test.data$cluster==2,]$TravelTime
obs3 <- test.data[test.data$cluster==3,]$TravelTime

rmse3 <- sqrt(mean((obs1-estimated1)^2+(obs2-estimated2)^2+(obs3-estimated3)^2))
#rmse2 <- sqrt(mean((obs2-estimated2)^2))
#rmse3 <- sqrt(mean((obs3-estimated3)^2))

# plot for each cluster with linear regression
g1<- ggplot(GateData, aes(traffic, speed, color=kc$cluster)) + geom_point()
print(g1)
cluster1 <- ggplot(data_cluster1, aes(traffic, speed))+geom_point()+geom_smooth(method = "lm")
print(cluster1)

cluster2 <- ggplot(data_cluster2, aes(traffic, speed))+geom_point()+geom_smooth(method = "lm")
print(cluster2)

cluster3 <- ggplot(data_cluster3, aes(traffic, speed))+geom_point()+geom_smooth(method = "lm")
print(cluster3)
#end of Gate 3

# Start Gate 4
trainGate4 <- GateDataOp(GatesName[4],"04")

# empty data.frame
GateData <- data.frame((matrix(0, ncol=2, nrow=length(unlist(trainGate1[1])))))

TimePerGate <- unlist(trainGate1[3])

# assign varibles' name
x <- c("speed", "traffic")
colnames(GateData) <- x


# given the speed and traffic
GateData$speed <- unlist(trainGate1[1])
GateData$traffic <-unlist(trainGate1[2])

kc <- kmeans(GateData$speed, 3)
GateData$cluster <- kc$cluster

GateData$TravelTime <- TimePerGate
GateData <- ddply(GateData, "speed", transform, set = sample(c("train", "test"), length(speed),replace = TRUE, prob = c(2, 1)))

#partition data as training set and testing set
train.data <- subset(GateData, set == "train")
test.data <- subset(GateData, set == "test")

#extract observations by clusters
data_cluster1 <- train.data[train.data$cluster==1,]
data_cluster2 <- train.data[train.data$cluster==2,]
data_cluster3 <- train.data[train.data$cluster==3,]

# fit by linear regression for each cluster
fit1 <- lm(TravelTime ~ speed+traffic, data = data_cluster1)
estimated1 <- predict(fit1, test.data[test.data$cluster==1,])
print(length(estimated1))

fit2 <- lm(TravelTime~speed+traffic, data=data_cluster2)
estimated2 <- predict(fit2, test.data[test.data$cluster==2,])

fit3 <- lm(data_cluster3$TravelTime~data_cluster3$speed+data_cluster3$traffic)
print(length(test.data[test.data$cluster==3,]))
estimated3 <- predict(fit3, test.data[test.data$cluster==3,])
print(length(estimated2))

obs1 <- test.data[test.data$cluster==1,]$TravelTime
obs2 <- test.data[test.data$cluster==2,]$TravelTime
obs3 <- test.data[test.data$cluster==3,]$TravelTime

rmse4 <- sqrt(mean((obs1-estimated1)^2+(obs2-estimated2)^2+(obs3-estimated3)^2))
#rmse2 <- sqrt(mean((obs2-estimated2)^2))
#rmse3 <- sqrt(mean((obs3-estimated3)^2))

# plot for each cluster with linear regression
g1<- ggplot(GateData, aes(traffic, speed, color=kc$cluster)) + geom_point()
print(g1)
cluster1 <- ggplot(data_cluster1, aes(traffic, speed))+geom_point()+geom_smooth(method = "lm")
print(cluster1)

cluster2 <- ggplot(data_cluster2, aes(traffic, speed))+geom_point()+geom_smooth(method = "lm")
print(cluster2)

cluster3 <- ggplot(data_cluster3, aes(traffic, speed))+geom_point()+geom_smooth(method = "lm")
print(cluster3)
#end of Gate 4

# Start Gate 5

trainGate5 <- GateDataOp(GatesName[5],"04")

# empty data.frame
GateData <- data.frame((matrix(0, ncol=2, nrow=length(unlist(trainGate1[1])))))

TimePerGate <- unlist(trainGate1[3])

# assign varibles' name
x <- c("speed", "traffic")
colnames(GateData) <- x


# given the speed and traffic
GateData$speed <- unlist(trainGate1[1])
GateData$traffic <-unlist(trainGate1[2])

kc <- kmeans(GateData$speed, 3)
GateData$cluster <- kc$cluster

GateData$TravelTime <- TimePerGate
GateData <- ddply(GateData, "speed", transform, set = sample(c("train", "test"), length(speed),replace = TRUE, prob = c(2, 1)))

#partition data as training set and testing set
train.data <- subset(GateData, set == "train")
test.data <- subset(GateData, set == "test")

#extract observations by clusters
data_cluster1 <- train.data[train.data$cluster==1,]
data_cluster2 <- train.data[train.data$cluster==2,]
data_cluster3 <- train.data[train.data$cluster==3,]

# fit by linear regression for each cluster
fit1 <- lm(TravelTime ~ speed+traffic, data = data_cluster1)
estimated1 <- predict(fit1, test.data[test.data$cluster==1,])
print(length(estimated1))

fit2 <- lm(TravelTime~speed+traffic, data=data_cluster2)
estimated2 <- predict(fit2, test.data[test.data$cluster==2,])

fit3 <- lm(data_cluster3$TravelTime~data_cluster3$speed+data_cluster3$traffic)
print(length(test.data[test.data$cluster==3,]))
estimated3 <- predict(fit3, test.data[test.data$cluster==3,])
print(length(estimated2))

obs1 <- test.data[test.data$cluster==1,]$TravelTime
obs2 <- test.data[test.data$cluster==2,]$TravelTime
obs3 <- test.data[test.data$cluster==3,]$TravelTime

rmse5 <- sqrt(mean((obs1-estimated1)^2+(obs2-estimated2)^2+(obs3-estimated3)^2))
#rmse2 <- sqrt(mean((obs2-estimated2)^2))
#rmse3 <- sqrt(mean((obs3-estimated3)^2))

# plot for each cluster with linear regression
g1<- ggplot(GateData, aes(traffic, speed, color=kc$cluster)) + geom_point()
print(g1)
cluster1 <- ggplot(data_cluster1, aes(traffic, speed))+geom_point()+geom_smooth(method = "lm")
print(cluster1)

cluster2 <- ggplot(data_cluster2, aes(traffic, speed))+geom_point()+geom_smooth(method = "lm")
print(cluster2)

cluster3 <- ggplot(data_cluster3, aes(traffic, speed))+geom_point()+geom_smooth(method = "lm")
print(cluster3)

#end of Gate 5

# Start Gate 6

trainGate6 <- GateDataOp(GatesName[6],"04")

# empty data.frame
GateData <- data.frame((matrix(0, ncol=2, nrow=length(unlist(trainGate1[1])))))

TimePerGate <- unlist(trainGate1[3])

# assign varibles' name
x <- c("speed", "traffic")
colnames(GateData) <- x


# given the speed and traffic
GateData$speed <- unlist(trainGate1[1])
GateData$traffic <-unlist(trainGate1[2])

kc <- kmeans(GateData$speed, 3)
GateData$cluster <- kc$cluster

GateData$TravelTime <- TimePerGate
GateData <- ddply(GateData, "speed", transform, set = sample(c("train", "test"), length(speed),replace = TRUE, prob = c(2, 1)))

#partition data as training set and testing set
train.data <- subset(GateData, set == "train")
test.data <- subset(GateData, set == "test")

#extract observations by clusters
data_cluster1 <- train.data[train.data$cluster==1,]
data_cluster2 <- train.data[train.data$cluster==2,]
data_cluster3 <- train.data[train.data$cluster==3,]

# fit by linear regression for each cluster
fit1 <- lm(TravelTime ~ speed+traffic, data = data_cluster1)
estimated1 <- predict(fit1, test.data[test.data$cluster==1,])
print(length(estimated1))

fit2 <- lm(TravelTime~speed+traffic, data=data_cluster2)
estimated2 <- predict(fit2, test.data[test.data$cluster==2,])

fit3 <- lm(data_cluster3$TravelTime~data_cluster3$speed+data_cluster3$traffic)
print(length(test.data[test.data$cluster==3,]))
estimated3 <- predict(fit3, test.data[test.data$cluster==3,])
print(length(estimated2))

obs1 <- test.data[test.data$cluster==1,]$TravelTime
obs2 <- test.data[test.data$cluster==2,]$TravelTime
obs3 <- test.data[test.data$cluster==3,]$TravelTime

rmse6 <- sqrt(mean((obs1-estimated1)^2+(obs2-estimated2)^2+(obs3-estimated3)^2))
#rmse2 <- sqrt(mean((obs2-estimated2)^2))
#rmse3 <- sqrt(mean((obs3-estimated3)^2))

# plot for each cluster with linear regression
g1<- ggplot(GateData, aes(traffic, speed, color=kc$cluster)) + geom_point()
print(g1)
cluster1 <- ggplot(data_cluster1, aes(traffic, speed))+geom_point()+geom_smooth(method = "lm")
print(cluster1)

cluster2 <- ggplot(data_cluster2, aes(traffic, speed))+geom_point()+geom_smooth(method = "lm")
print(cluster2)

cluster3 <- ggplot(data_cluster3, aes(traffic, speed))+geom_point()+geom_smooth(method = "lm")
print(cluster3)
# end of Gate 6

print(sum(rmse1+rmse2+rmse3+rmse4+rmse5+rmse6))
#trainGate2 <- GateDataOp(GatesName[2],"04")
#trainGate3 <- GateDataOp(GatesName[3],"04")
#trainGate4 <- GateDataOp(GatesName[4],"04")
#trainGate5 <- GateDataOp(GatesName[5],"04")
#trainGate6 <- GateDataOp(GatesName[6],"04")

#print(GateData)
  

#DaySpeed <- data.frame(hoursPerDay, avgSpeed)
#saveFile <- paste(holidays[len], ".png", sep="")
#png(saveFile, width=4, height=4, units="in", res=300)
#barplot(DaySpeed$avgSpeed, names.arg = DaySpeed$hoursPerDay, main = holidays[len], xlab = "hours", ylab = "Speed(KM/H)")
#dev.off()