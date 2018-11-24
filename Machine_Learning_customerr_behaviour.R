####################################
# RAJIB MANDAL (TA17002)

###################################


library(car)
library(class)
library(caTools)
library(ROCR)
library(e1071)
library(h2o)
library(caret) 
library(ggplot2)
library(gridExtra)

#################################################
# set  working directory

##################################################
setwd("C:\\Software\\xlri\\Machine Learning\\Assignment")

####################################################
### Reading the csv file
#################################################
telecomcustomerdataframe<-read.csv("Telecom Customer Data.csv",header=TRUE, na.strings=c("",'NA'))

###################################################
###Get Dataset information
###################################################
str(telecomcustomerdataframe)
dim(telecomcustomerdataframe)
summary(telecomcustomerdataframe)

###################################################
#Outlier detection and treatment
##################################################


# Outliers for tenure
boxplot(telecomcustomerdataframe$tenure,col = "bisque",xlab="Tenure")


# Outliers for monthly charge

boxplot(telecomcustomerdataframe$MonthlyCharges,col = "bisque",xlab="Monthly Charges")


# Outlier for total charge

boxplot(telecomcustomerdataframe$TotalCharges,col = "bisque",xlab="Total Charges")



# create new column "tenureinterval" from the tenure column
group_tenure <- function(tenure){
  if (tenure >= 0 && tenure <= 6){
    return('0-6 Month')
  }else if(tenure > 6 && tenure <= 12){
    return('6-12 Month')
  }else if (tenure > 12 && tenure <= 24){
    return('12-24 Month')
  }else if (tenure > 24 && tenure <=36){
    return('24-36 Month')
  }else if (tenure > 36 && tenure <=48){
    return('36-48 Month')
  }else if (tenure > 48 && tenure <= 62){
    return('48-62 Month')
  }else if (tenure > 62){
    return('> 62 Month')
  }
}

# apply group_tenure function on each row of dataframe
telecomcustomerdataframe$tenureinterval <- sapply(telecomcustomerdataframe$tenure,group_tenure)
# telecomcustomerdataframe$tenureinterval <- as.factor(telecomcustomerdataframe$tenureinterval)

###################################################
# Analysis on numeric data
###################################################

ggplot() + geom_bar(data = telecomcustomerdataframe,
           aes(x = factor(telecomcustomerdataframe$tenureinterval), fill = factor(telecomcustomerdataframe$Churn)),
           position = "stack") +scale_x_discrete("Tenure")  + scale_y_continuous("Percent") + guides(fill=guide_legend(title="Tenure drives Churn")) + scale_fill_manual(values=c("blue","red"))




##################################################
#Exploratory data analysis and feature selection
##################################################

# Make bar charts to find  relationships between variables.
#A function to make bar plots and save it in the working directory

churnbargraph <- function(z, na.rm = TRUE, ...) {
  nm <- names(z)
  for (i in seq_along(nm)) {
    plots <-ggplot(z,aes_string(x=nm[i],fill=factor(z$Churn))) + geom_bar(position = "fill")+
      guides(fill=guide_legend(reverse=TRUE))+
      scale_fill_discrete(labels=c("Not Churned Customer","Churned Customer"))+
       #scale_fill_manual(values=c("blue","red"))+
      labs(fill='churn status')
    ggsave(plots,width = 20, height = 8, units = "cm",filename=paste("churnedplot",nm[i],".png",sep=""))
  }
}

churnbargraph(telecomcustomerdataframe[,-c(1,2,5,6,19,20,22)])


plotgender <- ggplot(telecomcustomerdataframe, aes(x=gender)) + ggtitle("Gender") + xlab("Gender") +
   geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
 plotseniorcitizen <- ggplot(telecomcustomerdataframe, aes(x=SeniorCitizen)) + ggtitle("Senior Citizen") + xlab("Senior Citizen") +
   geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
 plotpartner <- ggplot(telecomcustomerdataframe, aes(x=Partner)) + ggtitle("Partner") + xlab("Partner") +
   geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()


 plotdependents <- ggplot(telecomcustomerdataframe, aes(x=Dependents)) + ggtitle("Dependents") + xlab("Dependents") +
   geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()

 plotphoneservice <- ggplot(telecomcustomerdataframe, aes(x=PhoneService)) + ggtitle("Phone Service") + xlab("Phone Service") +
   geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
 plotmultiplelines <- ggplot(telecomcustomerdataframe, aes(x=MultipleLines)) + ggtitle("Multiple Lines") + xlab("Multiple Lines") +
   geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
 plotinternetservice <- ggplot(telecomcustomerdataframe, aes(x=InternetService)) + ggtitle("Internet Service") + xlab("Internet Service") +
   geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
 plotOnlineSecurity <- ggplot(telecomcustomerdataframe, aes(x=OnlineSecurity)) + ggtitle("Online Security") + xlab("Online Security") +
   geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()

 plotonlinebackup <- ggplot(telecomcustomerdataframe, aes(x=OnlineBackup)) + ggtitle("Online Backup") + xlab("Online Backup") +
   geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()

grid.arrange(plotgender, plotseniorcitizen, plotpartner,
             plotdependents, plotphoneservice,plotmultiplelines,
             plotinternetservice,plotOnlineSecurity,plotonlinebackup,
             ncol=3)

plotDeviceProtection <- ggplot(telecomcustomerdataframe, aes(x=DeviceProtection)) + ggtitle("Device Protection") + xlab("Device Protection") +
   geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
 plotTechSupport <- ggplot(telecomcustomerdataframe, aes(x=TechSupport)) + ggtitle("Tech Support") + xlab("Tech Support") +
   geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
 plotStreamingTV <- ggplot(telecomcustomerdataframe, aes(x=StreamingTV)) + ggtitle("Streaming TV") + xlab("Streaming TV") +
   geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()

 plotStreamingMovies <- ggplot(telecomcustomerdataframe, aes(x=StreamingMovies)) + ggtitle("Streaming Movies") + xlab("Streaming Movies") +
   geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
 plotContract <- ggplot(telecomcustomerdataframe, aes(x=Contract)) + ggtitle("Contract") + xlab("Contract") +
   geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
 plotPaperlessBilling <- ggplot(telecomcustomerdataframe, aes(x=PaperlessBilling)) + ggtitle("Paperless Billing") + xlab("Paperless Billing") +
   geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
 plotPaymentMethod <- ggplot(telecomcustomerdataframe, aes(x=PaymentMethod)) + ggtitle("Payment Method") + xlab("Payment Method") +
   geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()

grid.arrange(plotDeviceProtection, plotTechSupport, plotStreamingTV,
             plotStreamingMovies, plotContract, plotPaperlessBilling,
             plotPaymentMethod,plotPaperlessBilling,
             # ,plottenuregroup,
             ncol=3)


##################################################################################
# Assign "No" for "No phone service" & No internet service"


###################################################################################
telecomcustomerdataframe$MultipleLines[telecomcustomerdataframe$MultipleLines=="No phone service"] <- "No"
telecomcustomerdataframe$OnlineSecurity[telecomcustomerdataframe$OnlineSecurity=="No internet service"] <- "No"
telecomcustomerdataframe$OnlineBackup[telecomcustomerdataframe$OnlineBackup=="No internet service"] <- "No"
telecomcustomerdataframe$DeviceProtection[telecomcustomerdataframe$DeviceProtection=="No internet service"] <- "No"
telecomcustomerdataframe$TechSupport[telecomcustomerdataframe$TechSupport=="No internet service"] <- "No"
telecomcustomerdataframe$StreamingTV[telecomcustomerdataframe$StreamingTV=="No internet service"] <- "No"
telecomcustomerdataframe$StreamingMovies[telecomcustomerdataframe$StreamingMovies=="No internet service"] <- "No"


##################################################################################
# convert factor variables into binary 1/0
###################################################################################

telecomcustomerdataframe$gender<-ifelse(telecomcustomerdataframe$gender=='Male',1,0)
telecomcustomerdataframe$Partner<-ifelse(telecomcustomerdataframe$Partner=='Yes',1,0)
telecomcustomerdataframe$Dependents<-ifelse(telecomcustomerdataframe$Dependents=='Yes',1,0)
telecomcustomerdataframe$PhoneService<-ifelse(telecomcustomerdataframe$PhoneService=='Yes',1,0)
telecomcustomerdataframe$MultipleLines<-ifelse(telecomcustomerdataframe$MultipleLines=='Yes',1,0)
telecomcustomerdataframe$OnlineSecurity<-ifelse(telecomcustomerdataframe$OnlineSecurity=='Yes',1,0)
telecomcustomerdataframe$OnlineBackup<-ifelse(telecomcustomerdataframe$OnlineBackup=='Yes',1,0)
telecomcustomerdataframe$DeviceProtection<-ifelse(telecomcustomerdataframe$DeviceProtection=='Yes',1,0)
telecomcustomerdataframe$TechSupport<-ifelse(telecomcustomerdataframe$TechSupport=='Yes',1,0)
telecomcustomerdataframe$StreamingTV<-ifelse(telecomcustomerdataframe$StreamingTV=='Yes',1,0)
telecomcustomerdataframe$StreamingMovies<-ifelse(telecomcustomerdataframe$StreamingMovies=='Yes',1,0)
telecomcustomerdataframe$PaperlessBilling<-ifelse(telecomcustomerdataframe$PaperlessBilling=='Yes',1,0)
telecomcustomerdataframe$Churn<-ifelse(telecomcustomerdataframe$Churn=='Yes',1,0)
##################################
##Transformed into dummy varaibles
#################################

for(level in unique(telecomcustomerdataframe$InternetService)){
  telecomcustomerdataframe[paste("InternetService",   gsub("-","_", gsub(" ","_",level, fixed=TRUE), fixed=TRUE), sep = "_")] <- ifelse(telecomcustomerdataframe$InternetService == level, 1, 0)
}

for(level in unique(telecomcustomerdataframe$Contract)){
  telecomcustomerdataframe[paste("Contract",   gsub("-","_", gsub(" ","_",level, fixed=TRUE), fixed=TRUE), sep = "_")] <- ifelse(telecomcustomerdataframe$Contract == level, 1, 0)
}

for(level in unique(telecomcustomerdataframe$PaymentMethod)){
  telecomcustomerdataframe[paste("PaymentMethod",   gsub("-","_", gsub(" ","_",level, fixed=TRUE), fixed=TRUE), sep = "_")] <- ifelse(telecomcustomerdataframe$PaymentMethod == level, 1, 0)
}

#########################################################
#Missing values are present in Total charge only
#Replace missing value with mean
##########################################################
telecomcustomerdataframe$TotalCharges[is.na(telecomcustomerdataframe$TotalCharges)]<-mean(telecomcustomerdataframe$TotalCharges, na.rm = TRUE)

###########################################
# Standardize Data in min- max scale
##########################################

minmaxstandard <- function(x)
              {
                (x-min(x))/(max(x)-min(x))
            }

telecomcustomerdataframe$TotalCharges<-as.numeric(minmaxstandard(telecomcustomerdataframe$TotalCharges))
telecomcustomerdataframe$MonthlyCharges <-as.numeric(minmaxstandard(telecomcustomerdataframe$MonthlyCharges))

#######################################################
# Remove customerID,InternetService,Contract & PaymentMethod
## FROM DATASET
#####################################################

telecomcustomerdataframe$customerID<-NULL
telecomcustomerdataframe$InternetService<-NULL
telecomcustomerdataframe$Contract<-NULL
telecomcustomerdataframe$PaymentMethod<-NULL
telecomcustomerdataframe$tenureinterval<-NULL
# telecomcustomerdataframe$tenure<-NULL

##################################################
# set the seed it will output same output when ever the model is executed
##################################################
set.seed(123)

##################################################

#Splliting the dataset
# sample the input data with 70% for training and 30% for testing
################################################

sample <- sample.split(telecomcustomerdataframe$Churn,SplitRatio=0.70)
trainData <- subset(telecomcustomerdataframe,sample==TRUE)
testData <- subset(telecomcustomerdataframe,sample==FALSE)


######################################
##SVM MODEL
######################################




######################################
##SVM MODEL DEFINITION
######################################
svmmodel <- svm(formula = Churn~.,
                data = trainData,
                type = 'C-classification',
                kernel = 'radial',
                #cost=0.01,
                probability=TRUE
)
summary(svmmodel)
svmmodelpred<-predict(svmmodel, newdata = testData[-17], type="probability")
str(svmmodelpred)
##########################################
##PLOT THE ROC CURVE AND GET AUC
##########################################
modelprediction<-prediction(as.numeric(svmmodelpred),as.numeric(testData$Churn))

modelprediction
modelperformance<-performance(modelprediction,"tpr","fpr")
plot(modelperformance,colorize = TRUE)
abline(a=0,b=1,lwd=3,lty=2,col = "green")
svmaccrcy<-performance(modelprediction,"auc")
svmaccrcy


###########################################
### ACCURACY OF THE SUPPORT VECTOR MACHINE
###  confusion Matrix ,SENSIVITY & SPECIFICITY

###########################################

svmconfusionmatrix <- table(testData[,17], svmmodelpred)
sum(diag(svmconfusionmatrix))/sum(svmconfusionmatrix)
sensitivity(svmconfusionmatrix)
specificity(svmconfusionmatrix)
#confusionMatrix(svmmodelpred,testData$Churn,positive='1')


####################################
# KNN Model
###################################

######################################
# Identify optimal K
####################################

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3333)
knnfit <- train(as.factor(Churn )~., data = trainData, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 10)

  knnfit
####################################
# DEFINE KNN Model
###################################

knnmodel = knn(train = trainData[,-17],
                  test = testData[,-17],
                  cl = trainData[,17],
                  k = 23,
                  prob=TRUE,
                 use.all = TRUE
                 )

summary(knnmodel)
##########################################
##PLOT THE ROC CURVE AND GET THE AUC
##########################################
knnmodel<-as.numeric(knnmodel)

modelprediction<-prediction(knnmodel,testData$Churn)

modelperformance<-performance(modelprediction,"tpr","fpr")
plot(modelperformance,colorize = TRUE)
abline(a=0,b=1,lwd=2,lty=2,col = "green")
knnacrcy<-performance(modelprediction,"auc")
knnacrcy

###########################################
### ACCURACY OF THE KNN MODEL
###  confusion Matrix ,SENSIVITY & SPECIFICITY

###########################################

knnconfusionmatrix <- table(testData[,17], knnmodel)
knnconfusionmatrix
sum(diag(knnconfusionmatrix))/sum(knnconfusionmatrix)
colnames(knnconfusionmatrix)<-c("0","1")
sensitivity(knnconfusionmatrix)
specificity(knnconfusionmatrix)


####################################
# ANN Model
###################################

set.seed(200)
# h2o.init(nthreads = -1)
h2o.init(max_mem_size="4g", nthreads=-1)


######################################
##ANN MODEL DEFINITION
######################################
classifier = h2o.deeplearning(y = 'Churn',
                              training_frame = as.h2o(trainData),
                              activation = 'Rectifier',
                              hidden = c(5,5),
                              epochs = 5,
                              shuffle_training_data=TRUE,
                              replicate_training_data=TRUE,
                              adaptive_rate=T,
                              overwrite_with_best_model=TRUE,
                              train_samples_per_iteration = -2,
                              #rate=0.01, 
                              seed=100)

summary(classifier)

# h2o.varimp : obtaining the variable importance

head( as.data.frame.table( h2o.varimp(classifier) ) )
####################################################

##############################################
#Predicting the test set results
############################################
annprobpred = h2o.predict(classifier, newdata = as.h2o(testData[-17]))
predtestdata =  ifelse(annprobpred > 0.5, 1, 0)
predtestdata = as.vector(predtestdata )



##########################################
##PLOT THE ROC CURVE AND GET AUC
##########################################

modelprediction<-prediction(predtestdata,testData$Churn)


modelperformance<-performance(modelprediction,"tpr","fpr")
plot(modelperformance,colorize = TRUE)
abline(a=0,b=1,lwd=2,lty=2,col = "green")
annaccry<-performance(modelprediction,"auc")
annaccry



###########################################
### ACCURACY OF THE ANN
###  confusion Matrix ,SENSIVITY & SPECIFICITY

###########################################

annconfusionmatrix <- table(testData[,17], predtestdata)
sum(diag(annconfusionmatrix))/sum(annconfusionmatrix)
sensitivity(annconfusionmatrix)
specificity(annconfusionmatrix)



