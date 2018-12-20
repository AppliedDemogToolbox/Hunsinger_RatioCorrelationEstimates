#######################################################################################################################################################################################################
#SIMPLE R CODE FOR SIMPLE RATIO-CORRELATION POPULATION ESTIMATES FOR US STATES, UNDER 18, FOLLOWING INSTRUCTIONS FROM:
#H. Shyrock and J. Segal (1980). 'The Methods and Materials of Demography', Volume 2. U.S. Department of Commerce.
#EDDIE HUNSINGER, AUGUST 2010 (UPDATED DECEMBER 2018)
#JUST LEARNING/RESEARCHING
#######################################################################################################################################################################################################


#######################################################################################################################################################################################################
#STEP 1: READ DATA IN AND GET RATIOS FOR THE RATIO-CORRELATION METHOD
#######################################################################################################################################################################################################

ModelData<-read.table(file="https://github.com/AppliedDemogToolbox/Hunsinger_RatioCorrelationEstimates/raw/master/WorkingVariables.csv",header=TRUE,sep=",")

QC_2000<-ModelData$QCEWEmployment_2000
QC_1990<-ModelData$QCEWEmployment_1990
QC_Ratios<-(QC_2000/sum(QC_2000))/(QC_1990/sum(QC_1990))

IT_2000<-ModelData$IncomeTax_2000
IT_1990<-ModelData$IncomeTax_1990
IT_Ratios<-(IT_2000/sum(IT_2000))/(IT_1990/sum(IT_1990))

P_2000<-ModelData$Population_2000
P_1990<-ModelData$Population_1990
P_1990Ratios<-(P_1990/sum(P_1990))
P_Ratios<-(P_2000/sum(P_2000))/(P_1990/sum(P_1990))


#######################################################################################################################################################################################################
#STEP 2: PLOT ANY SELECTED CORRELATIONS
#######################################################################################################################################################################################################

plot(P_Ratios~IT_Ratios+QC_Ratios)


#######################################################################################################################################################################################################
#STEP 3: MAKE A MODEL
#######################################################################################################################################################################################################

Model<-lm(P_Ratios~IT_Ratios+QC_Ratios)


#######################################################################################################################################################################################################
#STEP 4: REVIEW MODEL
#######################################################################################################################################################################################################

summary(Model)
hist(residuals(Model),10,xlim=c(-.2,.2),col="blue")


#######################################################################################################################################################################################################
#STEP 5: MAKE THE MODEL-PREDICTED ESTIMATES AND COMPARE THEM TO THE ACTUAL 2000 CENSUS (JUST TO SEE THE ERRORS FROM THE MODEL ITSELF, NOT TO SAY HOW A 2010 PREDICTION WILL COMPARE TO THE 2010 CENSUS)
#######################################################################################################################################################################################################

P_2000RatioChangePrediction<-Model$coefficients[1]+IT_Ratios*Model$coefficients[2]+QC_Ratios*Model$coefficients[3]
P_2000Prediction<-(P_2000RatioChangePrediction*P_1990Ratios*sum(P_2000))
P_2000Prediction<-P_2000Prediction/sum(P_2000Prediction)*sum(P_2000)

plot(P_2000Prediction~P_2000)

plot(P_2000Prediction,lwd="3")
points(P_2000,col="red")

hist((P_2000Prediction-P_2000)/P_2000,10,xlim=c(-.2,.2),col="green")
quantile((P_2000Prediction-P_2000)/P_2000,seq(0,1,.05))

hist((P_2000Prediction-P_2000),20,xlim=c(-1000000,1000000),col="yellow")
quantile(P_2000Prediction-P_2000,seq(0,1,.05))

ModelMAPE<-mean(abs((P_2000Prediction-P_2000)/P_2000))*100
ModelMAPE


#######################################################################################################################################################################################################
#STEP 6: TABLE AND WRITE THE DATA OUT TO A CSV...CONSIDER MORE
#######################################################################################################################################################################################################

P_2000PredictionAndActual<-array(c(P_2000Prediction,P_2000,P_1990,P_2000Prediction-P_2000,(P_2000Prediction-P_2000)/P_2000),c(length(P_2000Prediction),5))
write.table(P_2000PredictionAndActual,file="C:/Users/Eddie/Desktop/WorkingModelFit.csv",sep=",")
Sys.sleep(3)


#######################################################################################################################################################################################################
#STEP 7: READ IN DATA FOR 2009 VARIABLES (AND 2000 WITH UPDATED GEOGRAPHIC BOUNDARIES), TO BE USED IN A 2009 RATIO-CORRELATION POPULATION ESTIMATE
#######################################################################################################################################################################################################

#EstimateData2009<-read.table(file="https://github.com/AppliedDemogToolbox/Hunsinger_RatioCorrelationEstimates/raw/master/2009EstimateVariables.csv,header=TRUE,sep=",")

B2009_2009<-EstimateData2009$Births_2009
B2009_2000<-EstimateData2009$Births_2000
B2009_Ratios<-(B2009_2009/sum(B2009_2009))/(B2009_2000/sum(B2009_2000))

SE2009_2009<-EstimateData2009$SchoolEnrollment_2009
SE2009_2000<-EstimateData2009$SchoolEnrollment_2000
SE2009_Ratios<-(SE2009_2009/sum(SE2009_2009))/(SE2009_2000/sum(SE2009_2000))

P2009_2009<-EstimateData2009$Population_2009
P2009_2000<-EstimateData2009$Population_2000
P2009_2000Ratios<-(P2009_2000/sum(P2009_2000))
P2009_Ratios<-(P2009_2009/sum(P2009_2009))/(P2009_2000/sum(P2009_2000))


#######################################################################################################################################################################################################
#STEP 8: MAKE THE MODEL-PREDICTED ESTIMATES AND COMPARE THEM TO OTHER ESTIMATES 
#######################################################################################################################################################################################################

P_2009RatioChangePrediction<-Model$coefficients[1]+B2009_Ratios*Model$coefficients[2]+SE2009_Ratios*Model$coefficients[3]
P_2009Prediction<-(P_2009RatioChangePrediction*P2009_2000Ratios*sum(P2009_2009))
P_2009Prediction<-P_2009Prediction/sum(P_2009Prediction)*sum(P2009_2009)

plot(P_2009Prediction~P2009_2009)

plot(P_2009Prediction,lwd="3")
points(P2009_2009,col="red")

hist((P_2009Prediction-P2009_2009)/P2009_2009,40,xlim=c(-.2,.2),col="green")
quantile((P_2009Prediction-P2009_2009)/P2009_2009,seq(0,1,.05))

hist((P_2009Prediction-P2009_2009),10,xlim=c(-100000,100000),col="yellow")
quantile(P_2009Prediction-P2009_2009,seq(0,1,.05))

EstimateCompareMAPE<-mean(abs((P_2009Prediction-P2009_2009)/P2009_2009))*100
EstimateCompareMAPE


#######################################################################################################################################################################################################
#STEP 9: TABLE AND WRITE THE 2009 ESTIMATE DATA OUT TO A CSV...CONSIDER MORE
#######################################################################################################################################################################################################

P_2009PredictionAndActual<-array(c(P_2009Prediction,P2009_2009,P2009_2000,P_2009Prediction-P2009_2009,(P_2009Prediction-P2009_2009)/P2009_2009),c(length(P_2009Prediction),5))
#write.table(P_2009PredictionAndActual,file="...Estimated2009.csv",sep=",")
