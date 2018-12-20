#######################################################################################################################################################################################################
##SIMPLE R CODE FOR SIMPLE RATIO-CORRELATION POPULATION ESTIMATES FOR COLORADO COUNTIES, FOLLOWING INSTRUCTIONS FROM:
##H. Shyrock and J. Segal (1980). 'The Methods and Materials of Demography', Volume 2. U.S. Department of Commerce.
##NOTE: THE MODEL IS APPLIED HERE WITH THE FULL (TEN YEAR) RATIO ADJUSTMENT, BUT FOLLOWING REASON, THE RATIO ADJUSTMENT SHOULD BE INTERPOLATED.
##FURTHER NOTES AT http://www.demog.berkeley.edu/~eddieh/AppliedDemographyToolbox/RatioCorrelationEstimates/RegressionEstimates.pdf
##
##EDDIE HUNSINGER, AUGUST 2010 (LAST UPDATED APRIL 2014)
##http://www.demog.berkeley.edu/~eddieh/
##edynivn@gmail.com
##
##TO SEE THE RELATED PAPER (WITH REFERENCES), GO TO http://www.demog.berkeley.edu/~eddieh/RatioCorrelationEstimates/RegressionEstimates.pdf
##
##IF YOU WOULD LIKE TO USE, SHARE OR REPRODUCE THIS CODE, BE SURE TO CITE THE SOURCE
##
##EXAMPLE DATA IS LINKED, SO YOU SHOULD BE ABLE TO SIMPLY COPY ALL AND PASTE INTO R
##
##THERE IS NO WARRANTY FOR THIS CODE
#######################################################################################################################################################################################################


#######################################################################################################################################################################################################
#STEP 1: READ DATA IN AND GET RATIOS FOR THE RATIO-CORRELATION METHOD
#######################################################################################################################################################################################################

ModelData<-read.table(file="http://www.demog.berkeley.edu/~eddieh/RatioCorrelationEstimates/ModelVariables.csv",header=TRUE,sep=",")

HU_2000<-ModelData$HousingUnits_2000
HU_1990<-ModelData$HousingUnits_1990
HU_Ratios<-(HU_2000/sum(HU_2000))/(HU_1990/sum(HU_1990))

B_2000<-ModelData$Births_2000
B_1990<-ModelData$Births_1990
B_Ratios<-(B_2000/sum(B_2000))/(B_1990/sum(B_1990))

D_2000<-ModelData$Deaths_2000
D_1990<-ModelData$Deaths_1990
D_Ratios<-(D_2000/sum(D_2000))/(D_1990/sum(D_1990))

SE_2000<-ModelData$SchoolEnrollment_2000
SE_1990<-ModelData$SchoolEnrollment_1990
SE_Ratios<-(SE_2000/sum(SE_2000))/(SE_1990/sum(SE_1990))

QC_2000<-ModelData$QCEW_2000
QC_1990<-ModelData$QCEW_1990
QC_Ratios<-(QC_2000/sum(QC_2000))/(QC_1990/sum(QC_1990))

RVe_2000<-ModelData$RegisteredVehicles_2000
RVe_1990<-ModelData$RegisteredVehicles_1990
RVe_Ratios<-(RVe_2000/sum(RVe_2000))/(RVe_1990/sum(RVe_1990))

RVo_2000<-ModelData$RegisteredVoters_2000
RVo_1990<-ModelData$RegisteredVoters_1990
RVo_Ratios<-(RVo_2000/sum(RVo_2000))/(RVo_1990/sum(RVo_1990))

Small<-ModelData$Small
DenverMetro<-ModelData$DenverMetro
Tourism<-ModelData$Tourism
Prison<-ModelData$Prison

P_2000<-ModelData$HouseholdPopulation_2000
P_1990<-ModelData$HouseholdPopulation_1990
P_1990Ratios<-(P_1990/sum(P_1990))
P_Ratios<-(P_2000/sum(P_2000))/(P_1990/sum(P_1990))


#######################################################################################################################################################################################################
#STEP 2: PLOT ANY SELECTED CORRELATIONS
#######################################################################################################################################################################################################

plot(P_Ratios~B_Ratios+SE_Ratios+RVe_Ratios+RVo_Ratios)


#######################################################################################################################################################################################################
#STEP 3: MAKE A MODEL
#######################################################################################################################################################################################################

Model<-lm(P_Ratios~B_Ratios+SE_Ratios+RVe_Ratios+RVo_Ratios)


#######################################################################################################################################################################################################
#STEP 4: REVIEW MODEL
#######################################################################################################################################################################################################

summary(Model)
hist(residuals(Model),10,xlim=c(-.2,.2),col="blue")


#######################################################################################################################################################################################################
#STEP 5: MAKE THE MODEL-PREDICTED ESTIMATES AND COMPARE THEM TO THE ACTUAL 2000 CENSUS (JUST TO SEE THE ERRORS FROM THE MODEL ITSELF, NOT TO SAY HOW A 2010 PREDICTION WILL COMPARE TO THE 2010 CENSUS)
#######################################################################################################################################################################################################

P_2000RatioChangePrediction<-Model$coefficients[1]+B_Ratios*Model$coefficients[2]+SE_Ratios*Model$coefficients[3]+RVe_Ratios*Model$coefficients[4]+RVo_Ratios*Model$coefficients[5]
P_2000Prediction<-(P_2000RatioChangePrediction*P_1990Ratios*sum(P_2000))
P_2000Prediction<-P_2000Prediction/sum(P_2000Prediction)*sum(P_2000)

plot(P_2000Prediction~P_2000)

plot(P_2000Prediction,lwd="3")
points(P_2000,col="red")

hist((P_2000Prediction-P_2000)/P_2000,10,xlim=c(-.2,.2),col="green")
quantile((P_2000Prediction-P_2000)/P_2000,seq(0,1,.05))

hist((P_2000Prediction-P_2000),20,xlim=c(-30000,30000),col="yellow")
quantile(P_2000Prediction-P_2000,seq(0,1,.05))

ModelMAPE<-mean(abs((P_2000Prediction-P_2000)/P_2000))*100
ModelMAPE


#######################################################################################################################################################################################################
#STEP 6: TABLE AND WRITE THE DATA OUT TO A CSV...CONSIDER MORE
#######################################################################################################################################################################################################

P_2000PredictionAndActual<-array(c(P_2000Prediction,P_2000,P_1990,P_2000Prediction-P_2000,(P_2000Prediction-P_2000)/P_2000),c(length(P_2000Prediction),5))
#write.table(P_2000PredictionAndActual,file="...FittedModel.csv",sep=",")
Sys.sleep(3)


#######################################################################################################################################################################################################
#STEP 7: READ IN DATA FOR 2009 VARIABLES (AND 2000 WITH UPDATED GEOGRAPHIC BOUNDARIES), TO BE USED IN A 2009 RATIO-CORRELATION POPULATION ESTIMATE
#######################################################################################################################################################################################################

EstimateData2009<-read.table(file="http://www.demog.berkeley.edu/~eddieh/RatioCorrelationEstimates/2009EstimateVariables.csv",header=TRUE,sep=",")

HU2009_2009<-EstimateData2009$HousingUnits_2009
HU2009_2000<-EstimateData2009$HousingUnits_2000
HU2009_Ratios<-(HU2009_2009/sum(HU2009_2009))/(HU2009_2000/sum(HU2009_2000))

B2009_2009<-EstimateData2009$Births_2009
B2009_2000<-EstimateData2009$Births_2000
B2009_Ratios<-(B2009_2009/sum(B2009_2009))/(B2009_2000/sum(B2009_2000))

D2009_2009<-EstimateData2009$Deaths_2009
D2009_2000<-EstimateData2009$Deaths_2000
D2009_Ratios<-(D2009_2009/sum(D2009_2009))/(D2009_2000/sum(D2009_2000))

SE2009_2009<-EstimateData2009$SchoolEnrollment_2009
SE2009_2000<-EstimateData2009$SchoolEnrollment_2000
SE2009_Ratios<-(SE2009_2009/sum(SE2009_2009))/(SE2009_2000/sum(SE2009_2000))

QC2009_2009<-EstimateData2009$QCEW_2009
QC2009_2000<-EstimateData2009$QCEW_2000
QC2009_Ratios<-(QC2009_2009/sum(QC2009_2009))/(QC2009_2000/sum(QC2009_2000))

RVe2009_2009<-EstimateData2009$RegisteredVehicles_2009
RVe2009_2000<-EstimateData2009$RegisteredVehicles_2000
RVe2009_Ratios<-(RVe2009_2009/sum(RVe2009_2009))/(RVe2009_2000/sum(RVe2009_2000))

RVo2009_2009<-EstimateData2009$RegisteredVoters_2009
RVo2009_2000<-EstimateData2009$RegisteredVoters_2000
RVo2009_Ratios<-(RVo2009_2009/sum(RVo2009_2009))/(RVo2009_2000/sum(RVo2009_2000))

Small2009<-EstimateData2009$Small
DenverMetro2009<-EstimateData2009$DenverMetro
Tourism2009<-EstimateData2009$Tourism
Prison2009<-EstimateData2009$Prison

P2009_2009<-EstimateData2009$HouseholdPopulation_2009
P2009_2000<-EstimateData2009$HouseholdPopulation_2000
P2009_2000Ratios<-(P2009_2000/sum(P2009_2000))
P2009_Ratios<-(P2009_2009/sum(P2009_2009))/(P2009_2000/sum(P2009_2000))


#######################################################################################################################################################################################################
#STEP 8: MAKE THE MODEL-PREDICTED ESTIMATES AND COMPARE THEM TO OTHER ESTIMATES 
#######################################################################################################################################################################################################

P_2009RatioChangePrediction<-Model$coefficients[1]+B2009_Ratios*Model$coefficients[2]+SE2009_Ratios*Model$coefficients[3]+RVe2009_Ratios*Model$coefficients[4]+RVo2009_Ratios*Model$coefficients[5]
P_2009Prediction<-(P_2009RatioChangePrediction*P2009_2000Ratios*sum(P2009_2009))
P_2009Prediction<-P_2009Prediction/sum(P_2009Prediction)*sum(P2009_2009)

plot(P_2009Prediction~P2009_2009)

plot(P_2009Prediction,lwd="3")
points(P2009_2009,col="red")

hist((P_2009Prediction-P2009_2009)/P2009_2009,40,xlim=c(-.2,.2),col="green")
quantile((P_2009Prediction-P2009_2009)/P2009_2009,seq(0,1,.05))

hist((P_2009Prediction-P2009_2009),10,xlim=c(-30000,30000),col="yellow")
quantile(P_2009Prediction-P2009_2009,seq(0,1,.05))

EstimateCompareMAPE<-mean(abs((P_2009Prediction-P2009_2009)/P2009_2009))*100
EstimateCompareMAPE


#######################################################################################################################################################################################################
#STEP 9: TABLE AND WRITE THE 2009 ESTIMATE DATA OUT TO A CSV...CONSIDER MORE
#######################################################################################################################################################################################################

P_2009PredictionAndActual<-array(c(P_2009Prediction,P2009_2009,P2009_2000,P_2009Prediction-P2009_2009,(P_2009Prediction-P2009_2009)/P2009_2009),c(length(P_2009Prediction),5))
#write.table(P_2009PredictionAndActual,file="...Estimated2009.csv",sep=",")
