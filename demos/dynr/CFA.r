#--- Confirmatory factor analysis ----
# This example illustrates how to fit a confirmatory factor analysis
# model to cross-sectional data (T = 1) using estimation routines
# designed for state-space models. Note that the trick here is to
# "pretend" that the data at hand have two time points, but all of
# the observations at t = 1 are missing.
#
# Author: Sy-Miin Chow
#------------------------------------------------------------------
filey=paste0('../Data/CFA.dat')  # output file for obs y

data = read.table(filey,header=FALSE)
data$ID = 1:dim(data)[1] #Add subject ID to the data set
np = dim(data)[1] #Number of subjects, np = number of rows of the data
data$Time = rep(1,np) #Create a time index variable

#Creating an additional row of missing data for each subject
data = rbind(data.frame(V1=rep(NA,np),V2=rep(NA,np),V3=rep(NA,np),V4=rep(NA,np),
                        V5=rep(NA,np),V6=rep(NA,np),ID=data$ID,Time=rep(0,np)),
             data)
#Order the data by ID and time so the data set is a relational (long)
#data set. Moreover, all time points for one subject are first stacked
#together before data from the next subject are included i.e.,
# subject_1   time_0
# subject_1   time_1
# subject_2   time_0
# subject_2   time_1
# ...
# subject_n   time_0
# subject_n   time_1

data = data[order(data$ID,data$Time),] 

library('dynr')

# Set up the data structure for dynr
data2 <- dynr.data(data, id="ID", time="Time", observed=paste0("V",1:6))

# Define the dynamic model - the transition matrix
dynamics <- prep.matrixDynamics(
  values.dyn=matrix(rep(0,4), ncol=2,byrow=TRUE),
  params.dyn=matrix(rep('fixed',4), ncol=2,byrow=TRUE), 
  values.int = c(0,0),
  params.int =c('fixed','fixed'),
  isContinuousTime=FALSE)

meas <- prep.measurement(
  values.load=matrix(c(1,0,
                       2,0,
                       1,0,
                       0,1,
                       0,2,
                       0,1),ncol=2,byrow=T), #Starting values for entries in Lambda
  params.load=matrix(c('fixed',0,
                       'lambda21',0,
                       'lambda31',0,
                       0,'fixed',
                       0,'lambda52',
                       0,'lambda62'),
                     ncol=2,byrow=T), #Labels for fixed and freed parameters in Lambda
  values.int = rep(0.1,6),
  params.int = paste0('int',1:6), 
  state.names=c("eta1","eta2"), #Labels for latent variables in eta(t)
  obs.names=paste0('V',1:6) #Labels for observed variables in y(t)
)

#Note that in dynr, prep.initial sets the structure of E(eta(1|0)) and Cov(eta(1|0))
#Here we set the initial means to zeros and the initial state covariance matrix
#to a diagonal matrix of 1s - doesn't matter as the transition matrix is a null matrix anyway.

initial <- prep.initial(
  values.inistate=c(0, 0),
  params.inistate=c('fixed', 'fixed'),#initial means fixed to a vector of zeros.
  values.inicov=matrix(c(1,0,0,1),ncol=2),
  params.inicov=matrix(c('fixed','fixed','fixed','fixed'),ncol=2)) #initial covariance fixed to a 

#Process and measurement noise covariance matrices
mdcov <- prep.noise(
  values.latent=matrix(c(2,.5,
                         .5,6),ncol=2,byrow=T), 
  params.latent=matrix(c('psi_11','psi_12',
                         'psi_12','psi_22'),ncol=2,byrow=T), 
  values.observed=diag(rep(.5,6),6), 
  params.observed=diag(paste0('var_e',1:6),6)
)

model <- dynr.model(dynamics=dynamics, measurement=meas,
                    noise=mdcov, initial=initial, data=data2,
                    outfile="PFA.c")

res <- dynr.cook(model)
summary(res)