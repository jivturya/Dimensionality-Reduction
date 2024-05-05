rm(list=ls())
set.seed(100)

#Read Crime data to R
crime_data=read.table("M:/OMSA/ISYE6501/HW5/crime.txt",header=TRUE)
head(crime_data)

#Computing Principal Components
crime_data.pca=prcomp(crime_data[,-16],center=T,scale=T)
summary(crime_data.pca)

#Finding optimal number of N for PCA
#calculate proportional variance
var=crime_data.pca$sdev^2
propvar=var/sum(var)
plot(propvar)

#Using 5 as N for PCA (Choosing firt 5 PC values) we will run a PCA model
#store new dataframe
transformed_crime=cbind(crime_data.pca$x[,1:5],crime_data[,16])
head(transformed_crime)

#build regression model
model=lm(V6~.,data=as.data.frame(transformed_crime))
summary(model)


#Converting PCA model coefficients to original data
beta_0=model$coefficients[1]
sum_betas=model$coefficients[2:6]
sum_betas

   #Find eigen vector of PCA 
crime_data.pca$rotation[,1:5]

alphas=crime_data.pca$rotation[,1:5] %*% sum_betas
t(alphas)

#Converting from scaled to unscaled data coefficients
og_alpha=alphas/sapply(crime_data[,1:15],sd)
og_beta_0=beta_0-sum(alphas*sapply(crime_data[,1:15],mean)/sapply(crime_data[,1:15],sd))
t(og_alpha)
og_beta_0

#Regression model for original coefficients is as follows
predict_model_og=as.matrix(crime_data[,1:15])%*%og_alpha+og_beta_0
predict_model_og

#Calculate R2 and adjusted R2 value to compare if model predicts with same efficiency post conversion
SSE=sum((predict_model_og-crime_data[,16])^2)
SStot=sum((crime_data[,16]-mean(crime_data[,16]))^2)
1-SSE/SStot

R2=1-SSE/SStot
R2-(1-R2)*5/(nrow(crime_data)-5-1)

#using new model to predct last weeks new city data
new_city_data=data.frame(M = 14.00,So = 0,Ed = 10.0,Po1 = 12.0,Po2 = 15.5,LF = 0.640,M.F = 94.0,Pop = 150,
                     NW = 1.1,U1 = 0.120,U2 = 3.6,Wealth = 3200,Ineq = 20.1,Prob = 0.04,Time = 39.0)
predicted_model_fit=as.matrix(new_city_data[,1:15])%*%og_alpha+og_beta_0
predicted_model_fit
