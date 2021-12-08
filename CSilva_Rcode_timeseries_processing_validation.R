#Cláudio Silva #9/11/2021
#Virtual mobility grant - NDVI time-series dta processing and validation from Mediterranean cork oak woodlands 

#goodness-of-fit; t-test; Granger causality test and example of how to plot data w/ empty data points.


#goodness-of-fit####

##note: 
#all data (columns) must have the same frequencies
#to facilitate operations all data is in one dataframe

#1# %Mean
#RESULTS.ME  = mean(Vobs-Vest)
mean(df1$Vobs, na.rm = T) - colMeans(df1$Vest, na.rm = T)

#suggestion: if you have several "Vest", target the positions in the df. df1[,3:6]



#2# %Mean absolute error
#RESULTS.MAE  = mean(abs(Vobs-Vest));

#two solutions

#1.
o<-df1$Vobs
p<-df1$Vest

mae <- function(o,p,m=T) {
  mean(abs(o-p),na.rm=m)
}
mae(o,p)

#2.
mean(abs(df1$Vobs-df1$Vest), na.rm = TRUE) 



#3# Root mean squared error (RMSE)
#RESULTS.RMSE = sqrt(mean((Vobs-Vest).^2));

sqrt( mean( (df1$Vobs-df1$Vest)^2 ,na.rm = TRUE ) ) 



#4# (%) Relative RMSE
#RESULTS.RELRMSE = (RESULTS.RMSE/mean(Vobs))*100;

#two solutions

#1.
(sqrt( mean( (df1$Vobs-df1$Vest)^2 ,na.rm = TRUE ) )/ (mean(df1$Vobs, na.rm = TRUE)))*100

#2.
((0.08803218)/ (mean(df1$Vobs, na.rm = TRUE)))*100



#5#%Normalized RMSE
#RESULTS.NRMSE=RESULTS.RMSE/range(Vobs);

(sqrt( mean( (df1$Vobs-df1$Vest)^2 ,na.rm = TRUE )))/range(df1$Vobs, na.rm = TRUE)



#6#%Correlation coefficient & Rsquared
#[rr, ~]     = corrcoef(Vobs,Vest);
#RESULTS.R   = rr(1,2);
#RESULTS.R2  = rr(1,2)^2;

#Correlation
cor(df1$Vobs,df1$Ves, use = "complete.obs")

#R
install.packages("rsq")
library(rsq)

x<-df1$Vobs
y1<-df1$Vest

rsq <- function(x, y1) cor(x, y1, use = "complete.obs")
rsq(x, y1)

#R^2
rsq <- function(x, y1) cor(x, y1, use = "complete.obs")^2
rsq(x, y1)




#t-test####
t.test(df1$Vobs, df1$Vest, paired=FALSE)

#note:
#paired=FALSE since Vobs and Vest are independent samples




#Granger-causality test
install.packages("lmtest")
library(lmtest)

grangertest( Vobs ~ Vest , order = 2, data = df1)

#Vest is usefull to forecast Vobs?

#note:
#order = 2. number of lags to use in the first time series (Vobs). Default is 1




#plot time-series w empty data points####

colors <- c("Vobs raw" = "black" , "Vobs linear" = "black", "Vest raw" = "blue", 
            "Vest linear" = "blue", "Vest GPR" = "red", "Vest NEXT" = "green")

ggplot(df1,aes(x=Time)) + 
  geom_line(aes(y=Vobs_raw , color="Vobs raw"), size = .7)  +
  geom_point(aes(y=Vobs_linear , colour="Vobs linear"), size = 3) +
  geom_errorbar(aes(ymin=Vobs_raw-Vobs_sdraw, ymax=Vobs_raw-Vobs_sdraw), width=.2) +
  geom_line(aes(y=Vest_raw , color="Vest raw"))  +
  geom_point(aes(y=Vest_linear , color="Vest linear")) +
  geom_line(aes(y=Vest_GPR , color="Vest GPR"), size=0.9)+
  geom_line(aes(y=Vest_NEXT , color="Vest NEXT"), size=0.9)+
  theme_bw()+
  labs(x="Time",
       y="NDVI",
       color="Legend")+
  scale_y_continuous(limits = c(0.2, 0.8))+
  scale_color_manual(values = colors)

