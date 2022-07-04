library(xts)
library(tseries)
library(TSA)
library(fBasics)
library(zoo)
library(forecast)
library(FinTS)
library(rugarch)
library(fGarch)

getSymbols.yahoo.fix <- function (symbol,
                                  from       ="2007-01-01",
                                  to         = Sys.Date(),
                                  period     = c("daily","weekly","monthly"),
                                  envir      = globalenv(),
                                  crumb      ="YourCrumb",
                                  DLdir      ="~/Downloads/") { #1
  # build yahoo query
  query1    <- paste("https://query1.finance.yahoo.com/v7/finance/download/",symbol,"?",sep="")
  fromPosix <- as.numeric(as.POSIXlt(from))
  toPosix   <- as.numeric(as.POSIXlt(to))
  query2    <- paste("period1=", fromPosix,"&period2=", toPosix, sep ="")
  interval  <- switch(period[1], daily ="1d", weekly ="1wk", monthly ="1mo")
  query3    <- paste("&interval=", interval,"&events=history&crumb=", crumb, sep ="")
  yahooURL  <- paste(query1, query2, query3, sep ="")
  #' requires browser to be open
  utils::browseURL("https://www.google.com")
  #' run the query - downloads the security as a csv file
  #' DLdir defaults to download directory in browser preferences
  utils::browseURL(yahooURL)
  #' wait 500 msec for download to complete - mileage may vary
  Sys.sleep(time = 0.5)
  yahooCSV  <- paste(DLdir, symbol,".csv", sep ="")
  yahooDF   <- utils::read.csv(yahooCSV, header = TRUE)
  #' -------
  #' if you get: Error in file(file,"rt") : cannot open the connection
  #' it's because the csv file has not completed downloading
  #' try increasing the time for Sys.sleep(time = x)
  #' -------
  #' delete the csv file
  file.remove(yahooCSV)
  # convert date as character to date format
  yahooDF$Date <- as.Date(yahooDF$Date)
  # convert to xts
  yahoo.xts    <- xts(yahooDF[,-1],order.by=yahooDF$Date)
  # assign the xts file to the specified environment
  # default is globalenv()
  assign(symbol, yahoo.xts, envir = as.environment(envir))
  print(symbol)
} #1

getSymbols.yahoo.fix("TSLA")

TSLA = read.csv("TSLA.csv")
TSLA$Date<-as.Date(TSLA$Date)
head(TSLA)
library(ggplot2)
win.graph(width=10,height=4.5,pointsize=8)
TSLA %>%
  ggplot(aes(x = Date, y = Adj.Close)) +
  geom_line() +
  ggtitle("Tesla since 2010") +
  labs(x = "Date", "Price") +
  scale_x_date(date_breaks = "years", date_labels = "%Y") +
  labs(x = "Date", y = "Adjusted Price") +
  theme_bw()

# Calculate daily returns
library(tidyquant)
TSLA_daily_returns <- TSLA %>%
  tq_transmute(select = Adj.Close,           # this specifies which column to select   
               mutate_fun = periodReturn,   # This specifies what to do with that column
               period = "daily",      # This argument calculates Daily returns
               col_rename = "tesla_returns") # renames the column
# We will use a line chart for daily returns

TSLA_daily_returns %>%
  ggplot(aes(x = Date, y = tesla_returns)) +
  geom_line() +
  theme_classic() +
  labs(x = "Date", y = "Daily returns") +
  ggtitle("Daily Returns for TSLA") +
  scale_x_date(date_breaks = "years", date_labels = "%Y") +
  scale_y_continuous(breaks = seq(-0.5,0.6,0.05),
                     labels = scales::percent) 


TSLA_daily_returns %>%
  ggplot(aes(x = Date, y = abs(tesla_returns))) +
  geom_line() +
  theme_classic() +
  labs(x = "Date", y = "Abs Daily returns") +
  ggtitle("Absolute Daily Returns for TSLA") +
  scale_x_date(date_breaks = "years", date_labels = "%Y") +
  scale_y_continuous(breaks = seq(-0.5,0.6,0.05),
                     labels = scales::percent) 

TSLA_daily_returns %>%
  ggplot(aes(x = Date, y = tesla_returns*tesla_returns)) +
  geom_line() +
  theme_classic() +
  labs(x = "Date", y = "Squared Daily returns") +
  ggtitle("Squared Daily Returns for TSLA") +
  scale_x_date(date_breaks = "years", date_labels = "%Y") +
  scale_y_continuous(breaks = seq(-0.5,0.6,0.05),
                     labels = scales::percent)

adf.test(TSLA_daily_returns$tesla_returns)
Box.test(TSLA_daily_returns$tesla_returns,type="Ljung-Box",lag=12)
Box.test(TSLA_daily_returns$tesla_returns,type="Box",lag=12)
Box.test(TSLA_daily_returns$tesla_returns,type="Ljung-Box",lag=36)
Box.test(TSLA_daily_returns$tesla_returns,type="Box",lag=36)


win.graph(width=8,height=4.5,pointsize=8)
acf(TSLA_daily_returns$tesla_returns)
pacf(TSLA_daily_returns$tesla_returns)
acf(abs(TSLA_daily_returns$tesla_returns))
pacf(abs(TSLA_daily_returns$tesla_returns))
acf(TSLA_daily_returns$tesla_returns)
pacf(TSLA_daily_returns$tesla_returns)


t.test(TSLA_daily_returns$tesla_returns)

mean(TSLA_daily_returns$tesla_returns)
dailyreturn<-TSLA_daily_returns$tesla_returns-mean(TSLA_daily_returns$tesla_returns)
Box.test(dailyreturn)
source("archTest.R")  # R script available on the book web site.
archTest(dailyreturn,12) 
##win.graph(width=6,height=6,pointsize=8)
##plot(TSLA_daily_returns$Date,dailyreturn)

win.graph(width=8,height=4.5,pointsize=8)
acf(dailyreturn)
pacf(dailyreturn)



win.graph(width=8,height=4.5,pointsize=8)
smoothScatter(x=TSLA$Date,y=(dailyreturn)^2)
smoothScatter(x=TSLA$Date,y=dailyreturn)
plot(x=TSLA$Date,y=(dailyreturn)^2,type='p')
win.graph(width = 4.875,height = 3,pointsize = 8)
McLeod.Li.test(dailyreturn)
ArchTest(dailyreturn)



ug_spec = ugarchspec()
ug_spec
best_spec = ugarchspec(variance.model=list(model="eGARCH", garchOrder=c(1,1)), 
                       mean.model=list(armaOrder=c(0,0), include.mean=TRUE),  
                       distribution.model="std", fixed.pars=list(omega=0))

bestfit = ugarchfit(spec = best_spec, data = dailyreturn)
bestfit
gBox(bestfit,method = "squared")
best_var <- bestfit@fit$var   # save the estimated conditional variances
best_res2 <- (bestfit@fit$residuals)^2   # save the estimated squared residuals
#Let's plot the squared residuals and the estimated conditional variance:

plot(best_res2, type = "l")
lines(best_var, col = "green")

r<-bestfit@fit$residuals
r<-na.omit(r)
win.graph(width=8,height=4.5,pointsize=8)
plot(r,ylab='Residuals of best fit',type='b',main="Residuals of best fit")
win.graph(width=6,height=4.5,pointsize=8)
qqnorm(r)
qqline(r)
shapiro.test(r)
adf.test(r)

################################################################################
# A Garch Tutorial with R - Perform ARCH test in series of returns
# Paper at <https://rac.anpad.org.br/index.php/rac/article/view/1420>
#
# This script will test for arch effects for a given vector of returns, given
# lags and save results in .html file.

## OPTIONS

max_lag <- 5

## END OPTIONS

library(tidyverse)
library(knitr)
library(kableExtra)
library(writexl)

source('garch_fcts.R')

# do arch test
tab_out <- do_arch_test(x = dailyreturn, max_lag = max_lag)

tab_out

################################################################################

# A Garch Tutorial with R - Finding the best model 
# Paper at <https://rac.anpad.org.br/index.php/rac/article/view/1420>
#
# This script will estimate several garch models and find the best using the BIC
# criteria. A plot with the results, Figure 02 in the paper, is saved in a .png file
# at folder /figs. 

## MAIN OPTIONS

max_lag_AR <- 1 # used 1 in paper
max_lag_MA <- 1 # used 1 in paper
max_lag_ARCH <- 2 # used 2 in paper
max_lag_GARCH <- 1 # used 1 in paper
dist_to_use <- c('norm', 'std') # see rugarch::ugarchspec help for more
models_to_estimate <- c('sGARCH', 'eGARCH', 'gjrGARCH','tGARCH') # see rugarch::rugarchspec help for more

## END OPTIONS

library(tidyverse)
library(purrr)

graphics.off()

source('garch_fcts.R')

out <- find_best_arch_model(x = dailyreturn, 
                            type_models = models_to_estimate,
                            dist_to_use = dist_to_use,
                            max_lag_AR = max_lag_AR,
                            max_lag_MA = max_lag_MA,
                            max_lag_ARCH = max_lag_ARCH,
                            max_lag_GARCH = max_lag_GARCH)

# get table with estimation results
tab_out <- out$tab_out

# pivot table to long format (better for plotting)
df_long <- tidyr::pivot_longer(data = tab_out %>%
                                 select(model_name,
                                        type_model,
                                        type_dist,
                                        AIC, BIC),  cols = c('AIC', 'BIC'))

models_names <- unique(df_long$model_name)
best_models <- c(tab_out$model_name[which.min(tab_out$AIC)],
                 tab_out$model_name[which.min(tab_out$BIC)])

# figure out where is the best model
df_long <- df_long %>%
  mutate(order_model = if_else(model_name %in% best_models, 'Best Model', 'Not Best Model') ) %>%
  na.omit()

# make table with best models
df_best_models <- df_long %>%
  group_by(name) %>%
  summarise(model_name = model_name[which.min(value)],
            value = value[which.min(value)],
            type_model = type_model[which.min(value)])

# plot results
p1 <- ggplot(df_long %>%
               arrange(type_model), 
             aes(x = reorder(model_name, 
                             order(type_model)),
                 y = value, 
                 shape = type_dist,
                 color = type_model)) + 
  geom_point(size = 3.5, alpha = 0.65) + 
  coord_flip() + 
  theme_bw(base_family = "TT Times New Roman") + 
  facet_wrap(~name, scales = 'free_x') + 
  geom_point(data = df_best_models, mapping = aes(x = reorder(model_name, 
                                                              order(type_model)),
                                                  y = value), 
             color = 'blue', size = 5, shape = 8) +
  labs(title = 'Selecting Garch Models by Fitness Criteria', 
       subtitle = 'The best model is the one with lowest AIC or BIC (with star)',
       x = '',
       y = 'Value of Fitness Criteria',
       shape = 'Type of Dist.',
       color = 'Type of Model') + 
  theme(legend.position = "right")

x11()  ; p1 ; ggsave('figs/fig04_best_garch.png')

head(tab_out)

# estimate best garch model by BIC (used in next section)
best_spec = ugarchspec(variance.model = list(model =  out$best_bic$type_model, 
                                             garchOrder = c(out$best_bic$lag_arch,
                                                            out$best_bic$lag_garch)),
                       mean.model = list(armaOrder = c(out$best_bic$lag_ar, 
                                                       out$best_bic$lag_ma)),
                       distribution = 'std')

require(xts) 

time <- TSLA_daily_returns$Date
RV.xts <- na.omit(xts(x = TSLA_daily_returns$tesla_returns-mean(TSLA_daily_returns$tesla_returns),order.by = time))
my_best_garch <- ugarchfit(spec = best_spec, 
                           data = RV.xts,solver = 'hybrid')

my_best_garch
plot(my_best_garch@fit$residuals,type='h',ylab='Standardized Residuals')
v1=volatility(my_best_garch@fit$sigma)  # Obtain volatility
resi=my_best_garch@fit$residuals
vol=ts(v1,frequency=12,start=c(3015,1))
res=ts(resi,frequency=12,start=c(3015,1))
par(mfcol=c(2,1))  # Show volatility and residuals
plot(vol,xlab='year',ylab='volatility',type='l')
plot(res,xlab='year',ylab='st. resi',type='l') 

write_rds(my_best_garch, 'data/garch_model.rds')


failed_spec = ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)), 
                       mean.model=list(armaOrder=c(0,1), include.mean=TRUE),  
                       distribution.model="norm", fixed.pars=list(omega=0))

failedfit = ugarchfit(spec = failed_spec, data = dailyreturn,solver = "hybrid")
failedfit


failed=garchFit(formula=~arma(0,1)+garch(1,1),data=dailyreturn,cond.dist = "norm")
summary(failed)
do_single_garch <- function(x, 
                            type_model, 
                            type_dist, 
                            lag_ar, 
                            lag_ma, 
                            lag_arch, 
                            lag_garch) {
  require(rugarch)
  
  
  spec = ugarchspec(variance.model = list(model =  type_model, 
                                          garchOrder = c(lag_arch, lag_garch)),
                    mean.model = list(armaOrder = c(lag_ar, lag_ma)),
                    distribution = type_dist)
  
  message('Estimating ARMA(',lag_ar, ',', lag_ma,')-',
          type_model, '(', lag_arch, ',', lag_garch, ')', 
          ' dist = ', type_dist,
          appendLF = FALSE)
  
  try({
    my_rugarch <- list()
    my_rugarch <- ugarchfit(spec = spec, data = x,solver = "hybrid")
  })
  
  if (!is.null(coef(my_rugarch))) {
    message('\tDone')
    
    AIC <- rugarch::infocriteria(my_rugarch)[1]
    BIC <- rugarch::infocriteria(my_rugarch)[2]
  } else {
    message('\tEstimation failed..')
    
    AIC <- NA
    BIC <- NA
  }
  
  est_tab <- tibble(lag_ar, 
                    lag_ma,
                    lag_arch,
                    lag_garch,
                    AIC =  AIC,
                    BIC = BIC,
                    type_model = type_model,
                    type_dist,
                    model_name = paste0('ARMA(', lag_ar, ',', lag_ma, ')+',
                                        type_model, '(', lag_arch, ',', lag_garch, ') ',
                                        type_dist) ) 
  
  return(est_tab)
}


win.graph(width = 5,height = 4,pointsize = 8)
r<-my_best_garch@fit$residuals
r<-na.omit(r)
jarque.bera.test(r)
shapiro.test(r)
qqnorm(r)
qqline(r)
adf.test(r)

Box.test(my_best_garch@fit$residuals^2,type="Ljung-Box",lag=12)



m1_spec = ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)), 
                         mean.model=list(armaOrder=c(0,1), include.mean=TRUE),  
                         distribution.model="std", fixed.pars=list(omega=0))

m1fit = ugarchfit(spec = m1_spec, data = dailyreturn,solver = "hybrid")
m1fit
plot(m1fit@fit$residuals,type='h',ylab='Standardized Residuals')
r1<-m1fit@fit$residuals
r1<-na.omit(r1)
jarque.bera.test(r1)
shapiro.test(r1)
qqnorm(r1)
qqline(r1)
adf.test(r1)
rugarch::infocriteria(m1fit)[1]
rugarch::infocriteria(m1fit)[2]

m2_spec = ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)), 
                     mean.model=list(armaOrder=c(1,0), include.mean=TRUE),  
                     distribution.model="std", fixed.pars=list(omega=0))

m2fit = ugarchfit(spec = m2_spec, data = dailyreturn,solver = "hybrid")
m2fit
plot(m2fit@fit$residuals,type='h',ylab='Standardized Residuals')
r2<-m2fit@fit$residuals
r2<-na.omit(r2)
jarque.bera.test(r2)
shapiro.test(r2)
qqnorm(r2)
qqline(r2)
adf.test(r2)
rugarch::infocriteria(m2fit)[1]
rugarch::infocriteria(m2fit)[2]

rugarch::infocriteria(my_best_garch)[1]
rugarch::infocriteria(my_best_garch)[2]

garchroll<-ugarchroll(best_spec, data = RV.xts,n.start =500, 
                      refit.window="moving", refit.every =200)

garchroll
coef(garchroll)[(1)]
coef(garchroll)[(3)]
preds<-as.data.frame(garchroll)

#prediction error
e=(preds$Realized-preds$Mu)/preds$Realized
mean(abs(e))

garchroll<-ugarchroll(m1_spec, data = RV.xts,n.start =500, 
                      refit.window="moving", refit.every =200)

garchroll
preds<-as.data.frame(garchroll)

#prediction error
e=(preds$Realized-preds$Mu)/preds$Realized
mean(abs(e))

garchroll<-ugarchroll(m2_spec, data = RV.xts,n.start =500, 
                      refit.window="moving", refit.every =200)

garchroll
preds<-as.data.frame(garchroll)

#prediction error
e=(preds$Realized-preds$Mu)/preds$Realized
mean(abs(e))


###dailyreturn[1:2580,drop=FALSE]
forecast = ugarchforecast(my_best_garch, n.ahead = 100, n.roll = 0, data = dailyreturn, out.sample = 2579);
sigma(forecast);
fitted(forecast)

plot(forecast)
plot(my_best_garch)

################################################################################
################################################################################
################################################################################

### Outlier Detection in GARCH(1,1) by Doornik & Ooms 2002
#Preparation
T<- 3015 #length()
Ct<- 5.66+1.88*log10(T)
specgarch0 <- ugarchspec()
mod0<- ugarchfit()
lb<-c()
mod0.resSt<-c()
mod0.res.abs<-c()
a<-c()
dt<-matrix()
dt1<-matrix()
specgarch<-ugarchspec()
mod<-ugarchfit()
lm<-c()
C<-c()
mod.resSt<-c()
mod.res.abs<-c()
loc<-matrix()
outliers<-matrix()
critval<-c("FALSE")
no<-c()
k<-c()
outliers<-c()
### Outlier Detection in GARCH(1,1) by Doornik & Ooms 2002
## Step 1
# Estimate baseline GARCH model to obtain log-likelihood and residuals

mod0<- ugarchfit(data=RV.xts, spec=best_spec,solver = 'hybrid')
lb<-likelihood(mod0)

## Step 2
# Find largest absolute standardized residual
mod0.resSt<-residuals(mod0, standardize=TRUE)
mod0.res.abs<-abs(mod0.resSt)
a<-which.max(mod0.res.abs)

# Estimate the extended GARCH model with dummy dt in mean and dt-1 in variance
# Dummies
dt<-matrix(0,T)
dt[a]<-1
dt1<-matrix(0,T)
dt1[a-1]<-1
# Extended GARCH model
# If C < Ct then terminate: no further outliers are present!
while (critval == "FALSE") {
  specgarch <- ugarchspec(variance.model=list(model="eGARCH", garchOrder=c(1,1), external.regressors= dt1), mean.model=list(armaOrder=c(0,1), external.regressors= dt), distribution="std")
  mod<- ugarchfit(data=RV.xts, solver = 'hybrid', spec=specgarch)
  lm<-likelihood(mod)
  C<- 2*(lm-lb)
  mod.resSt<-residuals(mod, standardize=TRUE)
  mod.res.abs<-abs(mod.resSt)
  a<-which.max(mod.res.abs)
  dt[a]<-1
  dt1[a-1]<-1
  critval<- C < Ct}

outliers<-cbind(RV.xts[which(dt==1)])
print(outliers)

dates <- as.Date(c("2012-01-13","2019-10-24"))
RV.xts[dates] <- NA
RV.xts<-na.omit(RV.xts)

my_best_garch_outlier <- ugarchfit(spec = best_spec, data = RV.xts,solver = 'hybrid')

my_best_garch_outlier

rugarch::infocriteria(my_best_garch)[1]
rugarch::infocriteria(my_best_garch)[2]
rugarch::infocriteria(my_best_garch_outlier)[1]
rugarch::infocriteria(my_best_garch_outlier)[2]


#Fit a GARCH-M Model with Normal Distribution
myspec1=ugarchspec(variance.model=list(model="fGARCH",garchOrder=c(1,1),submodel="GARCH"),mean.model=list(armaOrder=c(1,0),include.mean=T,archm=T,archpow=2),distribution.model="norm")
gm_garchm=ugarchfit(spec=myspec1,data=RV.xts,solver="hybrid")
gm_garchm
