#===================================================================================
#R version 3.5.1 (2018-07-02) -- "Feather Spray"
#Copyright (C) 2018 The R Foundation for Statistical Computing
#Platform: x86_64-w64-mingw32/x64 (64-bit)
#===================================================================================
# example of package 'BoostedHP'
#===================================================================================
# Date: 2019-07-23
#===================================================================================
# By Chen Yang: chen_yang@link.cuhk.edu.hk
#===================================================================================

# install package 'BoostedHp'

devtools::install_github("chenyang45/BoostedHP/BoostedHP")

# or

devtools::install_github("chenyang45/BoostedHP/BoostedHP", INSTALL_opts=c("--no-multiarch"))

library(BoostedHP)

# conduct the HP-filter and produce object bHP

?BoostedHP

library(tseries)

lam <- 100 # tuning parameter for the annaul data

data(IRE) # laod the data 'IRE'

# raw HP filter
bx_HP <- BoostedHP(IRE, lambda = lam, iter= FALSE)

# by BIC
bx_BIC <- BoostedHP(IRE, lambda = lam, iter= TRUE, test_type = "BIC")

# by ADF
bx_ADF <- BoostedHP(IRE, lambda = lam, iter= TRUE, test_type = "adf")

# by none test type
# Iterated HP filter until Max_Iter and keep the path of BIC.

bx_none <- BoostedHP(IRE, lambda = lam, iter= TRUE, test_type = "none")

#-------- plot -----------

?plot.bHP

#--------- start to plot the content of bHP -----------------

#--------- for dynamic style (default)--------
plot(bx_ADF)

plot(bx_ADF, iteration_location = "upright") # change the location of text

plot(bx_ADF, iteration_location = c(30,12)) # assign the location of text by x-y co-ordinates

plot(bx_BIC, interval_t = 0.8 ) # change the time interval of animation

plot(bx_none, cex_legend = 2, cex_text = 3) # change the magnification of legend and text

# change the color
plot(bx_ADF,main = "dynamic graph with new color",col_raw = "#685F74", col_trend_h = "#39A1A8", col_trend_f = "#DD4B4F", col_pvalue_BIC = "#E96145")

plot(bx_ADF,main = "dynamic graph with new trancparency setting",raw_alpha = 200, trend_h_alpha = 55, trend_f_alpha = 250, pvalue_BIC_alpha = 250)

plot(bx_HP)
# none-iter' bHP doesn't have dynamic picture: returning NA

#--------- for JS style ----------

plot(bx_ADF,plot_type = "JS")

# change the color
plot(bx_ADF,plot_type = "JS",main = "Js graph with new color", col_raw = "#685F74", col_trend_f = "#DD4B4F", col_pvalue_BIC = "#39A1A8")

plot(bx_BIC,plot_type = "JS")

plot(bx_none,plot_type = "JS")

plot(bx_HP,plot_type = "JS")

#--------- for static style ----------

plot(bx_ADF,plot_type = "static",cex_legend = 0.7, cex_text = 0.8 )

plot(bx_HP,plot_type = "static")

plot(bx_BIC,plot_type = "static",cex_legend = 0.7, cex_text = 0.8 )

plot(bx_none,plot_type = "static",cex_legend = 0.8, cex_text = 0.8 )

#----------- print -------------------------------

?print.bHP

#--------- start to print the content of bHP -----------------
print(bx_ADF)

print(bx_ADF, Head = F, Tail = T, trend_hist = F)

print(bx_ADF, Head = T, Tail = T, trend_hist = F)

print(bx_ADF, Head = F, Tail = F, trend_hist = F)

print(bx_BIC, Head = F, Tail = F, trend_hist = T, select_trend_hist = 1:bx_BIC$iter_num)

print(bx_BIC, Head = F, Tail = F, trend_hist = T,  select_trend_hist = c(1,3,5))

# when the trend_hist is FALSE, select_trend_hist is invalid
print(bx_BIC, Head = F, Tail = F, trend_hist = F, select_trend_hist = c(1,3,5))

print(bx_BIC, Head = F, Tail = T, trend_hist = F, print_type = "latex")

print(bx_BIC, Head = F, Tail = T, trend_hist = F, print_type = "html")

# show the generic print function output
print(bx_ADF, type = "generic default")



#------------------ summary -----------------

?summary.bHP

summary(bx_ADF)
summary(bx_BIC)
summary(bx_none)
summary(bx_HP)

#------------------ predict -----------------

?predict.bHP

predict(bx_HP) #Iterated number of HP filter: 1

predict(bx_ADF) #Iterated number of HP filter: 19

predict(bx_BIC) #Iterated number of HP filter: 5

predict(bx_none) #Iterated number of HP filter: 99


#------------------ residuals -----------------

?residuals.bHP

residuals(bx_HP) #Iterated number of HP filter: 1

residuals(bx_ADF) #Iterated number of HP filter: 19

#------------------ BIC -------------------------

?BIC.bHP

BIC(bx_BIC)

#Retrun the value path of BIC.
#Iterated number of HP filter: 5
#Keep the path of BIC till iterated 6 times to show the tuning point.
#[1] 1.586255 1.366335 1.293931 1.264323 1.254397 1.254620

BIC(bx_none)

#Retrun the BIC path of none.
#Iterated number of HP filter: 99
#Keep the path of BIC till iterated 100 times to show the tuning point.
#[1] 1.586255 1.366335 1.293931 1.264323 1.254397 1.254620 1.260345 1.269139 1.279670 1.291179
#[11] 1.303223 ...


### If the test type is not "adf", Pvalue.bHP will return error

# raw HP filter
BIC(bx_HP)

# Error in BIC.bHP(bx_HP) :
# The stationary test type is none-iter, not BIC or none.


# by ADF
BIC(bx_ADF)

#Error in BIC.bHP(bx_ADF) :
#The stationary test type is adf, not BIC or none.


#--------------- Pvalue ---------------------

?Pvalue.bHP

Pvalue(bx_ADF)

# Retrun the value path of adf.
# Iterated number of HP filter: 19
# [1] 0.26932206 0.16154351 0.10943027 0.09301570 0.08624282 0.08172733 0.07880462 0.07692725
# [9] 0.07561611 0.07449014 0.07326910 0.07175650 0.06981805 0.06736339 0.06433257 0.06068690
# [17] 0.05640284 0.05146806 0.04785197


### If the test type is not "adf", Pvalue.bHP will return error

# raw HP filter

Pvalue(bx_HP)

# Error in Pvalue.bHP(bx_HP) :
# The stationary test type is none-iter, not ADF.

# by BIC

Pvalue(bx_BIC)

# Error in Pvalue.bHP(bx_BIC) : The stationary test type is BIC, not ADF.

# by none test type
# Iterated HP filter until Max_Iter and keep the path of BIC.

Pvalue(bx_none)

#Error in Pvalue.bHP(bx_none) : The stationary test type is none, not ADF.

