# datasets ----------------------------------------------------------------


pdata=read.csv("performance_data.csv")
head(pdata)

wdata=read.csv("weather_data.csv")
head(wdata)


#Notations:
#p=performance
#w=weather
#env=environment
#h=hybrid


# Environment data --------------

isDuplicated=duplicated(pdata["ENV_ID"]) #1 row = 1 env
envdata=pdata[!isDuplicated,]
head(envdata)

envdata=envdata[c("ENV_ID","ENV_YIELD_MEAN","ENV_YIELD_STD","ENV_MG","YEAR","LAT","LONG","IRRIGATION",
                  "ELEVATION","CLAY","SILT","SAND","AWC","PH","OM","CEC","KSAT")]#keep env-related features
head(envdata)
# #create dummy var
# library(psych)
# envdata=cbind(envdata,dummy.code(envdata$IRRIGATION))
# colnames(envdata)[colnames(envdata) == 'NULL'] <- 'UNKWOWN' #with the name "NULL", removing the column doesnt work (reserved notation for NULL)
#envdata <- envdata[, -which(names(envdata) %in% c('IRRIGATIONNONE'))] #
#head(envdata)

attach(envdata)

lm_env = lm(ENV_YIELD_MEAN~. - ENV_ID, data = envdata)
summary(lm_env)
