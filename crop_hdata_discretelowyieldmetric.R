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


# Hybrid data ---------------------------------------------------


head(pdata)
hdata=pdata
attach(hdata)

agg=aggregate( YIELD ~ HYBRID_ID, hdata, mean)
colnames(agg)[colnames(agg)=='YIELD'] <- 'HYBRID_YIELD_MEAN'

hdata=merge(hdata,agg,by="HYBRID_ID")
tail(hdata)

##add HYBRID_YIELD_STD

agg2=aggregate( YIELD ~ HYBRID_ID, hdata, sd)
colnames(agg2)[colnames(agg2)=='YIELD'] <- 'HYBRID_YIELD_STD'
hdata=merge(hdata,agg2,by="HYBRID_ID")

attach(hdata)


##add a column to indicate if the yield of an hybrid is suspiciously low for the environment it has been planted in

threshold=3 #number of sigmas below the mean
yieldNormalized=(hdata$YIELD-hdata$HYBRID_YIELD_MEAN)/hdata$HYBRID_YIELD_STD
yieldLow=yieldNormalized<=-threshold

hdata=cbind(hdata,data.frame("YIELD_NORMALIZED"=yieldNormalized))
hdata=cbind(hdata,data.frame("YIELD_ISLOW"=yieldLow))
head(hdata)
#pmin(hdata$YIELD_NORMALIZED,hdata$YIELD_NORMALIZED*0)

##ranking of the environments
aggEnv=aggregate( YIELD_ISLOW ~ ENV_ID, hdata, mean)
#[]number of plants par env(/!\plants is different than hybrids because the same hybrid can be planted several times) 
countPlantsInEnv=as.data.frame(table(hdata$ENV_ID))
colnames(countPlantsInEnv)<- c('ENV_ID','ENV_NPLANTS')
aggEnv=merge(aggEnv,countPlantsInEnv,by="ENV_ID")
#[/]
#[]number of hybrids par env(/!\plants is different than hybrids because the same hybrid can be planted several times) 
isDuplicatedHybridInEnvironment=duplicated(pdata[,c("HYBRID_ID","ENV_ID")]) 
oneHybridPerEnvdata=pdata[!isDuplicatedHybridInEnvironment,]
head(oneHybridPerEnvdata)

countHybridsInEnv=as.data.frame(table(oneHybridPerEnvdata$ENV_ID))
colnames(countHybridsInEnv)<- c('ENV_ID','ENV_NHYBRIDS')
aggEnv=merge(aggEnv,countHybridsInEnv,by="ENV_ID")
#[/]
colnames(aggEnv)[colnames(aggEnv)=='YIELD_ISLOW'] <- 'RATIO_LOW_YIELD'
aggEnvOrdered=aggEnv[order(-aggEnv$RATIO_LOW_YIELD),] #100%ratio underperforming plants to 0% underperforming plants
aggEnvOrdered$RANK <- seq.int(nrow(aggEnvOrdered))

threshold
head(aggEnvOrdered)
aggEnvOrdered[201:206,]
tail(aggEnvOrdered)


