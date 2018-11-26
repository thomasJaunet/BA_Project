
pdata3=read.csv("performance_data_final_BEN.csv")


pdata3$GROW_PERIOD=pdata3$HARVEST_DATE_DAY_NO-pdata3$PLANT_DATE_DAY_NO

pdata3=na.omit(pdata3)

isDuplicated=duplicated(pdata3["ENV_ID"])
pdata4=pdata3[!isDuplicated,]
head(pdata4)


summary(lm(YIELD~.-HYBRID_ID-HYBRID_MG-PLANT_DATE-HARVEST_DATE-PLANT_DATE_DAY_NO-HARVEST_DATE_DAY_NO-ENV_ID,pdata4))
library(leaps)  
final.stepwise = regsubsets(STRESS_METRIC~., data = pdata4, nvmax = 19, method = "forward")


summary(lm(YIELD~.-HYBRID_ID-HYBRID_MG-PLANT_DATE-HARVEST_DATE-PLANT_DATE_DAY_NO-HARVEST_DATE_DAY_NO-ENV_ID-ENV_YIELD_MEAN-STRESS_METRIC,pdata4))