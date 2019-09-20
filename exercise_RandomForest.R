#Three method (decission three)

#example sulution
library(islr)
library(dplyr)
tb<-merge(ExceptionFinder, welloffsetwell_avg, all = T) %>%arrange(desc(STRING_CODE))%>%
  mutate(residu=LAST_BFPD_AVG_OS-LAST_BOPD)%>%
  select(residu)
View(tb)

delta1<-merge(ExceptionFinder,welloffsetwell_avg, all = F) %>%
  arrange(desc(STRING_CODE)) %>%
  mutate(delt=LAST_BFPD_AVG_OS-LAST_BOPD)%>%
  select(UWI,STRING_CODE,delt)
delta2<-ExceptionFinder%>% filter(UWI=='DURI01211V1')

library(ggplot2)
qplot(BFPD_3MO,BFPD_12MO,data=ExceptionFinder, color=COMPLETION_TYPE)+ggtitle("Perbandingan BFPD 3MO & 12 MO")

ggplot(ExceptionFinder, aes(x = BFPD_3MO, y = BFPD_12MO))+
  geom_point(aes(color = COMPLETION_TYPE))+               
  geom_smooth(aes(color = AREA, fill = AREA))+
  facet_wrap(~AREA, ncol = 7, nrow = 2) + ggtitle("Trend penurunan liquid dari 3MO-6MO")


welloffsetwell_avg2 <- welloffsetwell_Merge %>% group_by(STRING_CODE) %>%
  summarize(
    LAST_BFPD_AVG_OS = round(mean(LAST_BFPD),2),
    LAST_BOPD_AVG_OS = round(mean(LAST_BOPD),2),
    FLUID_CAPACITY_AVG_OS = round(mean(FLUID_CAPACITY),2),
    OIL_CAPACITY_AVG_OS = round(mean(OIL_CAPACITY),2), 
    BFPD_12MO_AVG_OS = round(mean(BFPD_12MO),2), 
    BOPD_12MO_AVG_OS = round(mean(BOPD_12MO),2), 
    BFPD_6MO_AVG_OS = round(mean(BOPD_12MO),2), 
    BOPD_6MO_AVG_OS = round(mean(BOPD_6MO),2), 
    BFPD_3MO_AVG_OS = round(mean(BFPD_3MO),2), 
    BOPD_3MO_AVG_OS = round(mean(BOPD_3MO),2), 
    BFPD_3LAST_AVG_OS = round(mean(BFPD_3LAST),2), 
    BOPD_3LAST_AVG_OS = round(mean(BOPD_3LAST),2))
