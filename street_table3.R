library(sf)
library(openxlsx)
library(tidyverse)
library(data.table)
library(nycflights13)
library(stringr)
library(dplyr)
library(tidyr)
library(MatchIt)
library(cobalt)
library(geosphere)
library(units)
library(lfe)
library(Matrix)
library(car)
library(MASS)
library(fixest)
library(broom)
library(spdep)
library(tmap)
library(ggplot2)
library(psych)
library(corrplot)
library(reshape2)
library(pscl)
library(ngspatial)
library(CARBayes)
library(spatialreg)
library(ggeffects)
library(patchwork)
library(ggtext)
library(grid)
library(cowplot)
library(gridExtra)
library(e1071)
library(tidycensus)
library(splines)
library(lme4)
library(broom.mixed)


blockface_centerlline_prj_sub <- fread("E:/did_process/data/table/blockgroup/blockface_centerline_prj_sub2.csv")
bfc_candidate <- blockface_centerlline_prj_sub%>%
  mutate(
    candidate_treated=ifelse(PlntSeas=='',0,1)
  )

#crime---------------------------------------------------------------------------
crime_data <- fread("E:/data/crime/NYPD_Arrests_Data__Historic__20250315.csv") 
crime_data[, ARREST_DATE := as.Date(ARREST_DATE, format = "%m/%d/%Y")]  

crime06 <- crime_data[format(ARREST_DATE,"%Y")=="2006"]
crime07 <- crime_data[format(ARREST_DATE,"%Y")=="2007"]
crime08 <- crime_data[format(ARREST_DATE,"%Y")=="2008"]
crime09 <- crime_data[format(ARREST_DATE,"%Y")=="2009"]
crime10 <- crime_data[format(ARREST_DATE,"%Y")=="2010"]
crime11 <- crime_data[format(ARREST_DATE,"%Y")=="2011"]
crime12 <- crime_data[format(ARREST_DATE,"%Y")=="2012"]
crime13 <- crime_data[format(ARREST_DATE,"%Y")=="2013"]
crime14 <- crime_data[format(ARREST_DATE,"%Y")=="2014"]
crime15 <- crime_data[format(ARREST_DATE,"%Y")=="2015"]

crime06_clean <- crime06[!apply(is.na(crime06),1,any),]
crime07_clean <- crime07[!apply(is.na(crime07),1,any),]
crime08_clean <- crime08[!apply(is.na(crime08),1,any),]
crime09_clean <- crime09[!apply(is.na(crime09),1,any),]
crime10_clean <- crime10[!apply(is.na(crime10),1,any),]
crime11_clean <- crime11[!apply(is.na(crime11),1,any),]
crime12_clean <- crime12[!apply(is.na(crime12),1,any),]
crime13_clean <- crime13[!apply(is.na(crime13),1,any),]
crime14_clean <- crime14[!apply(is.na(crime14),1,any),]
crime15_clean <- crime15[!apply(is.na(crime15),1,any),]

violent <- c("ROBBERY","ASSAULT 3 & RELATED OFFENSES","FELONY ASSAULT",
             "OFFENSES AGAINST THE PERSON","KIDNAPPING & RELATED OFFENSES",
             "MURDER & NON-NEGL. MANSLAUGHTER","RAPE")

property <- c("BURGLARY","JOSTLING","THEFT-FRAUD","GRAND LARCENY","PETIT LARCENY","CRIMINAL MISCHIEF & RELATED OFFENSES",
              "OTHER OFFENSES RELATED TO THEFT","POSSESSION OF STOLEN PROPERTY","GRAND LARCENY OF MOTOR VEHICLE",
              "POSSESSION OF STOLEN PROPERTY 5","UNAUTHORIZED USE OF A VEHICLE 3 (UUV)","BURGLAR'S TOOLS","ARSON")



#violent-------------------------------------------------------------------------
violent01 <- crime01_clean%>%
  filter(OFNS_DESC %in% violent)
violent02 <- crime02_clean%>%
  filter(OFNS_DESC %in% violent)
violent03 <- crime03_clean%>%
  filter(OFNS_DESC %in% violent)
violent04 <- crime04_clean%>%
  filter(OFNS_DESC %in% violent)
violent05 <- crime05_clean%>%
  filter(OFNS_DESC %in% violent)
violent06 <- crime06_clean%>%
  filter(OFNS_DESC %in% violent)
violent07 <- crime07_clean%>%
  filter(OFNS_DESC %in% violent)
violent08 <- crime08_clean%>%
  filter(OFNS_DESC %in% violent)
violent09 <- crime09_clean%>%
  filter(OFNS_DESC %in% violent)
violent10 <- crime10_clean%>%
  filter(OFNS_DESC %in% violent)
violent11 <- crime11_clean%>%
  filter(OFNS_DESC %in% violent)
violent12 <- crime12_clean%>%
  filter(OFNS_DESC %in% violent)
violent13 <- crime13_clean%>%
  filter(OFNS_DESC %in% violent)
violent14 <- crime14_clean%>%
  filter(OFNS_DESC %in% violent)
violent15 <- crime15_clean%>%
  filter(OFNS_DESC %in% violent)

write.csv(violent01,"E:/did_process/data/table/crime/violent01.csv",row.names = FALSE)
write.csv(violent02,"E:/did_process/data/table/crime/violent02.csv",row.names = FALSE)
write.csv(violent03,"E:/did_process/data/table/crime/violent03.csv",row.names = FALSE)
write.csv(violent04,"E:/did_process/data/table/crime/violent04.csv",row.names = FALSE)
write.csv(violent05,"E:/did_process/data/table/crime/violent05.csv",row.names = FALSE)
write.csv(violent06,"E:/did_process/data/table/crime/violent06.csv",row.names = FALSE)
write.csv(violent07,"E:/did_process/data/table/crime/violent07.csv",row.names = FALSE)
write.csv(violent08,"E:/did_process/data/table/crime/violent08.csv",row.names = FALSE)
write.csv(violent09,"E:/did_process/data/table/crime/violent09.csv",row.names = FALSE)
write.csv(violent10,"E:/did_process/data/table/crime/violent10.csv",row.names = FALSE)
write.csv(violent11,"E:/did_process/data/table/crime/violent11.csv",row.names = FALSE)
write.csv(violent12,"E:/did_process/data/table/crime/violent12.csv",row.names = FALSE)
write.csv(violent13,"E:/did_process/data/table/crime/violent13.csv",row.names = FALSE)
write.csv(violent14,"E:/did_process/data/table/crime/violent14.csv",row.names = FALSE)
write.csv(violent15,"E:/did_process/data/table/crime/violent15.csv",row.names = FALSE)
write.csv(violent16,"E:/did_process/data/table/crime/violent16.csv",row.names = FALSE)
write.csv(violent17,"E:/did_process/data/table/crime/violent17.csv",row.names = FALSE)
write.csv(violent18,"E:/did_process/data/table/crime/violent18.csv",row.names = FALSE)
write.csv(violent19,"E:/did_process/data/table/crime/violent19.csv",row.names = FALSE)
write.csv(violent20,"E:/did_process/data/table/crime/violent20.csv",row.names = FALSE)
write.csv(violent21,"E:/did_process/data/table/crime/violent21.csv",row.names = FALSE)

violent06_near <- fread("E:/did_process/data/table/crime/violent06_near.csv")
violent07_near <- fread("E:/did_process/data/table/crime/violent07_near.csv")
violent08_near <- fread("E:/did_process/data/table/crime/violent08_near.csv")
violent09_near <- fread("E:/did_process/data/table/crime/violent09_near.csv")
violent10_near <- fread("E:/did_process/data/table/crime/violent10_near.csv")
violent11_near <- fread("E:/did_process/data/table/crime/violent11_near.csv")
violent12_near <- fread("E:/did_process/data/table/crime/violent12_near.csv")
violent13_near <- fread("E:/did_process/data/table/crime/violent13_near.csv")
violent14_near <- fread("E:/did_process/data/table/crime/violent14_near.csv")
violent15_near <- fread("E:/did_process/data/table/crime/violent15_near.csv")
violent16_near <- fread("E:/did_process/data/table/crime/violent16_near.csv")
violent17_near <- fread("E:/did_process/data/table/crime/violent17_near.csv")
violent18_near <- fread("E:/did_process/data/table/crime/violent18_near.csv")
violent19_near <- fread("E:/did_process/data/table/crime/violent19_near.csv")
violent20_near <- fread("E:/did_process/data/table/crime/violent20_near.csv")
violent21_near <- fread("E:/did_process/data/table/crime/violent21_near.csv")

bfcandi_vio06 <- bfc_candidate%>%
  left_join(dplyr::select(violent06_near,OBJECTID,NEAR_FID),by=c("OBJECTID_1"="NEAR_FID"))
bfcandi_vio07 <- bfc_candidate%>%
  left_join(dplyr::select(violent07_near,OBJECTID,NEAR_FID),by=c("OBJECTID_1"="NEAR_FID"))
bfcandi_vio08 <- bfc_candidate%>%
  left_join(dplyr::select(violent08_near,OBJECTID,NEAR_FID),by=c("OBJECTID_1"="NEAR_FID"))
bfcandi_vio09 <- bfc_candidate%>%
  left_join(dplyr::select(violent09_near,OBJECTID,NEAR_FID),by=c("OBJECTID_1"="NEAR_FID"))
bfcandi_vio10 <- bfc_candidate%>%
  left_join(dplyr::select(violent10_near,OBJECTID,NEAR_FID),by=c("OBJECTID_1"="NEAR_FID"))
bfcandi_vio11 <- bfc_candidate%>%
  left_join(dplyr::select(violent11_near,OBJECTID,NEAR_FID),by=c("OBJECTID_1"="NEAR_FID"))
bfcandi_vio12 <- bfc_candidate%>%
  left_join(dplyr::select(violent12_near,OBJECTID,NEAR_FID),by=c("OBJECTID_1"="NEAR_FID"))
bfcandi_vio13 <- bfc_candidate%>%
  left_join(dplyr::select(violent13_near,OBJECTID,NEAR_FID),by=c("OBJECTID_1"="NEAR_FID"))
bfcandi_vio14 <- bfc_candidate%>%
  left_join(dplyr::select(violent14_near,OBJECTID,NEAR_FID),by=c("OBJECTID_1"="NEAR_FID"))
bfcandi_vio15 <- bfc_candidate%>%
  left_join(dplyr::select(violent15_near,OBJECTID,NEAR_FID),by=c("OBJECTID_1"="NEAR_FID"))
bfcandi_vio16 <- bfc_candidate%>%
  left_join(dplyr::select(violent16_near,OBJECTID,NEAR_FID),by=c("OBJECTID_1"="NEAR_FID"))
bfcandi_vio17 <- bfc_candidate%>%
  left_join(dplyr::select(violent17_near,OBJECTID,NEAR_FID),by=c("OBJECTID_1"="NEAR_FID"))
bfcandi_vio18 <- bfc_candidate%>%
  left_join(dplyr::select(violent18_near,OBJECTID,NEAR_FID),by=c("OBJECTID_1"="NEAR_FID"))
bfcandi_vio19 <- bfc_candidate%>%
  left_join(dplyr::select(violent19_near,OBJECTID,NEAR_FID),by=c("OBJECTID_1"="NEAR_FID"))
bfcandi_vio20 <- bfc_candidate%>%
  left_join(dplyr::select(violent20_near,OBJECTID,NEAR_FID),by=c("OBJECTID_1"="NEAR_FID"))
bfcandi_vio21 <- bfc_candidate%>%
  left_join(dplyr::select(violent21_near,OBJECTID,NEAR_FID),by=c("OBJECTID_1"="NEAR_FID"))

#06
bfcandi_vio06count <- bfcandi_vio06%>%
  group_by(OBJECTID_1)%>%
  mutate(
    violent_count06=n()
  )
bfcandi_vio06count$violent_count06 <- ifelse(is.na(bfcandi_vio06count$OBJECTID),0,bfcandi_vio06count$violent_count06)
bfcandi_vio06count <- bfcandi_vio06count%>%
  group_by(OBJECTID_1)%>%
  summarise(
    violent_count06=first(violent_count06)
  )

#07
bfcandi_vio07count <- bfcandi_vio07%>%
  group_by(OBJECTID_1)%>%
  mutate(
    violent_count07=n()
  )
bfcandi_vio07count$violent_count07 <- ifelse(is.na(bfcandi_vio07count$OBJECTID),0,bfcandi_vio07count$violent_count07)
bfcandi_vio07count <- bfcandi_vio07count%>%
  group_by(OBJECTID_1)%>%
  summarise(
    violent_count07=first(violent_count07)
  )

#08
bfcandi_vio08count <- bfcandi_vio08%>%
  group_by(OBJECTID_1)%>%
  mutate(
    violent_count08=n()
  )
bfcandi_vio08count$violent_count08 <- ifelse(is.na(bfcandi_vio08count$OBJECTID),0,bfcandi_vio08count$violent_count08)
bfcandi_vio08count <- bfcandi_vio08count%>%
  group_by(OBJECTID_1)%>%
  summarise(
    violent_count08=first(violent_count08)
  )

#09
bfcandi_vio09count <- bfcandi_vio09%>%
  group_by(OBJECTID_1)%>%
  mutate(
    violent_count09=n()
  )
bfcandi_vio09count$violent_count09 <- ifelse(is.na(bfcandi_vio09count$OBJECTID),0,bfcandi_vio09count$violent_count09)
bfcandi_vio09count <- bfcandi_vio09count%>%
  group_by(OBJECTID_1)%>%
  summarise(
    violent_count09=first(violent_count09)
  )

#10
bfcandi_vio10count <- bfcandi_vio10%>%
  group_by(OBJECTID_1)%>%
  mutate(
    violent_count10=n()
  )
bfcandi_vio10count$violent_count10 <- ifelse(is.na(bfcandi_vio10count$OBJECTID),0,bfcandi_vio10count$violent_count10)
bfcandi_vio10count <- bfcandi_vio10count%>%
  group_by(OBJECTID_1)%>%
  summarise(
    violent_count10=first(violent_count10)
  )

#11
bfcandi_vio11count <- bfcandi_vio11%>%
  group_by(OBJECTID_1)%>%
  mutate(
    violent_count11=n()
  )
bfcandi_vio11count$violent_count11 <- ifelse(is.na(bfcandi_vio11count$OBJECTID),0,bfcandi_vio11count$violent_count11)
bfcandi_vio11count <- bfcandi_vio11count%>%
  group_by(OBJECTID_1)%>%
  summarise(
    violent_count11=first(violent_count11)
  )

#12
bfcandi_vio12count <- bfcandi_vio12%>%
  group_by(OBJECTID_1)%>%
  mutate(
    violent_count12=n()
  )
bfcandi_vio12count$violent_count12 <- ifelse(is.na(bfcandi_vio12count$OBJECTID),0,bfcandi_vio12count$violent_count12)
bfcandi_vio12count <- bfcandi_vio12count%>%
  group_by(OBJECTID_1)%>%
  summarise(
    violent_count12=first(violent_count12)
  )

#13
bfcandi_vio13count <- bfcandi_vio13%>%
  group_by(OBJECTID_1)%>%
  mutate(
    violent_count13=n()
  )
bfcandi_vio13count$violent_count13 <- ifelse(is.na(bfcandi_vio13count$OBJECTID),0,bfcandi_vio13count$violent_count13)
bfcandi_vio13count <- bfcandi_vio13count%>%
  group_by(OBJECTID_1)%>%
  summarise(
    violent_count13=first(violent_count13)
  )

#14
bfcandi_vio14count <- bfcandi_vio14%>%
  group_by(OBJECTID_1)%>%
  mutate(
    violent_count14=n()
  )
bfcandi_vio14count$violent_count14 <- ifelse(is.na(bfcandi_vio14count$OBJECTID),0,bfcandi_vio14count$violent_count14)
bfcandi_vio14count <- bfcandi_vio14count%>%
  group_by(OBJECTID_1)%>%
  summarise(
    violent_count14=first(violent_count14)
  )

#15
bfcandi_vio15count <- bfcandi_vio15%>%
  group_by(OBJECTID_1)%>%
  mutate(
    violent_count15=n()
  )
bfcandi_vio15count$violent_count15 <- ifelse(is.na(bfcandi_vio15count$OBJECTID),0,bfcandi_vio15count$violent_count15)
bfcandi_vio15count <- bfcandi_vio15count%>%
  group_by(OBJECTID_1)%>%
  summarise(
    violent_count15=first(violent_count15)
  )

#16
bfcandi_vio16count <- bfcandi_vio16%>%
  group_by(OBJECTID_1)%>%
  mutate(
    violent_count16=n()
  )
bfcandi_vio16count$violent_count16 <- ifelse(is.na(bfcandi_vio16count$OBJECTID),0,bfcandi_vio16count$violent_count16)
bfcandi_vio16count <- bfcandi_vio16count%>%
  group_by(OBJECTID_1)%>%
  summarise(
    violent_count16=first(violent_count16)
  )

#17
bfcandi_vio17count <- bfcandi_vio17%>%
  group_by(OBJECTID_1)%>%
  mutate(
    violent_count17=n()
  )
bfcandi_vio17count$violent_count17 <- ifelse(is.na(bfcandi_vio17count$OBJECTID),0,bfcandi_vio17count$violent_count17)
bfcandi_vio17count <- bfcandi_vio17count%>%
  group_by(OBJECTID_1)%>%
  summarise(
    violent_count17=first(violent_count17)
  )

#18
bfcandi_vio18count <- bfcandi_vio18%>%
  group_by(OBJECTID_1)%>%
  mutate(
    violent_count18=n()
  )
bfcandi_vio18count$violent_count18 <- ifelse(is.na(bfcandi_vio18count$OBJECTID),0,bfcandi_vio18count$violent_count18)
bfcandi_vio18count <- bfcandi_vio18count%>%
  group_by(OBJECTID_1)%>%
  summarise(
    violent_count18=first(violent_count18)
  )

#19
bfcandi_vio19count <- bfcandi_vio19%>%
  group_by(OBJECTID_1)%>%
  mutate(
    violent_count19=n()
  )
bfcandi_vio19count$violent_count19 <- ifelse(is.na(bfcandi_vio19count$OBJECTID),0,bfcandi_vio19count$violent_count19)
bfcandi_vio19count <- bfcandi_vio19count%>%
  group_by(OBJECTID_1)%>%
  summarise(
    violent_count19=first(violent_count19)
  )

#20
bfcandi_vio20count <- bfcandi_vio20%>%
  group_by(OBJECTID_1)%>%
  mutate(
    violent_count20=n()
  )
bfcandi_vio20count$violent_count20 <- ifelse(is.na(bfcandi_vio20count$OBJECTID),0,bfcandi_vio20count$violent_count20)
bfcandi_vio20count <- bfcandi_vio20count%>%
  group_by(OBJECTID_1)%>%
  summarise(
    violent_count20=first(violent_count20)
  )

#21
bfcandi_vio21count <- bfcandi_vio21%>%
  group_by(OBJECTID_1)%>%
  mutate(
    violent_count21=n()
  )
bfcandi_vio21count$violent_count21 <- ifelse(is.na(bfcandi_vio21count$OBJECTID),0,bfcandi_vio21count$violent_count21)
bfcandi_vio21count <- bfcandi_vio21count%>%
  group_by(OBJECTID_1)%>%
  summarise(
    violent_count21=first(violent_count21)
  )

bfc_candidate <- bfc_candidate%>%
  left_join(bfcandi_vio06count,by="OBJECTID_1")
bfc_candidate <- bfc_candidate%>%
  left_join(bfcandi_vio07count,by="OBJECTID_1")
bfc_candidate <- bfc_candidate%>%
  left_join(bfcandi_vio08count,by="OBJECTID_1")
bfc_candidate <- bfc_candidate%>%
  left_join(bfcandi_vio09count,by="OBJECTID_1")
bfc_candidate <- bfc_candidate%>%
  left_join(bfcandi_vio10count,by="OBJECTID_1")
bfc_candidate <- bfc_candidate%>%
  left_join(bfcandi_vio11count,by="OBJECTID_1")
bfc_candidate <- bfc_candidate%>%
  left_join(bfcandi_vio12count,by="OBJECTID_1")
bfc_candidate <- bfc_candidate%>%
  left_join(bfcandi_vio13count,by="OBJECTID_1")
bfc_candidate <- bfc_candidate%>%
  left_join(bfcandi_vio14count,by="OBJECTID_1")
bfc_candidate <- bfc_candidate%>%
  left_join(bfcandi_vio15count,by="OBJECTID_1")
bfc_candidate <- bfc_candidate%>%
  left_join(bfcandi_vio16count,by="OBJECTID_1")
bfc_candidate <- bfc_candidate%>%
  left_join(bfcandi_vio17count,by="OBJECTID_1")
bfc_candidate <- bfc_candidate%>%
  left_join(bfcandi_vio18count,by="OBJECTID_1")
bfc_candidate <- bfc_candidate%>%
  left_join(bfcandi_vio19count,by="OBJECTID_1")
bfc_candidate <- bfc_candidate%>%
  left_join(bfcandi_vio20count,by="OBJECTID_1")
bfc_candidate <- bfc_candidate%>%
  left_join(bfcandi_vio21count,by="OBJECTID_1")

#property------------------------------------------------------------------------
property06 <- crime06_clean%>%
  filter(OFNS_DESC %in% property)
property07 <- crime07_clean%>%
  filter(OFNS_DESC %in% property)
property08 <- crime08_clean%>%
  filter(OFNS_DESC %in% property)
property09 <- crime09_clean%>%
  filter(OFNS_DESC %in% property)
property10 <- crime10_clean%>%
  filter(OFNS_DESC %in% property)
property11 <- crime11_clean%>%
  filter(OFNS_DESC %in% property)
property12 <- crime12_clean%>%
  filter(OFNS_DESC %in% property)
property13 <- crime13_clean%>%
  filter(OFNS_DESC %in% property)
property14 <- crime14_clean%>%
  filter(OFNS_DESC %in% property)
property15 <- crime15_clean%>%
  filter(OFNS_DESC %in% property)

write.csv(property06,"E:/did_process/data/table/crime/property06.csv",row.names = FALSE)
write.csv(property07,"E:/did_process/data/table/crime/property07.csv",row.names = FALSE)
write.csv(property08,"E:/did_process/data/table/crime/property08.csv",row.names = FALSE)
write.csv(property09,"E:/did_process/data/table/crime/property09.csv",row.names = FALSE)
write.csv(property10,"E:/did_process/data/table/crime/property10.csv",row.names = FALSE)
write.csv(property11,"E:/did_process/data/table/crime/property11.csv",row.names = FALSE)
write.csv(property12,"E:/did_process/data/table/crime/property12.csv",row.names = FALSE)
write.csv(property13,"E:/did_process/data/table/crime/property13.csv",row.names = FALSE)
write.csv(property14,"E:/did_process/data/table/crime/property14.csv",row.names = FALSE)
write.csv(property15,"E:/did_process/data/table/crime/property15.csv",row.names = FALSE)
write.csv(property16,"E:/did_process/data/table/crime/property16.csv",row.names = FALSE)
write.csv(property17,"E:/did_process/data/table/crime/property17.csv",row.names = FALSE)
write.csv(property18,"E:/did_process/data/table/crime/property18.csv",row.names = FALSE)
write.csv(property19,"E:/did_process/data/table/crime/property19.csv",row.names = FALSE)
write.csv(property20,"E:/did_process/data/table/crime/property20.csv",row.names = FALSE)
write.csv(property21,"E:/did_process/data/table/crime/property21.csv",row.names = FALSE)

property06_near <- fread("E:/did_process/data/table/crime/property06_near.csv")
property07_near <- fread("E:/did_process/data/table/crime/property07_near.csv")
property08_near <- fread("E:/did_process/data/table/crime/property08_near.csv")
property09_near <- fread("E:/did_process/data/table/crime/property09_near.csv")
property10_near <- fread("E:/did_process/data/table/crime/property10_near.csv")
property11_near <- fread("E:/did_process/data/table/crime/property11_near.csv")
property12_near <- fread("E:/did_process/data/table/crime/property12_near.csv")
property13_near <- fread("E:/did_process/data/table/crime/property13_near.csv")
property14_near <- fread("E:/did_process/data/table/crime/property14_near.csv")
property15_near <- fread("E:/did_process/data/table/crime/property15_near.csv")
property16_near <- fread("E:/did_process/data/table/crime/property16_near.csv")
property17_near <- fread("E:/did_process/data/table/crime/property17_near.csv")
property18_near <- fread("E:/did_process/data/table/crime/property18_near.csv")
property19_near <- fread("E:/did_process/data/table/crime/property19_near.csv")
property20_near <- fread("E:/did_process/data/table/crime/property20_near.csv")
property21_near <- fread("E:/did_process/data/table/crime/property21_near.csv")

bfcandi_pro06 <- bfc_candidate%>%
  left_join(dplyr::select(property06_near,OBJECTID,NEAR_FID),by=c("OBJECTID_1"="NEAR_FID"))
bfcandi_pro07 <- bfc_candidate%>%
  left_join(dplyr::select(property07_near,OBJECTID,NEAR_FID),by=c("OBJECTID_1"="NEAR_FID"))
bfcandi_pro08 <- bfc_candidate%>%
  left_join(dplyr::select(property08_near,OBJECTID,NEAR_FID),by=c("OBJECTID_1"="NEAR_FID"))
bfcandi_pro09 <- bfc_candidate%>%
  left_join(dplyr::select(property09_near,OBJECTID,NEAR_FID),by=c("OBJECTID_1"="NEAR_FID"))
bfcandi_pro10 <- bfc_candidate%>%
  left_join(dplyr::select(property10_near,OBJECTID,NEAR_FID),by=c("OBJECTID_1"="NEAR_FID"))
bfcandi_pro11 <- bfc_candidate%>%
  left_join(dplyr::select(property11_near,OBJECTID,NEAR_FID),by=c("OBJECTID_1"="NEAR_FID"))
bfcandi_pro12 <- bfc_candidate%>%
  left_join(dplyr::select(property12_near,OBJECTID,NEAR_FID),by=c("OBJECTID_1"="NEAR_FID"))
bfcandi_pro13 <- bfc_candidate%>%
  left_join(dplyr::select(property13_near,OBJECTID,NEAR_FID),by=c("OBJECTID_1"="NEAR_FID"))
bfcandi_pro14 <- bfc_candidate%>%
  left_join(dplyr::select(property14_near,OBJECTID,NEAR_FID),by=c("OBJECTID_1"="NEAR_FID"))
bfcandi_pro15 <- bfc_candidate%>%
  left_join(dplyr::select(property15_near,OBJECTID,NEAR_FID),by=c("OBJECTID_1"="NEAR_FID"))
bfcandi_pro16 <- bfc_candidate%>%
  left_join(dplyr::select(property16_near,OBJECTID,NEAR_FID),by=c("OBJECTID_1"="NEAR_FID"))
bfcandi_pro17 <- bfc_candidate%>%
  left_join(dplyr::select(property17_near,OBJECTID,NEAR_FID),by=c("OBJECTID_1"="NEAR_FID"))
bfcandi_pro18 <- bfc_candidate%>%
  left_join(dplyr::select(property18_near,OBJECTID,NEAR_FID),by=c("OBJECTID_1"="NEAR_FID"))
bfcandi_pro19 <- bfc_candidate%>%
  left_join(dplyr::select(property19_near,OBJECTID,NEAR_FID),by=c("OBJECTID_1"="NEAR_FID"))
bfcandi_pro20 <- bfc_candidate%>%
  left_join(dplyr::select(property20_near,OBJECTID,NEAR_FID),by=c("OBJECTID_1"="NEAR_FID"))
bfcandi_pro21 <- bfc_candidate%>%
  left_join(dplyr::select(property21_near,OBJECTID,NEAR_FID),by=c("OBJECTID_1"="NEAR_FID"))

#06
bfcandi_pro06count <- bfcandi_pro06%>%
  group_by(OBJECTID_1)%>%
  mutate(
    property_count06=n()
  )
bfcandi_pro06count$property_count06 <- ifelse(is.na(bfcandi_pro06count$OBJECTID),0,bfcandi_pro06count$property_count06)
bfcandi_pro06count <- bfcandi_pro06count%>%
  group_by(OBJECTID_1)%>%
  summarise(
    property_count06=first(property_count06)
  )

#07
bfcandi_pro07count <- bfcandi_pro07%>%
  group_by(OBJECTID_1)%>%
  mutate(
    property_count07=n()
  )
bfcandi_pro07count$property_count07 <- ifelse(is.na(bfcandi_pro07count$OBJECTID),0,bfcandi_pro07count$property_count07)
bfcandi_pro07count <- bfcandi_pro07count%>%
  group_by(OBJECTID_1)%>%
  summarise(
    property_count07=first(property_count07)
  )

#08
bfcandi_pro08count <- bfcandi_pro08%>%
  group_by(OBJECTID_1)%>%
  mutate(
    property_count08=n()
  )
bfcandi_pro08count$property_count08 <- ifelse(is.na(bfcandi_pro08count$OBJECTID),0,bfcandi_pro08count$property_count08)
bfcandi_pro08count <- bfcandi_pro08count%>%
  group_by(OBJECTID_1)%>%
  summarise(
    property_count08=first(property_count08)
  )

#09
bfcandi_pro09count <- bfcandi_pro09%>%
  group_by(OBJECTID_1)%>%
  mutate(
    property_count09=n()
  )
bfcandi_pro09count$property_count09 <- ifelse(is.na(bfcandi_pro09count$OBJECTID),0,bfcandi_pro09count$property_count09)
bfcandi_pro09count <- bfcandi_pro09count%>%
  group_by(OBJECTID_1)%>%
  summarise(
    property_count09=first(property_count09)
  )

#10
bfcandi_pro10count <- bfcandi_pro10%>%
  group_by(OBJECTID_1)%>%
  mutate(
    property_count10=n()
  )
bfcandi_pro10count$property_count10 <- ifelse(is.na(bfcandi_pro10count$OBJECTID),0,bfcandi_pro10count$property_count10)
bfcandi_pro10count <- bfcandi_pro10count%>%
  group_by(OBJECTID_1)%>%
  summarise(
    property_count10=first(property_count10)
  )

#11
bfcandi_pro11count <- bfcandi_pro11%>%
  group_by(OBJECTID_1)%>%
  mutate(
    property_count11=n()
  )
bfcandi_pro11count$property_count11 <- ifelse(is.na(bfcandi_pro11count$OBJECTID),0,bfcandi_pro11count$property_count11)
bfcandi_pro11count <- bfcandi_pro11count%>%
  group_by(OBJECTID_1)%>%
  summarise(
    property_count11=first(property_count11)
  )

#12
bfcandi_pro12count <- bfcandi_pro12%>%
  group_by(OBJECTID_1)%>%
  mutate(
    property_count12=n()
  )
bfcandi_pro12count$property_count12 <- ifelse(is.na(bfcandi_pro12count$OBJECTID),0,bfcandi_pro12count$property_count12)
bfcandi_pro12count <- bfcandi_pro12count%>%
  group_by(OBJECTID_1)%>%
  summarise(
    property_count12=first(property_count12)
  )

#13
bfcandi_pro13count <- bfcandi_pro13%>%
  group_by(OBJECTID_1)%>%
  mutate(
    property_count13=n()
  )
bfcandi_pro13count$property_count13 <- ifelse(is.na(bfcandi_pro13count$OBJECTID),0,bfcandi_pro13count$property_count13)
bfcandi_pro13count <- bfcandi_pro13count%>%
  group_by(OBJECTID_1)%>%
  summarise(
    property_count13=first(property_count13)
  )

#14
bfcandi_pro14count <- bfcandi_pro14%>%
  group_by(OBJECTID_1)%>%
  mutate(
    property_count14=n()
  )
bfcandi_pro14count$property_count14 <- ifelse(is.na(bfcandi_pro14count$OBJECTID),0,bfcandi_pro14count$property_count14)
bfcandi_pro14count <- bfcandi_pro14count%>%
  group_by(OBJECTID_1)%>%
  summarise(
    property_count14=first(property_count14)
  )

#15
bfcandi_pro15count <- bfcandi_pro15%>%
  group_by(OBJECTID_1)%>%
  mutate(
    property_count15=n()
  )
bfcandi_pro15count$property_count15 <- ifelse(is.na(bfcandi_pro15count$OBJECTID),0,bfcandi_pro15count$property_count15)
bfcandi_pro15count <- bfcandi_pro15count%>%
  group_by(OBJECTID_1)%>%
  summarise(
    property_count15=first(property_count15)
  )

#16
bfcandi_pro16count <- bfcandi_pro16%>%
  group_by(OBJECTID_1)%>%
  mutate(
    property_count16=n()
  )
bfcandi_pro16count$property_count16 <- ifelse(is.na(bfcandi_pro16count$OBJECTID),0,bfcandi_pro16count$property_count16)
bfcandi_pro16count <- bfcandi_pro16count%>%
  group_by(OBJECTID_1)%>%
  summarise(
    property_count16=first(property_count16)
  )

#17
bfcandi_pro17count <- bfcandi_pro17%>%
  group_by(OBJECTID_1)%>%
  mutate(
    property_count17=n()
  )
bfcandi_pro17count$property_count17 <- ifelse(is.na(bfcandi_pro17count$OBJECTID),0,bfcandi_pro17count$property_count17)
bfcandi_pro17count <- bfcandi_pro17count%>%
  group_by(OBJECTID_1)%>%
  summarise(
    property_count17=first(property_count17)
  )

#18
bfcandi_pro18count <- bfcandi_pro18%>%
  group_by(OBJECTID_1)%>%
  mutate(
    property_count18=n()
  )
bfcandi_pro18count$property_count18 <- ifelse(is.na(bfcandi_pro18count$OBJECTID),0,bfcandi_pro18count$property_count18)
bfcandi_pro18count <- bfcandi_pro18count%>%
  group_by(OBJECTID_1)%>%
  summarise(
    property_count18=first(property_count18)
  )

#19
bfcandi_pro19count <- bfcandi_pro19%>%
  group_by(OBJECTID_1)%>%
  mutate(
    property_count19=n()
  )
bfcandi_pro19count$property_count19 <- ifelse(is.na(bfcandi_pro19count$OBJECTID),0,bfcandi_pro19count$property_count19)
bfcandi_pro19count <- bfcandi_pro19count%>%
  group_by(OBJECTID_1)%>%
  summarise(
    property_count19=first(property_count19)
  )

#20
bfcandi_pro20count <- bfcandi_pro20%>%
  group_by(OBJECTID_1)%>%
  mutate(
    property_count20=n()
  )
bfcandi_pro20count$property_count20 <- ifelse(is.na(bfcandi_pro20count$OBJECTID),0,bfcandi_pro20count$property_count20)
bfcandi_pro20count <- bfcandi_pro20count%>%
  group_by(OBJECTID_1)%>%
  summarise(
    property_count20=first(property_count20)
  )

#21
bfcandi_pro21count <- bfcandi_pro21%>%
  group_by(OBJECTID_1)%>%
  mutate(
    property_count21=n()
  )
bfcandi_pro21count$property_count21 <- ifelse(is.na(bfcandi_pro21count$OBJECTID),0,bfcandi_pro21count$property_count21)
bfcandi_pro21count <- bfcandi_pro21count%>%
  group_by(OBJECTID_1)%>%
  summarise(
    property_count21=first(property_count21)
  )

bfc_candidate <- bfc_candidate%>%
  left_join(bfcandi_pro06count,by="OBJECTID_1")
bfc_candidate <- bfc_candidate%>%
  left_join(bfcandi_pro07count,by="OBJECTID_1")
bfc_candidate <- bfc_candidate%>%
  left_join(bfcandi_pro08count,by="OBJECTID_1")
bfc_candidate <- bfc_candidate%>%
  left_join(bfcandi_pro09count,by="OBJECTID_1")
bfc_candidate <- bfc_candidate%>%
  left_join(bfcandi_pro10count,by="OBJECTID_1")
bfc_candidate <- bfc_candidate%>%
  left_join(bfcandi_pro11count,by="OBJECTID_1")
bfc_candidate <- bfc_candidate%>%
  left_join(bfcandi_pro12count,by="OBJECTID_1")
bfc_candidate <- bfc_candidate%>%
  left_join(bfcandi_pro13count,by="OBJECTID_1")
bfc_candidate <- bfc_candidate%>%
  left_join(bfcandi_pro14count,by="OBJECTID_1")
bfc_candidate <- bfc_candidate%>%
  left_join(bfcandi_pro15count,by="OBJECTID_1")
bfc_candidate <- bfc_candidate%>%
  left_join(bfcandi_pro16count,by="OBJECTID_1")
bfc_candidate <- bfc_candidate%>%
  left_join(bfcandi_pro17count,by="OBJECTID_1")
bfc_candidate <- bfc_candidate%>%
  left_join(bfcandi_pro18count,by="OBJECTID_1")
bfc_candidate <- bfc_candidate%>%
  left_join(bfcandi_pro19count,by="OBJECTID_1")
bfc_candidate <- bfc_candidate%>%
  left_join(bfcandi_pro20count,by="OBJECTID_1")
bfc_candidate <- bfc_candidate%>%
  left_join(bfcandi_pro21count,by="OBJECTID_1")

#allcrime------------------------------------------------------------------------
allcrime06 <- crime06_clean%>%
  filter(OFNS_DESC %in% property | OFNS_DESC %in% violent)
allcrime07 <- crime07_clean%>%
  filter(OFNS_DESC %in% property | OFNS_DESC %in% violent)
allcrime08 <- crime08_clean%>%
  filter(OFNS_DESC %in% property | OFNS_DESC %in% violent)
allcrime09 <- crime09_clean%>%
  filter(OFNS_DESC %in% property | OFNS_DESC %in% violent)
allcrime10 <- crime10_clean%>%
  filter(OFNS_DESC %in% property | OFNS_DESC %in% violent)
allcrime11 <- crime11_clean%>%
  filter(OFNS_DESC %in% property | OFNS_DESC %in% violent)
allcrime12 <- crime12_clean%>%
  filter(OFNS_DESC %in% property | OFNS_DESC %in% violent)
allcrime13 <- crime13_clean%>%
  filter(OFNS_DESC %in% property | OFNS_DESC %in% violent)
allcrime14 <- crime14_clean%>%
  filter(OFNS_DESC %in% property | OFNS_DESC %in% violent)
allcrime15 <- crime15_clean%>%
  filter(OFNS_DESC %in% property | OFNS_DESC %in% violent)

write.csv(allcrime06,"E:/did_process/data/table/crime/allcrime06.csv",row.names = FALSE)
write.csv(allcrime07,"E:/did_process/data/table/crime/allcrime07.csv",row.names = FALSE)
write.csv(allcrime08,"E:/did_process/data/table/crime/allcrime08.csv",row.names = FALSE)
write.csv(allcrime09,"E:/did_process/data/table/crime/allcrime09.csv",row.names = FALSE)
write.csv(allcrime10,"E:/did_process/data/table/crime/allcrime10.csv",row.names = FALSE)
write.csv(allcrime11,"E:/did_process/data/table/crime/allcrime11.csv",row.names = FALSE)
write.csv(allcrime12,"E:/did_process/data/table/crime/allcrime12.csv",row.names = FALSE)
write.csv(allcrime13,"E:/did_process/data/table/crime/allcrime13.csv",row.names = FALSE)
write.csv(allcrime14,"E:/did_process/data/table/crime/allcrime14.csv",row.names = FALSE)
write.csv(allcrime15,"E:/did_process/data/table/crime/allcrime15.csv",row.names = FALSE)
write.csv(allcrime16,"E:/did_process/data/table/crime/allcrime16.csv",row.names = FALSE)
write.csv(allcrime17,"E:/did_process/data/table/crime/allcrime17.csv",row.names = FALSE)
write.csv(allcrime18,"E:/did_process/data/table/crime/allcrime18.csv",row.names = FALSE)
write.csv(allcrime19,"E:/did_process/data/table/crime/allcrime19.csv",row.names = FALSE)
write.csv(allcrime20,"E:/did_process/data/table/crime/allcrime20.csv",row.names = FALSE)
write.csv(allcrime21,"E:/did_process/data/table/crime/allcrime21.csv",row.names = FALSE)

allcrime06_near <- fread("E:/did_process/data/table/crime/allcrime06_near.csv")
allcrime07_near <- fread("E:/did_process/data/table/crime/allcrime07_near.csv")
allcrime08_near <- fread("E:/did_process/data/table/crime/allcrime08_near.csv")
allcrime09_near <- fread("E:/did_process/data/table/crime/allcrime09_near.csv")
allcrime10_near <- fread("E:/did_process/data/table/crime/allcrime10_near.csv")
allcrime11_near <- fread("E:/did_process/data/table/crime/allcrime11_near.csv")
allcrime12_near <- fread("E:/did_process/data/table/crime/allcrime12_near.csv")
allcrime13_near <- fread("E:/did_process/data/table/crime/allcrime13_near.csv")
allcrime14_near <- fread("E:/did_process/data/table/crime/allcrime14_near.csv")
allcrime15_near <- fread("E:/did_process/data/table/crime/allcrime15_near.csv")
allcrime16_near <- fread("E:/did_process/data/table/crime/allcrime16_near.csv")
allcrime17_near <- fread("E:/did_process/data/table/crime/allcrime17_near.csv")
allcrime18_near <- fread("E:/did_process/data/table/crime/allcrime18_near.csv")
allcrime19_near <- fread("E:/did_process/data/table/crime/allcrime19_near.csv")
allcrime20_near <- fread("E:/did_process/data/table/crime/allcrime20_near.csv")
allcrime21_near <- fread("E:/did_process/data/table/crime/allcrime21_near.csv")

bfcandi_all06 <- bfc_candidate%>%
  left_join(dplyr::select(allcrime06_near,OBJECTID,NEAR_FID),by=c("OBJECTID_1"="NEAR_FID"))
bfcandi_all07 <- bfc_candidate%>%
  left_join(dplyr::select(allcrime07_near,OBJECTID,NEAR_FID),by=c("OBJECTID_1"="NEAR_FID"))
bfcandi_all08 <- bfc_candidate%>%
  left_join(dplyr::select(allcrime08_near,OBJECTID,NEAR_FID),by=c("OBJECTID_1"="NEAR_FID"))
bfcandi_all09 <- bfc_candidate%>%
  left_join(dplyr::select(allcrime09_near,OBJECTID,NEAR_FID),by=c("OBJECTID_1"="NEAR_FID"))
bfcandi_all10 <- bfc_candidate%>%
  left_join(dplyr::select(allcrime10_near,OBJECTID,NEAR_FID),by=c("OBJECTID_1"="NEAR_FID"))
bfcandi_all11 <- bfc_candidate%>%
  left_join(dplyr::select(allcrime11_near,OBJECTID,NEAR_FID),by=c("OBJECTID_1"="NEAR_FID"))
bfcandi_all12 <- bfc_candidate%>%
  left_join(dplyr::select(allcrime12_near,OBJECTID,NEAR_FID),by=c("OBJECTID_1"="NEAR_FID"))
bfcandi_all13 <- bfc_candidate%>%
  left_join(dplyr::select(allcrime13_near,OBJECTID,NEAR_FID),by=c("OBJECTID_1"="NEAR_FID"))
bfcandi_all14 <- bfc_candidate%>%
  left_join(dplyr::select(allcrime14_near,OBJECTID,NEAR_FID),by=c("OBJECTID_1"="NEAR_FID"))
bfcandi_all15 <- bfc_candidate%>%
  left_join(dplyr::select(allcrime15_near,OBJECTID,NEAR_FID),by=c("OBJECTID_1"="NEAR_FID"))
bfcandi_all16 <- bfc_candidate%>%
  left_join(dplyr::select(allcrime16_near,OBJECTID,NEAR_FID),by=c("OBJECTID_1"="NEAR_FID"))
bfcandi_all17 <- bfc_candidate%>%
  left_join(dplyr::select(allcrime17_near,OBJECTID,NEAR_FID),by=c("OBJECTID_1"="NEAR_FID"))
bfcandi_all18 <- bfc_candidate%>%
  left_join(dplyr::select(allcrime18_near,OBJECTID,NEAR_FID),by=c("OBJECTID_1"="NEAR_FID"))
bfcandi_all19 <- bfc_candidate%>%
  left_join(dplyr::select(allcrime19_near,OBJECTID,NEAR_FID),by=c("OBJECTID_1"="NEAR_FID"))
bfcandi_all20 <- bfc_candidate%>%
  left_join(dplyr::select(allcrime20_near,OBJECTID,NEAR_FID),by=c("OBJECTID_1"="NEAR_FID"))
bfcandi_all21 <- bfc_candidate%>%
  left_join(dplyr::select(allcrime21_near,OBJECTID,NEAR_FID),by=c("OBJECTID_1"="NEAR_FID"))

#06
bfcandi_all06count <- bfcandi_all06%>%
  group_by(OBJECTID_1)%>%
  mutate(
    allcrime_count06=n()
  )
bfcandi_all06count$allcrime_count06 <- ifelse(is.na(bfcandi_all06count$OBJECTID),0,bfcandi_all06count$allcrime_count06)
bfcandi_all06count <- bfcandi_all06count%>%
  group_by(OBJECTID_1)%>%
  summarise(
    allcrime_count06=first(allcrime_count06)
  )

#07
bfcandi_all07count <- bfcandi_all07%>%
  group_by(OBJECTID_1)%>%
  mutate(
    allcrime_count07=n()
  )
bfcandi_all07count$allcrime_count07 <- ifelse(is.na(bfcandi_all07count$OBJECTID),0,bfcandi_all07count$allcrime_count07)
bfcandi_all07count <- bfcandi_all07count%>%
  group_by(OBJECTID_1)%>%
  summarise(
    allcrime_count07=first(allcrime_count07)
  )

#08
bfcandi_all08count <- bfcandi_all08%>%
  group_by(OBJECTID_1)%>%
  mutate(
    allcrime_count08=n()
  )
bfcandi_all08count$allcrime_count08 <- ifelse(is.na(bfcandi_all08count$OBJECTID),0,bfcandi_all08count$allcrime_count08)
bfcandi_all08count <- bfcandi_all08count%>%
  group_by(OBJECTID_1)%>%
  summarise(
    allcrime_count08=first(allcrime_count08)
  )

#09
bfcandi_all09count <- bfcandi_all09%>%
  group_by(OBJECTID_1)%>%
  mutate(
    allcrime_count09=n()
  )
bfcandi_all09count$allcrime_count09 <- ifelse(is.na(bfcandi_all09count$OBJECTID),0,bfcandi_all09count$allcrime_count09)
bfcandi_all09count <- bfcandi_all09count%>%
  group_by(OBJECTID_1)%>%
  summarise(
    allcrime_count09=first(allcrime_count09)
  )

#10
bfcandi_all10count <- bfcandi_all10%>%
  group_by(OBJECTID_1)%>%
  mutate(
    allcrime_count10=n()
  )
bfcandi_all10count$allcrime_count10 <- ifelse(is.na(bfcandi_all10count$OBJECTID),0,bfcandi_all10count$allcrime_count10)
bfcandi_all10count <- bfcandi_all10count%>%
  group_by(OBJECTID_1)%>%
  summarise(
    allcrime_count10=first(allcrime_count10)
  )

#11
bfcandi_all11count <- bfcandi_all11%>%
  group_by(OBJECTID_1)%>%
  mutate(
    allcrime_count11=n()
  )
bfcandi_all11count$allcrime_count11 <- ifelse(is.na(bfcandi_all11count$OBJECTID),0,bfcandi_all11count$allcrime_count11)
bfcandi_all11count <- bfcandi_all11count%>%
  group_by(OBJECTID_1)%>%
  summarise(
    allcrime_count11=first(allcrime_count11)
  )

#12
bfcandi_all12count <- bfcandi_all12%>%
  group_by(OBJECTID_1)%>%
  mutate(
    allcrime_count12=n()
  )
bfcandi_all12count$allcrime_count12 <- ifelse(is.na(bfcandi_all12count$OBJECTID),0,bfcandi_all12count$allcrime_count12)
bfcandi_all12count <- bfcandi_all12count%>%
  group_by(OBJECTID_1)%>%
  summarise(
    allcrime_count12=first(allcrime_count12)
  )

#13
bfcandi_all13count <- bfcandi_all13%>%
  group_by(OBJECTID_1)%>%
  mutate(
    allcrime_count13=n()
  )
bfcandi_all13count$allcrime_count13 <- ifelse(is.na(bfcandi_all13count$OBJECTID),0,bfcandi_all13count$allcrime_count13)
bfcandi_all13count <- bfcandi_all13count%>%
  group_by(OBJECTID_1)%>%
  summarise(
    allcrime_count13=first(allcrime_count13)
  )

#14
bfcandi_all14count <- bfcandi_all14%>%
  group_by(OBJECTID_1)%>%
  mutate(
    allcrime_count14=n()
  )
bfcandi_all14count$allcrime_count14 <- ifelse(is.na(bfcandi_all14count$OBJECTID),0,bfcandi_all14count$allcrime_count14)
bfcandi_all14count <- bfcandi_all14count%>%
  group_by(OBJECTID_1)%>%
  summarise(
    allcrime_count14=first(allcrime_count14)
  )

#15
bfcandi_all15count <- bfcandi_all15%>%
  group_by(OBJECTID_1)%>%
  mutate(
    allcrime_count15=n()
  )
bfcandi_all15count$allcrime_count15 <- ifelse(is.na(bfcandi_all15count$OBJECTID),0,bfcandi_all15count$allcrime_count15)
bfcandi_all15count <- bfcandi_all15count%>%
  group_by(OBJECTID_1)%>%
  summarise(
    allcrime_count15=first(allcrime_count15)
  )

#16
bfcandi_all16count <- bfcandi_all16%>%
  group_by(OBJECTID_1)%>%
  mutate(
    allcrime_count16=n()
  )
bfcandi_all16count$allcrime_count16 <- ifelse(is.na(bfcandi_all16count$OBJECTID),0,bfcandi_all16count$allcrime_count16)
bfcandi_all16count <- bfcandi_all16count%>%
  group_by(OBJECTID_1)%>%
  summarise(
    allcrime_count16=first(allcrime_count16)
  )

#17
bfcandi_all17count <- bfcandi_all17%>%
  group_by(OBJECTID_1)%>%
  mutate(
    allcrime_count17=n()
  )
bfcandi_all17count$allcrime_count17 <- ifelse(is.na(bfcandi_all17count$OBJECTID),0,bfcandi_all17count$allcrime_count17)
bfcandi_all17count <- bfcandi_all17count%>%
  group_by(OBJECTID_1)%>%
  summarise(
    allcrime_count17=first(allcrime_count17)
  )

#18
bfcandi_all18count <- bfcandi_all18%>%
  group_by(OBJECTID_1)%>%
  mutate(
    allcrime_count18=n()
  )
bfcandi_all18count$allcrime_count18 <- ifelse(is.na(bfcandi_all18count$OBJECTID),0,bfcandi_all18count$allcrime_count18)
bfcandi_all18count <- bfcandi_all18count%>%
  group_by(OBJECTID_1)%>%
  summarise(
    allcrime_count18=first(allcrime_count18)
  )

#19
bfcandi_all19count <- bfcandi_all19%>%
  group_by(OBJECTID_1)%>%
  mutate(
    allcrime_count19=n()
  )
bfcandi_all19count$allcrime_count19 <- ifelse(is.na(bfcandi_all19count$OBJECTID),0,bfcandi_all19count$allcrime_count19)
bfcandi_all19count <- bfcandi_all19count%>%
  group_by(OBJECTID_1)%>%
  summarise(
    allcrime_count19=first(allcrime_count19)
  )

#20
bfcandi_all20count <- bfcandi_all20%>%
  group_by(OBJECTID_1)%>%
  mutate(
    allcrime_count20=n()
  )
bfcandi_all20count$allcrime_count20 <- ifelse(is.na(bfcandi_all20count$OBJECTID),0,bfcandi_all20count$allcrime_count20)
bfcandi_all20count <- bfcandi_all20count%>%
  group_by(OBJECTID_1)%>%
  summarise(
    allcrime_count20=first(allcrime_count20)
  )

#21
bfcandi_all21count <- bfcandi_all21%>%
  group_by(OBJECTID_1)%>%
  mutate(
    allcrime_count21=n()
  )
bfcandi_all21count$allcrime_count21 <- ifelse(is.na(bfcandi_all21count$OBJECTID),0,bfcandi_all21count$allcrime_count21)
bfcandi_all21count <- bfcandi_all21count%>%
  group_by(OBJECTID_1)%>%
  summarise(
    allcrime_count21=first(allcrime_count21)
  )

bfc_candidate <- bfc_candidate%>%
  left_join(bfcandi_all06count,by="OBJECTID_1")
bfc_candidate <- bfc_candidate%>%
  left_join(bfcandi_all07count,by="OBJECTID_1")
bfc_candidate <- bfc_candidate%>%
  left_join(bfcandi_all08count,by="OBJECTID_1")
bfc_candidate <- bfc_candidate%>%
  left_join(bfcandi_all09count,by="OBJECTID_1")
bfc_candidate <- bfc_candidate%>%
  left_join(bfcandi_all10count,by="OBJECTID_1")
bfc_candidate <- bfc_candidate%>%
  left_join(bfcandi_all11count,by="OBJECTID_1")
bfc_candidate <- bfc_candidate%>%
  left_join(bfcandi_all12count,by="OBJECTID_1")
bfc_candidate <- bfc_candidate%>%
  left_join(bfcandi_all13count,by="OBJECTID_1")
bfc_candidate <- bfc_candidate%>%
  left_join(bfcandi_all14count,by="OBJECTID_1")
bfc_candidate <- bfc_candidate%>%
  left_join(bfcandi_all15count,by="OBJECTID_1")
bfc_candidate <- bfc_candidate%>%
  left_join(bfcandi_all16count,by="OBJECTID_1")
bfc_candidate <- bfc_candidate%>%
  left_join(bfcandi_all17count,by="OBJECTID_1")
bfc_candidate <- bfc_candidate%>%
  left_join(bfcandi_all18count,by="OBJECTID_1")
bfc_candidate <- bfc_candidate%>%
  left_join(bfcandi_all19count,by="OBJECTID_1")
bfc_candidate <- bfc_candidate%>%
  left_join(bfcandi_all20count,by="OBJECTID_1")
bfc_candidate <- bfc_candidate%>%
  left_join(bfcandi_all21count,by="OBJECTID_1")

#demographic---------------------------------------------------------------------
blockgroup_did <- fread("E:/did_process/data/table/blockgroup/blockgroup.csv")
edu13 <- read.csv("E:/did_process/data/table/demographic/edu/~2013 5 year ACS/ACSDT5Y2013.B15003-Data.csv")
edu18 <- read.csv("E:/did_process/data/table/demographic/edu/~2018/ACSDT5Y2018.B15003-Data.csv")
edu23 <- read.csv("E:/did_process/data/table/demographic/edu/~2023/ACSDT5Y2023.B15003-Data.csv")

emp13 <- read.csv("E:/did_process/data/table/demographic/emp/~2013 5 year ACS/ACSDT5Y2013.B23025-Data.csv")
emp18 <- read.csv("E:/did_process/data/table/demographic/emp/~2018/ACSDT5Y2018.B23025-Data.csv")
emp23 <- read.csv("E:/did_process/data/table/demographic/emp/~2023/ACSDT5Y2023.B23025-Data.csv")

eth13 <- read.csv("E:/did_process/data/table/demographic/eth/~2013/ACSDT5Y2013.B03002-Data.csv")
eth18 <- read.csv("E:/did_process/data/table/demographic/eth/~2018/ACSDT5Y2018.B03002-Data.csv")
eth23 <- read.csv("E:/did_process/data/table/demographic/eth/~2023/ACSDT5Y2023.B03002-Data.csv")

inc13 <- read.csv("E:/did_process/data/table/demographic/inc/~2013 5 years ACS/ACSDT5Y2013.B19013-Data.csv")
inc18 <- read.csv("E:/did_process/data/table/demographic/inc/~2018 5 years ACS/ACSDT5Y2018.B19013-Data.csv")
inc23 <- read.csv("E:/did_process/data/table/demographic/inc/~2023 5 years ACS/ACSDT5Y2023.B19013-Data.csv")

pop13 <- read.csv("E:/did_process/data/table/demographic/pop/~2013/ACSDT5Y2013.B01003-Data.csv")
pop18 <- read.csv("E:/did_process/data/table/demographic/pop/~2018/ACSDT5Y2018.B01003-Data.csv")
pop23 <- read.csv("E:/did_process/data/table/demographic/pop/~2023/ACSDT5Y2023.B01003-Data.csv")

rac13 <- read.csv("E:/did_process/data/table/demographic/rac/~2013/ACSDT5Y2013.B02001-Data.csv")
rac18 <- read.csv("E:/did_process/data/table/demographic/rac/~2018/ACSDT5Y2018.B02001-Data.csv")
rac23 <- read.csv("E:/did_process/data/table/demographic/rac/~2023/ACSDT5Y2023.B02001-Data.csv")

ten13 <- read.csv("E:/did_process/data/table/demographic/ten/~2013 5 years ACS/ACSDT5Y2013.B25003-Data.csv")
ten18 <- read.csv("E:/did_process/data/table/demographic/ten/~2018 5 years ACS/ACSDT5Y2018.B25003-Data.csv")
ten23 <- read.csv("E:/did_process/data/table/demographic/ten/~2023 5 years ACS/ACSDT5Y2023.B25003-Data.csv")




census_api_key("826f5c4cbeea53e491c2ec69e74ef8f5de018148", install = TRUE)
readRenviron("~/.Renviron")


vars1 <- c(
  #population
  population = "P001001",
  
  # race
  race_total = "P003001",
  White = "P003003",
  Black = "P003004",
  AIAN = "P003005",
  Asian = "P003006",
  
  # tenure
  tenure_total = "H004001",
  owner = "H004002",
  
  # vacancy
  occupancy_total = "H003001",
  vacant = "H003003"
)

decennial2000 <- get_decennial(
  geography = "block group",
  variables = vars1,
  state = "NY",
  year = 2000,
  county = c("Bronx", "Kings", "Queens", "New York", "Richmond"),
  sumfile = "sf1",
  geometry = FALSE
)

decennial2010 <- get_decennial(
  geography = "block group",
  variables = vars1,
  state = "NY",
  year = 2010,
  county = c("Bronx", "Kings", "Queens", "New York", "Richmond"),
  sumfile = "sf1",
  geometry = FALSE
)


vars2 <- c(
  #population
  population = "B01003_001",
  
  # race
  race_total = "B02001_001",
  White = "B02001_002",
  Black = "B02001_003",
  AIAN = "B02001_004",
  Asian = "B02001_005",
  
  # tenure
  tenure_total = "B25003_001",
  owner = "B25003_002",
  
  # vacancy
  occupancy_total = "B25002_001",
  vacant = "B25002_003",
  
  #education 
  education_total = "B15003_001",
  education1 = "B15003_017",
  education2 = "B15003_018",
  education3 = "B15003_019",
  education4 = "B15003_020",
  education5 = "B15003_021",
  education6 = "B15003_022",
  education7 = "B15003_023",
  education8 = "B15003_024",
  education9 = "B15003_025",
  
  #median household income 
  income = "B19013_001"
)

ACS_2013 <- get_acs(
  geography = "block group",
  variables = vars2,
  year = 2015,
  survey = "acs5",
  state = "NY",
  county = c("061", "047", "081", "005", "085"),
  geometry = FALSE
)



ACS_2021 <- get_acs(
  geography = "block group",
  variables = vars2,
  year = 2023,
  survey = "acs5",
  state = "NY",
  county = c("061", "047", "081", "005", "085"),
  geometry = FALSE
)


decennial2000_wider <- decennial2000%>%
  pivot_wider(names_from = variable, values_from = value)

decennial2010_wider <- decennial2010%>%
  pivot_wider(names_from = variable, values_from = value)

ACS_2013_wider <- ACS_2013%>%
  dplyr::select(-moe)%>%
  pivot_wider(names_from = variable, values_from = estimate)



ACS_2021_wider <- ACS_2021%>%
  dplyr::select(-moe)%>%
  pivot_wider(names_from = variable, values_from = estimate)


decennial2000_wider <- decennial2000_wider%>%
  mutate(
    pct_White = White / race_total,
    pct_Black = Black / race_total,
    pct_AIAN = AIAN / race_total,
    pct_Asian = Asian / race_total,
    pct_owner = owner / tenure_total,
    pct_vacancy = vacant / occupancy_total
  )

decennial2010_wider <- decennial2010_wider%>%
  mutate(
    pct_White = White / race_total,
    pct_Black = Black / race_total,
    pct_AIAN = AIAN / race_total,
    pct_Asian = Asian / race_total,
    pct_owner = owner / tenure_total,
    pct_vacancy = vacant / occupancy_total
  )

ACS_2013_wider <- ACS_2013_wider%>%
  mutate(
    pct_White = White / race_total,
    pct_Black = Black / race_total,
    pct_AIAN = AIAN / race_total,
    pct_Asian = Asian / race_total,
    pct_owner = owner / tenure_total,
    pct_vacancy = vacant / occupancy_total,
    pct_education = (education1 + education2 + education3 + education4 + education5 + 
      education6 + education7 + education8 + education9) / education_total
  )



ACS_2021_wider <- ACS_2021_wider%>%
  mutate(
    pct_White = White / race_total,
    pct_Black = Black / race_total,
    pct_AIAN = AIAN / race_total,
    pct_Asian = Asian / race_total,
    pct_owner = owner / tenure_total,
    pct_vacancy = vacant / occupancy_total
  )


#Connect all statistical data to BG----------------------------------------------
bg_did_demo <- blockgroup_did%>%
  mutate(GEOID1=as.character(GEOID1))

#2000
bg_did_demo <- bg_did_demo%>%
  left_join(
    dplyr::select(decennial2000_wider, GEOID, population, pct_White, pct_Black, pct_AIAN, 
                  pct_Asian, pct_owner, pct_vacancy),
    by = c("GEOID1" = "GEOID")
  )%>%
  rename(
    pop2000 = population,
    pct_White_2000 = pct_White,
    pct_Black_2000 = pct_Black,
    pct_AIAN_2000 = pct_AIAN,
    pct_Asian_2000 = pct_Asian,
    pct_owner_2000 = pct_owner,
    pct_vacancy_2000 = pct_vacancy
  )

#2010
bg_did_demo <- bg_did_demo%>%
  left_join(
    dplyr::select(decennial2010_wider, GEOID, population, pct_White, pct_Black, pct_AIAN, 
                  pct_Asian, pct_owner, pct_vacancy),
    by = c("GEOID1" = "GEOID")
  )%>%
  rename(
    pop2010 = population,
    pct_White_2010 = pct_White,
    pct_Black_2010 = pct_Black,
    pct_AIAN_2010 = pct_AIAN,
    pct_Asian_2010 = pct_Asian,
    pct_owner_2010 = pct_owner,
    pct_vacancy_2010 = pct_vacancy
  )

#2013
bg_did_demo <- bg_did_demo%>%
  left_join(
    dplyr::select(ACS_2013_wider, GEOID, population, pct_White, pct_Black, pct_AIAN, 
                  pct_Asian, pct_owner, pct_vacancy, pct_education, income),
    by = c("GEOID1" = "GEOID")
  )%>%
  rename(
    pop2013 = population,
    pct_White_2013 = pct_White,
    pct_Black_2013 = pct_Black,
    pct_AIAN_2013 = pct_AIAN,
    pct_Asian_2013 = pct_Asian,
    pct_owner_2013 = pct_owner,
    pct_vacancy_2013 = pct_vacancy,
    pct_education_2013 = pct_education,  
    income2013 = income                  
  )

#2021
bg_did_demo <- bg_did_demo%>%
  left_join(
    dplyr::select(ACS_2021_wider, GEOID, population, pct_White, pct_Black, pct_AIAN, 
                  pct_Asian, pct_owner, pct_vacancy),
    by = c("GEOID1" = "GEOID")
  )%>%
  rename(
    pop2021 = population,
    pct_White_2021 = pct_White,
    pct_Black_2021 = pct_Black,
    pct_AIAN_2021 = pct_AIAN,
    pct_Asian_2021 = pct_Asian,
    pct_owner_2021 = pct_owner,
    pct_vacancy_2021 = pct_vacancy
  )

#Clean up block group data
bg_did_demo_sub <- bg_did_demo[!apply(is.na(bg_did_demo),1,any),]

write.csv(bg_did_demo,"E:/did_process/data/table/blockgroup/bg_did_demo.csv",row.names = FALSE)
write.csv(bg_did_demo_sub,"E:/did_process/data/table/blockgroup/bg_did_demo_sub.csv",row.names = FALSE)


#linear interpolation
#Convert to long data first
bdds_longer <- bg_did_demo_sub%>%
  pivot_longer(
    cols = c("pop2000", "pop2010", "pop2013", "pop2021"),
    names_to = "pop_years",
    values_to = "population"
  )%>%
  pivot_longer(
    cols = c("pct_White_2000", "pct_White_2010", "pct_White_2013", "pct_White_2021"),
    names_to = "White_years",
    values_to = "White"
  )%>%
  pivot_longer(
    cols = c("pct_Black_2000", "pct_Black_2010", "pct_Black_2013", "pct_Black_2021"),
    names_to = "Black_years",
    values_to = "Black"
  )%>%
  pivot_longer(
    cols = c("pct_AIAN_2000", "pct_AIAN_2010", "pct_AIAN_2013", "pct_AIAN_2021"),
    names_to = "AIAN_years",
    values_to = "AIAN"
  )%>%
  pivot_longer(
    cols = c("pct_Asian_2000", "pct_Asian_2010", "pct_Asian_2013", "pct_Asian_2021"),
    names_to = "Asian_years",
    values_to = "Asian"
  )%>%
  pivot_longer(
    cols = c("pct_owner_2000", "pct_owner_2010", "pct_owner_2013", "pct_owner_2021"),
    names_to = "owner_years",
    values_to = "owner"
  )%>%
  pivot_longer(
    cols = c("pct_vacancy_2000", "pct_vacancy_2010", "pct_vacancy_2013", "pct_vacancy_2021"),
    names_to = "vacancy_years",
    values_to = "vacancy"
  )%>%
  filter(
    (pop_years == "pop2000" & White_years == "pct_White_2000" & 
      Black_years == "pct_Black_2000" & AIAN_years == "pct_AIAN_2000" & 
      Asian_years == "pct_Asian_2000" &  owner_years == "pct_owner_2000" & 
      vacancy_years == "pct_vacancy_2000") |
      (pop_years == "pop2010" & White_years == "pct_White_2010" & 
         Black_years == "pct_Black_2010" & AIAN_years == "pct_AIAN_2010" & 
         Asian_years == "pct_Asian_2010" &  owner_years == "pct_owner_2010" & 
         vacancy_years == "pct_vacancy_2010") |
      (pop_years == "pop2013" & White_years == "pct_White_2013" & 
         Black_years == "pct_Black_2013" & AIAN_years == "pct_AIAN_2013" & 
         Asian_years == "pct_Asian_2013" &  owner_years == "pct_owner_2013" & 
         vacancy_years == "pct_vacancy_2013") |
      (pop_years == "pop2021" & White_years == "pct_White_2021" & 
         Black_years == "pct_Black_2021" & AIAN_years == "pct_AIAN_2021" & 
         Asian_years == "pct_Asian_2021" &  owner_years == "pct_owner_2021" & 
         vacancy_years == "pct_vacancy_2021")
  )%>%
  dplyr::select(
    -White_years, -Black_years, -AIAN_years, -Asian_years, -owner_years, -vacancy_years
  )%>%
  rename(years = pop_years)%>%
  mutate(
    years = case_when(
      years == "pop2000" ~ "2000",
      years == "pop2010" ~ "2010",
      years == "pop2013" ~ "2013",
      years == "pop2021" ~ "2021"
    )
  )

#Linear interpolation

bdds_longer_interp <- bdds_longer%>%
  group_by(OBJECTID_1) %>%
  do({
    d <- .
    full_years <- data.frame(years = 2000:2021)
    
    merged <- merge(full_years, d, by = "years", all.x = TRUE)
    
    merged$population <- approx(d$years, d$population, xout = merged$years)$y
    merged$White      <- approx(d$years, d$White, xout = merged$years)$y
    merged$Black      <- approx(d$years, d$Black, xout = merged$years)$y
    merged$AIAN       <- approx(d$years, d$AIAN, xout = merged$years)$y
    merged$Asian      <- approx(d$years, d$Asian, xout = merged$years)$y
    merged$owner      <- approx(d$years, d$owner, xout = merged$years)$y
    merged$vacancy    <- approx(d$years, d$vacancy, xout = merged$years)$y
    
    merged
  }) %>%
  ungroup()

bdds_longer_interp <- bdds_longer_interp %>%
  fill(OBJECTID_1, .direction = "downup")

#Convert the interpolated table to wide data
bddsli_wider <- bdds_longer_interp %>%
  pivot_wider(
    id_cols = OBJECTID_1,
    names_from = years,
    values_from = c(population, White, Black, AIAN, Asian, owner, vacancy, pct_education_2013, income2013),
    names_glue = "{.value}_{years}"
  )%>%
  dplyr::select(
    -pct_education_2013_2001, -pct_education_2013_2002, -pct_education_2013_2003, 
    -pct_education_2013_2004, -pct_education_2013_2005, -pct_education_2013_2006, 
    -pct_education_2013_2007, -pct_education_2013_2008, -pct_education_2013_2009, 
    -pct_education_2013_2010, -pct_education_2013_2011, -pct_education_2013_2012, 
    -pct_education_2013_2013, -pct_education_2013_2014, -pct_education_2013_2015, 
    -pct_education_2013_2016, -pct_education_2013_2017, -pct_education_2013_2018, 
    -pct_education_2013_2019, -pct_education_2013_2020, -pct_education_2013_2021, 
    -income2013_2001, -income2013_2002, -income2013_2003, -income2013_2004, -income2013_2005, 
    -income2013_2006, -income2013_2007, -income2013_2008, -income2013_2009, -income2013_2010, 
    -income2013_2011, -income2013_2012, -income2013_2013, -income2013_2014, -income2013_2015, 
    -income2013_2016, -income2013_2017, -income2013_2018, -income2013_2019, -income2013_2020, 
    -income2013_2021, 
  )%>%
  rename(
    pct_education_2013 = pct_education_2013_2000,
    income_2013 = income2013_2000
  )



bddsli_wider <- bddsli_wider%>%
  mutate(
    population_2014 = population_2013, population_2015 = population_2013,
    White_2014 = White_2013, White_2015 = White_2013,
    Black_2014 = Black_2013, Black_2015 = Black_2013,
    AIAN_2014 = AIAN_2013, AIAN_2015 = AIAN_2013,
    Asian_2014 = Asian_2013, Asian_2015 = Asian_2013, 
    owner_2014 = owner_2013, owner_2015 = owner_2013,
    vacancy_2014 = vacancy_2013, vacancy_2015 = vacancy_2013
  )


write.csv(bddsli_wider,"E:/did_process/data/table/blockgroup/bddsli_wider2.csv",row.names = FALSE)

#Connect statistical data to bfc_candidate---------------------------------------
bf_cps_demo_sub <- read.csv("E:/did_process/data/table/blockgroup/bf_cps_demo_sub.csv")
bf_cps_demo_sub2 <- read.csv("E:/did_process/data/table/blockgroup/bf_cps_demo_sub2.csv")

#Take the average of the BFs between two BGs, and summarize the values from the first row
bcds_mean <- bf_cps_demo_sub%>%
  group_by(TARGET_FID)%>%
  mutate(
    White_2006 = mean(White_2006),
    White_2007 = mean(White_2007), White_2008 = mean(White_2008), White_2009 = mean(White_2009), 
    White_2010 = mean(White_2010), White_2011 = mean(White_2011), White_2012 = mean(White_2012), 
    White_2013 = mean(White_2013), White_2014 = mean(White_2014), White_2015 = mean(White_2015),
    Black_2006 = mean(Black_2006),
    Black_2007 = mean(Black_2007), Black_2008 = mean(Black_2008), Black_2009 = mean(Black_2009),
    Black_2010 = mean(Black_2010), Black_2011 = mean(Black_2011), Black_2012 = mean(Black_2012),
    Black_2013 = mean(Black_2013), Black_2014 = mean(Black_2014), Black_2015 = mean(Black_2015),
    AIAN_2006 = mean(AIAN_2006),
    AIAN_2007 = mean(AIAN_2007), AIAN_2008 = mean(AIAN_2008), AIAN_2009 = mean(AIAN_2009),
    AIAN_2010 = mean(AIAN_2010), AIAN_2011 = mean(AIAN_2011), AIAN_2012 = mean(AIAN_2012),
    AIAN_2013 = mean(AIAN_2013), AIAN_2014 = mean(AIAN_2014), AIAN_2015 = mean(AIAN_2015),
    Asian_2006 = mean(Asian_2006),
    Asian_2007 = mean(Asian_2007), Asian_2008 = mean(Asian_2008), Asian_2009 = mean(Asian_2009),
    Asian_2010 = mean(Asian_2010), Asian_2011 = mean(Asian_2011), Asian_2012 = mean(Asian_2012),
    Asian_2013 = mean(Asian_2013), Asian_2014 = mean(Asian_2014), Asian_2015 = mean(Asian_2015),
    owner_2006 = mean(owner_2006),
    owner_2007 = mean(owner_2007), owner_2008 = mean(owner_2008), owner_2009 = mean(owner_2009),
    owner_2010 = mean(owner_2010), owner_2011 = mean(owner_2011), owner_2012 = mean(owner_2012),
    owner_2013 = mean(owner_2013), owner_2014 = mean(owner_2014), owner_2015 = mean(owner_2015),
    vacancy_2006 = mean(vacancy_2006),
    vacancy_2007 = mean(vacancy_2007), vacancy_2008 = mean(vacancy_2008), vacancy_2009 = mean(vacancy_2009),
    vacancy_2010 = mean(vacancy_2010), vacancy_2011 = mean(vacancy_2011), vacancy_2012 = mean(vacancy_2012),
    vacancy_2013 = mean(vacancy_2013), vacancy_2014 = mean(vacancy_2014), vacancy_2015 = mean(vacancy_2015),
    pct_education_2013 = mean(pct_education_2013), income_2013 = mean(income_2013)
  )%>%
  summarise(
    White_2006 = first(White_2006),
    White_2007 = first(White_2007), White_2008 = first(White_2008), White_2009 = first(White_2009), 
    White_2010 = first(White_2010), White_2011 = first(White_2011), White_2012 = first(White_2012), 
    White_2013 = first(White_2013), White_2014 = first(White_2014), White_2015 = first(White_2015),
    Black_2006 = first(Black_2006),
    Black_2007 = first(Black_2007), Black_2008 = first(Black_2008), Black_2009 = first(Black_2009),
    Black_2010 = first(Black_2010), Black_2011 = first(Black_2011), Black_2012 = first(Black_2012),
    Black_2013 = first(Black_2013), Black_2014 = first(Black_2014), Black_2015 = first(Black_2015),
    AIAN_2006 = first(AIAN_2006),
    AIAN_2007 = first(AIAN_2007), AIAN_2008 = first(AIAN_2008), AIAN_2009 = first(AIAN_2009),
    AIAN_2010 = first(AIAN_2010), AIAN_2011 = first(AIAN_2011), AIAN_2012 = first(AIAN_2012),
    AIAN_2013 = first(AIAN_2013), AIAN_2014 = first(AIAN_2014), AIAN_2015 = first(AIAN_2015),
    Asian_2006 = first(Asian_2006),
    Asian_2007 = first(Asian_2007), Asian_2008 = first(Asian_2008), Asian_2009 = first(Asian_2009),
    Asian_2010 = first(Asian_2010), Asian_2011 = first(Asian_2011), Asian_2012 = first(Asian_2012),
    Asian_2013 = first(Asian_2013), Asian_2014 = first(Asian_2014), Asian_2015 = first(Asian_2015),
    owner_2006 = first(owner_2006),
    owner_2007 = first(owner_2007), owner_2008 = first(owner_2008), owner_2009 = first(owner_2009),
    owner_2010 = first(owner_2010), owner_2011 = first(owner_2011), owner_2012 = first(owner_2012),
    owner_2013 = first(owner_2013), owner_2014 = first(owner_2014), owner_2015 = first(owner_2015),
    vacancy_2006 = first(vacancy_2006),
    vacancy_2007 = first(vacancy_2007), vacancy_2008 = first(vacancy_2008), vacancy_2009 = first(vacancy_2009),
    vacancy_2010 = first(vacancy_2010), vacancy_2011 = first(vacancy_2011), vacancy_2012 = first(vacancy_2012),
    vacancy_2013 = first(vacancy_2013), vacancy_2014 = first(vacancy_2014), vacancy_2015 = first(vacancy_2015),
    pct_education_2013 = first(pct_education_2013), income_2013 = first(income_2013)
  )

#Draw a parallel trend test chart 
bcds_mean2 <- bf_cps_demo_sub2%>%
  group_by(TARGET_FID)%>%
  mutate(
    White_2006 = mean(White_2006),
    White_2007 = mean(White_2007), White_2008 = mean(White_2008), White_2009 = mean(White_2009), 
    White_2010 = mean(White_2010), White_2011 = mean(White_2011), White_2012 = mean(White_2012), 
    White_2013 = mean(White_2013), White_2014 = mean(White_2014), White_2015 = mean(White_2015),
    White_2016 = mean(White_2016), White_2017 = mean(White_2017), White_2018 = mean(White_2018),
    White_2019 = mean(White_2019), White_2020 = mean(White_2020), White_2021 = mean(White_2021),
    Black_2006 = mean(Black_2006),
    Black_2007 = mean(Black_2007), Black_2008 = mean(Black_2008), Black_2009 = mean(Black_2009),
    Black_2010 = mean(Black_2010), Black_2011 = mean(Black_2011), Black_2012 = mean(Black_2012),
    Black_2013 = mean(Black_2013), Black_2014 = mean(Black_2014), Black_2015 = mean(Black_2015),
    Black_2016 = mean(Black_2016), Black_2017 = mean(Black_2017), Black_2018 = mean(Black_2018),
    Black_2019 = mean(Black_2019), Black_2020 = mean(Black_2020), Black_2021 = mean(Black_2021),
    AIAN_2006 = mean(AIAN_2006),
    AIAN_2007 = mean(AIAN_2007), AIAN_2008 = mean(AIAN_2008), AIAN_2009 = mean(AIAN_2009),
    AIAN_2010 = mean(AIAN_2010), AIAN_2011 = mean(AIAN_2011), AIAN_2012 = mean(AIAN_2012),
    AIAN_2013 = mean(AIAN_2013), AIAN_2014 = mean(AIAN_2014), AIAN_2015 = mean(AIAN_2015),
    AIAN_2016 = mean(AIAN_2016), AIAN_2017 = mean(AIAN_2017), AIAN_2018 = mean(AIAN_2018),
    AIAN_2019 = mean(AIAN_2019), AIAN_2020 = mean(AIAN_2020), AIAN_2021 = mean(AIAN_2021),
    Asian_2006 = mean(Asian_2006),
    Asian_2007 = mean(Asian_2007), Asian_2008 = mean(Asian_2008), Asian_2009 = mean(Asian_2009),
    Asian_2010 = mean(Asian_2010), Asian_2011 = mean(Asian_2011), Asian_2012 = mean(Asian_2012),
    Asian_2013 = mean(Asian_2013), Asian_2014 = mean(Asian_2014), Asian_2015 = mean(Asian_2015),
    Asian_2016 = mean(Asian_2016), Asian_2017 = mean(Asian_2017), Asian_2018 = mean(Asian_2018),
    Asian_2019 = mean(Asian_2019), Asian_2020 = mean(Asian_2020), Asian_2021 = mean(Asian_2021),
    owner_2006 = mean(owner_2006),
    owner_2007 = mean(owner_2007), owner_2008 = mean(owner_2008), owner_2009 = mean(owner_2009),
    owner_2010 = mean(owner_2010), owner_2011 = mean(owner_2011), owner_2012 = mean(owner_2012),
    owner_2013 = mean(owner_2013), owner_2014 = mean(owner_2014), owner_2015 = mean(owner_2015),
    owner_2016 = mean(owner_2016), owner_2017 = mean(owner_2017), owner_2018 = mean(owner_2018),
    owner_2019 = mean(owner_2019), owner_2020 = mean(owner_2020), owner_2021 = mean(owner_2021),
    vacancy_2006 = mean(vacancy_2006),
    vacancy_2007 = mean(vacancy_2007), vacancy_2008 = mean(vacancy_2008), vacancy_2009 = mean(vacancy_2009),
    vacancy_2010 = mean(vacancy_2010), vacancy_2011 = mean(vacancy_2011), vacancy_2012 = mean(vacancy_2012),
    vacancy_2013 = mean(vacancy_2013), vacancy_2014 = mean(vacancy_2014), vacancy_2015 = mean(vacancy_2015),
    vacancy_2016 = mean(vacancy_2016), vacancy_2017 = mean(vacancy_2017), vacancy_2018 = mean(vacancy_2018),
    vacancy_2019 = mean(vacancy_2019), vacancy_2020 = mean(vacancy_2020), vacancy_2021 = mean(vacancy_2021),
    pct_education_2013 = mean(pct_education_2013), income_2013 = mean(income_2013)
  )%>%
  summarise(
    White_2006 = first(White_2006),
    White_2007 = first(White_2007), White_2008 = first(White_2008), White_2009 = first(White_2009), 
    White_2010 = first(White_2010), White_2011 = first(White_2011), White_2012 = first(White_2012), 
    White_2013 = first(White_2013), White_2014 = first(White_2014), White_2015 = first(White_2015),
    White_2016 = first(White_2016), White_2017 = first(White_2017), White_2018 = first(White_2018),
    White_2019 = first(White_2019), White_2020 = first(White_2020), White_2021 = first(White_2021),
    Black_2006 = first(Black_2006),
    Black_2007 = first(Black_2007), Black_2008 = first(Black_2008), Black_2009 = first(Black_2009),
    Black_2010 = first(Black_2010), Black_2011 = first(Black_2011), Black_2012 = first(Black_2012),
    Black_2013 = first(Black_2013), Black_2014 = first(Black_2014), Black_2015 = first(Black_2015),
    Black_2016 = first(Black_2016), Black_2017 = first(Black_2017), Black_2018 = first(Black_2018),
    Black_2019 = first(Black_2019), Black_2020 = first(Black_2020), Black_2021 = first(Black_2021),
    AIAN_2006 = first(AIAN_2006),
    AIAN_2007 = first(AIAN_2007), AIAN_2008 = first(AIAN_2008), AIAN_2009 = first(AIAN_2009),
    AIAN_2010 = first(AIAN_2010), AIAN_2011 = first(AIAN_2011), AIAN_2012 = first(AIAN_2012),
    AIAN_2013 = first(AIAN_2013), AIAN_2014 = first(AIAN_2014), AIAN_2015 = first(AIAN_2015),
    AIAN_2016 = first(AIAN_2016), AIAN_2017 = first(AIAN_2017), AIAN_2018 = first(AIAN_2018),
    AIAN_2019 = first(AIAN_2019), AIAN_2020 = first(AIAN_2020), AIAN_2021 = first(AIAN_2021),
    Asian_2006 = first(Asian_2006),
    Asian_2007 = first(Asian_2007), Asian_2008 = first(Asian_2008), Asian_2009 = first(Asian_2009),
    Asian_2010 = first(Asian_2010), Asian_2011 = first(Asian_2011), Asian_2012 = first(Asian_2012),
    Asian_2013 = first(Asian_2013), Asian_2014 = first(Asian_2014), Asian_2015 = first(Asian_2015),
    Asian_2016 = first(Asian_2016), Asian_2017 = first(Asian_2017), Asian_2018 = first(Asian_2018),
    Asian_2019 = first(Asian_2019), Asian_2020 = first(Asian_2020), Asian_2021 = first(Asian_2021),
    owner_2006 = first(owner_2006),
    owner_2007 = first(owner_2007), owner_2008 = first(owner_2008), owner_2009 = first(owner_2009),
    owner_2010 = first(owner_2010), owner_2011 = first(owner_2011), owner_2012 = first(owner_2012),
    owner_2013 = first(owner_2013), owner_2014 = first(owner_2014), owner_2015 = first(owner_2015),
    owner_2016 = first(owner_2016), owner_2017 = first(owner_2017), owner_2018 = first(owner_2018),
    owner_2019 = first(owner_2019), owner_2020 = first(owner_2020), owner_2021 = first(owner_2021),
    vacancy_2006 = first(vacancy_2006),
    vacancy_2007 = first(vacancy_2007), vacancy_2008 = first(vacancy_2008), vacancy_2009 = first(vacancy_2009),
    vacancy_2010 = first(vacancy_2010), vacancy_2011 = first(vacancy_2011), vacancy_2012 = first(vacancy_2012),
    vacancy_2013 = first(vacancy_2013), vacancy_2014 = first(vacancy_2014), vacancy_2015 = first(vacancy_2015),
    vacancy_2016 = first(vacancy_2016), vacancy_2017 = first(vacancy_2017), vacancy_2018 = first(vacancy_2018),
    vacancy_2019 = first(vacancy_2019), vacancy_2020 = first(vacancy_2020), vacancy_2021 = first(vacancy_2021),
    pct_education_2013 = first(pct_education_2013), income_2013 = first(income_2013)
  )


#Perform regression and connect to bfc_bandidate
bfc_candidate <- bfc_candidate%>%
  left_join(
    dplyr::select(bcds_mean, TARGET_FID, 
                  White_2006,
                  White_2007, White_2008, White_2009, White_2010, White_2011,
                  White_2012, White_2013, White_2014, White_2015, 
                  Black_2006,
                  Black_2007, Black_2008, Black_2009, Black_2010, Black_2011,
                  Black_2012, Black_2013, Black_2014, Black_2015,
                  AIAN_2006,
                  AIAN_2007, AIAN_2008, AIAN_2009, AIAN_2010, AIAN_2011, AIAN_2012,
                  AIAN_2013, AIAN_2014, AIAN_2015,
                  Asian_2006,
                  Asian_2007, Asian_2008, Asian_2009, Asian_2010, Asian_2011, Asian_2012,
                  Asian_2013, Asian_2014, Asian_2015, 
                  owner_2006,
                  owner_2007, owner_2008, owner_2009, owner_2010, owner_2011, owner_2012,
                  owner_2013, owner_2014, owner_2015, 
                  vacancy_2006,
                  vacancy_2007, vacancy_2008, vacancy_2009, vacancy_2010, vacancy_2011,
                  vacancy_2012, vacancy_2013, vacancy_2014, vacancy_2015, 
                  pct_education_2013, income_2013
                  ),
    by=c("OBJECTID_1" = "TARGET_FID")
  )

#Clean up Blockface data---------------------------------------------------------------
bfc_candidate_sub <- bfc_candidate[!apply(is.na(bfc_candidate),1,any),]

#tree----------------------------------------------------------------------------
tree05_near <- read.csv("E:/did_process/data/table/tree/tree05_near.csv")
tree15_near <- read.csv("E:/did_process/data/table/tree/tree15_near.csv")

#stewardship
tree15_near <- tree15_near%>%
  mutate(
    steward = case_when(
      steward == "none" ~ 0,
      steward == "1or2" ~ 1,
      steward == "3or4" ~ 2,
      steward == "4orMore" ~ 3,
      TRUE ~ 0
    )
  )

bfcandis_tree05 <- bfc_candidate_sub%>%
  left_join(dplyr::select(tree05_near,OBJECTID,NEAR_FID),by=c("OBJECTID_1"="NEAR_FID"))
bfcandis_tree15 <- bfc_candidate_sub%>%
  left_join(
    dplyr::select(tree15_near,OBJECTID, NEAR_FID, steward),
    by=c("OBJECTID_1"="NEAR_FID")
  )

#05
bfcandis_tree05count <- bfcandis_tree05%>%
  group_by(OBJECTID_1)%>%
  mutate(
    tree_count05=n()
  )
bfcandis_tree05count$tree_count05 <- ifelse(is.na(bfcandis_tree05count$OBJECTID),0,bfcandis_tree05count$tree_count05)
bfcandis_tree05count <- bfcandis_tree05count%>%
  group_by(OBJECTID_1)%>%
  summarise(
    tree_count05=first(tree_count05)
  )

#15 + stewardship
#Stewardship connects to the final panel
bfcandis_tree15count <- bfcandis_tree15%>%
  group_by(OBJECTID_1)%>%
  mutate(
    tree_count15= n(),
    stewardship = sum(steward)
  )
bfcandis_tree15count$tree_count15 <- ifelse(is.na(bfcandis_tree15count$OBJECTID),0,bfcandis_tree15count$tree_count15)
bfcandis_tree15count$stewardship <- ifelse(is.na(bfcandis_tree15count$OBJECTID),0,bfcandis_tree15count$stewardship)
bfcandis_tree15count <- bfcandis_tree15count%>%
  group_by(OBJECTID_1)%>%
  summarise(
    tree_count15 = first(tree_count15),
    stewardship = first(stewardship)
  )

bfc_candidate_sub <- bfc_candidate_sub%>%
  left_join(bfcandis_tree05count,by="OBJECTID_1")
bfc_candidate_sub <- bfc_candidate_sub%>%
  left_join(bfcandis_tree15count,by="OBJECTID_1")

#Tree changes
bfc_candidate_sub <- bfc_candidate_sub%>%
  mutate(
    tree_change = tree_count15 - tree_count05
  )



#Match treatment group and control group-----------------------------------------
bfc_candidate_sub%>%
  group_by(boroughcod)%>%
  filter(candidate_treated==1)%>%
  count()

bfc_candidate_sub%>%
  group_by(boroughcod)%>%
  filter(candidate_treated==0)%>%
  count()


bf_cps_point <- read.csv("E:/did_process/data/table/blockgroup/bf_cps_point.csv")
bfc_candidate_sub <- bfc_candidate_sub%>%
  left_join(
    dplyr::select(bf_cps_point,objectid,POINT_X,POINT_Y),
    by = "objectid"
  )



bfc_candidate_sub$POINT_X <- as.numeric(bfc_candidate_sub$POINT_X)
bfc_candidate_sub$POINT_Y <- as.numeric(bfc_candidate_sub$POINT_Y)
bfc_candidate_sub <- bfc_candidate_sub%>%
  mutate(distance=NA)

min_distance <- set_units(0.25,"mile")%>%
  set_units("foot")%>%
  as.numeric()

calc_distance <- function(x1,y1,x2,y2){
  sqrt((x2-x1)^2+(y2-y1)^2)
}




matching_env <- new.env()
matching_env$used_controls <- c()


match_controls_unique <- function(treated_street,data,ratio=1){
  treated_edu <- treated_street$pct_education_2013
  treated_inc <- treated_street$income_2013  
  treated_x <- treated_street$POINT_X
  treated_y <- treated_street$POINT_Y
  treated_tree05 <- treated_street$tree_count05

  potential <- data[data$boroughcod == treated_street$boroughcod & data$candidate_treated == 0 &
                      !(data$OBJECTID_1 %in% matching_env$used_controls),]

  edu_lower <- treated_edu*0.8
  edu_upper <- treated_edu*1.2
  inc_lower <- treated_inc*0.8
  inc_upper <- treated_inc*1.2
  
  eligible <- potential[
    potential$pct_education_2013>=edu_lower&
      potential$pct_education_2013<=edu_upper&
      potential$income_2013>=inc_lower&
      potential$income_2013<=inc_upper&
      potential$tree_count05>=treated_tree05-2&
      potential$tree_count05<=treated_tree05+2
    ,
  ]


  eligible <- eligible[!(eligible$OBJECTID_1 %in% matching_env$used_controls), ]

  if(nrow(eligible)<ratio){
    return(rep(NA,ratio))
  }
  

  distances <- calc_distance(
    treated_x,treated_y,eligible$POINT_X,eligible$POINT_Y
  )
  
  eligible$distance <- distances
  eligible <- eligible[eligible$distance>=min_distance,]
  
  if(nrow(eligible)<ratio){
    return(rep(NA,ratio))
  }
  
  
  eligible_ids <- unique(eligible$OBJECTID_1)
  if (length(eligible_ids) < ratio) {
    return(rep(NA, ratio))
  }
  
  control_ids <- eligible_ids[
    sample.int(length(eligible_ids), size = ratio, replace = FALSE)
  ]
  control_ids <- setdiff(control_ids, matching_env$used_controls)
  
  if (length(control_ids) < ratio) {
    return(rep(NA, ratio))
  }
  
  if (!all(control_ids %in% data$OBJECTID_1)) {
    print(control_ids)
    print(head(eligible$OBJECTID_1))
    stop("BUG: control_ids 不在传入的 data$OBJECTID_1 中")
  }
  

  matching_env$used_controls <- c(matching_env$used_controls,control_ids)
  
  return(control_ids)
}



treated_streets <- bfc_candidate_sub[bfc_candidate_sub$candidate_treated==1 
                            ,]
treated_streets <- treated_streets[order(-treated_streets$pct_education_2013,
                                         -treated_streets$income_2013),]

matched_pairs <- list()

matching_env$used_controls <- c()

for (i in 1:nrow(treated_streets)) {
  street <- treated_streets[i,]
  controls <- match_controls_unique(street,bfc_candidate_sub,ratio = 1)
  print(paste("treated_ID: ",street$OBJECTID_1,"matched_ID: ",controls))
  
  if(!any(is.na(controls))){
    matched_pairs[[i]] <- data.frame(
      treat_id=street$OBJECTID_1,
      control_id=controls,
      pair_id=i
    )
  }else{
    warning(paste("Unable to process group street",street$OBJECTID_1,"find enough unique controls"))
  }
}



finalmatch <- do.call(rbind,matched_pairs)
finalmatch <- finalmatch[order(finalmatch$treat_id),]
write.csv(finalmatch, "E:/did_process/data/table/data_panel_re/finalmatch.csv", row.names = FALSE)


control <- bfc_candidate_sub%>%filter(OBJECTID_1 %in% finalmatch$control_id)
treatment <- bfc_candidate_sub%>%filter(OBJECTID_1 %in% finalmatch$treat_id)


finalmatch_tree <- finalmatch%>%
  left_join(
    dplyr::select(treatment, OBJECTID_1, allcrime_count06, allcrime_count07, 
                  tree_count05, tree_count15, tree_change, 
                  PlntSeas, boroughcod, income_2013, White_2013, Black_2013, 
                  AIAN_2013, Asian_2013),
    by = c("treat_id" = "OBJECTID_1")
  )%>%
  rename(
    treated_tree05 = tree_count05,
    treated_tree15 = tree_count15,
    treated_treechange = tree_change,
    PlntYears_fake = PlntSeas
  )%>%
  mutate(
    PlntYears_fake = as.numeric(substring(PlntYears_fake, first = nchar(PlntYears_fake)-1)),
    PlntYears_fake = case_when(
      PlntYears_fake == 7~ "2007",
      PlntYears_fake == 8~ "2008",
      PlntYears_fake == 9~ "2009",
      PlntYears_fake == 10~ "2010",
      PlntYears_fake == 11~ "2011",
      PlntYears_fake == 12~ "2012",
      PlntYears_fake == 13~ "2013",
      PlntYears_fake == 14~ "2014",
      PlntYears_fake == 15~ "2015"
    ),
    mean_crime0607 = (allcrime_count06 + allcrime_count07) / 2
  )%>%
  left_join(
    dplyr::select(control, OBJECTID_1, tree_count05, tree_count15),
    by = c("control_id" = "OBJECTID_1")
  )%>%
  rename(
    controled_tree05 = tree_count05,
    controled_tree15 = tree_count15
  )


#Stewardship grouping------------------------------------------------------------
finalmatch_stew <- finalmatch_tree%>%
  left_join(
    dplyr::select(bfcandis_tree15count, OBJECTID_1, stewardship),
    by = c("treat_id" = "OBJECTID_1")
  )


#1/3
finalmatch_stew_1 <- finalmatch_stew%>%
  arrange(stewardship)%>%
  slice(1:floor(n()/3))

#1/3~2/3
finalmatch_stew_2 <- finalmatch_stew%>%
  arrange(stewardship)%>%
  slice(
    (floor(n()/3)+1) : floor(n()/3*2)
  )
#2/3~3/3
finalmatch_stew_3 <- finalmatch_stew%>%
  arrange(stewardship)%>%
  slice(
    (floor(n()/3*2)+1) : n()
  )


#1/4
finalmatch_stew_41 <- finalmatch_stew%>%
  arrange(stewardship)%>%
  slice(1:floor(n()/4))

#1/4~2/4
finalmatch_stew_42 <- finalmatch_stew%>%
  arrange(stewardship)%>%
  slice(
    (floor(n()/4)+1) : floor(n()/4*2)
  )
#2/4~3/4
finalmatch_stew_43 <- finalmatch_stew%>%
  arrange(stewardship)%>%
  slice(
    (floor(n()/4*2)+1) : floor(n()/4*3)
  )
#3/4~4/4
finalmatch_stew_44 <- finalmatch_stew%>%
  arrange(stewardship)%>%
  slice(
    (floor(n()/4*3)+1) : n()
  )


#Intensity grouping--------------------------------------------------------------


#1/3
finalmatch_tree_1 <- finalmatch_tree%>%
  arrange(treated_treechange)%>%
  slice(1:floor(n()/3))

#1/3~2/3
finalmatch_tree_2 <- finalmatch_tree%>%
  arrange(treated_treechange)%>%
  slice(
    (floor(n()/3)+1) : floor(n()/3*2)
  )
#2/3~3/3
finalmatch_tree_3 <- finalmatch_tree%>%
  arrange(treated_treechange)%>%
  slice(
    (floor(n()/3*2)+1) : n()
  )


#1/4
finalmatch_tree_41 <- finalmatch_tree%>%
  arrange(treated_treechange)%>%
  slice(1:floor(n()/4))

#1/4~2/4
finalmatch_tree_42 <- finalmatch_tree%>%
  arrange(treated_treechange)%>%
  slice(
    (floor(n()/4)+1) : floor(n()/4*2)
  )
#2/4~3/4
finalmatch_tree_43 <- finalmatch_tree%>%
  arrange(treated_treechange)%>%
  slice(
    (floor(n()/4*2)+1) : floor(n()/4*3)
  )
#3/4~4/4
finalmatch_tree_44 <- finalmatch_tree%>%
  arrange(treated_treechange)%>%
  slice(
    (floor(n()/4*3)+1) : n()
  )


#baseline tree scarcity--------------------------------------------
finalmatch_hscar <- finalmatch_tree%>%
  filter(
    treated_tree05 >= 0 & treated_tree05 <= 1
  )

finalmatch_mscar <- finalmatch_tree%>%
  filter(
    treated_tree05 >= 2 & treated_tree05 <= 4
  )

finalmatch_lscar <- finalmatch_tree%>%
  filter(
    treated_tree05 >= 5 
  )

#Income grouping-----------------------------------------------------------------

finalmatch_low <- finalmatch_tree%>%
  arrange(income_2013)%>%
  slice(1:floor(n()/3))

finalmatch_med <- finalmatch_tree%>%
  arrange(income_2013)%>%
  slice(
    (floor(n()/3)+1) : floor(n()/3*2)
  )

finalmatch_high <- finalmatch_tree%>%
  arrange(income_2013)%>%
  slice(
    (floor(n()/3*2)+1) : n()
  )

#Racial grouping-----------------------------------------------------------------

finalmatch_White <- finalmatch_tree%>%
  filter(
    White_2013 >= 0.5
  )

finalmatch_Black <- finalmatch_tree%>%
  filter(
    White_2013 < 0.5 & Black_2013 >= 0.5
  )

finalmatch_Asian <- finalmatch_tree%>%
  filter(
    White_2013 < 0.5 & Black_2013 < 0.5 & Asian_2013 >= 0.5
  )

finalmatch_Mixed <- finalmatch_tree%>%
  filter(
    White_2013 < 0.5 & Black_2013 < 0.5 & Asian_2013 < 0.5
  )

#Crime grouping------------------------------------------------------------------

#baseline crime level
finalmatch_lc <- finalmatch_tree%>%
  arrange(allcrime_count06)%>%
  slice(1:floor(n()/3))

finalmatch_mc <- finalmatch_tree%>%
  arrange(allcrime_count06)%>%
  slice(
    (floor(n()/3)+1) : floor(n()/3*2)
  )

finalmatch_hc <- finalmatch_tree%>%
  arrange(allcrime_count06)%>%
  slice(
    (floor(n()/3*2)+1) : n()
  )

#--------------------------------------------------------------------------------
#Heterogeneity+sensitivity analysis+parallel trend test, classify first----------
bfccs_panel <- bfc_candidate_sub%>%
  filter(OBJECTID_1 %in% finalmatch$treat_id | 
           OBJECTID_1 %in% finalmatch$control_id)


bfccs_panel_para <- bfccs_panel%>%
  left_join(
    dplyr::select(bcds_mean2, TARGET_FID, 
                  White_2006, White_2007, White_2008, White_2009,
                  White_2010, White_2011, White_2012, White_2013,
                  White_2014, White_2015, 
                  White_2016, White_2017, White_2018, White_2019,
                  White_2020, White_2021,
                  Black_2006, Black_2007, Black_2008, Black_2009,
                  Black_2010, Black_2011, Black_2012, Black_2013,
                  Black_2014, Black_2015, 
                  Black_2016, Black_2017, Black_2018, Black_2019,
                  Black_2020, Black_2021,
                  AIAN_2006, AIAN_2007, AIAN_2008, AIAN_2009,
                  AIAN_2010, AIAN_2011, AIAN_2012, AIAN_2013,
                  AIAN_2014, AIAN_2015, 
                  AIAN_2016, AIAN_2017, AIAN_2018, AIAN_2019,
                  AIAN_2020, AIAN_2021,
                  Asian_2006, Asian_2007, Asian_2008, Asian_2009,
                  Asian_2010, Asian_2011, Asian_2012, Asian_2013,
                  Asian_2014, Asian_2015, 
                  Asian_2016, Asian_2017, Asian_2018, Asian_2019,
                  Asian_2020, Asian_2021,
                  owner_2006, owner_2007, owner_2008, owner_2009,
                  owner_2010, owner_2011, owner_2012, owner_2013,
                  owner_2014, owner_2015, 
                  owner_2016, owner_2017, owner_2018, owner_2019,
                  owner_2020, owner_2021,
                  vacancy_2006, vacancy_2007, vacancy_2008, vacancy_2009,
                  vacancy_2010, vacancy_2011, vacancy_2012, vacancy_2013,
                  vacancy_2014, vacancy_2015, 
                  vacancy_2016, vacancy_2017, vacancy_2018, vacancy_2019,
                  vacancy_2020, vacancy_2021,
    ),
    by=c("OBJECTID_1" = "TARGET_FID")
  )

bfccsp_para_sub <- bfccs_panel_para%>%
  filter(!is.na(White_2016))


#Intensity grouping--------------------------------------------------------------
bfccs_panel1 <- bfc_candidate_sub%>%
  filter(OBJECTID_1 %in% finalmatch_tree_1$treat_id | 
           OBJECTID_1 %in% finalmatch_tree_1$control_id)
bfccs_panel2 <- bfc_candidate_sub%>%
  filter(OBJECTID_1 %in% finalmatch_tree_2$treat_id | 
           OBJECTID_1 %in% finalmatch_tree_2$control_id)
bfccs_panel3 <- bfc_candidate_sub%>%
  filter(OBJECTID_1 %in% finalmatch_tree_3$treat_id | 
           OBJECTID_1 %in% finalmatch_tree_3$control_id)
bfccs_panel41 <- bfc_candidate_sub%>%
  filter(OBJECTID_1 %in% finalmatch_tree_41$treat_id | 
           OBJECTID_1 %in% finalmatch_tree_41$control_id)
bfccs_panel42 <- bfc_candidate_sub%>%
  filter(OBJECTID_1 %in% finalmatch_tree_42$treat_id | 
           OBJECTID_1 %in% finalmatch_tree_42$control_id)

bfccs_panel43 <- bfc_candidate_sub%>%
  filter(OBJECTID_1 %in% finalmatch_tree_43$treat_id | 
           OBJECTID_1 %in% finalmatch_tree_43$control_id)

bfccs_panel44 <- bfc_candidate_sub%>%
  filter(OBJECTID_1 %in% finalmatch_tree_44$treat_id | 
           OBJECTID_1 %in% finalmatch_tree_44$control_id)

#baseline tree scarcity ---------------------------------------------------------
bfccs_panel_lscar <- bfc_candidate_sub%>%
  filter(OBJECTID_1 %in% finalmatch_lscar$treat_id | 
           OBJECTID_1 %in% finalmatch_lscar$control_id)

bfccs_panel_mscar <- bfc_candidate_sub%>%
  filter(OBJECTID_1 %in% finalmatch_mscar$treat_id | 
           OBJECTID_1 %in% finalmatch_mscar$control_id)

bfccs_panel_hscar <- bfc_candidate_sub%>%
  filter(OBJECTID_1 %in% finalmatch_hscar$treat_id | 
           OBJECTID_1 %in% finalmatch_hscar$control_id)

#Income grouping-----------------------------------------------------------------
bfccs_panel_low <- bfc_candidate_sub%>%
  filter(OBJECTID_1 %in% finalmatch_low$treat_id | 
           OBJECTID_1 %in% finalmatch_low$control_id)

bfccs_panel_med <- bfc_candidate_sub%>%
  filter(OBJECTID_1 %in% finalmatch_med$treat_id | 
           OBJECTID_1 %in% finalmatch_med$control_id)

bfccs_panel_high <- bfc_candidate_sub%>%
  filter(OBJECTID_1 %in% finalmatch_high$treat_id | 
           OBJECTID_1 %in% finalmatch_high$control_id)

#Racial grouping-----------------------------------------------------------------
bfccs_panel_White <- bfc_candidate_sub%>%
  filter(OBJECTID_1 %in% finalmatch_White$treat_id | 
           OBJECTID_1 %in% finalmatch_White$control_id)
bfccs_panel_Black <- bfc_candidate_sub%>%
  filter(OBJECTID_1 %in% finalmatch_Black$treat_id | 
           OBJECTID_1 %in% finalmatch_Black$control_id)
bfccs_panel_Asian <- bfc_candidate_sub%>%
  filter(OBJECTID_1 %in% finalmatch_Asian$treat_id | 
           OBJECTID_1 %in% finalmatch_Asian$control_id)
bfccs_panel_Mixed <- bfc_candidate_sub%>%
  filter(OBJECTID_1 %in% finalmatch_Mixed$treat_id | 
           OBJECTID_1 %in% finalmatch_Mixed$control_id)

#Crime grouping------------------------------------------------------------------
bfccs_panel_lc <- bfc_candidate_sub%>%
  filter(OBJECTID_1 %in% finalmatch_lc$treat_id | 
           OBJECTID_1 %in% finalmatch_lc$control_id)

bfccs_panel_mc <- bfc_candidate_sub%>%
  filter(OBJECTID_1 %in% finalmatch_mc$treat_id | 
           OBJECTID_1 %in% finalmatch_mc$control_id)

bfccs_panel_hc <- bfc_candidate_sub%>%
  filter(OBJECTID_1 %in% finalmatch_hc$treat_id | 
           OBJECTID_1 %in% finalmatch_hc$control_id)

#--------------------------------------------------------------------------------
#Transforming to long data----------------------------------------------------------


bfccs_long <- bfccs_panel%>%
  pivot_longer(
    cols = c(
      # violent_count06,
      violent_count07, violent_count08, violent_count09,
      violent_count10, violent_count11, violent_count12, violent_count13,
      violent_count14, violent_count15,
      # violent_count16, violent_count17,
      # violent_count18, violent_count19, violent_count20, violent_count21
    ),
    names_to = "violent_year",
    values_to = "violent_value"
  )%>%
  pivot_longer(
    cols = c(
      # property_count06,
      property_count07, property_count08, property_count09,
      property_count10, property_count11, property_count12, property_count13,
      property_count14, property_count15,
      # property_count16, property_count17,
      # property_count18, property_count19, property_count20, property_count21
    ),
    names_to = "property_year",
    values_to = "property_value"
  )%>%
  pivot_longer(
    cols = c(
      # allcrime_count06,
      allcrime_count07, allcrime_count08, allcrime_count09,
      allcrime_count10, allcrime_count11, allcrime_count12, allcrime_count13,
      allcrime_count14, allcrime_count15, 
      # allcrime_count16, allcrime_count17,
      # allcrime_count18, allcrime_count19, allcrime_count20, allcrime_count21
    ),
    names_to = "allcrime_year",
    values_to = "allcrime_value"
  )%>%
  filter(
    # (violent_year=="violent_count06"&property_year=="property_count06"&allcrime_year=="allcrime_count06")|
    (violent_year=="violent_count07"&property_year=="property_count07"&allcrime_year=="allcrime_count07")|
    (violent_year=="violent_count08"&property_year=="property_count08"&allcrime_year=="allcrime_count08")|
    (violent_year=="violent_count09"&property_year=="property_count09"&allcrime_year=="allcrime_count09")|
    (violent_year=="violent_count10"&property_year=="property_count10"&allcrime_year=="allcrime_count10")|
    (violent_year=="violent_count11"&property_year=="property_count11"&allcrime_year=="allcrime_count11")|      
    (violent_year=="violent_count12"&property_year=="property_count12"&allcrime_year=="allcrime_count12")|
    (violent_year=="violent_count13"&property_year=="property_count13"&allcrime_year=="allcrime_count13")|
    (violent_year=="violent_count14"&property_year=="property_count14"&allcrime_year=="allcrime_count14")|
    (violent_year=="violent_count15"&property_year=="property_count15"&allcrime_year=="allcrime_count15")
    # |
    # (violent_year=="violent_count16"&property_year=="property_count16"&allcrime_year=="allcrime_count16")|
    # (violent_year=="violent_count17"&property_year=="property_count17"&allcrime_year=="allcrime_count17")|
    # (violent_year=="violent_count18"&property_year=="property_count18"&allcrime_year=="allcrime_count18")|
    # (violent_year=="violent_count19"&property_year=="property_count19"&allcrime_year=="allcrime_count19")|
    # (violent_year=="violent_count20"&property_year=="property_count20"&allcrime_year=="allcrime_count20")|
    # (violent_year=="violent_count21"&property_year=="property_count21"&allcrime_year=="allcrime_count21")
    )


bfccs_long_para <- bfccsp_para_sub%>%
  pivot_longer(
    cols = c(
      violent_count06,
      violent_count07, violent_count08, violent_count09,
      violent_count10, violent_count11, violent_count12, violent_count13,
      violent_count14, violent_count15,
      violent_count16, violent_count17,
      violent_count18, violent_count19, violent_count20, violent_count21
    ),
    names_to = "violent_year",
    values_to = "violent_value"
  )%>%
  pivot_longer(
    cols = c(
      property_count06,
      property_count07, property_count08, property_count09,
      property_count10, property_count11, property_count12, property_count13,
      property_count14, property_count15,
      property_count16, property_count17,
      property_count18, property_count19, property_count20, property_count21
    ),
    names_to = "property_year",
    values_to = "property_value"
  )%>%
  pivot_longer(
    cols = c(
      allcrime_count06,
      allcrime_count07, allcrime_count08, allcrime_count09,
      allcrime_count10, allcrime_count11, allcrime_count12, allcrime_count13,
      allcrime_count14, allcrime_count15, 
      allcrime_count16, allcrime_count17,
      allcrime_count18, allcrime_count19, allcrime_count20, allcrime_count21
    ),
    names_to = "allcrime_year",
    values_to = "allcrime_value"
  )%>%
  filter(
    (violent_year=="violent_count06"&property_year=="property_count06"&allcrime_year=="allcrime_count06")|
      (violent_year=="violent_count07"&property_year=="property_count07"&allcrime_year=="allcrime_count07")|
      (violent_year=="violent_count08"&property_year=="property_count08"&allcrime_year=="allcrime_count08")|
      (violent_year=="violent_count09"&property_year=="property_count09"&allcrime_year=="allcrime_count09")|
      (violent_year=="violent_count10"&property_year=="property_count10"&allcrime_year=="allcrime_count10")|
      (violent_year=="violent_count11"&property_year=="property_count11"&allcrime_year=="allcrime_count11")|      
      (violent_year=="violent_count12"&property_year=="property_count12"&allcrime_year=="allcrime_count12")|
      (violent_year=="violent_count13"&property_year=="property_count13"&allcrime_year=="allcrime_count13")|
      (violent_year=="violent_count14"&property_year=="property_count14"&allcrime_year=="allcrime_count14")|
      (violent_year=="violent_count15"&property_year=="property_count15"&allcrime_year=="allcrime_count15")
    |
      (violent_year=="violent_count16"&property_year=="property_count16"&allcrime_year=="allcrime_count16")|
      (violent_year=="violent_count17"&property_year=="property_count17"&allcrime_year=="allcrime_count17")|
      (violent_year=="violent_count18"&property_year=="property_count18"&allcrime_year=="allcrime_count18")|
      (violent_year=="violent_count19"&property_year=="property_count19"&allcrime_year=="allcrime_count19")|
      (violent_year=="violent_count20"&property_year=="property_count20"&allcrime_year=="allcrime_count20")|
      (violent_year=="violent_count21"&property_year=="property_count21"&allcrime_year=="allcrime_count21")
  )

#Intensity grouping--------------------------------------------------------------
bfccs_long1 <- bfccs_panel1%>%
  pivot_longer(
    cols = c(violent_count07, violent_count08, violent_count09,
             violent_count10, violent_count11, violent_count12, violent_count13,
             violent_count14, violent_count15),
    names_to = "violent_year",
    values_to = "violent_value"
  )%>%
  pivot_longer(
    cols = c(property_count07, property_count08, property_count09,
             property_count10, property_count11, property_count12, property_count13,
             property_count14, property_count15),
    names_to = "property_year",
    values_to = "property_value"
  )%>%
  pivot_longer(
    cols = c(allcrime_count07, allcrime_count08, allcrime_count09,
             allcrime_count10, allcrime_count11, allcrime_count12, allcrime_count13,
             allcrime_count14, allcrime_count15),
    names_to = "allcrime_year",
    values_to = "allcrime_value"
  )%>%
  filter(
    # (violent_year=="violent_count06"&property_year=="property_count06"&allcrime_year=="allcrime_count06")|
    (violent_year=="violent_count07"&property_year=="property_count07"&allcrime_year=="allcrime_count07")|
      (violent_year=="violent_count08"&property_year=="property_count08"&allcrime_year=="allcrime_count08")|
      (violent_year=="violent_count09"&property_year=="property_count09"&allcrime_year=="allcrime_count09")|
      (violent_year=="violent_count10"&property_year=="property_count10"&allcrime_year=="allcrime_count10")|
      (violent_year=="violent_count11"&property_year=="property_count11"&allcrime_year=="allcrime_count11")|      
      (violent_year=="violent_count12"&property_year=="property_count12"&allcrime_year=="allcrime_count12")|
      (violent_year=="violent_count13"&property_year=="property_count13"&allcrime_year=="allcrime_count13")|
      (violent_year=="violent_count14"&property_year=="property_count14"&allcrime_year=="allcrime_count14")|
      (violent_year=="violent_count15"&property_year=="property_count15"&allcrime_year=="allcrime_count15")
  )

bfccs_long2 <- bfccs_panel2%>%
  pivot_longer(
    cols = c(violent_count07, violent_count08, violent_count09,
             violent_count10, violent_count11, violent_count12, violent_count13,
             violent_count14, violent_count15),
    names_to = "violent_year",
    values_to = "violent_value"
  )%>%
  pivot_longer(
    cols = c(property_count07, property_count08, property_count09,
             property_count10, property_count11, property_count12, property_count13,
             property_count14, property_count15),
    names_to = "property_year",
    values_to = "property_value"
  )%>%
  pivot_longer(
    cols = c(allcrime_count07, allcrime_count08, allcrime_count09,
             allcrime_count10, allcrime_count11, allcrime_count12, allcrime_count13,
             allcrime_count14, allcrime_count15),
    names_to = "allcrime_year",
    values_to = "allcrime_value"
  )%>%
  filter(
    # (violent_year=="violent_count06"&property_year=="property_count06"&allcrime_year=="allcrime_count06")|
    (violent_year=="violent_count07"&property_year=="property_count07"&allcrime_year=="allcrime_count07")|
      (violent_year=="violent_count08"&property_year=="property_count08"&allcrime_year=="allcrime_count08")|
      (violent_year=="violent_count09"&property_year=="property_count09"&allcrime_year=="allcrime_count09")|
      (violent_year=="violent_count10"&property_year=="property_count10"&allcrime_year=="allcrime_count10")|
      (violent_year=="violent_count11"&property_year=="property_count11"&allcrime_year=="allcrime_count11")|      
      (violent_year=="violent_count12"&property_year=="property_count12"&allcrime_year=="allcrime_count12")|
      (violent_year=="violent_count13"&property_year=="property_count13"&allcrime_year=="allcrime_count13")|
      (violent_year=="violent_count14"&property_year=="property_count14"&allcrime_year=="allcrime_count14")|
      (violent_year=="violent_count15"&property_year=="property_count15"&allcrime_year=="allcrime_count15")
  )
bfccs_long3 <- bfccs_panel3%>%
  pivot_longer(
    cols = c(violent_count07, violent_count08, violent_count09,
             violent_count10, violent_count11, violent_count12, violent_count13,
             violent_count14, violent_count15),
    names_to = "violent_year",
    values_to = "violent_value"
  )%>%
  pivot_longer(
    cols = c(property_count07, property_count08, property_count09,
             property_count10, property_count11, property_count12, property_count13,
             property_count14, property_count15),
    names_to = "property_year",
    values_to = "property_value"
  )%>%
  pivot_longer(
    cols = c(allcrime_count07, allcrime_count08, allcrime_count09,
             allcrime_count10, allcrime_count11, allcrime_count12, allcrime_count13,
             allcrime_count14, allcrime_count15),
    names_to = "allcrime_year",
    values_to = "allcrime_value"
  )%>%
  filter(
    # (violent_year=="violent_count06"&property_year=="property_count06"&allcrime_year=="allcrime_count06")|
    (violent_year=="violent_count07"&property_year=="property_count07"&allcrime_year=="allcrime_count07")|
      (violent_year=="violent_count08"&property_year=="property_count08"&allcrime_year=="allcrime_count08")|
      (violent_year=="violent_count09"&property_year=="property_count09"&allcrime_year=="allcrime_count09")|
      (violent_year=="violent_count10"&property_year=="property_count10"&allcrime_year=="allcrime_count10")|
      (violent_year=="violent_count11"&property_year=="property_count11"&allcrime_year=="allcrime_count11")|      
      (violent_year=="violent_count12"&property_year=="property_count12"&allcrime_year=="allcrime_count12")|
      (violent_year=="violent_count13"&property_year=="property_count13"&allcrime_year=="allcrime_count13")|
      (violent_year=="violent_count14"&property_year=="property_count14"&allcrime_year=="allcrime_count14")|
      (violent_year=="violent_count15"&property_year=="property_count15"&allcrime_year=="allcrime_count15")
  )

bfccs_long41 <- bfccs_panel41%>%
  pivot_longer(
    cols = c(violent_count07, violent_count08, violent_count09,
             violent_count10, violent_count11, violent_count12, violent_count13,
             violent_count14, violent_count15),
    names_to = "violent_year",
    values_to = "violent_value"
  )%>%
  pivot_longer(
    cols = c(property_count07, property_count08, property_count09,
             property_count10, property_count11, property_count12, property_count13,
             property_count14, property_count15),
    names_to = "property_year",
    values_to = "property_value"
  )%>%
  pivot_longer(
    cols = c(allcrime_count07, allcrime_count08, allcrime_count09,
             allcrime_count10, allcrime_count11, allcrime_count12, allcrime_count13,
             allcrime_count14, allcrime_count15),
    names_to = "allcrime_year",
    values_to = "allcrime_value"
  )%>%
  filter(
    # (violent_year=="violent_count06"&property_year=="property_count06"&allcrime_year=="allcrime_count06")|
    (violent_year=="violent_count07"&property_year=="property_count07"&allcrime_year=="allcrime_count07")|
      (violent_year=="violent_count08"&property_year=="property_count08"&allcrime_year=="allcrime_count08")|
      (violent_year=="violent_count09"&property_year=="property_count09"&allcrime_year=="allcrime_count09")|
      (violent_year=="violent_count10"&property_year=="property_count10"&allcrime_year=="allcrime_count10")|
      (violent_year=="violent_count11"&property_year=="property_count11"&allcrime_year=="allcrime_count11")|      
      (violent_year=="violent_count12"&property_year=="property_count12"&allcrime_year=="allcrime_count12")|
      (violent_year=="violent_count13"&property_year=="property_count13"&allcrime_year=="allcrime_count13")|
      (violent_year=="violent_count14"&property_year=="property_count14"&allcrime_year=="allcrime_count14")|
      (violent_year=="violent_count15"&property_year=="property_count15"&allcrime_year=="allcrime_count15")
  )

bfccs_long42 <- bfccs_panel42%>%
  pivot_longer(
    cols = c(violent_count07, violent_count08, violent_count09,
             violent_count10, violent_count11, violent_count12, violent_count13,
             violent_count14, violent_count15),
    names_to = "violent_year",
    values_to = "violent_value"
  )%>%
  pivot_longer(
    cols = c(property_count07, property_count08, property_count09,
             property_count10, property_count11, property_count12, property_count13,
             property_count14, property_count15),
    names_to = "property_year",
    values_to = "property_value"
  )%>%
  pivot_longer(
    cols = c(allcrime_count07, allcrime_count08, allcrime_count09,
             allcrime_count10, allcrime_count11, allcrime_count12, allcrime_count13,
             allcrime_count14, allcrime_count15),
    names_to = "allcrime_year",
    values_to = "allcrime_value"
  )%>%
  filter(
    # (violent_year=="violent_count06"&property_year=="property_count06"&allcrime_year=="allcrime_count06")|
    (violent_year=="violent_count07"&property_year=="property_count07"&allcrime_year=="allcrime_count07")|
      (violent_year=="violent_count08"&property_year=="property_count08"&allcrime_year=="allcrime_count08")|
      (violent_year=="violent_count09"&property_year=="property_count09"&allcrime_year=="allcrime_count09")|
      (violent_year=="violent_count10"&property_year=="property_count10"&allcrime_year=="allcrime_count10")|
      (violent_year=="violent_count11"&property_year=="property_count11"&allcrime_year=="allcrime_count11")|      
      (violent_year=="violent_count12"&property_year=="property_count12"&allcrime_year=="allcrime_count12")|
      (violent_year=="violent_count13"&property_year=="property_count13"&allcrime_year=="allcrime_count13")|
      (violent_year=="violent_count14"&property_year=="property_count14"&allcrime_year=="allcrime_count14")|
      (violent_year=="violent_count15"&property_year=="property_count15"&allcrime_year=="allcrime_count15")
  )

bfccs_long43 <- bfccs_panel43%>%
  pivot_longer(
    cols = c(violent_count07, violent_count08, violent_count09,
             violent_count10, violent_count11, violent_count12, violent_count13,
             violent_count14, violent_count15),
    names_to = "violent_year",
    values_to = "violent_value"
  )%>%
  pivot_longer(
    cols = c(property_count07, property_count08, property_count09,
             property_count10, property_count11, property_count12, property_count13,
             property_count14, property_count15),
    names_to = "property_year",
    values_to = "property_value"
  )%>%
  pivot_longer(
    cols = c(allcrime_count07, allcrime_count08, allcrime_count09,
             allcrime_count10, allcrime_count11, allcrime_count12, allcrime_count13,
             allcrime_count14, allcrime_count15),
    names_to = "allcrime_year",
    values_to = "allcrime_value"
  )%>%
  filter(
    # (violent_year=="violent_count06"&property_year=="property_count06"&allcrime_year=="allcrime_count06")|
    (violent_year=="violent_count07"&property_year=="property_count07"&allcrime_year=="allcrime_count07")|
      (violent_year=="violent_count08"&property_year=="property_count08"&allcrime_year=="allcrime_count08")|
      (violent_year=="violent_count09"&property_year=="property_count09"&allcrime_year=="allcrime_count09")|
      (violent_year=="violent_count10"&property_year=="property_count10"&allcrime_year=="allcrime_count10")|
      (violent_year=="violent_count11"&property_year=="property_count11"&allcrime_year=="allcrime_count11")|      
      (violent_year=="violent_count12"&property_year=="property_count12"&allcrime_year=="allcrime_count12")|
      (violent_year=="violent_count13"&property_year=="property_count13"&allcrime_year=="allcrime_count13")|
      (violent_year=="violent_count14"&property_year=="property_count14"&allcrime_year=="allcrime_count14")|
      (violent_year=="violent_count15"&property_year=="property_count15"&allcrime_year=="allcrime_count15")
  )

bfccs_long44 <- bfccs_panel44%>%
  pivot_longer(
    cols = c(violent_count07, violent_count08, violent_count09,
             violent_count10, violent_count11, violent_count12, violent_count13,
             violent_count14, violent_count15),
    names_to = "violent_year",
    values_to = "violent_value"
  )%>%
  pivot_longer(
    cols = c(property_count07, property_count08, property_count09,
             property_count10, property_count11, property_count12, property_count13,
             property_count14, property_count15),
    names_to = "property_year",
    values_to = "property_value"
  )%>%
  pivot_longer(
    cols = c(allcrime_count07, allcrime_count08, allcrime_count09,
             allcrime_count10, allcrime_count11, allcrime_count12, allcrime_count13,
             allcrime_count14, allcrime_count15),
    names_to = "allcrime_year",
    values_to = "allcrime_value"
  )%>%
  filter(
    # (violent_year=="violent_count06"&property_year=="property_count06"&allcrime_year=="allcrime_count06")|
      (violent_year=="violent_count07"&property_year=="property_count07"&allcrime_year=="allcrime_count07")|
      (violent_year=="violent_count08"&property_year=="property_count08"&allcrime_year=="allcrime_count08")|
      (violent_year=="violent_count09"&property_year=="property_count09"&allcrime_year=="allcrime_count09")|
      (violent_year=="violent_count10"&property_year=="property_count10"&allcrime_year=="allcrime_count10")|
      (violent_year=="violent_count11"&property_year=="property_count11"&allcrime_year=="allcrime_count11")|      
      (violent_year=="violent_count12"&property_year=="property_count12"&allcrime_year=="allcrime_count12")|
      (violent_year=="violent_count13"&property_year=="property_count13"&allcrime_year=="allcrime_count13")|
      (violent_year=="violent_count14"&property_year=="property_count14"&allcrime_year=="allcrime_count14")|
      (violent_year=="violent_count15"&property_year=="property_count15"&allcrime_year=="allcrime_count15")
  )

#baseline tree measure ----------------------------------------------------------
bfccs_long_lscar <- bfccs_panel_lscar%>%
  pivot_longer(
    cols = c(violent_count07, violent_count08, violent_count09,
             violent_count10, violent_count11, violent_count12, violent_count13,
             violent_count14, violent_count15),
    names_to = "violent_year",
    values_to = "violent_value"
  )%>%
  pivot_longer(
    cols = c(property_count07, property_count08, property_count09,
             property_count10, property_count11, property_count12, property_count13,
             property_count14, property_count15),
    names_to = "property_year",
    values_to = "property_value"
  )%>%
  pivot_longer(
    cols = c(allcrime_count07, allcrime_count08, allcrime_count09,
             allcrime_count10, allcrime_count11, allcrime_count12, allcrime_count13,
             allcrime_count14, allcrime_count15),
    names_to = "allcrime_year",
    values_to = "allcrime_value"
  )%>%
  filter(
    # (violent_year=="violent_count06"&property_year=="property_count06"&allcrime_year=="allcrime_count06")|
    (violent_year=="violent_count07"&property_year=="property_count07"&allcrime_year=="allcrime_count07")|
      (violent_year=="violent_count08"&property_year=="property_count08"&allcrime_year=="allcrime_count08")|
      (violent_year=="violent_count09"&property_year=="property_count09"&allcrime_year=="allcrime_count09")|
      (violent_year=="violent_count10"&property_year=="property_count10"&allcrime_year=="allcrime_count10")|
      (violent_year=="violent_count11"&property_year=="property_count11"&allcrime_year=="allcrime_count11")|      
      (violent_year=="violent_count12"&property_year=="property_count12"&allcrime_year=="allcrime_count12")|
      (violent_year=="violent_count13"&property_year=="property_count13"&allcrime_year=="allcrime_count13")|
      (violent_year=="violent_count14"&property_year=="property_count14"&allcrime_year=="allcrime_count14")|
      (violent_year=="violent_count15"&property_year=="property_count15"&allcrime_year=="allcrime_count15")
  )

bfccs_long_mscar <- bfccs_panel_mscar%>%
  pivot_longer(
    cols = c(violent_count07, violent_count08, violent_count09,
             violent_count10, violent_count11, violent_count12, violent_count13,
             violent_count14, violent_count15),
    names_to = "violent_year",
    values_to = "violent_value"
  )%>%
  pivot_longer(
    cols = c(property_count07, property_count08, property_count09,
             property_count10, property_count11, property_count12, property_count13,
             property_count14, property_count15),
    names_to = "property_year",
    values_to = "property_value"
  )%>%
  pivot_longer(
    cols = c(allcrime_count07, allcrime_count08, allcrime_count09,
             allcrime_count10, allcrime_count11, allcrime_count12, allcrime_count13,
             allcrime_count14, allcrime_count15),
    names_to = "allcrime_year",
    values_to = "allcrime_value"
  )%>%
  filter(
    # (violent_year=="violent_count06"&property_year=="property_count06"&allcrime_year=="allcrime_count06")|
    (violent_year=="violent_count07"&property_year=="property_count07"&allcrime_year=="allcrime_count07")|
      (violent_year=="violent_count08"&property_year=="property_count08"&allcrime_year=="allcrime_count08")|
      (violent_year=="violent_count09"&property_year=="property_count09"&allcrime_year=="allcrime_count09")|
      (violent_year=="violent_count10"&property_year=="property_count10"&allcrime_year=="allcrime_count10")|
      (violent_year=="violent_count11"&property_year=="property_count11"&allcrime_year=="allcrime_count11")|      
      (violent_year=="violent_count12"&property_year=="property_count12"&allcrime_year=="allcrime_count12")|
      (violent_year=="violent_count13"&property_year=="property_count13"&allcrime_year=="allcrime_count13")|
      (violent_year=="violent_count14"&property_year=="property_count14"&allcrime_year=="allcrime_count14")|
      (violent_year=="violent_count15"&property_year=="property_count15"&allcrime_year=="allcrime_count15")
  )

bfccs_long_hscar <- bfccs_panel_hscar%>%
  pivot_longer(
    cols = c(violent_count07, violent_count08, violent_count09,
             violent_count10, violent_count11, violent_count12, violent_count13,
             violent_count14, violent_count15),
    names_to = "violent_year",
    values_to = "violent_value"
  )%>%
  pivot_longer(
    cols = c(property_count07, property_count08, property_count09,
             property_count10, property_count11, property_count12, property_count13,
             property_count14, property_count15),
    names_to = "property_year",
    values_to = "property_value"
  )%>%
  pivot_longer(
    cols = c(allcrime_count07, allcrime_count08, allcrime_count09,
             allcrime_count10, allcrime_count11, allcrime_count12, allcrime_count13,
             allcrime_count14, allcrime_count15),
    names_to = "allcrime_year",
    values_to = "allcrime_value"
  )%>%
  filter(
    # (violent_year=="violent_count06"&property_year=="property_count06"&allcrime_year=="allcrime_count06")|
      (violent_year=="violent_count07"&property_year=="property_count07"&allcrime_year=="allcrime_count07")|
      (violent_year=="violent_count08"&property_year=="property_count08"&allcrime_year=="allcrime_count08")|
      (violent_year=="violent_count09"&property_year=="property_count09"&allcrime_year=="allcrime_count09")|
      (violent_year=="violent_count10"&property_year=="property_count10"&allcrime_year=="allcrime_count10")|
      (violent_year=="violent_count11"&property_year=="property_count11"&allcrime_year=="allcrime_count11")|      
      (violent_year=="violent_count12"&property_year=="property_count12"&allcrime_year=="allcrime_count12")|
      (violent_year=="violent_count13"&property_year=="property_count13"&allcrime_year=="allcrime_count13")|
      (violent_year=="violent_count14"&property_year=="property_count14"&allcrime_year=="allcrime_count14")|
      (violent_year=="violent_count15"&property_year=="property_count15"&allcrime_year=="allcrime_count15")
  )

#Income grouping-----------------------------------------------------------------
bfccs_long_low <- bfccs_panel_low%>%
  pivot_longer(
    cols = c(violent_count07, violent_count08, violent_count09,
             violent_count10, violent_count11, violent_count12, violent_count13,
             violent_count14, violent_count15),
    names_to = "violent_year",
    values_to = "violent_value"
  )%>%
  pivot_longer(
    cols = c(property_count07, property_count08, property_count09,
             property_count10, property_count11, property_count12, property_count13,
             property_count14, property_count15),
    names_to = "property_year",
    values_to = "property_value"
  )%>%
  pivot_longer(
    cols = c(allcrime_count07, allcrime_count08, allcrime_count09,
             allcrime_count10, allcrime_count11, allcrime_count12, allcrime_count13,
             allcrime_count14, allcrime_count15),
    names_to = "allcrime_year",
    values_to = "allcrime_value"
  )%>%
  filter(
    # (violent_year=="violent_count06"&property_year=="property_count06"&allcrime_year=="allcrime_count06")|
    (violent_year=="violent_count07"&property_year=="property_count07"&allcrime_year=="allcrime_count07")|
      (violent_year=="violent_count08"&property_year=="property_count08"&allcrime_year=="allcrime_count08")|
      (violent_year=="violent_count09"&property_year=="property_count09"&allcrime_year=="allcrime_count09")|
      (violent_year=="violent_count10"&property_year=="property_count10"&allcrime_year=="allcrime_count10")|
      (violent_year=="violent_count11"&property_year=="property_count11"&allcrime_year=="allcrime_count11")|      
      (violent_year=="violent_count12"&property_year=="property_count12"&allcrime_year=="allcrime_count12")|
      (violent_year=="violent_count13"&property_year=="property_count13"&allcrime_year=="allcrime_count13")|
      (violent_year=="violent_count14"&property_year=="property_count14"&allcrime_year=="allcrime_count14")|
      (violent_year=="violent_count15"&property_year=="property_count15"&allcrime_year=="allcrime_count15")
  )

bfccs_long_med <- bfccs_panel_med%>%
  pivot_longer(
    cols = c(violent_count07, violent_count08, violent_count09,
             violent_count10, violent_count11, violent_count12, violent_count13,
             violent_count14, violent_count15),
    names_to = "violent_year",
    values_to = "violent_value"
  )%>%
  pivot_longer(
    cols = c(property_count07, property_count08, property_count09,
             property_count10, property_count11, property_count12, property_count13,
             property_count14, property_count15),
    names_to = "property_year",
    values_to = "property_value"
  )%>%
  pivot_longer(
    cols = c(allcrime_count07, allcrime_count08, allcrime_count09,
             allcrime_count10, allcrime_count11, allcrime_count12, allcrime_count13,
             allcrime_count14, allcrime_count15),
    names_to = "allcrime_year",
    values_to = "allcrime_value"
  )%>%
  filter(
    # (violent_year=="violent_count06"&property_year=="property_count06"&allcrime_year=="allcrime_count06")|
    (violent_year=="violent_count07"&property_year=="property_count07"&allcrime_year=="allcrime_count07")|
      (violent_year=="violent_count08"&property_year=="property_count08"&allcrime_year=="allcrime_count08")|
      (violent_year=="violent_count09"&property_year=="property_count09"&allcrime_year=="allcrime_count09")|
      (violent_year=="violent_count10"&property_year=="property_count10"&allcrime_year=="allcrime_count10")|
      (violent_year=="violent_count11"&property_year=="property_count11"&allcrime_year=="allcrime_count11")|      
      (violent_year=="violent_count12"&property_year=="property_count12"&allcrime_year=="allcrime_count12")|
      (violent_year=="violent_count13"&property_year=="property_count13"&allcrime_year=="allcrime_count13")|
      (violent_year=="violent_count14"&property_year=="property_count14"&allcrime_year=="allcrime_count14")|
      (violent_year=="violent_count15"&property_year=="property_count15"&allcrime_year=="allcrime_count15")
  )

bfccs_long_high <- bfccs_panel_high%>%
  pivot_longer(
    cols = c(violent_count07, violent_count08, violent_count09,
             violent_count10, violent_count11, violent_count12, violent_count13,
             violent_count14, violent_count15),
    names_to = "violent_year",
    values_to = "violent_value"
  )%>%
  pivot_longer(
    cols = c(property_count07, property_count08, property_count09,
             property_count10, property_count11, property_count12, property_count13,
             property_count14, property_count15),
    names_to = "property_year",
    values_to = "property_value"
  )%>%
  pivot_longer(
    cols = c(allcrime_count07, allcrime_count08, allcrime_count09,
             allcrime_count10, allcrime_count11, allcrime_count12, allcrime_count13,
             allcrime_count14, allcrime_count15),
    names_to = "allcrime_year",
    values_to = "allcrime_value"
  )%>%
  filter(
    # (violent_year=="violent_count06"&property_year=="property_count06"&allcrime_year=="allcrime_count06")|
      (violent_year=="violent_count07"&property_year=="property_count07"&allcrime_year=="allcrime_count07")|
      (violent_year=="violent_count08"&property_year=="property_count08"&allcrime_year=="allcrime_count08")|
      (violent_year=="violent_count09"&property_year=="property_count09"&allcrime_year=="allcrime_count09")|
      (violent_year=="violent_count10"&property_year=="property_count10"&allcrime_year=="allcrime_count10")|
      (violent_year=="violent_count11"&property_year=="property_count11"&allcrime_year=="allcrime_count11")|      
      (violent_year=="violent_count12"&property_year=="property_count12"&allcrime_year=="allcrime_count12")|
      (violent_year=="violent_count13"&property_year=="property_count13"&allcrime_year=="allcrime_count13")|
      (violent_year=="violent_count14"&property_year=="property_count14"&allcrime_year=="allcrime_count14")|
      (violent_year=="violent_count15"&property_year=="property_count15"&allcrime_year=="allcrime_count15")
  )

#Racial grouping-----------------------------------------------------------------
bfccs_long_White <- bfccs_panel_White%>%
  pivot_longer(
    cols = c(violent_count07, violent_count08, violent_count09,
             violent_count10, violent_count11, violent_count12, violent_count13,
             violent_count14, violent_count15),
    names_to = "violent_year",
    values_to = "violent_value"
  )%>%
  pivot_longer(
    cols = c(property_count07, property_count08, property_count09,
             property_count10, property_count11, property_count12, property_count13,
             property_count14, property_count15),
    names_to = "property_year",
    values_to = "property_value"
  )%>%
  pivot_longer(
    cols = c(allcrime_count07, allcrime_count08, allcrime_count09,
             allcrime_count10, allcrime_count11, allcrime_count12, allcrime_count13,
             allcrime_count14, allcrime_count15),
    names_to = "allcrime_year",
    values_to = "allcrime_value"
  )%>%
  filter(
    # (violent_year=="violent_count06"&property_year=="property_count06"&allcrime_year=="allcrime_count06")|
    (violent_year=="violent_count07"&property_year=="property_count07"&allcrime_year=="allcrime_count07")|
      (violent_year=="violent_count08"&property_year=="property_count08"&allcrime_year=="allcrime_count08")|
      (violent_year=="violent_count09"&property_year=="property_count09"&allcrime_year=="allcrime_count09")|
      (violent_year=="violent_count10"&property_year=="property_count10"&allcrime_year=="allcrime_count10")|
      (violent_year=="violent_count11"&property_year=="property_count11"&allcrime_year=="allcrime_count11")|      
      (violent_year=="violent_count12"&property_year=="property_count12"&allcrime_year=="allcrime_count12")|
      (violent_year=="violent_count13"&property_year=="property_count13"&allcrime_year=="allcrime_count13")|
      (violent_year=="violent_count14"&property_year=="property_count14"&allcrime_year=="allcrime_count14")|
      (violent_year=="violent_count15"&property_year=="property_count15"&allcrime_year=="allcrime_count15")
  )

bfccs_long_Black <- bfccs_panel_Black%>%
  pivot_longer(
    cols = c(violent_count07, violent_count08, violent_count09,
             violent_count10, violent_count11, violent_count12, violent_count13,
             violent_count14, violent_count15),
    names_to = "violent_year",
    values_to = "violent_value"
  )%>%
  pivot_longer(
    cols = c(property_count07, property_count08, property_count09,
             property_count10, property_count11, property_count12, property_count13,
             property_count14, property_count15),
    names_to = "property_year",
    values_to = "property_value"
  )%>%
  pivot_longer(
    cols = c(allcrime_count07, allcrime_count08, allcrime_count09,
             allcrime_count10, allcrime_count11, allcrime_count12, allcrime_count13,
             allcrime_count14, allcrime_count15),
    names_to = "allcrime_year",
    values_to = "allcrime_value"
  )%>%
  filter(
    # (violent_year=="violent_count06"&property_year=="property_count06"&allcrime_year=="allcrime_count06")|
    (violent_year=="violent_count07"&property_year=="property_count07"&allcrime_year=="allcrime_count07")|
      (violent_year=="violent_count08"&property_year=="property_count08"&allcrime_year=="allcrime_count08")|
      (violent_year=="violent_count09"&property_year=="property_count09"&allcrime_year=="allcrime_count09")|
      (violent_year=="violent_count10"&property_year=="property_count10"&allcrime_year=="allcrime_count10")|
      (violent_year=="violent_count11"&property_year=="property_count11"&allcrime_year=="allcrime_count11")|      
      (violent_year=="violent_count12"&property_year=="property_count12"&allcrime_year=="allcrime_count12")|
      (violent_year=="violent_count13"&property_year=="property_count13"&allcrime_year=="allcrime_count13")|
      (violent_year=="violent_count14"&property_year=="property_count14"&allcrime_year=="allcrime_count14")|
      (violent_year=="violent_count15"&property_year=="property_count15"&allcrime_year=="allcrime_count15")
  )

bfccs_long_Asian <- bfccs_panel_Asian%>%
  pivot_longer(
    cols = c(violent_count07, violent_count08, violent_count09,
             violent_count10, violent_count11, violent_count12, violent_count13,
             violent_count14, violent_count15),
    names_to = "violent_year",
    values_to = "violent_value"
  )%>%
  pivot_longer(
    cols = c(property_count07, property_count08, property_count09,
             property_count10, property_count11, property_count12, property_count13,
             property_count14, property_count15),
    names_to = "property_year",
    values_to = "property_value"
  )%>%
  pivot_longer(
    cols = c(allcrime_count07, allcrime_count08, allcrime_count09,
             allcrime_count10, allcrime_count11, allcrime_count12, allcrime_count13,
             allcrime_count14, allcrime_count15),
    names_to = "allcrime_year",
    values_to = "allcrime_value"
  )%>%
  filter(
    # (violent_year=="violent_count06"&property_year=="property_count06"&allcrime_year=="allcrime_count06")|
    (violent_year=="violent_count07"&property_year=="property_count07"&allcrime_year=="allcrime_count07")|
      (violent_year=="violent_count08"&property_year=="property_count08"&allcrime_year=="allcrime_count08")|
      (violent_year=="violent_count09"&property_year=="property_count09"&allcrime_year=="allcrime_count09")|
      (violent_year=="violent_count10"&property_year=="property_count10"&allcrime_year=="allcrime_count10")|
      (violent_year=="violent_count11"&property_year=="property_count11"&allcrime_year=="allcrime_count11")|      
      (violent_year=="violent_count12"&property_year=="property_count12"&allcrime_year=="allcrime_count12")|
      (violent_year=="violent_count13"&property_year=="property_count13"&allcrime_year=="allcrime_count13")|
      (violent_year=="violent_count14"&property_year=="property_count14"&allcrime_year=="allcrime_count14")|
      (violent_year=="violent_count15"&property_year=="property_count15"&allcrime_year=="allcrime_count15")
  )

bfccs_long_Mixed <- bfccs_panel_Mixed%>%
  pivot_longer(
    cols = c(violent_count07, violent_count08, violent_count09,
             violent_count10, violent_count11, violent_count12, violent_count13,
             violent_count14, violent_count15),
    names_to = "violent_year",
    values_to = "violent_value"
  )%>%
  pivot_longer(
    cols = c(property_count07, property_count08, property_count09,
             property_count10, property_count11, property_count12, property_count13,
             property_count14, property_count15),
    names_to = "property_year",
    values_to = "property_value"
  )%>%
  pivot_longer(
    cols = c(allcrime_count07, allcrime_count08, allcrime_count09,
             allcrime_count10, allcrime_count11, allcrime_count12, allcrime_count13,
             allcrime_count14, allcrime_count15),
    names_to = "allcrime_year",
    values_to = "allcrime_value"
  )%>%
  filter(
    # (violent_year=="violent_count06"&property_year=="property_count06"&allcrime_year=="allcrime_count06")|
      (violent_year=="violent_count07"&property_year=="property_count07"&allcrime_year=="allcrime_count07")|
      (violent_year=="violent_count08"&property_year=="property_count08"&allcrime_year=="allcrime_count08")|
      (violent_year=="violent_count09"&property_year=="property_count09"&allcrime_year=="allcrime_count09")|
      (violent_year=="violent_count10"&property_year=="property_count10"&allcrime_year=="allcrime_count10")|
      (violent_year=="violent_count11"&property_year=="property_count11"&allcrime_year=="allcrime_count11")|      
      (violent_year=="violent_count12"&property_year=="property_count12"&allcrime_year=="allcrime_count12")|
      (violent_year=="violent_count13"&property_year=="property_count13"&allcrime_year=="allcrime_count13")|
      (violent_year=="violent_count14"&property_year=="property_count14"&allcrime_year=="allcrime_count14")|
      (violent_year=="violent_count15"&property_year=="property_count15"&allcrime_year=="allcrime_count15")
  )

#Crime grouping------------------------------------------------------------------
bfccs_long_lc <- bfccs_panel_lc%>%
  pivot_longer(
    cols = c(violent_count07, violent_count08, violent_count09,
             violent_count10, violent_count11, violent_count12, violent_count13,
             violent_count14, violent_count15),
    names_to = "violent_year",
    values_to = "violent_value"
  )%>%
  pivot_longer(
    cols = c(property_count07, property_count08, property_count09,
             property_count10, property_count11, property_count12, property_count13,
             property_count14, property_count15),
    names_to = "property_year",
    values_to = "property_value"
  )%>%
  pivot_longer(
    cols = c(allcrime_count07, allcrime_count08, allcrime_count09,
             allcrime_count10, allcrime_count11, allcrime_count12, allcrime_count13,
             allcrime_count14, allcrime_count15),
    names_to = "allcrime_year",
    values_to = "allcrime_value"
  )%>%
  filter(
    # (violent_year=="violent_count06"&property_year=="property_count06"&allcrime_year=="allcrime_count06")|
      (violent_year=="violent_count07"&property_year=="property_count07"&allcrime_year=="allcrime_count07")|
      (violent_year=="violent_count08"&property_year=="property_count08"&allcrime_year=="allcrime_count08")|
      (violent_year=="violent_count09"&property_year=="property_count09"&allcrime_year=="allcrime_count09")|
      (violent_year=="violent_count10"&property_year=="property_count10"&allcrime_year=="allcrime_count10")|
      (violent_year=="violent_count11"&property_year=="property_count11"&allcrime_year=="allcrime_count11")|      
      (violent_year=="violent_count12"&property_year=="property_count12"&allcrime_year=="allcrime_count12")|
      (violent_year=="violent_count13"&property_year=="property_count13"&allcrime_year=="allcrime_count13")|
      (violent_year=="violent_count14"&property_year=="property_count14"&allcrime_year=="allcrime_count14")|
      (violent_year=="violent_count15"&property_year=="property_count15"&allcrime_year=="allcrime_count15")
  )

bfccs_long_mc <- bfccs_panel_mc%>%
  pivot_longer(
    cols = c(violent_count07, violent_count08, violent_count09,
             violent_count10, violent_count11, violent_count12, violent_count13,
             violent_count14, violent_count15),
    names_to = "violent_year",
    values_to = "violent_value"
  )%>%
  pivot_longer(
    cols = c(property_count07, property_count08, property_count09,
             property_count10, property_count11, property_count12, property_count13,
             property_count14, property_count15),
    names_to = "property_year",
    values_to = "property_value"
  )%>%
  pivot_longer(
    cols = c(allcrime_count07, allcrime_count08, allcrime_count09,
             allcrime_count10, allcrime_count11, allcrime_count12, allcrime_count13,
             allcrime_count14, allcrime_count15),
    names_to = "allcrime_year",
    values_to = "allcrime_value"
  )%>%
  filter(
    # (violent_year=="violent_count06"&property_year=="property_count06"&allcrime_year=="allcrime_count06")|
    (violent_year=="violent_count07"&property_year=="property_count07"&allcrime_year=="allcrime_count07")|
      (violent_year=="violent_count08"&property_year=="property_count08"&allcrime_year=="allcrime_count08")|
      (violent_year=="violent_count09"&property_year=="property_count09"&allcrime_year=="allcrime_count09")|
      (violent_year=="violent_count10"&property_year=="property_count10"&allcrime_year=="allcrime_count10")|
      (violent_year=="violent_count11"&property_year=="property_count11"&allcrime_year=="allcrime_count11")|      
      (violent_year=="violent_count12"&property_year=="property_count12"&allcrime_year=="allcrime_count12")|
      (violent_year=="violent_count13"&property_year=="property_count13"&allcrime_year=="allcrime_count13")|
      (violent_year=="violent_count14"&property_year=="property_count14"&allcrime_year=="allcrime_count14")|
      (violent_year=="violent_count15"&property_year=="property_count15"&allcrime_year=="allcrime_count15")
  )

bfccs_long_hc <- bfccs_panel_hc%>%
  pivot_longer(
    cols = c(violent_count07, violent_count08, violent_count09,
             violent_count10, violent_count11, violent_count12, violent_count13,
             violent_count14, violent_count15),
    names_to = "violent_year",
    values_to = "violent_value"
  )%>%
  pivot_longer(
    cols = c(property_count07, property_count08, property_count09,
             property_count10, property_count11, property_count12, property_count13,
             property_count14, property_count15),
    names_to = "property_year",
    values_to = "property_value"
  )%>%
  pivot_longer(
    cols = c(allcrime_count07, allcrime_count08, allcrime_count09,
             allcrime_count10, allcrime_count11, allcrime_count12, allcrime_count13,
             allcrime_count14, allcrime_count15),
    names_to = "allcrime_year",
    values_to = "allcrime_value"
  )%>%
  filter(
    # (violent_year=="violent_count06"&property_year=="property_count06"&allcrime_year=="allcrime_count06")|
    (violent_year=="violent_count07"&property_year=="property_count07"&allcrime_year=="allcrime_count07")|
      (violent_year=="violent_count08"&property_year=="property_count08"&allcrime_year=="allcrime_count08")|
      (violent_year=="violent_count09"&property_year=="property_count09"&allcrime_year=="allcrime_count09")|
      (violent_year=="violent_count10"&property_year=="property_count10"&allcrime_year=="allcrime_count10")|
      (violent_year=="violent_count11"&property_year=="property_count11"&allcrime_year=="allcrime_count11")|      
      (violent_year=="violent_count12"&property_year=="property_count12"&allcrime_year=="allcrime_count12")|
      (violent_year=="violent_count13"&property_year=="property_count13"&allcrime_year=="allcrime_count13")|
      (violent_year=="violent_count14"&property_year=="property_count14"&allcrime_year=="allcrime_count14")|
      (violent_year=="violent_count15"&property_year=="property_count15"&allcrime_year=="allcrime_count15")
  )


#--------------------------------------------------------------------------------
#Transforming SES to long data---------------------------------------------------
bfccs_long <- bfccs_long%>%
  pivot_longer(
    cols = c(
      White_2007, White_2008, White_2009, White_2010, White_2011, White_2012,
      White_2013, White_2014, White_2015
    ),
    names_to = "White_year",
    values_to = "White_value"
  )%>%
  pivot_longer(
    cols = c(
      Black_2007, Black_2008, Black_2009, Black_2010, Black_2011, Black_2012,
      Black_2013, Black_2014, Black_2015
    ),
    names_to = "Black_year",
    values_to = "Black_value"
  )%>%
  filter(
      (violent_year=="violent_count07" & White_year=="White_2007" & Black_year=="Black_2007") |
      (violent_year=="violent_count08" & White_year=="White_2008" & Black_year=="Black_2008") |
      (violent_year=="violent_count09" & White_year=="White_2009" & Black_year=="Black_2009") |
      (violent_year=="violent_count10" & White_year=="White_2010" & Black_year=="Black_2010") |
      (violent_year=="violent_count11" & White_year=="White_2011" & Black_year=="Black_2011") |
      (violent_year=="violent_count12" & White_year=="White_2012" & Black_year=="Black_2012") |
      (violent_year=="violent_count13" & White_year=="White_2013" & Black_year=="Black_2013") |
      (violent_year=="violent_count14" & White_year=="White_2014" & Black_year=="Black_2014") |
      (violent_year=="violent_count15" & White_year=="White_2015" & Black_year=="Black_2015") 
  )

bfccs_long <- bfccs_long%>%
  pivot_longer(
    cols = c(
      AIAN_2007, AIAN_2008, AIAN_2009, AIAN_2010, AIAN_2011, AIAN_2012, AIAN_2013,
      AIAN_2014, AIAN_2015
    ),
    names_to = "AIAN_year",
    values_to = "AIAN_value"
  )%>%
  pivot_longer(
    cols = c(
      Asian_2007, Asian_2008, Asian_2009, Asian_2010, Asian_2011, Asian_2012,
      Asian_2013, Asian_2014, Asian_2015
    ),
    names_to = "Asian_year",
    values_to = "Asian_value"
  )%>%
  filter(
      (violent_year=="violent_count07" & AIAN_year=="AIAN_2007" & Asian_year=="Asian_2007") |
      (violent_year=="violent_count08" & AIAN_year=="AIAN_2008" & Asian_year=="Asian_2008") |
      (violent_year=="violent_count09" & AIAN_year=="AIAN_2009" & Asian_year=="Asian_2009") |
      (violent_year=="violent_count10" & AIAN_year=="AIAN_2010" & Asian_year=="Asian_2010") |
      (violent_year=="violent_count11" & AIAN_year=="AIAN_2011" & Asian_year=="Asian_2011") |
      (violent_year=="violent_count12" & AIAN_year=="AIAN_2012" & Asian_year=="Asian_2012") |
      (violent_year=="violent_count13" & AIAN_year=="AIAN_2013" & Asian_year=="Asian_2013") |
      (violent_year=="violent_count14" & AIAN_year=="AIAN_2014" & Asian_year=="Asian_2014") |
      (violent_year=="violent_count15" & AIAN_year=="AIAN_2015" & Asian_year=="Asian_2015") 
  )

bfccs_long <- bfccs_long%>%
  pivot_longer(
    cols = c(
      owner_2007, owner_2008, owner_2009, owner_2010, owner_2011, owner_2012, owner_2013,
      owner_2014, owner_2015
    ),
    names_to = "owner_year",
    values_to = "owner_value"
  )%>%
  pivot_longer(
    cols = c(
      vacancy_2007, vacancy_2008, vacancy_2009, vacancy_2010, vacancy_2011, vacancy_2012,
      vacancy_2013, vacancy_2014, vacancy_2015
    ),
    names_to = "vacancy_year",
    values_to = "vacancy_value"
  )%>%
  filter(
      (violent_year=="violent_count07"&owner_year=="owner_2007"&vacancy_year=="vacancy_2007")|
      (violent_year=="violent_count08"&owner_year=="owner_2008"&vacancy_year=="vacancy_2008")|
      (violent_year=="violent_count09"&owner_year=="owner_2009"&vacancy_year=="vacancy_2009")|
      (violent_year=="violent_count10"&owner_year=="owner_2010"&vacancy_year=="vacancy_2010")|
      (violent_year=="violent_count11"&owner_year=="owner_2011"&vacancy_year=="vacancy_2011")|
      (violent_year=="violent_count12"&owner_year=="owner_2012"&vacancy_year=="vacancy_2012")|
      (violent_year=="violent_count13"&owner_year=="owner_2013"&vacancy_year=="vacancy_2013")|
      (violent_year=="violent_count14"&owner_year=="owner_2014"&vacancy_year=="vacancy_2014")|
      (violent_year=="violent_count15"&owner_year=="owner_2015"&vacancy_year=="vacancy_2015")
  )


bfccs_long_para <- bfccs_long_para%>%
  pivot_longer(
    cols = c(
      White_2006.y,
      White_2007.y, White_2008.y, White_2009.y, White_2010.y, White_2011.y, White_2012.y,
      White_2013.y, White_2014.y, White_2015.y, White_2016, White_2017, White_2018, 
      White_2019, White_2020, White_2021
    ),
    names_to = "White_year",
    values_to = "White_value"
  )%>%
  pivot_longer(
    cols = c(
      Black_2006.y,
      Black_2007.y, Black_2008.y, Black_2009.y, Black_2010.y, Black_2011.y, Black_2012.y,
      Black_2013.y, Black_2014.y, Black_2015.y, Black_2016, Black_2017, Black_2018, 
      Black_2019, Black_2020, Black_2021
    ),
    names_to = "Black_year",
    values_to = "Black_value"
  )%>%
  filter(
    (violent_year=="violent_count06" & White_year=="White_2006.y" & Black_year=="Black_2006.y") |
      (violent_year=="violent_count07" & White_year=="White_2007.y" & Black_year=="Black_2007.y") |
      (violent_year=="violent_count08" & White_year=="White_2008.y" & Black_year=="Black_2008.y") |
      (violent_year=="violent_count09" & White_year=="White_2009.y" & Black_year=="Black_2009.y") |
      (violent_year=="violent_count10" & White_year=="White_2010.y" & Black_year=="Black_2010.y") |
      (violent_year=="violent_count11" & White_year=="White_2011.y" & Black_year=="Black_2011.y") |
      (violent_year=="violent_count12" & White_year=="White_2012.y" & Black_year=="Black_2012.y") |
      (violent_year=="violent_count13" & White_year=="White_2013.y" & Black_year=="Black_2013.y") |
      (violent_year=="violent_count14" & White_year=="White_2014.y" & Black_year=="Black_2014.y") |
      (violent_year=="violent_count15" & White_year=="White_2015.y" & Black_year=="Black_2015.y") |
      (violent_year=="violent_count16" & White_year=="White_2016" & Black_year=="Black_2016") |
      (violent_year=="violent_count17" & White_year=="White_2017" & Black_year=="Black_2017") |
      (violent_year=="violent_count18" & White_year=="White_2018" & Black_year=="Black_2018") |
      (violent_year=="violent_count19" & White_year=="White_2019" & Black_year=="Black_2019") |
      (violent_year=="violent_count20" & White_year=="White_2020" & Black_year=="Black_2020") |
      (violent_year=="violent_count21" & White_year=="White_2021" & Black_year=="Black_2021")
  )

bfccs_long_para <- bfccs_long_para%>%
  pivot_longer(
    cols = c(
      AIAN_2006.y,
      AIAN_2007.y, AIAN_2008.y, AIAN_2009.y, AIAN_2010.y, AIAN_2011.y, AIAN_2012.y, AIAN_2013.y,
      AIAN_2014.y, AIAN_2015.y, AIAN_2016, AIAN_2017, AIAN_2018, AIAN_2019, AIAN_2020, AIAN_2021
    ),
    names_to = "AIAN_year",
    values_to = "AIAN_value"
  )%>%
  pivot_longer(
    cols = c(
      Asian_2006.y,
      Asian_2007.y, Asian_2008.y, Asian_2009.y, Asian_2010.y, Asian_2011.y, Asian_2012.y,
      Asian_2013.y, Asian_2014.y, Asian_2015.y, Asian_2016, Asian_2017, Asian_2018, 
      Asian_2019, Asian_2020, Asian_2021
    ),
    names_to = "Asian_year",
    values_to = "Asian_value"
  )%>%
  filter(
    (violent_year=="violent_count06" & AIAN_year=="AIAN_2006.y" & Asian_year=="Asian_2006.y") |
      (violent_year=="violent_count07" & AIAN_year=="AIAN_2007.y" & Asian_year=="Asian_2007.y") |
      (violent_year=="violent_count08" & AIAN_year=="AIAN_2008.y" & Asian_year=="Asian_2008.y") |
      (violent_year=="violent_count09" & AIAN_year=="AIAN_2009.y" & Asian_year=="Asian_2009.y") |
      (violent_year=="violent_count10" & AIAN_year=="AIAN_2010.y" & Asian_year=="Asian_2010.y") |
      (violent_year=="violent_count11" & AIAN_year=="AIAN_2011.y" & Asian_year=="Asian_2011.y") |
      (violent_year=="violent_count12" & AIAN_year=="AIAN_2012.y" & Asian_year=="Asian_2012.y") |
      (violent_year=="violent_count13" & AIAN_year=="AIAN_2013.y" & Asian_year=="Asian_2013.y") |
      (violent_year=="violent_count14" & AIAN_year=="AIAN_2014.y" & Asian_year=="Asian_2014.y") |
      (violent_year=="violent_count15" & AIAN_year=="AIAN_2015.y" & Asian_year=="Asian_2015.y") |
      (violent_year=="violent_count16" & AIAN_year=="AIAN_2016" & Asian_year=="Asian_2016") |
      (violent_year=="violent_count17" & AIAN_year=="AIAN_2017" & Asian_year=="Asian_2017") |
      (violent_year=="violent_count18" & AIAN_year=="AIAN_2018" & Asian_year=="Asian_2018") |
      (violent_year=="violent_count19" & AIAN_year=="AIAN_2019" & Asian_year=="Asian_2019") |
      (violent_year=="violent_count20" & AIAN_year=="AIAN_2020" & Asian_year=="Asian_2020") |
      (violent_year=="violent_count21" & AIAN_year=="AIAN_2021" & Asian_year=="Asian_2021")
  )

bfccs_long_para <- bfccs_long_para%>%
  pivot_longer(
    cols = c(
      owner_2006.y,
      owner_2007.y, owner_2008.y, owner_2009.y, owner_2010.y, owner_2011.y, owner_2012.y, owner_2013.y,
      owner_2014.y, owner_2015.y, owner_2016, owner_2017, owner_2018, owner_2019, owner_2020, owner_2021
    ),
    names_to = "owner_year",
    values_to = "owner_value"
  )%>%
  pivot_longer(
    cols = c(
      vacancy_2006.y,
      vacancy_2007.y, vacancy_2008.y, vacancy_2009.y, vacancy_2010.y, vacancy_2011.y, vacancy_2012.y,
      vacancy_2013.y, vacancy_2014.y, vacancy_2015.y, vacancy_2016, vacancy_2017, vacancy_2018, 
      vacancy_2019, vacancy_2020, vacancy_2021
    ),
    names_to = "vacancy_year",
    values_to = "vacancy_value"
  )%>%
  filter(
    (violent_year=="violent_count06"&owner_year=="owner_2006.y"&vacancy_year=="vacancy_2006.y")|
      (violent_year=="violent_count07"&owner_year=="owner_2007.y"&vacancy_year=="vacancy_2007.y")|
      (violent_year=="violent_count08"&owner_year=="owner_2008.y"&vacancy_year=="vacancy_2008.y")|
      (violent_year=="violent_count09"&owner_year=="owner_2009.y"&vacancy_year=="vacancy_2009.y")|
      (violent_year=="violent_count10"&owner_year=="owner_2010.y"&vacancy_year=="vacancy_2010.y")|
      (violent_year=="violent_count11"&owner_year=="owner_2011.y"&vacancy_year=="vacancy_2011.y")|
      (violent_year=="violent_count12"&owner_year=="owner_2012.y"&vacancy_year=="vacancy_2012.y")|
      (violent_year=="violent_count13"&owner_year=="owner_2013.y"&vacancy_year=="vacancy_2013.y")|
      (violent_year=="violent_count14"&owner_year=="owner_2014.y"&vacancy_year=="vacancy_2014.y")|
      (violent_year=="violent_count15"&owner_year=="owner_2015.y"&vacancy_year=="vacancy_2015.y")|
      (violent_year=="violent_count16"&owner_year=="owner_2016"&vacancy_year=="vacancy_2016")|
      (violent_year=="violent_count17"&owner_year=="owner_2017"&vacancy_year=="vacancy_2017")|
      (violent_year=="violent_count18"&owner_year=="owner_2018"&vacancy_year=="vacancy_2018")|
      (violent_year=="violent_count19"&owner_year=="owner_2019"&vacancy_year=="vacancy_2019")|
      (violent_year=="violent_count20"&owner_year=="owner_2020"&vacancy_year=="vacancy_2020")|
      (violent_year=="violent_count21"&owner_year=="owner_2021"&vacancy_year=="vacancy_2021")
  )

#Intensity grouping--------------------------------------------------------------
bfccs_long44 <- bfccs_long44%>%
  pivot_longer(
    cols = c(
      # White_2006, 
      White_2007, White_2008, White_2009, White_2010, White_2011, White_2012,
      White_2013, White_2014, White_2015
    ),
    names_to = "White_year",
    values_to = "White_value"
  )%>%
  pivot_longer(
    cols = c(
      # Black_2006, 
      Black_2007, Black_2008, Black_2009, Black_2010, Black_2011, Black_2012,
      Black_2013, Black_2014, Black_2015
    ),
    names_to = "Black_year",
    values_to = "Black_value"
  )%>%
  filter(
    # (violent_year=="violent_count06" & White_year=="White_2006" & Black_year=="Black_2006") |
    (violent_year=="violent_count07" & White_year=="White_2007" & Black_year=="Black_2007") |
      (violent_year=="violent_count08" & White_year=="White_2008" & Black_year=="Black_2008") |
      (violent_year=="violent_count09" & White_year=="White_2009" & Black_year=="Black_2009") |
      (violent_year=="violent_count10" & White_year=="White_2010" & Black_year=="Black_2010") |
      (violent_year=="violent_count11" & White_year=="White_2011" & Black_year=="Black_2011") |
      (violent_year=="violent_count12" & White_year=="White_2012" & Black_year=="Black_2012") |
      (violent_year=="violent_count13" & White_year=="White_2013" & Black_year=="Black_2013") |
      (violent_year=="violent_count14" & White_year=="White_2014" & Black_year=="Black_2014") |
      (violent_year=="violent_count15" & White_year=="White_2015" & Black_year=="Black_2015")
  )

bfccs_long44 <- bfccs_long44%>%
  pivot_longer(
    cols = c(
      # AIAN_2006, 
      AIAN_2007, AIAN_2008, AIAN_2009, AIAN_2010, AIAN_2011, AIAN_2012, AIAN_2013,
      AIAN_2014, AIAN_2015
    ),
    names_to = "AIAN_year",
    values_to = "AIAN_value"
  )%>%
  pivot_longer(
    cols = c(
      # Asian_2006, 
      Asian_2007, Asian_2008, Asian_2009, Asian_2010, Asian_2011, Asian_2012,
      Asian_2013, Asian_2014, Asian_2015
    ),
    names_to = "Asian_year",
    values_to = "Asian_value"
  )%>%
  filter(
    # (violent_year=="violent_count06" & AIAN_year=="AIAN_2006" & Asian_year=="Asian_2006") |
    (violent_year=="violent_count07" & AIAN_year=="AIAN_2007" & Asian_year=="Asian_2007") |
      (violent_year=="violent_count08" & AIAN_year=="AIAN_2008" & Asian_year=="Asian_2008") |
      (violent_year=="violent_count09" & AIAN_year=="AIAN_2009" & Asian_year=="Asian_2009") |
      (violent_year=="violent_count10" & AIAN_year=="AIAN_2010" & Asian_year=="Asian_2010") |
      (violent_year=="violent_count11" & AIAN_year=="AIAN_2011" & Asian_year=="Asian_2011") |
      (violent_year=="violent_count12" & AIAN_year=="AIAN_2012" & Asian_year=="Asian_2012") |
      (violent_year=="violent_count13" & AIAN_year=="AIAN_2013" & Asian_year=="Asian_2013") |
      (violent_year=="violent_count14" & AIAN_year=="AIAN_2014" & Asian_year=="Asian_2014") |
      (violent_year=="violent_count15" & AIAN_year=="AIAN_2015" & Asian_year=="Asian_2015")
  )

bfccs_long44 <- bfccs_long44%>%
  pivot_longer(
    cols = c(
      # owner_2006, 
      owner_2007, owner_2008, owner_2009, owner_2010, owner_2011, owner_2012, owner_2013,
      owner_2014, owner_2015
    ),
    names_to = "owner_year",
    values_to = "owner_value"
  )%>%
  pivot_longer(
    cols = c(
      # vacancy_2006, 
      vacancy_2007, vacancy_2008, vacancy_2009, vacancy_2010, vacancy_2011, vacancy_2012,
      vacancy_2013, vacancy_2014, vacancy_2015
    ),
    names_to = "vacancy_year",
    values_to = "vacancy_value"
  )%>%
  filter(
    # (violent_year=="violent_count06"&owner_year=="owner_2006"&vacancy_year=="vacancy_2006")|
    (violent_year=="violent_count07"&owner_year=="owner_2007"&vacancy_year=="vacancy_2007")|
      (violent_year=="violent_count08"&owner_year=="owner_2008"&vacancy_year=="vacancy_2008")|
      (violent_year=="violent_count09"&owner_year=="owner_2009"&vacancy_year=="vacancy_2009")|
      (violent_year=="violent_count10"&owner_year=="owner_2010"&vacancy_year=="vacancy_2010")|
      (violent_year=="violent_count11"&owner_year=="owner_2011"&vacancy_year=="vacancy_2011")|
      (violent_year=="violent_count12"&owner_year=="owner_2012"&vacancy_year=="vacancy_2012")|
      (violent_year=="violent_count13"&owner_year=="owner_2013"&vacancy_year=="vacancy_2013")|
      (violent_year=="violent_count14"&owner_year=="owner_2014"&vacancy_year=="vacancy_2014")|
      (violent_year=="violent_count15"&owner_year=="owner_2015"&vacancy_year=="vacancy_2015")
  )
#baseline tree measure ----------------------------------------------------------
bfccs_long_hscar <- bfccs_long_hscar%>%
  pivot_longer(
    cols = c(
      # White_2006, 
      White_2007, White_2008, White_2009, White_2010, White_2011, White_2012,
      White_2013, White_2014, White_2015
    ),
    names_to = "White_year",
    values_to = "White_value"
  )%>%
  pivot_longer(
    cols = c(
      # Black_2006, 
      Black_2007, Black_2008, Black_2009, Black_2010, Black_2011, Black_2012,
      Black_2013, Black_2014, Black_2015
    ),
    names_to = "Black_year",
    values_to = "Black_value"
  )%>%
  filter(
    # (violent_year=="violent_count06" & White_year=="White_2006" & Black_year=="Black_2006") |
    (violent_year=="violent_count07" & White_year=="White_2007" & Black_year=="Black_2007") |
      (violent_year=="violent_count08" & White_year=="White_2008" & Black_year=="Black_2008") |
      (violent_year=="violent_count09" & White_year=="White_2009" & Black_year=="Black_2009") |
      (violent_year=="violent_count10" & White_year=="White_2010" & Black_year=="Black_2010") |
      (violent_year=="violent_count11" & White_year=="White_2011" & Black_year=="Black_2011") |
      (violent_year=="violent_count12" & White_year=="White_2012" & Black_year=="Black_2012") |
      (violent_year=="violent_count13" & White_year=="White_2013" & Black_year=="Black_2013") |
      (violent_year=="violent_count14" & White_year=="White_2014" & Black_year=="Black_2014") |
      (violent_year=="violent_count15" & White_year=="White_2015" & Black_year=="Black_2015")
  )

bfccs_long_hscar <- bfccs_long_hscar%>%
  pivot_longer(
    cols = c(
      # AIAN_2006, 
      AIAN_2007, AIAN_2008, AIAN_2009, AIAN_2010, AIAN_2011, AIAN_2012, AIAN_2013,
      AIAN_2014, AIAN_2015
    ),
    names_to = "AIAN_year",
    values_to = "AIAN_value"
  )%>%
  pivot_longer(
    cols = c(
      # Asian_2006, 
      Asian_2007, Asian_2008, Asian_2009, Asian_2010, Asian_2011, Asian_2012,
      Asian_2013, Asian_2014, Asian_2015
    ),
    names_to = "Asian_year",
    values_to = "Asian_value"
  )%>%
  filter(
    # (violent_year=="violent_count06" & AIAN_year=="AIAN_2006" & Asian_year=="Asian_2006") |
    (violent_year=="violent_count07" & AIAN_year=="AIAN_2007" & Asian_year=="Asian_2007") |
      (violent_year=="violent_count08" & AIAN_year=="AIAN_2008" & Asian_year=="Asian_2008") |
      (violent_year=="violent_count09" & AIAN_year=="AIAN_2009" & Asian_year=="Asian_2009") |
      (violent_year=="violent_count10" & AIAN_year=="AIAN_2010" & Asian_year=="Asian_2010") |
      (violent_year=="violent_count11" & AIAN_year=="AIAN_2011" & Asian_year=="Asian_2011") |
      (violent_year=="violent_count12" & AIAN_year=="AIAN_2012" & Asian_year=="Asian_2012") |
      (violent_year=="violent_count13" & AIAN_year=="AIAN_2013" & Asian_year=="Asian_2013") |
      (violent_year=="violent_count14" & AIAN_year=="AIAN_2014" & Asian_year=="Asian_2014") |
      (violent_year=="violent_count15" & AIAN_year=="AIAN_2015" & Asian_year=="Asian_2015")
  )

bfccs_long_hscar <- bfccs_long_hscar%>%
  pivot_longer(
    cols = c(
      # owner_2006, 
      owner_2007, owner_2008, owner_2009, owner_2010, owner_2011, owner_2012, owner_2013,
      owner_2014, owner_2015
    ),
    names_to = "owner_year",
    values_to = "owner_value"
  )%>%
  pivot_longer(
    cols = c(
      # vacancy_2006, 
      vacancy_2007, vacancy_2008, vacancy_2009, vacancy_2010, vacancy_2011, vacancy_2012,
      vacancy_2013, vacancy_2014, vacancy_2015
    ),
    names_to = "vacancy_year",
    values_to = "vacancy_value"
  )%>%
  filter(
    # (violent_year=="violent_count06"&owner_year=="owner_2006"&vacancy_year=="vacancy_2006")|
    (violent_year=="violent_count07"&owner_year=="owner_2007"&vacancy_year=="vacancy_2007")|
      (violent_year=="violent_count08"&owner_year=="owner_2008"&vacancy_year=="vacancy_2008")|
      (violent_year=="violent_count09"&owner_year=="owner_2009"&vacancy_year=="vacancy_2009")|
      (violent_year=="violent_count10"&owner_year=="owner_2010"&vacancy_year=="vacancy_2010")|
      (violent_year=="violent_count11"&owner_year=="owner_2011"&vacancy_year=="vacancy_2011")|
      (violent_year=="violent_count12"&owner_year=="owner_2012"&vacancy_year=="vacancy_2012")|
      (violent_year=="violent_count13"&owner_year=="owner_2013"&vacancy_year=="vacancy_2013")|
      (violent_year=="violent_count14"&owner_year=="owner_2014"&vacancy_year=="vacancy_2014")|
      (violent_year=="violent_count15"&owner_year=="owner_2015"&vacancy_year=="vacancy_2015")
  )

#Income grouping-----------------------------------------------------------------
bfccs_long_high <- bfccs_long_high%>%
  pivot_longer(
    cols = c(
      # White_2006, 
      White_2007, White_2008, White_2009, White_2010, White_2011, White_2012,
      White_2013, White_2014, White_2015
    ),
    names_to = "White_year",
    values_to = "White_value"
  )%>%
  pivot_longer(
    cols = c(
      # Black_2006, 
      Black_2007, Black_2008, Black_2009, Black_2010, Black_2011, Black_2012,
      Black_2013, Black_2014, Black_2015
    ),
    names_to = "Black_year",
    values_to = "Black_value"
  )%>%
  filter(
    # (violent_year=="violent_count06" & White_year=="White_2006" & Black_year=="Black_2006") |
    (violent_year=="violent_count07" & White_year=="White_2007" & Black_year=="Black_2007") |
      (violent_year=="violent_count08" & White_year=="White_2008" & Black_year=="Black_2008") |
      (violent_year=="violent_count09" & White_year=="White_2009" & Black_year=="Black_2009") |
      (violent_year=="violent_count10" & White_year=="White_2010" & Black_year=="Black_2010") |
      (violent_year=="violent_count11" & White_year=="White_2011" & Black_year=="Black_2011") |
      (violent_year=="violent_count12" & White_year=="White_2012" & Black_year=="Black_2012") |
      (violent_year=="violent_count13" & White_year=="White_2013" & Black_year=="Black_2013") |
      (violent_year=="violent_count14" & White_year=="White_2014" & Black_year=="Black_2014") |
      (violent_year=="violent_count15" & White_year=="White_2015" & Black_year=="Black_2015")
  )

bfccs_long_high <- bfccs_long_high%>%
  pivot_longer(
    cols = c(
      # AIAN_2006, 
      AIAN_2007, AIAN_2008, AIAN_2009, AIAN_2010, AIAN_2011, AIAN_2012, AIAN_2013,
      AIAN_2014, AIAN_2015
    ),
    names_to = "AIAN_year",
    values_to = "AIAN_value"
  )%>%
  pivot_longer(
    cols = c(
      # Asian_2006, 
      Asian_2007, Asian_2008, Asian_2009, Asian_2010, Asian_2011, Asian_2012,
      Asian_2013, Asian_2014, Asian_2015
    ),
    names_to = "Asian_year",
    values_to = "Asian_value"
  )%>%
  filter(
    # (violent_year=="violent_count06" & AIAN_year=="AIAN_2006" & Asian_year=="Asian_2006") |
    (violent_year=="violent_count07" & AIAN_year=="AIAN_2007" & Asian_year=="Asian_2007") |
      (violent_year=="violent_count08" & AIAN_year=="AIAN_2008" & Asian_year=="Asian_2008") |
      (violent_year=="violent_count09" & AIAN_year=="AIAN_2009" & Asian_year=="Asian_2009") |
      (violent_year=="violent_count10" & AIAN_year=="AIAN_2010" & Asian_year=="Asian_2010") |
      (violent_year=="violent_count11" & AIAN_year=="AIAN_2011" & Asian_year=="Asian_2011") |
      (violent_year=="violent_count12" & AIAN_year=="AIAN_2012" & Asian_year=="Asian_2012") |
      (violent_year=="violent_count13" & AIAN_year=="AIAN_2013" & Asian_year=="Asian_2013") |
      (violent_year=="violent_count14" & AIAN_year=="AIAN_2014" & Asian_year=="Asian_2014") |
      (violent_year=="violent_count15" & AIAN_year=="AIAN_2015" & Asian_year=="Asian_2015")
  )

bfccs_long_high <- bfccs_long_high%>%
  pivot_longer(
    cols = c(
      # owner_2006, 
      owner_2007, owner_2008, owner_2009, owner_2010, owner_2011, owner_2012, owner_2013,
      owner_2014, owner_2015
    ),
    names_to = "owner_year",
    values_to = "owner_value"
  )%>%
  pivot_longer(
    cols = c(
      # vacancy_2006, 
      vacancy_2007, vacancy_2008, vacancy_2009, vacancy_2010, vacancy_2011, vacancy_2012,
      vacancy_2013, vacancy_2014, vacancy_2015
    ),
    names_to = "vacancy_year",
    values_to = "vacancy_value"
  )%>%
  filter(
    # (violent_year=="violent_count06"&owner_year=="owner_2006"&vacancy_year=="vacancy_2006")|
    (violent_year=="violent_count07"&owner_year=="owner_2007"&vacancy_year=="vacancy_2007")|
      (violent_year=="violent_count08"&owner_year=="owner_2008"&vacancy_year=="vacancy_2008")|
      (violent_year=="violent_count09"&owner_year=="owner_2009"&vacancy_year=="vacancy_2009")|
      (violent_year=="violent_count10"&owner_year=="owner_2010"&vacancy_year=="vacancy_2010")|
      (violent_year=="violent_count11"&owner_year=="owner_2011"&vacancy_year=="vacancy_2011")|
      (violent_year=="violent_count12"&owner_year=="owner_2012"&vacancy_year=="vacancy_2012")|
      (violent_year=="violent_count13"&owner_year=="owner_2013"&vacancy_year=="vacancy_2013")|
      (violent_year=="violent_count14"&owner_year=="owner_2014"&vacancy_year=="vacancy_2014")|
      (violent_year=="violent_count15"&owner_year=="owner_2015"&vacancy_year=="vacancy_2015")
  )

#Racial grouping-----------------------------------------------------------------
bfccs_long_Mixed <- bfccs_long_Mixed%>%
  pivot_longer(
    cols = c(
      # White_2006, 
      White_2007, White_2008, White_2009, White_2010, White_2011, White_2012,
      White_2013, White_2014, White_2015
    ),
    names_to = "White_year",
    values_to = "White_value"
  )%>%
  pivot_longer(
    cols = c(
      # Black_2006, 
      Black_2007, Black_2008, Black_2009, Black_2010, Black_2011, Black_2012,
      Black_2013, Black_2014, Black_2015
    ),
    names_to = "Black_year",
    values_to = "Black_value"
  )%>%
  filter(
    # (violent_year=="violent_count06" & White_year=="White_2006" & Black_year=="Black_2006") |
    (violent_year=="violent_count07" & White_year=="White_2007" & Black_year=="Black_2007") |
      (violent_year=="violent_count08" & White_year=="White_2008" & Black_year=="Black_2008") |
      (violent_year=="violent_count09" & White_year=="White_2009" & Black_year=="Black_2009") |
      (violent_year=="violent_count10" & White_year=="White_2010" & Black_year=="Black_2010") |
      (violent_year=="violent_count11" & White_year=="White_2011" & Black_year=="Black_2011") |
      (violent_year=="violent_count12" & White_year=="White_2012" & Black_year=="Black_2012") |
      (violent_year=="violent_count13" & White_year=="White_2013" & Black_year=="Black_2013") |
      (violent_year=="violent_count14" & White_year=="White_2014" & Black_year=="Black_2014") |
      (violent_year=="violent_count15" & White_year=="White_2015" & Black_year=="Black_2015")
  )

bfccs_long_Mixed <- bfccs_long_Mixed%>%
  pivot_longer(
    cols = c(
      # AIAN_2006, 
      AIAN_2007, AIAN_2008, AIAN_2009, AIAN_2010, AIAN_2011, AIAN_2012, AIAN_2013,
      AIAN_2014, AIAN_2015
    ),
    names_to = "AIAN_year",
    values_to = "AIAN_value"
  )%>%
  pivot_longer(
    cols = c(
      # Asian_2006, 
      Asian_2007, Asian_2008, Asian_2009, Asian_2010, Asian_2011, Asian_2012,
      Asian_2013, Asian_2014, Asian_2015
    ),
    names_to = "Asian_year",
    values_to = "Asian_value"
  )%>%
  filter(
    # (violent_year=="violent_count06" & AIAN_year=="AIAN_2006" & Asian_year=="Asian_2006") |
    (violent_year=="violent_count07" & AIAN_year=="AIAN_2007" & Asian_year=="Asian_2007") |
      (violent_year=="violent_count08" & AIAN_year=="AIAN_2008" & Asian_year=="Asian_2008") |
      (violent_year=="violent_count09" & AIAN_year=="AIAN_2009" & Asian_year=="Asian_2009") |
      (violent_year=="violent_count10" & AIAN_year=="AIAN_2010" & Asian_year=="Asian_2010") |
      (violent_year=="violent_count11" & AIAN_year=="AIAN_2011" & Asian_year=="Asian_2011") |
      (violent_year=="violent_count12" & AIAN_year=="AIAN_2012" & Asian_year=="Asian_2012") |
      (violent_year=="violent_count13" & AIAN_year=="AIAN_2013" & Asian_year=="Asian_2013") |
      (violent_year=="violent_count14" & AIAN_year=="AIAN_2014" & Asian_year=="Asian_2014") |
      (violent_year=="violent_count15" & AIAN_year=="AIAN_2015" & Asian_year=="Asian_2015")
  )

bfccs_long_Mixed <- bfccs_long_Mixed%>%
  pivot_longer(
    cols = c(
      # owner_2006, 
      owner_2007, owner_2008, owner_2009, owner_2010, owner_2011, owner_2012, owner_2013,
      owner_2014, owner_2015
    ),
    names_to = "owner_year",
    values_to = "owner_value"
  )%>%
  pivot_longer(
    cols = c(
      # vacancy_2006, 
      vacancy_2007, vacancy_2008, vacancy_2009, vacancy_2010, vacancy_2011, vacancy_2012,
      vacancy_2013, vacancy_2014, vacancy_2015
    ),
    names_to = "vacancy_year",
    values_to = "vacancy_value"
  )%>%
  filter(
    # (violent_year=="violent_count06"&owner_year=="owner_2006"&vacancy_year=="vacancy_2006")|
    (violent_year=="violent_count07"&owner_year=="owner_2007"&vacancy_year=="vacancy_2007")|
      (violent_year=="violent_count08"&owner_year=="owner_2008"&vacancy_year=="vacancy_2008")|
      (violent_year=="violent_count09"&owner_year=="owner_2009"&vacancy_year=="vacancy_2009")|
      (violent_year=="violent_count10"&owner_year=="owner_2010"&vacancy_year=="vacancy_2010")|
      (violent_year=="violent_count11"&owner_year=="owner_2011"&vacancy_year=="vacancy_2011")|
      (violent_year=="violent_count12"&owner_year=="owner_2012"&vacancy_year=="vacancy_2012")|
      (violent_year=="violent_count13"&owner_year=="owner_2013"&vacancy_year=="vacancy_2013")|
      (violent_year=="violent_count14"&owner_year=="owner_2014"&vacancy_year=="vacancy_2014")|
      (violent_year=="violent_count15"&owner_year=="owner_2015"&vacancy_year=="vacancy_2015")
  )
#Crime grouping -----------------------------------------------------------------
bfccs_long_lc <- bfccs_long_lc%>%
  pivot_longer(
    cols = c(
      # White_2006, 
      White_2007, White_2008, White_2009, White_2010, White_2011, White_2012,
      White_2013, White_2014, White_2015
    ),
    names_to = "White_year",
    values_to = "White_value"
  )%>%
  pivot_longer(
    cols = c(
      # Black_2006, 
      Black_2007, Black_2008, Black_2009, Black_2010, Black_2011, Black_2012,
      Black_2013, Black_2014, Black_2015
    ),
    names_to = "Black_year",
    values_to = "Black_value"
  )%>%
  filter(
    # (violent_year=="violent_count06" & White_year=="White_2006" & Black_year=="Black_2006") |
    (violent_year=="violent_count07" & White_year=="White_2007" & Black_year=="Black_2007") |
      (violent_year=="violent_count08" & White_year=="White_2008" & Black_year=="Black_2008") |
      (violent_year=="violent_count09" & White_year=="White_2009" & Black_year=="Black_2009") |
      (violent_year=="violent_count10" & White_year=="White_2010" & Black_year=="Black_2010") |
      (violent_year=="violent_count11" & White_year=="White_2011" & Black_year=="Black_2011") |
      (violent_year=="violent_count12" & White_year=="White_2012" & Black_year=="Black_2012") |
      (violent_year=="violent_count13" & White_year=="White_2013" & Black_year=="Black_2013") |
      (violent_year=="violent_count14" & White_year=="White_2014" & Black_year=="Black_2014") |
      (violent_year=="violent_count15" & White_year=="White_2015" & Black_year=="Black_2015")
  )

bfccs_long_lc <- bfccs_long_lc%>%
  pivot_longer(
    cols = c(
      # AIAN_2006, 
      AIAN_2007, AIAN_2008, AIAN_2009, AIAN_2010, AIAN_2011, AIAN_2012, AIAN_2013,
      AIAN_2014, AIAN_2015
    ),
    names_to = "AIAN_year",
    values_to = "AIAN_value"
  )%>%
  pivot_longer(
    cols = c(
      # Asian_2006, 
      Asian_2007, Asian_2008, Asian_2009, Asian_2010, Asian_2011, Asian_2012,
      Asian_2013, Asian_2014, Asian_2015
    ),
    names_to = "Asian_year",
    values_to = "Asian_value"
  )%>%
  filter(
    # (violent_year=="violent_count06" & AIAN_year=="AIAN_2006" & Asian_year=="Asian_2006") |
    (violent_year=="violent_count07" & AIAN_year=="AIAN_2007" & Asian_year=="Asian_2007") |
      (violent_year=="violent_count08" & AIAN_year=="AIAN_2008" & Asian_year=="Asian_2008") |
      (violent_year=="violent_count09" & AIAN_year=="AIAN_2009" & Asian_year=="Asian_2009") |
      (violent_year=="violent_count10" & AIAN_year=="AIAN_2010" & Asian_year=="Asian_2010") |
      (violent_year=="violent_count11" & AIAN_year=="AIAN_2011" & Asian_year=="Asian_2011") |
      (violent_year=="violent_count12" & AIAN_year=="AIAN_2012" & Asian_year=="Asian_2012") |
      (violent_year=="violent_count13" & AIAN_year=="AIAN_2013" & Asian_year=="Asian_2013") |
      (violent_year=="violent_count14" & AIAN_year=="AIAN_2014" & Asian_year=="Asian_2014") |
      (violent_year=="violent_count15" & AIAN_year=="AIAN_2015" & Asian_year=="Asian_2015")
  )

bfccs_long_lc <- bfccs_long_lc%>%
  pivot_longer(
    cols = c(
      # owner_2006, 
      owner_2007, owner_2008, owner_2009, owner_2010, owner_2011, owner_2012, owner_2013,
      owner_2014, owner_2015
    ),
    names_to = "owner_year",
    values_to = "owner_value"
  )%>%
  pivot_longer(
    cols = c(
      # vacancy_2006, 
      vacancy_2007, vacancy_2008, vacancy_2009, vacancy_2010, vacancy_2011, vacancy_2012,
      vacancy_2013, vacancy_2014, vacancy_2015
    ),
    names_to = "vacancy_year",
    values_to = "vacancy_value"
  )%>%
  filter(
    # (violent_year=="violent_count06"&owner_year=="owner_2006"&vacancy_year=="vacancy_2006")|
    (violent_year=="violent_count07"&owner_year=="owner_2007"&vacancy_year=="vacancy_2007")|
      (violent_year=="violent_count08"&owner_year=="owner_2008"&vacancy_year=="vacancy_2008")|
      (violent_year=="violent_count09"&owner_year=="owner_2009"&vacancy_year=="vacancy_2009")|
      (violent_year=="violent_count10"&owner_year=="owner_2010"&vacancy_year=="vacancy_2010")|
      (violent_year=="violent_count11"&owner_year=="owner_2011"&vacancy_year=="vacancy_2011")|
      (violent_year=="violent_count12"&owner_year=="owner_2012"&vacancy_year=="vacancy_2012")|
      (violent_year=="violent_count13"&owner_year=="owner_2013"&vacancy_year=="vacancy_2013")|
      (violent_year=="violent_count14"&owner_year=="owner_2014"&vacancy_year=="vacancy_2014")|
      (violent_year=="violent_count15"&owner_year=="owner_2015"&vacancy_year=="vacancy_2015")
  )
#--------------------------------------------------------------------------------
#Organize panel -----------------------------------------------------------------
final_panel <- bfccs_long%>%
  mutate(
    PlntSeas = as.numeric(substring(bfccs_long$PlntSeas, first = nchar(bfccs_long$PlntSeas)-1)),
    violent_year = as.numeric(substring(bfccs_long$violent_year, first = 14))
  )%>%
  rename(
    PlntYear = PlntSeas,
    Years = violent_year,
    treated = candidate_treated,
  )%>%
  mutate(
    lagged_year = Years - PlntYear,
    post = ifelse(is.na(PlntYear), 0, ifelse(lagged_year < 0, 0, 1)),
    did = treated * post,
    PlntYear = case_when(
      PlntYear == 6~ 2006,
      PlntYear == 7~ 2007,
      PlntYear == 8~ 2008,
      PlntYear == 9~ 2009,
      PlntYear == 10~ 2010,
      PlntYear == 11~ 2011,
      PlntYear == 12~ 2012,
      PlntYear == 13~ 2013,
      PlntYear == 14~ 2014,
      PlntYear == 15~ 2015,
      PlntYear == 16~ 2016,
      PlntYear == 17~ 2017,
      PlntYear == 18~ 2018,
      PlntYear == 19~ 2019,
      PlntYear == 20~ 2020,
      PlntYear == 21~ 2021
    ),
    Years = case_when(
      Years == 6~ 2006,
      Years == 7~ 2007,
      Years == 8~ 2008,
      Years == 9~ 2009,
      Years == 10~ 2010,
      Years == 11~ 2011,
      Years == 12~ 2012,
      Years == 13~ 2013,
      Years == 14~ 2014,
      Years == 15~ 2015,
      Years == 16~ 2016,
      Years == 17~ 2017,
      Years == 18~ 2018,
      Years == 19~ 2019,
      Years == 20~ 2020,
      Years == 21~ 2021
    )
  )%>%
  dplyr::select(
    OBJECTID_1, Years, treated, post, did, PlntYear, lagged_year, violent_value,
    property_value, allcrime_value, 
    White_value, Black_value, AIAN_value, Asian_value,
    owner_value, vacancy_value, pct_education_2013, income_2013,
    tree_count05, tree_count15, tree_change, Shape_Length
  )%>%
  rename(
    violent_count = violent_value, 
    property_count = property_value,
    allcrime_count = allcrime_value
  )


final_panel <- final_panel%>%left_join(
  dplyr::select(finalmatch_tree, control_id, PlntYears_fake), 
  by = c("OBJECTID_1"="control_id")
)%>%mutate(
  post = ifelse(is.na(PlntYear), ifelse(Years < PlntYears_fake, 0, 1), post)
)

# write.csv(final_panel,"E:/did_process/data/table/data_panel/final_panel3.csv", row.names=FALSE)


final_panel_para <- bfccs_long_para%>%
  mutate(
    PlntSeas = as.numeric(substring(bfccs_long_para$PlntSeas, first = nchar(bfccs_long_para$PlntSeas)-1)),
    violent_year = as.numeric(substring(bfccs_long_para$violent_year, first = 14))
  )%>%
  rename(
    PlntYear = PlntSeas,
    Years = violent_year,
    treated = candidate_treated,
  )%>%
  mutate(
    lagged_year = Years - PlntYear,
    post = ifelse(is.na(PlntYear), 0, ifelse(lagged_year < 0, 0, 1)),
    did = treated * post,
    PlntYear = case_when(
      PlntYear == 6~ 2006,
      PlntYear == 7~ 2007,
      PlntYear == 8~ 2008,
      PlntYear == 9~ 2009,
      PlntYear == 10~ 2010,
      PlntYear == 11~ 2011,
      PlntYear == 12~ 2012,
      PlntYear == 13~ 2013,
      PlntYear == 14~ 2014,
      PlntYear == 15~ 2015,
      PlntYear == 16~ 2016,
      PlntYear == 17~ 2017,
      PlntYear == 18~ 2018,
      PlntYear == 19~ 2019,
      PlntYear == 20~ 2020,
      PlntYear == 21~ 2021
    ),
    Years = case_when(
      Years == 6~ 2006,
      Years == 7~ 2007,
      Years == 8~ 2008,
      Years == 9~ 2009,
      Years == 10~ 2010,
      Years == 11~ 2011,
      Years == 12~ 2012,
      Years == 13~ 2013,
      Years == 14~ 2014,
      Years == 15~ 2015,
      Years == 16~ 2016,
      Years == 17~ 2017,
      Years == 18~ 2018,
      Years == 19~ 2019,
      Years == 20~ 2020,
      Years == 21~ 2021
    )
  )%>%
  dplyr::select(
    OBJECTID_1, Years, treated, post, did, PlntYear, lagged_year, violent_value,
    property_value, allcrime_value, 
    White_value, Black_value, AIAN_value, Asian_value,
    owner_value, vacancy_value, pct_education_2013, income_2013,
    tree_count05, tree_count15, tree_change, Shape_Length
  )%>%
  rename(
    violent_count = violent_value, 
    property_count = property_value,
    allcrime_count = allcrime_value
  )

write.csv(final_panel_para,"E:/did_process/revise/data panel/final_panel_para.csv", row.names=FALSE)
write.csv(final_panel_para,"E:/did_process/data/table/data_panel/final_panel_para.csv", row.names=FALSE)

#Stewardship grouping------------------------------------------------------------
fp_stewardship <- final_panel%>%
  left_join(
    dplyr::select(bfcandis_tree15count, OBJECTID_1, stewardship),
    by = "OBJECTID_1"
  )


fp_stewardship <- fp_stewardship%>%
  mutate(
    stew_3group = case_when(
      OBJECTID_1 %in% finalmatch_stew_1$treat_id ~ "1/3",
      OBJECTID_1 %in% finalmatch_stew_2$treat_id ~ "2/3",
      OBJECTID_1 %in% finalmatch_stew_3$treat_id ~ "3/3",
      TRUE ~ "none"
    ),
    stew_4group = case_when(
      OBJECTID_1 %in% finalmatch_stew_41$treat_id ~ "1/4",
      OBJECTID_1 %in% finalmatch_stew_42$treat_id ~ "2/4",
      OBJECTID_1 %in% finalmatch_stew_43$treat_id ~ "3/4",
      OBJECTID_1 %in% finalmatch_stew_44$treat_id ~ "4/4",
      TRUE ~ "none"
    )
  )


fp_stewardship$did31 <- ifelse(
  fp_stewardship$stew_3group == "1/3",
  fp_stewardship$did,
  0
)
fp_stewardship$did32 <- ifelse(
  fp_stewardship$stew_3group == "2/3",
  fp_stewardship$did,
  0
)
fp_stewardship$did33 <- ifelse(
  fp_stewardship$stew_3group == "3/3",
  fp_stewardship$did,
  0
)


fp_stewardship$did41 <- ifelse(
  fp_stewardship$stew_4group == "1/4",
  fp_stewardship$did,
  0
)
fp_stewardship$did42 <- ifelse(
  fp_stewardship$stew_4group == "2/4",
  fp_stewardship$did,
  0
)
fp_stewardship$did43 <- ifelse(
  fp_stewardship$stew_4group == "3/4",
  fp_stewardship$did,
  0
)
fp_stewardship$did44 <- ifelse(
  fp_stewardship$stew_4group == "4/4",
  fp_stewardship$did,
  0
)

#Intensity grouping--------------------------------------------------------------
final_panel1 <- bfccs_long1%>%
  mutate(
    PlntSeas = as.numeric(substring(bfccs_long1$PlntSeas, first = nchar(bfccs_long1$PlntSeas)-1)),
    violent_year = as.numeric(substring(bfccs_long1$violent_year, first = 14))
  )%>%
  rename(
    PlntYear = PlntSeas,
    Years = violent_year,
    treated = candidate_treated,
  )%>%
  mutate(
    lagged_year = Years - PlntYear,
    post = ifelse(is.na(PlntYear), 0, ifelse(lagged_year < 0, 0, 1)),
    did = treated * post,
    PlntYear = case_when(
      # PlntYear == 6~ 2006,
      PlntYear == 7~ 2007,
      PlntYear == 8~ 2008,
      PlntYear == 9~ 2009,
      PlntYear == 10~ 2010,
      PlntYear == 11~ 2011,
      PlntYear == 12~ 2012,
      PlntYear == 13~ 2013,
      PlntYear == 14~ 2014,
      PlntYear == 15~ 2015
    ),
    Years = case_when(
      # Years == 6~ 2006,
      Years == 7~ 2007,
      Years == 8~ 2008,
      Years == 9~ 2009,
      Years == 10~ 2010,
      Years == 11~ 2011,
      Years == 12~ 2012,
      Years == 13~ 2013,
      Years == 14~ 2014,
      Years == 15~ 2015
    )
  )%>%
  dplyr::select(
    OBJECTID_1, Years, treated, post, did, PlntYear, lagged_year, violent_value,
    property_value, allcrime_value, 
    White_value, Black_value, AIAN_value, Asian_value,
    owner_value, vacancy_value, pct_education_2013, income_2013,
    tree_count05, tree_count15, tree_change, Shape_Length
  )%>%
  rename(
    violent_count = violent_value, 
    property_count = property_value,
    allcrime_count = allcrime_value
  )


fp_intensity <- final_panel%>%
  mutate(
    did_intensity = did * tree_change
  )


fp_intensity <- fp_intensity%>%
  mutate(
    tree_3group = case_when(
      OBJECTID_1 %in% finalmatch_tree_1$treat_id ~ "1/3",
      OBJECTID_1 %in% finalmatch_tree_2$treat_id ~ "2/3",
      OBJECTID_1 %in% finalmatch_tree_3$treat_id ~ "3/3",
      TRUE ~ "none"
    ),
    tree_4group = case_when(
      OBJECTID_1 %in% finalmatch_tree_41$treat_id ~ "1/4",
      OBJECTID_1 %in% finalmatch_tree_42$treat_id ~ "2/4",
      OBJECTID_1 %in% finalmatch_tree_43$treat_id ~ "3/4",
      OBJECTID_1 %in% finalmatch_tree_44$treat_id ~ "4/4",
      TRUE ~ "none"
    )
  )



fp_intensity%>%
  filter(tree_3group=="1/3")%>%
  count()
fp_intensity%>%
  filter(tree_4group=="1/4")%>%
  count()


fp_intensity$did31 <- ifelse(
  fp_intensity$tree_3group == "1/3",
  fp_intensity$did,
  0
)
fp_intensity$did32 <- ifelse(
  fp_intensity$tree_3group == "2/3",
  fp_intensity$did,
  0
)
fp_intensity$did33 <- ifelse(
  fp_intensity$tree_3group == "3/3",
  fp_intensity$did,
  0
)


fp_intensity$did41 <- ifelse(
  fp_intensity$tree_4group == "1/4",
  fp_intensity$did,
  0
)
fp_intensity$did42 <- ifelse(
  fp_intensity$tree_4group == "2/4",
  fp_intensity$did,
  0
)
fp_intensity$did43 <- ifelse(
  fp_intensity$tree_4group == "3/4",
  fp_intensity$did,
  0
)
fp_intensity$did44 <- ifelse(
  fp_intensity$tree_4group == "4/4",
  fp_intensity$did,
  0
)



#baseline tree measure ----------------------------------------------------------
final_panel_hscar <- bfccs_long_hscar%>%
  mutate(
    PlntSeas = as.numeric(substring(bfccs_long_hscar$PlntSeas, first = nchar(bfccs_long_hscar$PlntSeas)-1)),
    violent_year = as.numeric(substring(bfccs_long_hscar$violent_year, first = 14))
  )%>%
  rename(
    PlntYear = PlntSeas,
    Years = violent_year,
    treated = candidate_treated,
  )%>%
  mutate(
    lagged_year = Years - PlntYear,
    post = ifelse(is.na(PlntYear), 0, ifelse(lagged_year < 0, 0, 1)),
    did = treated * post,
    PlntYear = case_when(
      # PlntYear == 6~ 2006,
      PlntYear == 7~ 2007,
      PlntYear == 8~ 2008,
      PlntYear == 9~ 2009,
      PlntYear == 10~ 2010,
      PlntYear == 11~ 2011,
      PlntYear == 12~ 2012,
      PlntYear == 13~ 2013,
      PlntYear == 14~ 2014,
      PlntYear == 15~ 2015
    ),
    Years = case_when(
      # Years == 6~ 2006,
      Years == 7~ 2007,
      Years == 8~ 2008,
      Years == 9~ 2009,
      Years == 10~ 2010,
      Years == 11~ 2011,
      Years == 12~ 2012,
      Years == 13~ 2013,
      Years == 14~ 2014,
      Years == 15~ 2015
    )
  )%>%
  dplyr::select(
    OBJECTID_1, Years, treated, post, did, PlntYear, lagged_year, violent_value,
    property_value, allcrime_value, 
    White_value, Black_value, AIAN_value, Asian_value,
    owner_value, vacancy_value, pct_education_2013, income_2013,
    tree_count05, tree_count15, tree_change, Shape_Length
  )%>%
  rename(
    violent_count = violent_value, 
    property_count = property_value,
    allcrime_count = allcrime_value
  )

final_panel_lscar <- final_panel_lscar%>%left_join(
  dplyr::select(finalmatch_tree, control_id, PlntYears_fake), 
  by = c("OBJECTID_1"="control_id")
)%>%mutate(
  post = ifelse(is.na(PlntYear), ifelse(Years < PlntYears_fake, 0, 1), post)
)

#Income grouping-----------------------------------------------------------------
final_panel_low <- bfccs_long_low%>%
  mutate(
    PlntSeas = as.numeric(substring(bfccs_long_low$PlntSeas, first = nchar(bfccs_long_low$PlntSeas)-1)),
    violent_year = as.numeric(substring(bfccs_long_low$violent_year, first = 14))
  )%>%
  rename(
    PlntYear = PlntSeas,
    Years = violent_year,
    treated = candidate_treated,
  )%>%
  mutate(
    lagged_year = Years - PlntYear,
    post = ifelse(is.na(PlntYear), 0, ifelse(lagged_year < 0, 0, 1)),
    did = treated * post,
    PlntYear = case_when(
      # PlntYear == 6~ 2006,
      PlntYear == 7~ 2007,
      PlntYear == 8~ 2008,
      PlntYear == 9~ 2009,
      PlntYear == 10~ 2010,
      PlntYear == 11~ 2011,
      PlntYear == 12~ 2012,
      PlntYear == 13~ 2013,
      PlntYear == 14~ 2014,
      PlntYear == 15~ 2015
    ),
    Years = case_when(
      # Years == 6~ 2006,
      Years == 7~ 2007,
      Years == 8~ 2008,
      Years == 9~ 2009,
      Years == 10~ 2010,
      Years == 11~ 2011,
      Years == 12~ 2012,
      Years == 13~ 2013,
      Years == 14~ 2014,
      Years == 15~ 2015
    )
  )%>%
  dplyr::select(
    OBJECTID_1, Years, treated, post, did, PlntYear, lagged_year, violent_value,
    property_value, allcrime_value,
    White_value, Black_value, AIAN_value, Asian_value, owner_value, vacancy_value, 
    pct_education_2013, income_2013, 
    tree_count05, tree_count15, tree_change, Shape_Length
  )%>%
  rename(
    violent_count = violent_value, 
    property_count = property_value,
    allcrime_count = allcrime_value
  )


final_panel_high <- final_panel_high%>%left_join(
  dplyr::select(finalmatch_tree, control_id, PlntYears_fake), 
  by = c("OBJECTID_1"="control_id")
)%>%mutate(
  post = ifelse(is.na(PlntYear), ifelse(Years < PlntYears_fake, 0, 1), post)
)

#Racial grouping-----------------------------------------------------------------
final_panel_White <- bfccs_long_White%>%
  mutate(
    PlntSeas = as.numeric(substring(bfccs_long_White$PlntSeas, first = nchar(bfccs_long_White$PlntSeas)-1)),
    violent_year = as.numeric(substring(bfccs_long_White$violent_year, first = 14))
  )%>%
  rename(
    PlntYear = PlntSeas,
    Years = violent_year,
    treated = candidate_treated,
  )%>%
  mutate(
    lagged_year = Years - PlntYear,
    post = ifelse(is.na(PlntYear), 0, ifelse(lagged_year < 0, 0, 1)),
    did = treated * post,
    PlntYear = case_when(
      # PlntYear == 6~ 2006,
      PlntYear == 7~ 2007,
      PlntYear == 8~ 2008,
      PlntYear == 9~ 2009,
      PlntYear == 10~ 2010,
      PlntYear == 11~ 2011,
      PlntYear == 12~ 2012,
      PlntYear == 13~ 2013,
      PlntYear == 14~ 2014,
      PlntYear == 15~ 2015
    ),
    Years = case_when(
      # Years == 6~ 2006,
      Years == 7~ 2007,
      Years == 8~ 2008,
      Years == 9~ 2009,
      Years == 10~ 2010,
      Years == 11~ 2011,
      Years == 12~ 2012,
      Years == 13~ 2013,
      Years == 14~ 2014,
      Years == 15~ 2015
    )
  )%>%
  dplyr::select(
    OBJECTID_1, Years, treated, post, did, PlntYear, lagged_year, violent_value,
    property_value, allcrime_value,
    White_value, Black_value, AIAN_value, Asian_value, owner_value, vacancy_value, 
    pct_education_2013, income_2013, 
    tree_count05, tree_count15, tree_change, Shape_Length
  )%>%
  rename(
    violent_count = violent_value, 
    property_count = property_value,
    allcrime_count = allcrime_value
  )


final_panel_Mixed <- final_panel_Mixed%>%left_join(
  dplyr::select(finalmatch_tree, control_id, PlntYears_fake), 
  by = c("OBJECTID_1"="control_id")
)%>%mutate(
  post = ifelse(is.na(PlntYear), ifelse(Years < PlntYears_fake, 0, 1), post)
)

#Crime grouping------------------------------------------------------------------
final_panel_hc <- bfccs_long_hc%>%
  mutate(
    PlntSeas = as.numeric(substring(bfccs_long_hc$PlntSeas, first = nchar(bfccs_long_hc$PlntSeas)-1)),
    violent_year = as.numeric(substring(bfccs_long_hc$violent_year, first = 14))
  )%>%
  rename(
    PlntYear = PlntSeas,
    Years = violent_year,
    treated = candidate_treated,
  )%>%
  mutate(
    lagged_year = Years - PlntYear,
    post = ifelse(is.na(PlntYear), 0, ifelse(lagged_year < 0, 0, 1)),
    did = treated * post,
    PlntYear = case_when(
      # PlntYear == 6~ 2006,
      PlntYear == 7~ 2007,
      PlntYear == 8~ 2008,
      PlntYear == 9~ 2009,
      PlntYear == 10~ 2010,
      PlntYear == 11~ 2011,
      PlntYear == 12~ 2012,
      PlntYear == 13~ 2013,
      PlntYear == 14~ 2014,
      PlntYear == 15~ 2015
    ),
    Years = case_when(
      # Years == 6~ 2006,
      Years == 7~ 2007,
      Years == 8~ 2008,
      Years == 9~ 2009,
      Years == 10~ 2010,
      Years == 11~ 2011,
      Years == 12~ 2012,
      Years == 13~ 2013,
      Years == 14~ 2014,
      Years == 15~ 2015
    )
  )%>%
  dplyr::select(
    OBJECTID_1, Years, treated, post, did, PlntYear, lagged_year, violent_value,
    property_value, allcrime_value,
    White_value, Black_value, AIAN_value, Asian_value, owner_value, vacancy_value, 
    pct_education_2013, income_2013, 
    tree_count05, tree_count15, tree_change, Shape_Length
  )%>%
  rename(
    violent_count = violent_value, 
    property_count = property_value,
    allcrime_count = allcrime_value
  )


final_panel_lc <- final_panel_lc%>%left_join(
  dplyr::select(finalmatch_tree, control_id, PlntYears_fake), 
  by = c("OBJECTID_1"="control_id")
)%>%mutate(
  post = ifelse(is.na(PlntYear), ifelse(Years < PlntYears_fake, 0, 1), post)
)

#--------------------------------------------------------------------------------
#Regression----------------------------------------------------------------------

#No controlled variables
mod_did_nb_v_non <- glmer.nb(
  violent_count ~ treated + post + did 
  + offset(log(Shape_Length))
  + (1|OBJECTID_1) + (1|Years),
  data = final_panel,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(mod_did_nb_v_non)
exp(cbind(
  IRR = fixef(mod_did_nb_v_non),
  confint(mod_did_nb_v_non, parm = "beta_", method = "Wald")
))

mod_did_nb_p_non <- glmer.nb(
  property_count ~ treated + post + did 
  + offset(log(Shape_Length))
  + (1|OBJECTID_1) + (1|Years),
  data = final_panel,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(mod_did_nb_p_non)
exp(cbind(
  IRR = fixef(mod_did_nb_p_non),
  confint(mod_did_nb_p_non, parm = "beta_", method = "Wald")
))

mod_did_nb_a_non <- glmer.nb(
  allcrime_count ~ treated + post + did 
  + offset(log(Shape_Length))
  + (1|OBJECTID_1) + (1|Years),
  data = final_panel,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(mod_did_nb_a_non)
exp(cbind(
  IRR = fixef(mod_did_nb_a_non),
  confint(mod_did_nb_a_non, parm = "beta_", method = "Wald")
))


#Controlled variables
mod_did_nb_v <- glmer.nb(
  violent_count ~ treated + post + did 
  + White_value + owner_value + vacancy_value + offset(log(Shape_Length))
  + (1|OBJECTID_1) + (1|Years),
  data = final_panel,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(mod_did_nb_v)
exp(cbind(
  IRR = fixef(mod_did_nb_v),
  confint(mod_did_nb_v, parm = "beta_", method = "Wald")
))

mod_did_nb_p <- glmer.nb(
  property_count ~ treated + post + did 
  + White_value + owner_value + vacancy_value + offset(log(Shape_Length))
  + (1|OBJECTID_1) + (1|Years),
  data = final_panel,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(mod_did_nb_p)
exp(cbind(
  IRR = fixef(mod_did_nb_p),
  confint(mod_did_nb_p, parm = "beta_", method = "Wald")
))

mod_did_nb_a <- glmer.nb(
  allcrime_count ~ treated + post + did 
  + White_value + owner_value + vacancy_value + offset(log(Shape_Length))
  + (1|OBJECTID_1) + (1|Years),
  data = final_panel,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(mod_did_nb_a)
exp(cbind(
  IRR = fixef(mod_did_nb_a),
  confint(mod_did_nb_a, parm = "beta_", method = "Wald")
))

#Stewardship grouping------------------------------------------------------------

#tertile
mod_did_nb_stew_v2 <- glmer.nb(
  violent_count ~ treated + post + did31 + did32+ did33 + 
    White_value + owner_value + vacancy_value + offset(log(Shape_Length))
  + (1|OBJECTID_1) + (1|Years),
  data = fp_stewardship,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(mod_did_nb_stew_v2)
exp(cbind(
  IRR = fixef(mod_did_nb_stew_v2),
  confint(mod_did_nb_stew_v2, parm = "beta_", method = "Wald")
))

mod_did_nb_stew_p2 <- glmer.nb(
  property_count ~ treated + post + did31 + did32+ did33 + 
    White_value + owner_value + vacancy_value + offset(log(Shape_Length))
  + (1|OBJECTID_1) + (1|Years),
  data = fp_stewardship,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(mod_did_nb_stew_p2)
exp(cbind(
  IRR = fixef(mod_did_nb_stew_p2),
  confint(mod_did_nb_stew_p2, parm = "beta_", method = "Wald")
))

mod_did_nb_stew_a2 <- glmer.nb(
  allcrime_count ~ treated + post + did31 + did32+ did33 + 
    White_value + owner_value + vacancy_value + offset(log(Shape_Length))
  + (1|OBJECTID_1) + (1|Years),
  data = fp_stewardship,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(mod_did_nb_stew_a2)
exp(cbind(
  IRR = fixef(mod_did_nb_stew_a2),
  confint(mod_did_nb_stew_a2, parm = "beta_", method = "Wald")
))

#quartile
mod_did_nb_stew_v3 <- glmer.nb(
  violent_count ~ treated + post + did41 + did42+ did43 + did44 +
    White_value + owner_value + vacancy_value + offset(log(Shape_Length))
  + (1|OBJECTID_1) + (1|Years),
  data = fp_stewardship,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(mod_did_nb_stew_v3)
exp(cbind(
  IRR = fixef(mod_did_nb_stew_v3),
  confint(mod_did_nb_stew_v3, parm = "beta_", method = "Wald")
))

mod_did_nb_stew_p3 <- glmer.nb(
  property_count ~ treated + post + did41 + did42+ did43 + did44 +
    White_value + owner_value + vacancy_value + offset(log(Shape_Length))
  + (1|OBJECTID_1) + (1|Years),
  data = fp_stewardship,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(mod_did_nb_stew_p3)
exp(cbind(
  IRR = fixef(mod_did_nb_stew_p3),
  confint(mod_did_nb_stew_p3, parm = "beta_", method = "Wald")
))

mod_did_nb_stew_a3 <- glmer.nb(
  allcrime_count ~ treated + post + did41 + did42+ did43 + did44 +
    White_value + owner_value + vacancy_value + offset(log(Shape_Length))
  + (1|OBJECTID_1) + (1|Years),
  data = fp_stewardship,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(mod_did_nb_stew_a3)
exp(cbind(
  IRR = fixef(mod_did_nb_stew_a3),
  confint(mod_did_nb_stew_a3, parm = "beta_", method = "Wald")
))
#Intensity grouping--------------------------------------------------------------

#tertile
mod_did_nb_inten_v2 <- glmer.nb(
  violent_count ~ treated + post + did31 + did32+ did33 + 
    White_value + owner_value + vacancy_value + offset(log(Shape_Length))
  + (1|OBJECTID_1) + (1|Years),
  data = fp_intensity,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(mod_did_nb_inten_v2)
exp(cbind(
  IRR = fixef(mod_did_nb_inten_v2),
  confint(mod_did_nb_inten_v2, parm = "beta_", method = "Wald")
))

mod_did_nb_inten_p2 <- glmer.nb(
  property_count ~ treated + post + did31 + did32+ did33 + 
    White_value + owner_value + vacancy_value + offset(log(Shape_Length))
  + (1|OBJECTID_1) + (1|Years),
  data = fp_intensity,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(mod_did_nb_inten_p2)
exp(cbind(
  IRR = fixef(mod_did_nb_inten_p2),
  confint(mod_did_nb_inten_p2, parm = "beta_", method = "Wald")
))

mod_did_nb_inten_a2 <- glmer.nb(
  allcrime_count ~ treated + post + did31 + did32+ did33 + 
    White_value + owner_value + vacancy_value + offset(log(Shape_Length))
  + (1|OBJECTID_1) + (1|Years),
  data = fp_intensity,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(mod_did_nb_inten_a2)
exp(cbind(
  IRR = fixef(mod_did_nb_inten_p2),
  confint(mod_did_nb_inten_p2, parm = "beta_", method = "Wald")
))

#quartile
mod_did_nb_inten_v3 <- glmer.nb(
  violent_count ~ treated + post + did41 + did42+ did43 + did44 +
    White_value + owner_value + vacancy_value + offset(log(Shape_Length))
  + (1|OBJECTID_1) + (1|Years),
  data = fp_intensity,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(mod_did_nb_inten_v3)
exp(cbind(
  IRR = fixef(mod_did_nb_inten_v3),
  confint(mod_did_nb_inten_v3, parm = "beta_", method = "Wald")
))

mod_did_nb_inten_p3 <- glmer.nb(
  property_count ~ treated + post + did41 + did42+ did43 + did44 +
    White_value + owner_value + vacancy_value + offset(log(Shape_Length))
  + (1|OBJECTID_1) + (1|Years),
  data = fp_intensity,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(mod_did_nb_inten_p3)
exp(cbind(
  IRR = fixef(mod_did_nb_inten_p3),
  confint(mod_did_nb_inten_p3, parm = "beta_", method = "Wald")
))

mod_did_nb_inten_a3 <- glmer.nb(
  allcrime_count ~ treated + post + did41 + did42+ did43 + did44 +
    White_value + owner_value + vacancy_value + offset(log(Shape_Length))
  + (1|OBJECTID_1) + (1|Years),
  data = fp_intensity,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(mod_did_nb_inten_a3)
exp(cbind(
  IRR = fixef(mod_did_nb_inten_a3),
  confint(mod_did_nb_inten_a3, parm = "beta_", method = "Wald")
))


#baseline tree measure ----------------------------------------------------------
#low scarity
mod_did_nb_lscar_v <- glmer.nb(
  violent_count ~ treated + post + did + 
    White_value + owner_value + vacancy_value + offset(log(Shape_Length))
  + (1|OBJECTID_1) + (1|Years),
  data = final_panel_lscar,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(mod_did_nb_lscar_v)
exp(cbind(
  IRR = fixef(mod_did_nb_lscar_v),
  confint(mod_did_nb_lscar_v, parm = "beta_", method = "Wald")
))

mod_did_nb_lscar_p <- glmer.nb(
  property_count ~ treated + post + did + 
    White_value + owner_value + vacancy_value + offset(log(Shape_Length))
  + (1|OBJECTID_1) + (1|Years),
  data = final_panel_lscar,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(mod_did_nb_lscar_p)
exp(cbind(
  IRR = fixef(mod_did_nb_lscar_p),
  confint(mod_did_nb_lscar_p, parm = "beta_", method = "Wald")
))

mod_did_nb_lscar_a <- glmer.nb(
  allcrime_count ~ treated + post + did + 
    White_value + owner_value + vacancy_value + offset(log(Shape_Length))
  + (1|OBJECTID_1) + (1|Years),
  data = final_panel_lscar,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(mod_did_nb_lscar_a)
exp(cbind(
  IRR = fixef(mod_did_nb_lscar_a),
  confint(mod_did_nb_lscar_a, parm = "beta_", method = "Wald")
))

#middle scarity
mod_did_nb_mscar_v <- glmer.nb(
  violent_count ~ treated + post + did + 
    White_value + owner_value + vacancy_value + offset(log(Shape_Length))
  + (1|OBJECTID_1) + (1|Years),
  data = final_panel_mscar,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(mod_did_nb_mscar_v)
exp(cbind(
  IRR = fixef(mod_did_nb_mscar_v),
  confint(mod_did_nb_mscar_v, parm = "beta_", method = "Wald")
))

mod_did_nb_mscar_p <- glmer.nb(
  property_count ~ treated + post + did + 
    White_value + owner_value + vacancy_value + offset(log(Shape_Length))
  + (1|OBJECTID_1) + (1|Years),
  data = final_panel_mscar,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(mod_did_nb_mscar_p)
exp(cbind(
  IRR = fixef(mod_did_nb_mscar_p),
  confint(mod_did_nb_mscar_p, parm = "beta_", method = "Wald")
))

mod_did_nb_mscar_a <- glmer.nb(
  allcrime_count ~ treated + post + did + 
    White_value + owner_value + vacancy_value + offset(log(Shape_Length))
  + (1|OBJECTID_1) + (1|Years),
  data = final_panel_mscar,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(mod_did_nb_mscar_a)
exp(cbind(
  IRR = fixef(mod_did_nb_mscar_a),
  confint(mod_did_nb_mscar_a, parm = "beta_", method = "Wald")
))

#high scarity
mod_did_nb_hscar_v <- glmer.nb(
  violent_count ~ treated + post + did + 
    White_value + owner_value + vacancy_value + offset(log(Shape_Length))
  + (1|OBJECTID_1) + (1|Years),
  data = final_panel_hscar,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(mod_did_nb_hscar_v)
exp(cbind(
  IRR = fixef(mod_did_nb_hscar_v),
  confint(mod_did_nb_hscar_v, parm = "beta_", method = "Wald")
))

mod_did_nb_hscar_p <- glmer.nb(
  property_count ~ treated + post + did + 
    White_value + owner_value + vacancy_value + offset(log(Shape_Length))
  + (1|OBJECTID_1) + (1|Years),
  data = final_panel_hscar,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(mod_did_nb_hscar_p)
exp(cbind(
  IRR = fixef(mod_did_nb_hscar_p),
  confint(mod_did_nb_hscar_p, parm = "beta_", method = "Wald")
))

mod_did_nb_hscar_a <- glmer.nb(
  allcrime_count ~ treated + post + did + 
    White_value + owner_value + vacancy_value + offset(log(Shape_Length))
  + (1|OBJECTID_1) + (1|Years),
  data = final_panel_hscar,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(mod_did_nb_hscar_a)
exp(cbind(
  IRR = fixef(mod_did_nb_hscar_a),
  confint(mod_did_nb_hscar_a, parm = "beta_", method = "Wald")
))

#Income grouping-----------------------------------------------------------------
#high
mod_did_nb_high_v <- glmer.nb(
  violent_count ~ treated + post + did + 
    White_value + owner_value + vacancy_value + offset(log(Shape_Length))
  + (1|OBJECTID_1) + (1|Years),
  data = final_panel_high,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(mod_did_nb_high_v)
exp(cbind(
  IRR = fixef(mod_did_nb_high_v),
  confint(mod_did_nb_high_v, parm = "beta_", method = "Wald")
))

mod_did_nb_high_p <- glmer.nb(
  property_count ~ treated + post + did + 
    White_value + owner_value + vacancy_value + offset(log(Shape_Length))
  + (1|OBJECTID_1) + (1|Years),
  data = final_panel_high,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(mod_did_nb_high_p)
exp(cbind(
  IRR = fixef(mod_did_nb_high_p),
  confint(mod_did_nb_high_p, parm = "beta_", method = "Wald")
))

mod_did_nb_high_a <- glmer.nb(
  allcrime_count ~ treated + post + did + 
    White_value + owner_value + vacancy_value + offset(log(Shape_Length))
  + (1|OBJECTID_1) + (1|Years),
  data = final_panel_high,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(mod_did_nb_high_a)
exp(cbind(
  IRR = fixef(mod_did_nb_high_a),
  confint(mod_did_nb_high_a, parm = "beta_", method = "Wald")
))

#middle
mod_did_nb_med_v <- glmer.nb(
  violent_count ~ treated + post + did + 
    White_value + owner_value + vacancy_value + offset(log(Shape_Length))
  + (1|OBJECTID_1) + (1|Years),
  data = final_panel_med,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(mod_did_nb_med_v)
exp(cbind(
  IRR = fixef(mod_did_nb_med_v),
  confint(mod_did_nb_med_v, parm = "beta_", method = "Wald")
))

mod_did_nb_med_p <- glmer.nb(
  property_count ~ treated + post + did + 
    White_value + owner_value + vacancy_value + offset(log(Shape_Length))
  + (1|OBJECTID_1) + (1|Years),
  data = final_panel_med,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(mod_did_nb_med_p)
exp(cbind(
  IRR = fixef(mod_did_nb_med_p),
  confint(mod_did_nb_med_p, parm = "beta_", method = "Wald")
))

mod_did_nb_med_a <- glmer.nb(
  allcrime_count ~ treated + post + did + 
    White_value + owner_value + vacancy_value + offset(log(Shape_Length))
  + (1|OBJECTID_1) + (1|Years),
  data = final_panel_med,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(mod_did_nb_med_a)
exp(cbind(
  IRR = fixef(mod_did_nb_med_a),
  confint(mod_did_nb_med_a, parm = "beta_", method = "Wald")
))

#low
mod_did_nb_low_v <- glmer.nb(
  violent_count ~ treated + post + did + 
    White_value + owner_value + vacancy_value + offset(log(Shape_Length))
  + (1|OBJECTID_1) + (1|Years),
  data = final_panel_low,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(mod_did_nb_low_v)
exp(cbind(
  IRR = fixef(mod_did_nb_low_v),
  confint(mod_did_nb_low_v, parm = "beta_", method = "Wald")
))

mod_did_nb_low_p <- glmer.nb(
  property_count ~ treated + post + did + 
    White_value + owner_value + vacancy_value + offset(log(Shape_Length))
  + (1|OBJECTID_1) + (1|Years),
  data = final_panel_low,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(mod_did_nb_low_p)
exp(cbind(
  IRR = fixef(mod_did_nb_low_p),
  confint(mod_did_nb_low_p, parm = "beta_", method = "Wald")
))

mod_did_nb_low_a <- glmer.nb(
  allcrime_count ~ treated + post + did + 
    White_value + owner_value + vacancy_value + offset(log(Shape_Length))
  + (1|OBJECTID_1) + (1|Years),
  data = final_panel_low,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(mod_did_nb_low_a)
exp(cbind(
  IRR = fixef(mod_did_nb_low_a),
  confint(mod_did_nb_low_a, parm = "beta_", method = "Wald")
))

#Racial grouping-----------------------------------------------------------------
#White
mod_did_nb_White_v <- glmer.nb(
  violent_count ~ treated + post + did + 
    White_value + owner_value + vacancy_value + offset(log(Shape_Length))
  + (1|OBJECTID_1) + (1|Years),
  data = final_panel_White,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(mod_did_nb_White_v)
exp(cbind(
  IRR = fixef(mod_did_nb_White_v),
  confint(mod_did_nb_White_v, parm = "beta_", method = "Wald")
))

mod_did_nb_White_p <- glmer.nb(
  property_count ~ treated + post + did + 
    White_value + owner_value + vacancy_value + offset(log(Shape_Length))
  + (1|OBJECTID_1) + (1|Years),
  data = final_panel_White,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(mod_did_nb_White_p)
exp(cbind(
  IRR = fixef(mod_did_nb_White_p),
  confint(mod_did_nb_White_p, parm = "beta_", method = "Wald")
))

mod_did_nb_White_a <- glmer.nb(
  allcrime_count ~ treated + post + did + 
    White_value + owner_value + vacancy_value + offset(log(Shape_Length))
  + (1|OBJECTID_1) + (1|Years),
  data = final_panel_White,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(mod_did_nb_White_a)
exp(cbind(
  IRR = fixef(mod_did_nb_White_a),
  confint(mod_did_nb_White_a, parm = "beta_", method = "Wald")
))

#Black
mod_did_nb_Black_v <- glmer.nb(
  violent_count ~ treated + post + did + 
    White_value + owner_value + vacancy_value + offset(log(Shape_Length))
  + (1|OBJECTID_1) + (1|Years),
  data = final_panel_Black,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(mod_did_nb_Black_v)
exp(cbind(
  IRR = fixef(mod_did_nb_Black_v),
  confint(mod_did_nb_Black_v, parm = "beta_", method = "Wald")
))

mod_did_nb_Black_p <- glmer.nb(
  property_count ~ treated + post + did + 
    White_value + owner_value + vacancy_value + offset(log(Shape_Length))
  + (1|OBJECTID_1) + (1|Years),
  data = final_panel_Black,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(mod_did_nb_Black_p)
exp(cbind(
  IRR = fixef(mod_did_nb_Black_p),
  confint(mod_did_nb_Black_p, parm = "beta_", method = "Wald")
))

mod_did_nb_Black_a <- glmer.nb(
  allcrime_count ~ treated + post + did + 
    White_value + owner_value + vacancy_value + offset(log(Shape_Length))
  + (1|OBJECTID_1) + (1|Years),
  data = final_panel_Black,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(mod_did_nb_Black_a)
exp(cbind(
  IRR = fixef(mod_did_nb_Black_a),
  confint(mod_did_nb_Black_a, parm = "beta_", method = "Wald")
))

#Asian
mod_did_nb_Asian_v <- glmer.nb(
  violent_count ~ treated + post + did + 
    White_value + owner_value + vacancy_value + offset(log(Shape_Length))
  + (1|OBJECTID_1) + (1|Years),
  data = final_panel_Asian,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(mod_did_nb_Asian_v)
exp(cbind(
  IRR = fixef(mod_did_nb_Asian_v),
  confint(mod_did_nb_Asian_v, parm = "beta_", method = "Wald")
))

mod_did_nb_Asian_p <- glmer.nb(
  property_count ~ treated + post + did + 
    White_value + owner_value + vacancy_value + offset(log(Shape_Length))
  + (1|OBJECTID_1) + (1|Years),
  data = final_panel_Asian,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(mod_did_nb_Asian_p)
exp(cbind(
  IRR = fixef(mod_did_nb_Asian_p),
  confint(mod_did_nb_Asian_p, parm = "beta_", method = "Wald")
))

mod_did_nb_Asian_a <- glmer.nb(
  allcrime_count ~ treated + post + did + 
    White_value + owner_value + vacancy_value + offset(log(Shape_Length))
  + (1|OBJECTID_1) + (1|Years),
  data = final_panel_Asian,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(mod_did_nb_Asian_a)
exp(cbind(
  IRR = fixef(mod_did_nb_Asian_a),
  confint(mod_did_nb_Asian_a, parm = "beta_", method = "Wald")
))

#Mixed
mod_did_nb_Mixed_v <- glmer.nb(
  violent_count ~ treated + post + did + 
    White_value + owner_value + vacancy_value + offset(log(Shape_Length))
  + (1|OBJECTID_1) + (1|Years),
  data = final_panel_Mixed,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(mod_did_nb_Mixed_v)
exp(cbind(
  IRR = fixef(mod_did_nb_Mixed_v),
  confint(mod_did_nb_Mixed_v, parm = "beta_", method = "Wald")
))

mod_did_nb_Mixed_p <- glmer.nb(
  property_count ~ treated + post + did + 
    White_value + owner_value + vacancy_value + offset(log(Shape_Length))
  + (1|OBJECTID_1) + (1|Years),
  data = final_panel_Mixed,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(mod_did_nb_Mixed_p)
exp(cbind(
  IRR = fixef(mod_did_nb_Mixed_p),
  confint(mod_did_nb_Mixed_p, parm = "beta_", method = "Wald")
))

mod_did_nb_Mixed_a <- glmer.nb(
  allcrime_count ~ treated + post + did + 
    White_value + owner_value + vacancy_value + offset(log(Shape_Length))
  + (1|OBJECTID_1) + (1|Years),
  data = final_panel_Mixed,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(mod_did_nb_Mixed_a)
exp(cbind(
  IRR = fixef(mod_did_nb_Mixed_a),
  confint(mod_did_nb_Mixed_a, parm = "beta_", method = "Wald")
))

#Crime grouping------------------------------------------------------------------
#low crime
mod_did_nb_lc_v <- glmer.nb(
  violent_count ~ treated + post + did + 
    White_value + owner_value + vacancy_value + offset(log(Shape_Length))
  + (1|OBJECTID_1) + (1|Years),
  data = final_panel_lc,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(mod_did_nb_lc_v)
exp(cbind(
  IRR = fixef(mod_did_nb_lc_v),
  confint(mod_did_nb_lc_v, parm = "beta_", method = "Wald")
))

mod_did_nb_lc_p <- glmer.nb(
  property_count ~ treated + post + did + 
    White_value + owner_value + vacancy_value + offset(log(Shape_Length))
  + (1|OBJECTID_1) + (1|Years),
  data = final_panel_lc,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(mod_did_nb_lc_p)
exp(cbind(
  IRR = fixef(mod_did_nb_lc_p),
  confint(mod_did_nb_lc_p, parm = "beta_", method = "Wald")
))

mod_did_nb_lc_a <- glmer.nb(
  allcrime_count ~ treated + post + did + 
    White_value + owner_value + vacancy_value + offset(log(Shape_Length))
  + (1|OBJECTID_1) + (1|Years),
  data = final_panel_lc,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(mod_did_nb_lc_a)
exp(cbind(
  IRR = fixef(mod_did_nb_lc_a),
  confint(mod_did_nb_lc_a, parm = "beta_", method = "Wald")
))

#middle crime
mod_did_nb_mc_v <- glmer.nb(
  violent_count ~ treated + post + did + 
    White_value + owner_value + vacancy_value + offset(log(Shape_Length))
  + (1|OBJECTID_1) + (1|Years),
  data = final_panel_mc,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(mod_did_nb_mc_v)
exp(cbind(
  IRR = fixef(mod_did_nb_mc_v),
  confint(mod_did_nb_mc_v, parm = "beta_", method = "Wald")
))

mod_did_nb_mc_p <- glmer.nb(
  property_count ~ treated + post + did + 
    White_value + owner_value + vacancy_value + offset(log(Shape_Length))
  + (1|OBJECTID_1) + (1|Years),
  data = final_panel_mc,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(mod_did_nb_mc_p)
exp(cbind(
  IRR = fixef(mod_did_nb_mc_p),
  confint(mod_did_nb_mc_p, parm = "beta_", method = "Wald")
))

mod_did_nb_mc_a <- glmer.nb(
  allcrime_count ~ treated + post + did + 
    White_value + owner_value + vacancy_value + offset(log(Shape_Length))
  + (1|OBJECTID_1) + (1|Years),
  data = final_panel_mc,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(mod_did_nb_mc_a)
exp(cbind(
  IRR = fixef(mod_did_nb_mc_a),
  confint(mod_did_nb_mc_a, parm = "beta_", method = "Wald")
))

#high crime
mod_did_nb_hc_v <- glmer.nb(
  violent_count ~ treated + post + did + 
    White_value + owner_value + vacancy_value + offset(log(Shape_Length))
  + (1|OBJECTID_1) + (1|Years),
  data = final_panel_hc,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(mod_did_nb_hc_v)
exp(cbind(
  IRR = fixef(mod_did_nb_hc_v),
  confint(mod_did_nb_hc_v, parm = "beta_", method = "Wald")
))

mod_did_nb_hc_p <- glmer.nb(
  property_count ~ treated + post + did + 
    White_value + owner_value + vacancy_value + offset(log(Shape_Length))
  + (1|OBJECTID_1) + (1|Years),
  data = final_panel_hc,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(mod_did_nb_hc_p)
exp(cbind(
  IRR = fixef(mod_did_nb_hc_p),
  confint(mod_did_nb_hc_p, parm = "beta_", method = "Wald")
))

mod_did_nb_hc_a <- glmer.nb(
  allcrime_count ~ treated + post + did + 
    White_value + owner_value + vacancy_value + offset(log(Shape_Length))
  + (1|OBJECTID_1) + (1|Years),
  data = final_panel_hc,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(mod_did_nb_hc_a)
exp(cbind(
  IRR = fixef(mod_did_nb_hc_a),
  confint(mod_did_nb_hc_a, parm = "beta_", method = "Wald")
))

#statistic analysis--------------------------------------------------------------

fp <- final_panel%>%
  mutate(
    PlntYears_fake = ifelse(is.na(PlntYears_fake), PlntYear, PlntYears_fake),
    lagged_year_fake = as.numeric(Years) - as.numeric(PlntYears_fake),
    lagged_year_fake = pmax(pmin(lagged_year_fake, 5), -5)
  )


sta_total <- fp%>%
  summarise(
    violent_mean = mean(violent_count, na.rm = TRUE),
    violent_sd = sd(violent_count, na.rm = TRUE),
    violent_min = min(violent_count, na.rm = TRUE),
    violent_max = max(violent_count, na.rm = TRUE),
    property_mean = mean(property_count, na.rm = TRUE),
    property_sd = sd(property_count, na.rm = TRUE),
    property_min = min(property_count, na.rm = TRUE),
    property_max = max(property_count, na.rm = TRUE),
    allcrime_mean = mean(allcrime_count, na.rm = TRUE),
    allcrime_sd = sd(allcrime_count, na.rm = TRUE),
    allcrime_min = min(allcrime_count, na.rm = TRUE),
    allcrime_max = max(allcrime_count, na.rm = TRUE),
    White_mean = mean(White_value, na.rm = TRUE),
    White_sd = sd(White_value, na.rm = TRUE),
    White_min = min(White_value, na.rm = TRUE),
    White_max = max(White_value, na.rm = TRUE),
    owner_mean = mean(owner_value, na.rm = TRUE),
    owner_sd = sd(owner_value, na.rm = TRUE),
    owner_min = min(owner_value, na.rm = TRUE),
    owner_max = max(owner_value, na.rm = TRUE),
    vacancy_mean = mean(vacancy_value, na.rm = TRUE),
    vacancy_sd = sd(vacancy_value, na.rm = TRUE),
    vacancy_min = min(vacancy_value, na.rm = TRUE),
    vacancy_max = max(vacancy_value, na.rm = TRUE),
    edu_mean = mean(pct_education_2013, na.rm = TRUE),
    edu_sd = sd(pct_education_2013, na.rm =  TRUE),
    edu_min = min(pct_education_2013, na.rm = TRUE),
    edu_max = max(pct_education_2013, na.rm = TRUE),
    income_mean = mean(income_2013, na.rm = TRUE),
    income_sd = sd(income_2013, na.rm = TRUE),
    income_min = min(income_2013, na.rm = TRUE),
    income_max = max(income_2013, na.rm = TRUE),
  )

#control pre
control_pre <- fp%>%filter(
    treated == 0 & lagged_year_fake < 0
  )%>%
  summarise(
    violent_mean = mean(violent_count, na.rm = TRUE),
    violent_sd = sd(violent_count, na.rm = TRUE),
    property_mean = mean(property_count, na.rm = TRUE),
    property_sd = sd(property_count, na.rm = TRUE),
    allcrime_mean = mean(allcrime_count, na.rm = TRUE),
    allcrime_sd = sd(allcrime_count, na.rm = TRUE)
  )

#control post
control_post <- fp%>%filter(
  treated == 0 & lagged_year_fake >= 0
)%>%
  summarise(
    violent_mean = mean(violent_count, na.rm = TRUE),
    violent_sd = sd(violent_count, na.rm = TRUE),
    property_mean = mean(property_count, na.rm = TRUE),
    property_sd = sd(property_count, na.rm = TRUE),
    allcrime_mean = mean(allcrime_count, na.rm = TRUE),
    allcrime_sd = sd(allcrime_count, na.rm = TRUE)
  )

#treatment pre
treatment_pre <- fp%>%filter(
  treated == 1 & lagged_year_fake < 0
)%>%
  summarise(
    violent_mean = mean(violent_count, na.rm = TRUE),
    violent_sd = sd(violent_count, na.rm = TRUE),
    property_mean = mean(property_count, na.rm = TRUE),
    property_sd = sd(property_count, na.rm = TRUE),
    allcrime_mean = mean(allcrime_count, na.rm = TRUE),
    allcrime_sd = sd(allcrime_count, na.rm = TRUE)
  )

#treatment post
treatment_post <- fp%>%filter(
  treated == 1 & lagged_year_fake >= 0
)%>%
  summarise(
    violent_mean = mean(violent_count, na.rm = TRUE),
    violent_sd = sd(violent_count, na.rm = TRUE),
    property_mean = mean(property_count, na.rm = TRUE),
    property_sd = sd(property_count, na.rm = TRUE),
    allcrime_mean = mean(allcrime_count, na.rm = TRUE),
    allcrime_sd = sd(allcrime_count, na.rm = TRUE)
  )

#covariates
control_cov <- fp%>%filter(
  treated == 0
)%>%
  summarise(
    White_mean = mean(White_value, na.rm = TRUE),
    White_sd = sd(White_value, na.rm = TRUE),
    owner_mean = mean(owner_value, na.rm = TRUE),
    owner_sd = sd(owner_value, na.rm = TRUE),
    vacancy_mean = mean(vacancy_value, na.rm = TRUE),
    vacancy_sd = sd(vacancy_value, na.rm = TRUE),
    edu_mean = mean(pct_education_2013, na.rm = TRUE),
    edu_sd = sd(pct_education_2013, na.rm =  TRUE),
    income_mean = mean(income_2013, na.rm = TRUE),
    income_sd = sd(income_2013, na.rm = TRUE)
    )

treatment_cov <- fp%>%filter(
  treated == 1
)%>%
  summarise(
    White_mean = mean(White_value, na.rm = TRUE),
    White_sd = sd(White_value, na.rm = TRUE),
    owner_mean = mean(owner_value, na.rm = TRUE),
    owner_sd = sd(owner_value, na.rm = TRUE),
    vacancy_mean = mean(vacancy_value, na.rm = TRUE),
    vacancy_sd = sd(vacancy_value, na.rm = TRUE),
    edu_mean = mean(pct_education_2013, na.rm = TRUE),
    edu_sd = sd(pct_education_2013, na.rm =  TRUE),
    income_mean = mean(income_2013, na.rm = TRUE),
    income_sd = sd(income_2013, na.rm = TRUE),
    tree05_mean = mean(tree_count05, na.rm = TRUE),
    tree05_sd = sd(tree_count05, na.rm = TRUE),
    tree05_min = min(tree_count05, na.rm = TRUE),
    tree05_max = max(tree_count05, na.rm = TRUE)
  )

treated_basecrime <- fp%>%
  left_join(
  dplyr::select(finalmatch_tree, treat_id, allcrime_count06), by = c("OBJECTID_1" = "treat_id")
)%>%
  filter(
  treated == 1
)%>%
  summarise(
    crime06_mean = mean(allcrime_count06, na.rm = TRUE),
    crime06_sd = sd(allcrime_count06, na.rm = TRUE),
    crime06_min = min(allcrime_count06, na.rm = TRUE),
    crime06_max = max(allcrime_count06, na.rm = TRUE)
  )

#Income grouping
fp_low <- final_panel_low%>%
  summarise(
    income_mean = mean(income_2013, na.rm = TRUE),
    income_sd = sd(income_2013, na.rm = TRUE),
    income_min = min(income_2013, na.rm = TRUE),
    income_max = max(income_2013, na.rm = TRUE)
  )
fp_med <- final_panel_med%>%
  summarise(
    income_mean = mean(income_2013, na.rm = TRUE),
    income_sd = sd(income_2013, na.rm = TRUE),
    income_min = min(income_2013, na.rm = TRUE),
    income_max = max(income_2013, na.rm = TRUE)
  )
fp_high <- final_panel_high%>%
  summarise(
    income_mean = mean(income_2013, na.rm = TRUE),
    income_sd = sd(income_2013, na.rm = TRUE),
    income_min = min(income_2013, na.rm = TRUE),
    income_max = max(income_2013, na.rm = TRUE)
  )

#Racial grouping
fp_White <- final_panel_White%>%
  summarise(
    White_mean = mean(White_value, na.rm = TRUE),
    White_sd = sd(White_value, na.rm = TRUE),
    White_min = min(White_value, na.rm = TRUE),
    White_max = max(White_value, na.rm = TRUE)
  )
fp_Black <- final_panel_Black%>%
  summarise(
    Black_mean = mean(Black_value, na.rm = TRUE),
    Black_sd = sd(Black_value, na.rm = TRUE),
    Black_min = min(Black_value, na.rm = TRUE),
    Black_max = max(Black_value, na.rm = TRUE)
  )
fp_Asian <- final_panel_Asian%>%
  summarise(
    Asian_mean = mean(Asian_value, na.rm = TRUE),
    Asian_sd = sd(Asian_value, na.rm = TRUE),
    Asian_min = min(Asian_value, na.rm = TRUE),
    Asian_max = max(Asian_value, na.rm = TRUE)
  )

#baseline tree
fp_lscar <- final_panel_lscar%>%
  filter(treated == 1)%>%
  summarise(
    tree05_mean = mean(tree_count05, na.rm = TRUE),
    tree05_sd = sd(tree_count05, na.rm = TRUE),
    tree05_min = min(tree_count05, na.rm = TRUE),
    tree05_max = max(tree_count05, na.rm = TRUE)
  )
fp_mscar <- final_panel_mscar%>%
  filter(treated == 1)%>%
  summarise(
    tree05_mean = mean(tree_count05, na.rm = TRUE),
    tree05_sd = sd(tree_count05, na.rm = TRUE),
    tree05_min = min(tree_count05, na.rm = TRUE),
    tree05_max = max(tree_count05, na.rm = TRUE)
  )
fp_hscar <- final_panel_hscar%>%
  filter(treated == 1)%>%
  summarise(
    tree05_mean = mean(tree_count05, na.rm = TRUE),
    tree05_sd = sd(tree_count05, na.rm = TRUE),
    tree05_min = min(tree_count05, na.rm = TRUE),
    tree05_max = max(tree_count05, na.rm = TRUE)
  )

#baseline crime
bp_lc <- bfccs_panel_lc%>%
  filter(candidate_treated == 1)%>%
  summarise(
    crime06_mean = mean(allcrime_count06, na.rm = TRUE),
    crime06_sd = sd(allcrime_count06, na.rm = TRUE),
    crime06_min = min(allcrime_count06, na.rm = TRUE),
    crime06_max = max(allcrime_count06, na.rm = TRUE)
  )
bp_mc <- bfccs_panel_mc%>%
  filter(candidate_treated == 1)%>%
  summarise(
    crime06_mean = mean(allcrime_count06, na.rm = TRUE),
    crime06_sd = sd(allcrime_count06, na.rm = TRUE),
    crime06_min = min(allcrime_count06, na.rm = TRUE),
    crime06_max = max(allcrime_count06, na.rm = TRUE)
  )
bp_hc <- bfccs_panel_hc%>%
  filter(candidate_treated == 1)%>%
  summarise(
    crime06_mean = mean(allcrime_count06, na.rm = TRUE),
    crime06_sd = sd(allcrime_count06, na.rm = TRUE),
    crime06_min = min(allcrime_count06, na.rm = TRUE),
    crime06_max = max(allcrime_count06, na.rm = TRUE)
  )
  
print(sta_total, width = Inf)
print(control_pre, width = Inf)
print(control_post, width = Inf)
print(treatment_pre, width = Inf)
print(treatment_post, width = Inf)
print(control_cov, width = Inf)
print(treatment_cov, width = Inf)
print(treated_basecrime, width = Inf)
print(fp_low, width = Inf)
print(fp_med, width = Inf)
print(fp_high, width = Inf)
print(fp_White, width = Inf)
print(fp_Black, width = Inf)
print(fp_Asian, width = Inf)
print(fp_lscar, width = Inf)
print(fp_mscar, width = Inf)
print(fp_hscar, width = Inf)
print(bp_lc, width = Inf)
print(bp_mc, width = Inf)
print(bp_hc, width = Inf)

#Robustness test ----------------------------------------------------------------
#balance test--------------------------------------------------------------------

vars <- c("violent_count", "property_count", "allcrime_count", "White_value", "owner_value",
          "vacancy_value", "pct_education_2013", "income_2013")

result <- lapply(vars, function(v){
  
  # control mean
  mean_c <- mean(
    fp %>%
      filter(treated == 0, lagged_year_fake < 0) %>%
      pull(all_of(v)),
    na.rm = TRUE
  )
  
  sd_c <- sd(
    fp %>%
      filter(treated == 0, lagged_year_fake < 0) %>%
      pull(all_of(v)),
    na.rm = TRUE
  )
  
  # treatment mean
  mean_t <- mean(
    fp %>%
      filter(treated == 1, lagged_year_fake < 0) %>%
      pull(all_of(v)),
    na.rm = TRUE
  )
  
  sd_t <- sd(
    fp %>%
      filter(treated == 1, lagged_year_fake < 0) %>%
      pull(all_of(v)),
    na.rm = TRUE
  )
  
  # raw mean difference
  diff <- mean_t - mean_c
  
  # standardized mean difference (SMD)
  smd <- (mean_t - mean_c) /
    sqrt((sd_t^2 + sd_c^2) / 2)
  
  # t test
  ttest <- t.test(fp[[v]] ~ fp$treated)
  
  pvalue <- ttest$p.value
  
  data.frame(
    Variable = v,
    Mean_Control = mean_c,
    SD_Control = sd_c,
    Mean_Treatment = mean_t,
    SD_Treatment = sd_t,
    Difference = diff,
    SMD = smd,
    P_value = pvalue
  )
  
})

balance_table <- bind_rows(result)

balance_table
#placebo test--------------------------------------------------------------------
#lagged year + 3
fp_3placebo <- final_panel%>%
  mutate(
    pair_treat_year = coalesce(as.numeric(PlntYear), as.numeric(PlntYears_fake)),
    placebo_treat_year = pair_treat_year - 3,
  )%>%
  filter(Years < pair_treat_year)%>%
  mutate(
    placebo_post = ifelse(Years >= placebo_treat_year, 1, 0),
    placebo_did = treated * placebo_post
  )

#Regression
mod_did_nb_3pla_v <- glmer.nb(
  violent_count ~ treated + placebo_post + placebo_did 
  + White_value + owner_value + vacancy_value + offset(log(Shape_Length))
  + (1|OBJECTID_1) + (1|Years),
  data = fp_3placebo,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(mod_did_nb_3pla_v)
exp(cbind(
  IRR = fixef(mod_did_nb_3pla_v),
  confint(mod_did_nb_3pla_v, parm = "beta_", method = "Wald")
))

mod_did_nb_3pla_p <- glmer.nb(
  property_count ~ treated + placebo_post + placebo_did 
  + White_value + owner_value + vacancy_value + offset(log(Shape_Length))
  + (1|OBJECTID_1) + (1|Years),
  data = fp_3placebo,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(mod_did_nb_3pla_p)
exp(cbind(
  IRR = fixef(mod_did_nb_3pla_p),
  confint(mod_did_nb_3pla_p, parm = "beta_", method = "Wald")
))

mod_did_nb_3pla_a <- glmer.nb(
  allcrime_count ~ treated + placebo_post + placebo_did 
  + White_value + owner_value + vacancy_value + offset(log(Shape_Length))
  + (1|OBJECTID_1) + (1|Years),
  data = fp_3placebo,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(mod_did_nb_3pla_a)
exp(cbind(
  IRR = fixef(mod_did_nb_3pla_a),
  confint(mod_did_nb_3pla_a, parm = "beta_", method = "Wald")
))

#lagged year + 2
fp_2placebo <- final_panel%>%
  mutate(
    pair_treat_year = coalesce(as.numeric(PlntYear), as.numeric(PlntYears_fake)),
    placebo_treat_year = pair_treat_year - 2,
  )%>%
  filter(Years < pair_treat_year)%>%
  mutate(
    placebo_post = ifelse(Years >= placebo_treat_year, 1, 0),
    placebo_did = treated * placebo_post
  )


#Regression
mod_did_nb_2pla_v <- glmer.nb(
  violent_count ~ treated + placebo_post + placebo_did 
  + White_value + owner_value + vacancy_value + offset(log(Shape_Length))
  + (1|OBJECTID_1) + (1|Years),
  data = fp_2placebo,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(mod_did_nb_2pla_v)
exp(cbind(
  IRR = fixef(mod_did_nb_2pla_v),
  confint(mod_did_nb_2pla_v, parm = "beta_", method = "Wald")
))

mod_did_nb_2pla_p <- glmer.nb(
  property_count ~ treated + placebo_post + placebo_did 
  + White_value + owner_value + vacancy_value + offset(log(Shape_Length))
  + (1|OBJECTID_1) + (1|Years),
  data = fp_2placebo,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(mod_did_nb_2pla_p)
exp(cbind(
  IRR = fixef(mod_did_nb_2pla_p),
  confint(mod_did_nb_2pla_p, parm = "beta_", method = "Wald")
))

mod_did_nb_2pla_a <- glmer.nb(
  allcrime_count ~ treated + placebo_post + placebo_did 
  + White_value + owner_value + vacancy_value + offset(log(Shape_Length))
  + (1|OBJECTID_1) + (1|Years),
  data = fp_2placebo,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(mod_did_nb_2pla_a)
exp(cbind(
  IRR = fixef(mod_did_nb_2pla_a),
  confint(mod_did_nb_2pla_a, parm = "beta_", method = "Wald")
))

#lagged year + 1
fp_1placebo <- final_panel%>%
  mutate(
    pair_treat_year = coalesce(as.numeric(PlntYear), as.numeric(PlntYears_fake)),
    placebo_treat_year = pair_treat_year - 1,
  )%>%
  filter(Years < pair_treat_year)%>%
  mutate(
    placebo_post = ifelse(Years >= placebo_treat_year, 1, 0),
    placebo_did = treated * placebo_post
  )

#Regression
mod_did_nb_1pla_v <- glmer.nb(
  violent_count ~ treated + placebo_post + placebo_did 
  + White_value + owner_value + vacancy_value + offset(log(Shape_Length))
  + (1|OBJECTID_1) + (1|Years),
  data = fp_1placebo,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(mod_did_nb_1pla_v)
exp(cbind(
  IRR = fixef(mod_did_nb_1pla_v),
  confint(mod_did_nb_1pla_v, parm = "beta_", method = "Wald")
))

mod_did_nb_1pla_p <- glmer.nb(
  property_count ~ treated + placebo_post + placebo_did 
  + White_value + owner_value + vacancy_value + offset(log(Shape_Length))
  + (1|OBJECTID_1) + (1|Years),
  data = fp_1placebo,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(mod_did_nb_1pla_p)
exp(cbind(
  IRR = fixef(mod_did_nb_1pla_p),
  confint(mod_did_nb_1pla_p, parm = "beta_", method = "Wald")
))

mod_did_nb_1pla_a <- glmer.nb(
  allcrime_count ~ treated + placebo_post + placebo_did 
  + White_value + owner_value + vacancy_value + offset(log(Shape_Length))
  + (1|OBJECTID_1) + (1|Years),
  data = fp_1placebo,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(mod_did_nb_1pla_a)
exp(cbind(
  IRR = fixef(mod_did_nb_1pla_a),
  confint(mod_did_nb_1pla_a, parm = "beta_", method = "Wald")
))


#Complete block planning bar chart--------------------------------------------------------
treatment <- treatment%>%
  mutate(
    PlntYears = as.numeric(substring(treatment$PlntSeas, first = nchar(treatment$PlntSeas)-1)),
    PlntYears = case_when(
      PlntYears == 7 ~ 2007,
      PlntYears == 8 ~ 2008,
      PlntYears == 9 ~ 2009,
      PlntYears == 10 ~ 2010,
      PlntYears == 11 ~ 2011,
      PlntYears == 12 ~ 2012,
      PlntYears == 13 ~ 2013,
      PlntYears == 14 ~ 2014,
      PlntYears == 15 ~ 2015
    )
  )

bar_finish <- treatment %>%
  count(PlntYears) %>%
  complete(PlntYears = seq(min(PlntYears), max(PlntYears), by = 1), fill = list(n = 0)) %>%
  ggplot(aes(x = factor(PlntYears), y = n)) +
  geom_col(
    fill = "grey73",
    color = "black",
    width = 0.5
  ) +
  scale_x_discrete(
    expand = expansion(mult = c(0,0))
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    x = "Year of block planting completion",
    y = "Number of segments"
  ) +
  theme(
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.line = element_line(linewidth = 0.8),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.5),
    panel.background = element_rect(fill = "white")
  )

ggsave("E:/did_process/revise/figures/bar_finish.png",plot = bar_finish,width = 5,height = 4,dpi = 300)

#Stewardship grouping coefficient Images-----------------------------------------
#tertile-------------------------------------------------------------------------
coef_nb_stew_v2 <- as.data.frame(coef(summary(mod_did_nb_stew_v2))) %>%
  mutate(
    term = rownames(.),
    conf.low = Estimate - 1.96 * `Std. Error`,
    conf.high = Estimate + 1.96 * `Std. Error`
  ) %>%
  rename(
    estimate = Estimate,
    std.error = `Std. Error`,
    statistic = `z value`,
    p.value = `Pr(>|z|)`
  )

coef_nb_stew_v2_var <- coef_nb_stew_v2%>%
  filter(
    term == "did31" | term == "did32" | term == "did33"
  )%>%
  mutate(
    crime_type = "violent",
    model = "nb_stew_v2",
    signif = cut(
      p.value, 
      breaks = c(0, 0.01, 0.05, 0.1, 1), 
      labels = c("*\n*\n*", "*\n*", "*", "")
    )
  )


coef_nb_stew_p2 <- as.data.frame(coef(summary(mod_did_nb_stew_p2))) %>%
  mutate(
    term = rownames(.),
    conf.low = Estimate - 1.96 * `Std. Error`,
    conf.high = Estimate + 1.96 * `Std. Error`
  ) %>%
  rename(
    estimate = Estimate,
    std.error = `Std. Error`,
    statistic = `z value`,
    p.value = `Pr(>|z|)`
  )

coef_nb_stew_p2_var <- coef_nb_stew_p2%>%
  filter(
    term == "did31" | term == "did32" | term == "did33"
  )%>%
  mutate(
    crime_type = "property",
    model = "nb_stew_p2",
    signif = cut(
      p.value, 
      breaks = c(0, 0.01, 0.05, 0.1, 1), 
      labels = c("*\n*\n*", "*\n*", "*", "")
    )
  )

coef_nb_stew_a2 <- as.data.frame(coef(summary(mod_did_nb_stew_a2))) %>%
  mutate(
    term = rownames(.),
    conf.low = Estimate - 1.96 * `Std. Error`,
    conf.high = Estimate + 1.96 * `Std. Error`
  ) %>%
  rename(
    estimate = Estimate,
    std.error = `Std. Error`,
    statistic = `z value`,
    p.value = `Pr(>|z|)`
  )

coef_nb_stew_a2_var <- coef_nb_stew_a2%>%
  filter(
    term == "did31" | term == "did32" | term == "did33"
  )%>%
  mutate(
    crime_type = "total crime",
    model = "nb_stew_a2",
    signif = cut(
      p.value, 
      breaks = c(0, 0.01, 0.05, 0.1, 1), 
      labels = c("*\n*\n*", "*\n*", "*", "")
    )
  )

#quartile------------------------------------------------------------------------
coef_nb_stew_v3 <- as.data.frame(coef(summary(mod_did_nb_stew_v3))) %>%
  mutate(
    term = rownames(.),
    conf.low = Estimate - 1.96 * `Std. Error`,
    conf.high = Estimate + 1.96 * `Std. Error`
  ) %>%
  rename(
    estimate = Estimate,
    std.error = `Std. Error`,
    statistic = `z value`,
    p.value = `Pr(>|z|)`
  )

coef_nb_stew_v3_var <- coef_nb_stew_v3%>%
  filter(
    term == "did41" | term == "did42" | term == "did43" | term == "did44"
  )%>%
  mutate(
    crime_type = "violent",
    model = "nb_stew_v3",
    signif = cut(
      p.value, 
      breaks = c(0, 0.01, 0.05, 0.1, 1), 
      labels = c("*\n*\n*", "*\n*", "*", "")
    )
  )

coef_nb_stew_p3 <- as.data.frame(coef(summary(mod_did_nb_stew_p3))) %>%
  mutate(
    term = rownames(.),
    conf.low = Estimate - 1.96 * `Std. Error`,
    conf.high = Estimate + 1.96 * `Std. Error`
  ) %>%
  rename(
    estimate = Estimate,
    std.error = `Std. Error`,
    statistic = `z value`,
    p.value = `Pr(>|z|)`
  )

coef_nb_stew_p3_var <- coef_nb_stew_p3%>%
  filter(
    term == "did41" | term == "did42" | term == "did43" | term == "did44"
  )%>%
  mutate(
    crime_type = "property",
    model = "nb_stew_p3",
    signif = cut(
      p.value, 
      breaks = c(0, 0.01, 0.05, 0.1, 1), 
      labels = c("*\n*\n*", "*\n*", "*", "")
    )
  )

coef_nb_stew_a3 <- as.data.frame(coef(summary(mod_did_nb_stew_a3))) %>%
  mutate(
    term = rownames(.),
    conf.low = Estimate - 1.96 * `Std. Error`,
    conf.high = Estimate + 1.96 * `Std. Error`
  ) %>%
  rename(
    estimate = Estimate,
    std.error = `Std. Error`,
    statistic = `z value`,
    p.value = `Pr(>|z|)`
  )

coef_nb_stew_a3_var <- coef_nb_stew_a3%>%
  filter(
    term == "did41" | term == "did42" | term == "did43" | term == "did44"
  )%>%
  mutate(
    crime_type = "total crime",
    model = "nb_stew_a3",
    signif = cut(
      p.value, 
      breaks = c(0, 0.01, 0.05, 0.1, 1), 
      labels = c("*\n*\n*", "*\n*", "*", "")
    )
  )

#Draw----------------------------------------------------------------------------
#tertile-------------------------------------------------------------------------
coef_steward2 <- rbind(
  coef_nb_stew_v2_var,
  coef_nb_stew_p2_var,
  coef_nb_stew_a2_var
)

coef_steward2 <- coef_steward2%>%mutate(
  crime_type = factor(crime_type, levels = c("violent", "property", "total crime")),
  term = factor(term, levels = c("did31", "did32", "did33")),
)

p_steward2 <- ggplot(coef_steward2, aes(x = term, y = estimate, color = crime_type)) +
  geom_point(
    position = position_dodge(width = 0.6),
    size = 4
  ) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(width = 0.6),
    width = 0.1,
    linewidth = 1
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dotted"
  ) +
  geom_text(
    aes(
      x = as.numeric(factor(term)) - 0.05,
      y = (conf.high + conf.low) / 2 + 0.06,    # ⭐ 放在误差线上方
      label = signif
    ),
    position = position_dodge(width = 0.6),
    size = 10,
    show.legend = FALSE,
    lineheight = 0.3
  ) +
  labs(
    x = "DID × Three-category Stewardship",
    y = "Coefficients",
    color = "",
    # shape = "Significance"
  ) +
  scale_x_discrete(
    limits = c("did31", "did32", "did33"),
    labels = c(
      "did31" = "1/3",
      "did32" = "2/3",
      "did33" = "3/3"
    )
  ) +
  scale_y_continuous(limits = c(-1.2, 1.2)) +
  scale_color_manual(values = c(
    "violent" = "#4E79A7",
    "property" = "#B07AA1",
    "total crime" = "#9CBA43"
  )) +
  # guides(
  #   color = guide_legend(order = 1),
  #   shape = guide_legend(order = 2)
  # ) +
  theme(
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 18),
    axis.line = element_line(linewidth = 0.8),
    axis.ticks = element_line(linewidth = 1),
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    legend.position = c(0.25, 0.05),
    legend.text = element_text(size = 16),
    legend.direction = "horizontal"
    # legend.title = element_text(size = 16)
  )


ggsave("E:/did_process/figures/p_steward2.png",plot = p_steward2, width = 9, height = 7, dpi = 300)

#quartile------------------------------------------------------------------------

coef_steward3 <- rbind(
  coef_nb_stew_v3_var,
  coef_nb_stew_p3_var,
  coef_nb_stew_a3_var
)

coef_steward3 <- coef_steward3%>%mutate(
  crime_type = factor(crime_type, levels = c("violent", "property", "total crime")),
  term = factor(term, levels = c("did41", "did42", "did43", "did44")),
)

p_steward3 <- ggplot(coef_steward3, aes(x = term, y = estimate, color = crime_type)) +
  geom_point(
    position = position_dodge(width = 0.6),
    size = 4
  ) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(width = 0.6),
    width = 0.1,
    linewidth = 1
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dotted"
  ) +
  geom_text(
    aes(
      x = as.numeric(factor(term)) - 0.05,
      y = (conf.high + conf.low) / 2 + 0.06,    
      label = signif
    ),
    position = position_dodge(width = 0.6),
    size = 10,
    show.legend = FALSE,
    lineheight = 0.3
  ) +
  labs(
    x = "DID × Four-category Stewardship",
    y = "Coefficients",
    color = "",
    # shape = "Significance"
  ) +
  scale_x_discrete(
    limits = c("did41", "did42", "did43", "did44"),
    labels = c(
      "did41" = "1/4",
      "did42" = "2/4",
      "did43" = "3/4",
      "did44" = "4/4"
    )
  ) +
  scale_y_continuous(limits = c(-1.2, 1.2)) +
  scale_color_manual(values = c(
    "violent" = "#4E79A7",
    "property" = "#B07AA1",
    "total crime" = "#9CBA43"
  )) +
  # guides(
  #   color = guide_legend(order = 1),
  #   shape = guide_legend(order = 2)
  # ) +
  theme(
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 18),
    axis.line = element_line(linewidth = 0.8),
    axis.ticks = element_line(linewidth = 1),
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    legend.position = c(0.25, 0.05),
    legend.text = element_text(size = 16),
    legend.direction = "horizontal"
    # legend.title = element_text(size = 16)
  )


ggsave("E:/did_process/figures/p_steward3.png",plot = p_steward3, width = 9, height = 7, dpi = 300)

#Intensity grouping coefficient Images-------------------------------------------
#tertile-------------------------------------------------------------------------
coef_nb_inten_v2 <- as.data.frame(coef(summary(mod_did_nb_inten_v2))) %>%
  mutate(
    term = rownames(.),
    conf.low = Estimate - 1.96 * `Std. Error`,
    conf.high = Estimate + 1.96 * `Std. Error`
  ) %>%
  rename(
    estimate = Estimate,
    std.error = `Std. Error`,
    statistic = `z value`,
    p.value = `Pr(>|z|)`
  )

coef_nb_inten_v2_var <- coef_nb_inten_v2%>%
  filter(
    term == "did31" | term == "did32" | term == "did33"
  )%>%
  mutate(
    crime_type = "violent",
    model = "nb_inten_v2",
    signif = cut(
      p.value, 
      breaks = c(0, 0.01, 0.05, 0.1, 1), 
      labels = c("*\n*\n*", "*\n*", "*", "")
    )
  )

coef_nb_inten_p2 <- as.data.frame(coef(summary(mod_did_nb_inten_p2))) %>%
  mutate(
    term = rownames(.),
    conf.low = Estimate - 1.96 * `Std. Error`,
    conf.high = Estimate + 1.96 * `Std. Error`
  ) %>%
  rename(
    estimate = Estimate,
    std.error = `Std. Error`,
    statistic = `z value`,
    p.value = `Pr(>|z|)`
  )

coef_nb_inten_p2_var <- coef_nb_inten_p2%>%
  filter(
    term == "did31" | term == "did32" | term == "did33"
  )%>%
  mutate(
    crime_type = "property",
    model = "nb_inten_p2",
    signif = cut(
      p.value, 
      breaks = c(0, 0.01, 0.05, 0.1, 1), 
      labels = c("*\n*\n*", "*\n*", "*", "")
    )
  )

coef_nb_inten_a2 <- as.data.frame(coef(summary(mod_did_nb_inten_a2))) %>%
  mutate(
    term = rownames(.),
    conf.low = Estimate - 1.96 * `Std. Error`,
    conf.high = Estimate + 1.96 * `Std. Error`
  ) %>%
  rename(
    estimate = Estimate,
    std.error = `Std. Error`,
    statistic = `z value`,
    p.value = `Pr(>|z|)`
  )

coef_nb_inten_a2_var <- coef_nb_inten_a2%>%
  filter(
    term == "did31" | term == "did32" | term == "did33"
  )%>%
  mutate(
    crime_type = "total crime",
    model = "nb_inten_a2",
    signif = cut(
      p.value, 
      breaks = c(0, 0.01, 0.05, 0.1, 1), 
      labels = c("*\n*\n*", "*\n*", "*", "")
    )
  )

#quartile------------------------------------------------------------------------
coef_nb_inten_v3 <- as.data.frame(coef(summary(mod_did_nb_inten_v3))) %>%
  mutate(
    term = rownames(.),
    conf.low = Estimate - 1.96 * `Std. Error`,
    conf.high = Estimate + 1.96 * `Std. Error`
  ) %>%
  rename(
    estimate = Estimate,
    std.error = `Std. Error`,
    statistic = `z value`,
    p.value = `Pr(>|z|)`
  )

coef_nb_inten_v3_var <- coef_nb_inten_v3%>%
  filter(
    term == "did41" | term == "did42" | term == "did43" | term == "did44"
  )%>%
  mutate(
    crime_type = "violent",
    model = "nb_inten_v3",
    signif = cut(
      p.value, 
      breaks = c(0, 0.01, 0.05, 0.1, 1), 
      labels = c("*\n*\n*", "*\n*", "*", "")
    )
  )

coef_nb_inten_p3 <- as.data.frame(coef(summary(mod_did_nb_inten_p3))) %>%
  mutate(
    term = rownames(.),
    conf.low = Estimate - 1.96 * `Std. Error`,
    conf.high = Estimate + 1.96 * `Std. Error`
  ) %>%
  rename(
    estimate = Estimate,
    std.error = `Std. Error`,
    statistic = `z value`,
    p.value = `Pr(>|z|)`
  )

coef_nb_inten_p3_var <- coef_nb_inten_p3%>%
  filter(
    term == "did41" | term == "did42" | term == "did43" | term == "did44"
  )%>%
  mutate(
    crime_type = "property",
    model = "nb_inten_p3",
    signif = cut(
      p.value, 
      breaks = c(0, 0.01, 0.05, 0.1, 1), 
      labels = c("*\n*\n*", "*\n*", "*", "")
    )
  )

coef_nb_inten_a3 <- as.data.frame(coef(summary(mod_did_nb_inten_a3))) %>%
  mutate(
    term = rownames(.),
    conf.low = Estimate - 1.96 * `Std. Error`,
    conf.high = Estimate + 1.96 * `Std. Error`
  ) %>%
  rename(
    estimate = Estimate,
    std.error = `Std. Error`,
    statistic = `z value`,
    p.value = `Pr(>|z|)`
  )

coef_nb_inten_a3_var <- coef_nb_inten_a3%>%
  filter(
    term == "did41" | term == "did42" | term == "did43" | term == "did44"
  )%>%
  mutate(
    crime_type = "total crime",
    model = "nb_inten_a3",
    signif = cut(
      p.value, 
      breaks = c(0, 0.01, 0.05, 0.1, 1), 
      labels = c("*\n*\n*", "*\n*", "*", "")
    )
  )

#Draw----------------------------------------------------------------------------
#tertile-------------------------------------------------------------------------
coef_intensity2 <- rbind(
  coef_nb_inten_v2_var,
  coef_nb_inten_p2_var,
  coef_nb_inten_a2_var
)

coef_intensity2 <- coef_intensity2%>%mutate(
  crime_type = factor(crime_type, levels = c("violent", "property", "total crime")),
  term = factor(term, levels = c("did31", "did32", "did33")),
)

p_intensity2 <- ggplot(coef_intensity2, aes(x = term, y = estimate, color = crime_type)) +
  geom_point(
    position = position_dodge(width = 0.6),
    size = 4
  ) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(width = 0.6),
    width = 0.1,
    linewidth = 1
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dotted"
  ) +
  geom_text(
    aes(
      x = as.numeric(factor(term)) - 0.05,
      y = (conf.high + conf.low) / 2 + 0.06,   
      label = signif
    ),
    position = position_dodge(width = 0.6),
    size = 10,
    show.legend = FALSE,
    lineheight = 0.3
  ) +
  labs(
    x = "DID × Three-category Intensity",
    y = "Coefficients",
    color = "",
    # shape = "Significance"
  ) +
  scale_x_discrete(
    limits = c("did31", "did32", "did33"),
    labels = c(
      "did31" = "1/3",
      "did32" = "2/3",
      "did33" = "3/3"
    )
  ) +
  scale_y_continuous(limits = c(-1.2, 1.2)) +
  scale_color_manual(values = c(
    "violent" = "#4E79A7",
    "property" = "#B07AA1",
    "total crime" = "#9CBA43"
  )) +
  # guides(
  #   color = guide_legend(order = 1),
  #   shape = guide_legend(order = 2)
  # ) +
  theme(
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 18),
    axis.line = element_line(linewidth = 0.8),
    axis.ticks = element_line(linewidth = 1),
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    legend.position = c(0.25, 0.05),
    legend.text = element_text(size = 16),
    legend.direction = "horizontal"
    # legend.title = element_text(size = 16)
  )

ggsave("E:/did_process/figures/p_intensity2.png",plot = p_intensity2, width = 9, height = 7, dpi = 300)

#quartile------------------------------------------------------------------------
coef_intensity3 <- rbind(
  coef_nb_inten_v3_var,
  coef_nb_inten_p3_var,
  coef_nb_inten_a3_var
)

coef_intensity3 <- coef_intensity3%>%mutate(
  crime_type = factor(crime_type, levels = c("violent", "property", "total crime")),
  term = factor(term, levels = c("did41", "did42", "did43", "did44")),
)

p_intensity3 <- ggplot(coef_intensity3, aes(x = term, y = estimate, color = crime_type)) +
  geom_point(
    position = position_dodge(width = 0.6),
    size = 4
  ) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(width = 0.6),
    width = 0.1,
    linewidth = 1
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dotted"
  ) +
  geom_text(
    aes(
      x = as.numeric(factor(term)) - 0.05,
      y = (conf.high + conf.low) / 2 + 0.06,    
      label = signif
    ),
    position = position_dodge(width = 0.6),
    size = 10,
    show.legend = FALSE,
    lineheight = 0.3
  ) +
  labs(
    x = "DID × Four-category Intensity",
    y = "Coefficients",
    color = "",
    # shape = "Significance"
  ) +
  scale_x_discrete(
    limits = c("did41", "did42", "did43", "did44"),
    labels = c(
      "did41" = "1/4",
      "did42" = "2/4",
      "did43" = "3/4",
      "did44" = "4/4"
    )
  ) +
  scale_y_continuous(limits = c(-1.2, 1.2)) +
  scale_color_manual(values = c(
    "violent" = "#4E79A7",
    "property" = "#B07AA1",
    "total crime" = "#9CBA43"
  )) +
  # guides(
  #   color = guide_legend(order = 1),
  #   shape = guide_legend(order = 2)
  # ) +
  theme(
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 18),
    axis.line = element_line(linewidth = 0.8),
    axis.ticks = element_line(linewidth = 1),
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    legend.position = c(0.25, 0.05),
    legend.text = element_text(size = 16),
    legend.direction = "horizontal"
    # legend.title = element_text(size = 16)
  )

ggsave("E:/did_process/figures/p_intensity3.png",plot = p_intensity3, width = 9, height = 7, dpi = 300)

#Baseline tree measure coefficient Images----------------------------------------
#nb_hscar------------------------------------------------------------------------
coef_nb_hscar_v <- as.data.frame(coef(summary(mod_did_nb_hscar_v))) %>%
  mutate(
    term = rownames(.),
    conf.low = Estimate - 1.96 * `Std. Error`,
    conf.high = Estimate + 1.96 * `Std. Error`
  ) %>%
  rename(
    estimate = Estimate,
    std.error = `Std. Error`,
    statistic = `z value`,
    p.value = `Pr(>|z|)`
  )

coef_nb_hscar_v_var <- coef_nb_hscar_v%>%
  filter(term=="did")%>%
  mutate(
    crime_type = "violent",
    model = "nb_hscar"
  )

coef_nb_hscar_p <- as.data.frame(coef(summary(mod_did_nb_hscar_p))) %>%
  mutate(
    term = rownames(.),
    conf.low = Estimate - 1.96 * `Std. Error`,
    conf.high = Estimate + 1.96 * `Std. Error`
  ) %>%
  rename(
    estimate = Estimate,
    std.error = `Std. Error`,
    statistic = `z value`,
    p.value = `Pr(>|z|)`
  )

coef_nb_hscar_p_var <- coef_nb_hscar_p%>%
  filter(term=="did")%>%
  mutate(
    crime_type = "property",
    model = "nb_hscar"
  )

coef_nb_hscar_a <- as.data.frame(coef(summary(mod_did_nb_hscar_a))) %>%
  mutate(
    term = rownames(.),
    conf.low = Estimate - 1.96 * `Std. Error`,
    conf.high = Estimate + 1.96 * `Std. Error`
  ) %>%
  rename(
    estimate = Estimate,
    std.error = `Std. Error`,
    statistic = `z value`,
    p.value = `Pr(>|z|)`
  )

coef_nb_hscar_a_var <- coef_nb_hscar_a%>%
  filter(term=="did")%>%
  mutate(
    crime_type = "total crime",
    model = "nb_hscar"
  )

#nb_mscar------------------------------------------------------------------------
coef_nb_mscar_v <- as.data.frame(coef(summary(mod_did_nb_mscar_v))) %>%
  mutate(
    term = rownames(.),
    conf.low = Estimate - 1.96 * `Std. Error`,
    conf.high = Estimate + 1.96 * `Std. Error`
  ) %>%
  rename(
    estimate = Estimate,
    std.error = `Std. Error`,
    statistic = `z value`,
    p.value = `Pr(>|z|)`
  )

coef_nb_mscar_v_var <- coef_nb_mscar_v%>%
  filter(term=="did")%>%
  mutate(
    crime_type = "violent",
    model = "nb_mscar"
  )

coef_nb_mscar_p <- as.data.frame(coef(summary(mod_did_nb_mscar_p))) %>%
  mutate(
    term = rownames(.),
    conf.low = Estimate - 1.96 * `Std. Error`,
    conf.high = Estimate + 1.96 * `Std. Error`
  ) %>%
  rename(
    estimate = Estimate,
    std.error = `Std. Error`,
    statistic = `z value`,
    p.value = `Pr(>|z|)`
  )

coef_nb_mscar_p_var <- coef_nb_mscar_p%>%
  filter(term=="did")%>%
  mutate(
    crime_type = "property",
    model = "nb_mscar"
  )

coef_nb_mscar_a <- as.data.frame(coef(summary(mod_did_nb_mscar_a))) %>%
  mutate(
    term = rownames(.),
    conf.low = Estimate - 1.96 * `Std. Error`,
    conf.high = Estimate + 1.96 * `Std. Error`
  ) %>%
  rename(
    estimate = Estimate,
    std.error = `Std. Error`,
    statistic = `z value`,
    p.value = `Pr(>|z|)`
  )

coef_nb_mscar_a_var <- coef_nb_mscar_a%>%
  filter(term=="did")%>%
  mutate(
    crime_type = "total crime",
    model = "nb_mscar"
  )

#nb_lscar------------------------------------------------------------------------
coef_nb_lscar_v <- as.data.frame(coef(summary(mod_did_nb_lscar_v))) %>%
  mutate(
    term = rownames(.),
    conf.low = Estimate - 1.96 * `Std. Error`,
    conf.high = Estimate + 1.96 * `Std. Error`
  ) %>%
  rename(
    estimate = Estimate,
    std.error = `Std. Error`,
    statistic = `z value`,
    p.value = `Pr(>|z|)`
  )

coef_nb_lscar_v_var <- coef_nb_lscar_v%>%
  filter(term=="did")%>%
  mutate(
    crime_type = "violent",
    model = "nb_lscar"
  )

coef_nb_lscar_p <- as.data.frame(coef(summary(mod_did_nb_lscar_p))) %>%
  mutate(
    term = rownames(.),
    conf.low = Estimate - 1.96 * `Std. Error`,
    conf.high = Estimate + 1.96 * `Std. Error`
  ) %>%
  rename(
    estimate = Estimate,
    std.error = `Std. Error`,
    statistic = `z value`,
    p.value = `Pr(>|z|)`
  )

coef_nb_lscar_p_var <- coef_nb_lscar_p%>%
  filter(term=="did")%>%
  mutate(
    crime_type = "property",
    model = "nb_lscar"
  )

coef_nb_lscar_a <- as.data.frame(coef(summary(mod_did_nb_lscar_a))) %>%
  mutate(
    term = rownames(.),
    conf.low = Estimate - 1.96 * `Std. Error`,
    conf.high = Estimate + 1.96 * `Std. Error`
  ) %>%
  rename(
    estimate = Estimate,
    std.error = `Std. Error`,
    statistic = `z value`,
    p.value = `Pr(>|z|)`
  )

coef_nb_lscar_a_var <- coef_nb_lscar_a%>%
  filter(term=="did")%>%
  mutate(
    crime_type = "total crime",
    model = "nb_lscar"
  )

#Draw----------------------------------------------------------------------------
coef_tree_scar <- rbind(
  coef_nb_hscar_v_var, coef_nb_hscar_p_var, coef_nb_hscar_a_var,
  coef_nb_mscar_v_var, coef_nb_mscar_p_var, coef_nb_mscar_a_var,
  coef_nb_lscar_v_var, coef_nb_lscar_p_var, coef_nb_lscar_a_var
)

coef_tree_scar <- coef_tree_scar%>%mutate(
  crime_type = factor(crime_type, levels = c("violent", "property", "total crime")),
  model = factor(model, levels = c("nb_lscar", "nb_mscar", "nb_hscar")),
  signif = cut(
    p.value, 
    breaks = c(0, 0.01, 0.05, 0.1, 1), 
    labels = c("*\n*\n*", "*\n*", "*", "")
  )
  # x_offset = c(0.8, 1, 1.2, 1.8, 2, 2.2, 2.8, 3, 3.2)
)


p_tree_scar <- ggplot(coef_tree_scar, aes(x = model, y = estimate, color = crime_type)) +
  geom_point(
    position = position_dodge(width = 0.6),
    size=4
  ) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(width = 0.6),
    width = 0.1, 
    linewidth = 1
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dotted"
  ) +
  geom_text(
    aes(
      x = as.numeric(factor(model)) - 0.05,
      y = (conf.high + conf.low) / 2 + 0.06,  
      label = signif,
    ),
    position = position_dodge(width = 0.6),
    size = 10,
    show.legend = FALSE,
    lineheight = 0.3
  ) +
  scale_color_manual(values = c(
    "violent" = "#4E79A7",
    "property" = "#B07AA1",
    "total crime" = "#9CBA43"
  )) +
  scale_x_discrete(
    limits = c(
      "nb_lscar", "nb_mscar", "nb_hscar"
    ),
    labels = c(
      "nb_lscar" = "Low Scarcity",
      "nb_mscar" = "Medium Scarcity",
      "nb_hscar" = "High Scarcity"
    )
  ) +
  scale_y_continuous(limits = c(-1.2, 1.2)) +
  labs(
    x = "Streets Classification",
    y = "Coefficient of Interaction Terms",
    color = ""
  ) +
  theme(
    axis.line = element_line(linewidth = 0.8),
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 18),
    axis.ticks = element_line(linewidth = 1),
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    legend.position = c(0.25,0.05),
    legend.direction = "horizontal",
    # legend.title = element_text(size = 16),
    legend.text = element_text(size = 16)
  )

ggsave("E:/did_process/figures/p_tree_scar.png",plot = p_tree_scar, width = 9, height = 7, dpi = 300)




#Income grouping coefficient Images----------------------------------------------
#low-income----------------------------------------------------------------------
coef_nb_low_v <- as.data.frame(coef(summary(mod_did_nb_low_v))) %>%
  mutate(
    term = rownames(.),
    conf.low = Estimate - 1.96 * `Std. Error`,
    conf.high = Estimate + 1.96 * `Std. Error`
  ) %>%
  rename(
    estimate = Estimate,
    std.error = `Std. Error`,
    statistic = `z value`,
    p.value = `Pr(>|z|)`
  )

coef_nb_low_v_var <- coef_nb_low_v%>%
  filter(term=="did")%>%
  mutate(
    crime_type = "violent",
    model = "nb_low"
  )

coef_nb_low_p <- as.data.frame(coef(summary(mod_did_nb_low_p))) %>%
  mutate(
    term = rownames(.),
    conf.low = Estimate - 1.96 * `Std. Error`,
    conf.high = Estimate + 1.96 * `Std. Error`
  ) %>%
  rename(
    estimate = Estimate,
    std.error = `Std. Error`,
    statistic = `z value`,
    p.value = `Pr(>|z|)`
  )

coef_nb_low_p_var <- coef_nb_low_p%>%
  filter(term=="did")%>%
  mutate(
    crime_type = "property",
    model = "nb_low"
  )

coef_nb_low_a <- as.data.frame(coef(summary(mod_did_nb_low_a))) %>%
  mutate(
    term = rownames(.),
    conf.low = Estimate - 1.96 * `Std. Error`,
    conf.high = Estimate + 1.96 * `Std. Error`
  ) %>%
  rename(
    estimate = Estimate,
    std.error = `Std. Error`,
    statistic = `z value`,
    p.value = `Pr(>|z|)`
  )

coef_nb_low_a_var <- coef_nb_low_a%>%
  filter(term=="did")%>%
  mutate(
    crime_type = "total crime",
    model = "nb_low"
  )

#medium-income-------------------------------------------------------------------
coef_nb_med_v <- as.data.frame(coef(summary(mod_did_nb_med_v))) %>%
  mutate(
    term = rownames(.),
    conf.low = Estimate - 1.96 * `Std. Error`,
    conf.high = Estimate + 1.96 * `Std. Error`
  ) %>%
  rename(
    estimate = Estimate,
    std.error = `Std. Error`,
    statistic = `z value`,
    p.value = `Pr(>|z|)`
  )

coef_nb_med_v_var <- coef_nb_med_v%>%
  filter(term=="did")%>%
  mutate(
    crime_type = "violent",
    model = "nb_med"
  )

coef_nb_med_p <- as.data.frame(coef(summary(mod_did_nb_med_p))) %>%
  mutate(
    term = rownames(.),
    conf.low = Estimate - 1.96 * `Std. Error`,
    conf.high = Estimate + 1.96 * `Std. Error`
  ) %>%
  rename(
    estimate = Estimate,
    std.error = `Std. Error`,
    statistic = `z value`,
    p.value = `Pr(>|z|)`
  )

coef_nb_med_p_var <- coef_nb_med_p%>%
  filter(term=="did")%>%
  mutate(
    crime_type = "property",
    model = "nb_med"
  )

coef_nb_med_a <- as.data.frame(coef(summary(mod_did_nb_med_a))) %>%
  mutate(
    term = rownames(.),
    conf.low = Estimate - 1.96 * `Std. Error`,
    conf.high = Estimate + 1.96 * `Std. Error`
  ) %>%
  rename(
    estimate = Estimate,
    std.error = `Std. Error`,
    statistic = `z value`,
    p.value = `Pr(>|z|)`
  )

coef_nb_med_a_var <- coef_nb_med_a%>%
  filter(term=="did")%>%
  mutate(
    crime_type = "total crime",
    model = "nb_med"
  )

#high-income---------------------------------------------------------------------
coef_nb_high_v <- as.data.frame(coef(summary(mod_did_nb_high_v))) %>%
  mutate(
    term = rownames(.),
    conf.low = Estimate - 1.96 * `Std. Error`,
    conf.high = Estimate + 1.96 * `Std. Error`
  ) %>%
  rename(
    estimate = Estimate,
    std.error = `Std. Error`,
    statistic = `z value`,
    p.value = `Pr(>|z|)`
  )

coef_nb_high_v_var <- coef_nb_high_v%>%
  filter(term=="did")%>%
  mutate(
    crime_type = "violent",
    model = "nb_high"
  )

coef_nb_high_p <- as.data.frame(coef(summary(mod_did_nb_high_p))) %>%
  mutate(
    term = rownames(.),
    conf.low = Estimate - 1.96 * `Std. Error`,
    conf.high = Estimate + 1.96 * `Std. Error`
  ) %>%
  rename(
    estimate = Estimate,
    std.error = `Std. Error`,
    statistic = `z value`,
    p.value = `Pr(>|z|)`
  )

coef_nb_high_p_var <- coef_nb_high_p%>%
  filter(term=="did")%>%
  mutate(
    crime_type = "property",
    model = "nb_high"
  )

coef_nb_high_a <- as.data.frame(coef(summary(mod_did_nb_high_a))) %>%
  mutate(
    term = rownames(.),
    conf.low = Estimate - 1.96 * `Std. Error`,
    conf.high = Estimate + 1.96 * `Std. Error`
  ) %>%
  rename(
    estimate = Estimate,
    std.error = `Std. Error`,
    statistic = `z value`,
    p.value = `Pr(>|z|)`
  )

coef_nb_high_a_var <- coef_nb_high_a%>%
  filter(term=="did")%>%
  mutate(
    crime_type = "total crime",
    model = "nb_high"
  )

#Draw----------------------------------------------------------------------------

coef_income <- rbind(
  coef_nb_low_v_var, coef_nb_low_p_var, coef_nb_low_a_var,
  coef_nb_med_v_var, coef_nb_med_p_var, coef_nb_med_a_var,
  coef_nb_high_v_var, coef_nb_high_p_var, coef_nb_high_a_var
)

coef_income <- coef_income%>%mutate(
  crime_type = factor(crime_type, levels = c("violent", "property", "total crime")),
  model = factor(model, levels = c("nb_low", "nb_med", "nb_high")),
  signif = cut(
    p.value, 
    breaks = c(0, 0.01, 0.05, 0.1, 1), 
    labels = c("*\n*\n*", "*\n*", "*", "")
  )
  # x_offset = c(0.8, 1, 1.2, 1.8, 2, 2.2, 2.8, 3, 3.2)
)

p_income <- ggplot(coef_income, aes(x = model, y = estimate, color = crime_type)) +
  geom_point(
    position = position_dodge(width = 0.6),
    size = 4
  ) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(width = 0.6),
    width = 0.1,
    linewidth = 1
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dotted"
  ) +
  geom_text(
    aes(
      x = as.numeric(factor(model)) - 0.05,
      y = (conf.high + conf.low) / 2 + 0.06,    
      label = signif
    ),
    position = position_dodge(width = 0.6),
    size = 10,
    show.legend = FALSE,
    lineheight = 0.3
  ) +
  labs(
    x = "Income Level",
    y = "Estimated DID Coefficient",
    color = "",
    # shape = "Significance"
  ) +
  scale_x_discrete(
    limits = c("nb_low", "nb_med", "nb_high"),
    labels = c(
      "nb_low" = "Low-income",
      "nb_med" = "Middle-income",
      "nb_high" = "High-income"
    )
  ) +
  scale_y_continuous(limits = c(-1.2, 1.2)) +
  scale_color_manual(values = c(
    "violent" = "#4E79A7",
    "property" = "#B07AA1",
    "total crime" = "#9CBA43"
  )) +
  # guides(
  #   color = guide_legend(order = 1),
  #   shape = guide_legend(order = 2)
  # ) +
  theme(
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 18),
    axis.line = element_line(linewidth = 0.8),
    axis.ticks = element_line(linewidth = 1),
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    legend.position = c(0.25, 0.05),
    legend.text = element_text(size = 16),
    legend.direction = "horizontal"
    # legend.title = element_text(size = 16)
  )

ggsave("E:/did_process/figures/p_income.png",plot = p_income, width = 9, height = 7, dpi = 300)


#Racial grouping coefficient Images----------------------------------------------
#White---------------------------------------------------------------------------
coef_nb_White_v <- as.data.frame(coef(summary(mod_did_nb_White_v))) %>%
  mutate(
    term = rownames(.),
    conf.low = Estimate - 1.96 * `Std. Error`,
    conf.high = Estimate + 1.96 * `Std. Error`
  ) %>%
  rename(
    estimate = Estimate,
    std.error = `Std. Error`,
    statistic = `z value`,
    p.value = `Pr(>|z|)`
  )

coef_nb_White_v_var <- coef_nb_White_v%>%
  filter(term=="did")%>%
  mutate(
    crime_type = "violent",
    model = "nb_W"
  )

coef_nb_White_p <- as.data.frame(coef(summary(mod_did_nb_White_p))) %>%
  mutate(
    term = rownames(.),
    conf.low = Estimate - 1.96 * `Std. Error`,
    conf.high = Estimate + 1.96 * `Std. Error`
  ) %>%
  rename(
    estimate = Estimate,
    std.error = `Std. Error`,
    statistic = `z value`,
    p.value = `Pr(>|z|)`
  )

coef_nb_White_p_var <- coef_nb_White_p%>%
  filter(term=="did")%>%
  mutate(
    crime_type = "property",
    model = "nb_W"
  )

coef_nb_White_a <- as.data.frame(coef(summary(mod_did_nb_White_a))) %>%
  mutate(
    term = rownames(.),
    conf.low = Estimate - 1.96 * `Std. Error`,
    conf.high = Estimate + 1.96 * `Std. Error`
  ) %>%
  rename(
    estimate = Estimate,
    std.error = `Std. Error`,
    statistic = `z value`,
    p.value = `Pr(>|z|)`
  )

coef_nb_White_a_var <- coef_nb_White_a%>%
  filter(term=="did")%>%
  mutate(
    crime_type = "total crime",
    model = "nb_W"
  )

#Black---------------------------------------------------------------------------
coef_nb_Black_v <- as.data.frame(coef(summary(mod_did_nb_Black_v))) %>%
  mutate(
    term = rownames(.),
    conf.low = Estimate - 1.96 * `Std. Error`,
    conf.high = Estimate + 1.96 * `Std. Error`
  ) %>%
  rename(
    estimate = Estimate,
    std.error = `Std. Error`,
    statistic = `z value`,
    p.value = `Pr(>|z|)`
  )

coef_nb_Black_v_var <- coef_nb_Black_v%>%
  filter(term=="did")%>%
  mutate(
    crime_type = "violent",
    model = "nb_B"
  )

coef_nb_Black_p <- as.data.frame(coef(summary(mod_did_nb_Black_p))) %>%
  mutate(
    term = rownames(.),
    conf.low = Estimate - 1.96 * `Std. Error`,
    conf.high = Estimate + 1.96 * `Std. Error`
  ) %>%
  rename(
    estimate = Estimate,
    std.error = `Std. Error`,
    statistic = `z value`,
    p.value = `Pr(>|z|)`
  )

coef_nb_Black_p_var <- coef_nb_Black_p%>%
  filter(term=="did")%>%
  mutate(
    crime_type = "property",
    model = "nb_B"
  )

coef_nb_Black_a <- as.data.frame(coef(summary(mod_did_nb_Black_a))) %>%
  mutate(
    term = rownames(.),
    conf.low = Estimate - 1.96 * `Std. Error`,
    conf.high = Estimate + 1.96 * `Std. Error`
  ) %>%
  rename(
    estimate = Estimate,
    std.error = `Std. Error`,
    statistic = `z value`,
    p.value = `Pr(>|z|)`
  )

coef_nb_Black_a_var <- coef_nb_Black_a%>%
  filter(term=="did")%>%
  mutate(
    crime_type = "total crime",
    model = "nb_B"
  )


#Asian---------------------------------------------------------------------------
coef_nb_Asian_v <- as.data.frame(coef(summary(mod_did_nb_Asian_v))) %>%
  mutate(
    term = rownames(.),
    conf.low = Estimate - 1.96 * `Std. Error`,
    conf.high = Estimate + 1.96 * `Std. Error`
  ) %>%
  rename(
    estimate = Estimate,
    std.error = `Std. Error`,
    statistic = `z value`,
    p.value = `Pr(>|z|)`
  )

coef_nb_Asian_v_var <- coef_nb_Asian_v%>%
  filter(term=="did")%>%
  mutate(
    crime_type = "violent",
    model = "nb_A"
  )

coef_nb_Asian_p <- as.data.frame(coef(summary(mod_did_nb_Asian_p))) %>%
  mutate(
    term = rownames(.),
    conf.low = Estimate - 1.96 * `Std. Error`,
    conf.high = Estimate + 1.96 * `Std. Error`
  ) %>%
  rename(
    estimate = Estimate,
    std.error = `Std. Error`,
    statistic = `z value`,
    p.value = `Pr(>|z|)`
  )

coef_nb_Asian_p_var <- coef_nb_Asian_p%>%
  filter(term=="did")%>%
  mutate(
    crime_type = "property",
    model = "nb_A"
  )

coef_nb_Asian_a <- as.data.frame(coef(summary(mod_did_nb_Asian_a))) %>%
  mutate(
    term = rownames(.),
    conf.low = Estimate - 1.96 * `Std. Error`,
    conf.high = Estimate + 1.96 * `Std. Error`
  ) %>%
  rename(
    estimate = Estimate,
    std.error = `Std. Error`,
    statistic = `z value`,
    p.value = `Pr(>|z|)`
  )

coef_nb_Asian_a_var <- coef_nb_Asian_a%>%
  filter(term=="did")%>%
  mutate(
    crime_type = "total crime",
    model = "nb_A"
  )

#Mixed---------------------------------------------------------------------------
coef_nb_Mixed_v <- as.data.frame(coef(summary(mod_did_nb_Mixed_v))) %>%
  mutate(
    term = rownames(.),
    conf.low = Estimate - 1.96 * `Std. Error`,
    conf.high = Estimate + 1.96 * `Std. Error`
  ) %>%
  rename(
    estimate = Estimate,
    std.error = `Std. Error`,
    statistic = `z value`,
    p.value = `Pr(>|z|)`
  )

coef_nb_Mixed_v_var <- coef_nb_Mixed_v%>%
  filter(term=="did")%>%
  mutate(
    crime_type = "violent",
    model = "nb_M"
  )

coef_nb_Mixed_p <- as.data.frame(coef(summary(mod_did_nb_Mixed_p))) %>%
  mutate(
    term = rownames(.),
    conf.low = Estimate - 1.96 * `Std. Error`,
    conf.high = Estimate + 1.96 * `Std. Error`
  ) %>%
  rename(
    estimate = Estimate,
    std.error = `Std. Error`,
    statistic = `z value`,
    p.value = `Pr(>|z|)`
  )

coef_nb_Mixed_p_var <- coef_nb_Mixed_p%>%
  filter(term=="did")%>%
  mutate(
    crime_type = "property",
    model = "nb_M"
  )

coef_nb_Mixed_a <- as.data.frame(coef(summary(mod_did_nb_Mixed_a))) %>%
  mutate(
    term = rownames(.),
    conf.low = Estimate - 1.96 * `Std. Error`,
    conf.high = Estimate + 1.96 * `Std. Error`
  ) %>%
  rename(
    estimate = Estimate,
    std.error = `Std. Error`,
    statistic = `z value`,
    p.value = `Pr(>|z|)`
  )

coef_nb_Mixed_a_var <- coef_nb_Mixed_a%>%
  filter(term=="did")%>%
  mutate(
    crime_type = "total crime",
    model = "nb_M"
  )

#Draw----------------------------------------------------------------------------

coef_race <- rbind(
  coef_nb_White_v_var, coef_nb_White_p_var, coef_nb_White_a_var,
  coef_nb_Black_v_var, coef_nb_Black_p_var, coef_nb_Black_a_var,
  coef_nb_Asian_v_var, coef_nb_Asian_p_var, coef_nb_Asian_a_var,
  coef_nb_Mixed_v_var, coef_nb_Mixed_p_var, coef_nb_Mixed_a_var
)

coef_race <- coef_race%>%mutate(
  crime_type = factor(crime_type, levels = c("violent", "property", "total crime")),
  model = factor(model, levels = c("nb_W", "nb_B", "nb_A", "nb_M")),
  signif = cut(
    p.value, 
    breaks = c(0, 0.01, 0.05, 0.1, 1), 
    labels = c("*\n*\n*", "*\n*", "*", "")
  )
  # x_offset = c(0.8, 1, 1.2, 1.8, 2, 2.2, 2.8, 3, 3.2, 3.8, 4, 4.2)
)

p_race <- ggplot(coef_race, aes(x = model, y = estimate, color = crime_type)) +
  geom_point(
    position = position_dodge(width = 0.6),
    size = 4
  ) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(width = 0.6),
    width = 0.1,
    linewidth = 1
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dotted"
  ) +
  geom_text(
    aes(
      x = as.numeric(factor(model)) - 0.05,
      y = (conf.high + conf.low) / 2 + 0.06,   
      label = signif
    ),
    position = position_dodge(width = 0.6),
    size = 10,
    show.legend = FALSE,
    lineheight = 0.3
  ) +
  labs(
    x = "Race",
    y = "Coefficient of Interaction Terms",
    color = "",
    # shape = "Significance"
  ) +
  scale_x_discrete(
    limits = c("nb_W", "nb_B", "nb_A", "nb_M"),
    labels = c(
      "nb_W" = "White",
      "nb_B" = "Black",
      "nb_A" = "Asian",
      "nb_M" = "Mixed"
    )
  ) +
  scale_y_continuous(limits = c(-1.2, 1.2)) +
  scale_color_manual(values = c(
    "violent" = "#4E79A7",
    "property" = "#B07AA1",
    "total crime" = "#9CBA43"
  )) +
  # guides(
  #   color = guide_legend(order = 1),
  #   shape = guide_legend(order = 2)
  # ) +
  theme(
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 18),
    axis.line = element_line(linewidth = 0.8),
    axis.ticks = element_line(linewidth = 1),
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    legend.position = c(0.25, 0.05),
    legend.text = element_text(size = 16),
    legend.direction = "horizontal"
    # legend.title = element_text(size = 16)
  )

ggsave("E:/did_process/figures/p_race.png",plot = p_race, width = 9, height = 7, dpi = 300)


#Crime grouping coefficient Images-----------------------------------------------
#low-crime-----------------------------------------------------------------------
coef_nb_lc_v <- as.data.frame(coef(summary(mod_did_nb_lc_v))) %>%
  mutate(
    term = rownames(.),
    conf.low = Estimate - 1.96 * `Std. Error`,
    conf.high = Estimate + 1.96 * `Std. Error`
  ) %>%
  rename(
    estimate = Estimate,
    std.error = `Std. Error`,
    statistic = `z value`,
    p.value = `Pr(>|z|)`
  )

coef_nb_lc_v_var <- coef_nb_lc_v%>%
  filter(term=="did")%>%
  mutate(
    crime_type = "violent",
    model = "nb_lc"
  )

coef_nb_lc_p <- as.data.frame(coef(summary(mod_did_nb_lc_p))) %>%
  mutate(
    term = rownames(.),
    conf.low = Estimate - 1.96 * `Std. Error`,
    conf.high = Estimate + 1.96 * `Std. Error`
  ) %>%
  rename(
    estimate = Estimate,
    std.error = `Std. Error`,
    statistic = `z value`,
    p.value = `Pr(>|z|)`
  )

coef_nb_lc_p_var <- coef_nb_lc_p%>%
  filter(term=="did")%>%
  mutate(
    crime_type = "property",
    model = "nb_lc"
  )

coef_nb_lc_a <- as.data.frame(coef(summary(mod_did_nb_lc_a))) %>%
  mutate(
    term = rownames(.),
    conf.low = Estimate - 1.96 * `Std. Error`,
    conf.high = Estimate + 1.96 * `Std. Error`
  ) %>%
  rename(
    estimate = Estimate,
    std.error = `Std. Error`,
    statistic = `z value`,
    p.value = `Pr(>|z|)`
  )

coef_nb_lc_a_var <- coef_nb_lc_a%>%
  filter(term=="did")%>%
  mutate(
    crime_type = "total crime",
    model = "nb_lc"
  )

#medium-crime-----------------------------------------------------------------------
coef_nb_mc_v <- as.data.frame(coef(summary(mod_did_nb_mc_v))) %>%
  mutate(
    term = rownames(.),
    conf.low = Estimate - 1.96 * `Std. Error`,
    conf.high = Estimate + 1.96 * `Std. Error`
  ) %>%
  rename(
    estimate = Estimate,
    std.error = `Std. Error`,
    statistic = `z value`,
    p.value = `Pr(>|z|)`
  )

coef_nb_mc_v_var <- coef_nb_mc_v%>%
  filter(term=="did")%>%
  mutate(
    crime_type = "violent",
    model = "nb_mc"
  )

coef_nb_mc_p <- as.data.frame(coef(summary(mod_did_nb_mc_p))) %>%
  mutate(
    term = rownames(.),
    conf.low = Estimate - 1.96 * `Std. Error`,
    conf.high = Estimate + 1.96 * `Std. Error`
  ) %>%
  rename(
    estimate = Estimate,
    std.error = `Std. Error`,
    statistic = `z value`,
    p.value = `Pr(>|z|)`
  )

coef_nb_mc_p_var <- coef_nb_mc_p%>%
  filter(term=="did")%>%
  mutate(
    crime_type = "property",
    model = "nb_mc"
  )

coef_nb_mc_a <- as.data.frame(coef(summary(mod_did_nb_mc_a))) %>%
  mutate(
    term = rownames(.),
    conf.low = Estimate - 1.96 * `Std. Error`,
    conf.high = Estimate + 1.96 * `Std. Error`
  ) %>%
  rename(
    estimate = Estimate,
    std.error = `Std. Error`,
    statistic = `z value`,
    p.value = `Pr(>|z|)`
  )

coef_nb_mc_a_var <- coef_nb_mc_a%>%
  filter(term=="did")%>%
  mutate(
    crime_type = "total crime",
    model = "nb_mc"
  )

#high-crime-----------------------------------------------------------------------
coef_nb_hc_v <- as.data.frame(coef(summary(mod_did_nb_hc_v))) %>%
  mutate(
    term = rownames(.),
    conf.low = Estimate - 1.96 * `Std. Error`,
    conf.high = Estimate + 1.96 * `Std. Error`
  ) %>%
  rename(
    estimate = Estimate,
    std.error = `Std. Error`,
    statistic = `z value`,
    p.value = `Pr(>|z|)`
  )

coef_nb_hc_v_var <- coef_nb_hc_v%>%
  filter(term=="did")%>%
  mutate(
    crime_type = "violent",
    model = "nb_hc"
  )

coef_nb_hc_p <- as.data.frame(coef(summary(mod_did_nb_hc_p))) %>%
  mutate(
    term = rownames(.),
    conf.low = Estimate - 1.96 * `Std. Error`,
    conf.high = Estimate + 1.96 * `Std. Error`
  ) %>%
  rename(
    estimate = Estimate,
    std.error = `Std. Error`,
    statistic = `z value`,
    p.value = `Pr(>|z|)`
  )

coef_nb_hc_p_var <- coef_nb_hc_p%>%
  filter(term=="did")%>%
  mutate(
    crime_type = "property",
    model = "nb_hc"
  )

coef_nb_hc_a <- as.data.frame(coef(summary(mod_did_nb_hc_a))) %>%
  mutate(
    term = rownames(.),
    conf.low = Estimate - 1.96 * `Std. Error`,
    conf.high = Estimate + 1.96 * `Std. Error`
  ) %>%
  rename(
    estimate = Estimate,
    std.error = `Std. Error`,
    statistic = `z value`,
    p.value = `Pr(>|z|)`
  )

coef_nb_hc_a_var <- coef_nb_hc_a%>%
  filter(term=="did")%>%
  mutate(
    crime_type = "total crime",
    model = "nb_hc"
  )

#Draw----------------------------------------------------------------------------

coef_crime <- rbind(
  coef_nb_hc_v_var, coef_nb_hc_p_var, coef_nb_hc_a_var,
  coef_nb_mc_v_var, coef_nb_mc_p_var, coef_nb_mc_a_var,
  coef_nb_lc_v_var, coef_nb_lc_p_var, coef_nb_lc_a_var
)

coef_crime <- coef_crime%>%mutate(
  crime_type = factor(crime_type, levels = c("violent", "property", "total crime")),
  model = factor(model, levels = c("nb_lc", "nb_mc", "nb_hc")),
  signif = cut(
    p.value, 
    breaks = c(0, 0.01, 0.05, 0.1, 1), 
    labels = c("*\n*\n*", "*\n*", "*", "")
  )
  # x_offset = c(0.8, 1, 1.2, 1.8, 2, 2.2, 2.8, 3, 3.2, 3.8, 4, 4.2)
)

p_crime <- ggplot(coef_crime, aes(x = model, y = estimate, color = crime_type)) +
  geom_point(
    position = position_dodge(width = 0.6),
    size = 4
  ) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(width = 0.6),
    width = 0.1,
    linewidth = 1
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dotted"
  ) +
  geom_text(
    aes(
      x = as.numeric(factor(model)) - 0.05,
      y = (conf.high + conf.low) / 2 + 0.06,   
      label = signif
    ),
    position = position_dodge(width = 0.6),
    size = 10,
    show.legend = FALSE,
    lineheight = 0.3
  ) +
  labs(
    x = "Streets Classification",
    y = "Coefficient of Interaction Terms",
    color = "",
    # shape = "Significance"
  ) +
  scale_x_discrete(
    limits = c("nb_lc", "nb_mc", "nb_hc"),
    labels = c(
      "nb_lc" = "Low baseline crime",
      "nb_mc" = "Medium baseline crime",
      "nb_hc" = "High baseline crime"
    )
  ) +
  scale_y_continuous(limits = c(-1.2, 1.2)) +
  scale_color_manual(values = c(
    "violent" = "#4E79A7",
    "property" = "#B07AA1",
    "total crime" = "#9CBA43"
  )) +
  # guides(
  #   color = guide_legend(order = 1),
  #   shape = guide_legend(order = 2)
  # ) +
  theme(
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 18),
    axis.line = element_line(linewidth = 0.8),
    axis.ticks = element_line(linewidth = 1),
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    legend.position = c(0.25, 0.05),
    legend.text = element_text(size = 16),
    legend.direction = "horizontal"
    # legend.title = element_text(size = 16)
  )

ggsave("E:/did_process/figures/p_crime.png",plot = p_crime, width = 9, height = 7, dpi = 300)



#Heterogeneity+Intensity+Stewardship Analysis: Combining 6 Images----------------
coef_income <- coef_income %>%
  mutate(
    panel = "(A) Income level",
    subgroup = case_when(
      model == "nb_low"  ~ "low",
      model == "nb_med"  ~ "middle",
      model == "nb_high" ~ "high"
    ),
    subgroup = factor(
      subgroup,
      levels = c("low", "middle", "high")
    )
  )

coef_race <- coef_race %>%
  mutate(
    panel = "(B) Race composition",
    subgroup = case_when(
      model == "nb_W" ~ "White",
      model == "nb_B" ~ "Black",
      model == "nb_A" ~ "Asian",
      model == "nb_M" ~ "Mixed"
    ),
    subgroup = factor(
      subgroup,
      levels = c("White", "Black","Asian", "Mixed")
    )
  )

coef_tree_scar <- coef_tree_scar %>%
  mutate(
    panel = "(C) Baseline tree scarcity",
    subgroup = case_when(
      model == "nb_lscar" ~ "low",
      model == "nb_mscar" ~ "middle",
      model == "nb_hscar" ~ "high"
    ),
    subgroup = factor(
      subgroup,
      levels = c("low", "middle", "high")
    )
  )

coef_crime <- coef_crime %>%
  mutate(
    panel = "(D) Baseline crime level",
    subgroup = case_when(
      model == "nb_lc" ~ "low",
      model == "nb_mc" ~ "middle",
      model == "nb_hc" ~ "high"
    ),
    subgroup = factor(
      subgroup,
      levels = c("low", "middle", "high")
    )
  )

coef_intensity2 <- coef_intensity2 %>%
  mutate(
    panel = "(E) Planting",
    subgroup = case_when(
      term == "did31" ~ "low",
      term == "did32" ~ "middle",
      term == "did33" ~ "high"
    ),
    subgroup = factor(
      subgroup,
      levels = c("low", "middle", "high")
    )
  )

coef_steward2 <- coef_steward2 %>%
  mutate(
    panel = "(F) Stewardship",
    subgroup = case_when(
      term == "did31" ~ "low",
      term == "did32" ~ "middle",
      term == "did33" ~ "high"
    ),
    subgroup = factor(
      subgroup,
      levels = c("low", "middle", "high")
    )
  )


coef_all <- bind_rows(
  coef_income,
  coef_race,
  coef_tree_scar,
  coef_crime,
  coef_intensity2,
  coef_steward2
)


coef_all$panel <- factor(
  coef_all$panel,
  levels = c(
    "(A) Income level",
    "(B) Race composition",
    "(C) Baseline tree scarcity",
    "(D) Baseline crime level",
    "(E) Planting",
    "(F) Stewardship"
  )
)


p_facet <- ggplot(coef_all, aes(x = subgroup, y = estimate, color = crime_type)) +
  geom_hline(
    yintercept = 0,
    linetype = "dotted",
    linewidth = 0.7,
    color = "grey"
  ) +
  geom_point(
    position = position_dodge(width = 0.6),
    size = 3
  ) +
  geom_errorbar(
    aes(
      ymin = conf.low,
      ymax = conf.high
    ),
    position = position_dodge(width = 0.6),
    width = 0.15,
    linewidth = 0.6
  ) +
  geom_text(
    aes(
      label = signif,
      # y = conf.high + 0.05
      y = (conf.high + conf.low) / 2 + 0.08
    ),
    position = position_dodge(width = 0.6),
    # nudge_x = -0.12,
    hjust = 1.4,
    size = 6,
    show.legend = FALSE,
    lineheight = 0.3
  ) +
  facet_wrap(~ panel, scales = "free_x", ncol = 2) +
  scale_y_continuous(limits = c(-1.2, 1.2)) +
  scale_color_manual(
    values = c(
      "violent" = "#4E79A7",
      "property" = "#B07AA1",
      "total crime" = "#9CBA43"
    )
  ) +
  labs(
    x = NULL,
    y = "Estimated DiD Coefficient",
    color = NULL
  ) +
  theme_bw() +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(
      size = 13, 
      face = "bold", 
      hjust = 0, 
      vjust = 0.2, 
      # margin = margin(l = 5)
    ),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 15),
    panel.grid = element_blank(),
    # panel.spacing = unit(1, "lines"),
    axis.line = element_line(linewidth = 0.8),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.text = element_text(size = 12)
  )

p_facet

ggsave("E:/did_process/revise/figures/p_facet.png",plot = p_facet, width = 10, height = 10, dpi = 600)


#intensity+steward: Combining 2 Images-------------------------------------------

coef_intensity3 <- coef_intensity3 %>%
  mutate(
    panel = "(A) DiD × Four-category Planting",
    subgroup = case_when(
      term == "did41" ~ "1/4",
      term == "did42" ~ "2/4",
      term == "did43" ~ "3/4",
      term == "did44" ~ "4/4"
    ),
    subgroup = factor(
      subgroup,
      levels = c("1/4", "2/4","3/4", "4/4")
    )
  )

coef_steward3 <- coef_steward3 %>%
  mutate(
    panel = "(B) DiD × Four-category Stewardship",
    subgroup = case_when(
      term == "did41" ~ "1/4",
      term == "did42" ~ "2/4",
      term == "did43" ~ "3/4",
      term == "did44" ~ "4/4"
    ),
    subgroup = factor(
      subgroup,
      levels = c("1/4", "2/4","3/4", "4/4")
    )
  )


coef_is <- bind_rows(
  coef_intensity3,
  coef_steward3
)


coef_is$panel <- factor(
  coef_is$panel,
  levels = c(
    "(A) DiD × Four-category Planting",
    "(B) DiD × Four-category Stewardship"
  )
)


p_combine <- ggplot(coef_is, aes(x = subgroup, y = estimate, color = crime_type)) +
  geom_hline(
    yintercept = 0,
    linetype = "dotted",
    linewidth = 0.7,
    color = "grey"
  ) +
  geom_point(
    position = position_dodge(width = 0.6),
    size = 3
  ) +
  geom_errorbar(
    aes(
      ymin = conf.low,
      ymax = conf.high
    ),
    position = position_dodge(width = 0.6),
    width = 0.15,
    linewidth = 0.6
  ) +
  geom_text(
    aes(
      label = signif,
      # y = conf.high + 0.05
      y = (conf.high + conf.low) / 2 + 0.06
    ),
    position = position_dodge(width = 0.6),
    # nudge_x = -0.12,
    hjust = 1.4,
    size = 6,
    show.legend = FALSE,
    lineheight = 0.3
  ) +
  facet_wrap(~ panel, scales = "free_x", ncol = 2) +
  scale_y_continuous(limits = c(-0.8, 0.8)) +
  scale_color_manual(
    values = c(
      "violent" = "#4E79A7",
      "property" = "#B07AA1",
      "total crime" = "#9CBA43"
    )
  ) +
  labs(
    x = NULL,
    y = "Estimated DiD Coefficient",
    color = NULL
  ) +
  theme_bw() +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(
      size = 13, 
      face = "bold", 
      hjust = 0, 
      vjust = 0.2, 
      # margin = margin(l = 5)
    ),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 15),
    panel.grid = element_blank(),
    # panel.spacing = unit(1, "lines"),
    axis.line = element_line(linewidth = 0.8),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.text = element_text(size = 12)
  )
              
p_combine

ggsave("E:/did_process/revise/figures/p_combine.png",plot = p_combine, width = 10, height = 4, dpi = 300)
