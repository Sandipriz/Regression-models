library(tidyverse)
library(lme4)
library(ggplot2)

##Comparing between with P and without P and with N and without N
data3 <- all_years
attach <- data3

##generating new columns depending on old one

data3$N_treatment=ifelse(data3$Treatment %in% c("NP","N"), "With_N","Without_N") 
data3$P_treatment=ifelse(data3$Treatment %in% c("NP","P"), "With_P","Without_P")
                       
##count the category of rows under the column
data3 %>% count(data3$N_treatment)
data3 %>% count(data3$P_treatment) 

##subset data as per the age class
young_tree <- subset(data3, Age=="~30 years old")
View(young_tree)
medium_tree <- subset(data3, Age=="~60 years old")
mature_tree <- subset(data3, Age=="~100 years old")

##test of significance of N_treatments across the age class
y_t.aov <- aov(young_tree$mean.max.canopy.ht.aop~young_tree$N_treatment)
y_t.aov
summary(y_t.aov)
TukeyHSD(y_t.aov)

y_t.aov <- aov(young_tree$mean.max.canopy.ht.aop~young_tree$P_treatment)
y_t.aov
summary(y_t.aov)
TukeyHSD(y_t.aov)

##display significant difference in plot
ggplot(data= young_tree,
      aes(x= Years,
          y= mean.max.canopy.ht.aop,
          color=as.factor(P_treatment)))+
  geom_boxplot()
          
            fill(young_tree$P_treatment))


me_t.aov <- aov(medium_tree$mean.max.canopy.ht.aop~medium_tree$N_treatment)
summary(me_t.aov)
TukeyHSD(me_t.aov)

me_t.aov <- aov(medium_tree$mean.max.canopy.ht.aop~medium_tree$P_treatment)
summary(me_t.aov)
TukeyHSD(me_t.aov)

ma_t.aov <- aov(mature_tree$mean.max.canopy.ht.aop~mature_tree$N_treatment)
summary(ma_t.aov)
TukeyHSD(ma_t.aov)

ma_t.aov <- aov(mature_tree$mean.max.canopy.ht.aop~mature_tree$P_treatment)
summary(ma_t.aov)
TukeyHSD(ma_t.aov)


##checking rumple significance against the treatment effect
y_rumple.aov <- aov(young_tree$rumple.aop~young_tree$N_treatment)
summary(y_rumple.aov)
TukeyHSD(y_rumple.aov)

y_rumple.aov <- aov(young_tree$rumple.aop~young_tree$P_treatment)
summary(y_rumple.aov)
TukeyHSD(y_rumple.aov)

me_rumple.aov <- aov(medium_tree$rumple.aop~medium_tree$N_treatment)
summary(me_rumple.aov)
TukeyHSD(me_rumple.aov)

me_rumple.aov <- aov(medium_tree$rumple.aop~medium_tree$P_treatment)
summary(me_rumple.aov)
TukeyHSD(me_rumple.aov)

ma_rumple.aov<- aov(mature_tree$rumple.aop~mature_tree$N_treatment)
summary(ma_rumple.aov)
TukeyHSD(ma_rumple.aov)

ma_rumple.aov<- aov(mature_tree$rumple.aop~mature_tree$N_ttreatment)
summary(ma_rumple.aov)
TukeyHSD(ma_rumple.aov)

##checking deep gap fraction significance against the treatment effect
y_deepgap.frac.aov <- aov(young_tree$deepgap.fraction.aop~young_tree$N_treatment)
summary(y_deepgap.frac.aov)
TukeyHSD(y_deepgap.frac.aov)

y_deepgap.frac.aov <- aov(young_tree$deepgap.fraction.aop~young_tree$P_treatment)
summary(y_deepgap.frac.aov)
TukeyHSD(y_deepgap.frac.aov)

me_deepgap.frac.aov <- aov(medium_tree$deepgap.fraction.aop~medium_tree$N_treatment)
summary(me_deepgap.frac.aov)
TukeyHSD(me_deepgap.frac.aov)

me_deepgap.frac.aov <- aov(medium_tree$deepgap.fraction.aop~medium_tree$P_treatment)
summary(me_deepgap.frac.aov)
TukeyHSD(me_deepgap.frac.aov)

ma_deepgap.frac.aov <- aov(mature_tree$deepgap.fraction.aop~mature_tree$N_treatment)
summary(ma_deepgap.frac.aov)
TukeyHSD(ma_deepgap.frac.aov)

ma_deepgap.frac.aov <- aov(mature_tree$deepgap.fraction.aop~mature_tree$P_treatment)
summary(ma_deepgap.frac.aov)
TukeyHSD(ma_deepgap.frac.aov)

##checking rugosity significance against the treatment effect
y_rugosity.aov <- aov(young_tree$top.rugosity.aop~young_tree$N_treatment)
summary(y_rugosity.aov)
TukeyHSD(y_rugosity.aov)

y_rugosity.aov <- aov(young_tree$top.rugosity.aop~young_tree$P_treatment)
summary(y_rugosity.aov)
TukeyHSD(y_rugosity.aov)

me_rugosity.aov <- aov(medium_tree$top.rugosity.aop~medium_tree$N_treatment)
summary(me_rugosity.aov)
TukeyHSD(me_rugosity.aov)

me_rugosity.aov <- aov(medium_tree$top.rugosity.aop~medium_tree$P_treatment)
summary(me_rugosity.aov)
TukeyHSD(me_rugosity.aov)

ma_rugosity.aov <- aov(mature_tree$top.rugosity.aop~mature_tree$N_treatment)
summary(ma_rugosity.aov)
TukeyHSD(ma_rugosity.aov)

ma_rugosity.aov <- aov(mature_tree$top.rugosity.aop~mature_tree$P_treatment)
summary(ma_rugosity.aov)
TukeyHSD(ma_rugosity.aov)

##checking std of height significance against the treatment effect
y_vert_sd.aov <- aov(young_tree$vert.sd.aop~young_tree$N_treatment)
summary(y_vert_sd.aov)
TukeyHSD(y_vert_sd.aov)

y_vert_sd.aov <- aov(young_tree$vert.sd.aop~young_tree$P_treatment)
summary(y_vert_sd.aov)
TukeyHSD(y_vert_sd.aov)

me_vert_sd.aov <- aov(medium_tree$vert.sd.aop~medium_tree$N_treatment)
summary(me_vert_sd.aov)
TukeyHSD(me_vert_sd.aov)

me_vert_sd.aov <- aov(medium_tree$vert.sd.aop~medium_tree$P_treatment)
summary(me_vert_sd.aov)
TukeyHSD(me_vert_sd.aov)

ma_vert_sd.aov <- aov(mature_tree$vert.sd.aop~mature_tree$N_treatment)
summary(ma_vert_sd.aov)
TukeyHSD(ma_vert_sd.aov)

ma_vert_sd.aov <- aov(mature_tree$vert.sd.aop~mature_tree$P_treatment)
summary(ma_vert_sd.aov)
TukeyHSD(ma_vert_sd.aov)

##checking Vegetation Area Index significance against the treatment effect
y_VAI.aov <- aov(young_tree$VAI.AOP.aop~young_tree$N_treatment)
summary(y_VAI.aov)
TukeyHSD(y_VAI.aov)

y_VAI.aov <- aov(young_tree$VAI.AOP.aop~young_tree$P_treatment)
summary(y_VAI.aov)
TukeyHSD(y_VAI.aov)

me_VAI.aov <- aov(medium_tree$VAI.AOP.aop~medium_tree$N_treatment)
summary(me_VAI.aov)
TukeyHSD(me_VAI.aov)

me_VAI.aov <- aov(medium_tree$VAI.AOP.aop~medium_tree$P_treatment)
summary(me_VAI.aov)
TukeyHSD(me_VAI.aov)

ma_VAI.aov <- aov(mature_tree$VAI.AOP.aop~mature_tree$N_treatment)
summary(ma_VAI.aov)
TukeyHSD(ma_VAI.aov)

ma_VAI.aov <- aov(mature_tree$VAI.AOP.aop~mature_tree$P_treatment)
summary(ma_VAI.aov)
TukeyHSD(ma_VAI.aov)




















