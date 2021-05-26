library(tidyverse)
library(scales)
library(lme4)
library(lmtest)
library(influence.ME)
library(car)

# meaningless comment added when playing with Github

library(readr)
Feed <- read_csv("Feed.csv", col_types = cols(PIT = col_character()))
View(Feed)

getwd()

summary(Feed)

Feed %>% 
  group_by(TREATMENT,PIT,Shuttle) %>% 
  summarise(
    count = n()
    ) %>% 
  write.csv("delete.csv")
  
summary(Feed)


shapiro.test(Feed$pCO2)#Shapiro test for normality

fligner.test((pCO2) ~ interaction(Shuttle,TREATMENT), data = Feed)#Fligner-Killeen test for equal variances
bartlett.test((pCO2) ~ interaction(Shuttle,TREATMENT), data = Feed)
leveneTest((pCO2) ~ Shuttle*TREATMENT, data = Feed)

feed1 <- lmer((rank(pCO2))~Shuttle*TREATMENT+(1|TREATMENT:PIT), data = Feed)#random intercepts only - works!
feed2 <- lmer(pCO2~Shuttle*TREATMENT+(Shuttle||TREATMENT:PIT), data = Feed)#R won't run this model

summary(feed1)
plot(feed1) #Makes a plot of model residuals
qqnorm(resid(feed1)) #Makes a Q-Q plot to confirm normality.
coef(feed1)
hist(resid(feed1))

resid<-resid(feed1) 
hist(resid(feed1))
plot(resid~fitted(feed1))
qqnorm((resid(feed1))) #Makes a Q-Q plot to confirm normality.
qqline(resid(feed1))
plot(Feed$TREATMENT,resid,xlab="Treatment",ylab="Resid")
plot(Feed$Shuttle,resid,xlab="Temp_Bin",ylab="Resid")
acf(residuals(feed1),main="")
pacf(residuals(feed1),main="")

coefplot(feed1)

library(lmerTest) #provides estimates of both numerator and denominator degrees of freedom.  Approved by Ben Bolker in 
# https://stats.stackexchange.com/questions/84268/are-degrees-of-freedom-in-lmertestanova-correct-they-are-very-different-from
#Kenward Roger method appears to be preferrable as per https://doi.org/10.1198/108571102726
summary(feed1, ddf="Satterthwaite")
summary(feed1, ddf="Kenward-Roger")
anova(feed1)
anova(feed1,ddf="Kenward-Roger")
anova(feed1,ddf="lme4")
citation(package = "lmerTest")

(.packages())
getwd()

###########################################################################################################


Figure2_Data <- read_csv("Figure2_Data.csv", 
                         col_types = cols(PIT = col_character()))
Figure2_Data %>% 
  group_by(TREATMENT,Shuttle) %>% 
  summarise(
    count = n(),
  )

Ordered_Axis <- factor(Figure2_Data$TREATMENT, levels=c('Fasted', 'Fed'))

windows()

ggplot(Figure2_Data, aes(x=Ordered_Axis, y = pCO2, fill=Shuttle, na.rm=TRUE))+
  geom_boxplot(lwd=0.75,color="black", outlier.size = 2.5, outlier.fill = "black", na.rm=TRUE)+
  scale_fill_manual(values=c("white","gray70"))+
  labs(y=expression(bold(paste(italic("p"),"CO"["2"]," (",mu,"atm)"))))+
  xlab("Treatment")+
  theme_classic()+
  theme(plot.title = element_blank(), 
        axis.title.y = element_text(face="bold", colour="black", size=14),
        axis.title.x = element_text(face="bold", colour="black", size=14),
        axis.ticks.length = unit(0.25,"cm"),
        axis.ticks = element_line(size = 0.75, colour = "black"),
        axis.text.y = element_text(face="bold", colour="black", size=12),
        axis.line = element_line(size = 0.75),
        axis.text = element_text(size = 12.0, face = "bold", color = "black"),
        panel.grid.major.x = element_blank(),
        legend.background = element_blank(),
        legend.position = c(0.85,0.9),#first number is left/righ axis from 0-1
        legend.title = element_blank(),
        legend.text = element_text(face = "bold", size = 12),
        aspect.ratio = 2 / (1 + sqrt(5)),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.background = element_blank())+
  stat_summary(fun.y=mean, geom="point", shape=5, size=1.75, stroke=2, na.rm=TRUE,
               color="black", position=position_dodge(width=0.75),show.legend = FALSE)+
  scale_y_continuous(labels = comma)

ggsave("Figure_2.jpeg", dpi = 600)
dev.off()
ggsave("Figure_2.eps", dpi = 600)
dev.off()
