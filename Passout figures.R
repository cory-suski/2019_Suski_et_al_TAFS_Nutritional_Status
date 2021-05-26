
library(tidyverse)
PASS_DAT <- read_csv("PASS_DAT.csv", col_types = cols(PIT = col_character()))

getwd()

PASS_DAT %>% 
  group_by(PIT) %>% 
  summarise(
    count = n()
    ) %>% 
  write.csv("delete2.csv")


#The outputs for these tests should be correct. However, column titles on the input file have changed.
#Code may therefore need to be adjusted slightly (e.g., add underscores) to get stats to run.

t.test(TL~GROUP)
plot(GROUP,TL, data = TL_data)


t.test(`PASS_T`~GROUP)

lm(`PASS_T`~`Rec_T`, data = PASS_DAT)

Welch Two Sample t-test

data:  PASS_T by GROUP
t = 2.2898, df = 20.964, p-value = 0.0325
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
  3.105561 64.617516
sample estimates:
  mean in group fasted    mean in group fed 
217.4000             183.5385 

> t.test(`Rec T`~GROUP)

Welch Two Sample t-test

data:  Rec T by GROUP
t = -0.28201, df = 20.079, p-value = 0.7808
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
  -38.68087  29.46549
sample estimates:
  mean in group fasted    mean in group fed 
127.7000             132.3077 

################################################################################################

#Panel a - boxplot showing time to EQUILIBRIUM LOSS across treatments

Panel_A<-ggplot(PASS_DAT, aes(x=GROUP,y=PASS_T,na.rm=TRUE))+
  geom_boxplot(lwd = 0.75, color = "black", na.rm = TRUE)+
  ylim(0,270)+
  ylab("Time to Equilibrium Loss (Sec)")+
  xlab("Treatment")+
  scale_y_continuous(name = "Time to Equilibrium Loss (Sec)",
                     breaks = seq(50, 270, 100),
                     limits=c(50, 270))+
  annotate("text", x = 1, y = 260, label = "*", fontface = "bold", colour = "black", size = 14)+
  annotate("text", x = 2.5, y = 268, label = "a", fontface = "bold", colour = "black", size = 10)+
  theme_classic()+
  theme(plot.title = element_blank(),
        axis.title.y = element_text(face = "bold",colour = "black", size = 14),
        axis.title.x = element_text(face = "bold", colour = "black", size = 14),
        axis.ticks.length = unit(0.25,"cm"),
        axis.ticks = element_line(size = 0.75, colour = "black"),
        axis.text.y = element_text(face="bold", colour="black", size=12),
        axis.line = element_line(size = 0.75),
        axis.text = element_text(size = 12.0, face = "bold", color = "black"),
        panel.grid.major.x = element_blank(),aspect.ratio =  ((1 + sqrt(5))/ 2),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank())+
  stat_summary(fun.y=mean, geom="point",
               shape=5, size=1.75, stroke=2,
               na.rm=TRUE,color="black",
               position=position_dodge(width=0.75),
               show.legend = FALSE)

#Figure seemed to look better without jitter.  Code is below to add later if desired.
  geom_jitter(mapping = NULL, data = NULL,
              stat = "identity",
              width = 0.2,
              alpha = 0.5,
              na.rm = FALSE,
              show.legend = NA,
              inherit.aes = TRUE)+

###############################################################################################

#Panel b - boxplot showing time to RECOVER across treatments

Panel_B<-ggplot(PASS_DAT, aes(x=GROUP,y=Rec_T,na.rm=TRUE))+
  geom_boxplot(lwd = 0.75, color = "black", na.rm = TRUE)+
  ylim(0,270)+
  ylab("Time to Recover (Sec)")+
  xlab("Treatment")+
    scale_y_continuous(name = "Time to Recover (Sec)",
                       breaks = seq(50, 270, 100),
                       limits=c(50, 270))+
  annotate("text", x = 2.5, y = 268, label = "b", fontface = "bold", colour = "black", size = 10)+
  theme_classic()+
  theme(plot.title = element_blank(),
        axis.title.y = element_text(face = "bold", colour = "black", size = 14),
        axis.title.x = element_text(face = "bold", colour = "black", size = 14),
        axis.ticks.length = unit(0.25,"cm"),
        axis.ticks = element_line(size = 0.75, colour = "black"),
        axis.text.y = element_text(face="bold", colour="black", size=12),
        axis.line = element_line(size = 0.75),
        axis.text = element_text(size = 12.0, face = "bold", color = "black"),
        panel.grid.major.x = element_blank(),
        aspect.ratio =  ((1 + sqrt(5))/ 2),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank())+
  stat_summary(fun.y=mean, geom="point", shape=5, size=1.75, stroke=2, na.rm=TRUE,
               color="black", position=position_dodge(width=0.75),show.legend = FALSE)
  
  #Figure seemed to look better without jitter.  Code is below to add later if desired.
  geom_jitter(mapping = NULL, data = NULL,
              stat = "identity",
              alpha = 0.5,
              width = 0.2,
              na.rm = FALSE,
              show.legend = NA,
              inherit.aes = TRUE)+


#####################################################################################################

#Ignore this figure as it has some problems

P2<-ggplot(PASS_DAT, aes(`Rec T`,`PASS T`,fill = GROUP, colour = GROUP, na.rm=TRUE))
P2+geom_point(aes(shape = GROUP), alpha = 0.85, lwd = 2.75, na.rm = TRUE)+
 geom_smooth(method = lm, se = FALSE, fullrange = TRUE)+
  ylim(50,270)+
  xlim(50,270)+
  ylab("Time to Equilibrium loss (Sec)")+
  xlab("Time to Recovery (Sec)")+
  scale_y_continuous(name = "Time to Equilibrium Loss (Sec)",
                     breaks = seq(50, 270, 100),
                     limits=c(50, 270))+
  annotate("text", x = 268, y = 268, label = "c", fontface = "bold", colour = "black", size = 10)+
  theme_classic()+
  theme(plot.title = element_blank(), axis.title.y = element_text(face = "bold", colour = "black", size = 14),
        axis.title.x = element_text(face = "bold", colour = "black", size = 14),
        axis.ticks.length = unit(0.25,"cm"),
        axis.ticks = element_line(size = 0.75, colour = "black"),
        axis.text.y = element_text(face="bold", colour="black", size=12),
        axis.line = element_line(size = 0.75),
        axis.text = element_text(size = 12.0, face = "bold", color = "black"),
        panel.grid.major.x = element_blank(),aspect.ratio =  ((1 + sqrt(5))/ 2),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        legend.background = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.8,0.2),
        legend.text = element_text(face = "bold", size = 12))+
  scale_color_manual(breaks = c("fasted","fed"), values = c("black","gray39"))+
  ggsave("Passout_to_REC.png", dpi = 1500)

#####################################################################################################################

#Panel c - regression for equilibrium time vs. recovery time

Panel_C<-ggplot(PASS_DAT, aes(x=Rec_T, y=PASS_T,colour = GROUP, group=GROUP, na.rm=TRUE, linetype=GROUP))+
  geom_point(aes(shape = GROUP), lwd = 3.75, na.rm = TRUE)+
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE, size=1.25)+
  ylab("Time to Equilibrium Loss (Sec)")+
  xlab("Time to Recover (Sec)")+
  scale_y_continuous(name = "Time to Equilibrium Loss (Sec)",
                     breaks = seq(50, 270, 100),
                     limits=c(50, 270))+
  scale_x_continuous(name = "Time to Recover (Sec)",
                     breaks = seq(50, 270, 100),
                     limits=c(50, 270))+
  annotate("text", x = 268, y = 268, label = "c", fontface = "bold", colour = "black", size = 10)+
  theme_classic()+
  theme(plot.title = element_blank(),
        axis.title.y = element_text(face = "bold", colour = "black", size = 14),
        axis.title.x = element_text(face = "bold", colour = "black", size = 14),
        axis.ticks.length = unit(0.25,"cm"),
        axis.ticks = element_line(size = 0.75, colour = "black"),
        axis.text.y = element_text(face="bold", colour="black", size=12),
        axis.line = element_line(size = 0.75),
        axis.text = element_text(size = 12.0, face = "bold", color = "black"),
        panel.grid.major.x = element_blank(),aspect.ratio =  ((1 + sqrt(5))/ 2),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        legend.background = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.2,0.2),
        legend.text = element_text(face = "bold", size = 12))+
  scale_color_manual(breaks = c("Fasted","Fed"), values = c("black","gray70"))
  ggsave("Passout_to_REC.png", dpi = 1500)

##############################################################################################################

#Making multi-panel plot
 
Figure_3 <- arrangeGrob(Panel_A,Panel_B,Panel_C,ncol=3) #generates Figure_3
ggsave(file="Figure_3.eps", Figure_3, width = 12, height = 9, dpi = 600) #saves Figure_3.eps
dev.off()
  
