# ANOVA code

options(contrasts = c("contr.sum", "contr,poly"))

leng1<-aov(TL~GROUP, data = Lengths)

drop1(leng1, ~., test = "F")