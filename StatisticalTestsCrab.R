######################################################################
############## NORMALIZED POINT SAMPLING APPROACH ####################
######################################################################

fit <- lmer(value ~ variable + (1|Island), data= meltHourlyTrackDataDF)
summary(fit)
ftable = drop1(fit, ~. ,test="Chisq")
ftable

kud95stats = subset(meltHourlyTrackDataDF,(variable == "CocosWI" | variable=="NativesWI"))
fit <- lmer(value ~ variable + (1|Island), data= kud95stats)
summary(fit)
ftable = drop1(fit, ~. ,test="Chisq")
ftable

wilcox.test(subset(meltHourlyTrackDataDF,variable=="NativesWI")$value, subset(meltHourlyTrackDataDF,variable=="CocosWI")$value)

t.test(subset(meltHourlyTrackDataDF,variable=="NativesWI")$value, subset(meltHourlyTrackDataDF,variable=="CocosWI")$value)


######################################################################
########## GEOMETRIC MEDIAN PER HOUR APPROACH ##############
######################################################################

kud95statsAll = subset(meltkudallwiframe,type=="kud95")
kud95stats = subset(meltkudallwiframe,type=="kud95" & (variable == "CocosWI" | variable=="NativesWI"))

fit <- lmer(value ~ variable + (1|Island) + (1|Year), data= kud95statsAll)
summary(fit)
ftable = drop1(fit, ~. ,test="Chisq")
ftable


fit1 <- lmer(value ~ variable + (1|Year), data= kud95stats, REML=FALSE)
summary(fit1)
ftable = drop1(fit1, ~. ,test="Chisq")
ftable
fit2 <- lmer(value ~ variable + (1|Island) + (1|Year), data= kud95stats, REML=FALSE)
summary(fit2)
ftable = drop1(fit2, ~. ,test="Chisq")
ftable
anova(fit1,fit2)


wilcox.test(subset(kud95stats,variable=="NativesWI")$value, subset(kud95stats,variable=="CocosWI")$value)
t.test(subset(kud95stats,variable=="NativesWI")$value, subset(kud95stats,variable=="CocosWI")$value)

var(subset(kud95stats,variable=="NativesWI")$value)
var(subset(kud95stats,variable=="CocosWI")$value)
var.test(subset(kud95stats,variable=="NativesWI")$value, subset(kud95stats,variable=="CocosWI")$value)


