######################################################################
############## NORMALIZED POINT SAMPLING APPROACH ####################
######################################################################



######################################################################
########## GEOMETRIC MEDIAN PER HOUR APPROACH ##############
######################################################################

mcp95statsAll = subset(meltmcpallwiframe,type=="mcp95")
fit <- lmer(value ~ variable + (1|Island) + (1|Year), data= mcp95statsAll)
summary(fit)
ftable = drop1(fit, ~. ,test="Chisq")
ftable

mcp95stats = subset(meltmcpallwiframe,type=="mcp95" & (variable == "CocosWI" | variable=="NativesWI"))
fit <- lmer(value ~ variable + (1|Island) + (1|Year), data= mcp95stats)
summary(fit)
ftable = drop1(fit, ~. ,test="Chisq")
ftable

wilcox.test(subset(mcp95stats,variable=="NativesWI")$value, subset(mcp95stats,variable=="CocosWI")$value)
t.test(subset(mcp95stats,variable=="NativesWI")$value, subset(mcp95stats,variable=="CocosWI")$value)

