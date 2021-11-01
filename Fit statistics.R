#####Model fit statistics
gof.uni.R <- tam.modelfit(mod.uni.R.1)
gof.uni.2PL <- tam.modelfit(mod.uni.2PL.1)
gof.uni.3PL <- tam.modelfit(mod.uni.3PL.1)
gof.cons.R.1 <- tam.modelfit(mod.cons.R.1.1)
gof.cons.R.2 <- tam.modelfit(mod.cons.R.2.1)
gof.cons.R.3 <- tam.modelfit(mod.cons.R.3.1)
gof.cons.R.4 <- tam.modelfit(mod.cons.R.4.1)
gof.cons.2PL.1 <- tam.modelfit(mod.cons.2PL.1.1)
gof.cons.2PL.2 <- tam.modelfit(mod.cons.2PL.2.1)
gof.cons.2PL.3 <- tam.modelfit(mod.cons.2PL.3.1)
gof.cons.2PL.4 <- tam.modelfit(mod.cons.2PL.4.1)
gof.cons.3PL.1 <- tam.modelfit(mod.cons.3PL.1.1)
gof.cons.3PL.2 <- tam.modelfit(mod.cons.3PL.2.1)
gof.cons.3PL.3 <- tam.modelfit(mod.cons.3PL.3.1)
gof.cons.3PL.4 <- tam.modelfit(mod.cons.3PL.4.1)
gof.mult.R <- tam.modelfit(mod.mult.R.1)
gof.mult.2PL <- tam.modelfit(mod.mult.2PL.1)

tam.modelfit

gof.uni.R$modelfit.test
gof.uni.R$statlist
gof.uni.R$fitstat
gof.uni.R$chi2.stat
gof.uni.R$stat.MADaQ3
gof.uni.2PL$stat.MADaQ3
gof.uni.3PL$stat.MADaQ3
gof.mult.R$stat.MADaQ3
gof.mult.2PL$stat.MADaQ3

gof.uni.R$Q3_summary
gof.uni.2PL$Q3_summary
gof.uni.3PL$Q3_summary
gof.cons.R.1$Q3_summary
gof.cons.R.2$Q3_summary
gof.cons.R.3$Q3_summary
gof.cons.R.4$Q3_summary

gof.cons.2PL.1$Q3_summary
gof.cons.2PL.2$Q3_summary
gof.cons.2PL.3$Q3_summary
gof.cons.2PL.4$Q3_summary


gof.cons.3PL.1$Q3_summary
gof.cons.3PL.2$Q3_summary
gof.cons.3PL.3$Q3_summary
gof.cons.3PL.4$Q3_summary


mod.mult.3PL$item[dim.1,1]
mod.mult.3PL$item[dim.2,1]
mod.mult.3PL$item[dim.3,1]
mod.mult.3PL$item[dim.4,1]


paramB.mult.2PL.[dim.1,1]
paramB.mult.2PL.[dim.2,1]
paramB.mult.2PL.[dim.3,1]
paramB.mult.2PL.[dim.4,1]


gof.mult.R$Q3_summary
gof.mult.2PL$Q3_summary


gof.uni.2PL$modelfit.test
gof.uni.2PL$fitstat

summary(mod.mult.R)
summary(mod.mult.2PL)


IRT.compareModels(mod.mult.R,mod.mult.2PL)



gof.mult.2PL$fitstat
gof.mult.R$fitstat
gof.uni.3PL$fitstat
gof.uni.2PL$fitstat
gof.uni.R$fitstat

gof.cons.2PL.1$fitstat
gof.cons.2PL.2$fitstat
gof.cons.2PL.3$fitstat
gof.cons.2PL.4$fitstat

gof.cons.3PL.1$fitstat
gof.cons.3PL.2$fitstat
gof.cons.3PL.3$fitstat
gof.cons.3PL.4$fitstat

gof.cons.R.1$fitstat
gof.cons.R.2$fitstat
gof.cons.R.3$fitstat
gof.cons.R.4$fitstat

gof.uni.R$Q3_summary
gof.uni.R$stat.MADaQ3
gof.uni.R$modelfit.test


summary(mod.uni.R.1)
summary(mod.uni.2PL.1)


anova(mod.uni.R,mod.uni.2PL)
anova(mod.uni.R,mod.uni.3PL)
anova(mod.uni.R,mod.mult.R)
anova(mod.uni.R,mod.mult.2PL)
anova(mod.uni.2PL,mod.uni.3PL)
anova(mod.uni.2PL,mod.mult.R)
anova(mod.uni.2PL,mod.mult.2PL)
anova(mod.uni.3PL,mod.mult.R)
anova(mod.uni.3PL,mod.mult.2PL)
anova(mod.mult.R,mod.mult.2PL)

rm(dat)
