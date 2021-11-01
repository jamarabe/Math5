


## Reliabilities


## Item parameters
param.uni.R <- matrix(c(seq(1,length(mod.uni.R$xsi$xsi)),mod.uni.R$xsi$xsi), ncol = 2)
param.uni.2PL <- matrix(c(seq(1,length(mod.uni.2PL$xsi$xsi)),mod.uni.2PL$xsi$xsi), ncol = 2)
param.cons.R.1 <- matrix(c(seq(1,length(mod.cons.R.1$xsi$xsi)),mod.cons.R.1$xsi$xsi), ncol = 2)
param.cons.R.2 <- matrix(c(seq(1,length(mod.cons.R.2$xsi$xsi)),mod.cons.R.2$xsi$xsi), ncol = 2)
param.cons.R.3 <- matrix(c(seq(1,length(mod.cons.R.3$xsi$xsi)),mod.cons.R.3$xsi$xsi), ncol = 2)
param.cons.R.4 <- matrix(c(seq(1,length(mod.cons.R.4$xsi$xsi)),mod.cons.R.4$xsi$xsi), ncol = 2)

param.cons.2PL.1 <- matrix(c(seq(1,length(mod.cons.2PL.1$xsi$xsi)),mod.cons.2PL.1$xsi$xsi), ncol = 2)
param.cons.2PL.2 <- matrix(c(seq(1,length(mod.cons.2PL.2$xsi$xsi)),mod.cons.2PL.2$xsi$xsi), ncol = 2)
param.cons.2PL.3 <- matrix(c(seq(1,length(mod.cons.2PL.3$xsi$xsi)),mod.cons.2PL.3$xsi$xsi), ncol = 2)
param.cons.2PL.4 <- matrix(c(seq(1,length(mod.cons.2PL.4$xsi$xsi)),mod.cons.2PL.4$xsi$xsi), ncol = 2)

paramB.cons.2PL.1 <- mod.cons.2PL.1$B
paramB.cons.2PL.2 <- mod.cons.2PL.2$B
paramB.cons.2PL.3 <- mod.cons.2PL.3$B
paramB.cons.2PL.4 <- mod.cons.2PL.4$B


param.mult.R <- matrix(c(seq(1,length(mod.mult.R$xsi$xsi)),mod.mult.R$xsi$xsi), ncol = 2)

##Item Fit
fit.it.uni.R <- tam.fit(mod.uni.R)


## Student abilities - Uni R
mod.uni.R.1 <- tam.mml(scored_mis_0, xsi.fixed = param.uni.R)
abil.uni.R <- tam.wle(mod.uni.R.1)
write.csv(abil.uni.R,"abil.uni.R.csv")


write.csv(fit.it.uni.R$itemfit, "fit.it.uni.R.csv")
write.csv(abil.uni.R, "abil.uni.R.csv")


# Student abilities - Uni-2PL
paramB.uni.2PL <- mod.uni.2PL$B #Discrimination parameters

mod.uni.2PL.1 <- tam.mml.2pl(scored_mis_0, xsi.fixed = param.uni.2PL, B=paramB.cons.2PL) #Model with discrimination and difficulty fixed
abil.uni.2PL <- tam.wle(mod.uni.2PL.1)

fit.it.uni.2PL <- tam.fit(mod.uni.2PL)
write.csv(fit.it.uni.2PL$itemfit, "fit.it.uni.2PL.csv")
write.csv(abil.uni.2PL, "abil.uni.2PL.csv")


#Student abilities - Cons R
mod.cons.R.1.1 <- TAM::tam.mml( scored.strat_4_st[,dim.1], xsi.fixed = param.cons.R.1)
mod.cons.R.2.1 <- TAM::tam.mml( scored.strat_4_st[,dim.2], xsi.fixed = param.cons.R.2)
mod.cons.R.3.1 <- TAM::tam.mml( scored.strat_4_st[,dim.3], xsi.fixed = param.cons.R.3)
mod.cons.R.4.1 <- TAM::tam.mml( scored.strat_4_st[,dim.4], xsi.fixed = param.cons.R.4)

abil.cons.R.1 <- tam.wle(mod.cons.R.1.1)
abil.cons.R.2 <- tam.wle(mod.cons.R.2.1)
abil.cons.R.3 <- tam.wle(mod.cons.R.3.1)
abil.cons.R.4 <- tam.wle(mod.cons.R.4.1)

write.csv(abil.cons.R.1,"abil.cons.R.1.csv")
write.csv(abil.cons.R.2,"abil.cons.R.2.csv")
write.csv(abil.cons.R.3,"abil.cons.R.3.csv")
write.csv(abil.cons.R.4,"abil.cons.R.4.csv")


#Student abilities - Cons 2PL
mod.cons.2PL.1.1 <- TAM::tam.mml.2pl( scored.strat_4_st[,dim.1], irtmodel = "2PL", xsi.fixed = param.cons.2PL.1, B=paramB.cons.2PL.1 )
mod.cons.2PL.2.1 <- TAM::tam.mml.2pl( scored.strat_4_st[,dim.2], irtmodel = "2PL", xsi.fixed = param.cons.2PL.2, B=paramB.cons.2PL.2 )
mod.cons.2PL.3.1 <- TAM::tam.mml.2pl( scored.strat_4_st[,dim.3], irtmodel = "2PL", xsi.fixed = param.cons.2PL.3, B=paramB.cons.2PL.3 )
mod.cons.2PL.4.1 <- TAM::tam.mml.2pl( scored.strat_4_st[,dim.4], irtmodel = "2PL", xsi.fixed = param.cons.2PL.4, B=paramB.cons.2PL.4 )

abil.cons.2PL.1 <- tam.wle(mod.cons.2PL.1.1)
abil.cons.2PL.2 <- tam.wle(mod.cons.2PL.2.1)
abil.cons.2PL.3 <- tam.wle(mod.cons.2PL.3.1)
abil.cons.2PL.4 <- tam.wle(mod.cons.2PL.4.1)

write.csv(abil.cons.2PL.1,"abil.cons.2PL.1.csv")
write.csv(abil.cons.2PL.2,"abil.cons.2PL.2.csv")
write.csv(abil.cons.2PL.3,"abil.cons.2PL.3.csv")
write.csv(abil.cons.2PL.4,"abil.cons.2PL.4.csv")



## Student abilities - multidimensional models
# Mult-R
mod.mult.R.1 <- tam.mml(scored_mis_0, xsi.fixed = param.mult.R)
abil.mult.R <- tam.wle(mod.mult.R.1)
write.csv(abil.mult.R, "abil.mult.R.csv")

fit.it.mult.R <- tam.fit(mod.mult.R)


# Mult-2PL
param.mult.2PL <- matrix(c(seq(1,length(mod.mult.2PL$xsi$xsi)),mod.mult.2PL$xsi$xsi), ncol = 2)
paramB.mult.2PL <- mod.mult.2PL$B
mod.mult.2PL.1 <- TAM::tam.mml.2pl( scored.strat_4_st, irtmodel = "2PL", xsi.fixed = param.mult.2PL, B=paramB.mult.2PL,
                                    control=list(nodes=seq(-6,6,len=21), snodes=2000, QMC=TRUE,
                                                 convD=.001,conv=.0001, convM=.0001, Msteps=4, 
                                                 maxiter=1000, max.increment=1, 
                                                 min.variance=.001, progress=TRUE, ridge=0, 
                                                 seed=NULL, xsi.start0=0, increment.factor=1, 
                                                 fac.oldxsi=0, acceleration="none", dev_crit="absolute", trim_increment="half"  ))


abil.mult.2PL <- tam.wle(mod.mult.2PL.1) #Resulted with the ff. warning message:In log((PersonScores + 0.5)/(PersonMax - PersonScores + 1)) : NaNs produced
write.csv(abil.mult.2PL,"abil.mult.2PL.csv")

