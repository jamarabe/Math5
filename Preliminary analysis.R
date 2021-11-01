#Preliminary analysis
##Effect of treatment of missing data on psychometric properties

##Strategy 1
mod.strat1 <- TAM::tam.mml(scored.strat_1)
param.strat1 <- matrix(c(seq(1,length(mod.strat1$xsi$xsi)),mod.strat1$xsi$xsi), ncol = 2)

abil.strat1 <- tam.wle(mod.strat1)
ctt.strat1 <- tam.ctt(scored.strat_1, wlescore = abil.strat1$theta)


##Strategy 2
mod.strat2 <- TAM::tam.mml(scored.strat_2)
param.strat2 <- matrix(c(seq(1,length(mod.strat2$xsi$xsi)),mod.strat2$xsi$xsi), ncol = 2)

abil.strat2 <- tam.wle(mod.strat2)

ctt.strat2 <- tam.ctt(scored.strat_2, wlescore = abil.strat2$theta)

summary(scored.strat_2)

##Strategy 3
mod.strat3 <- TAM::tam.mml(scored.strat_3)
param.strat3 <- matrix(c(seq(1,length(mod.strat3$xsi$xsi)),mod.strat3$xsi$xsi), ncol = 2)

abil.strat3 <- tam.wle(mod.strat3)

ctt.strat3 <- tam.ctt(scored.strat_3, wlescore = abil.strat3$theta)


##Strategy 4
###Phase 1 - Item parameter estimation
mod.strat4.1 <- TAM::tam.mml(scored.strat_4_it)
param.strat4 <- matrix(c(seq(1,length(mod.strat4.1$xsi$xsi)),mod.strat4.1$xsi$xsi), ncol = 2)

###Phase 2 - Student ability estimation (w/ fixed item parameters based on Phase 1)
mod.strat4.2 <- TAM::tam.mml(scored.strat_4_st, xsi.fixed = param.strat4)
abil.strat4 <- tam.wle(mod.strat4.2)

ctt.strat4 <- tam.ctt(scored.strat_4_st, wlescore = abil.strat4$theta)

write.csv(ctt.strat1,"ctt_strat1.csv")
write.csv(ctt.strat2,"ctt_strat2.csv")
write.csv(ctt.strat3,"ctt_strat3.csv")
write.csv(ctt.strat4,"ctt_strat4.csv")
write.csv(abil.strat1,"abil_strat1.csv")
write.csv(abil.strat2,"abil_strat2.csv")
write.csv(abil.strat3,"abil_strat3.csv")
write.csv(abil.strat4,"abil_strat4.csv")
write.csv(param.strat1, "param.strat1.csv")
write.csv(param.strat2, "param.strat2.csv")
write.csv(param.strat3, "param.strat3.csv")
write.csv(param.strat4, "param.strat4.csv")

write.csv(mod.strat1$xsi$se.xsi,"param.strat1.se.csv")
write.csv(mod.strat2$xsi$se.xsi,"param.strat2.se.csv")
write.csv(mod.strat3$xsi$se.xsi,"param.strat3.se.csv")
write.csv(mod.strat4$xsi$se.xsi,"param.strat4.se.csv")


##### Person fit
fit.prsn.strat1 <- tam.personfit(mod.strat1)
fit.prsn.strat2 <- tam.personfit(mod.strat2)
fit.prsn.strat3 <- tam.personfit(mod.strat3)
fit.prsn.strat4 <- tam.personfit(mod.strat4)


write.csv(fit.prsn.strat1, "fit.prsn.strat1.csv")
write.csv(fit.prsn.strat2, "fit.prsn.strat2.csv")
write.csv(fit.prsn.strat3, "fit.prsn.strat3.csv")
write.csv(fit.prsn.strat4, "fit.prsn.strat4.csv")




write.csv(mod.strat1, "mod_strat1")

summary(mod.strat1)
summary(mod.strat2)
summary(mod.strat3)
summary(mod.strat4)


summary(mod.uni.R)

rm(fit.prsn.strat1)
rm(fit.prsn.strat2)
rm(fit.prsn.strat3)
rm(fit.prsn.strat4)
rm(param.strat1)
rm(param.strat2)
rm(param.strat3)
rm(param.strat4)

rm(abil.strat1)
rm(abil.strat2)
rm(abil.strat3)
rm(abil.strat4)

rm(ctt.strat1)
rm(ctt.strat2)
rm(ctt.strat3)
rm(ctt.strat4)

rm(mod.strat1)
rm(mod.strat2)
rm(mod.strat3)
rm(mod.strat4)


