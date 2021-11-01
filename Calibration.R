#####
#Defining Q matrix
Qmatrix <- matrix(0, nrow = 57, ncol = 4)
dim.1 <- c(1,2,3,4,5,6,9,15,19,32,33,41,42,45,49,53,55,56,57)
dim.2 <- c(7,11,13,16,17,18,20,22,23,24,27,35,37,39,40,43,54)
dim.3 <- c(8,28,29,30,34,36,44,51,52)
dim.4 <- c(10,12,14,21,25,26,31,38,46,47,48,50)

Qmatrix[dim.1,1] <- Qmatrix[dim.2,2] <- Qmatrix[dim.3,3] <- Qmatrix[dim.4,4] <- 1

Qmatrix


#####
# Unidimensional Rasch calibration
mod.uni.R <- TAM::tam.mml(scored.strat_4_it)


# Unidimensional 2-PL calibration
mod.uni.2PL <- TAM::tam.mml.2pl( scored.strat_4_it, irtmodel = "2PL" )


# Unidimensional 3-PL calibration
mod.uni.3PL <- TAM::tam.mml.3pl(resp = , irtmodel)


# Consecutive Rasch
mod.cons.R.1 <- TAM::tam.mml(scored.strat_4_it[,dim.1])
mod.cons.R.2 <- TAM::tam.mml(scored.strat_4_it[,dim.2])
mod.cons.R.3 <- TAM::tam.mml(scored.strat_4_it[,dim.3])
mod.cons.R.4 <- TAM::tam.mml(scored.strat_4_it[,dim.4])


# Consecutive 2-PL
mod.cons.2PL.1 <- TAM::tam.mml.2pl( scored.strat_4_it[,dim.1], irtmodel = "2PL" )
mod.cons.2PL.2 <- TAM::tam.mml.2pl( scored.strat_4_it[,dim.2], irtmodel = "2PL" )
mod.cons.2PL.3 <- TAM::tam.mml.2pl( scored.strat_4_it[,dim.3], irtmodel = "2PL" )
mod.cons.2PL.4 <- TAM::tam.mml.2pl( scored.strat_4_it[,dim.4], irtmodel = "2PL" )


# Consecutive 3-PL
mod.cons.3PL.1
mod.cons.3PL.2
mod.cons.3PL.3
mod.cons.3PL.4


# Multidimensional Rasch calibration
mod.mult.R <- TAM::tam.mml(scored.strat_4_it, Q = Qmatrix)


# Multidimensional 2-PL calibration
mod.mult.2PL <- TAM::tam.mml.2pl(scored.strat_4_it, irtmodel = "2PL" , Q = Qmatrix)


# Multidimensional 3-PL calibration
mod.mult.3PL







