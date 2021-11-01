##Plotting and correlations

# Mult R vs Mult 2PL
# Dim 1
plot(abil.mult.R$theta.Dim01,abil.mult.2PL$theta.Dim01, xlab = "Ability Estimates using Multidimensional Rasch Model", ylab = "Ability Estimates using Multidimensional 2PL Model")
cor(abil.mult.R$theta.Dim01,abil.mult.2PL$theta.Dim01)
cor.test(abil.mult.R$theta.Dim01,abil.mult.2PL$theta.Dim01)

# Dim 2
plot(abil.mult.R$theta.Dim02,abil.mult.2PL$theta.Dim02, xlab = "Ability Estimates using Multidimensional Rasch Model", ylab = "Ability Estimates using Multidimensional 2PL Model")
cor(abil.mult.R$theta.Dim02,abil.mult.2PL$theta.Dim02)
cor.test(abil.mult.R$theta.Dim02,abil.mult.2PL$theta.Dim02)

# Dim 3
plot(abil.mult.R$theta.Dim03,abil.mult.2PL$theta.Dim03, xlab = "Ability Estimates using Multidimensional Rasch Model", ylab = "Ability Estimates using Multidimensional 2PL Model")
cor(abil.mult.R$theta.Dim03,abil.mult.2PL$theta.Dim03)
cor.test(abil.mult.R$theta.Dim03,abil.mult.2PL$theta.Dim03)

# Dim 4
plot(abil.mult.R$theta.Dim04,abil.mult.2PL$theta.Dim04, xlab = "Ability Estimates using Multidimensional Rasch Model", ylab = "Ability Estimates using Multidimensional 2PL Model")
cor(abil.mult.R$theta.Dim04,abil.mult.2PL$theta.Dim04)
cor.test(abil.mult.R$theta.Dim04,abil.mult.2PL$theta.Dim04)

dim1.reg.multR_2PL <- lm(abil.mult.R$theta.Dim01~abil.mult.2PL$theta.Dim01)
dim1.reg.multR_2PL$coefficients
summary(dim1.reg.multR_2PL)


# Uni Rasch vs 2PL vs 3PL
# Uni Rasch vs 2PL
plot(abil.uni.R$theta,abil.uni.2PL$theta, xlab = "Ability Estimates using Unidimensional Rasch Model", ylab = "Ability Estimates using Unidimensional 2PL Model")
cor(abil.uni.R$theta,abil.uni.2PL$theta)
cor.test(abil.uni.R$theta,abil.mult.2PL$theta)

# Uni Rasch vs 3PL
plot(abil.uni.R$theta,abil.uni.3PL$theta, xlab = "Ability Estimates using Unidimensional Rasch Model", ylab = "Ability Estimates using Unidimensional 3PL Model")
cor(abil.uni.R$theta,abil.uni.3PL$theta)
cor.test(abil.uni.R$theta,abil.mult.3PL$theta)

# Uni 2PL vs 3PL
plot(abil.uni.2PL$theta,abil.uni.3PL$theta, xlab = "Ability Estimates using Unidimensional 2PL Model", ylab = "Ability Estimates using Unidimensional 3PL Model")
cor(abil.uni.2PL$theta,abil.uni.3PL$theta)
cor.test(abil.uni.2PL$theta,abil.mult.3PL$theta)



#Plotting dimension 1
dim1.abil <- read.csv("Dimension1.csv", header = TRUE)

pairs(~theta.cons.R+theta.cons.2PL+theta.cons.3PL+theta.mult.R+theta.mult.2PL,data=dim1.abil,
      main="Scatterplot Matrix of Student Ability Estimates for Dimension 1")

#Plotting dimension 2
dim2.abil <- read.csv("Dimension2.csv", header = TRUE)

pairs(~theta.cons.R+theta.cons.2PL+theta.cons.3PL+theta.mult.R+theta.mult.2PL,data=dim2.abil,
      main="Scatterplot Matrix of Student Ability Estimates for Dimension 2")


#Plotting dimension 3
dim3.abil <- read.csv("Dimension3.csv", header = TRUE)

pairs(~theta.cons.R+theta.cons.2PL+theta.cons.3PL+theta.mult.R+theta.mult.2PL,data=dim3.abil,
      main="Scatterplot Matrix of Student Ability Estimates for Dimension 3")


#Plotting dimension 4
dim4.abil <- read.csv("Dimension4.csv", header = TRUE)

pairs(~theta.cons.R+theta.cons.2PL+theta.cons.3PL+theta.mult.R+theta.mult.2PL,data=dim4.abil,
      main="Scatterplot Matrix of Student Ability Estimates for Dimension 4")



