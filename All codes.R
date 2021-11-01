#####
csv_dat <- "/Users/mariachristellaflorendo/Documents/SP/SP Analysis/R2_Math_G5.csv"

#Data import and cleaning
dat <- read.csv(csv_dat)
str(dat)
summary(dat)
head(dat)


#Strategy 1
#Missing data as skipped item (not administered),NA
dat_mis_NA <- dat[,5:63]
dim(dat_mis_NA)
dat_mis_NA[dat_mis_NA==-1] <- NA

all_na <- apply(dat_mis_NA,1,FUN = function(x){all(is.na(x))})
summary(all_na)

dat_mis_NA <- dat_mis_NA[!all_na,] # removes rows with all NA
tail(dat_mis_NA)
dim(dat_mis_NA)

na.some <- apply(dat_mis_NA,1,FUN = function(x){is.na(x)})
class(na.some)
na.indicator <- t(na.some)

na.ind <- is.na(dat_mis_NA)
head(na.ind)
omit.ind <- t(matrix(1:59,nrow = 59,ncol = 1569))
head(omit.ind)
head(strat_3.1)
try <- omit.ind<strat_3.1 # determines possible position for omitted items

omit.ind2 <- try*na.indicator
dat_mis_NA[omit.ind2]

str(omit.ind2[which(is.na(dat_mis_NA))])

tail(dat_mis_NA, 10)
tail(dat_mis_NA[1-omit.ind2])

na.ind2 <- 1-omit.ind2
summary(na.ind2==0)
na.ind3

dat.strategy3 <- apply(scored_mis_0, 1, FUN = function(x){ifelse(scored_mis_0[is.na(x) & na.ind2[x]==0] <- NA,scored_mis_0)})



tail(which(is.na(dat_mis_NA)))

?subset

dat_mis_NA[is.na(dat_mis_NA) & which(dat_mis_NA,)]

which(1:10 > 3, arr.ind = TRUE)



#Strategy 2
#Missing data as incorrect response, 0
dat_mis_0 <- dat[,5:63]

dat_mis_0 <- dat_mis_0[!all_na,] # removes rows with all NA

dat_mis_0[dat_mis_0==-1] <- 0

summary(dat_mis_0)



#Strategy 3
#Missing data are either ommitted (incorrect) or not administered (NA)
#Not administered items are items with missing responses that follows the final item with completed response
dat_strat3 <- dat[,5:63]

dat_strat3 <- dat_strat3[!all_na,] # removes rows with all NA

dat_strat3 <- is.na(dat_strat3[,x<comp_id])

dat_strat3[,x<comp_id]

score_mis_strat3 <- sapply(seq(1:ncol(dat_strat3), FUN = function(x){}))

?'if'

#Determines last completed item
max(which(scored_mis_NA[1,]==0|scored_mis_NA[1,]==1))
max(which(scored_mis_NA[2,]==0|scored_mis_NA[2,]==1))
max(which(scored_mis_NA[5,]==0|scored_mis_NA[5,]==1))

strat_3

comp_id <- function(x){max(which(x==0|x==1))}
strat_3 <- apply(scored_mis_NA,1,FUN = comp_id)
tail(strat_3)

###USE THIS FOR FINAL FOR LOOP!!!!
strat_3.1 <- apply(scored_mis_NA,1,FUN = function(ii){max(which(ii==0|ii==1))}) #determines column index for last completed response per row
strat_3.1
tail(strat_3.1)
cbind(strat_3.1)

strat_3.1_dat <- matrix(rep(strat_3.1,59),ncol = 59) #converts strat_3.1 vector to matrix
head(strat_3.1_dat)
tail(strat_3.1_dat)

#####ENDS HERE !!!!###

tail(strat3.1)

tail(scored_mis_NA)

tail(dat_mis_NA)

which(dat_mis_NA[all(is.na(dat_mis_NA))])

strat.3 <- sapply(scored_mis_NA,FUN = function(x){if(is.na(x) & <max(which(x==0|x==1))) x<-0})


scored_strat_3 <- which(is.na(scored_mis_NA))
scored_strat_3
                     
strat_3.1 <- rep(strat_3.1,59)
strat_3.1 <- matrix(strat_3.1,ncol = 59)
strat_3.1

strategy_3 <- ifelse(stra)

#Scored data
key <- c(4,1,3,4,4,3,4,1,3,4,2,1,2,3,4,4,3,3,2,1,1,2,4,1,4,4,3,2,1,3,3,2,1,1,2,4,3,3,2,
         3,3,3,3,1,4,2,3,3,3,3,3,1,2,2,3,3,3,1,4)
scored_mis_NA <- sapply(seq(1,length(key)), FUN = function(ii){ 1*(dat_mis_NA[,ii] == key[ii])})
scored_mis_0 <- sapply(seq(1,length(key)), FUN = function(ii){ 1*(dat_mis_0[,ii] == key[ii])})

colnames(scored_mis_NA) <-  colnames(dat_mis_NA)
scored_mis_NA <- as.data.frame(scored_mis_NA)

colnames(scored_mis_0) <-  colnames(dat_mis_0)
scored_mis_0 <- as.data.frame(scored_mis_0)

str(scored_mis_0)

head(scored_mis_NA)
head(scored_mis_0)

ite_rest <- sapply(seq(1,ncol(scored_mis_0)), FUN = function(iii)
  {(cor(scored_mis_0[,iii],y = rowSums(scored_mis_0[,-iii])))})
ite_rest

ite_rest.test <- sapply(seq(1,ncol(scored_mis_0)), FUN = function(iii)
{(cor.test(scored_mis_0[,iii],y = rowSums(scored_mis_0[,-iii])))})

ite_rest.test


ite_rest_NA <- sapply(seq(1,ncol(scored_mis_NA)), FUN = function(iii)
  {(cor.test(scored_mis_NA[,iii],y = rowSums(scored_mis_NA[-iii],na.rm = TRUE),use = "complete.obs"))})
ite_rest_NA[,56]

attributes(ite_rest.test)

ite_rest.test
(t(ite_rest.test)[,1:3])

ite_rest_0 <- cbind(ite_rest,t(ite_rest.test))
ite_rest_0

rowSums(scored_mis_NA,na.rm = TRUE)




#####
# Preliminary analysis
## Proportion correct per item - facility
facility1 <- colMeans(na.omit(scored_mis_NA))
facility2 <- colMeans(scored_mis_0)
facility <- cbind(facility1,facility2)
colnames(facility) <- c("Facility (Missing as NA)", "Facility (Missing as Incorrect)")
facility

## Point biserial correlation
total_score <- rowSums(scored_mis_0)

cor(scored_mis_NA[,1],total_score,use = "complete.obs")
cor(scored_mis_0[,1],total_score,use = "complete.obs")

sapply(scored_mis_NA,cor,y=total_score,use="complete.obs")
sapply(scored_mis_0,cor,y=total_score,use="complete.obs")

scored_mis_0 <- as.data.frame(scored_mis_0)

str(scored_mis_0)
class(scored_mis_0)

### Item-rest correlation
cor(scored_mis_NA[,1],total_score-scored_mis_NA[,1],use = "complete.obs")
cor(scored_mis_0[,1],total_score-scored_mis_0[,1], use = "complete.obs")

sapply(scored_mis_NA,cor,y=,use="complete.obs")
sapply(scored_mis_0,cor,y=total_score,use="complete.obs")


# count of NA per item
na_per_item <- colSums(is.na(scored_mis_NA))
na_per_item
prop.na_per_item <- colMeans(is.na(scored_mis_NA))


plot(1:length(na_per_item),na_per_item, xlab = "Item Number", ylab = "Number of Missing Responses")
plot(1:length(prop.na_per_item),prop.na_per_item, xlab = "Item Number", ylab = "Proportion of Missing Responses") + lines(x = 1:length(prop.na_per_item),y=rep(.1,length(prop.na_per_item)))
  + lines(x = 1:length(prop.na_per_item),y=rep(.05,length(prop.na_per_item)))



plot(1:length(na_per_item),na_per_item, xlab = "Item Number", ylab = "Number of Missing Responses") + lines(x = 1:length(na_per_item),y=rep(150,length(na_per_item)))

### Proportion correct per student
prop_correct_NA <- 100*rowMeans(scored_mis_NA, na.rm = TRUE)
prop_correct_0 <- 100*rowMeans(scored_mis_0)
high_score_NA <- apply(scored_mis_NA, 1, FUN = function(x){length(x)-sum(is.na(x))})
high_score_0 <- rowSums(scored_mis_0)

rm(na_count)

prop_correct <- cbind(prop_correct_NA,prop_correct_0)
?which


Qmatrix <- 


#####
# Unidimensional Rasch calibration
## Estimation
mod.uni.R <- TAM::tam.mml( datsample, Q=Q )


## Reliabilities


## Item parameters



# Unidimensional 2-PL calibration
## Estimation
mod.uni.2PL <- TAM::tam.mml( datsample, Q=Q )


## Reliabilities


## Item parameters



# Unidimensional 3-PL calibration
## Estimation
mod.uni.3PL <- TAM::tam.mml( datsample, Q=Q )


## Reliabilities


## Item parameters





# Consecutive Rasch
##calibration
mod.cons.R.1
mod.cons.R.2
mod.cons.R.3
mod.cons.R.4


# Consecutive 2-PL
##calibration
mod.cons.2PL.1
mod.cons.2PL.2
mod.cons.2PL.3
mod.cons.2PL.4


# Consecutive 3-PL
##calibration
mod.cons.3PL.1
mod.cons.3PL.2
mod.cons.3PL.3
mod.cons.3PL.4


# Multidimensional Rasch calibration
mod.mult.R



# Multidimensional 2-PL calibration
mod.mult.2PL




# Multidimensional 3-PL calibration
mod.mult.3PL





######

### Model comparison
IRT.compareModels() # will yield loglike, deviance, Npars, AIC, BIC, and LR test


################### Item fit statistics

########## Item Fit statistics
######

#############################################################################
# EXAMPLE 1: RMSD item fit statistic data.read
#############################################################################

library(sirt)
data(data.read,package="sirt")
dat <- data.read

#*** fit 1PL model
mod1 <- TAM::tam.mml( dat )
summary(mod1)

#*** fit 2PL model
mod2 <- TAM::tam.mml.2pl( dat )
summary(mod2)

#*** assess RMSEA item fit
fmod1 <- IRT.itemfit(mod1)
fmod2 <- IRT.itemfit(mod2)

# summary of fit statistics
summary( fmod1 )
summary( fmod2 )

#############################################################################
# EXAMPLE 2: Simulated 2PL data and fit of 1PL model
#############################################################################


set.seed(987)
N <- 1000    # 1000 persons
I <- 10      # 10 items
# define item difficulties and item slopes
b <- seq(-2,2,len=I)
a <- rep(1,I)
a[c(3,8)] <- c( 1.7, .4 )
# simulate 2PL data
dat <- sirt::sim.raschtype( theta=rnorm(N), b=b, fixed.a=a)

# fit 1PL model
mod <- TAM::tam.mml( dat )

# RMSEA item fit
fmod <- IRT.itemfit(mod)
round(fmod, 3 )
summary(fmod)

pfit <- tam.personfit(mod)


summary(mod4_1)
summary(mod4_3)
anova(mod4_1,mod4_3)

IRT.compareModels(mod4_1,mod4_3)

IRT.itemfit
?IRT.p

