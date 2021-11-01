#####
csv_dat <- "/Users/mariachristellaflorendo/Documents/SP/SP Analysis/R2_Math_G5.csv"

#Importing data
dat <- read.csv(csv_dat)
str(dat)
summary(dat)
head(dat)

#Recoding missing responses -1 to NA
dat_mis_NA <- dat[,5:63]
dat_mis_NA[dat_mis_NA==-1] <- NA

#Removing rows with all responses missing
all_na <- apply(dat_mis_NA,1,FUN = function(x){all(is.na(x))}) #identifies rows with all responses missing
dat_mis_NA <- dat_mis_NA[!all_na,] # removes rows with all NA
dim(dat_mis_NA)

#Recoding missing response as incorrect response, 0
dat_mis_0 <- dat[,5:63]
dat_mis_0 <- dat_mis_0[!all_na,] # removes rows with all NA
dat_mis_0[dat_mis_0==-1] <- 0
summary(dat_mis_0)


#Scoring
key <- c(4,1,3,4,4,3,4,1,3,4,2,1,2,3,4,4,3,3,2,1,1,2,4,1,4,4,3,2,1,3,3,2,1,1,2,4,3,3,2,
         3,3,3,3,1,4,2,3,3,3,3,3,1,2,2,3,3,3,1,4)
scored_mis_NA <- sapply(seq(1,length(key)), FUN = function(ii){ 1*(dat_mis_NA[,ii] == key[ii])})
scored_mis_0 <- sapply(seq(1,length(key)), FUN = function(ii){ 1*(dat_mis_0[,ii] == key[ii])})

colnames(scored_mis_NA) <-  colnames(dat_mis_NA)
scored_mis_NA <- as.data.frame(scored_mis_NA)

colnames(scored_mis_0) <-  colnames(dat_mis_0)
scored_mis_0 <- as.data.frame(scored_mis_0)

str(scored_mis_0)
str(scored_mis_NA)

head(scored_mis_0)
head(scored_mis_NA)


#Removing problematic items (Items 28 and 36)
dat_mis_NA <- dat_mis_NA[,-c(28,36)]
dat_mis_0 <- dat_mis_0[,-c(28,36)]
scored_mis_NA <- scored_mis_NA[,-c(28,36)]
scored_mis_0 <- scored_mis_0[,-c(28,36)]
dim(scored_mis_0)
dim(scored_mis_NA)

#Strategy 1
#Missing data as not administered item, NA
scored.strat_1 <- scored_mis_NA


#Strategy 2
#Missing data as incorrect response, 0
scored.strat_2 <- scored_mis_0

#Strategy 3
##Missing data are either ommitted (incorrect) or not administered (NA)
##Not administered items are items with missing responses that follow the last item with completed response

strat_3.1 <- apply(scored_mis_NA,1,FUN = function(ii){max(which(ii==0|ii==1))}) #determines column index for last completed response per row
strat_3.1_dat <- matrix(rep(strat_3.1,ncol(scored_mis_NA)),ncol = ncol(scored_mis_NA)) #converts strat_3.1 vector to matrix
head(strat_3.1_dat)
tail(strat_3.1_dat)

scored.strat_3 <- scored_mis_0 #Initialize strategy 3 data

##Loop for recoding 0 to NA for not administered items
for(y in 1:nrow(strat_3.1_dat)){
  for (x in 1:ncol(scored_mis_NA)) {
    if (x > strat_3.1_dat[y,x] & is.na(scored_mis_NA[y,x]))
      scored.strat_3[y,x] <- NA
    else 
      scored.strat_3[y,x] <- scored.strat_3[y,x]
  }
}

tail(scored.strat_3)
summary(scored.strat_3)
dim(scored.strat_3)

#Strategy 4
scored.strat_4_it <- scored.strat_3
scored.strat_4_st <- scored.strat_2



