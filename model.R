rm(list=ls())

#####################LIBRARIES######################
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(scales)

library('rjags')
library(coda)
library(plotMCMC)

library(sqldf)
library(splines)
library(LaplacesDemon)

setwd("C:/Users/vanen/Dropbox/TESI/R/MODEL")
load("myFunctions.Rdata")
load("myFunctionsT.Rdata")

#####################DATASETS#######################
library(deuce)

data(mcp_points)

########### FIGURE ###########

men <- mcp_points %>%
  dplyr::mutate(
    year = as.numeric(substr(match_id, 1, 4)),
    ATP = ifelse(grepl("[0-9]-M-", match_id), "ATP", "WTA"),
    rallyCount = as.numeric(ifelse(rallyCount == 0, 1, rallyCount))
  ) %>%
  filter(year >= 2012, !is.na(rallyCount), Gender != "W")

attach(men)

men <- data.frame(match_id, Serving, rallyLen, isSvrWinner)
head(men)

newdata = data.frame(sqldf("SELECT DISTINCT match_id, Serving
                            FROM men
                            ORDER BY match_id, Serving"))

pippo = data.frame(sqldf("SELECT DISTINCT match_id, Serving as Receving
                          FROM men 
                         ORDER BY match_id, Serving DESC"))

attach(pippo)
attach(newdata)

prova = data.frame(newdata$match_id, newdata$Serving, Receving)
colnames(prova)<- c("match", "Serve", "Receving")

dmen = left_join(prova, men, by = c("match"= "match_id", "Serve"= "Serving") )

colnames(dmen)<-c("match_id", "Serving", "Receving", "RallyLen", "isSvrWinner")

tabu = tabulate(dmen$RallyLen)

len = length(tabu)

out  = vector(length = len)
out1 = vector(length = len)

for(i in 1:len){
  
  #create a new subset for each value of rally 
  b = subset(dmen, (RallyLen == i-1))
  
  #how much game the ServicePlayer win
  tt = sum(b$isSvrWinner)
  
  out1[i] = tt
  out[i]  = tt/length(b$isSvrWinner)
}

col = vector(length = len)
for (i in 1:len) {
  if(!(i%%2)){
    col[i]="odd"
  }
  else
    col[i]="even"
}

col
out30 = out[1:31]
colors= col[1:31]

#percentual<- NULL
#for (intel in 0:30) {
#  percentual<- c(percentual, count(dmen[which(dmen$RallyLen==i & dmen$isSvrWinner==1),])/dim(dmen)[1])
#}
#unlist(percentual)

percentual <- tabu[1:31]


rally30 = data.frame(out30, colors, percentual)

###PLOTS###

require(ggplot2)

# par(mfcol = c(1,2)) non va con ggplot2

ggplot(rally30, aes(x = 0:30, y = out30), color = colors)+
  geom_point(aes(size=unlist(percentual), color = colors))+
  labs(title = "probability of win given the rally length",
       x = "Rally Length", y = "P[win|x]")

####################################################################################################
rm(list=ls())
#########SPLINE MODEL ##############

data("gs_point_by_point")
#head(gs_point_by_point)

men <- gs_point_by_point %>%
  filter(Tour == "atp", year >= 2012, RallyCount <= 30) %>%
  dplyr::mutate(
    server      = ifelse(PointServer == 1, player1, player2),
    receiver    = ifelse(PointServer == 1, player2, player1),
    isSvrWinner = ifelse(PointServer == PointWinner, 1, 0)
  )%>%
  select(match_id, court = slam, server, receiver, isSvrWinner, rallyLen = RallyCount)

sum(is.na(men$rallyLen))

men <- men %>%
  filter(!is.na(rallyLen))

men$court

###AGGREGATION OF DATAPOINTS

men <- men %>% dplyr::mutate(
         rallyLen  = ifelse(rallyLen %% 2 != 0, rallyLen - 1, rallyLen),
         court = recode(court, "wimbledon" = "grass", "frenchopen" = "clay", 
                "usopen" = "hard", "ausopen" = "hard"))
men$rallyLen <- men$rallyLen/2 +1

#CONTROL
count(men[which(men$rallyLen>30),])
head(men)
str(men$server)

men$court
men$court <- as.factor(men$court)

#WE INCLUDE PLAYERS WITH THREE MATCH OR MORE IN THE DATA

serving <- sqldf("SELECT server
                 FROM men
                 GROUP BY server
                 HAVING count(distinct match_id) >= 3")

dmen <- men %>%
  group_by(server) %>%
  dplyr::mutate(
    matches = n_distinct(match_id)
  ) %>%
  filter(matches > 2)

dmen <- dmen[which(dmen$receiver %in% serving$server),]

############################################# SORTING THE DATASET
#s = sort(dmen$rallyLen) 
dmen <- dmen %>% arrange(dmen$rallyLen)

#############################################

#CONVERT SERVE AND RECEIVE IN INT 

dmen$s <- (1:n_distinct(dmen$server))[factor(dmen$server)]
dmen$r <- (1:n_distinct(dmen$receiver))[factor(dmen$receiver)]

dmen$server[which(dmen$s==108)]

# CREATE TRAINING AND TEST SET
x <- seq(1:max(dmen$s))
set.seed(11)
x <- sample(x) 

dtrain<- 90
train <- x[1:dtrain]

test <- x[(dtrain+1):max(dmen$s)]

# Train data
trainData <- dmen[which(dmen$s %in% train),]

###############################################
############### CREATE THE SPLINES #####################

se<-seq(1,16,length=100)
#Bpred <- bs(se, df = 8, intercept = T)
Bpred<- bs(se, df = 4, knots = c(2,3,4,7,11), intercept = T)
dim(Bpred)

a = c(attr(Bpred, "knots"))

matplot(se,Bpred,type="l",xlab="rallyLen",ylab="Basis function, Bj(X)",
        main="Splines",cex.lab=1.5,cex.axis=1.5,lwd=2)
#title(sub = "knots =(1,1.9,2,3,7) with intercept")
#abline(v = 1.0)
#abline(v = 2.6, col = "lightblue")
abline(v = 5, col = "lightgreen")

b <- bs(trainData$rallyLen, df = 4, knots = a, intercept = T)

dim(b)
points(trainData$rallyLen, b[, 1], pch = 16)
for(i in 2:dim(b)[2]) points(trainData$rallyLen, b[,i])

trainData$b = b
######################################################################

idx_s <- sort(unique(trainData$s)) ### Indices of the servers
idx_r <- sort(unique(trainData$r))  ### Indices of the receicers

n_s <- length(idx_s) #number of servers
n_r <- length(idx_r) #number of receivers

sum(diff(idx_r)!=1) ##  we have 140 playes (all the indices between 1 and 140)
sum(idx_s%in%idx_r) ### All 90 servers are also reveivers

N <- length(trainData$s)

########################################################################
###################
#number of constrained splines
nconstr<-3
##################
setwd("C:/Users/vanen/Dropbox/TESI/R/MODELCORR")

jags <- rjags::jags.model('splinecourt2.bug',
                          data = list('j' = trainData$s,
                                      'b' = trainData$b,
                                      'z' = trainData$r,
                                      'N' = nrow(trainData),
                                      'K' = dim(b)[2]-nconstr,
                                      'n_r'= n_r,
                                      'court' = as.integer(trainData$court),
                                      'P' = dim(b)[2],
                                      'y' =trainData$isSvrWinner
                          ),
                          n.chains = 1,
                          n.adapt = 500)

######### PARAMETERS ##########
params <- c("beta", "alpha","beta.zero","epsilon","omega","be",
             "ae")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
param.posterior <- coda.samples(jags, params,
                                n.iter = 20000,thin=20)

geweke.plot(param.posterior[,"alpha[1,77]"])
summary(param.posterior)



################################################################################
################################### TRACEPLOT #########################################################
setwd("C:/Users/vanen/Dropbox/TESI/R/MODELCORR/MODELCOURT/ModelCourt3")


alp<- matrix(" ", ncol = 140 , nrow = 3)

for (j in 1:3) {
  for( i in 1:140){
    alp[j,i] <- paste("alpha[",j,",",i,"]",sep = "")  
  }
}   

traceplot(param.posterior[,alp[1,44]], main = "traceplot alpha on clay court",
          cex.lab=2.5, cex.axis=2, cex.main=2.7, cex.sub=1.5)


########################## BETA PARAMETERS ############################

bet<- matrix(" ", ncol = 140 , nrow = dim(trainData$b)[2])
for (j in 1:dim(trainData$b)[2]) {
  for( i in 1:140){
    bet[j,i] <- paste("beta[",j,",",i,"]",sep = "")  
    }
}   

bet[1,6]
traceplot(param.posterior[,bet[9,6]])
title(main = "traceplot player 1", sub = "beta 9, model with the intercept")

betz <- NULL
for( i in 1:dim(Bpred)[2]){
  betz[i] <- paste("beta.zero[",i,"]",sep = "")  
}
traceplot(param.posterior[,betz[7]]) 

om <- NULL
for( i in 1:dim(Bpred)[2]){
  om[i] <- paste("omega[",i,"]",sep = "")  
}
traceplot(param.posterior[,om[2]]) 

#####################################################################################
################################################################# AFTER ANALYSIS

param.posterior1 <- do.call("rbind", param.posterior)


############################# BETA 1 ############################

beta.posterior.1 <- as.data.frame(param.posterior1[,bet[1,]]) %>%
  gather("player", "beta1", contains("beta"))

beta.posterior.1$player  <- as.numeric(sapply(str_extract_all(beta.posterior.1$player, "[0-9]+"), "[", 2))

beta.median.1 <- beta.posterior.1 %>%
  group_by(player) %>%
  dplyr::summarise(
    beta1 = median(beta1)
  )

############################ BETA 2 ############################

beta.posterior.2 <- as.data.frame(param.posterior1[,bet[2,]]) %>%
  gather("player", "beta2", contains("beta"))

beta.posterior.2$player  <- as.numeric(sapply(str_extract_all(beta.posterior.2$player, "[0-9]+"), "[", 2))

beta.median.2 <- beta.posterior.2 %>%
  group_by(player) %>%
  dplyr::summarise(
    beta2 = median(beta2)
  )

############################# BETA 3 ############################

beta.posterior.3 <- as.data.frame(param.posterior1[,bet[3,]]) %>%
  gather("player", "beta3", contains("beta"))

beta.posterior.3$player  <- as.numeric(sapply(str_extract_all(beta.posterior.3$player, "[0-9]+"), "[", 2))

beta.median.3 <- beta.posterior.3 %>%
  group_by(player) %>%
  dplyr::summarise(
    beta3 = median(beta3)
  )
############################# BETA 4 ############################

beta.posterior.4 <- as.data.frame(param.posterior1[,bet[4,]]) %>%
  gather("player", "beta4", contains("beta"))

beta.posterior.4$player  <- as.numeric(sapply(str_extract_all(beta.posterior.4$player, "[0-9]+"), "[", 2))

beta.median.4 <- beta.posterior.4 %>%
  group_by(player) %>%
  dplyr::summarise(
    beta4 = median(beta4)
  )
############################# BETA 5 ############################

beta.posterior.5 <- as.data.frame(param.posterior1[,bet[5,]]) %>%
  gather("player", "beta5", contains("beta"))

beta.posterior.5$player  <- as.numeric(sapply(str_extract_all(beta.posterior.5$player, "[0-9]+"), "[", 2))

beta.median.5 <- beta.posterior.5 %>%
  group_by(player) %>%
  dplyr::summarise(
    beta5 = median(beta5)
  )
############################# BETA 6 ############################

beta.posterior.6 <- as.data.frame(param.posterior1[,bet[6,]]) %>%
  gather("player", "beta6", contains("beta"))

beta.posterior.6$player  <- as.numeric(sapply(str_extract_all(beta.posterior.6$player, "[0-9]+"), "[", 2))

beta.median.6 <- beta.posterior.6 %>%
  group_by(player) %>%
  dplyr::summarise(
    beta6 = median(beta6)
  )
############################# BETA 7 ############################

beta.posterior.7 <- as.data.frame(param.posterior1[,bet[7,]]) %>%
  gather("player", "beta7", contains("beta"))

beta.posterior.7$player  <- as.numeric(sapply(str_extract_all(beta.posterior.7$player, "[0-9]+"), "[", 2))

beta.median.7 <- beta.posterior.7 %>%
  group_by(player) %>%
  dplyr::summarise(
    beta7 = median(beta7)
  )
############################# BETA 8 ############################

beta.posterior.8 <- as.data.frame(param.posterior1[,bet[8,]]) %>%
  gather("player", "beta8", contains("beta"))

beta.posterior.8$player  <- as.numeric(sapply(str_extract_all(beta.posterior.8$player, "[0-9]+"), "[", 2))

beta.median.8 <- beta.posterior.8 %>%
  group_by(player) %>%
  dplyr::summarise(
    beta8 = median(beta8)
  )

############################# BETA 9 ############################

beta.posterior.9 <- as.data.frame(param.posterior1[,bet[9,]]) %>%
  gather("player", "beta9", contains("beta"))

beta.posterior.9$player  <- as.numeric(sapply(str_extract_all(beta.posterior.9$player, "[0-9]+"), "[", 2))

beta.median.9 <- beta.posterior.9 %>%
  group_by(player) %>%
  dplyr::summarise(
    beta9 = median(beta9)
  )

##################### ALPHA #####################  

alpha.posterior.1 <- as.data.frame(param.posterior1[,alp[1,]]) %>%
  gather("player", "alpha1", contains("alpha"))

alpha.posterior.1$player  <- as.numeric(sapply(str_extract_all(alpha.posterior.1$player, "[0-9]+"), "[", 2))

alpha.median.1 <- alpha.posterior.1 %>%
  group_by(player) %>%
  dplyr::summarise(
    alpha1 = median(alpha1)
  )

alpha.posterior.2 <- as.data.frame(param.posterior1[,alp[2,]]) %>%
  gather("player", "alpha2", contains("alpha"))

alpha.posterior.2$player  <- as.numeric(sapply(str_extract_all(alpha.posterior.2$player, "[0-9]+"), "[", 2))

alpha.median.2 <- alpha.posterior.2 %>%
  group_by(player) %>%
  dplyr::summarise(
    alpha2 = median(alpha2)
  )

alpha.posterior.3 <- as.data.frame(param.posterior1[,alp[3,]]) %>%
  gather("player", "alpha3", contains("alpha"))

alpha.posterior.3$player  <- as.numeric(sapply(str_extract_all(alpha.posterior.3$player, "[0-9]+"), "[", 2))

alpha.median.3 <- alpha.posterior.3 %>%
  group_by(player) %>%
  dplyr::summarise(
    alpha3 = median(alpha3)
  )
##################### PLOT BETA Vs ALPHA ##############

combine <- merge(alpha.median.1, alpha.median.2, by = "player")
combine <- merge(combine, alpha.median.3, by = "player")
combine <- merge(combine, beta.median.1, by = "player")
combine <- merge(combine, beta.median.2, by = "player")
combine <- merge(combine, beta.median.3, by = "player")
combine <- merge(combine, beta.median.4, by = "player")
combine <- merge(combine, beta.median.5, by = "player")
combine <- merge(combine, beta.median.6, by = "player")
combine <- merge(combine, beta.median.7, by = "player")
combine <- merge(combine, beta.median.8, by = "player")
combine <- merge(combine, beta.median.9, by = "player")
combine <- merge(combine, unique(ungroup(trainData) %>% 
                                   select(name = server, player = s)), by = "player")

# + alpha??

Si <- invLogit(combine$beta1 + 
               combine$beta2*as.numeric(trainData$b[which(trainData$rallyLen == 1),][1,2]) 
               + combine$alpha3) - invLogit(combine$alpha3)
Ri <- invLogit(combine$beta6*as.numeric(trainData$b[which(trainData$rallyLen == 15),][1,6])
              +combine$beta7*as.numeric(trainData$b[which(trainData$rallyLen == 15),][1,7])
              +combine$beta8*as.numeric(trainData$b[which(trainData$rallyLen == 15),][1,8])
              +combine$beta9*as.numeric(trainData$b[which(trainData$rallyLen == 15),][1,9])
              +combine$alpha3)

combine %>%
  ggplot(aes(y = Si, x = Ri))+
  geom_point() + 
  geom_hline(yintercept = 0.30, col = "red") + 
  geom_vline(xintercept = 0.40, col = "red") + 
  labs(title = "Figure",
       x = "Rally ability", y = "Serve Advantage") +
  geom_text(aes(label = name), size = 3, col = "#1792d0") + 
  theme_hc()


###################################
# CONFIDENCE INTERVAL WINNING ON POINT #


omega <- combine$beta1 + combine$beta2 
omega <- exp(omega)/(1+exp(omega))

p.interval(omega, HPD=TRUE, MM=TRUE, prob=0.95)

#### CONFIDENCE INTERVAL LOSING SERVE ADVANTAGE

alpha <- Si

p.interval(Si, HPD=TRUE, MM=TRUE, prob=0.95)

####################################################
#############################################################################
# TEST DATA 
testData <- dmen[which(!dmen$s %in% train),]
#control ok
dim(testData)+dim(trainData)

###########################################################################

setwd("C:/Users/vanen/Dropbox/TESI/R/MODELCORR")

jags <- rjags::jags.model('spline4.bug',
                          data = list('j' = trainData$s,
                                      'b' = trainData$b,
                                      'z' = trainData$r,
                                      'N' = nrow(trainData),
                                      'K' = dim(b)[2]-nconstr,
                                      'n_r'= n_r,
                                      
                                      'P' = dim(b)[2],
                                      'y' =trainData$isSvrWinner
                          ),
                          n.chains = 3,
                          n.adapt = 500)

dic <- dic.samples(jags, n.iter = 100, thin=1)
dic

#############################################################################
########################### WAIC ############################################
params_after<-c("loglik","lik")

setwd("C:/Users/vanen/Dropbox/TESI/R/MODELCORR")
library(jagsUI)
np.sim1 <- jags(data = list('j' = trainData$s,
                            'b' = trainData$b,
                            'z' = trainData$r,
                            'N' = nrow(trainData),
                            'K' = dim(b)[2]-nconstr,
                            'n_r'= n_r,
                            
                            'P' = dim(b)[2],
                            'y' =trainData$isSvrWinner), 
                parameters.to.save =  params_after, model.file = 'spline4.bug', 
                n.chains = 1, n.iter = 10)

library(loo)
sim1_loglik <- np.sim1$sims.list$loglik
sim1_waic <- waic(sim1_loglik)
sim1_waic

CPO <- 1/np.sim1$sims.list$lik
LPML<- sum(log(CPO))
LPML

################################################################
################################################################
################################################################


############### QUANTILE ###############
alpha.CI.1 <- alpha.posterior.1 %>%
  group_by(player) %>%
  dplyr::summarise(
    lower.alpha1 = quantile(alpha1,probs= 0.10),
    mean.alpha1  = quantile(alpha1,probs= 0.5),
    upper.alpha1 = quantile(alpha1,probs = 0.90)
  )

alpha.CI.2 <- alpha.posterior.2 %>%
  group_by(player) %>%
  dplyr::summarise(
    lower.alpha2 = quantile(alpha2,probs= 0.10),
    mean.alpha2  = quantile(alpha2,probs= 0.5),
    upper.alpha2 = quantile(alpha2,probs = 0.90)
  )

alpha.CI.3 <- alpha.posterior.3 %>%
  group_by(player) %>%
  dplyr::summarise(
    lower.alpha3 = quantile(alpha3,probs= 0.10),
    mean.alpha3  = quantile(alpha3,probs= 0.5),
    upper.alpha3 = quantile(alpha3,probs = 0.90)
  )

beta.CI.1 <- beta.posterior.1 %>%
  group_by(player) %>%
  dplyr::summarise(
    lower.beta1 = quantile(beta1,probs= 0.025),
    mean.beta1  = quantile(beta1,probs= 0.5),
    upper.beta1 = quantile(beta1,probs = 0.975)
  )

beta.CI.2 <- beta.posterior.2 %>%
  group_by(player) %>%
  dplyr::summarise(
    lower.beta2 = quantile(beta2,probs= 0.025),
    mean.beta2  = quantile(beta2,probs= 0.5),
    upperbeta2 = quantile(beta2,probs = 0.975)
  )


CredInt <- merge(alpha.CI.1, alpha.CI.2, by = "player")
CredInt <- merge(CredInt, alpha.CI.3, by = "player")
CredInt <- merge(CredInt, beta.CI.1, by = "player")
CredInt <- merge(CredInt, beta.CI.2, by = "player")
CredInt <- merge(CredInt, unique(ungroup(trainData) %>% 
                                   select(name = server, player = s)), by = "player")


x= seq(1:90)
df <- data.frame(x,
                 F =c(min(CredInt$lower.alpha1),max(CredInt$upper.alpha1)),
                 L =CredInt$lower.alpha1,
                 U =CredInt$upper.alpha1)

require(ggplot2)
ggplot(df, aes(x = x, y = F)) +
  geom_point(aes(y = CredInt$mean.alpha1)) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  geom_hline(aes(yintercept = 0.0), color = 'red')+
  labs(title = "Credible Intervals for alpha 1",
       x = "servers", y = "Ranges")

df <- data.frame(x,
                 F =c(min(CredInt$lower.alpha2),max(CredInt$upper.alpha2)),
                 L =CredInt$lower.alpha2,
                 U =CredInt$upper.alpha2)

ggplot(df, aes(x = x, y = F)) +
  geom_point(aes(y = CredInt$mean.alpha2)) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  geom_hline(aes(yintercept = 0.0), color = 'red')+
  labs(title = "Credible Intervals for alpha 2",
       x = "servers", y = "Ranges")

df <- data.frame(x,
                 F =c(min(CredInt$lower.alpha3),max(CredInt$upper.alpha3)),
                 L =CredInt$lower.alpha3,
                 U =CredInt$upper.alpha3)

ggplot(df, aes(x = x, y = F)) +
  geom_point(aes(y = CredInt$mean.alpha3)) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  geom_hline(aes(yintercept = 0.0), color = 'red')+
  labs(title = "Credible Intervals for alpha 3",
       x = "servers", y = "Ranges")


CredInt$name

Cred<- combine[-c(1,2,3,4,6,7,9,10,11,12,14,15,18,20,21,22,23,25,26,28,30,31,32,34,
                  35,37,38,39,40,43,44,46,47,48,49,50,51,52,53,54,55,56,57,59,61,62,
                  64,65,66,67,69,71,73,75,76,78,79,80,81,82,84,85,87,89,90),]
dim(Cred)

newSi <- invLogit(Cred$beta1 + 
                 Cred$beta2*as.numeric(trainData$b[which(trainData$rallyLen == 1),][1,2]) 
               + Cred$alpha3) - invLogit(Cred$alpha3)
newRi <- invLogit(Cred$beta6*as.numeric(trainData$b[which(trainData$rallyLen == 15),][1,6])
               +Cred$beta7*as.numeric(trainData$b[which(trainData$rallyLen == 15),][1,7])
               +Cred$beta8*as.numeric(trainData$b[which(trainData$rallyLen == 15),][1,8])
               +Cred$beta9*as.numeric(trainData$b[which(trainData$rallyLen == 15),][1,9])
               +Cred$alpha3)

Cred %>%
  ggplot(aes(y = newSi, x = newRi))+
  geom_point() + 
  geom_hline(yintercept = 0.30, col = "red") + 
  geom_vline(xintercept = 0.40, col = "red") + 
  labs(title = "Serve Advantage vs Rally Ability",
       x = "Rally ability", y = "Serve Advantage") +
  geom_text(aes(label = name), size = 3, col = "#1792d0") + 
  theme_hc()

##############################################################################
##############################################################################
######### DIFFERENT ALPHA INTERVALS #################


CredInt$lower.alpha3[which(CredInt$name=="Andy Murray")]
CredInt$mean.alpha3[which(CredInt$name=="Andy Murray")]
CredInt$upper.alpha3[which(CredInt$name=="Andy Murray")]

CredInt$lower.alpha3[which(CredInt$name=="Rafael Nadal")]
CredInt$mean.alpha3[which(CredInt$name=="Rafael Nadal")]
CredInt$upper.alpha3[which(CredInt$name=="Rafael Nadal")]

CredInt$lower.alpha3[which(CredInt$name=="Roger Federer")]
CredInt$mean.alpha3[which(CredInt$name=="Roger Federer")]
CredInt$upper.alpha3[which(CredInt$name=="Roger Federer")]

CredInt$lower.alpha3[which(CredInt$name=="Novak Djokovic")]
CredInt$mean.alpha3[which(CredInt$name=="Novak Djokovic")]
CredInt$upper.alpha3[which(CredInt$name=="Novak Djokovic")]



