require(data.table);require(cluster);require(factoextra);require(evd)
setwd("C:/Users/canti/OneDrive - University of Leeds/PhD Thesis/Modelling/Aggregation analysis")
rm(list=ls())
#
#
#### Simulates data 1 variable and 2 modes N=1000X2 ####

n      = 1000 # sample size
tvar   = 0.6 # share threshold for explanatory variables (var)
zones  = 10 # define number of zones in case of defining travel time and travel cost by zones
ltime1 = c(10,20,30,40,50) # levels for travel time mode 1
lcost1 = c(2,4,6,8,10) # levels for travel time mode 1
ltime2 = c(5,15,25,30,35) # levels for travel time mode 2
lcost2 = c(3,6,9,12,14) # levels for travel time mode 2
asc1   = 0.4
asc2   = 0
bt     = -0.06
bc     = 60*bt/15 # assuming VOT=15 £/hr
bvar1  = 0.9

# Add extreme value error in the utility
# calculate rmsea in the simulated data to see how deterministic is the simulated data

database=data.table(id=1:n)

# creates draws 
database[,`:=`(draws_var1        = runif(nrow(database)),
               draws_origin      = runif(nrow(database)),
               draws_destination = runif(nrow(database)),
               draws_time1       = sample(1:length(ltime1),nrow(database),replace = TRUE),
               draws_cost1       = sample(1:length(lcost1),nrow(database),replace = TRUE),
               draws_time2       = sample(1:length(ltime2),nrow(database),replace = TRUE),
               draws_cost2       = sample(1:length(lcost2),nrow(database),replace = TRUE),
               draws_mode1       = runif(nrow(database)))] 

# creates variables
database[,`:=`(var1              = ifelse(draws_var1<tvar,1,0),
               origin            = round(1+(zones-1)*draws_origin,0),
               destination       = round(1+(zones-1)*draws_destination,0),
               time1             = ltime1[draws_time1],
               cost1             = lcost1[draws_cost1],
               time2             = ltime2[draws_time2],
               cost2             = lcost2[draws_cost2])] 

# creates levels of service by od in case of using zones but this is not been currently used for simulating choices
od = setDT(merge(1:zones,1:zones))
od[,`:=`(draws_time1 = sample(1:length(ltime1),nrow(od),replace = TRUE),
         draws_cost1 = sample(1:length(lcost1),nrow(od),replace = TRUE),
         draws_time2 = sample(1:length(ltime2),nrow(od),replace = TRUE),
         draws_cost2 = sample(1:length(lcost2),nrow(od),replace = TRUE))]

od[,`:=`(time1_od       = ltime1[draws_time1],
         cost1_od       = lcost1[draws_cost1],
         time2_od       = ltime2[draws_time2],
         cost2_od       = lcost2[draws_cost2])]

database=merge(database,od[,!grepl("draw",names(od)),with=FALSE],by.x=c("origin","destination"),by.y=c("x","y"),all.x=TRUE)

# duplicates database to get two agents with identical characteristics
database=rbind(database,database)[(n+1):(n*2),id:=id+n][order(id)]

# simulates choices
database[,`:=`(v1 = asc1 + time1*bt + cost1*bc + var1*bvar1,
               v2 = asc2 + time2*bt + cost2*bc)]

database[,`:=`(ev1=exp(v1),
               ev2=exp(v2))]

database[,`:=`(p1=ev1/(ev1+ev2),
               p2=ev2/(ev1+ev2))]

database[,choice:=ifelse(draws_mode1<p1,1,2)]

# database[,choice:=ifelse(p1<p1,1,2)]

hist(database$v1-database$v2)

saveRDS(database,"database_simulation_v0.rds")
#
#### Simulates data 1 variable and 2 modes OD N=1000X2 ####

n      = 1000 # sample size
tvar   = 0.6 # share threshold for explanatory variables (var)
zones  = 10 # define number of zones in case of defining travel time and travel cost by zones
ltime1 = c(10,20,30,40,50) # levels for travel time mode 1
lcost1 = c(2,4,6,8,10) # levels for travel time mode 1
ltime2 = c(5,15,25,30,35) # levels for travel time mode 2
lcost2 = c(3,6,9,12,14) # levels for travel time mode 2
asc1   = 0.4
asc2   = 0
bt     = -0.06
bc     = 60*bt/15 # assuming VOT=15 £/hr
bvar1  = 0.9

# Add extreme value error in the utility
# calculate rmsea in the simulated data to see how deterministic is the simulated data

database=data.table(id=1:n)

# creates draws 
database[,`:=`(draws_var1        = runif(nrow(database)),
               draws_origin      = runif(nrow(database)),
               draws_destination = runif(nrow(database)),
               draws_time1       = sample(1:length(ltime1),nrow(database),replace = TRUE),
               draws_cost1       = sample(1:length(lcost1),nrow(database),replace = TRUE),
               draws_time2       = sample(1:length(ltime2),nrow(database),replace = TRUE),
               draws_cost2       = sample(1:length(lcost2),nrow(database),replace = TRUE),
               draws_mode1       = runif(nrow(database)))] 

# creates variables
database[,`:=`(var1              = ifelse(draws_var1<tvar,1,0),
               origin            = round(1+(zones-1)*draws_origin,0),
               destination       = round(1+(zones-1)*draws_destination,0),
               time1             = ltime1[draws_time1],
               cost1             = lcost1[draws_cost1],
               time2             = ltime2[draws_time2],
               cost2             = lcost2[draws_cost2])] 

# creates levels of service by od in case of using zones but this is not been currently used for simulating choices
od = setDT(merge(1:zones,1:zones))
od[,`:=`(draws_time1 = sample(1:length(ltime1),nrow(od),replace = TRUE),
         draws_cost1 = sample(1:length(lcost1),nrow(od),replace = TRUE),
         draws_time2 = sample(1:length(ltime2),nrow(od),replace = TRUE),
         draws_cost2 = sample(1:length(lcost2),nrow(od),replace = TRUE))]

od[,`:=`(time1_od       = ltime1[draws_time1],
         cost1_od       = lcost1[draws_cost1],
         time2_od       = ltime2[draws_time2],
         cost2_od       = lcost2[draws_cost2])]

database=merge(database,od[,!grepl("draw",names(od)),with=FALSE],by.x=c("origin","destination"),by.y=c("x","y"),all.x=TRUE)

# duplicates database to get two agents with identical characteristics
database=rbind(database,database)[(n+1):(n*2),id:=id+n][order(id)]

# simulates choices
database[,`:=`(gev1=-log(-log(runif(nrow(database)))),
               gev2=-log(-log(runif(nrow(database))))
)]

scale=1

database[,`:=`(v1 = scale*(asc1 + time1_od*bt + cost1_od*bc + var1*bvar1) ,
               v2 = scale*(asc2 + time2_od*bt + cost2_od*bc             ) )]
database[,`:=`(u1 = v1 + gev1,
               u2 = v2 + gev2)]

database[,choice_test:=ifelse(v2>v1,2,1)]
database[,choice:=ifelse(u2>u1,2,1)]
database[,P1:=exp(v1)/(exp(v1)+exp(v2))]
database[,logPch:=(choice==1)*log(P1)+(choice==2)*log(1-P1)]

rho2=1-sum(database[,logPch])/(nrow(database)*log(0.5))
rho2

database[choice!=choice_test,.N]/nrow(database)

database[,id:=1:nrow(database)]

database[,od:=paste0(origin,"-",destination)]

saveRDS(database,"database_simulation_v0od.rds")
#
#### Simulates data 1 variable and 2 modes N=all combinations x 10 ####

# Add extreme value error in the utility
# calculate rmsea in the simulated data to see how deterministic is the simulated data

n      = 1000 # sample size
tvar   = 0.6 # share threshold for explanatory variables (var)
zones  = 10 # define number of zones in case of defining travel time and travel cost by zones
ltime1 = c(10,20,30,40,50) # levels for travel time mode 1
lcost1 = c(2,4,6,8,10) # levels for travel time mode 1
ltime2 = c(5,15,25,30,40) # levels for travel time mode 2
lcost2 = c(3,6,9,12,14) # levels for travel time mode 2
asc1   = 0.4
asc2   = 0
bt     = -0.06
bc     = 60*bt/15 # assuming VOT=15 £/hr
bvar1  = 0.9

database=setDT(expand.grid(ltime1,lcost1,ltime2,lcost2,bvar1))
names(database)=c("time1","cost1","time2","cost2","var1")
database[,id_comb:=1:nrow(database)]

for (i in 1:9){database=rbind(database,database[1:625])}

# creates draws 
database[,`:=`(draws_origin      = runif(nrow(database)),
               draws_destination = runif(nrow(database)),
               draws_mode1       = runif(nrow(database)))] 

# allocates zones
database[,`:=`(origin            = round(1+(zones-1)*draws_origin,0),
               destination       = round(1+(zones-1)*draws_destination,0))] 

# creates levels of service by od in case of using zones but this is not been currently used for simulating choices
od = setDT(merge(1:zones,1:zones))
od[,`:=`(draws_time1 = sample(1:length(ltime1),nrow(od),replace = TRUE),
         draws_cost1 = sample(1:length(lcost1),nrow(od),replace = TRUE),
         draws_time2 = sample(1:length(ltime2),nrow(od),replace = TRUE),
         draws_cost2 = sample(1:length(lcost2),nrow(od),replace = TRUE))]

od[,`:=`(time1_od       = ltime1[draws_time1],
         cost1_od       = lcost1[draws_cost1],
         time2_od       = ltime2[draws_time2],
         cost2_od       = lcost2[draws_cost2])]

database=merge(database,od[,!grepl("draw",names(od)),with=FALSE],by.x=c("origin","destination"),by.y=c("x","y"),all.x=TRUE)

# simulates choices
database[,`:=`(v1 = asc1 + time1*bt + cost1*bc + var1*bvar1,
               v2 = asc2 + time2*bt + cost2*bc)]

database[,`:=`(ev1=exp(v1),
               ev2=exp(v2))]

database[,`:=`(p1=ev1/(ev1+ev2),
               p2=ev2/(ev1+ev2))]

database[,choice:=ifelse(draws_mode1<p1,1,2)]

# database[,choice:=ifelse(p1<p1,1,2)]

hist(database$v1-database$v2)

database[,id:=1:nrow(database)]

saveRDS(database,"database_simulation_v1.rds")
#
#### Simulates data 1 variable and 2 modes N=all combinations x 10 + GEV error ####

# Add extreme value error in the utility
# calculate rmsea in the simulated data to see how deterministic is the simulated data

n      = 1000 # sample size
tvar   = 0.6 # share threshold for explanatory variables (var)
zones  = 10 # define number of zones in case of defining travel time and travel cost by zones
ltime1 = c(10,20,30,40,50) # levels for travel time mode 1
lcost1 = c(2,4,6,8,10) # levels for travel time mode 1
ltime2 = c(5,15,25,30,40) # levels for travel time mode 2
lcost2 = c(3,6,9,12,14) # levels for travel time mode 2
asc1   = 0.4
asc2   = 0
bt     = -0.06
bc     = 60*bt/15 # assuming VOT=15 £/hr
bvar1  = 0.9

database=setDT(expand.grid(ltime1,lcost1,ltime2,lcost2,bvar1))
names(database)=c("time1","cost1","time2","cost2","var1")
database[,id_comb:=1:nrow(database)]

for (i in 2:10){database=rbind(database,database[1:625][])}

# creates draws 
database[,`:=`(draws_origin      = runif(nrow(database)),
               draws_destination = runif(nrow(database)),
               draws_mode1       = runif(nrow(database)))] 

# allocates zones
database[,`:=`(origin            = round(1+(zones-1)*draws_origin,0),
               destination       = round(1+(zones-1)*draws_destination,0))] 

# creates levels of service by od in case of using zones but this is not been currently used for simulating choices
od = setDT(merge(1:zones,1:zones))
od[,`:=`(draws_time1 = sample(1:length(ltime1),nrow(od),replace = TRUE),
         draws_cost1 = sample(1:length(lcost1),nrow(od),replace = TRUE),
         draws_time2 = sample(1:length(ltime2),nrow(od),replace = TRUE),
         draws_cost2 = sample(1:length(lcost2),nrow(od),replace = TRUE))]

od[,`:=`(time1_od       = ltime1[draws_time1],
         cost1_od       = lcost1[draws_cost1],
         time2_od       = ltime2[draws_time2],
         cost2_od       = lcost2[draws_cost2])]

database=merge(database,od[,!grepl("draw",names(od)),with=FALSE],by.x=c("origin","destination"),by.y=c("x","y"),all.x=TRUE)

# simulates choices
database[,`:=`(gev1=-log(-log(runif(nrow(database)))),
               gev2=-log(-log(runif(nrow(database))))
)]

scale=2

database[,`:=`(v1 = scale*(asc1 + time1*bt + cost1*bc + var1*bvar1) ,
               v2 = scale*(asc2 + time2*bt + cost2*bc             ) )]
database[,`:=`(u1 = v1 + gev1,
               u2 = v2 + gev2)]

database[,choice_test:=ifelse(v2>v1,2,1)]
database[,choice:=ifelse(u2>u1,2,1)]
database[,P1:=exp(v1)/(exp(v1)+exp(v2))]
database[,logPch:=(choice==1)*log(P1)+(choice==2)*log(1-P1)]

rho2=1-sum(database[,logPch])/(nrow(database)*log(0.5))
rho2

database[choice!=choice_test,.N]/nrow(database)

database[,.N,by=c("choice","choice_test")]

database[,id:=1:nrow(database)]

saveRDS(database,"database_simulation_v2.rds")
#
#### Clustering ####

database=readRDS("database_simulation_v1.rds")

keep=c("origin","destination","var1","time1","cost1","time2","cost2")
keep=c("var1","time1","cost1","time2","cost2")

dist=daisy(database[,..keep],metric="gower")

fviz_nbclust(as.matrix(dist), cluster::pam, method = "silhouette",k.max=10)

fit=pam(dist,diss=TRUE,999)

database[,clustering:=c(fit$clustering,fit$clustering)]

View(database[clustering==1])

keep=c("var1","time1","cost1","time2","cost2")
dist=daisy(database[1:625,..keep],metric="gower")
fit=pam(dist,diss=TRUE,j[i])
database[,clustering:=c(fit$clustering,fit$clustering)]

database[,N:=.N,by=clustering]

