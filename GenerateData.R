set.seed(1234)

### initial parameters
nGroup <- 3
nFac <- 4
nPhy <- 3
nEpisode <- 300


### First generate the data based on initial parameters
dataUse <- do.call(rbind, lapply(1:nGroup, function(grp) {
    do.call(rbind, lapply(1:nFac, function(fac) {
        
        data.frame(grp    = grp,
              hospID = paste0("Hosp_", fac),
              physID = rep(paste0("Phys_", LETTERS[1:nPhy]), 
                           times = round(rnorm(nPhy, 
                                              mean = nEpisode,
                                              sd = nEpisode/10))))
    
    }))
}))
totalN <- nrow(dataUse)

### add the procedure date
startDate <- as.Date("2015/01/01", "%Y/%m/%d") 
dataUse$procDate <- startDate + round(runif(totalN, min = 0, max = 850))

### Gender AGE
dataUse$gender <- sample(c("M", "F"), totalN, replace = TRUE)
dataUse$age <- sample(19:98, totalN, replace = TRUE)


### ao
dataUse$preAdverseOutcome <- runif(n = totalN, min = 0.001, max =0.4)
dataUse$obsAdverseOutcome <- rbinom(totalN, 1, prob = exp(dataUse$preAdverseOutcome))


### cost
dataUse$preCost <- runif(n = totalN, min = 22000, max = 35000)
dataUse$obsCost <- exp(rnorm(n = totalN, mean = 9.8, sd = 1))

### diagnosis code
diagSource <- read.csv("./data/diag.csv")

listOfDiag <- paste(diagSource$CODE, diagSource$LONG.DESCRIPTION, sep = ": ")
nDiag <- 5
for(i in 1:nDiag) {
    dataUse[, paste0("diag_", i)] <- sample(listOfDiag, totalN, replace = TRUE)
    ### alone with POA information
    dataUse[, paste0("poa_", i)] <- sample(0:1, totalN, replace = TRUE)
}
### first diag code is always POA
dataUse[, "poa_1"] <- 1



### procedure code
procSource <- read.csv("./data/proc.csv")

listOfProc <- paste(procSource$ICD.10.CODES, procSource$Procedure.Code.Descriptions, sep = ": ")
nProc <- 3
for(i in 1:nProc) {
    dataUse[, paste0("proc_", i)] <- sample(listOfProc, totalN, replace = TRUE)
}


### output the data
write.csv(dataUse, "./data/episode_data.csv")
    
    








