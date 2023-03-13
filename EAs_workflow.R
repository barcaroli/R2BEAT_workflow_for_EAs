## ----setup, include=FALSE, message=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
	echo = TRUE,
	message = FALSE,
	warning = FALSE
)
library(R2BEAT)
library(ReGenesees)


## --------------------------------------------------------------------------------------------
load("frame_pop_EA.RData")
str(frame_pop_EA)


## --------------------------------------------------------------------------------------------
length(unique(frame_pop_EA$id_hh))


## --------------------------------------------------------------------------------------------
length(unique(frame_pop_EA$id_MUN_EA))


## --------------------------------------------------------------------------------------------
summary(frame_pop_EA$nfam_in_EA)

## --------------------------------------------------------------------------------------------
hist(frame_pop_EA$nfam_in_EA)


## --------------------------------------------------------------------------------------------
load("sample.RData")
str(samp)


## ----message=FALSE, warning=FALSE------------------------------------------------------------
samp$stratum_2 <- as.factor(samp$stratum_2)
sample.des <- e.svydesign(samp, 
                          ids= ~ id_MUN_EA + id_hh, 
                          strata = ~ stratum_2, 
                          weights = ~ weight,
                          self.rep.str = ~ SR,
                          check.data = TRUE)


## --------------------------------------------------------------------------------------------
ls <- find.lon.strata(sample.des)
if (!is.null(ls)) sample.des <- collapse.strata(sample.des)


## --------------------------------------------------------------------------------------------
totals <- pop.template(sample.des,
                       calmodel = ~ sex : cl_age, 
                       partition = ~ region)
totals <- fill.template(frame_pop_EA, totals, mem.frac = 10)
sample.cal <- e.calibrate(sample.des, 
                          totals,
                          calmodel = ~ sex : cl_age, 
                          partition = ~ region,
                          calfun = "logit",
                          bounds = c(0.3, 2.6), 
                          aggregate.stage = 2,
                          force = FALSE)


## ----message=FALSE, warning=FALSE------------------------------------------------------------
samp_frame <- frame_pop_EA
RGdes <- sample.des
RGcal <- sample.cal
strata_var <- c("stratum")      
target_vars <- c("income_hh",
                 "active",
                 "inactive",
                 "unemployed")   
weight_var <- "weight"
deff_var <- "stratum"            
id_PSU <- c("id_MUN_EA")      
id_SSU <- c("id_hh")             
domain_var <- c("region") 
delta <- length(unique(samp_frame$id_ind)) / length(unique(samp_frame$id_hh))                  
minimum <- 24                

inp <- prepareInputToAllocation2(
  samp_frame,  # sampling frame
  RGdes,       # ReGenesees design object
  RGcal,       # ReGenesees calibrated object
  id_PSU,      # identification variable of PSUs
  id_SSU,      # identification variable of SSUs
  strata_var,  # strata variables
  target_vars, # target variables
  deff_var,    # deff variables
  domain_var,  # domain variables
  delta,       # Average number of SSUs for each selection unit
  minimum      # Minimum number of SSUs to be selected in each PSU
)


## --------------------------------------------------------------------------------------------
strata <- inp$strata
deff <- inp$deff


## --------------------------------------------------------------------------------------------
strata2 <- strata
strata2$S1 <- strata$S1 * sqrt(deff$DEFF1)
strata2$S2 <- strata$S2 * sqrt(deff$DEFF2)
strata2$S3 <- strata$S3 * sqrt(deff$DEFF3)
strata2$S4 <- strata$S4 * sqrt(deff$DEFF4)


## --------------------------------------------------------------------------------------------
cv <- as.data.frame(list(DOM=c("DOM1","DOM2"),
                         CV1=c(0.02,0.03),
                         CV2=c(0.03,0.06),
                         CV3=c(0.03,0.06),
                         CV4=c(0.05,0.08)))
cv


## --------------------------------------------------------------------------------------------
alloc <- beat.1st(stratif = strata2, errors = cv)
sum(alloc$n)


## --------------------------------------------------------------------------------------------
alloc_no_deff <- beat.1st(stratif = strata, errors = cv)
sum(alloc_no_deff$n)


## --------------------------------------------------------------------------------------------
# source("sens_names.R")
sens_names(s=alloc$sensitivity,
           target_vars=target_vars,
           strata=strata2)


## --------------------------------------------------------------------------------------------
des_file <- inp$des_file 
des_file$DELTA <- with(samp_frame, tapply(id_ind, list(stratum), function(x) length(unique(x)))) / 
  with(samp_frame, tapply(id_hh, list(stratum), function(x) length(unique(x))))
des_file


## --------------------------------------------------------------------------------------------
# source("select_PSU2.R")
psu_file <- inp$psu_file
PSU_sample <- select_PSU2(alloc=alloc, 
                    type="ALLOC", 
                    des_file=des_file,
                    psu_file=psu_file)


## --------------------------------------------------------------------------------------------
head(PSU_sample$sample_PSUs,20)


## --------------------------------------------------------------------------------------------
PSU_sample$PSU_stats


## --------------------------------------------------------------------------------------------
sum(1/PSU_sample$sample_PSUs$pik_1st)


## --------------------------------------------------------------------------------------------
# source("select_SSU.R")
PSU_sample$sample_PSUs$Pik <- PSU_sample$sample_PSUs$pik_1st
samp <- select_SSU(df=samp_frame,
                   PSU_code="id_MUN_EA",
                   SSU_code="id_hh",
                   PSU_sampled=PSU_sample$sample_PSU,
                   verbose=FALSE)


## --------------------------------------------------------------------------------------------
head(samp)


## --------------------------------------------------------------------------------------------
sum(samp$weight)

