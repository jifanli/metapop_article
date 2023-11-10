# profile for R0_be

i <- 3
set.seed(12354)
library(metapoppkg)
library(doParallel)
library(doRNG)
library(tidyverse)

cores <-  as.numeric(Sys.getenv('SLURM_NTASKS_PER_NODE', unset = NA))
if(is.na(cores)) cores <- detectCores()
registerDoParallel(cores)

out_dir <- paste0("out_",i,"/")
if(!dir.exists(out_dir)) dir.create(out_dir)

U <-switch(i,5,373,373)
Nrep <-switch(i,3,5,9)    # number of ibpf searches
Nbpf <-switch(i,3,20,50)  # number of iterations for each search
Np <-switch(i,5,100,1e3)   # number of particles for each iteration
Nlik <-switch(i,2,5,10)    # number of replications of likelihood evaluation
Np_lik <- switch(i,5,100,2e3)  # number of particles for each likelihood evaluation
profile_pts <- 7

m1 <- li23v2(U=U)
p1 <- coef(m1)
IVP_names <- names(p1)[c(grep("E_0",names(p1)),grep("A_0",names(p1)))]
AF_names <- names(p1)[grep("af",names(p1))]
BE_names <- names(p1)[grep("be",names(p1))]
RP_names <- setdiff(names(p1),c(IVP_names,AF_names,BE_names))

parNames <- c(IVP_names,BE_names,AF_names,RP_names)

basicParNames <- c("alpha_be","R0_be","alpha_af","R0_af",
                   "mu_be","Z_be","D_be","mu_af","Z_af","D_af",
                   "theta","tau","Td_be","Td_af","sigma_SE",
                   "E_0","A_0")

params_full <- readRDS("l8.rds")
params_full$R0_be1 <- R0(params_full, be=T)[,1]
params_full$R0_af1 <- R0(params_full, be=F)[,1]
params <- params_full[,c("logLik","logLik_se")]
params_full <- params_full[names(params_full)%in%names(coef(m1))] 

for(k in basicParNames) {
  params <- cbind(params,params_full[,paste0(k,1)])
}
colnames(params)[-c(1,2)] <- paste0(basicParNames,1)

params %>% 
  filter(logLik>max(logLik)-30) %>%
  select(-logLik,-logLik_se) %>%
  gather(variable,value) %>%
  group_by(variable) %>%
  summarize(min=min(value),max=max(value)) %>%
  ungroup() %>%
  column_to_rownames(var="variable") %>%
  t() -> box

idx <- setdiff(1:ncol(box), grep("R0_be", colnames(box)))
profile_design(
  R0_be1=seq(9,24,length=profile_pts),
  lower=box["min",idx],upper=box["max",idx],
  nprof=Nrep
) -> starts

# expand starts
starts <- as.data.frame(rep(starts[,1:ncol(starts)],each=U))
# change the order of the names
basicParNames <- substring(colnames(box),
                           first=1, last = nchar(colnames(box))-1)
basicParNames <- c("R0_be",basicParNames[idx])
colnames(starts) <- paste0(rep(basicParNames,each=U),1:U)

rp_sd <- 0.02 / 5  # reduction for ibpf
ivp_sd <- 0.05/5

R0_be_expanded <- paste0(rep("R0_be",each=U),1:U)
# remove R0_be
Td_expanded <- c(paste0(rep("Td_be",each=U),1:U),paste0(rep("Td_af",each=U),1:U)) # remove Td
BE_names <- BE_names[-grep("R0_be", BE_names)]
BE_names <- BE_names[-grep("Td", BE_names)]
AF_names <- AF_names[-grep("Td", AF_names)]
# Set the rw.sd for the parameters
string_rwsd="rw_sd(" 
for(k in R0_be_expanded) string_rwsd=paste(string_rwsd, paste0(k,"=0,"))
for(k in Td_expanded) string_rwsd=paste(string_rwsd, paste0(k,"=0,"))
for(k in RP_names) string_rwsd=paste(string_rwsd, paste0(k,"=",rp_sd,","))
for(k in AF_names) string_rwsd=paste(string_rwsd, paste0(k,"=ifelse(time>14,rp_sd,0),"))
for(k in BE_names) string_rwsd=paste(string_rwsd, paste0(k,"=ifelse(time<15,rp_sd,0),"))
for(k in IVP_names) string_rwsd=paste(string_rwsd, paste0(k,"=ivp(",ivp_sd,"),"))
substring(string_rwsd, first=nchar(string_rwsd), last = nchar(string_rwsd))=")"
covid_rw.sd<-eval(parse(text=string_rwsd))


registerDoRNG(3123462)
bake(file =paste0(out_dir,"profile_R0_be.rds"), {
  
  cat(capture.output(sessionInfo()),
      file=paste0(out_dir,"sessionInfo_R0_be.txt"),sep="\n")
  
  foreach(i=1:nrow(starts),.packages='spatPomp',.combine=c) %dopar% {
    ibpf(m1,
         Nbpf=Nbpf,
         rw.sd = covid_rw.sd,
         params=starts[i,],
         cooling.type = "geometric",
         Np=Np,
         cooling.fraction.50 = 0.5,
         block_size=1,
         spat_regression=0.1,
         sharedParNames=basicParNames,
         unitParNames=NULL,
         verbose=FALSE
    ) -> s
    coef(s) <- mean_by_unit(coef(s),basicParNames,U=U)
    s
  } -> searches
  foreach(s=searches,.packages='spatPomp',.combine=rbind) %dopar% {
    logmeanexp(
      replicate(Nlik,
                logLik(bpfilter(m1,params=coef(s),Np=Np_lik,block_size=1))
      ),se = TRUE
    )
  } -> evals  
  data.frame(logLik=evals[,1],logLik_se=evals[,2],t(sapply(searches,coef)))
}) -> out

head(sort(out$logLik,decreasing=T),10)

attributes(out)$system.time

