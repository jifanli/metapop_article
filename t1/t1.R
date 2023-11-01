# t1 starts at the mle found by profile6 (alpha_be)
# t1 uses li23v3
i <- 3

library(metapoppkg)
library(doParallel)
library(doRNG)

cores <-  as.numeric(Sys.getenv('SLURM_NTASKS_PER_NODE', unset = NA))
if(is.na(cores)) cores <- detectCores()
registerDoParallel(cores)

out_dir <- paste0("out_",i,"/")
if(!dir.exists(out_dir)) dir.create(out_dir)

para <- readRDS("profile_alpha_be_full.rds")
mle <- unlist(para[which.max(para$logLik),-c(1,2)])
mle <- mle[names(mle)%in%names(coef(li23v3(U = 2, for_ibpf = F)))] # from for_ibpf=T to for_ibpf=F

U <-switch(i,5,373,373)
Nrep <-switch(i,3,10,36)    # number of ibpf searches
Nbpf <-switch(i,3,20,50)  # number of iterations for each search
Np <-switch(i,5,100,1e3)   # number of particles for each iteration
Nlik <-switch(i,2,5,10)    # number of replications of likelihood evaluation
Np_lik <- switch(i,5,100,2e3)  # number of particles for each likelihood evaluation

m1 <- li23v3(U=U)
p1 <- coef(m1)
IVP_names <- names(p1)[c(grep("E_0",names(p1)),grep("A_0",names(p1)))]
AF_names <- names(p1)[grep("af",names(p1))]
BE_names <- names(p1)[grep("be",names(p1))]
RP_names <- setdiff(names(p1),c(IVP_names,AF_names,BE_names))

parNames <- c(IVP_names,BE_names,AF_names,RP_names)

basicParNames <- substring(names(mle),
  first=1, last = nchar(names(mle))-1)
mle_expanded <- rep(mle,each=U)
names(mle_expanded) <- paste0(rep(basicParNames,each=U),
  rep(1:U,length(basicParNames))
)

rp_sd <- 0.02 / 5  # reduction for ibpf
ivp_sd <- 0.05/5

Td_expanded <- c(paste0(rep("Td_be",each=U),1:U),paste0(rep("Td_af",each=U),1:U))
# remove Td
BE_names <- BE_names[-grep("Td", BE_names)]
AF_names <- AF_names[-grep("Td", AF_names)]
# Set the rw.sd for the parameters
string_rwsd="rw_sd(" 
for(k in Td_expanded) string_rwsd=paste(string_rwsd, paste0(k,"=0,"))
for(k in RP_names) string_rwsd=paste(string_rwsd, paste0(k,"=",rp_sd,","))
for(k in AF_names) string_rwsd=paste(string_rwsd, paste0(k,"=ifelse(time>14,rp_sd,0),"))
for(k in BE_names) string_rwsd=paste(string_rwsd, paste0(k,"=ifelse(time<15,rp_sd,0),"))
for(k in IVP_names) string_rwsd=paste(string_rwsd, paste0(k,"=ivp(",ivp_sd,"),"))
substring(string_rwsd, first=nchar(string_rwsd), last = nchar(string_rwsd))=")"
covid_rw.sd<-eval(parse(text=string_rwsd))

# Do the local search and save the results
registerDoRNG(3123462)
bake(file =paste0(out_dir,"t1.rds"), {

  cat(capture.output(sessionInfo()),
    file=paste0(out_dir,"sessionInfo_t1.txt"),sep="\n")

  foreach(nrep = 1:Nrep,.packages='spatPomp',.combine=c) %dopar% {
    ibpf(m1,
      Nbpf=Nbpf,
      rw.sd = covid_rw.sd,
      params=mle_expanded,
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

