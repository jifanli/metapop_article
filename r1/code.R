# r1 investigates residuals for the best fit so far (s5).
# comparing conditional block likelihoods to benchmarks
# investigating the sd of the logLik estimate


# this was originally computed for b11, which struggled with Zhuhai.
# e3 does not have that problem, which is nice

i <- 2

U <-switch(i,5,373)


out <- readRDS("../s5/out_3/s5.rds")
mle <- unlist(out[which.max(out$logLik),-c(1,2)])
lik <- unlist(out[which.max(out$logLik),c(1,2)])

mle_contracted <- spatPomp::contract_params(mle,expanded=c(
  'E_0','A_0','alpha_be','alpha_af',
 'Beta_be','Beta_af','Z','D','Td_be','Td_af','mu_be','mu_af','tau', 'sigma_SE','theta'),
  U=373)
 
library(metapoppkg)
library(doParallel)
library(doRNG)

cores <-  as.numeric(Sys.getenv('SLURM_NTASKS_PER_NODE', unset = NA))
if(is.na(cores)) cores <- detectCores()
registerDoParallel(cores)

out_dir <- paste0("out_c_",i,"/")
if(!dir.exists(out_dir)) dir.create(out_dir)

m1 <- li23v3(U=U)

# basicParNames <- substring(names(mle),
#   first=1, last = nchar(names(mle))-1)
mle_expanded <- mle

# filter and save the results
registerDoRNG(43011)
stew(file =paste0(out_dir,"filters.rda"), {
  out <- foreach(s=1:20,.packages='spatPomp') %dopar% {
    bpfilter(m1,params=mle_expanded,
      Np=switch(i,10,2e3),block_size=1)
  }
  logliks <- sapply(out,logLik)
    # array of block conditional log likelihood estimates
  Nrep <- length(out)
  bc3 <- array(NA,c(Nrep,dim(out[[1]]@block.cond.loglik)))
  for(nrep in 1:Nrep) bc3[nrep,,] <- out[[nrep]]@block.cond.loglik

  bc3sd <- apply(bc3,c(2,3),sd)
  bc3m <- apply(bc3,c(2,3),mean)
  rownames(bc3sd) <- m1@unit_names
  rownames(bc3m) <- m1@unit_names  
  rm(out)
}) -> out



sort(logliks,decreasing=T)

attributes(out)$system.time

stew(file=paste0(out_dir,"ar.rda"),{

  dat <- obs(m1)
  rownames(dat) <- unit_names(m1)

  # Jan 10-23 is observation 1-14
  #  x1 <- incidence[1:14,2:(U+1)]
  x1 <- t(dat[1:U,1:14])


  # Jan 10- Feb 3 is observation 1-25
  #  x2 <- incidence[1:25,2:(U+1)]
  x2 <- t(dat[1:U,1:25])

  # Jan 10- Feb 8 is observation 1-30
  #  x3 <- incidence[1:30,2:(U+1)]
  x3 <- t(dat[1:U,1:30])


  ar_negloglik <- function(theta,x) {
    z <- 0
    for(u in 1:ncol(x)) z <- z - sum(dnbinom(x[,u],
      size=exp(theta["log.size"]),
      mu=exp(theta[u])+exp(theta["log.phi"])*c(0,x[1:(nrow(x)-1),u]),log=T))
    return(z)
  }  

  ar_theta1=c(log.mu=log(0.5*colMeans(x1)+0.01),log.size=log(2),log.phi=log(0.8))
  ar_mle1 <- optim(ar_theta1,ar_negloglik,x=x1)

  ar_theta2=c(log.mu=log(0.5*colMeans(x2)+0.01),log.size=log(2),log.phi=log(0.8))
  ar_mle2 <- optim(ar_theta2,ar_negloglik,x=x2)

  ar_theta3=c(log.mu=log(0.5*colMeans(x3)+0.01),log.size=log(2),log.phi=log(0.8))
  ar_mle3 <- optim(ar_theta3,ar_negloglik,x=x3)


  ar_cond_loglik <- function(theta,x) {
    resid <- x
    for(u in 1:ncol(x)) {
      resid[,u] <- dnbinom(x[,u],
        size=exp(theta["log.size"]),
        mu=exp(theta[u])+exp(theta["log.phi"])*c(0,x[1:(nrow(x)-1),u]),log=T)
    }
    resid
  }  

  arc1 <- ar_cond_loglik(ar_mle1$par,x1)

  sum(arc1)
  ar_mle1$value

  arc2 <- ar_cond_loglik(ar_mle2$par,x2)
  arc3 <- ar_cond_loglik(ar_mle3$par,x3)

})

