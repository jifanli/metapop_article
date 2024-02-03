library(tidyverse)
library(metapoppkg)
library(ggplot2)
library(ggnewscale)
library(doParallel)
library(doRNG)

cores <-  as.numeric(Sys.getenv('SLURM_NTASKS_PER_NODE', unset = NA))
if(is.na(cores)) cores <- detectCores()
registerDoParallel(cores)

  # copied from bagged_filters
li23_model_dir <- "li23Model/"
if(!dir.exists(li23_model_dir)) dir.create(li23_model_dir)

li23_spatPomp <- stew(file=paste0(li23_model_dir,"li23_spatPomp6.rda"),{
  out <- readRDS("profile_theta3.rds")
  li23_params <- unlist(out[which.max(out$logLik),-c(1,2)])
  names(li23_params)[which(names(li23_params)=="Iu_01")] <- "A_01"
  li23_params <- li23_params[names(li23_params)%in%names(coef(li23(U = 2, for_ibpf = F)))] # from for_ibpf=T to for_ibpf=F

  li23_subset <- function(m_U,m_N){
    m <- li23(U = m_U, for_ibpf = F)
    li23_sim <- simulate(m,params=li23_params,
                         times=time(m)[1:m_N])
    return(li23_sim)
  }
})

run_level=2
files_dir <- paste0("run_level_",run_level,"/")
if(!dir.exists(files_dir)) dir.create(files_dir)

stew(file=paste0(files_dir,"settings6.rda"),{
  
  # copy variables that should be included in the stew
  run_level <- run_level 
  cores <- cores
  
  tol <- 1e-300
  
  if(run_level==1){
    U <- c(4, 2)
    N <- 30
    replicates <- 2 # number of Monte Carlo replicates
    girf_Np <- 2
    girf_lookahead <- 1
    girf_nguide <- 2
    girf_Ninter <- 2
    pfilter_Np <- 10
    abf_Nrep <- 3
    abf_Np_per_replicate <- 10
    ubf_Nrep <- 20
    abfir_Nrep <- 2
    abfir_Np_per_replicate <- 2
    abfir_Ninter <- 2
    enkf_Np <- 10
    bpf_units_per_block <- 1
    bpf_Np <- 5
    # bootgirf_Np <- 5
    # bootgirf_nguide <- 10
    # bootgirf_lookahead <- 2
  } else if(run_level==2){
    replicates <- 5 # number of Monte Carlo replicates
    U <- c(373, 256, 128, 64, 32, 16, 8, 4, 2)
    N <- 30
    girf_Np <- 100
    girf_lookahead <- 1
    girf_nguide <- 50
    girf_Ninter <- 5
    pfilter_Np <- 4000
    abf_Nrep <- 500
    abf_Np_per_replicate <- 75
    ubf_Nrep <- 18000
    abfir_Nrep <- 10
    abfir_Np_per_replicate <- 5
    abfir_Ninter <- 5
    enkf_Np <- 4000
    bpf_units_per_block <- 1
    bpf_Np <- 3000
    # bootgirf_Np <- 2000
    # bootgirf_nguide <- 20
    # bootgirf_lookahead <- 2
  } 
})

jobs <- expand.grid(U=U,reps=1:replicates)
jobs$U_id <- rep(seq_along(U),times=replicates)


spatPomp <- stew(file=paste0(files_dir,"spatPomp6.rda"),{
  load(file=paste0(li23_model_dir,"li23_spatPomp6.rda"))  
  li23_list <- vector("list", length = length(U))  
  set.seed(324739)
  for(i in 1:length(U)) li23_list[[i]] <- li23_subset(m_U=U[i], m_N=N)
})

nbhd <- function(object, time, unit) {
  nbhd_list <- list()
  if(time>1) nbhd_list <- c(nbhd_list, list(c(unit, time-1)))
  if(time>2) nbhd_list <- c(nbhd_list, list(c(unit, time-2)))
  return(nbhd_list)
}

girf <- stew(file=paste0(files_dir,"girf6.rda"),seed=5981724,{
  foreach(job=iter(jobs,"row")) %dopar% {
    system.time(
      girf(li23_list[[job$U_id]],
           kind="moment",
           Np=girf_Np,
           Ninter = girf_Ninter,
           lookahead = girf_lookahead,
           Nguide = girf_nguide,
           tol = tol) -> girf_out
      ) -> girf_time
      #    uncomment for debugging
      #    list(logLik=logLik(girf_out),time=girf_time["elapsed"],girf_out)
    list(logLik=logLik(girf_out),time=girf_time["elapsed"])
  } -> girf_list
})
jobs$girf_logLik <- vapply(girf_list,function(x)x$logLik,numeric(1))
jobs$girf_time <- vapply(girf_list,function(x) x$time,numeric(1))

abf <- stew(file=paste0(files_dir,"abf6.rda"),seed=844424,{
  foreach(job=iter(jobs,"row")) %do% {
    system.time(
      abf(li23_list[[job$U_id]],
          Nrep = abf_Nrep,
          Np=abf_Np_per_replicate,
          nbhd = nbhd, tol=tol) -> abf_out
    ) -> abf_time
      #    uncomment for debugging
      #    list(logLik=logLik(abf_out),time=abf_time["elapsed"],abf_out)
    list(logLik=logLik(abf_out),time=abf_time["elapsed"])
  } -> abf_list
})
jobs$abf_logLik <- vapply(abf_list,function(x)x$logLik,numeric(1))
jobs$abf_time <- vapply(abf_list,function(x) x$time,numeric(1))


ubf <- stew(file=paste0(files_dir,"ubf6.rda"),seed=844424,{
  foreach(job=iter(jobs,"row")) %do% {
    system.time(
      abf(li23_list[[job$U_id]], 
          Nrep = ubf_Nrep,
          Np=1,
          nbhd = nbhd, tol=tol) -> ubf_out 
    ) -> ubf_time
      #    uncomment for debugging 
      #    list(logLik=logLik(ubf_out),time=ubf_time["elapsed"],ubf_out)
    list(logLik=logLik(ubf_out),time=ubf_time["elapsed"])
  } -> ubf_list
})
jobs$ubf_logLik <- vapply(ubf_list,function(x)x$logLik,numeric(1))
jobs$ubf_time <- vapply(ubf_list,function(x) x$time,numeric(1))


abfir <- stew(file=paste0(files_dir,"abfir6.rda"),seed=53398,{
  foreach(job=iter(jobs,"row")) %do% {
    system.time(
      abfir(li23_list[[job$U_id]],
            Nrep = as.integer(abfir_Nrep),
            Np=abfir_Np_per_replicate,
            Ninter = abfir_Ninter,
            nbhd = nbhd, tol=tol) -> abfir_out
    ) -> abfir_time
      #    uncomment for debugging
      #    list(logLik=logLik(abfir_out),time=abfir_time["elapsed"],abfir_out)
    list(logLik=logLik(abfir_out),time=abfir_time["elapsed"])
  } -> abfir_list
})
jobs$abfir_logLik <- vapply(abfir_list,function(x)x$logLik,numeric(1))
jobs$abfir_time <- vapply(abfir_list,function(x) x$time,numeric(1))


  
pfilter <- stew(file=paste0(files_dir,"pfilter6.rda"),seed=53285,{
  foreach(job=iter(jobs,"row")) %dopar% {
    system.time(
      pfilter(li23_list[[job$U_id]],Np=pfilter_Np) -> pfilter_out
    ) -> pfilter_time
      #    uncomment for debugging 
      #    list(logLik=logLik(pfilter_out),time=pfilter_time["elapsed"],pfilter_out)
    list(logLik=logLik(pfilter_out),time=pfilter_time["elapsed"])
  } -> pfilter_list
})
jobs$pfilter_logLik <- vapply(pfilter_list,function(x)x$logLik,numeric(1))
jobs$pfilter_time <- vapply(pfilter_list,function(x) x$time,numeric(1))

  
bpf <- stew(file=paste0(files_dir,"bpf6.rda"),seed=53285,{
  foreach(job=iter(jobs,"row")) %dopar% {
    system.time(
      bpfilter(li23_list[[job$U_id]],
               Np=bpf_Np,
               block_size=bpf_units_per_block) -> bpf_out
    ) -> bpf_time
      #    uncomment for debugging 
      #    list(logLik=logLik(bpf_out),time=bpf_time["elapsed"],bpf_out)
    list(logLik=bpf_out@loglik,time=bpf_time["elapsed"])
  } -> bpf_list
})
jobs$bpf_logLik <- vapply(bpf_list,function(x)x$logLik,numeric(1))
jobs$bpf_time <- vapply(bpf_list,function(x) x$time,numeric(1))

  
  ###  genkf(list[[1]],Np=enkf_Np) -> enkf_out
enkf <- stew(file=paste0(files_dir,"enkf6.rda"),seed=53285,{
  foreach(job=iter(jobs,"row")) %dopar% {
    system.time(
      enkf(li23_list[[job$U_id]],
               Np=enkf_Np) -> enkf_out
    ) -> enkf_time
    #    uncomment for debugging 
    #    list(logLik=logLik(enkf_out),time=enkf_time["elapsed"],enkf_out)
    list(logLik=enkf_out@loglik,time=enkf_time["elapsed"])
  } -> enkf_list
})

load(file="run_level_2/enkf5.rda")

jobs$enkf_logLik <- vapply(enkf_list,function(x)x$logLik,numeric(1))
jobs$enkf_time <- vapply(enkf_list,function(x) x$time,numeric(1))


  # put output in tall format for plotting
  
# methods <- c("ABF", "UBF", "PF","BPF","EnKF")
methods <- c("ABF", "ABF-IR","UBF","GIRF","PF","BPF","EnKF")
# methods <- c("ABF", "ABF-IR","UBF","GIRF","PF","BPF","EnKF","bootgirf")
results <- data.frame(
  Method=rep(methods,each=nrow(jobs)),
  logLik=c(
    jobs$abf_logLik,
    jobs$abfir_logLik,
    jobs$ubf_logLik,
    jobs$girf_logLik,
    jobs$pfilter_logLik,
    jobs$bpf_logLik,
    jobs$enkf_logLik
    #    jobs$bootgirf_logLik
  ),
  Units=rep(jobs$U,reps=length(methods))
)
point_perturbation <- 0.25
results$U <- results$Units+
  rep( point_perturbation*seq(from=-1,to=1,length=length(methods)),
       each=nrow(jobs))
results$logLik_per_unit <- results$logLik/results$Units
results$logLik_per_obs <- results$logLik_per_unit/N

save(file=paste0(files_dir,"results6.rda"),results,jobs)

max <- max(results$logLik_per_obs)

linetype <- c(1,1,1,1,2,2,2,3)
shape <- c(1,2,4,5,1,2,4,3)
#color <- cbPalette[c(2:8,1)]
cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# color <- cbPalette[1:5]
color <- cbPalette[2:8]
# color <- c(1,2,3,4,5,6,8)

filter_tests <- ggplot(results,mapping = aes(x = U, y = logLik_per_obs, group=Method,color=Method,linetype=Method,shape=Method)) +
  scale_linetype_manual(values=linetype) +
  scale_shape_manual(values=shape) +
  scale_color_manual(values=color)+
  geom_point() +
  stat_summary(fun=mean, geom="line") +
  coord_cartesian(ylim=c(max-4,max))+
  theme(legend.key.width = unit(1,"cm"))+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=0.8),
        axis.line = element_line(colour = "black"))+
  ylab("log likelihood per unit per time")

ggsave("filter_tests6.png", plot=filter_tests, width=10, height=5, dpi=300)

  
  
