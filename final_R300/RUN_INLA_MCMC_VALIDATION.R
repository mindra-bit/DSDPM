source('DSDPM_ALL_IN_ONE_v3_4.R')
OUT<-dsdpm_safe_dir('outputs/inla_mcmc_validation')
R_VALID<-as.integer(Sys.getenv('DSDPM_MCMC_VALIDATION_R','100'))
if(is.na(R_VALID)||R_VALID<1) stop('Invalid DSDPM_MCMC_VALIDATION_R')
scenarios<-dsdpm_validation_scenarios()
prior<-dsdpm_default_prior_spec()
inla_args<-list(prior=prior,int_strategy='ccd',num_threads='1:1',transform_points=8192L)
mcmc_args<-list(prior=prior,n_chains=4L,n_iter=30000L,burn=10000L,thin=2L,rhat_limit=1.01,ess_min=400)
config<-list(signature=dsdpm_validation_signature(),scenarios=scenarios,R=R_VALID,inla_args=inla_args,mcmc_args=mcmc_args,base_seed=15000L)
hash<-dsdpm_config_hash(config)
hf<-file.path(OUT,'CONFIG_HASH.txt')
if(file.exists(hf)&&!identical(readLines(hf),hash)) stop('Validation configuration changed; use a fresh output directory')
writeLines(hash,hf);saveRDS(config,file.path(OUT,'configuration.rds'))
jobs<-dsdpm_safe_dir(file.path(OUT,'jobs'));rows<-list();k<-1L
for(i in seq_len(nrow(scenarios))) for(r in seq_len(R_VALID)) {
 sc<-scenarios[i,];fn<-file.path(jobs,sprintf('scenario_%02d_rep_%04d.rds',i,r))
 if(file.exists(fn)) {
  old<-readRDS(fn);if(!identical(old$hash,hash)) stop('Stale validation checkpoint');ans<-old$data
 } else {
  seed<-15000L+10000L*i+r
  sim<-dsdpm_simulate(sc$N,sc$Tt,dsdpm_make_queen_W(sc$N)$W,dsdpm_scenario_to_par(sc),seed=seed,keep_innovations=FALSE)
  cat('Bayesian validation',i,'rep',r,'of',R_VALID,'\n')
  ti<-proc.time()[3];fi<-try(do.call(fit_dsdpm_inla_full,c(list(sim=sim),inla_args)),silent=TRUE);ti<-proc.time()[3]-ti
  tm<-proc.time()[3];fm<-try(do.call(fit_dsdpm_mcmc_multichain_full,c(list(sim=sim,seeds=seed+700000L+1:4),mcmc_args)),silent=TRUE);tm<-proc.time()[3]-tm
  ans<-cbind(scenario=i,design=sc$design,replication=r,dsdpm_bayesian_comparison(sim,fi,fm,ti,tm))
  saveRDS(list(hash=hash,data=ans),fn)
 }
 rows[[k]]<-ans;k<-k+1L
 write.csv(do.call(rbind,rows),file.path(OUT,'matched_replications.csv'),row.names=FALSE)
}
raw<-do.call(rbind,rows)
sp<-split(raw,interaction(raw$scenario,raw$parameter,drop=TRUE))
summary<-do.call(rbind,lapply(sp,function(d) {
 valid<-d$reference_valid & d$INLA_strict
 v<-d[valid,];avg<-function(x) if(length(x)) mean(x,na.rm=TRUE) else NA_real_
 data.frame(scenario=d$scenario[1],design=d$design[1],N=d$N[1],Tt=d$Tt[1],parameter=d$parameter[1],
   n_attempted=nrow(d),n_valid=nrow(v),reference_success=mean(d$reference_valid),
   mean_abs_difference=avg(v$absolute_mean_difference),mean_sd_ratio=avg(v$posterior_sd_ratio),
   INLA_coverage=avg(v$INLA_coverage),MCMC_coverage=avg(v$MCMC_coverage),
   agreement_rate=avg(v$agreement_screen),INLA_seconds=avg(d$INLA_seconds),MCMC_seconds=avg(d$MCMC_seconds))
}))
write.csv(summary,file.path(OUT,'summary.csv'),row.names=FALSE)
dsdpm_session_info(file.path(OUT,'sessionInfo.txt'))
cat('Validation complete. Review reference convergence and approximation differences before making accuracy claims.\n')