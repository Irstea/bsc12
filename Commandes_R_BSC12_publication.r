### code ran under R3.1 to R3.4. 



library(gdata)

library(coda)
#library(rjags)
#library(R2WinBUGS)
library(lme4)
library(AICcmodavg)
library(Hmisc)
if (floor(as.double(version$minor))<=13&floor(as.double(version$major))<=2) {library(Design)} else { library(rms)    }
library(rstan)
library(nlme)
library(VGAM)
library(rgeos)
library(rworldmap)
library(loo)
library(abind)


#### variables that will countain the paths to (i) where to save csv files; and (ii) where to put RData files. These can be the same.
wd.RData<-"RData"
wd.csv<-"D:\\Dossier Frederic\\Articles\\BSC_Callois\\DATA_ANALYSIS"


#### code to load the RData that countains functions and data
if (Sys.info()["sysname"]=="Windows") {setwd(wd.RData)} 
load("BSC_Data_functions.RData")



########################### MAIN FUNCTIONS ###########################
############# other functions may be prsent in BSC_Data_functions.RData
{

#modif: parsdinner[j] ~ uniform(-10.0,10.0): pour éviter valeurs trop fortes
stanmodel.heteroscbetabinom.priorsdlowrestricted <- "
	data
		{
			#we will take a zero-inflated beta distribution inspired from Herpigjny & Gosselin (2015): MTUnlimited
			int <lower=0> Ndelta;
				#number of fixed parameters to estimate in the formula for delta.
			int <lower=0> Neta;
				#number of fixed parameters to estimate in the formula for eta
			int <lower=0> NCOU;
				#number of countries
			#int <lower=0> NPtype;
				#number of protection types
			int <lower=0> Nsdinner;
				#number of protection types
			int <lower=0> Ncases;
				#number of observations
			
			int COU[Ncases];
				#will include the integer number identifying the country
			#int Ptype[Ncases];
				#will include the integer number identifying the Protection type - can vary..
	 
			matrix[Ncases,Ndelta] matrixdelta;
			matrix[Ncases,Neta] matrixeta;
			matrix[Ncases,Nsdinner] matrixsdinner;
			int Y[Ncases] ;
				#will include the variable to be explained: actually the ratio of areas. Can be 0 but not 1. Must be <1
			int N[Ncases] ;
			
			
		}
	parameters
		{	#real pardelta;
			real pareta[Neta];
			real parsdinner[Nsdinner];
			real easCOU[NCOU];
			#real easinner[Ncases];
			#real easPtype[NPtype];
			real sdCOU;
			#real sdPtype;
			#real sdinner ;
			
		}



	transformed parameters 
		{#je les mets en transformed parameters pour pouvoir les appeler aussi bien dans modle que generated parameters
		vector[Ncases] delta ;	
		vector[Ncases] eta ;
		vector[Ncases] mutot;
		vector[Ncases] alpha;
		vector[Ncases] beta;
		vector[Ncases] P0;
		vector[Ncases] sdinner;
	         
				  for (n in 1:Ncases)
				{
					eta[n] <- easCOU[COU[n]];
					for (j in 1:Neta)
						{eta[n]<-eta[n]+pareta[j]*matrixeta[n][j];}
						## +easPtype[Ptype[n]]
					sdinner[n] <- 0.0;
					for (j in 1:Nsdinner)
						{sdinner[n]<-sdinner[n]+parsdinner[j]*matrixsdinner[n][j];}
					sdinner[n]<-exp(sdinner[n]);
					
					mutot[n]<-(1.0/(1.0+exp(-eta[n])));
					alpha[n]<-sdinner[n]*mutot[n];
					beta[n]<-sdinner[n]*(1.0-mutot[n]);
					
					

					}
        }
		
		
		
	model	{


##int n;
##int j;

		

	  
#### Calculs vraisemblance
##int n;
	
	## Priors
			 for (j in 1:Neta)
			 {pareta[j] ~ normal(0,5.0) ;}
			  for (j in 1:Nsdinner)
			 {parsdinner[j] ~ uniform(-10.0,10.0) ;}
			
		sdCOU ~ uniform(0.0,100.0);
  		#sdPtype ~ uniform(0.0,100.0);
		#sdinner ~ uniform(0.0,100.0);
		
		
		
 
	## Random effects
		easCOU ~ normal(0.0,sdCOU); 
		
		#easPtype ~ normal(0.0,sdPtype);
  
### likelihood
for (n in 1:Ncases)
{
	
	
	#Y[n]~beta_binomial(N[n],alpha[n],beta[n]);
	increment_log_prob(beta_binomial_log(Y[n],N[n],alpha[n],beta[n]));

	}

	
	
}
generated quantities	{

vector[Ncases] log_lik ;

for (n in 1:Ncases)
{
	log_lik[n]<-beta_binomial_log(Y[n],N[n],alpha[n],beta[n]);

}	
}
"



### same model as MUNLIMITED in Herpigny, B. and F. Gosselin, 2015. Analyzing plant cover class data quantitatively: customized cumulative zero-inflated beta distributions show promising results. Ecological Informatics, 26(3), 18-26.
### except that no classes

stanmodel.delta1 <- "
	data
		{
			#we will take a zero-inflated beta distribution inspired from Herpigjny & Gosselin (2015): MTUnlimited
			int <lower=0> Ndelta;
				#number of fixed parameters to estimate in the formula for delta.
			int <lower=0> Neta;
				#number of fixed parameters to estimate in the formula for eta
			int <lower=0> NCOU;
				#number of countries
			#int <lower=0> NPtype;
				#number of protection types
			int <lower=0> Nsdinner;
				#number of protection types
			int <lower=0> Ncases;
				#number of observations
			
			int COU[Ncases];
				#will include the integer number identifying the country
			#int Ptype[Ncases];
				#will include the integer number identifying the Protection type - can vary..
	 
			matrix[Ncases,Ndelta] matrixdelta;
			matrix[Ncases,Neta] matrixeta;
			matrix[Ncases,Nsdinner] matrixsdinner;
			real Y[Ncases] ;
				#will include the variable to be explained: actually the ratio of areas. Can be 0 but not 1. Must be <1
			
			
		}
	parameters
		{	real pardelta;
			real pareta[Neta];
			real parsdinner[Nsdinner];
			real easCOU[NCOU];
			#real easPtype[NPtype];
			real sdCOU;
			#real sdPtype;
			#real sdinner ;
			
		}



	transformed parameters 
		{#je les mets en transformed parameters pour pouvoir les appeler aussi bien dans modle que generated parameters
		vector[Ncases] delta ;	
		vector[Ncases] eta ;
		vector[Ncases] mutot;
		vector[Ncases] alpha;
		vector[Ncases] beta;
		vector[Ncases] P0;
		vector[Ncases] sdinner;
	         
				  for (n in 1:Ncases)
				{
					eta[n] <- easCOU[COU[n]];
					for (j in 1:Neta)
						{eta[n]<-eta[n]+pareta[j]*matrixeta[n][j];}
						## +easPtype[Ptype[n]]
					delta[n] <- 0.0;
					for (j in 1:Ndelta)
						{delta[n]<-delta[n]+pardelta*matrixdelta[n][j];}
					sdinner[n] <- 0.0;
					for (j in 1:Nsdinner)
						{sdinner[n]<-sdinner[n]+parsdinner[j]*matrixsdinner[n][j];}
					sdinner[n]<-exp(sdinner[n]);
					P0[n]<-1.0-(1.0/(1.0+exp(-exp(delta[n])-eta[n])));
					if (Y[n]!=0.0)
					{
					mutot[n]<-(1.0/(1.0+exp(-eta[n])));
					alpha[n]<-sdinner[n]*mutot[n]/(1-P0[n]);
					beta[n]<-sdinner[n]*(1.0-mutot[n]/(1-P0[n]));
					}

					}
        }
		
		
		
	model	{


##int n;
##int j;

		

	  
#### Calculs vraisemblance
##int n;
	
	## Priors
			 for (j in 1:Neta)
			 {pareta[j] ~ normal(0,5.0) ;}
			 for (j in 1:Ndelta)
			 {pardelta ~ normal(0,5.0) ;}
			  for (j in 1:Nsdinner)
			 {parsdinner[j] ~ normal(0,5.0) ;}
			
		sdCOU ~ uniform(0.0,100.0);
  		#sdPtype ~ uniform(0.0,100.0);
		#sdinner ~ uniform(0.0,100.0);
		
		
		
 
	## Random effects
		easCOU ~ normal(0.0,sdCOU); 
		#easPtype ~ normal(0.0,sdPtype);
  
### likelihood
for (n in 1:Ncases)
{
	
	if (Y[n]==0.0)
	{increment_log_prob(log(P0[n])); }
	else
	{
	
	increment_log_prob(log(1.0-P0[n]));
	Y[n]~beta(alpha[n],beta[n]);}

	}

	
	
}
generated quantities	{

vector[Ncases] log_lik ;

for (n in 1:Ncases)
{if (Y[n]==0.0)
	{log_lik[n]<-(log(P0[n])); }
	
	else
	{
	log_lik[n]<-(log(1.0-P0[n]))+beta_log(	Y[n],alpha[n],beta[n]);}

}	
}
"





stanmodel.delta1.nsdi1.woCOU <- "
	data
		{
			#we will take a zero-inflated beta distribution inspired from Herpigjny & Gosselin (2015): MTUnlimited
			int <lower=0> Ndelta;
				#number of fixed parameters to estimate in the formula for delta.
			int <lower=0> Neta;
				#number of fixed parameters to estimate in the formula for eta
			int <lower=0> NCOU;
				#number of countries
			#int <lower=0> NPtype;
				#number of protection types
			int <lower=0> Nsdinner;
				#number of protection types
			int <lower=0> Ncases;
				#number of observations
			
			int COU[Ncases];
				#will include the integer number identifying the country
			#int Ptype[Ncases];
				#will include the integer number identifying the Protection type - can vary..
	 
			matrix[Ncases,Ndelta] matrixdelta;
			matrix[Ncases,Neta] matrixeta;
			matrix[Ncases,Nsdinner] matrixsdinner;
			real Y[Ncases] ;
				#will include the variable to be explained: actually the ratio of areas. Can be 0 but not 1. Must be <1
			
			
		}
	parameters
		{	real pardelta;
			real pareta[Neta];
			real parsdinner;
			#real easCOU[NCOU];
			#real easPtype[NPtype];
			#real sdCOU;
			#real sdPtype;
			#real sdinner ;
			
		}



	transformed parameters 
		{#je les mets en transformed parameters pour pouvoir les appeler aussi bien dans modle que generated parameters
		vector[Ncases] delta ;	
		vector[Ncases] eta ;
		vector[Ncases] mutot;
		vector[Ncases] alpha;
		vector[Ncases] beta;
		vector[Ncases] P0;
		vector[Ncases] sdinner;
	         
				  for (n in 1:Ncases)
				{
					eta[n] <- 0.0;
					for (j in 1:Neta)
						{eta[n]<-eta[n]+pareta[j]*matrixeta[n][j];}
						## +easPtype[Ptype[n]]
					delta[n] <- 0.0;
					for (j in 1:Ndelta)
						{delta[n]<-delta[n]+pardelta*matrixdelta[n][j];}
					sdinner[n] <- 0.0;
					for (j in 1:Nsdinner)
						{sdinner[n]<-sdinner[n]+parsdinner*matrixsdinner[n][j];}
					sdinner[n]<-exp(sdinner[n]);
					P0[n]<-1.0-(1.0/(1.0+exp(-exp(delta[n])-eta[n])));
					if (Y[n]!=0.0)
					{
					mutot[n]<-(1.0/(1.0+exp(-eta[n])));
					alpha[n]<-sdinner[n]*mutot[n]/(1-P0[n]);
					beta[n]<-sdinner[n]*(1.0-mutot[n]/(1-P0[n]));
					}

					}
        }
		
		
		
	model	{


##int n;
##int j;

		

	  
#### Calculs vraisemblance
##int n;
	
	## Priors
			 for (j in 1:Neta)
			 {pareta[j] ~ normal(0,5.0) ;}
			 for (j in 1:Ndelta)
			 {pardelta ~ normal(0,5.0) ;}
			  for (j in 1:Nsdinner)
			 {parsdinner ~ normal(0,5.0) ;}
			
		#sdCOU ~ uniform(0.0,100.0);
  		#sdPtype ~ uniform(0.0,100.0);
		#sdinner ~ uniform(0.0,100.0);
		
		
		
 
	## Random effects
		#easCOU ~ normal(0.0,sdCOU); 
		#easPtype ~ normal(0.0,sdPtype);
  
### likelihood
for (n in 1:Ncases)
{
	
	if (Y[n]==0.0)
	{increment_log_prob(log(P0[n])); }
	else
	{
	
	increment_log_prob(log(1.0-P0[n]));
	Y[n]~beta(alpha[n],beta[n]);}

	}

	
	
}
generated quantities	{

vector[Ncases] log_lik ;

for (n in 1:Ncases)
{if (Y[n]==0.0)
	{log_lik[n]<-(log(P0[n])); }
	
	else
	{
	log_lik[n]<-(log(1.0-P0[n]))+beta_log(	Y[n],alpha[n],beta[n]);}

}	
}
"





#changement pour sdinner,pareta
inits.betabinomquad.extinct <- function()
{
pareta<-c(rnorm(min(win.data$Neta,10),-4,0.3),rnorm(max(win.data$Neta-10,0),0,0.3))
pardelta<-(rnorm(win.data$Ndelta,0,0.3))
parsdinner<-(rnorm(win.data$Nsdinner,5,0.3))
sdCOU<-runif(1,0.1,0.3)
easCOU<-(rnorm(win.data$NCOU,0,sdCOU))
easinner<-(rnorm(win.data$Ncases,0,0.3))



		list (pareta=as.vector(pareta),pardelta=as.vector(pardelta),parsdinner=as.vector(parsdinner),easCOU=easCOU,sdCOU=sdCOU)
}	



#changement pour sdinner,pareta
inits.betabinomter.threat <- function()
{
pareta<-c(rnorm(min(win.data$Neta,10),-1,0.3),rnorm(max(win.data$Neta-10,0),0,0.3))
pardelta<-(rnorm(win.data$Ndelta,0,0.3))
parsdinner<-(rnorm(win.data$Nsdinner,3,0.3))
sdCOU<-runif(1,0.1,0.3)
easCOU<-(rnorm(win.data$NCOU,0,sdCOU))
easinner<-(rnorm(win.data$Ncases,0,0.3))



		list (pareta=as.vector(pareta),pardelta=as.vector(pardelta),parsdinner=as.vector(parsdinner),easCOU=easCOU,sdCOU=sdCOU)
}	



initsPAbis <- function()
{
pareta<-c(rnorm(min(win.data$Neta,4),-4,0.3),rnorm(max(win.data$Neta-4,0),0,0.3))
pardelta<-(rnorm(win.data$Ndelta,2,0.3))
parsdinner<-(rnorm(win.data$Nsdinner,3,0.3))

sdCOU<-runif(1,0.1,0.3)
easCOU<-(rnorm(win.data$NCOU,0,sdCOU))


		list (pareta=as.vector(pareta),pardelta=as.vector(pardelta),parsdinner=as.vector(parsdinner),easCOU=easCOU,sdCOU=sdCOU)
}		





initsSEALsixt <- function()
{
pareta<-c(rnorm(1,-3,0.3),rnorm(win.data$Neta-1,0,0.3))
pardelta<-(rnorm(win.data$Ndelta,5,0.5))
parsdinner<-(rnorm(win.data$Nsdinner,0,0.5))




		list (pareta=as.vector(pareta),pardelta=as.vector(pardelta),parsdinner=as.vector(parsdinner))
}	


DICmarginal.stanmodel.heteroscbetabinom<-function(selected.model,rep.inner=100,reps=60,reps.mean=1000,win.data=win.data,seed=1,Nestmax=10000){
#Nestmax: max number of extimators taken into account
#pour test: selected.model<-modExtinct_betabinomheteroscpriorsdlowrest_EA_bis
#je vais prendre rep.inner pour répliquer la partie marginalisation
#choix ici de prendre les mêmes tirages au sort pour tout le monde (toutes les valeurs des params, yc mean)
set.seed(seed)

sdCOU<-extract(selected.model,"sdCOU")[[1]]
 if (length(sdCOU)<=Nestmax) {samples<-1:length(sdCOU)} else {samples<-sample(1:length(sdCOU),Nestmax,replace=FALSE)}
sdCOU<-extract(selected.model,"sdCOU")[[1]][samples]

 easCOU<-extract(selected.model,"easCOU")[[1]][samples,]

pareta<-extract(selected.model,"pareta")[[1]][samples,]

parsdinner<-extract(selected.model,"parsdinner")[[1]][samples,]


###

#print(dim(pareta))
#print(dim(win.data$matrixeta))
sdinner<-t(parsdinner%*%t(win.data$matrixsdinner))
eta<-t(pareta%*%t(win.data$matrixeta))
#+t(easCOU[,win.data$COU])
mutot<-1/(1+exp(-eta))


alpha<-exp(sdinner)*mutot
beta<-exp(sdinner)*(1-mutot)

set.seed(seed)
### wrong: deviances<--2*((dbeta(win.data$Y,alpha,beta,log=T)+log(1-P0))*(win.data$Y>0)+(win.data$Y==0)*log(P0))
temp<-list.as.array(tapply(1:(length(win.data$Y)),win.data$COU,function(x){tempb<-list.as.array(lapply(x,function(x){sapply(1:rep.inner,function(y,x){set.seed(seed+y);
																									eascou<-rnorm(win.data$NCOU,0,1);#print(x);
																									eta<-eta[x,]+(eascou[win.data$COU[x]])*t(sdCOU);
																									mutot<-1/(1+exp(-eta))


																									alpha<-exp(sdinner[x,])*mutot
																									beta<-exp(sdinner[x,])*(1-mutot)
																								dbetabinom.ab(win.data$Y[x],win.data$N[x],alpha,beta,log=T)},
																								x,simplify=T)}))
print(dim(tempb))
															apply(exp(apply(tempb,c(1,2),sum)),1,mean)
															
															},
													simplify=F))
print(dim(temp))
#############dimensions of temp: MCMCreplicates*rep.inner*Nobs	
#print(dim(list.as.array(tapply(1:dim(temp)[3],win.data$COU,function(x){toto<-apply(temp[,,x,drop=FALSE],c(1,2),sum);print(dim(toto));toto}))))												
#temp<-apply(exp(list.as.array(tapply(1:dim(temp)[3],win.data$COU,function(x){apply(temp[,,x,drop=FALSE],c(1,2),sum)}))),c(1,3),mean)

print(dim(temp))
deviances<-apply(-2*log(temp),1,sum)
temp_loglik<-log(temp)

###
#print(deviances)

sdinnermean<-apply(t(parsdinner%*%t(win.data$matrixsdinner)),1,mean)
etamean<-apply(t(pareta%*%t(win.data$matrixeta)),1,mean)
sdCOUmean<-mean(sdCOU)
#+t(easCOU[,win.data$COU]),1,mean)
mutotmean<-1/(1+exp(-etamean))
alphamean<-exp(sdinnermean)*mutotmean
betamean<-exp(sdinnermean)*(1-mutotmean)
####deviances.thetamean<--2*((win.data$Y>0)*(dbeta(win.data$Y,alphamean,betamean,log=T)+log(1-P0mean))+(win.data$Y==0)*log(P0mean))
### wrong: deviances<--2*((dbeta(win.data$Y,alpha,beta,log=T)+log(1-P0))*(win.data$Y>0)+(win.data$Y==0)*log(P0))


set.seed(seed)
tempmean<-list.as.array(sapply(1:(length(win.data$Y)),function(x){tempb<-sapply(1:rep.inner,function(y,x){set.seed(seed+y);eascou<-rnorm(win.data$NCOU,0,1);

					eta<-etamean[x]+(eascou[win.data$COU[x]])*(sdCOUmean);
					mutot<-1/(1+exp(-eta))


alpha<-exp(sdinnermean[x])*mutot
beta<-exp(sdinnermean[x])*(1-mutot)
dbetabinom.ab(win.data$Y[x],win.data$N[x],alpha,beta,log=F)},x,simplify=T)
mean(tempb)},simplify=F))


deviance.thetamean<-sum(-2*log(tempmean))

#print(deviance.thetamean)

deviance.min<-min(deviances)

pDvar<-0.5*var(deviances)

#deviances<-(-2)*apply(as.matrix(model)[,grep("logProb",dimnames(as.matrix(model))[[2]])],1,sum)
		
		Dbar<-mean(deviances)
	#calculation of deviance for WAIC
		deviance.mean.prob<-(-2)*log(apply(exp(temp_loglik),2,mean))
		
		Dbar.WAIC<-sum(deviance.mean.prob)
	      pD1.WAIC=-(Dbar.WAIC-Dbar)
	     
log_lik<-temp_loglik
loo1<-loo(log_lik)

waic1<-get("waic",pos="package:loo")(log_lik)



c(DIC=2*mean(deviances)-deviance.thetamean,DICvar=mean(deviances)+pDvar,DICmin=2*mean(deviances)-deviance.min,IC=mean(deviances)+2*pDvar,pDvar=pDvar,pD1.WAIC=pD1.WAIC,looic=loo1$looic,waic=waic1$waic,propkfair=pareto_k_table(loo1)[2,2],propkbad=pareto_k_table(loo1)[3,2],propkverybad=pareto_k_table(loo1)[4,2])

}



DICmarginal.ZIbeta<-function(selected.model,rep.inner=100,reps=60,reps.mean=1000,win.data=win.data,seed=1,Nestmax=10000){
#selected.model<-"mod.myco.RStot.Gess.Alti.linear.fixe.SampleDUR.s1"
#je vais prendre rep.inner pour répliquer la partie marginalisation
#choix ici de prendre les mêmes tirages au sort pour tout le monde (toutes les valeurs des params, yc mean)


set.seed(seed)

sdCOU<-extract(selected.model,"sdCOU")[[1]]
 if (length(sdCOU)<=Nestmax) {samples<-1:length(sdCOU)} else {samples<-sample(1:length(sdCOU),Nestmax,replace=FALSE)}
sdCOU<-extract(selected.model,"sdCOU")[[1]][samples]

 easCOU<-extract(selected.model,"easCOU")[[1]][samples,]

parsdinner<-extract(selected.model,"parsdinner")[[1]][samples,]



pardelta<-extract(selected.model,"pardelta")[[1]][samples]

if (win.data$Neta>0)
{
pareta<-extract(selected.model,"pareta")[[1]][samples,]}
else {pareta<-0}



#print(dim(pareta))
#print(dim(win.data$matrixeta))
sdinner<-t(parsdinner%*%t(win.data$matrixsdinner))
if (win.data$Neta>0){eta<-t(pareta%*%t(win.data$matrixeta))} else {eta<-0}
#+t(easCOU[,win.data$COU])
delta<-t(pardelta%*%t(win.data$matrixdelta))
P0<-1-1/(1+exp(-exp(delta)-eta))
mutot<-1/(1+exp(-eta))
alpha<-exp(sdinner)*mutot/(1-P0)
beta<-exp(sdinner)*(1-mutot/(1-P0))

### wrong: deviances<--2*((dbeta(win.data$Y,alpha,beta,log=T)+log(1-P0))*(win.data$Y>0)+(win.data$Y==0)*log(P0))
set.seed(seed)
### wrong: deviances<--2*((dbeta(win.data$Y,alpha,beta,log=T)+log(1-P0))*(win.data$Y>0)+(win.data$Y==0)*log(P0))

### wrong: deviances<--2*((dbeta(win.data$Y,alpha,beta,log=T)+log(1-P0))*(win.data$Y>0)+(win.data$Y==0)*log(P0))
temp<-list.as.array(tapply(1:(length(win.data$Y)),win.data$COU,function(x){tempb<-list.as.array(lapply(x,function(x){sapply(1:rep.inner,function(y,x){set.seed(seed+y);
																									eascou<-rnorm(win.data$NCOU,0,1);#print(x);
																									eta<-eta[x,]+(eascou[win.data$COU[x]])*t(sdCOU);
																									mutot<-1/(1+exp(-eta))
																									P0<-1-1/(1+exp(-exp(delta[x,])-eta))

																									alpha<-exp(sdinner[x,])*mutot/(1-P0)
																									beta<-exp(sdinner[x,])*(1-mutot)/(1-P0)
																								if(win.data$Y[x]>0){log(dbeta(win.data$Y[x],alpha,beta,log=F)*(1-P0))} else {log(P0)}},
																								x,simplify=T)}))
print(dim(tempb))
															apply(exp(apply(tempb,c(1,2),sum)),1,mean)
															
															},
													simplify=F))
print(dim(temp))
#############dimensions of temp: MCMCreplicates*rep.inner*Nobs	
#print(dim(list.as.array(tapply(1:dim(temp)[3],win.data$COU,function(x){toto<-apply(temp[,,x,drop=FALSE],c(1,2),sum);print(dim(toto));toto}))))												
#temp<-apply(exp(list.as.array(tapply(1:dim(temp)[3],win.data$COU,function(x){apply(temp[,,x,drop=FALSE],c(1,2),sum)}))),c(1,3),mean)


#temp<-apply(exp(list.as.array(tapply(1:dim(temp)[3],win.data$COU,function(x){apply(temp[,,x,drop=FALSE],c(1,2),sum)}))),c(1,3),mean)

deviances<-apply(-2*log(temp),1,sum)
temp_loglik<-log(temp)


#deviances<-apply(-2*sapply(1:(length(win.data$Y)),function(x){if(win.data$Y[x]>0){dbeta(win.data$Y[x],alpha[x,],beta[x,],log=T)+log(1-P0[x,])} else {log(P0[x,])}},simplify=T),1,sum)
###

sdinnermean<-apply(t(parsdinner%*%t(win.data$matrixsdinner)),1,mean)
if (win.data$Neta>0){etamean<-apply(t(pareta%*%t(win.data$matrixeta)),1,mean)} else {etamean<-0}
deltamean<-apply(t(pardelta%*%t(win.data$matrixdelta)),1,mean)
P0mean<-1-1/(1+exp(-exp(deltamean)-etamean))
mutotmean<-1/(1+exp(-etamean))
alphamean<-exp(sdinnermean)*mutotmean/(1-P0mean)
betamean<-exp(sdinnermean)*(1-mutotmean/(1-P0mean))
sdCOUmean<-mean(sdCOU)
####deviances.thetamean<--2*((win.data$Y>0)*(dbeta(win.data$Y,alphamean,betamean,log=T)+log(1-P0mean))+(win.data$Y==0)*log(P0mean))

set.seed(seed)
tempmean<-list.as.array(sapply(1:(length(win.data$Y)),function(x){tempb<-sapply(1:rep.inner,function(y,x){set.seed(seed+y);eascou<-rnorm(win.data$NCOU,0,1);
#tempmean<-list.as.array(sapply(1:rep.inner,function(x){eascou<-rnorm(win.data$NCOU,0,1);sapply(1:(length(win.data$Y)),function(x){
					eta<-etamean[x]+(eascou[win.data$COU[x]])*(sdCOUmean);
					mutot<-1/(1+exp(-eta))

					P0mean<-1-1/(1+exp(-exp(deltamean[x])-eta))

alpha<-exp(sdinnermean[x])*mutot/(1-P0mean)
beta<-exp(sdinnermean[x])*(1-mutot)/(1-P0mean)
#dbetabinom.ab(win.data$Y[x],win.data$N[x],alpha,beta,log=T)
if(win.data$Y[x]>0){dbeta(win.data$Y[x],alpha,beta,log=F)*(1-P0mean)} else {(P0mean)}
},x,simplify=T)
mean(tempb)},simplify=F))


deviance.thetamean<-sum(-2*log(tempmean))
#deviance.thetamean<-sum(-2*sapply(1:(length(win.data$Y)),function(x){if(win.data$Y[x]>0){dbeta(win.data$Y[x],alphamean[x],betamean[x],log=T)+log(1-P0mean[x])} else {log(P0mean[x])}},simplify=T))



deviance.min<-min(deviances)

pDvar<-0.5*var(deviances)

Dbar<-mean(deviances)
	#calculation of deviance for WAIC
		deviance.mean.prob<-(-2)*log(apply(exp(temp_loglik),2,mean))
		
		Dbar.WAIC<-sum(deviance.mean.prob)
	      pD1.WAIC=-(Dbar.WAIC-Dbar)
	     
log_lik<-temp_loglik
loo1<-loo(log_lik)

waic1<-get("waic",pos="package:loo")(log_lik)




c(DIC=2*mean(deviances)-deviance.thetamean,DICvar=mean(deviances)+pDvar,DICmin=2*mean(deviances)-deviance.min,IC=mean(deviances)+2*pDvar,pDvar=pDvar,
pD1.WAIC=pD1.WAIC,looic=loo1$looic,waic=waic1$waic,propkfair=pareto_k_table(loo1)[2,2],propkbad=pareto_k_table(loo1)[3,2],propkverybad=pareto_k_table(loo1)[4,2])
}






DICZIbeta.woCOU<-function(selected.model,rep.inner=100,reps=60,reps.mean=1000,win.data=win.data){
#selected.model<-"mod.myco.RStot.Gess.Alti.linear.fixe.SampleDUR.s1"
#commande pour extraction paramètres: extract(modExtinct_betabinomheterosc_anthropo,"pareta")[[1]]
#commande pour summary: summary(modExtinct_betabinomheterosc_anthropo)$summary



#sdCOU<-extract(selected.model,"sdCOU")[[1]]

#easCOU<-extract(selected.model,"easCOU")[[1]]

pardelta<-as.matrix(extract(selected.model,"pardelta")[[1]])

pareta<-as.matrix(extract(selected.model,"pareta")[[1]])

parsdinner<-extract(selected.model,"parsdinner")[[1]]

#print(dim(pareta))
#print(dim(win.data$matrixeta))
sdinner<-t(parsdinner%*%t(win.data$matrixsdinner))
eta<-t(pareta%*%t(win.data$matrixeta))
delta<-t(pardelta%*%t(win.data$matrixdelta))
P0<-1-1/(1+exp(-exp(delta)-eta))
mutot<-1/(1+exp(-eta))
alpha<-exp(sdinner)*mutot/(1-P0)
beta<-exp(sdinner)*(1-mutot/(1-P0))

### wrong: deviances<--2*((dbeta(win.data$Y,alpha,beta,log=T)+log(1-P0))*(win.data$Y>0)+(win.data$Y==0)*log(P0))

deviances<-apply(-2*sapply(1:(length(win.data$Y)),function(x){if(win.data$Y[x]>0){dbeta(win.data$Y[x],alpha[x,],beta[x,],log=T)+log(1-P0[x,])} else {log(P0[x,])}},simplify=T),1,sum)
###

sdinnermean<-apply(t(parsdinner%*%t(win.data$matrixsdinner)),1,mean)
etamean<-apply(t(pareta%*%t(win.data$matrixeta)),1,mean)
deltamean<-apply(t(pardelta%*%t(win.data$matrixdelta)),1,mean)
P0mean<-1-1/(1+exp(-exp(deltamean)-etamean))
mutotmean<-1/(1+exp(-etamean))
alphamean<-exp(sdinnermean)*mutotmean/(1-P0mean)
betamean<-exp(sdinnermean)*(1-mutotmean/(1-P0mean))
####deviances.thetamean<--2*((win.data$Y>0)*(dbeta(win.data$Y,alphamean,betamean,log=T)+log(1-P0mean))+(win.data$Y==0)*log(P0mean))


deviance.thetamean<-sum(-2*sapply(1:(length(win.data$Y)),function(x){if(win.data$Y[x]>0){dbeta(win.data$Y[x],alphamean[x],betamean[x],log=T)+log(1-P0mean[x])} else {log(P0mean[x])}},simplify=T))



deviance.min<-min(deviances)

pDvar<-0.5*var(deviances)

Dbar<-mean(deviances)
	#calculation of deviance for WAIC
	log_lik<-extract_log_lik(selected.model)
		deviance.mean.prob<-(-2)*log(apply(exp(log_lik),2,mean))
		
		Dbar.WAIC<-sum(deviance.mean.prob)
	      pD1.WAIC=-(Dbar.WAIC-Dbar)
		  

loo1<-loo(log_lik)

waic1<-get("waic",pos="package:loo")(log_lik)





c(DIC=2*mean(deviances)-deviance.thetamean,DICvar=mean(deviances)+pDvar,DICmin=2*mean(deviances)-deviance.min,IC=mean(deviances)+2*pDvar,pDvar=pDvar,
pD1.WAIC=pD1.WAIC,looic=loo1$looic,waic=waic1$waic,propkfair=pareto_k_table(loo1)[2,2],propkbad=pareto_k_table(loo1)[3,2],propkverybad=pareto_k_table(loo1)[4,2])
}








### function to interpret magnitude and /or significance:
interp.magnsign<-function(model,mult=NULL,index=11,multest=1)
{#print(summary(model)$summary[paste("pareta[",index,"]",sep=""),])
effects<-extract(model,"pareta")[[1]][,index]
### beta p-value:
pbrute<-min(sum(effects<0),sum(effects>0))
pbeta<-rbeta(1,shape1=pbrute+1,shape2=length(effects)-pbrute+1)

magn=NULL
if (!is.null(mult))
{
magn=""
if (mean((effects*mult)<1&(effects*mult)>(-1))>0.95)
	{magn=paste(magn,"0",sep="")}
if (mean((effects*mult)<0.5&(effects*mult)>(-0.5))>0.95)
	{magn=paste(magn,"0",sep="")}
if (mean((effects*mult)<0.1&(effects*mult)>(-0.1))>0.95)
	{magn=paste(magn,"0",sep="")}
if (mean((effects*mult)>(0.1))>0.95)
	{magn=paste(magn,"+",sep="")}
	if (mean((effects*mult)>(0.5))>0.95)
	{magn=paste(magn,"+",sep="")}
if (mean((effects*mult)>(1))>0.95)
	{magn=paste(magn,"+",sep="")}
	
	
	if (mean((effects*mult)<(-0.1))>0.95)
	{magn=paste(magn,"-",sep="")}
	if (mean((effects*mult)<(-0.5))>0.95)
	{magn=paste(magn,"-",sep="")}
if (mean((effects*mult)<(-1))>0.95)
	{magn=paste(magn,"-",sep="")}
	
}

pexplore<-abs(mean(effects)/sd(effects))

if (pbeta<0.001|pexplore>3.3)
{print (paste ("effet",index,"significativement < 0.001"))}

list(estimates=paste(round(summary(model)$summary[paste("pareta[",index,"]",sep=""),1]*multest,digits=3)," (",round(summary(model)$summary[paste("pareta[",index,"]",sep=""),3]*multest,digits=3),")",sep=""),pbeta=round(pbeta,digits=5),magn=magn,pexplore=pexplore)}




}






########################### SUMMARY STATISTICS: + dependencies
{
			
if (Sys.info()["sysname"]=="Windows") {setwd(wd.RData)} 
load("BSC_Data_functions.RData")



##### Code to produce Figure SM1
{						model.eta<-gls(SEAL.2009~1,data=datatot,
									subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

						names.variables<-c("HANPP.1900.Dullinger","HANPP.1950.Dullinger","HANPP.2000.Dullinger","SEAL.2009","INC.SEAL.CA","PIB.1950.Dullinger","PIB1950aD","POPd.Dullinger.1950","PIB.1900.Dullinger","PIB1900aD","POPd.Dullinger.1900","PIB.2000.Dullinger","PIB2000aD","POPd.Dullinger.2000","PIB1950h","PIB1950a","POP1950d","PIBa","PIBh","POPd" )
								
						temp<-sapply(names.variables,function(x){
						as.double(getData(model.eta)[,x])
						})
						temp<-as.data.frame(temp)
						temp[,"GDPa.1900"]<-temp[,"PIB1900aD"]
						temp[,"GDPc.1900"]<-temp[,"PIB.1900.Dullinger"]
						temp[,"HPD.1900"]<-temp[,"POPd.Dullinger.1900"]
						temp[,"GDPa.2000"]<-temp[,"PIB2000aD"]
						temp[,"GDPc.2000"]<-temp[,"PIB.2000.Dullinger"]
						temp[,"HPD.2000"]<-temp[,"POPd.Dullinger.2000"]
						temp[,"GDPa.1950"]<-temp[,"PIB1950aD"]
						temp[,"GDPc.1950"]<-temp[,"PIB.1950.Dullinger"]
						temp[,"HPD.1950"]<-temp[,"POPd.Dullinger.1950"]
						
						temp[,"HANPP.1900"]<-temp[,"HANPP.1900.Dullinger"]
						temp[,"HANPP.1950"]<-temp[,"HANPP.1950.Dullinger"]
						temp[,"HANPP.2000"]<-temp[,"HANPP.2000.Dullinger"]
						
						temp[,"GDPa.1950.orig"]<-temp[,"PIB1950a"]
						temp[,"GDPc.1950.orig"]<-temp[,"PIB1950h"]
						temp[,"HPD.1950.orig"]<-temp[,"POP1950d"]
						
						temp[,"GDPa.orig"]<-temp[,"PIBa"]
						temp[,"GDPc.orig"]<-temp[,"PIBh"]
						temp[,"HPD.orig"]<-temp[,"POPd"]
						
						temp[,"SEAL"]<-temp[,"SEAL.2009"]
						temp[,"iSEAL"]<-temp[,"INC.SEAL.CA"]

						
						
						
						tree<-varclus(x=~HANPP.1900+HANPP.1950+HANPP.2000+GDPa.1900+GDPc.1900+HPD.1900+GDPa.1950+GDPc.1950+HPD.1950+GDPa.2000+GDPc.2000+HPD.2000, data = as.data.frame(temp),similarity="pearson", na.action=na.retain, method="average")
						graphe<-plot(tree)
						title(graphe, main = "variable clustering (varclus) - with correlations",sub="method : average")

}








######## Code to produce Table SM1: explanatory variables
##################################################################




if (Sys.info()["sysname"]=="Windows") {setwd(wd.RData)} 
load("BSC_Data_functions.RData")

summary.BSC<-function(x)
	{paste("Untransformed: ", paste(format(mean(x),digits=2,nsmall=2),"(+- ",format(sd(x),digits=2,nsmall=2),")",sep="")," [",format(min(x),digits=2,nsmall=2),"; ",format(max(x),digits=2,nsmall=2),"]"," wwLog: ", paste(format(mean(log(x)),digits=2,nsmall=2),"(+- ",format(sd(log(x)),digits=2,nsmall=2),")",sep="")," [",format(min(log(x)),digits=2,nsmall=2),"; ",format(max(log(x)),digits=2,nsmall=2),"]",sep="")}

model.delta<-gls(INC.SEAL.CA~1, data=datatot,
	subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

table.to.summarize<-cbind(getData(model.delta)[,c("PIB.2000.Dullinger","PIB2000aD","POPd.Dullinger.2000","HANPP.2000.Dullinger","PIB.1950.Dullinger","PIB1950aD","POPd.Dullinger.1950","HANPP.1950.Dullinger","PIB.1900.Dullinger","PIB1900aD","POPd.Dullinger.1900","HANPP.1900.Dullinger")],square=getData(model.delta)[,c("PIB.2000.Dullinger","PIB2000aD","PIB.1950.Dullinger","PIB1950aD","PIB.1900.Dullinger","PIB1900aD")]^2)
temp<-sapply(table.to.summarize,summary.BSC)


setwd(wd.csv)
write.csv(cbind.data.frame(names(temp), temp),file="TableSM1_BSC12.csv")



######## Code to produce Table SM2: variables to be explained
##################################################################



########## On Fourthsample:
if (Sys.info()["sysname"]=="Windows") {setwd(wd.RData)} 
load("BSC_Data_functions.RData")

summary.BSC.wolog<-function(x)
	{x<-x[!is.na(x)];paste(paste(format(mean(x),digits=2,nsmall=2),"(+- ",format(sd(x),digits=2,nsmall=2),")",sep="")," [",format(min(x),digits=2,nsmall=2),"; ",format(max(x),digits=2,nsmall=2),"]",sep="")}

model.delta<-gls(INC.SEAL.CA~1, data=datatot,
	subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

table.to.summarize<-cbind(getData(model.delta)[,c("AP1a","AP1b","AP2","AP3","AP4","LandArea",names(getData(model.delta))[162:188],"SEAL.2009","INC.SEAL.CA")])

table.to.summarize[,"RAP1"]<-(table.to.summarize[,"AP1a"]+table.to.summarize[,"AP1b"])/table.to.summarize[,"LandArea"]
table.to.summarize[,"RAP2"]<-(table.to.summarize[,"AP2"])/table.to.summarize[,"LandArea"]
table.to.summarize[,"RAP3"]<-(table.to.summarize[,"AP3"])/table.to.summarize[,"LandArea"]
table.to.summarize[,"RAP4"]<-(table.to.summarize[,"AP4"])/table.to.summarize[,"LandArea"]

table.to.summarize[,"Prop.Vpextinct"]<-table.to.summarize[,"Vpextinct"]/table.to.summarize[,"Vpnative"]
table.to.summarize[,"Prop.Brextinct"]<-table.to.summarize[,"Brextinct"]/table.to.summarize[,"Brnative"]
table.to.summarize[,"Prop.Maextinct"]<-table.to.summarize[,"Maextinct"]/table.to.summarize[,"Manative"]
table.to.summarize[,"Prop.Biextinct"]<-table.to.summarize[,"Biextinct"]/table.to.summarize[,"Binative"]
table.to.summarize[,"Prop.Fiextinct"]<-table.to.summarize[,"Fiextinct"]/table.to.summarize[,"Finative"]
table.to.summarize[,"Prop.Reextinct"]<-table.to.summarize[,"Reextinct"]/table.to.summarize[,"Renative"]
table.to.summarize[,"Prop.Amextinct"]<-table.to.summarize[,"Amextinct"]/table.to.summarize[,"Amnative"]
table.to.summarize[,"Prop.Dfextinct"]<-table.to.summarize[,"Dfextinct"]/table.to.summarize[,"Dfnative"]
table.to.summarize[,"Prop.Ghextinct"]<-table.to.summarize[,"Ghextinct"]/table.to.summarize[,"Ghnative"]


table.to.summarize[,"Prop.Vpthreat"]<-table.to.summarize[,"Vpdanger"]/(table.to.summarize[,"Vpnative"]-table.to.summarize[,"Vpextinct"])
table.to.summarize[,"Prop.Brthreat"]<-table.to.summarize[,"Brdanger"]/(table.to.summarize[,"Brnative"]-table.to.summarize[,"Brextinct"])
table.to.summarize[,"Prop.Mathreat"]<-table.to.summarize[,"Madanger"]/(table.to.summarize[,"Manative"]-table.to.summarize[,"Maextinct"])
table.to.summarize[,"Prop.Bithreat"]<-table.to.summarize[,"Bidanger"]/(table.to.summarize[,"Binative"]-table.to.summarize[,"Biextinct"])
table.to.summarize[,"Prop.Fithreat"]<-table.to.summarize[,"Fidanger"]/(table.to.summarize[,"Finative"]-table.to.summarize[,"Fiextinct"])
table.to.summarize[,"Prop.Rethreat"]<-table.to.summarize[,"Redanger"]/(table.to.summarize[,"Renative"]-table.to.summarize[,"Reextinct"])
table.to.summarize[,"Prop.Amthreat"]<-table.to.summarize[,"Amdanger"]/(table.to.summarize[,"Amnative"]-table.to.summarize[,"Amextinct"])
table.to.summarize[,"Prop.Dfthreat"]<-table.to.summarize[,"Dfdanger"]/(table.to.summarize[,"Dfnative"]-table.to.summarize[,"Dfextinct"])
table.to.summarize[,"Prop.Ghthreat"]<-table.to.summarize[,"Ghdanger"]/(table.to.summarize[,"Ghnative"]-table.to.summarize[,"Ghextinct"])



temp<-sapply(table.to.summarize,summary.BSC.wolog)



setwd(wd.csv)
write.csv(cbind.data.frame(names(temp), temp),file="TableSM2_BSC12.csv")


}



#####################################################################
##### CODES for TABLES IN THE MAIN FILE (1 to 6)
#####################################################################
{
########## EXTINCT SPECIES
{

###### model fitting & IC calculation (Tables 2, 3 & 5)

{
if (Sys.info()["sysname"]=="Windows") {setwd(wd.RData)} 
load("BSC_Data_functions.RData")

#### parameters used to control which part of the process to do : estimation (runStan) or calculation of model comparison indices (runDIC)
runStan<-TRUE
runDIC<-TRUE


DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger<-matrix(NA,ncol=11,nrow=8)
dimnames(DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger)<-list(c(
			
			"POPd","POPdl","POPdsq","GDP1900a","GDP1900al",
			"SEAL.2009sq","SEAL.2009l","SEAL.2009"),c("DIC", "DICvar","DICmin", "IC",  "pDvar",  "pD1.WAIC",        "looic", "waic",   "propkfair",  "propkbad", "propkverybad"))



			
			
			
			
			
			
			

	
			
			
			
			
			
					
model.delta<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Extinct,Native-Extinct)~Taxon+I(scale(log(PIB2000aD/2400),scale=F))-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Extinct~Native-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Extinct-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Extinct,Native-Extinct)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Extinct,Native-Extinct)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modExtinct_betabinomheteroscpriorsdlowrest_GDP2000al_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomquad.extinct,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modExtinct_betabinomheteroscpriorsdlowrest_GDP2000al_fourthsamplewotrees_bis,file="modExtinct_betabinomheteroscpriorsdlowrest_GDP2000al_fourthsamplewotrees_bis.RData")
rm(modExtinct_betabinomheteroscpriorsdlowrest_GDP2000al_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modExtinct_betabinomheteroscpriorsdlowrest_GDP2000al_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger,GDP2000al=DICmarginal.stanmodel.heteroscbetabinom(modExtinct_betabinomheteroscpriorsdlowrest_GDP2000al_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
}


			
			
			
						
			
			
			
model.delta<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Extinct,Native-Extinct)~Taxon+I(scale(log(PIB1950aD/400),scale=F))-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Extinct~Native-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Extinct-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Extinct,Native-Extinct)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Extinct,Native-Extinct)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modExtinct_betabinomheteroscpriorsdlowrest_GDP1950al_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomquad.extinct,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modExtinct_betabinomheteroscpriorsdlowrest_GDP1950al_fourthsamplewotrees_bis,file="modExtinct_betabinomheteroscpriorsdlowrest_GDP1950al_fourthsamplewotrees_bis.RData")
rm(modExtinct_betabinomheteroscpriorsdlowrest_GDP1950al_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modExtinct_betabinomheteroscpriorsdlowrest_GDP1950al_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger,GDP1950al=DICmarginal.stanmodel.heteroscbetabinom(modExtinct_betabinomheteroscpriorsdlowrest_GDP1950al_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
}



model.delta<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Extinct,Native-Extinct)~Taxon+I(scale(log(PIB1900aD/400),scale=F))-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Extinct~Native-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Extinct-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Extinct,Native-Extinct)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Extinct,Native-Extinct)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modExtinct_betabinomheteroscpriorsdlowrest_GDP1900al_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomquad.extinct,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modExtinct_betabinomheteroscpriorsdlowrest_GDP1900al_fourthsamplewotrees_bis,file="modExtinct_betabinomheteroscpriorsdlowrest_GDP1900al_fourthsamplewotrees_bis.RData")
rm(modExtinct_betabinomheteroscpriorsdlowrest_GDP1900al_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modExtinct_betabinomheteroscpriorsdlowrest_GDP1900al_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger,GDP1900al=DICmarginal.stanmodel.heteroscbetabinom(modExtinct_betabinomheteroscpriorsdlowrest_GDP1900al_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
}







model.delta<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Extinct,Native-Extinct)~Taxon+I(scale(log(POPd.Dullinger.1900/50),scale=F))-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Extinct~Native-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Extinct-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Extinct,Native-Extinct)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Extinct,Native-Extinct)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modExtinct_betabinomheteroscpriorsdlowrest_POPd1900l_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomquad.extinct,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modExtinct_betabinomheteroscpriorsdlowrest_POPd1900l_fourthsamplewotrees_bis,file="modExtinct_betabinomheteroscpriorsdlowrest_POPd1900l_fourthsamplewotrees_bis.RData")
rm(modExtinct_betabinomheteroscpriorsdlowrest_POPd1900l_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modExtinct_betabinomheteroscpriorsdlowrest_POPd1900l_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger,POPd1900l=DICmarginal.stanmodel.heteroscbetabinom(modExtinct_betabinomheteroscpriorsdlowrest_POPd1900l_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
}









model.delta<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Extinct,Native-Extinct)~Taxon+I(scale(PIB1900aD,scale=F)/400)-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Extinct~Native-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Extinct-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Extinct,Native-Extinct)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Extinct,Native-Extinct)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modExtinct_betabinomheteroscpriorsdlowrest_GDP1900a_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomquad.extinct,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modExtinct_betabinomheteroscpriorsdlowrest_GDP1900a_fourthsamplewotrees_bis,file="modExtinct_betabinomheteroscpriorsdlowrest_GDP1900a_fourthsamplewotrees_bis.RData")
rm(modExtinct_betabinomheteroscpriorsdlowrest_GDP1900a_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modExtinct_betabinomheteroscpriorsdlowrest_GDP1900a_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger,GDP1900a=DICmarginal.stanmodel.heteroscbetabinom(modExtinct_betabinomheteroscpriorsdlowrest_GDP1900a_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
}





model.delta<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Extinct,Native-Extinct)~Taxon+I(scale((POPd1900.Dullinger.1900/50),scale=F))-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Extinct~Native-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Extinct-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Extinct,Native-Extinct)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Extinct,Native-Extinct)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
				
				
nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modExtinct_betabinomheteroscpriorsdlowrest_POPd1900_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomquad.extinct,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modExtinct_betabinomheteroscpriorsdlowrest_POPd1900_fourthsamplewotrees_bis,file="modExtinct_betabinomheteroscpriorsdlowrest_POPd1900_fourthsamplewotrees_bis.RData")
rm(modExtinct_betabinomheteroscpriorsdlowrest_POPd1900_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modExtinct_betabinomheteroscpriorsdlowrest_POPd1900_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger,POPd1900=DICmarginal.stanmodel.heteroscbetabinom(modExtinct_betabinomheteroscpriorsdlowrest_POPd1900_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
}






model.delta<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Extinct,Native-Extinct)~Taxon+I(scale(SEAL.2009,scale=F)/2)-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Extinct~Native-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Extinct-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Extinct,Native-Extinct)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Extinct,Native-Extinct)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modExtinct_betabinomheteroscpriorsdlowrest_SEAL.2009_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomquad.extinct,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modExtinct_betabinomheteroscpriorsdlowrest_SEAL.2009_fourthsamplewotrees_bis,file="modExtinct_betabinomheteroscpriorsdlowrest_SEAL.2009_fourthsamplewotrees_bis.RData")
rm(modExtinct_betabinomheteroscpriorsdlowrest_SEAL.2009_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modExtinct_betabinomheteroscpriorsdlowrest_SEAL.2009_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger,SEAL.2009=DICmarginal.stanmodel.heteroscbetabinom(modExtinct_betabinomheteroscpriorsdlowrest_SEAL.2009_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
}







model.delta<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Extinct,Native-Extinct)~Taxon+I(scale(SEAL.2009/2,scale=F))+I((scale(SEAL.2009/2,scale=F))^2)-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Extinct,Native-Extinct)~Taxon+I((scale(SEAL.2009/2,scale=F))^2)-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Extinct~Native-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Extinct-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Extinct,Native-Extinct)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Extinct,Native-Extinct)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
				
				
nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modExtinct_betabinomheteroscpriorsdlowrest_SEAL.2099sq_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomquad.extinct,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modExtinct_betabinomheteroscpriorsdlowrest_SEAL.2099sq_fourthsamplewotrees_bis,file="modExtinct_betabinomheteroscpriorsdlowrest_SEAL.2099sq_fourthsamplewotrees_bis.RData")
rm(modExtinct_betabinomheteroscpriorsdlowrest_SEAL.2099sq_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modExtinct_betabinomheteroscpriorsdlowrest_SEAL.2099sq_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger,SEAL.2099sq=DICmarginal.stanmodel.heteroscbetabinom(modExtinct_betabinomheteroscpriorsdlowrest_SEAL.2099sq_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
}


model.delta<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Extinct,Native-Extinct)~Taxon+I(scale(log(SEAL.2009/2),scale=F))-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Extinct~Native-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Extinct-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Extinct,Native-Extinct)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Extinct,Native-Extinct)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
				
				
nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modExtinct_betabinomheteroscpriorsdlowrest_SEAL.2009l_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomquad.extinct,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modExtinct_betabinomheteroscpriorsdlowrest_SEAL.2009l_fourthsamplewotrees_bis,file="modExtinct_betabinomheteroscpriorsdlowrest_SEAL.2009l_fourthsamplewotrees_bis.RData")
rm(modExtinct_betabinomheteroscpriorsdlowrest_SEAL.2009l_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modExtinct_betabinomheteroscpriorsdlowrest_SEAL.2009l_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger,SEAL.2009l=DICmarginal.stanmodel.heteroscbetabinom(modExtinct_betabinomheteroscpriorsdlowrest_SEAL.2009l_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
}






model.delta<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Extinct,Native-Extinct)~Taxon+I(scale(POPd.Dullinger.1900/50,scale=F))+I((scale(POPd.Dullinger.1900/50,scale=F))^2)-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
			model.eta<-glm(cbind(Extinct,Native-Extinct)~Taxon+I((scale(POPd.Dullinger.1900/50,scale=F))^2)-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Extinct~Native-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Extinct-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Extinct,Native-Extinct)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Extinct,Native-Extinct)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
				
				
nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modExtinct_betabinomheteroscpriorsdlowrest_POPdsq_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomquad.extinct,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modExtinct_betabinomheteroscpriorsdlowrest_POPdsq_fourthsamplewotrees_bis,file="modExtinct_betabinomheteroscpriorsdlowrest_POPd1900sq_fourthsamplewotrees_bis.RData")
rm(modExtinct_betabinomheteroscpriorsdlowrest_POPdsq_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modExtinct_betabinomheteroscpriorsdlowrest_POPd1900sq_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger,POPdsq=DICmarginal.stanmodel.heteroscbetabinom(modExtinct_betabinomheteroscpriorsdlowrest_POPdsq_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
}






model.delta<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Extinct,Native-Extinct)~Taxon+I((scale(PIB1900aD,scale=F)/400)^2)-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Extinct~Native-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Extinct-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Extinct,Native-Extinct)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Extinct,Native-Extinct)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
				
				
				
nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modExtinct_betabinomheteroscpriorsdlowrest_GDP1900asq_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomquad.extinct,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modExtinct_betabinomheteroscpriorsdlowrest_GDP1900asq_fourthsamplewotrees_bis,file="modExtinct_betabinomheteroscpriorsdlowrest_GDP1900asq_fourthsamplewotrees_bis.RData")
rm(modExtinct_betabinomheteroscpriorsdlowrest_GDP1900asq_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modExtinct_betabinomheteroscpriorsdlowrest_GDP1900asq_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger,GDP1900asq=DICmarginal.stanmodel.heteroscbetabinom(modExtinct_betabinomheteroscpriorsdlowrest_GDP1900asq_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
}


### les deux suivants n'ont pas formellement torné mais les résultats dans xlsx sont OK


model.delta<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Extinct,Native-Extinct)~Taxon+I(scale(SEAL.2009/2,scale=F))+I((scale(SEAL.2009/2,scale=F))^2)-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))

model.bidon<-glm(Extinct~Native-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Extinct-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Extinct,Native-Extinct)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Extinct,Native-Extinct)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				

model.delta<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Extinct,Native-Extinct)~Taxon+I(scale(SEAL.2009,scale=F))+I(scale(INC.SEAL.CA,scale=F))-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Extinct~Native-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Extinct-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Extinct,Native-Extinct)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Extinct,Native-Extinct)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
				
nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modExtinct_betabinomheteroscpriorsdlowrest_SEAL_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomquad.extinct,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modExtinct_betabinomheteroscpriorsdlowrest_SEAL_fourthsamplewotrees_bis,file="modExtinct_betabinomheteroscpriorsdlowrest_SEAL_fourthsamplewotrees_bis.RData")
rm(modExtinct_betabinomheteroscpriorsdlowrest_SEAL_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modExtinct_betabinomheteroscpriorsdlowrest_SEAL_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger,SEAL=DICmarginal.stanmodel.heteroscbetabinom(modExtinct_betabinomheteroscpriorsdlowrest_SEAL_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
}





model.delta<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Extinct,Native-Extinct)~Taxon+I(scale(PIB.1950.Dullinger/2,scale=F))+I((scale(PIB.1950.Dullinger/2,scale=F))^2)-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Extinct~Native-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Extinct-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Extinct,Native-Extinct)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Extinct,Native-Extinct)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modExtinct_betabinomheteroscpriorsdlowrest_Kuznets1950_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomquad.extinct,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modExtinct_betabinomheteroscpriorsdlowrest_Kuznets1950_fourthsamplewotrees_bis,file="modExtinct_betabinomheteroscpriorsdlowrest_Kuznets1950_fourthsamplewotrees_bis.RData")
rm(modExtinct_betabinomheteroscpriorsdlowrest_Kuznets1950_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modExtinct_betabinomheteroscpriorsdlowrest_Kuznets1950_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger,Kuznets1950=DICmarginal.stanmodel.heteroscbetabinom(modExtinct_betabinomheteroscpriorsdlowrest_Kuznets1950_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
}




model.delta<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Extinct,Native-Extinct)~Taxon+I(scale(log(PIB.1950.Dullinger/2),scale=F))+I((scale(log(PIB.1950.Dullinger/2),scale=F))^2)-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Extinct~Native-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Extinct-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Extinct,Native-Extinct)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Extinct,Native-Extinct)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modExtinct_betabinomheteroscpriorsdlowrest_lKuznets1950_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomquad.extinct,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modExtinct_betabinomheteroscpriorsdlowrest_lKuznets1950_fourthsamplewotrees_bis,file="modExtinct_betabinomheteroscpriorsdlowrest_lKuznets1950_fourthsamplewotrees_bis.RData")
rm(modExtinct_betabinomheteroscpriorsdlowrest_lKuznets1950_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modExtinct_betabinomheteroscpriorsdlowrest_lKuznets1950_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger,lKuznets1950=DICmarginal.stanmodel.heteroscbetabinom(modExtinct_betabinomheteroscpriorsdlowrest_lKuznets1950_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
}




model.delta<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Extinct,Native-Extinct)~Taxon+I(scale(PIB1950aD,scale=F)/400)+I((scale(PIB1950aD,scale=F)/400)^2)-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Extinct~Native-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Extinct-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Extinct,Native-Extinct)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Extinct,Native-Extinct)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modExtinct_betabinomheteroscpriorsdlowrest_Kuznets1950a_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomquad.extinct,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modExtinct_betabinomheteroscpriorsdlowrest_Kuznets1950a_fourthsamplewotrees_bis,file="modExtinct_betabinomheteroscpriorsdlowrest_Kuznets1950a_fourthsamplewotrees_bis.RData")
rm(modExtinct_betabinomheteroscpriorsdlowrest_Kuznets1950a_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modExtinct_betabinomheteroscpriorsdlowrest_Kuznets1950a_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger,Kuznets1950a=DICmarginal.stanmodel.heteroscbetabinom(modExtinct_betabinomheteroscpriorsdlowrest_Kuznets1950a_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
}


model.delta<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Extinct,Native-Extinct)~Taxon+I(scale(log(PIB1950aD/400),scale=F))+I((scale(log(PIB1950aD/400),scale=F))^2)-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Extinct~Native-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Extinct-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Extinct,Native-Extinct)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Extinct,Native-Extinct)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
				
nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modExtinct_betabinomheteroscpriorsdlowrest_lKuznets1950a_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomquad.extinct,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modExtinct_betabinomheteroscpriorsdlowrest_lKuznets1950a_fourthsamplewotrees_bis,file="modExtinct_betabinomheteroscpriorsdlowrest_lKuznets1950a_fourthsamplewotrees_bis.RData")
rm(modExtinct_betabinomheteroscpriorsdlowrest_lKuznets1950a_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modExtinct_betabinomheteroscpriorsdlowrest_lKuznets1950a_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger,lKuznets1950a=DICmarginal.stanmodel.heteroscbetabinom(modExtinct_betabinomheteroscpriorsdlowrest_lKuznets1950a_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
}





model.delta<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Extinct,Native-Extinct)~Taxon+I(scale((PIB.1950.Dullinger/2),scale=F))+I(scale((POPd.Dullinger.1950/50),scale=F))-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Extinct~Native-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Extinct-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Extinct,Native-Extinct)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Extinct,Native-Extinct)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modExtinct_betabinomheteroscpriorsdlowrest_eco1950_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomquad.extinct,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modExtinct_betabinomheteroscpriorsdlowrest_eco1950_fourthsamplewotrees_bis,file="modExtinct_betabinomheteroscpriorsdlowrest_eco1950_fourthsamplewotrees_bis.RData")
rm(modExtinct_betabinomheteroscpriorsdlowrest_eco1950_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modExtinct_betabinomheteroscpriorsdlowrest_eco1950_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger,eco1950=DICmarginal.stanmodel.heteroscbetabinom(modExtinct_betabinomheteroscpriorsdlowrest_eco1950_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
}

model.delta<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Extinct,Native-Extinct)~Taxon+I(scale(log(PIB.1950.Dullinger/2),scale=F))+I(scale(log(POPd.Dullinger.1950/50),scale=F))-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Extinct~Native-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Extinct-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Extinct,Native-Extinct)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Extinct,Native-Extinct)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modExtinct_betabinomheteroscpriorsdlowrest_leco1950_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomquad.extinct,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modExtinct_betabinomheteroscpriorsdlowrest_leco1950_fourthsamplewotrees_bis,file="modExtinct_betabinomheteroscpriorsdlowrest_leco1950_fourthsamplewotrees_bis.RData")
rm(modExtinct_betabinomheteroscpriorsdlowrest_leco1950_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modExtinct_betabinomheteroscpriorsdlowrest_leco1950_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger,leco1950=DICmarginal.stanmodel.heteroscbetabinom(modExtinct_betabinomheteroscpriorsdlowrest_leco1950_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
}












model.delta<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Extinct,Native-Extinct)~Taxon+I(scale(PIB.1900.Dullinger/2,scale=F))+I((scale(PIB.1900.Dullinger/2,scale=F))^2)-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Extinct~Native-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Extinct-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Extinct,Native-Extinct)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Extinct,Native-Extinct)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
				
nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modExtinct_betabinomheteroscpriorsdlowrest_Kuznets1900_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomquad.extinct,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modExtinct_betabinomheteroscpriorsdlowrest_Kuznets1900_fourthsamplewotrees_bis,file="modExtinct_betabinomheteroscpriorsdlowrest_Kuznets1900_fourthsamplewotrees_bis.RData")
rm(modExtinct_betabinomheteroscpriorsdlowrest_Kuznets1900_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modExtinct_betabinomheteroscpriorsdlowrest_Kuznets1900_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger,Kuznets1900=DICmarginal.stanmodel.heteroscbetabinom(modExtinct_betabinomheteroscpriorsdlowrest_Kuznets1900_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
}


model.delta<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Extinct,Native-Extinct)~Taxon+I(scale(log(PIB.1900.Dullinger/2),scale=F))+I((scale(log(PIB.1900.Dullinger/2),scale=F))^2)-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Extinct~Native-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Extinct-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Extinct,Native-Extinct)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Extinct,Native-Extinct)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modExtinct_betabinomheteroscpriorsdlowrest_lKuznets1900_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomquad.extinct,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modExtinct_betabinomheteroscpriorsdlowrest_lKuznets1900_fourthsamplewotrees_bis,file="modExtinct_betabinomheteroscpriorsdlowrest_lKuznets1900_fourthsamplewotrees_bis.RData")
rm(modExtinct_betabinomheteroscpriorsdlowrest_lKuznets1900_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modExtinct_betabinomheteroscpriorsdlowrest_lKuznets1900_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger,lKuznets1900=DICmarginal.stanmodel.heteroscbetabinom(modExtinct_betabinomheteroscpriorsdlowrest_lKuznets1900_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
}




model.delta<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Extinct,Native-Extinct)~Taxon+I(scale(PIB1900aD,scale=F)/400)+I((scale(PIB1900aD,scale=F)/400)^2)-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Extinct~Native-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Extinct-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Extinct,Native-Extinct)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Extinct,Native-Extinct)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modExtinct_betabinomheteroscpriorsdlowrest_Kuznets1900a_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomquad.extinct,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modExtinct_betabinomheteroscpriorsdlowrest_Kuznets1900a_fourthsamplewotrees_bis,file="modExtinct_betabinomheteroscpriorsdlowrest_Kuznets1900a_fourthsamplewotrees_bis.RData")
rm(modExtinct_betabinomheteroscpriorsdlowrest_Kuznets1900a_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modExtinct_betabinomheteroscpriorsdlowrest_Kuznets1900a_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger,Kuznets1900a=DICmarginal.stanmodel.heteroscbetabinom(modExtinct_betabinomheteroscpriorsdlowrest_Kuznets1900a_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
}






model.delta<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Extinct,Native-Extinct)~Taxon+I(scale(log(PIB1900aD/400),scale=F))+I((scale(log(PIB1900aD/400),scale=F))^2)-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Extinct~Native-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Extinct-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Extinct,Native-Extinct)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Extinct,Native-Extinct)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modExtinct_betabinomheteroscpriorsdlowrest_lKuznets1900a_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomquad.extinct,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modExtinct_betabinomheteroscpriorsdlowrest_lKuznets1900a_fourthsamplewotrees_bis,file="modExtinct_betabinomheteroscpriorsdlowrest_lKuznets1900a_fourthsamplewotrees_bis.RData")
rm(modExtinct_betabinomheteroscpriorsdlowrest_lKuznets1900a_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modExtinct_betabinomheteroscpriorsdlowrest_lKuznets1900a_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger,lKuznets1900a=DICmarginal.stanmodel.heteroscbetabinom(modExtinct_betabinomheteroscpriorsdlowrest_lKuznets1900a_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
}





















model.delta<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Extinct,Native-Extinct)~Taxon+I(scale((PIB.1900.Dullinger/2),scale=F))+I(scale((POPd.Dullinger.1900/50),scale=F))-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Extinct~Native-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Extinct-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Extinct,Native-Extinct)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Extinct,Native-Extinct)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modExtinct_betabinomheteroscpriorsdlowrest_eco1900_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomquad.extinct,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modExtinct_betabinomheteroscpriorsdlowrest_eco1900_fourthsamplewotrees_bis,file="modExtinct_betabinomheteroscpriorsdlowrest_eco1900_fourthsamplewotrees_bis.RData")
rm(modExtinct_betabinomheteroscpriorsdlowrest_eco1900_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modExtinct_betabinomheteroscpriorsdlowrest_eco1900_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger,eco1900=DICmarginal.stanmodel.heteroscbetabinom(modExtinct_betabinomheteroscpriorsdlowrest_eco1900_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
}






#load("BSC_Extinct_fourthsamplewotrees.RData")
save.image("BSC_Extinct_fourthsamplewotrees.RData")

model.delta<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Extinct,Native-Extinct)~Taxon+I(scale(log(PIB.1900.Dullinger/2),scale=F))+I(scale(log(POPd.Dullinger.1900/50),scale=F))-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Extinct~Native-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Extinct-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Extinct,Native-Extinct)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Extinct,Native-Extinct)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modExtinct_betabinomheteroscpriorsdlowrest_leco1900_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomquad.extinct,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modExtinct_betabinomheteroscpriorsdlowrest_leco1900_fourthsamplewotrees_bis,file="modExtinct_betabinomheteroscpriorsdlowrest_leco1900_fourthsamplewotrees_bis.RData")
rm(modExtinct_betabinomheteroscpriorsdlowrest_leco1900_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modExtinct_betabinomheteroscpriorsdlowrest_leco1900_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger,leco1900=DICmarginal.stanmodel.heteroscbetabinom(modExtinct_betabinomheteroscpriorsdlowrest_leco1900_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
}


























#### A LANCER
model.delta<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Extinct,Native-Extinct)~Taxon+I(scale(PIB.2000.Dullinger/7,scale=F))+I((scale(PIB.2000.Dullinger/7,scale=F))^2)-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Extinct~Native-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Extinct-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Extinct,Native-Extinct)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Extinct,Native-Extinct)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))


nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modExtinct_betabinomheteroscpriorsdlowrest_Kuznets2000_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomquad.extinct,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modExtinct_betabinomheteroscpriorsdlowrest_Kuznets2000_fourthsamplewotrees_bis,file="modExtinct_betabinomheteroscpriorsdlowrest_Kuznets2000_fourthsamplewotrees_bis.RData")
rm(modExtinct_betabinomheteroscpriorsdlowrest_Kuznets2000_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modExtinct_betabinomheteroscpriorsdlowrest_Kuznets2000_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger,Kuznets2000=DICmarginal.stanmodel.heteroscbetabinom(modExtinct_betabinomheteroscpriorsdlowrest_Kuznets2000_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
}





model.delta<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Extinct,Native-Extinct)~Taxon+I(scale(log(PIB.2000.Dullinger/2),scale=F))+I((scale(log(PIB.2000.Dullinger/2),scale=F))^2)-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Extinct~Native-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Extinct-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Extinct,Native-Extinct)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Extinct,Native-Extinct)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modExtinct_betabinomheteroscpriorsdlowrest_lKuznets2000_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomquad.extinct,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modExtinct_betabinomheteroscpriorsdlowrest_lKuznets2000_fourthsamplewotrees_bis,file="modExtinct_betabinomheteroscpriorsdlowrest_lKuznets2000_fourthsamplewotrees_bis.RData")
rm(modExtinct_betabinomheteroscpriorsdlowrest_lKuznets2000_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modExtinct_betabinomheteroscpriorsdlowrest_lKuznets2000_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger,lKuznets2000=DICmarginal.stanmodel.heteroscbetabinom(modExtinct_betabinomheteroscpriorsdlowrest_lKuznets2000_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
}




model.delta<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Extinct,Native-Extinct)~Taxon+I(scale(PIB2000aD,scale=F)/2400)+I((scale(PIB2000aD,scale=F)/2400)^2)-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Extinct~Native-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Extinct-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Extinct,Native-Extinct)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Extinct,Native-Extinct)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))


nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modExtinct_betabinomheteroscpriorsdlowrest_Kuznets2000a_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomquad.extinct,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modExtinct_betabinomheteroscpriorsdlowrest_Kuznets2000a_fourthsamplewotrees_bis,file="modExtinct_betabinomheteroscpriorsdlowrest_Kuznets2000a_fourthsamplewotrees_bis.RData")
rm(modExtinct_betabinomheteroscpriorsdlowrest_Kuznets2000a_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modExtinct_betabinomheteroscpriorsdlowrest_Kuznets2000a_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger,Kuznets2000a=DICmarginal.stanmodel.heteroscbetabinom(modExtinct_betabinomheteroscpriorsdlowrest_Kuznets2000a_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
}






model.delta<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Extinct,Native-Extinct)~Taxon+I(scale(log(PIB2000aD/400),scale=F))+I((scale(log(PIB2000aD/400),scale=F))^2)-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Extinct~Native-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Extinct-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Extinct,Native-Extinct)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Extinct,Native-Extinct)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))


nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modExtinct_betabinomheteroscpriorsdlowrest_lKuznets2000a_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomquad.extinct,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modExtinct_betabinomheteroscpriorsdlowrest_lKuznets2000a_fourthsamplewotrees_bis,file="modExtinct_betabinomheteroscpriorsdlowrest_lKuznets2000a_fourthsamplewotrees_bis.RData")
rm(modExtinct_betabinomheteroscpriorsdlowrest_lKuznets2000a_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modExtinct_betabinomheteroscpriorsdlowrest_lKuznets2000a_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger,lKuznets2000a=DICmarginal.stanmodel.heteroscbetabinom(modExtinct_betabinomheteroscpriorsdlowrest_lKuznets2000a_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
}



model.delta<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Extinct,Native-Extinct)~Taxon+I(scale((PIB.2000.Dullinger/7),scale=F))+I(scale((POPd.Dullinger.2000/100),scale=F))-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Extinct~Native-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Extinct-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Extinct,Native-Extinct)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Extinct,Native-Extinct)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))


nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modExtinct_betabinomheteroscpriorsdlowrest_eco2000_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomquad.extinct,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modExtinct_betabinomheteroscpriorsdlowrest_eco2000_fourthsamplewotrees_bis,file="modExtinct_betabinomheteroscpriorsdlowrest_eco2000_fourthsamplewotrees_bis.RData")
rm(modExtinct_betabinomheteroscpriorsdlowrest_eco2000_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modExtinct_betabinomheteroscpriorsdlowrest_eco2000_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger,eco2000=DICmarginal.stanmodel.heteroscbetabinom(modExtinct_betabinomheteroscpriorsdlowrest_eco2000_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
}




#load("BSC_Extinct_fourthsamplewotrees.RData")
save.image("BSC_Extinct_fourthsamplewotrees.RData")

model.delta<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Extinct,Native-Extinct)~Taxon+I(scale(log(PIB.2000.Dullinger/2),scale=F))+I(scale(log(POPd.Dullinger.2000/50),scale=F))-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Extinct~Native-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Extinct-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Extinct,Native-Extinct)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Extinct,Native-Extinct)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))


nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modExtinct_betabinomheteroscpriorsdlowrest_leco2000_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomquad.extinct,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modExtinct_betabinomheteroscpriorsdlowrest_leco2000_fourthsamplewotrees_bis,file="modExtinct_betabinomheteroscpriorsdlowrest_leco2000_fourthsamplewotrees_bis.RData")
rm(modExtinct_betabinomheteroscpriorsdlowrest_leco2000_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modExtinct_betabinomheteroscpriorsdlowrest_leco2000_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger,leco2000=DICmarginal.stanmodel.heteroscbetabinom(modExtinct_betabinomheteroscpriorsdlowrest_leco2000_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
}





model.delta<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Extinct,Native-Extinct)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Extinct~Native-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Extinct-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Extinct,Native-Extinct)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Extinct,Native-Extinct)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))


nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modExtinct_betabinomheteroscpriorsdlowrest_Null_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomquad.extinct,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modExtinct_betabinomheteroscpriorsdlowrest_Null_fourthsamplewotrees_bis,file="modExtinct_betabinomheteroscpriorsdlowrest_Null_fourthsamplewotrees_bis.RData")
rm(modExtinct_betabinomheteroscpriorsdlowrest_Null_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modExtinct_betabinomheteroscpriorsdlowrest_Null_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger,Null=DICmarginal.stanmodel.heteroscbetabinom(modExtinct_betabinomheteroscpriorsdlowrest_Null_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
}












#####

model.delta<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1950aD)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Extinct,Native-Extinct)~Taxon+I(scale(log(POPd.Dullinger.1950/50),scale=F))-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1950aD)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Extinct~Native-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1950aD)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Extinct-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1950aD)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Extinct,Native-Extinct)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1950aD)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1950aD)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Extinct,Native-Extinct)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1950aD)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modExtinct_betabinomheteroscpriorsdlowrest_POPd1950l_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomquad.extinct,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modExtinct_betabinomheteroscpriorsdlowrest_POPd1950l_fourthsamplewotrees_bis,file="modExtinct_betabinomheteroscpriorsdlowrest_POPd1950l_fourthsamplewotrees_bis.RData")
rm(modExtinct_betabinomheteroscpriorsdlowrest_POPd1950l_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modExtinct_betabinomheteroscpriorsdlowrest_POPd1950l_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger,POPd1950l=DICmarginal.stanmodel.heteroscbetabinom(modExtinct_betabinomheteroscpriorsdlowrest_POPd1950l_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
}








model.delta<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1950aD)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Extinct,Native-Extinct)~Taxon+I(scale(PIB1950aD,scale=F)/400)-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1950aD)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Extinct~Native-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1950aD)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Extinct-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1950aD)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Extinct,Native-Extinct)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1950aD)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1950aD)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Extinct,Native-Extinct)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1950aD)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modExtinct_betabinomheteroscpriorsdlowrest_GDP1950a_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomquad.extinct,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modExtinct_betabinomheteroscpriorsdlowrest_GDP1950a_fourthsamplewotrees_bis,file="modExtinct_betabinomheteroscpriorsdlowrest_GDP1950a_fourthsamplewotrees_bis.RData")
rm(modExtinct_betabinomheteroscpriorsdlowrest_GDP1950a_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modExtinct_betabinomheteroscpriorsdlowrest_GDP1950a_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger,GDP1950a=DICmarginal.stanmodel.heteroscbetabinom(modExtinct_betabinomheteroscpriorsdlowrest_GDP1950a_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
}





model.delta<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1950aD)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Extinct,Native-Extinct)~Taxon+I(scale((POPd1950.Dullinger.1950/50),scale=F))-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1950aD)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Extinct~Native-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1950aD)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Extinct-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1950aD)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Extinct,Native-Extinct)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1950aD)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1950aD)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Extinct,Native-Extinct)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1950aD)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
				
				
nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modExtinct_betabinomheteroscpriorsdlowrest_POPd1950_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomquad.extinct,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modExtinct_betabinomheteroscpriorsdlowrest_POPd1950_fourthsamplewotrees_bis,file="modExtinct_betabinomheteroscpriorsdlowrest_POPd1950_fourthsamplewotrees_bis.RData")
rm(modExtinct_betabinomheteroscpriorsdlowrest_POPd1950_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modExtinct_betabinomheteroscpriorsdlowrest_POPd1950_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger,POPd1950=DICmarginal.stanmodel.heteroscbetabinom(modExtinct_betabinomheteroscpriorsdlowrest_POPd1950_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
}








model.delta<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Extinct,Native-Extinct)~Taxon+I(scale(log(POPd.Dullinger.2000/50),scale=F))-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Extinct~Native-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Extinct-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Extinct,Native-Extinct)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Extinct,Native-Extinct)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modExtinct_betabinomheteroscpriorsdlowrest_POPd2000l_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomquad.extinct,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modExtinct_betabinomheteroscpriorsdlowrest_POPd2000l_fourthsamplewotrees_bis,file="modExtinct_betabinomheteroscpriorsdlowrest_POPd2000l_fourthsamplewotrees_bis.RData")
rm(modExtinct_betabinomheteroscpriorsdlowrest_POPd2000l_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modExtinct_betabinomheteroscpriorsdlowrest_POPd2000l_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger,POPd2000l=DICmarginal.stanmodel.heteroscbetabinom(modExtinct_betabinomheteroscpriorsdlowrest_POPd2000l_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
}








model.delta<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Extinct,Native-Extinct)~Taxon+I(scale(PIB2000aD,scale=F)/2400)-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Extinct~Native-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Extinct-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Extinct,Native-Extinct)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Extinct,Native-Extinct)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modExtinct_betabinomheteroscpriorsdlowrest_GDP2000a_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomquad.extinct,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modExtinct_betabinomheteroscpriorsdlowrest_GDP2000a_fourthsamplewotrees_bis,file="modExtinct_betabinomheteroscpriorsdlowrest_GDP2000a_fourthsamplewotrees_bis.RData")
rm(modExtinct_betabinomheteroscpriorsdlowrest_GDP2000a_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modExtinct_betabinomheteroscpriorsdlowrest_GDP2000a_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger,GDP2000a=DICmarginal.stanmodel.heteroscbetabinom(modExtinct_betabinomheteroscpriorsdlowrest_GDP2000a_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
}





model.delta<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Extinct,Native-Extinct)~Taxon+I(scale((POPd2000.Dullinger.2000/100),scale=F))-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Extinct~Native-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Extinct-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Extinct,Native-Extinct)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Extinct,Native-Extinct)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
				
				
nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modExtinct_betabinomheteroscpriorsdlowrest_POPd2000_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomquad.extinct,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modExtinct_betabinomheteroscpriorsdlowrest_POPd2000_fourthsamplewotrees_bis,file="modExtinct_betabinomheteroscpriorsdlowrest_POPd2000_fourthsamplewotrees_bis.RData")
rm(modExtinct_betabinomheteroscpriorsdlowrest_POPd2000_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modExtinct_betabinomheteroscpriorsdlowrest_POPd2000_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger,POPd2000=DICmarginal.stanmodel.heteroscbetabinom(modExtinct_betabinomheteroscpriorsdlowrest_POPd2000_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
}











model.delta<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Extinct,Native-Extinct)~Taxon+I(scale(log(HANPP.2000.Dullinger),scale=F))-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Extinct~Native-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Extinct-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Extinct,Native-Extinct)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Extinct,Native-Extinct)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modExtinct_betabinomheteroscpriorsdlowrest_HANPP2000l_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomquad.extinct,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modExtinct_betabinomheteroscpriorsdlowrest_HANPP2000l_fourthsamplewotrees_bis,file="modExtinct_betabinomheteroscpriorsdlowrest_HANPP2000l_fourthsamplewotrees_bis.RData")
rm(modExtinct_betabinomheteroscpriorsdlowrest_HANPP2000l_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modExtinct_betabinomheteroscpriorsdlowrest_HANPP2000l_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger,HANPP2000l=DICmarginal.stanmodel.heteroscbetabinom(modExtinct_betabinomheteroscpriorsdlowrest_HANPP2000l_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
}



	
			
model.delta<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Extinct,Native-Extinct)~Taxon+I(scale(log(HANPP.1950.Dullinger),scale=F))-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Extinct~Native-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Extinct-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Extinct,Native-Extinct)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Extinct,Native-Extinct)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modExtinct_betabinomheteroscpriorsdlowrest_HANPP1950l_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomquad.extinct,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modExtinct_betabinomheteroscpriorsdlowrest_HANPP1950l_fourthsamplewotrees_bis,file="modExtinct_betabinomheteroscpriorsdlowrest_HANPP1950l_fourthsamplewotrees_bis.RData")
rm(modExtinct_betabinomheteroscpriorsdlowrest_HANPP1950l_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modExtinct_betabinomheteroscpriorsdlowrest_HANPP1950l_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger,HANPP1950l=DICmarginal.stanmodel.heteroscbetabinom(modExtinct_betabinomheteroscpriorsdlowrest_HANPP1950l_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
}




model.delta<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Extinct,Native-Extinct)~Taxon+I(scale(log(HANPP.1900.Dullinger),scale=F))-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Extinct~Native-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Extinct-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Extinct,Native-Extinct)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Extinct,Native-Extinct)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modExtinct_betabinomheteroscpriorsdlowrest_HANPP1900l_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomquad.extinct,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modExtinct_betabinomheteroscpriorsdlowrest_HANPP1900l_fourthsamplewotrees_bis,file="modExtinct_betabinomheteroscpriorsdlowrest_HANPP1900l_fourthsamplewotrees_bis.RData")
rm(modExtinct_betabinomheteroscpriorsdlowrest_HANPP1900l_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modExtinct_betabinomheteroscpriorsdlowrest_HANPP1900l_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger,HANPP1900l=DICmarginal.stanmodel.heteroscbetabinom(modExtinct_betabinomheteroscpriorsdlowrest_HANPP1900l_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
}


		

			
model.delta<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Extinct,Native-Extinct)~Taxon+I(scale((HANPP.2000.Dullinger/0.2),scale=F))-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Extinct~Native-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Extinct-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Extinct,Native-Extinct)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Extinct,Native-Extinct)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modExtinct_betabinomheteroscpriorsdlowrest_HANPP2000_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomquad.extinct,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modExtinct_betabinomheteroscpriorsdlowrest_HANPP2000_fourthsamplewotrees_bis,file="modExtinct_betabinomheteroscpriorsdlowrest_HANPP2000_fourthsamplewotrees_bis.RData")
rm(modExtinct_betabinomheteroscpriorsdlowrest_HANPP2000_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modExtinct_betabinomheteroscpriorsdlowrest_HANPP2000_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger,HANPP2000=DICmarginal.stanmodel.heteroscbetabinom(modExtinct_betabinomheteroscpriorsdlowrest_HANPP2000_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
}


	
			
model.delta<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Extinct,Native-Extinct)~Taxon+I(scale((HANPP.1950.Dullinger/0.2),scale=F))-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Extinct~Native-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Extinct-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Extinct,Native-Extinct)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Extinct,Native-Extinct)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modExtinct_betabinomheteroscpriorsdlowrest_HANPP1950_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomquad.extinct,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modExtinct_betabinomheteroscpriorsdlowrest_HANPP1950_fourthsamplewotrees_bis,file="modExtinct_betabinomheteroscpriorsdlowrest_HANPP1950_fourthsamplewotrees_bis.RData")
rm(modExtinct_betabinomheteroscpriorsdlowrest_HANPP1950_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modExtinct_betabinomheteroscpriorsdlowrest_HANPP1950_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger,HANPP1950=DICmarginal.stanmodel.heteroscbetabinom(modExtinct_betabinomheteroscpriorsdlowrest_HANPP1950_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
}



model.delta<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Extinct,Native-Extinct)~Taxon+I(scale((HANPP.1900.Dullinger/0.2),scale=F))-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Extinct~Native-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Extinct-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Extinct,Native-Extinct)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Extinct,Native-Extinct)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modExtinct_betabinomheteroscpriorsdlowrest_HANPP1900_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomquad.extinct,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modExtinct_betabinomheteroscpriorsdlowrest_HANPP1900_fourthsamplewotrees_bis,file="modExtinct_betabinomheteroscpriorsdlowrest_HANPP1900_fourthsamplewotrees_bis.RData")
rm(modExtinct_betabinomheteroscpriorsdlowrest_HANPP1900_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modExtinct_betabinomheteroscpriorsdlowrest_HANPP1900_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger,HANPP1900=DICmarginal.stanmodel.heteroscbetabinom(modExtinct_betabinomheteroscpriorsdlowrest_HANPP1900_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Extinct_fourthsamplewotrees_supp_muchlonger.RData")
}

















if (runDIC)
{

#write.csv(DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger,file="DICmextinct.csv")
setwd(wd.csv)
write.csv(t(t(DICmarginal1000r.extinct.fourthsamplewotrees.supp.muchlonger)),file="DICm1000rThreat_fourthsamplewotrees_supp_muchlonger_all.csv")

}

}



######## analysis of parameter estimators (Table 6)
{
library(rstan)
if (Sys.info()["sysname"]=="Windows") {setwd(wd.RData)} 
load("BSC_Data_functions.RData")

Estimates_extinct_fourthsample<-data.frame(NA,ncol=3,nrow=28)
dimnames(Estimates_extinct_fourthsample)[[2]]<-c("Summary_est","pbeta","magnitude")

attach("modExtinct_betabinomheteroscpriorsdlowrest_GDP1900al_fourthsamplewotrees_bis.RData",pos=2)

set.seed(1)
model.to.diagnose<-modExtinct_betabinomheteroscpriorsdlowrest_GDP1900al_fourthsamplewotrees_bis
temp<-interp.magnsign(model.to.diagnose,mult=1.15,index=10)
Estimates_extinct_fourthsample[1,]<-c(temp[1],temp[2],temp[3])
detach(2)

attach("modExtinct_betabinomheteroscpriorsdlowrest_GDP1950al_fourthsamplewotrees_bis.RData",pos=2)

set.seed(1)
model.to.diagnose<-modExtinct_betabinomheteroscpriorsdlowrest_GDP1950al_fourthsamplewotrees_bis
temp<-interp.magnsign(model.to.diagnose,mult=1.15,index=10)
Estimates_extinct_fourthsample[2,]<-c(temp[1],temp[2],temp[3])
detach(2)


attach("modExtinct_betabinomheteroscpriorsdlowrest_GDP2000al_fourthsamplewotrees_bis.RData",pos=2)

set.seed(1)
model.to.diagnose<-modExtinct_betabinomheteroscpriorsdlowrest_GDP2000al_fourthsamplewotrees_bis
temp<-interp.magnsign(model.to.diagnose,mult=1.15,index=10)
Estimates_extinct_fourthsample[3,]<-c(temp[1],temp[2],temp[3])
detach(2)


setwd(wd.csv)
write.csv(Estimates_extinct_fourthsample,file="Estimates_Extinct_fourthsample.csv")


}


}



########## THREATENED SPECIES
{


###### model fitting & IC calculation (Tables 2, 3 & 5)
{
if (Sys.info()["sysname"]=="Windows") {setwd(wd.RData)} 
load("BSC_Data_functions.RData")

#### parameters used to control which part of the process to do : estimation (runStan) or calculation of model comparison indices (runDIC)
runStan<-TRUE
runDIC<-TRUE



DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger<-matrix(NA,ncol=11,nrow=8)
dimnames(DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger)<-list(c(
			
			"POPd","POPdl","POPdsq","GDP1900a","GDP1900al",
			"SEAL.2009sq","SEAL.2009l","SEAL.2009"),c("DIC", "DICvar","DICmin", "IC",  "pDvar",  "pD1.WAIC",        "looic", "waic",   "propkfair",  "propkbad", "propkverybad"))






model.delta<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon+I(scale(log(PIB2000aD/400),scale=F))-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Threatened~I(Native-Extinct)-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Threatened-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Threatened,Native-Extinct-Threatened)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

nc <- 3	;		
ni <- 200000;  		
nb <- 20000 ;					
                  
nt <- 15;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modThreat_betabinomheteroscpriorsdlowrest_GDP2000al_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomter.threat,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modThreat_betabinomheteroscpriorsdlowrest_GDP2000al_fourthsamplewotrees_bis,file="modThreat_betabinomheteroscpriorsdlowrest_GDP2000al_fourthsamplewotrees_bis.RData")
rm(modThreat_betabinomheteroscpriorsdlowrest_GDP2000al_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modThreat_betabinomheteroscpriorsdlowrest_GDP2000al_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger,GDP2000al=DICmarginal.stanmodel.heteroscbetabinom(modThreat_betabinomheteroscpriorsdlowrest_GDP2000al_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
}









model.delta<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon+I(scale(log(PIB1950aD/400),scale=F))-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Threatened~I(Native-Extinct)-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Threatened-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Threatened,Native-Extinct-Threatened)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

nc <- 3	;		
ni <- 200000;  		
nb <- 20000 ;					
                  
nt <- 15;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modThreat_betabinomheteroscpriorsdlowrest_GDP1950al_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomter.threat,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modThreat_betabinomheteroscpriorsdlowrest_GDP1950al_fourthsamplewotrees_bis,file="modThreat_betabinomheteroscpriorsdlowrest_GDP1950al_fourthsamplewotrees_bis.RData")
rm(modThreat_betabinomheteroscpriorsdlowrest_GDP1950al_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modThreat_betabinomheteroscpriorsdlowrest_GDP1950al_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger,GDP1950al=DICmarginal.stanmodel.heteroscbetabinom(modThreat_betabinomheteroscpriorsdlowrest_GDP1950al_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
}




model.delta<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon+I(scale(log(PIB1900aD/400),scale=F))-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Threatened~I(Native-Extinct)-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Threatened-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Threatened,Native-Extinct-Threatened)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

nc <- 3	;		
ni <- 200000;  		
nb <- 20000 ;					
                  
nt <- 15;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modThreat_betabinomheteroscpriorsdlowrest_GDP1900al_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomter.threat,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modThreat_betabinomheteroscpriorsdlowrest_GDP1900al_fourthsamplewotrees_bis,file="modThreat_betabinomheteroscpriorsdlowrest_GDP1900al_fourthsamplewotrees_bis.RData")
rm(modThreat_betabinomheteroscpriorsdlowrest_GDP1900al_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modThreat_betabinomheteroscpriorsdlowrest_GDP1900al_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger,GDP1900al=DICmarginal.stanmodel.heteroscbetabinom(modThreat_betabinomheteroscpriorsdlowrest_GDP1900al_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
}




model.delta<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon+I(scale(log(POPd.Dullinger.1900/50),scale=F))-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Threatened~I(Native-Extinct)-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Threatened-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Threatened,Native-Extinct-Threatened)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

nc <- 3	;		
ni <- 200000;  		
nb <- 20000 ;					
                  
nt <- 15;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modThreat_betabinomheteroscpriorsdlowrest_POPd1900l_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomter.threat,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modThreat_betabinomheteroscpriorsdlowrest_POPd1900l_fourthsamplewotrees_bis,file="modThreat_betabinomheteroscpriorsdlowrest_POPd1900l_fourthsamplewotrees_bis.RData")
rm(modThreat_betabinomheteroscpriorsdlowrest_POPd1900l_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modThreat_betabinomheteroscpriorsdlowrest_POPd1900l_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger,POPd1900l=DICmarginal.stanmodel.heteroscbetabinom(modThreat_betabinomheteroscpriorsdlowrest_POPd1900l_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
}




model.delta<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon+I(scale(SEAL.2009,scale=F)/2)-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Threatened~I(Native-Extinct)-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Threatened-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Threatened,Native-Extinct-Threatened)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
nc <- 3	;		
ni <- 200000;  		
nb <- 20000 ;					
                  
nt <- 15;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modThreat_betabinomheteroscpriorsdlowrest_SEAL_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomter.threat,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modThreat_betabinomheteroscpriorsdlowrest_SEAL_fourthsamplewotrees_bis,file="modThreat_betabinomheteroscpriorsdlowrest_SEAL_fourthsamplewotrees_bis.RData")
rm(modThreat_betabinomheteroscpriorsdlowrest_SEAL_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modThreat_betabinomheteroscpriorsdlowrest_SEAL_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger,SEAL=DICmarginal.stanmodel.heteroscbetabinom(modThreat_betabinomheteroscpriorsdlowrest_SEAL_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
}







model.delta<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon+I(scale(PIB1900aD,scale=F)/400)-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Threatened~I(Native-Extinct)-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Threatened-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Threatened,Native-Extinct-Threatened)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
nc <- 3	;		
ni <- 200000;  		
nb <- 20000 ;					
                  
nt <- 15;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modThreat_betabinomheteroscpriorsdlowrest_GDP1900a_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomter.threat,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modThreat_betabinomheteroscpriorsdlowrest_GDP1900a_fourthsamplewotrees_bis,file="modThreat_betabinomheteroscpriorsdlowrest_GDP1900a_fourthsamplewotrees_bis.RData")
rm(modThreat_betabinomheteroscpriorsdlowrest_GDP1900a_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modThreat_betabinomheteroscpriorsdlowrest_GDP1900a_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger,GDP1900a=DICmarginal.stanmodel.heteroscbetabinom(modThreat_betabinomheteroscpriorsdlowrest_GDP1900a_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
}





model.delta<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon+I(scale((POPd.Dullinger.1900/50),scale=F))-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Threatened~I(Native-Extinct)-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Threatened-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Threatened,Native-Extinct-Threatened)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
				
				
nc <- 3	;		
ni <- 200000;  		
nb <- 20000 ;					
                  
nt <- 15;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modThreat_betabinomheteroscpriorsdlowrest_POPd1900_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomter.threat,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modThreat_betabinomheteroscpriorsdlowrest_POPd1900_fourthsamplewotrees_bis,file="modThreat_betabinomheteroscpriorsdlowrest_POPd1900_fourthsamplewotrees_bis.RData")
rm(modThreat_betabinomheteroscpriorsdlowrest_POPd1900_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modThreat_betabinomheteroscpriorsdlowrest_POPd1900_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger,POPd1900=DICmarginal.stanmodel.heteroscbetabinom(modThreat_betabinomheteroscpriorsdlowrest_POPd1900_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
}







model.delta<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon+I(scale(SEAL.2009/2,scale=F))+I((scale(SEAL.2009/2,scale=F))^2)-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon+I((scale(SEAL.2009/2,scale=F))^2)-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Threatened~I(Native-Extinct)-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Threatened-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Threatened,Native-Extinct-Threatened)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
				
				
nc <- 3	;		
ni <- 200000;  		
nb <- 20000 ;					
                  
nt <- 15;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modThreat_betabinomheteroscpriorsdlowrest_SEAL.2099sq_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomter.threat,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modThreat_betabinomheteroscpriorsdlowrest_SEAL.2099sq_fourthsamplewotrees_bis,file="modThreat_betabinomheteroscpriorsdlowrest_SEAL.2099sq_fourthsamplewotrees_bis.RData")
rm(modThreat_betabinomheteroscpriorsdlowrest_SEAL.2099sq_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modThreat_betabinomheteroscpriorsdlowrest_SEAL.2099sq_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger,SEAL.2099sq=DICmarginal.stanmodel.heteroscbetabinom(modThreat_betabinomheteroscpriorsdlowrest_SEAL.2099sq_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
}


model.delta<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon+I(scale(log(SEAL.2009/2),scale=F))-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Threatened~I(Native-Extinct)-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Threatened-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Threatened,Native-Extinct-Threatened)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
				
				
nc <- 3	;		
ni <- 200000;  		
nb <- 20000 ;					
                  
nt <- 15;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modThreat_betabinomheteroscpriorsdlowrest_SEAL.2009l_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomter.threat,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modThreat_betabinomheteroscpriorsdlowrest_SEAL.2009l_fourthsamplewotrees_bis,file="modThreat_betabinomheteroscpriorsdlowrest_SEAL.2009l_fourthsamplewotrees_bis.RData")
rm(modThreat_betabinomheteroscpriorsdlowrest_SEAL.2009l_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modThreat_betabinomheteroscpriorsdlowrest_SEAL.2009l_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger,SEAL.2009l=DICmarginal.stanmodel.heteroscbetabinom(modThreat_betabinomheteroscpriorsdlowrest_SEAL.2009l_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
}






model.delta<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon+I(scale(POPd.Dullinger.1900/50,scale=F))+I((scale(POPd.Dullinger.1900/50,scale=F))^2)-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
			model.eta<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon+I((scale(POPd.Dullinger.1900/50,scale=F))^2)-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Threatened~I(Native-Extinct)-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Threatened-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Threatened,Native-Extinct-Threatened)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
				
				
nc <- 3	;		
ni <- 200000;  		
nb <- 20000 ;					
                  
nt <- 15;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modThreat_betabinomheteroscpriorsdlowrest_POPdsq_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomter.threat,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modThreat_betabinomheteroscpriorsdlowrest_POPdsq_fourthsamplewotrees_bis,file="modThreat_betabinomheteroscpriorsdlowrest_POPd1900sq_fourthsamplewotrees_bis.RData")
rm(modThreat_betabinomheteroscpriorsdlowrest_POPdsq_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modThreat_betabinomheteroscpriorsdlowrest_POPdsq_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger,POPdsq=DICmarginal.stanmodel.heteroscbetabinom(modThreat_betabinomheteroscpriorsdlowrest_POPdsq_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
}






model.delta<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon+I((scale(PIB1900aD,scale=F)/400)^2)-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Threatened~I(Native-Extinct)-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Threatened-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Threatened,Native-Extinct-Threatened)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
				
				
				
nc <- 3	;		
ni <- 200000;  		
nb <- 20000 ;					
                  
nt <- 15;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modThreat_betabinomheteroscpriorsdlowrest_GDP1900asq_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomter.threat,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modThreat_betabinomheteroscpriorsdlowrest_GDP1900asq_fourthsamplewotrees_bis,file="modThreat_betabinomheteroscpriorsdlowrest_GDP1900asq_fourthsamplewotrees_bis.RData")
rm(modThreat_betabinomheteroscpriorsdlowrest_GDP1900asq_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modThreat_betabinomheteroscpriorsdlowrest_GDP1900asq_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger,GDP1900asq=DICmarginal.stanmodel.heteroscbetabinom(modThreat_betabinomheteroscpriorsdlowrest_GDP1900asq_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
}


### les deux suivants n'ont pas formellement torné mais les résultats dans xlsx sont OK


model.delta<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon+I(scale(SEAL.2009/2,scale=F))+I((scale(SEAL.2009/2,scale=F))^2)-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))

model.bidon<-glm(Threatened~I(Native-Extinct)-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Threatened-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Threatened,Native-Extinct-Threatened)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				

model.delta<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon+I(scale(SEAL.2009,scale=F))+I(scale(INC.SEAL.CA,scale=F))-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Threatened~I(Native-Extinct)-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Threatened-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Threatened,Native-Extinct-Threatened)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
				
nc <- 3	;		
ni <- 200000;  		
nb <- 20000 ;					
                  
nt <- 15;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modThreat_betabinomheteroscpriorsdlowrest_SEAL2_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomter.threat,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modThreat_betabinomheteroscpriorsdlowrest_SEAL2_fourthsamplewotrees_bis,file="modThreat_betabinomheteroscpriorsdlowrest_SEAL2_fourthsamplewotrees_bis.RData")
rm(modThreat_betabinomheteroscpriorsdlowrest_SEAL2_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modThreat_betabinomheteroscpriorsdlowrest_SEAL2_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger,SEAL2=DICmarginal.stanmodel.heteroscbetabinom(modThreat_betabinomheteroscpriorsdlowrest_SEAL2_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
}





model.delta<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon+I(scale(PIB.1950.Dullinger/2,scale=F))+I((scale(PIB.1950.Dullinger/2,scale=F))^2)-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Threatened~I(Native-Extinct)-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Threatened-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Threatened,Native-Extinct-Threatened)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
nc <- 3	;		
ni <- 200000;  		
nb <- 20000 ;					
                  
nt <- 15;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modThreat_betabinomheteroscpriorsdlowrest_Kuznets1950_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomter.threat,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modThreat_betabinomheteroscpriorsdlowrest_Kuznets1950_fourthsamplewotrees_bis,file="modThreat_betabinomheteroscpriorsdlowrest_Kuznets1950_fourthsamplewotrees_bis.RData")
rm(modThreat_betabinomheteroscpriorsdlowrest_Kuznets1950_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modThreat_betabinomheteroscpriorsdlowrest_Kuznets1950_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger,Kuznets1950=DICmarginal.stanmodel.heteroscbetabinom(modThreat_betabinomheteroscpriorsdlowrest_Kuznets1950_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
}




model.delta<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon+I(scale(log(PIB.1950.Dullinger/2),scale=F))+I((scale(log(PIB.1950.Dullinger/2),scale=F))^2)-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Threatened~I(Native-Extinct)-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Threatened-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Threatened,Native-Extinct-Threatened)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
nc <- 3	;		
ni <- 200000;  		
nb <- 20000 ;					
                  
nt <- 15;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modThreat_betabinomheteroscpriorsdlowrest_lKuznets1950_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomter.threat,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modThreat_betabinomheteroscpriorsdlowrest_lKuznets1950_fourthsamplewotrees_bis,file="modThreat_betabinomheteroscpriorsdlowrest_lKuznets1950_fourthsamplewotrees_bis.RData")
rm(modThreat_betabinomheteroscpriorsdlowrest_lKuznets1950_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modThreat_betabinomheteroscpriorsdlowrest_lKuznets1950_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger,lKuznets1950=DICmarginal.stanmodel.heteroscbetabinom(modThreat_betabinomheteroscpriorsdlowrest_lKuznets1950_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
}




model.delta<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon+I(scale(PIB1950aD,scale=F)/400)+I((scale(PIB1950aD,scale=F)/400)^2)-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Threatened~I(Native-Extinct)-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Threatened-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Threatened,Native-Extinct-Threatened)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
nc <- 3	;		
ni <- 200000;  		
nb <- 20000 ;					
                  
nt <- 15;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modThreat_betabinomheteroscpriorsdlowrest_Kuznets1950a_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomter.threat,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modThreat_betabinomheteroscpriorsdlowrest_Kuznets1950a_fourthsamplewotrees_bis,file="modThreat_betabinomheteroscpriorsdlowrest_Kuznets1950a_fourthsamplewotrees_bis.RData")
rm(modThreat_betabinomheteroscpriorsdlowrest_Kuznets1950a_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modThreat_betabinomheteroscpriorsdlowrest_Kuznets1950a_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger,Kuznets1950a=DICmarginal.stanmodel.heteroscbetabinom(modThreat_betabinomheteroscpriorsdlowrest_Kuznets1950a_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
}


model.delta<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon+I(scale(log(PIB1950aD/400),scale=F))+I((scale(log(PIB1950aD/400),scale=F))^2)-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Threatened~I(Native-Extinct)-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Threatened-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Threatened,Native-Extinct-Threatened)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
				
nc <- 3	;		
ni <- 200000;  		
nb <- 20000 ;					
                  
nt <- 15;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modThreat_betabinomheteroscpriorsdlowrest_lKuznets1950a_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomter.threat,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modThreat_betabinomheteroscpriorsdlowrest_lKuznets1950a_fourthsamplewotrees_bis,file="modThreat_betabinomheteroscpriorsdlowrest_lKuznets1950a_fourthsamplewotrees_bis.RData")
rm(modThreat_betabinomheteroscpriorsdlowrest_lKuznets1950a_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modThreat_betabinomheteroscpriorsdlowrest_lKuznets1950a_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger,lKuznets1950a=DICmarginal.stanmodel.heteroscbetabinom(modThreat_betabinomheteroscpriorsdlowrest_lKuznets1950a_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
}





model.delta<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon+I(scale((PIB.1950.Dullinger/2),scale=F))+I(scale((POPd.Dullinger.1950/50),scale=F))-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Threatened~I(Native-Extinct)-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Threatened-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Threatened,Native-Extinct-Threatened)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
nc <- 3	;		
ni <- 200000;  		
nb <- 20000 ;					
                  
nt <- 15;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modThreat_betabinomheteroscpriorsdlowrest_eco1950_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomter.threat,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modThreat_betabinomheteroscpriorsdlowrest_eco1950_fourthsamplewotrees_bis,file="modThreat_betabinomheteroscpriorsdlowrest_eco1950_fourthsamplewotrees_bis.RData")
rm(modThreat_betabinomheteroscpriorsdlowrest_eco1950_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modThreat_betabinomheteroscpriorsdlowrest_eco1950_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger,eco1950=DICmarginal.stanmodel.heteroscbetabinom(modThreat_betabinomheteroscpriorsdlowrest_eco1950_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
}

model.delta<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon+I(scale(log(PIB.1950.Dullinger/2),scale=F))+I(scale(log(POPd.Dullinger.1950/50),scale=F))-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Threatened~I(Native-Extinct)-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Threatened-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Threatened,Native-Extinct-Threatened)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
nc <- 3	;		
ni <- 200000;  		
nb <- 20000 ;					
                  
nt <- 15;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modThreat_betabinomheteroscpriorsdlowrest_leco1950_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomter.threat,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modThreat_betabinomheteroscpriorsdlowrest_leco1950_fourthsamplewotrees_bis,file="modThreat_betabinomheteroscpriorsdlowrest_leco1950_fourthsamplewotrees_bis.RData")
rm(modThreat_betabinomheteroscpriorsdlowrest_leco1950_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modThreat_betabinomheteroscpriorsdlowrest_leco1950_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger,leco1950=DICmarginal.stanmodel.heteroscbetabinom(modThreat_betabinomheteroscpriorsdlowrest_leco1950_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
}












model.delta<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon+I(scale(PIB.1900.Dullinger/2,scale=F))+I((scale(PIB.1900.Dullinger/2,scale=F))^2)-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Threatened~I(Native-Extinct)-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Threatened-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Threatened,Native-Extinct-Threatened)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
				
nc <- 3	;		
ni <- 200000;  		
nb <- 20000 ;					
                  
nt <- 15;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modThreat_betabinomheteroscpriorsdlowrest_Kuznets1900_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomter.threat,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modThreat_betabinomheteroscpriorsdlowrest_Kuznets1900_fourthsamplewotrees_bis,file="modThreat_betabinomheteroscpriorsdlowrest_Kuznets1900_fourthsamplewotrees_bis.RData")
rm(modThreat_betabinomheteroscpriorsdlowrest_Kuznets1900_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modThreat_betabinomheteroscpriorsdlowrest_Kuznets1900_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger,Kuznets1900=DICmarginal.stanmodel.heteroscbetabinom(modThreat_betabinomheteroscpriorsdlowrest_Kuznets1900_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
}


model.delta<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon+I(scale(log(PIB.1900.Dullinger/2),scale=F))+I((scale(log(PIB.1900.Dullinger/2),scale=F))^2)-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Threatened~I(Native-Extinct)-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Threatened-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Threatened,Native-Extinct-Threatened)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
nc <- 3	;		
ni <- 200000;  		
nb <- 20000 ;					
                  
nt <- 15;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modThreat_betabinomheteroscpriorsdlowrest_lKuznets1900_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomter.threat,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modThreat_betabinomheteroscpriorsdlowrest_lKuznets1900_fourthsamplewotrees_bis,file="modThreat_betabinomheteroscpriorsdlowrest_lKuznets1900_fourthsamplewotrees_bis.RData")
rm(modThreat_betabinomheteroscpriorsdlowrest_lKuznets1900_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modThreat_betabinomheteroscpriorsdlowrest_lKuznets1900_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger,lKuznets1900=DICmarginal.stanmodel.heteroscbetabinom(modThreat_betabinomheteroscpriorsdlowrest_lKuznets1900_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
}




model.delta<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon+I(scale(PIB1900aD,scale=F)/400)+I((scale(PIB1900aD,scale=F)/400)^2)-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Threatened~I(Native-Extinct)-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Threatened-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Threatened,Native-Extinct-Threatened)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
nc <- 3	;		
ni <- 200000;  		
nb <- 20000 ;					
                  
nt <- 15;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modThreat_betabinomheteroscpriorsdlowrest_Kuznets1900a_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomter.threat,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modThreat_betabinomheteroscpriorsdlowrest_Kuznets1900a_fourthsamplewotrees_bis,file="modThreat_betabinomheteroscpriorsdlowrest_Kuznets1900a_fourthsamplewotrees_bis.RData")
rm(modThreat_betabinomheteroscpriorsdlowrest_Kuznets1900a_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modThreat_betabinomheteroscpriorsdlowrest_Kuznets1900a_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger,Kuznets1900a=DICmarginal.stanmodel.heteroscbetabinom(modThreat_betabinomheteroscpriorsdlowrest_Kuznets1900a_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
}









##############



model.delta<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon+I(scale(log(PIB1900aD/400),scale=F))+I((scale(log(PIB1900aD/400),scale=F))^2)-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Threatened~I(Native-Extinct)-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Threatened-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Threatened,Native-Extinct-Threatened)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
nc <- 3	;		
ni <- 200000;  		
nb <- 20000 ;					
                  
nt <- 15;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modThreat_betabinomheteroscpriorsdlowrest_lKuznets1900a_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomter.threat,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modThreat_betabinomheteroscpriorsdlowrest_lKuznets1900a_fourthsamplewotrees_bis,file="modThreat_betabinomheteroscpriorsdlowrest_lKuznets1900a_fourthsamplewotrees_bis.RData")
rm(modThreat_betabinomheteroscpriorsdlowrest_lKuznets1900a_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modThreat_betabinomheteroscpriorsdlowrest_lKuznets1900a_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger,lKuznets1900a=DICmarginal.stanmodel.heteroscbetabinom(modThreat_betabinomheteroscpriorsdlowrest_lKuznets1900a_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
}




















model.delta<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon+I(scale((PIB.1900.Dullinger/2),scale=F))+I(scale((POPd.Dullinger.1900/50),scale=F))-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Threatened~I(Native-Extinct)-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Threatened-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Threatened,Native-Extinct-Threatened)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
nc <- 3	;		
ni <- 200000;  		
nb <- 20000 ;					
                  
nt <- 15;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modThreat_betabinomheteroscpriorsdlowrest_eco1900_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomter.threat,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modThreat_betabinomheteroscpriorsdlowrest_eco1900_fourthsamplewotrees_bis,file="modThreat_betabinomheteroscpriorsdlowrest_eco1900_fourthsamplewotrees_bis.RData")
rm(modThreat_betabinomheteroscpriorsdlowrest_eco1900_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modThreat_betabinomheteroscpriorsdlowrest_eco1900_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger,eco1900=DICmarginal.stanmodel.heteroscbetabinom(modThreat_betabinomheteroscpriorsdlowrest_eco1900_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
}






model.delta<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon+I(scale(log(PIB.1900.Dullinger/2),scale=F))+I(scale(log(POPd.Dullinger.1900/50),scale=F))-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Threatened~I(Native-Extinct)-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Threatened-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Threatened,Native-Extinct-Threatened)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
nc <- 3	;		
ni <- 200000;  		
nb <- 20000 ;					
                  
nt <- 15;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modThreat_betabinomheteroscpriorsdlowrest_leco1900_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomter.threat,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modThreat_betabinomheteroscpriorsdlowrest_leco1900_fourthsamplewotrees_bis,file="modThreat_betabinomheteroscpriorsdlowrest_leco1900_fourthsamplewotrees_bis.RData")
rm(modThreat_betabinomheteroscpriorsdlowrest_leco1900_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modThreat_betabinomheteroscpriorsdlowrest_leco1900_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger,leco1900=DICmarginal.stanmodel.heteroscbetabinom(modThreat_betabinomheteroscpriorsdlowrest_leco1900_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
}






####
model.delta<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon+I(scale(PIB.2000.Dullinger/7,scale=F))+I((scale(PIB.2000.Dullinger/7,scale=F))^2)-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Threatened~I(Native-Extinct)-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Threatened-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Threatened,Native-Extinct-Threatened)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))


nc <- 3	;		
ni <- 200000;  		
nb <- 20000 ;					
                  
nt <- 15;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modThreat_betabinomheteroscpriorsdlowrest_Kuznets2000_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomter.threat,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modThreat_betabinomheteroscpriorsdlowrest_Kuznets2000_fourthsamplewotrees_bis,file="modThreat_betabinomheteroscpriorsdlowrest_Kuznets2000_fourthsamplewotrees_bis.RData")
rm(modThreat_betabinomheteroscpriorsdlowrest_Kuznets2000_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modThreat_betabinomheteroscpriorsdlowrest_Kuznets2000_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger,Kuznets2000=DICmarginal.stanmodel.heteroscbetabinom(modThreat_betabinomheteroscpriorsdlowrest_Kuznets2000_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
}





model.delta<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon+I(scale(log(PIB.2000.Dullinger/2),scale=F))+I((scale(log(PIB.2000.Dullinger/2),scale=F))^2)-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Threatened~I(Native-Extinct)-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Threatened-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Threatened,Native-Extinct-Threatened)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

nc <- 3	;		
ni <- 200000;  		
nb <- 20000 ;					
                  
nt <- 15;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modThreat_betabinomheteroscpriorsdlowrest_lKuznets2000_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomter.threat,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modThreat_betabinomheteroscpriorsdlowrest_lKuznets2000_fourthsamplewotrees_bis,file="modThreat_betabinomheteroscpriorsdlowrest_lKuznets2000_fourthsamplewotrees_bis.RData")
rm(modThreat_betabinomheteroscpriorsdlowrest_lKuznets2000_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modThreat_betabinomheteroscpriorsdlowrest_lKuznets2000_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger,lKuznets2000=DICmarginal.stanmodel.heteroscbetabinom(modThreat_betabinomheteroscpriorsdlowrest_lKuznets2000_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
}




model.delta<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon+I(scale(PIB2000aD,scale=F)/2400)+I((scale(PIB2000aD,scale=F)/2400)^2)-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Threatened~I(Native-Extinct)-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Threatened-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Threatened,Native-Extinct-Threatened)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))


nc <- 3	;		
ni <- 200000;  		
nb <- 20000 ;					
                  
nt <- 15;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modThreat_betabinomheteroscpriorsdlowrest_Kuznets2000a_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomter.threat,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modThreat_betabinomheteroscpriorsdlowrest_Kuznets2000a_fourthsamplewotrees_bis,file="modThreat_betabinomheteroscpriorsdlowrest_Kuznets2000a_fourthsamplewotrees_bis.RData")
rm(modThreat_betabinomheteroscpriorsdlowrest_Kuznets2000a_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modThreat_betabinomheteroscpriorsdlowrest_Kuznets2000a_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger,Kuznets2000a=DICmarginal.stanmodel.heteroscbetabinom(modThreat_betabinomheteroscpriorsdlowrest_Kuznets2000a_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
}






model.delta<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon+I(scale(log(PIB2000aD/400),scale=F))+I((scale(log(PIB2000aD/400),scale=F))^2)-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Threatened~I(Native-Extinct)-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Threatened-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Threatened,Native-Extinct-Threatened)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))


nc <- 3	;		
ni <- 200000;  		
nb <- 20000 ;					
                  
nt <- 15;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modThreat_betabinomheteroscpriorsdlowrest_lKuznets2000a_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomter.threat,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modThreat_betabinomheteroscpriorsdlowrest_lKuznets2000a_fourthsamplewotrees_bis,file="modThreat_betabinomheteroscpriorsdlowrest_lKuznets2000a_fourthsamplewotrees_bis.RData")
rm(modThreat_betabinomheteroscpriorsdlowrest_lKuznets2000a_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modThreat_betabinomheteroscpriorsdlowrest_lKuznets2000a_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger,lKuznets2000a=DICmarginal.stanmodel.heteroscbetabinom(modThreat_betabinomheteroscpriorsdlowrest_lKuznets2000a_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
}



model.delta<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon+I(scale((PIB.2000.Dullinger/7),scale=F))+I(scale((POPd.Dullinger.2000/100),scale=F))-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Threatened~I(Native-Extinct)-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Threatened-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Threatened,Native-Extinct-Threatened)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))


nc <- 3	;		
ni <- 200000;  		
nb <- 20000 ;					
                  
nt <- 15;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modThreat_betabinomheteroscpriorsdlowrest_eco2000_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomter.threat,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modThreat_betabinomheteroscpriorsdlowrest_eco2000_fourthsamplewotrees_bis,file="modThreat_betabinomheteroscpriorsdlowrest_eco2000_fourthsamplewotrees_bis.RData")
rm(modThreat_betabinomheteroscpriorsdlowrest_eco2000_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modThreat_betabinomheteroscpriorsdlowrest_eco2000_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger,eco2000=DICmarginal.stanmodel.heteroscbetabinom(modThreat_betabinomheteroscpriorsdlowrest_eco2000_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
}




#load("BSC_Threat_fourthsamplewotrees.RData")
#save.image("BSC_Threat_fourthsamplewotrees.RData")

model.delta<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon+I(scale(log(PIB.2000.Dullinger/2),scale=F))+I(scale(log(POPd.Dullinger.2000/50),scale=F))-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Threatened~I(Native-Extinct)-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Threatened-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Threatened,Native-Extinct-Threatened)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))


nc <- 3	;		
ni <- 200000;  		
nb <- 20000 ;					
                  
nt <- 15;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modThreat_betabinomheteroscpriorsdlowrest_leco2000_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomter.threat,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modThreat_betabinomheteroscpriorsdlowrest_leco2000_fourthsamplewotrees_bis,file="modThreat_betabinomheteroscpriorsdlowrest_leco2000_fourthsamplewotrees_bis.RData")
rm(modThreat_betabinomheteroscpriorsdlowrest_leco2000_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modThreat_betabinomheteroscpriorsdlowrest_leco2000_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger,leco2000=DICmarginal.stanmodel.heteroscbetabinom(modThreat_betabinomheteroscpriorsdlowrest_leco2000_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
}





model.delta<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Threatened~I(Native-Extinct)-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Threatened-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Threatened,Native-Extinct-Threatened)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))


nc <- 3	;		
ni <- 200000;  		
nb <- 20000 ;					
                  
nt <- 15;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modThreat_betabinomheteroscpriorsdlowrest_Null_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomter.threat,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modThreat_betabinomheteroscpriorsdlowrest_Null_fourthsamplewotrees_bis,file="modThreat_betabinomheteroscpriorsdlowrest_Null_fourthsamplewotrees_bis.RData")
rm(modThreat_betabinomheteroscpriorsdlowrest_Null_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modThreat_betabinomheteroscpriorsdlowrest_Null_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger,Null=DICmarginal.stanmodel.heteroscbetabinom(modThreat_betabinomheteroscpriorsdlowrest_Null_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
}








############## essai maintennat version linéaire pour faire le pendant à Holland et al. 2009


model.delta<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon+I(scale(log(PIB.2000.Dullinger/2),scale=F))+I(scale(log(POPd.Dullinger.2000/50),scale=F))-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Threatened~I(Native-Extinct)-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Threatened-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Threatened,Native-Extinct-Threatened)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))


nc <- 3	;		
ni <- 200000;  		
nb <- 20000 ;					
                  
nt <- 15;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modThreat_linear_leco2000_fourthsamplewotrees_bis <- stan(model_code = stanmodel.linear.eqheteroscbetabinom.woeaCOU.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomter.threat,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modThreat_linear_leco2000_fourthsamplewotrees_bis,file="modThreat_linear_leco2000_fourthsamplewotrees_bis.RData")
rm(modThreat_linear_leco2000_fourthsamplewotrees_bis)
}








model.delta<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1950aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon+I(scale(log(POPd.Dullinger.1950/50),scale=F))-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1950aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Threatened~I(Native-Extinct)-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1950aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Threatened-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1950aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Threatened,Native-Extinct-Threatened)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1950aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1950aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1950aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

nc <- 3	;		
ni <- 200000;  		
nb <- 20000 ;					
                  
nt <- 15;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modThreat_betabinomheteroscpriorsdlowrest_POPd1950l_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomter.threat,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modThreat_betabinomheteroscpriorsdlowrest_POPd1950l_fourthsamplewotrees_bis,file="modThreat_betabinomheteroscpriorsdlowrest_POPd1950l_fourthsamplewotrees_bis.RData")
rm(modThreat_betabinomheteroscpriorsdlowrest_POPd1950l_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modThreat_betabinomheteroscpriorsdlowrest_POPd1950l_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger,POPd1950l=DICmarginal.stanmodel.heteroscbetabinom(modThreat_betabinomheteroscpriorsdlowrest_POPd1950l_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
}







model.delta<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1950aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon+I(scale(PIB1950aD,scale=F)/400)-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1950aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Threatened~I(Native-Extinct)-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1950aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Threatened-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1950aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Threatened,Native-Extinct-Threatened)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1950aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1950aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1950aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
nc <- 3	;		
ni <- 200000;  		
nb <- 20000 ;					
                  
nt <- 15;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modThreat_betabinomheteroscpriorsdlowrest_GDP1950a_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomter.threat,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modThreat_betabinomheteroscpriorsdlowrest_GDP1950a_fourthsamplewotrees_bis,file="modThreat_betabinomheteroscpriorsdlowrest_GDP1950a_fourthsamplewotrees_bis.RData")
rm(modThreat_betabinomheteroscpriorsdlowrest_GDP1950a_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modThreat_betabinomheteroscpriorsdlowrest_GDP1950a_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger,GDP1950a=DICmarginal.stanmodel.heteroscbetabinom(modThreat_betabinomheteroscpriorsdlowrest_GDP1950a_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
}





model.delta<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1950aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon+I(scale((POPd.Dullinger.1950/50),scale=F))-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1950aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Threatened~I(Native-Extinct)-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1950aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Threatened-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1950aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Threatened,Native-Extinct-Threatened)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1950aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1950aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1950aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
				
				
nc <- 3	;		
ni <- 200000;  		
nb <- 20000 ;					
                  
nt <- 15;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modThreat_betabinomheteroscpriorsdlowrest_POPd1950_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomter.threat,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modThreat_betabinomheteroscpriorsdlowrest_POPd1950_fourthsamplewotrees_bis,file="modThreat_betabinomheteroscpriorsdlowrest_POPd1950_fourthsamplewotrees_bis.RData")
rm(modThreat_betabinomheteroscpriorsdlowrest_POPd1950_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modThreat_betabinomheteroscpriorsdlowrest_POPd1950_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger,POPd1950=DICmarginal.stanmodel.heteroscbetabinom(modThreat_betabinomheteroscpriorsdlowrest_POPd1950_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
}






model.delta<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon+I(scale(log(POPd.Dullinger.2000/50),scale=F))-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Threatened~I(Native-Extinct)-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Threatened-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Threatened,Native-Extinct-Threatened)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

nc <- 3	;		
ni <- 200000;  		
nb <- 20000 ;					
                  
nt <- 15;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modThreat_betabinomheteroscpriorsdlowrest_POPd2000l_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomter.threat,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modThreat_betabinomheteroscpriorsdlowrest_POPd2000l_fourthsamplewotrees_bis,file="modThreat_betabinomheteroscpriorsdlowrest_POPd2000l_fourthsamplewotrees_bis.RData")
rm(modThreat_betabinomheteroscpriorsdlowrest_POPd2000l_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modThreat_betabinomheteroscpriorsdlowrest_POPd2000l_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger,POPd2000l=DICmarginal.stanmodel.heteroscbetabinom(modThreat_betabinomheteroscpriorsdlowrest_POPd2000l_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
}







model.delta<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon+I(scale(PIB2000aD,scale=F)/2400)-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Threatened~I(Native-Extinct)-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Threatened-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Threatened,Native-Extinct-Threatened)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
nc <- 3	;		
ni <- 200000;  		
nb <- 20000 ;					
                  
nt <- 15;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modThreat_betabinomheteroscpriorsdlowrest_GDP2000a_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomter.threat,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modThreat_betabinomheteroscpriorsdlowrest_GDP2000a_fourthsamplewotrees_bis,file="modThreat_betabinomheteroscpriorsdlowrest_GDP2000a_fourthsamplewotrees_bis.RData")
rm(modThreat_betabinomheteroscpriorsdlowrest_GDP2000a_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modThreat_betabinomheteroscpriorsdlowrest_GDP2000a_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger,GDP2000a=DICmarginal.stanmodel.heteroscbetabinom(modThreat_betabinomheteroscpriorsdlowrest_GDP2000a_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
}





model.delta<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon+I(scale((POPd.Dullinger.2000/50),scale=F))-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Threatened~I(Native-Extinct)-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Threatened-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Threatened,Native-Extinct-Threatened)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
				
				
nc <- 3	;		
ni <- 200000;  		
nb <- 20000 ;					
                  
nt <- 15;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modThreat_betabinomheteroscpriorsdlowrest_POPd2000_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomter.threat,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modThreat_betabinomheteroscpriorsdlowrest_POPd2000_fourthsamplewotrees_bis,file="modThreat_betabinomheteroscpriorsdlowrest_POPd2000_fourthsamplewotrees_bis.RData")
rm(modThreat_betabinomheteroscpriorsdlowrest_POPd2000_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modThreat_betabinomheteroscpriorsdlowrest_POPd2000_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger,POPd2000=DICmarginal.stanmodel.heteroscbetabinom(modThreat_betabinomheteroscpriorsdlowrest_POPd2000_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
}










model.delta<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon+I(scale(log(HANPP.2000.Dullinger),scale=F))-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Threatened~I(Native-Extinct)-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Threatened-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Threatened,Native-Extinct-Threatened)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modThreat_betabinomheteroscpriorsdlowrest_HANPP2000l_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomter.threat,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modThreat_betabinomheteroscpriorsdlowrest_HANPP2000l_fourthsamplewotrees_bis,file="modThreat_betabinomheteroscpriorsdlowrest_HANPP2000l_fourthsamplewotrees_bis.RData")
rm(modThreat_betabinomheteroscpriorsdlowrest_HANPP2000l_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modThreat_betabinomheteroscpriorsdlowrest_HANPP2000l_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger,HANPP2000l=DICmarginal.stanmodel.heteroscbetabinom(modThreat_betabinomheteroscpriorsdlowrest_HANPP2000l_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
}



			
model.delta<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon+I(scale(log(HANPP.1950.Dullinger),scale=F))-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Threatened~I(Native-Extinct)-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Threatened-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Threatened,Native-Extinct-Threatened)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modThreat_betabinomheteroscpriorsdlowrest_HANPP1950l_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomter.threat,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modThreat_betabinomheteroscpriorsdlowrest_HANPP1950l_fourthsamplewotrees_bis,file="modThreat_betabinomheteroscpriorsdlowrest_HANPP1950l_fourthsamplewotrees_bis.RData")
rm(modThreat_betabinomheteroscpriorsdlowrest_HANPP1950l_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modThreat_betabinomheteroscpriorsdlowrest_HANPP1950l_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger,HANPP1950l=DICmarginal.stanmodel.heteroscbetabinom(modThreat_betabinomheteroscpriorsdlowrest_HANPP1950l_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
}



			
	

model.delta<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon+I(scale(log(HANPP.1900.Dullinger),scale=F))-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Threatened~I(Native-Extinct)-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Threatened-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Threatened,Native-Extinct-Threatened)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modThreat_betabinomheteroscpriorsdlowrest_HANPP1900l_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomter.threat,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modThreat_betabinomheteroscpriorsdlowrest_HANPP1900l_fourthsamplewotrees_bis,file="modThreat_betabinomheteroscpriorsdlowrest_HANPP1900l_fourthsamplewotrees_bis.RData")
rm(modThreat_betabinomheteroscpriorsdlowrest_HANPP1900l_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modThreat_betabinomheteroscpriorsdlowrest_HANPP1900l_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger,HANPP1900l=DICmarginal.stanmodel.heteroscbetabinom(modThreat_betabinomheteroscpriorsdlowrest_HANPP1900l_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
}


	

			
model.delta<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon+I(scale((HANPP.2000.Dullinger/0.2),scale=F))-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Threatened~I(Native-Extinct)-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Threatened-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Threatened,Native-Extinct-Threatened)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modThreat_betabinomheteroscpriorsdlowrest_HANPP2000_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomter.threat,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modThreat_betabinomheteroscpriorsdlowrest_HANPP2000_fourthsamplewotrees_bis,file="modThreat_betabinomheteroscpriorsdlowrest_HANPP2000_fourthsamplewotrees_bis.RData")
rm(modThreat_betabinomheteroscpriorsdlowrest_HANPP2000_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modThreat_betabinomheteroscpriorsdlowrest_HANPP2000_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger,HANPP2000=DICmarginal.stanmodel.heteroscbetabinom(modThreat_betabinomheteroscpriorsdlowrest_HANPP2000_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
}

			
model.delta<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon+I(scale((HANPP.1950.Dullinger/0.2),scale=F))-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Threatened~I(Native-Extinct)-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Threatened-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Threatened,Native-Extinct-Threatened)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modThreat_betabinomheteroscpriorsdlowrest_HANPP1950_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomter.threat,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modThreat_betabinomheteroscpriorsdlowrest_HANPP1950_fourthsamplewotrees_bis,file="modThreat_betabinomheteroscpriorsdlowrest_HANPP1950_fourthsamplewotrees_bis.RData")
rm(modThreat_betabinomheteroscpriorsdlowrest_HANPP1950_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modThreat_betabinomheteroscpriorsdlowrest_HANPP1950_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger,HANPP1950=DICmarginal.stanmodel.heteroscbetabinom(modThreat_betabinomheteroscpriorsdlowrest_HANPP1950_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
}




model.delta<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon+I(scale((HANPP.1900.Dullinger/0.2),scale=F))-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Threatened~I(Native-Extinct)-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Threatened-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Threatened,Native-Extinct-Threatened)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modThreat_betabinomheteroscpriorsdlowrest_HANPP1900_fourthsamplewotrees_bis <- stan(model_code = stanmodel.heteroscbetabinom.priorsdlowrestricted, model_name = "Stan_binom", data = win.data,init = inits.betabinomter.threat,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modThreat_betabinomheteroscpriorsdlowrest_HANPP1900_fourthsamplewotrees_bis,file="modThreat_betabinomheteroscpriorsdlowrest_HANPP1900_fourthsamplewotrees_bis.RData")
rm(modThreat_betabinomheteroscpriorsdlowrest_HANPP1900_fourthsamplewotrees_bis)
}

if (runDIC)
{attach("modThreat_betabinomheteroscpriorsdlowrest_HANPP1900_fourthsamplewotrees_bis.RData",pos=2)

DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger<-rbind(DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger,HANPP1900=DICmarginal.stanmodel.heteroscbetabinom(modThreat_betabinomheteroscpriorsdlowrest_HANPP1900_fourthsamplewotrees_bis,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
save.image("BSC_Threat_fourthsamplewotrees_supp_muchlonger.RData")
}



			


















if (runDIC)
{



#write.csv(DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger,file="DICmthreat.csv")

setwd(wd.csv)
write.csv(t(t(DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger)),file="DICm1000rThreat_fourthsamplewotrees_supp_muchlonger_all.csv")

																		
				

}


}




######## analysis of parameter estimators (Table 6)
{
library(rstan)
if (Sys.info()["sysname"]=="Windows") {setwd(wd.RData)} 
load("BSC_Data_functions.RData")

Estimates_Threat_fourthsample<-data.frame(NA,ncol=3,nrow=28)
dimnames(Estimates_Threat_fourthsample)[[2]]<-c("Summary_est","pbeta","magnitude")

attach("modThreat_betabinomheteroscpriorsdlowrest_GDP1900al_fourthsamplewotrees_bis.RData",pos=2)

set.seed(1)
model.to.diagnose<-modThreat_betabinomheteroscpriorsdlowrest_GDP1900al_fourthsamplewotrees_bis
temp<-interp.magnsign(model.to.diagnose,mult=1.15,index=10)
Estimates_Threat_fourthsample[1,]<-c(temp[1],temp[2],temp[3])
detach(2)

attach("modThreat_betabinomheteroscpriorsdlowrest_GDP1950al_fourthsamplewotrees_bis.RData",pos=2)

set.seed(1)
model.to.diagnose<-modThreat_betabinomheteroscpriorsdlowrest_GDP1950al_fourthsamplewotrees_bis
temp<-interp.magnsign(model.to.diagnose,mult=1.15,index=10)
Estimates_Threat_fourthsample[2,]<-c(temp[1],temp[2],temp[3])
detach(2)


attach("modThreat_betabinomheteroscpriorsdlowrest_GDP2000al_fourthsamplewotrees_bis.RData",pos=2)

set.seed(1)
model.to.diagnose<-modThreat_betabinomheteroscpriorsdlowrest_GDP2000al_fourthsamplewotrees_bis
temp<-interp.magnsign(model.to.diagnose,mult=1.15,index=10)
Estimates_Threat_fourthsample[3,]<-c(temp[1],temp[2],temp[3])
detach(2)

setwd(wd.csv)
write.csv(Estimates_Threat_fourthsample,file="Estimates_Threat_fourthsample.csv")



}

}


				

########## PROTECTED AREAS
{


### model fitting & IC calculation (Table 2)

{
if (Sys.info()["sysname"]=="Windows") {setwd(wd.RData)} 
load("BSC_Data_functions.RData")

#### parameters used to control which part of the process to do : estimation (runStan) or calculation of model comparison indices (runDIC)
runStan<-TRUE
runDIC<-TRUE


DICmarginal1000r.PA.fourthsample.supp.muchlonger<-matrix(NA,ncol=11,nrow=1)
dimnames(DICmarginal1000r.PA.fourthsample.supp.muchlonger)<-list(c(
			
			"SEAL.2009"),c("DIC", "DICvar","DICmin", "IC",  "pDvar",  "pD1.WAIC",        "looic", "waic",   "propkfair",  "propkbad", "propkverybad"))



			
			
			
					
model.delta<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.eta<-glm(I(PA/TotArea)~NumPA+I(scale(log(PIB2000aD/400),scale=F))-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.bidon<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.bidonbis<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.COU<-glm(I(PA/TotArea)~COU-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.Ptype<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.sdinner<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))

			win.data <-list(Y = (model.delta)$y,
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pardelta","pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modPA_GDP2000al_fourthsample <- stan(model_code = stanmodel.delta1, model_name = "Stan_binom", data = win.data,init = initsPAbis,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modPA_GDP2000al_fourthsample,file="modPA_GDP2000al_fourthsample.RData")
rm(modPA_GDP2000al_fourthsample)
}

if (runDIC)
{attach("modPA_GDP2000al_fourthsample.RData",pos=2)

DICmarginal1000r.PA.fourthsample.supp.muchlonger<-rbind(DICmarginal1000r.PA.fourthsample.supp.muchlonger,GDP2000al=DICmarginal.ZIbeta(modPA_GDP2000al_fourthsample,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_PA_fourthsample_supp_muchlonger.RData")
save.image("BSC_PA_fourthsample_supp_muchlonger.RData")
}

		
			
model.delta<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.eta<-glm(I(PA/TotArea)~NumPA+I(scale(log(PIB1950aD/400),scale=F))-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.bidon<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.bidonbis<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.COU<-glm(I(PA/TotArea)~COU-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.Ptype<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.sdinner<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))

			win.data <-list(Y = (model.delta)$y,
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pardelta","pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modPA_GDP1950al_fourthsample <- stan(model_code = stanmodel.delta1, model_name = "Stan_binom", data = win.data,init = initsPAbis,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modPA_GDP1950al_fourthsample,file="modPA_GDP1950al_fourthsample.RData")
rm(modPA_GDP1950al_fourthsample)
}

if (runDIC)
{attach("modPA_GDP1950al_fourthsample.RData",pos=2)

DICmarginal1000r.PA.fourthsample.supp.muchlonger<-rbind(DICmarginal1000r.PA.fourthsample.supp.muchlonger,GDP1950al=DICmarginal.ZIbeta(modPA_GDP1950al_fourthsample,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_PA_fourthsample_supp_muchlonger.RData")
save.image("BSC_PA_fourthsample_supp_muchlonger.RData")
}


	


model.delta<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.eta<-glm(I(PA/TotArea)~NumPA+I(scale(log(PIB1900aD/400),scale=F))-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.bidon<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.bidonbis<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.COU<-glm(I(PA/TotArea)~COU-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.Ptype<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.sdinner<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))

			win.data <-list(Y = (model.delta)$y,
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pardelta","pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modPA_GDP1900al_fourthsample <- stan(model_code = stanmodel.delta1, model_name = "Stan_binom", data = win.data,init = initsPAbis,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modPA_GDP1900al_fourthsample,file="modPA_GDP1900al_fourthsample.RData")
rm(modPA_GDP1900al_fourthsample)
}

if (runDIC)
{attach("modPA_GDP1900al_fourthsample.RData",pos=2)

DICmarginal1000r.PA.fourthsample.supp.muchlonger<-rbind(DICmarginal1000r.PA.fourthsample.supp.muchlonger,GDP1900al=DICmarginal.ZIbeta(modPA_GDP1900al_fourthsample,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_PA_fourthsample_supp_muchlonger.RData")
save.image("BSC_PA_fourthsample_supp_muchlonger.RData")
}



			
model.delta<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.eta<-glm(I(PA/TotArea)~NumPA+I(scale(log(HANPP.2000.Dullinger),scale=F))-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.bidon<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.bidonbis<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.COU<-glm(I(PA/TotArea)~COU-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.Ptype<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.sdinner<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))

			win.data <-list(Y = (model.delta)$y,
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pardelta","pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modPA_HANPP2000l_fourthsample <- stan(model_code = stanmodel.delta1, model_name = "Stan_binom", data = win.data,init = initsPAbis,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modPA_HANPP2000l_fourthsample,file="modPA_HANPP2000l_fourthsample.RData")
rm(modPA_HANPP2000l_fourthsample)
}

if (runDIC)
{attach("modPA_HANPP2000l_fourthsample.RData",pos=2)

DICmarginal1000r.PA.fourthsample.supp.muchlonger<-rbind(DICmarginal1000r.PA.fourthsample.supp.muchlonger,HANPP2000l=DICmarginal.ZIbeta(modPA_HANPP2000l_fourthsample,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_PA_fourthsample_supp_muchlonger.RData")
save.image("BSC_PA_fourthsample_supp_muchlonger.RData")
}


			
model.delta<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.eta<-glm(I(PA/TotArea)~NumPA+I(scale(log(HANPP.1950.Dullinger),scale=F))-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.bidon<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.bidonbis<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.COU<-glm(I(PA/TotArea)~COU-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.Ptype<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.sdinner<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))

			win.data <-list(Y = (model.delta)$y,
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pardelta","pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modPA_HANPP1950l_fourthsample <- stan(model_code = stanmodel.delta1, model_name = "Stan_binom", data = win.data,init = initsPAbis,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modPA_HANPP1950l_fourthsample,file="modPA_HANPP1950l_fourthsample.RData")
rm(modPA_HANPP1950l_fourthsample)
}

if (runDIC)
{attach("modPA_HANPP1950l_fourthsample.RData",pos=2)

DICmarginal1000r.PA.fourthsample.supp.muchlonger<-rbind(DICmarginal1000r.PA.fourthsample.supp.muchlonger,HANPP1950l=DICmarginal.ZIbeta(modPA_HANPP1950l_fourthsample,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_PA_fourthsample_supp_muchlonger.RData")
save.image("BSC_PA_fourthsample_supp_muchlonger.RData")
}



model.delta<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.eta<-glm(I(PA/TotArea)~NumPA+I(scale(log(HANPP.1900.Dullinger),scale=F))-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.bidon<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.bidonbis<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.COU<-glm(I(PA/TotArea)~COU-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.Ptype<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.sdinner<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))

			win.data <-list(Y = (model.delta)$y,
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pardelta","pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modPA_HANPP1900l_fourthsample <- stan(model_code = stanmodel.delta1, model_name = "Stan_binom", data = win.data,init = initsPAbis,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modPA_HANPP1900l_fourthsample,file="modPA_HANPP1900l_fourthsample.RData")
rm(modPA_HANPP1900l_fourthsample)
}

if (runDIC)
{attach("modPA_HANPP1900l_fourthsample.RData",pos=2)

DICmarginal1000r.PA.fourthsample.supp.muchlonger<-rbind(DICmarginal1000r.PA.fourthsample.supp.muchlonger,HANPP1900l=DICmarginal.ZIbeta(modPA_HANPP1900l_fourthsample,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_PA_fourthsample_supp_muchlonger.RData")
save.image("BSC_PA_fourthsample_supp_muchlonger.RData")
}




			
model.delta<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.eta<-glm(I(PA/TotArea)~NumPA+I(scale((HANPP.2000.Dullinger/0.2),scale=F))-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.bidon<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.bidonbis<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.COU<-glm(I(PA/TotArea)~COU-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.Ptype<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.sdinner<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))

			win.data <-list(Y = (model.delta)$y,
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pardelta","pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modPA_HANPP2000_fourthsample <- stan(model_code = stanmodel.delta1, model_name = "Stan_binom", data = win.data,init = initsPAbis,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modPA_HANPP2000_fourthsample,file="modPA_HANPP2000_fourthsample.RData")
rm(modPA_HANPP2000_fourthsample)
}

if (runDIC)
{attach("modPA_HANPP2000_fourthsample.RData",pos=2)

DICmarginal1000r.PA.fourthsample.supp.muchlonger<-rbind(DICmarginal1000r.PA.fourthsample.supp.muchlonger,HANPP2000=DICmarginal.ZIbeta(modPA_HANPP2000_fourthsample,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_PA_fourthsample_supp_muchlonger.RData")
save.image("BSC_PA_fourthsample_supp_muchlonger.RData")
}



	
	
			
model.delta<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.eta<-glm(I(PA/TotArea)~NumPA+I(scale((HANPP.1950.Dullinger/0.2),scale=F))-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.bidon<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.bidonbis<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.COU<-glm(I(PA/TotArea)~COU-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.Ptype<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.sdinner<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))

			win.data <-list(Y = (model.delta)$y,
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pardelta","pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modPA_HANPP1950_fourthsample <- stan(model_code = stanmodel.delta1, model_name = "Stan_binom", data = win.data,init = initsPAbis,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modPA_HANPP1950_fourthsample,file="modPA_HANPP1950_fourthsample.RData")
rm(modPA_HANPP1950_fourthsample)
}

if (runDIC)
{attach("modPA_HANPP1950_fourthsample.RData",pos=2)

DICmarginal1000r.PA.fourthsample.supp.muchlonger<-rbind(DICmarginal1000r.PA.fourthsample.supp.muchlonger,HANPP1950=DICmarginal.ZIbeta(modPA_HANPP1950_fourthsample,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_PA_fourthsample_supp_muchlonger.RData")
save.image("BSC_PA_fourthsample_supp_muchlonger.RData")
}



	

model.delta<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.eta<-glm(I(PA/TotArea)~NumPA+I(scale((HANPP.1900.Dullinger/0.2),scale=F))-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.bidon<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.bidonbis<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.COU<-glm(I(PA/TotArea)~COU-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.Ptype<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.sdinner<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))

			win.data <-list(Y = (model.delta)$y,
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pardelta","pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modPA_HANPP1900_fourthsample <- stan(model_code = stanmodel.delta1, model_name = "Stan_binom", data = win.data,init = initsPAbis,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modPA_HANPP1900_fourthsample,file="modPA_HANPP1900_fourthsample.RData")
rm(modPA_HANPP1900_fourthsample)
}

if (runDIC)
{attach("modPA_HANPP1900_fourthsample.RData",pos=2)

DICmarginal1000r.PA.fourthsample.supp.muchlonger<-rbind(DICmarginal1000r.PA.fourthsample.supp.muchlonger,HANPP1900=DICmarginal.ZIbeta(modPA_HANPP1900_fourthsample,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_PA_fourthsample_supp_muchlonger.RData")
save.image("BSC_PA_fourthsample_supp_muchlonger.RData")
}

	
			



model.delta<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.eta<-glm(I(PA/TotArea)~NumPA+I(scale((POPd.Dullinger.1900/50),scale=F))-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.bidon<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.bidonbis<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.COU<-glm(I(PA/TotArea)~COU-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.Ptype<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.sdinner<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))

			win.data <-list(Y = (model.delta)$y,
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
				
				
nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pardelta","pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modPA_POPd_fourthsample <- stan(model_code = stanmodel.delta1, model_name = "Stan_binom", data = win.data,init = initsPAbis,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modPA_POPd_fourthsample,file="modPA_POPd1900_fourthsample.RData")
rm(modPA_POPd_fourthsample)
}

if (runDIC)
{attach("modPA_POPd1900_fourthsample.RData",pos=2)

DICmarginal1000r.PA.fourthsample.supp.muchlonger<-rbind(DICmarginal1000r.PA.fourthsample.supp.muchlonger,POPd1900=DICmarginal.ZIbeta(modPA_POPd_fourthsample,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_PA_fourthsample_supp_muchlonger.RData")
save.image("BSC_PA_fourthsample_supp_muchlonger.RData")
}





		


			

model.delta<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.eta<-glm(I(PA/TotArea)~NumPA+I(scale(PIB1900aD,scale=F)/400)+I((scale(PIB1900aD,scale=F)/400)^2)-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.bidon<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.bidonbis<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.COU<-glm(I(PA/TotArea)~COU-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.Ptype<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.sdinner<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))

			win.data <-list(Y = (model.delta)$y,
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pardelta","pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modPA_Kuznets1900a_fourthsample <- stan(model_code = stanmodel.delta1, model_name = "Stan_binom", data = win.data,init = initsPAbis,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modPA_Kuznets1900a_fourthsample,file="modPA_Kuznets1900a_fourthsample.RData")
rm(modPA_Kuznets1900a_fourthsample)
}

if (runDIC)
{attach("modPA_Kuznets1900a_fourthsample.RData",pos=2)

DICmarginal1000r.PA.fourthsample.supp.muchlonger<-rbind(DICmarginal1000r.PA.fourthsample.supp.muchlonger,Kuznets1900a=DICmarginal.ZIbeta(modPA_Kuznets1900a_fourthsample,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_PA_fourthsample_supp_muchlonger.RData")
save.image("BSC_PA_fourthsample_supp_muchlonger.RData")
}




















model.delta<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.eta<-glm(I(PA/TotArea)~NumPA+I(scale(log(PIB1900aD/400),scale=F))+I((scale(log(PIB1900aD/400),scale=F))^2)-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.bidon<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.bidonbis<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.COU<-glm(I(PA/TotArea)~COU-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.Ptype<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.sdinner<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))

			win.data <-list(Y = (model.delta)$y,
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pardelta","pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modPA_lKuznets1900a_fourthsample <- stan(model_code = stanmodel.delta1, model_name = "Stan_binom", data = win.data,init = initsPAbis,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modPA_lKuznets1900a_fourthsample,file="modPA_lKuznets1900a_fourthsample.RData")
rm(modPA_lKuznets1900a_fourthsample)
}

if (runDIC)
{attach("modPA_lKuznets1900a_fourthsample.RData",pos=2)

DICmarginal1000r.PA.fourthsample.supp.muchlonger<-rbind(DICmarginal1000r.PA.fourthsample.supp.muchlonger,lKuznets1900a=DICmarginal.ZIbeta(modPA_lKuznets1900a_fourthsample,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_PA_fourthsample_supp_muchlonger.RData")
save.image("BSC_PA_fourthsample_supp_muchlonger.RData")
}


















model.delta<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.eta<-glm(I(PA/TotArea)~NumPA+I(scale(log(PIB.1900.Dullinger/2),scale=F))+I((scale(log(PIB.1900.Dullinger/2),scale=F))^2)-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.bidon<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.bidonbis<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.COU<-glm(I(PA/TotArea)~COU-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.Ptype<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.sdinner<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))

			win.data <-list(Y = (model.delta)$y,
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pardelta","pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modPA_lKuznets1900_fourthsample <- stan(model_code = stanmodel.delta1, model_name = "Stan_binom", data = win.data,init = initsPAbis,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modPA_lKuznets1900_fourthsample,file="modPA_lKuznets1900_fourthsample.RData")
rm(modPA_lKuznets1900_fourthsample)
}

if (runDIC)
{attach("modPA_lKuznets1900_fourthsample.RData",pos=2)

DICmarginal1000r.PA.fourthsample.supp.muchlonger<-rbind(DICmarginal1000r.PA.fourthsample.supp.muchlonger,lKuznets1900=DICmarginal.ZIbeta(modPA_lKuznets1900_fourthsample,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_PA_fourthsample_supp_muchlonger.RData")
save.image("BSC_PA_fourthsample_supp_muchlonger.RData")
}





		
		
		
		
		

model.delta<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.eta<-glm(I(PA/TotArea)~NumPA+I(scale((PIB.1900.Dullinger/2),scale=F))+I((scale((PIB.1900.Dullinger/2),scale=F))^2)-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.bidon<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.bidonbis<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.COU<-glm(I(PA/TotArea)~COU-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.Ptype<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.sdinner<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))

			win.data <-list(Y = (model.delta)$y,
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pardelta","pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modPA_Kuznets1900_fourthsample <- stan(model_code = stanmodel.delta1, model_name = "Stan_binom", data = win.data,init = initsPAbis,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modPA_Kuznets1900_fourthsample,file="modPA_Kuznets1900_fourthsample.RData")
rm(modPA_Kuznets1900_fourthsample)
}

if (runDIC)
{attach("modPA_Kuznets1900_fourthsample.RData",pos=2)

DICmarginal1000r.PA.fourthsample.supp.muchlonger<-rbind(DICmarginal1000r.PA.fourthsample.supp.muchlonger,Kuznets1900=DICmarginal.ZIbeta(modPA_Kuznets1900_fourthsample,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_PA_fourthsample_supp_muchlonger.RData")
save.image("BSC_PA_fourthsample_supp_muchlonger.RData")
}

















model.delta<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1950aD))
model.eta<-glm(I(PA/TotArea)~NumPA+I(scale(log(PIB.1950.Dullinger/2),scale=F))+I((scale(log(PIB.1950.Dullinger/2),scale=F))^2)-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1950aD))
model.bidon<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1950aD))
model.bidonbis<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1950aD))
model.COU<-glm(I(PA/TotArea)~COU-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1950aD))
model.Ptype<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1950aD))
model.sdinner<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1950aD))

			win.data <-list(Y = (model.delta)$y,
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pardelta","pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modPA_lKuznets1950_fourthsample <- stan(model_code = stanmodel.delta1, model_name = "Stan_binom", data = win.data,init = initsPAbis,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modPA_lKuznets1950_fourthsample,file="modPA_lKuznets1950_fourthsample.RData")
rm(modPA_lKuznets1950_fourthsample)
}

if (runDIC)
{attach("modPA_lKuznets1950_fourthsample.RData",pos=2)

DICmarginal1000r.PA.fourthsample.supp.muchlonger<-rbind(DICmarginal1000r.PA.fourthsample.supp.muchlonger,lKuznets1950=DICmarginal.ZIbeta(modPA_lKuznets1950_fourthsample,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_PA_fourthsample_supp_muchlonger.RData")
save.image("BSC_PA_fourthsample_supp_muchlonger.RData")
}



		
		
		

model.delta<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1950aD))
model.eta<-glm(I(PA/TotArea)~NumPA+I(scale((PIB.1950.Dullinger/2),scale=F))+I((scale((PIB.1950.Dullinger/2),scale=F))^2)-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1950aD))
model.bidon<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1950aD))
model.bidonbis<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1950aD))
model.COU<-glm(I(PA/TotArea)~COU-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1950aD))
model.Ptype<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1950aD))
model.sdinner<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1950aD))

			win.data <-list(Y = (model.delta)$y,
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pardelta","pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modPA_Kuznets1950_fourthsample <- stan(model_code = stanmodel.delta1, model_name = "Stan_binom", data = win.data,init = initsPAbis,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modPA_Kuznets1950_fourthsample,file="modPA_Kuznets1950_fourthsample.RData")
rm(modPA_Kuznets1950_fourthsample)
}

if (runDIC)
{attach("modPA_Kuznets1950_fourthsample.RData",pos=2)

DICmarginal1000r.PA.fourthsample.supp.muchlonger<-rbind(DICmarginal1000r.PA.fourthsample.supp.muchlonger,Kuznets1950=DICmarginal.ZIbeta(modPA_Kuznets1950_fourthsample,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_PA_fourthsample_supp_muchlonger.RData")
save.image("BSC_PA_fourthsample_supp_muchlonger.RData")
}











model.delta<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB2000aD))
model.eta<-glm(I(PA/TotArea)~NumPA+I(scale(log(PIB.2000.Dullinger/2),scale=F))+I((scale(log(PIB.2000.Dullinger/2),scale=F))^2)-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB2000aD))
model.bidon<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB2000aD))
model.bidonbis<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB2000aD))
model.COU<-glm(I(PA/TotArea)~COU-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB2000aD))
model.Ptype<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB2000aD))
model.sdinner<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB2000aD))

			win.data <-list(Y = (model.delta)$y,
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pardelta","pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modPA_lKuznets2000_fourthsample <- stan(model_code = stanmodel.delta1, model_name = "Stan_binom", data = win.data,init = initsPAbis,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modPA_lKuznets2000_fourthsample,file="modPA_lKuznets2000_fourthsample.RData")
rm(modPA_lKuznets2000_fourthsample)
}

if (runDIC)
{attach("modPA_lKuznets2000_fourthsample.RData",pos=2)

DICmarginal1000r.PA.fourthsample.supp.muchlonger<-rbind(DICmarginal1000r.PA.fourthsample.supp.muchlonger,lKuznets2000=DICmarginal.ZIbeta(modPA_lKuznets2000_fourthsample,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_PA_fourthsample_supp_muchlonger.RData")
save.image("BSC_PA_fourthsample_supp_muchlonger.RData")
}




		
		
		
		
		

model.delta<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB2000aD))
model.eta<-glm(I(PA/TotArea)~NumPA+I(scale((PIB.2000.Dullinger/7),scale=F))+I((scale((PIB.2000.Dullinger/7),scale=F))^2)-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB2000aD))
model.bidon<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB2000aD))
model.bidonbis<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB2000aD))
model.COU<-glm(I(PA/TotArea)~COU-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB2000aD))
model.Ptype<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB2000aD))
model.sdinner<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB2000aD))

			win.data <-list(Y = (model.delta)$y,
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pardelta","pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modPA_Kuznets2000_fourthsample <- stan(model_code = stanmodel.delta1, model_name = "Stan_binom", data = win.data,init = initsPAbis,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modPA_Kuznets2000_fourthsample,file="modPA_Kuznets2000_fourthsample.RData")
rm(modPA_Kuznets2000_fourthsample)
}

if (runDIC)
{attach("modPA_Kuznets2000_fourthsample.RData",pos=2)

DICmarginal1000r.PA.fourthsample.supp.muchlonger<-rbind(DICmarginal1000r.PA.fourthsample.supp.muchlonger,Kuznets2000=DICmarginal.ZIbeta(modPA_Kuznets2000_fourthsample,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_PA_fourthsample_supp_muchlonger.RData")
save.image("BSC_PA_fourthsample_supp_muchlonger.RData")
}


	











model.delta<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.eta<-glm(I(PA/TotArea)~NumPA+I(scale((PIB.1900.Dullinger/2),scale=F))+I(scale((POPd.Dullinger.1900/50),scale=F))-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.bidon<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.bidonbis<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.COU<-glm(I(PA/TotArea)~COU-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.Ptype<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.sdinner<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))

			win.data <-list(Y = (model.delta)$y,
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pardelta","pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modPA_eco1900_fourthsample <- stan(model_code = stanmodel.delta1, model_name = "Stan_binom", data = win.data,init = initsPAbis,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modPA_eco1900_fourthsample,file="modPA_eco1900_fourthsample.RData")
rm(modPA_eco1900_fourthsample)
}

if (runDIC)
{attach("modPA_eco1900_fourthsample.RData",pos=2)

DICmarginal1000r.PA.fourthsample.supp.muchlonger<-rbind(DICmarginal1000r.PA.fourthsample.supp.muchlonger,eco1900=DICmarginal.ZIbeta(modPA_eco1900_fourthsample,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_PA_fourthsample_supp_muchlonger.RData")
save.image("BSC_PA_fourthsample_supp_muchlonger.RData")
}


			
		

#load("BSC_PA_fourthsample.RData")
#save.image("BSC_PA_fourthsample.RData")

model.delta<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.eta<-glm(I(PA/TotArea)~NumPA+I(scale(log(PIB.1900.Dullinger/2),scale=F))+I(scale(log(POPd.Dullinger.1900/50),scale=F))-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.bidon<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.bidonbis<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.COU<-glm(I(PA/TotArea)~COU-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.Ptype<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.sdinner<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))

			win.data <-list(Y = (model.delta)$y,
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pardelta","pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modPA_leco1900_fourthsample <- stan(model_code = stanmodel.delta1, model_name = "Stan_binom", data = win.data,init = initsPAbis,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modPA_leco1900_fourthsample,file="modPA_leco1900_fourthsample.RData")
rm(modPA_leco1900_fourthsample)
}

if (runDIC)
{attach("modPA_leco1900_fourthsample.RData",pos=2)

DICmarginal1000r.PA.fourthsample.supp.muchlonger<-rbind(DICmarginal1000r.PA.fourthsample.supp.muchlonger,leco1900=DICmarginal.ZIbeta(modPA_leco1900_fourthsample,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_PA_fourthsample_supp_muchlonger.RData")
save.image("BSC_PA_fourthsample_supp_muchlonger.RData")
}


	



model.delta<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.eta<-glm(I(PA/TotArea)~NumPA+I(scale(PIBa,scale=F)/50000)+I((scale(PIBa,scale=F)/50000)^2)-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.bidon<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.bidonbis<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.COU<-glm(I(PA/TotArea)~COU-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.Ptype<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.sdinner<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))

			win.data <-list(Y = (model.delta)$y,
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))


nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pardelta","pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modPA_Kuznetsa_fourthsample <- stan(model_code = stanmodel.delta1, model_name = "Stan_binom", data = win.data,init = initsPAbis,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modPA_Kuznetsa_fourthsample,file="modPA_Kuznetsa_fourthsample.RData")
rm(modPA_Kuznetsa_fourthsample)
}

if (runDIC)
{attach("modPA_Kuznetsa_fourthsample.RData",pos=2)

DICmarginal1000r.PA.fourthsample.supp.muchlonger<-rbind(DICmarginal1000r.PA.fourthsample.supp.muchlonger,Kuznetsa=DICmarginal.ZIbeta(modPA_Kuznetsa_fourthsample,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_PA_fourthsample_supp_muchlonger.RData")
save.image("BSC_PA_fourthsample_supp_muchlonger.RData")
}






model.delta<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.eta<-glm(I(PA/TotArea)~NumPA+I(scale((PIBa)/50000,scale=F))+I(scale(log((PIBa)/50000),scale=F)^2)-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.bidon<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.bidonbis<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.COU<-glm(I(PA/TotArea)~COU-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.Ptype<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.sdinner<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))

			win.data <-list(Y = (model.delta)$y,
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))


nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pardelta","pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modPA_lKuznetsa_fourthsample <- stan(model_code = stanmodel.delta1, model_name = "Stan_binom", data = win.data,init = initsPAbis,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modPA_lKuznetsa_fourthsample,file="modPA_lKuznetsa_fourthsample.RData")
rm(modPA_lKuznetsa_fourthsample)
}

if (runDIC)
{attach("modPA_lKuznetsa_fourthsample.RData",pos=2)

DICmarginal1000r.PA.fourthsample.supp.muchlonger<-rbind(DICmarginal1000r.PA.fourthsample.supp.muchlonger,lKuznetsa=DICmarginal.ZIbeta(modPA_lKuznetsa_fourthsample,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_PA_fourthsample_supp_muchlonger.RData")
save.image("BSC_PA_fourthsample_supp_muchlonger.RData")
}






model.delta<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.eta<-glm(I(PA/TotArea)~NumPA+I(scale(log(PIBh/10000),scale=F))+I(scale(log(POPd/100),scale=F))-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.bidon<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.bidonbis<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.COU<-glm(I(PA/TotArea)~COU-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.Ptype<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.sdinner<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))

			win.data <-list(Y = (model.delta)$y,
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))


nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pardelta","pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modPA_leco_fourthsample <- stan(model_code = stanmodel.delta1, model_name = "Stan_binom", data = win.data,init = initsPAbis,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modPA_leco_fourthsample,file="modPA_leco_fourthsample.RData")
rm(modPA_leco_fourthsample)
}

if (runDIC)
{attach("modPA_leco_fourthsample.RData",pos=2)

DICmarginal1000r.PA.fourthsample.supp.muchlonger<-rbind(DICmarginal1000r.PA.fourthsample.supp.muchlonger,leco=DICmarginal.ZIbeta(modPA_leco_fourthsample,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_PA_fourthsample_supp_muchlonger.RData")
save.image("BSC_PA_fourthsample_supp_muchlonger.RData")
}


	




model.delta<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.eta<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.bidon<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.bidonbis<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.COU<-glm(I(PA/TotArea)~COU-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.Ptype<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.sdinner<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))

			win.data <-list(Y = (model.delta)$y,
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))


nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pardelta","pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modPA_Null_fourthsample <- stan(model_code = stanmodel.delta1, model_name = "Stan_binom", data = win.data,init = initsPAbis,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modPA_Null_fourthsample,file="modPA_Null_fourthsample.RData")
rm(modPA_Null_fourthsample)
}

if (runDIC)
{attach("modPA_Null_fourthsample.RData",pos=2)

DICmarginal1000r.PA.fourthsample.supp.muchlonger<-rbind(DICmarginal1000r.PA.fourthsample.supp.muchlonger,Null=DICmarginal.ZIbeta(modPA_Null_fourthsample,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_PA_fourthsample_supp_muchlonger.RData")
save.image("BSC_PA_fourthsample_supp_muchlonger.RData")
}




		
		
			
			

					
model.delta<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.eta<-glm(I(PA/TotArea)~NumPA+I(scale((PIB2000aD/2400),scale=F))-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.bidon<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.bidonbis<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.COU<-glm(I(PA/TotArea)~COU-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.Ptype<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.sdinner<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))

			win.data <-list(Y = (model.delta)$y,
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pardelta","pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modPA_GDP2000a_fourthsample <- stan(model_code = stanmodel.delta1, model_name = "Stan_binom", data = win.data,init = initsPAbis,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modPA_GDP2000a_fourthsample,file="modPA_GDP2000a_fourthsample.RData")
rm(modPA_GDP2000a_fourthsample)
}

if (runDIC)
{attach("modPA_GDP2000a_fourthsample.RData",pos=2)

DICmarginal1000r.PA.fourthsample.supp.muchlonger<-rbind(DICmarginal1000r.PA.fourthsample.supp.muchlonger,GDP2000a=DICmarginal.ZIbeta(modPA_GDP2000a_fourthsample,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_PA_fourthsample_supp_muchlonger.RData")
save.image("BSC_PA_fourthsample_supp_muchlonger.RData")
}

		
			
			
model.delta<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.eta<-glm(I(PA/TotArea)~NumPA+I(scale((PIB1950aD/400),scale=F))-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.bidon<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.bidonbis<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.COU<-glm(I(PA/TotArea)~COU-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.Ptype<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.sdinner<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))

			win.data <-list(Y = (model.delta)$y,
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pardelta","pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modPA_GDP1950a_fourthsample <- stan(model_code = stanmodel.delta1, model_name = "Stan_binom", data = win.data,init = initsPAbis,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modPA_GDP1950a_fourthsample,file="modPA_GDP1950a_fourthsample.RData")
rm(modPA_GDP1950a_fourthsample)
}

if (runDIC)
{attach("modPA_GDP1950a_fourthsample.RData",pos=2)

DICmarginal1000r.PA.fourthsample.supp.muchlonger<-rbind(DICmarginal1000r.PA.fourthsample.supp.muchlonger,GDP1950a=DICmarginal.ZIbeta(modPA_GDP1950a_fourthsample,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_PA_fourthsample_supp_muchlonger.RData")
save.image("BSC_PA_fourthsample_supp_muchlonger.RData")
}

	


model.delta<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.eta<-glm(I(PA/TotArea)~NumPA+I(scale((PIB1900aD/400),scale=F))-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.bidon<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.bidonbis<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.COU<-glm(I(PA/TotArea)~COU-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.Ptype<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.sdinner<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))

			win.data <-list(Y = (model.delta)$y,
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pardelta","pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modPA_GDP1900a_fourthsample <- stan(model_code = stanmodel.delta1, model_name = "Stan_binom", data = win.data,init = initsPAbis,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modPA_GDP1900a_fourthsample,file="modPA_GDP1900a_fourthsample.RData")
rm(modPA_GDP1900a_fourthsample)
}

if (runDIC)
{attach("modPA_GDP1900a_fourthsample.RData",pos=2)

DICmarginal1000r.PA.fourthsample.supp.muchlonger<-rbind(DICmarginal1000r.PA.fourthsample.supp.muchlonger,GDP1900a=DICmarginal.ZIbeta(modPA_GDP1900a_fourthsample,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_PA_fourthsample_supp_muchlonger.RData")
save.image("BSC_PA_fourthsample_supp_muchlonger.RData")
}






model.delta<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.eta<-glm(I(PA/TotArea)~NumPA+I(scale(log(POPd.Dullinger.1900/50),scale=F))-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.bidon<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.bidonbis<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.COU<-glm(I(PA/TotArea)~COU-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.Ptype<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.sdinner<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))

			win.data <-list(Y = (model.delta)$y,
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
				
				
nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pardelta","pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modPA_POPd1900l_fourthsample <- stan(model_code = stanmodel.delta1, model_name = "Stan_binom", data = win.data,init = initsPAbis,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modPA_POPd1900l_fourthsample,file="modPA_POPd1900l_fourthsample.RData")
rm(modPA_POPd1900l_fourthsample)
}

if (runDIC)
{attach("modPA_POPd1900l_fourthsample.RData",pos=2)

DICmarginal1000r.PA.fourthsample.supp.muchlonger<-rbind(DICmarginal1000r.PA.fourthsample.supp.muchlonger,POPd1900l=DICmarginal.ZIbeta(modPA_POPd1900l_fourthsample,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_PA_fourthsample_supp_muchlonger.RData")
save.image("BSC_PA_fourthsample_supp_muchlonger.RData")
}





			

model.delta<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.eta<-glm(I(PA/TotArea)~NumPA+I(scale((POPd.Dullinger.1950/50),scale=F))-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.bidon<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.bidonbis<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.COU<-glm(I(PA/TotArea)~COU-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.Ptype<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.sdinner<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))

			win.data <-list(Y = (model.delta)$y,
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
				
				
nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pardelta","pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modPA_POPd1950_fourthsample <- stan(model_code = stanmodel.delta1, model_name = "Stan_binom", data = win.data,init = initsPAbis,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modPA_POPd1950_fourthsample,file="modPA_POPd1950_fourthsample.RData")
rm(modPA_POPd1950_fourthsample)
}

if (runDIC)
{attach("modPA_POPd1950_fourthsample.RData",pos=2)

DICmarginal1000r.PA.fourthsample.supp.muchlonger<-rbind(DICmarginal1000r.PA.fourthsample.supp.muchlonger,POPd1950=DICmarginal.ZIbeta(modPA_POPd1950_fourthsample,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_PA_fourthsample_supp_muchlonger.RData")
save.image("BSC_PA_fourthsample_supp_muchlonger.RData")
}







model.delta<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.eta<-glm(I(PA/TotArea)~NumPA+I(scale(log(POPd.Dullinger.1950/50),scale=F))-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.bidon<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.bidonbis<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.COU<-glm(I(PA/TotArea)~COU-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.Ptype<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.sdinner<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))

			win.data <-list(Y = (model.delta)$y,
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
				
				
nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pardelta","pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modPA_POPd1950l_fourthsample <- stan(model_code = stanmodel.delta1, model_name = "Stan_binom", data = win.data,init = initsPAbis,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modPA_POPd1950l_fourthsample,file="modPA_POPd1950l_fourthsample.RData")
rm(modPA_POPd1950l_fourthsample)
}

if (runDIC)
{attach("modPA_POPd1950l_fourthsample.RData",pos=2)

DICmarginal1000r.PA.fourthsample.supp.muchlonger<-rbind(DICmarginal1000r.PA.fourthsample.supp.muchlonger,POPd1950l=DICmarginal.ZIbeta(modPA_POPd1950l_fourthsample,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_PA_fourthsample_supp_muchlonger.RData")
save.image("BSC_PA_fourthsample_supp_muchlonger.RData")
}









model.delta<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.eta<-glm(I(PA/TotArea)~NumPA+I(scale((POPd.Dullinger.2000/50),scale=F))-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.bidon<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.bidonbis<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.COU<-glm(I(PA/TotArea)~COU-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.Ptype<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.sdinner<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))

			win.data <-list(Y = (model.delta)$y,
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
				
				
nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pardelta","pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modPA_POPd2000_fourthsample <- stan(model_code = stanmodel.delta1, model_name = "Stan_binom", data = win.data,init = initsPAbis,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modPA_POPd2000_fourthsample,file="modPA_POPd2000_fourthsample.RData")
rm(modPA_POPd2000_fourthsample)
}

if (runDIC)
{attach("modPA_POPd2000_fourthsample.RData",pos=2)

DICmarginal1000r.PA.fourthsample.supp.muchlonger<-rbind(DICmarginal1000r.PA.fourthsample.supp.muchlonger,POPd2000=DICmarginal.ZIbeta(modPA_POPd2000_fourthsample,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_PA_fourthsample_supp_muchlonger.RData")
save.image("BSC_PA_fourthsample_supp_muchlonger.RData")
}









model.delta<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.eta<-glm(I(PA/TotArea)~NumPA+I(scale(log(POPd.Dullinger.2000/50),scale=F))-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.bidon<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.bidonbis<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.COU<-glm(I(PA/TotArea)~COU-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.Ptype<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.sdinner<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))

			win.data <-list(Y = (model.delta)$y,
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
				
				
nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pardelta","pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modPA_POPd2000l_fourthsample <- stan(model_code = stanmodel.delta1, model_name = "Stan_binom", data = win.data,init = initsPAbis,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modPA_POPd2000l_fourthsample,file="modPA_POPd2000l_fourthsample.RData")
rm(modPA_POPd2000l_fourthsample)
}

if (runDIC)
{attach("modPA_POPd2000l_fourthsample.RData",pos=2)

DICmarginal1000r.PA.fourthsample.supp.muchlonger<-rbind(DICmarginal1000r.PA.fourthsample.supp.muchlonger,POPd2000l=DICmarginal.ZIbeta(modPA_POPd2000l_fourthsample,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_PA_fourthsample_supp_muchlonger.RData")
save.image("BSC_PA_fourthsample_supp_muchlonger.RData")
}




							
							

model.delta<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.eta<-glm(I(PA/TotArea)~NumPA+I(scale((PIBh/10000),scale=F))+I(scale((POPd/100),scale=F))-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.bidon<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.bidonbis<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.COU<-glm(I(PA/TotArea)~COU-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.Ptype<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.sdinner<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))

			win.data <-list(Y = (model.delta)$y,
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))


nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pardelta","pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modPA_eco_fourthsample <- stan(model_code = stanmodel.delta1, model_name = "Stan_binom", data = win.data,init = initsPAbis,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modPA_eco_fourthsample,file="modPA_eco_fourthsample.RData")
rm(modPA_eco_fourthsample)
}

if (runDIC)
{attach("modPA_eco_fourthsample.RData",pos=2)

DICmarginal1000r.PA.fourthsample.supp.muchlonger<-rbind(DICmarginal1000r.PA.fourthsample.supp.muchlonger,eco=DICmarginal.ZIbeta(modPA_eco_fourthsample,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_PA_fourthsample_supp_muchlonger.RData")
save.image("BSC_PA_fourthsample_supp_muchlonger.RData")
}


		
		
		
		
		
		
		
		
		
		
		
		


model.delta<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.eta<-glm(I(PA/TotArea)~NumPA+I(scale(PIB1950aD,scale=F)/400)+I((scale(PIB1950aD,scale=F)/400)^2)-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.bidon<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.bidonbis<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.COU<-glm(I(PA/TotArea)~COU-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.Ptype<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.sdinner<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))

			win.data <-list(Y = (model.delta)$y,
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pardelta","pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modPA_Kuznets1950a_fourthsample <- stan(model_code = stanmodel.delta1, model_name = "Stan_binom", data = win.data,init = initsPAbis,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modPA_Kuznets1950a_fourthsample,file="modPA_Kuznets1950a_fourthsample.RData")
rm(modPA_Kuznets1950a_fourthsample)
}

if (runDIC)
{attach("modPA_Kuznets1950a_fourthsample.RData",pos=2)

DICmarginal1000r.PA.fourthsample.supp.muchlonger<-rbind(DICmarginal1000r.PA.fourthsample.supp.muchlonger,Kuznets1950a=DICmarginal.ZIbeta(modPA_Kuznets1950a_fourthsample,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_PA_fourthsample_supp_muchlonger.RData")
save.image("BSC_PA_fourthsample_supp_muchlonger.RData")
}




	
















model.delta<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.eta<-glm(I(PA/TotArea)~NumPA+I(scale(log(PIB1950aD/400),scale=F))+I((scale(log(PIB1950aD/400),scale=F))^2)-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.bidon<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.bidonbis<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.COU<-glm(I(PA/TotArea)~COU-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.Ptype<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.sdinner<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))

			win.data <-list(Y = (model.delta)$y,
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pardelta","pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modPA_lKuznets1950a_fourthsample <- stan(model_code = stanmodel.delta1, model_name = "Stan_binom", data = win.data,init = initsPAbis,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modPA_lKuznets1950a_fourthsample,file="modPA_lKuznets1950a_fourthsample.RData")
rm(modPA_lKuznets1950a_fourthsample)
}

if (runDIC)
{attach("modPA_lKuznets1950a_fourthsample.RData",pos=2)

DICmarginal1000r.PA.fourthsample.supp.muchlonger<-rbind(DICmarginal1000r.PA.fourthsample.supp.muchlonger,lKuznets1950a=DICmarginal.ZIbeta(modPA_lKuznets1950a_fourthsample,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_PA_fourthsample_supp_muchlonger.RData")
save.image("BSC_PA_fourthsample_supp_muchlonger.RData")
}
















model.delta<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.eta<-glm(I(PA/TotArea)~NumPA+I(scale((PIB.1950.Dullinger/2),scale=F))+I(scale((POPd.Dullinger.1950/50),scale=F))-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.bidon<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.bidonbis<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.COU<-glm(I(PA/TotArea)~COU-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.Ptype<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.sdinner<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))

			win.data <-list(Y = (model.delta)$y,
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pardelta","pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modPA_eco1950_fourthsample <- stan(model_code = stanmodel.delta1, model_name = "Stan_binom", data = win.data,init = initsPAbis,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modPA_eco1950_fourthsample,file="modPA_eco1950_fourthsample.RData")
rm(modPA_eco1950_fourthsample)
}

if (runDIC)
{attach("modPA_eco1950_fourthsample.RData",pos=2)

DICmarginal1000r.PA.fourthsample.supp.muchlonger<-rbind(DICmarginal1000r.PA.fourthsample.supp.muchlonger,eco1950=DICmarginal.ZIbeta(modPA_eco1950_fourthsample,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_PA_fourthsample_supp_muchlonger.RData")
save.image("BSC_PA_fourthsample_supp_muchlonger.RData")
}


		
		

#load("BSC_PA_fourthsample.RData")
#save.image("BSC_PA_fourthsample.RData")

model.delta<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.eta<-glm(I(PA/TotArea)~NumPA+I(scale(log(PIB.1950.Dullinger/2),scale=F))+I(scale(log(POPd.Dullinger.1950/50),scale=F))-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.bidon<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.bidonbis<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.COU<-glm(I(PA/TotArea)~COU-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.Ptype<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.sdinner<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))

			win.data <-list(Y = (model.delta)$y,
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pardelta","pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modPA_leco1950_fourthsample <- stan(model_code = stanmodel.delta1, model_name = "Stan_binom", data = win.data,init = initsPAbis,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modPA_leco1950_fourthsample,file="modPA_leco1950_fourthsample.RData")
rm(modPA_leco1950_fourthsample)
}

if (runDIC)
{attach("modPA_leco1950_fourthsample.RData",pos=2)

DICmarginal1000r.PA.fourthsample.supp.muchlonger<-rbind(DICmarginal1000r.PA.fourthsample.supp.muchlonger,leco1950=DICmarginal.ZIbeta(modPA_leco1950_fourthsample,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_PA_fourthsample_supp_muchlonger.RData")
save.image("BSC_PA_fourthsample_supp_muchlonger.RData")
}


		
		


		
		
		

model.delta<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.eta<-glm(I(PA/TotArea)~NumPA+I(scale(PIB2000aD,scale=F)/2400)+I((scale(PIB2000aD,scale=F)/2400)^2)-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.bidon<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.bidonbis<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.COU<-glm(I(PA/TotArea)~COU-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.Ptype<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.sdinner<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))

			win.data <-list(Y = (model.delta)$y,
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))


nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pardelta","pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modPA_Kuznets2000a_fourthsample <- stan(model_code = stanmodel.delta1, model_name = "Stan_binom", data = win.data,init = initsPAbis,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modPA_Kuznets2000a_fourthsample,file="modPA_Kuznets2000a_fourthsample.RData")
rm(modPA_Kuznets2000a_fourthsample)
}

if (runDIC)
{attach("modPA_Kuznets2000a_fourthsample.RData",pos=2)

DICmarginal1000r.PA.fourthsample.supp.muchlonger<-rbind(DICmarginal1000r.PA.fourthsample.supp.muchlonger,Kuznets2000a=DICmarginal.ZIbeta(modPA_Kuznets2000a_fourthsample,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_PA_fourthsample_supp_muchlonger.RData")
save.image("BSC_PA_fourthsample_supp_muchlonger.RData")
}





			
		

model.delta<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.eta<-glm(I(PA/TotArea)~NumPA+I(scale(log(PIB2000aD/2400),scale=F))+I((scale(log(PIB2000aD/2400),scale=F))^2)-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.bidon<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.bidonbis<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.COU<-glm(I(PA/TotArea)~COU-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.Ptype<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.sdinner<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))

			win.data <-list(Y = (model.delta)$y,
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))


nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pardelta","pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modPA_lKuznets2000a_fourthsample <- stan(model_code = stanmodel.delta1, model_name = "Stan_binom", data = win.data,init = initsPAbis,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modPA_lKuznets2000a_fourthsample,file="modPA_lKuznets2000a_fourthsample.RData")
rm(modPA_lKuznets2000a_fourthsample)
}

if (runDIC)
{attach("modPA_lKuznets2000a_fourthsample.RData",pos=2)

DICmarginal1000r.PA.fourthsample.supp.muchlonger<-rbind(DICmarginal1000r.PA.fourthsample.supp.muchlonger,lKuznets2000a=DICmarginal.ZIbeta(modPA_lKuznets2000a_fourthsample,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_PA_fourthsample_supp_muchlonger.RData")
save.image("BSC_PA_fourthsample_supp_muchlonger.RData")
}




		

model.delta<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.eta<-glm(I(PA/TotArea)~NumPA+I((scale(log(PIB.2000.Dullinger/7),scale=F)))+I(scale(log(POPd.Dullinger.2000/100),scale=F))-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.bidon<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.bidonbis<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.COU<-glm(I(PA/TotArea)~COU-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.Ptype<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.sdinner<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))

			win.data <-list(Y = (model.delta)$y,
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))


nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pardelta","pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modPA_leco2000_fourthsample <- stan(model_code = stanmodel.delta1, model_name = "Stan_binom", data = win.data,init = initsPAbis,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modPA_leco2000_fourthsample,file="modPA_leco2000_fourthsample.RData")
rm(modPA_leco2000_fourthsample)
}

if (runDIC)
{attach("modPA_leco2000_fourthsample.RData",pos=2)

DICmarginal1000r.PA.fourthsample.supp.muchlonger<-rbind(DICmarginal1000r.PA.fourthsample.supp.muchlonger,leco2000=DICmarginal.ZIbeta(modPA_leco2000_fourthsample,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_PA_fourthsample_supp_muchlonger.RData")
save.image("BSC_PA_fourthsample_supp_muchlonger.RData")
}

	
		










model.delta<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.eta<-glm(I(PA/TotArea)~NumPA+I((scale((PIB.2000.Dullinger/7),scale=F)))+I(scale((POPd.Dullinger.2000/100),scale=F))-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.bidon<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.bidonbis<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.COU<-glm(I(PA/TotArea)~COU-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.Ptype<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.sdinner<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))

			win.data <-list(Y = (model.delta)$y,
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))


nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pardelta","pareta","parsdinner","sdCOU","easCOU","log_lik")
if (runStan)
{
set.seed(29)
modPA_eco2000_fourthsample <- stan(model_code = stanmodel.delta1, model_name = "Stan_binom", data = win.data,init = initsPAbis,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modPA_eco2000_fourthsample,file="modPA_eco2000_fourthsample.RData")
rm(modPA_eco2000_fourthsample)
}

if (runDIC)
{attach("modPA_eco2000_fourthsample.RData",pos=2)

DICmarginal1000r.PA.fourthsample.supp.muchlonger<-rbind(DICmarginal1000r.PA.fourthsample.supp.muchlonger,eco2000=DICmarginal.ZIbeta(modPA_eco2000_fourthsample,win.data=win.data,rep.inner=1000))

detach(2)
#load("BSC_PA_fourthsample_supp_muchlonger.RData")
save.image("BSC_PA_fourthsample_supp_muchlonger.RData")
}

	









if (runDIC)
{



#write.csv(DICmarginal1000r.threatened.fourthsamplewotrees.supp.muchlonger,file="DICmthreat.csv")
setwd(wd.csv)
write.csv(t(t(DICmarginal1000r.PA.fourthsample.supp.muchlonger)),file="DICm1000rPA_fourthsamplewotrees_supp_muchlonger_all.csv")

																		
				

}


							


}


}
		


########## SEAL
{

###### model fitting & IC calculation (Tables 2 & 4)
{
if (Sys.info()["sysname"]=="Windows") {setwd(wd.RData)} 
load("BSC_Data_functions.RData")

#### parameters used to control which part of the process to do : estimation (runStan) or calculation of model comparison indices (runDIC)
runStan<-TRUE
runDIC<-TRUE


DIC.SEAL.fourthsample<-matrix(NA,ncol=11,nrow=1)
dimnames(DIC.SEAL.fourthsample)<-list(c(
			
			"SEAL.2009"),c("DIC", "DICvar","DICmin", "IC",  "pDvar",  "pD1.WAIC",        "looic", "waic",   "propkfair",  "propkbad", "propkverybad"))


			

	
			
			
			
			
			
					
model.delta<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(SEAL.2009~I(scale(log(PIB2000aD/400),scale=F)),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(SEAL.2009~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

nc <- 3	;		
ni <- 200000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modSEAL_GDP2000al_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modSEAL_GDP2000al_fourthsample,file="modSEAL_GDP2000al_fourthsample.RData")
rm(modSEAL_GDP2000al_fourthsample)
}

if (runDIC)
{attach("modSEAL_GDP2000al_fourthsample.RData",pos=2)
DIC.SEAL.fourthsample<-rbind(DIC.SEAL.fourthsample,GDP2000al=DICZIbeta.woCOU(modSEAL_GDP2000al_fourthsample,win.data=win.data))


detach(2)
#load("BSC_SEAL_fourthsample.RData")
save.image("BSC_SEAL_fourthsample.RData")
}
			
			
			
model.delta<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(SEAL.2009~I(scale(log(PIB1950aD/400),scale=F)),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(SEAL.2009~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

nc <- 3	;		
ni <- 200000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modSEAL_GDP1950al_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modSEAL_GDP1950al_fourthsample,file="modSEAL_GDP1950al_fourthsample.RData")
rm(modSEAL_GDP1950al_fourthsample)
}

if (runDIC)
{attach("modSEAL_GDP1950al_fourthsample.RData",pos=2)
DIC.SEAL.fourthsample<-rbind(DIC.SEAL.fourthsample,GDP1950al=DICZIbeta.woCOU(modSEAL_GDP1950al_fourthsample,win.data=win.data))


detach(2)
#load("BSC_SEAL_fourthsample.RData")
save.image("BSC_SEAL_fourthsample.RData")
}




model.delta<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(SEAL.2009~I(scale(log(PIB1900aD/400),scale=F)),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(SEAL.2009~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

nc <- 3	;		
ni <- 200000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modSEAL_GDP1900al_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modSEAL_GDP1900al_fourthsample,file="modSEAL_GDP1900al_fourthsample.RData")
rm(modSEAL_GDP1900al_fourthsample)
}

if (runDIC)
{attach("modSEAL_GDP1900al_fourthsample.RData",pos=2)
DIC.SEAL.fourthsample<-rbind(DIC.SEAL.fourthsample,GDP1900al=DICZIbeta.woCOU(modSEAL_GDP1900al_fourthsample,win.data=win.data))


detach(2)
#load("BSC_SEAL_fourthsample.RData")
save.image("BSC_SEAL_fourthsample.RData")
}

			
model.delta<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(SEAL.2009~I(scale(log(HANPP.2000.Dullinger),scale=F)),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(SEAL.2009~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

nc <- 3	;		
ni <- 200000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modSEAL_HANPP2000l_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modSEAL_HANPP2000l_fourthsample,file="modSEAL_HANPP2000l_fourthsample.RData")
rm(modSEAL_HANPP2000l_fourthsample)
}

if (runDIC)
{attach("modSEAL_HANPP2000l_fourthsample.RData",pos=2)
DIC.SEAL.fourthsample<-rbind(DIC.SEAL.fourthsample,HANPP2000l=DICZIbeta.woCOU(modSEAL_HANPP2000l_fourthsample,win.data=win.data))


detach(2)
#load("BSC_SEAL_fourthsample.RData")
save.image("BSC_SEAL_fourthsample.RData")
}


model.delta<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(SEAL.2009~I(scale(log(HANPP.1950.Dullinger),scale=F)),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(SEAL.2009~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

nc <- 3	;		
ni <- 200000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modSEAL_HANPP1950l_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modSEAL_HANPP1950l_fourthsample,file="modSEAL_HANPP1950l_fourthsample.RData")
rm(modSEAL_HANPP1950l_fourthsample)
}

if (runDIC)
{attach("modSEAL_HANPP1950l_fourthsample.RData",pos=2)
DIC.SEAL.fourthsample<-rbind(DIC.SEAL.fourthsample,HANPP1950l=DICZIbeta.woCOU(modSEAL_HANPP1950l_fourthsample,win.data=win.data))


detach(2)
#load("BSC_SEAL_fourthsample.RData")
save.image("BSC_SEAL_fourthsample.RData")
}



model.delta<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(SEAL.2009~I(scale(log(HANPP.1900.Dullinger),scale=F)),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(SEAL.2009~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

nc <- 3	;		
ni <- 200000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modSEAL_HANPP1900l_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modSEAL_HANPP1900l_fourthsample,file="modSEAL_HANPP1900l_fourthsample.RData")
rm(modSEAL_HANPP1900l_fourthsample)
}

if (runDIC)
{attach("modSEAL_HANPP1900l_fourthsample.RData",pos=2)
DIC.SEAL.fourthsample<-rbind(DIC.SEAL.fourthsample,HANPP1900l=DICZIbeta.woCOU(modSEAL_HANPP1900l_fourthsample,win.data=win.data))


detach(2)
#load("BSC_SEAL_fourthsample.RData")
save.image("BSC_SEAL_fourthsample.RData")
}

	

			
model.delta<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(SEAL.2009~I(scale((HANPP.2000.Dullinger/0.2),scale=F)),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(SEAL.2009~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

nc <- 3	;		
ni <- 200000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modSEAL_HANPP2000_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modSEAL_HANPP2000_fourthsample,file="modSEAL_HANPP2000_fourthsample.RData")
rm(modSEAL_HANPP2000_fourthsample)
}

if (runDIC)
{attach("modSEAL_HANPP2000_fourthsample.RData",pos=2)
DIC.SEAL.fourthsample<-rbind(DIC.SEAL.fourthsample,HANPP2000=DICZIbeta.woCOU(modSEAL_HANPP2000_fourthsample,win.data=win.data))


detach(2)
#load("BSC_SEAL_fourthsample.RData")
save.image("BSC_SEAL_fourthsample.RData")
}



			
	
	
			
model.delta<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(SEAL.2009~I(scale((HANPP.1950.Dullinger/0.2),scale=F)),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(SEAL.2009~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

nc <- 3	;		
ni <- 200000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modSEAL_HANPP1950_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modSEAL_HANPP1950_fourthsample,file="modSEAL_HANPP1950_fourthsample.RData")
rm(modSEAL_HANPP1950_fourthsample)
}

if (runDIC)
{attach("modSEAL_HANPP1950_fourthsample.RData",pos=2)
DIC.SEAL.fourthsample<-rbind(DIC.SEAL.fourthsample,HANPP1950=DICZIbeta.woCOU(modSEAL_HANPP1950_fourthsample,win.data=win.data))


detach(2)
#load("BSC_SEAL_fourthsample.RData")
save.image("BSC_SEAL_fourthsample.RData")
}




model.delta<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(SEAL.2009~I(scale((HANPP.1900.Dullinger/0.2),scale=F)),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(SEAL.2009~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

nc <- 3	;		
ni <- 200000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modSEAL_HANPP1900_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modSEAL_HANPP1900_fourthsample,file="modSEAL_HANPP1900_fourthsample.RData")
rm(modSEAL_HANPP1900_fourthsample)
}

if (runDIC)
{attach("modSEAL_HANPP1900_fourthsample.RData",pos=2)
DIC.SEAL.fourthsample<-rbind(DIC.SEAL.fourthsample,HANPP1900=DICZIbeta.woCOU(modSEAL_HANPP1900_fourthsample,win.data=win.data))


detach(2)
#load("BSC_SEAL_fourthsample.RData")
save.image("BSC_SEAL_fourthsample.RData")
}




model.delta<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(SEAL.2009~I(scale((POPd.Dullinger.1900/50),scale=F)),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(SEAL.2009~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
				
				
nc <- 3	;		
ni <- 200000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modSEAL_POPd_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modSEAL_POPd_fourthsample,file="modSEAL_POPd1900_fourthsample.RData")
rm(modSEAL_POPd_fourthsample)
}

if (runDIC)
{attach("modSEAL_POPd1900_fourthsample.RData",pos=2)
DIC.SEAL.fourthsample<-rbind(DIC.SEAL.fourthsample,POPd1900=DICZIbeta.woCOU(modSEAL_POPd_fourthsample,win.data=win.data))


detach(2)
#load("BSC_SEAL_fourthsample.RData")
save.image("BSC_SEAL_fourthsample.RData")
}







			
				

model.delta<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(SEAL.2009~I(scale(PIB1900aD,scale=F)/400)+I((scale(PIB1900aD,scale=F)/400)^2),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(SEAL.2009~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
nc <- 3	;		
ni <- 200000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modSEAL_Kuznets1900a_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modSEAL_Kuznets1900a_fourthsample,file="modSEAL_Kuznets1900a_fourthsample.RData")
rm(modSEAL_Kuznets1900a_fourthsample)
}

if (runDIC)
{attach("modSEAL_Kuznets1900a_fourthsample.RData",pos=2)
DIC.SEAL.fourthsample<-rbind(DIC.SEAL.fourthsample,Kuznets1900a=DICZIbeta.woCOU(modSEAL_Kuznets1900a_fourthsample,win.data=win.data))


detach(2)
#load("BSC_SEAL_fourthsample.RData")
save.image("BSC_SEAL_fourthsample.RData")
}













model.delta<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(SEAL.2009~I(scale(log(PIB1900aD/400),scale=F))+I((scale(log(PIB1900aD/400),scale=F))^2),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(SEAL.2009~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
nc <- 3	;		
ni <- 200000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modSEAL_lKuznets1900a_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modSEAL_lKuznets1900a_fourthsample,file="modSEAL_lKuznets1900a_fourthsample.RData")
rm(modSEAL_lKuznets1900a_fourthsample)
}

if (runDIC)
{attach("modSEAL_lKuznets1900a_fourthsample.RData",pos=2)
DIC.SEAL.fourthsample<-rbind(DIC.SEAL.fourthsample,lKuznets1900a=DICZIbeta.woCOU(modSEAL_lKuznets1900a_fourthsample,win.data=win.data))


detach(2)
#load("BSC_SEAL_fourthsample.RData")
save.image("BSC_SEAL_fourthsample.RData")
}



		






















model.delta<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(SEAL.2009~I(scale((PIB.1900.Dullinger/2),scale=F))+I(scale((POPd.Dullinger.1900/50),scale=F)),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(SEAL.2009~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
nc <- 3	;		
ni <- 200000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modSEAL_eco1900_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modSEAL_eco1900_fourthsample,file="modSEAL_eco1900_fourthsample.RData")
rm(modSEAL_eco1900_fourthsample)
}

if (runDIC)
{attach("modSEAL_eco1900_fourthsample.RData",pos=2)
DIC.SEAL.fourthsample<-rbind(DIC.SEAL.fourthsample,eco1900=DICZIbeta.woCOU(modSEAL_eco1900_fourthsample,win.data=win.data))


detach(2)
#load("BSC_SEAL_fourthsample.RData")
save.image("BSC_SEAL_fourthsample.RData")
}


		

#load("BSC_SEAL_fourthsample.RData")
#save.image("BSC_SEAL_fourthsample.RData")

model.delta<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(SEAL.2009~I(scale(log(PIB.1900.Dullinger/2),scale=F))+I(scale(log(POPd.Dullinger.1900/50),scale=F)),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(SEAL.2009~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
nc <- 3	;		
ni <- 200000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modSEAL_leco1900_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modSEAL_leco1900_fourthsample,file="modSEAL_leco1900_fourthsample.RData")
rm(modSEAL_leco1900_fourthsample)
}

if (runDIC)
{attach("modSEAL_leco1900_fourthsample.RData",pos=2)
DIC.SEAL.fourthsample<-rbind(DIC.SEAL.fourthsample,leco1900=DICZIbeta.woCOU(modSEAL_leco1900_fourthsample,win.data=win.data))


detach(2)
#load("BSC_SEAL_fourthsample.RData")
save.image("BSC_SEAL_fourthsample.RData")
}






model.delta<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(SEAL.2009~I(scale(PIBa,scale=F)/50000)+I((scale(PIBa,scale=F)/50000)^2),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(SEAL.2009~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))


nc <- 3	;		
ni <- 200000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modSEAL_Kuznetsa_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modSEAL_Kuznetsa_fourthsample,file="modSEAL_Kuznetsa_fourthsample.RData")
rm(modSEAL_Kuznetsa_fourthsample)
}

if (runDIC)
{attach("modSEAL_Kuznetsa_fourthsample.RData",pos=2)
DIC.SEAL.fourthsample<-rbind(DIC.SEAL.fourthsample,Kuznetsa=DICZIbeta.woCOU(modSEAL_Kuznetsa_fourthsample,win.data=win.data))


detach(2)
#load("BSC_SEAL_fourthsample.RData")
save.image("BSC_SEAL_fourthsample.RData")
}



		

model.delta<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(SEAL.2009~I(scale(log((PIB2000aD)/50000),scale=F))+I(scale(log((PIB2000aD)/50000),scale=F)^2),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(SEAL.2009~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))


nc <- 3	;		
ni <- 200000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modSEAL_lKuznetsa_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modSEAL_lKuznetsa_fourthsample,file="modSEAL_lKuznetsa_fourthsample.RData")
rm(modSEAL_lKuznetsa_fourthsample)
}

if (runDIC)
{attach("modSEAL_lKuznetsa_fourthsample.RData",pos=2)
if (is.element("lKuznetsa",dimnames(DIC.SEAL.fourthsample)[[1]]))
{DIC.SEAL.fourthsample["lKuznetsa",]<-DICZIbeta.woCOU(modSEAL_lKuznetsa_fourthsample,win.data=win.data)}
else
{
DIC.SEAL.fourthsample<-rbind(DIC.SEAL.fourthsample,lKuznetsa=DICZIbeta.woCOU(modSEAL_lKuznetsa_fourthsample,win.data=win.data))
}

detach(2)
#load("BSC_SEAL_fourthsample.RData")
save.image("BSC_SEAL_fourthsample.RData")
}





model.delta<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(SEAL.2009~I(scale(log(PIBh/10000),scale=F))+I(scale(log(POPd/100),scale=F)),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(SEAL.2009~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))


nc <- 3	;		
ni <- 200000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modSEAL_leco_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modSEAL_leco_fourthsample,file="modSEAL_leco_fourthsample.RData")
rm(modSEAL_leco_fourthsample)
}

if (runDIC)
{attach("modSEAL_leco_fourthsample.RData",pos=2)
DIC.SEAL.fourthsample<-rbind(DIC.SEAL.fourthsample,leco=DICZIbeta.woCOU(modSEAL_leco_fourthsample,win.data=win.data))


detach(2)
#load("BSC_SEAL_fourthsample.RData")
save.image("BSC_SEAL_fourthsample.RData")
}


		

		


model.delta<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(SEAL.2009~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))


nc <- 3	;		
ni <- 200000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modSEAL_Null_fourthsample <- stan(model_code = stanmodel.delta1.ne1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modSEAL_Null_fourthsample,file="modSEAL_Null_fourthsample.RData")
rm(modSEAL_Null_fourthsample)
}

if (runDIC)
{attach("modSEAL_Null_fourthsample.RData",pos=2)
DIC.SEAL.fourthsample<-rbind(DIC.SEAL.fourthsample,Null=DICZIbeta.woCOU(modSEAL_Null_fourthsample,win.data=win.data))

detach(2)
#load("BSC_SEAL_fourthsample.RData")
save.image("BSC_SEAL_fourthsample.RData")
}




		
		
			
			
			

			
			

					
model.delta<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(SEAL.2009~I(scale((PIB2000aD/2400),scale=F)),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(SEAL.2009~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

nc <- 3	;		
ni <- 200000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modSEAL_GDP2000a_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modSEAL_GDP2000a_fourthsample,file="modSEAL_GDP2000a_fourthsample.RData")
rm(modSEAL_GDP2000a_fourthsample)
}

if (runDIC)
{attach("modSEAL_GDP2000a_fourthsample.RData",pos=2)
DIC.SEAL.fourthsample<-rbind(DIC.SEAL.fourthsample,GDP2000a=DICZIbeta.woCOU(modSEAL_GDP2000a_fourthsample,win.data=win.data))


detach(2)
#load("BSC_SEAL_fourthsample.RData")
save.image("BSC_SEAL_fourthsample.RData")
}



			
model.delta<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(SEAL.2009~I(scale((PIB1950aD/400),scale=F)),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(SEAL.2009~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

nc <- 3	;		
ni <- 200000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modSEAL_GDP1950a_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modSEAL_GDP1950a_fourthsample,file="modSEAL_GDP1950a_fourthsample.RData")
rm(modSEAL_GDP1950a_fourthsample)
}

if (runDIC)
{attach("modSEAL_GDP1950a_fourthsample.RData",pos=2)
DIC.SEAL.fourthsample<-rbind(DIC.SEAL.fourthsample,GDP1950a=DICZIbeta.woCOU(modSEAL_GDP1950a_fourthsample,win.data=win.data))


detach(2)
#load("BSC_SEAL_fourthsample.RData")
save.image("BSC_SEAL_fourthsample.RData")
}


model.delta<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(SEAL.2009~I(scale((PIB1900aD/400),scale=F)),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(SEAL.2009~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

nc <- 3	;		
ni <- 200000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modSEAL_GDP1900a_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modSEAL_GDP1900a_fourthsample,file="modSEAL_GDP1900a_fourthsample.RData")
rm(modSEAL_GDP1900a_fourthsample)
}

if (runDIC)
{attach("modSEAL_GDP1900a_fourthsample.RData",pos=2)
DIC.SEAL.fourthsample<-rbind(DIC.SEAL.fourthsample,GDP1900a=DICZIbeta.woCOU(modSEAL_GDP1900a_fourthsample,win.data=win.data))


detach(2)
#load("BSC_SEAL_fourthsample.RData")
save.image("BSC_SEAL_fourthsample.RData")
}







model.delta<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(SEAL.2009~I(scale(log(POPd.Dullinger.1900/50),scale=F)),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(SEAL.2009~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
				
				
nc <- 3	;		
ni <- 200000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modSEAL_POPd1900l_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modSEAL_POPd1900l_fourthsample,file="modSEAL_POPd1900l_fourthsample.RData")
rm(modSEAL_POPd1900l_fourthsample)
}

if (runDIC)
{attach("modSEAL_POPd1900l_fourthsample.RData",pos=2)
DIC.SEAL.fourthsample<-rbind(DIC.SEAL.fourthsample,POPd1900l=DICZIbeta.woCOU(modSEAL_POPd1900l_fourthsample,win.data=win.data))


detach(2)
#load("BSC_SEAL_fourthsample.RData")
save.image("BSC_SEAL_fourthsample.RData")
}






			

model.delta<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(SEAL.2009~I(scale((POPd.Dullinger.1950/50),scale=F)),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(SEAL.2009~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
				
				
nc <- 3	;		
ni <- 200000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modSEAL_POPd1950_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modSEAL_POPd1950_fourthsample,file="modSEAL_POPd1950_fourthsample.RData")
rm(modSEAL_POPd1950_fourthsample)
}

if (runDIC)
{attach("modSEAL_POPd1950_fourthsample.RData",pos=2)
DIC.SEAL.fourthsample<-rbind(DIC.SEAL.fourthsample,POPd1950=DICZIbeta.woCOU(modSEAL_POPd1950_fourthsample,win.data=win.data))


detach(2)
#load("BSC_SEAL_fourthsample.RData")
save.image("BSC_SEAL_fourthsample.RData")
}









model.delta<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(SEAL.2009~I(scale(log(POPd.Dullinger.1950/50),scale=F)),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(SEAL.2009~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
				
				
nc <- 3	;		
ni <- 200000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modSEAL_POPd1950l_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modSEAL_POPd1950l_fourthsample,file="modSEAL_POPd1950l_fourthsample.RData")
rm(modSEAL_POPd1950l_fourthsample)
}

if (runDIC)
{attach("modSEAL_POPd1950l_fourthsample.RData",pos=2)
DIC.SEAL.fourthsample<-rbind(DIC.SEAL.fourthsample,POPd1950l=DICZIbeta.woCOU(modSEAL_POPd1950l_fourthsample,win.data=win.data))


detach(2)
#load("BSC_SEAL_fourthsample.RData")
save.image("BSC_SEAL_fourthsample.RData")
}








model.delta<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(SEAL.2009~I(scale((POPd.Dullinger.2000/50),scale=F)),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(SEAL.2009~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
				
				
nc <- 3	;		
ni <- 200000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modSEAL_POPd2000_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modSEAL_POPd2000_fourthsample,file="modSEAL_POPd2000_fourthsample.RData")
rm(modSEAL_POPd2000_fourthsample)
}

if (runDIC)
{attach("modSEAL_POPd2000_fourthsample.RData",pos=2)
DIC.SEAL.fourthsample<-rbind(DIC.SEAL.fourthsample,POPd2000=DICZIbeta.woCOU(modSEAL_POPd2000_fourthsample,win.data=win.data))


detach(2)
#load("BSC_SEAL_fourthsample.RData")
save.image("BSC_SEAL_fourthsample.RData")
}





model.delta<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(SEAL.2009~I(scale(log(POPd.Dullinger.2000/50),scale=F)),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(SEAL.2009~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
				
				
nc <- 3	;		
ni <- 200000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modSEAL_POPd2000l_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modSEAL_POPd2000l_fourthsample,file="modSEAL_POPd2000l_fourthsample.RData")
rm(modSEAL_POPd2000l_fourthsample)
}

if (runDIC)
{attach("modSEAL_POPd2000l_fourthsample.RData",pos=2)
DIC.SEAL.fourthsample<-rbind(DIC.SEAL.fourthsample,POPd2000l=DICZIbeta.woCOU(modSEAL_POPd2000l_fourthsample,win.data=win.data))


detach(2)
#load("BSC_SEAL_fourthsample.RData")
save.image("BSC_SEAL_fourthsample.RData")
}


					
							
							

model.delta<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(SEAL.2009~I(scale((PIBh/10000),scale=F))+I(scale((POPd/100),scale=F)),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(SEAL.2009~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))


nc <- 3	;		
ni <- 200000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modSEAL_eco_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modSEAL_eco_fourthsample,file="modSEAL_eco_fourthsample.RData")
rm(modSEAL_eco_fourthsample)
}

if (runDIC)
{attach("modSEAL_eco_fourthsample.RData",pos=2)
DIC.SEAL.fourthsample<-rbind(DIC.SEAL.fourthsample,eco=DICZIbeta.woCOU(modSEAL_eco_fourthsample,win.data=win.data))


detach(2)
#load("BSC_SEAL_fourthsample.RData")
save.image("BSC_SEAL_fourthsample.RData")
}


	
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		


model.delta<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(SEAL.2009~I(scale(PIB1950aD,scale=F)/400)+I((scale(PIB1950aD,scale=F)/400)^2),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(SEAL.2009~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
nc <- 3	;		
ni <- 200000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modSEAL_Kuznets1950a_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modSEAL_Kuznets1950a_fourthsample,file="modSEAL_Kuznets1950a_fourthsample.RData")
rm(modSEAL_Kuznets1950a_fourthsample)
}

if (runDIC)
{attach("modSEAL_Kuznets1950a_fourthsample.RData",pos=2)
DIC.SEAL.fourthsample<-rbind(DIC.SEAL.fourthsample,Kuznets1950a=DICZIbeta.woCOU(modSEAL_Kuznets1950a_fourthsample,win.data=win.data))


detach(2)
#load("BSC_SEAL_fourthsample.RData")
save.image("BSC_SEAL_fourthsample.RData")
}










model.delta<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(SEAL.2009~I(scale(log(PIB1950aD/400),scale=F))+I((scale(log(PIB1950aD/400),scale=F))^2),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(SEAL.2009~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
nc <- 3	;		
ni <- 200000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modSEAL_lKuznets1950a_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modSEAL_lKuznets1950a_fourthsample,file="modSEAL_lKuznets1950a_fourthsample.RData")
rm(modSEAL_lKuznets1950a_fourthsample)
}

if (runDIC)
{attach("modSEAL_lKuznets1950a_fourthsample.RData",pos=2)
DIC.SEAL.fourthsample<-rbind(DIC.SEAL.fourthsample,lKuznets1950a=DICZIbeta.woCOU(modSEAL_lKuznets1950a_fourthsample,win.data=win.data))

detach(2)
#load("BSC_SEAL_fourthsample.RData")
save.image("BSC_SEAL_fourthsample.RData")
}










model.delta<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(SEAL.2009~I(scale((PIB.1950.Dullinger/2),scale=F))+I(scale((POPd.Dullinger.1950/50),scale=F)),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(SEAL.2009~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
nc <- 3	;		
ni <- 200000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modSEAL_eco1950_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modSEAL_eco1950_fourthsample,file="modSEAL_eco1950_fourthsample.RData")
rm(modSEAL_eco1950_fourthsample)
}

if (runDIC)
{attach("modSEAL_eco1950_fourthsample.RData",pos=2)
DIC.SEAL.fourthsample<-rbind(DIC.SEAL.fourthsample,eco1950=DICZIbeta.woCOU(modSEAL_eco1950_fourthsample,win.data=win.data))


detach(2)
#load("BSC_SEAL_fourthsample.RData")
save.image("BSC_SEAL_fourthsample.RData")
}

	
		

#load("BSC_SEAL_fourthsample.RData")
#save.image("BSC_SEAL_fourthsample.RData")

model.delta<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(SEAL.2009~I(scale(log(PIB.1950.Dullinger/2),scale=F))+I(scale(log(POPd.Dullinger.1950/50),scale=F)),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(SEAL.2009~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
nc <- 3	;		
ni <- 200000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modSEAL_leco1950_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modSEAL_leco1950_fourthsample,file="modSEAL_leco1950_fourthsample.RData")
rm(modSEAL_leco1950_fourthsample)
}

if (runDIC)
{attach("modSEAL_leco1950_fourthsample.RData",pos=2)
DIC.SEAL.fourthsample<-rbind(DIC.SEAL.fourthsample,leco1950=DICZIbeta.woCOU(modSEAL_leco1950_fourthsample,win.data=win.data))

detach(2)
#load("BSC_SEAL_fourthsample.RData")
save.image("BSC_SEAL_fourthsample.RData")
}



	
	
	
	
	
	
model.delta<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(SEAL.2009~I(scale(PIB2000aD,scale=F)/2400)+I((scale(PIB2000aD,scale=F)/2400)^2),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(SEAL.2009~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))


nc <- 3	;		
ni <- 200000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modSEAL_Kuznets2000a_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modSEAL_Kuznets2000a_fourthsample,file="modSEAL_Kuznets2000a_fourthsample.RData")
rm(modSEAL_Kuznets2000a_fourthsample)
}

if (runDIC)
{attach("modSEAL_Kuznets2000a_fourthsample.RData",pos=2)
DIC.SEAL.fourthsample<-rbind(DIC.SEAL.fourthsample,Kuznets2000a=DICZIbeta.woCOU(modSEAL_Kuznets2000a_fourthsample,win.data=win.data))


detach(2)
#load("BSC_SEAL_fourthsample.RData")
save.image("BSC_SEAL_fourthsample.RData")
}





model.delta<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(SEAL.2009~I(scale(log(PIB2000aD/2400),scale=F))+I((scale(log(PIB2000aD/2400),scale=F))^2),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(SEAL.2009~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))


nc <- 3	;		
ni <- 200000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modSEAL_lKuznets2000a_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modSEAL_lKuznets2000a_fourthsample,file="modSEAL_lKuznets2000a_fourthsample.RData")
rm(modSEAL_lKuznets2000a_fourthsample)
}

if (runDIC)
{attach("modSEAL_lKuznets2000a_fourthsample.RData",pos=2)
if (is.element("lKuznets2000a",dimnames(DIC.SEAL.fourthsample)[[1]]))
{DIC.SEAL.fourthsample["lKuznets2000a",]<-DICZIbeta.woCOU(modSEAL_lKuznets2000a_fourthsample,win.data=win.data)}
else
{
DIC.SEAL.fourthsample<-rbind(DIC.SEAL.fourthsample,lKuznets2000a=DICZIbeta.woCOU(modSEAL_lKuznets2000a_fourthsample,win.data=win.data))
}

detach(2)
#load("BSC_SEAL_fourthsample.RData")
save.image("BSC_SEAL_fourthsample.RData")
}



	

						

model.delta<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(SEAL.2009~I((scale(log(PIB.2000.Dullinger/7),scale=F)))+I(scale(log(POPd.Dullinger.2000/100),scale=F)),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(SEAL.2009~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))


nc <- 3	;		
ni <- 200000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modSEAL_leco2000_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modSEAL_leco2000_fourthsample,file="modSEAL_leco2000_fourthsample.RData")
rm(modSEAL_leco2000_fourthsample)
}

if (runDIC)
{attach("modSEAL_leco2000_fourthsample.RData",pos=2)
DIC.SEAL.fourthsample<-rbind(DIC.SEAL.fourthsample,leco2000=DICZIbeta.woCOU(modSEAL_leco2000_fourthsample,win.data=win.data))


detach(2)
#load("BSC_SEAL_fourthsample.RData")
save.image("BSC_SEAL_fourthsample.RData")
}




model.delta<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(SEAL.2009~I((scale((PIB.2000.Dullinger/7),scale=F)))+I(scale((POPd.Dullinger.2000/100),scale=F)),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(SEAL.2009~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))


nc <- 3	;		
ni <- 200000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modSEAL_eco2000_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modSEAL_eco2000_fourthsample,file="modSEAL_eco2000_fourthsample.RData")
rm(modSEAL_eco2000_fourthsample)
}

if (runDIC)
{attach("modSEAL_eco2000_fourthsample.RData",pos=2)
DIC.SEAL.fourthsample<-rbind(DIC.SEAL.fourthsample,eco2000=DICZIbeta.woCOU(modSEAL_eco2000_fourthsample,win.data=win.data))


detach(2)
#load("BSC_SEAL_fourthsample.RData")
save.image("BSC_SEAL_fourthsample.RData")
}

	
		




model.delta<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(SEAL.2009~I(scale(PIB.2000.Dullinger/7,scale=F))+I((scale(PIB.2000.Dullinger/7,scale=F))^2),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(SEAL.2009~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))


nc <- 3	;		
ni <- 200000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modSEAL_Kuznets2000_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modSEAL_Kuznets2000_fourthsample,file="modSEAL_Kuznets2000_fourthsample.RData")
rm(modSEAL_Kuznets2000_fourthsample)
}

if (runDIC)
{attach("modSEAL_Kuznets2000_fourthsample.RData",pos=2)
DIC.SEAL.fourthsample<-rbind(DIC.SEAL.fourthsample,Kuznets2000=DICZIbeta.woCOU(modSEAL_Kuznets2000_fourthsample,win.data=win.data))


detach(2)
#load("BSC_SEAL_fourthsample.RData")
save.image("BSC_SEAL_fourthsample.RData")
}




		

model.delta<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(SEAL.2009~I(scale(log(PIB.2000.Dullinger/7),scale=F))+I((scale(log(PIB.2000.Dullinger/7),scale=F))^2),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(SEAL.2009~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))


nc <- 3	;		
ni <- 200000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modSEAL_lKuznets2000_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modSEAL_lKuznets2000_fourthsample,file="modSEAL_lKuznets2000_fourthsample.RData")
rm(modSEAL_lKuznets2000_fourthsample)
}

if (runDIC)
{attach("modSEAL_lKuznets2000_fourthsample.RData",pos=2)
if (is.element("lKuznets2000",dimnames(DIC.SEAL.fourthsample)[[1]]))
{DIC.SEAL.fourthsample["lKuznets2000",]<-DICZIbeta.woCOU(modSEAL_lKuznets2000_fourthsample,win.data=win.data)}
else
{
DIC.SEAL.fourthsample<-rbind(DIC.SEAL.fourthsample,lKuznets2000=DICZIbeta.woCOU(modSEAL_lKuznets2000_fourthsample,win.data=win.data))
}

detach(2)
#load("BSC_SEAL_fourthsample.RData")
save.image("BSC_SEAL_fourthsample.RData")
}









			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			


model.delta<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(SEAL.2009~I(scale(PIB.1950.Dullinger/2,scale=F))+I((scale(PIB.1950.Dullinger/2,scale=F))^2),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(SEAL.2009~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))


nc <- 3	;		
ni <- 195000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modSEAL_Kuznets1950_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modSEAL_Kuznets1950_fourthsample,file="modSEAL_Kuznets1950_fourthsample.RData")
rm(modSEAL_Kuznets1950_fourthsample)
}

if (runDIC)
{attach("modSEAL_Kuznets1950_fourthsample.RData",pos=2)
DIC.SEAL.fourthsample<-rbind(DIC.SEAL.fourthsample,Kuznets1950=DICZIbeta.woCOU(modSEAL_Kuznets1950_fourthsample,win.data=win.data))


detach(2)
#load("BSC_SEAL_fourthsample.RData")
save.image("BSC_SEAL_fourthsample.RData")
}





model.delta<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(SEAL.2009~I(scale(log(PIB.1950.Dullinger/2),scale=F))+I((scale(log(PIB.1950.Dullinger/2),scale=F))^2),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(SEAL.2009~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))


nc <- 3	;		
ni <- 195000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modSEAL_lKuznets1950_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modSEAL_lKuznets1950_fourthsample,file="modSEAL_lKuznets1950_fourthsample.RData")
rm(modSEAL_lKuznets1950_fourthsample)
}

if (runDIC)
{attach("modSEAL_lKuznets1950_fourthsample.RData",pos=2)
if (is.element("lKuznets1950",dimnames(DIC.SEAL.fourthsample)[[1]]))
{DIC.SEAL.fourthsample["lKuznets1950",]<-DICZIbeta.woCOU(modSEAL_lKuznets1950_fourthsample,win.data=win.data)}
else
{
DIC.SEAL.fourthsample<-rbind(DIC.SEAL.fourthsample,lKuznets1950=DICZIbeta.woCOU(modSEAL_lKuznets1950_fourthsample,win.data=win.data))
}

detach(2)
#load("BSC_SEAL_fourthsample.RData")
save.image("BSC_SEAL_fourthsample.RData")
}


			
			
			
			
			
			
			
			
			


model.delta<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(SEAL.2009~I(scale(PIB.1900.Dullinger/1,scale=F))+I((scale(PIB.1900.Dullinger/1,scale=F))^2),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(SEAL.2009~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))


nc <- 3	;		
ni <- 190000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modSEAL_Kuznets1900_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modSEAL_Kuznets1900_fourthsample,file="modSEAL_Kuznets1900_fourthsample.RData")
rm(modSEAL_Kuznets1900_fourthsample)
}

if (runDIC)
{attach("modSEAL_Kuznets1900_fourthsample.RData",pos=2)
DIC.SEAL.fourthsample<-rbind(DIC.SEAL.fourthsample,Kuznets1900=DICZIbeta.woCOU(modSEAL_Kuznets1900_fourthsample,win.data=win.data))


detach(2)
#load("BSC_SEAL_fourthsample.RData")
save.image("BSC_SEAL_fourthsample.RData")
}





model.delta<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(SEAL.2009~I(scale(log(PIB.1900.Dullinger/1),scale=F))+I((scale(log(PIB.1900.Dullinger/1),scale=F))^2),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(SEAL.2009~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))


nc <- 3	;		
ni <- 190000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modSEAL_lKuznets1900_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modSEAL_lKuznets1900_fourthsample,file="modSEAL_lKuznets1900_fourthsample.RData")
rm(modSEAL_lKuznets1900_fourthsample)
}

if (runDIC)
{attach("modSEAL_lKuznets1900_fourthsample.RData",pos=2)
if (is.element("lKuznets1900",dimnames(DIC.SEAL.fourthsample)[[1]]))
{DIC.SEAL.fourthsample["lKuznets1900",]<-DICZIbeta.woCOU(modSEAL_lKuznets1900_fourthsample,win.data=win.data)}
else
{
DIC.SEAL.fourthsample<-rbind(DIC.SEAL.fourthsample,lKuznets1900=DICZIbeta.woCOU(modSEAL_lKuznets1900_fourthsample,win.data=win.data))
}

detach(2)
#load("BSC_SEAL_fourthsample.RData")
save.image("BSC_SEAL_fourthsample.RData")
}

}


######## analysis of parameter estimators (Table 6)
{
library(rstan)
if (Sys.info()["sysname"]=="Windows") {setwd(wd.RData)} 
load("BSC_Data_functions.RData")

Estimates_SEAL_fourthsample<-data.frame(NA,ncol=3,nrow=28)
dimnames(Estimates_SEAL_fourthsample)[[2]]<-c("Summary_est","pbeta","magnitude")

attach("modSEAL_POPd2000l_fourthsample.RData",pos=2)

set.seed(1)
model.to.diagnose<-modSEAL_POPd2000l_fourthsample
temp<-interp.magnsign(model.to.diagnose,mult=0.89,index=2)
Estimates_SEAL_fourthsample[1,]<-c(temp[1],temp[2],temp[3])
detach(2)

attach("modSEAL_POPd1950l_fourthsample.RData",pos=2)

set.seed(1)
model.to.diagnose<-modSEAL_POPd1950l_fourthsample
temp<-interp.magnsign(model.to.diagnose,mult=0.89,index=2)
Estimates_SEAL_fourthsample[2,]<-c(temp[1],temp[2],temp[3])
detach(2)

attach("modSEAL_POPd1900l_fourthsample.RData",pos=2)

set.seed(1)
model.to.diagnose<-modSEAL_POPd1900l_fourthsample
temp<-interp.magnsign(model.to.diagnose,mult=0.89,index=2)
Estimates_SEAL_fourthsample[3,]<-c(temp[1],temp[2],temp[3])
detach(2)


setwd(wd.csv)
write.csv(Estimates_SEAL_fourthsample,file="Estimates_SEAL_fourthsample.csv")




}


}



########## iSEAL
{
###### model fitting & IC calculation (Tables 2 & 4)
{
if (Sys.info()["sysname"]=="Windows") {setwd(wd.RData)} 
load("BSC_Data_functions.RData")

#### parameters used to control which part of the process to do : estimation (runStan) or calculation of model comparison indices (runDIC)
runStan<-TRUE
runDIC<-TRUE

DIC.iSEALca.fourthsample<-matrix(NA,ncol=11,nrow=1)
dimnames(DIC.iSEALca.fourthsample)<-list(c(
			
			"SEAL.2009"),c("DIC", "DICvar","DICmin", "IC",  "pDvar",  "pD1.WAIC",        "looic", "waic",   "propkfair",  "propkbad", "propkverybad"))


			

	
			
			
			
			
			
					
model.delta<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(INC.SEAL.CA~I(scale(log(PIB2000aD/400),scale=F)),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(INC.SEAL.CA~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

nc <- 3	;		
ni <- 200000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modiSEALca_GDP2000al_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modiSEALca_GDP2000al_fourthsample,file="modiSEALca_GDP2000al_fourthsample.RData")
rm(modiSEALca_GDP2000al_fourthsample)
}

if (runDIC)
{attach("modiSEALca_GDP2000al_fourthsample.RData",pos=2)
DIC.iSEALca.fourthsample<-rbind(DIC.iSEALca.fourthsample,GDP2000al=DICZIbeta.woCOU(modiSEALca_GDP2000al_fourthsample,win.data=win.data))


detach(2)
#load("BSC_iSEALca_fourthsample.RData")
save.image("BSC_iSEALca_fourthsample.RData")
}

				
			
			
			
model.delta<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(INC.SEAL.CA~I(scale(log(PIB1950aD/400),scale=F)),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(INC.SEAL.CA~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

nc <- 3	;		
ni <- 200000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modiSEALca_GDP1950al_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modiSEALca_GDP1950al_fourthsample,file="modiSEALca_GDP1950al_fourthsample.RData")
rm(modiSEALca_GDP1950al_fourthsample)
}

if (runDIC)
{attach("modiSEALca_GDP1950al_fourthsample.RData",pos=2)
DIC.iSEALca.fourthsample<-rbind(DIC.iSEALca.fourthsample,GDP1950al=DICZIbeta.woCOU(modiSEALca_GDP1950al_fourthsample,win.data=win.data))


detach(2)
#load("BSC_iSEALca_fourthsample.RData")
save.image("BSC_iSEALca_fourthsample.RData")
}




model.delta<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(INC.SEAL.CA~I(scale(log(PIB1900aD/400),scale=F)),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(INC.SEAL.CA~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

nc <- 3	;		
ni <- 200000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modiSEALca_GDP1900al_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modiSEALca_GDP1900al_fourthsample,file="modiSEALca_GDP1900al_fourthsample.RData")
rm(modiSEALca_GDP1900al_fourthsample)
}

if (runDIC)
{attach("modiSEALca_GDP1900al_fourthsample.RData",pos=2)
DIC.iSEALca.fourthsample<-rbind(DIC.iSEALca.fourthsample,GDP1900al=DICZIbeta.woCOU(modiSEALca_GDP1900al_fourthsample,win.data=win.data))


detach(2)
#load("BSC_iSEALca_fourthsample.RData")
save.image("BSC_iSEALca_fourthsample.RData")
}



			
model.delta<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(INC.SEAL.CA~I(scale(log(HANPP.2000.Dullinger),scale=F)),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(INC.SEAL.CA~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

nc <- 3	;		
ni <- 200000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modiSEALca_HANPP2000l_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modiSEALca_HANPP2000l_fourthsample,file="modiSEALca_HANPP2000l_fourthsample.RData")
rm(modiSEALca_HANPP2000l_fourthsample)
}

if (runDIC)
{attach("modiSEALca_HANPP2000l_fourthsample.RData",pos=2)
DIC.iSEALca.fourthsample<-rbind(DIC.iSEALca.fourthsample,HANPP2000l=DICZIbeta.woCOU(modiSEALca_HANPP2000l_fourthsample,win.data=win.data))


detach(2)
#load("BSC_iSEALca_fourthsample.RData")
save.image("BSC_iSEALca_fourthsample.RData")
}



			
	if (FALSE)
{attach("modiSEALca_HANPP2000l_fourthsample.RData",pos=2)

dimnames(DICmarginal.extinct.fourthsample.supp.muchlonger.byTaxon)[[1]]<-c(dimnames(DICmarginal.extinct.fourthsample.supp.muchlonger.byTaxon)[[1]][!is.na(dimnames(DICmarginal.extinct.fourthsample.supp.muchlonger.byTaxon)[[1]])],"HANPP2000l",rep(NA,sum(is.na(dimnames(DICmarginal.extinct.fourthsample.supp.muchlonger.byTaxon)[[1]]))-1))
dimnames(DICmarginal1000r.extinct.fourthsample.supp.muchlonger.byTaxon)[[1]]<-c(dimnames(DICmarginal1000r.extinct.fourthsample.supp.muchlonger.byTaxon)[[1]][!is.na(dimnames(DICmarginal1000r.extinct.fourthsample.supp.muchlonger.byTaxon)[[1]])],"HANPP2000l",rep(NA,sum(is.na(dimnames(DICmarginal1000r.extinct.fourthsample.supp.muchlonger.byTaxon)[[1]]))-1))
DICmarginal.extinct.fourthsample.supp.muchlonger.byTaxon["HANPP2000l",,]<-DICmarginal.stanmodel.heteroscbetabinom.byTaxon(modiSEALca_HANPP2000l_fourthsample,win.data=win.data)

DICmarginal1000r.extinct.fourthsample.supp.muchlonger.byTaxon["HANPP2000l",,]<-DICmarginal.stanmodel.heteroscbetabinom.byTaxon(modiSEALca_HANPP2000l_fourthsample,win.data=win.data,rep.inner=1000)

detach(2)
#load("BSC_iSEALca_firstsample_muchlonger.RData")
save.image("BSC_iSEALca_fourthsample_supp_muchlonger_byTaxon.RData")}

	
			
model.delta<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(INC.SEAL.CA~I(scale(log(HANPP.1950.Dullinger),scale=F)),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(INC.SEAL.CA~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

nc <- 3	;		
ni <- 200000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modiSEALca_HANPP1950l_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modiSEALca_HANPP1950l_fourthsample,file="modiSEALca_HANPP1950l_fourthsample.RData")
rm(modiSEALca_HANPP1950l_fourthsample)
}

if (runDIC)
{attach("modiSEALca_HANPP1950l_fourthsample.RData",pos=2)
DIC.iSEALca.fourthsample<-rbind(DIC.iSEALca.fourthsample,HANPP1950l=DICZIbeta.woCOU(modiSEALca_HANPP1950l_fourthsample,win.data=win.data))


detach(2)
#load("BSC_iSEALca_fourthsample.RData")
save.image("BSC_iSEALca_fourthsample.RData")
}



model.delta<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(INC.SEAL.CA~I(scale(log(HANPP.1900.Dullinger),scale=F)),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(INC.SEAL.CA~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

nc <- 3	;		
ni <- 200000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modiSEALca_HANPP1900l_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modiSEALca_HANPP1900l_fourthsample,file="modiSEALca_HANPP1900l_fourthsample.RData")
rm(modiSEALca_HANPP1900l_fourthsample)
}

if (runDIC)
{attach("modiSEALca_HANPP1900l_fourthsample.RData",pos=2)
DIC.iSEALca.fourthsample<-rbind(DIC.iSEALca.fourthsample,HANPP1900l=DICZIbeta.woCOU(modiSEALca_HANPP1900l_fourthsample,win.data=win.data))


detach(2)
#load("BSC_iSEALca_fourthsample.RData")
save.image("BSC_iSEALca_fourthsample.RData")
}



			
model.delta<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(INC.SEAL.CA~I(scale((HANPP.2000.Dullinger/0.2),scale=F)),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(INC.SEAL.CA~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

nc <- 3	;		
ni <- 200000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modiSEALca_HANPP2000_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modiSEALca_HANPP2000_fourthsample,file="modiSEALca_HANPP2000_fourthsample.RData")
rm(modiSEALca_HANPP2000_fourthsample)
}

if (runDIC)
{attach("modiSEALca_HANPP2000_fourthsample.RData",pos=2)
DIC.iSEALca.fourthsample<-rbind(DIC.iSEALca.fourthsample,HANPP2000=DICZIbeta.woCOU(modiSEALca_HANPP2000_fourthsample,win.data=win.data))


detach(2)
#load("BSC_iSEALca_fourthsample.RData")
save.image("BSC_iSEALca_fourthsample.RData")
}



			
model.delta<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(INC.SEAL.CA~I(scale((HANPP.1950.Dullinger/0.2),scale=F)),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(INC.SEAL.CA~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

nc <- 3	;		
ni <- 200000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modiSEALca_HANPP1950_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modiSEALca_HANPP1950_fourthsample,file="modiSEALca_HANPP1950_fourthsample.RData")
rm(modiSEALca_HANPP1950_fourthsample)
}

if (runDIC)
{attach("modiSEALca_HANPP1950_fourthsample.RData",pos=2)
DIC.iSEALca.fourthsample<-rbind(DIC.iSEALca.fourthsample,HANPP1950=DICZIbeta.woCOU(modiSEALca_HANPP1950_fourthsample,win.data=win.data))


detach(2)
#load("BSC_iSEALca_fourthsample.RData")
save.image("BSC_iSEALca_fourthsample.RData")
}




model.delta<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(INC.SEAL.CA~I(scale((HANPP.1900.Dullinger/0.2),scale=F)),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(INC.SEAL.CA~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

nc <- 3	;		
ni <- 200000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modiSEALca_HANPP1900_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modiSEALca_HANPP1900_fourthsample,file="modiSEALca_HANPP1900_fourthsample.RData")
rm(modiSEALca_HANPP1900_fourthsample)
}

if (runDIC)
{attach("modiSEALca_HANPP1900_fourthsample.RData",pos=2)
DIC.iSEALca.fourthsample<-rbind(DIC.iSEALca.fourthsample,HANPP1900=DICZIbeta.woCOU(modiSEALca_HANPP1900_fourthsample,win.data=win.data))


detach(2)
#load("BSC_iSEALca_fourthsample.RData")
save.image("BSC_iSEALca_fourthsample.RData")
}





model.delta<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(INC.SEAL.CA~I(scale((POPd.Dullinger.1900/50),scale=F)),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(INC.SEAL.CA~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
				
				
nc <- 3	;		
ni <- 200000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modiSEALca_POPd_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modiSEALca_POPd_fourthsample,file="modiSEALca_POPd1900_fourthsample.RData")
rm(modiSEALca_POPd_fourthsample)
}

if (runDIC)
{attach("modiSEALca_POPd1900_fourthsample.RData",pos=2)
DIC.iSEALca.fourthsample<-rbind(DIC.iSEALca.fourthsample,POPd1900=DICZIbeta.woCOU(modiSEALca_POPd_fourthsample,win.data=win.data))


detach(2)
#load("BSC_iSEALca_fourthsample.RData")
save.image("BSC_iSEALca_fourthsample.RData")
}



			
				

model.delta<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(INC.SEAL.CA~I(scale(PIB1900aD,scale=F)/400)+I((scale(PIB1900aD,scale=F)/400)^2),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(INC.SEAL.CA~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
nc <- 3	;		
ni <- 200000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modiSEALca_Kuznets1900a_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modiSEALca_Kuznets1900a_fourthsample,file="modiSEALca_Kuznets1900a_fourthsample.RData")
rm(modiSEALca_Kuznets1900a_fourthsample)
}

if (runDIC)
{attach("modiSEALca_Kuznets1900a_fourthsample.RData",pos=2)
DIC.iSEALca.fourthsample<-rbind(DIC.iSEALca.fourthsample,Kuznets1900a=DICZIbeta.woCOU(modiSEALca_Kuznets1900a_fourthsample,win.data=win.data))


detach(2)
#load("BSC_iSEALca_fourthsample.RData")
save.image("BSC_iSEALca_fourthsample.RData")
}





















model.delta<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(INC.SEAL.CA~I(scale(log(PIB1900aD/400),scale=F))+I((scale(log(PIB1900aD/400),scale=F))^2),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(INC.SEAL.CA~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
nc <- 3	;		
ni <- 200000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modiSEALca_lKuznets1900a_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modiSEALca_lKuznets1900a_fourthsample,file="modiSEALca_lKuznets1900a_fourthsample.RData")
rm(modiSEALca_lKuznets1900a_fourthsample)
}

if (runDIC)
{attach("modiSEALca_lKuznets1900a_fourthsample.RData",pos=2)
DIC.iSEALca.fourthsample<-rbind(DIC.iSEALca.fourthsample,lKuznets1900a=DICZIbeta.woCOU(modiSEALca_lKuznets1900a_fourthsample,win.data=win.data))


detach(2)
#load("BSC_iSEALca_fourthsample.RData")
save.image("BSC_iSEALca_fourthsample.RData")
}




















model.delta<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(INC.SEAL.CA~I(scale((PIB.1900.Dullinger/2),scale=F))+I(scale((POPd.Dullinger.1900/50),scale=F)),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(INC.SEAL.CA~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
nc <- 3	;		
ni <- 200000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modiSEALca_eco1900_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modiSEALca_eco1900_fourthsample,file="modiSEALca_eco1900_fourthsample.RData")
rm(modiSEALca_eco1900_fourthsample)
}

if (runDIC)
{attach("modiSEALca_eco1900_fourthsample.RData",pos=2)
DIC.iSEALca.fourthsample<-rbind(DIC.iSEALca.fourthsample,eco1900=DICZIbeta.woCOU(modiSEALca_eco1900_fourthsample,win.data=win.data))


detach(2)
#load("BSC_iSEALca_fourthsample.RData")
save.image("BSC_iSEALca_fourthsample.RData")
}



#load("BSC_iSEALca_fourthsample.RData")
#save.image("BSC_iSEALca_fourthsample.RData")

model.delta<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(INC.SEAL.CA~I(scale(log(PIB.1900.Dullinger/2),scale=F))+I(scale(log(POPd.Dullinger.1900/50),scale=F)),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(INC.SEAL.CA~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
nc <- 3	;		
ni <- 200000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modiSEALca_leco1900_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modiSEALca_leco1900_fourthsample,file="modiSEALca_leco1900_fourthsample.RData")
rm(modiSEALca_leco1900_fourthsample)
}

if (runDIC)
{attach("modiSEALca_leco1900_fourthsample.RData",pos=2)
DIC.iSEALca.fourthsample<-rbind(DIC.iSEALca.fourthsample,leco1900=DICZIbeta.woCOU(modiSEALca_leco1900_fourthsample,win.data=win.data))


detach(2)
#load("BSC_iSEALca_fourthsample.RData")
save.image("BSC_iSEALca_fourthsample.RData")
}





model.delta<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(INC.SEAL.CA~I(scale(PIBa,scale=F)/50000)+I((scale(PIBa,scale=F)/50000)^2),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(INC.SEAL.CA~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))


nc <- 3	;		
ni <- 200000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modiSEALca_Kuznetsa_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modiSEALca_Kuznetsa_fourthsample,file="modiSEALca_Kuznetsa_fourthsample.RData")
rm(modiSEALca_Kuznetsa_fourthsample)
}

if (runDIC)
{attach("modiSEALca_Kuznetsa_fourthsample.RData",pos=2)
DIC.iSEALca.fourthsample<-rbind(DIC.iSEALca.fourthsample,Kuznetsa=DICZIbeta.woCOU(modiSEALca_Kuznetsa_fourthsample,win.data=win.data))


detach(2)
#load("BSC_iSEALca_fourthsample.RData")
save.image("BSC_iSEALca_fourthsample.RData")
}




		

model.delta<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(INC.SEAL.CA~I(scale(log((PIBa)/50000),scale=F))+I(scale(log((PIBa)/50000),scale=F)^2),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(INC.SEAL.CA~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))


nc <- 3	;		
ni <- 200000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modiSEALca_lKuznetsa_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modiSEALca_lKuznetsa_fourthsample,file="modiSEALca_lKuznetsa_fourthsample.RData")
rm(modiSEALca_lKuznetsa_fourthsample)
}

if (runDIC)
{attach("modiSEALca_lKuznetsa_fourthsample.RData",pos=2)

if (is.element("lKuznetsa",dimnames(DIC.iSEALca.fourthsample)[[1]]))
{DIC.iSEALca.fourthsample["lKuznetsa",]<-DICZIbeta.woCOU(modiSEALca_lKuznetsa_fourthsample,win.data=win.data)}
else
{
DIC.iSEALca.fourthsample<-rbind(DIC.iSEALca.fourthsample,lKuznetsa=DICZIbeta.woCOU(modiSEALca_lKuznetsa_fourthsample,win.data=win.data))
}

detach(2)
#load("BSC_iSEALca_fourthsample.RData")
save.image("BSC_iSEALca_fourthsample.RData")
}




model.delta<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(INC.SEAL.CA~I(scale(log(PIBh/10000),scale=F))+I(scale(log(POPd/100),scale=F)),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(INC.SEAL.CA~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))


nc <- 3	;		
ni <- 200000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modiSEALca_leco_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modiSEALca_leco_fourthsample,file="modiSEALca_leco_fourthsample.RData")
rm(modiSEALca_leco_fourthsample)
}

if (runDIC)
{attach("modiSEALca_leco_fourthsample.RData",pos=2)
DIC.iSEALca.fourthsample<-rbind(DIC.iSEALca.fourthsample,leco=DICZIbeta.woCOU(modiSEALca_leco_fourthsample,win.data=win.data))


detach(2)
#load("BSC_iSEALca_fourthsample.RData")
save.image("BSC_iSEALca_fourthsample.RData")
}





model.delta<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(INC.SEAL.CA~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))


nc <- 3	;		
ni <- 200000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modiSEALca_Null_fourthsample <- stan(model_code = stanmodel.delta1.ne1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modiSEALca_Null_fourthsample,file="modiSEALca_Null_fourthsample.RData")
rm(modiSEALca_Null_fourthsample)
}

if (runDIC)
{attach("modiSEALca_Null_fourthsample.RData",pos=2)
DIC.iSEALca.fourthsample<-rbind(DIC.iSEALca.fourthsample,Null=DICZIbeta.woCOU(modiSEALca_Null_fourthsample,win.data=win.data))

detach(2)
#load("BSC_iSEALca_fourthsample.RData")
save.image("BSC_iSEALca_fourthsample.RData")
}



			


model.delta<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(INC.SEAL.CA~I(scale(log(PIB1900aD/400),scale=F))+I(scale(log(PIB1950aD/400)-log(PIB1900aD/400),scale=F))+I(scale(log(PIB2000aD/400)-log(PIB1950aD/400),scale=F)),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(INC.SEAL.CA~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

nc <- 3	;		
ni <- 200000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modiSEALca_GDPalinc_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modiSEALca_GDPalinc_fourthsample,file="modiSEALca_GDPalinc_fourthsample.RData")
rm(modiSEALca_GDPalinc_fourthsample)
}

if (runDIC)
{attach("modiSEALca_GDPalinc_fourthsample.RData",pos=2)
DIC.iSEALca.fourthsample<-rbind(DIC.iSEALca.fourthsample,GDPalinc=DICZIbeta.woCOU(modiSEALca_GDPalinc_fourthsample,win.data=win.data))


detach(2)
#load("BSC_iSEALca_fourthsample.RData")
save.image("BSC_iSEALca_fourthsample.RData")
}



			
			

					
model.delta<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(INC.SEAL.CA~I(scale((PIB2000aD/2400),scale=F)),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(INC.SEAL.CA~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

nc <- 3	;		
ni <- 200000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modiSEALca_GDP2000a_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modiSEALca_GDP2000a_fourthsample,file="modiSEALca_GDP2000a_fourthsample.RData")
rm(modiSEALca_GDP2000a_fourthsample)
}

if (runDIC)
{attach("modiSEALca_GDP2000a_fourthsample.RData",pos=2)
DIC.iSEALca.fourthsample<-rbind(DIC.iSEALca.fourthsample,GDP2000a=DICZIbeta.woCOU(modiSEALca_GDP2000a_fourthsample,win.data=win.data))


detach(2)
#load("BSC_iSEALca_fourthsample.RData")
save.image("BSC_iSEALca_fourthsample.RData")
}

			
model.delta<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(INC.SEAL.CA~I(scale((PIB1950aD/400),scale=F)),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(INC.SEAL.CA~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

nc <- 3	;		
ni <- 200000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modiSEALca_GDP1950a_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modiSEALca_GDP1950a_fourthsample,file="modiSEALca_GDP1950a_fourthsample.RData")
rm(modiSEALca_GDP1950a_fourthsample)
}

if (runDIC)
{attach("modiSEALca_GDP1950a_fourthsample.RData",pos=2)
DIC.iSEALca.fourthsample<-rbind(DIC.iSEALca.fourthsample,GDP1950a=DICZIbeta.woCOU(modiSEALca_GDP1950a_fourthsample,win.data=win.data))


detach(2)
#load("BSC_iSEALca_fourthsample.RData")
save.image("BSC_iSEALca_fourthsample.RData")
}



model.delta<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(INC.SEAL.CA~I(scale((PIB1900aD/400),scale=F)),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(INC.SEAL.CA~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

nc <- 3	;		
ni <- 200000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modiSEALca_GDP1900a_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modiSEALca_GDP1900a_fourthsample,file="modiSEALca_GDP1900a_fourthsample.RData")
rm(modiSEALca_GDP1900a_fourthsample)
}

if (runDIC)
{attach("modiSEALca_GDP1900a_fourthsample.RData",pos=2)
DIC.iSEALca.fourthsample<-rbind(DIC.iSEALca.fourthsample,GDP1900a=DICZIbeta.woCOU(modiSEALca_GDP1900a_fourthsample,win.data=win.data))


detach(2)
#load("BSC_iSEALca_fourthsample.RData")
save.image("BSC_iSEALca_fourthsample.RData")
}




model.delta<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(INC.SEAL.CA~I(scale(log(POPd.Dullinger.1900/50),scale=F)),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(INC.SEAL.CA~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
				
				
nc <- 3	;		
ni <- 200000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modiSEALca_POPd1900l_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modiSEALca_POPd1900l_fourthsample,file="modiSEALca_POPd1900l_fourthsample.RData")
rm(modiSEALca_POPd1900l_fourthsample)
}

if (runDIC)
{attach("modiSEALca_POPd1900l_fourthsample.RData",pos=2)
DIC.iSEALca.fourthsample<-rbind(DIC.iSEALca.fourthsample,POPd1900l=DICZIbeta.woCOU(modiSEALca_POPd1900l_fourthsample,win.data=win.data))


detach(2)
#load("BSC_iSEALca_fourthsample.RData")
save.image("BSC_iSEALca_fourthsample.RData")
}





			

model.delta<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(INC.SEAL.CA~I(scale((POPd.Dullinger.1950/50),scale=F)),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(INC.SEAL.CA~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
				
				
nc <- 3	;		
ni <- 200000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modiSEALca_POPd1950_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modiSEALca_POPd1950_fourthsample,file="modiSEALca_POPd1950_fourthsample.RData")
rm(modiSEALca_POPd1950_fourthsample)
}

if (runDIC)
{attach("modiSEALca_POPd1950_fourthsample.RData",pos=2)
DIC.iSEALca.fourthsample<-rbind(DIC.iSEALca.fourthsample,POPd1950=DICZIbeta.woCOU(modiSEALca_POPd1950_fourthsample,win.data=win.data))


detach(2)
#load("BSC_iSEALca_fourthsample.RData")
save.image("BSC_iSEALca_fourthsample.RData")
}






model.delta<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(INC.SEAL.CA~I(scale(log(POPd.Dullinger.1950/50),scale=F)),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(INC.SEAL.CA~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
				
				
nc <- 3	;		
ni <- 200000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modiSEALca_POPd1950l_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modiSEALca_POPd1950l_fourthsample,file="modiSEALca_POPd1950l_fourthsample.RData")
rm(modiSEALca_POPd1950l_fourthsample)
}

if (runDIC)
{attach("modiSEALca_POPd1950l_fourthsample.RData",pos=2)
DIC.iSEALca.fourthsample<-rbind(DIC.iSEALca.fourthsample,POPd1950l=DICZIbeta.woCOU(modiSEALca_POPd1950l_fourthsample,win.data=win.data))


detach(2)
#load("BSC_iSEALca_fourthsample.RData")
save.image("BSC_iSEALca_fourthsample.RData")
}





model.delta<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(INC.SEAL.CA~I(scale((POPd.Dullinger.2000/50),scale=F)),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(INC.SEAL.CA~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
				
				
nc <- 3	;		
ni <- 200000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modiSEALca_POPd2000_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modiSEALca_POPd2000_fourthsample,file="modiSEALca_POPd2000_fourthsample.RData")
rm(modiSEALca_POPd2000_fourthsample)
}

if (runDIC)
{attach("modiSEALca_POPd2000_fourthsample.RData",pos=2)
DIC.iSEALca.fourthsample<-rbind(DIC.iSEALca.fourthsample,POPd2000=DICZIbeta.woCOU(modiSEALca_POPd2000_fourthsample,win.data=win.data))


detach(2)
#load("BSC_iSEALca_fourthsample.RData")
save.image("BSC_iSEALca_fourthsample.RData")
}






model.delta<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(INC.SEAL.CA~I(scale(log(POPd.Dullinger.2000/50),scale=F)),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(INC.SEAL.CA~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
				
				
nc <- 3	;		
ni <- 200000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modiSEALca_POPd2000l_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modiSEALca_POPd2000l_fourthsample,file="modiSEALca_POPd2000l_fourthsample.RData")
rm(modiSEALca_POPd2000l_fourthsample)
}

if (runDIC)
{attach("modiSEALca_POPd2000l_fourthsample.RData",pos=2)
DIC.iSEALca.fourthsample<-rbind(DIC.iSEALca.fourthsample,POPd2000l=DICZIbeta.woCOU(modiSEALca_POPd2000l_fourthsample,win.data=win.data))


detach(2)
#load("BSC_iSEALca_fourthsample.RData")
save.image("BSC_iSEALca_fourthsample.RData")
}





							
							

model.delta<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(INC.SEAL.CA~I(scale((PIBh/10000),scale=F))+I(scale((POPd/100),scale=F)),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(INC.SEAL.CA~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))


nc <- 3	;		
ni <- 200000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modiSEALca_eco_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modiSEALca_eco_fourthsample,file="modiSEALca_eco_fourthsample.RData")
rm(modiSEALca_eco_fourthsample)
}

if (runDIC)
{attach("modiSEALca_eco_fourthsample.RData",pos=2)
DIC.iSEALca.fourthsample<-rbind(DIC.iSEALca.fourthsample,eco=DICZIbeta.woCOU(modiSEALca_eco_fourthsample,win.data=win.data))


detach(2)
#load("BSC_iSEALca_fourthsample.RData")
save.image("BSC_iSEALca_fourthsample.RData")
}




		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		


model.delta<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(INC.SEAL.CA~I(scale(PIB1950aD,scale=F)/400)+I((scale(PIB1950aD,scale=F)/400)^2),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(INC.SEAL.CA~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
nc <- 3	;		
ni <- 200000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modiSEALca_Kuznets1950a_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modiSEALca_Kuznets1950a_fourthsample,file="modiSEALca_Kuznets1950a_fourthsample.RData")
rm(modiSEALca_Kuznets1950a_fourthsample)
}

if (runDIC)
{attach("modiSEALca_Kuznets1950a_fourthsample.RData",pos=2)
DIC.iSEALca.fourthsample<-rbind(DIC.iSEALca.fourthsample,Kuznets1950a=DICZIbeta.woCOU(modiSEALca_Kuznets1950a_fourthsample,win.data=win.data))


detach(2)
#load("BSC_iSEALca_fourthsample.RData")
save.image("BSC_iSEALca_fourthsample.RData")
}



















model.delta<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(INC.SEAL.CA~I(scale(log(PIB1950aD/400),scale=F))+I((scale(log(PIB1950aD/400),scale=F))^2),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(INC.SEAL.CA~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
nc <- 3	;		
ni <- 200000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modiSEALca_lKuznets1950a_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modiSEALca_lKuznets1950a_fourthsample,file="modiSEALca_lKuznets1950a_fourthsample.RData")
rm(modiSEALca_lKuznets1950a_fourthsample)
}

if (runDIC)
{attach("modiSEALca_lKuznets1950a_fourthsample.RData",pos=2)
DIC.iSEALca.fourthsample<-rbind(DIC.iSEALca.fourthsample,lKuznets1950a=DICZIbeta.woCOU(modiSEALca_lKuznets1950a_fourthsample,win.data=win.data))

detach(2)
#load("BSC_iSEALca_fourthsample.RData")
save.image("BSC_iSEALca_fourthsample.RData")
}



















model.delta<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(INC.SEAL.CA~I(scale((PIB.1950.Dullinger/2),scale=F))+I(scale((POPd.Dullinger.1950/50),scale=F)),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(INC.SEAL.CA~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
nc <- 3	;		
ni <- 200000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modiSEALca_eco1950_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modiSEALca_eco1950_fourthsample,file="modiSEALca_eco1950_fourthsample.RData")
rm(modiSEALca_eco1950_fourthsample)
}

if (runDIC)
{attach("modiSEALca_eco1950_fourthsample.RData",pos=2)
DIC.iSEALca.fourthsample<-rbind(DIC.iSEALca.fourthsample,eco1950=DICZIbeta.woCOU(modiSEALca_eco1950_fourthsample,win.data=win.data))


detach(2)
#load("BSC_iSEALca_fourthsample.RData")
save.image("BSC_iSEALca_fourthsample.RData")
}



#load("BSC_iSEALca_fourthsample.RData")
#save.image("BSC_iSEALca_fourthsample.RData")

model.delta<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(INC.SEAL.CA~I(scale(log(PIB.1950.Dullinger/2),scale=F))+I(scale(log(POPd.Dullinger.1950/50),scale=F)),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(INC.SEAL.CA~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				
				
nc <- 3	;		
ni <- 200000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modiSEALca_leco1950_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modiSEALca_leco1950_fourthsample,file="modiSEALca_leco1950_fourthsample.RData")
rm(modiSEALca_leco1950_fourthsample)
}

if (runDIC)
{attach("modiSEALca_leco1950_fourthsample.RData",pos=2)
DIC.iSEALca.fourthsample<-rbind(DIC.iSEALca.fourthsample,leco1950=DICZIbeta.woCOU(modiSEALca_leco1950_fourthsample,win.data=win.data))

detach(2)
#load("BSC_iSEALca_fourthsample.RData")
save.image("BSC_iSEALca_fourthsample.RData")
}



		
		










model.delta<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(INC.SEAL.CA~I(scale(PIB2000aD,scale=F)/2400)+I((scale(PIB2000aD,scale=F)/2400)^2),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(INC.SEAL.CA~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))


nc <- 3	;		
ni <- 200000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modiSEALca_Kuznets2000a_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modiSEALca_Kuznets2000a_fourthsample,file="modiSEALca_Kuznets2000a_fourthsample.RData")
rm(modiSEALca_Kuznets2000a_fourthsample)
}

if (runDIC)
{attach("modiSEALca_Kuznets2000a_fourthsample.RData",pos=2)
DIC.iSEALca.fourthsample<-rbind(DIC.iSEALca.fourthsample,Kuznets2000a=DICZIbeta.woCOU(modiSEALca_Kuznets2000a_fourthsample,win.data=win.data))


detach(2)
#load("BSC_iSEALca_fourthsample.RData")
save.image("BSC_iSEALca_fourthsample.RData")
}





model.delta<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(INC.SEAL.CA~I(scale(log(PIB2000aD/2400),scale=F))+I((scale(log(PIB2000aD/2400),scale=F))^2),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(INC.SEAL.CA~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))


nc <- 3	;		
ni <- 200000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modiSEALca_lKuznets2000a_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modiSEALca_lKuznets2000a_fourthsample,file="modiSEALca_lKuznets2000a_fourthsample.RData")
rm(modiSEALca_lKuznets2000a_fourthsample)
}

if (runDIC)
{attach("modiSEALca_lKuznets2000a_fourthsample.RData",pos=2)
if (is.element("lKuznets2000a",dimnames(DIC.iSEALca.fourthsample)[[1]]))
{DIC.iSEALca.fourthsample["lKuznets2000a",]<-DICZIbeta.woCOU(modiSEALca_lKuznets2000a_fourthsample,win.data=win.data)}
else
{
DIC.iSEALca.fourthsample<-rbind(DIC.iSEALca.fourthsample,lKuznets2000a=DICZIbeta.woCOU(modiSEALca_lKuznets2000a_fourthsample,win.data=win.data))
}

detach(2)
#load("BSC_iSEALca_fourthsample.RData")
save.image("BSC_iSEALca_fourthsample.RData")
}



model.delta<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(INC.SEAL.CA~I((scale(log(PIB.2000.Dullinger/7),scale=F)))+I(scale(log(POPd.Dullinger.2000/100),scale=F)),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(INC.SEAL.CA~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))


nc <- 3	;		
ni <- 200000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modiSEALca_leco2000_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modiSEALca_leco2000_fourthsample,file="modiSEALca_leco2000_fourthsample.RData")
rm(modiSEALca_leco2000_fourthsample)
}

if (runDIC)
{attach("modiSEALca_leco2000_fourthsample.RData",pos=2)
DIC.iSEALca.fourthsample<-rbind(DIC.iSEALca.fourthsample,leco2000=DICZIbeta.woCOU(modiSEALca_leco2000_fourthsample,win.data=win.data))


detach(2)
#load("BSC_iSEALca_fourthsample.RData")
save.image("BSC_iSEALca_fourthsample.RData")
}




model.delta<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(INC.SEAL.CA~I((scale((PIB.2000.Dullinger/7),scale=F)))+I(scale((POPd.Dullinger.2000/100),scale=F)),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(INC.SEAL.CA~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))


nc <- 3	;		
ni <- 200000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modiSEALca_eco2000_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modiSEALca_eco2000_fourthsample,file="modiSEALca_eco2000_fourthsample.RData")
rm(modiSEALca_eco2000_fourthsample)
}

if (runDIC)
{attach("modiSEALca_eco2000_fourthsample.RData",pos=2)
DIC.iSEALca.fourthsample<-rbind(DIC.iSEALca.fourthsample,eco2000=DICZIbeta.woCOU(modiSEALca_eco2000_fourthsample,win.data=win.data))


detach(2)
#load("BSC_iSEALca_fourthsample.RData")
save.image("BSC_iSEALca_fourthsample.RData")
}






model.delta<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(INC.SEAL.CA~I(scale(PIB.2000.Dullinger/7,scale=F))+I((scale(PIB.2000.Dullinger/7,scale=F))^2),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(INC.SEAL.CA~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))


nc <- 3	;		
ni <- 200000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modiSEALca_Kuznets2000_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modiSEALca_Kuznets2000_fourthsample,file="modiSEALca_Kuznets2000_fourthsample.RData")
rm(modiSEALca_Kuznets2000_fourthsample)
}

if (runDIC)
{attach("modiSEALca_Kuznets2000_fourthsample.RData",pos=2)
DIC.iSEALca.fourthsample<-rbind(DIC.iSEALca.fourthsample,Kuznets2000=DICZIbeta.woCOU(modiSEALca_Kuznets2000_fourthsample,win.data=win.data))


detach(2)
#load("BSC_iSEALca_fourthsample.RData")
save.image("BSC_iSEALca_fourthsample.RData")
}



		

model.delta<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(INC.SEAL.CA~I(scale(log(PIB.2000.Dullinger/7),scale=F))+I((scale(log(PIB.2000.Dullinger/7),scale=F))^2),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(INC.SEAL.CA~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))


nc <- 3	;		
ni <- 200000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modiSEALca_lKuznets2000_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modiSEALca_lKuznets2000_fourthsample,file="modiSEALca_lKuznets2000_fourthsample.RData")
rm(modiSEALca_lKuznets2000_fourthsample)
}

if (runDIC)
{attach("modiSEALca_lKuznets2000_fourthsample.RData",pos=2)
if (is.element("lKuznets2000",dimnames(DIC.iSEALca.fourthsample)[[1]]))
{DIC.iSEALca.fourthsample["lKuznets2000",]<-DICZIbeta.woCOU(modiSEALca_lKuznets2000_fourthsample,win.data=win.data)}
else
{
DIC.iSEALca.fourthsample<-rbind(DIC.iSEALca.fourthsample,lKuznets2000=DICZIbeta.woCOU(modiSEALca_lKuznets2000_fourthsample,win.data=win.data))
}

detach(2)
#load("BSC_iSEALca_fourthsample.RData")
save.image("BSC_iSEALca_fourthsample.RData")
}



	

model.delta<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(INC.SEAL.CA~I(scale(PIB.1950.Dullinger/2,scale=F))+I((scale(PIB.1950.Dullinger/2,scale=F))^2),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(INC.SEAL.CA~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))


nc <- 3	;		
ni <- 195000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modiSEALca_Kuznets1950_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modiSEALca_Kuznets1950_fourthsample,file="modiSEALca_Kuznets1950_fourthsample.RData")
rm(modiSEALca_Kuznets1950_fourthsample)
}

if (runDIC)
{attach("modiSEALca_Kuznets1950_fourthsample.RData",pos=2)
DIC.iSEALca.fourthsample<-rbind(DIC.iSEALca.fourthsample,Kuznets1950=DICZIbeta.woCOU(modiSEALca_Kuznets1950_fourthsample,win.data=win.data))


detach(2)
#load("BSC_iSEALca_fourthsample.RData")
save.image("BSC_iSEALca_fourthsample.RData")
}



model.delta<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(INC.SEAL.CA~I(scale(log(PIB.1950.Dullinger/2),scale=F))+I((scale(log(PIB.1950.Dullinger/2),scale=F))^2),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(INC.SEAL.CA~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))


nc <- 3	;		
ni <- 195000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modiSEALca_lKuznets1950_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modiSEALca_lKuznets1950_fourthsample,file="modiSEALca_lKuznets1950_fourthsample.RData")
rm(modiSEALca_lKuznets1950_fourthsample)
}

if (runDIC)
{attach("modiSEALca_lKuznets1950_fourthsample.RData",pos=2)
if (is.element("lKuznets1950",dimnames(DIC.iSEALca.fourthsample)[[1]]))
{DIC.iSEALca.fourthsample["lKuznets1950",]<-DICZIbeta.woCOU(modiSEALca_lKuznets1950_fourthsample,win.data=win.data)}
else
{
DIC.iSEALca.fourthsample<-rbind(DIC.iSEALca.fourthsample,lKuznets1950=DICZIbeta.woCOU(modiSEALca_lKuznets1950_fourthsample,win.data=win.data))
}

detach(2)
#load("BSC_iSEALca_fourthsample.RData")
save.image("BSC_iSEALca_fourthsample.RData")
}



	
	
	
	
	
	
	
	
	





model.delta<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(INC.SEAL.CA~I(scale(PIB.1900.Dullinger/1,scale=F))+I((scale(PIB.1900.Dullinger/1,scale=F))^2),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(INC.SEAL.CA~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))


nc <- 3	;		
ni <- 190000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modiSEALca_Kuznets1900_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modiSEALca_Kuznets1900_fourthsample,file="modiSEALca_Kuznets1900_fourthsample.RData")
rm(modiSEALca_Kuznets1900_fourthsample)
}

if (runDIC)
{attach("modiSEALca_Kuznets1900_fourthsample.RData",pos=2)
DIC.iSEALca.fourthsample<-rbind(DIC.iSEALca.fourthsample,Kuznets1900=DICZIbeta.woCOU(modiSEALca_Kuznets1900_fourthsample,win.data=win.data))


detach(2)
#load("BSC_iSEALca_fourthsample.RData")
save.image("BSC_iSEALca_fourthsample.RData")
}

	

model.delta<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(INC.SEAL.CA~I(scale(log(PIB.1900.Dullinger/1),scale=F))+I((scale(log(PIB.1900.Dullinger/1),scale=F))^2),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(INC.SEAL.CA~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			win.data <-list(Y = (model.delta)$y/100,
				#N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))


nc <- 3	;		
ni <- 190000;  		
nb <- 10000 ;					
                  
nt <- 10;	

params <- c("pardelta","pareta","parsdinner","log_lik")
if (runStan)
{
set.seed(29)
modiSEALca_lKuznets1900_fourthsample <- stan(model_code = stanmodel.delta1.nsdi1.woCOU, model_name = "Stan_binom", data = win.data,init = initsSEALsixt,pars = params,iter = ni, chains = nc, warmup=nb, thin = nt, save_dso = TRUE,seed = 28,verbose = TRUE)
#
save(modiSEALca_lKuznets1900_fourthsample,file="modiSEALca_lKuznets1900_fourthsample.RData")
rm(modiSEALca_lKuznets1900_fourthsample)
}

if (runDIC)
{attach("modiSEALca_lKuznets1900_fourthsample.RData",pos=2)
if (is.element("lKuznets1900",dimnames(DIC.iSEALca.fourthsample)[[1]]))
{DIC.iSEALca.fourthsample["lKuznets1900",]<-DICZIbeta.woCOU(modiSEALca_lKuznets1900_fourthsample,win.data=win.data)}
else
{
DIC.iSEALca.fourthsample<-rbind(DIC.iSEALca.fourthsample,lKuznets1900=DICZIbeta.woCOU(modiSEALca_lKuznets1900_fourthsample,win.data=win.data))
}

detach(2)
#load("BSC_iSEALca_fourthsample.RData")
save.image("BSC_iSEALca_fourthsample.RData")
}



		
		
		
					


}


######## analysis of parameter estimators (Table 6)
{
library(rstan)
if (Sys.info()["sysname"]=="Windows") {setwd(wd.RData)} 
load("BSC_Data_functions.RData")

Estimates_iSEALca_fourthsample<-data.frame(NA,ncol=3,nrow=28)
dimnames(Estimates_iSEALca_fourthsample)[[2]]<-c("Summary_est","pbeta","magnitude")

attach("modiSEALca_POPd2000_fourthsample.RData",pos=2)

set.seed(1)
model.to.diagnose<-modiSEALca_POPd2000_fourthsample
temp<-interp.magnsign(model.to.diagnose,mult=106/50,index=2)
Estimates_iSEALca_fourthsample[1,]<-c(temp[1],temp[2],temp[3])
detach(2)

attach("modiSEALca_POPd1950_fourthsample.RData",pos=2)

set.seed(1)
model.to.diagnose<-modiSEALca_POPd1950_fourthsample
temp<-interp.magnsign(model.to.diagnose,mult=106/50,index=2)
Estimates_iSEALca_fourthsample[2,]<-c(temp[1],temp[2],temp[3])
detach(2)

attach("modiSEALca_POPd1900_fourthsample.RData",pos=2)

set.seed(1)
model.to.diagnose<-modiSEALca_POPd_fourthsample
temp<-interp.magnsign(model.to.diagnose,mult=106/50,index=2)
Estimates_iSEALca_fourthsample[3,]<-c(temp[1],temp[2],temp[3])
detach(2)

setwd(wd.csv)
write.csv(Estimates_iSEALca_fourthsample,file="Estimates_iSEALca_fourthsample.csv")



}

}

}


#####################################################################
######## CODE FOR FIGURE 1:
#####################################################################

{setwd(wd.RData)
library(rstan)
library(nlme)
library(Hmisc)
load("modExtinct_betabinomheteroscpriorsdlowrest_Null_fourthsamplewotrees_bis.RData")
load("modThreat_betabinomheteroscpriorsdlowrest_Null_fourthsamplewotrees_bis.RData")

load("BSC_Data_functions.RData")

##### sortie deseffets aléatoires Countrydans le modèle nul
summary(modExtinct_betabinomheteroscpriorsdlowrest_Null_fourthsamplewotrees_bis)[[1]][20:41,]



### 


### models that will countain explanatory variables & SEAL & iSEAL:
model.eta<-glm(cbind(Extinct,Native-Extinct)~Taxon+PIB1950aD+PIB1900aD+PIB2000aD+POPd.Dullinger.1900+SEAL.2009+INC.SEAL.CA+POPd.Dullinger.2000+POPd.Dullinger.1950-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Extinct,Native-Extinct)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.eta.gls<-gls(cbind(Extinct,Native-Extinct)~Taxon+PIB1950aD+PIB1900aD+PIB2000aD+POPd.Dullinger.1900+POPd.Dullinger.2000+SEAL.2009+INC.SEAL.CA+POPd.Dullinger.1950-1,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB2000aD)&!is.na(Native)&!is.na(Extinct))
						
COU=apply(model.matrix(model.COU),1,function(x){which(x>0)})


#log(Kuznetsa) pfor Extinct:
	par(mfrow=c(1,2))
plot(log(model.matrix(model.eta)[,"PIB1900aD"]),summary(modExtinct_betabinomheteroscpriorsdlowrest_Null_fourthsamplewotrees_bis)[[1]][20:41,][COU],xlab="log(GDP1900a)",ylab="Country r.e. in Extinct Null model")
corr<-rcorr(log(model.matrix(model.eta)[,"PIB1900aD"]),summary(modExtinct_betabinomheteroscpriorsdlowrest_Null_fourthsamplewotrees_bis)[[1]][20:41,][COU],type="pearson")$r[2,1]
format(corr,digits=3)
text(3,0.9,paste("r=",format(corr,digits=3),sep=""))
plot(log(model.matrix(model.eta)[,"PIB2000aD"]),summary(modExtinct_betabinomheteroscpriorsdlowrest_Null_fourthsamplewotrees_bis)[[1]][20:41,][COU],xlab="log(GDP2000a)",ylab="Country r.e. in Extinct Null model")
corr<-rcorr(log(model.matrix(model.eta)[,"PIB2000aD"]),summary(modExtinct_betabinomheteroscpriorsdlowrest_Null_fourthsamplewotrees_bis)[[1]][20:41,][COU],type="pearson")$r[2,1]
format(corr,digits=3)
text(6,0.9,paste("r=",format(corr,digits=3),sep=""))


#log(Kuznetsa) for Threat:
	par(mfrow=c(1,2))
	plot(log(model.matrix(model.eta)[,"PIB1900aD"]),summary(modThreat_betabinomheteroscpriorsdlowrest_Null_fourthsamplewotrees_bis)[[1]][20:41,][COU],xlab="log(GDP1900a)",ylab="Country r.e. in Threat. Null model")
corr<-rcorr(log(model.matrix(model.eta)[,"PIB1900aD"]),summary(modThreat_betabinomheteroscpriorsdlowrest_Null_fourthsamplewotrees_bis)[[1]][20:41,][COU],type="pearson")$r[2,1]
text(3,0.55,paste("r=",format(corr,digits=3),sep=""))
plot(log(model.matrix(model.eta)[,"PIB2000aD"]),summary(modThreat_betabinomheteroscpriorsdlowrest_Null_fourthsamplewotrees_bis)[[1]][20:41,][COU],xlab="log(GDP2000a)",ylab="Country r.e. in Threat. Null model")
corr<-rcorr(log(model.matrix(model.eta)[,"PIB2000aD"]),summary(modThreat_betabinomheteroscpriorsdlowrest_Null_fourthsamplewotrees_bis)[[1]][20:41,][COU],type="pearson")$r[2,1]
text(6,0.55,paste("r=",format(corr,digits=3),sep=""))
### similar order with Spearman



#HPD2000-1900 for SEAL:	
	par(mfrow=c(1,2))
	plot((model.matrix(model.eta)[,"POPd.Dullinger.1900"]),model.matrix(model.eta)[,"SEAL.2009"],xlab="HPD.1900",ylab="SEAL")
	corr<-rcorr((model.matrix(model.eta)[,"POPd.Dullinger.1900"]),model.matrix(model.eta)[,"SEAL.2009"],type="pearson")$r[2,1]
text(25,7.5,paste("r=",format(corr,digits=3),sep=""))

plot((model.matrix(model.eta)[,"POPd.Dullinger.2000"]),model.matrix(model.eta)[,"SEAL.2009"],xlab="HPD.2000",ylab="SEAL")
	corr<-rcorr((model.matrix(model.eta)[,"POPd.Dullinger.2000"]),model.matrix(model.eta)[,"SEAL.2009"],type="pearson")$r[2,1]
text(50,7.5,paste("r=",format(corr,digits=3),sep=""))

corr<-rcorr(log(model.matrix(model.eta)[,"POPd.Dullinger.2000"]),model.matrix(model.eta)[,"SEAL.2009"],type="spearman")$r[2,1]
corr<-rcorr((model.matrix(model.eta)[,"POPd.Dullinger.2000"]),model.matrix(model.eta)[,"SEAL.2009"],type="spearman")$r[2,1]
	
# HPD2000-1900 for iSEALca:	
	par(mfrow=c(1,2))
	plot((model.matrix(model.eta)[,"POPd.Dullinger.1900"]),model.matrix(model.eta)[,"INC.SEAL.CA"],xlab="HPD.1900",ylab="iSEALca")
	corr<-rcorr((model.matrix(model.eta)[,"POPd.Dullinger.1900"]),model.matrix(model.eta)[,"INC.SEAL.CA"],type="pearson")$r[2,1]
text(25,0.08,paste("r=",format(corr,digits=3),sep=""))
plot((model.matrix(model.eta)[,"POPd.Dullinger.2000"]),model.matrix(model.eta)[,"INC.SEAL.CA"],xlab="HPD.2000",ylab="iSEALca")
	corr<-rcorr((model.matrix(model.eta)[,"POPd.Dullinger.2000"]),model.matrix(model.eta)[,"INC.SEAL.CA"],type="pearson")$r[2,1]
text(50,0.08,paste("r=",format(corr,digits=3),sep=""))


}




#####################################################################
######## CODE FOR GOODNESS-OF-FIT p-values
#####################################################################
{

 library(coda)
#library(bindata)
#library(nlme)
library(gam)
if (Sys.info()["sysname"]=="Windows")  {library(Design)}
library(rms)
library(rstan)
library(VGAM)

library(gdata)

library(coda)
#library(rjags)
#library(R2WinBUGS)
library(lme4)
library(AICcmodavg)
library(Hmisc)
if (floor(as.double(version$minor))<=13&floor(as.double(version$major))<=2) {library(Design)} else { library(rms)    }
library(rstan)
library(nlme)
library(VGAM)
library(rgeos)
library(rworldmap)

##### USEFUL FUNCTIONS for GOF:
{

"unlist.lme"<-
function(x, y = NULL, w = NULL, m1="character")
{# m1 will determine the class of the elements in w; it can be a vector, with length the number of rows in w or a character chain
	if(is.null(y)) {
		x <- as.data.frame(x)
		z <- cbind.data.frame(y = unlist(x), quant = rep(names(x),
			each = dim(x)[1]), plotunlist.lme = rep(row.names(
			x), times = dim(x)[2]))
	}
	else {
		x <- as.data.frame(x)
		y <- as.data.frame(y)
		if(dim(y)[1] != dim(x)[1]) {
			print(c("nombre de lignes des deux tableaux non egaux; second tableau non pris en compte"
				))
			z <- cbind.data.frame(y = unlist(x), quant = rep(names(
				x), each = dim(x)[1]), plotunlist.lme = rep(
				row.names(x), times = dim(x)[2]))
		}
		else {
			z <- cbind.data.frame(y = unlist(x), quant = rep(names(
				x), each = dim(x)[1]), plotunlist.lme = rep(
				row.names(x), times = dim(x)[2]))
			for(i in 1:dim(y)[2]) {
				m <- class(y[, i])
				z <- cbind.data.frame(z, rep(y[, i], times = 
					dim(x)[2]))
				names(z)[dim(z)[2]] <- names(y)[i]
				class(z[, dim(z)[2]]) <- m
			}
		}
	}
	if(!is.null(w)) {if (is.R()){assign("mgbhfxyz",m1,pos=1)}else{assign("mgbhfxyz",m1,where=1)}

		
		if(dim(w)[2] != dim(x)[2]) {
			print(c("nombre de colonnes des tableaux x et w non egaux; troisieme tableau non pris en compte"
				))
		}
		else {
			for(i in 1:dim(w)[1]) {
				
						if(is.R()){assign("totogbhfxyz",rep(t(w[i,  ]), each = dim(x)[1]),pos=1)}else{
						assign("totogbhfxyz",rep(t(w[i,  ]), each = dim(x)[1]),where=1)}
				if (length(m1)==1){
				if (is.R()){
z <- cbind.data.frame(z, get("totogbhfxyz",pos=1))
class(z[,dim(z)[2]])<-get("mgbhfxyz",pos=1)
}
else {	
				z <- insert.col(z, target.column="@END", count=1, column.type=get("mgbhfxyz",where=1),fill.expression=get("totogbhfxyz",where=1))}
									
				}
				
				else {
				if (is.R()){
z <- cbind.data.frame(z, get("totogbhfxyz",pos=1))
class(z[,dim(z)[2]])<-get("mgbhfxyz",pos=1)[i]
}
else {	
				z <- insert.col(z, target.column="@END", count=1, column.type=get("mgbhfxyz",where=1)[i],fill.expression=get("totogbhfxyz",where=1))}

				}
				names(z)[dim(z)[2]] <- (dimnames(w)[[1]])[i]
			}
			
		}
	}
	z
}

list.as.array<-function(x)
{#transforms a list whose components are all of the same dims (either: vectors, matrices or arrays) into an array whose last component is the level in the list
#max.length<-max(sapply(x,length))
#max.dims<-max(sapply(x,dim))
array(unlist(x),dim=c(dim(as.array(x[[1]])),length(x)))}


list.as.matrix<-function(x,to.add=NA)
{#transforms a list whose components are all of the same dims (either: vectors, matrices or arrays) into an array whose last component is the level in the list
max.length<-max(sapply(x,length))
x.new<-sapply(x,function(x,max.length){c(x,rep(to.add,max.length-length(x)))},max.length)
array(unlist(x.new),dim=c(max.length,length(x)))}

 

quantile.for.diags<- function (x, y) 
 {
     prop.equal <- runif(1)
     k <- rbeta(1, sum(x < y) + sum(x == y) * prop.equal + 1, 
         sum(x > y) + sum(x == y) * (1 - prop.equal) + 1)
         #print(k)
    min( k ,(1-k))*2
}

quantile.for.diags.uncorrected<- function (x, y) 
 {
     prop.equal <- runif(1)
     k <- rbeta(1, sum(x < y) + sum(x == y) * prop.equal + 1, 
         sum(x > y) + sum(x == y) * (1 - prop.equal) + 1)
         #print(k)
    k
}
    


skewness<- function(x){mean((x-mean(x))^3)/mean((x-mean(x))^2)^1.5}

kurtosis<- function(x){mean((x-mean(x))^4)/mean((x-mean(x))^2)^2-3}








### NB: dans le cas de modèles avec Effets COU fixes, la première partie n'a pas d'intérêt
Gennorm.betabinom <- function(x,data.model=win.data,seed=25){
set.seed(seed)
selected.model<-x
index<-sample(length(extract(selected.model,"sdCOU")[[1]]),1)
sdCOU<-extract(selected.model,"sdCOU")[[1]][index]

easCOU<-extract(selected.model,"easCOU")[[1]][index,,drop=F]

pareta<-extract(selected.model,"pareta")[[1]][index,,drop=F]

parsdinner<-extract(selected.model,"parsdinner")[[1]][index,,drop=F]

#print(dim(pareta))
#print(dim(win.data$matrixeta))
sdinner<-(parsdinner%*%t(win.data$matrixsdinner))
eta<-(pareta%*%t(win.data$matrixeta))+t(easCOU[,win.data$COU])
mutot<-1/(1+exp(-eta))

fitted<-mutot
fitted<-as.vector(mutot)
alpha<-exp(sdinner)*mutot
beta<-exp(sdinner)*(1-mutot)

x1<-win.data$matrixeta[,10]
x2<-win.data$matrixeta[,10]

eas.a<-easCOU/sdCOU
eas.a.ordered<-easCOU[,win.data$COU[!duplicated(win.data$COU)]]
x.ordered<-win.data$x[!duplicated(win.data$COU)]
y.ordered<-win.data$y[!duplicated(win.data$COU)]
tempgls<-try(gls(eas.a.ordered~1,correlation=corExp(form = ~ x.ordered + y.ordered,                  nugget = TRUE),control=list(returnObject=TRUE)))

#temprand<-gls(rnorm(length(eas.a.ordered))~1,correlation=corExp(c(1, 0.2), form = ~ x.ordered + y.ordered,                  nugget = TRUE)); summary(temprand)
win.data<-data.model
ynorm<-qnorm(pbetabinom.ab(win.data$Y-1,win.data$N,alpha,beta,log=F)+dbetabinom.ab(win.data$Y,win.data$N,alpha,beta,log=F)*runif(length(win.data$Y)))


#removed from the moment: I only keep the normalized version:
#mean(ysim),sd(ysim),skewness(ysim),kurtosis(ysim),hoeffd(ysim,fitted)$D[1,2],hoeffd(ysim,Data$x)$D[1,2],
c(mean.eaCOU=mean(eas.a),sd.eaCOU=sd(eas.a),skew.eaCOU=skewness(eas.a),kurt.eaCOU=kurtosis(eas.a),range.eaCOU=ifelse(class(tempgls)=="try-error",NA,(summary(tempgls))$modelStruct$corStruct[1]),nugget.eaCOU=ifelse(class(tempgls)=="try-error",NA,(summary(tempgls))$modelStruct$corStruct[2]),
mean.yn=mean(ynorm),sd.yn=sd(ynorm),skew.yn=skewness(ynorm),kurt.yn=kurtosis(ynorm),Linearity=hoeffd(ynorm,fitted)$D[1,2],Linearity.x1=hoeffd(ynorm,x1)$D[1,2],Linearity.x2=hoeffd(ynorm,x2)$D[1,2],heterosc=hoeffd(ynorm^2,fitted)$D[1,2])}







Gennormeq.betabinom <- function(x,data.model=win.data,seed=25){

ynorm<-rnorm(length(as.double(data.model$Y)))
eas.a<-rnorm((data.model$NCOU))

#viré car sinon force à être à la même graine... d'appl en appel... set.seed(seed)
selected.model<-x
index<-sample(length(extract(selected.model,"sdCOU")[[1]]),1)
sdCOU<-extract(selected.model,"sdCOU")[[1]][index]

easCOU<-extract(selected.model,"easCOU")[[1]][index,,drop=F]

pareta<-extract(selected.model,"pareta")[[1]][index,,drop=F]

parsdinner<-extract(selected.model,"parsdinner")[[1]][index,,drop=F]

#print(dim(pareta))
#print(dim(win.data$matrixeta))
sdinner<-(parsdinner%*%t(win.data$matrixsdinner))
eta<-(pareta%*%t(win.data$matrixeta))+t(easCOU[,win.data$COU])
mutot<-1/(1+exp(-eta))

x1<-win.data$matrixeta[,10]
x2<-win.data$matrixeta[,10]

x.ordered<-win.data$x[!duplicated(win.data$COU)]
y.ordered<-win.data$y[!duplicated(win.data$COU)]
tempgls<-try(gls(eas.a~1,correlation=corExp(form = ~ x.ordered + y.ordered,                  nugget = TRUE),control=list(returnObject=TRUE)))
while (class(tempgls)=="try-error")
{eas.a<-rnorm((data.model$NCOU))
tempgls<-try(gls(eas.a~1,correlation=corExp(form = ~ x.ordered + y.ordered,                  nugget = TRUE),control=list(returnObject=TRUE)))
}


fitted<-as.vector(mutot)

#removed from the moment: I only keep the normalized version:
#mean(ysim),sd(ysim),skewness(ysim),kurtosis(ysim),hoeffd(ysim,fitted)$D[1,2],hoeffd(ysim,Data$x)$D[1,2],
c(mean.eaCOU=mean(eas.a),sd.eaCOU=sd(eas.a),skew.eaCOU=skewness(eas.a),kurt.eaCOU=kurtosis(eas.a),range.eaCOU=(summary(tempgls))$modelStruct$corStruct[1],nugget.eaCOU=(summary(tempgls))$modelStruct$corStruct[2],
mean.yn=mean(ynorm),sd.yn=sd(ynorm),skew.yn=skewness(ynorm),kurt.yn=kurtosis(ynorm),Linearity=hoeffd(ynorm,fitted)$D[1,2],Linearity.x1=hoeffd(ynorm,x1)$D[1,2],Linearity.x2=hoeffd(ynorm,x2)$D[1,2],heterosc=hoeffd(ynorm^2,fitted)$D[1,2])
}




plot.betabinom <- function(x,data.model=win.data,seed=25){
set.seed(seed)
selected.model<-x
index<-sample(length(extract(selected.model,"sdCOU")[[1]]),1)
sdCOU<-extract(selected.model,"sdCOU")[[1]][index]

easCOU<-extract(selected.model,"easCOU")[[1]][index,,drop=F]

pareta<-extract(selected.model,"pareta")[[1]][index,,drop=F]

parsdinner<-extract(selected.model,"parsdinner")[[1]][index,,drop=F]

#print(dim(pareta))
#print(dim(win.data$matrixeta))
sdinner<-(parsdinner%*%t(win.data$matrixsdinner))
eta<-(pareta%*%t(win.data$matrixeta))+t(easCOU[,win.data$COU])
mutot<-1/(1+exp(-eta))

fitted<-mutot
fitted<-as.vector(mutot)
alpha<-exp(sdinner)*mutot
beta<-exp(sdinner)*(1-mutot)

x1<-win.data$matrixeta[,10]
x2<-win.data$matrixeta[,10]

eas.a<-easCOU/sdCOU

win.data<-data.model
ynorm<-qnorm(pbetabinom.ab(win.data$Y-1,win.data$N,alpha,beta,log=F)+dbetabinom.ab(win.data$Y,win.data$N,alpha,beta,log=F)*runif(length(win.data$Y)))


windows()
print(as.vector(eas.a))

qqnorm(as.vector(eas.a))
abline(0,1)


windows()

qqnorm(as.vector(ynorm))
abline(0,1)


windows()
plot(fitted,ynorm)

windows()
temp <- xYplot(ynorm~fitted, method = smean.cl.normal, methodArgs = list(conf.int = 0.95), nx = 30, abline = list(a = 0, b = 0), aspect = "fill", , cex = 0.6)
plot(temp)

windows()
temp <- xYplot(ynorm~win.data$matrixeta[,6], method = smean.cl.normal, methodArgs = list(conf.int = 0.95), nx = 30, abline = list(a = 0, b = 0), aspect = "fill", , cex = 0.6)
plot(temp)

windows()
temp <- xYplot(ynorm~win.data$matrixeta[,7], method = smean.cl.normal, methodArgs = list(conf.int = 0.95), nx = 30, abline = list(a = 0, b = 0), aspect = "fill", , cex = 0.6)
plot(temp)

windows()
temp <- xYplot(ynorm~easCOU[,win.data$COU], method = smean.cl.normal, methodArgs = list(conf.int = 0.95), nx = 30, abline = list(a = 0, b = 0), aspect = "fill", , cex = 0.6)
plot(temp)
#removed from the moment: I only keep the normalized version:
#mean(ysim),sd(ysim),skewness(ysim),kurtosis(ysim),hoeffd(ysim,fitted)$D[1,2],hoeffd(ysim,Data$x)$D[1,2],
c(mean.eaCOU=mean(eas.a),sd.eaCOU=sd(eas.a),skew.eaCOU=skewness(eas.a),kurt.eaCOU=kurtosis(eas.a),
mean.yn=mean(ynorm),sd.yn=sd(ynorm),skew.yn=skewness(ynorm),kurt.yn=kurtosis(ynorm),Linearity=hoeffd(ynorm,fitted)$D[1,2],heterosc=hoeffd(ynorm^2,fitted)$D[1,2])}





Gennorm.ZIbeta <- function(x,data.model=win.data,seed=25){
set.seed(seed)
selected.model<-x
index<-sample(length(extract(selected.model,"sdCOU")[[1]]),1)
sdCOU<-extract(selected.model,"sdCOU")[[1]][index]

easCOU<-extract(selected.model,"easCOU")[[1]][index,,drop=F]

pareta<-extract(selected.model,"pareta")[[1]][index,,drop=F]
pardelta<-extract(selected.model,"pardelta")[[1]][index]

parsdinner<-extract(selected.model,"parsdinner")[[1]][index,,drop=F]

#print(dim(pareta))
#print(dim(win.data$matrixeta))
sdinner<-(parsdinner%*%t(win.data$matrixsdinner))
eta<-(pareta%*%t(win.data$matrixeta))+t(easCOU[,win.data$COU])
delta<-(pardelta%*%t(win.data$matrixdelta))
mutot<-1/(1+exp(-eta))
P0<-1-1/(1+exp(-exp(delta)-eta))

fitted<-mutot
fitted<-as.vector(mutot)
alpha<-exp(sdinner)*mutot/(1-P0)
beta<-exp(sdinner)*(1-mutot/(1-P0))


if (dim(win.data$matrixeta)[2]>=6)
{
x1<-win.data$matrixeta[,5]
x2<-win.data$matrixeta[,6]
}

else
{x1<-rnorm(dim(win.data$matrixeta)[1])
x2<-rnorm(dim(win.data$matrixeta)[1])}


eas.a<-easCOU/sdCOU
print(sort(eas.a))
eas.a.ordered<-easCOU[,win.data$COU[!duplicated(win.data$COU)]]
x.ordered<-win.data$x[!duplicated(win.data$COU)]
y.ordered<-win.data$y[!duplicated(win.data$COU)]
tempgls<-try(gls(eas.a.ordered~1,correlation=corExp(form = ~ x.ordered + y.ordered,                  nugget = TRUE),control=list(returnObject=TRUE)))


win.data<-data.model
ynorm<-win.data$Y
for (i in 1:length(win.data$Y))
{if (win.data$Y[i]>0)
{ynorm[i]<-qnorm(P0[i]+(1-P0[i])*pbeta(win.data$Y[i],alpha[i],beta[i],log=F))}
else
{ynorm[i]<-qnorm(P0[i]*runif(1))
}
}



#removed from the moment: I only keep the normalized version:
#mean(ysim),sd(ysim),skewness(ysim),kurtosis(ysim),hoeffd(ysim,fitted)$D[1,2],hoeffd(ysim,Data$x)$D[1,2],
c(mean.eaCOU=mean(eas.a),sd.eaCOU=sd(eas.a),skew.eaCOU=skewness(eas.a),kurt.eaCOU=kurtosis(eas.a),range.eaCOU=ifelse(class(tempgls)=="try-error",NA,(summary(tempgls))$modelStruct$corStruct[1]),nugget.eaCOU=ifelse(class(tempgls)=="try-error",NA,(summary(tempgls))$modelStruct$corStruct[2]),
mean.yn=mean(ynorm),sd.yn=sd(ynorm),skew.yn=skewness(ynorm),kurt.yn=kurtosis(ynorm),Linearity=hoeffd(ynorm,fitted)$D[1,2],Linearity.x1=hoeffd(ynorm,x1)$D[1,2],Linearity.x2=hoeffd(ynorm,x2)$D[1,2],heterosc=hoeffd(ynorm^2,fitted)$D[1,2])}





Gennormeq.ZIbeta <- function(x,data.model=win.data,seed=25){

ynorm<-rnorm(length(as.double(data.model$Y)))
eas.a<-rnorm((data.model$NCOU))

#virĂ© car sinon tout pareil!!!!! et du coupop pb... set.seed(seed)
selected.model<-x
index<-sample(length(extract(selected.model,"sdCOU")[[1]]),1)
sdCOU<-extract(selected.model,"sdCOU")[[1]][index]

easCOU<-extract(selected.model,"easCOU")[[1]][index,,drop=F]

pareta<-extract(selected.model,"pareta")[[1]][index,,drop=F]

parsdinner<-extract(selected.model,"parsdinner")[[1]][index,,drop=F]

#print(dim(pareta))
#print(dim(win.data$matrixeta))
sdinner<-(parsdinner%*%t(win.data$matrixsdinner))
eta<-(pareta%*%t(win.data$matrixeta))+t(easCOU[,win.data$COU])
mutot<-1/(1+exp(-eta))

fitted<-as.vector(mutot)


if (dim(win.data$matrixeta)[2]>=6)
{
x1<-win.data$matrixeta[,5]
x2<-win.data$matrixeta[,6]
}

else
{x1<-rnorm(dim(win.data$matrixeta)[1])
x2<-rnorm(dim(win.data$matrixeta)[1])}


x.ordered<-win.data$x[!duplicated(win.data$COU)]
y.ordered<-win.data$y[!duplicated(win.data$COU)]
tempgls<-try(gls(eas.a~1,correlation=corExp(form = ~ x.ordered + y.ordered,                  nugget = TRUE),control=list(returnObject=TRUE)))
while (class(tempgls)=="try-error")
{eas.a<-rnorm((data.model$NCOU))
tempgls<-try(gls(eas.a~1,correlation=corExp(form = ~ x.ordered + y.ordered,                  nugget = TRUE),control=list(returnObject=TRUE)))
}

#removed from the moment: I only keep the normalized version:
#mean(ysim),sd(ysim),skewness(ysim),kurtosis(ysim),hoeffd(ysim,fitted)$D[1,2],hoeffd(ysim,Data$x)$D[1,2],
c(mean.eaCOU=mean(eas.a),sd.eaCOU=sd(eas.a),skew.eaCOU=skewness(eas.a),kurt.eaCOU=kurtosis(eas.a),range.eaCOU=(summary(tempgls))$modelStruct$corStruct[1],nugget.eaCOU=(summary(tempgls))$modelStruct$corStruct[2],
mean.yn=mean(ynorm),sd.yn=sd(ynorm),skew.yn=skewness(ynorm),kurt.yn=kurtosis(ynorm),Linearity=hoeffd(ynorm,fitted)$D[1,2],Linearity.x1=hoeffd(ynorm,x1)$D[1,2],Linearity.x2=hoeffd(ynorm,x2)$D[1,2],heterosc=hoeffd(ynorm^2,fitted)$D[1,2])
}




plot.ZIbeta <- function(x,data.model=win.data,seed=25){
set.seed(seed)
selected.model<-x
index<-sample(length(extract(selected.model,"sdCOU")[[1]]),1)
sdCOU<-extract(selected.model,"sdCOU")[[1]][index]

easCOU<-extract(selected.model,"easCOU")[[1]][index,,drop=F]

pareta<-extract(selected.model,"pareta")[[1]][index,,drop=F]
pardelta<-extract(selected.model,"pardelta")[[1]][index]

parsdinner<-extract(selected.model,"parsdinner")[[1]][index,,drop=F]

#print(dim(pareta))
#print(dim(win.data$matrixeta))
sdinner<-(parsdinner%*%t(win.data$matrixsdinner))
eta<-(pareta%*%t(win.data$matrixeta))+t(easCOU[,win.data$COU])
delta<-(pardelta%*%t(win.data$matrixdelta))
mutot<-1/(1+exp(-eta))
P0<-1-1/(1+exp(-exp(delta)-eta))

fitted<-mutot
fitted<-as.vector(mutot)
alpha<-exp(sdinner)*mutot/(1-P0)
beta<-exp(sdinner)*(1-mutot/(1-P0))

eas.a<-easCOU/sdCOU

win.data<-data.model
ynorm<-win.data$Y
for (i in 1:length(win.data$Y))
{if (win.data$Y[i]>0)
{ynorm[i]<-qnorm(P0[i]+(1-P0[i])*pbeta(win.data$Y[i],alpha[i],beta[i],log=F))}
else
{ynorm[i]<-qnorm(P0[i]*runif(1))
}
}
windows()
print(as.vector(eas.a))

qqnorm(as.vector(eas.a))
abline(0,1)


windows()
plot(fitted,ynorm)

windows()
temp <- xYplot(ynorm~fitted, method = smean.cl.normal, methodArgs = list(conf.int = 0.95), nx = 30, abline = list(a = 0, b = 0), aspect = "fill", , cex = 0.6)
plot(temp)

#windows()
#temp <- xYplot(ynorm~win.data$matrixeta[,5], method = smean.cl.normal, methodArgs = list(conf.int = 0.95), nx = 30, abline = list(a = 0, b = 0), aspect = "fill", , cex = 0.6)
#plot(temp)


windows()
temp <- xYplot(ynorm~easCOU[,win.data$COU], method = smean.cl.normal, methodArgs = list(conf.int = 0.95), nx = 30, abline = list(a = 0, b = 0), aspect = "fill", , cex = 0.6)
plot(temp)
#removed from the moment: I only keep the normalized version:
#mean(ysim),sd(ysim),skewness(ysim),kurtosis(ysim),hoeffd(ysim,fitted)$D[1,2],hoeffd(ysim,Data$x)$D[1,2],
c(mean.eaCOU=mean(eas.a),sd.eaCOU=sd(eas.a),skew.eaCOU=skewness(eas.a),kurt.eaCOU=kurtosis(eas.a),
mean.yn=mean(ynorm),sd.yn=sd(ynorm),skew.yn=skewness(ynorm),kurt.yn=kurtosis(ynorm),Linearity=hoeffd(ynorm,fitted)$D[1,2],heterosc=hoeffd(ynorm^2,fitted)$D[1,2])}






Gennorm.ZIbeta <- function(x,data.model=win.data,seed=25){
set.seed(seed)
selected.model<-x
index<-sample(length(extract(selected.model,"pardelta")[[1]]),1)
#sdCOU<-extract(selected.model,"sdCOU")[[1]][index]

#easCOU<-extract(selected.model,"easCOU")[[1]][index,,drop=F]

pareta<-extract(selected.model,"pareta")[[1]][index,,drop=F]
pardelta<-extract(selected.model,"pardelta")[[1]][index]

parsdinner<-extract(selected.model,"parsdinner")[[1]][index]

#print(dim(pareta))
#print(dim(win.data$matrixeta))
sdinner<-(parsdinner%*%t(win.data$matrixsdinner))
eta<-(pareta%*%t(win.data$matrixeta))
delta<-(pardelta%*%t(win.data$matrixdelta))
mutot<-1/(1+exp(-eta))
P0<-1-1/(1+exp(-exp(delta)-eta))

fitted<-mutot
fitted<-as.vector(mutot)
alpha<-exp(sdinner)*mutot/(1-P0)
beta<-exp(sdinner)*(1-mutot/(1-P0))


win.data<-data.model
ynorm<-win.data$Y
for (i in 1:length(win.data$Y))
{if (win.data$Y[i]>0)
{ynorm[i]<-qnorm(P0[i]+(1-P0[i])*pbeta(win.data$Y[i],alpha[i],beta[i],log=F))}
else
{ynorm[i]<-qnorm(P0[i]*runif(1))
}
}

x1<-win.data$matrixeta[,2]
x2<-win.data$matrixeta[,2]


x.ordered<-win.data$x
y.ordered<-win.data$y
tempgls<-try(gls(ynorm~1,correlation=corExp(form = ~ x.ordered + y.ordered,                  nugget = TRUE),control=list(returnObject=TRUE)))



#removed from the moment: I only keep the normalized version:
#mean(ysim),sd(ysim),skewness(ysim),kurtosis(ysim),hoeffd(ysim,fitted)$D[1,2],hoeffd(ysim,Data$x)$D[1,2],
c(mean.yn=mean(ynorm),sd.yn=sd(ynorm),skew.yn=skewness(ynorm),kurt.yn=kurtosis(ynorm),
range.eaCOU=ifelse(class(tempgls)=="try-error",NA,(summary(tempgls))$modelStruct$corStruct[1]),nugget.eaCOU=ifelse(class(tempgls)=="try-error",NA,(summary(tempgls))$modelStruct$corStruct[2]),
Linearity=hoeffd(ynorm,fitted)$D[1,2],Linearity.x1=hoeffd(ynorm,x1)$D[1,2],Linearity.x2=hoeffd(ynorm,x2)$D[1,2],heterosc=hoeffd(ynorm^2,fitted)$D[1,2],min.ynorm=min(ynorm),max.ynorm=max(ynorm))}





Gennormeq.ZIbeta <- function(x,data.model=win.data,seed=25){

ynorm<-rnorm(length(as.double(data.model$Y)))
eas.a<-rnorm((data.model$NCOU))

#virĂ© car sinon tout pareil!!!!! et du coupop pb... set.seed(seed)
selected.model<-x
index<-sample(length(extract(selected.model,"pardelta")[[1]]),1)


pareta<-extract(selected.model,"pareta")[[1]][index,,drop=F]

parsdinner<-extract(selected.model,"parsdinner")[[1]][index]

#print(dim(pareta))
#print(dim(win.data$matrixeta))
sdinner<-(parsdinner%*%t(win.data$matrixsdinner))
eta<-(pareta%*%t(win.data$matrixeta))
mutot<-1/(1+exp(-eta))

fitted<-as.vector(mutot)


x1<-win.data$matrixeta[,2]
x2<-win.data$matrixeta[,2]


x.ordered<-win.data$x
y.ordered<-win.data$y
tempgls<-try(gls(ynorm~1,correlation=corExp(form = ~ x.ordered + y.ordered,                  nugget = TRUE),control=list(returnObject=TRUE)))
while (class(tempgls)=="try-error")
{ynorm<-rnorm(length(as.double(data.model$Y)))
tempgls<-try(gls(ynorm~1,correlation=corExp(form = ~ x.ordered + y.ordered,                  nugget = TRUE),control=list(returnObject=TRUE)))
}


#removed from the moment: I only keep the normalized version:
#mean(ysim),sd(ysim),skewness(ysim),kurtosis(ysim),hoeffd(ysim,fitted)$D[1,2],hoeffd(ysim,Data$x)$D[1,2],
c(mean.yn=mean(ynorm),sd.yn=sd(ynorm),skew.yn=skewness(ynorm),kurt.yn=kurtosis(ynorm),
range.eaCOU=ifelse(class(tempgls)=="try-error",NA,(summary(tempgls))$modelStruct$corStruct[1]),nugget.eaCOU=ifelse(class(tempgls)=="try-error",NA,(summary(tempgls))$modelStruct$corStruct[2]),
Linearity=hoeffd(ynorm,fitted)$D[1,2],Linearity.x1=hoeffd(ynorm,x1)$D[1,2],Linearity.x2=hoeffd(ynorm,x2)$D[1,2],heterosc=hoeffd(ynorm^2,fitted)$D[1,2],min.ynorm=min(ynorm),max.ynorm=max(ynorm))}





plot.ZIbeta <- function(x,data.model=win.data,seed=25){
set.seed(seed)
selected.model<-x
index<-sample(length(extract(selected.model,"pardelta")[[1]]),1)

pareta<-extract(selected.model,"pareta")[[1]][index,,drop=F]
pardelta<-extract(selected.model,"pardelta")[[1]][index]

parsdinner<-extract(selected.model,"parsdinner")[[1]][index]

#print(dim(pareta))
#print(dim(win.data$matrixeta))
sdinner<-(parsdinner%*%t(win.data$matrixsdinner))
eta<-(pareta%*%t(win.data$matrixeta))+t(easCOU[,win.data$COU])
delta<-(pardelta%*%t(win.data$matrixdelta))
mutot<-1/(1+exp(-eta))
P0<-1-1/(1+exp(-exp(delta)-eta))

fitted<-mutot
fitted<-as.vector(mutot)
alpha<-exp(sdinner)*mutot/(1-P0)
beta<-exp(sdinner)*(1-mutot/(1-P0))

eas.a<-easCOU/sdCOU

win.data<-data.model
ynorm<-win.data$Y
for (i in 1:length(win.data$Y))
{if (win.data$Y[i]>0)
{ynorm[i]<-qnorm(P0[i]+(1-P0[i])*pbeta(win.data$Y[i],alpha[i],beta[i],log=F))}
else
{ynorm[i]<-qnorm(P0[i]*runif(1))
}
}
windows()
print(as.vector(eas.a))

qqnorm(as.vector(eas.a))
abline(0,1)


windows()
plot(fitted,ynorm)

windows()
temp <- xYplot(ynorm~fitted, method = smean.cl.normal, methodArgs = list(conf.int = 0.95), nx = 30, abline = list(a = 0, b = 0), aspect = "fill", , cex = 0.6)
plot(temp)

windows()
temp <- xYplot(ynorm~win.data$matrixeta[,6], method = smean.cl.normal, methodArgs = list(conf.int = 0.95), nx = 30, abline = list(a = 0, b = 0), aspect = "fill", , cex = 0.6)
plot(temp)

windows()
temp <- xYplot(ynorm~win.data$matrixeta[,7], method = smean.cl.normal, methodArgs = list(conf.int = 0.95), nx = 30, abline = list(a = 0, b = 0), aspect = "fill", , cex = 0.6)
plot(temp)

windows()
temp <- xYplot(ynorm~easCOU[,win.data$COU], method = smean.cl.normal, methodArgs = list(conf.int = 0.95), nx = 30, abline = list(a = 0, b = 0), aspect = "fill", , cex = 0.6)
plot(temp)
#removed from the moment: I only keep the normalized version:
#mean(ysim),sd(ysim),skewness(ysim),kurtosis(ysim),hoeffd(ysim,fitted)$D[1,2],hoeffd(ysim,Data$x)$D[1,2],
c(mean.eaCOU=mean(eas.a),sd.eaCOU=sd(eas.a),skew.eaCOU=skewness(eas.a),kurt.eaCOU=kurtosis(eas.a),
mean.yn=mean(ynorm),sd.yn=sd(ynorm),skew.yn=skewness(ynorm),kurt.yn=kurtosis(ynorm),Linearity=hoeffd(ynorm,fitted)$D[1,2],heterosc=hoeffd(ynorm^2,fitted)$D[1,2])}





}


####### FIRST FOR EXTINCT SPECIES:
{reps.pvalue<-10000
function.diags.statsgen="Gennormeq.betabinom"
function.diags.statistics="Gennorm.betabinom"
#name.model.characters<-"modThreat_betabinomheterosc_Kuznetsa"
name.model.characters<-"modExtinct_betabinomheteroscpriorsdlowrest_GDP1900al_fourthsamplewotrees_bis"
load(paste(wd.RData,name.model.characters,".RData",sep=""))
#modThreat_betabinomheteroscpriorsdlowrest_Kuznetsa_firstsample_bis
#modExtinct_betabinomheteroscpriorsdlowrest_Kuznetsa_firstsample_bis
name.model<-get(name.model.characters)


load(paste(wd.RData,"BSC_Data_functions",".RData",sep=""))





model.delta<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Extinct,Native-Extinct)~Taxon+I(scale(log(PIB1900aD/400),scale=F))-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Extinct~Native-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Extinct-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Extinct,Native-Extinct)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Extinct,Native-Extinct)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Extinct,Native-Extinct)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))

			model.xy<-glm(cbind(Extinct,Native-Extinct)~x+y-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Native)&!is.na(Extinct))

			
			
			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				#################################
				x=model.matrix(model.xy)[,"x"],y=model.matrix(model.xy)[,"y"],
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

nc <- 3	;		
ni <- 350000;  		
nb <- 20000 ;					
                  
nt <- 30;	

params <- c("pareta","parsdinner","sdCOU","easCOU","log_lik")


				set.seed(1)
test<-sapply(function.diags.statistics,function(x,y,data){do.call(x,list(y,data))},name.model,win.data)
 test.bis<-lapply(1:reps.pvalue,function(x,y,data){sapply(function.diags.statsgen,function(x,y,data){do.call(x,list(y,data))},y,data)},name.model,win.data)
           temp<-sapply(test.bis,function(x,test){x<test},test)
                  p.values<-sapply(1:dim(temp)[1],function(x,temp){quantile.for.diags(as.double(temp[x,]),0)},temp)
                 names(p.values)<-dimnames(test)[[1]]
p.values




}


####### SECOND FOR THREATENED SPECIES:
{


reps.pvalue<-10000
function.diags.statsgen="Gennormeq.betabinom"
function.diags.statistics="Gennorm.betabinom"
#name.model.characters<-"modThreat_betabinomheterosc_Kuznetsa"
name.model.characters<-"modThreat_betabinomheteroscpriorsdlowrest_GDP1900al_fourthsamplewotrees_bis"
load(paste(wd.RData,name.model.characters,".RData",sep=""))
#modThreat_betabinomheteroscpriorsdlowrest_Kuznetsa_firstsample_bis
#modExtinct_betabinomheteroscpriorsdlowrest_Kuznetsa_firstsample_bis
name.model<-get(name.model.characters)


load(paste(wd.RData,"BSC_Data_functions",".RData",sep=""))

#### best model pour Threat####





model.delta<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.eta<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon+I(scale(log(PIB1900aD/400),scale=F))-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidon<-glm(Threatened~I(Native-Extinct)-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.bidonbis<-glm(Native~Threatened-1,family=gaussian,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.COU<-glm(cbind(Threatened,Native-Extinct-Threatened)~COU-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.Ptype<-glm(cbind(Threatened,Native-Extinct-Threatened)~1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.sdinner<-glm(cbind(Threatened,Native-Extinct-Threatened)~Taxon-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))
model.xy<-glm(cbind(Threatened,Native-Extinct-Threatened)~x+y-1,family=binomial,data=data.unlisted.sp,
			subset=Taxon!="T"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD)&!is.na(Threatened)&!is.na(Native)&!is.na(Extinct))

			win.data <-list(Y = as.vector(model.matrix(model.bidonbis)),
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				#################################
				x=model.matrix(model.xy)[,"x"],y=model.matrix(model.xy)[,"y"],
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))

				


				set.seed(1)
test<-sapply(function.diags.statistics,function(x,y,data){do.call(x,list(y,data))},name.model,win.data)
 test.bis<-lapply(1:reps.pvalue,function(x,y,data){sapply(function.diags.statsgen,function(x,y,data){do.call(x,list(y,data))},y,data)},name.model,win.data)
           temp<-sapply(test.bis,function(x,test){x<test},test)
                  p.values<-sapply(1:dim(temp)[1],function(x,temp){quantile.for.diags(as.double(temp[x,]),0)},temp)
                 names(p.values)<-dimnames(test)[[1]]
p.values}


####### THIRD FOR PROTECTED AREAS
{



reps.pvalue<-10000
function.diags.statsgen="Gennormeq.ZIbeta"
function.diags.statistics="Gennorm.ZIbeta"
name.model<-modPA_null

name.model.characters<-"modPA_Null_fourthsample"
load(paste(wd.RData,name.model.characters,".RData",sep=""))
#modThreat_betabinomheteroscpriorsdlowrest_Kuznetsa_firstsample_bis
#modExtinct_betabinomheteroscpriorsdlowrest_Kuznetsa_firstsample_bis
name.model<-get(name.model.characters)


load(paste(wd.RData,"BSC_Data_functions",".RData",sep=""))





model.delta<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.eta<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.bidon<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.bidonbis<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.COU<-glm(I(PA/TotArea)~COU-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.Ptype<-glm(I(PA/TotArea)~1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))
model.sdinner<-glm(I(PA/TotArea)~NumPA-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))

			model.xy<-glm(I(PA/TotArea)~x+y-1,family=gaussian,data=data.protected.areas,
			subset=TypePA!="MCPFE"&!is.element(COU,c("CYP","LUX","MAL"))&!is.na(PA)&!is.na(TotArea)&!is.na(PIB1900aD))

			win.data <-list(Y = (model.delta)$y,
				N = as.vector(model.matrix(model.bidon)),
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				#################################
				x=model.matrix(model.xy)[,"x"],y=model.matrix(model.xy)[,"y"],
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))




	set.seed(1)
test<-sapply(function.diags.statistics,function(x,y,data){do.call(x,list(y,data))},name.model,win.data)
 test.bis<-lapply(1:reps.pvalue,function(x,y,data){sapply(function.diags.statsgen,function(x,y,data){do.call(x,list(y,data))},y,data)},name.model,win.data)
           temp<-sapply(test.bis,function(x,test){x<test},test)
                  p.values<-sapply(1:dim(temp)[1],function(x,temp){quantile.for.diags(as.double(temp[x,]),0)},temp)
                 names(p.values)<-dimnames(test)[[1]]
p.values
 
}



####### FOURTH FOR SEAL
{



reps.pvalue<-10000
function.diags.statsgen="Gennormeq.ZIbeta"
function.diags.statistics="Gennorm.ZIbeta"
#name.model.characters<-"modThreat_betabinomheterosc_Kuznetsa"
name.model.characters<-"modSEAL_POPd1900l_fourthsample"
load(paste(wd.RData,name.model.characters,".RData",sep=""))
#load(paste("E:\\Dossier Frederic\\Articles\\BSC_Callois\\DATA_ANALYSIS\\","BSC_SEAL",".RData",sep=""))
#modThreat_betabinomheteroscpriorsdlowrest_Kuznetsa_firstsample_bis
#modExtinct_betabinomheteroscpriorsdlowrest_Kuznetsa_firstsample_bis
name.model<-get(name.model.characters)


load(paste(wd.RData,"BSC_Data_functions",".RData",sep=""))




model.delta<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(SEAL.2009~I(scale(log(POPd.Dullinger.1900/50),scale=F)),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(SEAL.2009~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(SEAL.2009~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
	
model.xy<-glm(SEAL.2009~x+y-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

	
			
			
		win.data <-list(Y = (model.delta)$y/100,
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				#################################
				x=model.matrix(model.xy)[,"x"],y=model.matrix(model.xy)[,"y"],
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))


set.seed(1)
test<-sapply(function.diags.statistics,function(x,y,data){do.call(x,list(y,data))},name.model,win.data)
 test.bis<-lapply(1:reps.pvalue,function(x,y,data){sapply(function.diags.statsgen,function(x,y,data){do.call(x,list(y,data))},y,data)},name.model,win.data)
           temp<-sapply(test.bis,function(x,test){x<test},test)
                  p.values.leco<-sapply(1:dim(temp)[1],function(x,temp){quantile.for.diags(as.double(temp[x,]),0)},temp)
                 names(p.values.leco)<-dimnames(test)[[1]]
p.values.leco

}

####### FIFTH FOR iSEAL:

{


reps.pvalue<-10000
function.diags.statsgen="Gennormeq.ZIbeta"
function.diags.statistics="Gennorm.ZIbeta"
#name.model.characters<-"modThreat_betabinomheterosc_Kuznetsa"
name.model.characters<-"modiSEALca_eco2000_fourthsample"
load(paste(wd.RData,name.model.characters,".RData",sep=""))
#load(paste("E:\\Dossier Frederic\\Articles\\BSC_Callois\\DATA_ANALYSIS\\","BSC_iSEALca",".RData",sep=""))
#modThreat_betabinomheteroscpriorsdlowrest_Kuznetsa_firstsample_bis
#modExtinct_betabinomheteroscpriorsdlowrest_Kuznetsa_firstsample_bis
name.model<-get(name.model.characters)


load(paste(wd.RData,"BSC_Data_functions",".RData",sep=""))





model.delta<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.eta<-glm(INC.SEAL.CA~I((scale((PIB.2000.Dullinger/7),scale=F)))+I(scale((POPd.Dullinger.2000/100),scale=F)),family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidon<-glm(Extinct~Native,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.bidonbis<-glm(Native~Extinct,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.COU<-glm(INC.SEAL.CA~COU-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.Ptype<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))
model.sdinner<-glm(INC.SEAL.CA~1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

model.xy<-glm(INC.SEAL.CA~x+y-1,family=gaussian,data=datatot,
			subset=!is.element(COU,c("CYP","LUX","MAL"))&!is.na(SEAL.2009+INC.SEAL.CA+PIB1900aD))

			
	
			
			
		win.data <-list(Y = (model.delta)$y/100,
				matrixeta=model.matrix(model.eta),
				matrixdelta=model.matrix(model.delta),
				matrixsdinner=model.matrix(model.sdinner),
				Ncases=length( (model.delta)$y),
				Ndelta=dim(model.matrix(model.delta))[2],
				Neta=dim(model.matrix(model.eta))[2],
				Nsdinner=dim(model.matrix(model.sdinner))[2],
				COU=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				Ptype=apply(model.matrix(model.COU),1,function(x){which(x>0)}),
				#################################
				x=model.matrix(model.xy)[,"x"],y=model.matrix(model.xy)[,"y"],
				NCOU=length(unique(apply(model.matrix(model.COU),1,function(x){which(x>0)}))))


set.seed(1)
test<-sapply(function.diags.statistics,function(x,y,data){do.call(x,list(y,data))},name.model,win.data)
 test.bis<-lapply(1:reps.pvalue,function(x,y,data){sapply(function.diags.statsgen,function(x,y,data){do.call(x,list(y,data))},y,data)},name.model,win.data)
           temp<-sapply(test.bis,function(x,test){x<test},test)
                  p.values.leco<-sapply(1:dim(temp)[1],function(x,temp){quantile.for.diags(as.double(temp[x,]),0)},temp)
                 names(p.values.leco)<-dimnames(test)[[1]]
p.values.leco

}


}









