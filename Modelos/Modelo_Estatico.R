# Regresion Avanzada: Examen Final
# Modelo Estático

#- Specifying hyperparameters
niter<-10000
nchains<-2
nburning<-0.1*niter
semilla<-3567

#-Defining data-
data_estat <- list("n"=n,"m"=m,"y"=c(datos$WTI[1:(n-3)],rep(NA,m)), "x"=select(datos, -WTI))

#-Defining inits-
inits<-function(){list(alpha=0,beta=rep(0,k),tau=1,yp=rep(1,n))}

#-Selecting parameters to monitor-
parameters<-c("alpha","beta","tau","yp")


#-Running code in JAGS-
set.seed(semilla)
mod_estat.sim<-jags(data_estat,inits,parameters,model.file="mod_estatico.txt",
                    n.iter=niter,n.chains=nchains,n.burnin=nburning,n.thin=1,
                    progress.bar='none')


#-Monitoring the chains-

#JAGS Output
out_estat<-mod_estat.sim$BUGSoutput$sims.list

# # Chain for coefficients
# z<-out_estat$beta[,1]
# par(mfrow=c(2,2))
# plot(z,type="l")
# plot(cumsum(z)/(1:length(z)),type="l")
# hist(z,freq=FALSE)
# acf(z)

#-Coefficient Summary-

# Simulations
out_estat.sum<-mod_estat.sim$BUGSoutput$summary

# Modes
modas_estat<-apply(cbind(out_estat$alpha,out_estat$beta),2,getmode)

# Summary
out_estat.sum.t<-cbind(rbind(out_estat.sum[grep("alpha",rownames(out_estat.sum)),c(1,3,5,7)],
                       out_estat.sum[grep("beta",rownames(out_estat.sum)),c(1,3,5,7)]),
                       modas_estat)
out_estat.sum.t<-cbind(out_estat.sum.t,apply(cbind(out_estat$alpha,out_estat$beta),2,prob))
out_estat.sum.t<-out_estat.sum.t[,c(1,3,5,2,4,6)]
colnames(out_estat.sum.t)<-c("Media","Mediana","Moda","2.5%","97.5%","Prob.")
rownames(out_estat.sum.t)<-c('Intercepto','JPM Dollar Ind.','VIX Ind','Prod. OPEP','Dem. OPEP','T-Bill 10YR','T-Bill 1YR')


#-DIC-
out_estat.dic<-mod_estat.sim$BUGSoutput$DIC

#-Predictions-
out_estat.yp<-out_estat.sum[grep("yp",rownames(out_estat.sum)),]
