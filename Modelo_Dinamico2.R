# Regresion Avanzada: Examen Final
# Modelo Estático

#- Specifying hyperparameters
niter<-10000
nchains<-2
nburning<-0.1*niter
semilla<-3567

#-Defining data-
data_dinam2 <- list("n"=n,"m"=m,"y"=datos_train$WTI, "x"=select(datos_train, -WTI))

#-Defining inits-
inits<-function(){list(alpha=0,beta=matrix(0,k,n),tau.y=1,tau.b=rep(1,k),yp=rep(1,n),yf=rep(1,m))}

#-Selecting parameters to monitor-
parameters<-c("alpha","beta","tau.y","tau.b","yp","yf")


#-Running code in JAGS-
set.seed(semilla)
mod_dinam2.sim<-jags(data_dinam2,inits,parameters,model.file="mod_dinamico2.txt",
                     n.iter=niter,n.chains=nchains,n.burnin=nburning,n.thin=1)

#-Monitoring the chains-

#JAGS Output
out_dinam<-mod_dinam.sim$BUGSoutput$sims.list

# # Chain for coefficients
# z<-out_dinam$beta[,1,1] # simulaciones de beta 1 al tiempo 1
# par(mfrow=c(2,2))
# plot(z,type="l")
# plot(cumsum(z)/(1:length(z)),type="l")
# hist(z,freq=FALSE)
# acf(z)

#-Coefficient Summary-

# Simulations
out_dinam.sum<-mod_dinam.sim$BUGSoutput$summary

# Modes
modas_dinam_alpha<-apply(out_dinam$alpha,2,getmode)
modas_dinam_beta<-unlist(lapply(1:n,function(x){apply(out_dinam$beta[,,x],2,getmode)}))

# Summary
out_dinam.sum.t_alpha<-cbind(out_dinam.sum[grep("alpha",rownames(out_dinam.sum)),c(1,3,5,7)],modas_dinam_alpha)
out_dinam.sum.t_alpha<-cbind(out_dinam.sum.t_alpha,apply(out_dinam$alpha,2,prob))
out_dinam.sum.t_alpha<-out_dinam.sum.t_alpha[,c(1,3,5,2,4,6)]
colnames(out_dinam.sum.t_alpha)<-c("Media","Mediana","Moda","2.5%","97.5%","Prob.")
rownames(out_dinam.sum.t_alpha)<-paste('Intercepto t=',1:n,sep='_')


out_dinam.sum.t_beta<-cbind(out_dinam.sum[grep("beta",rownames(out_dinam.sum)),c(1,3,5,7)],modas_dinam_beta)
out_dinam.sum.t_beta<-cbind(out_dinam.sum.t_beta,apply(out_dinam$beta,2,prob))
out_dinam.sum.t_beta<-out_dinam.sum.t_beta[,c(1,3,5,2,4,6)]
colnames(out_dinam.sum.t_beta)<-c("Media","Mediana","Moda","2.5%","97.5%","Prob.")
rownames(out_dinam.sum.t_beta)<-paste(rep(c('JPM Dollar Ind.','VIX Ind','Prod. OPEP','Dem. OPEP','T-Bill 10YR','T-Bill 1YR'),n),rep(1:n,each=k),sep=' t=')

#-DIC-
out_dinam.dic<-mod_dinam.sim$BUGSoutput$DIC

#-Predictions-
out_dinam.yp<-out_dinam.sum[grep("yp",rownames(out_dinam.sum)),]

#-Forecast-
out_dinam.yf<-out_dinam.sum[grep("yf",rownames(out_dinam.sum)),]

#-alpha-
out_dinam.alpha<-out_dinam.sum[grep("alpha",rownames(out_dinam.sum)),]

#-Betas-
out_dinam.beta<-out_dinam.sum[grep("beta",rownames(out_dinam.sum)),]
