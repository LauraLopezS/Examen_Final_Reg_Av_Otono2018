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
                     n.iter=niter,n.chains=nchains,n.burnin=nburning,n.thin=1,
                     progress.bar='none')

#-Monitoring the chains-

#JAGS Output
out_dinam2<-mod_dinam2.sim$BUGSoutput$sims.list

# # Chain for coefficients
# z<-out_dinam$beta[,1,1] # simulaciones de beta 1 al tiempo 1
# par(mfrow=c(2,2))
# plot(z,type="l")
# plot(cumsum(z)/(1:length(z)),type="l")
# hist(z,freq=FALSE)
# acf(z)

#-Coefficient Summary-

# Simulations
out_dinam2.sum<-mod_dinam2.sim$BUGSoutput$summary

# Modes
modas_dinam2<-c(apply(out_dinam2$alpha,2,getmode),unlist(lapply(1:n,function(x){apply(out_dinam2$beta[,,x],2,getmode)})))

# Summary
out_dinam2.sum.t<-cbind(rbind(out_dinam2.sum[grep("alpha",rownames(out_dinam2.sum)),c(1,3,5,7)],
                                  out_dinam2.sum[grep("beta",rownames(out_dinam2.sum)),c(1,3,5,7)]),
                            modas_dinam2)
out_dinam2.sum.t<-cbind(out_dinam2.sum.t,
                        c(apply(out_dinam2$alpha,2,prob),
                          unlist(lapply(1:n, function(x){apply(out_dinam2$beta[,,x],2,prob)}))))
out_dinam2.sum.t<-out_dinam2.sum.t[,c(1,3,5,2,4,6)]
colnames(out_dinam2.sum.t)<-c("Media","Mediana","Moda","2.5%","97.5%","Prob.")
rownames(out_dinam2.sum.t)<-c('Intercepto',
                              paste(rep(c('JPM Dollar Ind.','VIX Ind','Prod. OPEP','Dem. OPEP','T-Bill 10YR','T-Bill 1YR'),n),rep(1:n,each=k),sep=' t='))

#-DIC-
out_dinam2.dic<-mod_dinam2.sim$BUGSoutput$DIC

#-Predictions-
out_dinam2.yp<-out_dinam2.sum[grep("yp",rownames(out_dinam2.sum)),]

#-Forecast-
out_dinam2.yf<-out_dinam2.sum[grep("yf",rownames(out_dinam2.sum)),]

#-Betas-
out_dinam2.beta<-out_dinam2.sum[grep("beta",rownames(out_dinam2.sum)),]
