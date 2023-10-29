planCorregidoFunction<-function(tasaContinua,planPago,etti){

  ## Toma el plan de pagos de un frances y corrige por la etti para que refleje la tasa que hace el VA = 0 a la etti vigente
    
  nCuotas<-nrow(planPago[which(planPago[,1]!=0),])
  capital<-sum(planPago[,1])
  # par<-optim(par=tasaContinua,errorFrances,capital=capital,plazo=nCuotas,etti=etti, method = "L-BFGS-B",lower=0.010,upper=99) #si uso esta tengo que devolver E^2
  par<-uniroot(function(tasaContinua) errorFrances(capital=capital,plazo=nCuotas,etti=etti,tasaContinua=tasaContinua),interval=c(tasaContinua*.5,tasaContinua*1.5))
  
  planPagoconCero<-matrix(0,ncol=3,nrow=nrow(planPago))
  planPago<-prestamoFrancesFunction(capital=capital,tasaContinua=par$root,nCuotas=nCuotas,perEntreCuotas=30)
  if(nCuotas<nrow(planPagoconCero)){planPagoconCero[nCuotas:nrow(planPagoconCero),]<-planPago}else{planPagoconCero<-planPago}
  return(list(planPagoconCero,par$root))
}


# etti<-matTasasAmpliada[,1]
errorFrances<-function(tasaContinua,capital,plazo,etti){
  
  ### convierto la etti a TEA (la etti viene en continua)
  plazos<-as.numeric(gsub(x=names(etti),"\\D",""))
  plazos<-plazos[-1]
  TEA<-exp(etti[-1]*plazos/30)^(1/(plazos/30))-1
  FD<-((1+TEA)^(1/12))^(-plazos/30)
  ### calculo el plan de pago con una Tasa
  planPago<-prestamoFrancesFunction(capital=capital,tasaContinua=tasaContinua,nCuotas=plazo,perEntreCuotas=30)
  VAexcedente=sum(FD[1:nrow(planPago)]*planPago[,3])-sum(planPago[,1])
  return(VAexcedente)
}

