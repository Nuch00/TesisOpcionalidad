# genero funcion para calcular plan de pagos de prestamo frances, recibe una tasa Continua, una cantidad de cuotas y tiempo entre cuotas y devuelve un plan de pagos de capital e interes


prestamoFrancesFunction<-function(capital,tasaContinua,nCuotas,perEntreCuotas){
  planPago<-matrix(0,ncol=3,nrow=nCuotas)
  ###transformo TNA a continua NO
  # tasa<-log((1+TNA*perEntreCuotas/360)) 
  tasa<-exp(tasaContinua*nCuotas)^(1/nCuotas)-1 ## transforma de continua a efectiva Y TRABAJO TODA EN EFECTIVA
  tasa<-(1+tasa)^(1/(360/perEntreCuotas))-1
  cantCuotasXaño<-360/perEntreCuotas
  # valorCuota<-capital*(exp(tasa*nCuotas)*(exp(tasa)-1))/(exp(tasa*nCuotas)-1)
  # valorCuota<-capital*((1+TNA/(360/perEntreCuotas))^nCuotas*TNA/(360/perEntreCuotas))/((1+TNA/(360/perEntreCuotas))^nCuotas-1) #en TNA
  valorCuota<-capital*((1+tasa)^nCuotas*tasa)/((1+tasa)^nCuotas-1) #en EFECTIVA TRABAJA
  ## calculo Marcha Prestamo
  planPago[,3]<-valorCuota
  for(iCuota in 1:nCuotas){
    saldo<-capital-sum(planPago[(1:iCuota),1])
    # planPago[iCuota,2]<-saldo*(exp(tasa*perEntreCuotas/360)-1) #cuotaInt
    # planPago[iCuota,2]<-saldo*(TNA/(360/perEntreCuotas)) #cuotaInt
    planPago[iCuota,2]<-saldo*(tasa) #cuotaInt
    planPago[iCuota,1]<-valorCuota-planPago[iCuota,2] #cuotaCap
  }
  colnames(planPago)<-c("CuoCap","CuoInt","CuoTot")
  rownames(planPago)<-seq(1:nCuotas)
  return(planPago)
  
}
