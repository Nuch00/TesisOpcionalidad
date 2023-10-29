simularMatrizTasaFunction<-function(nodosMatriz,largoNodo,nodosFuturos,matrizParametros,valoresIniciales,matAleat){
  
  simNodo30<-valoresIniciales[1]
  for(iterFut in 1:length(nodosFuturos)){
  # simNodo30<-valoresIniciales[1]*exp(matrizParametros[1,1]*nodosFuturos[2]/360+matrizParametros[1,2]*(nodosFuturos[2]/360)^0.5*matAleat[1,])
    aux<-simNodo30[iterFut]*exp(matrizParametros[1,1]*nodosFuturos[2]/360+matrizParametros[1,2]*(nodosFuturos[2]/360)^0.5*matAleat[1,iterFut])
    simNodo30<-c(simNodo30,aux)
  }
  simNodo30<-simNodo30[-1]
  
  
  p1<-log(valoresIniciales[2:length(valoresIniciales)]/valoresIniciales[1:(length(valoresIniciales)-1)])
  p2<-exp(-matrizParametros[(2:nrow(matrizParametros)),3]*largoNodo/360)
  p3<-matrizParametros[(2:nrow(matrizParametros)),1]*(1-exp(matrizParametros[(2:nrow(matrizParametros)),1]*largoNodo/360))
  
  matTasas<-matrix(0,ncol=length(nodosFuturos),nrow=length(nodosMatriz))
  # matAleat<-matrix(rnorm(n=length(nodosFuturos)*length(nodosMatriz)),ncol=length(nodosFuturos),nrow=length(nodosMatriz))
  matTasas[,1]<-valoresIniciales[1:nrow(matTasas)]
  
  colnames(matTasas)<-paste0("FT",nodosFuturos)
  rownames(matTasas)<-paste0("ND",nodosMatriz)
  
  ###LA MAT TASAS TIENE EN SUS COLUMNAS LA ETTI DE CADA FECHA, EN LA PRIMERA EL VALOR INICIAL ES DECIR EN 0 Y LUEGO EN CADA FECHA FUTURA 30, 60
  ###90 ETC LOS VALORES DE COMO EVOLUCIONAN ESAS TASAS
  ### EN SUS FILAS TIENE SIEMPRE EL VALOR DEL MISMO NODO DE LA ETTI PERO EN MOMENTOS DE TIEMPO DISTINTOS
  
  for(iterFut in 2:length(nodosFuturos)){
    matTasas[1,iterFut]<-matTasas[1,(iterFut-1)]*exp(matrizParametros[1,1]*largoNodo/360+matrizParametros[1,2]*(largoNodo/360)^0.5*matAleat[1,iterFut])  
    for(iterIntra in 2:nrow(matrizParametros)){
      # print(paste(iterIntra, iterFut))
    # for(iterIntra in 2:nrow(nodosMatriz)){
      p1<-log(matTasas[iterIntra,(iterFut-1)]/matTasas[(iterIntra-1),(iterFut-1)])
      p2<-exp(-matrizParametros[iterIntra,3]*largoNodo/360)
      p3<-matrizParametros[iterIntra,1]*(1-exp(-matrizParametros[iterIntra,3]*largoNodo/360))
      p4<-((1-exp(-2*matrizParametros[iterIntra,3]*largoNodo/360))*matrizParametros[iterIntra,2]^2/(2*matrizParametros[iterIntra,3]))^0.5
      matTasas[iterIntra,iterFut]<-matTasas[(iterIntra-1),(iterFut)]*exp(p1*p2+p3+p4*matAleat[iterIntra,iterFut])
    }
  }
  
  # colnames(matTasas)<-nodosFuturos
  # rownames(matTasas)<-nodosMatriz
  return(matTasas)
}
