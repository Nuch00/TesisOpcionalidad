lapply(paste0("./Functions/",dir("./Functions")),FUN=source)


nodosMatriz<-c(30,60,90,120,180,270,360,720,1800,3600) #los nodos en los que me voy a fijar la tasa
largoNodo<-30
#los momentos en los que voy a ver la evolucion de las tasas

matrizParametros<-matrix(0,ncol=3,nrow=length(nodosMatriz)) #por ahora los pongo a mano

matrizParametros<-matrix(ncol=3,c(1.70,	22.30,	0,5.60,	12.70,	866.2,3.40,	9.00,	1211,3.90,	7.00,	979,4.40,	7.70,	1749,2.70,	12.60,	1765,2.00,	2.20,	529,4.60,	5.90,	423,5.60,	7.20,	665,25.30,	12.60,	950)/100,byrow = TRUE)

variacion = -.15 # parametro para mover la curva inicial

valoresIniciales<-c(20.40,21.83,22.29,23.06,23.91,24.30,24.70,25.52,26.60,34.69)/100+variacion

res8<-data.frame(largo=numeric(),valor=numeric())

for(largo in nodosMatriz[-c(1,2,3)]){
  # for(largo in c(1260)){
  
  nodosFuturos<-seq(from=0,to=largo,by=largoNodo)
  
  VAopcion<-c()
  
  for(iter in 1:10000){
    
    matAleat<-matrix(rnorm(length(nodosFuturos)*length(nodosMatriz)),ncol=length(nodosFuturos),nrow=length(nodosMatriz))
    ### Simulo para cada periodo la decision del agente
    
    ref<-length(which(nodosMatriz<=largo))
    
    matTasas<-simularMatrizTasaFunction(nodosMatriz=nodosMatriz[which(nodosMatriz<=largo)],largoNodo=largoNodo,nodosFuturos=nodosFuturos,
                                        matrizParametros=matrizParametros[1:ref,],
                                        valoresIniciales=valoresIniciales[1:ref]
                                        ,matAleat=matAleat)
    
    
    ## interpolo para los valores faltantes
    matTasasAmpliada<-interpolarMatrizFunction(matTasas,nodosFuturos)
    # plot(y=matTasasAmpliada[,3],x=as.numeric(rownames(matTasasAmpliada)))
    
    ## defino las caracteristicas iniciales del prestamo
    plazo<-largo
    tasaContinua<-matTasas[which(rownames(matTasas)==paste0("ND",plazo)),1] #tasa Continua que sale de la simulacion
    
    capital<-10000
    perEntreCuotas<-30
    nCuotas<-plazo/30
    planPago<-prestamoFrancesFunction(capital=capital,tasaContinua =tasaContinua,nCuotas=nCuotas,perEntreCuotas=30)
    planPago<-planCorregidoFunction(tasaContinua = tasaContinua,planPago = planPago,etti=matTasasAmpliada[,1])
    tasaAnterior<-planPago[[2]]
    planPago<-planPago[[1]]
    valorCuota<-planPago[nrow(planPago),3]
    
    resultado<-rep(0,plazo/30)
    contadorPago=0
    planPagoconCero<-matrix(0,ncol=3,nrow=nrow(planPago))
    planes<-list()
    planes$Inicial<-data.frame(planPago)
    
    for(iPer in 2:(length(nodosFuturos)-2)){
      periodo<-nodosFuturos[iPer] #me paro en periodo y miro la tasa
      tiempoRestante<-largo-nodosFuturos[iPer]
      ref<-which(colnames(matTasasAmpliada)==paste0("FT",as.character(tiempoRestante)))
      
      
      tasaContinua<-matTasasAmpliada[ref,iPer]
      capital<-sum(planPago[(iPer:nrow(planPago)),1])
      nCuotas<-largo/30-iPer+1
      
      
      planPagoPropuesto<-prestamoFrancesFunction(capital=capital,tasaContinua=tasaContinua,nCuotas=nCuotas,perEntreCuotas=30)
      planPagoPropuesto<-planCorregidoFunction(tasaContinua = tasaContinua,planPago = planPagoPropuesto,etti=matTasasAmpliada[,iPer])
      
      tasaVigente<-planPagoPropuesto[[2]]
      planPagoPropuesto<-planPagoPropuesto[[1]]
      aux<-planPagoconCero
      aux[iPer:nrow(planPagoconCero),]<-planPagoPropuesto
      planPagoPropuesto<-aux
      valorCuotaPropuesto<-planPagoPropuesto[nrow(planPagoPropuesto),3]
      
      if(tasaVigente<tasaAnterior&iPer!=(length(nodosFuturos)-1)){
        eval(parse(text=paste0("planes$Periodo",iPer,"<-data.frame(planPagoPropuesto)")))
        tasaAnterior<-tasaVigente
        nuevoPlazo<-largo-iPer*perEntreCuotas
        ahorro<-valorCuota-valorCuotaPropuesto
        
        plazos<-seq(from=0,to=nuevoPlazo,by=30)/30
        ref<-(length(nodosFuturos[-1])-length(plazos)+1):length(nodosFuturos[-1])
        VA<-sum((1+matTasasAmpliada[ref,iPer]/12)^(-plazos)*ahorro) #este ahorro esta a VA de iPer, lo actualizo a 0 con la etti de 0
        VA<-VA*(1+matTasasAmpliada[iPer,1])^(-nodosFuturos[iPer]/30)
        
        #listo ahora solo queda guardar el resultado y actualizar los plazos y variables
        planPago<-planPagoPropuesto
        tasaAnterior<-tasaVigente
        valorCuota<-valorCuotaPropuesto
        resultado[iPer]<-VA
        
      }else{contadorPago<-contadorPago+contadorPago}
      
    }
    
    VAopcion<-c(VAopcion,sum(resultado))
  }
  
  
  res7<-rbind(res7,data.frame(largo=largo,valor=mean(VAopcion)))
  
}