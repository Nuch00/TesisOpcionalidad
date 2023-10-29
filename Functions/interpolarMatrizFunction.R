interpolarMatrizFunction<-function(matTasas,nodosFuturos){

  
  matTasasAmpliada<-matrix(0,ncol=ncol(matTasas),nrow=length(nodosFuturos))
  rownames(matTasasAmpliada)<-paste0("ND",nodosFuturos)
  colnames(matTasasAmpliada)<-paste0("FT",nodosFuturos)
  matTasasAmpliada[which(rownames(matTasasAmpliada) %in% rownames(matTasas)),]<-matTasas
  ### interpolo para continuar
  for(iNodo in 2:length(nodosMatriz)){
    inf<-matTasasAmpliada[which(rownames(matTasasAmpliada)==paste0("ND",nodosMatriz[(iNodo-1)])),]
    sup<-matTasasAmpliada[which(rownames(matTasasAmpliada)==paste0("ND",nodosMatriz[iNodo])),]
    # aux<-as.numeric(rownames(matTasasAmpliada))
    aux<-as.numeric(gsub("\\D","",rownames(matTasasAmpliada)))
    nodosAInterpolar<-which(aux>(nodosMatriz[iNodo-1])&aux<nodosMatriz[iNodo])
    if(length(nodosAInterpolar)!=0){
      for(iInt in 1:length(nodosAInterpolar)){
        # x<-as.numeric(colnames(matTasasAmpliada)[nodosAInterpolar[iInt]])
        x<-as.numeric(gsub("\\D","",colnames(matTasasAmpliada)[nodosAInterpolar[iInt]]))
        valor<-inf+((sup-inf)/(nodosMatriz[(iNodo)]-nodosMatriz[(iNodo-1)]))*(x-nodosMatriz[(iNodo-1)])
        matTasasAmpliada[which(rownames(matTasasAmpliada)==paste0("ND",as.character(x))),]<-valor    
      }
    }
    
  }
return(matTasasAmpliada)
}
