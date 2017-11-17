logibox <-
function(x,y, boxwd = 0.1, wiswd=0.05, displac=0,
   sizepat=1,fillb=NA, colob=1, colpat=1, colbig=1,colmed=1,
   sizeb=1, sizebig=1, sizemed=2,  colout=1, sizeout=1, shapeout=1,
   pglm=FALSE, se=FALSE, sizeglm=1, colglm=1){

       

        labx<-deparse(substitute(x))
        laby<-deparse(substitute(y))

   if(sum(class(x)=="glm")>0){
      laby <- x$terms[[2]]
      labx <- x$terms[[3]]
      y<- x$data[,names(x$data)==laby]
      x<- x$data[,names(x$data)==labx]# OJO, x tiene que asignarse siempre el ultimo porque se recicla el nombre "x"
      if(pglm==FALSE) pglm<-TRUE
      #if(is.null(pglm)) pglm<-TRUE
      }
#
        a<-data.frame (x,y)
        names(a)<- c(labx,laby)
#



  
   # posicion en la que se van a dibujar los boxpots
   at<- c(0,1)

# displac: desplazamiento hacia afuera (por encima de 1 y por debajo de 0 para dibujar los boxplots
#boxwd: grosor de la caja (en unidades relativas a ylim= c(0,1)
#wiswd: grosor de las patillas (en unidades relativas a ylim= c(0,1)
#sizepat: color del borde de la caja uno o dos valores
#fillb: color para el relleno de la caja, uno o dos colores
#colob: color para el borde de la caja
#collpat: color para las patillas
#colbig: color para los bigotes
#colmed: color para la mediana 
#sizeb: grosor del bordede la caja
#sizepat: grososr de las patillas
#sizebig: grosor de los bigotes
#sizemed: grososr de la mediana
   if(!is.null(sizepat)){
      if(length(sizepat)>1){
         sizepat0<-sizepat[1] 
         sizepat1<-sizepat[2]
      }
      if(length(sizepat)<2){
         sizepat0<-sizepat1<-sizepat 
      }
   }

   if(is.null(fillb)) fillb<-NA
   if(!is.null(fillb)){
      if(length(fillb)>1){
         fillb0<-fillb[1] 
         fillb1<-fillb[2]
      }
      if(length(fillb)<2){
         fillb0<-fillb1<-fillb 
      }
   }

   if(!is.null(colob)){
      if(length(colob)>1){
         colob0<-colob[1] 
         colob1<-colob[2]
      }
      if(length(colob)<2){
         colob0<-colob1<-colob 
      }
   }

   if(!is.null(colpat)){
      if(length(colpat)>1){
         colpat0<-colpat[1] 
         colpat1<-colpat[2]
      }
      if(length(colpat)<2){
         colpat0<-colpat1<-colpat 
      }
   }

   if(!is.null(colbig)){
      if(length(colbig)>1){
         colbig0<-colbig[1] 
         colbig1<-colbig[2]
      }
      if(length(colbig)<2){
         colbig0<-colbig1<-colbig 
      }
   }

   if(!is.null(colmed)){
      if(length(colmed)>1){
         colmed0<-colmed[1] 
         colmed1<-colmed[2]
      }
      if(length(colmed)<2){
         colmed0<-colmed1<-colmed 
      }
   }

   if(!is.null(sizeb)){
      if(length(sizeb)>1){
         sizeb0<-sizeb[1] 
         sizeb1<-sizeb[2]
      }
      if(length(sizeb)<2){
         sizeb0<-sizeb1<-sizeb 
      }
   }

   if(!is.null(sizepat)){
      if(length(sizepat)>1){
         sizepat0<-sizepat[1] 
         sizepat1<-sizepat[2]
      }
      if(length(sizepat)<2){
         sizepat0<-sizepat1<-sizepat 
      }
   }

   if(!is.null(sizebig)){
      if(length(sizebig)>1){
         sizebig0<-sizebig[1] 
         sizebig1<-sizebig[2]
      }
      if(length(sizebig)<2){
         sizebig0<-sizebig1<-sizebig 
      }
   }
  
 if(!is.null(sizemed)){
      if(length(sizemed)>1){
         sizemed0<-sizemed[1] 
         sizemed1<-sizemed[2]
      }
      if(length(sizemed)<2){
         sizemed0<-sizemed1<-sizemed 
      }
   }


   if(!is.null(colout)){
      if(length(colout)>1){
         colout0<-colout[1] 
         colout1<-colout[2]
      }
      if(length(colout)<2){
         colout0<-colout1<-colout 
      }
   }

if(!is.null(shapeout)){
      if(length(shapeout)>1){
         shapeout0<-shapeout[1] 
         shapeout1<-shapeout[2]
      }
      if(length(shapeout)<2){
         shapeout0<-shapeout1<-shapeout 
      }
   }

 if(!is.null(sizeout)){
      if(length(sizeout)>1){
         sizeout0<-sizeout[1] 
         sizeout1<-sizeout[2]
      }
      if(length(sizeout)<2){
         sizeout0<-sizeout1<-sizeout 
      }
   }


bp<-boxplot(x~as.factor(y), horizontal =TRUE,plot=FALSE )

# para el 0
  i=1
  at=0-displac
caja0 <- data.frame(x=bp$stats[c(2,4,4,2),i], y=at+c(-1,-1,+1,+1)*boxwd/2)
bigotes10 <- data.frame(x=bp$stats[c(1,1),i], y=at+c(-1,+1)*wiswd/2)
bigotes20 <- data.frame(x=bp$stats[c(5,5),i], y=at+c(-1,+1)*wiswd/2)


mediana0<- data.frame(x=bp$stats[c(3,3),i], y=at+c(-1,+1)*boxwd/2)
patilla10<- data.frame(x=bp$stats[c(1,2),i], y=c(at, at))
patilla20<- data.frame(x=bp$stats[c(4,5),i], y=c(at, at))

# METER UN IF (por si no hay outliers)
outliers0<-bp$out[bp$group==i]
outliers0<-data.frame(x=outliers0, y=rep(at, length(outliers0)))



# para el 1
  i=2
  at=1+displac
caja1 <- data.frame(x=bp$stats[c(2,4,4,2),i], y=at+c(-1,-1,+1,+1)*boxwd/2)
bigotes11 <- data.frame(x=bp$stats[c(1,1),i], y=at+c(-1,+1)*wiswd/2)
bigotes21 <- data.frame(x=bp$stats[c(5,5),i], y=at+c(-1,+1)*wiswd/2)


mediana1<- data.frame(x=bp$stats[c(3,3),i], y=at+c(-1,+1)*boxwd/2)
patilla11<- data.frame(x=bp$stats[c(1,2),i], y=c(at, at))
patilla21<- data.frame(x=bp$stats[c(4,5),i], y=c(at, at))

# METER UN IF (por si no hay outliers)
outliers1<-bp$out[bp$group==i]
outliers1<-data.frame(x=outliers1, y=rep(at, length(outliers1)))

p <- ggplot(a, aes(x ,y))
p<-p+geom_polygon(data=caja0, aes(x=x, y=y), colour=colob0,fill=fillb0, size=sizeb0)+
          geom_line(data=bigotes10, aes(x=x, y=y),size=sizebig0, colour=colbig0)+
          geom_line(data=bigotes20, aes(x=x, y=y),size=sizebig0, colour=colbig0)+
          geom_line(data=patilla10, aes(x=x, y=y),size=sizepat0, colour=colpat0)+
          geom_line(data=patilla20, aes(x=x, y=y),size=sizepat0, colour=colpat0)+
          geom_line(data=mediana0, aes(x=x, y=y),size=sizemed0, colour=colmed0)+
          geom_point(data=outliers0, aes(x=x, y=y),size=sizeout0, colour=colout0, shape=shapeout0)+
          geom_polygon(data=caja1, aes(x=x, y=y), colour=colob1,fill=fillb1, size=sizeb1)+
          geom_line(data=bigotes11, aes(x=x, y=y),size=sizebig1, colour=colbig1)+
          geom_line(data=bigotes21, aes(x=x, y=y),size=sizebig1, colour=colbig1)+
          geom_line(data=patilla11, aes(x=x, y=y),size=sizepat1, colour=colpat1)+
          geom_line(data=patilla21, aes(x=x, y=y),size=sizepat1, colour=colpat1)+
          geom_line(data=mediana1, aes(x=x, y=y),size=sizemed1, colour=colmed1)+
          geom_point(data=outliers1, aes(x=x, y=y),size=sizeout1, colour=colout1, shape=shapeout1)+
          ylab(laby)+xlab(labx)

 if(pglm==TRUE) p <- p+   stat_smooth(method = "glm", method.args = list(family = "binomial"),se=se, size=sizeglm, colour=colglm)

p
}
