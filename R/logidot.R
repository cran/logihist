logidot <-
function(x, y,  incre = NULL,
   sizedot=NULL, coldot=1,shapedot=1,
   pglm=FALSE, se=FALSE, sizeglm=1, colglm=1){

   # ------------   Compute coordinates of points -------------------------#
   #--------------------------------------------------------------------------# 
   newdot<-function(x, y, incre=NULL, cexR=NULL){
      # numero de puntos para cada valor
      dot0<- table(x[y==0])
      dot1<- table(x[y==1])

      # maximo numerodepountos en algun valor de 0 o 1
      maxnpO<- max(c(dot0,dot1))

      # maximo numero de puntos que  podria haber en algun valor de 0 o 1
      # para una representacion sin solapescon cex=1 
      # ojo: para una window de 7 x 7 inch conelpar(mar) habitual
      maxnpT <- 25 
 
      #cex recomendado
       if(is.null(incre)& is.null(cexR)){
          cexR <-maxnpT/maxnpO

         # reajustar solo si el cex recoemendado es <1; 
         # en caso contrario usar el cex por defecto
         # incre Recomendado
            if(cexR<1)   incre <-diff(seq(0,1, le=2*maxnpO))[1]
      if(cexR>=1){
                 cexR<-1.5
                 incre<-cexR/150
              # incre<-diff(seq(0,1, le=50))[1]
      # cexR<- 1
            }
        }
        
         # if(is.null(incre)& !is.null(cexR))  incre <- diff(seq(0,1, le=round(50/cexR)))[1]
          if(is.null(incre)& !is.null(cexR))  incre <- cexR/150
          
           #if(!is.null(incre)& is.null(cexR)) cexR <-incre/diff(seq(0,1, le=50))[1]
         if(!is.null(incre)& is.null(cexR)) cexR <-incre*150




       #coordenadas x de los valores
       c0x<-sort(unique((x[y==0])))
       c1x<-sort(unique((x[y==1])))

       puntos0<- NULL
       for(i in 1:length(dot0)){
          puntos0 <- rbind(puntos0, cbind(x=rep(c0x[i], dot0[i]),
                            y=seq(from=0, le=dot0[i], by=incre)))
       }
       puntos1<- NULL
       for(i in 1:length(dot1)){
          puntos1 <- rbind(puntos1, cbind(x=rep(c1x[i], dot1[i]),
                            y=seq(from=1, le=dot1[i], by=-incre)))
       }

       return(list(puntos0=data.frame(puntos0),
                       puntos1=data.frame(puntos1),
                       cexR=cexR))

   }
   #--------------------------------------------------------------------------#


        labx<-deparse(substitute(x))
        laby<-deparse(substitute(y))


   if(sum(class(x)=="glm")>0){
      laby <- x$terms[[2]]
      labx <- x$terms[[3]]
      y<- x$data[,names(x$data)==laby]
      x<- x$data[,names(x$data)==labx]# OJO, x tiene que asignarse siempre el ultimo porque se recicla el nombre "x"
      if(pglm==FALSE) pglm<-TRUE
     

      }

#
    
        a<-data.frame (x,y)
        names(a)<- c(labx,laby)
#



   if(!is.null(coldot)){
      if(length(coldot)>1){
         coldot0<-coldot[1] 
         coldot1<-coldot[2]
      }
      if(length(coldot)<2){
         coldot0<-coldot1<-coldot 
      }
   }

  
   if(!is.null(shapedot)){
      if(length(shapedot)>1){
         shapedot0<-shapedot[1] 
         shapedot1<-shapedot[2]
      }
      if(length(shapedot)<2){
         shapedot0<-shapedot1<-shapedot 
      }
   }


dito<- newdot(x, y, incre=incre, cexR=sizedot)

if(is.null(sizedot)) sizedot<-dito$cexR
 cat("\nSize of points = ",sizedot,"\n\n")

p <- ggplot(a, aes(x ,y))
p<- p+geom_point(data=dito$puntos0, aes(x=x, y=y), size=sizedot, colour=coldot0,shape=shapedot0)+
    geom_point(data=dito$puntos1, aes(x=x, y=y), size=sizedot, colour=coldot1,shape=shapedot1)+
    ylab(laby)+xlab(labx)

 if(pglm==TRUE) p <- p+   stat_smooth(method = "glm", method.args = list(family = "binomial"),se=se, size=sizeglm, colour=colglm)

p
#
}
