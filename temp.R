SP <- 5
dv_SPinitial <- 20
minSPlevel <- 5
y=(1.25*(exp(-1.25*0.01*dv_SPinitial*(SP-minSPlevel)))+0.25)
y

SP=seq(5,20,length=600)
y=(1.25*(exp(-1.25*0.01*dv_SPinitial*(SP-minSPlevel)))+0.25)
plot(SP,y,type="l",lwd=2,col="red",ylab="p")




x=seq(0,4,length=600)
y=dexp(x,rate=1)+0.25
plot(x,y,type="l",lwd=2,col="red",ylab="p")
x=seq(0,1,length=200)
y=dexp(x,rate=1)
polygon(c(0,x,1),c(0,y,0),col="lightgray")


x=seq(0,5,length=600)
y=dexp(x,rate=0.8)+0.25
plot(x,y,type="l",lwd=2,col="red",ylab="p")
x=seq(0,1,length=200)
y=dexp(x,rate=0.8)+0.25
polygon(c(0,x,1),c(0,y,0),col="lightgray")

fx <- ifelse(x > -0.326 & x <0.625, 0.632,
             ifelse(x > -1.793 & x < -1.304,  0.454,
                    ifelse(x > 1.630 & x < 2.119, 0.227, 0)))
plot(fx)
x

x1 <- (1000-200-0)/(1000-200)*5
y1 <- dexp(x1,1.5)+0.5

x=seq(0,3,length=600)
y=dexp(x,1.51)+0.49
plot(x,y,type="l",lwd=2,col="red",ylab="p",ylim = c(0,2))

library(lmomco)

lmr <- vec2lmom(c(2,0.25), lscale=FALSE)
x <- seq(0.25,2,by=0.001)
y <- pdftexp(x,partexp(lmr))
plot(x,y,type="l",lwd=2,col="red",ylab="p")


## Not run:
Ff <- seq(0,1,by=0.001)
A <- partexp(vec2lmom(c(100, 0.5), lscale=FALSE))
x <- quatexp(Ff, A)
plot(x, pdftexp(x, A), pch=16, type='l')
by <- 0.01; lcvs <- c(1/3, seq(1/3+by, 1/2-by, by=by), 1/2)
reds <- (lcvs - 1/3)/max(lcvs - 1/3)
for(lcv in lcvs) {
  A <- partexp(vec2lmom(c(100, lcv), lscale=FALSE))
  x <- quatexp(F, A)
  lines(x, pdftexp(x, A),
        pch=16, col=rgb(reds[lcvs == lcv],0,0))
}
## End(Not run)
