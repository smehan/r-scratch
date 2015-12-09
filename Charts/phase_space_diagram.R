###########################################################
# Class that calculates a p phase space of a kicked rotor
# http://www.r-bloggers.com/phase-space-plot-of-the-kicked-rotor/
###########################################################

smap<-function(t,l) {
    K<- 1
    for ( i in seq(1e4) ) { 
        t[i+1]<- (t[i]+l[i]) %% (2*pi)
        l[i+1]<- (l[i]+K*sin(t[i+1])) %% (2*pi)
    }   
    cbind(t,l)
}

par(mar=rep(0,4))
plot(NULL,xlim=c(0,2*pi),ylim=c(0,2*pi),axes=F,xlab="",ylab="")
for (i in seq(10000) ) { 
    t<-c(runif(1)*2*pi,runif(1)*2*pi)
    m<-smap( t[1],t[2] )
    points(m[,1],m[,2],col=rgb(runif(1),runif(1),runif(1)) ,pch=".",cex=2)
}

