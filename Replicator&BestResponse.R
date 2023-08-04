replicator2=function(p1,p2,delta=0.001,x0=seq(0,1,length=15),T=5000){
  n=length(x0)
  x=matrix(0,T+1,n)
  x[1,]=x0
  for(t in 1:T)x[t+1,]=(1-delta)*x[t,]+delta*p1(x[t,])*x[t,]/(p1(x[t,])*x[t,]+p2(x[t,])*(1-x[t,]))
  par(cex.lab=1.5,cex.axis=1.5)
  matplot(x,type="l",lty=1,lwd=2,xlab="time",ylab="frequency")
}

best.response=function(p1,p2,delta=0.001,x0=seq(0,1,length=15),T=1500){
  n=length(x0)
  x=matrix(0,T+1,n)
  x[1,]=x0
  for(t in 1:T){
    temp=x[t,]*(1-delta)
    become1=which(p1(x[t,])>p2(x[t,]))
    if(length(become1>0))temp[become1]=temp[become1]+delta
    x[t+1,]=temp
  }
  par(cex.lab=1.5,cex.axis=1.5)
  matplot(x,type="l",lty=1,lwd=2,xlab="time",ylab="frequency")
}

replicator3=function(p1,p2,p3,delta=0.01,T=15000,x0,y0,type=1){
  n=length(x0)
  x=matrix(0,T+1,n)
  y=matrix(0,T+1,n)
  x[1,]=x0
  y[1,]=y0
  for(i in 1:n){
    for(t in 1:T){
      xt=x[t,i]
      yt=y[t,i]
      temp1=max(p1(xt,yt)*xt,0)
      temp2=max(p2(xt,yt)*yt,0)
      temp3=max(p3(xt,yt)*(1-xt-yt),0)
      temp=temp1+temp2+temp3
      x[t+1,i]=(1-delta)*xt+delta*temp1/temp
      y[t+1,i]=(1-delta)*yt+delta*temp2/temp
    }
  }
  if(type==1){
    x.new=x+y/2
    y.new=y/1.14142
    par(cex.lab=1.5,cex.axis=1.5)
    matplot(x.new,y.new,type="l",lty=1,lwd=2,xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=c(-0.1,1.1),ylim=c(-0.1,1/1.14142+0.1))
    lines(c(0,1,1/2,0),c(0,0,1/1.14142,0),lwd=3)
    text(0,-0.05,"strategy 3",cex=1.25)	
    text(1,-0.05,"strategy 1",cex=1.25)	
    text(0.5,1/1.14142+0.05,"strategy 2",cex=1.25)
  }
  if(type==2){
    matplot(x,type="l",lwd=2,xlab="time",ylab="frequencies",ylim=c(0,1),col="blue")
    matplot(y,type="l",lwd=2,add=TRUE,col="red")
    matplot(1-x-y,type="l",lwd=2,add=TRUE,col="green")}	
}




