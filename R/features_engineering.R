#### adding the periodicity of 3 years variable
w <- 2*pi/52
Time <- 1:nrow(merged_iq_train)
Nfourier<-52
for(i in c(1:Nfourier))
{
  assign(paste("cos", i, sep=""),cos(w*Time*i))
  assign(paste("sin", i, sep=""),sin(w*Time*i))
}
objects()
#####################insertion de la base de fourier dans la data.frame

cos<-paste('cos',c(1:Nfourier),sep="",collapse=",")                         
sin<-paste('sin',c(1:Nfourier),sep="",collapse=",")
paste("data.frame(merged_iq_train,",cos,",",sin,")",sep="")
merged_iq_train <- eval(parse(text=paste("data.frame(merged_iq_train,",cos,",",sin,")",sep="")))
names(merged_iq_train)