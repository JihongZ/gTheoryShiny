rm(list=ls())
library(mvtnorm)
library(tidyverse)
V=2
Np=100
# Ni=12
Ni=50
Ncol<-24
grand.mu<-c(50,60)
zero.vector<-rep(0,V)
p.effect.cov<-matrix(c(4,2,2,3.4),2,2)
i.effect.cov<-matrix(c(1.8,0.5,0.5,0.8),2,2)
h.effect.cov<-matrix(c(3.5,1.5,1.5,2.5),2,2)

pi.effect.cov<-matrix(c(1.0,0.3,0.3,1.4),2,2)
ih.effect.cov<-matrix(c(1.8,0.5,0.5,1.2),2,2)
ph.effect.cov<-matrix(c(2.2,0.2,0.2,1.2),2,2)

e.effect.cov<-matrix(c(3.8,1.2,1.2,2.8),2,2)


#######################Design 1#######################
#------------#
# Dim: 100 * 24
#------------#

temp.p<-rmvnorm(Np,grand.mu,p.effect.cov)
temp.i<-rmvnorm(Ni,zero.vector,i.effect.cov)
temp.e<-rmvnorm(Np*Ni,zero.vector,e.effect.cov)

	#For Person Effect
	TEMP.P<-NULL
	for(v in 1:V){
		for( i in 1:Ni){
		TEMP.P<-cbind(TEMP.P,temp.p[,v])
        } 
	}	
	#For Item Effect
	TEMP.I<-NULL
	for (p in 1:Np){
		TEMP.I<-rbind(TEMP.I,c(t(temp.i)[1,],t(temp.i)[2,]))
	}
	#For Error Effect
	TEMP.E<-matrix(temp.e,Np,Ni*V)
Dat1<-TEMP.P+TEMP.I+TEMP.E

write.table(Dat1,'ExampleCode/Zhehan/Dat1.txt',row.name=F,col.name=F)	

#------------#
# For shiny apps
#------------#
# Add facet name tags
Dat1_Shiny <- rbind(
  c("Person", paste0("I", 1:Ni), paste0("I", 1:Ni)),
  c("Person", rep("V1", Ni), rep("V2", Ni)),
  as.data.frame(Dat1) |> rownames_to_column("Person")
)
dim(Dat1_Shiny)
write.csv(Dat1_Shiny, file = "ExampleCode/Zhehan/Dat1_Shiny.csv", row.names = F)

#######################Design 2#######################
Ni.v1<-11
Ni.v2<-13
temp.p<-rmvnorm(Np,grand.mu,p.effect.cov)
temp.i.v1<-rnorm(Ni.v1,0,sqrt(1.8))
temp.i.v2<-rnorm(Ni.v2,0,sqrt(0.8))
temp.e.v1<-rnorm(Ni.v1*Np,0,sqrt(3.8))
temp.e.v2<-rnorm(Ni.v2*Np,0,sqrt(2.8))
	#For Person Effect
	TEMP.P<-cbind(matrix(rep(temp.p[,1],Ni.v1),Np,Ni.v1),matrix(rep(temp.p[,2],Ni.v2),Np,Ni.v2))
	#For Item Effect
	TEMP.I<-cbind(matrix(rep(temp.i.v1,Np),Np,Ni.v1,byrow=T),matrix(rep(temp.i.v2,Np),Np,Ni.v2,byrow=T))
	#For Error Effect
	TEMP.E<-cbind(matrix(temp.e.v1,Np,Ni.v1),matrix(temp.e.v2,Np,Ni.v2))
Dat2<-TEMP.P+TEMP.I+TEMP.E
write.table(Dat2,'ExampleCode/Zhehan/Dat2.txt',row.name=F,col.name=F)	

#------------#
# Shiny app
#------------#
Dat2_Shiny <- rbind(
  c("Person", paste0("I", 1:24)),
  c("Person", rep("V1", 11), rep("V2", 13)),
  as.data.frame(Dat2) |> rownames_to_column("Person")
)
write.csv(Dat2_Shiny, file = "ExampleCode/Zhehan/Dat2_Shiny.csv", row.names = F)

#######################Design 3#######################
Nh<-3
Ni<-4
temp.p<-rmvnorm(Np,grand.mu,p.effect.cov)
temp.i<-rmvnorm(Ni,zero.vector,i.effect.cov)
temp.h<-rmvnorm(Nh,zero.vector,h.effect.cov)
temp.e<-rmvnorm(Np*Ni*Nh,zero.vector,e.effect.cov)
temp.pi<-rmvnorm(Ni*Np,zero.vector,pi.effect.cov)
temp.ph<-rmvnorm(Nh*Np,zero.vector,ph.effect.cov)
temp.ih<-rmvnorm(Ni*Nh,zero.vector,ih.effect.cov)
    #For Person Effect
	TEMP.P<-NULL
	for(v in 1:V){
		for( i in 1:(Ni*Nh)){
		TEMP.P<-cbind(TEMP.P,temp.p[,v])
        } 
	}	
	#For Item Effect
	TEMP.I<-NULL
	for (p in 1:Np){
		TEMP.I<-rbind(TEMP.I,c(rep(t(temp.i)[1,],Nh),rep(t(temp.i)[2,],Nh)))
	}
	#For H Effect
	TEMP.H<-NULL
	for (p in 1:Np){
			TEMP.H<-rbind(TEMP.H,c(rep(temp.h[,1],each=Ni),rep(temp.h[,2],each=Ni)))
	}
	#For Error Effect
	TEMP.E<-matrix(temp.e,Np,Ni*Nh*V)
	#For PI Effect
	TEMP.PI<-NULL
	TEMP.PI<-cbind(matrix(rep(matrix(temp.pi[,1],Np,Ni),Nh),Np,Ni*Nh),matrix(rep(matrix(temp.pi[,2],Np,Ni),Nh),Np,Ni*Nh))
	#For PH Effect
	TEMP.PH<-NULL
	TEMP.PH<-cbind(matrix(rep(temp.ph[,1],each=Ni),Np,Ni*Nh,byrow=T),matrix(rep(temp.ph[,2],each=Ni),Np,Ni*Nh,byrow=T))
	#For IH Effect
	TEMP.IH<-NULL
	for (p in 1:Np){
	TEMP.IH<-rbind(TEMP.IH,c(temp.ih[,1],temp.ih[,2]))
	}
	#For Error Effect
	TEMP.E<-cbind(matrix(temp.e[,1],Np,Ni*Nh),matrix(temp.e[,2],Np,Ni*Nh))
	
Dat3<-TEMP.P + TEMP.I + TEMP.H + TEMP.IH + TEMP.PH + TEMP.PI + TEMP.E	
write.table(Dat3,'Dat3.txt',row.name=F,col.name=F)	
#######################Design 4#######################
Nh<-8
Nh.v1<-3
Nh.v2<-5
Ni<-3
temp.p<-rmvnorm(Np,grand.mu,p.effect.cov)
temp.i<-rmvnorm(Ni,zero.vector,i.effect.cov)
temp.h.v1<-rnorm(Nh.v1,0,sqrt(3.5))
temp.h.v2<-rnorm(Nh.v2,0,sqrt(2.5))
temp.pi<-rmvnorm(Ni*Np,zero.vector,pi.effect.cov)
temp.ph.v1<-rnorm((Np*Nh.v1),0,sqrt(2.2))
temp.ph.v2<-rnorm(Np*Nh.v2,0,sqrt(1.2))
temp.ih.v1<-rnorm(Ni*Nh.v1,0,sqrt(1.8))
temp.ih.v2<-rnorm(Ni*Nh.v2,0,sqrt(1.2))
temp.e.v1<-rnorm(Ni*Nh.v1*Np,0,sqrt(3.8))
temp.e.v2<-rnorm(Ni*Nh.v2*Np,0,sqrt(2.8))
	#For Person Effect
	TEMP.P<-NULL
	TEMP.P<-cbind(matrix(rep(temp.p[,1],Ni*Nh.v1),Np,Ni*Nh.v1),matrix(rep(temp.p[,2],Ni*Nh.v2),Np,Ni*Nh.v2))
	#For Item Effect
	TEMP.I<-NULL
	for (p in 1:Np){
	TEMP.I<-rbind(TEMP.I,c(rep(temp.i[,1],Nh.v1),rep(temp.i[,2],Nh.v2)))
	}
	#For H Effect
	TEMP.H<-NULL
	for (p in 1:Np){
	TEMP.H<-rbind(TEMP.H,c(rep(temp.h.v1,each=Ni),rep(temp.h.v2,each=Ni)))
	}
	#For PI Effect
	TEMP.PI<-NULL
	TEMP.PI<-cbind(matrix(rep(matrix(temp.pi[,1],Np,Ni),Nh.v1),Np,Ni*Nh.v1),matrix(rep(matrix(temp.pi[,2],Np,Ni),Nh.v2),Np,Ni*Nh.v2))
	#For PH Effect
	TEMP.PH<-NULL
	for (p in 1:Np){
	TEMP.PH<-cbind(matrix(rep(temp.ph.v1,each=Ni),Np,Ni*Nh.v1,byrow=T),matrix(rep(temp.ph.v2,each=Ni),Np,Ni*Nh.v2,byrow=T))
	}
	#For IH Effect
	TEMP.IH<-NULL
	for (p in 1:Np){
	TEMP.IH<-rbind(TEMP.IH,c(temp.ih.v1,temp.ih.v2))
	}
	
	#For Error Effect
TEMP.E<-cbind(matrix(temp.e.v1,Np,Nh.v1*Ni),matrix(temp.e.v2,Np,Nh.v2*Ni))
Dat4<-TEMP.P + TEMP.I + TEMP.H + TEMP.IH + TEMP.PH + TEMP.PI + TEMP.E
write.table(Dat4,'Dat4.txt',row.name=F,col.name=F)	
#######################Design 5######################
Nh<-3
Ni.v1<-12
Ni.v2<-12
Ni.h1.v1<-3
Ni.h2.v1<-4
Ni.h3.v1<-5
Ni.h1.v2<-3
Ni.h2.v2<-5
Ni.h3.v2<-4
temp.p<-rmvnorm(Np,grand.mu,p.effect.cov)
temp.h<-rmvnorm(Nh,zero.vector,h.effect.cov)
temp.ih.v1<-rnorm(Ni.v1,0,sqrt(1.8))
temp.ih.v2<-rnorm(Ni.v2,0,sqrt(1.2))
temp.ph<-rmvnorm(Nh*Np,zero.vector,ph.effect.cov)
temp.e.v1<-rnorm(Ni.v1*Np,0,sqrt(3.8))
temp.e.v2<-rnorm(Ni.v2*Np,0,sqrt(2.8))
	#For Person Effect
	TEMP.P<-NULL
	TEMP.P<-cbind(matrix(rep(temp.p[,1],Ni.v1),Np,Ni.v1),matrix(rep(temp.p[,2],Ni.v2),Np,Ni.v2))
	#For H Effect
	TEMP.H<-NULL
	for (p in 1:Np){
			TEMP.H<-rbind(TEMP.H,c(rep(temp.h[1,1],Ni.h1.v1),rep(temp.h[2,1],Ni.h2.v1),rep(temp.h[3,1],Ni.h3.v1),
			rep(temp.h[1,2],Ni.h1.v2),rep(temp.h[2,2],Ni.h2.v2),rep(temp.h[3,2],Ni.h3.v2)))	
	}
	#For IH Effect
	TEMP.IH<-NULL
	for (p in 1:Np){
		TEMP.IH<-rbind(TEMP.IH,c(temp.ih.v1,temp.ih.v2))
	}
	#For PH Effect
	TEMP.PH1<-NULL
	TEMP.PH2<-NULL
	for (h in 1:Nh){
		sel1<-c(Ni.h1.v1,Ni.h2.v1,Ni.h3.v1)
		TEMP.PH1<-c(TEMP.PH1,rep(matrix(temp.ph[,1],Np,Nh)[,h],sel1[h]))
	}	
	for (h in 1:Nh){
		sel2<-c(Ni.h1.v2,Ni.h2.v2,Ni.h3.v2)
		TEMP.PH2<-c(TEMP.PH2,rep(matrix(temp.ph[,2],Np,Nh)[,h],sel2[h]))
	}
	TEMP.PH1<-matrix(TEMP.PH1,Np,(Ni.v1))
	TEMP.PH2<-matrix(TEMP.PH2,Np,(Ni.v2))
	TEMP.PH<-cbind(TEMP.PH1,TEMP.PH2)
	#For E Effect
	TEMP.E<-cbind(matrix(temp.e.v1,Np,Ni.v1),matrix(temp.e.v2,Np,Ni.v2))
Dat5<-TEMP.P + TEMP.H + TEMP.IH + TEMP.PH + TEMP.E
write.table(Dat5,'Dat5.txt',row.name=F,col.name=F)	
#######################Design 6######################
#              v1                            v2
#         h1         h2            h3        h4          h5
#       i1-i5       i6-i12      i13-i15    i16-i19     i20-i24
Ni.h1<-5
Ni.h2<-7
Ni.h3<-3
Ni.h4<-4
Ni.h5<-5
Nh.v1<-2
Nh.v2<-3
Ni.v1<-12
Ni.v2<-12
Nh<-5
temp.p<-rmvnorm(Np,grand.mu,p.effect.cov)
temp.h<-rmvnorm(Nh,zero.vector,h.effect.cov)
temp.ih.v1<-rnorm(Ni.v1,0,sqrt(1.8))
temp.ih.v2<-rnorm(Ni.v2,0,sqrt(1.2))
temp.ph.v1<-rnorm(Np*Nh.v1,0,sqrt(2.2))
temp.ph.v2<-rnorm(Np*Nh.v2,0,sqrt(1.2))
temp.e.v1<-rnorm(Ni.v1*Np,0,sqrt(3.8))
temp.e.v2<-rnorm(Ni.v2*Np,0,sqrt(2.8))

	#For Person Effect
	TEMP.P<-NULL
	TEMP.P<-cbind(matrix(rep(temp.p[,1],Ni.v1),Np,Ni.v1),matrix(rep(temp.p[,2],Ni.v2),Np,Ni.v2))
	#For H Effect
	TEMP.H<-NULL
	for( p in 1:Np){
	TEMP.H<-rbind(TEMP.H,c(rep(temp.h[1,1],Ni.h1),rep(temp.h[2,1],Ni.h2),rep(temp.h[1,2],Ni.h3),rep(temp.h[2,2],Ni.h4),rep(temp.h[3,2],Ni.h5)))
	}
	#For IH Effect
	TEMP.IH<-NULL
	for (p in 1:Np){
	TEMP.IH<-rbind(TEMP.IH,c(temp.ih.v1,temp.ih.v2))
	}
	#For PH Effect
	TEMP.PH<-NULL
	TEMP.PH1<-NULL
	TEMP.PH2<-NULL
	for (h in 1:Nh.v1){
	sel1<-c(Ni.h1,Ni.h2)
	TEMP.PH1<-c(TEMP.PH1,rep(matrix(temp.ph.v1,Np,Nh.v1)[,h],sel1[h]))}
	for (h in 1:Nh.v2){
	sel2<-c(Ni.h3,Ni.h4,Ni.h5)
	TEMP.PH2<-c(TEMP.PH2,rep(matrix(temp.ph.v2,Np,Nh.v2)[,h],sel2[h]))}
	TEMP.PH1<-matrix(TEMP.PH1,Np,(Ni.v1))
	TEMP.PH2<-matrix(TEMP.PH2,Np,(Ni.v2))
	TEMP.PH<-cbind(TEMP.PH1,TEMP.PH2)
	#For E Effect
	TEMP.E<-cbind(matrix(temp.e.v1,Np,Ni.v1),matrix(temp.e.v2,Np,Ni.v2))
Dat6<-round((TEMP.P + TEMP.H + TEMP.IH + TEMP.PH + TEMP.E),2)
write.table(Dat6,'Dat6.txt',row.name=F,col.name=F)





