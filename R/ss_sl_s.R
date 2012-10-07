d<-read.delim("stp_ltp_sfn_lfn_fp_u3_h1_gp1_d2.data",sep=",")
d[is.na(d)] <- 0


pe<-1
sr<-2
pesr<-3
hydra<-4
gasvpro<-5
dellype<-6
dellysr<-7


#cover<-c("20x","10x","5x","2x")
cover<-c("20x","5x","2x")
cover_i<-0

postscript(file="ss_sl_s-un_hy_gv_dl-r10x.eps",width=6.5,height=4,paper="special", horizontal=F )
#png(file="ss_sl_s-un_hy_gv_dl-r10x.png",width=10,height=4,units="in",res=300)
#png(file="ss_sl_s-un_hy_gv_dl-r10x.png",width=6.5,height=4,units="in",res=300)

#dev.new(width=16, height=4.5)
par(
	mfrow=c(4,3),
	oma=c(3.5, #south
		  0, #west
		  0, #north
		  0), #east
	mar=c(.5, #south
		  2.1, #west
		  1, #north
		  0), #east
	mgp = c(1.25, # axis title
		.25, #axis labels
		.1) #axis
)

for (var in c("Deletion", "Duplication","Insertion","Inversion") ) {

	#v<-subset(d,d$variety == "dup")
	v<-subset(d,d$variety == var)

	#for( offset in c(2,7,12,17) ) {
	for( offset in c(2,12,17) ) {
		tp_s<-offset
		tp_l<-offset+1
		fn_s<-offset+2
		fn_l<-offset+3
		fp<-offset+4

		cover_i <- cover_i + 1

		tmp_d<-c()
		for (i in 1:7 ) {
	
			sensitivity_s<- v[i,tp_s] / (v[i,tp_s] + v[i,fn_s]) 
			sensitivity_l<- v[i,tp_l] / (v[i,tp_l] + v[i,fn_l]) 

			tp <- v[i,tp_s] + v[i,tp_l]  
			fn <- v[i,fn_s] + v[i,fn_l]  

			sensitivity <- tp/(tp+fn)

			#print(v[i,fp])

			#fdr <- v[i,fp]/(v[i,fp] + tp)

			#tmp_d<-c(tmp_d,sensitivity_s,sensitivity_l,sensitivity,fdr)
			tmp_d<-c(tmp_d,sensitivity_s,sensitivity_l,sensitivity)
		}
		m<-matrix(tmp_d, nrow=3,byrow = FALSE)

		#print(m)

		if (var == "Deletion" ) {
			b<-barplot(m,beside=TRUE,ylim=c(0,1.3),axes = F,main=cover[cover_i])
		} else {
			b<-barplot(m,beside=TRUE,ylim=c(0,1.3),axes = F)
		}



		#text(1,1,labels=c("a"))
		#text(10,1,labels=c("b"))
		#text(20,1,labels=c("c"))
		#text(30,1,labels=c("d"))
		#text(40,1,labels=c("e"))


		if (offset == 2 ) {
			axis(at=c(0,0.25,0.5,0.75,1),side=2,tck=-0.025,cex.axis=0.75)
			title(ylab=var)
		}
		if (var == "Inversion" ) {
			axis(srt=45,side=1,
			#at=c(2.5,7.5,12.5,17.5,22.5,27.5,32.5),
			at=c(2.5,6.5,10.5,14.5,18.5,22.5,26.5),
			tick = FALSE,		
			las=2,
			labels=c("lumpy-pe","lumpy-sr","lumpy","hydra","gasvpro","delly-pe","delly-sr"))
		}
		lines(c(1,35),replicate(2,0.25),col="white")
		lines(c(1,35),replicate(2,0.5),col="white")
		lines(c(1,35),replicate(2,0.75),col="white")

		if ( (var == "Deletion" ) && (cover_i == 3) ){
			legend("topright",
			c("Variants < 1KB","Variants >= 1KB","All Variants"),
            pch=15,
            col=(grey.colors(n=3)),
			bty="n",
			cex=0.75)
		}



		#text(b,tmp_d+0.025,labels=round(tmp_d,2), pos=4,offset=0,cex=0.7,srt=90)
		#text(b+0.4,tmp_d+0.15,labels=round(tmp_d,2), pos=3,offset=0,cex=0.65,srt=90)

		text(b[3,]+0.25, m[3,]+.175, labels=round(m[3,],2),cex=.75,srt=90)

		#best<-sort(m[3,],decreasing=T)[1]
		#best_i<-which(m[3,]==best)

		#next_best<-sort(m[3,],decreasing=T)[2]
		#next_best_i<-which(m[3,]==next_best)

		#rt<-45

		#text(4*(best_i-1)+3.5,best+0.02,labels=c(round(best,2)),
			#pos=4,offset=0,cex=0.6,srt=rt)
		#text(4*(next_best_i-1)+3.5,next_best+0.02,labels=c(round(next_best,2)),
			#pos=4,offset=0,cex=0.6,srt=rt)


		#lines(b)
	}

	#legend("bottomleft",
		   #c("True Positives < 1KB","True Positive >= 1KB","False Positive"),
			#pch=15,
			#col=(grey.colors(n=3)),
		   #bty="n"
	#)

}
dev.off()
