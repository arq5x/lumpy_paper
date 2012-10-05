d<-read.table("res.data",sep=",",header=T)



postscript(file="resolution-un.eps",width=3,height=3,paper="special", horizontal=F )
#png(file="resolution-un.png",width=3,height=3,units="in",res=300)



par(
	oma=c(2.5, #south
		  0, #west
		  0, #north
		  0), #east
	mar=c(0, #south
		  2.5, #west
		  1, #north
		  1.5), #east
	mgp = c(1.5, # axis title
		.5, #axis labels
		.1) #axis
)


b<-barplot(
	as.matrix(d[1:3,c(2,4,5)]),
	beside=T,
	ylim=c(1,700),
	log="y",
	axes=T,
	ylab="Median Interval Size (bp)",
	#main="Resolution for Deletion Calls",
	axisnames=F,
	cex.axis=0.75,
	cex=0.75
)
s<-c(as.matrix(d[1:3,c(2,4,5)]))
e<-c(as.matrix(d[1:3,c(6,8,9)]))

for (l in c(2,5,10,20,50,100,200)) {
	lines(c(1,12),c(l,l),col="white")
}
arrows(b,s, b, s+e, angle=90, code=3, length=0.1)
axis(srt=45,side=1,
	at=c(1:3,5:7,9:11)+0.5,
	tick = FALSE,		
	las=2,
	labels=c("LUMPY-PE","LUMPY-SR","LUMPY","LUMPY-PE","LUMPY-SR","LUMPY","LUMPY-PE","LUMPY-SR","LUMPY")
)
text(c(2.5,6.5,10.5), 500, labels=c("20x","5x","2x"))

dev.off()
