library(circleplot)
library(readxl)
library(RColorBrewer)

# test data
# input<-list(dataset="richness", method="method2", target="inverts", budget="10", line.breaks="absolute", line.cols="increasing")

import.xl.sheets <- function(file, toDist=FALSE) {
	sheets <- readxl::excel_sheets(file)
	result<-lapply(sheets, function(x) readxl::read_excel(file, sheet = x))
	if(toDist){result<-lapply(result, FUN=function(x){as.dist(as.matrix(x[, -1]))})}
	names(result) <- sheets
	return(result)
}

capitalize<-function(vector){
	unlist(lapply(strsplit(vector, ""), 
		FUN=function(x){paste(c(toupper(x[1]), x[c(2:length(x))]), collapse="")})
	)}

# import data
congruence<-list(
	composition=import.xl.sheets("composition_congruence.xlsx", toDist=TRUE),
	richness=import.xl.sheets("richness_congruence.xlsx", toDist=TRUE))
surrogacy<-import.xl.sheets("surrogacy_value.xlsx")
target.values<-list(
	composition=data.frame(
		target=c('allSpecies', 'inverts', 'plants', 'verts'),
		start=c(1, 6, 5, 1),
		n=c(11, 6, 1, 4),
		stringsAsFactors=FALSE),
	richness=data.frame(
		target=c('allSpecies', 'inverts', 'plants', 'verts'),
		start=c(1, 7, 5, 1),
		n=c(12, 6, 2, 4),
		stringsAsFactors=FALSE))

# wrap later code in Shiny infrastructure
shinyServer(function(input, output) {
output$circleplot<-renderPlot({

# select data for this run
congruence.thisrun<-congruence[[input$dataset]][[input$method]]
	attr(congruence.thisrun, "Labels")<-capitalize(attr(congruence.thisrun, "Labels"))
surr<-surrogacy[[input$dataset]]
rows<-which(c(surr$input==input$method &
	surr$target==input$target & 
	surr$budget<=as.numeric(input$budget))==TRUE)
surrogacy.thisrun<-surr[rows, ]

# line breaks
if(input$line.breaks=="quantile"){
	line.breaks<-quantile(congruence.thisrun, breaks=c(0, 0.25, 0.5, 0.75, 1))
}else{line.breaks<-seq(min(congruence.thisrun), max(congruence.thisrun), length.out=5)}

if(input$line.cols=="increasing"){line.cols<-brewer.pal(4, "Purples")
}else{line.cols<-brewer.pal(4, "RdBu")[4:1]}

# get values for a line to show which are the 'target' taxa
selector<-target.values[[input$dataset]]
selector<-selector[which(selector$target==input$target), ]

if(input$target!="allSpecies"){
full.circle<-make.circle(100)[, -1]
points.initial<-make.circle(attr(congruence.thisrun, "Size"))[, -1]
segment.ends<-as.data.frame(rbind(points.initial[selector$start, ], 
	points.initial[(selector$start+selector$n-1),]))
point.selector<-as.numeric(apply(segment.ends, 1, FUN=function(x, comparison){
	x<-as.numeric(x)
	dist<-sqrt((comparison$x-x[1])^2 + (comparison$y-x[2])^2)
	which.min(dist)
	}, comparison=full.circle))
points.final<-c((min(point.selector)-3):(max(point.selector)+3))
	if(any(points.final<=0))points.final<-points.final[which(points.final>0)]
	if(any(points.final>100))points.final<-points.final[which(points.final<=100)]
segment.final<-full.circle[points.final, ]
}

# get labels, such that 'selected' taxa are larger and in bold
taxon.selector<-surrogacy.thisrun[nrow(surrogacy.thisrun), -c(1:5)]
colnames(taxon.selector)<-capitalize(colnames(taxon.selector))
point.labels<-data.frame(labels=colnames(taxon.selector),
	col="grey", cex=1, stringsAsFactors=FALSE)
point.labels$col[which(taxon.selector ==1)]<-"black"
point.labels$cex[which(taxon.selector ==1)]<-1.5

# work out density
density.result<-density(log10(congruence.thisrun), bw=0.1, n=100, from=-3, to=0)
# add polygons for plotting on histogram
row.nums<-as.numeric(sapply(line.breaks, 
	FUN=function(x){which.min(sqrt(((10^density.result$x)-x)^2))}))
polygon.list<-list()
for(i in 1:4){
	rows.thisrun<-c(row.nums[i]:row.nums[(i+1)])
	result<-data.frame(x=density.result$x[rows.thisrun], y=density.result$y[rows.thisrun])
	result<-rbind(
		c(min(result$x), 0),
		result,
		c(max(result$x), 0),
		c(min(result$x), 0))
	polygon.list[[i]]<-result}

# set log scale info
x.locs<-c(
	seq(10^-3, 10^-2, 10^-3),
	seq(10^-2, 10^-1, 10^-2),
	seq(10^-1, 10^0, 10^-1))

# set circleplot attr
point.attr<-point.labels; point.attr$cex<-0
point.attr$order<-c(1:nrow(point.attr))

plot.attr<-list(
	plot=list(xlim=c(-1.75, 1.75), ylim=c(-1.75, 1.75)),
	points= point.attr,
	point.labels=point.labels,
	line.breaks=line.breaks, 
	line.cols=line.cols,
	line.widths=seq(1, 2, length.out=4)
	)

# divide and run plot
screen.matrix<-matrix(data=c(
	0, 0.75, 0, 1, # circleplot
	0.75, 1, 0.5, 1, # histogram
	0.75, 1, 0, 0.5), # surrogacy  
	nrow=3, ncol=4, byrow=TRUE)
split.screen(screen.matrix)

screen(1) # circleplot
circleplot(congruence.thisrun, plot.control=plot.attr, style="clock")
edge.locs<-make.circle(n=300)[, -1]
edge.locs<-rbind(edge.locs, edge.locs[1, ])
if(input$target=="allSpecies"){lines(edge.locs, lwd=2, col="black")
}else{
	lines(edge.locs, lwd=2, col="grey")
	lines(segment.final, lwd=3, col="black") }

screen(2) # histogram
par(mar=c(4, 4, 1, 1))
plot(1~1, type="n", ann=FALSE, axes=FALSE, xlim=c(-3, 0), ylim=c(0, max(density.result$y)))
	axis(1, at=log10(x.locs), labels=rep("", length(x.locs)))
	axis(1, at=seq(-3, 0, 1), labels=c(0.001, 0.01, 0.1, 1), lwd=0)
	axis(2, las=1); box(bty="l")
	title(xlab="Expected R-squared", ylab="Density", line=2.5)
	for(i in 1:4){polygon(x=polygon.list[[i]]$x, y=polygon.list[[i]]$y, col=line.cols[i])}
	lines(density.result$x, density.result$y, col= "black", lwd=2)

screen(3) # surrogacy value
par(mar=c(4, 4, 1, 1))
plot(1~1, type="n", ann=FALSE, axes=FALSE, xlim=c(0, selector$n), ylim=c(0, selector$n))
	axis(1); axis(2, las=1); box(bty="l")
	title(xlab="Number of taxa", ylab="Cumulative surrogacy value", line=2.5)
	# draw 'null' region
	polygon(x=c(0, selector$n, selector$n, 0), y=c(0, 0, selector$n, 0), col="grey", border=NA)
	# draw 'added' region
	max.x<-min(selector$n, max(surrogacy.thisrun$budget))
	polygon(x=c(0, c(1: max.x), max.x, 0),
		y=c(0, surrogacy.thisrun$value[c(1: max.x)], max.x, 0), col= line.cols[4])
	# abline(a=0, b=1, col="grey40")
	lines(x=c(0, surrogacy.thisrun$budget)[c(1: max.x)], 
		y=c(0, surrogacy.thisrun$value)[c(1: max.x)], col= "black", lwd=2)
	lines(x=rep(max.x, 2), y=c(0, max(surrogacy.thisrun$value)), lty=2)
	points(x= max.x, y=surrogacy.thisrun$value[length(rows)], 
		pch=19, cex=2, col= "black")

close.screen(all=TRUE)


  })
})

