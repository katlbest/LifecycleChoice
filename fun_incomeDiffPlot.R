incomeDiffPlot<- function(completeData, listByAttend, coeffList){

  #fetch data
    coeffListCopy = coeffList
    for (i in 1:length(coeffList)){ #this removes null entries, which is not relevant since we don't have any
      if (is.na(coeffList[[i]])[1]){
        coeffListCopy[[i]]= NULL
      } 
    }
    coeffList = coeffListCopy
    stackedData = ldply(coeffList)
  
  #ribbon plot
    #ggplot(outData, aes(x = attend2, y = mean, group = admit, colour=admit))+ geom_ribbon(aes(ymin=lb, ymax=ub, fill =admit),alpha=0.3)+geom_line(aes(y=mean))+ theme_bw()
    #ggplot(outData, aes(x = attend2, y = mean, group = admit, colour=admit))+geom_line(aes(y=mean)+ theme_bw() + scale_fill_brewer(palette = "Set1")

  #stacked bar graph with error bars
  
  #individual bar graphs with error bars
    #ggplot(myDat, aes(x=attend2, y=-mean)) + geom_bar(stat = "identity", aes(fill = attend2))+ geom_errorbar(aes(ymin=lb, ymax=ub), width=.1) + scale_colour_brewer(palette = "Set1") + theme_bw()
    #ggplot(outData, aes(x=admit, y=mean, group=attend2)) + geom_bar(stat = "identity", position = position_dodge())+ geom_errorbar(aes(ymin=lb, ymax=ub), width=.1, position=position_dodge())+theme_bw() +scale_color_brewer(palette = "Set3")                                                                     
}

#notes:
  #colour is continuous, fill is discrete
  #proposed stacked bar plot example, see http://stackoverflow.com/questions/10417003/stacked-barplot-with-errorbars-using-ggplot2
    #df <- data.frame(substrate = gl(6, 2, 12, labels=letters[1:6]),
    #depth = gl(2, 1, 12, labels=c("surf", "deep")),
    #mean = 10 * runif(12),
    #err = runif(12))
    #df <- ddply(df,.(depth),transform,ystart = cumsum(mean),yend = cumsum(mean) + err)
    #p <- ggplot(df, aes(x=depth, y=mean, fill=substrate)) + 
    #geom_bar(stat="identity")
    #p + geom_segment(aes(xend = depth,y = ystart,yend = yend)) + 
    #geom_point(aes(x = depth,y = yend),shape = "|",show_guide = FALSE) +
    #coord_flip()

                                                                                   