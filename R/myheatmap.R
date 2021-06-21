#' Plot the heat map
#'
#' Another bespoke function for Quan Seminar talk.
#'
#' **Notes**: Probably too customized and prelimanary to keep.
#'
#' @param dend Dendogram object
#' @param kobject Object out of `kmeans()`
#' @param main.n The number of main clusters. These will have different colors.
#' @param sub.n The number of sub levels. This will have different shades.
#' @param ramp How to create the shading.
#' @param ramp.type Type of ramping of the shades.
#' @param plotit If TRUE, plot the heat map.
#'
#' @return A ggplot object.
#'
#' @export
myheatmap <- function(dend, kobject, main.n=3, sub.n=7, ramp=c("mean", "var"),
                      ramp.type=1, plotit=TRUE){
  cluster <- kobject$cluster
  img.group <- kobject$cluster
  CC <- kobject$centers
  # library(ggplot2)
  df <- data.frame(date=as.Date(names(cluster)), cluster=as.factor(cluster))
  df$month <- factor(months(df$date), levels=month.name[1:12])
  df$year <- as.numeric(format(df$date,'%Y'))

  dend.group <- stats::cutree(dend, k=sub.n)
  for(i in 1:length(dend.group)) img.group[img.group==i] <- dend.group[i]
  df$sub.group <- as.factor(img.group)

  # Set up my palette; ramp within the main group
  dend.group.main <- cutree(dend, k=main.n)
  pal.sub <- rep(NA, attr(dend, "members"))
  pals <- c("YlOrRd", "Greys", "Greens", "BuPu", "Blues")
  pals[main.n] <- "Blues"
  temps <- apply(CC,1,get(ramp))
  main.temps <- tapply(temps, dend.group.main, mean)
  tmp.main <- sort(main.temps, index.return=TRUE, decreasing=TRUE)$ix
  if(ramp.type==1){
    for(i in 1:main.n){
      main.group <- dend.group.main==tmp.main[i]
      sub.group <- unique(dend.group[main.group])
      cols <- rev(RColorBrewer::brewer.pal(max(3, length(sub.group)+2), pals[i]))
      sub.temps <- tapply(temps[main.group], dend.group[main.group], mean)
      tmp <- sort(sub.temps, index.return=TRUE, decreasing=TRUE)$ix
      for(j in 1:length(sub.group))
        pal.sub[dend.group==sub.group[tmp[j]]] <- cols[j]
    }
  }else{
    pal.sub <- rep(NA, main.n)
    main.temps <- tapply(apply(CC,1,mean), dend.group.main, mean)
    tmp.main <- sort(main.temps, index.return=TRUE, decreasing=TRUE)$ix
    for(i in 1:main.n){
      pal.sub[tmp.main[i]] <- rev(RColorBrewer::brewer.pal(3, pals[i]))[1]
    }
    pal.sub <- pal.sub[dend.group.main]
    temp.scaled <- 0.2+(temps-min(temps))/((max(temps)-min(temps))*1.25)
    for(i in 1:length(pal.sub)) pal.sub[i] <- desat(pal.sub[i], sat=temp.scaled[i])
  }

  # Plot
  p1 <- ggplot2::ggplot(df, ggplot2::aes(x=year, y=month, fill= cluster)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_manual(values=pal.sub)
  if(plotit) ggplot2::plot(p1)
  return(p1)
}
