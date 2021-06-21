#' Heat map function
#'
#' Bespoke function for Quan Seminar talk.
#'
#' **Notes**: Probably too customized and prelimanary to keep.
#'
#' @param n_K Number of K cluster to use
#' @param X_norm The normed centers as a matrix, each row is a centroid image
#' @param datalist The list with dat, dat.clean and pos.loc.
#' @param main.n The number of main clusters. These will have different colors.
#' @param sub.n The number of sub levels. This will have different shades.
#' @param iter.max Passed to the kmeans function.
#' @param nstart Passed to the kmeans function.
#' @param plotit If TRUE, plot the heat map.
#'
#' @return A list with two types of objects: a heatmap and a dendogram.
#'
#' @export
kheatmap <- function(n_K, X_norm, datalist, main.n=5, sub.n=n_K, iter.max=25,
                     nstart=100, plotit=TRUE,
                     ramp="var", ramp.type=2, dend.type="original"){
  set.seed(1221966)
  out_kmeans <- stats::kmeans(X_norm, n_K, iter.max=iter.max, nstart=nstart)
  img.list <- imgVectortoRaster(out_kmeans$centers, datalist)$list
  CC <- out_kmeans$centers
  labs <- paste0("C", 1:n_K)
  rownames(CC) <- labs
  pal <- colorRamps::matlab.like(100)
  dend <- dendIMG(CC, img.list, type=dend.type, pal=pal, scale.same=TRUE, lab.extra="none")
  dendextend::rect.dendrogram( dend, k=main.n, lty = 5, lwd = 0, col=rgb(0.1, 0.2, 0.4, 0.1) )
  if(sub.n < n_K) dendextend::rect.dendrogram(dend, sub.n, border = 2)
  ph <- myheatmap(dend, out_kmeans, main.n=main.n, sub.n=sub.n, ramp=ramp, ramp.type=ramp.type, plotit=plotit)
  return(list(heatmap=ph, dend=dend))
}
