#' Draws ecospace with species response surface for two dimensional data simulated and sampled by simul.comm and sample.comm.
#' @author David Zeleny (zeleny.david@@gmail.com)
#' @param saco Object with simulated community data created by \code{sample.comm} function.
#' @param plot.grad List gradients which should be plotted by the function.
#' @param resolution Number of items drawn vertically and horizontally. Default = 200.
#' @param colors Vector of hexadecimal strings of colors used to draw individual species response surfaces. Each element should be 7 characters long, starting with \code{"#"} (e.g. \code{"#8DD3C7"}). Default is a vector of 12 colors created by pallet "Set3" from \code{library (RColorBrewer)}.
#' @param species Vector with ID's of species drawn into plot. Default = NULL, which means all species are drawn.
#' @param asp Aspect ratio of the plotted region (see argument \code{asp} in \code{\link{plot.window}}).
#' @param sample.pch,sample.cex Plotting character and its size for points showing location of samples in ecospace. See \code{pch} and \code{cex} in function \code{\link{points}}.
#' @param species.pch,species.cex Plotting character and its size for points forming the species response surface in the ecospace. See \code{pch} and \code{cex} in function \code{\link{points}}.
#' @param xlab,ylab x- and y-axis labels.
#' @param box Logical; draw the box around plotting region? Default is TRUE.
#' @param axes Logical; draw axes? Default is TRUE.
#' @param ... Other arguments passed into the function \code{title}.
#' @importFrom graphics lines par points
#' @rdname draw.ecospace
#' @export
draw.ecospace <- function (saco, plot.grad = c(1,2), resolution = 200, colors = NULL, species = NULL, asp = NULL, sample.pch = 3, sample.cex = .5, species.pch = 16, species.cex = 1, xlab = 'gradient 1', ylab = 'gradient 2', axes = TRUE, box = TRUE, ...)
{
  
  if (is.null (colors)) colors <- RColorBrewer::brewer.pal (12, 'Set3')
  if (is.null (species)) species <- 1:saco$args.simcom$S
  if (length (plot.grad) != 2) stop ('Two gradients need to be provided.')
  
  graphics::plot.new ()
  graphics::plot.window (xlim = c(0, resolution), ylim = c(0, resolution), asp = asp)
  graphics::title (xlab = xlab, ylab = ylab) 
  
  gr1 <- round (seq (1, saco$args.simcom$gr.length[plot.grad[1]], length = resolution))
  gr2 <- round (seq (1, saco$args.simcom$gr.length[plot.grad[2]], length = resolution))
#  spec.prob <- lapply (species, FUN = function (sp) saco$args.simcom$Ao[sp]*outer (saco$args.simcom$A.all[[1]][gr1, sp], saco$args.simcom$A.all[[2]][gr2, sp], `*`))
  
  arr <- list ()
  arr_1 <- array (1, dim = c(resolution, resolution)) #c(max (gr1), max (gr2)))
  for (L in species){
    arr[[L]] <- arr_1
    arr[[L]] <- sweep (arr[[L]], 1, saco$args.simcom$A.all[[plot.grad[1]]][,L][gr1], `*`)
    arr[[L]] <- sweep (arr[[L]], 2, saco$args.simcom$A.all[[plot.grad[2]]][,L][gr2], `*`)
    }
  names (arr) <- species
  spec.prob <- arr

  sorted.species <- order (saco$args.simcom$niche[species], decreasing = TRUE)
  colors.all <- rep (colors, length.out = length (species))


  for (sp in sorted.species)
  {
    if (sum (spec.prob[[sp]] > 0)>0)
    {
      lower.bound <- stats::quantile (spec.prob[[sp]][spec.prob[[sp]]>0], prob = .5)
      points.coords <- which (spec.prob[[sp]] > lower.bound, arr.ind = TRUE)
      species.prob <- spec.prob[[sp]][spec.prob[[sp]] > lower.bound]
      species.prob.ranged <- (species.prob - min (species.prob))/(max (species.prob) - min (species.prob))*255
      colors.temp <- paste (colors.all[sp], format (as.hexmode (round (species.prob.ranged)), upper = T), sep = '')
      graphics::points (points.coords, col = colors.temp, pch = species.pch, cex = species.cex)
    }
  }
  graphics::plot.window (xlim = c(0, saco$args.simcom$gr.length[plot.grad[1]]), ylim = c(0, saco$args.simcom$gr.length[plot.grad[2]]), asp = asp)
  points (as.data.frame (saco$sample.x), pch = sample.pch, cex = sample.cex)
  if (axes) graphics::axis (1)
  if (axes) graphics::axis (2)
  if (box) graphics::box ()
}
