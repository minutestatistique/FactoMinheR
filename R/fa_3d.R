#' A 3D plot function for Factorial Analyses result
#'
#' This function allows you to plot three dimensions of a Factorial Analyses
#' result (rows and columns).
#' @param x an object of class CA, MCA or PCA.
#' @param dim the three dimensions to plot.
#' @param grps list of rownames of different group(s).
#' @param colors colors of different group(s).
#' @return Nothing. Used for side-effects.
#' @keywords 3D
#' @export
#' @examples
#' library(FactoMineR)
#' data(decathlon)
#' res.pca = PCA(decathlon[, 1:10], scale.unit = TRUE, ncp = 5, graph = TRUE)
#' plot_fa_3d(res.pca)
plot_fa_3d <- function(x, dim = 1:3, grps = NULL, colors = rainbow(length(grps))) {
  if (!requireNamespace("rgl", quietly = TRUE)) {
    stop("rgl needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if (length(dim) != 3) {
    stop("The number of dimensions must be 3.")
  }

  if (length(grps) != length(colors)) {
    stop("The groups number is different from the colors length.")
  }

  if (inherits(x, "MCA") || inherits(x, "PCA")) {
    my.coord.ind <- x$ind$coord
    my.coord.var <- x$var$coord
  } else if (inherits(x, "CA")) {
    my.coord.ind <- x$row$coord
    my.coord.var <- x$col$coord
  } else {
    stop("Analysis not supported.")
  }

  if (is.null(grps)) {
    grps <- list(row.names(my.coord.ind))
    colors <- rainbow(1)
  }

  if (any(ncol(my.coord.ind) < dim)) {
    stop("Undefined dimension selected.")
  }

  rgl::open3d()
  my.x.dim <- dim[1]
  my.y.dim <- dim[2]
  my.z.dim <- dim[3]
  my.x.lim <- c(min(my.coord.ind[, my.x.dim]), max(my.coord.ind[, my.x.dim]))
  my.y.lim <- c(min(my.coord.ind[, my.y.dim]), max(my.coord.ind[, my.y.dim]))
  my.z.lim <- c(min(my.coord.ind[, my.z.dim]), max(my.coord.ind[, my.z.dim]))

  # plot rows
  for (i in 1:length(grps)) {
    if (i == 1) {
      rgl::plot3d(x = my.coord.ind[grps[[i]], my.x.dim],
                  y = my.coord.ind[grps[[i]], my.y.dim],
                  z = my.coord.ind[grps[[i]], my.z.dim],
                  xlab = colnames(my.coord.ind)[my.x.dim],
                  ylab = colnames(my.coord.ind)[my.y.dim],
                  zlab = colnames(my.coord.ind)[my.z.dim],
                  xlim = my.x.lim, ylim = my.y.lim, zlim = my.z.lim,
                  col = colors[i],
                  main = "rows")
    } else {
      rgl::points3d(my.coord.ind[grps[[i]], my.x.dim],
                    my.coord.ind[grps[[i]], my.y.dim],
                    my.coord.ind[grps[[i]], my.z.dim],
                    col = colors[i])
    }
  }

  # plot cols
  rgl::open3d()
  rgl::plot3d(x = my.coord.var[, my.x.dim],
              y = my.coord.var[, my.y.dim],
              z = my.coord.var[, my.z.dim],
              xlab = colnames(my.coord.var)[my.x.dim],
              ylab = colnames(my.coord.var)[my.y.dim],
              zlab = colnames(my.coord.var)[my.z.dim],
              main = "cols")
  rgl::text3d(x = my.coord.var[, my.x.dim],
              y = my.coord.var[, my.y.dim],
              z = my.coord.var[, my.z.dim],
              rownames(my.coord.var))
}
