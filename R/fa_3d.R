#' A 3D plot function for Factorial Analyses result
#'
#' This function allows you to plot three dimensions of a Factorial Analyses
#' result (rows and columns).
#' @param x an object of class CA, MCA or PCA.
#' @param dim the three dimensions to plot.
#' @param grps list of rownames of different group(s).
#' @param colors colors of different group(s).
#' @param alpha transparence for the PCA variables sphere.
#' @return Nothing. Used for side-effects.
#' @keywords 3D
#' @export
#' @examples
#' library(FactoMineR)
#' data(decathlon)
#' res.pca = PCA(decathlon[, 1:10], scale.unit = TRUE, ncp = 5, graph = TRUE)
#' plot_fa_3d(res.pca)
plot_fa_3d <- function(x, dim = 1:3, grps = NULL, colors = rainbow(length(grps)), alpha = 0.1) {
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
    my.coord.row <- x$ind$coord
    my.coord.col <- x$var$coord
    my.main.row <- "inds"
    my.main.col <- "vars"
  } else if (inherits(x, "CA")) {
    my.coord.row <- x$row$coord
    my.coord.col <- x$col$coord
    my.main.row <- "rows"
    my.main.col <- "cols"
  } else {
    stop("Analysis not supported.")
  }

  if (is.null(grps)) {
    grps <- list(row.names(my.coord.row))
    colors <- rainbow(1)
  }

  if (any(ncol(my.coord.row) < dim)) {
    stop("Undefined dimension selected.")
  }

  rgl::open3d()
  my.x.dim <- dim[1]
  my.y.dim <- dim[2]
  my.z.dim <- dim[3]
  my.x.lim <- c(min(my.coord.row[, my.x.dim]), max(my.coord.row[, my.x.dim]))
  my.y.lim <- c(min(my.coord.row[, my.y.dim]), max(my.coord.row[, my.y.dim]))
  my.z.lim <- c(min(my.coord.row[, my.z.dim]), max(my.coord.row[, my.z.dim]))

  # plot rows
  for (i in 1:length(grps)) {
    if (i == 1) {
      rgl::plot3d(x = my.coord.row[grps[[i]], my.x.dim],
                  y = my.coord.row[grps[[i]], my.y.dim],
                  z = my.coord.row[grps[[i]], my.z.dim],
                  xlab = colnames(my.coord.row)[my.x.dim],
                  ylab = colnames(my.coord.row)[my.y.dim],
                  zlab = colnames(my.coord.row)[my.z.dim],
                  xlim = my.x.lim, ylim = my.y.lim, zlim = my.z.lim,
                  col = colors[i],
                  main = my.main.row)
    } else {
      rgl::points3d(my.coord.row[grps[[i]], my.x.dim],
                    my.coord.row[grps[[i]], my.y.dim],
                    my.coord.row[grps[[i]], my.z.dim],
                    col = colors[i])
    }
  }

  # plot cols
  rgl::open3d()
  if (inherits(x, "PCA")) {
    rgl::plot3d(x = my.coord.col[, my.x.dim],
                y = my.coord.col[, my.y.dim],
                z = my.coord.col[, my.z.dim],
                xlab = colnames(my.coord.col)[my.x.dim],
                ylab = colnames(my.coord.col)[my.y.dim],
                zlab = colnames(my.coord.col)[my.z.dim],
                xlim = c(-1, 1), ylim = c(-1, 1), zlim = c(-1, 1),
                main = my.main.col)
    rgl::spheres3d(x = 0, y = 0, z = 0, radius = 1, color = "grey",
                   alpha = alpha, smooth = TRUE)
  } else {
    rgl::plot3d(x = my.coord.col[, my.x.dim],
                y = my.coord.col[, my.y.dim],
                z = my.coord.col[, my.z.dim],
                xlab = colnames(my.coord.col)[my.x.dim],
                ylab = colnames(my.coord.col)[my.y.dim],
                zlab = colnames(my.coord.col)[my.z.dim],
                main = my.main.col)
  }
  rgl::text3d(x = my.coord.col[, my.x.dim],
              y = my.coord.col[, my.y.dim],
              z = my.coord.col[, my.z.dim],
              rownames(my.coord.col))
}
