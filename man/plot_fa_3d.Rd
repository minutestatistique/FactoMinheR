% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/fa_3d.R
\name{plot_fa_3d}
\alias{plot_fa_3d}
\title{A 3D plot function for Factorial Analyses result}
\usage{
plot_fa_3d(x, dim = 1:3, grps = NULL, colors = rainbow(length(grps)),
  alpha = 0.1)
}
\arguments{
\item{x}{an object of class CA, MCA or PCA.}

\item{dim}{the three dimensions to plot.}

\item{grps}{list of rownames of different group(s).}

\item{colors}{colors of different group(s).}

\item{alpha}{sphere transparence for the PCA variables plot.}
}
\value{
Nothing. Used for side-effects.
}
\description{
This function allows you to plot three dimensions of a Factorial Analyses
result (rows and columns).
}
\examples{
library(FactoMineR)
data(decathlon)
res.pca = PCA(decathlon[, 1:10], scale.unit = TRUE, ncp = 5, graph = TRUE)
plot_fa_3d(res.pca)
}
\keyword{3D}

