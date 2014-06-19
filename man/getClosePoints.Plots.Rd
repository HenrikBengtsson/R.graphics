%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  Plots.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{getClosePoints.Plots}
\alias{getClosePoints.Plots}
\alias{Plots.getClosePoints}
\alias{getClosePoints.Plots}
\alias{getClosePoints,Plots-method}


\title{Indentifies data points that are \"close\"}

\description{
  Indentifies data points that are "close". This could be used to exclude
  high density areas in scatter plots etc where the points are "on top
  of each other". This becomes a speed/memory issue plots that are
  printed as vector graphics, e.g. eps.
}

\synopsis{getClosePoints.Plots(this, points, nclass=5, threshold=NULL, ...)}
\usage{Plots$getClosePoints(points, nclass=5, threshold=NULL, ...)}

\arguments{
  \item{points}{-}
  \item{nclass}{-}
  \item{threshold}{-}
  \item{...}{Not used.}
}

\value{Returns nothing.}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}
\keyword{classes}
\keyword{internal}
\keyword{methods}
