% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/directory_utils.R
\name{dirs.r_home}
\alias{dirs.r_home}
\title{Home directory for R folders}
\usage{
dirs.r_home(package = NULL)
}
\arguments{
\item{package}{string the name of a package in the local directory

dirs.r_home('whyles')%>%list.files}
}
\description{
\code{dirs.r_home}
}
\details{
Provides the systems path to the home R directory, or if package
passed as an arg, will append and return a valid path to the dir.
}

