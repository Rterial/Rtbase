% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/startup_utils.R
\name{match.names}
\alias{match.names}
\title{Name matching helper}
\usage{
match.names(obj_1, obj_2, inverse = FALSE)
}
\arguments{
\item{obj_1, obj_2}{The data objects to compare}

\item{inverse}{logical Should the negated names be returned or
the matched names? Defaults to FALSE for inclusion}
}
\description{
\code{match.names}
}
\details{
Match the names of one list or data frame
to an altered list or data frame or key
ref table
}
\examples{

# For brevity
names(mtcars)
[1] "mpg"  "cyl"  "disp" "hp"   "drat" "wt"
    "qsec" "vs"   "am"   "gear" "carb"


mtcars2 <- mtcars[1:4]
names(mtcars2)
[1] "mpg"  "cyl"  "disp" "hp"


> match.names(mtcars,mtcars2)
[1] "mpg"  "cyl"  "disp" "hp"
> match.names(mtcars,mtcars2,T)
[1] "drat" "wt"   "qsec" "vs"   "am"   "gear" "carb"

}

