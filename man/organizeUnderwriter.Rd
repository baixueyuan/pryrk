% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/organizeUnderwriter.R
\name{organizeUnderwriter}
\alias{organizeUnderwriter}
\title{Cut Underwriter's Name to Short Form}
\usage{
organizeUnderwriter(uw)
}
\arguments{
\item{uw}{the full form of underwriters' names}
}
\value{
The character vector of shor name.
}
\description{
Cut underwriter's name from the full form to short form, all in Chinese
}
\details{
The data read from Wind Client has the full-name of underwriter, it is long
and hard to use or review. This function cut them to short form, usually four
Chinese character, maybe three or five for some special names. This function
can handle with the most underwriter's name. Generally, the securities
company has the short form "XX securities", and bank has the name "XX bank".
And then, some ones have the well-known short name.
}

