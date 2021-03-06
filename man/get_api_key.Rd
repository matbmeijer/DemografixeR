% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api_structure.R
\name{get_api_key}
\alias{get_api_key}
\title{Get previously saved API keys}
\usage{
get_api_key()
}
\value{
Returns the saved API key in the environment. If no API key has been
  saved, returns \code{NULL} value.
}
\description{
Function to get the previously saved API key. It has the
  advantage that it is not necessary to explicitly publish the key in the
  users code. Just do it one time and you're set. To save an API, use the
  \code{\link{save_api_key}} function. To remove a previously saved key, use
  the \code{\link{remove_api_key}} function.
}
\section{Warning}{
 Please be careful when dealing with API keys and other
  secrets & tokens - keep them private and do not publish them.
}

\examples{
\dontrun{
get_api_key(type="genderize")
get_api_key(type="agify")
get_api_key(type="nationalize")
}
}
\seealso{
\code{\link{save_api_key}}, \code{\link{remove_api_key}}
}
\author{
Matthias Brenninkmeijer -
  \href{https://github.com/matbmeijer}{https://github.com/matbmeijer}
}
