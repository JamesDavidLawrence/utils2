\name{parseAddress}
\alias{parseAddress}

\title{
Extract components of an address
}
\description{
\code{parseAddress} attempts to extract the flat number, house number, house name and street from an address string.
}
\usage{
parseAddress(x)
}
\arguments{
	\item{x}{
Character vector of raw address strings to process.
}
}

\details{
The algorithm used by \code{parseAddress} is likely to change as the package evolves, but currently the process is as follows:\cr
First, if any string begins with "Flat <number>", "Room <number>" or "Apartment <number>" (including the abbreviations "Apt" and "Apt."), this is deemed to be the flat number, and is assigned as such and stripped from the front.\cr
Then, if there is a number remaining in an element, the format is deemed to be "<house name> <number> <street>".\cr
If there is no number but either a comma or the word "house" or "cottage", then the comma or word (and optional space after) is deemed to separate the house name and street, and the number is unknown.\cr
Lastly if there is no other clue as to where the house name ends and the street begins, the last two words are deemed to be the street and anything before that is the house name.
}
\value{
A list with four components, \code{flatNumber}, \code{Number}, \code{houseName} and \code{street} containing the extracted components of \code{x}.
}

\author{
James Lawrence
}

\note{
\code{parseAddress} is case-insensitive for matching purposes, but the output will have the same case as the input.\cr
"Number"s may not be coercible to actual numeric vectors as they may contain non-numeric characters, for example "221b" or "1-6" (or they may be absent entirely, a NA value).
}

\seealso{
\link{regex} for details on regular expression matching and substitution, which underpins this function.
}

\examples{
## these work well
x1 <- c(
	"RSA Building, St. Mark's Court",
	"RSA Building 1 St. Mark's Court",
	"RSA Building Fenchurch Street",
	"Flat 2 221b Baker Street",
	"221b Baker Street"
)
## these don't work so well
x2 <- c(
	"RSA Building St. Mark's Court",
	"15-18 The Walkie Talkie 30 Fenchurch Street"
)
parseAddress(x1)
parseAddress(x2)
}
