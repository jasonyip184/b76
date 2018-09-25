\name{plotImpliedVol}
\alias{plotImpliedVol}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plots the Volatility Skew given a dataframe of options data}
\description{
  Given a dataframe with the columns \cr
  1) strike # the option strike - in $\cr
  2) type # either c for call option or p for a put option\cr
  3) optionPrice # the option price - in $\cr
  4) futurePrice # the price of the underlying future - in $\cr
  5) time_to_expiry \cr
   \cr
  plotImpliedVol() plots the Implied Volatility across Call & Put options and option time to expiry. \cr
   \cr
  Risk Free Rate r is taken from 3-Month US T-Bills of the specified date argument
}
\usage{
plotImpliedVol(df, date="2019-09-18", mode="B")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
    Data Frame with 5 columns: \cr
      1) strike ("numeric" or "double")\cr
      2) type ("string")\cr
      3) optionPrice ("numeric" or "double")\cr
      4) futurePrice ("numeric" or "double")\cr
      5) time_to_expiry ("numeric" or "double")\cr
  }
  \item{date}{
    Date of option data ("character" in format "yyyy-mm-dd" including quotes)
  }
  \item{mode}{
    Specify plots based on different mode types \cr
      1) "C" plots Implied Volatility of Call options across varying time to expiry\cr
      2) "P" plots Implied Volatility of Put options across varying time to expiry\cr
      3) default - "B" plots Implied Volatility of options across varying time to expiry\cr
      4) "CP" plots Implied Volatility of Call options against Strike options without varying across time to expiry\cr
  }
}
\references{
  https://www.glynholton.com/notes/black_1976/
}
\author{
  Jason Yip
}
\dependencies{
  "ggplot2"\cr
  "quantmod"
}
\examples{
df = data.frame(
      strike = c(50, 23, 48, 24, 52, 20,51,22,49),
      type = c("C","P","C","P","C","P","C","P","C"),
      optionPrice = c(1.62,0.01,1.62,0.01,1.62,0.01,1.5,0.02,1.7),
      futurePrice = c(48.03, 48.03,48.03, 48.03,48.03, 48.03, 48.03, 48.03, 48.03),
      time_to_expiry = c(0.1423, 0.1423,0.1323, 0.1323,0.1223, 0.1223, 0.12, 0.11, 0.15))

plotImpliedVol(df, date="2018-09-18", mode="B")
plotImpliedVol(df, date="2018-09-18", mode="C")
plotImpliedVol(df, date="2018-09-18", mode="P")
plotImpliedVol(df, date="2018-09-18", mode="CP")
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
