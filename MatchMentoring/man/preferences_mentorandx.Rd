% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/preferences_mentorandx.R
\name{preferences_mentorandx}
\alias{preferences_mentorandx}
\title{Assign probabilities based on the preferences of the mentorandx}
\usage{
preferences_mentorandx(
  info_mentorando,
  data_mentxr = data_mentxr,
  p = c(0.1, 0.35, 0.35, 0.2)
)
}
\arguments{
\item{info_mentorando}{a vector containing four elements: email, nodo, sexo_mentor1, exp_mentor1.}

\item{data_mentxr}{data with all the information of the mentores}

\item{p}{vector of probabilities for each mentorandxs info}
}
\value{
a probabilities vector for each mentxr of the dataset, based on the preferences of the mentorandx
}
\description{
`preferences_mentorandx` Main function to assign probabilities based on the preferences of the mentorandx
}
\author{
Joaquín Martínez-Minaya <\email{jomartinez@bcamath.org}>
}
