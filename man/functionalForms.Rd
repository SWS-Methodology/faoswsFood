% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functionalForms.R
\name{linear}
\alias{funcforms}
\alias{functionalForms}
\alias{linear}
\alias{logInverse}
\alias{logLog}
\alias{semiLog}
\title{Functional forms}
\usage{
linear(food_t0, elas, gdp_pc_t0, gdp_pc_t1, pop_t0, pop_t1)

logLog(food_t0, elas, gdp_pc_t0, gdp_pc_t1, pop_t0, pop_t1)

semiLog(food_t0, elas, gdp_pc_t0, gdp_pc_t1, pop_t0, pop_t1)

logInverse(food_t0, elas, gdp_pc_t0, gdp_pc_t1, pop_t0, pop_t1)
}
\arguments{
\item{food_t0}{The food consumption at time t.}

\item{elas}{The elasticity of the commodity.  This will vary by country and 
commodity.}

\item{gdp_pc_t0}{Per person GDP (gross domestic product) at time t.}

\item{gdp_pc_t1}{Per person GDP (gross domestic product) at time t+1.}

\item{pop_t0}{Population at time t.}

\item{pop_t1}{Population at time t+1.}
}
\value{
An estimate for the food consumption in year t1.
}
\description{
These four different functions provide estimates for the food
  consumption values in year t+1 given the consumption in year t, changes in
  income, and the elasticity of the particular commodity.
}
\keyword{internal}

