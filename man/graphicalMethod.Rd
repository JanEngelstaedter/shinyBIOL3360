% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/launchers.R
\name{graphicalMethod}
\alias{graphicalMethod}
\title{The graphical method}
\usage{
graphicalMethod(launchBrowser = FALSE)
}
\arguments{
\item{launch.browser}{If this is FALSE (the default option), the shiny app will be launched using default settings (usually a new window created by RStudio). If set to TRUE, the app will be opened in the system's default web browser.}
}
\description{
Launches a shiny app that illustrates the graphical method for understanding recurrence equation models, using the logistic model for population growth as an example.
}
\details{
The recurrence equation of this model is \eqn{x'=bx(1-x)}, where \eqn{x} is the relative population size and \eqn{b} is the number of offspring produced in the absence of any competition.
}
