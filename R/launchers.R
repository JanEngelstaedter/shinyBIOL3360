
#' Haploid selection model
#'
#' Launches a shiny app that allows the user to change the parameters of a model of natural selection in a haploid populatiom, and observe the resulting dynamics.
#'
#' @param launch.browser If this is FALSE (the default option), the shiny app will be launched using default settings (usually a new window created by RStudio). If set to TRUE, the app will be opened in the system's default web browser.
#'
#' @details The recurrence equation of this model is \eqn{p'=(1+s)p/(1+sp)}, where p is the frequency of the focal allele (with 1-p the frequency of the other allele), and s is the selection coefficient.
#'
#' @import shiny
#' @export
#'
haploidSelection <- function(launchBrowser = FALSE) {
  if (launchBrowser) runApp(appHaploidSelection, launch.browser = TRUE)
  else runApp(appHaploidSelection)
}


#' The graphical method
#'
#' Launches a shiny app that illustrates the graphical method for understanding recurrence equation models, using the logistic model for population growth as an example.
#'
#' @param launch.browser If this is FALSE (the default option), the shiny app will be launched using default settings (usually a new window created by RStudio). If set to TRUE, the app will be opened in the system's default web browser.
#'
#' @details The recurrence equation of this model is \eqn{x'=bx(1-x)}, where \eqn{x} is the relative population size and \eqn{b} is the number of offspring produced in the absence of any competition.
#'
#' @import shiny
#' @export
#'
graphicalMethod <- function(launchBrowser = FALSE) {
  if (launchBrowser) runApp(appGraphicalMethod, launch.browser = TRUE)
  else runApp(appGraphicalMethod)
}


#' Allee effect model
#'
#' Launches a shiny app that allows the user to change the parameters of an allee effect model
#' of population growth and observe the resulting dynamics.
#'
#' @param launch.browser If this is FALSE (the default option), the shiny app will be launched using default settings (usually a new window created by RStudio). If set to TRUE, the app will be opened in the system's default web browser.
#'
#' @details The ordinary differential equation for this model is \eqn{dN/dt = rN(1-N/K)(N/T-1)},
#' where \eqn{N} is the population size, \eqn{K} the carrying capacity,
#'  and \eqn{T} the threshold population size below which the population goes extinct.
#'
#' @import shiny
#' @export
#'
alleeEffectDynamics <- function(launchBrowser = FALSE) {
  if (launchBrowser) runApp(appAlleeEffectDynamics, launch.browser = TRUE)
  else runApp(appAlleeEffectDynamics)
}


#' Epidemiological ODE models
#'
#' Launches a shiny app that interatively plots the dynamics of three commonly used epidemiological models: the SI, SIS and SIR model.
#'
#' @param launch.browser If this is FALSE (the default option), the shiny app will be launched using default settings (usually a new window created by RStudio). If set to TRUE, the app will be opened in the system's default web browser.
#'
#' @details The differential equation systems for the three models are as follows:
#'
#' SI model: \eqn{dS/dt = \nu - \betaIS - \muS, dI/dt = \betaIS - (\mu + \alpha)I}
#'
#' SIS model: \eqn{dS/dt = \nu - \betaIS - \muS + \gammaI, dI/dt = \betaIS - (\mu + \alpha - \gamma)I}
#'
#' SIR model: \eqn{dS/dt = \betaIS, dI/dt = \betaIS - \gammaI, dR/dt = \gammaI}
#'
#' @import shiny
#' @export
#'
epidemiologyDynamics <- function(launchBrowser = FALSE) {
  if (launchBrowser) runApp(appEpidemiologyDynamics, launch.browser = TRUE)
  else runApp(appEpidemiologyDynamics)
}


#' Poisson process
#'
#' Launches a shiny app illustrating the Poisson process for different rates.
#'
#' @param launch.browser If this is FALSE (the default option), the shiny app will be launched using default settings (usually a new window created by RStudio). If set to TRUE, the app will be opened in the system's default web browser.
#'
#' @details The Poisson process takes place over a fixed time period of 100 units.
#'
#' @import shiny
#' @export
#'
poissonProcess <- function(launchBrowser = FALSE) {
  if (launchBrowser) runApp(appPoissonProcess, launch.browser = TRUE)
  else runApp(appPoissonProcess)
}

