# Solving ODE systems modeling different epidemiological dynamics,
# plotting the result and embedding it into a shiny app.
#
# Author: Jan Engelstaedter
#
# Last date changed: 20/04/2020


ODEsSImodel <- function (times, state, parameters) {
  with (as.list(c(state, parameters)), {
    dS<- nu - beta*S*I - mu*S
    dI<- beta*S*I - alpha*I - mu*I
    return(list(c(dS, dI)))
  })
}

ODEsSISmodel <- function (times, state, parameters) {
  with (as.list(c(state, parameters)), {
    dS<- nu-beta*S*I + gamma*I - mu*S
    dI<- beta*S*I - alpha*I-gamma*I - mu*I
    return(list(c(dS, dI)))
  })
}

ODEsSIRmodel <- function (times, state, parameters) {
  with (as.list(c(state, parameters)), {
    dS<- nu-beta*S*I -mu*S
    dI<- beta*S*I - alpha*I-gamma*I - mu*I
    dR<- gamma*I - mu*R
    return(list(c(dS, dI, dR)))
  })
}

plotEpidemiologyDynamics<-function(model="SI", nu=10, mu=0.01, alpha=0.2, beta=0.001, gamma=0.2, tmax=200, S0=1000, I0=1){

  times<-seq(0,tmax,by=0.1)

  if (model=="SI"){
    parameters<-c(nu=nu, mu=mu, alpha=alpha, beta=beta)
    iniState<-c(S=S0, I=I0)
    solution<-deSolve::ode(iniState,times,ODEsSImodel,parameters)
    plot(x=solution[,"time"], y=solution[,"S"], xlab="Time", ylab="Number of individuals", type="l", ylim=c(0,max(S0+I0,nu/mu)))
    lines(x=solution[,"time"], y=solution[,"I"], col="red")
    legend("topright",c("S","I"),col=c("black","red"),lwd=1)
  } else if (model=="SIS") {
    parameters<-c(nu=nu, mu=mu, alpha=alpha, beta=beta, gamma=gamma)
    iniState<-c(S=S0, I=I0)
    solution<-deSolve::ode(iniState,times,ODEsSISmodel,parameters)
    plot(x=solution[,"time"], y=solution[,"S"], xlab="Time", ylab="Number of individuals", type="l", ylim=c(0,max(S0+I0,nu/mu)))
    lines(x=solution[,"time"], y=solution[,"I"], col="red")
    legend("topright",c("S","I"),col=c("black","red"),lwd=1)
  } else if (model=="SIR") {
    parameters<-c(nu=nu, mu=mu, alpha=alpha, beta=beta, gamma=gamma)
    iniState<-c(S=S0, I=I0, R=0)
    solution<-deSolve::ode(iniState,times,ODEsSIRmodel,parameters)
    plot(x=solution[,"time"], y=solution[,"S"], xlab="Time", ylab="Number of individuals", type="l", ylim=c(0,max(S0+I0,nu/mu)))
    lines(x=solution[,"time"], y=solution[,"I"], col="red")
    lines(x=solution[,"time"], y=solution[,"R"], col="blue")
    legend("topright",c("S","I","R"),col=c("black","red","blue"),lwd=1)
  }
}



appEpidemiologyDynamics <- shinyApp(

  ui<-fluidPage(
    titlePanel("Epidemiological ODE models"),
    h4("The plot shows numerical solutions of one of three ODE systems describing epidemiological dynamics: the SI model, the SIS model and the SIR model."),
    hr(),
    sidebarLayout(
      sidebarPanel(
        radioButtons(inputId="model", label="Type of epidemiological model:",choices=c("SI","SIS","SIR"), inline=TRUE),
        sliderInput(inputId = "nu",label = "Immigration rate nu:", min = 0, max = 100, value = 10, step = 1),
        sliderInput(inputId = "mu", label = "Baseline mortality mu:", min = 0, max = 0.1, value = 0.01, step = 0.001),
        sliderInput(inputId = "beta", label = "Transmission rate beta:", min = 0, max = 0.01, value = 0.001, step = 0.0001),
        sliderInput(inputId = "alpha", label = "Disease-induced mortality alpha:", min = 0, max = 0.5, value = 0.2, step = 0.01),
        conditionalPanel(condition = "input.model != 'SI'",
          sliderInput(inputId = "gamma", label = "Recovery rate gamma:",min = 0, max = 1, value = 0.2, step = 0.01)),
        selectInput(inputId = "tmax", label="Time to be simulated", choices = c(10,20,50,100,200,500,1000), selected=100)
      ),
      mainPanel(plotOutput(outputId = "main_plot", height = "600px"))
    )
  ),

  server<-function(input, output) {
    output$main_plot <- renderPlot({
      plotEpidemiologyDynamics(model = input$model,
                               nu = input$nu,
                               mu = input$mu,
                               beta = input$beta,
                              alpha = input$alpha,
                               gamma = input$gamma,
                               tmax = as.integer(input$tmax))
    })
  }


)
