# Solving an ODE model specifying an allee effect model,
# plotting the result and embedding it into a shiny app.
#
# Authors: Nicole Fortuna, Jan Engelstaedter
#
# Last date changed: 20/04/2020

plotAlleeEffectDynamics <- function(r = 0.15, K=2000, TT=400) {
	parameters<-c(r=r, K= K, TT=TT)
	times<-seq(0,200,by=0.1)
	par(mar=c(5,5,0,0), oma=c(0,0,1,1), mgp=c(2.5,1,0), xpd=T)
	plot(x=times, y=rep(NA, length(times)), ylim=c(0,2000), ylab="Population size", xlab="Time", cex.lab=2) # empty 	plot
	iniStates=seq(100, 2000, 100)  # initial pop sizes to be tested

	for (iniN in iniStates){
	  iniState<-c(N=iniN)
  	solution<-deSolve::ode(iniState,times, AlleeEffectODE,parameters)
  	lines(solution)
	}
}

AlleeEffectODE<-function (times, state, parameters) {
  with (as.list(c(state, parameters)), {
    dN<-r*N*(1-N/K)*(N/TT-1)
    return(list(c(dN)))
  })
}

appAlleeEffectDynamics <- shinyApp(
	ui = fluidPage(
		titlePanel("Allee effect model"),
    h4("The plot shows numerical solutions of the ODE model dN/dt = r*N*(1-N/K)*(N/T-1),
       where N is the population size, K the carrying capacity,
       and T the threshold (minimum viable) population size."),
		hr(),
		sidebarLayout(
			sidebarPanel(
				sliderInput("growth", "Growth Rate:",
					min = 0, max = 0.1,
					value = 0.02, step = 0.005),

				sliderInput("capacity", "Carrying Capacity:",
					min = 0, max = 2000,
					value = 1000, step = 100),

				sliderInput("threshold", "Threshold:",
					min = 0, max = 1000,
					value = 300, step = 100)
			),

			mainPanel(
				plotOutput("plot")
			),

			"left"
		)
	),

	server = function(input, output) {
		output$plot <- renderPlot({
			plotAlleeEffectDynamics(r=input$growth, K=input$capacity, TT=input$threshold)
		})
	}
)



