# Illustrating the graphical method for the discrete time logistical model
# of population growth.
#
# Authors: Nicole Fortuna, Jan Engelstaedter
#
# Last date changed: 21/04/2020


plotGraphicalMethod <- function(rate=2, initialPop=0.05, gens=100) {
	xLog<- seq(0,1,0.01)
	yLog<- rate* xLog*(1-xLog)

	par(mfrow=c(1,2), mar=c(5,5,0,0), oma=c(0,0,1,1), mgp=c(2.5,1,0))
	plot(y=yLog, x= xLog, type="l", col="red", ylim=c(0,1),
		xlab=expression('x'[t]), ylab=expression('x'[t+1]), cex.lab=2)

	lines(x=seq(0,1,0.01), y=seq(0,1,0.01), type="l")

	x <- rep(NA, gens+1)
	x[1] <- initialPop

	for(i in 1:gens)
	  x[i+1] <- rate * x[i] * (1 - x[i])

	lines(x = rep(x[-length(x)], each = 2),
	      y = c(0, rep(x[c(-1, -length(x))], each = 2), x[length(x)]),
	      type="l", col="blue")

	plot(x=0:gens, y=x,ylim=c(0,1), xlim=c(0, gens),
	     xlab="Generations", ylab="Population Size", type="l", col="blue", cex.lab=2)
}


appGraphicalMethod <- shinyApp(
	ui = fluidPage(
		titlePanel("The graphical method"),
		h4("This app illustrates the graphical method for discrete time (recurrence equation) models, using the example of the logistic growth equation. The left plot shows in red the function defining this model, x'=f(x)=bx(1-x), with the relative population size x at the current generation on the horizontal axis and the relative population size x' at the next generation on the y axis. The blue line shows the trajectory when starting from a specified relative population size in generation zero. The right plot shows the same dynamics through time."),

	sidebarLayout(
		sidebarPanel(
			sliderInput("rate", "Growth rate:",
				min = 0.9, max = 4.1,
				value = 3.2, step = 0.1),

			sliderInput("initPop", "Initial relative population size:",
				min = 0, max = 1,
				value = 0.05, step = 0.01),

			sliderInput("gens", "Generations:",
				min = 1, max = 200,
				value = 50, step = 1)

			),

			mainPanel(
				plotOutput("plot")
			),

			"left"
		)
	),

	server = function(input, output) {
		output$plot <- renderPlot({
			plotGraphicalMethod(rate = input$rate, initialPop=input$initPop, gens=input$gens)
		})
	}
)

