# Solving an discrete-time model for haploid selection,
# plotting the result and embedding it into a shiny app.
#
# Authors: Nicole Fortuna, Jan Engelstaedter
#
# Last date changed: 20/04/2020


plotHaploidSelection <- function(freqA = 0.5, s = 0.5, gen = 20) {
	generations 	<- 0:gen
	frequency		<- rep(NA, length(generations))
	frequency[1]	<- freqA

	for (i in 1:gen) {
		frequency[i+1]	<- ((1 + s) * frequency[i])/(1 + s * frequency[i])
	}

	par(mar=c(5,5,0,0), oma=c(0,0,1,1), mgp=c(2.5,1,0))
	plot(NA, ylim=c(0,1), xlim=c(0,gen), ylab="Allele Frequencies", xlab="Generation", cex.lab=2)
	polygon(x=c(0, gen, gen,0), y=c(1,1,0,0), col="blue")
	polygon(x=c(gen,0,0:gen), y=c(0,0, frequency), col="red")
}

appHaploidSelection <- shinyApp(
	ui = fluidPage(
		titlePanel("Natural selection in a haploid population"),
		h4("The plot shows numerical solutions of the recurrence eqation model p'=p(1+s)/(1+sp),
		    where s is the selection coefficient. Red represents the frequency of the focal allele,
		    blue the frequency of the other allele."),
		hr(),

		sidebarLayout(
			sidebarPanel(
				sliderInput("freqA", "Initial frequency of allele A:",
							min = 0, max = 1,
							value = 0.5, step = 0.01),

				sliderInput("s", "Selection coefficient s:",
							min = -1, max = 1,
							value = 0.5, step = 0.01),
				sliderInput("gen", "Number of generations:",
							min = 10, max = 100,
							value = 20)
			),

			mainPanel(
				plotOutput("plot")
			),

    			"left"
		)
	),

	server = function(input, output) {
		output$plot <- renderPlot({
  			plotHaploidSelection(freqA = input$freqA, s = input$s, gen = input$gen)
		})
	}
)
