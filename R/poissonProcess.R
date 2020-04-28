# Illustrating the Poisson process.
#
# Authors: Jan Engelstaedter
#
# Last date changed: 28/04/2020

generatePoissonEvents <- function(lambda = 1, duration = 100) {
  i <- 1
  x <- rep(NA, 1000)    # empty vector to increase speed
  x[1] <- rexp(1, lambda)
  while(x[i] < duration) {
    x[i + 1] <- x[i] + rexp(1, lambda)
    i <- i + 1
  }
  if (i == 1) { # no event happened
    x <- double(0)
  } else {
    x <- x[1:(i-1)]
  }
  return(x)
}

plotPoissonProcess <- function(lambda = 1, duration = 100) {
  x <- generatePoissonEvents(lambda)
  par(mar=c(0, 0, 2, 0), cex.lab = 2, cex.axis = 1.5, cex.main = 2)
  layout(mat = matrix(c(1,2,1,3,1,4), nrow = 2), heights = c(1,5))
  plot(x = -1000, y = 0, xlim = c(0, duration), ylim = c(-1,1), main = "Events through time",
       axes = FALSE, cex=1.2)
  lines(x = c(0, duration), y = c(0, 0))
  if (length(x) >= 1) {
    for(i in 1:length(x))
      lines(x = c(x[i], x[i]), y = c(-0.75, 0.75), col = "blue")
  }

   par(mar=c(4, 5, 2, 1))
  if (length(x) >= 1) {
    maxy <- length(x)
  } else {
    maxy <- 1
  }
 plot(x = -100, y = 0, xlim = c(0, duration), ylim = c(0, maxy), type = "l",
       xlab = "Time", ylab = "Cumulative number of events")
  if (length(x) >= 1) {
    for(i in 1:length(x))
      lines(x = c(0, rep(x, each = 2), duration), y = rep(0:length(x), each = 2), col = "blue")
  }

  bincs <- matrixStats::binCounts(x, bx = 0:duration)
  hist(bincs,
       breaks = 0:(max(bincs)+1) - 0.5,
       xlab = "Number of events per time unit", main = NULL, col = "grey")
  box()

  if (length(x) >= 1) {
    timesteps <- x[2:length(x)] - x[1:(length(x)-1)]
  } else {
    timesteps <- 0
  }
  hist(timesteps, xlab = "Time between events", main = NULL, col = "grey")
  box()
}


appPoissonProcess <- shinyApp(
  ui = fluidPage(
    titlePanel("Poisson process"),
    h4("This app illustrates the Poisson process, a stochastic process where certain events occur independently of each other and (in the simplest case) at a constant rate lambda through time, or sometimes through space. In this example, events can happen at different rates over a time period of 100 units."),

    sidebarLayout(
      sidebarPanel(
        sliderInput("lambda", "lambda",
                    min = 0, max = 10,
                    value = 1, step = 0.01),
        actionButton("run", "Run again")
      ),

      mainPanel(
        plotOutput("plot")
      ),
      "left"
    )
  ),

  server = function(input, output) {
    observeEvent(input$run, {
      output$plot <- renderPlot({
        plotPoissonProcess(lambda = input$lambda)
      })
    })
    output$plot <- renderPlot({
      plotPoissonProcess(lambda = input$lambda)
    })

  }
)
