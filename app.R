## app.R ##
library(ggplot2)
library(shiny)
library(shinydashboard)

span_seq <- function(x) {
  r <- range(x)
  s <- seq.int(from = r[1], to = r[2])
  return(as.character(s))
}

ui <- dashboardPage(
  dashboardHeader(title = "Central Limit Theorem"),
  dashboardSidebar(
    radioButtons("dist", "Distribution",
                 choices = list("Binomial" = "binomial",
                                "Exponential" = "exponential",
                                "F Distribution" = "f",
                                "Gamma" = "gamma",
                                "Hypergeometric" = "hypergeometric",
                                "Poisson" = "poisson",
                                "Student's T Distribution" = "t",
                                "Uniform" = "uniform",
                                "Weibull" = "weibull"),
                 selected = "uniform"),
    sliderInput("sample_size", "Sample Size", min = 1, max = 500, value = 10,
                step = 1, round = TRUE),
    sliderInput("n_samples", "Number of Samples Drawn", min = 100,
                max = 5000, value = 1000, step = 1, round = TRUE),
    sliderInput("n_bins", "Histogram Bins", min = 1, max = 50, value = 30,
                step = 1, round = TRUE)
  ),
  dashboardBody(
    fluidRow(
      box(plotOutput("plot1", height = 300)),
      box(plotOutput("plot2", height = 300))
    ),
    fluidRow(
      infoBoxOutput("dist_info"),
      infoBoxOutput("empirical_mean"),
      infoBoxOutput("empirical_sd")
    ),
    fluidRow(
      infoBoxOutput("shapirowilkes"),
      infoBoxOutput("sampling_mean"),
      infoBoxOutput("sampling_sd")
    )
  )
)



server <- function(input, output) {
  aqua <- "#00C0EF"
  yellow <- "#F39C12"
  fuchsia <- "#F012BE"
  rdist <- function(n) {
    switch(input$dist,
           binomial = rbinom(ceiling(n), size = 10, prob = 0.5),
           exponential = rexp(n, rate = 1),
           f = rf(n, df1 = 5, df2 = 5),
           gamma = rgamma(n, shape = 1),
           hypergeometric = rhyper(nn = n, m = 5, n = 5, k = 5),
           poisson = rpois(n, lambda = 1),
           t = rt(n, df = 5),
           uniform = runif(n),
           weibull = rweibull(n, shape = 1)
    )
  }
  ddist <- function(x) {
    switch(input$dist,
           binomial = dbinom(ceiling(x), size = 10, prob = 0.5),
           exponential = dexp(x, rate = 1),
           f = df(x, df1 = 5, df2 = 5),
           gamma = dgamma(x, shape = 1),
           hypergeometric = dhyper(x, m = 5, n = 5, k = 5),
           poisson = dpois(x, lambda = 1),
           t = dt(x, df = 5),
           uniform = dunif(x),
           weibull = dweibull(x, shape = 1)
    )
  }
  inputData <- reactive({
    lapply(1:input$n_samples, function(x) {rdist(input$sample_size)})
  })
  inputMeans <- reactive({
    means <- sapply(inputData(), mean)
    data.frame(x = means)
  })
  inputSample <- reactive({
    sample <- sapply(inputData(), function(x) {sample(x, size = 1)})
    data.frame(x = sample)
  })
  inputBins <- reactive({
    input$n_bins
  })
  dist_name <- reactive({
    switch(input$dist,
           "binomial" = c("Binomial", "n = 10, p = 0.5"),
           "exponential" = c("Exponential", "lambda = 1"),
           "f" = c("F Distribution", "df1 = 5, df2 = 5"),
           "gamma" = c("Gamma", "scale = 1, shape = 1"),
           "hypergeometric" = c("Hypergeometric", "n = 5, m = 5, k = 5"),
           "poisson" = c("Poisson", "lambda = 1"),
           "t" = c("Student's T", "df = 5"),
           "uniform" = c("Uniform", "a = 0, b = 1"),
           "weibull" = c("Weibull", "shape = 1, scale = 1"))
  })
  shapiro_wilkes_p <- reactive({
    results <- shapiro.test(inputMeans()$x)
    return(signif(results$p.value, digits = 5))
  })
  output$plot1 <- renderPlot({
    if (input$dist %in% c("binomial", "hypergeometric", "poisson")) {
      ggplot(data = inputSample(), aes(x = x)) +
        geom_bar(aes(y = ..count.. / sum(..count..)),
                 colour = "white",
                 fill = aqua) +
        geom_density(linetype = "dotted") +
        labs(title = "Empirical Sample", y = "Mass") +
        scale_x_discrete(breaks = span_seq(inputSample()$x))
    } else {
      ggplot(data = inputSample(), aes(x = x)) +
        geom_histogram(aes(y = ..density..),
                       bins = inputBins(),
                       colour = "white",
                       fill = aqua) +
        stat_function(fun = dnorm,
                      arg = list(mean = mean(inputSample()$x),
                                 sd = sd(inputSample()$x)),
                      colour = "red") +
        geom_density(linetype = "dotted") +
        labs(title = "Empirical Sample", y = "Density")
    }
  })
  output$plot2 <- renderPlot({
    ggplot(data = inputMeans(), aes(x = x)) +
      geom_histogram(aes(y = ..density..),
                     bins = inputBins(),
                     colour = "white",
                     fill = yellow) +
      stat_function(fun = dnorm,
                    arg = list(mean = mean(inputMeans()$x),
                               sd = sd(inputMeans()$x)),
                    colour = "red") +
      geom_density(linetype = "dotted") +
      labs(title = "Sampling Distribution", y = "Density")
  })
  output$shapirowilkes <- renderInfoBox({
    infoBox(title = "Shapiro-Wilkes Test for Normality",
            value = shapiro_wilkes_p(),
            subtitle = "p-value: Null is normality",
            icon = icon("area-chart"),
            color = "fuchsia")
  })
  output$dist_info <- renderInfoBox({
    infoBox(title = "Distribution:",
            value = dist_name()[1],
            subtitle = dist_name()[2],
            color = "lime")
  })
  output$empirical_mean <- renderInfoBox({
    infoBox("Empirical Mean",
            format(mean(inputSample()$x)))
  })
  output$empirical_sd <- renderInfoBox({
    infoBox("Empirical Standard Deviation",
            format(sd(inputSample()$x)))
  })
  output$sampling_mean <- renderInfoBox({
    infoBox("Sampling Distribution Mean",
            format(mean(inputMeans()$x)),
            color = "yellow")
  })
  output$sampling_sd <- renderInfoBox({
    infoBox("Sampling Distribution Standard Deviation",
            format(sd(inputMeans()$x)),
            color = "yellow")
  })
}

shinyApp(ui, server)