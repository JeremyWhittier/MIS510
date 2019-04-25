fluidPage(
  # Application title
  titlePanel("Should we go outside?"),
  
  sidebarLayout(
    # Sidebar with a slider and selection inputs
    sidebarPanel(
      #selectInput("selection", "Choose a City:",
      #            choices = books),
      textInput("city", "Enter A City", "Tucson"),
      actionButton("update", "Change"),
      hr(),
      sliderInput("freq",
                  "Minimum Frequency:",
                  min = 1,  max = 50, value = 20),
      sliderInput("max",
                  "Maximum Number of Words:",
                  min = 1,  max = 300,  value = 100),
      h2("Lets Score the Weather!"),
      h1(textOutput("plot3")),
      h2("General Word Cloud"),
      plotOutput("plot1",width = '400px', height = '400px')
    ),
    
    # Show Word Cloud
    mainPanel(

      h2("Histogram time!"),
      plotOutput("plot4"),
      h2("Emotional Breakdown"),
      plotOutput("plot2",width = '800px', height = '800px')

    )
  )
)


