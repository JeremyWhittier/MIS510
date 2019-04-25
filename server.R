
function(input, output, session) {
  # Define a reactive expression for the document term matrix
  terms <- reactive({
    # Change when the "update" button is pressed...
    input$update
    # ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        getTermMatrix(input$city)
      })
    })
  })

  terms2 <- reactive({
    # Change when the "update" button is pressed...
    input$update
    # ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Processing Emotion")
        getbanana(input$city)
      })
    })
  })

  terms3 <- reactive({
    # Change when the "update" button is pressed...
    input$update
    # ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Calculating a score!")
        getGrape(input$city)
      })
    })
  })  
  
  output$plot1 <- renderPlot({
    v <- terms()
    wordcloud(names(v), v, scale=c(4,0.5),
                  min.freq = input$freq, max.words=input$max,
                  colors=brewer.pal(8, "Dark2"))
  })
  
  output$plot2 <- renderPlot({
    b <- terms2()

    comparison.cloud(b, random.order=FALSE,
                     colors = c("#00B2FF", "red", "#FF0099", "#6600CC", "green", "orange", "blue", "brown"),
                     title.size=1, max.words=input$max, scale=c(2.5, 0.4),rot.per=0.4)
  })

  output$plot3 <- renderText({
    value = terms3()
    if (value > 1) {
      print(paste0('The weather is looking great!', value))
    } else if (value < -1){
      print(paste0('Be careful outside, weather potentially unsafe ', value))
    } else if (value > 0 & value < 1){
      print(paste0('The weather is good ', value))
    } else if (value < 0 & value > -1){
      print(paste0('The weather is poor, be cautious ', value))
    } else {
      print("Something has happened to earth :(")
    }
    
  })
  
  
  }
