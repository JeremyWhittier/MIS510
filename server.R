
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
  
  terms4 <- reactive({
    # Change when the "update" button is pressed...
    input$update
    # ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Show me a histogram")
        getPickle(input$city)
      })
    })
  }) 
  
  output$plot1 <- renderPlot({
    v <- terms()
    wordcloud(names(v), v, scale=c(5,0.5),
                  min.freq = input$freq, max.words=input$max,
                  colors=brewer.pal(8, "Dark2"))
  })
  
  output$plot2 <- renderPlot({
    b <- terms2()

    comparison.cloud(b, random.order=FALSE,
                     colors = c("#00B2FF", "red", "#FF0099", "#6600CC", "green", "orange", "blue", "brown"),
                     title.size=2, max.words=input$max, scale=c(5, 0.5),rot.per=0.4)
  })

  output$plot3 <- renderText({
    value = terms3()
    value = round(value,digits = 2)
    if (value > 1) {
      print(paste0('Everyone is loving this weather! Score: ', value))
    } else if (value < -1){
      print(paste0('Weather potentially unsafe! Score: ', value))
    } else if (value > 0 & value < .5){
      print(paste0('People are not real happy about the weather. Score: ', value))
    } else if (value < 0 & value > -1){
      print(paste0('The weather is bad, be cautious. Score: ', value))
    } else if (value > 0.5 & value < 1){
      print(paste0('Nice weather today! Score: ', value))
    }
    else {
      print("Something has happened to earth :(")
    }
    
  })
  
  output$plot4 <- renderPlot({
    df = terms4()
    
    ggplot(df, aes(x=syuzhet_emotion.df)) + 
      geom_histogram(aes(y=..density..), colour="black", fill="white", bins = 15)+
      geom_density(alpha=.2, fill="#64c651")

    
  })
  
  
  
  }
