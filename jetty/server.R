
shinyServer(function(input, output, session) {

  parcel <- NULL
  v <- reactiveValues(data = NULL)
  
  observeEvent(input$Add, {
    w <- ""
    if (input$parcelName %in% parcel$ProductName) {
      w <- "Parcel already exists!!!"
    } else if (input$quant ==0) {
      w <- "Quantity is 0!!!"
    } else {
      parcel <<- AddParcel(input$parcelName, input$quant, d, parcel)
      v$data <- parcel
    } 
    output$text0 <- renderText({
      w
    })
  })
  
  observeEvent(input$Delete, {
    w   <- ""
    if (!(input$parcelName %in% parcel$ProductName)) {
      w <- "Parcel doesn't exist!!!"
    } else {
      parcel <<- DeleteParcel(input$parcelName, parcel)
      v$data <- parcel
    } 
    output$text0 <- renderText({
      w
    })
  })
  
  output$ui4min <- renderUI({
    if (input$method=="anticipated") {
      numericInput("J4min", "J4min", value = 0)
    }
  })
  
  output$ui4max <- renderUI({
    if (input$method=="anticipated") {
      numericInput("J4max", "J4max", value = 0)
    }
  })
  
  output$ui17min <- renderUI({
    if (input$method=="anticipated") {
      numericInput("J17min", "J17min", value = 0)
    }
  })
  
  output$ui17max <- renderUI({
    if (input$method=="anticipated") {
      numericInput("J17max", "J17max", value = 0)
    }
  })
  
  output$ui18min <- renderUI({
    if (input$method=="anticipated") {
      numericInput("J18min", "J18min", value = 0)
    }
  })
  
  output$ui18max <- renderUI({
    if (input$method=="anticipated") {
      numericInput("J18max", "J18max", value = 0)
    }
  })
  
  output$ui35min <- renderUI({
    if (input$method=="anticipated") {
      numericInput("J35min", "J35min", value = 0)
    }
  })
  
  output$ui35max <- renderUI({
    if (input$method=="anticipated") {
      numericInput("J35max", "J35max", value = 0)
    }
  })
  
  output$ui35Amin <- renderUI({
    if (input$method=="anticipated") {
      numericInput("J35Amin", "J35Amin", value = 0)
    }
  })
  
  output$ui35Amax <- renderUI({
    if (input$method=="anticipated") {
      numericInput("J35Amax", "J35Amax", value = 0)
    }
  })
  
  tt <- eventReactive(input$Evaluate, {
    SimulationPernis(parcel, 
                     method = input$method, 
                     n.sample = input$nsamp, 
                     j.model = data.frame(jetty=c("J4","J17","J18", "J35", "J35A"), 
                                          ant_from=c(input$J4min, input$J17min, input$J18min, input$J35min, input$J35Amin), 
                                          ant_to=c(input$J4max, input$J17max, input$J18max, input$J35max, input$J35Amax)))
  })
  
  output$table <- DT::renderDataTable({
    DT::datatable(v$data)
  })
    
  output$downloadparcel <- downloadHandler(
    
    filename = function() {
      paste('parcel', 'csv', sep = ".")
    },
    
    content = function(file) {
      
      write.table(v$data, file, sep = ",",
                  row.names = FALSE)
    }
  )
  
  
  output$text1 <- renderText({
    paste0("The estimated total terminal time is ",ceiling(mean(tt()))," hrs")
  })
  
  
  output$histplot <- renderPlotly({
    plot_ly(x = tt(), type = "histogram", name="Total Terminal Time") %>% layout(xaxis=list(title='Time'),yaxis=list(title='Frequency')) 
  })
  
  
  # Quit
  session$onSessionEnded(function(){ q() })
   
})

