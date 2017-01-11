source("helpers.R")
shinyServer(function(input, output, session) {
  
    all_computations <- eventReactive(input$go, {wrapper_fun(input$search_name)})
    output$Plot <- renderPlotly({
      all_comp <- all_computations()
      
      # Error message
      if (all_comp$auth_success && !all_comp$artist_exists) {
        error_message <-  paste0("Could not find ", all_comp$band_name, " on Spotify. Did you spell the name correctly?")
      }
      
      if (all_comp$auth_success && !all_comp$search_success && all_comp$artist_exists) {
        error_message <- paste0("There are no albums by ", all_comp$band_name, " on Spotify.")
      }
      
      if (!all_comp$auth_success) {
        error_message <- "The authentication failed. Check your credentials."
      }
      
      if (all_comp$auth_success & all_comp$search_success) {
        p <- plot_function(all_comp$plot_data, all_comp$band_name)
        ggplotly(p)
      } else {
        plot(1, type="n", axes=F, xlab="", ylab="", main = error_message)
      }

  })
  
  output$player <- renderUI({ 
    all_comp <- all_computations()
    plot_data <- all_comp$plot_data
    d <- event_data("plotly_click")
    if (is.null(d)) {
       plot_ID <- plot_data$ID[which.max(plot_data[, 2])]
    } else {
      plot_ID <- plot_data$ID[which(plot_data$Name == d$key)]
    }
    album_ID <- gsub("spotify:album:", "", plot_ID)
    HTML(paste0('<iframe src="https://embed.spotify.com/?uri=spotify%3Aalbum%3A', album_ID, '&theme=white" width="300" height="200" frameborder="0" allowtransparency="true"></iframe>'))
  })
  
  
})
