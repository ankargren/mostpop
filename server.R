#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

library(httr)
library(ggplot2)
library(ggrepel)
library(tidyverse)




get_id <- function(search_name, HeaderValue) {
  band_nospace <- gsub(" ", "%20", search_name)
  artist_url <- paste0(paste0("https://api.spotify.com/v1/search?q=", band_nospace), "&type=artist")
  resp <- GET(url = artist_url, 
              add_headers(Authorization = HeaderValue))
  artist_items <- content(resp)$artists$items
  if (length(artist_items) == 0) {
    artist_id <- NULL 
  } else {
    artist_id <- data.frame(t(sapply(1:length(artist_items), function(x) c(artist_items[[x]]$name, artist_items[[x]]$uri, artist_items[[x]]$popularity))))
    colnames(artist_id) <- c("Artist", "ID", "Popularity")
    artist_id$ID <- sub("spotify:artist:", "", artist_id$ID)
    artist_id <- arrange(artist_id, desc(Popularity))
  }
  
  return(artist_id)
}

main_function <- function(band, country, HeaderValue) {
  resp2 <- GET(url = paste0(paste0("https://api.spotify.com/v1/artists/", band), paste0("/albums?album_type=album&market=", country)),
               add_headers(Authorization = HeaderValue))
  content_resp <- content(resp2)
  
  n_albums <- length(content_resp$items)
  if (n_albums < 1) {
    return(NULL)
  } else {
    album_IDs <- paste(sapply(1:n_albums, function(x) gsub("spotify:album:", "", content_resp$items[[x]]$uri)), collapse = ",")
    
    test <- GET(url = paste0("https://api.spotify.com/v1/albums?ids=", album_IDs), 
                add_headers(Authorization = HeaderValue))
    test2 <- content(test)
    
    plot_data <- data.frame(t(sapply(1:n_albums, function(x) c(test2$albums[[x]]$release_date, test2$albums[[x]]$popularity, test2$albums[[x]]$name, test2$albums[[x]]$uri))))
    colnames(plot_data) <- c("Date", "Popularity", "Name", "ID")
    plot_data$Date <- as.character(plot_data$Date)
    plot_data$Popularity <- as.numeric(as.character(plot_data$Popularity))
    plot_data$Name <- as.character(plot_data$Name)
    plot_data$Date <- sapply(1:n_albums, function(x) if (gsub("....-..-..", "", plot_data$Date[x]) == "") plot_data$Date[x] else paste0(plot_data$Date[x], "-07-01"))
    plot_data$Date <- as.Date(plot_data$Date, format = "%Y-%m-%d")
    return(plot_data)
  }
}

plot_function <- function(plot_data, band_name) {
  ggplot(plot_data) + 
                geom_point(aes(Date, Popularity), size = 5, color = "black", fill = "grey", stroke = 1, shape = 21) +
                xlab("Release date") + ylab("Popularity") + 
                theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ylim(0, 100) + 
                geom_label_repel(
                  aes(Date, Popularity, label = Name),
                  fontface = 'bold', color = 'Red',
                  box.padding = unit(0.25, "lines"),
                  point.padding = unit(0.5, "lines"),
                  size = 3
                ) + ggtitle(paste(band_name, "Albums"))
}
  
wrapper_fun <- function(search_name) {
  # Retrieve access token
  clientID <- "a8ff287d905549a19a2a6463d48e44a2"
  secret <- "3a54a7a6f0ea4b2db4539c61b2a892da"
  response <- POST(
    "https://accounts.spotify.com/api/token",
    accept_json(),
    authenticate(clientID, secret),
    body = list(grant_type = "client_credentials"),
    encode = "form"
  )
  auth_success <- ifelse(is.null(content(response)$error), TRUE, FALSE)
  
  if (auth_success) {
    access_token <- content(response)$access_token
    header_value <- paste("Bearer", access_token)
    id_res <- get_id(search_name, header_value)
    id_list <- setNames(as.list(id_res[, 2]), id_res[, 1])
    ## Get the position of the artist we're looking for
    # --- If there is an exact match (with everything in lower case), pick it
    # --- If there are multiple matches, check case sensitive matches
    # --- Otherwise, pick the most popular
    if (tolower(search_name) %in% tolower(id_res[, 1])) {
      row_select <- which(tolower(search_name) == tolower(id_res[, 1]))
      if (length(row_select) > 1) {
        row_select <- ifelse(search_name %in% id_res[, 1], which(search_name == id_res[, 1]), 0)
      }
    } else {
      row_select <- 1
    }
    band_ID <- id_res[row_select, 2]
    band_name <- ifelse(!is.null(id_res), as.character(id_res[row_select, 1]), search_name)
    plot_data <- main_function(band_ID, "SE", header_value)
    artist_exists <- ifelse(!is.null(band_ID), TRUE, FALSE)
    search_success <- ifelse(!is.null(plot_data), TRUE, FALSE)
  } else {
    plot_data <- NULL
    search_success <- FALSE
    band_name <- search_name
    artist_exists <- FALSE
  }
  
  if (search_success) {
    plot_data <- plot_data %>%
      mutate(name = tolower(Name)) %>%
      group_by(name) %>% top_n(n = 1, wt = Popularity) %>%
      ungroup %>% select(-name) %>%
      as.data.frame()
  }
  
  
  
  return(list(band_name = band_name, plot_data = plot_data, auth_success = auth_success,
              artist_exists = artist_exists, search_success = search_success))
}


shinyServer(function(input, output) {
  
    all_computations <- reactive({wrapper_fun(input$search_name)})
    output$Plot <- renderPlot({
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
        plot_function(all_comp$plot_data, all_comp$band_name)
      } else {
        plot(1, type="n", axes=F, xlab="", ylab="", main = error_message)
      }

  })
  
  output$player <- renderUI({ 
    all_comp <- all_computations()
    plot_data <- all_comp$plot_data
    album_ID <- gsub("spotify:album:", "", plot_data[which.max(plot_data[, 2]), 4])
    HTML(paste0('<iframe src="https://embed.spotify.com/?uri=spotify%3Aalbum%3A', album_ID, '&theme=white" width="300" height="380" frameborder="0" allowtransparency="true"></iframe>'))
  })
  
})
