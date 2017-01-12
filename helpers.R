library(shiny)
library(httr)
library(ggplot2)
library(tidyverse)
library(plotly)

clientID <- "YOUR ID"
secret <- "YOUR SECRET"

get_id <- function(search_name, header_value) {
  band_nospace <- gsub(" ", "%20", search_name)
  artist_url <- paste0(paste0("https://api.spotify.com/v1/search?q=", band_nospace), "&type=artist")
  resp <- GET(url = artist_url, 
              add_headers(Authorization = header_value))
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

main_function <- function(band, header_value) {
  resp2 <- GET(url = paste0("https://api.spotify.com/v1/artists/", band, "/albums?album_type=album"),
               add_headers(Authorization = header_value))
  content_resp <- content(resp2)
  
  n_albums <- length(content_resp$items)
  if (n_albums < 1) {
    return(NULL)
  } else {
    album_IDs <- paste(sapply(1:n_albums, function(x) gsub("spotify:album:", "", content_resp$items[[x]]$uri)), collapse = ",")
    
    test <- GET(url = paste0("https://api.spotify.com/v1/albums?ids=", album_IDs), 
                add_headers(Authorization = header_value))
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
  Album <- plot_data$Name
  p <- ggplot(plot_data, aes(Date, Popularity, key = Album)) +
    geom_hline(yintercept = max(plot_data$Popularity), linetype = "dashed", color = "gray") +
    geom_point(size = 5, 
               color = "black", 
               fill = "grey", 
               stroke = 0.5, 
               shape = 21) +
    xlab("Release date") + ylab("Popularity") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    ylim(0, 100) + 
    ggtitle(paste0('"', Album[which.max(plot_data$Popularity)], '" is the most popular album by ', band_name))
  return(p)
}

wrapper_fun <- function(search_name) {
  # Retrieve access token
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
    plot_data <- main_function(band_ID, header_value)
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
