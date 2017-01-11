# Most popular albums

https://sebastianankargren.shinyapps.io/most-pop/

## Description

This simple Shiny application lets the user search for an artist to see which of the artist's albums are more popular. A common problem when exploring new artists is that it may be hard to know where to start---what album should you start with? Hopefully, this can be some help in knowing where to begin.

## Implementation

The application is implemented in R using `httr` to fetch popularity data from Spotify. The results are visualized using `plotly`.

## Note

To run it on your own, you need to supply your own `clientID` and `secret` in `helpers.R`. You can get these from Spotify by registering an application.
