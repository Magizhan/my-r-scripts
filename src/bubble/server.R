library(dplyr)

shinyServer(function(input, output, session) {

  # Provide explicit colors for regions, so they don't get recoded when the
  # different series happen to be ordered differently from year to year.
  # http://andrewgelman.com/2014/09/11/mysterious-shiny-things/
  #13 for UPI
  defaultColors <- c("#3366cc", "#dc3912", "#ff9900", "#109618", "#990099", "#0099c6", "#dd4477","#cc9933","#12b5dc","#0066ff","#96108e","#c62d00","#44ddaa")
  #21 for DC
  #defaultColors <- c("#3366cc", "#dc3912", "#ff9900", "#109618", "#990099", "#0099c6", "#dd4477","#cc9933","#12b5dc","#0066ff","#96108e","#c62d00","#44ddaa","#f1c6de","#c6f1ef","#f1dac6","#dac6f1","#040106","#952adc","#2adc3c","#952adc")
  
  series <- structure(
    lapply(defaultColors, function(color) { list(color=color) }),
    names = levels(data$pg)
  )

  monthData <- reactive({
    # Filter to the desired year, and put the columns
    # in the order that Google's Bubble Chart expects
    # them (name, x, y, color, size). Also sort by region
    # so that Google Charts orders and colors the regions
    # consistently.
    df <- data %>%
      filter(mon == input$month) %>%
      select(mid, SR, prop,
        pg, tot) %>%
      arrange(pg)
  })

  output$chart <- reactive({
    # Return the data and options
    list(
      data = googleDataTable(monthData()),
      options = list(
        title = sprintf(
          "UPI distribution vs. Success Rate, %s",
          input$month),
        series = series
      )
    )
  })
})
