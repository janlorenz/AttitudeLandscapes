library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dashTable)
library(plotly)
library(tidyverse)

app <- Dash$new()

lscp <- read_csv(file = "lscp.csv", col_types = cols(
  topic = col_character(),
  cntry = col_character(),
  year = col_double(),
  attitude = col_double(),
  dweight = col_double()
))
country_indicator <- lapply(unique(lscp$cntry), function(ind) list(label = ind, value = ind))
topic_indicator <- lapply(unique(lscp$topic), function(ind) list(label = ind, value = ind))

app$layout(
  htmlDiv(
    list(
      dccDropdown(
        id = "cntry-dropdown",
        options = country_indicator,
        value = "DE",
        multi = FALSE
      ),
      dccDropdown(
        id = "topic-dropdown",
        options = topic_indicator,
        value = "lrscale",
        multi = FALSE
      ),
      dccGraph(id = "landscape"),
      dccSlider(
        id = "year-slider",
        min = 0,
        max = length(unique(lscp$year))-1,
        marks = unique(lscp$year),
        value = length(unique(lscp$year))-1
      )
    )
  )
)

app$callback(
  output = list(id='landscape', property='figure'),
  params = list(
    input(id='cntry-dropdown', property='value'),
    input(id='topic-dropdown', property='value'),
    input(id='year-slider', property='value')
  ),
  function(selected_country, selected_topic, selected_year_index) {
    lscp %>% filter(!is.na(attitude), cntry == selected_country, topic == selected_topic,
                    year == unique(lscp$year)[selected_year_index+1]) %>%
      mutate(freq = dweight/sum(dweight)) %>%
      plot_ly(x = ~attitude, y = ~freq, color = ~cntry, frame = ~year) %>%
      add_bars()
  }
)

app$run_server()






