library(dash)
library(dashHtmlComponents)
library(tidyverse)
library(eList)
library(glue)
library(plotly)

df = read_csv('clean_data.csv')
app = Dash$new(external_stylesheets=list('https://codepen.io/chriddyp/pen/bWLwgP.css','style.css'))


app$layout(
  htmlDiv(
    list(
      htmlH1(
        "Who Are in the Olympics?",
         style = list('color'= 'white', 'text-align'= 'left', 'padding'= '0px 0px 0px 20px', 'margin-bottom'= '-10px')
      ),
      htmlH4(
        "Insights for Olympic Athlete Information since 1896", 
        style = list('color'= 'white', 'text-align'= 'left', 'border-bottom'= '1px solid black', 'padding'= '0px 0px 10px 20px')
      ),
    
  # Main Container Div
    htmlDiv(
      list(
  
        # Sidebar (Filter) Div
        htmlDiv(
          list(
            htmlH2("Filters",
                   style = list('flex-grow'= '1','margin'= '0px','border-bottom'= '2px solid white','line-height'= '1')
                   ),
            htmlDiv(
              list(
                htmlH5("Drag Slider To Select Years"),
                dccRangeSlider(
                  min = 1896, max = 2016,
                  
                  marks = List(for (i in seq(1896, 2016, 8)) i = list("label" = glue('{i}'), 'style'= list('transform'= 'rotate(90deg)', 'color'= 'white')) ),
                  
                  id = 'year_range',
                  value = list(1896, 2016),
                  allowCross = FALSE,
                  step = 1 ,
                  tooltip=list("placement"= "top", "always_visible"= TRUE)
                  )
                ), style = list('width'= '100%', 'flex-grow'= '2')
              ),
            htmlDiv(
              list(
                  htmlH5("Select Sports"),
                  dccDropdown(
                    options=c("All",sort(unique(df$Sport))),
                    value=list('All'),
                    multi=TRUE,
                    id='sport'
                  )
                ), style = list('width'= '100%', 'color'= 'black', 'flex-grow'= '1.5')
              ),
            htmlDiv(
              list(
                  htmlH5("Select Countries"),
                  dccDropdown(
                    options=c("All", sort(unique(df$Team))),
                    value=list('All'),
                    multi=TRUE,
                    id='country'
                )
              ), style = list('width'= '100%', 'color'= 'black', 'flex-grow'= '1.5'),
             ),
            htmlDiv(
              list(
                  htmlH5("Medal Filter"),
                  dccRadioItems(
                    options=list('Gold', 'Silver', 'Bronze','All'),
                    value='All',
                    id='medals',
                    inline=TRUE
                  )
                ), style = list('width'= '100%', 'flex-grow'= '1', 'color'= 'white'),
              ),
            
            htmlDiv(
              list(
                htmlH5("Season Filter"),
                dccRadioItems(
                  options=c(unique(df$Season),'Both'),
                  value='Both',
                  id='season',
                  inline=TRUE
              )
            ), style = list('width'= '100%', 'flex-grow'= '4', 'color'= 'white'))
            
          ), style = list('width'= '23%', 'margin-top'= '0px', 'padding'= '25px',
            'background-color'= '#544F78', 'border-radius'= '10px',
            'display'= 'flex', 'justify-content'= 'space-around', 'flex-direction'= 'column')
          ),
        
        # Graph Container Div                  
        htmlDiv(
          list(
            dccLoading(
              id = 'loading_hist',
              children = list(
                dccGraph(id='hist', 
                          style = list('height'= '350px', 'width'= '33%', 'display'= 'inline-block'),
                          config=list('displayModeBar'=FALSE)
                          ),
                dccGraph(id='hist2',
                         style = list('height'= '350px', 'width'= '33%', 'display'= 'inline-block'),
                         config=list('displayModeBar'=FALSE)
                          ),
                dccGraph(id='hist3',
                         style = list('height'= '350px', 'width'= '33%', 'display'= 'inline-block'),
                         config=list('displayModeBar'=FALSE)
                          )
              ), type = 'circle', color = '#B33951'
            )
          ), style = list('width'= '70%', 'overflow'= 'hidden', 'height'= '950px', 
                           'background-color'= '#544F78', 'border-radius'= '10px', 
                           'padding'= '1%')
        )
      ),style = list('display'= 'flex', 'justify-content'= 'space-around')
    )
  ), style = list('display'= 'fixed', 'height'= '100%', 'background-color'= '#322c4a')
))


filter_data <- function (data, year_range=c(1896, 2016), season='Both', medals='All', sport=list('All'), country=list('All') ){
  
  year_filter <- (df$Year >= year_range[1]) & (df$Year <= year_range[2])
  season_filter <- if_else(season =="Both",TRUE,df$Season == season)
  medal_filter <- if_else(medals =="All",TRUE,df$Medal == medals)
  sport_filter <- if_else("All" %in% sport, TRUE, df$Sport %in% sport)
  country_filter <- if_else("All" %in% country, TRUE, df$Team %in% country)
  
  data <- data|>
    filter(year_filter,season_filter,medal_filter,sport_filter,country_filter)
  
  return (data)
}


app$callback(
    list(output('hist', 'figure'),
        output('hist2', 'figure'),
        output('hist3', 'figure')),
    list(input('year_range', 'value'),
        input('sport', 'value'),
        input('country', 'value'),
        input('medals', 'value'),
        input('season', 'value')),
    function(year_range, sport, country, medals, season){
      filtered = filter_data(df, year_range=year_range, sport=sport, country=country, medals=medals, season=season)
      fig1 <- ggplot(filtered, aes(x=Height,fill = Sex))+ geom_histogram(bins=50,alpha = 0.5,position = 'identity')
      fig2 <- ggplot(filtered, aes(x=Weight,fill = Sex))+ geom_histogram(bins=50,alpha = 0.5,position = 'identity')
      fig3 <- ggplot(filtered, aes(x=Age,fill = Sex))+ geom_histogram(bins=50,alpha = 0.5,position = 'identity')
      return (list(fig1,fig2,fig3))
    }
)




app$run_server(debug = T)