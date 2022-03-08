library(dash)
library(dashHtmlComponents)
library(readr)
library(dplyr)
library(tidyr)
library(eList)
library(plotly)

df = read_csv('data/clean_data.csv')
# df = read_csv(here::here('data', 'clean_data.csv'))

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
                  
                  marks = List(for (i in seq(1896, 2016, 8)) i = list("label" = sprintf('%s',i), 'style'= list('transform'= 'rotate(90deg)', 'color'= 'white')) ),
                  
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
  
  data <- data%>%
    filter((Year >= year_range[1] & Year <= year_range[2]) & 
          (if (season =='Both') TRUE else Season ==season ) &
          (if (medals =='All') TRUE else Medal ==medals) &
          (if("All" %in% sport) TRUE else Sport %in% sport)&
          (if("All" %in% country) TRUE else Team %in% country))
  
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
      filtered <- filtered %>%
        group_by(ID,Games)%>%
        summarise(Age = mean(Age), Height = mean(Height), Weight = mean(Weight), Sex = first(Sex))%>%
        unnest(cols = c(Sex))%>% 
        distinct()
    
      
      fig1 <- ggplot(filtered, aes(x=Height,fill = Sex))+ 
        geom_histogram(bins=50,alpha = 0.5,position = 'identity')+
        labs(x="Height (cm)", title = "Distribution of Heights")+
        scale_y_continuous(labels = scales::label_number_si())+
        theme(
          panel.background = element_rect(fill = "transparent"), # bg of the panel
          plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
          panel.grid.major.x = element_blank() ,
          axis.title.x = element_text(colour = "white"),
          axis.title.y = element_text(colour = "white"),
          axis.text.x = element_text(colour = "white"),
          axis.text.y = element_text(colour = "white"),
          plot.title = element_text(colour = "white"),
          legend.title = element_text(colour = "white"),
          legend.text = element_text(colour = "white"),
          legend.background = element_rect(fill = "transparent"), # get rid of legend bg
          legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
        )
      fig2 <- ggplot(filtered, aes(x=Weight,fill = Sex))+ geom_histogram(bins=50,alpha = 0.5,position = 'identity')+
        geom_histogram(bins=50,alpha = 0.5,position = 'identity')+
        labs(x="Weight (kg)", title = "Distribution of Weights")+
        scale_y_continuous(labels = scales::label_number_si())+
        theme(
          panel.background = element_rect(fill = "transparent"), # bg of the panel
          plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
          panel.grid.major.x = element_blank() ,
          axis.title.x = element_text(colour = "white"),
          axis.title.y = element_text(colour = "white"),
          axis.text.x = element_text(colour = "white"),
          axis.text.y = element_text(colour = "white"),
          plot.title = element_text(colour = "white"),
          legend.title = element_text(colour = "white"),
          legend.text = element_text(colour = "white"),
          legend.background = element_rect(fill = "transparent"), # get rid of legend bg
          legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
        )
      fig3 <- ggplot(filtered, aes(x=Age,fill = Sex))+ geom_histogram(bins=50,alpha = 0.5,position = 'identity')+
        geom_histogram(bins=50,alpha = 0.5,position = 'identity')+
        labs(x="Age (years)", title = "Distribution of Age")+
        scale_y_continuous(labels = scales::label_number_si())+
        theme(
          panel.background = element_rect(fill = "transparent"), # bg of the panel
          plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
          panel.grid.major.x = element_blank() ,
          axis.title.x = element_text(colour = "white"),
          axis.title.y = element_text(colour = "white"),
          axis.text.x = element_text(colour = "white"),
          axis.text.y = element_text(colour = "white"),
          plot.title = element_text(colour = "white"),
          legend.title = element_text(colour = "white"),
          legend.text = element_text(colour = "white"),
          legend.background = element_rect(fill = "transparent"), # get rid of legend bg
          legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
        )
      return(list(ggplotly(fig1),ggplotly(fig2),ggplotly(fig3)))
    }
)



# app$run_server(debug = T )
app$run_server(host= '0.0.0.0')