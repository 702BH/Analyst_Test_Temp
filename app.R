# Final App
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(shinydashboard)
library(shinyWidgets)
library(plotly)

# read in data
count_engage <- read.csv("user_count_engagement.csv", header = TRUE)


bind_data2 <- read.csv("bind_data_2.csv", header = TRUE)



color_map <- c(China = "#ff7f0e", Vietnam = "#9467bd", Canada = "#2ca02c")



color_map2 <- data.frame(country = unique(count_engage$client_CountryOrRegion),
                         color = c("#ff7f0e", "#9467bd","#2ca02c"))


# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "All41 Test Dashboard", tags$li(class = "dropdown",tags$img(src="all41logo.png", width = '200', height = '60'), style = "padding-right:1900px;")),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard Introduction", tabName = "intro", icon = icon("columns", lib = "font-awesome")),
      menuItem("Main Dashboard", tabName = "main", icon = icon("columns", lib = "font-awesome")),
      menuItem("Part 2 - A/B Test", tabName = "ab", icon = icon("columns", lib = "font-awesome"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "intro",
        includeMarkdown("dashboardintro.rmd")
        ),
      tabItem(
        tabName = "main",
        fluidRow(
          box(width = 3,
              p("Please select a country to filter on:", style = 'font-size:22px;')),
          box(width = 3, tags$style(".my-class {font-size:150%;}"),
              pickerInput("Country", choices = unique(count_engage$client_CountryOrRegion), selected = unique(count_engage$client_CountryOrRegion),
                          options = pickerOptions(`actions-box` = TRUE, style = "my-class"), choicesOpt = list(style = rep_len("font-size: 150%;",length(unique(count_engage$client_CountryOrRegion)))), multiple = T))
        ),
        fluidRow(
          valueBoxOutput("TotalUser"),
          valueBoxOutput("AverageSpinsPerUser"),
          valueBoxOutput("MostPopularDevice")
        ),
        fluidRow(
          splitLayout(cellWidths = c("33%", "33%", "33%"), plotlyOutput("TotalUserPlot"),
                      plotlyOutput("SpinsPerUser"), plotlyOutput("MostPopularDevicePlot"))
        ),
        br(),
        br(),
        br(),
        br(),
        br(),
        fluidRow(
          valueBoxOutput("MostPopularOS"),
          valueBoxOutput("MostPopularBrowser"),
          valueBoxOutput("MostPopularMode")
        ),
        fluidRow(
          splitLayout(cellWidths = c("33%", "33%", "33%"), plotlyOutput("MostPopularOSPlot"),
                      plotlyOutput("MostPopularBrowserPlot"), plotlyOutput("MostPopularModePlot"))),
        br(),
        br(),
        br(),
        br(),
        br(),
        fluidRow(
          column(width = 6, valueBoxOutput("MostPopularSpinMethod", width = NULL)),
          column(width = 6, valueBoxOutput("MostPopularStopMethod", width = NULL))),
        fluidRow(
          splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("MostPopularSpinMethodPlot"),
                      plotlyOutput("MostPopularStopMethodPlot")))
      ),
      tabItem(
        tabName = "ab",
        includeMarkdown("Data_Test_Part2.rmd")
      )
        
      )
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # filter both datasets for desired countries
  count_engage_react <- reactive(
    count_engage %>%
      filter(client_CountryOrRegion %in% input$Country)
  )
  
  bind_data_react <- reactive(
    bind_data2 %>%
      filter(client_CountryOrRegion %in% input$Country)
  )
  
  
  
  # fluidrow 1 value boxes
  output$TotalUser <- renderValueBox({
    valueBox(
      sum(count_engage_react()$user_count),
      subtitle = "Total Users"
      
    )
  })
  
  output$AverageSpinsPerUser <- renderValueBox({
    valueBox(
      round(mean(count_engage_react()$spin_per_user),1),
      subtitle = "Spins Per User"
    )
  })
  
  output$MostPopularDevice <- renderValueBox({
    valueBox(
      bind_data_react() %>%
        filter(variable == "client_Model") %>%
        group_by(subvariable) %>%
        summarise(count = sum(count)) %>%
        arrange(desc(count)) %>%
        slice(1) %>%
        select(subvariable),
      subtitle = "Most Popular Device Model"
    )
  })
  
  
  # fluidrow 2 plot outputs
  output$TotalUserPlot <- renderPlotly({
    
    plot_data <- count_engage_react() %>%
      select(client_CountryOrRegion, user_count)
    
    p <-plot_ly(plot_data, labels = ~client_CountryOrRegion, values = ~user_count,
                textposition = "inside",
                textinfo = 'label+percent',
                marker = list(colors = color_map[plot_data$client_CountryOrRegion])) %>%
      add_pie(hole = 0.5) %>%
      layout(title = list(text ="Total User Count", font = list(size = 15)), showlegend = F,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
             paper_bgcolor = 'rgba(0,0,0,0)',
             plot_bgcolor = 'rgba(0,0,0,0)',
             annotations = list(text = sum(plot_data$user_count), "showarrow" = F), font = list(size = 18))
    
    p
    
  })
  
  output$SpinsPerUser <- renderPlotly({
    
    if(length(input$Country) == 1){
      
      p <- plot_ly(count_engage_react(), x = ~client_CountryOrRegion, y = ~spin_per_user, type = "bar", text =~spin_per_user, textposition = "outside",
                   marker = list(color = color_map[count_engage_react()$client_CountryOrRegion])) %>%
        layout(title = list(text= paste("Spins Per User for", input$Country), font = list(size = 15)), showlegend = F, bargap = 0.5,
               xaxis = list(title = "",showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE, tickfont = list(size = 15)),
               yaxis = list(title = "",showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE, categoryorder = "total descending", tickfont = list(size = 15)),
               paper_bgcolor = 'rgba(0,0,0,0)',
               plot_bgcolor = 'rgba(0,0,0,0)')
      p
      
      
    }else{
      
      p <- plot_ly(count_engage_react(), x = ~spin_per_user, y = ~client_CountryOrRegion, type = "bar", orientation = "h", text =~spin_per_user, textposition = "outside",
                   marker = list(color = color_map[count_engage_react()$client_CountryOrRegion])) %>%
        layout(title = list(text = "Spins Per User for Each Country", font = list(size = 15)), showlegend = F,
               xaxis = list(title = "",showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE, tickfont = list(size = 15)),
               yaxis = list(title = "",showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE, categoryorder = "total descending", tickfont = list(size = 15)),
               paper_bgcolor = 'rgba(0,0,0,0)',
               plot_bgcolor = 'rgba(0,0,0,0)')
      p
      
    }
    
    
  })
  
  
  
  output$MostPopularDevicePlot <- renderPlotly({
    
    if(length(input$Country) == 1){
      
      temp_plot_data <- bind_data_react() %>%
        filter(variable == "client_Model")
      
      p <- plot_ly(temp_plot_data, x = ~count, y = ~label, type = "bar",  orientation = "h", marker = list(color = color_map[count_engage_react()$client_CountryOrRegion])) %>%
        layout(title = list(text = paste("Top 5 Device Models for", input$Country), font = list(size = 15)), showlegend = F,
               xaxis = list(title = "",showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE, tickfont = list(size = 15)),
               yaxis = list(title = "",showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE, tickfont = list(size = 15), categoryorder = "total descending"),
               paper_bgcolor = 'rgba(0,0,0,0)',
               plot_bgcolor = 'rgba(0,0,0,0)')
      
      p
      
      
    }else{
      temp_plot_data <- bind_data_react() %>%
        filter(variable == "client_Model") %>%
        group_by(client_CountryOrRegion) %>%
        slice(1) 
      
      p <- plot_ly(
        data=temp_plot_data,
        x = ~label,
        y = ~count,
        type = "bar",
        color = ~reorder(client_CountryOrRegion, -count),
        colors = color_map[temp_plot_data$client_CountryOrRegion],
        text =~client_CountryOrRegion, textposition = "outside"
      ) %>%
        layout(title = "The Most Popular Model in Each Country", showlegend = F,
               xaxis = list(title = "",showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE, tickfont = list(size = 15)),
               yaxis = list(title = "",showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE, tickfont = list(size = 15)),
               paper_bgcolor = 'rgba(0,0,0,0)',
               plot_bgcolor = 'rgba(0,0,0,0)')
      p
      
      
    }
    
  })
  
  
  # Fluid Row 3 value boxes
  
  output$MostPopularOS <- renderValueBox({
    valueBox(
      bind_data_react() %>%
        filter(variable == "client_OS") %>%
        group_by(subvariable) %>%
        summarise(count = sum(count)) %>%
        arrange(desc(count)) %>%
        slice(1) %>%
        select(subvariable),
      subtitle = "Most Popular Operating System"
    )})
  
  output$MostPopularBrowser <- renderValueBox({
    valueBox(
      bind_data_react() %>%
        filter(variable == "client_Browser") %>%
        group_by(subvariable) %>%
        summarise(count = sum(count)) %>%
        arrange(desc(count)) %>%
        slice(1) %>%
        select(subvariable),
      subtitle = "Most Popular Browswer"
    )})
  
  
  output$MostPopularMode <- renderValueBox({
    valueBox(
      bind_data_react() %>%
        filter(variable == "spin_play_mode") %>%
        group_by(subvariable) %>%
        summarise(count = sum(count)) %>%
        arrange(desc(count)) %>%
        slice(1) %>%
        select(subvariable),
      subtitle = "Most Popular Mode to Play Spin (Portrait or Landscape)"
    )})
  
  
  # Fluid Row 4 plot outputs
  
  output$MostPopularOSPlot <- renderPlotly({
    
    if(length(input$Country) == 1){
      
      temp_plot_data <- bind_data_react() %>%
        filter(variable == "client_OS")
      
      p <- plot_ly(temp_plot_data, x = ~label, y = ~count, type = "bar", marker = list(color = color_map[count_engage_react()$client_CountryOrRegion])) %>%
        layout(title = list(text= paste("Top 5 Operating Systems for", input$Country), font = list(size = 15)), showlegend = F,
               xaxis = list(title = "",showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE, categoryorder = "total descending", tickfont = list(size = 15)),
               yaxis = list(title = "",showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE, tickfont = list(size = 15)),
               paper_bgcolor = 'rgba(0,0,0,0)',
               plot_bgcolor = 'rgba(0,0,0,0)')
      
      p
      
      
    }else{
      temp_plot_data <- bind_data_react() %>%
        filter(variable == "client_OS") %>%
        group_by(client_CountryOrRegion) %>%
        slice(1)
      
      p <- plot_ly(
        data=temp_plot_data,
        x = ~reorder(label, count),
        y = ~count,
        type = "bar",
        color = ~reorder(client_CountryOrRegion, -count),
        colors = color_map[temp_plot_data$client_CountryOrRegion],
        text =~client_CountryOrRegion, textposition = "outside"
      ) %>%
        layout(title = "The Most Popular Operating System in Each Country", showlegend = F,
               xaxis = list(title = "",showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE, categoryorder = "total descending", tickfont = list(size = 15)),
               yaxis = list(title = "",showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE, tickfont = list(size = 15)),
               paper_bgcolor = 'rgba(0,0,0,0)',
               plot_bgcolor = 'rgba(0,0,0,0)')
      p
      
      
    }
    
  })
  
  
  
  output$MostPopularBrowserPlot <- renderPlotly({
    
    if(length(input$Country) == 1){
      
      temp_plot_data <- bind_data_react() %>%
        filter(variable == "client_Browser")
      
      p <- plot_ly(temp_plot_data, x = ~label, y = ~count, type = "bar", marker = list(color = color_map[count_engage_react()$client_CountryOrRegion])) %>%
        layout(title = list(text= paste("Top 5 Browsers for", input$Country), font = list(size = 15)), showlegend = F,
               xaxis = list(title = "",showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE, categoryorder = "total descending", tickfont = list(size = 12)),
               yaxis = list(title = "",showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE, tickfont = list(size = 15)),
               paper_bgcolor = 'rgba(0,0,0,0)',
               plot_bgcolor = 'rgba(0,0,0,0)')
      
      p
      
      
    }else{
      temp_plot_data <- bind_data_react() %>%
        filter(variable == "client_Browser") %>%
        group_by(client_CountryOrRegion) %>%
        slice(1) 
      
      p <- plot_ly(
        data=temp_plot_data,
        x = ~label,
        y = ~count,
        type = "bar",
        color = ~reorder(client_CountryOrRegion, -count),
        colors = color_map[temp_plot_data$client_CountryOrRegion],
        text =~client_CountryOrRegion, textposition = "outside"
      ) %>%
        layout(title = list(text = "The Most Popular Browser for Each Country", font = list(size = 15)), showlegend = F, bargap = 0.2,
               xaxis = list(title = "",showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE, categoryorder = "total descending", tickfont = list(size = 12)),
               yaxis = list(title = "",showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE, tickfont = list(size = 15)),
               paper_bgcolor = 'rgba(0,0,0,0)',
               plot_bgcolor = 'rgba(0,0,0,0)')
      p
      
      
    }
    
  })
  
  
  output$MostPopularModePlot <- renderPlotly({
    
    if(length(input$Country) == 1){
      
      temp_plot_data <- bind_data_react() %>%
        filter(variable == "spin_play_mode")
      
      p <-plot_ly(temp_plot_data, type = "pie", pull = 0.1, labels = ~subvariable, values = ~count,
                  textposition = "inside",
                  textinfo = 'label+percent', marker = list(colors = color_map[bind_data_react()$client_CountryOrRegion])) %>%
        layout(title = list(text= paste("Most Popular way to Play Spin (Portrait or Landscape) for", input$Country), font = list(size = 15)), showlegend = F,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
               paper_bgcolor = 'rgba(0,0,0,0)',
               plot_bgcolor = 'rgba(0,0,0,0)') 
      
      
    }else{
      temp_plot_data <- bind_data_react() %>%
        filter(variable == "spin_play_mode") %>%
        group_by(subvariable) %>%
        summarise(total = sum(count)) %>%
        arrange(desc(total))
      
      p <-plot_ly(temp_plot_data, type = "pie", pull = 0.1, labels = ~subvariable, values = ~total,
                  textposition = "inside", 
                  textinfo = 'label+percent',
                  marker = list(colors = "black")) %>%
        layout(title = list(text = "Most Popular Way to Play Spin (Portrait or Landscape) for All Selected Countries", font = list(size=15)), showlegend = F,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE, tickfont = list(size = 15)),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE, tickfont = list(size = 15)),
               paper_bgcolor = 'rgba(0,0,0,0)',
               plot_bgcolor = 'rgba(0,0,0,0)') 
      
      p
      
      
    }
    
  })
  
  
  # Fluid row 5 render boxes
  output$MostPopularSpinMethod <- renderValueBox({
    valueBox(
      bind_data_react() %>%
        filter(variable == "spinMethod") %>%
        group_by(subvariable) %>%
        summarise(count = sum(count)) %>%
        arrange(desc(count)) %>%
        slice(1) %>%
        select(subvariable),
      subtitle = "Most Popular Spin Method"
    )})
  
  output$MostPopularStopMethod <- renderValueBox({
    valueBox(
      bind_data_react() %>%
        filter(variable == "stopMethod") %>%
        group_by(subvariable) %>%
        summarise(count = sum(count)) %>%
        arrange(desc(count)) %>%
        slice(1) %>%
        select(subvariable),
      subtitle = "Most Popular Stop Method"
    )})
  
  
  # Fluid row 6 Render Output plots
  output$MostPopularSpinMethodPlot <- renderPlotly({
    
    if(length(input$Country) == 1){
      
      temp_plot_data <- bind_data_react() %>%
        filter(variable == "spinMethod")
      
      p <- plot_ly(temp_plot_data, x = ~label, y = ~count, type = "bar", marker = list(color = color_map[count_engage_react()$client_CountryOrRegion])) %>%
        layout(title = list(text= paste("Most Popular Spin Method for", input$Country), font = list(size = 15)), showlegend = F,
               xaxis = list(title = "",showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE, categoryorder = "total descending", tickfont = list(size = 15)),
               yaxis = list(title = "",showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE, tickfont = list(size = 15)),
               paper_bgcolor = 'rgba(0,0,0,0)',
               plot_bgcolor = 'rgba(0,0,0,0)')
      
      p
      
      
    }else{
      temp_plot_data <- bind_data_react() %>%
        filter(variable == "spinMethod") %>%
        group_by(client_CountryOrRegion) %>%
        slice(1) 
      
      p <- plot_ly(
        data=temp_plot_data,
        x = ~label,
        y = ~count,
        type = "bar",
        color = ~reorder(client_CountryOrRegion, -count),
        colors = color_map[temp_plot_data$client_CountryOrRegion],
        text =~client_CountryOrRegion, textposition = "outside"
      ) %>%
        layout(title = list(text = "The Most Popular Spin Method for Each Country",font = list(size = 15)), showlegend = F, bargap = 0.5,
               xaxis = list(title = "",showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE, categoryorder = "total descending", tickfont = list(size = 15)),
               yaxis = list(title = "",showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE, tickfont = list(size = 15)),
               paper_bgcolor = 'rgba(0,0,0,0)',
               plot_bgcolor = 'rgba(0,0,0,0)')
      p
      
      
    }
    
  })
  
  
  output$MostPopularStopMethodPlot <- renderPlotly({
    
    if(length(input$Country) == 1){
      
      temp_plot_data <- bind_data_react() %>%
        filter(variable == "stopMethod")
      
      p <- plot_ly(temp_plot_data, x = ~label, y = ~count, type = "bar", marker = list(color = color_map[count_engage_react()$client_CountryOrRegion])) %>%
        layout(title = list(text= paste("Most Popular Stop Method for", input$Country), font = list(size = 15)), showlegend = F,
               xaxis = list(title = "",showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE, categoryorder = "total descending", tickfont = list(size = 15)),
               yaxis = list(title = "",showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE, tickfont = list(size = 15)),
               paper_bgcolor = 'rgba(0,0,0,0)',
               plot_bgcolor = 'rgba(0,0,0,0)')
      
      p
      
      
    }else{
      temp_plot_data <- bind_data_react() %>%
        filter(variable == "stopMethod") %>%
        group_by(client_CountryOrRegion) %>%
        slice(1) 
      
      p <- plot_ly(
        data=temp_plot_data,
        x = ~label,
        y = ~count,
        type = "bar",
        color = ~reorder(client_CountryOrRegion, -count),
        colors = color_map[temp_plot_data$client_CountryOrRegion],
        text =~client_CountryOrRegion, textposition = "outside"
      ) %>%
        layout(title = list(text = "The Most Popular Stop Method for Each Country",font = list(size = 15)), showlegend = F, bargap = 0.5,
               xaxis = list(title = "",showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE, categoryorder = "total descending", tickfont = list(size = 15)),
               yaxis = list(title = "",showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE, tickfont = list(size = 15)),
               paper_bgcolor = 'rgba(0,0,0,0)',
               plot_bgcolor = 'rgba(0,0,0,0)')
      p
      
      
    }
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

