setwd("C:\\Users\\laleh\\documents")
suppressMessages(extrafont::loadfonts(device="win"))
suppressMessages(library(shiny))
suppressMessages(library(shinydashboard))
suppressMessages(library(shinyjs))
suppressMessages(library(shinyWidgets))
suppressMessages(library(shinyjqui))
suppressMessages(library(rintrojs))
suppressMessages(library(readxl))
suppressMessages(library(data.table))
suppressMessages(library(tidyverse))
suppressMessages(library(ggthemes))
suppressMessages(library(plotly))
suppressMessages(library(scales))
suppressMessages(library(rAmCharts))
suppressMessages(library(highcharter))
suppressMessages(library(htmlwidgets))

data <- read_xlsx("d.xlsx")
data$yy_mm <- as.Date(data$yy_mm)
qa <- mutate(data, Enroute1 = Open, ee1 = Enroute1 + E1, Enroute2 = ee1, ee2 = Enroute2 + E2,
             Full_Dis = ee2, ef = Full_Dis - full, With_Dis = ef, ew = With_Dis - with,
             Without_Dis = ew, ewo = Without_Dis - without)
dtqa <- as.data.table(qa)
start <- melt(dtqa[, c(1:9, 16, 18, 20, 22, 24)], 1:9, value.name = "start")
end <- melt(dtqa[, c(1:9, 17, 19, 21, 23, 25)], 1:9, value.name = "end")
s <- cbind(start, end = end$end)
s <- cbind(s, value = melt(as.data.table(data), c(1:10))$value)
s <- s[,c(1:10, 13, 11, 12)]
a <- s[variable %in% c("Enroute1", "Enroute2"),]
b <- s[variable %in% c("Full_Dis", "With_Dis", "Without_Dis"),]

############
header <- dashboardHeader(
            titleWidth = 300,
            title = "FAST Dashboard"
          )

sidebar <- dashboardSidebar(
  width = 300,
  sidebarMenu(
    menuItem(
      "Widgets",
      tabName = "widgets",
      icon = icon("eyedropper"),
      badgeLabel = "new",
      badgeColor = "green"
    ),
     
    menuItem(
      "Inputs",
      icon = icon("bar-chart-o"),
      sliderInput(
        inputId = "period",
        label = "Period: ",
        min = min(s$yy_mm),
        max = max(s$yy_mm),
        value = c(as.Date("2013-01-01"), as.Date("2015-01-01")),
        timeFormat = "%b-%y"
      ),
       
      selectInput(
        inputId = "product",
        label = "Choose Product:",
        unique(data$Art),
        width = '95%'
      )
    ),
      
    menuItem(
      "Info",
      tabName = "info",
      icon = icon("info-circle")
    )
  )
)  

body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "info",
      h2("FAST Dashboard for team analysis"),
      tags$div(class="header", checked=NA,
        list(
          tags$p("Ready to take the Shiny tutorial? If so"),
          tags$a(href="shiny.rstudio.com/tutorial", "Click Here!"),
          "Thank you"
        )
      ),
      h4("Had a long day?  This app will help you find the right drink for tonight! Just use the filters below..."),
      hr(),
      span("Data source:", 
        tags$a("OpenDataBC",
             href = "https://www.opendatabc.ca/dataset/bc-liquor-store-product-price-list-current-prices")),
      br(),
      span("Learn how to build this app", a(href = "http://deanattali.com/blog/building-shiny-apps-tutorial/", "with my Shiny tutorial")),
      br(), br(),
      em(
        span("Created by", a(href = "http://deanattali.com", "Dean Attali")),
        HTML("&bull;"),
        span("Code", a(href = "https://github.com/Mohsen-laleh/fast-dashboard/tree/app", "on GitHub"))
      ),
      downloadButton("download", "Download results")
    ),
        
    tabItem(
      tabName = "widgets",
      fluidRow(
        jqui_draggabled(
          box(
            title = strong("waterfallPlot"),
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12, 
            plotOutput(
              outputId = "waterfallPlot",
              height = "750px"
            )
          )  
        ),

        jqui_draggabled(
          box(
            title = strong("bar"),
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 6, 
            plotOutput(outputId = "bar",
                       height = "600px"
            )
          )
        )
      )
    )
  )
)

server <- function(input, output) {
    
  output$waterfallPlot <- renderPlot({
    ggplot(s, aes(fill = variable))+
      scale_fill_manual(
        values = c("seagreen3", "seagreen4", "#FF40C9", "#F4FF1F", "deepskyblue2"),
        guide = guide_legend(
          title = NULL,
          direction = "horizontal"
        ) 
      ) +
      geom_rect(
        aes(x = yy_mm,
            xmin = yy_mm - 8,
            xmax = yy_mm - 0.5, 
            ymin = start,
            ymax = end
           ),
        data = a[Art == input$product]
      ) +
      geom_rect(
        aes(x = yy_mm,
            xmin = yy_mm + 0.5,
            xmax = yy_mm + 8,
            ymin = start,
            ymax = end
        ),
        data = b[Art == input$product]
      ) +
      xlim(input$period) +
      labs(
        title = paste("Waterfall plot for product: ", input$product),
        subtitle = paste("Period between: ", input$period[1] , "and", input$period[2]),
        x = "Date",
        y = "Value",
        caption = "(Designed by planning department)"
      ) +
      geom_text(
        data = a[Art == input$product & variable == "Enroute1" & value > 0],
        aes(
          x = yy_mm - 4,
          y = pmin(start, end),
          label = round(OS_Coverage, 1)
        ),
        size = 2.8,
        angle = 90,
        hjust = 1.2
      ) +
      geom_text(
        data = b[Art == input$product & variable == "Without_Dis" & value > 0],
        aes(
          x = yy_mm + 4,
          y = pmin(start, end),
          label = round(DIH, 1)
        ),
        size = 2.8,
        angle = 90,
        hjust = 1.2
      ) +
      scale_x_date(
        date_breaks = "1 month",
        date_labels = "%b-%y",
        limits = input$period,
        expand = c(0.01, 0)
      ) +
      theme(
        plot.title = element_text(
          size = 15,
          face = "bold",
          family = "Comic Sans MS",
          color = "white",
          hjust = 0.5
        ),
        plot.subtitle = element_text(
          size = 10,
          family = "Comic Sans MS",
          face = "bold",
          hjust = 0.5
        ),
        plot.caption = element_text(size = 9),
        plot.background = element_rect(
          colour = "black",
          fill = "#B0CCEB"
        ),
        axis.title.x = element_text(
          size = 10,
          face = "bold"
        ),
        axis.title.y = element_text(
          size = 10,
          face = "bold"
        ),
        axis.text.x = element_text(
          size = 9,
          angle = 90
        ), 
        axis.text.y = element_text(size = 9),
        panel.background = element_rect(
          fill = '#D1EAFF',
          colour = 'blue'
        ),
        legend.title = element_text(
          colour = "black",
          size = 10,
          face = "bold"
        ),
        legend.text = element_text(
          colour = "black",
          size = 10
        ), 
        legend.background = element_rect(
          fill = "#D1F7FF",
          size = 0.1,
          linetype = "dotted"
        ),
        legend.key = element_rect(
          fill = "grey",
          colour = "white",
          size = 1
        ),
        legend.key.width = unit(0.4, "cm"),
        legend.key.height = unit(0.4, "cm"),
        legend.position = c(.4, .99),
        legend.justification = c("left", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)
       )
    })

  output$bar <- renderPlot({
    ggplot(s,aes(fill = variable)) + 
      geom_line(
        aes(x = yy_mm,
            y = start
        )
      ) +
      xlim(input$period)
  })
}

shinyApp(ui = dashboardPage(header, sidebar, body),  server = server)
