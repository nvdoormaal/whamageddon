#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load packages
library(shiny)
library(tidyverse)
library(RColorBrewer)
library(scales)
library(plotly)
library(shinythemes)
library(ggthemes)
library(lubridate)


# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme("darkly"),
    
    # Application title
    titlePanel("WHAMAGEDDON"),
    
    fluidRow(
        column(12,
               textOutput("CountDownWham"))
    ),
    
    fluidRow(
        column(9,
               plotlyOutput("WhamPlot", height = "600px")
        ),
        column(3,
               tableOutput("WinTable"),
               column(12,
                      tableOutput("SurvivalTable"),
                      column(12,
                             tableOutput("DeathlyDay")
                             )
                      )
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Read data
    Data <- read_csv("./Data/Whamageddon.csv",
                     col_types = cols( ID = col_integer(),
                                       When = col_date(format = "%d-%m-%Y %H:%M"),
                                       Hit = col_integer(),
                                       Comments = col_character()
                     )
    )
    # Data cleaning
    DataComplete <- Data %>%
        complete(When = c(seq(from = as.Date("2020-12-01"),
                              to = as.Date("2020-12-24"), by = "day"),
                          seq(from = as.Date("2021-12-01"), 
                              to = as.Date("2021-12-24"), by = "day")), 
                 Who) %>%
        replace_na(list(Hit =  0)) %>%
        select(c(When, Who, Hit)) %>% 
        mutate(
            Year = as.integer(year(When)),
        )
    
    DataCum <- DataComplete %>%
        group_by(Year, When, Who) %>%
        summarise(Hit = sum(Hit)) %>%
        group_by(Year, Who) %>%
        mutate(
            `Wham Count` = if_else(When > Sys.Date(),
                                   NaN,
                                   cumsum(Hit)
            )
        )
    
    # Create Wham Plot
    # First ggplot2
    p1 <- ggplot(data = DataCum, aes(x = When, y = `Wham Count`)) + 
        geom_step(aes(colour = Who), lwd = 1.5) + 
        scale_colour_brewer(palette = "Okabe-Ito") +
        scale_x_date("", date_breaks = "2 days",
                     date_labels = "%b-%d", expand = c(0.02,0.02)) +
        scale_y_continuous("Wham count", limits = c(0,16), breaks = seq(0,16,2)) +
        facet_wrap(~Year, scales = "free_x", ncol = 2) + 
        ggtitle("Wham counts per year") +
        theme_bw() +
        theme(legend.position = c(0.15, 0.85),
              legend.direction = "horizontal",
              legend.background  = element_rect(color = "black", size = 0.75),
              axis.text.x = element_text(angle = 60, hjust = 1),
              axis.title.y = element_text(vjust = -2.5)
              #plot.background = element_rect(fill = "grey50")
        )
    
    # Create the plotly
    ply1 <- ggplotly(p1) %>%
        layout(legend = list(orientation = "h", x = 0.015, y = 0.95),
               margin = list(l = 75, r = 0, b = 75, t = 60)
                ) %>% 
        config(displayModeBar = "static", displaylogo = FALSE, 
               modeBarButtonsToRemove = list("sendDataToCloud", "toImage", 
                                             "autoScale2d", "resetScale2d",
                                             "hoverClosestCartesian", 
                                             "hoverCompareCartesian", "select2d", 
                                             "lasso2d", "zoomIn2d", "zoomOut2d",
                                             "toggleSpikelines", "pan2d", "zoom2d")
        )
    
    output$WhamPlot <- renderPlotly({
        ply1
    })
    
    # Create survival table
    SurvTbl <- DataCum %>% 
        subset(When <= Sys.Date()) %>% 
        count(Year, Who, `Wham Count`) %>% 
        ungroup(Year) %>% 
        slice_max(order_by = n) %>% arrange(-n, `Wham Count`) %>%
        select(c(n, Who, Year))
    names(SurvTbl) <- c("Number of Days", "Name", "Year")
    
    output$SurvivalTable <- renderTable(SurvTbl, hover = TRUE, spacing = "s",
                                        caption = "Longest period w/o getting WHAMMED", 
                                        caption.placement = getOption("xtable.caption.placement", "top"))
    
    # Create win table
    output$WinTable <- renderTable(
        DataCum %>%  subset(When <= Sys.Date()) %>% 
            ungroup() %>% 
            group_by(Who) %>% 
            slice(which.max(`Wham Count`)) %>% 
            arrange(-`Wham Count`) %>%
            select(c(`Total hits` = `Wham Count`, Name = Who, Year = Year)),
        hover = TRUE, spacing = "s", caption = "Max WHAM Count",
        caption.placement = getOption("xtable.caption.placement", "top")
    )
    
    # Find Deathliest Day
    output$DeathlyDay <- renderTable(
        DataCum %>% ungroup() %>% 
            subset(When <= Sys.Date()) %>% 
            group_by(Year, When) %>% 
            summarise(Hits = sum(Hit)) %>% 
            slice_max(order_by = Hits) %>% 
            arrange(-Year) %>% 
            ungroup() %>% 
            select(When, Hits) %>% 
            mutate(
                When = as.character(format(When, format = "%d %b %Y"))
            ),
        hover = TRUE, spacing = "s", caption = "Deathliest days per year",
        caption.placement = getOption("xtable.caption.placement", "top")
    )
    
    # Create countdown for the next Whamageddon
    DaysUntilWham <- as.integer(as.Date("2022-12-01") - Sys.Date())
    
    output$CountDownWham <- renderText(
        if_else(DaysUntilWham <= 0,
                paste("WHAMAGEDDON", year(Sys.Date()), "IS ON!"),
                paste("Only", DaysUntilWham, "days until the next WHAMAGEDDON!")
    )
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
