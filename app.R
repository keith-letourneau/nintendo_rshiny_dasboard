library(shiny)
library(shinythemes)
library(gridExtra)
library(ggplot2)
library(cluster)
library(factoextra)
library(psych)
library(plotly)
library(shinyWidgets)
library(shinydashboard)

# Load data and run kmeans algorithm
df <- read.csv("nintendo_case_study.csv")

multi_factor <- df[,c("video_game_play","income", "comp", "res_impor","age")]
multi_factor_scaled <- scale(multi_factor)
multi_factor_6<-kmeans(multi_factor_scaled,centers=6,nstart=25)

df <- data.frame(df, multi_factor_cat = factor(multi_factor_6$cluster))

df$female = ifelse(df$gender=="Female","Female","Male")

# Define UI for application
ui <- fluidPage(theme = shinytheme("slate"),
        titlePanel(
          fluidRow(
            column(6, tags$div("Segmenting the Market for Nintendo", tags$br(),
                      tags$h5("Market consumer data is simulated data designed to mirror current Nintendo consumer landscape. All data was cleaned and visualized 25 May 2021 using R and RShiny to create this dashboard."))),
            column(6, div(img(src="nintendo2.png",height="40%", width="40%"),
                              style="text-align: right;")),
            br(),
  )
),
      
# Sidebar with a user inputs
      sidebarLayout(
        sidebarPanel(
          chooseSliderSkin("Flat"),
          setSliderColor("#d8081b",1),
          sliderInput("p","Level of User Competitiveness:",min = 0,max = 100,value = c(0,90)),
          br(),
          div(img(src="comp.png",height="20%", width="20%"),
                  style="text-align: center;"),
          replicate(10,br(), simplify = FALSE),
          selectInput("h", "Choose Variable for Density Plot to Analyze Segments:", choices=c('income','age', 'comp', 'num_of_purchases')),
          replicate(6,br(), simplify = FALSE),
          checkboxGroupInput("variable", "Histograms of Video Game Behavior For All Users:",
                             c("Hours Spent Gaming in a Year" = "video_game_play",
                               "Hours Spent Per Session" = "video_game_hours",
                               "Level of Competitiveness" = "comp"), selected="video_game_play"),
          replicate(1,br(), simplify = FALSE)
                                                                               
  
),

# Show a plot of the generated distribution
      mainPanel(
        plotlyOutput("distPlot"),
        br(),
        br(),
        fluidRow(
          column(6, plotlyOutput(outputId = "p2", width  = "425px",height = "325px")),  
          column(6, plotlyOutput(outputId = "p3", width  = "440px",height = "325px")),
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
    output$distPlot <- renderPlotly({
        
        p1 <- ggplot(df,aes(x=comp,y=num_of_purchases,colour=multi_factor_cat))+
                geom_point(alpha=.7) +
                geom_point(size=0.8) +
                labs(x = 'Level Competitiveness',
                     y = 'Number of Purchases',
                     title = 'Analyzing Segments Based on Number of Purchases') + 
                xlim(input$p) +
                theme_classic() +
                scale_color_discrete(name ="Segment") +
                theme(panel.background = element_rect(fill="#1c1e22", color="#1c1e22")) +
                theme(plot.background = element_rect(fill="#1c1e22", color="#1c1e22")) +
                theme(text=element_text(color="white"),axis.text=element_text(color="white")) +
                theme(legend.background = element_rect(fill="#1c1e22"))
        
        ggplotly(p1)
    })
    
    output$p2 <- renderPlotly({
      
        p2 <- ggplot(df, aes_string(x=input$h, fill=df$multi_factor_cat)) + geom_density(fill="#609bff", alpha=0.7) +
                geom_density(alpha=0.4) +
                theme_classic() +
                labs(x=input$h,
                     y='Density') +
                theme(panel.background = element_rect(fill="#1c1e22", color="#1c1e22")) +
                theme(plot.background = element_rect(fill="#1c1e22", color="#1c1e22")) +
                theme(text=element_text(color="white"),axis.text=element_text(color="white")) +
                theme(legend.background = element_rect(fill="#1c1e22")) +
                theme(axis.text.y=element_blank(),
                      axis.ticks.y=element_blank())
      
        ggplotly(p2)
      
    })
    
    output$p3 <- renderPlotly({
      
        p3 <- ggplot(df, aes_string(x=input$variable)) + geom_histogram(alpha=0.7, fill="#609bff")+
                theme_classic() +
                labs(x=input$variable,
                     y='Count') +
                theme(panel.background = element_rect(fill="#1c1e22", color="#1c1e22")) +
                theme(plot.background = element_rect(fill="#1c1e22", color="#1c1e22")) +
                theme(text=element_text(color="white"),axis.text=element_text(color="white")) +
                theme(legend.background = element_rect(fill="#1c1e22"))
      
        ggplotly(p3)
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)