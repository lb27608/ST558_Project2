library(shiny)
library(shinyalert)
library(tidyverse)
library(shinydashboard)

# Define UI for application that draws a histogram
ui <- fluidPage(

  titlePanel("US Superstore Sales Data Exploration"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Choose a subset of the data:"),
      radioButtons("segment",
                   label = h4("Market Segment"),
                   choices = levels(as_factor(sales_raw$Segment)),
                   selected = "all"
                  ),
      radioButtons("region",
                   label = h4("Region"),
                   choices = levels(as_factor(sales_raw$Region)),
                   selected = "all"
                  ),
      radioButtons("category",
                   label = h4("Product Category"),
                   choices = levels(as_factor(sales_raw$Category)),
                   selected = "all"
                  ),
  
      #Let user select a year
      selectizeInput("year",
                     "Choose year(s)",
                     choices=levels(as_factor(sales_raw$OrderYear)),
                     selected="all",multiple=TRUE
                    ),
      
      #Let user select a numeric variable (sales or profit)
      selectizeInput("num_var",
                     "Select sales or profit for analysis",
                     choices=c("Sales"="Sales","Profit"="Profit")
                    ),
      
      conditionalPanel(condition="input.num_var=='Sales'",
                       sliderInput("sales_range",
                                   label="Choose a range of sales values",
                                   min=min(sales_raw$Sales), max=max(sales_raw$Sales),
                                   value=20,step=10,
                                   ticks=TRUE)          
                   ),
      conditionalPanel(condition="input.num_var=='Profit'",
                       sliderInput("profit_range",
                                   label="Choose a range of profit values",
                                   min=min(sales_raw$Profit), max=max(sales_raw$Profit),
                                   value=20,step=10,
                                   ticks=TRUE)          
                  ),
      
      
      actionButton("get_subset","Subset the data")
    ),
  
    
    mainPanel(
      tabsetPanel(
        id="tabset",
        tabPanel("About",
                 img(src='dataset-cover.jpg', align = "right"),
                 h2("About the Data"),
                 HTML("This project creates an app to explore a set of sales data from US superstores downloaded from <a href='https://www.kaggle.com/datasets/juhi1994/superstore/data'>Kaggle</a>. 
                  <br><br>Using the sidebar, the can subset the data by region (West, Central, South, or East), market segment (corporate, home office, or consumer), and/or product category (furniture, office supplies, or technology)
                  The user can then choose to analyze either sales revenue or profit information for a selection of years (2014-2017).
                  <br><br> The 'Download Data' tab will display the subsetted data and allow the user to download a copy as a CSV file.
                  <br><br> On the 'Data Exploration' tab, the user can perform various analyses of the selected data, including
                  <ul><li>Frequecies for categorical variables and summary statistics for sales and/or profit</li>
                  <li>Graphical summaries of the data</li></ul>")
                  ),
        tabPanel("Download Data","This is where we will download the data."),
        tabPanel("Data Exploration")
      )
    
    )
  )
)

sales_raw <- readRDS("superstore_data.rds")

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    #Use an observeEvent() to look for the action button (get_subset)
    observeEvent(input$get_subset, {
      if(input$segment == "all"){
        seg_sub <- as.vector(levels(as.factor(sales_raw$Segment)))
      } else {
        seg_sub <- input$Segment
      }
      
      if(input$region == "all"){
        reg_sub <- as.vector(levels(as.factor(sales_raw$Region)))
      } else {
        reg_sub <- input$Region
      }
      
      if(input$cat == "all"){
        cat_sub <- as.vector(levels(as.factor(sales_raw$Category)))
      } else {
        cat_sub <- input$Category
      }
    
      #Subset data based on user selections
      subsetted_data <- sales_raw |>
        filter(
          Region %in% reg_sub,
          Segment %in% seg_sub,
          Category %in% cat_sub
        ) |> group_by(Region,State,Segment,OrderYear) |> summarize(across(c(Sales,Profit),list("sum"=sum, "mean"=mean,"median"=median,"sd"=sd),.names="{.fn}_{.col}")) 
    
      
      
      #Update the sample_corr reactive value object
      #the corr_data argument should be updated to be the subsetted_data[index,]
      #the corr_truth argument should be updated to be the correlation between 
      #the two variables selected: 
      #cor(sample_corr$corr_data |> select(corr_vars))[1,2]
      
     
    })
    
    #Create a renderPlot() object to output a scatter plot
    #Use the code below to validate that data exists,
    #(you'll need to install the shinyalert package if you don't have it)
    #and then create the appropriate scatter plot
    
    
}


# Run the application 
shinyApp(ui = ui, server = server)
