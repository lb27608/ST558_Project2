library(shiny)
library(shinyalert)
library(tidyverse)
library(geofacet)

sales_raw <- readRDS("superstore_data.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(

  titlePanel("US Superstore Sales Data Exploration"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Choose a subset of the data:"),
      radioButtons("segment",
                   label = h4("Market Segment"),
                   choices = c(levels(as_factor(sales_raw$Segment)),"All"="all"),
                   selected = "all"
                  ),
      radioButtons("region",
                   label = h4("Region"),
                   choices = c(levels(as_factor(sales_raw$Region)), "All"="all"),
                   selected = "all"
                  ),
      radioButtons("category",
                   label = h4("Product Category"),
                   choices = c(levels(as_factor(sales_raw$Category)), "All"="all"),
                   selected = "all"
                  ),
  
      #Let user select a year
      selectizeInput("years",
                     "Choose year(s)",
                     choices=c(levels(as_factor(sales_raw$OrderYear)), "All"="all"),
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
  
    #Create a main panel with 3 tabs for information, download, and data exploration
    mainPanel(
      tabsetPanel(
        id="tabset",
        
        #About tab
        tabPanel("About",
                 img(src="dataset-cover.jpg", height="30%", width="30%",align = "right"),
                 h2("About the Data"),
                 HTML("This project creates an app to explore a set of sales data from US superstores downloaded from <a href='https://www.kaggle.com/datasets/juhi1994/superstore/data'>Kaggle</a>. 
                  <br><br>Using the sidebar, the can subset the data by region (West, Central, South, or East), market segment (corporate, home office, or consumer), and/or product category (furniture, office supplies, or technology)
                  The user can then choose to analyze either sales revenue or profit information for a selection of years (2014-2017).
                  <br><br> The 'Download Data' tab will display the subsetted data and allow the user to download a copy as a CSV file.
                  <br><br> On the 'Data Exploration' tab, the user can perform various analyses of the selected data, including
                  <ul><li>Frequecies for categorical variables and summary statistics for sales and/or profit</li>
                  <li>Graphical summaries of the data</li></ul>")
                  ),
        
        #Data download tab
        tabPanel("Download Data",
                 DT::dataTableOutput('sub_table'),
                 downloadButton("downloadData", "Download")
                 ),
        
        #Data exploration tab
        tabPanel("Data Exploration",
                 plotOutput('graph1'),
                 plotOutput('graph2'),
                 plotOutput('graph3'),
                 plotOutput('graph4')
                 )
      )
    
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {

    #Use an observeEvent() to look for the action button (get_subset)
    observeEvent(input$get_subset, {
      if(input$segment == "all"){
        seg_sub <- as.vector(levels(as.factor(sales_raw$Segment)))
      } else {
        seg_sub <- as.vector(input$Segment)
      }
      
      if(input$region == "all"){
        reg_sub <- as.vector(levels(as.factor(sales_raw$Region)))
      } else {
        reg_sub <- as.vector(input$Region)
      }
      
      if(input$category == "all"){
        cat_sub <- as.vector(levels(as.factor(sales_raw$Category)))
      } else {
        cat_sub <- as.vector(input$Category)
      }
    
      if(input$years == "all"){
        year_sub <- as.vector(levels(as.factor(sales_raw$OrderYear)))
      } else {
        year_sub <- as.vector(input$years)
      }
      
      #Subset data based on user selections
      
      
    }) #observeEvent
    
  subsetted_data <- reactive(sales_raw |>
    filter(
      Region %in% reg_sub,
      Segment %in% seg_sub,
      Category %in% cat_sub,
      OrderYear %in% year_sub
    )
  )
  
  subset_stats <- reactive(subsetted_data |> 
    group_by(Region,State,Segment,OrderYear) |> 
    summarize(across(c(Sales,Profit),
                     list("sum"=sum, "mean"=mean,"median"=median,"sd"=sd),.names="{.fn}_{.col}"))
  )
  
  output$sub_table <- renderTable({
    subsetted_data()
  }) #renderTable
  
  #Create object for downloading data
  output$downloadData <- downloadHandler(
      #Assign a default filename 
      filename = function() {
         paste('data-', Sys.Date(), '.csv', sep='')
       },
      content = function(file) {
        write.csv(subsetted_data, file)
      }
   ) #downloadHandler
  
  
  #Create plots for data exploration tab
  output$graph_1 <- renderPlot({
    validate(
      need(!is.null(subsetted_data), "Please choose a subset of the data first.")
    ) 
    ggplot(subset_stats, aes(x=Region, y=sum_Sales, fill=Segment)) + 
      geom_bar(position="dodge",stat="identity") + 
      theme(plot.title = element_text(hjust = 0.5)) + 
      labs(x="Region", y="Total sales ($)", title="Total Sales by Region and Market Segment")
  })
  
  output$graph_2 <- renderPlot({
      validate(
        need(!is.null(subsetted_data), "Please choose a subset of the data first.")
      ) 
      ggplot(subset_stats, aes(x=Region, y=sum_Profit, fill=Segment)) + 
        geom_bar(position="dodge",stat="identity") + 
        theme(plot.title = element_text(hjust = 0.5)) + 
        labs(x="Region", y="Total profit ($)", title="Total Profit by Region and Market Segment")
  })
    
  output$graph_3 <- renderPlot({
      validate(
        need(!is.null(subsetted_data), "Please choose a subset of the data first.")
      ) 
      ggplot(stat_s, aes(x=Region, y=sum_Sales, fill=Segment)) + 
        geom_bar(position="dodge",stat="identity") + 
        theme(plot.title = element_text(hjust = 0.5)) + 
        labs(x="Region", y="Total sales ($)", title="Total Sales by Region, Market Segment, and Year") + 
        facet_wrap(~OrderYear)
  })

  output$graph_4 <- renderPlot({
      validate(
        need(!is.null(subsetted_data), "Please choose a subset of the data first.")
      ) 
      ggplot(sales_raw, aes(x=OrderYear, y=Sales, rank, fill = Segment)) +
        geom_bar(stat="identity") + 
        facet_geo(~ State) + 
        theme_bw()
  })
    
} #server


# Run the application 
shinyApp(ui = ui, server = server)
