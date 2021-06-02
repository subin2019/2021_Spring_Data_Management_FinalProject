# Load packages 
library(shiny)
library(tidyverse)

# data sets 
source("data.R")

# platform_cleaned
platform_new <- platform %>% 
  rename(Social_Media = "Social Media") %>%
  mutate(
    Radio = round(Radio, 2),
    Social_Media = round(Social_Media, 2),
    Sales = round(Sales, 2)
  ) %>%
  select(-Influencer) %>%
  gather(platform, budget, -Sales) %>%
  arrange(desc(Sales))

# sns_cleaned
sns_new <- sns %>%
  select(-c("Image URL", "Unnamed: 44")) %>%
  rename(platform = "Platform(in millions)", type = "Platform Type") %>%
  gather(year, usage, -c(platform, type)) %>%
  drop_na() %>%
  mutate(
    year = as.numeric(str_extract(year, "(2)(0)([01])([0-9])"))
  ) %>%
  arrange(year, platform) %>%
  group_by(year, platform) %>%
  summarise(usage = sum(usage))

# website cleaned
website_new <- website %>%
  select(-(Month:TrafficType))%>%
  mutate(
    VisitorType = str_extract(VisitorType, "([^_]+)")
  ) %>%
  rename(administrative = Administrative, 
         administrative_duration = Administrative_Duration, 
         informational = Informational, 
         informational_duration = Informational_Duration, 
         product_related = ProductRelated, 
         product_related_duration = ProductRelated_Duration, 
         visitor_type = VisitorType, 
         exit_rates = ExitRates, 
         bounce_rates = BounceRates, 
         page_values = PageValues, 
         holiday = SpecialDay, 
         weekend = Weekend, 
         revenue = Revenue
  ) %>%
  select(visitor_type, administrative:holiday, weekend, revenue)

# Average Values for Each Visitor Type
website_avg <- website_new %>%
  select(-(holiday:revenue)) %>%
  group_by(visitor_type) %>%
  summarise(
    administrative = mean(administrative), 
    administrative_duration = mean(administrative_duration), 
    informational = mean(informational), 
    informational_duration = mean(informational_duration), 
    product_related = mean(product_related), 
    product_related_duration = mean(product_related_duration), 
    exit_rates = mean(exit_rates), 
    bounce_rates = mean(bounce_rates), 
    page_values = mean(page_values)
  )

# Visit Frequency of Each Page Type
website_pg <- website_new %>%
  filter(revenue == "TRUE") %>%
  select(administrative, informational, product_related, revenue) %>%
  gather(page, count, administrative:product_related)%>%
  gather(y, value, count)

# Visit Duration on Each Page Type
website_dur <- website_new %>%
  filter(revenue == "TRUE") %>%
  select(administrative_duration, informational_duration, product_related_duration, revenue) %>%
  gather(page, duration, administrative_duration:product_related_duration) %>%
  mutate(
    page = str_extract(page, "([^_]+)")
  ) %>%
  gather(y, value, duration)
  
# Holiday Effect on Revenue
website_ho <- website_new %>%
  filter(holiday!=0) %>%
  select(holiday, revenue)%>%
  gather(y, value, holiday)

# Weekend Effect on Revenue
website_we <- website_new %>%
  filter(weekend == "TRUE") %>%
  select(weekend, revenue)%>%
  gather(y, true, weekend) %>%
  gather(x, revenue, revenue) %>%
  mutate(
    value = ifelse(revenue=="TRUE", 1, 1)
  )

# my theme
theme <- theme_bw() +
  theme(
    axis.title = element_text(face = "bold")
  )


# User interface
ui <- fluidPage(
  
  fluidRow(
    titlePanel("Platform Advertisement Budget and Sales"),
    sidebarLayout(
      sidebarPanel(
        selectInput("platform", 
                    label = "Select platform type:",
                    choices = c("TV", "Radio", "Social_Media"),
                    selected = "TV"),
        helpText("There are three platforms: TV, Radio, and Social Media."),
        helpText("The plot functional relationship between the budget invested 
                 of the selected platform and sales (revenue)."),
        checkboxInput("curve", "Show smooth curve", value = FALSE),
        helpText("Check to display smooth curve.")
      ), 
      mainPanel(plotOutput("graph1"))
    )
  ),
  
  hr(),
  fluidRow(
    titlePanel("Social Media Platform Usage Per Year"),
    sidebarLayout(
      sidebarPanel(
        selectInput("sns", 
                    label = "Select year:",
                    choices = c("2009", "2010", "2011", "2012", "2013",
                                "2014", "2015", "2016", "2017", "2018", "2019"),
                    selected = "2019"),
        helpText("You can choose any year between 2009 and 2019."),
        helpText("There are different types of social media platforms in each year."),
        helpText("The plot displays usage rate for each social media platform via
                 the bar plot."),
        checkboxInput("value", "Show usage rate", value = FALSE),
        helpText("Check to display usage rate value for each sns platform.")
      ), 
      mainPanel(plotOutput("graph2"))
    )
  ),
  
  hr(),
  fluidRow(
    titlePanel("Website Page Type Statistics"),
    sidebarLayout(
      sidebarPanel(
        selectInput("website", 
                    label = "Choose y-axis:",
                    choices = c("count", "duration"),
                    selected = "count"),
        helpText("There are two y variables to choose from."),
        helpText("count = shows the visit frequency of each page type,"),
        helpText("duration = shows the visit duration on each page type.")
      ), 
      mainPanel(plotOutput("graph3"))
    )
  ),
  
  hr(),
  fluidRow(
    titlePanel("Holiday and Weekend Effect on Revenue"),
    sidebarLayout(
      sidebarPanel(
        selectInput("special", 
                    label = "Select factor effect:",
                    choices = c("holiday", "weekend"),
                    selected = "holiday"),
        helpText("This plot shows whether a special day or event has an effect 
                 on revenue."),
        helpText("There are two variables: holiday and weekend."),
        helpText("holiday = shows the count of near holidays at the point of transaction,"),
        helpText("weekend = shows the count of weekends at the point of transaction.")
      ), 
      mainPanel(plotOutput("graph4"))
    )
  ),
  
  hr(),
  fluidRow(
    titlePanel("Category Keyword Search Relevance"),
    sidebarLayout(
      sidebarPanel(
        selectInput("keyword", 
                    label = "View group of",
                    choices = c("highest relevance rate", "lowest relevance rate"),
                    selected = "highest relevance rate"),
        helpText("There are two groups to choose from:
                 (1) 5 categories with highest relevance rate,
                 (2) 5 categories with lowest relevance rate"),
        helpText("highest relevance rate = stands for (1),"),
        helpText("lowest relevance rate = stands for (2),"),
        checkboxInput("mark", "Mark highest point(for highest relevance rate) or
                      lowest point(for lowest relevance rate)", value = FALSE),
        helpText("Check to display highest or lowest point.")
      ), 
      mainPanel(plotOutput("graph5"))
    )
  )

)

# Server logic
server <- function(input, output){
  
  #graph1: Platform Advertsing Budget vs Sales
  platformInput <- reactive({
    df <- platform_new %>%
      filter(platform == input$platform)
    df
  })
  
  output$graph1 <- renderPlot({
    p1 <- ggplot(data = platformInput(), aes(x = budget, y = Sales)) +
      theme +
      geom_point(position = "jitter", colour = "pink") +
      ylab("Sales") +
      xlab("Budget (in millions)")
    p1
    
    if (!input$curve){
      p1
    } else{
      p1 + geom_smooth(colour = "red")
    }
  
  })
  
  #graph2: SNS Platform Usage
  snsInput <- reactive({
    df <- sns_new %>%
      filter(year == input$sns)
    df
  })
  
  output$graph2 <- renderPlot({
    p1 <- ggplot(data = snsInput(), aes(x = platform, y = usage)) +
      theme +
      geom_col(fill = "pink") +
      theme(
        axis.text.x = element_text(angle = 90)
      ) +
      xlab("SNS Platforms") +
      ylab("Usage")
    p1
    
    if (!input$value){
      p1
    } else{
      p1 + geom_text(aes(label = round(usage)), vjust = -0.2, colour = "red")
    }
    
  })
  
  #graph3: Website Page Type Statistics
  websiteInput <- reactive({
    if (input$website == "count"){
      df <- website_pg
    } else{
      df <- website_dur
    }
  })
  
  output$graph3 <- renderPlot({
    p1 <- ggplot(data = websiteInput(), aes(x = page, y = value)) +
      theme +
      geom_col(fill = "pink") +
      xlab("Page Type") +
      scale_x_discrete(
        labels = c("Administrative", "Informational", "Product-Related")
      )
    p1
    
    if (input$website == "count"){
      p1 + ylab("Visit Frequency")
    } else{
      p1 + ylab("Time Spent")
    }
  })
  
  #graph4: Website Page Type Statistics
  specialInput <- reactive({
    if (input$special == "holiday"){
      df <- website_ho
    } else{
      df <- website_we
    }
  })
  
  output$graph4 <- renderPlot({
    p1 <- ggplot(data = specialInput(), aes(x = revenue, y = value)) +
      theme +
      geom_col(fill = "pink") +
      xlab("Revenue Status")
    p1
    
    if (input$special == "holiday"){
      p1 + ylab("Near Holiday Count")
    } else{
      p1 + ylab("Weekend Count")
    }
  })
  
  #graph5: Search Relevance Rate Per Category
  keywordInput <- reactive({
    if (input$keyword == "highest relevance rate"){
      df <- keyword_top
    } else{
      df <- keyword_bot
    }
  })
  
  output$graph5 <- renderPlot({
    p1 <- ggplot(data = keywordInput(), aes(x=category, y=relevance)) +
      theme +
      geom_point(colour = "red", size = 2) +
      theme(
        axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1)
      ) +
      xlab("Category") +
      ylab("Relevance Rate")
    p1
    
    if (!input$mark){
      p1
    } else{
      if (input$keyword == "highest relevance rate"){
        p1 +
          annotate("text", x = "samsonite\"", y = 3.98, 
                   label = "highest relevance", colour = "red")
      } else{
        p1 +
          annotate("text", x = "metal lathe", y = 1, 
                   label = "lowest relevance", colour = "red")
      }
    }
  })
  
}

# Run app
shinyApp(ui, server)
