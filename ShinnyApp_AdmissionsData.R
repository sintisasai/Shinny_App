##-------------------------------------------------------------------##
##                     Clean Data and Build Shinny App               ##                                ##
##                     Sinti Sasai                                   ##
##-------------------------------------------------------------------##
### Load required libraries 
library(tidyverse)
library(shiny)
library(viridis)
library(ggplot2)
library(haven)
library(wordcloud2)
library(car)
library(psych)
library(factoextra)
library(ggVennDiagram)
library(ggridges)

### import the IUP data into R

iup = read_csv(file.choose(),show_col_types = FALSE)

# The goal of this project is to clean IUP undergraduate
# admissions data.Do some EDA and build a shiny app to display some visualizations.

### Let's build a table to describe the distribution of 
### College Admission Decisions 

#iup = iup %>%
  #drop_na(GPA, Decision) %>%
  #select(Decision) %>%
  #table()

### You see there are six levels in the "Decision" variable
### AC - Admission Cancelled, ## AD - Admitted, ## AP - Admission Paid 
### AW - Admission Waived, ## PC - Paid Cancelled, ## RJ - Reject 
### We first split our data into two sets: train and test set
### Where the "testing" contains only the AD level.
### In train set, we (a) create a new variable APC that combines AC and PC;
### (b) create a new variable APW that combines AP and AW.
### (c) The decision variable should have 3 levels left: APC, APW, and RJ
### With a train set, we build a shiny App with multiple tabs
### In the first tab, we show the training data.
### In the second tab, visualize the relationship between "decision" and all 
### potential variables that can influence the decision.

# Drop NAs
iup_data = iup %>%
  drop_na(Decision, GPA, HighestTest, FAFSA, Program)

# Make test data with decision variable "AD"
iup_test = iup_data %>%
  filter(Decision == "AD")

# Make train data with the other decision variables "AC","AP", "AW", "PC","RJ"
iup_train = iup_data %>% 
  filter(Decision %in% c("AC", "AP", "AW", "PC","RJ"))

# Create variable APW - combine AP and AW & APC - combine AC and PC
iup_train$Decision <- ifelse(iup_train$Decision == "AP","APW", 
                             ifelse(iup_train$Decision == "AW", "APW", 
                                    ifelse(iup_train$Decision == "AC", "APC", 
                                           ifelse(iup_train$Decision == "PC", "APC", "RJ"))))
# Check decision variables 
iup_train %>% 
  select(Decision) %>%
  table()
### Looks good 

### Build App
# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("IUP UG 2019 Data Visualisation, Midterm Exam - Sinti"),
  tabsetPanel(
    tabPanel(
      "Choose File",
      #titlePanel("Choose a Dataset"),
  
  # Sidebar layout with a input and output definitions ----
        sidebarLayout(
    
    # Sidebar panel for inputs ----
          sidebarPanel(
      
      # Input: Selector for choosing dataset ----
            selectInput(inputId = "dataset",
                  label = "Dataset:",
                  choices = "iup_train")
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Verbatim text for data summary ----
      verbatimTextOutput("summary"),
      
      # Output : HTML table 
      DT::dataTableOutput("contents")
      
    )
  )),
  tabPanel(
    "Generate Plots",
    #titlePanel("Select X-axis and Y-axis Variables and produce plots"),
    pageWithSidebar(
      headerPanel(''),
      sidebarPanel(
        selectInput("anl1", "Y axis Variable", "", selected = ""),
        selectInput("anl2", "X axis Variable", "", selected = ""),
        
        selectInput(
          "plot",
          "Plot Type:",
          choices =
            c(
              "Boxplot" = "box" ,
              "Scatter Plot" = "scatter",
              "Connected Scatter Plot" = "conscatter",
              "Bar Plot" = "bar",
              "Principal Component Analysis" = "pca"
            ), selected = "scatter"
        ),
        
      ),
      mainPanel(
        plotOutput('MyPlot')
        )
    )
  )
))
# Define server logic to summarize and view selected dataset ----
server = shinyServer(function(input, output, session) {
  
  # Return the requested dataset ----
  datasetInput <- reactive({
    switch(input$dataset,
           "iup_train" = iup_train)
  
  }) 
  
  updateSelectInput(
    session,
    inputId = 'anl1',
    label = 'X axis Variable',
    choices = names(sapply(iup_train, is.numeric)),
    selected = grep("GPA", names(iup_train), value = TRUE)
  )
  
  updateSelectInput(
    session,
    inputId = 'anl2',
    label = 'Y axis Variable',
    choices = grep("Decision", names(iup_train), value = TRUE),
    selected = grep("Decision", names(iup_train), value = TRUE)
  )
 

  output$contents <- DT::renderDataTable({
  datasetInput()
  }) 
  
  output$summary = renderPrint({
    summary(datasetInput())
  })




  plot1 <- reactive({
    ggplot(datasetInput(),
           aes_string(
             x = input$anl1,
             y = input$anl2,
             fill = input$anl1
           )) + geom_boxplot(
             # custom boxes
             
             
             # custom outliers
             outlier.colour = "red",
             outlier.fill = "red",
             outlier.size = 3
           ) +
      scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
      geom_jitter(color = "blue",
                  size = 0.4,
                  alpha = 0.9) +
      theme(legend.position = "top",
            plot.title = element_text(size = 14)) +
      ggtitle("A boxplot with jitter")
    
  })
  
  plot2 <- reactive({
    # scatter plot
    p <-
      ggplot(datasetInput(), aes_string(x = input$anl1, y = input$anl2)) + 
      geom_point(
        shape = 21,
        color = "black",
        fill = "#69b3a2",
        size = 3
      )
    print(p)
  })
  
  plot3 <- reactive({
    #connected scatter plot
    p <- ggplot(datasetInput(), aes_string(x = input$anl1, y = input$anl2)) +
      geom_line(color = "grey") +
      geom_point(
        shape = 21,
        color = "black",
        fill = "#69b3a2",
        size = 3
      )
    print(p)
    
  })
  
  plot4 <- reactive({
    #Bar plot
    ggplot(data = datasetInput(),
           aes_string(
             x = input$anl1,
             y = input$anl2,
             fill = input$anl1
           ))  +
      stat_summary(
        #fun = sum,
        geom = "bar",
        colour = "#56B4E9",
        fill = "#56B4E9"
      ) +
      geom_bar(stat = "identity") +
      
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5))
    
  })
  
  plot5 <- reactive({# PCA Scree Plot
    
    pcadat <- select_if(datasetInput(), is.numeric)
    res.pca <- prcomp(pcadat, scale = TRUE)
    p <- fviz_eig(res.pca)
    print(p)
  })
  
  
  # Return the requested graph
  graphInput <- reactive({
    switch(
      input$plot,
      "box" = plot1(),
      "scatter" = plot2(),
      "conscatter" = plot3(),
      "bar" = plot4(),
      "pca" = plot5()
      
      
    )
  })
  
  output$MyPlot <- renderPlot({
    graphInput()
  })
  
})
shinyApp(ui=ui, server=server)
