# Load required libraries


library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(readr)
library(tools)
library(colourpicker)  # For colourInput()



# Define the UI
ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  titlePanel("Genome Coverage Visualizer"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload Coverage TSV File(s)", multiple = TRUE, accept = ".tsv"),
      numericInput("threshold", "Coverage Threshold (e.g. 10×):", value = 10, min = 0),
      colourInput("lineColor", "Line Color:", value = "steelblue"),
      checkboxInput("showThreshold", "Show Threshold Line", TRUE),
      textInput("plotTitle", "Plot Title:", "Genome Coverage Plot"),
      actionButton("plot_btn", "Plot Coverage"),
      br(),
      uiOutput("sampleSelector"),  # Appears if multiple files are uploaded
      downloadButton("downloadPlot", "Download Plot")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", uiOutput("plot_ui")),  # Dynamically rendered plot
        tabPanel("About",
                 h4("Instructions"),
                 p("• Upload one or more .tsv coverage files (3 columns: Ref, Position, Depth)."),
                 p("• Adjust plot threshold, line color, and title."),
                 p("• If multiple files are uploaded, you can select a specific sample to view."),
                 p("• Use the 'Download Plot' button to save the image.")
        )
      )
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  
  # Read and combine uploaded files when 'Plot Coverage' button is clicked
  coverage_data <- eventReactive(input$plot_btn, {
    req(input$file)
    
    bind_rows(lapply(input$file$datapath, function(path) {
      df <- read_tsv(path, col_names = c("Reference", "Position", "Depth"), show_col_types = FALSE)
      df$Sample <- file_path_sans_ext(basename(path))  # Extract file name without extension
      df
    }))
  })
  
  # If multiple samples are uploaded, show a dropdown to select one
  output$sampleSelector <- renderUI({
    df <- coverage_data()
    samples <- unique(df$Sample)
    if (length(samples) > 1) {
      selectInput("selectedSample", "Select Sample to Plot", choices = samples)
    }
  })
  
  # Render plot output area dynamically
  output$plot_ui <- renderUI({
    req(coverage_data())
    plotOutput("coveragePlot", height = "500px")
  })
  
  # Generate the genome coverage plot
  output$coveragePlot <- renderPlot({
    df <- coverage_data()
    if (!is.null(input$selectedSample)) {
      df <- df %>% filter(Sample == input$selectedSample)
    }
    
    gg <- ggplot(df, aes(x = Position, y = Depth)) +
      geom_line(color = input$lineColor) +
      labs(title = input$plotTitle, x = "Position on Genome", y = "Coverage Depth") +
      theme_minimal()
    
    # Add threshold line if selected
    if (input$showThreshold) {
      gg <- gg +
        geom_hline(yintercept = input$threshold, linetype = "dashed", color = "red") +
        annotate("text", x = max(df$Position, na.rm = TRUE)/2, y = input$threshold + 2,
                 label = paste0(input$threshold, "× threshold"), color = "red")
    }
    
    gg
  })
  
  # Download the plot as PNG
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste0("coverage_plot_", Sys.Date(), ".png")
    },
    content = function(file) {
      df <- coverage_data()
      if (!is.null(input$selectedSample)) {
        df <- df %>% filter(Sample == input$selectedSample)
      }
      
      p <- ggplot(df, aes(x = Position, y = Depth)) +
        geom_line(color = input$lineColor) +
        labs(title = input$plotTitle, x = "Position on Genome", y = "Coverage Depth") +
        theme_minimal()
      
      if (input$showThreshold) {
        p <- p +
          geom_hline(yintercept = input$threshold, linetype = "dashed", color = "red") +
          annotate("text", x = max(df$Position, na.rm = TRUE)/2, y = input$threshold + 2,
                   label = paste0(input$threshold, "× threshold"), color = "red")
      }
      
      ggsave(file, plot = p, device = "png", width = 8, height = 5)
    }
  )
  
  # Disable 'Plot Coverage' button unless a file is uploaded
  observe({
    shinyjs::toggleState("plot_btn", condition = !is.null(input$file))
  })
}

# Run the app
shinyApp(ui = ui, server = server)
