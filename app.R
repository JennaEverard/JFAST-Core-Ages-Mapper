#
# JFAST Core Mapper
#
# AUTHOR: Jenna Everard
# LAST MODIFIED: March 25, 2022
#

library(shiny)
library(ggplot2)
library(ggthemes)
library(ggpubr)
library(plotly)
library(DT)

ui <- fluidPage(

    # Application title
    titlePanel("JFAST Core Mapper"),
    
    # Input for data file        
    fileInput("sample_csv",
              "csv file:",
              multiple = FALSE,
              accept = "csv",
              buttonLabel = "Browse...",
              placeholder = "No file selected"),
    
    # Option to download a sample input file
    downloadButton(
        "sample_download",
        label="Example Input File",
        icon = shiny::icon("download")
        ),
    
    br(),
    br(),
    
    # Core diagram with ages plotted
    verbatimTextOutput("heading1"),
    plotlyOutput("coreGraph", width="100%"),
    
    br(),
    br(),
    
    # Unit Descriptions
    verbatimTextOutput("descriptions"),

    br(),
    br(),
            
    # Data table for core ages
    verbatimTextOutput("heading2"),
    DT::dataTableOutput("inputTable"),
            
    br(),
    br()
)


# Server logic for graphing core ages
server <- function(input, output) {

    sample_data <- read.csv("sample.csv")
    
    # heading for plot   
    output$heading1 <- renderText({
        req(input$sample_csv)
        "K/Ar Ages Along JFAST Core:"
    })
    
    # download a sample input file
    output$sample_download <- downloadHandler(
        filename = function() {
            paste("sample-JFAST-input.csv", sep="")
        },
        content = function(file) {
            write.csv(sample_data, file, row.names=FALSE)
        }
    )
    
    # Core Image
    output$coreGraph <- renderPlotly({
        
        # Load Data
        req(input$sample_csv)
        
        sample_i <- input$sample_csv  
        ext <- tools::file_ext(sample_i$datapath)
        req(sample_i)
        validate(need(ext == "csv", "Please upload a csv file"))
        
        data <- read.csv(sample_i$datapath, header=TRUE)
        
        # Separate Data into Panel 1 and Panel 2
        panel1_data <- subset(data, Depth<=184 & Depth>=180)
        panel2_data <- subset(data, Depth<=833.5 & Depth>=688.5)
        
        # Determine maximum age for y-axis bounds
        max_age <- max(data$Age)
        
        # plot panel 1
        panel1 <- ggplot() + xlim(180,184) + ylim(0,1) +
            geom_rect(data=NULL, aes(xmin=180, xmax=184, ymin=0, ymax=max_age),
                      fill="sienna1") +
            geom_point(data = panel1_data, aes(x = Depth, y = Age, label=Sample.ID)) +
            geom_errorbar(aes(x = panel1_data$Depth,
                              ymin = panel1_data$Age - panel1_data$Sigma, 
                              ymax = panel1_data$Age + panel1_data$Sigma),
                          size=0.5, width=0.5) +
            ylim(0, max_age) +
            xlim(180, 184) +
            theme(
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank()
            ) +
            scale_x_continuous(breaks = c(182.5)) +
            ylab("") +
            xlab("")
        
        # plot panel 2
        panel2 <- ggplot() + xlim(688.5, 833.5) + ylim(0,1) +
            geom_rect(data=NULL, aes(xmin=690, xmax=819.5, ymin=0, ymax=max_age),
                      fill="lemonchiffon2") +
            geom_rect(data=NULL, aes(xmin=819.5, xmax=822, ymin=0, ymax=max_age),
                      fill="sienna1") +
            geom_rect(data=NULL, aes(xmin=822, xmax=823.0, ymin=0, ymax=max_age),
                      fill="yellow1") +
            geom_rect(data=NULL, aes(xmin=823, xmax=825, ymin=0, ymax=max_age),
                      fill="slategray2") +
            geom_rect(data=NULL, aes(xmin=828, xmax=832.2, ymin=0, ymax=max_age),
                      fill="darkolivegreen2") +
            geom_rect(data=NULL, aes(xmin=832.2, xmax=833, ymin=0, ymax=max_age),
                      fill="sienna1") +
            geom_rect(data=NULL, aes(xmin=833, xmax=834, ymin=0, ymax=max_age),
                      fill="darkturquoise") +
            geom_point(data = panel2_data, aes(x = Depth, y = Age, label = Sample.ID)) +
            geom_errorbar(aes(x = panel2_data$Depth,
                              ymin = panel2_data$Age - panel2_data$Sigma,
                              ymax = panel2_data$Age + panel2_data$Sigma),
                          size=0.5, width=1) +
            ylim(0, max_age) +
            theme(
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank()
            ) +
            scale_x_continuous(breaks = c(690, 700, 710, 720, 730, 740, 750, 760, 770,
                                          780, 790, 800, 810, 820, 830, 840)) +
            ylab("") +
            xlab("")
        
        # combine panel 1 and panel 2
        plotly_panel1 <- ggplotly(panel1)
        plotly_panel2 <- ggplotly(panel2)
        
        combined_panel <- subplot(plotly_panel1, plotly_panel2,
                                  nrows = 1,
                                  widths = c(0.1, 0.9))
        
        # output combined graph
        ggplotly(combined_panel)
    })
    
    # Display unit descriptions below graph
    output$descriptions <- renderText({
        req(input$sample_csv)
        
        # Formatting text output
        line1 <- paste("Unit Descriptions:")
        line2 <- paste("Orange:", 
                       "Unit A2",
                       sep="\t")
        line3 <- paste("Beige:", 
                       "Unit A1",
                       sep="\t")
        line4 <- paste("Yellow:", 
                       "Unit C2 [Pelagic Clays]",
                       sep="\t")
        line5 <- paste("Light Blue:", 
                       "Unit B [Claystone]",
                       sep="\t")
        line6 <- paste("Green:", 
                       "Unit A3 [Mudstone]",
                       sep="\t")
        line7 <- paste("Turquoise:", 
                       "Unit D",
                       sep="\t")
        
        combined_lines <- paste(line1, line2, line3, line4, line5, line6, line7, sep="\n")
    })
    
    # heading for data table   
    output$heading2 <- renderText({
        req(input$sample_csv)
        "K/Ar Ages by Sample Number and Depth:"
    })
    
    # Display input table    
    output$inputTable <- DT::renderDataTable({
        req(input$sample_csv)
        
        sample_i <- input$sample_csv
        
        # display error message if no input was given
        ext <- tools::file_ext(sample_i$datapath)
        req(sample_i)
        validate(need(ext == "csv", "Please upload a csv file"))
        
        data <- read.csv(sample_i$datapath, header=TRUE)
        data
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
