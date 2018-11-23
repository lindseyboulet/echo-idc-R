# Load packages
y <- c("shiny", "plyr", "dplyr", "reshape", "shinythemes", "ggplot2", "readr", "shinyFiles",
       "shinyjs", "taRifx", "shinydashboard", "here", "plotrix", "data.table", "gridExtra",
       "rmarkdown", "signal", "rlist", "minpack.lm", "deSolve", "shinycssloaders")
for(i in 1:length(y)){is_installed <- function(mypkg){is.element(mypkg, installed.packages()[,1])}
if(!is_installed(y[i])){install.packages(y[i], repos="http://lib.stat.cmu.edu/R/CRAN")}
library(y[i], character.only=TRUE, quietly=TRUE,verbose=FALSE)}

# Define UI for application that draws a histogram
ui <- fluidPage( theme = "bootstrap.css",
                 br(),
   # Application title
   titlePanel("Echo - IDC"),
      # Show a plot of the generated distribution
   wellPanel(style = "background-color: #060606; font-size: 15px;, border-color: #060606;",
   tabsetPanel(
     tabPanel(h4("Fitted Data", style = "color: #20c997;"), 
              br(),
              br(),
   fluidRow(
     tags$style("#wn,#flow, #gas, #contrast, #sample, #flowPred {background-color:#060606; color:white; border-color: #555;font-size: 15px;}"),
     tags$style("#mass_in,#mtt_in,#lam_in {
                background-color:#060606; color:#2A9FD6; border-color: #555;font-size: 15px;
                }"),
     tags$style("#mass_out,#mtt_out,#lam_out {
                background-color:#060606; color:#2ca02c; border-color: #555;font-size: 15px;
                }"),
     tags$style(HTML("hr {border-top: 1px solid white;}")),
     column(1,offset = 4,uiOutput("gas")),
     column(1,uiOutput("flow")),
     column(1,uiOutput("contrast")),
     column(1,uiOutput("sample"))
     # column(1, offset = 1,
     #        radioButtons("model", "Model", choices = c("LDRW" = "ldrw", "FPT"= "fpt"))
            # )
   # column(1,
   #        radioButtons("flag", "Flag Data?", choices = c("No" = FALSE, "Yes"= TRUE))) # maybe add a data flag func
     ),
   hr(),
   fluidRow(
     column(2,offset = 2,
            tags$h2("Inflow", style = "color: #2A9FD6;")
   ),
   column(2, offset = 4,
          h2("Outflow", style = "color: #2ca02c;")
   )
   ),
   fluidRow(
     column(5,
            withSpinner(plotOutput("filtPlot1"))
     ),
     column(width = 5, offset = 2,
            withSpinner(plotOutput("filtPlot2"))
     )
   ),
   withSpinner(uiOutput("inParams")),
   br(),
   br(),

   fluidRow(
     column(width = 6, offset = 2,
            withSpinner(plotOutput("comPlot"))
     ),
     column(width = 3, 
            withSpinner(uiOutput("summaryNums"))
     )
   )
     ),
   tabPanel(h4("Data Summary", style = "color: #20c997")
   ),
   tabPanel(h4("", style = "color: #20c997;")
            )
   )
)
)

server <- function(input, output) {
  fileIndex <- reactive({
    fileID <- list.files(path = "./pyData/", pattern = ".csv")
    fileID <- strtrim(fileID, nchar(fileID)-4)
    fileIndex <- as.data.frame(matrix(as.character(unlist(strsplit(fileID, "_"))), 
                                      ncol = sapply(gregexpr("_", fileID[1]), length) +1, byrow = TRUE))
    fileIndex <- fileIndex[,1:4]
    colnames(fileIndex) <- c("gas", "flow", "contrast", "sample")
    # fileIndex$flow <- gsub("-", ".", fileIndex$flow)
    fileIndex <- mutate_all(fileIndex, as.character)
    fileIndex
                        })
  # output file selection inputs -------------------------------------------------------
  output$gas <- renderUI({
    fileIndex <- fileIndex() 
    div(selectInput(inputId = "gas", label = strong("Gas"),choices = unique(fileIndex$gas)),
        style = "background-color:#060606")
  })
  output$flow <- renderUI({
    fileIndex <- fileIndex() 
    selectInput(inputId = "flow", label = strong("Flow"),
                choices = unique(fileIndex$flow[which(fileIndex$gas == input$gas)]))
  })
  output$contrast <- renderUI({ 
      fileIndex <- fileIndex()
      selectInput(inputId = "contrast", label = strong("Contrast"),
                  choices = unique(fileIndex$contrast[which(fileIndex$gas == input$gas & fileIndex$flow == input$flow)]))
  })
  output$sample <- renderUI({
      fileIndex <- fileIndex()
      selectInput(inputId = "sample", label = strong("Sample"),
                  choices = unique(fileIndex$sample[which(fileIndex$gas == input$gas &
                                                              fileIndex$flow == input$flow & fileIndex$contrast == input$contrast)]))
      })

  plotData <- reactive({
    req(input$gas)
    fN <- paste0("./pyData/", paste(input$gas, input$flow, input$contrast, input$sample, sep = "_"), "_fits.csv")
    read.csv(fN)
  })
  
  summaryData <- reactive({
    read.csv("./summaryData/summary.csv")
  })
  
  dataPoint <- reactive({
    sumData <- summaryData()
    dplyr::filter(sumData, gas == input$gas & Q == gsub("-",".",input$flow) &
                    contrast == input$contrast & sample == input$sample) 
  })
  
  
  themeAdd <- function(x){
    if (x == 1){
      co <- "#ff7f0e";co2 <- "#2A9FD6"}else{co <- "#9467bd"; co2 <- "#2ca02c"}
    theme(text=element_text(colour=co,size = 16), 
          axis.ticks = element_line(color = co2),
          axis.line = element_blank(),
          axis.text = element_text(colour=co2),
          plot.background = element_rect(fill="#060606", colour = "#060606"),
          panel.background = element_rect(fill="#060606", color = "#060606"),
          axis.title = element_blank()) 
  }
  
  output$filtPlot1 <- renderPlot({
    req(input$sample)
    dp <- dataPoint()
    ggplot(plotData(), aes(x = time)) + theme_classic(base_size = 16) +
      geom_line(aes(y = t1Filt), color = "#2A9FD6")+ 
      geom_line(aes(y = t1Fit), color = "#d62a49", size = 1.3) + themeAdd(1)+
      scale_x_continuous(limits = c(0, 10))+ 
      annotate("text", -Inf, Inf, label = paste0("atop(chi^2==", formatC(dp$chi1, format = "e", 1), ",AIC==", round(dp$aic1,0), ")"),
               hjust = -3, vjust = 3, parse = TRUE, color = "#2A9FD6", size = 7)
  })
  output$filtPlot2 <- renderPlot({
    req(input$sample)
    dp <- dataPoint()
    ggplot(plotData(), aes(x = time)) + theme_classic(base_size = 16) +
      geom_line(aes(y = t2Filt), color = "#2ca02c")+ 
      geom_line(aes(y = t2Fit), color = "#9467bd", size = 1.3) + themeAdd(0)+
      scale_x_continuous(limits = c(0, 25)) + 
      annotate("text", -Inf, Inf, label = paste0("atop(chi^2==", formatC(dp$chi2, format = "e", 1), ",AIC==", round(dp$aic2,0), ")"),
               hjust = -3, vjust = 3, parse = TRUE, color = "#2ca02c", size = 7)
  })
  output$inParams <- renderUI({
    req(input$sample)
    dp <- dataPoint()
    fluidRow(
      column(2,
             h4(paste("Mass:", formatC(dp$m1, format = 'e', digits = 1)), style = "color: #d62a49;")
             ),
      column(2,
             h4(paste("MTT:", round(dp$mu1,2)), style = "color: #d62a49;")
      ),column(2,
               h4(HTML(paste0("&lambda;:"), round(dp$lam1, 2)), style = "color: #d62a49;")
      ),
      column(2,offset = 1,
             h4(paste("Mass:", formatC(dp$m2, format = 'e', digits = 1)), style = "color: #9467bd")
      ),
      column(2,
             h4(paste("MTT:", round(dp$mu2,2)), style = "color: #9467bd;")
      ),
      column(1,
               h4(HTML(paste0("&lambda;:"), round(dp$lam2, 2)), style = "color: #9467bd;")
      )
    )
  })
  
  output$comPlot <- renderPlot({
    req(input$sample)
    ggplot(plotData(), aes(x = time)) + theme_classic(base_size = 16) +
      geom_line(aes(y = t1Filt), color = "#2A9FD6")+ 
      geom_area(aes(y=t1Filt), fill = "#2A9FD6")+
      geom_line(aes(y = t2Filt), color = "#2ca02c")+ 
      geom_area(aes(y=t2Filt), fill = "#2ca02c")+
      geom_line(aes(y = t1Fit), color = "#d62a49", size = 1.3)+
      geom_line(aes(y = t2Fit), color = "#9467bd", size = 1.3) + themeAdd(0) 
  })
  
  output$summaryNums <- renderUI({
    req(input$sample)
    dp <- dataPoint()
    h3(HTML(paste0("<br/><br/><br/><br/><br/><br/><br/><br/>Conserved (AUC): ",
                   round(dp$cc_auc, 1),"%","<br/>Conserved (LDRW): ",
                   round(dp$cc_ldrw, 1),"%","<br/>Transit Time: ", 
                   round(dp$mu2-dp$mu1, 1), "s")), style = "color: #fd7e14")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

