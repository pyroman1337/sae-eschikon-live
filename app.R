###########################################################################
### SAE's Eschikon mesocosm experiment (almost) Live Data shiny app     ###
###########################################################################
## Author: Roman Hüppi
## Date : April 2020
## Version: 1.2

# libraries ---------------------------------------------------------------
library(shiny)
library(ggplot2)
# library(ggthemes)
library(plotly)
library(data.table)
library(DT)
# library(gasfluxes)
library(dplyr)
library(tidyr)
# library(EBImage)  # installation of fftw-devel on fedora required


# Define UI for application
ui <- fluidPage(
  img(src = "sae.png", height = 39, align = "right"),
  
   # Application title
   titlePanel("SAE Mesocosm Eschikon interactive data platform"),
   
   # Sidebar layout with a input and output definitions ----
   sidebarLayout(
     
     # Sidebar panel for inputs ----
     sidebarPanel(
       
       radioButtons("surface.em", "Select surface emissions",
                    choices = c("Nitrous oxide" = "N2O",
                                "Carbon dioxide" = "CO2",
                                "Methane" = "CH4"),
                    selected = 'N2O'),

       
       radioButtons("depth.pr", "Select depth profile",
                    choices = c("Nitrous oxide" = "N2O",
                                "Carbon dioxide" = "CO2",
                                "Methane" = "CH4",
                                "Oxygen" = "O2",   # eval(c(expression("N"[2]*"O"))
                                "Temperature" = "Temp",
                                "Soil moisture" = "SM"),
                                selected = 'N2O'),
 

       # Horizontal line ----
       tags$hr(),
       
       # ),
       h4("Visualisation options"),
       # uiOutput("siteInput"),
       

       # })
       # tags$hr(),
       
       selectInput("group.var",
                   label = "Grouping variable",
                   choices = c("Treatment factor 1",
                               "Treatment factor 2",
                               # "Treatment factor 3",
                               "Column/Chamber number",
                               "Block"),
                               # "Sub factor"),
                   selected = "Treatment factor 1"),

       
       # # Input: Select quotes ----
       radioButtons("reg.method", "Choose a flux calculation method",
                    choices = c("non-linear - HMR" = "HMR.f0",
                                "robust linear" = "robust.linear.f0"
                                # "dynamic kappa.max HMR" = "dynamic.kappa.f0",
                                ),
                    selected = 'robust.linear.f0'),

      #input for date slider
      sliderInput("date.range", "Select date range:",
                  min = as.Date("2019-04-01", "%Y-%m-%d"),
                  max = as.Date("2020-05-10", "%Y-%m-%d"),
                  value = c(as.Date("2019-04-05"), as.Date("2019-12-31")),
                  timeFormat = "%Y-%m-%d"
                  ),
    #input to add event lines to the gasflux plot
    radioButtons("event", "Show management events:",
                 choices = c("watering", "fertilization", "both")
                 )
       
     ),  # end sidebarPanel
     
     # Main panel for displaying outputs ----
     mainPanel(
       
       tabsetPanel(
         tabPanel(p(icon("bar-chart"), "Graph"), 
                  plotlyOutput("timelinePlot"), 
                  plotlyOutput("contourPlot"),width = "500px", height="500px"), 
         
         # Data 
         tabPanel(p(icon("table"), "Data plot"),
          # dataTableOutput(outputId="dTable")
          # verbatimTextOutput("value"),
          downloadButton("downloadData", "Download"),
          
          DT::dataTableOutput("plot.table") 
                   
         ), #, end of "Dataset" tab panel
         
         tabPanel(p(icon("question-circle"), "Help"),
            includeMarkdown("help.Rmd")
         )  ## maybe not "hahahah"
         
         # fluidRow(column(3, verbatimTextOutput("value")))
         ) # end of tab panel
       
       
     ) # end mainPanel
   ),  # end sidebarLayout
 
    img(src = "ETH_logo.jpg", height = 70, width = 200, align = "right")
 
)    # end fluidPage



# Define server logic to read selected file ----
server <- function(input, output) {
  
    get.profile.data <- function(){
      
      # if(input$col.data == "n2o.depth"){
        ghg.file   <- fread(paste0("data/GHG_soilair_2019.csv"), sep = ",") # input$separator)
        depth_info <- read.csv("data/level_information.csv", sep = ";")
        treatments <- read.csv("data/eschikon_treatments.csv", sep = ";")
        
        ghg.file[, c("column", "depth") := tstrsplit(ID, ".", fixed=T)]
        ghg.file$column <- as.numeric(substr(ghg.file$column,2,3))
        ghg.file$depth  <- as.numeric(ghg.file$depth)
        ghg.file[, date.strp := as.POSIXct(date, format = "%F")]
        
        ghg.file <- merge(ghg.file, depth_info, by.x = "depth", by.y = "level" )
        ghg.file <- merge(ghg.file, treatments, by.x = "column", by.y = "column.nr" )
        setnames(ghg.file, "depth.y", "depth.cm")
      
  }  # end get.flux.data()
  
    get.plot.data <- reactive({
      
      ghg.input <- fread(paste0("data/gasflux-",input$surface.em,"_Eschikon.csv")) # input$separator)
  

          tryCatch(
            {
              ghg.input[ ,date.strp := as.Date(date,format = "%Y-%m-%d")] #changed from as.POSIXct
              # ghg.file <- ghg.file[is.finite(linear.f0),]
              ghg.input[is.na(robust.linear.f0), robust.linear.f0 := linear.f0]  # use linear for if only 3 datapoints are available
              ghg.input[is.na(HMR.f0),              HMR.f0 := robust.linear.f0]  # use robust linear if HMR could not be fitted
                
              ghg.input[, block := as.factor(block)] # blocks as factors
              ghg.input[, column := as.factor(column)] # chamber number as factor
                # ghg.file[, sub_factor := as.factor(sub_factor)] # sub_factor as factor
            })
        

        ghg.file.melt  <- melt(ghg.input, id.vars = c("date.strp","column","treatment_fact1","block","treatment_fact2"), 
                               measure.vars = input$reg.method, value.name = "N2O.flux", variable.name = "method")
        
        group.var2 <- switch(input$group.var,
                             "Treatment factor 1" = "treatment_fact1",
                             "Treatment factor 2" = "treatment_fact2",
                             # "Treatment factor 3" = "treatment_fact3",
                             "Column/Chamber number" = "column" ,
                             "Block"              = "block"
                             # "Further factor"     = "sub_factor")
        )
        # 
        # ghg.file.melted <- ghg.file.melt[site %in% input$sites]
        
        ghg.file.plot <- ghg.file.melt[, .(mean = mean(N2O.flux),  # site == select.site
                                             sd = sd(N2O.flux,na.rm = T),
                                             n = length(N2O.flux)),
                                         # , se = sd/sqrt(n)),
                                         by = c("date.strp","method", group.var2)]

        ghg.file.plot[, se := sd/sqrt(n)]
        
      # return(ghg.file.plot)

      
      # })
  }) # end get.plot.data
    
    
#start get event data
    
      get.event.data <- reactive({
                                    events     <- fread("data/events.csv", header = TRUE)
                                    events$date <- as.Date(events$date, "%d.%m.%Y")
                                    events      <- events %>% filter(date >= input$date.range[1] & date <= input$date.range[2])
  
                                      if (input$event == "both"){
                                                                  events
                                                                  } else {
                                                                          events <-events %>% 
                                                                          filter(event == input$event)
                                                                          }
                                                                          })


  output$plot.table <- DT::renderDataTable(datatable(get.plot.data(), options = list(pageLength = 15,lengthChange=T))
                                           %>% formatSignif(c(4,5,7),3)
                                           %>% formatDate(1) )
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    # content = function(file) {
    filename = function() {
      paste("Eschikon-surface-flux_",input$gas.species,".csv", sep="")
    },
    content = function(file) {
      fwrite(get.plot.data(), file, row.names = FALSE, sep = ",")
    }
    
  )
  # output$plot.table2 <- DT::renderDataTable(datatable(gasfluxes.table(), options = list(pageLength = 15,lengthChange=T))
  #                                          %>% formatSignif(c(9:15),3))
  
  # output$value <- renderPrint({input$plot.table2_rows_selected })
  
  output$timelinePlot <- renderPlotly({
    
    # if(input$col.data == "N2O" || input$col.data == "CO2" || input$col.data == "CH4"){
    
      group.var2 <- switch(input$group.var,
                           "Treatment factor 1" = "treatment_fact1",
                           "Treatment factor 2" = "treatment_fact2",
                           # "Treatment factor 3" = "treatment_fact3",
                           "Column/Chamber number"     = "column" ,
                           "Block"              = "block")
      
      gas.unit <- switch(input$surface.em,
                         "N2O" = "mg N<sub>2</sub>O",
                         "CO2" = "g CO<sub>2</sub>" ,
                         "CH4" = "mg CH<sub>4</sub>")
      
      # data323 <- get.plot.data()
      # browser()
      
      # print(
        ggplotly(
          ggplot(data = get.plot.data(), aes_string(x ="date.strp", y = "mean", colour = group.var2)) + 
            geom_point(size = 0.95, show.legend = T) +
          # ggplot(data = ghg.file.plot, aes(x = date.strp, y = mean, colour = group.var)) + 
            geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2) +
            geom_line() +  #ylim(0,1) +
            geom_vline(data = get.event.data(), aes(xintercept = date, colour = event), size = 0.25) +
            # geom_smooth(method = "loess") +
            theme_light() +  # theme_gdocs() + 
            xlim(input$date.range[1], input$date.range[2])+
            ylab(paste0("flux [",gas.unit," m<sup>-2</sub> h<sup>-1</sub>]")) + xlab("") + 
            labs(colour = paste0(" grouped by:\n ",input$group.var,sep="")) +
            theme(plot.margin = unit(c(1, 1.75, 1, 0.5), "cm")) #top, right, bottom, left)  # # 
           ,height = 400, dynamicTicks = T) %>% layout(legend = list(orientation = "v",   # show entries horizontally
                                                                 xanchor = "right" ))  # ) ,  # use center of legend as anchor
            
                            
    # } # end if input$col.data
   } )  # end timelineplot output

    output$contourPlot <- renderPlotly({
      
    # if(input$col.data == "n2o.depth"){
      
      soil.ghg <- get.profile.data()
      if(input$depth.pr == "O2") soil.ghg <- fread(paste0("data/tidy-profile-",input$depth.pr,"_Eschikon.csv")) # input$separator)
      if(input$depth.pr == "Temp") soil.ghg <- fread(paste0("data/tidy-profile-",input$depth.pr,"_Eschikon.csv")) # input$separator)
      if(input$depth.pr == "SM") soil.ghg <- fread(paste0("data/tidy-profile-",input$depth.pr,"_Eschikon.csv")) # input$separator)
    
      
      # soil.ghg_mean <- soil.ghg %>% group_by(date.strp, depth.cm, treatment_fact1) %>%
      #   summarise(ghg.ppm = mean(N2O.ECD.ppm, na.rm = T)) #irregularities in the date column!!!
      # 
      # n.plots <- length(levels(soil.ghg_mean$treatment_fact1))
      if(input$group.var == "Treatment factor 2") soil.ghg$treatment_fact1 <- soil.ghg$treatment_fact2
      if(input$group.var == "Column/Chamber number") soil.ghg$treatment_fact1 <- soil.ghg$column
      if(input$group.var == "Block") soil.ghg$treatment_fact1 <- soil.ghg$block
      
      
      if(input$depth.pr == "N2O") soil.ghg_mean <- soil.ghg %>% group_by(date, depth.cm, treatment_fact1) %>%  # column / treatment_fact1
        summarise(ghg.ppm = mean(N2O.ECD.ppm, na.rm = T)) #irregularities in the date column!!!
      if(input$depth.pr == "CH4") soil.ghg_mean <- soil.ghg %>% group_by(date, depth.cm, treatment_fact1) %>%  # column / treatment_fact1
          summarise(ghg.ppm = mean(CH4.FID.ppm, na.rm = T)) #irregularities in the date column!!!
      if(input$depth.pr == "CO2") soil.ghg_mean <- soil.ghg %>% group_by(date, depth.cm, treatment_fact1) %>%  # column / treatment_fact1
          summarise(ghg.ppm = mean(CO2.TCD.ppm, na.rm = T)) #irregularities in the date column!!!
      if(input$depth.pr == "O2" || input$depth.pr == "Temp" || input$depth.pr == "SM") soil.ghg_mean <- soil.ghg %>% group_by(date, depth.cm, treatment_fact1) %>%  # column / treatment_fact1
          summarise(ghg.ppm = mean(mean, na.rm = T))
      
      #define units for each input variable
      
      if(input$depth.pr == "N2O" || input$depth.pr == "CH4" || input$depth.pr == "CO2") unit.cp <- "ppm"
      if(input$depth.pr == "O2") unit.cp <- "%"
      if(input$depth.pr == "Temp") unit.cp <- "°C"
      if(input$depth.pr == "SM") unit.cp <- "m^3/m^3"
      
      #defines the number of plots which are made
      n.plots <- length(levels(as.factor(pull(soil.ghg_mean[3]))))
      matrix7 <- as.data.table(soil.ghg_mean %>% spread(depth.cm, ghg.ppm) 
                               %>% filter(date >= input$date.range[1]) %>% filter(date <= input$date.range[2]))  # date range selection implemented for contour plots
      
      xcoords = unique(as.POSIXct(matrix7$date, tz ="UTC"))
      ycoords = c(-120, -90, -60, -30, -7.5)
      # surface.matrix = as.matrix(matrix7[c("120", "90", "60", "30", "7.5")])
      zrange <- c(min(soil.ghg_mean$ghg.ppm, na.rm = T),max(soil.ghg_mean$ghg.ppm, na.rm = T))

      # cp <- NA
      
      # for(i in 1:n.plots){
      surface.matrix <- list(NA)
      cp <- list(NA)
      
      plot.levels <- levels(as.factor(matrix7$treatment_fact1))  # column / treatment_fact1
      # browser()
      for(i in 1:n.plots){
        
        # matrix7[treatment_fact1 ==  plot.levels[i]]
        
        surface.matrix[[i]]  <- as.matrix(matrix7[treatment_fact1 == plot.levels[i], c("120", "90", "60", "30", "7.5")])  # column / treatment_fact1
        # surface.matrix  <- as.matrix(matrix7[treatment_fact1 == plot.levels[1], c("120", "90", "60", "30", "7.5")])

        cp[[i]] <- plot_ly(x = xcoords, y = ycoords, z = ~t(surface.matrix[[i]]),  type = "contour",   #contour histogram2dcontour
                           contours = list(showlabels = TRUE), color = I("black"))  %>%     # ,  colorscale = "Viridis"  color = I("black")
          colorbar(title = paste(plot.levels[i],"\n ",input$depth.pr," \n [", unit.cp, "]"), limits = zrange) %>%
          layout(legend = list(orientation = "h"), margin = list(l = 100)) # , colorway = "black"
        # colorscales: Greys,YlGnBu,Greens,YlOrRd,Bluered,RdBu,Reds,Blues,Picnic,Rainbow,
        # Portland,Jet,Hot,Blackbody,Earth,Electric,Viridis,Cividis
      }
      
      # subplot(cp1,cp2, nrows = 2, shareX = T)
      # browser()
      
      subplot(cp[1:n.plots], nrows = ceiling(sqrt(n.plots)), which_layout = "merge", shareX = T)  # , margin = c(0.5, 0.001, 0.02, 0.02))  # left right top down

  } )  # end contour plot output
  

} # end server function



# Run the application 
shinyApp(ui = ui, server = server)
