#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#


library(shiny)
library(tidyverse)
library(DT)
library(leaflet)


#create blank data for user input
blank <- tribble(
    ~Precip_mm, ~Temp_C, ~Location, ~Period,
    0,0,"User Input","1991-2020",0,0,"User Input","1991-2020",
    0,0,"User Input","1991-2020",0,0,"User Input","1991-2020",
    0,0,"User Input","1991-2020",0,0,"User Input","1991-2020",
    0,0,"User Input","1991-2020",0,0,"User Input","1991-2020",
    0,0,"User Input","1991-2020",0,0,"User Input","1991-2020",
    0,0,"User Input","1991-2020",0,0,"User Input","1991-2020"
   
)

LU <- read_csv("appdata_site_info.csv") %>%
    add_row(Location = "User Input", 
            station = NA, lat_dd = 37.1, lon_dd = -95.7, soilmax_mm = 100,
            .before = 1)

PT <- read_csv("appdata_temp_precip_multi.csv") %>%
    bind_rows(blank) 
   

#initialize the water budget function thorn_budget()
source('Thornthwaite_budget_function.R')

#set up plot defaults
# colorblind friendly palette with grey:
cbp1 <- c("#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#remove the first couple so initial colors don't duplicate
cbp2 <- c( "#CC79A7", "#0072B2", "#D55E00")

theme_set(theme_classic())
theme_update(text = element_text(size=20))

# To use for fills, add
# + scale_fill_manual(values = cbp2)

# To use for line colors, add
#+ scale_colour_manual(values=cbp1)

# Define UI for application 
ui <- fluidPage(

    # Application title
    titlePanel("Explore the Water Balance"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(
                inputId = "site",
                label = "Choose a NOAA observing station to show the water balance
                (click and press backspace to search)",
                choices = unique(LU$Location),
                selected = "BURKES GARDEN VA"
            ),
            h4(strong("Dataset 1")),
            selectInput(inputId = "Period30", 
                        label = "Select which 30-year climate values to show 
                        (use 1991-2020 if 'user input' is selected above)",
                        choices = unique(PT$Period),
                        multiple = FALSE,
                        selected = "1991-2020"),
            uiOutput("aswc_slider"),
            uiOutput("lat_slider"),
            sliderInput(inputId = "temp_change_input", 
                        label = "Increase/decrease monthly temps (deg C)",
                        min = -3, 
                        max = 3, 
                        value = 0),
       sliderInput(inputId = "precip_change_input", 
                        label = "Increase/decrease monthly precip (mm)",
                        min = -50, 
                        max = 50, 
                        value = 0),
            h4(strong("Dataset 2")),
            selectInput(inputId = "PeriodComp", 
                        label = "Select climate values 
                        to add as a comparison, 
                        (use NA if 'user input' is selected)",
                        choices = c(unique(PT$Period), "NA"),
                        multiple = FALSE,
                        selected = "NA")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Output Plots",
                                 h4("Right click a plot to copy or save it"),
                                 plotOutput("Inputs"),
                                 plotOutput("Storage"),
                                 plotOutput("Outputs"),
                                 plotOutput("Temps"),
                                 plotOutput("Precip")),
                        tabPanel("Output Data", 
                                 DTOutput("results"),
                                 uiOutput("filenameentry"),
                                 downloadButton("downloadData", "Download These Data")),
                        tabPanel("Map", 
                                 h4("Click a NOAA observing station to see its water balance"),
                                 leafletOutput("Map")),
                        tabPanel("View/Edit Input Data", 
                                 h4("Double-click to change values in dataset 1"),
                                 column(DTOutput("monthlydat"), 
                                        width = 5)),
                        tabPanel("Model Diagram", 
                                 plotOutput("diagram")),
                        tabPanel("Model description",
                                 HTML(read_file("swb_desc.html")))
            
        ))
    )
)


server <- function(input, output, session) {
    
    #soil water capacity slider update to send to UI
    output$aswc_slider <- renderUI({
        
        soilmax <- LU$soilmax_mm[LU$Location == input$site]
        
        sliderInput(inputId = "soilmax_input", 
                    label = "Adjust soil water capacity (mm)",
                    min = 0, 
                    max = soilmax * 2, 
                    value = soilmax)
    })
    
    #latitude slider update to send to UI
    output$lat_slider <- renderUI({
        
        sitelat <- LU$lat_dd[LU$Location == input$site]
        
        sliderInput(inputId = "lat_input", 
                    label = "Change the latitude of the site for PET calculation",
                    min = 0, 
                    max = 60, 
                    value = sitelat)
    })
    
    output$filenameentry <- renderUI({
      
        textInput("filename","Name your output file: ", value = input$site)
      
    })
    
    # Generate an HTML table view of the data ----
   
    md <- reactiveValues(df = NULL)
    
    observe({md$df = as.data.frame(filter(PT, Location == input$site,
                                          Period == input$Period30))})
    
    precip_orig <- reactive({
        PT$Precip_mm[PT$Location == input$site & 
                     PT$Period == input$Period30]
      })
    temp_orig <- reactive({
        PT$Temp_C[PT$Location == input$site &
                  PT$Period == input$Period30]
      })
    
    observeEvent(input$precip_change_input,{
        #md$df[, "Precip_mm"] <- md$df[, "Precip_orig"] + input$precip_change_input
      md$df[, "Precip_mm"] <- precip_orig() + input$precip_change_input  
      md$df[, "Precip_mm"][md$df[, "Precip_mm"] < 0] <- 0
    })

    observeEvent(input$temp_change_input,{
        #md$df[, "Temp_C"] <- md$df[, "Temp_orig"] + input$temp_change_input
        md$df[, "Temp_C"] <- temp_orig() + input$temp_change_input
    })
    
    output$monthlydat <-  renderDT(md$df[1:2], 
                                   selection = 'none', 
                                   editable = TRUE,
                                   rownames = c("Jan","Feb","Mar","Apr",
                                                "May","Jun","Jul","Aug",
                                                "Sep","Oct","Nov","Dec"),
                                   width = 1,
                                   options = list(pageLength = 12, dom = 't')) #turn off search, show all 12 months
    
    proxy = dataTableProxy('monthlydat')
    
    observeEvent(input$monthlydat_cell_edit, { #monthlydat_cell_edit
        info = input$monthlydat_cell_edit
        str(info)
        i = info$row
        j = info$col #+ 1
        v = info$value
        
        md$df[i, j] <<- DT::coerceValue(v, md$df[i, j])
        replaceData(proxy, md$df, resetPaging = FALSE, rownames = FALSE)
    })
    
    
    ### Run the water budget function
    Budget <- reactive({
    
    Monthly1 <- md$df 
    
    SiteInfo <- LU %>% filter(Location == input$site)
    
    Precip1 <-  md$df[, "Precip_mm"] 
    
    budget1 <- thorn_budget(Precip1, 
            Monthly1$Temp_C, 
            input$lat_input, 
            SiteInfo$lon_dd, 
            input$soilmax_input) 
    budget1$Period <- paste("1:", input$Period30)
    
    if(input$PeriodComp != "NA"){
      Monthly2 <- filter(PT, Location == input$site,
                         Period == input$PeriodComp)
      
      if(input$PeriodComp == input$Period30){
        precip2 <- precip_orig()
        temp2 <- temp_orig()
      }else{ 
        precip2 <- Monthly2$Precip_mm
        temp2 <- Monthly2$Temp_C
      }
      
      budget2 <- thorn_budget(precip2, 
                              temp2, 
                              LU$lat_dd[LU$Location == input$site], 
                              SiteInfo$lon_dd, 
                              LU$soilmax_mm[LU$Location == input$site]) 
      
      budget2$Period <- paste("2:", input$PeriodComp)
      
      budget <- rbind(budget1, budget2)
    }else{
      budget <- budget1
    }
    
    })
    
    output$Inputs <- renderPlot({
        #format rain and melt for stacked boxes
        inputs <- Budget() %>% dplyr::select(RAIN, MELT, Month, Period) %>% 
            pivot_longer(c("RAIN","MELT"))
        
      if(input$PeriodComp == "NA"){
        inputs %>% filter(Period == paste("1:",input$Period30)) %>%
            ggplot()+
            geom_bar(aes(x = Month, y = value, fill = name), 
                     stat = "identity", position = "stack", lwd = 1.5)+
            scale_x_continuous(breaks = seq(1:12), 
                             labels = unique(Budget()$MonthName))+
            ylab("mm of water")+
            xlab(element_blank())+
            labs(color = element_blank(), fill = element_blank())+
            scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
            theme(legend.position = "bottom")+
            scale_fill_manual(values = cbp2, 
                              labels=c("Snow Melt", "Rain"))+
            scale_colour_manual(values=cbp1)+
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
            coord_cartesian(clip = "off")+
            ggtitle("Inputs to Soil")
      }else{
        inputs %>% 
          ggplot()+
          geom_bar(aes(x = Month, y = value, fill = name), 
                   stat = "identity", position = "stack", lwd = 1.5)+
          scale_x_continuous(breaks = seq(1:12), 
                             labels = unique(Budget()$MonthName))+
          ylab("mm of water")+
          xlab(element_blank())+
          labs(color = element_blank(), fill = element_blank())+
          scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
          theme(legend.position = "bottom")+
          scale_fill_manual(values = cbp2, 
                            labels=c("Snow Melt", "Rain"))+
          scale_colour_manual(values=cbp1)+
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
          coord_cartesian(clip = "off")+
          ggtitle("Inputs to Soil")+
          facet_grid(rows = 2, facets = "Period")
      }
      
    })
       
    output$Outputs <- renderPlot({ 
        Budget() %>%
            ggplot(aes(Month, SURPLUS, color = "Surplus Water", 
                       group = Period, linetype = Period))+
            geom_line(lwd = 1.5)+
            geom_line(data = Budget(), aes(x = Month, y = ET, color = "ET", 
                                           group = Period,
                                           linetype = Period), lwd = 1.5)+
            scale_x_continuous(breaks = seq(1:12), 
                           labels = unique(Budget()$MonthName))+
            ylab("mm of water")+
            xlab(element_blank())+
            scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
            labs(color = element_blank(), linetype = element_blank())+
            theme(legend.position = "bottom")+
            scale_colour_manual(values=cbp1)+
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
            coord_cartesian(clip = "off")+
            ggtitle("Outputs from Soil")
    })
    
    output$Storage <- renderPlot({ 
        Budget() %>%
            ggplot(aes(Month, y = SOIL, color = "Soil Storage", linetype = Period))+
            geom_line(lwd = 1.5)+
            geom_line(aes(y = PACK, color = "Snow Pack"), lwd = 1.5)+
            scale_x_continuous(breaks = seq(1:12), 
                           labels = unique(Budget()$MonthName))+
            ylab("mm of water")+
            xlab(element_blank())+
            scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
            labs(color = element_blank(), linetype = element_blank())+
            theme(legend.position = "bottom")+
            scale_colour_manual(values=cbp1)+
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
            coord_cartesian(clip = "off")+
            ggtitle("Storage")
    })
    
  output$Temps <- renderPlot({   
        Budget() %>%
            ggplot(aes(x = Month, y = Temp, linetype = Period))+
            geom_line(lwd = 1.5)+
            scale_x_continuous(breaks = seq(1:12), 
                         labels = unique(Budget()$MonthName))+
            ylab("Temperature (deg C)")+
            xlab(element_blank())+
            scale_colour_manual(values=cbp1)+
            labs(linetype = element_blank())+
            coord_cartesian(clip = "off")+
            theme(legend.position = "bottom")+
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  })
  
  output$Precip <- renderPlot({   
    
    #format rain and melt for stacked boxes
    inputs <- Budget() %>% dplyr::select(RAIN, SNOW, Month, Period) %>% 
      pivot_longer(c("RAIN","SNOW"))
    
  if(input$PeriodComp == "NA"){
    inputs %>% filter(Period == paste("1:",input$Period30)) %>%
    ggplot()+
      geom_bar(aes(x = Month, y = value, fill = name), 
               stat = "identity", position = "stack", lwd = 2)+
      scale_x_continuous(breaks = seq(1:12), 
                         labels = unique(Budget()$MonthName))+
      ylab("mm of water")+
      xlab(element_blank())+
      labs(color = element_blank(), fill = element_blank())+
      scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
      theme(legend.position = "bottom")+
      scale_fill_manual(values = c(cbp2[2], cbp2[1]), 
                        labels=c("Rain", "Snow"))+
      scale_colour_manual(values=c(cbp1[2], cbp1[1]))+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
      coord_cartesian(clip = "off")+
      ggtitle("Precipitation")  
    }else{
      inputs %>% 
        ggplot()+
        geom_bar(aes(x = Month, y = value, fill = name), 
                 stat = "identity", position = "stack", lwd = 2)+
        scale_x_continuous(breaks = seq(1:12), 
                           labels = unique(Budget()$MonthName))+
        ylab("mm of water")+
        xlab(element_blank())+
        labs(color = element_blank(), fill = element_blank())+
        scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
        theme(legend.position = "bottom")+
        scale_fill_manual(values = c(cbp2[2], cbp2[1]), 
                          labels=c("Rain", "Snow"))+
        scale_colour_manual(values=c(cbp1[2], cbp1[1]))+
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
        coord_cartesian(clip = "off")+
        ggtitle("Precipitation") +
        facet_grid(rows = 2, facets = "Period")
    }
    
  })
  
  output$Map <- renderLeaflet({
      leaflet() %>% #addTiles() %>%
          addProviderTiles("OpenTopoMap",
                           options = providerTileOptions(noWrap = TRUE)) %>%
          addCircleMarkers(layerId = LU$Location, 
                           lat = LU$lat_dd, #input$lat_input, 
                           lng = LU$lon_dd,
                          radius = 4,
                          fillOpacity = 0.65) %>%
          addCircleMarkers(lat = LU$lat_dd[LU$Location == input$site],
                           lng = LU$lon_dd[LU$Location == input$site],
                           #radius = 4, 
                           color = "red") %>%
          setView(lat = input$lat_input, 
                  lng = LU$lon_dd[LU$Location == input$site],
                  zoom = 5)
  })
 
  #select sites by clicking on them
  observeEvent(input$Map_marker_click, {
      site <- input$Map_marker_click
      siteID <- site$id
      updateSelectInput(session, "site", selected = siteID)
  })
  
  output$diagram <- renderImage({
      filename <- normalizePath(file.path('./www/Balance_Diagram.png'))
      list(src = filename)
  }, deleteFile = FALSE)
  
  output$results <- renderDataTable(Budget(),
                    options = list(pageLength = nrow(Budget()), dom = 't'),
                   )
  output$downloadData <- downloadHandler(
    filename = function(){
      paste(input$filename, ".csv", sep = "")
    },
    content = function(file){
      write.csv(Budget(), file, row.names = FALSE)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
