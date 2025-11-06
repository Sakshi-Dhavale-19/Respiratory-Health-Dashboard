# Required Libraries
library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(DT)
library(sf)
library(viridis)
library(geojsonio)

# Enhanced Data Preprocessing Function
prepare_respiratory_data <- function() {
  # Read and process the data
  data <- read.csv("Data/Full_Data.csv")
  
  # Process GP level data first
  gp_data <- data %>%
    group_by(Practice_code, Practice_name, PCN_ODS_code, PCN_name, CCG.Name, 
             Integrated.Care.Board.Name, NHSE.Region.Name) %>%
    summarise(
      COPD_Prevalence = round(mean(COPD_Prevalence_2021_22, na.rm = TRUE), 2),
      Asthma_Prevalence = round(mean(`X2020_21_Asthma..QOF.prevalence..6..yrs.`, na.rm = TRUE), 2),
      COPD_Emergency_Admissions = round(mean(`X2020_21_Emergency.hospital.admissions.for.COPD..all.ages`, na.rm = TRUE),2),
      Asthma_Emergency_Admissions = round(mean(`X2020_21_Emergency.hospital.admissions.for.asthma.in.adults..aged.19.years.and.over.`, na.rm = TRUE),2),
      COPD_Achievement_Score = round(mean(COPD_Achievement_2021_22.., na.rm = TRUE), 2),
      List_Size = sum(List_size_2021_22, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    # Handle division by zero and NaN cases
    mutate(
      COPD_Emergency_Rate = round(ifelse(
        COPD_Prevalence > 0,
        (COPD_Emergency_Admissions / (List_Size * COPD_Prevalence/100)) * 1000,
        0
      ), 2),
      Asthma_Emergency_Rate = round(ifelse(
        Asthma_Prevalence > 0,
        (Asthma_Emergency_Admissions / (List_Size * Asthma_Prevalence/100)) * 1000,
        0
      ), 2)
    )
  
  return(gp_data)
}

calculate_performance_metrics <- function(data) {
  data %>%
    summarise(
      avg_copd_prevalence = round(weighted.mean(COPD_Prevalence, List_Size, na.rm = TRUE), 2),
      avg_asthma_prevalence = round(weighted.mean(Asthma_Prevalence, List_Size, na.rm = TRUE), 2),
      avg_copd_emergency = round(weighted.mean(COPD_Emergency_Rate, List_Size, na.rm = TRUE), 2),
      avg_achievement = round(weighted.mean(COPD_Achievement_Score, List_Size, na.rm = TRUE), 2),
      total_practices = n(),
      total_patients = sum(List_Size, na.rm = TRUE)
    )
}

# Add tooltips for better data interpretation
add_tooltip <- function(plot, title) {
  plot %>%
    layout(
      title = list(
        text = title,
        font = list(size = 16)
      ),
      hovermode = "closest",
      hoverlabel = list(
        bgcolor = "white",
        font = list(size = 12)
      )
    )
}

# Function to read and process boundaries
read_boundaries <- function() {
  ccg_boundaries <- geojson_read(
    "https://raw.githubusercontent.com/ONSvisual/topojson_boundaries/master/geogCCG2015.json",
    what = "sp"
  )
  
  ccg_sf <- st_as_sf(ccg_boundaries) %>%
    mutate(
      AREANM_Clean = toupper(gsub("NHS |CCG|ICB|AND|&", "", AREANM)) %>%
        trimws() %>%
        gsub("\\s+", " ", .)
    )
  
  return(ccg_sf)
}

# Function to aggregate data based on selected level
aggregate_data <- function(data, level) {
  if(level == "GP") {
    return(data)
  }
  
  group_cols <- switch(level,
                       "PCN" = c("PCN_ODS_code", "PCN_name", "CCG.Name", "Integrated.Care.Board.Name", "NHSE.Region.Name"),
                       "CCG" = c("CCG.Name", "Integrated.Care.Board.Name", "NHSE.Region.Name"),
                       "ICB" = c("Integrated.Care.Board.Name", "NHSE.Region.Name"),
                       "Region" = "NHSE.Region.Name"
  )
  
  if(is.null(group_cols)) {
    stop("Invalid level parameter")
  }
  
  data %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(
      COPD_Prevalence = weighted.mean(COPD_Prevalence, List_Size, na.rm = TRUE),
      Asthma_Prevalence = weighted.mean(Asthma_Prevalence, List_Size, na.rm = TRUE),
      COPD_Emergency_Rate = weighted.mean(COPD_Emergency_Rate, List_Size, na.rm = TRUE),
      Asthma_Emergency_Rate = weighted.mean(Asthma_Emergency_Rate, List_Size, na.rm = TRUE),
      COPD_Achievement_Score = weighted.mean(COPD_Achievement_Score, List_Size, na.rm = TRUE),
      List_Size = sum(List_Size, na.rm = TRUE),
      Practice_Count = n(),
      .groups = 'drop'
    )
}

# UI Definition
ui <- dashboardPage(
  dashboardHeader(title = "Respiratory Health Dashboard : COPD & Asthma Insights (2020–2022)"),
  dashboardSidebar(
    sidebarMenu(
      # Analysis Level Selection
      selectInput("level", "Analysis Level:",
                  choices = c("GP", "PCN", "CCG", "ICB", "Region"),
                  selected = "CCG"),
      
      # Filters
      selectizeInput("region_filter", "NHSE Region:",
                     choices = NULL,
                     selected = NULL,
                     options = list(placeholder = 'Select a region')),
      selectizeInput("icb_filter", "ICB:",
                     choices = NULL,
                     selected = NULL,
                     options = list(placeholder = 'Select an ICB')),
      selectizeInput("ccg_filter", "CCG:",
                     choices = NULL,
                     selected = NULL,
                     options = list(placeholder = 'Select a CCG')),
      selectizeInput("pcn_filter", "PCN:",
                     choices = NULL,
                     selected = NULL,
                     options = list(placeholder = 'Select a PCN')),
      
      # Menu Items
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Geographic View", tabName = "geography", icon = icon("map")),
      menuItem("Detailed Analysis", tabName = "analysis", icon = icon("chart-line")),
      menuItem("Data Table", tabName = "table", icon = icon("table"))
    )
  ),
  dashboardBody(
    # Add custom CSS for responsive design
    tags$head(
      tags$style(HTML("
        .content-wrapper { overflow: auto; }
        .box { margin-bottom: 15px; }
        .leaflet-container { min-height: 400px; }
        .value-box { margin-bottom: 15px; }
        .small-box { margin-bottom: 15px; }
        .nav-tabs-custom { margin-bottom: 15px; }
      "))
    ),
    tabItems(
      # Overview Tab
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("total_practices_box", width = 4),
                valueBoxOutput("avg_copd_box", width = 4),
                valueBoxOutput("achievement_box", width = 4)
              ),
              fluidRow(
                box(
                  title = "COPD vs Asthma Prevalence",
                  status = "primary",
                  solidHeader = TRUE,
                  plotlyOutput("prevalence_comparison"),
                  width = 12,
                  height = 500
                )
              ),
              fluidRow(
                box(
                  title = "Emergency Admission Rates",
                  status = "warning",
                  solidHeader = TRUE,
                  plotlyOutput("emergency_rates"),
                  width = 6,
                  height = 400
                ),
                box(
                  title = "Achievement Distribution",
                  status = "success",
                  solidHeader = TRUE,
                  plotlyOutput("achievement_dist"),
                  width = 6,
                  height = 400
                )
              )
      ),
      
      # Geographic View Tab
      tabItem(tabName = "geography",
              fluidRow(
                box(
                  title = "Geographic Distribution",
                  status = "primary",
                  solidHeader = TRUE,
                  leafletOutput("map", height = 600),
                  width = 12
                )
              ),
              fluidRow(
                box(
                  title = "Regional Comparison",
                  status = "info",
                  solidHeader = TRUE,
                  plotlyOutput("regional_comparison"),
                  width = 12,
                  height = 400
                )
              )
      ),
      
      # Detailed Analysis Tab
      tabItem(tabName = "analysis",
              fluidRow(
                box(
                  title = "COPD Achievement vs Emergency Rates",
                  status = "primary",
                  solidHeader = TRUE,
                  plotlyOutput("performance_scatter"),
                  width = 12,
                  height = 500
                )
              ),
              fluidRow(
                box(
                  title = "List Size vs Prevalence",
                  status = "warning",
                  solidHeader = TRUE,
                  plotlyOutput("size_prevalence"),
                  width = 6,
                  height = 400
                ),
                box(
                  title = "Achievement Score Analysis",
                  status = "success",
                  solidHeader = TRUE,
                  plotlyOutput("achievement_analysis"),
                  width = 6,
                  height = 400
                )
              )
      ),
      
      # Data Table Tab
      tabItem(tabName = "table",
              fluidRow(
                box(
                  title = "Detailed Metrics",
                  status = "primary",
                  solidHeader = TRUE,
                  DTOutput("metrics_table"),
                  width = 12
                )
              )
      )
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  # Load and prepare data
  raw_data <- reactive({
    withProgress(message = 'Loading data...', {
      tryCatch({
        prepare_respiratory_data()
      }, error = function(e) {
        showNotification(
          paste("Error loading data:", e$message),
          type = "error"
        )
        return(NULL)
      })
    })
  })
  
  # Initialize filter choices
  observe({
    req(raw_data())
    data <- raw_data()
    
    updateSelectizeInput(session, "region_filter",
                         choices = c("All", sort(unique(data$NHSE.Region.Name))),
                         selected = "All",
                         server = TRUE)
  })
  
  # Update ICB choices based on region selection
  observeEvent(input$region_filter, {
    req(raw_data())
    data <- raw_data()
    filtered_data <- data
    
    if(input$region_filter != "All") {
      filtered_data <- filter(filtered_data, NHSE.Region.Name == input$region_filter)
    }
    
    updateSelectizeInput(session, "icb_filter",
                         choices = c("All", sort(unique(filtered_data$Integrated.Care.Board.Name))),
                         selected = "All",
                         server = TRUE)
  })
  
  # Update CCG choices based on ICB selection
  observeEvent(input$icb_filter, {
    req(raw_data())
    data <- raw_data()
    filtered_data <- data
    
    if(input$region_filter != "All") {
      filtered_data <- filter(filtered_data, NHSE.Region.Name == input$region_filter)
    }
    
    if(input$icb_filter != "All") {
      filtered_data <- filter(filtered_data, Integrated.Care.Board.Name == input$icb_filter)
    }
    
    updateSelectizeInput(session, "ccg_filter",
                         choices = c("All", sort(unique(filtered_data$CCG.Name))),
                         selected = "All",
                         server = TRUE)
  })
  
  # Update PCN choices based on CCG selection
  observeEvent(input$ccg_filter, {
    req(raw_data())
    data <- raw_data()
    filtered_data <- data
    
    if(input$region_filter != "All") {
      filtered_data <- filter(filtered_data, NHSE.Region.Name == input$region_filter)
    }
    
    if(input$icb_filter != "All") {
      filtered_data <- filter(filtered_data, Integrated.Care.Board.Name == input$icb_filter)
    }
    
    if(input$ccg_filter != "All") {
      filtered_data <- filter(filtered_data, CCG.Name == input$ccg_filter)
    }
    
    updateSelectizeInput(session, "pcn_filter",
                         choices = c("All", sort(unique(filtered_data$PCN_name))),
                         selected = "All",
                         server = TRUE)
  })
  
  # Filtered Data
  filtered_data <- reactive({
    req(raw_data())
    data <- raw_data()
    
    # Apply filters sequentially
    if(input$region_filter != "All")
      data <- data %>% filter(NHSE.Region.Name == input$region_filter)
    
    if(input$icb_filter != "All")
      data <- data %>% filter(Integrated.Care.Board.Name == input$icb_filter)
    
    if(input$ccg_filter != "All")
      data <- data %>% filter(CCG.Name == input$ccg_filter)
    
    if(input$pcn_filter != "All")
      data <- data %>% filter(PCN_name == input$pcn_filter)
    
    # Aggregate data based on selected level
    aggregate_data(data, input$level)
  })
  
  # Value Boxes
  output$total_practices_box <- renderValueBox({
    req(filtered_data())
    data <- filtered_data()
    valueBox(
      formatC(nrow(data), big.mark = ","),
      if(input$level == "GP") "GP Practices" else paste(input$level, "Organizations"),
      icon = icon("hospital"),
      color = "blue"
    )
  })
  
  output$avg_copd_box <- renderValueBox({
    req(filtered_data())
    data <- filtered_data()
    valueBox(
      paste0(round(weighted.mean(data$COPD_Prevalence, data$List_Size, na.rm = TRUE), 2), "%"),
      "Average COPD Prevalence",
      icon = icon("percentage"),
      color = "yellow"
    )
  })
  
  output$achievement_box <- renderValueBox({
    req(filtered_data())
    data <- filtered_data()
    valueBox(
      paste0(round(weighted.mean(data$COPD_Achievement_Score, data$List_Size, na.rm = TRUE), 1), "%"),
      "Average QOF Achievement",
      icon = icon("award"),
      color = "green"
    )
  })
  
  # Map Output 1
  output$map <- renderLeaflet({
    req(filtered_data())
    data <- filtered_data()
    
    if(input$level == "CCG") {
      # Load boundaries
      boundaries <- tryCatch({
        read_boundaries()
      }, error = function(e) {
        showNotification(
          paste("Error loading boundary data:", e$message),
          type = "error"
        )
        return(NULL)
      })
      
      if(!is.null(boundaries)) {
        # Clean CCG names in both datasets for better matching
        clean_name <- function(x) {
          toupper(gsub("[^[:alnum:]]", " ", x)) %>%
            trimws() %>%
            gsub("\\s+", " ", .)
        }
        
        data <- data %>%
          mutate(CCG.Name.Clean = clean_name(CCG.Name))
        
        boundaries <- boundaries %>%
          mutate(AREANM.Clean = clean_name(AREANM))
        
        # Join data with boundaries
        map_data <- boundaries %>%
          left_join(data, by = c("AREANM.Clean" = "CCG.Name.Clean"))
        
        # Create color palette
        pal <- colorNumeric(
          palette = "viridis",
          domain = range(map_data$COPD_Prevalence, na.rm = TRUE)
        )
        
        leaflet(map_data) %>%
          addPolygons(
            fillColor = ~pal(COPD_Prevalence),
            weight = 2,
            opacity = 1,
            color = "white",
            dashArray = "3",
            fillOpacity = 0.7,
            highlight = highlightOptions(
              weight = 5,
              color = "#666",
              dashArray = "",
              fillOpacity = 0.7,
              bringToFront = TRUE
            ),
            label = ~sprintf(
              "<strong>%s</strong><br/>COPD Prevalence: %.2f%%<br/>Emergency Rate: %.2f",
              AREANM, COPD_Prevalence, COPD_Emergency_Rate
            ) %>% lapply(HTML),
            labelOptions = labelOptions(
              style = list("font-weight" = "normal", padding = "3px 8px"),
              textsize = "15px",
              direction = "auto"
            )
          ) %>%
          addLegend(
            position = "bottomright",
            pal = pal,
            values = ~COPD_Prevalence,
            title = "COPD Prevalence (%)",
            opacity = 0.7
          )
      }
    } else {
      leaflet() %>%
        addTiles() %>%
        setView(lng = -2, lat = 54, zoom = 6) %>%
        addControl(
          html = "<p>Map view only available at CCG level</p>",
          position = "topright"
        )
    }
  })
  
  # Map Output 2
  output$map <- renderLeaflet({
    req(filtered_data())
    data <- filtered_data()
    
    if(input$level == "CCG") {
      # Load boundaries
      boundaries <- tryCatch({
        read_boundaries()
      }, error = function(e) {
        showNotification(
          paste("Error loading boundary data:", e$message),
          type = "error"
        )
        return(NULL)
      })
      
      if(!is.null(boundaries)) {
        # Clean CCG names in both datasets for better matching
        clean_name <- function(x) {
          toupper(gsub("[^[:alnum:]]", " ", x)) %>%
            trimws() %>%
            gsub("\\s+", " ", .)
        }
        
        data <- data %>%
          mutate(CCG.Name.Clean = clean_name(CCG.Name))
        
        boundaries <- boundaries %>%
          mutate(AREANM.Clean = clean_name(AREANM))
        
        # Join data with boundaries
        map_data <- boundaries %>%
          left_join(data, by = c("AREANM.Clean" = "CCG.Name.Clean"))
        
        # Create color palette
        pal <- colorNumeric(
          palette = "viridis",
          domain = range(map_data$Asthma_Prevalence, na.rm = TRUE)
        )
        
        leaflet(map_data) %>%
          addPolygons(
            fillColor = ~pal(Asthma_Prevalence),
            weight = 2,
            opacity = 1,
            color = "white",
            dashArray = "3",
            fillOpacity = 0.7,
            highlight = highlightOptions(
              weight = 5,
              color = "#666",
              dashArray = "",
              fillOpacity = 0.7,
              bringToFront = TRUE
            ),
            label = ~sprintf(
              "<strong>%s</strong><br/>COPD Prevalence: %.2f%%<br/>Emergency Rate: %.2f",
              AREANM, Asthma_Prevalence, Asthma_Emergency_Rate
            ) %>% lapply(HTML),
            labelOptions = labelOptions(
              style = list("font-weight" = "normal", padding = "3px 8px"),
              textsize = "15px",
              direction = "auto"
            )
          ) %>%
          addLegend(
            position = "bottomright",
            pal = pal,
            values = ~Asthma_Prevalence,
            title = "Asthma Prevalence (%)",
            opacity = 0.7
          )
      }
    } else {
      leaflet() %>%
        addTiles() %>%
        setView(lng = -2, lat = 54, zoom = 6) %>%
        addControl(
          html = "<p>Map view only available at CCG level</p>",
          position = "topright"
        )
    }
  })
  
  # Prevalence Comparison Plot
  output$prevalence_comparison <- renderPlotly({
    req(filtered_data())
    data <- filtered_data()
    
    plot <- plot_ly(data) %>%
      add_trace(
        x = ~COPD_Prevalence,
        y = ~Asthma_Prevalence,
        type = 'scatter',
        mode = 'markers',
        marker = list(
          size = ~sqrt(List_Size/1000),
          color = ~COPD_Achievement_Score,
          colorscale = 'Viridis',
          showscale = TRUE,
          colorbar = list(title = 'Achievement Score (%)')
        ),
        hoverinfo = 'text',
        text = ~paste(
          if(input$level == "GP") Practice_name else 
            if(input$level == "PCN") PCN_name else
              if(input$level == "CCG") CCG.Name else
                if(input$level == "ICB") Integrated.Care.Board.Name else NHSE.Region.Name,
          '<br>COPD Prevalence:', sprintf("%.2f%%", COPD_Prevalence),
          '<br>Asthma Prevalence:', sprintf("%.2f%%", Asthma_Prevalence),
          '<br>List Size:', formatC(List_Size, big.mark = ","),
          '<br>Achievement Score:', sprintf("%.2f%%", COPD_Achievement_Score)
        )
      )
    
    # Remove missing data for trend line and correlation calculation
    data_complete <- na.omit(data[, c("COPD_Prevalence", "Asthma_Prevalence")])
    
    # Add trend line and calculate correlation if there is sufficient data
    if (nrow(data_complete) > 1) {
      fit <- lm(Asthma_Prevalence ~ COPD_Prevalence, data = data_complete)
      correlation <- cor(data_complete$COPD_Prevalence, data_complete$Asthma_Prevalence)
      
      plot <- plot %>%
        add_lines(
          x = data_complete$COPD_Prevalence,
          y = fitted(fit),
          line = list(color = 'red', dash = 'dash'),
          name = sprintf("Trend (r = %.2f)", correlation)
        ) %>%
        layout(
          title = 'COPD vs Asthma Prevalence',
          xaxis = list(title = 'COPD Prevalence (%)'),
          yaxis = list(title = 'Asthma Prevalence (%)'),
          annotations = list(
            x = max(data_complete$COPD_Prevalence, na.rm = TRUE),
            y = max(data_complete$Asthma_Prevalence, na.rm = TRUE),
            text = sprintf("Correlation: %.2f", correlation),
            showarrow = FALSE
          )
        )
    } else {
      plot <- plot %>%
        layout(
          title = 'COPD vs Asthma Prevalence',
          xaxis = list(title = 'COPD Prevalence (%)'),
          yaxis = list(title = 'Asthma Prevalence (%)')
        )
    }
    
    add_tooltip(plot, "Prevalence Comparison Analysis")
  })
  
  # Add insights text output
  output$prevalence_insights <- renderText({
    req(filtered_data())
    data <- filtered_data()
    
    metrics <- calculate_performance_metrics(data)
    
    sprintf(
      "Key Insights:\n
      - Average COPD Prevalence: %.2f%%\n
      - Average Asthma Prevalence: %.2f%%\n
      - COPD Emergency Rate: %.2f per 1,000 patients\n
      - Achievement Score: %.2f%%\n
      - Total Practices: %d\n
      - Total Patients: %s",
      metrics$avg_copd_prevalence,
      metrics$avg_asthma_prevalence,
      metrics$avg_copd_emergency,
      metrics$avg_achievement,
      metrics$total_practices,
      formatC(metrics$total_patients, big.mark = ",")
    )
  })
  
  # Emergency Rates Plot
  output$emergency_rates <- renderPlotly({
    req(filtered_data())
    data <- filtered_data()
    
    plot_ly(data) %>%
      add_boxplot(
        y = ~COPD_Emergency_Rate,
        name = 'COPD',
        boxpoints = 'outliers',
        jitter = 0.3,
        pointpos = -1.8
      ) %>%
      add_boxplot(
        y = ~Asthma_Emergency_Rate,
        name = 'Asthma',
        boxpoints = 'outliers',
        jitter = 0.3,
        pointpos = -1.8
      ) %>%
      layout(
        title = 'Emergency Admission Rates Distribution',
        yaxis = list(title = 'Emergency Admissions per 1000 Patients'),
        showlegend = TRUE
      )
  })
  
  # Achievement Distribution Plot
  output$achievement_dist <- renderPlotly({
    req(filtered_data())
    data <- filtered_data()
    
    plot_ly(data) %>%
      add_histogram(
        x = ~COPD_Achievement_Score,
        nbinsx = 30,
        name = 'Achievement Score'
      ) %>%
      layout(
        title = 'Distribution of COPD Achievement Scores',
        xaxis = list(title = 'Achievement Score (%)'),
        yaxis = list(title = 'Count'),
        bargap = 0.1
      )
  })
  
  
  output$performance_scatter <- renderPlotly({
    req(filtered_data())
    data <- filtered_data()
    
    plot_ly(data) %>%
      add_trace(
        x = ~COPD_Emergency_Rate,
        y = ~COPD_Achievement_Score,
        type = 'scatter',
        mode = 'markers',
        marker = list(
          size = ~sqrt(List_Size/1000),
          color = ~COPD_Prevalence,
          colorscale = 'Viridis',
          showscale = TRUE,
          colorbar = list(title = 'COPD Prevalence (%)')
        ),
        text = ~paste(
          if(input$level == "GP") Practice_name else 
            if(input$level == "PCN") PCN_name else
              if(input$level == "CCG") CCG.Name else
                if(input$level == "ICB") Integrated.Care.Board.Name else NHSE.Region.Name,
          '<br>Emergency Rate:', round(COPD_Emergency_Rate, 2),
          '<br>Achievement Score:', round(COPD_Achievement_Score, 2), '%',
          '<br>COPD Prevalence:', round(COPD_Prevalence, 2), '%',
          '<br>List Size:', formatC(List_Size, big.mark = ",")
        ),
        hoverinfo = 'text'
      ) %>%
      layout(
        title = 'COPD Achievement vs Emergency Rates',
        xaxis = list(title = 'Emergency Rate (per 1,000 patients)'),
        yaxis = list(title = 'Achievement Score (%)'),
        showlegend = FALSE
      )
  })
  
  
  
  # Regional Comparison Plot
  output$regional_comparison <- renderPlotly({
    req(filtered_data())
    data <- filtered_data()
    
    # Determine grouping column based on analysis level
    group_col <- switch(input$level,
                        "GP" = "Practice_name",
                        "PCN" = "PCN_name",
                        "CCG" = "CCG.Name",
                        "ICB" = "Integrated.Care.Board.Name",
                        "Region" = "NHSE.Region.Name")
    
    # Create summary based on selected level
    summary_data <- data %>%
      group_by(across(all_of(group_col))) %>%
      summarise(
        avg_copd = weighted.mean(COPD_Prevalence, List_Size, na.rm = TRUE),
        avg_emergency = weighted.mean(COPD_Emergency_Rate, List_Size, na.rm = TRUE),
        avg_achievement = weighted.mean(COPD_Achievement_Score, List_Size, na.rm = TRUE),
        total_list_size = sum(List_Size, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      # Sort by total list size to show largest organizations first
      arrange(desc(total_list_size)) %>%
      # Take top 10 for readability
      slice_head(n = 10)
    
    plot_ly(summary_data) %>%
      add_trace(
        x = as.formula(paste0("~", group_col)),
        y = ~avg_copd,
        type = 'bar',
        name = 'COPD Prevalence (%)'
      ) %>%
      add_trace(
        x = as.formula(paste0("~", group_col)),
        y = ~avg_achievement,
        type = 'scatter',
        mode = 'lines+markers',
        yaxis = 'y2',
        name = 'Achievement Score (%)'
      ) %>%
      layout(
        title = paste('Performance Comparison by', gsub("_", " ", input$level), '(Top 10 by List Size)'),
        xaxis = list(
          title = gsub(".Name", "", input$level),
          tickangle = 45
        ),
        yaxis = list(
          title = 'COPD Prevalence (%)',
          range = c(0, max(summary_data$avg_copd) * 1.1)
        ),
        yaxis2 = list(
          title = 'Achievement Score (%)',
          overlaying = "y",
          side = "right",
          range = c(0, 100)
        ),
        margin = list(b = 100),  # Add bottom margin for rotated labels
        showlegend = TRUE
      )
  })
  
  # Fixed Size vs Prevalence Plot
  output$size_prevalence <- renderPlotly({
    req(filtered_data())
    data <- filtered_data()
    
    plot_ly(data) %>%
      add_trace(
        x = ~List_Size,
        y = ~COPD_Prevalence,
        type = 'scatter',
        mode = 'markers',
        marker = list(
          color = ~COPD_Achievement_Score,
          colorscale = 'Viridis',
          showscale = TRUE,
          colorbar = list(title = 'Achievement Score (%)')
        ),
        text = ~paste(
          if(input$level == "GP") Practice_name else 
            if(input$level == "PCN") PCN_name else
              if(input$level == "CCG") CCG.Name else
                if(input$level == "ICB") Integrated.Care.Board.Name else NHSE.Region.Name,
          '<br>List Size:', formatC(List_Size, big.mark = ","),
          '<br>COPD Prevalence:', round(COPD_Prevalence, 2), '%',
          '<br>Achievement Score:', round(COPD_Achievement_Score, 2), '%'
        ),
        hoverinfo = 'text'
      ) %>%
      layout(
        title = 'List Size vs COPD Prevalence',
        xaxis = list(
          title = 'List Size',
          type = 'log'
        ),
        yaxis = list(title = 'COPD Prevalence (%)'),
        showlegend = FALSE
      )
  })
  
  # Fixed Achievement Analysis Plot
  output$achievement_analysis <- renderPlotly({
    req(filtered_data())
    data <- filtered_data()
    
    plot_ly(data) %>%
      add_trace(
        x = ~COPD_Prevalence,
        y = ~COPD_Achievement_Score,
        type = 'scatter',
        mode = 'markers',
        marker = list(
          size = ~sqrt(List_Size/1000),
          color = ~COPD_Emergency_Rate,
          colorscale = 'Viridis',
          showscale = TRUE,
          colorbar = list(title = 'Emergency Rate')
        ),
        text = ~paste(
          if(input$level == "GP") Practice_name else 
            if(input$level == "PCN") PCN_name else
              if(input$level == "CCG") CCG.Name else
                if(input$level == "ICB") Integrated.Care.Board.Name else NHSE.Region.Name,
          '<br>Prevalence:', round(COPD_Prevalence, 2), '%',
          '<br>Achievement Score:', round(COPD_Achievement_Score, 2), '%',
          '<br>Emergency Rate:', round(COPD_Emergency_Rate, 2)
        ),
        hoverinfo = 'text'
      ) %>%
      layout(
        title = 'COPD Prevalence vs Achievement Score',
        xaxis = list(title = 'COPD Prevalence (%)'),
        yaxis = list(title = 'Achievement Score (%)'),
        showlegend = FALSE
      )
  })
  
  # Metrics Table
  output$metrics_table <- renderDT({
    req(filtered_data())
    data <- filtered_data()
    
    # Format data for display
    display_data <- data %>%
      select(-contains("ODS_code")) %>%
      mutate(
        across(
          ends_with("Prevalence") | ends_with("Score") | ends_with("Rate"),
          ~round(., 2)
        ),
        List_Size = formatC(List_Size, big.mark = ",")
      )
    
    datatable(
      display_data,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
      ),
      extensions = 'Buttons',
      rownames = FALSE
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)