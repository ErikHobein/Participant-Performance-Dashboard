# Load R packages
library(shiny)
library(shinythemes)
library(tidyverse)
library(readxl)
library(DT)
library(plotly)
library(viridis)
library(ggpubr)

# Data
data_test <- read_excel("data.xlsx", sheet = 1)
data_1RM <- read_excel("data.xlsx", sheet = 2)

# define my theme for plots
mytheme <- theme_classic() +
  theme(
    plot.title = element_text(size = 14, color = "black", face = "plain", hjust = 0.5),
    axis.text = element_text(size = 10, color = "black"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.line = element_line(linewidth = 0.9),
    legend.title = element_text(size = 12, face = "bold", color = "black"),
    legend.text = element_text(size = 10, color = "black"),
    legend.key = element_rect(color = FALSE, fill = "grey"))


# Define UI ####
ui <- fluidPage(
  theme = shinytheme("yeti"),
  navbarPage(
    "Cluster Set Study",
    tabPanel(
      "Performance Dashboard",
      sidebarPanel(
        width = 2,
        tags$h3("Input:"),
        selectInput("selected_id", "ID", choices = sort(unique(data_1RM$ID)))
      ),
      mainPanel(
        width = 10,
        h2("Individual Profile"),
        h3("Load-Velocity Profile"),
        div(class = "plot-container", 
            style = "border: 1px solid #ddd; padding: 10px;",  
            plotlyOutput("Load_Velocity_plot", width = "100%", height = "600px")
        ),
        h3("Performance Profile"),
        fluidRow(
          column(8,
                 div(class = "plot-container", 
                     style = "border: 1px solid #ddd; padding: 10px;",  
                     plotOutput("circular_plot", width = "100%", height = "600px")
                 )
          ),
          column(4,
                 div(class = "table-container", 
                     style = "border: 1px solid #ddd; padding: 10px;",
                     dataTableOutput("data_table", width = "100%", height = "600px")
                 )
          )
        )
      )  
    )
  )
)


# Define server function ====
server <- function(input, output, session) {
  # Load - Velocity Profile ====
  output$Load_Velocity_plot <- renderPlotly({
    selected_data <- data_1RM %>% 
      filter(ID == input$selected_id)
    
    if (nrow(selected_data) > 0) {
      # Fit a linear model
      model <- lm(MPV ~ Load, data = selected_data)
      
      # Extract model details
      r_squared <- round(summary(model)$r.squared, 4)
      intercept <- coef(model)[1]
      slope <- coef(model)[2]
      equation <- paste0("y = ", round(intercept, 4), " + ", round(slope, 4), "x")
      
      plot <- ggplot(selected_data, aes(x = Load, y = MPV)) +
        geom_point(color = "#17365c", size = 3) + 
        geom_smooth(method = "lm", se = FALSE, color = "#17365c", linewidth = 1) + 
        labs(x = "Load [kg]", y = "MPV [m/s]") +
        scale_x_continuous(breaks = seq(0, 200, by = 20), limits = c(0, 200)) +
        scale_y_continuous(breaks = seq(0, 1.8, by = 0.2), limits = c(0, 1.8)) +
        mytheme +
        annotate("text", x = 150, y = 1.70, label = paste("R² =", r_squared), size = 5, hjust = 0, color = "#17365c") +
        annotate("text", x = 150, y = 1.50, label = equation, size = 5, hjust = 0, color = "#17365c")
      
      # Convert ggplot to plotly for interactivity
      ggplotly(plot)
    } else {
      plot_ly() %>%
        layout(title = "No data for selected variables")
    }
  })
  
  # circular bar plot with performance profile ====
  output$circular_plot <- renderPlot({
    selected_data <- data_test %>%
      group_by(Sex) %>%
      reframe(
        ID = ID,
        Time = Time,
        `CMJ` = percent_rank(CMJ_mean) * 100,
        `Isometric Force` = percent_rank(Isometric_force_mean) * 100, 
        `1RM Squat` = percent_rank(`1RM`) * 100, 
        `rel. 1RM` = percent_rank(`rel_1RM`) * 100, 
        `v70` = percent_rank(v70_mean) * 100, 
        `Max. Reps` = percent_rank(max_reps) * 100, 
        `10RM Bench-Press` = percent_rank(`10RM_Bench_Press`) * 100, 
        `10RM Leg-Curl` = percent_rank(`10RM_Leg_Curl`) * 100, 
        `10RM Single-Arm-Row` = percent_rank(`10RM_single_arm_row`) * 100
      ) %>%
      # Apply filter after percentile calculation but before pivoting
      filter(ID == input$selected_id) %>%
      pivot_longer(
        cols = starts_with("CMJ") | starts_with("Isometric") | starts_with("1RM") | starts_with("rel.") | starts_with("v70") | starts_with("Max.") | starts_with("10RM"),
        names_to = "Measurement",
        values_to = "Percentiles"
      )
    
    if (nrow(selected_data) > 0) {
      
      plt <- ggplot(selected_data) +
        # Custom panel grid lines
        geom_hline(
          aes(yintercept = y), 
          data.frame(y = seq(0, 100, by = 20)), 
          color = "gray80"
        ) + 
        # Bar chart
        geom_col(
          aes(
            x = reorder(str_wrap(Measurement, 5), Percentiles),
            y = Percentiles,
            fill = Percentiles
          ),
          position = "dodge2",
          show.legend = TRUE,
          alpha = 0.8
        ) +
        # Labels inside bars
        geom_label(
          aes(
            x = reorder(str_wrap(Measurement, 5), Percentiles),
            y = Percentiles - 5,  # Adjust so labels fit inside
            label = round(Percentiles)
          ),
          color = "black",
          fill = "white",
          size = 3,
          fontface = "bold",
          family = "Comic Sans MS",
          show.legend = FALSE
        ) +
        # Make it circular!
        coord_polar() +
        # Annotate custom scale inside plot
        annotate("text", x = 9.7, y = 105, label = "100", size = 4) +
        annotate("text", x = 9.7, y = 85, label = "80", size = 4) +
        annotate("text", x = 9.7, y = 65, label = "60", size = 4) +
        annotate("text", x = 9.7, y = 45, label = "40", size = 4) +
        annotate("text", x = 9.7, y = 25, label = "20", size = 4) +
        annotate("text", x = 9.7, y = 5, label = "0", size = 4) +
        # Scale y axis so bars don’t start in the center
        scale_y_continuous(
          limits = c(-10, 120),  # Adjusted for better spacing
          expand = c(0, 0),
          breaks = seq(0, 100, by = 20)
        ) + 
        # Fill colors and legend adjustments
        scale_fill_viridis_c(
          name = "Percentile",
          option = "C",
          begin = 0,
          end = 0.8,
          guide = guide_colorbar(barwidth = 15, barheight = 0.75)
        ) +
        # Final styling
        theme_void() +
        theme(
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_text(color = "gray12", size = 14),
          legend.position = "bottom"
        )
      
      
      plt  
    } else {
      plot.new() %>%
        layout(title = "No data for selected variables")
    }
  }) 
  
  # Performance Data Table ====
  output$data_table <- renderDataTable({
    # Filter Data
    selected_data <- data_test %>%
      filter(ID == input$selected_id)
    
    if (nrow(selected_data) == 0) {
      return(NULL)  
    }
    
    # Select parameters of relevance
    valid_columns <- c("Sex", "Body_weight", "CMJ_mean", "Isometric_force_mean", 
                       "1RM", "rel_1RM", "v70_mean", 
                       "max_reps", "10RM_Bench_Press", 
                       "10RM_Leg_Curl", "10RM_single_arm_row")
    
    # Filter available parameters
    selected_data <- selected_data %>%
      select(all_of(intersect(valid_columns, colnames(selected_data))))
    
    # Define names of parameters
    new_column_names <- c("Sex","Body Weight [kg]", "CMJ [cm]", "Isometric Force [N/BW]", 
                          "1RM Squat [kg]", "rel. 1RM [kg/BW]", "v70 [m/s]", 
                          "Max. Reps", "10RM Bench Press [kg]", 
                          "10RM Leg Curl [kg]", "10RM Single Arm Row [kg]")
    
    colnames(selected_data) <- new_column_names[1:ncol(selected_data)]
    
    # Round data to 2 decimal places
    selected_data <- selected_data %>%
      mutate(across(where(is.numeric), ~ round(., 2)))
    
    # Transpose the data frame
    transposed_data <- as.data.frame(t(selected_data))
    colnames(transposed_data) <- "Value"
    
    # Add a column for parameters
    transposed_data$Parameter <- rownames(transposed_data)
    transposed_data <- transposed_data[, c("Parameter", "Value")]
    rownames(transposed_data) <- NULL
    
    # Create the datatable output
    datatable_output <- datatable(
      transposed_data,
      options = list(
        scrollX = TRUE,
        autoWidth = TRUE,
        scrollY = '425px', 
        searching = FALSE,  
        paging = FALSE,
        columnDefs = list(
          list(className = 'dt-center', targets = '_all')
        )
      ),
      class = 'cell-border stripe',
      rownames = TRUE,  
      colnames = c("", "")  # Remove column names
    )
    
    # Output of table
    datatable_output
  })
  
}

# Create Shiny App
shinyApp(ui = ui, server = server)

