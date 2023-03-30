# Load required packages
library(tidyverse) 
library(shiny) 

# Define a function to return the class of facet for a given plot
return_facet_class <- function(plot) {
  # Extract the "facet" component from the plot's layout and return its class
  (class(ggplot_build(plot)[["layout"]][["facet"]]))[1]
}

# Define a function to convert a color to hexadecimal format
col2hex <- function(colour) {
  # Convert the color to RGB format and then to hexadecimal format
  rgb_vals <- col2rgb(colour)
  return(rgb(rgb_vals[1], rgb_vals[2], rgb_vals[3], max = 255))
}

# Define a function to check if a color is in hexadecimal format
is_hex <- function(colour) {
  # Check if the first character of the color string is "#"
  return(substr(colour, 1, 1) == "#")
}

# Define a function to return the background color of a plot
returns_plot_background <- function(plot) {
  # Extract the "fill" component of the "plot.background" component of the plot's theme
  col <- ggplot_build(plot)[["plot"]][["theme"]][["plot.background"]][["fill"]]
  if (is.null(col)) {
    # If the color is null, use the default color from the current theme
    col <- theme_get()[["plot.background"]][["colour"]]
  }
  # Remove the name attribute of the color and convert it to hexadecimal format if it isn't already
  col <- unname(col)
  ifelse(
    {
      is_hex(col)
    },
    {
      return(col)
    },
    {
      return(col2hex(col))
    }
  )
}

# Define a function to create a filtered plot for a FacetWrap plot
create_facet_wrap_filter <- function(plot, panel1var, panel1var_value) {
  # Copy the input plot to avoid modifying the original
  plot_out <- plot
  
  # Filter the data in the copied plot based on the selected panel1var and panel1var_value
  plot_out$data <- plot_out$data %>% filter(!!rlang::parse_expr(paste0(panel1var, sep = "==", panel1var_value)))
  
  # Print the filtered plot
  print(plot_out)
}

# Define a function to create a filtered plot for a FacetGrid plot
create_facet_grid_filter <- function(plot, panel1var, panel2var, panel1var_value, panel2var_value) {
  # Copy the input plot to avoid modifying the original
  plot_out <- plot
  
  # Filter the data in the copied plot based on the selected panel1var, panel2var, panel1var_value, and panel2var_value
  plot_out$data <- plot_out$data %>% filter(!!rlang::parse_expr(paste0(panel1var, sep = "==", panel1var_value)) & !!rlang::parse_expr(paste0(panel2var, sep = "==", panel2var_value)))
  
  # Print the filtered plot
  print(plot_out)
}

# Define a function to build a Shiny app from a given plot
build_shiny_app <- function(plot) {
  
  # Determine the type of facet used in the plot
  plot_type <- return_facet_class(plot)
  
  # If the plot does not have a facet class, stop the function and return an error message
  if(plot_type=="FacetNull"){stop("The plot does not contain a Facet Class")}
  
  # Get the background color of the plot
  background_colour <- returns_plot_background(plot)
  
  # Define the UI of the Shiny app
  ui <- fluidPage(
    # Set the background color of the app to the background color of the plot
    id = "main", 
    tags$style(sprintf('#main { background-color: %s;}',background_colour)), 
    fluidRow(column(6, uiOutput("mainplot")), column(6, plotOutput("subplot")))
  )
  
  # Define the server of the Shiny app
  server <- function(input, output) {
    
    # Output the main plot
    output$plot_render <- renderPlot({
      plot
    })
    
    # Render the main plot as a UI element
    output$mainplot <- renderUI({
      plotOutput("plot_render", click = "plot_click")
    })
    
    # Render the sub-plot
    output$subplot <- renderPlot({
      
      # If the plot has not been clicked yet, print the original plot
      ifelse(
        {
          is.null(input$plot_click)
        },
        {
          print(plot)
        },
        # Otherwise, create a filtered plot based on the clicked data point and the type of facet used
        {
          ifelse(
            {
              plot_type == "FacetWrap"
            },
            {
              print(create_facet_wrap_filter(plot, (input$plot_click)[["mapping"]][["panelvar1"]], (input$plot_click)[["panelvar1"]]))
            },
            {
              print(create_facet_grid_filter(
                plot, (input$plot_click)[["mapping"]][["panelvar1"]], (input$plot_click)[["mapping"]][["panelvar2"]],
                (input$plot_click)[["panelvar1"]], (input$plot_click)[["panelvar2"]]
              ))
            }
          )
        }
      )
      
    })
    
  }
  
  # Create and return the Shiny app
  shinyApp(ui, server)
}

