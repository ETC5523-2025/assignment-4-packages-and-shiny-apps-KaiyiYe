# inst/app/app.R
library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)

.param_opts <- sort(unique(as.character(yarrariver::yarra_wq_period$parameter)))

ui <- fluidPage( 
  theme = shinythemes::shinytheme("flatly"), 
  
  tags$head(tags$style(HTML("
    /* Page background + base text color */
    body { background-color: #f7f9fc; color: #1f2d3d; }

    /* Enlarge page title from titlePanel() */
    .container-fluid > h2, .container-fluid .title {
      font-size: 30px;           /* ↑ 标题更大 */
      line-height: 1.15;
      letter-spacing: .2px;
      margin-top: 6px;
      margin-bottom: 12px;
      font-weight: 700;
    }

    /* Enlarge tab labels (the headbar of tabsetPanel) */
    .nav-tabs > li > a {
      font-size: 18px;           /* ↑ 标签文字更大 */
      padding: 10px 16px;
      font-weight: 600;
      border-radius: 8px 8px 0 0;
    }
    .nav-tabs > li.active > a,
    .nav-tabs > li.active > a:focus,
    .nav-tabs > li.active > a:hover {
      font-size: 18px;           /* active 同步放大 */
    }

    /* Sidebar 'well' -> subtle card */
    .well {
      background: #ffffff !important;
      border: 1px solid #e6e6e6 !important;
      border-radius: 12px !important;
      box-shadow: 0 6px 18px rgba(0,0,0,0.06) !important;
      padding: 16px 18px !important;
    }

    /* Tab content container polish */
    .tab-content {
      background: #ffffff;
      border: 1px solid #e6e6e6; border-top: none;
      border-radius: 0 12px 12px 12px;
      box-shadow: 0 6px 18px rgba(0,0,0,0.06);
      padding: 14px 16px;
    }

    /* Plot containers get breathing room */
    .shiny-plot-output { padding: 6px; }
  "))),
  
  titlePanel("Yarra River Water Quality — 1990s vs Recent"), 
  sidebarLayout( 
    sidebarPanel( 
      width = 3, 
      h4("Controls"), 
      radioButtons( 
        "param_choice", "Parameter", 
        choices = c("All" = "ALL", 
                    setNames(.param_opts, .param_opts)), 
        selected = "ALL", 
        inline = TRUE ), 
      checkboxGroupInput( 
        "period", "Period", 
        choices = c("1990s","recent"), 
        selected = c("1990s","recent"), 
        inline = TRUE 
        ) 
      ), 
    
    mainPanel(
      width = 9,
      tabsetPanel(
        id = "main_tabs",  # optional, handy if you ever want to switch tabs programmatically
        tabPanel(
          "Distribution",
          plotOutput("p_dist", height = "700px")
        ),
        tabPanel(
          "Hourly medians",
          plotOutput("p_hourly", height = "700px")
        ),
        tabPanel(
          "Guide",
          h4("What the fields mean"),
          tags$ul(
            tags$li(tags$b("parameter"), ": pH; Water Temperature (°C); Turbidity (NTU); Salinity as electrical conductivity (µS/cm)."),
            tags$li(tags$b("period"), ": “1990s” (year ≤ 1999) vs “recent” (year ≥ 2015)."),
            tags$li(tags$b("hour"), ": 0–23; derived from measurement time.")
          ),
          h4("How to interpret the outputs"),
          tags$ol(
            tags$li(tags$b("Distribution"), ": Compare medians (box midline) and spread (IQR); points show individual observations."),
            tags$li(tags$b("Hourly medians"), ": Lines show hourly medians; ribbons show IQR (variability); check if one period stays higher/lower across hours.")
          )
        )
      )
    )
  )
)


server <- function(input, output, session) {
  
  base_filtered <- reactive({
    yarrariver::yarra_wq_period |>
      mutate(parameter = as.character(parameter)) |>
      filter(period %in% input$period)
  })
  
  # Distribution Plot
  output$p_dist <- renderPlot({
    df <- base_filtered()
    req(nrow(df) > 0)
    
    if (identical(input$param_choice, "ALL")) {
      ggplot(df, aes(x = period, y = value, colour = period)) +
        geom_boxplot(outlier.shape = NA, width = 0.65) +
        geom_jitter(width = 0.15, alpha = 0.45, size = 1.2) +
        facet_wrap(~ parameter, scales = "free_y", drop = TRUE) +
        labs(title = "Distribution by Period — All Parameters", x = NULL, y = "Value") +
        theme_bw(base_size = 16) +
        theme(
          aspect.ratio = 1,
          legend.position = "none",
          strip.text = element_text(size = 14, face = "bold"),
          axis.text  = element_text(size = 18),
          axis.title = element_text(size = 20),
          plot.title = element_text(size = 22, face = "bold")
        )
    } else {
      d1 <- df |> filter(parameter == input$param_choice)
      req(nrow(d1) > 0)
      ggplot(d1, aes(x = period, y = value, colour = period)) +
        geom_boxplot(outlier.shape = NA, width = 0.65) +
        geom_jitter(width = 0.15, alpha = 0.45, size = 1.2) +
        labs(title = paste0("Distribution by Period — ", input$param_choice),
             x = NULL, y = "Value") +
        theme_bw(base_size = 16) +
        theme(
          aspect.ratio = 1,
          legend.position = "none",
          axis.text  = element_text(size = 18),
          axis.title = element_text(size = 20),
          plot.title = element_text(size = 22, face = "bold")
        )
    }
  })
  
  # Hourly Medians Plot
  output$p_hourly <- renderPlot({
    df <- yarrariver::wq_hourly_median |>
      mutate(parameter = as.character(parameter)) |>
      filter(period %in% input$period)
    req(nrow(df) > 0)
    
    base_plot <- function(dat, title_txt) {
      ggplot(dat, aes(x = hour, y = median, colour = period, fill = period)) +
        geom_ribbon(aes(ymin = q1, ymax = q3), alpha = 0.18, colour = NA) +
        geom_line(linewidth = 1.0) +
        scale_x_continuous(breaks = unique(dat$hour)) +
        labs(title = title_txt, x = "Hour", y = "Median") +
        theme_bw(base_size = 16) +
        theme(
          aspect.ratio = 1,
          legend.position = "bottom",
          legend.text  = element_text(size = 16),
          axis.text    = element_text(size = 14),
          axis.title   = element_text(size = 20),
          strip.text   = element_text(size = 10, face = "bold"),
          plot.title   = element_text(size = 22, face = "bold")
        )
    }
    
    if (identical(input$param_choice, "ALL")) {
      base_plot(df, "Hourly Medians with IQR Bands — All Parameters") +
        facet_wrap(~ parameter, scales = "free_y", drop = TRUE)
    } else {
      d2 <- df |> filter(parameter == input$param_choice)
      req(nrow(d2) > 0)
      base_plot(d2, paste0("Hourly Medians with IQR Bands — ", input$param_choice))
    }
  })
}

shinyApp(ui, server)
