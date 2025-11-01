# inst/app/app.R
library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)

.param_opts <- sort(unique(as.character(yarrariver::yarra_wq_period$parameter)))

ui <- fluidPage(
  theme = shinythemes::shinytheme("flatly"),
  titlePanel("Yarra River Water Quality — 1990s vs Recent"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Controls"),
      radioButtons(
        "param_choice", "Parameter",
        choices  = c("All" = "ALL", setNames(.param_opts, .param_opts)),
        selected = "ALL",
        inline   = TRUE
      ),
      checkboxGroupInput(
        "period", "Period",
        choices  = c("1990s","recent"),
        selected = c("1990s","recent"),
        inline   = TRUE
      ),
      helpText("Using package data only: ",
               code("yarra_wq_period"), " & ", code("wq_hourly_median"),
               ". See ", code("?yarra_wq_period"), ".")
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Distribution",
                 plotOutput("p_dist", height = "700px")
        ),
        tabPanel("Hourly medians",
                 plotOutput("p_hourly", height = "700px")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # 按 period 先做基础过滤；parameter 在作图时处理 ALL vs 单参
  base_filtered <- reactive({
    yarrariver::yarra_wq_period |>
      mutate(parameter = as.character(parameter)) |>
      filter(period %in% input$period)
  })
  
  # ---------- Distribution 图（原始观测值） ----------
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
  
  # ---------- Hourly Medians 图（用小时中位数与 IQR） ----------
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
