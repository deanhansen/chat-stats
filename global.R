# Allow up to 50MB files -------------------------------------------------

options(shiny.maxRequestSize = 50 * 1024^2)

# Packages ---------------------------------------------------------------

suppressMessages(
  suppressMessages(
    suppressWarnings({

      ## For building shiny dashboard
      library(shiny)
      library(shinyjs)
      library(bslib)
      library(bsicons)
      library(DT)

      ## Data cleaning and transformation
      library(jsonlite)
      library(readr)
      library(dplyr)
      library(tidyr)
      library(stringr)
      library(lubridate)
      library(purrr)
      library(forcats)
      library(glue)

      ## Creating and styling plots
      library(ggplot2)
      library(scales)
      library(ggiraph)
      library(showtext)
      library(ggtext)
      library(ggprism)
      library(ggdist)
      library(ggbeeswarm)

      ## Text cleanup
      library(tidytext)
      library(stopwords)

      ## Load tiktoken (GPT-3.5 and GPT-4.5 tokenizer)
      library(reticulate)

      ## Add fonts for the plots
      font_add_google(name = "Share Tech Mono", family = "share")

      ## Set the font resolution
      showtext_auto()
      showtext_opts(dpi = 300)

      ## Set the theme for all plots
      theme_set(
        theme(

          ## Text
          text = element_text(family = "share", colour = "#00ff00"),
          axis.text.x = element_markdown(size = 13, colour = "#00ff00", vjust = 0, margin = margin(t = 12, r = 0, b = 8, l = 0)),
          axis.text.y.right = element_markdown(size = 13, colour = "#00ff00", hjust = 0, margin = margin(t = 0, r = 8, b = 0, l = 12)),
          legend.text = element_markdown(size = 13, colour = "#fff", margin = margin(t = 0, r = 0, b = 0, l = 6)),

          ## Axis line's
          axis.line.x = element_line(colour = "#00ff00", linewidth = 0.5),
          axis.line.y = element_line(colour = "#00ff00", linewidth = 0.5),

          ## Ticks
          axis.ticks.x = element_line(colour = "#00ff00", linewidth = 0.5),
          axis.ticks.length.x = unit(0.16, "in"),
          axis.ticks.y.right = element_line(colour = "#00ff00", linewidth = 0.5),
          axis.ticks.length.y.right = unit(0.16, "in"),

          ## Panel settings
          panel.grid = element_line(colour = "#00ff00", linewidth = 0.5),
          panel.background = element_rect(fill = "#0a0a0a", colour = "#0a0a0a"),

          ## Plot
          plot.background = element_rect(fill = "#0a0a0a", colour = "#0a0a0a"),

          ## Legend
          legend.background = element_rect(fill = "#0a0a0a", colour = "#0a0a0a"),
          legend.key.height = unit(0.3, "in"),
          legend.key.width = unit(0.4, "in"),
          legend.key = element_rect(fill = "#0a0a0a", color = NA),
          legend.key.size = unit(0.5, "lines"),
          legend.spacing.x = unit(0.3, "cm"),
          legend.position = "top"
        )
      )

      ## Load Python module
      tiktoken <- import("tiktoken")

      ## Load tokenizer
      enc <- tiktoken$get_encoding("cl100k_base")
      
    })
  )
)

# Generate Survey Options ------------------------------------------------

generate_survey_options <- function(true_val, n = 100, choose = 3) {
  offsets <- runif(n, min = -0.4, max = 0.2)
  options <- round(true_val * (1 + offsets))
  options <- unique(options[options > 0 & !is.na(options)])
  
  if (length(options) < choose) {
    extra <- round(true_val * (1 + runif(choose - length(options), -0.3, 0.3)))
    extra <- extra[extra > 0]
    options <- unique(c(options, extra))
  }
  
  options <- unique(c(options, true_val))  
  chosen <- sample(options, min(choose, length(options)), replace = FALSE)  
  all_opts <- unique(c(chosen, true_val))
  format(sort(all_opts), big.mark = ",")
}