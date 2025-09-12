# Allow up to 50MB files -------------------------------------------------

options(shiny.maxRequestSize = 50 * 1024^2)

# Packages ---------------------------------------------------------------

suppressMessages(
  suppressMessages(
    suppressWarnings({
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
      library(ggwordcloud)
      library(sysfonts)
      library(showtext)
      library(ggtext)

      # library(ggimage)
      library(ggfx)

      ## Text cleanup
      library(tidytext)

      ## Load tiktoken (GPT-3.5 and GPT-4.5 tokenizer)
      library(reticulate)

      ## ...
      font_add_google(name = "VT323", family = "vt")
      font_add_google(name = "Share Tech Mono", family = "share")

      ## ...
      showtext_auto()
      showtext_opts(dpi = 300)

      ## Plot style settings
      bg_col <- "gray5"
      main_col <- "#66FF00"
      title_font <- "vt"
      body_font <- "share"

      ## ...
      theme_set(
        theme(
          text = element_text(family = "share", colour = "#00ff00"),
          axis.text.x = element_markdown(size = 16, colour = "#00ff00", vjust = 0, margin = margin(t = 12, r = 0, b = 8, l = 0)),
          axis.text.y.right = element_markdown(size = 16, colour = "#00ff00", hjust = 0, margin = margin(t = 0, r = 8, b = 0, l = 12)),
          legend.text = element_markdown(size = 16, colour = "#fff", margin = margin(t = 0, r = 0, b = 0, l = 5)),
          panel.background = element_rect(fill = "#0a0a0a", colour = "#0a0a0a"),
          plot.background = element_rect(fill = "#0a0a0a", colour = "#0a0a0a"),
          legend.background = element_rect(fill = "#0a0a0a", colour = "#0a0a0a"),
          panel.grid = element_line(colour = "#00ff00", linewidth = 0.5),
          axis.line.x = element_line(colour = "#00ff00", linewidth = 0.5),
          axis.ticks.length = unit(0, "in"),
          axis.ticks.x = element_line(linewidth = 0.5),
          axis.ticks.y = element_line(linewidth = 0.5),
          legend.key.height = unit(0.45, "in"),
          legend.key.width = unit(0.6, "in"),
          legend.key = element_rect(fill = "#0a0a0a", color = NA),
          legend.key.size = unit(0.8, "lines"),
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

generate_survey_options <- function(true_val, n = 50, choose = 3) {
  offsets <- runif(n, min = -0.4, max = 0.2)
  options <- round(true_val * (1 + offsets))
  options <- options[options > 0]
  options <- options[!is.na(options)]
  options <- sample(options, 3, replace = FALSE)
  options <- unique(c(options, true_val))
  format(sort(options), big.mark = ",")
}


# Shiny Server -----------------------------------------------------------

function(input, output, session) {

  ## Set of reactive values that we'll update as the user interacts with the app
  conversations <- reactiveVal()
  survey_status <- reactiveVal(value = FALSE)

  ## A reactive value to track which questions have been answered
  answered_questions <- reactiveVal(character(0))

  ## Observe all survey inputs to see which questions have been answered
  observe({

    ## ...
    questions <- survey_questions()
    
    ## Check if questions() is ready before setting up observers
    if (!is.null(questions)) {
      lapply(names(questions), function(q_id) {
        observeEvent(input[[q_id]], {
          current_list <- answered_questions()
          if (!(q_id %in% current_list)) {
            answered_questions(c(current_list, q_id))
          }
        }, ignoreInit = TRUE)
      })
    }
  })

  # Wait for the user to upload their conversation history file ----------
  observeEvent(input$conversation_history, {

    ## Require that the `conversation_history` input exists
    req(input$conversation_history)

    ## Save the path to the uploaded file (instead of assuming the name)
    uploaded_file <- input$conversation_history$datapath
    output_file <- tempfile(fileext = ".json")

    ## Run script that extracts relevant parts of the conversation history into a new .json file
    system2(
      command = "python3",
      args = c("conversations.py", uploaded_file, output_file),
      wait = TRUE
    )

    ## Apply additional data processing to the .json file returned by the Python script
    conversation_history <-
      output_file |>
      read_json() |>
      bind_rows() |>
      unnest_wider(col = messages) |>
      mutate(id = row_number()) |>
      mutate(chat_id = cur_group_id(), .by = title) |>
      mutate(
        ## Set the author id's
        author_id = if_else(author == "user", 1L, 2L),
        ## Get the token counts per message sent to and from the assistant
        text = str_replace_all(
          string = text,
          pattern = "<\\|endoftext\\|>",
          replacement = "endoftext"
        ),
        ## Get the number of tokens (integer)
        tokens = enc$encode_batch(text) |> map_int(length),
        ## Add additional metadata for time based analysis
        create_date = as_date(create_time),
        updated_date = as_date(update_time),
        across(
          .cols = c("create_time", "update_time"),
          .fns = as_datetime
        ),
        across(
          .cols = c("create_time", "update_time"),
          .fns = \(.x) wday(x = .x, week_start = 1),
          .names = "{.col}_wday"
        ),
        across(
          .cols = c("create_time", "update_time"),
          .fns = hour,
          .names = "{.col}_hour"
        )
      ) |>
      select(
        "id",
        "chat_id",
        "title",
        "author_id",
        "author",
        "chat_seq",
        "text",
        "tokens",
        starts_with("create_"),
        starts_with("update_")
      )

    ## Set the `conversations` value to the `conversation_history` data (tibble)
    conversations(conversation_history)

    ## Tell the user their file has been successfully processed
    # showNotification("Processing complete!", type = "message", duration = 3, closeButton = TRUE)

  })

  ## Remove the progress bar once the file is loaded
  observeEvent(input$conversation_history, {
    
    ## Check `conversation_history` exists
    req(input$conversation_history)

    ## This tells the UI to remove the file upload element once we've loaded the data
    session$sendCustomMessage("removeProgressBar", "conversation_history")

  })

  # Create the survey questions once the data has been loaded ------------
  survey_questions <- reactive({

    ## Check `conversations` exists
    req(conversations())

    ## Datapoints required for the survey questions and answers
    n_chats <- n_distinct(conversations()$title)
    n_days <- n_distinct(conversations()$create_date)
    n_messages_user <- nrow(filter(
      conversations(),
      author == "user",
      year(create_date) == 2025
    ))

    ## Possible selections for each question
    q1_options <- generate_survey_options(n_chats)
    q2_options <- generate_survey_options(n_days)
    q3_options <- generate_survey_options(n_messages_user)

    ## List of questions, options and correct answer's
    list(
      ## Based on user data
      q1 = list(
        prompt = "How many separate conversations have you had with ChatGPT?",
        options = list(
          "opt_1" = q1_options[1],
          "opt_2" = q1_options[2],
          "opt_3" = q1_options[3],
          "opt_4" = q1_options[4]
        ),
        correct_answer = format(n_chats, big.mark = ",")
      ),

      ## Based on user data
      q2 = list(
        prompt = "On how many different days have you used ChatGPT?",
        options = list(
          "opt_1" = q2_options[1],
          "opt_2" = q2_options[2],
          "opt_3" = q2_options[3],
          "opt_4" = q2_options[4]
        ),
        correct_answer = format(n_days, big.mark = ",")
      ),

      ## Based on user data
      q3 = list(
        prompt = "How many messages have you sent to ChatGPT so far in 2025?",
        options = list(
          "opt_1" = q3_options[1],
          "opt_2" = q3_options[2],
          "opt_3" = q3_options[3],
          "opt_4" = q3_options[4]
        ),
        correct_answer = format(n_messages_user, big.mark = ",")
      ),

      ## 2022
      q4 = list(
        prompt = "What year was ChatGPT released to the public?",
        options = list(
          "opt_1" = "2020",
          "opt_2" = "2021",
          "opt_3" = "2022",
          "opt_4" = "2023"
        ),
        correct_answer = "2022"
      ),

      ## Generative Pre-Trained Transformer
      q5 = list(
        prompt = "What does GPT stand for?",
        options = list(
          "opt_1" = "general pre-trained transformer",
          "opt_2" = "generative personal talker",
          "opt_3" = "generative pre-trained transformer",
          "opt_4" = "gets people talking"
        ),
        correct_answer = "generative pre-trained transformer"
      ),

      ## AlexNet paper
      q6 = list(
        prompt = "Which of these famous papers started the current deep learning revolution?",
        options = list(
          "opt_1" = "attention is all you need",
          "opt_2" = "imagenet classification with deep cnn's",
          "opt_3" = "be terrified, be scared, the robots are coming for us",
          "opt_4" = "playing atari with deep reinforcement learning"
        ),
        correct_answer = "imagenet classification with deep cnn's"
      )
    )
  })

  # Survey UI ------------------------------------------------------------

  output$survey_ui <- renderUI({
    ## Verify again that `conversations` exists
    req(conversations())

    ## Set a reactive value
    questions <- survey_questions()

    ## Create the sequence of questions on the UI
    tagList(
      lapply(names(questions), function(q_id) {
        question <- questions[[q_id]]
        choices <- question$options
        div(
          id = q_id,
          h4(question$prompt),
          lapply(choices, function(choice) {
            actionButton(
              inputId = paste0(q_id, "_", choice),
              label = choice,
              class = "btn-block survey-btn",
              onclick = paste0("Shiny.setInputValue('", q_id, "', '", choice, "', {priority: 'event'});")
            )
          }),
          hr()
        )
      }),
      actionButton("skip_btn", "Skip"),
      actionButton("submit_survey", "Submit")
    )
  })

  # Function to handle survey submission ---------------------------------

  ## Survey Submission Logic
  observeEvent(input$submit_survey, {
    
    ## Verify `conversations` exists
    req(conversations())
    
    ## Get the total number of questions
    questions <- survey_questions()
    total_questions <- length(questions)

    ## Get the number of answered questions from the reactive value
    num_answered <- length(answered_questions())

    ## Check if all questions have been answered
    if (num_answered < total_questions) {
      showNotification(
        "Please answer all questions before submitting.",
        type = "warning",
        duration = 5
      )
      return() # Exit the function early
    }

    correct_answers <- 0

    ## Check answers and create a new UI with colors
    new_survey_ui <- tagList()
    for (q_id in names(questions)) {
      ## Save the question
      question <- questions[[q_id]]

      ## Save the user's answer
      user_answer <- input[[q_id]]

      ## Save the correct answer for the question
      correct_answer <- question$correct_answer

      ## Check if the answer is correct
      is_correct <- user_answer == correct_answer

      if (is_correct) {
        correct_answers <- correct_answers + 1
      }

      ## Add the question and colored options to the new UI
      new_survey_ui <- tagList(
        new_survey_ui,
        div(
          h3(question$prompt),
          div(
            class = "survey-results",
            lapply(question$options, function(option) {
              btn_class <- "disabled"

              if (option == correct_answer) {
                btn_class <- paste(btn_class, "btn-success")
              } else if (option == user_answer) {
                btn_class <- paste(btn_class, "btn-danger")
              } else {
                btn_class <- paste(btn_class, "btn-outline-secondary")
              }

              actionButton(
                inputId = paste0(q_id, "_", option),
                label = option,
                class = btn_class,
                disabled = "disabled"
              )
            })
          ),
          hr()
        )
      )
    }

    ## Update the modal with the results and continue button
    showModal(
      modalDialog(
        title = paste("You got", correct_answers, "out of", total_questions, "questions right"),
        new_survey_ui,
        footer = actionButton("continue_btn", "Continue", class = "btn-primary"),
        size = "xl"
      )
    )
  })

  ## Observe the 'Skip' button to remove the modal
  observeEvent(input$skip_btn, {
    survey_status(TRUE)
    removeModal()
  })

  ## Observe the 'Continue' button to remove the modal
  observeEvent(input$continue_btn, {
    survey_status(TRUE)
    removeModal()
  })

  ## Add a survey before the dashboard is displayed to the user
  observeEvent(input$conversation_history, {

    ## Check `conversations` exists
    req(conversations())

    ## Set the `survey_status` to FALSE
    survey_status(FALSE)

    ## Controls how the survey options are displayed
    showModal(
      modalDialog(
        title = "This survey is auto-generated based on your conversation history with ChatGPT. You can choose to skip it or see how well you know your usage patterns with ChatGPT!",
        uiOutput("survey_ui"),
        footer = NULL, 
        size = "xl",
        easyClose = FALSE,
        disabled = "disabled"
      )
    )
  })

  # Total messages exchanged with ChatGPT --------------------------------

  output$total_messages_exchanged <- renderText({
    req(conversations())
    conversations() |>
      nrow() |>
      format(big.mark = ",")
  })

  # Favourite day to talk to ChatGPT -------------------------------------

  output$favourite_day_of_the_week <- renderText({
    req(conversations())
    conversations() |>
      group_by(chat_id, create_date) |> 
      reframe(n = n_distinct(chat_id)) |> 
      mutate(wday_label = wday(x = create_date, label = TRUE, abbr = FALSE, week_start = 1)) |> 
      slice_max(order_by = n, n = 1, with_ties = FALSE) |> 
      pull(wday_label) |> 
      as.character()
  })

  # Average token length by user and ChatGPT -----------------------------

  output$average_token_length <- renderText({
    req(conversations())
    
    average_token_length_temp <-
      conversations() |>
      filter(str_to_lower(author) %in% c("user", "assistant")) |>
      group_by(author) |> 
      reframe(average_token_length = round(mean(tokens, na.rm = TRUE), 1)) |>
      arrange(desc(author)) |> 
      pull(average_token_length)

    glue("{average_token_length_temp[1]} ({average_token_length_temp[2]})")

  })

  # Total conversations started ------------------------------------------

  output$total_chats <- renderText({
    req(conversations())
    format(n_distinct(conversations()$title), big.mark = ",")
  })

  # Total days talking to ChatGPT in 2025 --------------------------------

  output$percentage_of_days_in_2025 <- renderText({
    req(conversations())
    check_data_in_2025 <- any(year(conversations()$create_date) == 2025)

    if (check_data_in_2025) {

      days_since_nyd <- as.numeric(today() - ymd(paste0(year(today()), "-01-01")))

      conversations() |> 
      filter(year(create_date) == 2025) |> 
      reframe(days_talking_to_chatgpt = round(n_distinct(create_date) / days_since_nyd, 1)) |> 
      pull(days_talking_to_chatgpt) |> 
      percent_format()()
      
    } else {
      return("No data in 2025")
    }

  })

  # Total days talking to ChatGPT ----------------------------------------

  output$total_days <- renderText({
    req(conversations())
    format(n_distinct(conversations()$create_date), big.mark = ",")
  })

  # First Row Plots - Weekday Bar Chart ----------------------------------

  output$wdayPlot <- renderGirafe({
    ## Check `conversations` exists
    req(conversations())

    ## Set the x-axis labels
    x_axis_labels <- c(
      "Mon",
      "Tues",
      "Wed",
      "Thurs",
      "Fri",
      "Sat",
      "Sun"
    )

    ## Get the ranks based on the highest average at the end
    conversations_wday_plot_data <-  
      conversations() |>
      group_by(create_time_wday) |>
      reframe(n = n_distinct(chat_id)) |>
      mutate(
        create_time_wday_label = factor(x = create_time_wday, labels = x_axis_labels),
        fill = if_else(create_time_wday < 6, "#0a0a0a", "#0a0a0a"),
        colour = if_else(create_time_wday < 6, "#00ff00", "#00ff00")
      ) |>
      select(
        "create_time_wday",
        "create_time_wday_label",
        "n",
        "fill",
        "colour"
      )
    
    ## Set the plot y-axis limits
    max_y <- max(conversations_wday_plot_data$n)
    limits_y <- c(0, max_y * 1.1)

    ## Create the conversations by weekday plot
    conversations_wday_plot <-
      conversations_wday_plot_data |>
      ggplot(
        aes(
          x = create_time_wday_label,
          y = n,
          colour = colour,
          fill = fill,
          data_id = create_time_wday_label
        )
      ) +
      geom_col_interactive(
        width = 0.5,
        show.legend = FALSE
      ) +
      geom_text_interactive(
        aes(label = n),
        vjust = 1.9,
        size = 18 / .pt,
        colour = "#00ff00",
        family = "share"
      ) +
      scale_x_discrete(
        expand = c(0.1, 0, 0.1, 0)
      ) +
      scale_y_continuous(
        expand = c(0, 0),
        limits = limits_y,
        minor_breaks = NULL,
        position = "right",
        labels = label_comma()
      ) +
      scale_fill_identity() +
      scale_colour_identity() +
      labs(
        title = NULL,
        subtitle = NULL,
        caption = NULL,
        x = NULL,
        y = NULL,
        colour = NULL
      ) +
      theme(
        axis.line.x = element_line(colour = "#00ff00"),
        axis.ticks = element_blank(), 
        axis.ticks.length.x = unit(0, "in"),
        axis.text.y.right = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()
      )

    ## Ouput the girafe() plot that will be passed the render function
    girafe(
      ggobj = conversations_wday_plot,
      width_svg = 9,
      height_svg = 6,
      options = list(
        opts_toolbar(
          saveaspng = TRUE,
          pngname = "conversations_by_weekday",
          hidden = c(
            "lasso_select",
            "lasso_deselect",
            "zoom_onoff",
            "zoom_rect",
            "zoom_reset"
          )
        ),
        opts_hover(css = "stroke-width: 5pt;"),
        opts_hover_inv(css = "opacity: 0.10;"),
        opts_tooltip(opacity = 0.80, offx = -80, offy = 25, delay_mouseover = 500, delay_mouseout = 500, css = "background-color: black; color: white; font-family: sans-serif; font-size: 15pt; padding-left: 8pt; padding-right: 8pt; padding-top: 5pt; padding-bottom: 5pt")
      )
    )
  })

  # First Row Plots - Token ECDF -----------------------------------------

  output$distPlot <- renderGirafe({
    ## Check `conversations` exists
    req(conversations())

    ## Create the dataset required for making token distribution plot
    token_distribution_plot_data <-
      conversations() |>
      group_by(chat_id, author_id, author) |>
      reframe(
        avg_tokens_per_conversation_by_author = mean(tokens, na.rm = TRUE)
      ) |>
      mutate(
        author = if_else(author_id == 1L, "You", "ChatGPT"),
        author = factor(author, levels = c("You", "ChatGPT"))
      ) |>
      select(
        "chat_id",
        "author_id",
        "author",
        "avg_tokens_per_conversation_by_author"
      )

    ## Set the plot y-axis limits
    max_x <- max(token_distribution_plot_data$avg_tokens_per_conversation_by_author)
    limits_x <- c(0, max_x)

    ## Cumulative distribution comparison
    token_distribution_plot <-
      token_distribution_plot_data |>
      ggplot(
        aes(x = avg_tokens_per_conversation_by_author, colour = author, data_id = author)
      ) +
      geom_step_interactive(
        aes(y = after_stat(y), group = author),
        stat = "ecdf",
        size = 1.5
      ) +
      scale_x_continuous(
        labels = label_comma(big.mark = ",", suffix = " tokens"),
        limits = limits_x,
        expand = c(0, 0, 0, 0)
      ) +
      scale_y_continuous(
        position = "right",
        labels = label_percent(),
        breaks = seq(0.25, 1, by = 0.25),
        limits = c(0, 1),
        expand = c(0, 0)
      ) +
      scale_colour_manual(
        values = c("You" = "#ffd23f", "ChatGPT" = "#0091d4ff")
      ) +
      labs(
        title = NULL,
        subtitle = NULL,
        x = NULL,
        y = NULL,
        color = NULL
      ) +
      theme(
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.margin = unit(c(5, 35, 5, 55), "pt")
      )

    ## Generate output given to the render function
    girafe(
      ggobj = token_distribution_plot,
      width_svg = 9,
      height_svg = 6,
      options = list(
        opts_toolbar(
          saveaspng = TRUE,
          pngname = "tokens_ecdf_distribution",
          hidden = c(
            "lasso_select",
            "lasso_deselect",
            "zoom_onoff",
            "zoom_rect",
            "zoom_reset"
          )
        ),
        opts_hover(css = "stroke-width: 5pt;"),
        opts_hover_inv(css = "opacity: 0.10;"),
        opts_tooltip(opacity = 0.80, offx = -80, offy = 25, delay_mouseover = 500, delay_mouseout = 500, css = "background-color: black; color: white; font-family: sans-serif; font-size: 15pt; padding-left: 8pt; padding-right: 8pt; padding-top: 5pt; padding-bottom: 5pt")
      )
    )
  })

  # Second Row Plot - Word Bar Chart ----------------------------------

  output$wordPlot <- renderGirafe({

    ## Check `conversations` exists
    req(conversations())

    ## Get the top words
    word_bar_plot_data <-
      conversations() |>
      select("title") |>
      unnest_tokens(input = title, output = word) |>
      anti_join(get_stopwords(), by = join_by("word")) |>
      count(word) |>
      slice_max(n = 10, with_ties = FALSE, order_by = n) |>
      select("word", "n")
    
    ## Set the plot y-axis limits
    max_y <- max(word_bar_plot_data$n)
    limits_y <- c(0, max_y * 1.1)

    ## Create the conversations by weekday plot
    word_bar_plot <-
      word_bar_plot_data |>
      ggplot(
        aes(
          x = fct_inorder(word),
          y = n,
          data_id = word
        )
      ) +
      geom_col_interactive(
        width = 0.5,
        show.legend = FALSE,
        fill = "#0a0a0a",
        colour = "#00ff00"
      ) +
      geom_text_interactive(
        aes(label = n),
        vjust = 1.9,
        size = 11 / .pt, 
        colour = "#00ff00",
        family = "share"
      ) +
      scale_x_discrete(
        expand = c(0.1, 0, 0.1, 0)
      ) +
      scale_y_continuous(
        expand = c(0, 0),
        limits = limits_y,
        breaks = NULL,
        minor_breaks = NULL,
        position = "right",
        labels = label_comma()
      ) +
      labs(
        title = NULL,
        subtitle = NULL,
        caption = NULL,
        x = NULL,
        y = NULL,
        colour = NULL
      ) +
      theme(
        axis.text.x = element_markdown(size = 11, colour = "#00ff00", vjust = 1, hjust = 1, margin = margin(t = 4, r = 0, b = 8, l = 0), angle = 45),
        axis.line.x = element_line(colour = "#00ff00"),
        axis.ticks = element_blank(),
        axis.ticks.length.x = unit(0, "in"),
        axis.text.y.right = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )

    ## Ouput the girafe() plot that will be passed the render function
    girafe(
      ggobj = word_bar_plot,
      width_svg = 9,
      height_svg = 6,
      options = list(
        opts_toolbar(
          saveaspng = TRUE,
          pngname = "top_25_words", 
          hidden = c(
            "lasso_select",
            "lasso_deselect",
            "zoom_onoff",
            "zoom_rect",
            "zoom_reset"
          )
        ),
        opts_hover(css = "stroke-width: 3pt;"),
        opts_hover_inv(css = "opacity: 0.10;"),
        opts_tooltip(opacity = 0.80, offx = -80, offy = 25, delay_mouseover = 500, delay_mouseout = 500)
      )
    )
  })

  # ## Word cloud plot ---------------------------------------------------
  
  # output$wordPlot <- renderImage({

  #   ## ...
  #   req(conversations())

  #   # Get the data for the word cloud
  #   word_cloud_plot_data <-
  #     conversations() |>
  #     select("title") |>
  #     unnest_tokens(input = title, output = word) |>
  #     anti_join(get_stopwords(), by = join_by("word")) |>
  #     count(word) |>
  #     slice_max(n = 25, with_ties = FALSE, order_by = n) |>
  #     select("word", "n")
    
  #   # Create a temporary file to save the plot to
  #   outfile <- tempfile(fileext = ".svg")


  #   # Define the colours for words
  #   standout_colour <- "black"
  #   other_colours <- paste0("grey", as.integer(seq(76, 99, length.out = 24)))
  #   word_colours <- c(standout_colour, other_colours)

  #   # Create the word cloud plot
  #   word_cloud_plot <-
  #     word_cloud_plot_data |>
  #     ggplot(
  #       aes(label = word, size = n, colour = fct_inorder(word))
  #     ) +
  #     geom_text_wordcloud_area(
  #       seed = 42,
  #       rstep = 0.05,
  #       use_richtext = TRUE,
  #       rm_outside = TRUE
  #     ) +
  #     scale_size_area(max_size = 50) +
  #     scale_colour_manual(values = word_colours) +
  #     theme_void()

  #   # Save the plot to the temporary file
  #   ggsave(
  #     outfile,
  #     plot = word_cloud_plot,
  #     width = 12,
  #     height = 4, 
  #     units = "in",
  #     device = "svg"
  #   )

  #   # Return a list containing the file path and content type
  #   list(
  #     src = outfile,
  #     contentType = "image/svg+xml",
  #     alt = "Word cloud of conversation topics"
  #   )
  # }, deleteFile = TRUE)

  # Nav Panel - Raw Data -------------------------------------------------

  output$conversation_table <- renderDataTable(
    {
      ## Check `conversations` exists
      req(conversations())

      ## Original data (for export)
      display_table <-
        conversations() |>
        rename("chat_date" = "create_date") |>
        mutate(
          text = if_else(
            nchar(text) < 126,
            text,
            paste0(str_sub(string = text, start = 1, end = 125), "...")
          )
        ) |>
        select(
          "chat_id",
          "chat_date",
          "title",
          "author",
          "chat_seq",
          "text",
          "tokens"
        )

      ## Render DT
      datatable(
        display_table,
        options = list(
          pageLength = 25,
          scrollX = TRUE
        ),
        rownames = FALSE,
        extensions = 'Buttons'
      ) |>
        formatStyle(
          columns = c("chat_id", "chat_date", "author", "chat_seq", "tokens"),
          `text-align` = "center"
        ) |>
        formatStyle(
          columns = c("title", "text"),
          `text-align` = "left"
        )
    },
    server = FALSE
  )

  # Allow users to download the underlying data into CSV format ----------

  output$download_conversations <- downloadHandler(
    filename = function() {
      paste0("chat_history_", Sys.Date(), ".csv")
    },
    content = function(file) {
      conversations_table <-
        conversations() |>
        rename("chat_date" = "create_date") |>
        select(
          "chat_id",
          "chat_date",
          "title",
          "author",
          "chat_seq",
          "text",
          "tokens"
        )

      ## Write with BOM for Excel
      write_excel_csv(conversations_table, file, na = "")
    },
    contentType = "text/csv"
  )

  # Verify data is ready before showing dashboard elements ---------------

  dataReady <- reactive({
    !is.null(conversations()) && survey_status()
  })

  # Set based on the value of dataReady reactive value
  output$dataReady <- reactive({
    dataReady()
  })

  # Suspend showing the dashboard until `conversations` and `survey_status` set
  outputOptions(output, "dataReady", suspendWhenHidden = FALSE)

  # Show the the fluidRow's in order
  observeEvent(dataReady(), {

    ## Check that dataReady() is TRUE
    req(dataReady())
    
    ## Wait for 0.5s then begin showing each element in order
    delay(ms = 0,    showElement(id = "first_row_boxes_wrapper", anim = TRUE, animType = "fade", time = 1.5))
    delay(ms = 1000, showElement(id = "second_row_boxes_wrapper", anim = TRUE, animType = "fade", time = 1.5))
    delay(ms = 2000, showElement(id = "first_row_plots_wrapper", anim = TRUE, animType = "fade", time = 1.5))
    delay(ms = 3000, showElement(id = "second_row_plots_wrapper", anim = TRUE, animType = "fade", time = 1.5))
  })

}