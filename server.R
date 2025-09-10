## Allow up to 50MB chat files
options(shiny.maxRequestSize = 50 * 1024^2)

## Load packages required for server
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
      library(ggridges)
      library(showtext)

      ## Add Neucha from Google Fonts
      font_add_google("Neucha", "neucha")
      showtext_auto()

      ## Text cleanup
      library(tidytext)

      ## For the survey
      library(surveydown)

      ## Load tiktoken (GPT-3.5 and GPT-4.5 tokenizer)
      library(reticulate)

      ## Set plotting variables
      plot_width <- "100%"
      plot_height <- "420px"

      ## Load Python module
      tiktoken <- import("tiktoken")

      ## Load tokenizer
      enc <- tiktoken$get_encoding("cl100k_base")
    })
  )
)

## Custom ggplot2 theme
theme_dashboard <- function() {
  theme(
    text = element_text(family = "Neucha"),
    plot.background = element_rect(fill = "#F8F9FA", color = "#F8F9FA"),
    panel.background = element_rect(fill = "transparent", color = "transparent")
  )
}

## Generate the survey options based on data provided by the user
generate_survey_options <- function(true_val, n = 50, choose = 3) {
  offsets <- runif(n, min = -0.4, max = 0.2)
  options <- round(true_val * (1 + offsets))
  options <- options[options > 0]
  options <- options[!is.na(options)]
  options <- sample(options, 3, replace = FALSE)
  options <- unique(c(options, true_val))
  format(sort(options), big.mark = ",")
}


## Server function ----------------------------------------------------

function(input, output, session) {
  ## ...
  # hide(selector = ".nav-link")

  ## Set of reactive values that we'll update as the user interacts with the app
  conversations <- reactiveVal()
  survey_status <- reactiveVal(value = FALSE)

  ## Wait for the user to upload their conversations.json file downloaded from ChatGPT website
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
        ## Get the token counts per message sent to and from the assistant
        text = str_replace_all(
          string = text,
          pattern = "<\\|endoftext\\|>",
          replacement = "endoftext"
        ),
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
    # showNotification("Processing complete!", type = "message", duration = 3)
  })

  ## Remove the progress bar once the file is loaded
  observeEvent(input$conversation_history, {
    req(input$conversation_history)
    session$sendCustomMessage("removeProgressBar", "conversation_history")
  })

  ## Survey once the user uploads their file and it was successfully parsed
  survey_questions <- reactive({

    ## Require that `conversations` exists
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

    ## ...
    list(
      q1 = list(
      prompt = "How many conversations have you started with ChatGPT?",
      options = list(
        "opt_1" = q1_options[1],
        "opt_2" = q1_options[2],
        "opt_3" = q1_options[3],
        "opt_4" = q1_options[4]
      ),
      correct_answer = format(n_chats, big.mark = ",")
    ),

    ## ...
    q2 = list(
      prompt = "How many days have you used ChatGPT?",
      options = list(
        "opt_1" = q2_options[1],
        "opt_2" = q2_options[2],
        "opt_3" = q2_options[3],
        "opt_4" = q2_options[4]
      ),
      correct_answer = format(n_days, big.mark = ",")
    ),

    ## ...
    q3 = list(
      prompt = "How many messages have you sent to ChatGPT in 2025?",
      options = list(
        "opt_1" = q3_options[1],
        "opt_2" = q3_options[2],
        "opt_3" = q3_options[3],
        "opt_4" = q3_options[4]
      ),
      correct_answer = format(n_messages_user, big.mark = ",")
    ),

    ## ...
    q4 = list(
      prompt = "What year was ChatGPT released to the public?",
      options = list(
        "opt_1" = 2020L,
        "opt_2" = 2021L,
        "opt_3" = 2022L,
        "opt_4" = 2023L
      ),
      correct_answer = 2022L
    ),

    ## ...
    q5 = list(
      prompt = "What does GPT stand for?",
      options = list(
        "opt_1" = "general pre-trained transformer",
        "opt_2" = "generative personal talker",
        "opt_3" = "generative pre-trained transformer",
        "opt_4" = "gets people talking"
      ),
      correct_answer = 2022L
    ),

    ## ...
    q6 = list(
      prompt = "What famous 2012 paper kick started the modern deep learning revolution?",
      options = list(
        "opt_1" = "ImageNet Classification with Deep Convolutional Neural Networks",
        "opt_2" = "Attention is All You Need",
        "opt_3" = "Natural Language Processing (Almost) from Scratch",
        "opt_4" = "Playing Atari with Deep Reinforcement Learning"
      ),
      correct_answer = "ImageNet Classification with Deep Convolutional Neural Networks"
    )
  )

  })

  
  # Create the survey UI ---------------------------------------------

  output$survey_ui <- renderUI({
    
    ## Verify again that `conversations` exists
    req(conversations())

    ## Set a reactive value
    questions <- survey_questions()

    ## ...
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
              onclick = paste0(
                "Shiny.setInputValue('",
                q_id,
                "', '",
                choice,
                "', {priority: 'event'});"
              )
            )
          }),
          hr()
        )
      }),
      actionButton("submit_survey", "Submit"),
      actionButton("skip_btn", "Skip")
    )
  })

  # Handler for the survey submission ---------------------------------------------

  ## Survey Submission Logic
  observeEvent(input$submit_survey, {

    ## Verify `conversations` exists
    req(conversations())

    ## ...
    questions <- survey_questions()
    correct_answers <- 0
    total_questions <- length(questions)

    ## Create the results UI
    results_ui <- tagList(
      hr(),
      actionButton("continue_btn", "Continue")
    )

    ## Check answers and create a new UI with colors
    new_survey_ui <- tagList()
    for (q_id in names(questions)) {

      ## Save the question
      question <- questions[[q_id]]

      ## Save the user's answer
      user_answer <- input[[q_id]]

      ## Save the correct answer for the question
      correct_answer <- question$correct_answer

      ## Check if the answer is correct (handle if nothing selected)
      is_correct <- ifelse(is.na(user_answer) || is.null(user_answer), FALSE, user_answer == correct_answer)

      if (is_correct) {
        correct_answers <- correct_answers + 1
      }

      ## Add the question and colored options to the new UI
      new_survey_ui <- tagList(
        new_survey_ui,
        div(
          h3(question$prompt),
          div(
            lapply(question$options, function(option) {
              btn_class <- "disabled"

              if (option == correct_answer) {
                btn_class <- paste(btn_class, "btn-success")
              } else if (option == user_answer) {
                btn_class <- paste(btn_class, "btn-danger")
              } else {
                btn_class <- paste(btn_class, "btn-outline-warning")
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

    # Update the modal with the results and continue button
    showModal(
      modalDialog(
        title = NULL,
        footer = NULL,
        new_survey_ui,
        h3(
          paste(
            "You got",
            correct_answers,
            "out of",
            total_questions,
            "questions right"
          )
        ),
        size = "xl",
        actionButton("continue_btn", "Continue", class = "btn-primary")
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
    ## ...
    req(conversations())

    ## Set the `survey_status` to FALSE
    survey_status(FALSE)

    ## ...
    showModal(
      modalDialog(
        title = "This survey is auto-generated based on your conversation history with ChatGPT. You can choose to skip it or see how well you know your usage patterns with ChatGPT!",
        uiOutput("survey_ui"),
        footer = NULL,
        size = "xl",
        easyClose = TRUE,
        disabled = "disabled"
      )
    )
  })

  # Total messages sent by user ---------------------------------------------

  output$total_messages <- renderText({
    req(conversations())
    conversations() |>
      filter(author == "user") |>
      nrow() |>
      format(big.mark = ",")
  })

  # Total messages sent by assistant ---------------------------------------------

  output$total_tokens <- renderText({
    req(conversations())
    conversations() |>
      filter(author == "assistant") |>
      nrow() |>
      format(big.mark = ",")
  })

  # Total conversations started ---------------------------------------------

  output$total_chats <- renderText({
    req(conversations())
    format(n_distinct(conversations()$title), big.mark = ",")
  })

  # Total days talking to assistant ---------------------------------------------

  output$total_days <- renderText({
    req(conversations())
    format(n_distinct(conversations()$create_date), big.mark = ",")
  })

  # Total weekdays where at least one conversation was started with assistant (since release in November 30th, 2022) --------------

  # output$percentage_weekdays <- renderText({
  #   req(conversations())
  #
  #   ## Create a simple date table for later
  #   start_date <- ymd("2022-11-30")
  #   end_date   <- max(conversations()$create_date)
  #   all_dates  <- seq(start_date, end_date, by = "day")
  #
  #   ## Get the number of weekdays since Chat release
  #   weekdays_since_chat_release <-
  #     all_dates |>
  #     wday(week_start = 1) |>
  #     `%in%`(1:5) |>
  #     sum()
  #
  #   ## Return the formatted percentage
  #   paste0(round(n_distinct(conversations()$create_date) / weekdays_since_chat_release * 100, 1), "%")
  #
  # })

  # Top three things you ask chat ---------------------------------------------

  # output$user_favourite_word <- renderText({
  #   req(conversations())

  #   conversations() |>
  #     filter(author == "user") |>
  #     select("text") |>
  #     unnest_ngrams(input = text, output = word, n = 3) |>
  #     count(word) |>
  #     slice_max(n = 3, with_ties = FALSE, order_by = n) |>
  #     pull("word") |>
  #     paste0(collapse = ", ")
  # })

  # Favourite day of the week to talk to chat ---------------------------------------------


  # Top three things chat tells you ---------------------------------------------

  # output$assistant_favourite_word <- renderText({
  #   req(conversations())
  #
  #   conversations() |>
  #     filter(author == "assistant") |>
  #     select("text") |>
  #     unnest_ngrams(input = text, output = word, n = 3) |>
  #     count(word) |>
  #     slice_max(n = 3, with_ties = FALSE, order_by = n) |>
  #     pull("word") |>
  #     paste0(collapse = ", ")
  #
  # })

  # Token output plot --------------------------------------------------------------

  output$distPlot <- renderGirafe({
    ## ...
    req(conversations())

    ## ...
    # conversations_ <-
    #   conversations() |>
    #   group_by(chat_id, author) |>
    #   reframe(avg_tokens_per_conversation = mean(tokens, na.rm = TRUE)) |>
    #   pivot_wider(
    #     names_from = author,
    #     values_from = avg_tokens_per_conversation
    #   ) |>
    #   drop_na(user, assisstant)

    ## ...
    conversations_agg <-
      conversations() |>
      group_by(chat_id, author) |>
      reframe(avg_tokens = mean(tokens, na.rm = TRUE))
    # pivot_wider(
    #   names_from = author,
    #   values_from = avg_tokens
    # )|>
    # drop_na(user, assisstant)

    ## ...
    plot_1 <-
      conversations_agg |>
      ggplot(
        aes(
          x = author,
          y = avg_tokens,
          data_id = chat_id
        )
      ) +
      geom_point_interactive(colour = "black", size = 1.75, alpha = 0.4) +
      # geom_vline(
      #   xintercept = conversations_agg$user,
      #   linetype = 2,
      #   linewidth = 0.1
      # ) +
      # geom_hline(
      #   yintercept = conversations_agg$assistant,
      #   linetype = 2,
      #   linewidth = 0.1
      # ) +
      # geom_point(data = conversations_agg,   aes(x = user, y = assistant), size = 26 / .pt, colour = "black", inherit.aes = FALSE) +
      # geom_point(data = conversations_agg,   aes(x = user, y = assistant), size = 22 / .pt, colour = "yellow2", inherit.aes = FALSE) +
      # geom_point(data = conversations_agg,   aes(x = user, y = assistant), size = 14 / .pt, colour = "black", inherit.aes = FALSE) +
      # geom_point(data = conversations_agg,   aes(x = user, y = assistant), size = 10 / .pt, colour = "red3",    inherit.aes = FALSE) +
      # geom_segment(
      #   data = conversations_agg,
      #   aes(x = user - 15 / .pt, xend = user + 15 / .pt, y = assistant),
      #   inherit.aes = FALSE
      # ) +
      # geom_segment(
      #   data = conversations_agg,
      #   aes(x = user, y = assistant - 124 / .pt, yend = assistant + 124 / .pt),
      #   inherit.aes = FALSE
      # ) +
      # geom_segment(data = conversations_agg, aes(x = user, y = assistant - 6 / .pt, yend = assistant + 6 / .pt), inherit.aes = FALSE) +
      # scale_x_log10(
      #   labels = label_log(digits = 1),
      #   minor_breaks = NULL
      # ) +
      scale_y_continuous(
        labels = label_comma(suffix = " tokens"),
        minor_breaks = NULL,
        position = "right"
      ) +
      labs(
        # title = "Chatty, Aren't We",
        # subtitle = "Tokens sent by ChatGPT and user, all conversations*",
        # caption = "Source: OpenAI Data",
        caption = NULL,
        x = NULL,
        y = NULL,
        colour = NULL
      ) +
      theme(
        plot.title = element_text(size = 15, face = "bold"),
        plot.subtitle = element_text(size = 11),
        axis.text.x = element_text(size = 9, vjust = 0.02, margin = margin(t = 5, r = 0, b = 0, l = 0)),
        axis.ticks.length.x = unit(x = 0, units = "cm"),
        axis.minor.ticks.length.x = unit(x = 0.05, units = "cm"),
        axis.ticks.y.right = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(linetype = 1, linewidth = 0.10, colour = "grey65"),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "#EEE")
      )

    ## ...
    girafe(
      ggobj = plot_1,
      options = list(
        opts_toolbar(saveaspng = TRUE, pngname = "tokens_distribution.png", hidden = c("lasso_select", "lasso_deselect", "zoom_onoff", "zoom_rect", "zoom_reset")),
        opts_tooltip(
          opacity = 0.80,
          offx = -20,
          offy = 20,
          delay_mouseover = 500,
          delay_mouseout = 500
        )
      )
    )
  })

  ## Weekday column plot
  output$wdayPlot <- renderGirafe({
    ## ...
    req(conversations())

    ## ...
    day_names <- c(
      "Mon",
      "Tues",
      "Wed",
      "Thurs",
      "Fri",
      "Sat",
      "Sun"
    )

    ## Get the ranks based on the highest average at the end
    conversations_wday <-
      conversations() |>
      reframe(n = n_distinct(chat_id), .by = create_time_wday) |>
      mutate(
        create_time_wday_label = factor(
          x = create_time_wday,
          labels = day_names
        ),
        fill = if_else(create_time_wday < 6, "grey10", "grey80"),
        colour = if_else(create_time_wday < 6, "white", "black")
      ) |>
      select(
        "create_time_wday",
        "create_time_wday_label",
        "n",
        "fill",
        "colour"
      )

    ## Plot limits
    max_y <- max(conversations_wday$n)
    limits_y <- c(
      0,
      ifelse(max_y / 25 == max_y, max_y + 25, ceiling(max_y / 25) * 25)
    )

    ## ...
    plot_2 <-
      conversations_wday |>
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
        colour = "transparent",
        width = 0.55,
        show.legend = FALSE
      ) +
      geom_text_interactive(
        aes(label = n),
        family = "sans-serif",
        vjust = 1.75,
        size = 15 / .pt
      ) +
      scale_x_discrete(expand = expansion(0.05)) +
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
        # title = "'...You Say Goodbye, And I Say Hello'",
        # subtitle = "Conversations started with Chat-GPT, by day of the week*",
        # caption = "Source: OpenAI Data",
        caption = NULL,
        x = NULL,
        y = NULL,
        colour = NULL
      ) +
      theme(
        axis.text.x = element_text(size = 13, vjust = 0.2, margin = margin(t = 8, r = 2, b = 0, l = 2)), 
        axis.line.x = element_line(linewidth = 0.55, colour = "black"),
        axis.ticks = element_blank(),
        axis.text.y.right = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "#ffffffff")
      )

    ## ...
    girafe(
      ggobj = plot_2,
      options = list(
        opts_toolbar(saveaspng = TRUE, pngname = "conversations_by_weekday.png", hidden = c("lasso_select", "lasso_deselect", "zoom_onoff", "zoom_rect", "zoom_reset")),
        opts_zoom(max = 1),
        opts_tooltip(
          opacity = 0.80,
          offx = -20,
          offy = 20,
          delay_mouseover = 500,
          delay_mouseout = 500
        )
      )
    )
  })

  # Word cloud plot --------------------------------------------------------------

  output$wordPlot <- renderPlot({
    req(conversations())

    ## Define the colors
    standout_color <- "black"
    other_colors <- paste0("grey", as.integer(seq(76, 99, length.out = 24)))

    ## Combine the colors into a single vector
    word_colors <- c(standout_color, other_colors)

    ## Create the word cloud
    set.seed(42)
    wday_plot <-
      conversations() |>
      select("title") |>
      unnest_tokens(input = title, output = word) |>
      anti_join(get_stopwords(), by = join_by("word")) |>
      count(word) |>
      slice_max(n = 25, with_ties = FALSE, order_by = n) |>
      ggplot(
        aes(label = word, size = n, colour = fct_inorder(word))
      ) +
      geom_text_wordcloud_area(
        rstep = 0.05,
        use_richtext = TRUE,
        rm_outside = TRUE
      ) +
      scale_size_area(max_size = 80) +
      scale_colour_manual(values = word_colors) +
      theme_void() +
      theme(
        plot.background = element_blank(),
        panel.background = element_blank()
      )

    ## ...
    wday_plot
  })

  ## Raw Data table
  output$conversation_table <- renderDataTable(
    {
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

  # --------------------------------------------------------------------

  output$download_conversations <- downloadHandler(
    filename = function() {
      paste0("chat_history_", Sys.Date(), ".csv")
    },
    content = function(file) {
      conversations_table <- conversations() |>
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

  # Survey -------------------------------------------------------------

  output$survey <- renderUI({
    req(conversations())

    ## Remove text column
    conversations_table <-
      conversations() |>
      mutate(
        text = paste0(str_sub(string = text, start = 1, end = 125), "...")
      ) |>
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
  })

  # Verify data is ready before showing dashboard elements -------------------------------------------------------------

  dataReady <- reactive({
    !is.null(conversations()) && survey_status()
  })

  output$dataReady <- reactive({
    dataReady()
  })

  outputOptions(output, "dataReady", suspendWhenHidden = FALSE)

  # See which set of elements to render -----------------------------------------------------

  # output$charts_ui <- renderUI({
  #   if (input$selected_tab == "Charts" && isTRUE(dataReady())) {
  #     uiOutput("plot_output")
  #   } else {
  #     NULL
  #   }
  # })
}
