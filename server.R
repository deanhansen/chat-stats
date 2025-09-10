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
      library(ggtext)
      library(ggimage)

      ## Text cleanup
      library(tidytext)

      ## For the survey
      library(surveydown)

      ## Load tiktoken (GPT-3.5 and GPT-4.5 tokenizer)
      library(reticulate)

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
  ## Set of reactive values that we'll update as the user interacts with the app
  conversations <- reactiveVal()
  survey_status <- reactiveVal(value = FALSE)

  # A reactive value to track which questions have been answered
  answered_questions <- reactiveVal(character(0))

  # Observe all survey inputs to see which questions have been answered
  observe({
    questions <- survey_questions()
    
    # Check if questions() is ready before setting up observers
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


  ## Wait for the user to upload their conversations.json file downloaded from ChatGPT website --------
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
    # showNotification("Processing complete!", type = "message", duration = 3)
  })

  ## Remove the progress bar once the file is loaded
  observeEvent(input$conversation_history, {
    ## Check `conversation_history` exists
    req(input$conversation_history)

    ## This tells the UI to remove the file upload element once we've loaded the data
    session$sendCustomMessage("removeProgressBar", "conversation_history")
  })

  ## Create the survey questions once the data has been successfully parsed ----------------------------
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

  ## Create the survey UI ---------------------------------------------

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

  ## Handler for the survey submission ---------------------------------------------

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

    ## Update the modal with the results and continue button
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
        easyClose = FALSE,
        disabled = "disabled"
      )
    )
  })

  ## Total messages sent by user ---------------------------------------------

  output$total_messages <- renderText({
    req(conversations())
    conversations() |>
      filter(author == "user") |>
      nrow() |>
      format(big.mark = ",")
  })

  ## Total messages sent by assistant ---------------------------------------------

  output$total_tokens <- renderText({
    req(conversations())
    conversations() |>
      filter(author == "assistant") |>
      nrow() |>
      format(big.mark = ",")
  })

  ## Total conversations started ---------------------------------------------

  output$total_chats <- renderText({
    req(conversations())
    format(n_distinct(conversations()$title), big.mark = ",")
  })

  ## Total days talking to assistant ---------------------------------------------

  output$total_days <- renderText({
    req(conversations())
    format(n_distinct(conversations()$create_date), big.mark = ",")
  })

  ## Token distribution plot --------------------------------------------------------------

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
      mutate(author = if_else(author_id == 1L, "You", "ChatGPT")) |>
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
        aes(x = avg_tokens_per_conversation_by_author, colour = author)
      ) +
      geom_step_interactive(
        aes(y = after_stat(y), group = author),
        stat = "ecdf",
        size = 1.5
      ) +
      scale_x_continuous(
        labels = label_comma(big.mark = ","),
        limits = limits_x,
        expand = c(0.05, 0)
      ) +
      scale_y_continuous(
        position = "right",
        labels = label_percent(),
        breaks = seq(0, 1, by = 0.25),
        limits = c(0, 1),
        expand = c(0, 0)
      ) +
      scale_colour_manual(
        values = c("ChatGPT" = "#10A37F", "You" = "#000000ff")
      ) +
      labs(
        title = NULL,
        subtitle = NULL,
        x = NULL,
        y = NULL,
        color = NULL
      ) +
      theme(
        text = element_text(family = "sans"),
        axis.text.x = element_text(size = 13, vjust = 0, margin = margin(t = 4, r = 0, b = 0, l = 0)),
        axis.text.y = element_text(size = 13, hjust = 0.4, vjust = -0.4, margin = margin(t = 0, r = -60, b = 0, l = -60)),
        axis.line.x = element_line(linewidth = 0.5, colour = "black"),
        panel.grid.major.x = element_line(linewidth = 0.5, colour = "black"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.5, colour = "black"),
        panel.grid.minor.y = element_blank(),
        plot.background = element_blank(),
        panel.background = element_rect(fill = "#ffffffff"),
        legend.position = "top",
        legend.key.size = unit(x = 6, units = "pt"),
        legend.text = element_text(size = 13, face = "bold", margin = margin(t = 0, r = 5, b = 0, l = 5))
      )

    ## Generate output given to the render function
    girafe(
      ggobj = token_distribution_plot,
      width_svg = 9,
      height_svg = 6,
      options = list(
        opts_toolbar(
          saveaspng = TRUE,
          pngname = "tokens_ecdf_distribution.png",
          hidden = c(
            "lasso_select",
            "lasso_deselect",
            "zoom_onoff",
            "zoom_rect",
            "zoom_reset"
          )
        ),
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

  ## Weekday column plot ----------------------------------------------------------------------------------------

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
        fill = if_else(create_time_wday < 6, "grey10", "grey80"),
        colour = if_else(create_time_wday < 6, "white", "black"),
        rank = rank(-n)
      ) |>
      group_by(rank) |> 
      mutate(
        rank = cur_group_id(),
        emoji_url = case_when(
          rank == 1L ~ "https://cdn.jsdelivr.net/gh/twitter/twemoji@14.0.2/assets/svg/1f947.svg",
          rank == 2L ~ "https://cdn.jsdelivr.net/gh/twitter/twemoji@14.0.2/assets/svg/1f948.svg",
          rank == 3L ~ "https://cdn.jsdelivr.net/gh/twitter/twemoji@14.0.2/assets/svg/1f949.svg",
          .default = NA
        )
      ) |> 
      ungroup() |> 
      select(
        "create_time_wday",
        "create_time_wday_label",
        "n",
        "fill",
        "colour",
        "emoji_url"
      )
    
    ## Set the plot y-axis limits
    max_y <- max(conversations_wday_plot_data$n)
    limits_y <- c(0, ifelse(max_y / 50 == max_y, max_y + 50, ceiling(max_y / 50) * 50))

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
        colour = "transparent",
        width = 0.45,
        show.legend = FALSE
      ) +
      geom_text_interactive(
        aes(label = n),
        vjust = 1.75,
        size = 13 / .pt
      ) +
      geom_image(
        aes(x = create_time_wday_label, image = emoji_url, y = n + 5),
        size = 0.07,
        inherit.aes = FALSE
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
        text = element_text(family = "sans"),
        axis.text.x = element_text(size = 13, vjust = 0, margin = margin(t = 4, r = 0, b = 0, l = 0)),
        axis.line.x = element_line(linewidth = 0.5, colour = "black"),
        axis.ticks = element_blank(),
        axis.text.y.right = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "#ffffffff")
      )

    ## Ouput the girafe() plot that will be passed the render function
    girafe(
      ggobj = conversations_wday_plot,
      width_svg = 9,
      height_svg = 6,
      options = list(
        opts_toolbar(
          saveaspng = TRUE,
          pngname = "conversations_by_weekday.png",
          hidden = c(
            "lasso_select",
            "lasso_deselect",
            "zoom_onoff",
            "zoom_rect",
            "zoom_reset"
          )
        ),
        opts_zoom(max = 1),
        opts_hover(reactive = FALSE),
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

  ## Word cloud plot --------------------------------------------------------------
  
  output$wordPlot <- renderImage({

    ## ...
    req(conversations())

    # Get the data for the word cloud
    word_cloud_plot_data <-
      conversations() |>
      select("title") |>
      unnest_tokens(input = title, output = word) |>
      anti_join(get_stopwords(), by = join_by("word")) |>
      count(word) |>
      slice_max(n = 25, with_ties = FALSE, order_by = n) |>
      select("word", "n")
    
    # Create a temporary file to save the plot to
    outfile <- tempfile(fileext = ".svg")


    # Define the colours for words
    standout_colour <- "black"
    other_colours <- paste0("grey", as.integer(seq(76, 99, length.out = 24)))
    word_colours <- c(standout_colour, other_colours)

    # Create the word cloud plot
    word_cloud_plot <-
      word_cloud_plot_data |>
      ggplot(
        aes(label = word, size = n, colour = fct_inorder(word))
      ) +
      geom_text_wordcloud_area(
        seed = 42,
        rstep = 0.05,
        use_richtext = TRUE,
        rm_outside = TRUE,
        family = "sans-serif"
      ) +
      scale_size_area(max_size = 50) +
      scale_colour_manual(values = word_colours) +
      theme_void() +
      theme(
        plot.background = element_blank(),
        panel.background = element_blank()
      )

    # Save the plot to the temporary file
    ggsave(
      outfile,
      plot = word_cloud_plot,
      width = 12,
      height = 4, 
      units = "in",
      device = "svg"
    )

    # Return a list containing the file path and content type
    list(
      src = outfile,
      contentType = "image/svg+xml",
      alt = "Word cloud of conversation topics"
    )
  }, deleteFile = TRUE)

  ## Table shown on the `Raw Data` tab -------------------------------------------

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

  ## Allow users to download the underlying data into CSV format -------------------

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

  ## Verify data is ready before showing dashboard elements --------------

  dataReady <- reactive({
    !is.null(conversations()) && survey_status()
  })

  ## Set based on the value of dataReady reactive value
  output$dataReady <- reactive({
    dataReady()
  })

  ## Suspend showing the dashboard until `conversations` and `survey_status` set
  outputOptions(output, "dataReady", suspendWhenHidden = FALSE)
}