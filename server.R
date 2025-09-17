source("global.R")

# Server function --------------------------------------------------------

function(input, output, session) {

  ## Set of reactive values that we'll update as the user interacts with the app
  conversations <- reactiveVal()
  survey_status <- reactiveVal(value = FALSE)
  answered_questions <- reactiveVal(value = character(0))

  ## Observe all survey inputs to see which questions have been answered
  observe({

    ## Set a reactive value with survey questions
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
    result <- tryCatch({
      exit_code <- system2(
        command = "python3",
        args = c("conversations.py", uploaded_file, output_file),
        stdout = TRUE,
        stderr = TRUE,
        wait = TRUE
      )

      attr(exit_code, "status") %||% 0  # return 0 if no error
    }, error = function(e) {
      showNotification(
        paste("Error running Python script:", e$message),
        type = "error",
        duration = 10
      )
      return(1) # non-zero means error
    })

    if (result != 0) {
      showNotification(
        "There was a problem processing your file. Please check the format and try again.",
        type = "error",
        duration = 10
      )
      return(NULL)
    }

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
        prompt = "On how many different days have you used ChatGPT since it's release?",
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
        actionButton(inputId = "continue_btn", class = "btn btn-default", label = "Close"),
        footer = NULL,
        size = "xl",
        easyClose = FALSE
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

  # Date of first conversation -------------------------------------------

  output$first_conversation_date <- renderText({
    req(conversations())
    format(ymd(min(conversations()$create_date, na.rm = TRUE)), "%b %d, %Y")
  })

  # Second Row Plots - Hour Bar Chart ------------------------------------

  output$hourPlot <- renderGirafe({
    
    ## Check `conversations` exists
    req(conversations())

    ## How many messages the user is sending by hour of the day
    conversations_hour_plot_data <-  
      conversations() |>
      filter(author == "user") |> 
      mutate(create_time_hour = hour(create_time)) |> 
      group_by(create_time_hour) |>
      # reframe(n = n_distinct(chat_id)) |>
      reframe(n = n()) |>
      complete(create_time_hour = 0L:23L, fill = list(n = 0)) |> 
      select(
        "create_time_hour",
        "n"
      )
    
    ## Set the plot y-axis limits
    max_y <- max(conversations_hour_plot_data$n, na.rm = TRUE)
    limits_y <- c(0, max_y * 1.1)

    ## Create the conversations by weekday plot
    conversations_hour_plot <-
      conversations_hour_plot_data |>
      ggplot(
        aes(
          x = create_time_hour,
          y = n,
          group = 1,
          data_id = create_time_hour,
          tooltip = glue("{create_time_hour}:00 ({n})")
        )
      ) +
      geom_line(
        linewidth = 0.75, 
        show.legend = FALSE,
        colour = "#00ff00"
      ) +
      geom_point(
        size = 2.75,
        show.legend = FALSE,
        colour = "#ffffffff"
      ) +
      geom_point_interactive(
        size = 2.25,
        show.legend = FALSE,
        colour = "#00ff00"
      ) +
      geom_area(
        fill = "#00ff00",
        alpha = 0.04
      ) +
      scale_x_continuous(
        breaks = seq(0, 23, by = 4),
        minor_breaks = seq(0, 23, by = 1),
        expand = c(0.1, 0, 0.1, 0),
        labels = label_number(suffix = ":00")
      ) +
      scale_y_continuous(
        expand = c(0, 0),
        limits = limits_y,
        minor_breaks = NULL,
        position = "right",
        labels = label_comma()
      ) + 
      guides(
        x = guide_axis(minor.ticks = TRUE),
        y = guide_axis(minor.ticks = FALSE)
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
        panel.grid = element_blank(),
        axis.minor.ticks.length.x.bottom = unit(0.08, "in"),
        axis.line.y = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.text.y.right = element_blank(),
        axis.minor.ticks.y.right = element_blank()
      )

    ## Ouput the girafe() plot that will be passed the render function
    girafe(
      ggobj = conversations_hour_plot,
      width_svg = 9,
      height_svg = 6,
      options = list(
        opts_toolbar(
          saveaspng = TRUE,
          pngname = "conversations_by_hour",
          hidden = c(
            "lasso_select",
            "lasso_deselect",
            "zoom_onoff",
            "zoom_rect",
            "zoom_reset"
          )
        ),
        opts_hover(css = "stroke-width: 4pt;"),
        opts_hover_inv(css = NULL),
        opts_tooltip(opacity = 0.80, delay_mouseover = 500, delay_mouseout = 500, css = NULL)
      )
    )
  })

  # Second Row Plots - Minute Bar Chart ----------------------------------

  # output$minutePlot <- renderGirafe({
    
  #   ## Check `conversations` exists
  #   req(conversations())

  #   ## How many messages the user is sending by hour of the day
  #   conversations_minute_plot_data <-  
  #     conversations() |>
  #     filter(author == "user") |> 
  #     mutate(create_time_minute = minute(create_time)) |> 
  #     group_by(create_time_minute) |>
  #     # reframe(n = n_distinct(chat_id)) |>
  #     reframe(n = n()) |>
  #     complete(create_time_minute = 0L:59L, fill = list(n = 0)) |> 
  #     select(
  #       "create_time_minute",
  #       "n"
  #     )
    
  #   ## Set the plot y-axis limits
  #   max_y <- max(conversations_minute_plot_data$n, na.rm = TRUE)
  #   limits_y <- c(0, max_y * 1.1)

  #   ## Create the conversations by minute plot
  #   conversations_minute_plot <-
  #     conversations_minute_plot_data |>
  #     ggplot(
  #       aes(
  #         x = create_time_minute,
  #         y = n,
  #         group = 1,
  #         data_id = create_time_minute,
  #         tooltip = glue("{create_time_minute} ({n})")
  #       )
  #     ) +
  #     geom_line(
  #       linewidth = 0.5,
  #       show.legend = FALSE,
  #       colour = "#00ff00"
  #     ) +
  #     geom_point(
  #       size = 2,
  #       show.legend = FALSE,
  #       colour = "#fff"
  #     ) +
  #     geom_point_interactive(
  #       size = 1.6,
  #       show.legend = FALSE,
  #       colour = "#00ff00"
  #     ) +
  #     geom_area(
  #       fill = "#00ff00",
  #       alpha = 0.04
  #     ) +
  #     scale_x_continuous(
  #       breaks = seq(0, 59, by = 10),
  #       minor_breaks = seq(1, 59, by = 1),
  #       expand = c(0.1, 0, 0.1, 0)
  #     ) +
  #     scale_y_continuous(
  #       expand = c(0, 0),
  #       limits = limits_y,
  #       minor_breaks = NULL,
  #       position = "right",
  #       labels = label_comma()
  #     ) +
  #     guides(
  #       x = guide_axis(minor.ticks = TRUE),
  #       y = guide_axis(minor.ticks = FALSE)
  #     ) +
  #     labs(
  #       title = NULL,
  #       subtitle = NULL,
  #       caption = NULL,
  #       x = NULL,
  #       y = NULL,
  #       colour = NULL
  #     ) +
  #     theme(
  #       panel.grid = element_blank(),
  #       axis.text.y.right = element_blank(),
  #       axis.minor.ticks.length.x.bottom = unit(0.08, "in"),
  #       axis.line.y = element_blank(),
  #       axis.text.y.right = element_blank()
  #     )

  #   ## Ouput the girafe() plot that will be passed the render function
  #   girafe(
  #     ggobj = conversations_minute_plot,
  #     width_svg = 9,
  #     height_svg = 6,
  #     options = list(
  #       opts_toolbar(
  #         saveaspng = TRUE,
  #         pngname = "conversations_by_minute",
  #         hidden = c(
  #           "lasso_select",
  #           "lasso_deselect",
  #           "zoom_onoff",
  #           "zoom_rect",
  #           "zoom_reset"
  #         )
  #       ),
  #       opts_hover(css = "stroke-width: 2pt;"),
  #       opts_hover_inv(css = NULL),
  #       opts_tooltip(opacity = 0.80, offx = -80, offy = 25, delay_mouseover = 500, delay_mouseout = 500, css = NULL)
  #     )
  #   )
  # })

  # Second Row Plots - Weekday Bar Chart ---------------------------------

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
      filter(author == "user") |> 
      group_by(create_time_wday) |>
      reframe(n = n()) |>
      mutate(
        create_time_wday_label = factor(x = create_time_wday, labels = x_axis_labels)
      ) |>
      select(
        "create_time_wday",
        "create_time_wday_label",
        "n"
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
          group = 1,
          data_id = create_time_wday_label,
          tooltip = glue("{create_time_wday_label} ({label_comma()(n)})")
        )
      ) +
      geom_line(
        linewidth = 1,
        show.legend = FALSE,
        colour = "#00ff00"
      ) +
      geom_point(
        size = 5,
        show.legend = FALSE,
        colour = "#fff"
      ) +
      geom_point_interactive(
        size = 4,
        show.legend = FALSE,
        colour = "#00ff00"
      ) +
      geom_area(
        fill = "#00ff00",
        alpha = 0.04
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
      labs(
        title = NULL,
        subtitle = NULL,
        caption = NULL,
        x = NULL,
        y = NULL,
        colour = NULL
      ) +
      theme(
        panel.grid = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.text.y.right = element_blank(),
        axis.minor.ticks.y.right = element_blank()
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
        opts_hover_inv(css = NULL),
        opts_tooltip(opacity = 0.80, offx = -80, offy = 25, delay_mouseover = 500, delay_mouseout = 500, css = NULL)
      )
    )
  })

  # First Row Plots - Token ECDF -----------------------------------------

  output$distPlot <- renderGirafe({
    
    ## Check `conversations` exists
    req(conversations())
    
    ## Compute the token ECDF per author
    token_distribution_plot_data <- 
      conversations() |> 
      group_by(author) |> 
      arrange(tokens) |> 
      mutate(ecdf = ecdf(tokens)(tokens)) |> 
      ungroup() |> 
      mutate(author = if_else(str_to_lower(author) == "user", "You", "ChatGPT")) |> 
      select("author", "tokens", "ecdf")

    ## Set the plot y-axis limits
    max_x <- max(token_distribution_plot_data$tokens, na.rm = TRUE)
    limits_x <- c(0, max_x * 1.1)

    ## Cumulative distribution comparison
    token_distribution_plot <-
      token_distribution_plot_data |>
      ggplot(
        aes(x = tokens, y = ecdf, colour = author, group = author, data_id = author, tooltip = author)
      ) +
      geom_step_interactive(
        linewidth = 1.5
      ) +
      scale_x_continuous(
        labels = label_number(big.mark = ",", suffix = " tkns"),
        limits = limits_x,
        expand = c(0, 0)
      ) +
      scale_y_continuous(
        position = "right",
        breaks = seq(0.25, 1, by = 0.25),
        minor_breaks = seq(0.125, 0.875, by = 0.125),
        labels = label_percent(),
        limits = c(0, 1),
        expand = c(0, 0)
      ) +
      scale_colour_manual(
        values = c("You" = "#BB86FC", "ChatGPT" = "#FFEB3B")
      ) +
      guides(
        x = guide_axis(minor.ticks = TRUE),
        y = guide_axis(minor.ticks = TRUE)
      ) +
      labs(
        title = NULL,
        subtitle = NULL,
        x = NULL,
        y = NULL,
        color = NULL
      ) +
      theme(
        axis.ticks.y = element_blank(),
        panel.grid = element_line(colour = "#999", linewidth = 0.5),
        panel.grid.minor = element_blank(),
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
        opts_hover(css = "stroke-width: 1pt;"),
        opts_hover_inv(css = NULL),
        opts_tooltip(opacity = 0.80, delay_mouseover = 500, delay_mouseout = 500, css = NULL)
      )
    )
  })

  # First Row Plot - Word Bar Chart --------------------------------------

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
      mutate(word = str_trunc(word, width = 16)) |> 
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
          data_id = word,
          tooltip = glue("{word} ({label_comma()(n)})")
        )
      ) +
      geom_segment(
        aes(y = 0, yend = n),
        linewidth = 0.75,
        colour = "#00ff00"
      ) +
      geom_point(
        size = 5,
        show.legend = FALSE,
        colour = "#00ff00"
      ) +
      geom_point_interactive(
        size = 4,
        show.legend = FALSE,
        colour = "#0a0a0a"
      ) +
      geom_text(
        aes(label = label_comma()(n)),
        vjust = -1.2,
        size = 13 / .pt, 
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
        axis.text.x = element_markdown(vjust = 1, hjust = 1, angle = 45, margin = margin(t =  8, r = 0, b = 0, l = 0)),
        axis.ticks.y = element_blank(),
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
          pngname = "top_ten_topics", 
          hidden = c(
            "lasso_select",
            "lasso_deselect",
            "zoom_onoff",
            "zoom_rect",
            "zoom_reset"
          )
        ),
        opts_hover(css = "stroke: #00ff00; stroke-width: 1pt;"),
        opts_hover_inv(css = NULL),
        opts_tooltip(opacity = 0.80, offx = -80, offy = 25, delay_mouseover = 500, delay_mouseout = 500)
      )
    )
  })

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