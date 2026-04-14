# =============================================================================
# Project 3 — Learning style mini-quiz (Shiny A/B) + Google Analytics 4 (gtag)
# =============================================================================
# Research-design alignment
#   H0: No difference in time to successful completion
#   H1a: Wizard-style interface leads to faster time to successful completion
#   H1b: Wizard-style interface increases probability of full completion
#   H1c: Wizard-style interface reduces interaction friction, measured through
#        fewer revisions and fewer friction events per answered item
#
# Backward-compatibility note:
#   Existing GA4 event names / CSV fields used by the team are largely preserved.
#   New hypothesis-aligned fields/events are ADDED rather than replacing old ones.
# =============================================================================

library(shiny)
library(shinyjs)

GA_MEASUREMENT_ID <- Sys.getenv("GA_MEASUREMENT_ID", unset = "G-LFFGXVY0PY")

ga_enabled <- function() {
  id <- GA_MEASUREMENT_ID
  nzchar(id) && grepl("^G-[A-Z0-9]+$", id)
}

ga_head_tags <- function(measurement_id) {
  tags$head(
    tags$script(async = NA, src = paste0("https://www.googletagmanager.com/gtag/js?id=", measurement_id)),
    tags$script(HTML(sprintf(
      "window.dataLayer = window.dataLayer || [];
function gtag(){dataLayer.push(arguments);}
gtag('js', new Date());
gtag('config', '%s');",
      measurement_id
    )))
  )
}

esc_js_str <- function(x) gsub("'", "\\'", as.character(x), fixed = TRUE)


ga_params_object_js <- function(params) {
  if (!length(params)) return("{}")
  kv <- mapply(names(params), params, FUN = function(nm, val) {
    if (is.numeric(val) && length(val) == 1L && !is.na(val)) {
      sprintf("%s:%s", nm, format(val, scientific = FALSE, trim = TRUE))
    } else if (is.logical(val) && length(val) == 1L) {
      sprintf("%s:%s", nm, if (isTRUE(val)) "true" else "false")
    } else {
      sprintf("%s:'%s'", nm, esc_js_str(paste0(val, collapse = "")))
    }
  }, SIMPLIFY = TRUE, USE.NAMES = FALSE)
  paste0("{", paste(kv, collapse = ","), "}")
}

ga_page_view <- function(page_path, page_title = "") {
  if (!ga_enabled()) return(invisible(NULL))
  params <- list(
    page_path = page_path,
    page_title = page_title,
    page_location = "__LOC__"
  )
  obj <- ga_params_object_js(params)
  obj <- sub("'__LOC__'", "window.location.href", obj, fixed = TRUE)
  js <- sprintf(
    "if(typeof gtag!=='undefined'){gtag('event','page_view',%s);}",
    obj
  )
  shinyjs::runjs(js)
}

ga_event <- function(event_name, params = list()) {
  if (!ga_enabled()) return(invisible(NULL))
  obj <- ga_params_object_js(params)
  js <- sprintf(
    "if(typeof gtag!=='undefined'){gtag('event','%s',%s);}",
    esc_js_str(event_name),
    obj
  )
  shinyjs::runjs(js)
}

ITEMS <- data.frame(
  id = 1:10,
  dim = c("V", "V", "V", "A", "A", "R", "R", "R", "K", "K"),
  text = c(
    "When learning new material, diagrams, charts, or videos help me understand faster than text alone.",
    "I remember information better when I can visualize it (e.g., maps, timelines, mental pictures).",
    "I prefer instructors who use slides with clear visuals rather than only talking.",
    "I learn well from lectures, podcasts, or group discussions where I can listen and ask questions.",
    "Repeating information out loud or explaining it to someone else helps me remember it.",
    "I like written instructions, outlines, and taking structured notes when I study.",
    "Reading articles, textbooks, or written summaries is one of my most effective study methods.",
    "I prefer to review by re-reading my notes rather than only listening to a recording again.",
    "I learn best by doing practice problems, labs, or hands-on activities.",
    "I understand concepts better after I try them myself (e.g., coding, experiments, simulations)."
  ),
  stringsAsFactors = FALSE
)

MAX_BY_DIM <- c(V = 15L, A = 10L, R = 15L, K = 10L)

likert_choices <- setNames(as.character(1:5), c(
  "1 — Strongly disagree",
  "2 — Disagree",
  "3 — Neutral",
  "4 — Agree",
  "5 — Strongly agree"
))

score_answers <- function(ans_named) {
  raw <- c(V = 0L, A = 0L, R = 0L, K = 0L)
  for (i in seq_len(nrow(ITEMS))) {
    key <- paste0("q", i)
    v <- suppressWarnings(as.integer(ans_named[[key]]))
    if (is.na(v)) next
    d <- ITEMS$dim[i]
    raw[d] <- raw[d] + v
  }
  raw
}

primary_label <- function(raw) {
  o <- order(-raw, names(raw))
  nm <- names(raw)[o]
  top <- nm[1]
  second <- nm[2]
  if (raw[top] - raw[second] <= 2L) {
    paste0("Mixed (", top, " + ", second, ")")
  } else {
    switch(
      top,
      V = "Visual",
      A = "Auditory",
      R = "Read/Write",
      K = "Kinesthetic",
      "Unknown"
    )
  }
}

ensure_log_dir <- function() {
  d <- file.path(getwd(), "logs")
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
  d
}

append_log <- function(row_df, fname = "sessions.csv") {
  f <- file.path(ensure_log_dir(), fname)
  exists <- file.exists(f)
  write.table(
    row_df,
    file = f,
    append = exists,
    sep = ",",
    row.names = FALSE,
    col.names = !exists,
    qmethod = "double"
  )
}

append_event_log <- function(session_id, group, event, extra = list()) {
  row <- data.frame(
    session_id = session_id,
    group = group,
    event = event,
    ts = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    stringsAsFactors = FALSE
  )
  if (length(extra)) {
    for (nm in names(extra)) row[[nm]] <- extra[[nm]]
  }
  append_log(row, fname = "session_events.csv")
}

new_session_id <- function() {
  paste0(
    format(Sys.time(), "%Y%m%d_%H%M%S_"),
    paste(sample(c(LETTERS, 0:9), 8, replace = TRUE), collapse = "")
  )
}

device_type_from_ua <- function(ua) {
  ua <- tolower(paste0(ua %||% ""))
  if (!nzchar(ua)) return("unknown")
  if (grepl("ipad|tablet", ua)) return("tablet")
  if (grepl("mobi|iphone|android", ua)) return("mobile")
  "desktop"
}

`%||%` <- function(x, y) if (is.null(x)) y else x

ui <- fluidPage(
  useShinyjs(),
  if (ga_enabled()) ga_head_tags(GA_MEASUREMENT_ID),
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$style(HTML("
      .well { padding: 14px; }
      .radio { margin-top: 8px; margin-bottom: 8px; }
      .btn { min-height: 44px; }
      @media (max-width: 576px) {
        .container-fluid { padding-left: 12px; padding-right: 12px; }
        h2 { font-size: 20px; }
        .progress { margin-bottom: 16px; }
        #next_b, #finish_b, #submit_a { width: 100%; }
        #back_b { width: 100%; margin-bottom: 10px; }
      }
    "))
  ),
  titlePanel("Learning preference mini-quiz (A/B demo)"),
  tags$p(
    style = "color:#555;",
    "Random assignment: open with ",
    tags$code("?group=A"), " or ", tags$code("?group=B"), " to fix variant; otherwise random per session."
  ),
  if (!ga_enabled()) {
    tags$p(style = "color:#a60;", "Google Analytics: set ", tags$code("GA_MEASUREMENT_ID"), " (see file header).")
  },
  uiOutput("body")
)

server <- function(input, output, session) {
  rv <- reactiveValues(
    group = NULL,
    session_id = new_session_id(),
    start_ts = NULL,
    completion_ts = NULL,
    step = 1L,
    b_answers = rep(NA_integer_, 10L),
    view = "quiz",
    ease_of_use = NA_integer_,
    ease_saved = FALSE,
    logged_results = FALSE,
    logged_start = FALSE,
    logged_summary = FALSE,
    n_submit_fail_a = 0L,
    n_next_fail_b = 0L,
    n_step_next_b = 0L,
    n_step_back_b = 0L,
    revision_count = 0L,
    friction_events = 0L,
    a_prev_answers = setNames(rep(NA_integer_, 10L), paste0("q", 1:10)),
    ga_assign_sent = FALSE,
    ga_pv_quiz = FALSE,
    ga_pv_results = FALSE,
    device_type = "unknown",
    core_completed = FALSE,
    result_raw = NULL,
    result_label = NULL
  )

  observe({
    ua <- session$clientData$userAgent
    rv$device_type <- device_type_from_ua(ua)
  })

  observe({
    if (!is.null(rv$group)) return(invisible(NULL))
    urlq <- session$clientData$url_search
    qs <- parseQueryString(if (is.null(urlq)) "" else urlq)
    g <- qs$group
    if (!is.null(g)) {
      gu <- toupper(as.character(g))
      if (gu %in% c("A", "B")) rv$group <- gu
    }
    if (is.null(rv$group)) {
      rv$group <- sample(c("A", "B"), 1L)
    }
    if (is.null(rv$start_ts)) {
      rv$start_ts <- Sys.time()
    }
  })

  observe({
    req(rv$group)
    if (isTRUE(rv$ga_assign_sent)) return(invisible(NULL))
    ga_event("ab_assign", list(variant = rv$group, condition = rv$group, app_session_id = rv$session_id))
    rv$ga_assign_sent <- TRUE
  })

  observe({
    req(rv$group)
    req(identical(rv$view, "quiz"))
    if (isTRUE(rv$ga_pv_quiz)) return(invisible(NULL))
    ga_page_view(
      paste0("/quiz-", rv$group),
      paste0("Learning quiz — variant ", rv$group)
    )
    rv$ga_pv_quiz <- TRUE
  })

  observe({
    req(identical(rv$view, "results"))
    if (isTRUE(rv$ga_pv_results)) return(invisible(NULL))
    ga_page_view("/results", "Learning style — results")
    rv$ga_pv_results <- TRUE
  })

  observe({
    req(rv$group)
    if (isTRUE(rv$logged_start)) return(invisible(NULL))
    append_event_log(rv$session_id, rv$group, "start")
    append_event_log(rv$session_id, rv$group, "quiz_start")
    ga_event("quiz_session_start", list(variant = rv$group, condition = rv$group))
    rv$logged_start <- TRUE
  })

  collect_inputs_a <- reactive({
    ans <- setNames(rep(NA_character_, 10L), paste0("q", 1:10))
    for (i in 1:10) {
      nm <- paste0("q", i)
      if (!is.null(input[[nm]])) ans[[nm]] <- input[[nm]]
    }
    ans
  })

  answered_n_a <- function() {
    ans <- collect_inputs_a()
    sum(!is.na(suppressWarnings(as.integer(unlist(ans, use.names = FALSE)))))
  }

  answered_n_b <- function() {
    tmp <- rv$b_answers
    i <- as.integer(rv$step)
    if (isTRUE(identical(rv$group, "B")) && i >= 1L && i <= 10L) {
      vcur <- suppressWarnings(as.integer(input$b_current))
      if (!is.na(vcur) && is.na(tmp[i])) tmp[i] <- vcur
    }
    sum(!is.na(tmp))
  }

  progress_depth <- function() {
    if (isTRUE(identical(rv$group, "A"))) {
      answered_n_a() / 10
    } else if (isTRUE(identical(rv$group, "B"))) {
      answered_n_b() / 10
    } else {
      NA_real_
    }
  }

  current_answer_vector <- function() {
    if (isTRUE(identical(rv$group, "A"))) {
      unlist(collect_inputs_a(), use.names = TRUE)
    } else if (isTRUE(identical(rv$group, "B"))) {
      tmp <- rv$b_answers
      i <- as.integer(rv$step)
      vcur <- suppressWarnings(as.integer(input$b_current))
      if (!is.na(vcur) && i >= 1L && i <= 10L && is.na(tmp[i])) tmp[i] <- vcur
      setNames(as.character(tmp), paste0("q", 1:10))
    } else {
      setNames(rep(NA_character_, 10L), paste0("q", 1:10))
    }
  }

  log_session_summary <- function(end_ts = Sys.time()) {
    if (isTRUE(rv$logged_summary)) return(invisible(NULL))
    if (is.null(rv$start_ts)) return(invisible(NULL))

    event_observed <- if (isTRUE(rv$core_completed)) 1L else 0L
    event_ts <- if (isTRUE(rv$core_completed) && !is.null(rv$completion_ts)) rv$completion_ts else end_ts

    time_to_event_sec <- as.numeric(difftime(event_ts, rv$start_ts, units = "secs"))
    duration_sec <- as.numeric(difftime(end_ts, rv$start_ts, units = "secs"))

    ans_vec <- current_answer_vector()
    raw <- if (isTRUE(rv$core_completed) && !is.null(rv$result_raw)) rv$result_raw else score_answers(ans_vec)
    answered_n_val <- if (isTRUE(identical(rv$group, "A"))) answered_n_a() else if (isTRUE(identical(rv$group, "B"))) answered_n_b() else NA_integer_

    row1 <- data.frame(
      session_id = rv$session_id,
      group = rv$group,
      condition = rv$group,
      device_type = rv$device_type,
      start_ts = format(rv$start_ts, "%Y-%m-%d %H:%M:%S"),
      end_ts = format(end_ts, "%Y-%m-%d %H:%M:%S"),
      event_ts = format(event_ts, "%Y-%m-%d %H:%M:%S"),
      completion_ts = if (isTRUE(rv$core_completed) && !is.null(rv$completion_ts)) format(rv$completion_ts, "%Y-%m-%d %H:%M:%S") else NA_character_,
      duration_sec = round(duration_sec, 3),
      time_to_event_sec = round(time_to_event_sec, 3),
      time_to_successful_completion_sec = round(time_to_event_sec, 3),
      time_among_completers_sec = if (event_observed == 1L) round(time_to_event_sec, 3) else NA_real_,
      event_observed = event_observed,
      full_completion = event_observed,
      reached_results = event_observed,
      completion_status = if (event_observed == 1L) "completed" else "censored",
      censor_reason = if (event_observed == 1L) NA_character_ else "session_end",
      progress_depth = round(progress_depth(), 3),
      answered_n = answered_n_val,
      answered_item_count = answered_n_val,
      max_step_b = if (isTRUE(identical(rv$group, "B"))) as.integer(rv$step) else NA_integer_,
      n_submit_fail_a = rv$n_submit_fail_a,
      n_next_fail_b = rv$n_next_fail_b,
      n_step_next_b = rv$n_step_next_b,
      n_step_back_b = rv$n_step_back_b,
      revision_count = rv$revision_count,
      friction_events = rv$friction_events,
      friction_event_count = rv$friction_events,
      revision_rate = if (!is.na(answered_n_val) && answered_n_val > 0) round(rv$revision_count / answered_n_val, 4) else NA_real_,
      revision_rate_per_answered_item = if (!is.na(answered_n_val) && answered_n_val > 0) round(rv$revision_count / answered_n_val, 4) else NA_real_,
      friction_event_rate = if (!is.na(answered_n_val) && answered_n_val > 0) round(rv$friction_events / answered_n_val, 4) else NA_real_,
      friction_event_rate_per_answered_item = if (!is.na(answered_n_val) && answered_n_val > 0) round(rv$friction_events / answered_n_val, 4) else NA_real_,
      score_V = raw[["V"]],
      score_A = raw[["A"]],
      score_R = raw[["R"]],
      score_K = raw[["K"]],
      ease_of_use = rv$ease_of_use,
      primary_label = if (isTRUE(rv$core_completed) && !is.null(rv$result_label)) rv$result_label else primary_label(raw),
      stringsAsFactors = FALSE
    )
    for (i in 1:10) {
      row1[[paste0("q", i)]] <- ans_vec[[paste0("q", i)]]
    }

    append_log(row1, fname = "sessions.csv")
    rv$logged_summary <- TRUE
    invisible(NULL)
  }

  for (i in 1:10) {
    local({
      idx <- i
      nm <- paste0("q", idx)
      observeEvent(input[[nm]], ignoreInit = TRUE, {
        v <- suppressWarnings(as.integer(input[[nm]]))
        if (is.na(v)) return(invisible(NULL))
        prev <- rv$a_prev_answers[[nm]]
        if (!is.na(prev) && prev != v) {
          rv$revision_count <- rv$revision_count + 1L
          append_event_log(rv$session_id, rv$group, "answer_changed", list(question = idx))
          ga_event("answer_changed", list(variant = "A", condition = "A", question = idx))
        }
        rv$a_prev_answers[[nm]] <- v
        append_event_log(rv$session_id, rv$group, "question_answered", list(question = idx))
        ga_event("question_answered", list(variant = "A", condition = "A", question = idx))
      })
    })
  }

  missing_indices <- function(ans_named) {
    which(vapply(1:10, function(i) {
      is.na(suppressWarnings(as.integer(ans_named[[paste0("q", i)]])))
    }, logical(1L)))
  }

  enter_results <- function(ans_named) {
    raw <- score_answers(ans_named)
    rv$result_raw <- raw
    rv$result_label <- primary_label(raw)
    rv$core_completed <- TRUE
    rv$completion_ts <- Sys.time()
    rv$view <- "results"
  }

  results_ui <- function() {
    raw <- rv$result_raw
    pct <- round(100 * raw / MAX_BY_DIM[names(raw)], 1)

    feedback_block <- if (isTRUE(rv$ease_saved)) {
      tags$p(style = "color:#2b7;", "Thanks — your feedback was saved.")
    } else {
      tagList(
        tags$hr(),
        tags$h4("Optional usability feedback"),
        tags$p("This interface was easy to use."),
        radioButtons(
          inputId = "ease_of_use",
          label = NULL,
          choices = setNames(as.character(1:5), c(
            "1 — Strongly disagree",
            "2 — Disagree",
            "3 — Neutral",
            "4 — Agree",
            "5 — Strongly agree"
          )),
          selected = character(0)
        ),
        actionButton("submit_ease", "Save feedback", class = "btn-default"),
        verbatimTextOutput("msg_ease", placeholder = TRUE)
      )
    }

    tagList(
      tags$h3("Your result"),
      tags$p(tags$strong("Assigned variant: "), rv$group),
      tags$p(tags$strong("Primary style label: "), rv$result_label),
      tags$table(
        class = "table table-condensed",
        tags$thead(tags$tr(tags$th("Dimension"), tags$th("Raw"), tags$th("Max"), tags$th("%"))),
        tags$tbody(
          tags$tr(tags$td("Visual"), tags$td(raw[["V"]]), tags$td(MAX_BY_DIM[["V"]]), tags$td(pct[["V"]])),
          tags$tr(tags$td("Auditory"), tags$td(raw[["A"]]), tags$td(MAX_BY_DIM[["A"]]), tags$td(pct[["A"]])),
          tags$tr(tags$td("Read/Write"), tags$td(raw[["R"]]), tags$td(MAX_BY_DIM[["R"]]), tags$td(pct[["R"]])),
          tags$tr(tags$td("Kinesthetic"), tags$td(raw[["K"]]), tags$td(MAX_BY_DIM[["K"]]), tags$td(pct[["K"]]))
        )
      ),
      tags$p(
        style = "font-size:0.9em;color:#666;",
        "This mini-quiz is for a classroom UX experiment, not a clinical learning-style assessment."
      ),
      feedback_block
    )
  }

  quiz_ui_a <- function() {
    tagList(
      tags$p(tags$strong("Variant A (control): "), "single page, all 10 items."),
      lapply(1:10, function(i) {
        wellPanel(
          tags$strong(paste0("Q", i, ".")),
          tags$p(ITEMS$text[i]),
          radioButtons(
            inputId = paste0("q", i),
            label = NULL,
            choices = likert_choices,
            selected = character(0)
          )
        )
      }),
      actionButton("submit_a", "Submit", class = "btn-primary"),
      verbatimTextOutput("msg_a", placeholder = TRUE)
    )
  }

  quiz_ui_b <- function(step) {
    i <- as.integer(step)
    tagList(
      tags$p(tags$strong("Variant B (treatment): "), "step-by-step wizard."),
      tags$div(
        class = "progress",
        style = "height: 8px; margin-bottom: 12px;",
        tags$div(
          class = "progress-bar",
          role = "progressbar",
          style = sprintf("width: %d%%;", round(100 * i / 10))
        )
      ),
      tags$p(sprintf("Question %d of 10", i)),
      wellPanel(
        tags$strong(paste0("Q", i, ".")),
        tags$p(ITEMS$text[i]),
        radioButtons(
          inputId = "b_current",
          label = NULL,
          choices = likert_choices,
          selected = if (is.na(rv$b_answers[i])) character(0) else as.character(rv$b_answers[i])
        )
      ),
      fluidRow(
        column(2, if (i > 1L) actionButton("back_b", "Back")),
        column(
          3,
          if (i < 10L) {
            actionButton("next_b", "Next", class = "btn-primary")
          } else {
            actionButton("finish_b", "See my result", class = "btn-success")
          }
        )
      ),
      verbatimTextOutput("msg_b", placeholder = TRUE)
    )
  }

  output$body <- renderUI({
    req(rv$group)
    if (identical(rv$view, "results")) return(results_ui())
    if (identical(rv$group, "A")) quiz_ui_a() else quiz_ui_b(rv$step)
  })

  output$msg_a <- renderText({
    input$submit_a
    isolate({
      sa <- input$submit_a
      if (is.null(sa) || length(sa) != 1L) return("")
      if (sa == 0L) return("")
      miss <- missing_indices(collect_inputs_a())
      if (length(miss)) {
        rv$n_submit_fail_a <- rv$n_submit_fail_a + 1L
        rv$friction_events <- rv$friction_events + 1L
        append_event_log(rv$session_id, rv$group, "friction_event", list(channel = "submit", error_type = "missing_items", n_missing = length(miss)))
        ga_event("quiz_validation_error", list(variant = "A", condition = "A", error_type = "missing_items"))
        ga_event("friction_event", list(variant = "A", condition = "A", friction_type = "missing_items", n_missing = length(miss)))
        return(paste("Please answer all items. Missing: Q", paste(miss, collapse = ", "), sep = ""))
      }
      ""
    })
  })

  observeEvent(input$submit_a, {
    miss <- missing_indices(collect_inputs_a())
    append_event_log(rv$session_id, rv$group, "submit_attempt", list(success = if (length(miss)) 0L else 1L))
    if (length(miss)) return(invisible(NULL))
    ga_event("quiz_complete", list(variant = "A", condition = "A", flow = "single_page"))
    ga_event("successful_completion", list(
      variant = "A",
      condition = "A",
      flow = "single_page",
      time_to_event_sec = round(as.numeric(difftime(Sys.time(), rv$start_ts, units = "secs")), 2)
    ))
    append_event_log(rv$session_id, rv$group, "successful_completion", list(flow = "single_page"))
    enter_results(collect_inputs_a())
  })

  observeEvent(input$next_b, {
    v <- suppressWarnings(as.integer(input$b_current))
    if (is.na(v)) {
      rv$n_next_fail_b <- rv$n_next_fail_b + 1L
      rv$friction_events <- rv$friction_events + 1L
      append_event_log(rv$session_id, rv$group, "friction_event", list(channel = "next", error_type = "no_answer", step = rv$step))
      ga_event("quiz_validation_error", list(variant = "B", condition = "B", error_type = "no_answer", step = rv$step))
      ga_event("friction_event", list(variant = "B", condition = "B", friction_type = "no_answer", step = rv$step))
      return(invisible(NULL))
    }
    old_v <- rv$b_answers[rv$step]
    if (!is.na(old_v) && old_v != v) {
      rv$revision_count <- rv$revision_count + 1L
      append_event_log(rv$session_id, rv$group, "answer_changed", list(question = rv$step))
      ga_event("answer_changed", list(variant = "B", condition = "B", question = rv$step))
    }
    append_event_log(rv$session_id, rv$group, "question_answered", list(question = rv$step))
    ga_event("question_answered", list(variant = "B", condition = "B", question = rv$step))
    ga_event("quiz_step_next", list(variant = "B", condition = "B", from_step = rv$step))
    rv$n_step_next_b <- rv$n_step_next_b + 1L
    rv$b_answers[rv$step] <- v
    rv$step <- rv$step + 1L
  })

  observeEvent(input$back_b, {
    if (rv$step > 1L) {
      ga_event("quiz_step_back", list(variant = "B", condition = "B", from_step = rv$step))
      append_event_log(rv$session_id, rv$group, "step_back", list(from_step = rv$step))
      rv$n_step_back_b <- rv$n_step_back_b + 1L
      rv$step <- rv$step - 1L
    }
  })

  observeEvent(input$finish_b, {
    v <- suppressWarnings(as.integer(input$b_current))
    if (is.na(v)) {
      rv$n_next_fail_b <- rv$n_next_fail_b + 1L
      rv$friction_events <- rv$friction_events + 1L
      append_event_log(rv$session_id, rv$group, "submit_attempt", list(success = 0L))
      append_event_log(rv$session_id, rv$group, "friction_event", list(channel = "finish", error_type = "no_answer", step = rv$step))
      ga_event("quiz_validation_error", list(variant = "B", condition = "B", error_type = "no_answer", step = rv$step))
      ga_event("friction_event", list(variant = "B", condition = "B", friction_type = "no_answer", step = rv$step))
      return(invisible(NULL))
    }
    old_v <- rv$b_answers[rv$step]
    if (!is.na(old_v) && old_v != v) {
      rv$revision_count <- rv$revision_count + 1L
      append_event_log(rv$session_id, rv$group, "answer_changed", list(question = rv$step))
      ga_event("answer_changed", list(variant = "B", condition = "B", question = rv$step))
    }
    append_event_log(rv$session_id, rv$group, "question_answered", list(question = rv$step))
    ga_event("question_answered", list(variant = "B", condition = "B", question = rv$step))
    rv$b_answers[rv$step] <- v
    append_event_log(rv$session_id, rv$group, "submit_attempt", list(success = 1L))
    ga_event("quiz_complete", list(variant = "B", condition = "B", flow = "wizard"))
    ga_event("successful_completion", list(
      variant = "B",
      condition = "B",
      flow = "wizard",
      time_to_event_sec = round(as.numeric(difftime(Sys.time(), rv$start_ts, units = "secs")), 2)
    ))
    append_event_log(rv$session_id, rv$group, "successful_completion", list(flow = "wizard"))
    ans <- setNames(as.character(rv$b_answers), paste0("q", 1:10))
    enter_results(ans)
  })

  output$msg_b <- renderText({
    list(input$next_b, input$finish_b)
    isolate({
      nb <- input$next_b
      fb <- input$finish_b
      nb <- if (is.null(nb) || length(nb) != 1L) 0L else as.integer(nb)
      fb <- if (is.null(fb) || length(fb) != 1L) 0L else as.integer(fb)
      if ((nb + fb) == 0L) return("")
      v <- suppressWarnings(as.integer(input$b_current))
      if (is.na(v)) "Please select an answer before continuing." else ""
    })
  })

  output$msg_ease <- renderText({
    input$submit_ease
    isolate({
      if (isTRUE(rv$ease_saved)) return("")
      se <- input$submit_ease
      if (is.null(se) || length(se) != 1L || se == 0L) return("")
      v <- suppressWarnings(as.integer(input$ease_of_use))
      if (is.na(v) || !(v %in% 1:5)) return("Please choose a rating from 1 to 5.")
      ""
    })
  })

  observeEvent(input$submit_ease, {
    if (isTRUE(rv$ease_saved)) return(invisible(NULL))
    v <- suppressWarnings(as.integer(input$ease_of_use))
    if (is.na(v) || !(v %in% 1:5)) return(invisible(NULL))
    rv$ease_of_use <- v
    rv$ease_saved <- TRUE
    append_event_log(rv$session_id, rv$group, "post_task_rating_submitted", list(ease_of_use = v))
    ga_event("post_task_rating_submitted", list(variant = rv$group, condition = rv$group, ease_of_use = v))
  })

  session$onSessionEnded(function() {
    if (isTRUE(rv$logged_summary)) return(invisible(NULL))
    end_ts <- Sys.time()
    append_event_log(rv$session_id, rv$group, "end")
    if (!isTRUE(rv$core_completed)) {
      append_event_log(rv$session_id, rv$group, "session_censored", list(progress_depth = round(progress_depth(), 3)))
      ga_event("quiz_session_censored", list(
        variant = rv$group,
        condition = rv$group,
        progress_depth = round(progress_depth(), 3),
        time_to_event_sec = round(as.numeric(difftime(end_ts, rv$start_ts, units = "secs")), 2)
      ))
    }
    log_session_summary(end_ts = end_ts)
    invisible(NULL)
  })

  observe({
    req(identical(rv$view, "results"))
    if (rv$logged_results) return(invisible(NULL))
    append_event_log(rv$session_id, rv$group, "results")
    ga_event(
      "view_results",
      list(
        variant = rv$group,
        condition = rv$group,
        time_to_event_sec = round(as.numeric(difftime(rv$completion_ts, rv$start_ts, units = "secs")), 2),
        primary_style = rv$result_label
      )
    )
    log_session_summary(end_ts = rv$completion_ts)
    rv$logged_results <- TRUE
  })
}

shinyApp(ui, server)
