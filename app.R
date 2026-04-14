# =============================================================================
# Project 3 — Learning style mini-quiz (Shiny A/B) + Google Analytics 4 (gtag)
# ============================================================================
# Google Analytics 4 (Web stream: “AB Test”, https://www.project3shiela.com):
#   Default Measurement ID is set below (same as GA “Install manually” gtag snippet).
#   Override if needed: Sys.setenv(GA_MEASUREMENT_ID = "G-...")
#
# --- Aligned with “R Shiny Google Analytics” (Appsilon PDF / blog) ---
#   • Two <script> entries in tags$head: async gtag/js URL + inline init (same idea as
#     external loader + optional www/static/js/gtag.js in the article).
#   • Custom behaviour: gtag('event', '<name>', { ... }) — see Google’s event reference.
#   • Verify: GA4 → Reports → Realtime → “Event count by Event name” (lag often 1–2 min).
#   • Legal: article notes GDPR / consent for EU users; add a cookie banner if required.
#   • Shiny is SPA-like: we send extra virtual page_view events when the UI switches
#     (quiz vs results) so paths show up in reports, not only the initial load.
# =============================================================================

# install.packages(c("shiny", "shinyjs"))

library(shiny)
library(shinyjs)

# --- GA4: Measurement ID (Google tag “Install manually” — one tag per page) ---
GA_MEASUREMENT_ID <- Sys.getenv("GA_MEASUREMENT_ID", unset = "G-LFFGXVY0PY")

ga_enabled <- function() {
  id <- GA_MEASUREMENT_ID
  nzchar(id) && grepl("^G-[A-Z0-9]+$", id)
}

#' Inject gtag.js in <head> (GA “Install manually”: one external + one inline script)
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

esc_js_str <- function(x) gsub("'", "\\\\'", as.character(x), fixed = TRUE)

#' Build a JS object literal { key: value, ... } for gtag (snake_case params per GA4 examples)
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

#' Virtual page views (Shiny rarely does full page loads; helps GA4 “pages” and engagement)
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

#' Custom GA4 events from server (same pattern as Appsilon’s gtag('event', ...) in jQuery)
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

# --- Item metadata ------------------------------------------------------------
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

# --- UI -----------------------------------------------------------------------
ui <- fluidPage(
  useShinyjs(),
  if (ga_enabled()) ga_head_tags(GA_MEASUREMENT_ID),
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$style(HTML("
      /* Mobile-first tweaks (Bootstrap is responsive, these just improve touch UX) */
      .well { padding: 14px; }
      .radio { margin-top: 8px; margin-bottom: 8px; }
      .btn { min-height: 44px; }
      @media (max-width: 576px) {
        .container-fluid { padding-left: 12px; padding-right: 12px; }
        h2 { font-size: 20px; }
        .progress { margin-bottom: 16px; }
        /* Make navigation buttons easier to tap */
        #next_b, #finish_b, #submit_a { width: 100%; }
        /* Keep Back visible but compact */
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

# --- Server -------------------------------------------------------------------
server <- function(input, output, session) {
  rv <- reactiveValues(
    group = NULL,
    session_id = new_session_id(),
    start_ts = NULL,
    step = 1L,
    b_answers = rep(NA_integer_, 10L),
    view = "quiz",
    ease_of_use = NA_integer_,
    logged_results = FALSE,
    logged_start = FALSE,
    logged_summary = FALSE,
    n_submit_fail_a = 0L,
    n_next_fail_b = 0L,
    revision_count = 0L,
    friction_events = 0L,
    a_prev_answers = setNames(rep(NA_integer_, 10L), paste0("q", 1:10)),
    ga_assign_sent = FALSE,
    ga_pv_quiz = FALSE,
    ga_pv_results = FALSE,
    device_type = "unknown"
  )

  observe({
    # best-effort device classification (useful for interpretation and balance checks)
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
    ga_event("ab_assign", list(variant = rv$group, app_session_id = rv$session_id))
    rv$ga_assign_sent <- TRUE
  })

  # Virtual page_view when quiz UI is shown (SPA-style; complements Appsilon’s page-level tag)
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

  # Virtual page_view on results (so “pages” reflect funnel end)
  observe({
    req(identical(rv$view, "results"))
    if (isTRUE(rv$ga_pv_results)) return(invisible(NULL))
    ga_page_view("/results", "Learning style — results")
    rv$ga_pv_results <- TRUE
  })

  observe({
    req(rv$group)
    if (isTRUE(rv$logged_start)) return(invisible(NULL))
    append_log(
      data.frame(
        session_id = rv$session_id,
        group = rv$group,
        event = "start",
        ts = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        stringsAsFactors = FALSE
      ),
      fname = "session_events.csv"
    )
    ga_event("quiz_session_start", list(variant = rv$group))
    rv$logged_start <- TRUE
  })

  answered_n_a <- function() {
    ans <- collect_inputs_a()
    sum(!is.na(suppressWarnings(as.integer(unlist(ans, use.names = FALSE)))))
  }

  answered_n_b <- function() {
    tmp <- rv$b_answers
    # Include current selection even if user hasn't clicked next yet
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

  max_step_b_now <- function() {
    if (!isTRUE(identical(rv$group, "B"))) return(NA_integer_)
    i <- as.integer(rv$step)
    # step is 1..10; "max reached" is previous steps fully passed through
    max(1L, min(10L, i)) %||% NA_integer_
  }

  log_session_summary <- function(reached_results, end_ts = Sys.time()) {
    if (isTRUE(rv$logged_summary)) return(invisible(NULL))
    if (is.null(rv$start_ts)) return(invisible(NULL))

    dur <- as.numeric(difftime(end_ts, rv$start_ts, units = "secs"))

    ans_vec <- if (isTRUE(identical(rv$group, "A"))) {
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

    raw <- if (isTRUE(reached_results) && !is.null(rv$result_raw)) {
      rv$result_raw
    } else {
      score_answers(ans_vec)
    }

    answered_n_val <- if (isTRUE(identical(rv$group, "A"))) answered_n_a() else if (isTRUE(identical(rv$group, "B"))) answered_n_b() else NA_integer_
    row1 <- data.frame(
      session_id = rv$session_id,
      group = rv$group,
      device_type = rv$device_type,
      start_ts = format(rv$start_ts, "%Y-%m-%d %H:%M:%S"),
      end_ts = format(end_ts, "%Y-%m-%d %H:%M:%S"),
      duration_sec = round(dur, 3),
      reached_results = if (isTRUE(reached_results)) 1L else 0L,
      progress_depth = round(progress_depth(), 3),
      answered_n = answered_n_val,
      max_step_b = if (isTRUE(identical(rv$group, "B"))) as.integer(rv$step) else NA_integer_,
      n_submit_fail_a = rv$n_submit_fail_a,
      n_next_fail_b = rv$n_next_fail_b,
      revision_count = rv$revision_count,
      friction_events = rv$friction_events,
      revision_rate = if (!is.na(answered_n_val) && answered_n_val > 0) round(rv$revision_count / answered_n_val, 4) else NA_real_,
      friction_event_rate = if (!is.na(answered_n_val) && answered_n_val > 0) round(rv$friction_events / answered_n_val, 4) else NA_real_,
      score_V = raw[["V"]],
      score_A = raw[["A"]],
      score_R = raw[["R"]],
      score_K = raw[["K"]],
      ease_of_use = rv$ease_of_use,
      primary_label = if (isTRUE(reached_results)) rv$result_label else primary_label(raw),
      stringsAsFactors = FALSE
    )
    for (i in 1:10) {
      row1[[paste0("q", i)]] <- ans_vec[[paste0("q", i)]]
    }

    append_log(row1, fname = "sessions.csv")
    rv$logged_summary <- TRUE
    invisible(NULL)
  }

  collect_inputs_a <- reactive({
    ans <- setNames(rep(NA_character_, 10L), paste0("q", 1:10))
    for (i in 1:10) {
      nm <- paste0("q", i)
      if (!is.null(input[[nm]])) ans[[nm]] <- input[[nm]]
    }
    ans
  })

  # Track answer revisions in single-page variant
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
          ga_event("answer_changed", list(variant = "A", question = idx))
        }
        rv$a_prev_answers[[nm]] <- v
        ga_event("question_answered", list(variant = "A", question = idx))
      })
    })
  }

  missing_indices <- function(ans_named) {
    which(vapply(1:10, function(i) {
      is.na(suppressWarnings(as.integer(ans_named[[paste0("q", i)]])))
    }, logical(1L)))
  }

  enter_post_task <- function(ans_named) {
    raw <- score_answers(ans_named)
    rv$result_raw <- raw
    rv$result_label <- primary_label(raw)
    rv$view <- "post_task"
  }

  go_results <- function() {
    rv$view <- "results"
  }

  output$body <- renderUI({
    req(rv$group)

    if (identical(rv$view, "post_task")) {
      return(post_task_ui())
    }

    if (identical(rv$view, "results")) {
      return(results_ui())
    }

    if (identical(rv$group, "A")) {
      quiz_ui_a()
    } else {
      quiz_ui_b(rv$step)
    }
  })

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

  results_ui <- function() {
    raw <- rv$result_raw
    pct <- round(100 * raw / MAX_BY_DIM[names(raw)], 1)
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
      )
    )
  }

  post_task_ui <- function() {
    tagList(
      tags$h3("One quick feedback question"),
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
      actionButton("submit_post", "Submit and see result", class = "btn-success"),
      verbatimTextOutput("msg_post", placeholder = TRUE)
    )
  }

  output$msg_a <- renderText({
    input$submit_a
    isolate({
      # Before inputs bind, input$submit_a can be NULL → if() gets length-0 condition
      sa <- input$submit_a
      if (is.null(sa) || length(sa) != 1L) return("")
      if (sa == 0L) return("")
      miss <- missing_indices(collect_inputs_a())
      if (length(miss)) {
        rv$n_submit_fail_a <- rv$n_submit_fail_a + 1L
        rv$friction_events <- rv$friction_events + 1L
        ga_event("quiz_validation_error", list(variant = "A", error_type = "missing_items"))
        return(paste("Please answer all items. Missing: Q", paste(miss, collapse = ", "), sep = ""))
      }
      ""
    })
  })

  observeEvent(input$submit_a, {
    miss <- missing_indices(collect_inputs_a())
    append_log(
      data.frame(
        session_id = rv$session_id,
        group = rv$group,
        event = "submit_attempt",
        ts = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        success = if (length(miss)) 0L else 1L,
        stringsAsFactors = FALSE
      ),
      fname = "session_events.csv"
    )
    if (length(miss)) return(invisible(NULL))
    ga_event("quiz_complete", list(variant = "A", flow = "single_page"))
    enter_post_task(collect_inputs_a())
  })

  observeEvent(input$next_b, {
    v <- suppressWarnings(as.integer(input$b_current))
    if (is.na(v)) {
      rv$n_next_fail_b <- rv$n_next_fail_b + 1L
      rv$friction_events <- rv$friction_events + 1L
      ga_event("quiz_validation_error", list(variant = "B", error_type = "no_answer", step = rv$step))
      return(invisible(NULL))
    }
    old_v <- rv$b_answers[rv$step]
    if (!is.na(old_v) && old_v != v) {
      rv$revision_count <- rv$revision_count + 1L
      ga_event("answer_changed", list(variant = "B", question = rv$step))
    }
    ga_event("question_answered", list(variant = "B", question = rv$step))
    ga_event("quiz_step_next", list(variant = "B", from_step = rv$step))
    rv$b_answers[rv$step] <- v
    rv$step <- rv$step + 1L
  })

  observeEvent(input$back_b, {
    if (rv$step > 1L) {
      ga_event("quiz_step_back", list(variant = "B", from_step = rv$step))
      rv$step <- rv$step - 1L
    }
  })

  observeEvent(input$finish_b, {
    v <- suppressWarnings(as.integer(input$b_current))
    if (is.na(v)) {
      rv$n_next_fail_b <- rv$n_next_fail_b + 1L
      rv$friction_events <- rv$friction_events + 1L
      ga_event("quiz_validation_error", list(variant = "B", error_type = "no_answer", step = rv$step))
      append_log(
        data.frame(
          session_id = rv$session_id,
          group = rv$group,
          event = "submit_attempt",
          ts = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          success = 0L,
          stringsAsFactors = FALSE
        ),
        fname = "session_events.csv"
      )
      return(invisible(NULL))
    }
    old_v <- rv$b_answers[rv$step]
    if (!is.na(old_v) && old_v != v) {
      rv$revision_count <- rv$revision_count + 1L
      ga_event("answer_changed", list(variant = "B", question = rv$step))
    }
    ga_event("question_answered", list(variant = "B", question = rv$step))
    rv$b_answers[rv$step] <- v
    append_log(
      data.frame(
        session_id = rv$session_id,
        group = rv$group,
        event = "submit_attempt",
        ts = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        success = 1L,
        stringsAsFactors = FALSE
      ),
      fname = "session_events.csv"
    )
    ga_event("quiz_complete", list(variant = "B", flow = "wizard"))
    ans <- setNames(as.character(rv$b_answers), paste0("q", 1:10))
    enter_post_task(ans)
  })

  output$msg_post <- renderText({
    input$submit_post
    isolate({
      sp <- input$submit_post
      if (is.null(sp) || length(sp) != 1L || sp == 0L) return("")
      v <- suppressWarnings(as.integer(input$ease_of_use))
      if (is.na(v) || !(v %in% 1:5)) return("Please choose a rating from 1 to 5.")
      ""
    })
  })

  observeEvent(input$submit_post, {
    v <- suppressWarnings(as.integer(input$ease_of_use))
    if (is.na(v) || !(v %in% 1:5)) return(invisible(NULL))
    rv$ease_of_use <- v
    ga_event("post_task_rating_submitted", list(variant = rv$group, ease_of_use = v))
    go_results()
  })

  output$msg_b <- renderText({
    list(input$next_b, input$finish_b)
    isolate({
      # Only one of next_b / finish_b exists in the UI at a time; missing input is NULL.
      # NULL in arithmetic gives numeric(0) → if() errors; coerce missing to 0L.
      nb <- input$next_b
      fb <- input$finish_b
      nb <- if (is.null(nb) || length(nb) != 1L) 0L else as.integer(nb)
      fb <- if (is.null(fb) || length(fb) != 1L) 0L else as.integer(fb)
      if ((nb + fb) == 0L) return("")
      v <- suppressWarnings(as.integer(input$b_current))
      if (is.na(v)) "Please select an answer before continuing."
      else ""
    })
  })

  # Ensure incomplete sessions are also logged (censoring for survival-style analysis)
  session$onSessionEnded(function() {
    if (isTRUE(rv$logged_results) || isTRUE(rv$logged_summary)) return(invisible(NULL))
    end_ts <- Sys.time()
    append_log(
      data.frame(
        session_id = rv$session_id,
        group = rv$group,
        event = "end",
        ts = format(end_ts, "%Y-%m-%d %H:%M:%S"),
        stringsAsFactors = FALSE
      ),
      fname = "session_events.csv"
    )
    log_session_summary(reached_results = FALSE, end_ts = end_ts)
    ga_event("quiz_session_end", list(variant = rv$group, reached_results = FALSE, progress_depth = round(progress_depth(), 3)))
    invisible(NULL)
  })

  observe({
    req(identical(rv$view, "results"))
    if (rv$logged_results) return(invisible(NULL))

    end_ts <- Sys.time()
    log_session_summary(reached_results = TRUE, end_ts = end_ts)
    append_log(
      data.frame(
        session_id = rv$session_id,
        group = rv$group,
        event = "results",
        ts = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        stringsAsFactors = FALSE
      ),
      fname = "session_events.csv"
    )

    ga_event(
      "view_results",
      list(
        variant = rv$group,
        duration_sec = round(as.numeric(difftime(end_ts, rv$start_ts, units = "secs")), 2),
        primary_style = rv$result_label
      )
    )

    rv$logged_results <- TRUE
  })
}

shinyApp(ui, server)
