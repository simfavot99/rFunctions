#' Interactive variable explorer (Shiny app)
#'
#' Launches a Shiny app with two tabs:
#' * **Explorer** — three-column layout: scrollable variable list (flat or
#'   grouped by type), an interactive frequency table with colour-bar
#'   percentages, and an interactive distribution plot.
#' * **Copy** — a read-only monospaced text area showing the top 30 most
#'   frequent values + percentage for every variable. Press Ctrl+A then Ctrl+C
#'   to copy the full summary.
#'
#' Variable types are classified as Integer, Numeric, Character, Factor,
#' Logical, Date, or Other. Integer and Numeric are treated separately so that
#' `int` columns are not lumped with `dbl` columns.
#'
#' @param data A data frame to explore.
#' @param max_n Maximum unique values shown in the Explorer frequency table and
#'   bar chart. Default `50`. The Copy tab always caps at 30, independently.
#' @return Launches a Shiny app via [shiny::shinyApp()].
#' @export
eye_shiny <- function(data, max_n = 50) {
  stopifnot(is.data.frame(data))
  if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
  pacman::p_load(shiny, bslib, ggiraph, ggplot2, DT)

  # --- helpers ----------------------------------------------------------------
  classify <- function(v) {
    if (inherits(v, c("Date", "POSIXct", "POSIXlt"))) "Date"
    else if (is.integer(v))   "Integer"
    else if (is.numeric(v))   "Numeric"
    else if (is.logical(v))   "Logical"
    else if (is.factor(v))    "Factor"
    else if (is.character(v)) "Character"
    else                      "Other"
  }

  types    <- setNames(sapply(data, classify), names(data))
  all_vars <- names(data)
  grp_ord  <- c("Integer", "Numeric", "Character", "Factor", "Logical", "Date", "Other")
  present  <- grp_ord[grp_ord %in% types]

  .is_uniform <- function(x) {
    tbl <- table(x, useNA = "no")
    length(tbl) > 1L && length(unique(as.integer(tbl))) == 1L
  }

  .freq_df <- function(x) {
    tbl <- sort(table(x, useNA = "ifany"), decreasing = TRUE)
    n   <- length(x)
    pct <- as.integer(tbl) / n * 100
    df  <- data.frame(
      Value   = names(tbl),
      Count   = as.integer(tbl),
      Percent = sprintf("%.1f%%", pct),
      pct_num = pct,
      stringsAsFactors = FALSE
    )
    list(df = head(df, max_n), n_total = nrow(df), capped = nrow(df) > max_n)
  }

  eye_theme <- theme_minimal(base_size = 13) +
    theme(
      panel.grid.minor  = element_blank(),
      axis.title        = element_text(size = 11, colour = "#555555"),
      plot.title        = element_text(face = "bold", size = 13, colour = "#2d4a7e"),
      axis.text         = element_text(colour = "#444444"),
      plot.margin       = margin(10, 15, 10, 10)
    )

  grouped_choices <- setNames(
    lapply(present, function(grp) all_vars[types == grp]),
    present
  )

  # --- plain-text copy summary (generated once) -------------------------------
  .make_copy_text <- function() {
    copy_n <- 30L
    sep    <- strrep("=", 62)
    thin   <- strrep("-", 62)
    val_w  <- 30L

    .box <- function(x) {
      tbl  <- sort(table(x, useNA = "ifany"), decreasing = TRUE)
      n    <- length(x)
      df   <- data.frame(
        Value   = names(tbl),
        Percent = sprintf("%.1f%%", as.integer(tbl) / n * 100),
        stringsAsFactors = FALSE
      )
      show   <- head(df, copy_n)
      capped <- nrow(df) > copy_n
      top  <- paste0("  +", strrep("-", val_w + 2L), "+--------+")
      hdr  <- sprintf("  | %-*s | %-6s |", val_w, "Value", "Pct")
      mid  <- paste0("  +", strrep("-", val_w + 2L), "+--------+")
      bot  <- paste0("  +", strrep("-", val_w + 2L), "+--------+")
      rows <- vapply(seq_len(nrow(show)), function(i) {
        val <- show$Value[i]
        if (is.na(val)) val <- "<NA>"
        if (nchar(val) > val_w) val <- paste0(substr(val, 1L, val_w - 1L), "~")
        sprintf("  | %-*s | %6s |", val_w, val, show$Percent[i])
      }, character(1))
      more <- if (capped)
        sprintf("  ... and %d more values", nrow(df) - copy_n)
      else character(0)
      c(top, hdr, mid, rows, bot, more)
    }

    lines <- c(sep,
               sprintf("  EYE SUMMARY  |  %d variables, %d rows", ncol(data), nrow(data)),
               sep)

    for (grp in present) {
      grp_vars <- all_vars[types == grp]
      lines <- c(lines, "",
                 sprintf("[ %s  (%d variable%s) ]",
                         grp, length(grp_vars), if (length(grp_vars) > 1) "s" else ""),
                 thin)
      for (v in grp_vars) {
        x      <- data[[v]]
        n      <- length(x)
        n_miss <- sum(is.na(x))
        n_uniq <- length(unique(na.omit(x)))
        lines  <- c(lines, "",
                    v,
                    sprintf("  n=%d  |  missing=%d (%.1f%%)  |  unique=%d",
                            n, n_miss, n_miss / n * 100, n_uniq))
        if (grp == "Date") {
          xv <- sort(na.omit(x))
          lines <- c(lines,
                     sprintf("  Range:  %s  -->  %s", format(xv[1]), format(xv[length(xv)])))
        }
        lines <- c(lines, .box(x))
      }
    }
    paste(c(lines, "", sep), collapse = "\n")
  }

  copy_text <- .make_copy_text()

  # --- UI scaffold ------------------------------------------------------------
  app_theme <- bs_theme(version = 5, bootswatch = "flatly", primary = "#2d4a7e")
  app_css <- HTML("
    body { background: #f5f6f8; }
    .card { border: none; border-radius: .6rem;
            box-shadow: 0 2px 8px rgba(0,0,0,.05); }
    .card-header { background: #fff; border-bottom: 1px solid #e9ecef;
                   font-weight: 600; padding: .7rem 1rem; }
    .uniform-banner { background: #fff8e1; border-left: 4px solid #ffc107;
                      padding: 10px 16px; margin-bottom: 14px; border-radius: 4px;
                      font-size: .9rem; display: flex; align-items: flex-start; gap: 10px; }
    .date-range-badge { background: #2d4a7e; color: white; padding: 6px 14px;
                        border-radius: 4px; font-size: .88rem; font-weight: 600;
                        margin-bottom: 10px; display: inline-block; }
    .date-info p { margin-bottom: .3rem; font-size: .9rem; }
    table.dataTable thead th { background: #f0f4fa !important;
                                color: #2d4a7e !important; font-weight: 600; }
    select { font-size: .85rem !important; }
    .copy-hint { color: #666; font-size: .88rem; margin-bottom: 8px; }
    textarea.copy-box { width: 100%; height: 620px; font-family: 'Courier New', monospace;
                        font-size: 12px; background: #f8f9fa; border: 1px solid #dee2e6;
                        border-radius: 4px; padding: 14px; resize: vertical;
                        white-space: pre; }
  ")

  title_bar <- div(
    style = paste0(
      "background: linear-gradient(90deg,#2d4a7e,#4e74a3); color: white; ",
      "padding: 18px 24px; border-radius: .6rem; margin-bottom: 16px; ",
      "box-shadow: 0 2px 8px rgba(0,0,0,.05);"
    ),
    h3("eye_shiny — Variable Explorer",
       style = "margin: 0; font-weight: 700; font-size: 1.4rem;"),
    p(sprintf("%d variables · %d rows", ncol(data), nrow(data)),
      style = "margin: 4px 0 0; opacity: .85; font-size: .9rem;")
  )

  explorer_tab <- nav_panel(
    title = "Explorer",
    br(),
    uiOutput("uniform_banner"),
    layout_columns(
      col_widths = c(3, 4, 5),
      card(
        card_header("Variables"),
        radioButtons(
          "var_mode", NULL,
          choices  = c("All" = "all", "By category" = "grouped"),
          selected = "all",
          inline   = TRUE
        ),
        uiOutput("var_list_ui")
      ),
      card(
        card_header("Frequency table"),
        uiOutput("tbl_header"),
        DT::DTOutput("freq_tbl")
      ),
      card(
        card_header("Distribution"),
        girafeOutput("dist_plot", height = "450px")
      )
    )
  )

  copy_tab <- nav_panel(
    title = "Copy",
    br(),
    tags$p("Click inside the box, then Ctrl+A to select all and Ctrl+C to copy.",
           class = "copy-hint"),
    tags$textarea(copy_text, class = "copy-box", readonly = NA)
  )

  ui <- page_fluid(
    theme = app_theme, tags$head(tags$style(app_css)),
    title_bar,
    navset_card_tab(explorer_tab, copy_tab)
  )

  # --- server -----------------------------------------------------------------
  server <- function(input, output, session) {

    output$var_list_ui <- renderUI({
      choices <- if (isTRUE(input$var_mode == "grouped")) grouped_choices else all_vars
      selectInput(
        "selected_var", NULL,
        choices   = choices,
        width     = "100%",
        size      = min(length(all_vars), 20),
        selectize = FALSE
      )
    })

    sel <- reactive({
      v <- input$selected_var
      req(v)
      req(v %in% all_vars)
      list(name = v, vec = data[[v]], type = unname(types[v]))
    })

    output$uniform_banner <- renderUI({
      s   <- sel()
      x   <- s$vec
      grp <- s$type
      if (grp == "Date" || !.is_uniform(x)) return(NULL)
      tbl  <- table(x, useNA = "no")
      freq <- unique(as.integer(tbl))[1L]
      div(class = "uniform-banner",
        tags$span("⚠️", style = "font-size: 1.1rem;"),
        tags$span(
          tags$b("Uniform distribution — "),
          sprintf(
            "all %d unique values appear equally: %d times each (%.1f%% each).",
            length(tbl), freq, freq / length(x) * 100
          )
        )
      )
    })

    output$tbl_header <- renderUI({
      s   <- sel()
      x   <- s$vec
      grp <- s$type
      if (grp != "Date") return(NULL)
      xv <- sort(na.omit(x))
      tagList(
        div(class = "date-range-badge",
          sprintf("  %s  →  %s", format(min(xv)), format(max(xv)))
        ),
        div(class = "date-info",
          tags$p(tags$b("Unique dates: "), length(unique(xv))),
          tags$p(tags$b("First: "), paste(format(head(xv, 5)), collapse = "  ")),
          tags$p(tags$b("Last:  "), paste(format(tail(xv, 5)), collapse = "  "))
        )
      )
    })

    output$freq_tbl <- DT::renderDT({
      s <- sel()
      if (s$type == "Date") {
        return(DT::datatable(
          data.frame(Info = "See date summary above."),
          rownames = FALSE,
          options  = list(dom = "t")
        ))
      }
      res     <- .freq_df(s$vec)
      cap     <- if (res$capped)
        sprintf("Showing top %d of %d unique values", max_n, res$n_total)
      else NULL
      display <- res$df[, c("Value", "Percent", "pct_num")]
      DT::datatable(
        display,
        caption  = cap,
        rownames = FALSE,
        options  = list(
          pageLength = max_n, dom = "t", ordering = FALSE,
          columnDefs = list(
            list(className = "dt-left",  targets = 0),
            list(className = "dt-right", targets = 1),
            list(visible = FALSE,        targets = 2),
            list(width = "72%", targets = 0),
            list(width = "28%", targets = 1)
          )
        ),
        class = "compact stripe"
      ) |>
        DT::formatStyle(
          columns      = "Percent",
          valueColumns = "pct_num",
          background   = DT::styleColorBar(c(0, 100), "#d0e4f7"),
          backgroundSize     = "98% 55%",
          backgroundRepeat   = "no-repeat",
          backgroundPosition = "center"
        )
    })

    output$dist_plot <- renderGirafe({
      s   <- sel()
      x   <- s$vec
      grp <- s$type
      nm  <- s$name

      g <- if (grp == "Date") {
        df_d      <- as.data.frame(table(date = as.Date(na.omit(x))))
        df_d$date <- as.Date(as.character(df_d$date))
        df_d$Freq <- as.integer(df_d$Freq)
        ggplot(df_d, aes(x = date, y = Freq)) +
          geom_col_interactive(
            aes(tooltip = paste0(format(date, "%Y-%m-%d"), "\nCount: ", Freq),
                data_id = as.character(date)),
            fill = "#4472C4", alpha = .85
          ) +
          labs(x = NULL, y = "Count", title = nm) +
          eye_theme +
          theme(panel.grid.major.x = element_blank())

      } else if (grp %in% c("Integer", "Numeric")) {
        xv    <- na.omit(x)
        n_obs <- length(xv)
        h     <- hist(xv, breaks = pretty(range(xv), n = 30), plot = FALSE)
        df_h  <- data.frame(
          xmid  = h$mids,
          xlo   = h$breaks[-length(h$breaks)],
          xhi   = h$breaks[-1L],
          count = h$counts,
          pct   = h$counts / n_obs * 100,
          stringsAsFactors = FALSE
        )
        ggplot(df_h, aes(x = xmid, y = pct, width = xhi - xlo)) +
          geom_col_interactive(
            aes(tooltip = paste0("[", round(xlo, 3), ", ", round(xhi, 3), ")\n",
                                 sprintf("%.1f%%", pct), " (n = ", count, ")"),
                data_id = as.character(xmid)),
            fill = "#4472C4", colour = "white", alpha = .85
          ) +
          scale_y_continuous(labels = function(y) paste0(round(y, 1), "%")) +
          labs(x = nm, y = "Percentage (%)", title = nm) +
          eye_theme +
          theme(
            panel.grid.major.x = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1)
          )

      } else {
        res        <- .freq_df(x)
        df_b       <- res$df
        df_b$Value <- factor(df_b$Value, levels = rev(df_b$Value))
        ggplot(df_b, aes(x = Value, y = pct_num)) +
          geom_col_interactive(
            aes(tooltip = paste0(Value, "\n",
                                 sprintf("%.1f%%", pct_num),
                                 " (n = ", Count, ")"),
                data_id = as.character(Value)),
            fill = "#4472C4", alpha = .85
          ) +
          scale_y_continuous(labels = function(y) paste0(round(y, 1), "%")) +
          coord_flip() +
          labs(x = NULL, y = "Percentage (%)", title = nm) +
          eye_theme +
          theme(
            panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_line(colour = "#e9ecef")
          )
      }

      girafe(
        ggobj      = g,
        width_svg  = 7,
        height_svg = 4.5,
        options    = list(
          opts_hover(css = "fill: #e3a978;"),
          opts_tooltip(css = paste0(
            "background: #2d4a7e; color: white; ",
            "padding: 6px 10px; border-radius: 4px; font-size: 13px;"
          ))
        )
      )
    })
  }

  shinyApp(ui, server)
}