# app.R

library(shiny)
library(bslib)
library(ggplot2)
library(pwr)

'
unlink("docs", recursive = TRUE, force = TRUE)
shinylive::export(appdir = ".", destdir = "docs")
httpuv::runStaticServer("docs", port = 8000)
'


if (FALSE) {
  library(munsell)
  library(tibble)
  #library(tidyverse)
}


Plot_theme <- function(
    base_font    = c("Avenir Next", "Verdana", "sans-serif"),
    heading_font = c("Trebuchet MS", "Avenir", "Verdana", "sans-serif"),
    base_size = 14,
    transparent = FALSE,
    show_minor_grid = TRUE
) {
  base_family    <- base_font[[1]]
  heading_family <- heading_font[[1]]
  bg <- if (transparent) "transparent" else "#f7f6f3"

  theme_minimal(base_size = base_size, base_family = base_family) %+replace% theme(
    # Backgrounds
    plot.background  = element_rect(fill = bg, color = NA),
    panel.background = element_rect(fill = bg, color = NA),

    # Text color baseline
    text = element_text(color = "#343A40"),

    # Grid lines (cool blue)
    panel.grid.major = element_line(color = "#cfdceb", linewidth = 0.45),
    panel.grid.minor = if (isTRUE(show_minor_grid)) {
      element_line(color = "#e3edf7", linewidth = 0.25)
    } else {
      element_blank()
    },

    # Axes
    axis.line = element_line(color = "#9fb3c1"),

    axis.text.x = element_text(color = "#343A40", family = base_family, size = 15, angle = 0),
    axis.text.y = element_text(color = "#343A40", family = base_family, size = 15, angle = 0),

    axis.title.x = element_text(color = "#343A40", family = heading_family, face = "bold", size = 15, angle = 0, margin = margin(t = 3)),
    axis.title.y = element_text(color = "#343A40", family = heading_family, face = "bold", size = 15, angle = 90, margin = margin(r = 3)),

    # Titles
    plot.title = element_text(
      family = heading_family, size = 18, face = "bold",
      color = "#2f3e46",
      margin = margin(b = 10, unit = "pt")
    ),
    plot.subtitle = element_text(family = base_family, size = 14, color = "#3b4a55"),

    # Legend
    legend.title = element_text(family = heading_family, face = "bold", color = "#2f3e46"),
    legend.text  = element_text(family = base_family, color = "#3b4a55"),

    # Spacing
    plot.margin = margin(t = 10, r = 20, b = 5, l = 10, unit = "pt")
  )
}


source("SSandP.R")

ui <- page_navbar(
  title = "Intuition Lab",
  theme = bs_theme(version = 5,
                   bg            = "#f7f6f3",   # page background
                   fg            = "#343A40",   # default text/link color
                   primary       = "#2f6f8f",   # .btn-primary, active tabs
                   secondary     = "#495057",   # navbar background
                   info          = "#2f6f8f",   # accents for h6, info alerts, etc.
                   code_bg       = "#f7f6f3",   # <code> & <pre> background
                   code_fg       = "#3182ce",   # <code> & <pre> text
                   base_font     = c("Avenir Next", "Verdana", "sans-serif"),
                   heading_font  = c("Trebuchet MS", "Avenir", "Verdana", "sans-serif"),
                   link_color_hover = "#137288",
                   link_visited     = "#137288",),


  tags$head(
    tags$link(rel = "stylesheet", href = "styles.css")
  ),

  # These two make the app use the viewport more naturally
  fillable = TRUE,

  # Optional: remove the “container” max-width so it spans the page nicely
  # (If you prefer constrained width, delete this line.)
  fluid = TRUE,

  # --- Left sidebar shared across all tabs ---
  sidebar = sidebar(
    width = 450,
    h5("Conceptual knobs"),
    selectInput(
      "mock_test",
      "Scenario",
      choices = c(
        "Two-group mean difference (t-test)" = "two_t",
        "Two-group mean difference (paired t-test)" = "paired_t",
        "One-way ANOVA (k groups)" = "anova1",
        "Chi-square (df)" = "chisq"
      ),
      selected = "two_t"
    ),
    conditionalPanel(
      condition = "input.mock_test === 'anova1'",

      tagList(
        p(strong("What it is")),
        p("In ANOVA, K is the number of groups being compared. Each group represents a distinct category or condition whose average outcome is being tested against the others."),

        p(strong("Why it matters")),
        p("As the number of groups increases, the test becomes more complex. With more groups, it is harder to detect differences unless the effects are large or the sample size is sufficiently high.")
      ),

      sliderInput("k_groups", "Number of groups (k)", min = 2, max = 10, value = 3, step = 1, width = "100%")
    ),
    conditionalPanel(
      condition = "input.mock_test === 'chisq'",

      tagList(
        p(strong("What it is")),
        p("Degrees of freedom describe how many independent categories or comparisons are involved in the chi-squared test. More categories mean more ways the data can vary."),

        p(strong("Why it matters")),
        p("As degrees of freedom increase, the chi-squared distribution changes. This affects how unusual a result must be before it is considered statistically significant, even if the total sample size stays the same.")
      ),

      sliderInput("df_chi", "Degrees of freedom (df)", min = 1, max = 20, value = 1, step = 1, width = "100%")
    ),

    tagList(
      p(strong("What it is")),
      p("The significance level (α) is the threshold for how much evidence is required before calling a result statistically significant. It controls how willing you are to risk a false alarm."),

      p(strong("Why it matters")),
      p("Lower α makes it harder to claim an effect (fewer false positives but more missed effects). Higher α makes it easier to detect effects but increases the chance of finding patterns that are actually due to random noise.")
    ),

    sliderInput("alpha", "Significance level (α)", min = 0.001, max = 0.2, value = 0.05, step = 0.05, width = "100%")
  ),

  # --- Top navbar tabs (these replace your old accordion “boxes”) ---
  nav_panel("Effect Size", SSP_tab_effect_size()),
  nav_panel("Sample Size", SSP_tab_sample_size()),
  nav_panel("Power", SSP_tab_power()),
  nav_panel("Trade-offs", SSP_tab_tradeoffs()),
  nav_panel(
    "Acknowledgments",
    card(
      card_body(
        p(strong("Funding and Disclaimer")),
        p("This project is supported by the Assistant Secretary for Technology Policy (ASTP) of the US Department of Health and Human Services (HHS) under grant number 90PH0005/01-13."),
        p("This information or content and conclusions are those of the authors and should not be construed as the official position or policy of, nor should any endorsements be inferred by ASTP, HHS or the U.S. Government.")
      )
    )
  )

)

server <- function(input, output, session) {

  effect_grid <- reactive({
    tt <- input$mock_test
    if (tt %in% c("two_t", "paired_t")) return(seq(0.05, 1, length.out = 200))  # d
    if (tt == "anova1")                return(seq(0.05, 1, length.out = 200)) # f
    if (tt == "chisq")                 return(seq(0.05, 1, length.out = 200)) # w
    seq(0.05, 1, length.out = 200)
  })

  effect_label <- reactive({
    tt <- input$mock_test
    if (tt %in% c("two_t", "paired_t")) "Effect size (Cohen's d)"
    if (tt == "anova1")                "Effect size (Cohen's f)"
    if (tt == "chisq")                 "Effect size (Cohen's w)"
    "Effect size"
  })

  output$var_selectorSSP <- renderUI({
    tagList(
      selectInput(
        "calc_powerSSP",
        "What do you want to solve for?",
        choices = c("Power (given n)" = "power", "Required n (given power)" = "n"),
        selected = "power"
      ),
      sliderInput(
        "effect_sizeSSP", effect_label(),
        min = min(effect_grid()), max = max(effect_grid()),
        value = median(effect_grid()),
        step = (max(effect_grid()) - min(effect_grid())) / 200,
        width = "100%"
      ),
      conditionalPanel(
        condition = "input.calc_powerSSP === 'power'",
        sliderInput(
          "sample_sizeSSP", "Sample size (per group)",
          min = 10, max = 250, value = 50, step = 5, width = "100%"
        )
      ),
      conditionalPanel(
        condition = "input.calc_powerSSP === 'n'",
        sliderInput(
          "target_powerSSP", "Target power",
          min = 0.50, max = 0.99, value = 0.80, step = 0.05, width = "100%"
        )
      )
    )
  })

  output$groupsSSP <- renderUI({
    if (input$mock_test == "anova1") {
      sliderInput("groupsSSP", "# groups (k)", min = 2, max = 10, value = input$k_groups, step = 1, width = "100%")
    } else NULL
  })

  output$df_chiSSP <- renderUI({
    if (input$mock_test == "chisq") {
      sliderInput("df_chiSSP", "Chi-square df", min = 1, max = 20, value = input$df_chi, step = 1, width = "100%")
    } else NULL
  })






  output$demo_effPowerPlotSSP <- renderPlot({
    tt <- input$mock_test
    n_fixed <- input$demo_nSSP
    req(n_fixed)

    es <- effect_grid()
    pw <- vapply(es, function(e) {
      solve_power(
        test_type = tt,
        effect    = e,
        n         = n_fixed,
        alpha     = input$alpha,
        k         = if (tt == "anova1") input$k_groups else NULL,
        df_chi    = if (tt == "chisq")  input$df_chi  else NULL
      )$power
    }, numeric(1))

    df <- data.frame(effect = es, power = pw)
    idx80 <- which.min(abs(df$power - 0.80))
    idx90 <- which.min(abs(df$power - 0.90))
    pts <- df[c(idx80, idx90), ]

    ggplot(df, aes(effect, power)) +
      geom_line(alpha = 0.75, linewidth = 0.9, colour = "#2f3e46") +
      geom_point(data = pts, aes(effect, power), colour = "#E69F00", size = 3) +
      geom_text(
        data = pts,
        aes(effect, power, label = sprintf("%.2f", power)),
        vjust = 1.3, hjust = -0.6,
        family = "Avenir Next", size = 5, colour = "#343A40"
      ) +
      labs(
        x = effect_label(), y = "Power",
        title = sprintf("Power vs Effect Size (n = %d, α = %.3f)", n_fixed, input$alpha)
      ) +
      scale_y_continuous(limits = c(0, 1), expand = c(0.001, 0.001)) +
      Plot_theme()
  })





  output$demo_ssPowerPlotSSP <- renderPlot({
    tt <- input$mock_test
    eff_fixed <- max(input$demo_effSSP, 0.01)  # guard against 0
    req(eff_fixed)

    ns <- 5:1000
    pw <- vapply(ns, function(n) {
      solve_power(
        test_type = tt,
        effect    = eff_fixed,
        n         = n,
        alpha     = input$alpha,
        k         = if (tt == "anova1") input$k_groups else NULL,
        df_chi    = if (tt == "chisq")  input$df_chi  else NULL
      )$power
    }, numeric(1))

    df <- data.frame(n = ns, power = pw)
    idx80 <- which.min(abs(df$power - 0.80))
    idx90 <- which.min(abs(df$power - 0.90))
    pts <- df[c(idx80, idx90), ]
    pts$label <- sprintf("n= %d, p= %.2f", pts$n, pts$power)

    ggplot(df, aes(n, power)) +
      geom_line() +
      geom_point(data = pts, aes(n, power), colour = "#E69F00", size = 3) +
      geom_text(data = pts, aes(n, power, label = label),
                vjust = 1.25, hjust = -0.3, family = "Avenir Next", size = 5, colour = "#343A40") +
      labs(x = "Sample size (per group)", y = "Power",
           title = sprintf("Power vs Sample Size (%s, α = %.3f)", effect_label(), input$alpha)) +
      scale_x_continuous(limits = c(5, 1000), expand = c(0.001, 0.001)) +
      scale_y_continuous(limits = c(0, 1), expand = c(0.001, 0.001)) + Plot_theme()
  })





  output$demo_effSSPlotSSP <- renderPlot({
    tt <- input$mock_test
    pow_fixed <- input$demo_powerSSP
    req(pow_fixed)

    es <- effect_grid()
    ns <- vapply(es, function(e) {
      solve_power(
        test_type = tt,
        effect    = e,
        power     = pow_fixed,
        alpha     = input$alpha,
        k         = if (tt == "anova1") input$k_groups else NULL,
        df_chi    = if (tt == "chisq")  input$df_chi  else NULL
      )$n
    }, numeric(1))

    df <- data.frame(effect = es, n = ns)

    targets <- quantile(es, probs = c(0.2, 0.5, 0.8))
    idxs <- vapply(targets, function(t) which.min(abs(df$effect - t)), integer(1))
    pts <- df[idxs, ]

    ggplot(df, aes(effect, n)) +
      geom_line(alpha = 0.75, linewidth = 0.9, colour = "#2f3e46") +
      geom_point(data = pts, aes(effect, n), colour = "#E69F00", size = 3) +
      geom_text(
        data = pts,
        aes(effect, n, label = round(n)),
        vjust = -1.25, hjust = -0.6,
        family = "Avenir Next", size = 5, colour = "#343A40"
      ) +
      labs(
        x = effect_label(), y = "Required sample size (per group)",
        title = sprintf("Required n vs Effect Size (power = %.2f, α = %.3f)", pow_fixed, input$alpha)
      ) +
      scale_y_continuous(limits = c(0, 1000), expand = c(0.001, 0.001)) +
      Plot_theme()
  })





  output$ss_power_plotSSP <- renderPlot({
    tt <- input$mock_test
    req(input$effect_sizeSSP)

    if (input$calc_powerSSP == "power") {
      req(input$sample_sizeSSP)
      res <- solve_power(
        test_type = tt,
        effect    = input$effect_sizeSSP,
        n         = input$sample_sizeSSP,
        alpha     = input$alpha,
        k         = if (tt == "anova1") input$k_groups else NULL,
        df_chi    = if (tt == "chisq")  input$df_chi  else NULL
      )
    } else {
      req(input$target_powerSSP)
      res <- solve_power(
        test_type = tt,
        effect    = input$effect_sizeSSP,
        power     = input$target_powerSSP,
        alpha     = input$alpha,
        k         = if (tt == "anova1") input$k_groups else NULL,
        df_chi    = if (tt == "chisq")  input$df_chi  else NULL
      )
    }

    axis_max <- 1000
    ns <- 5:axis_max
    pw <- vapply(ns, function(n) {
      solve_power(
        test_type = tt,
        effect    = input$effect_sizeSSP,
        n         = n,
        alpha     = input$alpha,
        k         = if (tt == "anova1") input$k_groups else NULL,
        df_chi    = if (tt == "chisq")  input$df_chi  else NULL
      )$power
    }, numeric(1))

    df <- data.frame(n = ns, power = pw)

    g <- ggplot(df, aes(n, power)) +
      geom_line() +
      labs(x = "Sample size (per group)", y = "Power", title = "Power curve") +
      scale_y_continuous(limits = c(0, 1), expand = c(0.001, 0.001)) +
      scale_x_continuous(limits = c(5, axis_max), expand = c(0.001, 0.001)) + Plot_theme()

    if (!is.null(res$n) && is.finite(res$n) && res$n <= axis_max) {
      pt <- data.frame(n = res$n, power = res$power)
      lbl <- paste0(
        effect_label(), ": ", sprintf("%.3f", input$effect_sizeSSP), "\n",
        " α: ", sprintf("%.3f", input$alpha), "\n",
        "  n: ", res$n, "\n",
        " Power: ", sprintf("%.2f", res$power)
      )

      g +
        geom_point(data = pt, aes(n, power), colour = "#E69F00", size = 4) +
        geom_label(data = pt, aes(n, power, label = lbl),
                   vjust = 1.2, hjust = -0.05,
                   family = "Avenir Next", size = 5,
                   colour = "#343A40", fill = "#FCFDFF",
                   label.size = 0.2)
    } else {
      g +
        annotate("label", x = axis_max * 0.65, y = 0.25,
                 label = paste0("Required n ≈ ", res$n, " (off-plot)\nTry a larger effect or lower target power."),
                 family = "Avenir Next", size = 5,
                 colour = "#343A40", fill = "#FCFDFF",
                 label.size = 0.2)
    }
  })
}

shinyApp(ui, server)
