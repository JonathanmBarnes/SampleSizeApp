# ---- shared constant (optional) ----
SSP_VH <- "calc(100vh - 120px)"  # adjust if navbar/header height differs


SSP_tab_effect_size <- function() {
  layout_columns(
    col_widths = c(4, 8),

    # Left: scrollable text + slider
    card(
      height = SSP_VH,
      card_header("Effect Size"),
      card_body(
        div(
          style = paste0(
            "overflow-y:auto;",
            "max-height:calc(100vh - 200px);",
            "padding-right:8px;"
          ),

          tagList(
            p(strong("What it is")),
            p("Effect size describes how large a real difference or relationship actually is. It is about magnitude, not just whether a result is statistically significant."),
            p("Because it is about size, effect size helps answer a practical question: even if something is real, is it big enough to matter in the real world?"),

            p(strong("Why it matters")),
            p("Large effects are easier to detect, even with small samples. Small effects can be real but difficult to observe unless a study has enough data."),
            p("A helpful intuition is “signal strength”: bigger effects are a louder signal, and power analysis asks how much listening (data) you need to reliably hear it through noise."),

            p(strong("What this visualization shows")),
            p("The curve shows how the probability of detecting an effect changes as the effect becomes larger, assuming a fixed sample size."),
            p("As effect size increases, the curve rises because the signal becomes easier to distinguish from random variation."),

            p(strong("Common pitfalls")),
            p("Effect size is often confused with statistical significance. A tiny effect can be statistically significant in a very large study, while a meaningful effect can fail to reach significance in a small study."),
            p("When planning, people often assume effects will be larger than they really are, which can lead to studies that are underpowered for the true (smaller) effect.")
          ),

          hr(),

          sliderInput(
            "demo_nSSP", "Sample size (n):",
            min = 10, max = 300, value = 50, step = 10,
            width = "100%", animate = TRUE
          )
        )
      )
    ),

    # Right: plot fills
    card(
      height = SSP_VH,
      full_screen = TRUE,
      card_header("Effect Size → Power"),
      card_body_fill(
        plotOutput("demo_effPowerPlotSSP", height = "100%")
      )
    )
  )
}


SSP_tab_sample_size <- function() {
  layout_columns(
    col_widths = c(4, 8),

    card(
      height = SSP_VH,
      card_header("Sample Size"),
      card_body(
        div(
          style = paste0(
            "overflow-y:auto;",
            "max-height:calc(100vh - 200px);",
            "padding-right:8px;"
          ),

          tagList(
            p(strong("What it is")),
            p("Sample size is the number of observations or participants in a study. It determines how much information is available to detect real patterns rather than random noise."),
            p("In general, larger samples also produce more precise estimates (narrower uncertainty), not just a higher chance of detecting an effect."),

            p(strong("Why it matters")),
            p("With too little data, real effects are often missed and estimates become unstable. With more data, patterns become clearer and uncertainty decreases."),
            p("But “more data” does not automatically mean “more useful insight”: if measurement is noisy or biased, adding more observations can simply give a very confident estimate of the wrong thing."),

            p(strong("What this visualization shows")),
            p("The curve shows how the chance of detecting an effect changes as the sample size increases, for a fixed effect size."),
            p("The curve typically increases quickly at first and then levels off, reflecting diminishing returns: early increases in sample size help a lot, later increases help less."),

            p(strong("Common pitfalls")),
            p("There is no universally correct sample size. Choosing sample sizes based on convenience rather than analytical needs can lead to inconclusive or misleading results."),
            p("Very large samples can make trivial effects appear statistically significant, so it is important to interpret results in terms of practical impact, not just p-values.")
          ),

          hr(),

          sliderInput(
            "demo_effSSP", "Effect size (d / r² / f / w):",
            min = 0.1, max = 1.5, value = 0.2, step = 0.1,
            width = "100%", animate = TRUE
          )
        )
      )
    ),

    card(
      height = SSP_VH,
      full_screen = TRUE,
      card_header("Sample Size → Power"),
      card_body_fill(
        plotOutput("demo_ssPowerPlotSSP", height = "100%")
      )
    )
  )
}


SSP_tab_power <- function() {
  layout_columns(
    col_widths = c(4, 8),

    card(
      height = SSP_VH,
      card_header("Statistical Power"),
      card_body(
        div(
          style = paste0(
            "overflow-y:auto;",
            "max-height:calc(100vh - 200px);",
            "padding-right:8px;"
          ),

          tagList(
            p(strong("What it is")),
            p("Statistical power is the probability that a study will detect a real effect if it exists. For example, 80% power means the study would succeed about 8 times out of 10."),
            p("Equivalently, power is how often you avoid a false negative (missing a real effect)."),

            p(strong("Why it matters")),
            p("Low-powered studies frequently miss real effects and produce unstable findings. Power reflects how capable a study is of answering its research question."),
            p("Power depends on several linked choices: how big an effect you care about, how much data you can collect, how noisy the measurements are, and how strict you are about false alarms (the significance level)."),

            p(strong("What this visualization shows")),
            p("The curve shows how the required sample size changes as the effect size changes, assuming a chosen power target."),
            p("Smaller effects require larger samples to reach the same power, because subtle signals are harder to distinguish from random variation."),

            p(strong("Common pitfalls")),
            p("Non-significant results are often interpreted as evidence of no effect, even when a study simply lacks enough data to detect it."),
            p("Post-hoc (after-the-fact) “power” calculations are commonly misused; they do not rescue an inconclusive study and often restate what the p-value already implies.")
          ),

          hr(),

          sliderInput(
            "demo_powerSSP", "Target power:",
            min = 0.5, max = 0.99, value = 0.8, step = 0.01,
            width = "100%", animate = TRUE
          )
        )
      )
    ),

    card(
      height = SSP_VH,
      full_screen = TRUE,
      card_header("Effect Size → Required Sample Size"),
      card_body_fill(
        plotOutput("demo_effSSPlotSSP", height = "100%")
      )
    )
  )
}


SSP_tab_tradeoffs <- function() {
  layout_columns(
    col_widths = c(4, 8),

    card(
      height = SSP_VH,
      card_header("Assumptions & Constraints"),
      card_body(
        div(
          style = paste0(
            "overflow-y:auto;",
            "max-height:calc(100vh - 200px);",
            "padding-right:8px;"
          ),

          tagList(
            p(strong("Study design is a set of choices under constraints.")),
            p("Power analysis is often taught as a mathematical relationship, but in practice it functions more like budgeting: you are deciding what level of evidence you can afford given limits in time, money, staffing, recruitment, and ethical burden on participants."),

            p(strong("The real constraint is usually resources.")),
            p("Funding and feasibility often dominate design decisions. Larger samples cost more to recruit, measure, and manage; longer follow-up increases attrition; tighter measurement protocols improve signal but increase burden; and multi-site or multi-group designs add complexity even when the math looks simple."),
            p("A hidden lever is measurement quality: clearer, more reliable measurement reduces noise and can dramatically reduce the sample size needed for the same power."),

            p(strong("What this visualization is meant to make visible.")),
            p("This tool shows how changing assumptions (effect size you care about, desired power, and significance level) forces changes in required sample size. The point is not to produce a single ‘correct’ number, but to reveal the trade-offs and to help you justify a design in plain language."),
            p("Statistical significance is often mistaken for practical importance. Small effects can be statistically significant in large studies, while meaningful effects can go unnoticed in small studies."),
            p("A useful way to interpret the trade-off is to ask: which mistake is more costly here—missing a real effect (false negative) or claiming an effect that is not real (false positive)? That choice should influence your power target and significance threshold.")
          ),

          hr(),


          # Your controls first (so people see knobs before prose)
          uiOutput("var_selectorSSP"),
          uiOutput("groupsSSP"),
          uiOutput("df_chiSSP"),
        )
      )
    ),

    card(
      height = SSP_VH,
      full_screen = TRUE,
      card_header("Power Curve"),
      card_body_fill(
        plotOutput("ss_power_plotSSP", height = "100%")
      )
    )
  )
}



classify_test <- function(data, outcome_var, predictor_var, paired = FALSE) {
  # Guard clauses: make sure the names exist
  if (is.null(outcome_var) || is.null(predictor_var) ||
      !outcome_var %in% names(data) || !predictor_var %in% names(data)) {
    return("unsupported")
  }

  x <- data[[outcome_var]]
  y <- data[[predictor_var]]

  ## 1) explicit paired design takes priority
  if (paired && is.numeric(x) && is.numeric(y))
    return("paired_t")

  ## 2) simple linear regression (both numeric, not paired)
  if (is.numeric(x) && is.numeric(y))
    return("slr")

  ## 3) two‑sample t (numeric outcome, binary predictor)
  if (is.numeric(x) && is.factor(y) && nlevels(y) == 2)
    return("two_t")

  ## 4) one‑way ANOVA (numeric outcome, >2‑level factor)
  if (is.numeric(x) && is.factor(y) && nlevels(y) > 2)
    return("anova1")

  ## 5) χ² test of independence (both factors)
  if (is.factor(x) && is.factor(y))
    return("chisq")

  ## fallback
  "unsupported"
}



solve_power <- function(test_type = c("paired_t", "two_t", "anova1",
                                      "slr", "chisq"),
                        effect,          # Cohen dz / d / f / w  OR r2 for slr
                        n      = NULL,   # supply *either* n *or* power
                        power  = NULL,   # supply *either* power *or* n
                        k      = NULL,   # # groups for anova1
                        df_chi = NULL,   # df for chisq
                        alpha = 0.05) {

  test_type <- match.arg(test_type)

  have_n     <- !is.null(n)
  have_power <- !is.null(power)

  if (have_n == have_power)
    stop("Supply *exactly one* of 'n' or 'power'.")

  # ------------------------------------------------------------------------
  # internal helper to make results uniform
  fmt_res <- function(out_n, out_power, note = "") {
    list(
      test_type = test_type,
      n         = unname(out_n),
      power     = unname(out_power),
      note      = note
    )
  }

  # ========================================================================
  # Paired‑samples t
  # ========================================================================
  if (test_type == "paired_t") {
    if (have_n) {
      pwr <- power.t.test(d = effect, n = n,
                          sig.level = alpha,
                          type = "paired")$power
      return(fmt_res(out_n = n, out_power = pwr,
                     note = "n = # subject pairs"))
    } else {               # solve for n
      out <- power.t.test(d = effect, power = power,
                          sig.level = alpha,
                          type = "paired")
      return(fmt_res(out_n = ceiling(out$n), out_power = power,
                     note = "n = # subject pairs"))
    }
  }

  # ========================================================================
  # Two‑sample t (equal group sizes)
  # ========================================================================
  if (test_type == "two_t") {
    if (have_n) {
      pwr <- power.t.test(d = effect, n = n,
                          sig.level = alpha,
                          type = "two.sample")$power
      return(fmt_res(out_n = n, out_power = pwr,
                     note = "n = subjects per group"))
    } else {
      out <- power.t.test(d = effect, power = power,
                          sig.level = alpha,
                          type = "two.sample")
      return(fmt_res(out_n = ceiling(out$n), out_power = power,
                     note = "n = subjects per group"))
    }
  }

  # ========================================================================
  # One‑way ANOVA (balanced)
  # ========================================================================
  if (test_type == "anova1") {
    if (is.null(k) || k < 2)
      stop("anova1: argument 'k' (# groups) must be >= 2")

    if (have_n) {
      pwr <- pwr.anova.test(k = k, f = effect, n = n, sig.level = alpha)$power
      return(fmt_res(out_n = n, out_power = pwr,
                     note = "n = subjects per group"))
    } else {
      out <- pwr.anova.test(k = k, f = effect, power = power, sig.level = alpha)
      return(fmt_res(out_n = ceiling(out$n), out_power = power,
                     note = "n = subjects per group"))
    }
  }

  # ========================================================================
  # Simple linear regression (one predictor)
  # effect is supplied as R² for transparency to users
  # ========================================================================
  if (test_type == "slr") {
    if (effect <= 0 || effect >= 1)
      stop("slr: 'effect' must be an anticipated R² in (0, 1).")
    f2 <- effect / (1 - effect)      # convert R² → f²
    u  <- 1                          # predictors

    if (have_n) {
      v <- n - u - 1
      if (v <= 0) stop("n too small for simple regression: need n > 2.")
      pwr <- pwr.f2.test(u = u, v = v, f2 = f2,
                         sig.level = alpha)$power
      return(fmt_res(out_n = n, out_power = pwr,
                     note = "n = total subjects"))
    } else {
      out <- pwr.f2.test(u = u, f2 = f2, power = power,
                         sig.level = alpha)
      n_tot <- ceiling(out$v + u + 1)
      return(fmt_res(out_n = n_tot, out_power = power,
                     note = "n = total subjects"))
    }
  }

  # ========================================================================
  # Chi‑square test of independence / goodness‑of‑fit
  # ========================================================================
  if (test_type == "chisq") {
    if (is.null(df_chi) || df_chi < 1)
      stop("chisq: argument 'df_chi' (# degrees of freedom) must be >= 1")

    if (have_n) {
      pwr <- pwr.chisq.test(w = effect, N = n, df = df_chi,
                            sig.level = alpha)$power
      return(fmt_res(out_n = n, out_power = pwr,
                     note = "n = total subjects"))
    } else {
      out <- pwr.chisq.test(w = effect, power = power, df = df_chi,
                            sig.level = alpha)
      return(fmt_res(out_n = ceiling(out$N), out_power = power,
                     note = "n = total subjects"))
    }
  }

  stop("Unknown test_type.")
}
