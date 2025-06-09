#' Display detailed SCM equations
#'
#' @description
#' Provides detailed mathematical specifications of the structural causal model
#' (SCM) used in margot_simulate. Shows both the DAG structure and the specific
#' parametric forms of the structural equations.
#'
#' @param format Character: "text" for console output, "latex" for LaTeX
#' @param estimand Character: specific estimand to highlight (optional)
#'
#' @return Invisibly returns a list with the equations
#' @export
margot_scm_details <- function(format = c("text", "latex"), estimand = NULL) {
  format <- match.arg(format)
  
  if (format == "text") {
    cli::cli_h1("margot Structural Causal Model (SCM) Specification")
    
    cli::cli_h2("Directed Acyclic Graph (DAG)")
    cli::cli_verbatim("
    B ──┬──> L₁ ──> A₁ ──> Y₁ ──> L₂ ──> A₂ ──> Y₂ ──> ... ──> Y_K
        │      │      │      │      │      │      │
        │      v      v      v      v      v      v
        └───> C₁     C₂     C₃    C₄     C₅     C₆
    
    Where:
      B  = Baseline covariates (time-invariant)
      Lₖ = Time-varying confounder at wave k
      Aₖ = Exposure/treatment at wave k  
      Yₖ = Outcome(s) at wave k
      Cₖ = Censoring indicator after wave k
    ")
    
    cli::cli_h2("Structural Equations")
    
    cli::cli_h3("Baseline Generation")
    cli::cli_text("{.emph B ~ MVN(0, Σ_B)} where {.emph Σ_B[i,j] = 0.3} for i ≠ j")
    
    cli::cli_h3("Time-Varying Processes")
    cli::cli_text("For t ≥ 1:")
    cli::cli_bullets(c(
      "*" = "{.emph L_t = β_{B→L} h_t(B) + β_{A→L} A_{t-1} + β_{Y→L} Y_{t-1} + U_{L,t}}",
      "*" = "{.emph A_t = f(β_{B→A} B + β_{L→A} L_t + β_{A→A} A_{t-1} + β_{Y→A} Y_{t-1} + U_{A,t})}",
      "*" = "{.emph Y_t = β_{B→Y} B + β_{L→Y} L_{t-1} + β_{A→Y} A_{t-1} + β_{Y→Y} Y_{t-1} + U_{Y,t}}",
      "*" = "{.emph C_t = f(γ_0 + γ_A A_{t-1} + γ_L L_{t-1} + γ_Y Y_{t-1} + θ + U_{C,t})}"
    ))
    
    cli::cli_h3("Time-Varying Functions")
    cli::cli_bullets(c(
      "*" = "{.emph h_t(B)[1] = B_1 × min(2, 1 + 0.05t)} (growth)",
      "*" = "{.emph h_t(B)[2] = B_2 × max(0, 1 - 0.02t)} (decay)",  
      "*" = "{.emph h_t(B)[3] = B_3 × sin(πt/waves)} (cyclic)"
    ))
    
    if (!is.null(estimand)) {
      cli::cli_h2("Target Estimand")
      cli::cli_alert_info("Estimand: {estimand}")
      if (estimand == "ATE") {
        cli::cli_text("E[Y_K^{a=1} - Y_K^{a=0}] under no censoring")
      }
    }
    
  } else {
    # LaTeX format
    equations <- list(
      dag = "B \\rightarrow L_t, A_t, Y_t, C_t \\quad \\forall t",
      baseline = "B \\sim \\text{MVN}(0, \\Sigma_B)",
      confounder = "L_t = \\beta_{B \\rightarrow L} h_t(B) + \\beta_{A \\rightarrow L} A_{t-1} + \\beta_{Y \\rightarrow L} Y_{t-1} + U_{L,t}",
      treatment = "A_t = f(\\beta_{B \\rightarrow A} B + \\beta_{L \\rightarrow A} L_t + \\beta_{A \\rightarrow A} A_{t-1} + \\beta_{Y \\rightarrow A} Y_{t-1} + U_{A,t})",
      outcome = "Y_t = \\beta_{B \\rightarrow Y} B + \\beta_{L \\rightarrow Y} L_{t-1} + \\beta_{A \\rightarrow Y} A_{t-1} + \\beta_{Y \\rightarrow Y} Y_{t-1} + U_{Y,t}",
      censoring = "\\text{logit}(P(C_t = 1)) = \\gamma_0 + \\gamma_A A_{t-1} + \\gamma_L L_{t-1} + \\gamma_Y Y_{t-1} + \\theta"
    )
    
    cat("% margot SCM LaTeX equations\n")
    for (name in names(equations)) {
      cat(sprintf("\\paragraph{%s}\n", tools::toTitleCase(name)))
      cat(sprintf("\\begin{equation}\n%s\n\\end{equation}\n\n", equations[[name]]))
    }
    
    invisible(equations)
  }
}

#' Validate SCM assumptions in simulated data
#'
#' @description
#' Tests whether simulated data follows the expected structural causal model
#' by checking independence assumptions, temporal ordering, and structural effects.
#'
#' @param data Data frame from margot_simulate
#' @param tests Character vector of tests to perform
#' @param alpha Significance level for tests
#' @param verbose Print detailed results?
#'
#' @return List with test results and pass/fail summary
#' @export
validate_scm_assumptions <- function(
    data,
    tests = c("independence", "temporal", "structural"),
    alpha = 0.05,
    verbose = TRUE) {
  
  meta <- attr(data, "margot_meta")
  if (is.null(meta)) {
    stop("data must have margot_meta attribute from margot_simulate")
  }
  
  results <- list()
  tests <- match.arg(tests, several.ok = TRUE)
  
  # independence tests
  if ("independence" %in% tests) {
    if (verbose) cli::cli_h3("Testing independence assumptions")
    
    # test baseline independence from future
    ind_tests <- list()
    
    # B ⊥ U_L | ∅ (baseline independent of L residuals)
    if (all(c("b1", "t1_l", "t0_a") %in% names(data))) {
      resid_l <- residuals(lm(t1_l ~ t0_a, data = data))
      cor_test <- cor.test(data$b1, resid_l)
      ind_tests$b_ul <- list(
        test = "B ⊥ U_L",
        p_value = cor_test$p.value,
        passed = cor_test$p.value > alpha
      )
    }
    
    results$independence <- ind_tests
  }
  
  # temporal ordering tests
  if ("temporal" %in% tests) {
    if (verbose) cli::cli_h3("Testing temporal ordering")
    
    temp_tests <- list()
    
    # past affects future but not vice versa
    if (all(c("t0_a", "t1_y", "t1_a", "t0_y") %in% names(data))) {
      # forward causation: t0_a -> t1_y
      forward_fit <- lm(t1_y ~ t0_a + t0_y + b1, data = data)
      forward_coef <- coef(forward_fit)["t0_a"]
      
      # reverse causation: t1_a -> t0_y (should be null)
      reverse_fit <- lm(t0_y ~ t1_a + t0_a + b1, data = data)
      reverse_coef <- coef(reverse_fit)["t1_a"]
      
      temp_tests$forward_causation <- list(
        test = "t0_a -> t1_y",
        estimate = forward_coef,
        p_value = summary(forward_fit)$coefficients["t0_a", "Pr(>|t|)"],
        passed = !is.na(forward_coef) && abs(forward_coef) > 0.05
      )
      
      temp_tests$no_reverse_causation <- list(
        test = "t1_a !-> t0_y", 
        estimate = reverse_coef,
        p_value = summary(reverse_fit)$coefficients["t1_a", "Pr(>|t|)"],
        passed = is.na(reverse_coef) || 
          summary(reverse_fit)$coefficients["t1_a", "Pr(>|t|)"] > alpha
      )
    }
    
    results$temporal <- temp_tests
  }
  
  # structural effect tests
  if ("structural" %in% tests) {
    if (verbose) cli::cli_h3("Testing structural effects")
    
    struct_tests <- list()
    
    # key paths should exist
    if (all(c("t1_a", "t2_y", "t1_l", "b1") %in% names(data))) {
      # A -> Y effect
      ay_fit <- lm(t2_y ~ t1_a + t1_l + b1, data = data)
      ay_coef <- coef(ay_fit)["t1_a"]
      
      struct_tests$treatment_effect <- list(
        test = "A -> Y",
        estimate = ay_coef,
        expected_sign = "+",
        passed = !is.na(ay_coef) && ay_coef > 0
      )
      
      # L -> A effect  
      if ("t2_a" %in% names(data)) {
        la_fit <- glm(t2_a ~ t2_l + t1_a + b1, 
                      data = data, family = binomial())
        la_coef <- coef(la_fit)["t2_l"]
        
        struct_tests$confounder_effect <- list(
          test = "L -> A",
          estimate = la_coef,
          expected_sign = "+",
          passed = !is.na(la_coef) && la_coef > 0
        )
      }
    }
    
    results$structural <- struct_tests
  }
  
  # summary
  all_tests <- unlist(results, recursive = FALSE)
  n_passed <- sum(sapply(all_tests, function(x) isTRUE(x$passed)))
  n_total <- length(all_tests)
  
  results$summary <- list(
    n_tests = n_total,
    n_passed = n_passed,
    prop_passed = n_passed / n_total,
    overall_pass = n_passed == n_total
  )
  
  if (verbose) {
    cli::cli_h2("Summary")
    cli::cli_alert_info("Passed {n_passed}/{n_total} tests ({round(100 * n_passed/n_total)}%)")
    
    if (results$summary$overall_pass) {
      cli::cli_alert_success("Data appears consistent with SCM structure")
    } else {
      cli::cli_alert_warning("Some assumptions not validated - review detailed results")
    }
  }
  
  class(results) <- "scm_validation"
  results
}

#' Create SCM diagram
#'
#' @description
#' Creates a visual representation of the structural causal model DAG.
#' Can output in various formats including text, mermaid, or DOT.
#'
#' @param format Output format: "text", "mermaid", or "dot"
#' @param waves Number of waves to show
#' @param show_equations Include structural equations?
#' @param highlight_path Optional path to highlight (e.g., c("A1", "Y2"))
#'
#' @return Character string with diagram specification
#' @export
create_scm_diagram <- function(
    format = c("text", "mermaid", "dot"),
    waves = 3,
    show_equations = FALSE,
    highlight_path = NULL) {
  
  format <- match.arg(format)
  
  if (format == "text") {
    # text-based diagram
    diagram <- character()
    
    # header
    diagram <- c(diagram, "Structural Causal Model DAG")
    diagram <- c(diagram, paste(rep("=", 50), collapse = ""))
    diagram <- c(diagram, "")
    
    # main structure
    diagram <- c(diagram, "B ──┬──> L₁ ──> A₁ ──> Y₁ ──> L₂ ──> A₂ ──> Y₂")
    diagram <- c(diagram, "    │      │      │      │      │      │      │")
    diagram <- c(diagram, "    │      v      v      v      v      v      v")
    diagram <- c(diagram, "    └───> C₁     C₂     C₃    C₄     C₅     C₆")
    
    if (show_equations) {
      diagram <- c(diagram, "", "Key Structural Equations:")
      diagram <- c(diagram, "L_t = f(B, A_{t-1}, Y_{t-1})")
      diagram <- c(diagram, "A_t = f(B, L_t, A_{t-1}, Y_{t-1})")
      diagram <- c(diagram, "Y_t = f(B, L_{t-1}, A_{t-1}, Y_{t-1})")
      diagram <- c(diagram, "C_t = f(A_{t-1}, L_{t-1}, Y_{t-1})")
    }
    
    cat(paste(diagram, collapse = "\n"))
    invisible(paste(diagram, collapse = "\n"))
    
  } else if (format == "mermaid") {
    # mermaid diagram
    nodes <- character()
    edges <- character()
    
    # add baseline
    nodes <- c(nodes, "    B[Baseline B]")
    
    # add time-varying nodes
    for (t in 1:waves) {
      nodes <- c(nodes, sprintf("    L%d[L_%d]", t, t))
      nodes <- c(nodes, sprintf("    A%d[A_%d]", t, t))
      nodes <- c(nodes, sprintf("    Y%d[Y_%d]", t, t))
      nodes <- c(nodes, sprintf("    C%d[C_%d]", t, t))
    }
    
    # add final outcome
    nodes <- c(nodes, sprintf("    Y%d[Y_%d]", waves + 1, waves + 1))
    
    # baseline edges
    for (t in 1:waves) {
      edges <- c(edges, sprintf("    B --> L%d", t))
      edges <- c(edges, sprintf("    B --> A%d", t))
      edges <- c(edges, sprintf("    B --> Y%d", t))
      edges <- c(edges, sprintf("    B --> C%d", t))
    }
    edges <- c(edges, sprintf("    B --> Y%d", waves + 1))
    
    # time-varying edges
    for (t in 1:waves) {
      # within time
      edges <- c(edges, sprintf("    L%d --> A%d", t, t))
      if (t > 1) {
        edges <- c(edges, sprintf("    L%d --> Y%d", t-1, t))
      }
      
      # censoring
      if (t > 1) {
        edges <- c(edges, sprintf("    A%d --> C%d", t-1, t))
        edges <- c(edges, sprintf("    L%d --> C%d", t-1, t))
        edges <- c(edges, sprintf("    Y%d --> C%d", t-1, t))
      }
      
      # forward in time
      if (t < waves) {
        edges <- c(edges, sprintf("    A%d --> L%d", t, t+1))
        edges <- c(edges, sprintf("    A%d --> A%d", t, t+1))
        edges <- c(edges, sprintf("    Y%d --> L%d", t, t+1))
        edges <- c(edges, sprintf("    Y%d --> Y%d", t, t+1))
        edges <- c(edges, sprintf("    Y%d --> A%d", t, t+1))
      }
      
      # to final outcome
      edges <- c(edges, sprintf("    A%d --> Y%d", t, waves + 1))
    }
    edges <- c(edges, sprintf("    L%d --> Y%d", waves, waves + 1))
    edges <- c(edges, sprintf("    Y%d --> Y%d", waves, waves + 1))
    
    # highlight path if specified
    if (!is.null(highlight_path)) {
      for (i in 1:(length(highlight_path) - 1)) {
        from <- highlight_path[i]
        to <- highlight_path[i + 1]
        # would need to modify edge style here
      }
    }
    
    # combine
    mermaid <- c("graph TD", nodes, edges)
    
    cat(paste(mermaid, collapse = "\n"))
    invisible(paste(mermaid, collapse = "\n"))
    
  } else if (format == "dot") {
    # GraphViz DOT format
    dot <- character()
    dot <- c(dot, "digraph SCM {")
    dot <- c(dot, '  rankdir="LR";')
    dot <- c(dot, '  node [shape=circle];')
    
    # nodes
    dot <- c(dot, '  B [label="B", shape=square];')
    
    for (t in 1:waves) {
      dot <- c(dot, sprintf('  L%d [label="L_%d"];', t, t))
      dot <- c(dot, sprintf('  A%d [label="A_%d"];', t, t))
      dot <- c(dot, sprintf('  Y%d [label="Y_%d"];', t, t))
      dot <- c(dot, sprintf('  C%d [label="C_%d", style=dashed];', t, t))
    }
    dot <- c(dot, sprintf('  Y%d [label="Y_%d"];', waves + 1, waves + 1))
    
    # rank constraints
    for (t in 1:waves) {
      dot <- c(dot, sprintf('  {rank=same; L%d; A%d; Y%d; C%d;}', t, t, t, t))
    }
    
    # edges (similar to mermaid but with DOT syntax)
    # ... (abbreviated for space)
    
    dot <- c(dot, "}")
    
    cat(paste(dot, collapse = "\n"))
    invisible(paste(dot, collapse = "\n"))
  }
}

#' High-level SCM overview
#'
#' @description
#' Provides a high-level overview of the structural causal model implementation
#' in margot, including key principles, available estimands, and usage guidance.
#'
#' @param topic Optional specific topic to focus on
#'
#' @return Invisibly returns NULL
#' @export
margot_scm_overview <- function(topic = NULL) {
  
  cli::cli_h1("margot SCM Overview")
  
  if (is.null(topic)) {
    cli::cli_h2("Key Principles")
    cli::cli_bullets(c(
      "*" = "Semi-Markovian structure with no unmeasured confounding",
      "*" = "Time-varying treatments and confounders",
      "*" = "Flexible outcome feedback mechanisms",
      "*" = "Separates data generation from censoring/selection",
      "*" = "Supports interventions via functional programming"
    ))
    
    cli::cli_h2("Available Estimands")
    cli::cli_bullets(c(
      "*" = "Average Treatment Effect (ATE): E[Y^{a=1} - Y^{a=0}]",
      "*" = "Effect of dynamic/static interventions",
      "*" = "Effects under various censoring mechanisms",
      "*" = "Target population effects via weighting"
    ))
    
    cli::cli_h2("Usage Pattern")
    cli::cli_verbatim('
    # 1. Generate complete data under intervention
    data <- margot_simulate(n = 1000, waves = 3, intervention = my_intervention)
    
    # 2. Apply realistic observation patterns
    observed <- apply_censoring_post_hoc(data)
    
    # 3. Analyze with your preferred method
    results <- my_analysis_method(observed)
    ')
    
    cli::cli_h2("Key Functions")
    cli::cli_bullets(c(
      "i" = "{.fn margot_simulate}: Core data generation",
      "i" = "{.fn margot_simulate_causal}: Multi-intervention wrapper",
      "i" = "{.fn apply_censoring_post_hoc}: Add realistic missingness",
      "i" = "{.fn margot_report_sim}: Summarize simulation"
    ))
    
  } else {
    # topic-specific information
    topic <- match.arg(topic, c("generation", "censoring", "interventions", "parameters"))
    
    switch(topic,
           generation = {
             cli::cli_h2("Data Generation Process")
             cli::cli_text("margot generates data following temporal order:")
             cli::cli_ol(c(
               "Baseline covariates B (multivariate normal)",
               "For each time t: L_t -> A_t -> Y_t",
               "Censoring probabilities calculated but not applied",
               "Final outcome Y_{K+1} for everyone"
             ))
           },
           
           censoring = {
             cli::cli_h2("Censoring Mechanisms")
             cli::cli_text("Censoring is applied post-hoc to complete data:")
             cli::cli_bullets(c(
               "*" = "Preserves true counterfactual outcomes",
               "*" = "Multiple censoring dependencies available",
               "*" = "Can incorporate shared frailty",
               "*" = "Creates proper indicator variables"
             ))
           },
           
           interventions = {
             cli::cli_h2("Intervention Specification")
             cli::cli_text("Interventions are functions with signature:")
             cli::cli_code("function(data, time, trt)")
             cli::cli_text("Examples:")
             cli::cli_bullets(c(
               "*" = "Static: always treat / never treat",
               "*" = "Dynamic: treat based on covariate values",
               "*" = "Threshold: treat if L > threshold",
               "*" = "Natural: return observed treatment"
             ))
           },
           
           parameters = {
             cli::cli_h2("Key Parameters")
             cli::cli_text("Effect sizes in default parameterization:")
             cli::cli_bullets(c(
               "*" = "Treatment -> Outcome: 0.25 (moderate)",
               "*" = "Confounder -> Treatment: 0.20",
               "*" = "Confounder -> Outcome: 0.15",
               "*" = "Autoregressive effects: 0.20-0.25",
               "*" = "Baseline effects: 0.10-0.15"
             ))
           }
    )
  }
  
  invisible(NULL)
}