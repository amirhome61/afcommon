#' Interpret Structural Equation Model Results
#'
#' @description
#' Generates a narrative interpretation of structural equation model (SEM) results by
#' extracting and summarizing key fit indices (CFI, TLI, RMSEA, SRMR) and standardized
#' regression path coefficients. Identifies the strongest predictor and evaluates overall
#' model fit quality. Prints formatted interpretation text to console.
#'
#' @param model (lavaan) A fitted lavaan structural equation model object
#'
#' @return NULL (prints interpretation text to console via cat)
#'
#' @import lavaan
#'
#' @export
af_sem_interpretation <- function(model) {
  # Extract key fit measures
  cfi <- lavaan::fitMeasures(model, "cfi")
  tli <- lavaan::fitMeasures(model, "tli")
  rmsea <- lavaan::fitMeasures(model, "rmsea")
  srmr <- lavaan::fitMeasures(model, "srmr")

  # Extract standardized parameter estimates for regression paths
  params <- lavaan::parameterEstimates(model, standardized = TRUE)
  regressions <- params[params$op == "~", ]

  # Identify strongest predictor based on standardized effect size
  strongest_predictor <- regressions[which.max(abs(regressions$std.all)), ]

  # Construct dynamic interpretation for each predictor
  predictor_interpretations <- lapply(1:nrow(regressions), function(i) {
    path <- regressions[i, ]
    effect_direction <- ifelse(path$est > 0, "increases", "decreases")
    paste0(
      path$rhs,
      " (",
      round(path$std.all, 3),
      ") ",
      effect_direction,
      " ",
      path$lhs,
      " by ",
      round(path$est, 3)
    )
  })

  # Assemble summary interpretation
  interpretation <- paste0(
    "\n\nIn summary, ",
    strongest_predictor$rhs,
    " (",
    round(strongest_predictor$std.all, 3),
    ") is the strongest predictor in this model, ",
    ifelse(strongest_predictor$est > 0, "increasing", "decreasing"),
    " ",
    strongest_predictor$lhs,
    ". ",
    paste(predictor_interpretations, collapse = "; "),
    ". ",
    "The model fit remains ",
    ifelse(
      cfi >= 0.90 & tli >= 0.90 & rmsea <= 0.08 & srmr <= 0.08,
      "acceptable",
      "poor"
    ),
    ", indicating a need for further refinement to better capture the data's structure. "
  )

  cat(interpretation, "\n")
}
