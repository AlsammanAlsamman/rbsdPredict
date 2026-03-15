#' Summarize Cluster-Specific Regression Equations
#'
#' Converts hybrid-model cluster regressions into a compact table of equations
#' for interpretability and reporting.
#'
#' @param model A trained object from train_hybrid_model()
#' @param digits Number of digits used when formatting coefficients
#' @return A data frame with cluster, target, sample size, and equation text
#' @examples
#' data(rbsd_data)
#' hybrid <- train_hybrid_model(rbsd_data)
#' summarize_cluster_equations(hybrid)
#' @export
summarize_cluster_equations <- function(model, digits = 3) {
  if (!inherits(model, "rbsd_hybrid_model")) {
    stop("model must be of class 'rbsd_hybrid_model'")
  }

  cluster_sizes <- table(model$cluster_result$clusters)
  rows <- list()

  format_equation <- function(fit) {
    coefs <- stats::coef(fit)
    if (is.null(coefs) || any(is.na(coefs))) {
      return(NA_character_)
    }

    paste0(
      names(coefs)[1], " = ",
      round(coefs[1], digits),
      " + ", round(coefs[2], digits), " * tmax",
      " + ", round(coefs[3], digits), " * tmin",
      " + ", round(coefs[4], digits), " * RH"
    )
  }

  for (cluster_id in sort(unique(as.integer(model$cluster_result$clusters)))) {
    key <- as.character(cluster_id)
    n_obs <- unname(cluster_sizes[key])

    if (!is.null(model$models_A[[key]])) {
      rows[[length(rows) + 1]] <- data.frame(
        Cluster = cluster_id,
        Target = "A",
        N = n_obs,
        Equation = format_equation(model$models_A[[key]]),
        stringsAsFactors = FALSE
      )
    }

    if (!is.null(model$models_PDI[[key]])) {
      rows[[length(rows) + 1]] <- data.frame(
        Cluster = cluster_id,
        Target = "PDI",
        N = n_obs,
        Equation = format_equation(model$models_PDI[[key]]),
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(rows) == 0) {
    return(data.frame())
  }

  do.call(rbind, rows)
}

#' Run Climate Scenario Simulation
#'
#' Generates scenario-level summaries by applying additive changes to
#' meteorological variables and predicting A or PDI.
#'
#' @param model Trained model from train_ml_models() or train_hybrid_model()
#' @param baseline_data Data frame containing at least tmax, tmin, and RH
#' @param scenario_grid Data frame with columns dtmax, dtmin, dRH
#' @param type Prediction target: "A" or "PDI"
#' @param summary_fun Function used to summarize scenario predictions
#' @return A list with scenario_summary and scenario_predictions
#' @examples
#' data(rbsd_data)
#' processed <- preprocess_data(rbsd_data)
#' ml <- train_ml_models(processed, models = c("lm", "rf"))
#' grid <- expand.grid(dtmax = c(-1, 0, 1), dtmin = c(-1, 0, 1), dRH = c(-5, 0, 5))
#' sim <- simulate_climate_scenarios(ml, rbsd_data[, c("tmax", "tmin", "RH")], grid, "PDI")
#' @export
simulate_climate_scenarios <- function(model, baseline_data, scenario_grid,
                                       type = c("A", "PDI"), summary_fun = mean) {
  type <- match.arg(type)

  if (!is.data.frame(baseline_data)) {
    stop("baseline_data must be a data frame")
  }

  needed_baseline <- c("tmax", "tmin", "RH")
  if (!all(needed_baseline %in% colnames(baseline_data))) {
    stop("baseline_data must include columns: tmax, tmin, RH")
  }

  if (!is.data.frame(scenario_grid)) {
    stop("scenario_grid must be a data frame")
  }

  needed_scenario <- c("dtmax", "dtmin", "dRH")
  if (!all(needed_scenario %in% colnames(scenario_grid))) {
    stop("scenario_grid must include columns: dtmax, dtmin, dRH")
  }

  scenario_predictions <- list()

  for (i in seq_len(nrow(scenario_grid))) {
    row <- scenario_grid[i, , drop = FALSE]
    scenario_data <- baseline_data
    scenario_data$tmax <- scenario_data$tmax + row$dtmax
    scenario_data$tmin <- scenario_data$tmin + row$dtmin
    scenario_data$RH <- scenario_data$RH + row$dRH

    preds <- predict_rbsd(model, scenario_data[, needed_baseline, drop = FALSE], type = type)

    scenario_predictions[[i]] <- data.frame(
      scenario_id = i,
      dtmax = row$dtmax,
      dtmin = row$dtmin,
      dRH = row$dRH,
      prediction = preds
    )
  }

  scenario_predictions_df <- do.call(rbind, scenario_predictions)

  scenario_summary <- stats::aggregate(
    prediction ~ scenario_id + dtmax + dtmin + dRH,
    data = scenario_predictions_df,
    FUN = function(x) summary_fun(x, na.rm = TRUE)
  )
  colnames(scenario_summary)[colnames(scenario_summary) == "prediction"] <- "summary_prediction"

  list(
    scenario_summary = scenario_summary,
    scenario_predictions = scenario_predictions_df,
    target = type
  )
}
