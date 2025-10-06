\name{plot_risk_map}
\alias{plot_risk_map}
\title{Plot RBSD Risk Map}
\description{Plots the spatio-temporal risk map for Rice Brown Spot Disease (RBSD) using climate scenario data and model predictions.}
\usage{plot_risk_map(climate_scenarios, model, ...)}
\arguments{
  \item{climate_scenarios}{A data frame or list containing climate scenario data.}
  \item{model}{A trained model object for RBSD risk prediction.}
  \item{...}{Additional arguments passed to the plotting function.}
}
\details{This function visualizes the predicted risk of RBSD across regions and time periods, using climate scenario data and model outputs. It is useful for identifying disease hotspots and supporting early warning systems.}
\value{A plot object (e.g., ggplot2 or base R plot) showing the risk map.}
\examples{
# Example usage:
# plot_risk_map(climate_scenarios, trained_model)
}
\author{Ajit Govind, Sudhasha Selvaraj, Alsamman M. Alsamman, et al.}
