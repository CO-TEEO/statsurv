% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_yhat-arima.R, R/extract_yhat.R
\name{extract_yhat.Arima}
\alias{extract_yhat.Arima}
\alias{extract_yhat.forecast_ARIMA}
\alias{extract_yhat.arima_tidy}
\title{Extract fitted values from an ARIMA model}
\usage{
\method{extract_yhat}{Arima}(fit, newdata, resp_var, newxreg = NULL, ...)

\method{extract_yhat}{forecast_ARIMA}(fit, newdata, newxreg = NULL, ...)

\method{extract_yhat}{arima_tidy}(fit, newdata, ...)
}
\arguments{
\item{fit}{An ARIMA model fit object.}

\item{newdata}{A data frame containing the data that should be used to predict the fitted values.
Must have all of the covariates used by the model fit object to calculate fitted values. The
first \code{n} rows should be exactly the data used to fit the model, and any additional rows should
correspond to the number of steps ahead that predictions should be generated.}

\item{resp_var}{The column in \code{newdata} containing the time series data the model was fitted on.
Only used for \code{stats::arima} models. The column should be specified as bare text (no quotes).}

\item{newxreg}{A matrix of covariate information containing both the values of \code{xreg} used to
originally fit the model, and any new values of xreg to be used for prediction. IMPORTANT:
Unlike \code{\link[stats]{predict.Arima}}, \code{newxreg} must contain both the old and new values
of xreg.}

\item{...}{Additional arguments passed into other methods}
}
\value{
The input data frame \code{newdata} with the added column \code{.fitted} containing the predicted
values. Some models may also return additional columns, such as \code{.resid}. All columns added by
\code{extract_yhat} or \code{augment} start with a dot.
}
\description{
Methods for extracting fitted values from ARIMA models, including those fit using
\code{\link[stats:arima]{stats::arima}}, \code{\link[forecast:Arima]{forecast::Arima}}, or
\code{\link{arima_tidy}}.

Compared to other types of models, extrating fitted values from Arima models is rather fiddly. In
particular, the \emph{order} of rows in \code{newdata} and \code{newxreg} is crucially important. If the ARIMA
model was originally fit on \code{n} points, \code{extract_yhat} assumes that the first \code{n} rows of
\code{newdata} and \code{newxreg} correspond exactly to the data used to fit the model. The number of
additional rows in \code{newdata} is taken as the number of steps ahead that predictions should be
generated.
}
\details{
Behind the scenes, \code{extract_yhat} is performing two different tasks: extracting already
fitted values from the ARIMA model, and forecasting future values of the response variable. For
most other models, these two tasks are the same, since once the model is fit, the output of one
data point is not dependent on any other data points. The fact that the predictions of an ARIMA
model depend on the order of the data points drives a lot of the oddities of \code{extract_yhat} for
ARIMA models.

Extracting already fitted values is simple for models fit with the \code{forecast::Arima} or
\code{arima_tidy} functions, because the fitted values are stored in the model fit object. Models
fit with \code{stats::arima} do \emph{not} store this information, and so this information must be
supplied to \code{extract_yhat} by through the parameters \code{newdata} and \code{resp_var}.

\code{extract_yhat} can only be used to predict fitted values conditional on the data used to fit
the model. For \code{forecast::Arima} and \code{stats::arima}, any covariates necessary to generate these
forecasts must be provided as the last rows in \code{newxreg}. For \code{arima_tidy}, the covariates must
be provided as the last rows in \code{newdata}. Although \code{newdata} is note used for generating
predictions for \code{forecast::Ariam} or \code{stats::arima}, it is still a required parameter to the
functions, so that it can be used to report the final result. This is done so that the output
from \code{extract_yhat} is consistent with the output from \code{augment} and the broader Tidyverse,
which uses data frames as the key interface for passing data between functions.

For a version of arima that is based entirely on data frames, see \code{\link{arima_tidy}}.
}
\examples{
data("presidents_df")
fit_stats_arima <- stats::arima(presidents_df$approval_rating, order = c(2, 0, 0))
extract_yhat(fit_stats_arima, presidents_df, resp_var = approval_rating)


fit_forecast  <- forecast::Arima(presidents_df$approval_rating, order = c(2, 0, 0))
extract_yhat(fit_forecast, presidents_df)
# We can extract fitted values and make predictions at the same time
fit <- stats::arima(presidents_df$approval_rating[1:110], order = c(2, 0, 0))
extract_yhat(fit, presidents_df, resp_var = approval_rating)

# We can supply an external predictor as well
fit <- stats::arima(presidents_df$approval_rating[1:110], order = c(2, 0, 0),
                    xreg = as.matrix(presidents_df[1:110, "election_year", drop = FALSE]))
extract_yhat(fit, presidents_df, resp_var = approval_rating,
             newxreg = as.matrix(presidents_df[, "election_year", drop = FALSE]))

# We can use arima_tidy to not have to generate xreg manually
fit <- arima_tidy(approval_rating ~ election_year, order = c(2, 0, 0),
                  data = presidents_df[1:110, ])
extract_yhat(fit, presidents_df)
}
\seealso{
\code{\link{extract_yhat}}  \code{\link[forecast]{simulate.Arima}}
\code{\link{arima_tidy}} \code{\link{augment.arima_tidy}}
}
