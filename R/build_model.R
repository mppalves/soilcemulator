#' @import keras
# library(keras)

build_model <- function(units, activation,input_shape, loss, optimizer) {
  model <- keras_model_sequential() %>%
    layer_dense(units = units[1], activation = activation, input_shape = input_shape ) %>%
    layer_dense(units = units[2], activation = activation) %>%
    layer_dense(units = units[3])

  model %>% compile(
    loss = loss,
    optimizer = optimizer,
    metrics = list("mean_absolute_error")
  )

  attr(model, "units") <- units
  attr(model, "activation") <- activation
  attr(model, "loss") <- loss
  attr(model, "optimizer") <- optimizer
  attr(model, "metrics") <- "mean_absolute_error"

  return(model)
}

