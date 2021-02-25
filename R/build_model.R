#' @title build_model
#' @description define and compile keras model
#' @return Untrained model
#' @param units vector list with number of neurons per layer (last layer has to be 1)
#' @param activation Activation function (either native from keras or custom)
#' @param input_shape number of neurons in the input layer, read from the pre processed dataset
#' @param loss loss function name (defaul: mean_absolute_error)
#' @param optimizer Keras optimizer
#' @author Marcos Alves
#' @import keras
#' @import tidyr
#' @export

build_model <- function(units, activation, input_shape, loss, optimizer) {
  model <- keras_model_sequential() %>%
    layer_dense(units = units[1], activation = activation, input_shape = input_shape) %>%
    layer_dropout(rate = 0.3, seed = 123) %>%
    layer_dense(units = units[2], activation = activation) %>%
    layer_dropout(rate = 0.3, seed = 123) %>%
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
