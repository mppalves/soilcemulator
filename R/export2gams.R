#' @name export2gams
#' @title Export neural nets weights and biases to .csv and write gams code
#' @description Save and export model weights and biases for use in GAMS
#'   reconstruction of neural networks.
#' @param model trained model
#' @param module magpie module number and name
#' @param means named vector with feature means
#' @param stddevs named vetor with feature std
#' @param inputs_vec vector with input names
#' @param type type of model created (s or p)
#' @param model_hash model id hash
#' @param flag flag used to identify the label column
#' @param inputs_vec vector with input column names
#' @param type Letter used to identify the dataset being used
#' @author Marcos Alves \email{mppalves@gmail.com}
#' @import keras
#' @import utils
#' @usage export2gams(model, module, means, stddevs, inputs_vec, type, model_hash, flag)
#' @export export2gams



list_col_names <- function(x) {
  k <- list()
  w <- NULL
  for (i in 1:length(x)) {
    l <- floor((i + 1) / 2)
    if ((i %% 2) == 0) {
      k[[i]] <- as.character(paste0("n", l))
    } else {
      # else, then weights.
      for (j in 1:ncol(x[[i]])) {
        w <- append(w, as.character(paste0("n", l, "_", j)))
      }
      k[[i]] <- w
      w <- NULL
    }
  }
  return(k)
}

update_weights <- function(inputs_vec, weights, names_list) {
  weights[[1]] <- cbind(inputs_vec, weights[[1]])
  weights[[1]] <- rbind(append("dummy", names_list[[1]]), weights[[1]])
  for (i in 2:length(weights)) {
    if ((i %% 2) != 0) {
      weights[[i]] <- cbind(names_list[[(i - 2)]], weights[[i]])
      weights[[i]] <- rbind(append("dummy", names_list[[i]]), weights[[i]])
    } else {
      weights[[i]] <- cbind(names_list[[(i - 1)]], weights[[i]])
      # weights[[i]] <- rbind(append("dummy", names_list[[i]]), weights[[i]])
    }
  }
  return(weights)
}

save_weights <- function(weights, type, model_hash) {
  hash <- substr(model_hash, 1, 6)
  for (i in 1:length(weights)) {
    l <- floor((i + 1) / 2)
    if ((i %% 2) != 0) {
      write.table(data.frame(weights[[i]]), paste0(hash, "_", type, "_weights_", l, ".csv"), col.names = F, row.names = F, sep = ",", quote = F)
    } else {
      write.table(data.frame(weights[[i]]), paste0(hash, "_", type, "_bias_", l, ".csv"), col.names = F, row.names = F, sep = ",", quote = F)
    }
  }
}

weights_names <- function(x) {
  weights_names <- list()
  w <- NULL
  for (i in 1:length(x)) {
    c <- (i + 1) / 2
    if ((i %% 2) != 0) {
      for (j in 1:ncol(x[[i]])) {
        y <- as.character(paste0("n", c, "_", j))
        w <- append(w, y)
      }
      weights_names[[c]] <- w
      w <- NULL
    }
  }
  return(weights_names)
}


write_sets <- function(weights_names, inputs_vec, type, model_hash) {
  printer <- file(paste0(type, "_sets", ".txt"), "w")
  write(paste("* model hash ID", model_hash), file = printer, append = T)
  # defining sets
  y <- paste0("in_types_", type)
  y <- append(y, paste0("in_lsu_", type))
  y <- append(y, paste0("in_env_", type))
  for (i in 1:length(weights_names)) {
    y <- append(y, paste0("ln", type, i))
  }
  write("sets", file = printer, )
  x <- paste0(y[1], " Neural net input features / ", capture.output(cat(inputs_vec, sep = ", ")), " /")
  x <- append(x, paste0(y[2], "(", y[1], ") LSU input type / LSU /"))
  x <- append(x, gsub("lsu,", "", paste0(y[3], "(", y[1], ") Weather input types /", capture.output(cat(inputs_vec, sep = ", ")), "/"), ignore.case = T))

  for (i in 1:length(weights_names)) {
    if (length(weights_names[[i]]) > 1) {
      x <- append(x, paste0(y[i + 3], " layer ", i, " / ", weights_names[[i]][1], " * ", weights_names[[i]][length(weights_names[[i]])], " / "))
    } else {
      x <- append(x, paste0(y[i + 3], " layer ", i, " / ", weights_names[[i]][1], " / "))
    }
  }
  x <- append(x, ";")
  write(x, file = printer, append = T)
  close(printer)

  return(y)
}

write_declarations <- function(weights_names, module_number, type, .mean_lsu, .std_lsu, .mean_out, .std_out, model_hash) {
  ext_type <- NULL
  if (type == "s") {
    ext_type <- "soilc"
  }
  if (type == "p") {
    ext_type <- "past"
  }
  if (type == "l") {
    ext_type <- "lsu_nr"
  }
  if (is.null(ext_type)) {
    stop("Invalid type. Please use 's' for soil carbon, 'p' for pasture yields and 'l' for lsu numbers ")
  }
  hash <- substr(model_hash, 1, 6)
  printer <- file(paste0(hash,"_",type, "_declarations", ".txt"), "w")
  write(paste("* model hash ID", model_hash), file = printer, append = T)
  # declaring variables

  dx <- paste0("v31_lsu")
  dx <- append(dx, paste0("v", module_number, "_inlsu"))
  dx <- append(dx, paste0("v", module_number, "_inEnv"))

  zx <- NULL
  ax <- NULL
  dzax <- NULL
  for (i in 1:length(weights_names)) {
    zx <- append(zx, paste0("v", module_number, "_z", i))
    ax <- append(ax, paste0("v", module_number, "_a", i))
    dzax <- append(dzax, c(zx[i], ax[i]))
  }
  dzax <- append(dx, dzax)

  write("variables", file = printer, append = T)
  x <- paste0(dx[1], "(j)", " LSU variable")
  x <- append(x, paste0(dx[2], "(j,ln", type, "1)", " LSU input layer"))
  x <- append(x, paste0(dx[3], "(j,ln", type, "1)", " Environmental input layer"))

  for (i in 1:(length(weights_names) - 1)) {
    x <- append(x, paste0(zx[i], "(j,ln", type, i, ")", " layer neurons"))
    x <- append(x, paste0(ax[i], "(j,ln", type, i, ")", " layer activation"))
  }
  x <- append(x, ";")
  write(x, file = printer, append = T)


  # declaring equations

  dy <- paste0("q", module_number, "_inlsu")
  dy <- append(dy, paste0("q", module_number, "_inEnv"))
  dy <- append(dy, paste0("q", module_number, "_rlsu"))
  dy <- append(dy, paste0("q", module_number, "_maxlsu"))
  dy <- append(dy, paste0("q", module_number, "_minlsu"))
  dy <- append(dy, paste0("q", module_number, "_", ext_type, "_yld"))

  zy <- NULL
  ay <- NULL
  dzay <- NULL
  for (i in 1:length(weights_names)) {
    zy <- append(zy, paste0("q", module_number, "_z", i))
    ay <- append(ay, paste0("q", module_number, "_a", i))
    dzay <- append(dzay, c(zy[i], ay[i]))
  }
  dzay <- append(dy, dzay)

  write("equations", file = printer, append = T)
  y <- paste0(dy[1], "(j,ln", type, "1)", " LSU input equation")
  y <- append(y, paste0(dy[2], "(j,ln", type, "1)", " LSU input equation"))
  y <- append(y, paste0(dy[3], "(j)", " real lsu equation"))
  y <- append(y, paste0(dy[4], "(j)", " max LSU"))
  y <- append(y, paste0(dy[5], "(j)", " min LSU"))
  y <- append(y, paste0(dy[6], "(j)", " output equation"))


  for (i in 1:(length(weights_names) - 1)) {
    y <- append(y, paste0(zy[i], "(j,ln", type, i, ")", " layer equation"))
    y <- append(y, paste0(ay[i], "(j,ln", type, i, ")", " activation equation"))
  }
  y <- append(y, ";")
  write(y, file = printer, append = T)

  dw <- paste0("v", module_number, "_", ext_type, "_yld")
  dw <- append(dw, paste0("v", module_number, "_rlsu"))

  write("positive variables", file = printer, append = T)
  w <- paste0(dw[1], "(j)", " output variable")
  w <- append(w, paste0(dw[2], "(j)", " real LSU variable"))
  w <- append(w, ";")
  write(w, file = printer, append = T)

  ds <- paste0("s", module_number, "_lsu_mean")
  ds <- append(ds, paste0("s", module_number, "_lsu_std"))
  ds <- append(ds, paste0("s", module_number, "_out_mean"))
  ds <- append(ds, paste0("s", module_number, "_out_std"))

  write("scalars", file = printer, append = T)
  s <- paste0(ds[1], " lsu conversion factor /", .mean_lsu, "/")
  s <- append(s, paste0(ds[2], " lsu conversion factor /", .std_lsu, "/"))
  s <- append(s, paste0(ds[3], " output conversion factor /", .mean_out, "/"))
  s <- append(s, paste0(ds[4], " output conversion factor /", .std_out, "/"))
  s <- append(s, ";")
  write(s, file = printer, append = T)
  close(printer)

  return(list(dzax, dzay, dw, ds))
}



write_inputs <- function(weights_names, dec, sets, module, type, module_number, model_hash) {
  hash <- substr(model_hash, 1, 6)
  printer <- file(paste0(hash,"_",type, "_inputs", ".txt"), "w")
  write(paste("* model hash ID", model_hash), file = printer, append = T)

  w <- paste0("f", module_number, "_nn_input")
  y <- paste0("f", module_number, "_w", 1)
  x <- paste0("f", module_number, "_b", 1)
  for (i in 2:length(weights_names)) {
    y <- append(y, paste0("f", module_number, "_w", i))
    x <- append(x, paste0("f", module_number, "_b", i))
  }

  d <- paste0("table ", w, "(j,", sets[3], ") aggregated environmental cell values
$ondelim
$include \"./modules/", module, "/input/environment_cell.csv\"
$offdelim
;")

  write(d, file = printer)

  d <- paste0("table ", y[1], "(", sets[1], ",", sets[4], ") weight
$ondelim
$include \"./modules/", module, "/input/", hash, "_", type, "_weights_", 1, ".csv\"
$offdelim
;")
  write(d, file = printer, append = T)


  for (i in 2:length(weights_names)) {
    d <- paste0("table ", y[i], "(", sets[i + 2], ",", sets[i + 3], ") weight
$ondelim
$include \"./modules/", module, "/input/", hash, "_", type, "_weights_", i, ".csv\"
$offdelim
;")
    write(d, file = printer, append = T)
  }

  for (i in 1:length(weights_names)) {
    d <- paste0("parameter ", x[i], "(", sets[i + 3], ") bias
/
$ondelim
$include \"./modules/", module, "/input/", hash, "_", type, "_bias_", i, ".csv\"
$offdelim
/;")
    write(d, file = printer, append = T)
  }
  close(printer)
  return(list(w, y, x))
}


write_equations <- function(dec, sets, wb, type, model_hash) {
  x <- paste0(dec[[2]][1], "(j2,", sets[4], ")..  ", dec[[1]][2], "(j2,", sets[4], ") =e= sum(", sets[2], ", ", dec[[1]][1], "(j2) * ", wb[[2]][1], "(", sets[2], ",", sets[4], "));")
  x <- append(x, paste0(dec[[2]][2], "(j2,", sets[4], ")..  ", dec[[1]][3], "(j2,", sets[4], ") =e= sum(", sets[3], ", ", wb[[1]][1], "(j2", ",", sets[3], ") * ", wb[[2]][1], "(", sets[3], ",", sets[4], "));"))
  x <- append(x, paste0(dec[[2]][7], "(j2,", sets[4], ")..  ", dec[[1]][4], "(j2,", sets[4], ") =e= ", dec[[1]][2], "(j2,", sets[4], ")", " + ", dec[[1]][3], "(j2,", sets[4], ")", " + ", wb[[3]][1], "(", sets[4], ")", ";"))
  x <- append(x, paste0(dec[[2]][8], "(j2,", sets[4], ")..  ", dec[[1]][5], "(j2,", sets[4], ") =e= 1/( 1 + system.exp(-", dec[[1]][4], "(j2,", sets[4], ")));"))
  j <- 5
  for (i in 6:(length(dec[[1]]) - 2)) {
    if (grepl("[z]", dec[[1]][i])) {
      x <- append(x, paste0(dec[[2]][i + 3], "(j2,", sets[j], ")..  ", dec[[1]][i], "(j2,", sets[j], ") =e= sum(", sets[j - 1], ", ", dec[[1]][i - 1], "(j2,", sets[j - 1], ")", " * ", wb[[2]][j - 3], "(", sets[j - 1], ",", sets[j], ")) + ", wb[[3]][j - 3], "(", sets[j], ");"))
    } else {
      x <- append(x, paste0(dec[[2]][i + 3], "(j2,", sets[j], ")..  ", dec[[1]][i], "(j2,", sets[j], ") =e= 1/( 1 + system.exp(-", dec[[1]][i - 1], "(j2,", sets[j], ")));"))
      j <- j + 1
    }
  }
  x <- append(x, paste0(grep("yld", dec[[2]], value = T), "(j2)..  ", dec[[3]][1], "(j2) =e= sum((", sets[length(sets) - 1], ",", sets[length(sets)], "), ", dec[[1]][length(dec[[1]]) - 2], "(j2,", sets[length(sets) - 1], ")", " * ", wb[[2]][length(wb[[3]])], "(", sets[length(sets) - 1], ",", sets[length(sets)], ") + ", wb[[3]][length(wb[[3]])], "(", sets[length(sets)], "));"))
  # x <- append(x, paste0(grep("max", dec[[2]], value = T), "(j2)..  ", dec[[1]][1], "(j2) =l= 2;"))
  # x <- append(x, paste0(grep("min", dec[[2]], value = T), "(j2)..  ", dec[[1]][1], "(j2) =g= -2;"))
  # x <- append(x, paste0(grep("rlsu", dec[[2]], value = T), "(j2)..  ", dec[[3]][2], "(j2) =e= ", dec[[1]][1], "(j2)", " * ", dec[[4]][2], " + ", dec[[4]][1], ";"))

  hash <- substr(model_hash, 1, 6)
  printer <- file(paste0(hash,"_",type, "_equations", ".txt"), "w")
  write(paste("* model hash ID", model_hash), file = printer, append = T)
  write(x, file = printer)
  close(printer)
}



# Additional functions
# bias_names = function(x) {
#   bias_names = list()
#   for (i in 1:length(x)) {
#     c = (i+1)/2
#     if ((i %% 2) == 0) {
#       bias_names[[c-0.5]] = as.character(paste0("b", c-0.5))
#     }
#   }
#   return(bias_names)
# }

# give_names = function(x) {
#
#   weights_names = list()
#   bias_names = list()
#   w = NULL
#   b = NULL
#   for (i in 1:length(x)) {
#     c = (i+1)/2
#     if ((i %% 2) == 0) {
#       #if even, then bias.
#       #print(paste0("b", c-0.5))
#       bias_names[[c-0.5]] = as.character(paste0("b", c-0.5))
#     }else{
#       #else, then weights.
#       for (j in 1:ncol(x[[i]])) {
#         #print(paste0("w", c, j))
#         y = as.character(paste0("w", c,"_", j))
#         w = append(w,y)
#       }
#       weights_names[[c]] = w
#       w = NULL
#     }
#   }
#   return(c(list(bias_names), list(weights_names)))
# }

# list_col_names = function(x) {
#   k = list()
#   w = NULL
#   for (i in 1:length(x)) {
#     l = floor((i + 1) / 2)
#     if ((i %% 2) == 0) {
#       k[[i]] = as.character(paste0("b", l))
#     }else{
#       #else, then weights.
#       for (j in 1:ncol(x[[i]])) {
#         w = append(w,as.character(paste0("w", l,"_", j)))
#       }
#       k[[i]] = w
#       w = NULL
#     }
#   }
#   return(k)
# }

export2gams <- function(model, module, means, stddevs, inputs_vec, type, model_hash, flag) {
  module_number <- as.numeric(unlist(regmatches(module, gregexpr("\\d{2,}", module))))
  mean_lsu <- means[grep("lsu", names(means))]
  std_lsu <- stddevs[grep("lsu", names(stddevs))]
  mean_out <- means[grep(flag, names(means))]
  std_out <- stddevs[grep(flag, names(stddevs))]

  # exporting weights and biases
  weights <- get_weights(model)
  names_list <- list_col_names(weights)
  weights_up <- update_weights(inputs_vec, weights, names_list)
  save_weights(weights_up, type, model_hash)

  # exporting gams code
  w_names <- weights_names(weights)
  dec <- write_declarations(w_names, module_number, type, mean_lsu, std_lsu, mean_out, std_out, model_hash)
  sets <- write_sets(w_names, inputs_vec, type, model_hash)
  wb <- write_inputs(w_names, dec, sets, module, type, module_number, model_hash)
  write_equations(dec, sets, wb, type, model_hash)
}
