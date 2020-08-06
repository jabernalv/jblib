# Moda de datos agrupados!
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

#' @title Moda de datos agrupados
#' @description  Esta función calcula la moda de datos agrupados del vector \code{x}.
#' @param x un elemento o vector numérico
#' @param breaks número de intervalos que se deben crear para dividir el vector \code{x}.
#' @details La función recibe un vector \code{x} y un número \code{breaks},
#' divide el vector en el número de intervalos y calcula la moda.
#' @examples
#' modgr(rnorm(1000, 20, 15))
#' modgr(rnorm(1000, 20, 15), 100)
#' @export
modgr <- function(x, breaks = 100){
  tabla.intervalos = transform(table(cut(x, breaks = breaks)))
  labs <- levels(cut(x, breaks = breaks))
  labs <- cbind(lower = as.numeric( sub("\\((.+),.*", "\\1", labs) ), upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", labs) ))
  fila <- as.integer(tabla.intervalos[tabla.intervalos$Freq==max(tabla.intervalos$Freq),][1])
  li <- labs[fila, 1]
  ti <- labs[fila, 2] - labs[fila,1]
  fi <- tabla.intervalos[fila, 2]
  fh <- ifelse(fila == 1, 0, tabla.intervalos[fila - 1, 2])
  fj <- ifelse(fila == breaks, 0, tabla.intervalos[fila + 1, 2])
  return(li+ti*((fi-fh)/(2*fi-fh-fj)))
}

