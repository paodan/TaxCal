
#' Calculate the tax by providing annual gross salary
#' #' @param salary annual salary in RMB.
#' @param bin the bins for taxing.
#' @param bin the rate for each taxing bin.
#' @return The tax you should pay.
#' @export
#' @examples
#' tax(100000)
#' tax(c(100000, 200000, 250000, 270000, 300000))
tax = function(salary,
               bin = rev(c(    0,  36, 144,  300, 420,  660, 960)) * 10^3,
               rate = rev(c(0.03, 0.1, 0.2, 0.25, 0.3, 0.35, 0.45))){

  # Quick deduction
  plus = rev(-cumsum(rev(bin)*rev(c(diff(rate), 0))))

  res = lapply(salary, function(x){
    y = x - 60 * 10^3
    for(i in seq_along(bin)){
      if (y >= bin[i]){
        tax0 = y * rate[i] - plus[i]
        break
      } else {
        next
      }
    }
    tax0
  })

  return(unlist(res))
}

#' Calculate the tax and net salary by providing anual gross salary
#' @param salary annual gross salary in RMB.
#' @param bin the bins for taxing.
#' @param bin the rate for each taxing bin.
#' @return a list, including annual gross salary, the tax you should pay, and the net anual salary.
#' And make a plot, projecting the net salary on the curve.
#' @export
#' @examples
#' taxNet(100000)
#' taxNet(c(100000, 200000, 250000, 270000, 300000))
taxNet = function(salary,
                  bin = rev(c(    0,  36, 144,  300, 420,  660, 960)) * 10^3,
                  rate = rev(c(0.03, 0.1, 0.2, 0.25, 0.3, 0.35, 0.45))){
  plus = rev(-cumsum(rev(bin)*rev(c(diff(rate), 0))))
  tax0 = tax(salary, bin, rate)
  if(max(salary) >= 61200){
    x = seq(60000, by = 120, to = max(salary))
    plot(x, tax(x), type = "l", xlim = c(50000, max(salary)*1.1))
    abline(v = rev(60+ c(    0,  36, 144,  300, 420,  660, 960)) * 10^3)
    abline(h = bin * rate - plus)
    text(x = salary, y = tax0, labels = salary - tax0)
  }
  return(list(salary = salary, tax = tax0, net = salary - tax0))
}
