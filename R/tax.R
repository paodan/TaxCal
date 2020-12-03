
#' Calculate the tax by providing annual gross salary
#' #' @param salary annual salary in RMB.
#' @param bin the bins for taxing.
#' The default is c(0, 36,000, 144,000, 300,000, 420,000, 660,000, 960,000).
#' @param rate the rate for each taxing bin.
#' The default is c(0.03, 0.1, 0.2, 0.25, 0.3, 0.35, 0.45).
#' @return The tax you should pay.
#' @export
#' @examples
#' tax(100000)
#' tax(c(100000, 200000, 250000, 270000, 300000))
tax = function(salary,
               bin = c(    0,  36, 144,  300, 420,  660, 960) * 10^3,
               rate = c(0.03, 0.1, 0.2, 0.25, 0.3, 0.35, 0.45)){

  bin_rev = rev(bin)
  rate_rev = rev(rate)

  # Quick deduction
  plus = rev(-cumsum(bin*rev(c(diff(rate_rev), 0))))

  res = lapply(salary, function(x){
    y = x - 60 * 10^3
    for(i in seq_along(bin_rev)){
      if (y >= bin_rev[i]){
        tax0 = y * rate_rev[i] - plus[i]
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
#' The default is c(0, 36,000, 144,000, 300,000, 420,000, 660,000, 960,000).
#' @param rate the rate for each taxing bin.
#' The default is c(0.03, 0.1, 0.2, 0.25, 0.3, 0.35, 0.45).
#' @return a list, including annual gross salary, the tax you should pay, and the net anual salary.
#' And make a plot, projecting the net salary on the curve.
#' @export
#' @examples
#' taxNet(100000)
#' taxNet(c(100000, 200000, 250000, 270000, 300000))
taxNet = function(salary,
                  bin = c(    0,  36, 144,  300, 420,  660, 960) * 10^3,
                  rate = c(0.03, 0.1, 0.2, 0.25, 0.3, 0.35, 0.45)){

  bin_rev = rev(bin)
  rate_rev = rev(rate)
  plus = rev(-cumsum(bin*rev(c(diff(rate_rev), 0))))
  tax0 = tax(salary, bin, rate)
  if(max(salary) >= 61200){
    x = seq(60000, by = 120, to = max(salary))
    plot(x, tax(x), type = "l", xlim = c(50000, max(salary)*1.1))
    abline(v = rev(60+ c(    0,  36, 144,  300, 420,  660, 960)) * 10^3)
    abline(h = bin_rev * rate_rev - plus)
    text(x = salary, y = tax0, labels = salary - tax0)
  }
  return(list(salary = salary, tax = tax0, net = salary - tax0))
}
