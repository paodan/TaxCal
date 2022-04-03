#' How much you can make in a US company
#' @param salary basic salary within certain months, default is the annual salary (120K)
#' @param months number of months corresponding to the salary
#' @param bonus bonus of the year
#' @param stateTaxRate state tax rate
#' @param married TRUE if you are married
#' @param joint TRUE if you pay for the tax jointly with your spouse
#' @param rate401k what is the percentage you save the pre-tax salary in your 401k account
#' @param bonus401k what is the extra percentage your company give (except the $1-to-$1 match)
#' in your 401k account
#' @param medical how much you pay for your healthy insurance monthly
#' @param dental how much you pay for your dental insuarance monthly
#' @param D2Y the exchange rate of Dollar to Yuan.
#' @export
#' @examples
#' ustax()
#' ustax(118000)
#' ustax(60000, months = 6, joint = T)
#' ustax(50000, months = 5, joint = F)
#' ustax(40000, months = 4, joint = F)
#' ustax(30000, months = 3, joint = F)
#' ustax(20000, months = 2, joint = F)
#' ustax(10000, months = 1, joint = F)
#'

ustax = function(salary = 120000, months = 12, bonus = salary * .08,
                 stateTaxRate = .05,
                 married = T, joint = T, rate401k = .06, bonus401k = .02,
                 insurance = 558, dental = 46.26, D2Y = 6.36){

  rmb = function(x){
    round(x * D2Y)
  }

  rate = c(.1, .12, .22, .24, .32, .35, .37)
  if (married){
    fed = c(0, 20550, 83550, 178150, 340100, 431900, 647850)
    deduction = c(0, 2055, 9615, 30427, 69295, 98671, 174253.5)
  } else {
    fed = c(0, 20550, 83550, 178150, 340100, 431900, 647850) # change it
    deduction = c(0, 2055, 9615, 30427, 69295, 98671, 174253.5) # change it
  }

  if(joint) {
    deduction = deduction
    fed = fed
  } else {
    deduction = deduction/2
    fed = fed/2
  }

  toTaxBasic = salary - (insurance + dental) * months - salary * rate401k

  id = tail(which(toTaxBasic > fed), 1)
  taxRate = rate[id]
  taxBasic = (toTaxBasic - fed[id]) * taxRate + deduction[id] + stateTaxRate * toTaxBasic

  # How much is left
  leftBasic = toTaxBasic - taxBasic

  # How much can I get every month
  basicPerM = round(leftBasic/months)

  # How much bonus can I get
  bonusLeft = round(bonus * (1 - taxRate))

  if (rate401k <= .06) {
    match401k = rate401k + bonus401k
  } else {
    match401k = .06 + bonus401k
  }

  # How much can I get in 401k account every year
  total401k = round((rate401k + match401k) * salary)

  cat("Every month I can get $", basicPerM, " (", rmb(basicPerM), " RMB);\n",
      "I can get a bonus of $", bonusLeft, " (", rmb(bonusLeft)," RMB) annualy;\n",
      "My annual take-home income (with health insurance being paid) is $", round(leftBasic + bonusLeft),
      " (", rmb(leftBasic + bonusLeft), " RMB);\n",
      "401K account accumulates $", total401k, " (", rmb(total401k), " RMB)",
      sep = "")

  return(invisible(list(basicPerM = basicPerM,
                        bonusLeft = bonusLeft,
                        takeHomeIncome = round(leftBasic + bonusLeft),
                        total401k = total401k
  )))

}
