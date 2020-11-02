#' drgplot
#'
#'The function drgplot makes a boxplot for either the average Medicare payments, the average total payment, or the average covered charges by DRG code.
#'
#' @param df a dataframe, use the dataset in the package drgdata2
#' @param a a string name for the payment variable in the datafram. Choose from Average Medicare payments, Average Total Payment, or Average Covered Charges
#'
#' @return drgplot returns a boxlpot of one of the payment variables by DRG
#' @export
#'
#' @examples
#' drgplot(drgdata2, "Average Total Payments")
#'
drgplot <- function(df, a) {

  if(is.element(a, c('Average Covered Charges', 'Average Total Payments', 'Average Medicare Payments'))) {
    ggplot(df, aes(x = drg, y = get(a))) +
      geom_boxplot() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))  +
      ggtitle('Payment of Top 100 Most Frequently Billed Medicare Severity Diagnosis Related Group (MS-DRG), 2011') +
      labs(x = "DRG", y = "DRG Payment") +
      theme(plot.title = element_text(hjust = 0.5))
  }
  else {
    print('Please check your input')
  }

}


#' drgsummary
#'
#'The function drgsummary calculates mean, median, or standard deviation for the average Medicare payments.
#'
#' @param df a dataframe, use the dataset in the package drgdata2
#' @param a a string name for the statistics, choose from mean, median, or sd for standard deviation
#'
#' @return drgsummary returns statistics of average Medicare payments
#' @export
#'
#' @examples
#' drgsummary(drgdata2, "Median")
#'
drgsummary <- function(df, a) {
  if(is.element(tolower(a), 'mean' )) {
    df %>%
      summarise(mean = sd(`Average Medicare Payments`)) } else

        if(is.element(tolower(a), 'median' )) {
          df %>%
            summarise(median = median(`Average Medicare Payments`)) } else

              if(is.element(tolower(a), 'sd' )) {
                df %>%
                  summarise(standard.deviation = sd(`Average Medicare Payments`)) } else

                    print('Please check your input')

}
