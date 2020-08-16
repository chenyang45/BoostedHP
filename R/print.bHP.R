#'Print method for class 'bHP'
#'
#'table of the bHP, in text (ASCII text output), latex (LaTeX code) or html
#'(HTML/CSS code).
#'
#'@param x an object of class 'bHP'
#'@param type a character string that specifies what style of print. Default
#'  choose is "bHP" showing designed table of class 'bHP'; choose "generic
#'  default" if you want to show the result of generic function plot.
#'@param trend_hist logical; if TRUE, adding trend component after each
#'  HP-filter conduction into the table and call 'select_trend_hist' to choose
#'  which iteration; if FALSE, don't add trend component history to the table.
#'@param select_trend_hist a numeric vector choosing which iteration time to
#'  show in the trend component history. It is valid only when 'trend_hist' is
#'  TRUE.
#'@param Head logical; if TRUE, showing the head of the table; if FALSE and
#'  'Tail' is TRUE, showing the tail of the table; if FALSE and 'Tail' is FALSE,
#'  showing the full-length of the table.
#'@param Tail logical; if TRUE, showing the tail of the table; if FALSE and
#'  'Head' is TRUE, showing the head of the table; if FALSE and 'Head' is FALSE,
#'  showing the full-length of the table.
#'@param print_type a character vector that specifies what type of output the
#'  command should produce. The possible values are "text" (default) for ASCII
#'  text output, "latex" for LaTeX code, "html" for HTML/CSS code.
#'@param digit controls the number of significant digits to print when printing
#'  numeric values. It is a suggestion only. Valid values are 1...22 with
#'  default 8. See the note in 'print.default' about values greater than 15.
#'
#'@return Table showing the content of bHP, "text" (default) for ASCII text
#'  output, "latex" for LaTeX code, "html" for HTML/CSS code.
#'@export
#'
#' @examples lam <- 100 # tuning parameter for the annual data
#'
#' data(IRE) # load the data 'IRE'
#'
#' # raw HP filter
#' bx_HP <- BoostedHP(IRE, lambda = lam, iter= FALSE)
#'
#' # by BIC
#' bx_BIC <- BoostedHP(IRE, lambda = lam, iter= TRUE, stopping = "BIC")
#'
#' # by ADF
#' bx_ADF <- BoostedHP(IRE, lambda = lam, iter= TRUE, stopping = "adf")
#'
#' # by nonstop test type
#' # Iterated HP filter until Max_Iter and keep the path of BIC.
#'
#' bx_nonstop <- BoostedHP(IRE, lambda = lam, iter= TRUE, stopping = "nonstop")
#' \dontrun{
#'#--------- start to print the content of bHP -----------------
#' print(bx_ADF)
#'
#' print(bx_ADF, Head = F, Tail = T, trend_hist = F)
#'
#' print(bx_ADF, Head = T, Tail = T, trend_hist = F)
#'
#' print(bx_ADF, Head = F, Tail = F, trend_hist = F)
#'
#' print(bx_BIC, Head = F, Tail = F, trend_hist = T, select_trend_hist = 1:bx_BIC$iter_num)
#'
#' print(bx_BIC, Head = F, Tail = F, trend_hist = T,  select_trend_hist = c(1,3,5))
#'
#' # when the trend_hist is FALSE, select_trend_hist is invalid
#' print(bx_BIC, Head = F, Tail = F, trend_hist = F, select_trend_hist = c(1,3,5))
#'
#' print(bx_BIC, Head = F, Tail = T, trend_hist = F, print_type = "latex")
#'
#' print(bx_BIC, Head = F, Tail = T, trend_hist = F, print_type = "html")
#'
#' # show the generic print function output
#' print(bx_ADF, type = "generic default")
#' }

print.bHP <-
  function(x,
           type = "bHP",
           trend_hist = TRUE,
           select_trend_hist = c(1),
           Head = FALSE,
           Tail = FALSE,
           print_type = "text",
           digit = 8) {
    options(digits = digit)

    if (type == "generic default") {
      message("This is a generic.default method print of 'bHP' class (",
              x$stopping,
              ").")
      print.default(x)
    }


    if (type == "bHP") {
      message("This is print method special for 'bHP' class (",
              x$stopping,
              ").")

      #----------------- manage data frame for "ADF or "BIC" bHP -----------------------
      if (x$stopping == "adf" | x$stopping == "BIC") {
        colname <-
          c(
            "Raw Data",
            paste0("Final Trend (", x$iter_num, "th)"),
            paste0("Trend (", 1:x$iter_num, "th)")
          )

        if (x$stopping == "adf") {
          value_hist <-
            c("P-value:",
              paste0(round(x$adf_p_hist[x$iter_num], 4), " (", x$iter_num, "th)"),
              paste0(
                round(x$adf_p_hist, 4),
                paste0(" (", 1:x$iter_num, "th)")
              ))
        }
        if (x$stopping == "BIC") {
          value_hist <-
            c("BIC value:",
              paste0(round(x$BIC_hist[x$iter_num], 4), " (", x$iter_num, "th)"),
              paste0(
                round(x$BIC_hist[1:x$iter_num], 4),
                paste0(" (", 1:x$iter_num, "th)")
              ))
        }

        raw_trend <- cbind.data.frame(x$raw_data, x$trend, x$trend_hist)
        data <- rbind(value_hist, raw_trend)
        colnames(data) <- colname


        #--------------------- trend selection ----------------------------------------------------

        if (trend_hist == TRUE) {
          data_trend_select <-
            as.matrix(data)[, c(1, 2, select_trend_hist + 2)] %>% as.data.frame()
        }
        if (trend_hist == FALSE) {
          data_trend_select <- as.matrix(data)[, c(1, 2)] %>% as.data.frame()
        }
        # end trend selection of ADF or BIC

      }

      #----------------- manage data frame for "nonstop-iter" bHP -----------------------
      if (x$stopping == "nonstop-iter") {
        colname <- c("Raw Data", "Trend Component")

        # nonstop BIC or P-value (Only Once HP-filter)
        value_hist <- c("Type:", "Once HP-filter")

        raw_trend <- cbind.data.frame(x$raw_data, x$trend)
        data <- rbind(value_hist, raw_trend)
        colnames(data) <- colname

        data_trend_select <- data

      }

      #----------------- manage data frame for "nonstop" bHP -----------------------
      if (x$stopping == "nonstop") {
        colname <-
          c("Raw Data",
            paste0("Final Trend (", dim(x$trend_hist)[2], "th)"),
            paste0(1:dim(x$trend_hist)[2], "th Trend"))

        value_hist <-
          c("BIC:",
            paste0(round(x$BIC_hist[dim(x$trend_hist)[2]], 4), " (", dim(x$trend_hist)[2], "th)"),
            paste0(round(x$BIC_hist[1:(dim(x$trend_hist)[2])], 4), paste0(" (", 1:(
              dim(x$trend_hist)[2]
            ), "th)")))

        raw_trend <- cbind.data.frame(x$raw_data, x$trend, x$trend_hist)
        data <- rbind(value_hist, raw_trend)
        colnames(data) <- colname

        #--------------------- trend selection ----------------------------------------------------
        if (trend_hist == TRUE) {
          data_trend_select <-
            as.matrix(data)[, c(1, 2, select_trend_hist + 2)] %>% as.data.frame()
        }
        if (trend_hist == FALSE) {
          data_trend_select <- as.matrix(data)[, c(1, 2)] %>% as.data.frame()
        }
        # end trend selection


      }

      # manage the Time Serise ID

      row.names(data) <- c(" ", 1:length(x$raw_data))
      row.names(data_trend_select) <- c(" ", 1:length(x$raw_data))

      #----------------------------------------------------------------------------------------
      #--------------------- row selection ----------------------------------------------------

      if (Head == FALSE & Tail == FALSE) {
        row <- 1:(dim(data)[1])
      }

      if (Head == TRUE & Tail == FALSE) {
        if (dim(data)[1] > 6) {
          row <- 1:6
        } else{
          row <- 1:(dim(data)[1])
        }
      }

      if (Head == FALSE & Tail == TRUE) {
        if (dim(data)[1] > 6) {
          row <- c(1, ((dim(data)[1] - 4):dim(data)[1]))
        } else{
          row <- 1:(dim(data)[1])
        }
      }

      if (Head == TRUE & Tail == TRUE) {
        if (dim(data)[1] > 11) {
          row <- c(1:6, ((dim(data)[1] - 4):dim(data)[1]))
        } else{
          row <- 1:(dim(data)[1])
        }
      }

      data_row_trend_select <- data_trend_select[row, ]


      stargazer::stargazer(data_row_trend_select,
                           summary = F,
                           type = print_type)

      View(data)
      message("The content of bHP can be viewd via the spreadsheet-style data viewer.")

    }
  }
