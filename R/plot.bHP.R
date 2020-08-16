#' Plot method for class \code{bHP}
#'
#' plot method for a \code{bHP} object
#'
#' @param x an object of class \code{bHP}
#' @param plot_type a character string specifies the style of plot.'static' for
#'   static figure, 'JS' for plotly.js, a web-based interactive charting
#'   library, 'dynamic' for dynamic figure showing history of iterated process.
#' @param interval_t a positive number to set the time interval of the animation
#'   (unit in seconds); default to be 0.3.
#' @param ylab a title for the y axis: see 'title' in package 'graphics'.
#' @param col_raw A specification for the default plotting color of raw data.
#'   See section ‘Color Specification’.
#' @param col_trend_h A specification for the default plotting color of trend
#'   history. See section ‘Color Specification’.
#' @param col_trend_f A specification for the default plotting color of final
#'   trend component. See section ‘Color Specification’.
#' @param col_pvalue_BIC A specification for the default plotting color of
#'   p-value of BIC. See section ‘Color Specification’.
#' @param raw_alpha a numeric vector from 0 to 255 modifying color transparency
#'   of plotting raw data. The smaller the number, the more transparent.
#' @param trend_h_alpha a numeric vector from 0 to 255 modifying color
#'   transparency of plotting trend history. The smaller the number, the more
#'   transparent.
#' @param trend_f_alpha a numeric vector from 0 to 255 modifying color
#'   transparency of plotting final trend component. The smaller the number, the
#'   more transparent.
#' @param pvalue_BIC_alpha a numeric vector from 0 to 255 modifying color
#'   transparency of plotting p-value or BIC. The smaller the number, the more
#'   transparent.
#' @param legend_location a character string or a pair of numeric vector
#'   specifying the location of legend. The choice set of the character sting
#'   are: upleft, downleft, upright, downright.
#' @param iteration_location a character string or a pair of numeric vector
#'   specifying the location of 'iteration time'. The choice set of the character
#'   sting are: upleft, downleft, upright, downright.
#' @param cex_text The magnification to be used for showing 'iteration time'
#'   relative to the current setting of cex.
#' @param cex_legend The magnification to be used for legend relative to the
#'   current setting of cex.
#' @param main an overall title for the plot: see 'title' in package 'graphics'.
#'
#' @return X-Y Plotting method for class 'bHP'
#' @export
#'
#' @examples
#'
#' lam <- 100 # tuning parameter for the annual data
#'
#' \dontrun{
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
#'
#' #--------- start to plot the content of bHP -----------------
#'
#'
#' #--------- for dynamic style (default)--------
#' plot(bx_ADF)
#'
#' plot(bx_ADF, iteration_location = "upright") # change the location of text
#'
#' plot(bx_ADF, iteration_location = c(30,12)) # assign the location of text by x-y co-ordinates
#'
#' plot(bx_BIC, interval_t = 0.8 ) # change the time interval of animation
#'
#' plot(bx_nonstop, cex_legend = 2, cex_text = 3) # change the magnification of legend and text
#'
#' # change the color
#' plot(bx_ADF,main = "dynamic graph with new color",col_raw = "#685F74", col_trend_h = "#39A1A8", col_trend_f = "#DD4B4F", col_pvalue_BIC = "#E96145")
#'
#' # change the transparency
#' plot(bx_ADF,main = "dynamic graph with new transparency setting",raw_alpha = 200, trend_h_alpha = 55, trend_f_alpha = 250, pvalue_BIC_alpha = 250)
#'
#' # Note: 'nonstop' bHP doesn't have dynamic figure
#' plot(bx_HP)
#' # Error in plot.bHP(bx_HP) :
#' #'nonstop-iter' bHP doesn't have dynamic picture: returning NA
#'
#' #--------- for JS style ----------
#'
#' plot(bx_ADF,plot_type = "JS")
#'
#' # change the color
#' plot(bx_ADF,plot_type = "JS",main = "Js graph with new color", col_raw = "#685F74", col_trend_f = "#DD4B4F", col_pvalue_BIC = "#39A1A8")
#'
#' plot(bx_BIC,plot_type = "JS")
#'
#' plot(bx_nonstop,plot_type = "JS")
#'
#' plot(bx_HP,plot_type = "JS")
#'
#' #--------- for static style ----------
#'
#' plot(bx_ADF,plot_type = "static",cex_legend = 0.7, cex_text = 0.8 )
#'
#' plot(bx_HP,plot_type = "static")
#'
#' plot(bx_BIC,plot_type = "static",cex_legend = 0.7, cex_text = 0.8 )
#'
#' plot(bx_nonstop,plot_type = "static",cex_legend = 0.8, cex_text = 0.8 )
#' }
#'
plot.bHP <-
  function(x,
           plot_type = "dynamic",
           interval_t = 0.3,
           ylab = "",
           col_raw = "#2D5375",
           col_trend_h = "#FBB545",
           col_trend_f = "red",
           col_pvalue_BIC = "red",
           # the range of alpha is 0-255
           raw_alpha = 255,
           trend_h_alpha = 75,
           trend_f_alpha = 255,
           pvalue_BIC_alpha = 255,
           legend_location = "upleft",
           iteration_location = "downright",
           cex_text = 1.7,
           cex_legend = 1.5,
           main = paste0("Figure of ", x$stopping, " bHP (", plot_type, ")")) {
    message(plot_type, " plot of ", x$stopping, " bHP")

    # load default par, for resetting
    data("def_par")

    location <- function(textinplot_location) {
      switch(
        textinplot_location,
        upright = c(xticks[length(xticks) - 2], yticks[length(yticks)]),
        downright = c(xticks[length(xticks) - 2], (yticks[1] + yticks[2]) /
                        2),
        upleft = c((2.5) * (xticks[1] + xticks[2]) / 3, yticks[length(yticks)]),
        downleft = c((2.5) * (xticks[1] + xticks[2]) / 3, (yticks[1] +
                                                             yticks[2]) / 2)
      )
    }

    # POSIXct (date/time) index

    if (plot_type == "static") {
      if (x$stopping == "nonstop-iter") {
        dev.next()
        par(def_par)
        #layout(matrix(1), widths = lcm(12), heights = lcm(12))
        par(las = 1)
        col_rgb <- as.numeric(col2rgb(col_raw))

        plot(
          x$raw_data,
          pch = 16,
          col = rgb(
            red = col_rgb[1],
            green = col_rgb[2],
            blue = col_rgb[3],
            alpha = raw_alpha,
            maxColorValue = 255
          )
          ,
          lwd = 1,
          ylab = ylab
        )

        col_rgb <- as.numeric(col2rgb(col_trend_f))
        col <-
          rgb(
            red = col_rgb[1],
            green = col_rgb[2],
            blue = col_rgb[3],
            alpha = trend_f_alpha,
            maxColorValue = 255
          )
        lines(x$trend ,
              type = "l",
              col = col,
              lwd = 2)

        x_l <- x$raw_data
        y_l <- NULL
        xy <- xy.coords(x_l, y_l, log = "")
        xy$xlab <- NULL

        #xlim_design <- range(xy$x[is.finite(xy$x)])
        #ylim_design <- range(xy$y[is.finite(xy$y)])

        localAxis <-
          function(..., col, bg, pch, cex, lty, lwd)
            Axis(...)
        xticks <- localAxis(xy$x, side = 1)
        yticks <- localAxis(xy$y, side = 2)
        # may try col = "gray90"
        abline(
          NULL,
          NULL,
          lty = 1,
          col = "gray80",
          lwd = .08,
          h = yticks
        )
        abline(
          NULL,
          NULL,
          lty = 1,
          col = "gray80",
          lwd = .08,
          h = NULL,
          v = xticks
        )

        # Title
        # mtext(main, side=3, line=1, cex=1.3, family="Consolas")
        mtext(main,
              side = 3,
              line = 1,
              cex = 1.3)


        # location of text

        temp <- try(location(iteration_location), silent = T)
        if ('try-error' %in% class(temp)) {
          locate_text <- as.numeric(iteration_location)
        } else{
          locate_text <- as.numeric(location(iteration_location))
        }


        # location of legend

        temp <- try(location(legend_location), silent = T)
        if ('try-error' %in% class(temp)) {
          locate_legend <- as.numeric(legend_location)
        } else{
          locate_legend <- as.numeric(location(legend_location))
        }

        # Text
        text(
          x = locate_text[1],
          y = locate_text[2],
          c("Iterated Only Once"),
          cex = cex_text,
          col = "black",
          font = 2
        )

        # Legend
        legend(
          x = locate_legend[1],
          y = locate_legend[2],
          c("Raw Data",  "Trend Component"),
          col = c(col_raw, col_trend_f),
          text.col = c(col_raw, col_trend_f),
          lty = c(-1,-1),
          pch = c(16,  15),
          text.font = 3,
          cex = cex_legend,
          bg = "white",
          bty = "n"
        )


      }

      if (x$stopping != "nonstop-iter") {
        dev.next()
        par(def_par)
        layout(matrix(c(1, 1, 2, 2), 2, 2, byrow = TRUE), c(6), c(4, 2), TRUE)
        par(mar = c(3, 4, 3, 3), las = 1)

        col_rgb <- as.numeric(col2rgb(col_raw))

        plot(
          x$raw_data,
          pch = 16,
          col = rgb(
            red = col_rgb[1],
            green = col_rgb[2],
            blue = col_rgb[3],
            alpha = raw_alpha,
            maxColorValue = 255
          )
          ,
          lwd = 1,
          ylab = ylab
        )

        for (i in 1:x$iter_num) {
          if (i == x$iter_num) {
            col_rgb <- as.numeric(col2rgb(col_trend_f))
            col <-
              rgb(
                red = col_rgb[1],
                green = col_rgb[2],
                blue = col_rgb[3],
                alpha = trend_f_alpha,
                maxColorValue = 255
              )
            lwd <- 1.6

          } else{
            col_rgb <- as.numeric(col2rgb(col_trend_h))
            col <-
              rgb(
                red = col_rgb[1],
                green = col_rgb[2],
                blue = col_rgb[3],
                alpha = trend_h_alpha,
                maxColorValue = 255
              )
            lwd <- 2
          }

          lines(x$trend_hist[, i],
                type = "l",
                col = col,
                lwd = lwd)
        }
        x_l <- x$raw_data
        y_l <- NULL
        xy <- xy.coords(x_l, y_l, log = "")
        xy$xlab <- NULL

        #xlim_design <- range(xy$x[is.finite(xy$x)])
        #ylim_design <- range(xy$y[is.finite(xy$y)])

        localAxis <-
          function(..., col, bg, pch, cex, lty, lwd)
            Axis(...)
        xticks <- localAxis(xy$x, side = 1)
        yticks <- localAxis(xy$y, side = 2)
        # may try col = "gray90"
        abline(
          NULL,
          NULL,
          lty = 1,
          col = "gray80",
          lwd = .08,
          h = yticks
        )
        abline(
          NULL,
          NULL,
          lty = 1,
          col = "gray80",
          lwd = .08,
          h = NULL,
          v = xticks
        )

        # Title
        # mtext(main, side=3, line=1, cex=1.3, family="Consolas")
        mtext(main,
              side = 3,
              line = 1,
              cex = 1.3)


        # location of text

        temp <- try(location(iteration_location), silent = T)
        if ('try-error' %in% class(temp)) {
          locate_text <- as.numeric(iteration_location)
        } else{
          locate_text <- as.numeric(location(iteration_location))
        }


        # location of legend

        temp <- try(location(legend_location), silent = T)
        if ('try-error' %in% class(temp)) {
          locate_legend <- as.numeric(legend_location)
        } else{
          locate_legend <- as.numeric(location(legend_location))
        }


        text(
          x = locate_text[1],
          y = locate_text[2],
          paste0("Iterated Times: ", as.character(c(1:x$iter_num)[i])),
          cex = cex_text,
          col = "black",
          font = 2
        )

        # Legend
        legend(
          x = locate_legend[1],
          y = locate_legend[2],
          c("Raw Data", "Trend History", "Final Trend"),
          col = c(col_raw, col_trend_h, col_trend_f),
          text.col = c(col_raw, col_trend_h, col_trend_f),
          lty = c(-1,-1,-1),
          pch = c(16, 15, 15),
          text.font = 3,
          cex = cex_legend,
          bg = "white",
          bty = "n"
        )

        if (x$stopping == "adf") {
          par(mar = c(3, 4, 3, 3), las = 1)

          col_rgb <- as.numeric(col2rgb(col_pvalue_BIC))
          col <-
            rgb(
              red = col_rgb[1],
              green = col_rgb[2],
              blue = col_rgb[3],
              alpha = pvalue_BIC_alpha,
              maxColorValue = 255
            )

          plot(
            x$adf_p_hist,
            main = c("p-value"),
            bty = "l",
            ylab = "",
            xlim = c(0, x$iter_num + 1),
            ylim = c(0, range(x$adf_p_hist)[2]),
            col = col,
            pch = 19
          )
          abline(h = x$signif_p ,  col = "gray50")
          text(0.5,
               (x$signif_p + 0.03),
               paste0("sig p = ", x$signif_p)  ,
               col = "gray50")
        }

        if (x$stopping == "BIC") {
          par(mar = c(3, 4, 3, 3), las = 1)

          col_rgb <- as.numeric(col2rgb(col_pvalue_BIC))
          col <-
            rgb(
              red = col_rgb[1],
              green = col_rgb[2],
              blue = col_rgb[3],
              alpha = pvalue_BIC_alpha,
              maxColorValue = 255
            )

          plot(
            x$BIC_hist,
            main = c("BIC value"),
            bty = "l",
            ylab = "",
            xlim = c(0, x$iter_num + 1),
            ylim = range(x$BIC_hist),
            col = col,
            pch = 19
          )
          abline(h = as.numeric(x$BIC_hist[x$iter_num]),  col = "gray50")
          abline(v = which.min(x$BIC_hist), col = "gray50")
          text(
            as.numeric(x$iter_num),
            as.numeric(x$BIC_hist[x$iter_num] + 0.05),
            paste0("min BIC(", x$iter_num, "th)")  ,
            col = "gray50"
          )
        }

        if (x$stopping == "nonstop") {
          par(mar = c(3, 4, 3, 3), las = 1)

          col_rgb <- as.numeric(col2rgb(col_pvalue_BIC))
          col <-
            rgb(
              red = col_rgb[1],
              green = col_rgb[2],
              blue = col_rgb[3],
              alpha = pvalue_BIC_alpha,
              maxColorValue = 255
            )

          plot(
            x$BIC_hist,
            main = c("BIC value"),
            bty = "l",
            ylab = "",
            xlim = c(0, x$iter_num + 1),
            ylim = c(0, range(x$BIC_hist)[2]),
            col = col,
            pch = 19
          )
          abline(h = min(as.numeric(x$BIC_hist)),  col = "gray50")
          abline(v = which.min(x$BIC_hist), col = "gray50")
          text(
            which.min(x$BIC_hist),
            as.numeric(min(x$BIC_hist)) - 0.05,
            paste0("minimal BIC(", which.min(x$BIC_hist), "th)")  ,
            col = "gray50"
          )
        }

      }


    }

    if (plot_type == "JS") {
      data_1 <-
        data.frame(
          date = 1:length(x$raw_data),
          raw_data = x$raw_data,
          trend_data = x$trend
        )
      require(magrittr)
      p1 <- plotly::plot_ly(data_1, x = ~ date, color = I(col_raw)) %>%
        plotly::add_lines(y = ~ raw_data,
                          name = "raw data",
                          legendgroup = "raw data") %>%
        plotly::add_lines(
          y = ~ trend_data,
          color = I(col_trend_f),
          name = "final trend",
          legendgroup = "final trend"
        ) %>%
        plotly::layout(
          showlegend = T,
          yaxis = list(title = ylab),
          title = main
        )

      if (x$stopping == "nonstop-iter") {
        return(p1)

      }

      if (x$stopping == "adf") {
        data_2 <-
          data.frame(iter_time = 1:x$iter_num,
                     p_value = x$adf_p_hist)
        p2 <-
          plotly::plot_ly(
            data_2,
            x = ~ iter_time,
            y = ~ p_value,
            color = I(col_pvalue_BIC),
            name = "p-value",
            legendgroup = "p-value"
          )

        return(plotly::subplot(
          p1,
          p2,
          heights = c(0.7, 0.3),
          nrows = 2,
          margin = 0.05
        ))
      }

      if (x$stopping == "BIC") {
        data_2 <-
          data.frame(iter_time = 1:x$iter_num,
                     BIC = x$BIC_hist[1:x$iter_num])

        p2 <-
          plotly::plot_ly(
            data_2,
            x = ~ iter_time,
            y = ~ BIC,
            color = I(col_pvalue_BIC),
            name = "BIC",
            legendgroup = "BIC"
          ) %>%
          plotly::layout(showlegend = T, yaxis = list(title = "BIC"))

        return(plotly::subplot(
          p1,
          p2,
          heights = c(0.7, 0.3),
          nrows = 2,
          margin = 0.05
        ))
      }

      if (x$stopping == "nonstop") {
        data_2 <-
          data.frame(iter_time = 1:x$iter_num,
                     BIC = x$BIC_hist[1:x$iter_num])

        p2 <-
          plotly::plot_ly(
            data_2,
            x = ~ iter_time,
            y = ~ BIC,
            color = I(col_pvalue_BIC),
            name = "BIC",
            legendgroup = "BIC"
          ) %>%
          plotly::layout(showlegend = T, yaxis = list(title = "BIC"))

        return(plotly::subplot(
          p1,
          p2,
          heights = c(0.7, 0.3),
          nrows = 2,
          margin = 0.05
        ))
      }

    }

    if (plot_type == "dynamic") {
      if (x$stopping == "nonstop-iter") {
        stop("'nonstop-iter' bHP doesn't have dynamic picture: returning NA")
        return(NA_real_)

      }

      if (x$stopping != "nonstop-iter") {
        cat(
          "It may take more seconds if iteration number is large.",
          "\n",
          "Please be patient!",
          "\n"
        )


        animation::saveGIF({
          for (i in 1:x$iter_num) {
            layout(
              matrix(c(1, 1, 2, 2), 2, 2, byrow = TRUE),
              widths = lcm(5),
              heights = lcm(16)
            )
            par(mar = c(3, 4, 5, 3), las = 1)

            col_rgb <- as.numeric(col2rgb(col_raw))

            plot(
              x$raw_data,
              pch = 16,
              col = rgb(
                red = col_rgb[1],
                green = col_rgb[2],
                blue = col_rgb[3],
                alpha = raw_alpha,
                maxColorValue = 255
              )
              ,
              lwd = 1,
              ylab = ylab
            )

            for (j in 1:i) {
              if (j == x$iter_num) {
                col_rgb <- as.numeric(col2rgb(col_trend_f))
                col <-
                  rgb(
                    red = col_rgb[1],
                    green = col_rgb[2],
                    blue = col_rgb[3],
                    alpha = trend_f_alpha,
                    maxColorValue = 255
                  )
                lwd <- 1.6

              } else{
                col_rgb <- as.numeric(col2rgb(col_trend_h))
                col <-
                  rgb(
                    red = col_rgb[1],
                    green = col_rgb[2],
                    blue = col_rgb[3],
                    alpha = trend_h_alpha,
                    maxColorValue = 255
                  )
                lwd <- 2
              }

              lines(
                x$trend_hist[, j],
                type = "l",
                col = col,
                lwd = lwd
              )

              x_l <- x$raw_data
              y_l <- NULL
              xy <- xy.coords(x_l, y_l, log = "")
              xy$xlab <- NULL

              #xlim_design <- range(xy$x[is.finite(xy$x)])

              #ylim_design <- range(xy$y[is.finite(xy$y)])

              localAxis <-
                function(..., col, bg, pch, cex, lty, lwd)
                  Axis(...)
              xticks <- localAxis(xy$x, side = 1)
              yticks <- localAxis(xy$y, side = 2)
              # may try col = "gray90"
              abline(
                NULL,
                NULL,
                lty = 1,
                col = "gray80",
                lwd = .08,
                h = yticks
              )
              abline(
                NULL,
                NULL,
                lty = 1,
                col = "gray80",
                lwd = .08,
                h = NULL,
                v = xticks
              )

              # Title
              # mtext(main, side=3, line=1, cex=1.3, family="Consolas")
              mtext(main,
                    side = 3,
                    line = 1,
                    cex = 1.3)


            }

            # location of text

            temp <- try(location(iteration_location), silent = T)
            if ('try-error' %in% class(temp)) {
              locate_text <- as.numeric(iteration_location)
            } else{
              locate_text <- as.numeric(location(iteration_location))
            }



            # location of legend

            temp <- try(location(legend_location), silent = T)
            if ('try-error' %in% class(temp)) {
              locate_legend <- as.numeric(legend_location)
            } else{
              locate_legend <- as.numeric(location(legend_location))
            }


            text(
              x = locate_text[1],
              y = locate_text[2],
              paste0("Iterated Times: ", as.character(c(
                1:x$iter_num
              )[i])),
              cex = cex_text,
              col = "black",
              font = 2
            )

            # Legend
            legend(
              x = locate_legend[1],
              y = locate_legend[2],
              c("Raw Data", "Trend History", "Final Trend"),
              col = c(col_raw, col_trend_h, col_trend_f),
              text.col = c(col_raw, col_trend_h, col_trend_f),
              lty = c(-1,-1,-1),
              pch = c(16, 15, 15),
              text.font = 3,
              cex = cex_legend,
              bg = "white",
              bty = "n"
            )

            if (x$stopping == "adf") {
              par(mar = c(3, 4, 3, 3), las = 1)

              col_rgb <- as.numeric(col2rgb(col_pvalue_BIC))
              col <-
                rgb(
                  red = col_rgb[1],
                  green = col_rgb[2],
                  blue = col_rgb[3],
                  alpha = pvalue_BIC_alpha,
                  maxColorValue = 255
                )

              plot(
                x$adf_p_hist[1:i],
                main = paste0("p-value = ", round(x$adf_p_hist[i], 4)),
                bty = "l",
                ylab = "",
                xlim = c(0, x$iter_num + 1),
                ylim = c(0, range(x$adf_p_hist)[2]),
                col = col,
                pch = 19
              )
              abline(h = x$signif_p ,  col = "gray50")
              text(0.5,
                   (x$signif_p + 0.03),
                   paste0("sig_p = ", x$signif_p)  ,
                   col = "gray50")
            }

            if (x$stopping == "BIC") {
              par(mar = c(3, 4, 3, 3), las = 1)

              col_rgb <- as.numeric(col2rgb(col_pvalue_BIC))
              col <-
                rgb(
                  red = col_rgb[1],
                  green = col_rgb[2],
                  blue = col_rgb[3],
                  alpha = pvalue_BIC_alpha,
                  maxColorValue = 255
                )

              plot(
                x$BIC_hist[1:i],
                main = paste0("BIC value = ", round(x$BIC_hist[i], 4)),
                bty = "l",
                ylab = "",
                xlim = c(0, x$iter_num + 1),
                ylim = c(0, range(x$BIC_hist)[2]),
                col = col,
                pch = 19
              )
              abline(h = as.numeric(x$BIC_hist[x$iter_num]),  col = "gray50")
              text(
                as.numeric(x$iter_num),
                as.numeric(x$BIC_hist[x$iter_num] - 0.3),
                paste0("min BIC(", x$iter_num, "th)")  ,
                col = "gray50"
              )
            }

            if (x$stopping == "nonstop") {
              par(mar = c(3, 4, 3, 3), las = 1)

              col_rgb <- as.numeric(col2rgb(col_pvalue_BIC))
              col <-
                rgb(
                  red = col_rgb[1],
                  green = col_rgb[2],
                  blue = col_rgb[3],
                  alpha = pvalue_BIC_alpha,
                  maxColorValue = 255
                )

              plot(
                x$BIC_hist[1:i],
                main = paste0("BIC value = ", round(x$BIC_hist[i], 4)),
                bty = "l",
                ylab = "",
                xlim = c(0, x$iter_num + 1),
                ylim = c(0, range(x$BIC_hist)[2]),
                col = col,
                pch = 19
              )
              abline(h = min(as.numeric(x$BIC_hist)),  col = "gray50")
              abline(v = which.min(x$BIC_hist), col = "gray50")
              text(
                which.min(x$BIC_hist),
                as.numeric(min(x$BIC_hist)) - 0.3,
                paste0("minimal BIC(", which.min(x$BIC_hist), "th)")  ,
                col = "gray50"
              )
            }

          }

        }, movie.name = "bHP_ani.gif" , interval = interval_t, nmax = as.numeric(x$iter_num),
        ani.width = 800, ani.height = 600)
      }


    }

  }
