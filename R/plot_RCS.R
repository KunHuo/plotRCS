plot_RCS <- function(data,
                     outcome = NULL,
                     time = NULL,
                     exposure = NULL,
                     covariates = NULL,
                     positive = NULL,
                     group = NULL,
                     knots = knots(),
                     ref.knot = 1,
                     ref.value = NULL,
                     show.pvalue = TRUE,
                     pvalue.digits = 3,
                     pvalue.position = c(0, 1),
                     fontsize = 12,
                     fontfamily = "serif",
                     linesize = 0.25,
                     linecolor = "red", ...){

  outcome    <- select_variable(data, outcome)
  exposure   <- select_variable(data, exposure)
  covariates <- select_variable(data, covariates)
  if(!is.null(time)){
    time <- select_variable(data, time)
  }

  if(is.null(group)){
    group <- select_variable(data, group)
  }

  if(is.null(positive)){
    if(is.numeric(data[[outcome]])){
      positive <- max(data[[outcome]])
    }else if(is.factor(data[[outcome]])){
      positive <- levels(data[[outcome]])[2]
    }else{
      stop("You need to specify the positive event of outcome.", call. = FALSE)
    }
  }
  data[[outcome]] <- ifelse(data[[outcome]] == positive, 1, 0)

  pos <- 1
  envir = as.environment(pos)
  assign("ddist_", rms::datadist(data), envir = envir)
  options(datadist = "ddist_")

  assign("m_", stats::quantile(data[[exposure]], knots), envir = envir)

  if(is.null(ref.value)){
    ddist_$limits["Adjust to", exposure] <<- m_[ref.knot]
  }else{
    ddist_$limits["Adjust to", exposure] <<- ref.value
  }

  if(is.null(time)){
    formula <- sprintf("%s ~ rms::rcs(%s, m_)", outcome, exposure)
    if(length(covariates) != 0){
      formula <- paste(formula, paste(covariates, collapse = " + "), sep = " + ")
    }
    formula <- stats::as.formula(formula)
    model <- rms::lrm(formula = formula, data = data)
  }else{
    formula <- sprintf("survival::Surv(%s, %s) ~ rms::rcs(%s, m_)", time, outcome, exposure)
    if(length(covariates) != 0){
      formula <- paste(formula, paste(covariates, collapse = " + "), sep = " + ")
    }
    formula <- stats::as.formula(formula)
    model <- rms::cph(formula = formula, data = data)
  }

  eval.text <- sprintf("rms::Predict(model, %s, ref.zero = TRUE, fun = exp)", exposure)
  plotdata <- eval(parse(text = eval.text))
  plotdata <- as.data.frame(plotdata)

  if(exists("ddist_")){
    rm("ddist_", inherits = TRUE, envir = envir)
  }

  if(exists("m_")){
    rm("m_", inherits = TRUE, envir = envir)
  }

  plotdata
  #
  # if (plot) {
  #   xbreaks <- pretty(plotdata[[exposure]])
  #   ybreaks <- pretty(c(0, plotdata$upper))
  #   xlabels <- attr(data[[exposure]], "label")
  #
  #   if(is.null(xlabels)){
  #     xlabels <- exposure
  #   }
  #
  #   if(is.null(time)){
  #     ylab <- "Odds ratio"
  #   }else{
  #     ylab <- "Hazard ratio"
  #   }
  #
  #   plot <- ggplot2::ggplot(plotdata) +
  #     ggplot2::geom_line(ggplot2::aes_string(x = exposure, y = "yhat"), color = color, size = line.size) +
  #     ggplot2::geom_ribbon(ggplot2::aes_string(exposure, ymin = "lower", ymax = "upper"), alpha = 0.1, fill = color) +
  #     ggplot2::geom_hline(yintercept = 1, linetype = 2, size = line.size) +
  #     gp_theme_sci(line.size = line.size, font.size = font.size, font.family = font.family, line.color = line.color, aspect.ratio = aspect.ratio) +
  #     ggplot2::xlab(xlabels) +
  #     ggplot2::ylab(ylab) +
  #     ggplot2::coord_cartesian(expand = FALSE) +
  #     ggplot2::scale_x_continuous(breaks = xbreaks, limits = c(min(xbreaks), max(xbreaks))) +
  #     ggplot2::scale_y_continuous(breaks = ybreaks, limits = c(min(ybreaks), max(ybreaks)))
  #
  #   if(show.pvalue){
  #     pdata <- stats::anova(model)
  #     pdata <- as.data.frame(pdata)
  #     pvalue <- pdata[2, 3]
  #     p.overall <- pdata[1, 3]
  #
  #     pvalue <- format_pvalue(pvalue, digits = pvalue.digits)
  #     if(!regex_detect(pvalue, "<", fixed = TRUE)){
  #       pvalue <- paste0("P for nonlinear = ", pvalue)
  #     }else{
  #       pvalue <- regex_replace(pvalue, "<", replacement = "", fixed = TRUE)
  #       pvalue <- paste0("P for nonlinear < ", pvalue)
  #     }
  #
  #     p.overall <- format_pvalue(p.overall, digits = pvalue.digits)
  #     if(!regex_detect(p.overall, "<", fixed = TRUE)){
  #       p.overall <- paste0("P for overall association = ", p.overall)
  #     }else{
  #       p.overall <- regex_replace(p.overall, "<", replacement = "", fixed = TRUE)
  #       p.overall <- paste0("P for overall association < ", p.overall)
  #     }
  #
  #     p.string <- paste(p.overall, pvalue, sep = "\n")
  #
  #
  #     if(is.null(pvalue.position)){
  #       px <- min(xbreaks) + max(xbreaks) / 30
  #       py <- max(ybreaks) - max(ybreaks) / 8
  #     }else{
  #       px <- pvalue.position[1]
  #       py <- pvalue.position[2]
  #     }
  #     plot + gp_drawlabel(p.string, font.size = font.size, font.family = font.family, x = px, y = py)
  #
  #   }else{
  #     plot
  #   }
  # }else{
  #   invisible(plotdata)
  # }
}
