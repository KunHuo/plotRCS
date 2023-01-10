#' Plot restricted cubic splines curves
#'
#' @description
#' Drawing of restricted cubic spline (RCS) curves form a logistic
#' regression model or a  Cox proportional hazards regression model.
#'
#' @param data a data frame contain the columns of outcome, time, exposure,
#' covariates, and group.
#' @param outcome the name of outcome variable in the data.
#' @param time the name of time variable in the data, for Cox regressions.
#' @param exposure the name of exposure variable in the data.
#' @param covariates the names of covariate variables in the data.
#' @param positive in which positive of outcome variable to make the comparison.
#' By default, positive is automatically defined. If outcome is a factor variable,
#' then positive is defined as the highest level. If outcome is a numerical
#' variable, then positive is defined as the largest value.
#' @param group the name of group variable in the data.
#' @param knots location of knots, detail see [knot] function.
#' @param ref.value referrence value for the RCS curve, 'min' means using the
#' minimum value of esposure as a reference, 'median' uses the median, 'mean'
#' uses the mean, 'k1' uses the first knot. 'k2' uses the second knot, 'k3'
#' uses the third 'knot', and so on. In addition, you can directly set the
#' numerical vector as the reference value.
#' @param conf.int logical indicating whether or not to draw confidence interval.
#' Defaults to TRUE.
#' @param conf.level the confidence level to use for the confidence interval if
#' conf.int = TRUE. Must be strictly greater than 0 and less than 1. Defaults to
#' 0.95, which corresponds to a 95 percent confidence interval.
#' @param conf.type confidence interval type of 'shape' (default) or 'line'.
#' @param pvalue logical indicating whether or not to show P values,
#' include P for overall association and P for nonlinear, default TRUE.
#' @param pvalue.digits digits for P values, default 3.
#' @param pvalue.position position for P value, numeric vector of length two (x-axis and y-axis).
#' @param fontsize font size, default 12.
#' @param fontfamily font family, default 'serif' (Times New Roman).
#' @param linesize line size, default 0.25.
#' @param linecolor line color, default '#0072B5FF'.
#' @param alpha alpha for the shape of confidence interval, default 0.1.
#' @param xbreaks breaks of x-axis.
#' @param ybreaks breaks of y-axis.
#' @param explain logical indicating whether or not to explain the figure, default TRUE.
#' @param ... further arguments.
#'
#' @seealso [rms::rcs]
#'
#' @references
#' Harrell, Frank E. Regression modeling strategies: with applications to linear
#' models, logistic regression, and survival analysis. Vol. 608. New York:
#' springer, 2001.
#'
#' @return a ggplot2 object.
#' @export
#'
#' @examples
#' \dontrun{
#' # aps dataset from aplore3 package
#' install.packages(aplore3)
#' library(aplore3)
#'
#' # RCS curves for logistic
#' rcsplot(data = aps,
#'         outcome = "elope",
#'         exposure = "age",
#'         covariates = c("race", "gender"))
#'
#' # Set 5 knots.
#' rcsplot(data = aps,
#'         outcome = "elope",
#'         exposure = "age",
#'         covariates = c("race", "gender"),
#'         knots = konts(5))
#'
#' # Set minimum value of exposure as reference value.
#' rcsplot(data = aps,
#'         outcome = "elope",
#'         exposure = "age",
#'         covariates = c("race", "gender"),
#'         knots = konts(5),
#'         ref.value = "min")
#'
#' # Plot by group
#' rcsplot(data = aps,
#'         outcome = "elope",
#'         exposure = "age",
#'         covariates = c("race", "gender"),
#'         group = "gender")
#'
#' # lung dataset from survival package
#' library(survival)
#'
#' # RCS curves for Cox
#' rcsplot(data = lung,
#'         outcome = "status",
#'         time = "time",
#'         exposure = "age",
#'         covariates = c("sex", "ph.ecog"))
#' }
rcsplot <- function(data,
                    outcome = NULL,
                    time = NULL,
                    exposure = NULL,
                    covariates = NULL,
                    positive = NULL,
                    group = NULL,
                    knots = c(0.05, 0.35, 0.65, 0.95),
                    ref.value = "k1",
                    conf.int = TRUE,
                    conf.level = 0.95,
                    conf.type = c("shape", "line"),
                    pvalue = TRUE,
                    pvalue.digits = 3,
                    pvalue.position = c(0.02, 0.98),
                    fontsize = 12,
                    fontfamily = "serif",
                    linesize = 0.25,
                    linecolor = "#0072B5FF",
                    alpha = 0.1,
                    xbreaks = NULL,
                    ybreaks = NULL,
                    explain = TRUE,
                    ...) {

  # Select variables and check
  outcome    <- select_variable(data, outcome)
  exposure   <- select_variable(data, exposure)
  if(!is.null(time)){
    time <- select_variable(data, time)
  }
  if(is.null(group)){
    group <- select_variable(data, group)
  }
  if(!is.null(covariates)){
    covariates <- select_variable(data, covariates)
    covariates <- setdiff(covariates, outcome)
    covariates <- setdiff(covariates, exposure)
    covariates <- setdiff(covariates, time)
  }

  # Set positive event
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

  # Set data to environments
  pos <- 1
  envir = as.environment(pos)
  assign("ddist_", rms::datadist(data), envir = envir)
  options(datadist = "ddist_")

  # Set reference
  assign("m_", stats::quantile(data[[exposure]], knots), envir = envir)
  if(is.character(ref.value)){
    if(ref.value == "min"){
      ref.value1 <- min(data[[exposure]], na.rm = TRUE)
    }else if(ref.value == "median"){
      ref.value1 <- stats::median(data[[exposure]], na.rm = TRUE)
    }else if(ref.value == "mean"){
      ref.value1 <- mean(data[[exposure]], na.rm = TRUE)
    }else{
      if(regex_detect(ref.value, pattern = "^k\\d+", ignore.case = TRUE)){
        ref.value1 <- regex_extract(ref.value, pattern = "\\d+")
        ref.value1 <- as.numeric(ref.value1)
        ref.value1 <- m_[ref.value1]
      }else{
        stop("Reference knot must start with k.", call. = FALSE)
      }
    }
  }else{
    ref.value1 <- ref.value
  }
  eval(parse(text = "ddist_$limits['Adjust to', exposure] <<- ref.value1"))

  # Fit models
  if(is.null(time)){
    formula <- sprintf("%s ~ rms::rcs(%s, m_)", outcome, exposure)
    if(length(covariates) != 0){
      formula <- paste(formula, paste(covariates, collapse = " + "), sep = " + ")
    }
    formula <- stats::as.formula(formula)
    model   <- rms::lrm(formula = formula, data = data)
  }else{
    formula <- sprintf("survival::Surv(%s, %s) ~ rms::rcs(%s, m_)", time, outcome, exposure)
    if(length(covariates) != 0){
      formula <- paste(formula, paste(covariates, collapse = " + "), sep = " + ")
    }
    formula <- stats::as.formula(formula)
    model   <- rms::cph(formula = formula, data = data)
  }

  # Check conf.level.
  if(conf.level < 0 | conf.level > 1){
    stop("The conf.level must be strictly between 0 and 1.", call. = FALSE)
  }

  # Get plotdata from models
  if(is.null(group)){
    eval.text <- sprintf("rms::Predict(model, %s, conf.int = %f, ref.zero = TRUE, fun = exp)",
                         exposure,
                         conf.level)
  }else{
    eval.text <- sprintf("rms::Predict(model, %s, %s, conf.int = %f, ref.zero = TRUE, fun = exp)",
                         exposure,
                         group,
                         conf.level)
  }
  plotdata <- eval(parse(text = eval.text))
  plotdata <- as.data.frame(plotdata)

  # Delete data from environments
  if(exists("ddist_")){
    rm("ddist_", inherits = TRUE, envir = envir)
  }
  if(exists("m_")){
    rm("m_", inherits = TRUE, envir = envir)
  }

  # Set breaks for x-axis or y-axis
  if(is.null(xbreaks)){
    xbreaks <- pretty(plotdata[[exposure]])
  }
  if(is.null(ybreaks)){
    if(conf.int){
      ybreaks <- pretty(c(0, plotdata$upper))
    }else{
      ybreaks <- pretty(c(0, plotdata$yhat))
    }
  }

  # Set labels for x-axis or y-axis
  xlabels <- attr(data[[exposure]], "label")
  if (is.null(xlabels)) {
    xlabels <- exposure
  }
  if (is.null(time)) {
    ylab <- "Odds ratio"
    if(conf.int){
      ylab <- sprintf("%s (%s%% CI)", ylab, as.character(conf.level * 100))
    }
  } else{
    ylab <- "Hazard ratio"
    if(conf.int){
      ylab <- sprintf("%s (%s%% CI)", ylab, as.character(conf.level * 100))
    }
  }

  conf.type <- match.arg(conf.type)

  # Plot by ggplot2
  if (is.null(group)) {
    plot <- ggplot2::ggplot(plotdata) +
      ggplot2::geom_line(ggplot2::aes_string(x = exposure, y = "yhat"),
                         color = linecolor,
                         size = linesize)
    if(conf.int){
      if(conf.type == "shape"){
        plot <- plot +
          ggplot2::geom_ribbon(ggplot2::aes_string(exposure, ymin = "lower", ymax = "upper"),
                               alpha = alpha,
                               fill = linecolor)
      }else{
        plot <- plot +
          ggplot2::geom_line(ggplot2::aes_string(x = exposure, y = "lower"),
                             color = linecolor,
                             size = linesize,
                             linetype = 2) +
          ggplot2::geom_line(ggplot2::aes_string(x = exposure, y = "upper"),
                             color = linecolor,
                             size = linesize,
                             linetype = 2)
      }
    }
  } else{
    plot <- ggplot2::ggplot(plotdata) +
      ggplot2::geom_line(ggplot2::aes_string(x = exposure, y = "yhat", color = group),
                         size = linesize)
    if(conf.int){
      if(conf.type == "shape"){
        plot <- plot +
          ggplot2::geom_ribbon(ggplot2::aes_string(exposure, ymin = "lower", ymax = "upper", fill = group),
                               alpha = alpha)
      }else{
        plot <- plot +
          ggplot2::geom_line(ggplot2::aes_string(x = exposure, y = "lower", color = group),
                             size = linesize,
                             linetype = 2) +
          ggplot2::geom_line(ggplot2::aes_string(x = exposure, y = "upper", color = group),
                             size = linesize,
                             linetype = 2)
      }
    }
  }
  plot <- plot +
    ggplot2::geom_hline(yintercept = 1, linetype = 2, size = linesize) +
    gg_theme_sci(font.size = fontsize, font.family = fontfamily) +
    ggplot2::xlab(xlabels) +
    ggplot2::ylab(ylab) +
    ggplot2::coord_cartesian(expand = FALSE) +
    ggplot2::scale_x_continuous(breaks = xbreaks, limits = c(min(xbreaks), max(xbreaks))) +
    ggplot2::scale_y_continuous(breaks = ybreaks, limits = c(min(ybreaks), max(ybreaks)))

  # Show P value
  if (pvalue) {
    pdata  <- stats::anova(model)
    pdata  <- as.data.frame(pdata)
    p.value <- pdata[2, 3]
    p.overall <- pdata[1, 3]

    p.value <- format_pvalue(p.value, digits = pvalue.digits)
    if (!regex_detect(p.value, "<", fixed = TRUE)) {
      p.value <- paste0("P for nonlinear = ", p.value)
    } else{
      p.value <- regex_replace(p.value, "<", replacement = "", fixed = TRUE)
      p.value <- paste0("P for nonlinear < ", p.value)
    }

    p.overall <- format_pvalue(p.overall, digits = pvalue.digits)
    if (!regex_detect(p.overall, "<", fixed = TRUE)) {
      p.overall <- paste0("P for overall = ", p.overall)
    } else{
      p.overall <-
        regex_replace(p.overall, "<", replacement = "", fixed = TRUE)
      p.overall <-
        paste0("P for overall < ", p.overall)
    }

    p.string <- paste(p.overall, p.value, sep = "\n")

    # Check p value position.
    if(length(pvalue.position) != 2L){
      stop("The pvalue.position must of length 2.", call. = FALSE)
    }
    if(pvalue.position[1] < 0 | pvalue.position[1] > 1){
      stop("The first element of pvalue.position must be strictly between 0 and 1.", call. = FALSE)
    }
    if(pvalue.position[2] < 0 | pvalue.position[2] > 1){
      stop("The Second element of pvalue.position must be strictly between 0 and 1.", call. = FALSE)
    }

    px <-  min(xbreaks) + (max(xbreaks) - min(xbreaks)) * pvalue.position[1]
    py <-  min(ybreaks) + (max(ybreaks) - min(ybreaks)) * pvalue.position[2]

    plot <- plot + draw_label(p.string,
                      size = fontsize,
                      fontfamily = fontfamily,
                      x = px,
                      y = py,
                      hjust = 0,
                      vjust = 1)
  }

  # Explain the figures, title and note.
  if(explain){
    title <- sprintf("Figure: Association Between %s and %s Using a Restricted Cubic Spline Regression Model.",
                     exposure,
                     outcome)
    note  <- sprintf("Graphs show %s for %s according to %s",
                     ifelse(is.null(time), "ORs", "HRs"),
                     outcome,
                     exposure)

    if(is.null(covariates)){
      note <- paste0(note, ".")
    }else{
      note <- sprintf("%s adjusted for %s.", note, paste(covariates, collapse = ", "))
    }

    if(is.null(time)){
      note <- paste(note, "Data were fitted by a logistic regression model,", sep = " ")
    }else{
      note <- paste(note, "Data were fitted by a restricted cubic spline Cox proportional hazards regression model,", sep = " ")
    }

    if(is.character(ref.value)){
      if(ref.value == "min"){
        reference <- "the minimum"
      }else if(ref.value == "median"){
        reference <- "the median"
      }else if(ref.value == "mean"){
        reference <- "mean"
      }else{
        reference <- regex_extract(ref.value, pattern = "\\d+")
        reference <-  paste("the", paste0(knots * 100, "th")[as.numeric(reference)], "percentile", sep = " ")
      }
    }else{
      reference <- ref.value
    }

    tmp <- sprintf("and the model was conducted with %d knots at the %s percentiles of %s (reference is %s).",
                   length(knots),
                   paste(paste0(knots * 100, "th"), collapse = ", "),
                   exposure,
                   reference)
    note <- paste(note, tmp, sep = " ")

    # The range of TSH was restricted to 0.34 to 7.5 mIU/L because predictions
    #  greater than 7.5 mIU/L (95th percentile) are based on too few data points
    # note <- paste(note, sprintf("The %s ranges from %.1f to %.1f.",
    #                             exposure,
    #                             min(data[[exposure]], na.rm = TRUE),
    #                             max(data[[exposure]], na.rm = TRUE)), sep = " ")

    note <- paste(note, sprintf("Solid lines indicate %s, and %s indicate %s%% CIs.",
                                ifelse(is.null(time), "ORs", "HRs"),
                                ifelse(conf.type == "shape", "shadow shape", "dashed lines"),
                                as.character(conf.level * 100)), sep = " ")
    note <- paste(note,
                  ifelse(is.null(time),
                         "OR, odds ratio; CI, confidence interval.",
                         "HR, hazard ratio; CI, confidence interval." ),
                  sep = " ")

    attr(plot, "title") <- title
    attr(plot, "note")  <- note
  }

  attr(plot, "explain")  <- explain
  class(plot) <- c("rcsplot", class(plot))

  plot
}

