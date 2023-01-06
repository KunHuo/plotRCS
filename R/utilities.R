select_variable <- function(data, ..., type = c("name", "data", "index")){

  type  <- match.arg(type)

  if(length(c(...)) == 0L){
    return(NULL)
  }

  index <- .col_index(data, ...)

  if(length(index) == 0L){
    return(NULL)
  }

  switch(type,
         data  = data[index],
         name  = {
           varname <- names(data)[index]
           names(varname) <- varname
           varname
         },
         index = index)
}

.col_index <- function(data, ...){
  varnames <- list(...)
  res <- lapply(varnames, function(x){
    if(is.numeric(x)){
      if(max(x) > ncol(data) | min(x) <= 0){
        stop("Out of range for column index.", call. = FALSE)
      }
      x
    }else{
      sapply(x, function(i){
        if(regex_detect(i, pattern = ":", fixed = TRUE)){
          st <- regex_split(i, pattern = ":", fixed = TRUE)[[1]]
          check_name(data, st[1])
          check_name(data, st[2])
          start <- which(names(data) == st[1])
          end   <- which(names(data) == st[2])
          start:end
        }else{
          check_name(data, i)
          which(names(data) == i)
        }
      })
    }
  })
  res <- unique(unlist(res))
  names(res) <- names(data)[res]
  res
}

check_name <- function(data, varnames){
  tmp <- varnames %in% names(data)
  if(!all(tmp)){
    tmpname <- varnames[!tmp]
    tmpname <- paste(tmpname, collapse = ", ")
    message <- sprintf("%s are (is) not included in the data frame.", tmpname)
    stop(message, call. = FALSE)
  }
}


regex_replace <- function(string,
                          pattern,
                          replacement,
                          ignore.case = FALSE,
                          perl = FALSE,
                          fixed = FALSE,
                          useBytes = FALSE){
  sub(pattern = pattern,
      replacement = replacement,
      x = string,
      ignore.case = ignore.case,
      perl = perl,
      fixed = fixed,
      useBytes = useBytes)
}

regex_split <- function(string,
                        pattern,
                        perl = FALSE,
                        fixed = FALSE,
                        useBytes = FALSE){
  strsplit(
    string,
    pattern,
    perl = perl,
    fixed = fixed,
    useBytes = useBytes)
}
