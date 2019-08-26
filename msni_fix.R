msni <- function (education_lsg, fsl_lsg, health_lsg, protection_lsg, 
          shelter_lsg, wash_lsg, capacity_gaps, impact, ...) 
{
  inputs <- list(education_lsg = education_lsg, fsl_lsg = fsl_lsg, 
                 health_lsg = health_lsg, protection_lsg = protection_lsg, 
                 shelter_lsg = shelter_lsg, wash_lsg = wash_lsg, capacity_gaps = capacity_gaps, 
                 impact = impact, ...)
  factorerror <- function(name) {
    paste(name, "must not be a factor. Use as.numeric() to convert _factor level indexes_ to numbers; use as.numeric(as.character()) to convert factors with numbers as labels ")
  }
  assertthat::assert_that(!is.factor(education_lsg), msg = factorerror(education_lsg))
  assertthat::assert_that(!is.factor(fsl_lsg), msg = factorerror(fsl_lsg))
  assertthat::assert_that(!is.factor(health_lsg), msg = factorerror(health_lsg))
  assertthat::assert_that(!is.factor(protection_lsg), msg = factorerror(protection_lsg))
  assertthat::assert_that(!is.factor(shelter_lsg), msg = factorerror(shelter_lsg))
  assertthat::assert_that(!is.factor(wash_lsg), msg = factorerror(wash_lsg))
  assertthat::assert_that(!is.factor(capacity_gaps), msg = factorerror(capacity_gaps))
  assertthat::assert_that(!is.factor(impact), msg = factorerror(capacity_gaps))
  assertthat::assert_that(typeof(education_lsg) %in% c("numeric", 
                                                       "double", "integer"), msg = ("education_lsg must be a numeric vector"))
  assertthat::assert_that(typeof(fsl_lsg) %in% c("numeric", 
                                                 "double", "integer"), msg = ("fsl_lsg must be numeric"))
  assertthat::assert_that(typeof(health_lsg) %in% c("numeric", 
                                                    "double", "integer"), msg = ("health_lsg must be a numeric vector"))
  assertthat::assert_that(typeof(protection_lsg) %in% c("numeric", 
                                                        "double", "integer"), msg = ("protection_lsg a numeric vector"))
  assertthat::assert_that(typeof(capacity_gaps) %in% c("numeric", 
                                                       "double", "integer"), msg = ("capacity_gaps must be a numeric vector"))
  assertthat::assert_that(typeof(wash_lsg) %in% c("numeric", 
                                                  "double", "integer"), msg = ("wash_lsg must be numeric"))
  assertthat::assert_that(typeof(impact) %in% c("numeric", 
                                                "double", "integer"), msg = ("wash_lsg must be numeric"))
  custom_input_indices <- list(...)
  purrr::walk2(custom_input_indices, names(custom_input_indices), 
               function(x, varname) {
                 if (is.factor(x)) {
                   stop(factorerror(varname))
                 }
                 if (!(typeof(x) %in% c("numeric", "double", "integer"))) {
                   stop(paste(varname, " must be numeric"))
                 }
               })
  inputs_numeric <- inputs %>% lapply(is.numeric) %>% unlist
  inputs_numeric_names <- inputs_numeric[!inputs_numeric] %>% 
    names
  if (length(inputs_numeric_names) != 0) {
    stop(paste("inputs must be numeric:", paste(inputs_numeric_names, 
                                                collapse = ", ")))
  }
  inputs_range <- inputs %>% lapply(function(x) {
    x <- x[!(is.na(x))]
    all(x >= 0) & all(x <= 5)
  }) %>% unlist
  inputs_range_names <- inputs_range[!inputs_range] %>% names
  if (length(inputs_range_names) != 0) {
    stop(paste("inputs are not in range 0-5:", paste(inputs_range_names, 
                                                     collapse = ", ")))
  }
  all_same_length <- inputs %>% lapply(length) %>% unlist %>% 
    (function(x) {
      length(unique(x)) == 1
    })
  if (!all_same_length) {
    stop("all inputs must be vectors of the same length")
  }
  msni <- pmax(health_lsg, protection_lsg, shelter_lsg)
  msni <- pmin(msni, impact)
  largest_lsg_combo <- pmax(pmin(health_lsg, protection_lsg), 
                            pmin(health_lsg, shelter_lsg), pmin(shelter_lsg, protection_lsg))
  msni <- pmax(msni, largest_lsg_combo)
  msni <- pmax(msni, wash_lsg, fsl_lsg, capacity_gaps)
  max_of_remaining <- c(custom_input_indices, list(health_lsg, 
                                                   protection_lsg, shelter_lsg, education_lsg)) %>% as.data.frame(stringsAsFactors = FALSE) %>% 
    apply(1, max, na.rm = FALSE)
  max_of_remaining_larger_equal_3 <- max_of_remaining >= 3
  note_applies <- (msni == 1) & max_of_remaining_larger_equal_3
  msni[note_applies] <- 2
  msni
}
