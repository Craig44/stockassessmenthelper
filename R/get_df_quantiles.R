#' get_df_quantiles
#' get quanitles from a data.frame for 'y_value', grouped by group_vars using purrr and dplyr
#' @param df a dataframe with columns in group_vars and y_value
#' @param group_vars a vector of strings specifying the grouping variables 
#' @param y_value a string specifying the column to calculate the quanitles for
#' @param quants numeric vector of values between 0-1 which define the quantiles to calculate.
#' @return a dataframe of quantiles 
#' @importFrom purrr map_chr partial set_names
#' @importFrom dplyr group_by summarize_at %>% 
#' @export
get_df_quantiles <- function(df, group_vars, y_value, quants = c(0.025, 0.5, 0.975)) {
  if(!all(group_vars %in% colnames(df)))
    stop("could not find all 'group_vars' in column names of 'df'.")
  if(!(y_value %in% colnames(df)))
    stop("could not find all 'y_value' in column names of 'df'.")
  if(any(quants < 0))
    stop("Found 'quants' < 0 this is not allowed.")
  if(any(quants > 1))
    stop("Found 'quants' > 1 this is not allowed.")
  
  p_names <- map_chr(quants, ~paste0(.x*100, "%"))
  p_funs <- map(quants, ~partial(quantile, probs = .x, na.rm = TRUE)) %>% 
    set_names(nm = p_names)
  quant_df = df %>% 
    group_by(across(all_of(group_vars))) %>% 
    summarize_at(vars(!!!y_value), p_funs)
  return(quant_df)
}