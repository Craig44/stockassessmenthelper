#' Used to split a single string, space serperated into multiple strings
#'
#' @author Dan Fu
#' @export
#'
"string.to.vector.of.words" <-
function(string)
{
  temp <- unpaste(string, sep = " ")
  return(temp[temp != ""])
}

