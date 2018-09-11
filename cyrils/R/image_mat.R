#' image_mat A function that reformats a matrix so that image() functions plot the matrix as you would expect.
#'
#' @author C.Marsh
#' @param Mat a matrix that you want to plot
#' @return a matrix that has been flipped rows and transposed
#' @export
#' @example
#' library(cyrils)
#' library(fields) ## used for image.plot() gives a legend which helps convey the point
#' demo_mat = matrix(rep(1:20,5),20,5,byrow = F)
#' demo_mat ## look at the matrix 
#' par(mfrow = c(1,2))
#' image.plot(demo_mat,main = "not reformated") ## look at the matrix it is not right
#' image.plot(image_mat(demo_mat),main = "reformated") ## look at the matrix it is not right

image_mat = function(Mat) {
  t(Mat[nrow(Mat):1,])
}