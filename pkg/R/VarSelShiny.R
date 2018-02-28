###################################################################################
##' @param X 
##' @examples
##' \dontrun{
##' data(iris)
##' res.LCM <- VarSelCluster(x, 2)
##' summary(res.LCM)
##' }
##' @export
##'
##'
VarSelShiny <-  function(X){
  G <- .GlobalEnv
  assign("resVSLC", X, envir=G)
  a=shiny::runApp(system.file("shinyApp",package="VarSelLCM"),launch.browser = TRUE)
  return(invisible(a))
}
