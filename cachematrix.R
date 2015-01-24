## Two functions below are used to find the inverse of a matrix x.
## If the inverse has been found, it is returned without relcalculating the inverse and saves computing time


## This function initialize a variable m, provides function get() 
## to obtain matrix for which inverse needs to be found, 
## gives function setimatrix() to assign inverse matrix of x to m and 
## getimatrix() gives cached inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  get <- function() x
  setimatrix <- function(Imatrix) m <<- imatrix
  getimatrix <- function() m
  list(get=get, setimatrix=setimatrix, getimatrix=getimatrix)
  
}


## This function calculates inverse of x. 
## It checks for inverse matrix has been found and returns results if yes.
## If not, inverse of x is calculated, saved to cache and returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getimatrix()
  if(!is.null(m)){
    message("Cached data found")
    return(m)
  }
  else {
    message("No cached data found")
    data <- x$get() 
    m <- solve(data) # finds inverse matrix
    x$setimatrix(m) # assigns resulting inverse matrix to object x
    message("Completed")
    return(m)
  }
  
}
