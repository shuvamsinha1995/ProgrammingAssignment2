## makeCacheMatrix creates a matrix to return a list to get the matrix, set the matrix and to do the same with their inverses through setInverse and getInverse
##Here, getInverse continues to enter inverses of matrices calulated from the matrix inputted through 'get'. It stores this value so that it doesn't have to re-calculate the inverse in case the same matrix is called again
makeCacheMatrix <- function(x = matrix()) {
  invert <- NULL
  set <- function(y){
    x <<- y
    invert <<- NULL
  }
  get <- function() x
  setInverse <- function(invertMatrix) invert <<- invertMatrix
  getInverse <- function() invert
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



## The function first checks whether the inverse is already previously calculated, in which case it retrieves the value from the cache and returns it.
##Else, it calulates and sets the inverse of the matrix that is entered and stores it into its cache

cacheSolve <- function(x, ...) {
  invert <- x$getInverse()
  if(!is.null(invert)){
    message("getting cached data")
    return(invert)
  }
  data <- x$get()
  invert <- solve(data)
  x$setInverse(invert)
  invert      
}