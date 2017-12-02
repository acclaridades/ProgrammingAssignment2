## functions that cache and get the matrix inverse


## caching the inverse of the matrix object
makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL ##initialization
  
  set <- function (matrix){
    m <<- matrix
    i <<- NULL
  } ##setting the matrix
  
  get <- function(){
    m
  } ##just return the matrix
  
  getInverse <- function(){
    i
  } ##just return the inverse
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## calculate the inverse of makeCacheMatrix, and if it's already solved, result is just retreived

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  data <- x$get() ##get the matrix from our object
  
  m <- solve(data) %*% data #calculate inverse
  
  x$setInverse(m) ##set as Inverse
  
  m ##Return the matrix
  
}
