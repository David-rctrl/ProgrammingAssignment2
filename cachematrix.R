##There are a pair of functions take and cache the inverse of a matrix.

##The makeCacheMatrix function takes an invertible square matrix as argument , and 
##makes a special matrix out of it, whose inverse is cached and can be retrieved when the matrix containing the same element is computed. 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<-y
    i <<-NULL
  }
  get <-function(){
    x
  }
  setInverse <- function(inverse) {
    i <<-inverse
  }
  getInverse <- function(){
    i
  }
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function, cacheSolve, computes the inverse of the special matrix above and caches the result.

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  matData <- x$get()
  i <- solve(matData,...)
  x$setInverse(i)
  i    
  
  
  ##EXAMPLE  
  ## x <- matrix(1:4, nrow=2, ncol=2) 
  
  ##mat <- makeCacheMatrix(x)
  
  ##cacheSolve(mat)
  ##                [,1] [,2]
  ##        [1,]   -2  1.5
  ##        [2,]    1 -0.5 
}
