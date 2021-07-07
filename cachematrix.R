#makeCacheMatrix is the function to represent the matrix
#identify the needed functions 

makeCacheMatrix <- function(x = matrix()) {

  #initializes the inverse as a NULL
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }

  #function stored in matrix x
  get <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function(){inv}

  #function to obtain the inverse to matrix x
  list(set = set, 
       get = get, setInverse = setInverse, 
       getInverse = getInverse)

}
cacheSolve <- function(x, ...)

# in order for the cache data to be obtained
{
  inv <- x$getInverse()
  if(!is.null(inv)){

    message("getting cached data!")
    #returns its inverse value
    return(inv) 
  }

  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  #return a matrix the inverse of the "x"
  inv
}
