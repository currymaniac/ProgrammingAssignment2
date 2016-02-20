## The following two functions allow you to compute the inverse
## of a matrix and cache it for future use
## Note, the functions assume that the inverse can be computed (i.e. the matrix is square)


## This function stores 2 values: x - the matrix and i - the inverse
## It creates a list of four functions, allowing you to get and set x and i
## Note, it does not compute the inverse matrix (this is done in cacheSolve() )
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  get <- function() x
  ## Note, the inverse must be cleared if the matrix changes
  set <- function(y = matrix()){
    x <<- y
    i <<- NULL
  }
  getInverse <- function() i
  setInverse <- function(inverse = matrix()) i <<- inverse
  list(set = set, get = get,
       getInverse = getInverse,
       setInverse = setInverse)
}



## Computes the inverse of the supplied matrix. 
## The matrix must be created using makeCacheMatrix
## It first checks if the inverse has already been computed If it has, 
## it returns the existing inverse, else it computes the inverse using solve()
cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if (!is.null(i))
  {
      message("getting cached data")
      return(i)
  }
  i <- solve(x$get())
  x$setInverse(i)
  i
}
