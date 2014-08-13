## makeCacheMatrix creates and returns a list of functions
## that set and get the matrix to operate on
## and set and get the inverse for that matrix


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
## setter and getter function  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
## set and get inverse of the matrix

  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x) {
   
  inv <- x$getinverse()
  if(!is.null(inv)) {
    ## returns cached data if inv is already available
    message("getting cached data")
    return(inv)
  }
  
  ## calculate the inverse
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
