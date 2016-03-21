## Programming Assignment 2 ##
# makeCacheMatrix caches the inverse of a matrix
# while chacheSolve checks to see if a matrix inverse
# has been changed
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    ## "<<" required for global environment so it doesn't overrite values in current environment
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse) inv <<- inverse
  get_inverse <- function() inv
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}

cacheSolve <- function(x, ...) {
  inv <- x$get_inverse()
  
  # Checks if the inverse is already calculated
  if(!is.null(inv)) {
    message("getting cached data")
    inv
  }
  
  # if invers is not calculated, then perform calc
  data <- x$get()
  inv <- solve(data, ...)
  x$set_inverse(inv)
  inv
}


# using below to test above functions
# Creating because cache_matrix stored as a function
# rather than a matrix when not in a function
check <- function(strd_matrix) {
  cached_matrix <- makeCacheMatrix(strd_matrix)
  cacheSolve(cached_matrix)
  #run a second time to throw "gettinc chached data" message
  cacheSolve(cached_matrix)
}


