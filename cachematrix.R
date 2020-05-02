## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function:
#     Craete a object that contains info about the matriz and its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function:
#   This function verify if the variable "inv" already exists
#   if so, return the information content into the variable inv, 
#   that is the value of inverse previously calculated.
#   Else, extract the information from the object "x" and calculate 
#   the inverse of the matrix.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

### FOR TESTING
# A <- matrix(c(1,2,3,4),2,2)
# A_cache <- makeCacheMatrix(A)
# cacheSolve(A_cache) #inverse returned after computation
#        [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

# If we run it again it takes the value from cahe
# cacheSolve(A_cache) #inverse returned after computation
#  getting cached data.
#        [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
