
## the functions below opperate to yield the inverse of a matrix, either out of
## cashe or calculated. if a<--makeCacheMatrix(matrix) and cacheSolve(a) is 
## inputted the inverse is calculated, outputted and cashed. The next call to
## casheSolve using a as the arguement will retrieve the inverse from cashe.

## makeCacheMatrix accepts a matrix and stores it in it's local environment under
## the variable name x and initiallises variable m as NULL. It outputs a list of
## 4 fuctions able to alter or obtain the items linked to x an m from outside of 
## the environment

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve checks the cached value for the inverted matrix x
##from the makeCacheMatrix environment, if it is not NULL
## it returns the cached value for the inverted matrix, else it calculates caches
## (in the makeCashMatrix environment) and returns the inverted matrix.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  }
