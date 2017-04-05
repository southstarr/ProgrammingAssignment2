## This is the R code corresponding to Programming Assignment 2 of the R programming course
## on Coursera.

## The code has two functions...
## "makeCacheMatrix" and "cacheSolve"



## "makeCacheMatrix" takes matrix as input and returns a list of four functions
## This is where the matrix and inverse values are cached
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


## "cacheSolve" returns the inverse of the matrix
## fetches the cached value if already exists. Else, it uses the solve() function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    print("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
