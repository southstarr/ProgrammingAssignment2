## This is the R code corresponding to Programming Assignment 2 of the R programming course
## on Coursera.

## The code has two functions...
## "makeCacheMatrix" and "cacheSolve"



## "makeCacheMatrix" takes matrix as input and returns a list of four functions
## This is where the matrix and inverse values are cached

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  # set the matrix value
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # get the matrix value
  get <- function() x
  # set the matrix inverse (inverse is cached here)
  setinverse <- function(inverse) m <<- inverse
  # get the matrix inverse
  getinverse <- function() m
  # return the list containing the four functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## "cacheSolve" returns the inverse of the matrix
## fetches the cached value if already exists. Else, it uses the solve() function

cacheSolve <- function(x, ...) {
  # trying to fetch the inverse from cache
  m <- x$getinverse()
  if(!is.null(m)) {
    print("getting cached data")
    # return the inverse fetched from cache( if not NULL)
    return(m)
  }
  # fetch the matrix value, if the inverse cache value is NULL
  data <- x$get()
  # compute the matrix inverse
  m <- solve(data, ...)
  # set the matrix inverse
  x$setinverse(m)
  # return the matrix inverse
  m
}
