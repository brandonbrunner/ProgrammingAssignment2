## Brandon Brunner
## Data Science Analyst, ConocoPhillips
## Johns Hopkins Data Science Specialization
## Assignment: Course 2, Week 3
## March 24, 2016 HAPPY EASTER (or spring equinox for you heathens)

## The function below uses lexical scoping to create a list
## of a matrix and its determinant

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setinverse <- function(solve) m <<- solve
     getinverse <- function() m
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## If a matrix is passed into the function below, it will compute the inverse
## If a "matrix list" as created by makeCacheMatrix is passed in, the function
## below will retrieve the inverse from the list

cacheSolve <- function(x, ...) {
     if(class(x) == 'list') {      #if makeCacheMatrix is used
          m <- x$getinverse()
          if(!is.null(m)) {
               message("getting cached data")
               return(m)
          }
          data <- x$get()
          m <- solve(data, ...)
          x$setinverse(m)
     } else {                      #if regular old matrix
          m <- solve(x)
     }
     return(m)
}
