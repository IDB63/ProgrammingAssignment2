## The functions makeCacheMatrix and cacheSolve can be used in conjunction to
## calculate the inverse of a matrix and either save the result or get it from
## memory. This works because objects in R can contain other objects, and this
## nesting allows the call to an object to access both its environment and that
## of the one in which it was defined in .


## The makeCacheMatrix function takes a formal argument, a matrix, calculates
## its inverse, and then using the <<- operator assigns the value of the named
## varible to the environment.


makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
        x <<- y
        m <<- NULL
      }
      get <- function() x
      setsolve <- function(solve) m <<- solve
      getsolve <- function() m
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve)
}


## The cacheSolve function takes a named object of the matrix class and uses
## lexical scoping to see if the named variable exists. If it does then it
## prints the string "getting cached data" and gets the value of stored variable
## from the parent function makeCacheMAtrix. If the named argument doesn't exist
## it applies the solve function on it, prints the results, and saves the value
## to the enviroment. Upon using the function a second time on the same named
## object the first result will occur.

cacheSolve <- function(x, ...) {
      m <- x$getsolve()
      if(!is.null(m)) {
        message("getting cached data")
        return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setsolve(m)
      m
        ## Return a matrix that is the inverse of 'x'
}
