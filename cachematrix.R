## The following two functions create a special matrix that stores a matrix
## and cache's its inverse

## This makeCacheMatrix function is a list of functions that creates 
## a special "matrix"

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y){      #set the value of the matrix
        x <<- y
        inv <<- NULL
      }

      get <- function()x      #get the value of the matrix
      setinv <- function(solve) inv <<- solve #set the value of the inverse
      getinv <- function() inv    #get the value of the inverse 
      list(set=set,get=get,
           setinv=setinv,
           getinv=getinv)
}


## This second function calculates the inverse of the matrix with above function

cacheSolve <- function(x, ...) {
      inv <- x$getinv() ## Return a matrix that is the inverse of 'x'
      if(!is.null(inv)){
              message("getting cached data")
              return(inv)
      }
      data <- x$get()
      inv <- solve(data,...)
      x$setinv(inv)
      inv
}
