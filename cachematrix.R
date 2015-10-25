## With the two functions below it is possible to calculate the inverse of a square invertible matrix 
## by using a cache function. Due to this already done calculations will be saved in the cache and
## can be reloaded instead of doing the ressource consuming calculations again.

## Creates a special "matrix", which is really a list containing a function to
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    #set the matrix
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    #get the matrix
    get <- function() x
    #set the inverse of the matrix
    setsolve <- function(solve) m <<- solve
    # get the inverse of the matrix
    getsolve <- function() m
    #return of a list with the 4 functions
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
  }




##Computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve will be retrieved from the cache.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    #checks if the matrix has been calculated before
    if(!is.null(m)) {
      #inverse matrix in cace available, return of this value
      message("getting cached data")
      return(m)
    }
    else{
      #no inverse matrix in cache, so calculate inverse and save it to cache with setsolve
      data <- x$get()
      m <- solve(data, ...)
      x$setsolve(m)
      m
    }
  }

