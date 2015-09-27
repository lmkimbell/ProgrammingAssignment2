## These functions will create a matrix, inverse that matrix and place the 
## results into Cache Memory to reduce computing and time cost of calculations.

## makeCacheMatrix creates a matrix and a lists of of calls for the function 
## to use by 

makeCacheMatrix <- function(x = matrix()) {
   i <- NULL  ## initialize variable
   set <- function(y) {
      x <<- y
      i <<- NULL  ##initialize cache
   }  ## End Set
   get <- function() x
   setinverse <- function(inverse) i <<- inverse
   getinverse <- function() i
   list(set = set, 
        get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}  ## End makeCacheMatrix


## cacheSolve will inverse of a matrix and place in cache.  Will advise user if
## retrieving a cached calculation or running the solve process.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   i <- x$getinverse()
   if(!is.null(i)){
      message("getting cached data")
      return (i)     ## final output
   }  #End if evaluation
   data <- x$get()   ## Get matrix dataset
      message("solving inverse data")
   i <- solve(data)   ## Solve for Inverse
   x$setinverse(i)   ## assign set inverse value.
   i  ## final output
}  ## End CacheSolve
