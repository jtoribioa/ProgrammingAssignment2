## Put comments here that give an overall description of what your
## functions do

## [Jaime] This function create an special matrix. That can store its inverse matrix.
makeCacheMatrix <- function(x = matrix()) {
      
      ## i: Inversed Matrix.
      i <- NULL
      
      ## To modify the matrix must use the Set function, then the inverse es set to NULL.
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      
      ## Gets the data of the matrix
      get <- function() x
      
      ## Store in cache the inversed matrix.
      setinverse <- function(inverse) i <<- inverse
      
      ## Gets the inversed matrix.
      getinverse <- function() i
      
      ## return (create) the special matrix
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## [Jaime] This function return the inverse of the special matrix "x" created by the function makeCacheMatrix. 
## If it was already processed and the matrix has not changed returns the inverse from the cache 
## else calculates, stores and returns the inverse.
cacheSolve <- function(x, ...) {
      
      ## retrieve the inverse from the cache
      i <- x$getinverse()
      
      ## if its not null the return the inversed matrix and ends.
      if(!is.null(i)) {
            message("getting cached data")
            return(i)
      }
      
      ## else, retrive the data from the special matrix
      data <- x$get()
      
      ## then use the function solve to compute the inversed matrix.
      i <- solve(data, ...)
      
      ## Set the inversed matrix in the special matrix to store in cache.
      x$setinverse(i)
      
      ## return 
      i
}
