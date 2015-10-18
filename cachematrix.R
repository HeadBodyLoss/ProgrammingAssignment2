##These function are to enable a user to cache a matrix along with it's inverse.
##'makeCacheMatrix' caches the matrix information, while
##'cacheSolve' either retrieves the cached inverse or calculates and stores the cache

##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      #Assigning values and functions that assign values in Global Environment
      matrix_inv <- NULL  
      set <- function(matrix_y){
            x <<- matrix_y
            matrix_inv <<- NULL
      }
      set_inv <- function(inv) matrix_inv <<- inv
      
      #Functions to retrieve the matrix and inverse matrix
      get <- function() x
      get_inv <- function() matrix_inv
      
      list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)
}


##This function computes the inverse of the special "matrix" returned by 
##makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve should retrieve 
##the inverse from the cache.

cacheSolve <- function(x, ...) {
      #Use cached inversed matrix if available
      matrix_inv <- x$get_inv()
      if(!is.null(matrix_inv)){
            message("getting cached data")
            return(matrix_inv)
      }
      
      #If inversed matrix is not cached then 
      #calculate and then cache using makeCacheMatrix
      matrix_inv <- solve(x$get(), ...)
      x$set_inv(matrix_inv)
}
