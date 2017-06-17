# Below are two functions that are used to create a # special "matrix" object that can cache its inverse and compute the inverse of the special
# "matrix"
# To test it, try the following on your R console: 
 # source('cachematrix.R')
 # aMatrix <- makeCacheMatrix( rbind(c(1, -1/4), c(-1/4, 1)) )  # invertible square matrix
 # aMatrix$get()                                                # retrieve the value of x
 # aMatrix$getinverse()                                         # retrieve the value of m, which should be NULL
 # aMatrix$set( rbind(c(3, 4), c(5, 7) ) )                      # set another invertible square matrix
 # cacheSolve(aMatrix)                                          # notice which inverse is calculated 
 # aMatrix$getinverse()                                         # retrieve it directly, now that it has been cached
 # cacheSolve(aMatrix)                                          # notice the message "getting cached data"


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

     inversed_x <- NULL

     set <- function(y) {
       x <<- y
       inversed_x <<- NULL
     }
     
     get <- function() x
     setinverse <- function(inverse_x) inversed_x <<- inverse_x
     getinverse <- function() inversed_x
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
  
    
}


## This function computes the inverse of the special "matrix" 
# returned by `makeCacheMatrix` above. If the inverse has
# already been calculated (and the matrix has not changed), then
# `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  #attempt to retrieve the inversed from the object passed in as the argument.
  inversed_x <- x$getinverse()    
 
     if(!is.null(inversed_x)) {
      message("getting cached data")
      return(inversed_x)
     }
  
     #gets the matrix from the input object, calculates the inverse
     data <- x$get()
     inversed_x <- solve(data, ...)
  
     #uses the setinverse() function on the input object to set the inversed in the input object,
     x$setinverse(inversed_x)
     
     #then returns the value of the inversed_x to the parent environment by printing the inversed_x object
     inversed_x
   }

