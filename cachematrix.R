## Creates a special “matrix” object that can cache its inverse, 
## which is really a list containing a function to
## 1. set() -> sets the matrix
## 2. get() -> gets the matrix
## 3. setinverse() -> sets the inverse of the matrix
## 4. getinverse() -> gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
        matrix_inverse <- NULL
        
        ## Set the matrix
        set <- function(y) {
          x <<- y
          matrix_inverse <<- NULL
        }
        
        ## Get the matrix
        get <- function() x
        
        ## Set the inverse of the matrix
        setinverse <- function(invert_matrix) matrix_inverse <<- invert_matrix
        
        ## Get the inverse of the matrix
        getinverse <- function() matrix_inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix 

cacheSolve <- function(x, ...) {
  
        ## Return a matrix that is the inverse of 'x'
        matrix_inverse <- x$getinverse()
        
        ## Check if the inverse is already cached and not changed, 
        ## if so return the cached inverse
        if(!is.null(matrix_inverse)) {
          message("getting cached data")
          return(matrix_inverse)
        }
        
        ## If the matrix is not cached, calculate the inverse and return
        matrix_data <- x$get()
        matrix_inverse <- solve(matrix_data, ...)
        x$setinverse(matrix_inverse)
        return(matrix_inverse)
}

