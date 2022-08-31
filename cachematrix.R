## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    #Initiate an empty entry for the invers variable (inverse matrix), when a new cache 
    #matrix object is generated
    inverse <- NULL
    set <- function(y) {
    x <<- y
    m <<- NULL
    }
    #return the value of the x variable as passed when making the 'CacheMatrix' object
    #or when re-set using the $set function of the object
    get <- function() x                           #get the value of matrix
    setinverse <- function(i) inverse <- i        #set the value of invertible matrix
    getinverse <- function() inverse              #get the value of invertible matrix  
    
    #Index the list with '$' callable descriptors
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
       
        inverse <- x$getinverse()     ## get the value of invertible matrix makeCacheMatrix function
        
        #if the invers variable is not NA, then print some text and return the matrix stored there
        if(!is.null(inverse)) {
          message("getting cached inverse")
          return(inverse)
        }
        
        ##if the value of invertible matrix is NULL then
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse       #returns inverse of x matrix
}
