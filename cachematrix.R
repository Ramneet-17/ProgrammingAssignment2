## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.


makeCacheMatix <- function(x = matrix()) { 
  #Initiate an empty entry for the invers variable (inverse matrix), when a new cache 
  #matrix object is generated
  invers <- NULL  
  set <- function(y) {  
    x <<- y   
    invers <<- NULL 
  } 
  #return the value of the x variable as passed when making the 'CacheMatrix' object
  #or when re-set using the $set function of the object
  get <- function() x                                #get the value of matrix
  setInverse <- function(mean) invers <<- inverse    #set the value of invertible matrix
  getInverse <- function() invers                    #get the value of invertible matrix  
  #Index the list with '$' callable descriptors
  list(set = set, get = get,     
       setInverse = setInverse, 
       getInverse = getInverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  ## get the value of invertible matrix makeCacheMatrix function
  invers <- x$getInverse() 
  #if the invers variable is not NA, then print some text and return the matrix stored there
  if(!is.null(invers)) {  
    message("getting cached invertible matrix")    
    return(invers) 
  }
  ##if the value of invertible matrix is NULL then
  data <- x$get() 
  invers <- solve(data, ...) 
  x$setInverse(invers)
  return(invers)    #returns inverse of x matrix
}
