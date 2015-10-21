##This function creates a special "matrix" object that can cache its
##inverse.

makeCacheMatrix <- function(   x = matrix()  ) 
{
  ##  Declare solution
  result <- NULL
  
  
  
  ## 1 -- Set the matrix
  set <- function(y) 
    {
    x <<- y
    ##    <<- operator which can be used to assign a value to an object in 
    ##    an environment that is different from the current environment
    result <<- NULL
  }
  
  ## 2 -- Get the matrix
  get <- function() x
  
  ## 3 -- Set the inverse
  setInverse <- function(tempInverse) result <<- tempInverse
  
  ## 4 -- Get the inverse
  getInverse <- function() result
    
  ## ***************
  ## Same as sample
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



##This function computes the inverse of the special "matrix" returned 
##by makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve should retrieve 
##the inverse from the cache.

cacheSolve <- function(x, ...) 
  {
        result <- x$getInverse()
        if(!is.null(result)) 
        {
          message("getting cached data")
          return(result)
        }
  
  data <- x$get()  ###  mat.data = ..same
  result <- solve (data, ...)
  x$setInverse(result)
  return (result)
}

