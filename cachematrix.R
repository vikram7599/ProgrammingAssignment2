## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y){ ## Set the value
    x <<- y
    m <<- NULL
  }
  get <- function() x ##Getting the value
  
  setinverse <- function(solve) m <<- solve  ##Set the value of inverse
  
  getinverse <- function() m ##Getting the value of inverse
  list(set = set, get = get,  
       setinverse = setinverse,
       getinverse = getinverse) ##Packaging the values in a list
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse() ## Getting the cached data
  
  if(!is.null(m)) { ## If the inverse is already found then it would be returned without calculating 
    message("getting cached data")
    return(m)
  }
  ##Else the inverse is found out again and cached
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
}
