## Below functions makeCacheMatrix and cacheSolve deals the storing the value
## of inverse of a matrix in the cache so that it will be available directly
## without being calculated again and again

## This function makeCacheMatrix creates a special vector which is really a list
## containing the following function
## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the value of the inverse
## 4. get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  
  cache <- NULL
  
  set <- function(y) {
    x<<-y
    cache <<-NULL
  }
  
  get <- function() x
  
  setMatrix <- function(inverse) cache<<-inverse
  
  getInverse <- function() cache
  
  list(set=set,get=get,
       setMatrix=setMatrix,getInverse=getInverse)
    

}



## This function cacheSolve calculates the inverse of a given matrix. Before that it
## will check if inverse has been calculated already.In that case, it gets the inverse 
## from the cache and skips the computation.Otherwise it calculate the inverse and 
## sets the value of inverse in the cache through setMatrix function


## Calculate the inverse of the special "matrix" created with the above
## function, reusing cached result if it is available

cacheSolve <- function(x, ...) {
  cache <- x$getinverse()
  if(!is.null(cache)) {
    message("getting cached data")
    return(cache)
  }
  m <- x$get()
  cache <- solve(m, ...)
  x$setinverse(cache)
  return(cache)
}