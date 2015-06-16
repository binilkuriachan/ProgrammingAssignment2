## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

## This function cacheSolve calculates the inverse of a given matrix. Before that it
## will check if inverse has been calculated already.In that case, it gets the inverse 
## from the cache and skips the computation.Otherwise it calculate the inverse and 
## sets the value of inverse in the cache through setMatrix function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  cache <- x$getInverse()
  
  if(!is.null(cache)){
          
          message("getting cached data")
          return(cache)
  }
  
  matrix <- x$get()
  
  tryCatch({
          
          cache <- solve(matrix,...)
  },
  finally={
          
          x$setMatrix(cache)
  }
          
  )
  
  return(cache)
}
