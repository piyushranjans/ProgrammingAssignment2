## A Short Comment
## makeCacheMatrix creates a list containing a function to
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse of the matrix
## 4.  get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## A Short Comment
## The following function first checks if the inverse has already been computed.
## If the inverse is found, it returns the inverse and skips computation
## If the inverse is not found, it calculates and caches the inverse

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)){
      message("Getting Cached Data")
      return(inv)
    }
    
    # Compute the Inverse and Cache for future
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
