Caching the inverse of a Matrix 


This function is able to create a special matrix object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
 
    j <- NULL
  set <- function(y){
    x <<- y
    j <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) j <<- inverse
  getInverse <- function() j 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse) 
}


This function computes the inverse of the matrix  created by the "makeCacheMatrix" function, written below. 
If the inverse has already been calculated, then this function will retrieve the inverse from the cache.  

cacheSolve <- function(x, ...) {
  j <- x$getInverse()
  if(!is.null(j)){
    message("getting cached data")
    return(j)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(j)
  j
}
