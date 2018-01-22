## The below functions allows the inverse of a matrix calculation to be cached. 

## The makeCacheMatrix function is used to construct a matrix for which the inverse calculation is "cache"able.

makeCacheMatrix <- function(x = matrix()) {
  
  matrix <- x
  inverse <- NULL
  
  
  set <- function(passedMatrix){
    matrix <<- passedMatrix
  }
  
  get <- function() matrix
  
  getInverse <- function() inverse
  
  setInverse <- function(passedInverse) inverse <<- passedInverse 
  
  list(matrix = matrix,
       set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse
       )
}


## The cacheSolve function is used to calculate the inverse of a inverse "cache"able matrix created using makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  if(!is.null(x$getInverse())){
    message("This is from the cache...")
    x$getInverse()
  }else{
    matrix <- x$get()
    inverse <- solve(matrix)
    x$setInverse(inverse)
    inverse
  } 
}
