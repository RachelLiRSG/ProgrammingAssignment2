## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix function is used to cache the inverse of a Matrix.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
  j <- NULL                 #initializes two variables j and inv with NULL values.
  set <- function(y){
    x <<- y                 #sets the value of the matrix x to y.
    j <<- NULL              #sets the value of j to NULL
  }
  get <- function()x        #The get function returns the value of the matrix x.
  setInverse <- function(inverse) j <<- inverse  #sets the value of j to the input argument inverse.
  getInverse <- function() j #returns the value of j
  list(set = set, get = get, #returns a list of functions set, get, setInverse, and getInverse.
       setInverse = setInverse, 
       getInverse = getInverse)
}

test <- makeCacheMatrix(matrix(c(1,2,3,4), nrow = 2, ncol = 2))
test$set()
test$get()
test$setInverse()
test$setInverse(solve(test$get()))
test$getInverse()


## The purpose of this function is to avoid redundant calculation of the inverse 
## of the same matrix. 
## If the inverse has already been calculated (and the matrix has not changed),
## then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  j <- x$getInverse()
  if(!is.null(j)){              #checks if the inverse of the matrix is stored in the cache
    message("getting cached data")
    return(j)
  }
  mat <- x$get()                #If the inverse is not found in the cache
  j <- solve(mat,...)
  x$setInverse(j)
  j
}
