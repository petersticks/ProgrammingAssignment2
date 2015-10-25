## This function is practically the same as the make Vector function
## Main diffences are that 1)this function takes in a matrix instead of vector
## 2)it returns the inverse of the matrix instead of the vector of matrix
## get() - returns matrix, set() sets the matrix of special matrix
## set inverse <- manually assigns the inverse, overwrites inverse
## get inverse <- returns inverse matrix null, calculate (in cachemean) or 
## by manually setting it with setinverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function gets the special matrix as parametes, looks to see if
## the inverse has been calculated.  If it has been calculated it prints out 
## message and returns cache inverse.  If it hasn't it calculates inverse
## and caches the inverse into special matrix

cacheSolve <- function(x) {
    i <- x$getinverse()
    if(!is.null(i)) {
      message("getting cached inverse matrix")
      return(i)
    }
    datainv <- x$get()
    i <- solve(datainv)
    x$setinverse(i)
    i
}

