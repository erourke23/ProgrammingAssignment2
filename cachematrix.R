## Caching the Inverse of a Matrix
## 2 Functions:
## 1) A function to cache the inverse of a matrix and 
## 2) A Function to compute the inverse of a matrix,
## using the cached value if it is available


## Function that creates a vector which is a list of functions that do the following:
## 1) Set the value of the matrix
## 2) Get the value of the matrix
## 3) Set the value of the inverse
## 4) Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    data <<- x
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  
}

## Function to calculate the inverse using the first function. 
## First checks to see if the value has already been cached.
## If so, it takes the cached value, and otherwise it calculates
## the inverse value for the cache using setinv.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}

