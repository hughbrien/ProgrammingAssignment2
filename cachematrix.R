# Cache Gettter and Seeter
cachesolve <- function(x, ...) { 
  i<- x$getInverse()
  if(!is.null(i)) {
    print("Returning Cached Inverse Matrix")
    return(i)
  }
  data <- x$get()
  i<- solve(data, ...)
  x$getInverse(i)
  i
}

# Create the inverse matrix using solve
makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  getInverse <- function(solve) i <<- solve
  getInverse <- function() i
  list(set = set, get = get, getInverse = getInverse, getInverse = getInverse)
  
}

