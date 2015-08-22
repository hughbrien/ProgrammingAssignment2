# 
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


m1 <- matrix(c(1,0,0,0,
               0,1,0,0,
               0,0,1,0,
               1,0,0,1), nrow = 4, ncol = 4)



m1


r1 <- makeCacheMatrix(m1)
r1$get()
r2 <- makeCacheMatrix(m1)
r2
r3 <- makeCacheMatrix(m1)
r3
r1$get()
r1$getInverse()

