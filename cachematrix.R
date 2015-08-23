## makeCacheMatrix(...) returns a storage list for input and output (inverted) 
## matrices. cacheSolve(...) takes a list and calculates new result or reads 
## previously calculated and cached result.


## makeCacheMatrix(...) simulates an object, which initializes with empty matrix
## or a given matrix. Previously stored matrix is matched with the argument 
## matrix and resets the previously calculated matrix on mismatch, which 
## enforces consistency between input and output matrices. Getter / setter
## functions are returned as list 'x', used in cacheSolve(x, ...) call.

makeCacheMatrix <- function(in_matrix = matrix()) {
  
  out_matrix <- NULL
  
  set <- function(y) {
    if(!identical(in_matrix, y)){
      in_matrix <<- y
      out_matrix <<- NULL
    }
  }
  
  get <- function() 
    in_matrix
    
  setinverse <- function(inverse) 
    out_matrix <<- inverse
  
  getinverse <- function() 
    out_matrix
  
  list(
    set = set
    , get = get
    , setinverse = setinverse
    , getinverse = getinverse
  )
}


## cacheSolve(x, ...) inverts argument 'x' in form of a list. If the matrix was 
## inverted in the previous attempt, it will be contained in 'm' and returned to
## the parent frame. If not, 'x' will be inverted in the current attempt 
## (assuming invertibility) with result written to 'x'.

cacheSolve <- function(x = makeCacheMatrix(), ...) {
  m <- x$getinverse()
  
  if(!is.null(m)) {
    message("getting cached inverse")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
