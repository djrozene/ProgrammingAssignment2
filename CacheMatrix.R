# Matrix inversion is usually a costly computation and there may be some benefit 
# to caching the inverse of a matrix rather than compute it repeatedly 
# # The function makeCacheMatrix creates a special "matrix" object 
# that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## @x: a square invertible matrix
  ## return: a list containing functions to
  ##              1. set the matrix
  ##              2. get the matrix
  ##              3. set the inverse
  ##              4. get the inverse
  ##         this list is used as the input to cacheSolve()
  
  inv <- NULL
  set <- function(y) {
    # use `<<-` to assign a value to an object in an environment 
    # different from the current environment. 
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse 
  getinv <- function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}
##
##
##
##
cacheSolve <- function(x, ...) {
 
  ## x: output of makeCacheMatrix()
  ## return: inverse of the original matrix input to makeCacheMatrix()
  
  inv <- x$getinv()
  
  # if the inverse has already been calculated
  if (!is.null(inv)){
    # get it from the cache and skip the computation. 
        message("getting cached data")
        return(inv)
  }
  # otherwise, calculates the inverse 
  matrix_data <- x$get()
    inv <- solve(matrix_data, ...)
    x$setinv(inv)
    # set the value of the inverse in the cache via the setinv function.
  x$setinv(inv)
  inv
}  
## return(inv)

##
##
##
##  Test the Functions
  source("/Volumes/Red Seagate 2 Tb -1/Courses/Coursea.org/DSTB/1. R Programming/")
  #
  ## Test 1
    TestData1 <- matrix(1:4, 2, 2)
    CacheMatrix <- makeCacheMatrix(TestData1)
    CacheMatrix$get()
    CacheMatrix$getinv()
    cacheSolve(CacheMatrix)
    cacheSolve(CacheMatrix)
##
##
  ## Test 2
    TestData2 <- matrix(c(2, 2, 1, 4), 2, 2)
    CacheMatrix <- makeCacheMatrix(TestData2)
    CacheMatrix$get()
    CacheMatrix$getinv()
    cacheSolve(CacheMatrix)
    cacheSolve(CacheMatrix)
##
##
    ## Test3
    TestData3 <- matrix(c(0, 10, 2, 4), 2, 2)
    CacheMatrix <- makeCacheMatrix(TestData3)
    CacheMatrix$get()
    CacheMatrix$getinv()
    cacheSolve(CacheMatrix)
    cacheSolve(CacheMatrix)