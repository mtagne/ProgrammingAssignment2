## Marc-Tagne-Assignment-2 Caching the inverse of a matrix
## Matrix inversion is usually a costly computation 
## and there may be some benefit to caching the inverse 
## of a matrix rather than compute it repeatedly 
## The functions "makeCacheMatrix" and "cacheSolve" 
## below compute the inverse of a matrix and make it 
## available in the cache environment


## makeCacheMatrix : Creates and returns a list of functions
## used by cacheSolve to get or set the cached inverted matrix 
## Input : x,  the matrix to be inverted
## Output : set, get, setIm, getIm. This functions are used by cacheSolve().
##          set   : set the value of the matrix
##          get   : get the value of the matrix
##          setIm : set the value of the inverted matrix
##          getIm : get the value of the inverted matrix
## Constraint : x is a square matrix and  is invertible 

makeCacheMatrix <- function(x = matrix()) {
  # Store the value of the inverted matrix (Im) or cached value of the matrix
    Im <- NULL
    
    # set the matrix in the working environment
    set <- function(y){
      x <<- y
      Im <<- NULL
    }
  
  #get the value of the matrix
    get <- function() x
  
  #set the value of the inverted matrix and cached it
    setIm <- function(solve) Im <<- solve

  # get the value of the inverse matrix from the cache   
    getIm <- function() Im
    
  #return the functions (set, get, setIm, getIm) to the working environment
    list(set = set, get = get,setIm = setIm, getIm = getIm)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the  
## it should retrieve the inverse from the cache and skip the computation.
## otherwise, it calculates the inverse of the matrix and set teh value of 
## the inverse matrix in the cache via setInverse
## Input : -x, an invertible matrix, 
##          -...  any objects, possibly named.
## Ouput : the inverted matrix (ImCache)
## Constraint : x is a square matrix and  is invertible
cacheSolve <- function(x, ...) {
  # get the cached value of x, if it exists and store the value in ImCache
  ImCache <- x$getIm()
  
  # return the inverted cached matrix if exists
  # else create the matrix in the current working environment.
  if(!is.null(ImCache)) {
    message("getting cached data")
    return(ImCache)
  }
  #the matrix x does not exist in the cache, create a new one.
  newMatrix <- x$get()
  
  #return the inverted of the newMatrix
  ImCache <- solve(newMatrix, ...)
  
  #set newMatrix inverted in the cache
  x$setIm(ImCache)
  
  # return newMatrix inverted and cached
  ImCache
}

## Test function 
# testMatInv <- function(matrix){
#   
#   tempMatrix <- makeCacheMatrix(matrix)
#   
#   start <- Sys.time()
#   cacheSolve(tempMatrix)
#   elapseTime <- Sys.time() - start
#   print(elapseTime)
#   
#   start <- Sys.time()
#   cacheSolve(tempMatrix)
#   elapseTime <- Sys.time() - start
#   print(elapseTime)
# }

# set.seed(123456890)
# r = rnorm(9000000)
# matrixTest <- matrix(r, nrow=3000, ncol=3000)
# testMatInv(matrixTest)