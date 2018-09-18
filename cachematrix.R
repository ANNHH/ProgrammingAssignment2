## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function creates a special "matrix", calculate and cache the inverse of a matrix. 
makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y) {                            
    x <<- y
    inver <<- NULL
  }
  get <- function() x                                      ##get the value of the Matrix
  setinverse <- function(inverse) inver <<- inverse        ##caculate the inverse of the matrix
  getinverse <- function() inver                           ##get the value of the invertible matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
##get the value of the invertible matrix from makeCachematrix
cacheSolve <- function(x, ...) {
  inver<-x$getinverse()
  if(!is.null(inver)){                           ## if inverse matrix is not NULL
    message("getting cached InverseMatrix")       ## massage: getting cached InverseMatrix
    return(inver)                                 ## return the invertible matrix
  }
  
  data<-x$get()                                       ##get the Matrix
  inver<-solve(data,...)                              ##solve(X) returns its inverse
  x$setinverse(inver)                                 ##set the invertible matrix
  inver                                               ## return the invertible Matrix
  ## Return a matrix that is the inverse of 'x'
}
