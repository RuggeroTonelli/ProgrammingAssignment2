#This function is able to cache potentially time-consuming computations

## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a funciton to:
#1- set the value of the matrix
#2- get the value of the vector
#3- set the value of the inverse
#4- get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
  inv<- NULL
  set<- function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function()x
  setinverse<- function(inverse) inv<<-inverse
  getinverse<-function()inv
  list(set=set,get=get,
        setinvere=setinverse,
        getinverse=getinverse)
}

#This function calculates the inverse of the special "vector" created with the above function. However, it first checks
# to see if the inverse has already been calculated. If so, it gets the mean from the cache and skips the computation.
# Otherwise, it calculates the inverse of the input matrix and sets the value of the mean in the cache via the function.

cacheSolve <- function(x, ...) {## Return a matrix that is the inverse of 'x'
  inv<-x$getinverse()
    if(!is.null(inv)){
    messagge("getting cached data")
    return(inv)
    }
  data<- x$get()
  inv<-solve(data,...)
  x$setinverse(inv)
  inv    
}
