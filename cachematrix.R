## makes a makeCacheMatrix object and use it to store matrix and inverse of matrix
## after inverse of matrix has been stored it will be used directly without
## making new calculations.

## Make makeCacheMatrix object. On this object you set/get matrix and set/get
## inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
     m<-NULL
     set<-function(y){
          x<<-y
          m<<-NULL
     }
     get<-function() x
     setinv<-function(solve) m<<- solve
     getinv<-function() m
     list(set=set, get=get,
          setinv=setinv,
          getinv=getinv)
}

## Returns the inverse of a matrix. If the solving has been done earlier the
## result is returned from the cache. Else the inverse is stored to the 
## makeCacheMatrix object after calculating the inverse of the matrix.

cacheSolve <- function(x=matrix(), ...) {
     m<-x$getinv()
     if(!is.null(m)){
          message("getting cached data")
          return(m)
     }
     matrix<-x$get()
     m<-solve(matrix, ...)
     x$setinv(m)
     m
}