## makeCacheMatrix instanciates an object that returns
##   4 functions: 
##       set: to set new matrix value. 
##       get: to get matrix value from local environment passed as parameter.
##       setinv: to set calculated inverse matrix to global environment.
##       getinv: to get calculated inverse matrix from global environment.
##   2 data objects x and i which are either saved on makeCacheMatrix local environment.
##     or on parent environment (global environment).
##
makeCacheMatrix <- function(x = matrix()) {
        ## x = square matrix
        ## i = inverse
        i <- NULL
        set <- function(y) {
               x  <<- y
               i  <<- NULL
        }
        get     <- function() x
        setinv  <- function(inverse) i <<- inverse
        getinv  <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}
## cacheSolve calculates the inverse of matrix x and store the result in variable i (local/global)
## i and x are the 2 data objects included on the object type makeCacheMatrix passed as parameter
## 

cacheSolve <- function(x, ...) {
        ## get i (inverse matrix) from global environment
        i <- x$getinv()
        if(!is.null(i)) {
                print("getting cached data")
                return(i)
        }
        ## if (i) isnull get matrix (x) to calculate its inverse 
        data <- x$get()
        ## calculating matrix inverse
        i <- solve(data)
        ## sets inverse into global environment
        x$setinv(i)
        print("getting calculated data")
        i
}
##############################################
##Test:
##
##>  a<-matrix(c(4,2,7,6),nrow=2,byrow=FALSE)
##>  a_cache<-makeCacheMatrix(a)
##>  cacheSolve(a_cache)
##   [1] "getting calculated data"
##        [,1] [,2]
##   [1,]  0.6 -0.7
##   [2,] -0.2  0.4
##> cacheSolve(a_cache)
##   [1] "getting cached data"
##        [,1] [,2]
##   [1,]  0.6 -0.7
##   [2,] -0.2  0.4
##
##Set new values:
##
##> a_cache$set(matrix(c(3,0,2,2,0,-2,0,1,1),nrow=3,byrow=TRUE))
##> cacheSolve(a_cache)
##  [1] "getting calculated data"
##     [,1] [,2] [,3]
##  [1,]  0.2  0.2    0
##  [2,] -0.2  0.3    1
##  [3,]  0.2 -0.3    0
##> cacheSolve(a_cache)
##  [1] "getting cached data"
##       [,1] [,2] [,3]
##  [1,]  0.2  0.2    0
##  [2,] -0.2  0.3    1
##  [3,]  0.2 -0.3    0
##> 
##############################################################

