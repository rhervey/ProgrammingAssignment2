## This assignment is to cache the inverse of a matrix so that we may learn to 
## cache data for expensive loops

## Sets and gets value of a matrix
## Sets and gets the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        INV <- NULL
        set <- function(y) {
                x <<- y
                INV <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) INV <<- inverse
        getInverse <- function() INV
        list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## This function finds the inverse of the matrix, but first checks to see
## if the data already exists (is cached)

cacheSolve <- function(x, ...) {
        INV <- x$getInverse()
        if(!is.null(INV)) {
                message("getting cached data.")
                return(INV)
        }
        data <- x$get()
        INV <- solve(data)
        x$setInverse(INV)
        INV
}

## I ran this several times and was able to finally achieve the matrix inverse.