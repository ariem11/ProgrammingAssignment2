## assignment: write a pair of functions that cache the inverse of a matrix

#1.makeCacheMatrix: this function creates a special "matrix" object that can cache its inverse 
    makeCacheMatrix <- function(matrix1 = matrix()) {
    inver1 <- NULL
    set <- function(matrix2) {
        matrix1 <<- matrix2
        inver1 <<- NULL
    }
    get <- function() {
        return(matrix1)    
    }
    setInverse <- function(inver2) {
        inver1 <<- inver2 
    }
    getInverse <- function() { 
        return(inver1)
    }
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
    }
}


#2. cacheSolve: this function computes the inverse of the 
# special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated then the 
# cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(matrix1) {
        ## Return the inverted matrix of matrix1
    inver1 <- matrix1$getInverse()
    if (!is.null(inver1)) {
        message("retrieve the inverse from the cache")
        return(inver1)
    }
    matrix2 <- matrix1$get() #instead of matrix1 which is a list, takes matrix2 which is of class matrix
    inver1 <- solve(matrix2) #the standard R function for matrix inversement is solve()
    matrix1$setInverse(inver1)
    return(inver1)
}
