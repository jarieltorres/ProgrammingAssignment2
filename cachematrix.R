## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
 
    invertir = NULL
    set = function(matriz) {
        x <<- matriz
        invertir <<- NULL
    }
    get = function() x
    
    setinvertir = function(inverse) invertir <<- inverse 
    getinvertir = function() invertir
    
    list(set=set, get=get, setinvertir=setinvertir, getinvertir=getinvertir)
}

cacheSolve <- function(x, ...) {

    invertir = x$getinv()
    
    if (!is.null(invertir)){
        message("Datos desde Cache")
        return(invertir)
    }
    
    datosMatriz = x$get()
    invertir = solve(datosMatriz, ...)
    
    x$setinv(invertir)
    
    return(invertir)
}
