
##Este método crea la inversa de una matriz y almacena en cache 
#C
makeCacheMatrix <- function(x = matrix()) {
  inverse<-NULL
  set<-function(y){
    x<<-y
    inverse<<-NULL
  }
  get <- function() x
  setInverse <- function(inv) inverse <<- inv
  getInverse <- function() inverse
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


##Funcion que computa el inverso de una matriz creada desde el método anterior
## si la inversa ya estaba calculada retorna la inversa desde el cache
cacheSolve <- function(x, ...) {

  inverse <- x$getInverse()
  if (!is.null(inverse)) {
    return(inversa)
  }
  matriz <- x$get()
  inverse <- solve(matriz, ...)
  x$setInverse(inverse)
  inverse
  
}

