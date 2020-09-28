makeCacheMatrix <- function( m = matrix() ) { ##Crear un objeto de matriz especial que puede almacenar en caché su inverso
  i <- NULL #inciar inversa
  set <- function( matrix ) { #setear la matrix
    m <<- matrix
    i <<- NULL
  }
  get <- function() { #traer la matrix
    m #retornar matrix
  }
  setInverse <- function(inverse) {#setear inversa de la matrix
    i <<- inverse
  }
  getInverse <- function() {#traer matrix inversa
    i
  }
  list(set = set, get = get, #retornar lista de metodos 
       setInverse = setInverse,
       getInverse = getInverse)
}



cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if( !is.null(m) ) {
    message("Obteniendo datos")
    return(m)
  }
  data <- x$get() #traer la matrix de nuestros objetos 
  m <- solve(data) %*% data #calcular inversa utilizando multiplicaciones de matrices
  x$setInverse(m)# establecer la inversa del objeto
  m #retirnar matrix
}
