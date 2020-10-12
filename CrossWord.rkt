;--------------------------------------------------------------------------------------------------------------------------------------
(require racket/list)

(define words '("MANIVELA" "SOLEADO" "LETRAS" "VIOLIN" "ARBOL" "LECHE" "JIRAFA" "SOPA")) ; Lista de las palabras de la sopa
(define word-list '("A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N"
                        "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z")) ; Lista para obtener letras aleatorias

(define matrix '((0 0 0 0 0 0 0 0 0 0)
                 (0 0 0 0 0 0 0 0 0 0)
                 (0 0 0 0 0 0 0 0 0 0)
                 (0 0 0 0 0 0 0 0 0 0)
                 (0 0 0 0 0 0 0 0 0 0)
                 (0 0 0 0 0 0 0 0 0 0)
                 (0 0 0 0 0 0 0 0 0 0)
                 (0 0 0 0 0 0 0 0 0 0)
                 (0 0 0 0 0 0 0 0 0 0)
                 (0 0 0 0 0 0 0 0 0 0))) ; Matriz para la sopa de letras

;--------------------------------------------------------------------------------------------------------------------------------------

; Obtiene una posicion x,y de una matriz
(define (get-pos y x matrix)
  (cond ((and
          (and (>= x 0) (< x (length matrix)))
          (and (>= y 0) (< y (length (list-ref matrix x)))))
         (list-ref (list-ref matrix x) y))
        (else
         '())))

; Imprime una matriz dada
(define (print-m matrix)
  (map (lambda (x) (displayln x)) matrix))

; Imprime matrix
(define (printm)
  (map (lambda (x) (displayln x)) matrix))

; Obtiene una letra aleatoria de una lista dada
(define get-random-l
  (lambda (wl) (first (shuffle wl))))

; Separa una lista de palabras en una matriz de letras
(define (separate-list wrds)
  (cond ((null? wrds) '())
        (else
         (append (list (separate  (first wrds))) (separate-list (rest wrds))))))

; Separa un palabra en una lista de letras
(define separate
  (lambda (w) (map string (string->list w))))

;--------------------------------------------------------------------------------------------------------------------------------------

; Setea la variable global matrix con la sopa de letras
(define (run)
  (set! matrix (set-matrix matrix)))

; Rellena la matrix con el resultado de meter las palabras aleatoriamente y las letras aleatorias
(define set-matrix
  (lambda (m)
    (fill-matrix (insert-words matrix (separate-list words)))))

; Llena una matriz con letras aleatorias
(define fill-matrix
  (lambda (matrix) (fill-y matrix 0 (get-random-l word-list))))

; Devuelve una matriz de letras aleatorias
(define fill-y
  (lambda (list y letter)
    (cond ((equal? y (length list)) list)
          (else (list-set
                 (fill-y list (+ y 1) (get-random-l word-list))
                 y (fill-x list (list-ref list y) 0 y (get-random-l word-list)))))))

; Devuelve una lista de letras aleatorias
(define fill-x
  (lambda (matrix list x y letter)
    (cond ((equal? x (length list)) list)
          (else
           (cond ((not (equal? (get-pos x y matrix) 0))
                  (fill-x matrix list (+ x 1) y (get-random-l word-list)))
                 (else
                  (list-set (fill-x matrix list (+ x 1) y (get-random-l word-list)) x (get-random-l word-list))))))))

; Inserta una lista de palabras en la matriz
(define insert-words
  (lambda (matrix words)
    (cond ((null? words) matrix)
          (else (insert-words (get-randoms matrix (first words)) (rest words))))))

; Llama a try-words con posX, posY y orientacion aleatoria
(define get-randoms
  (lambda (matrix word)
    (try-words matrix (random 10) (random 10) word (random 8))))

; Encicla el try-word con el get-randoms, si la palabra cabe la mete y si no obtiene otros parámetros
(define try-words
  (lambda (matrix posX posY word o)
    (cond ((null? (try-word matrix posX posY word (length word) o)) (get-randoms matrix word))
          (else (try-word matrix posX posY word (length word) o)))))

; Valida si una palabra tiene los parámetros correctos: si no se sale de la matriz y
; no hay otra palabra en medio
(define try-word
  (lambda (matrix posX posY word l o)
    (cond ((equal? o 0)
           (cond ((> (+ posX l) 10) '())
                 (else (cond ((not (andmap boolean? (zeros? matrix posX posY l o))) '())
                             (else (set-word matrix posX posY word o))))))
          ((equal? o 1)
           (cond ((< (- posX l) -1) '())
                 (else (cond ((not (andmap boolean? (zeros? matrix posX posY l o))) '())
                             (else (set-word matrix posX posY word o))))))
          ((equal? o 2)
           (cond ((> (+ posY l) 10) '())
                 (else (cond ((not (andmap boolean? (zeros? matrix posX posY l o))) '())
                             (else (set-word matrix posX posY word o))))))
          ((equal? o 3)
           (cond ((< (- posY l) -1) '())
                 (else (cond ((not (andmap boolean? (zeros? matrix posX posY l o))) '())
                             (else (set-word matrix posX posY word o))))))
          ((equal? o 4)
           (cond ((or (> (+ posX l) 10) (> (+ posY l) 10)) '())
                 (else (cond ((not (andmap boolean? (zeros? matrix posX posY l o))) '())
                             (else (set-word matrix posX posY word o))))))
          ((equal? o 5)
           (cond ((or (> (+ posX l) 10) (< (- posY l) -1)) '())
                 (else (cond ((not (andmap boolean? (zeros? matrix posX posY l o))) '())
                             (else (set-word matrix posX posY word o))))))
          ((equal? o 6)
           (cond ((or (< (- posX l) -1) (> (+ posY l) 10)) '())
                 (else (cond ((not (andmap boolean? (zeros? matrix posX posY l o))) '())
                             (else (set-word matrix posX posY word o))))))
          ((equal? o 7)
           (cond ((or (< (- posX l) -1) (< (- posY l) -1)) '())
                 (else (cond ((not (andmap boolean? (zeros? matrix posX posY l o))) '())
                             (else (set-word matrix posX posY word o)))))))))

; Devuelve una lista que puede contener #t si el espacio en la matriz está disponible
; y 0 si ya hay una letra
(define zeros?
  (lambda (matrix posX posY l o)
    (cond [(equal? l 0) (list #t)]
          [(equal? o 0) (cond
                          ((equal? (get-pos posX posY matrix) 0) (append (list #t) (zeros? matrix (+ posX 1) posY (- l 1) o)))
                          (else (append (list 0) (zeros? matrix (+ posX 1) posY (- l 1) o))))]
          [(equal? o 1) (cond
                          ((equal? (get-pos posX posY matrix) 0) (append (list #t) (zeros? matrix (- posX 1) posY (- l 1) o)))
                          (else (append (list 0) (zeros? matrix (- posX 1) posY (- l 1) o))))]
          [(equal? o 2) (cond
                          ((equal? (get-pos posX posY matrix) 0) (append (list #t) (zeros? matrix posX (+ posY 1) (- l 1) o)))
                          (else (append (list 0) (zeros? matrix posX (+ posY 1) (- l 1) o))))]
          [(equal? o 3) (cond
                          ((equal? (get-pos posX posY matrix) 0) (append (list #t) (zeros? matrix posX (- posY 1) (- l 1) o)))
                          (else (append (list 0) (zeros? matrix posX (- posY 1) (- l 1) o))))]
          [(equal? o 4) (cond
                          ((equal? (get-pos posX posY matrix) 0) (append (list #t) (zeros? matrix (+ posX 1) (+ posY 1) (- l 1) o)))
                          (else (append (list 0) (zeros? matrix (+ posX 1) (+ posY 1) (- l 1) o))))]
          [(equal? o 5) (cond
                          ((equal? (get-pos posX posY matrix) 0) (append (list #t) (zeros? matrix (+ posX 1) (- posY 1) (- l 1) o)))
                          (else (append (list 0) (zeros? matrix (+ posX 1) (- posY 1) (- l 1) o))))]
          [(equal? o 6) (cond
                          ((equal? (get-pos posX posY matrix) 0) (append (list #t) (zeros? matrix (- posX 1) (+ posY 1) (- l 1) o)))
                          (else (append (list 0) (zeros? matrix (- posX 1) (+ posY 1) (- l 1) o))))]
          [(equal? o 7) (cond
                          ((equal? (get-pos posX posY matrix) 0) (append (list #t) (zeros? matrix (- posX 1) (- posY 1) (- l 1) o)))
                          (else (append (list 0) (zeros? matrix (- posX 1) (- posY 1) (- l 1) o))))])))

; Coloca una palabra en la matriz desde una posX y posY en la orientación o
(define set-word
  (lambda (matrix posX posY word o)
    (cond [(null? word) matrix]
          [(equal? o 0) (set-word (change-y matrix posX posY (first word)) (+ posX 1) posY (rest word) o)] ; derecha
          [(equal? o 1) (set-word (change-y matrix posX posY (first word)) (- posX 1) posY (rest word) o)] ; izquierda
          [(equal? o 2) (set-word (change-y matrix posX posY (first word)) posX (+ posY 1) (rest word) o)] ; abajo
          [(equal? o 3) (set-word (change-y matrix posX posY (first word)) posX (- posY 1) (rest word) o)] ; arriba
          [(equal? o 4) (set-word (change-y matrix posX posY (first word)) (+ posX 1) (+ posY 1) (rest word) o)] ; abajo-derecha
          [(equal? o 5) (set-word (change-y matrix posX posY (first word)) (+ posX 1) (- posY 1) (rest word) o)] ; arriba-derecha
          [(equal? o 6) (set-word (change-y matrix posX posY (first word)) (- posX 1) (+ posY 1) (rest word) o)] ; abajo-izquierda
          [(equal? o 7) (set-word (change-y matrix posX posY (first word)) (- posX 1) (- posY 1) (rest word) o)] ; arriba-izquierda
          )))

; Coloca una letra dada en la posX y posY de la matriz
(define change-y
  (lambda (matrix posX posY letter) (list-set matrix posY (change-x (list-ref matrix posY) posX letter))))

; Coloca una letra dada en la posX de una lista
(define change-x
  (lambda (list posX letter) (list-set list posX letter)))

;--------------------------------------------------------------------------------------------------------------------------------------
; Devuelve la palabra que se encontró en las posiciones y orientación dada
; en caso de que no esté en la lista se lo indica
(define search
  (lambda (posX posY l o)
    (cond ((word-in-list (apply string-append (translate? posX posY l o)))
           (display
            (append
             '("Encontró la palabra:")
             (list (string-downcase (apply string-append (translate? posX posY l o)))))))
          (else (display (append '("La palabra no pertenece a la lista:")
                           (list (string-downcase (apply string-append (translate? posX posY l o))))))))))

; Cambia la palabra de orientación por el número
(define translate?
  (lambda (x y l o)
    (cond [(equal? o "derecha") (search-word x y l 0)]
          [(equal? o "izquierda") (search-word x y l 1)]
          [(equal? o "abajo") (search-word x y l 2)]
          [(equal? o "arriba") (search-word x y l 3)]
          [(equal? o "abajo-derecha") (search-word x y l 4)]
          [(equal? o "arriba-derecha") (search-word x y l 5)]
          [(equal? o "abajo-izquierda") (search-word x y l 6)]
          [(equal? o "arriba-izquierda") (search-word x y l 7)])))

; Devuelve la palabra que se le indica con las posiciones, el tamaño y la orientación
(define search-word
  (lambda (posX posY l o)
    (cond [(equal? l 0) '()]
          [(equal? o 0) (append (list (get-pos posX posY matrix)) (search-word (+ posX 1) posY (- l 1) o))]
          [(equal? o 1) (append (list (get-pos posX posY matrix)) (search-word (- posX 1) posY (- l 1) o))]
          [(equal? o 2) (append (list (get-pos posX posY matrix)) (search-word posX (+ posY 1) (- l 1) o))]
          [(equal? o 3) (append (list (get-pos posX posY matrix)) (search-word posX (- posY 1) (- l 1) o))]
          [(equal? o 4) (append (list (get-pos posX posY matrix)) (search-word (+ posX 1) (+ posY 1) (- l 1) o))]
          [(equal? o 5) (append (list (get-pos posX posY matrix)) (search-word (+ posX 1) (- posY 1) (- l 1) o))]
          [(equal? o 6) (append (list (get-pos posX posY matrix)) (search-word (- posX 1) (+ posY 1) (- l 1) o))]
          [(equal? o 7) (append (list (get-pos posX posY matrix)) (search-word (- posX 1) (- posY 1) (- l 1) o))])))

; Verifica si una palabra w está en la lista de palabras
(define word-in-list
  (lambda (w) (member w words)))

;--------------------------------------------------------------------------------------------------------------------------------------
; Retorna todas las rutas a seguir de un punto x,y
(define ways
  (lambda (posX posY o)
    (cond [(equal? o 8) '()]
          [(equal? o 0)
           (cond [(>= (+ posX 1) 10) (append '() (ways posX posY (+ o 1)))]
                 [else (append '(0) (ways posX posY (+ o 1)))])]
          [(equal? o 1)
           (cond [(<= (- posX 1) -1) (append '() (ways posX posY (+ o 1)))]
                 [else (append '(1) (ways posX posY (+ o 1)))])]
          [(equal? o 2)
           (cond [(>= (+ posY 1) 10) (append '() (ways posX posY (+ o 1)))]
                 [else (append '(2) (ways posX posY (+ o 1)))])]
          [(equal? o 3)
           (cond [(<= (- posY 1) -1) (append '() (ways posX posY (+ o 1)))]
                 [else (append '(3) (ways posX posY (+ o 1)))])]
          [(equal? o 4)
           (cond [(or (>= (+ posX 1) 10) (>= (+ posY 1) 10)) (append '() (ways posX posY (+ o 1)))]
                 [else (append '(4) (ways posX posY (+ o 1)))])]
          [(equal? o 5)
           (cond [(or (>= (+ posX 1) 10) (<= (- posY 1) -1)) (append '() (ways posX posY (+ o 1)))]
                 [else (append '(5) (ways posX posY (+ o 1)))])]
          [(equal? o 6)
           (cond [(or (<= (- posX 1) -1) (>= (+ posY 1) 10)) (append '() (ways posX posY (+ o 1)))]
                 [else (append '(6) (ways posX posY (+ o 1)))])]
          [(equal? o 7)
           (cond [(or (<= (- posX 1) -1) (<= (- posY 1) -1)) (append '() (ways posX posY (+ o 1)))]
                 [else (append '(7) (ways posX posY (+ o 1)))])])))

; Busca en una fila, diagonal o columna si hay una palabra presente empezando en la posX y posY dada
(define search-in-way
  (lambda (matrix posX posY x y l o)
    (cond [(or (equal? (get-length x y o) (length matrix)) (equal? (get-length x y o) -1)) '()]
          [(not (boolean? (word-in-list (apply string-append (search-word posX posY l o))))) (list posX posY l o)]
          [(equal? o 0) (search-in-way matrix posX posY (+ x 1) y (+ l 1) o)]
          [(equal? o 1) (search-in-way matrix posX posY (- x 1) y (+ l 1) o)]
          [(equal? o 2) (search-in-way matrix posX posY x (+ y 1) (+ l 1) o)]
          [(equal? o 3) (search-in-way matrix posX posY x (- y 1) (+ l 1) o)]
          [(equal? o 4) (search-in-way matrix posX posY (+ x 1) (+ y 1) (+ l 1) o)]
          [(equal? o 5) (search-in-way matrix posX posY (+ x 1) (- y 1) (+ l 1) o)]
          [(equal? o 6) (search-in-way matrix posX posY (- x 1) (+ y 1) (+ l 1) o)]
          [(equal? o 7) (search-in-way matrix posX posY (- x 1) (- y 1) (+ l 1) o)])))

; Obtiene un tamaño al cual hacer referencia para que search-in-way no se salga de la matriz
(define get-length
  (lambda (posX posY o)
    (cond [(equal? o 0) posX]
          [(equal? o 1) posX]
          [(equal? o 2) posY]
          [(equal? o 3) posY]
          [(equal? o 4) (cond [(> posX posY) posX]
                              [(< posX posY) posY]
                              [else posX])]
          [(equal? o 5) (cond [(equal? posX (length matrix)) posX]
                              [(equal? posY -1) posY]
                              [else posX])]
          [(equal? o 6) (cond [(equal? posX -1) posX]
                              [(equal? posY (length matrix)) posY]
                              [else posX])]
          [(equal? o 7) (cond [(> posX posY) posY]
                              [(< posX posY) posX]
                              [else posX])])))


; Filtra la matriz para quitar todos los valores nulos
(define (solve-matrix)
  (filter (lambda (element) (not (empty? element))) (solve-y matrix 0)))

; Resuelve una matriz dada
(define solve-y
  (lambda (list y) (cond [(equal? y (length list)) '()]
                         [else (append (solve-x (list-ref matrix y) 0 y) (solve-y list (+ y 1)))])))

; Resuelve una fila dada de la matriz
(define solve-x
  (lambda (list x y)
    (cond [(equal? x (length list)) '()]
          [else (cond [(null? (apply append (map (lambda (o) (search-in-way matrix x y x y 1 o)) (ways x y 0))))
                       (append '() (solve-x list (+ x 1) y))]
                      [else
                       (append (map (lambda (o) (search-in-way matrix x y x y 1 o)) (ways x y 0)) (solve-x list (+ x 1) y))])])))

; Hace el display de la solución
(define (solve)
  (map (lambda (x) (displayln x)) (display-solve)))

; Acomoda los valores
(define (display-solve)
  (map (lambda (item) (append (list "Palabra:") (list (name? item))
                              (list "| Posición X:") (list (x? item))
                              (list "| Posición Y:") (list (y? item))
                              (list "| Tamaño:") (list (l? item))
                              (list "| Orientación:") (list (o? item)))) (solve-matrix)))

; Devuelve la palabra con los datos del item
(define name?
  (lambda (item) (string-downcase (apply string-append (search-word
                  (list-ref item 0)
                  (list-ref item 1)
                  (list-ref item 2)
                  (list-ref item 3))))))

; Devuelve el item en la posicion 1: posición en X
(define x?
  (lambda (item) (list-ref item 0)))

; Devuelve el item en la posicion 2: posición en Y
(define y?
  (lambda (item) (list-ref item 1)))

; Devuelve el item en la posicion 3: tamaño de palabra
(define l?
  (lambda (item) (list-ref item 2)))

; Devuelve el item en la posicion 4: orientación
(define o?
  (lambda (item)
    (cond [(equal? (list-ref item 3) 0) "derecha"]
          [(equal? (list-ref item 3) 1) "izquierda"]
          [(equal? (list-ref item 3) 2) "abajo"]
          [(equal? (list-ref item 3) 3) "arriba"]
          [(equal? (list-ref item 3) 4) "abajo-derecha"]
          [(equal? (list-ref item 3) 5) "arriba-derecha"]
          [(equal? (list-ref item 3) 6) "abajo-izquierda"]
          [(equal? (list-ref item 3) 7) "arriba-izquierda"])))