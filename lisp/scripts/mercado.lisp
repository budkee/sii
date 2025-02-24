;;defvar diferencias
(defparameter *db* nil)

; Funciones principales
(defun crea-entrada (producto qnt precio)
  (list :producto producto :qnt qnt :precio precio))

(defun agrega-lista (lista-mercado)
  (push lista-mercado *db*))


; Mostrar en la pantalla
(defun selecciona (producto)
  (remove-if-not #'(lambda (tupla) (equal (getf tupla :producto) producto))
		 *db*))
     
(defun selecciona-muestra-producto (producto)
  (dolist (tupla (selecciona producto))
    (format t "item: ~a~%" tupla)))

(defun muestra-db ()
  (dolist (tupla *db*)
    (format t "~{~a:~10t~a~%~}~%" tupla)))


;; Exemplo de uso
; (agrega-lista (crea-entrada "Manzana" 7))
; (agrega-lista (crea-entrada "Tomate" 5))
; (agrega-lista (crea-entrada "Queso" 2))

;; Entrada del usuario por CLI
(format t "## ----------- Lista de mercado ----------- ## ~%")
(loop
  (format t "~%Digite un producto (o 's' para salir): ")
  (finish-output)  ; Garante que lo prompt sea mostrado imediatamiente
  (let ((producto (read-line)))
    (when (string-equal (string-upcase (string-trim '(#\Space) producto)) "S")
      (return))
    (format t "Digite la cantidad: ")
    (finish-output)  ; Garante que lo prompt sea mostrado imediatamiente
    (let ((qnt (read-line)))
      (format t "Digite el precio: ")
      (finish-output)  ; Garante que lo prompt sea mostrado imediatamiente
      (let ((precio (read-line)))
        (agrega-lista (crea-entrada producto qnt precio))
        (format t "~%Añadido: ~a, Cantidad: ~a, Precio: €~a~%" producto qnt precio)
      )
    )
  )
)
;; Mostrar un insumo específico de la base de datos
;; (selecciona-muestra-insumos "Pan")

;; Mostrar la base de datos
(format t "~%## ----------- Lista de mercado ----------- ## ~%")
(muestra-db)

;; Salida del programa
;; (sb-ext:quit)
:continue