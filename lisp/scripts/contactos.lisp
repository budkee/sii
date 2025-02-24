;;defvar diferencias
(defparameter *db* nil)

; Funciones principales
(defun crea-entrada (nombre telefono)
  (list :nombre nombre :telefono telefono))

(defun agrega-telefono (contato)
  (push contato *db*))
  
(defun selecciona (nombre)
  (remove-if-not #'(lambda (tupla) (equal (getf tupla :nombre) nombre))
		 *db*))

(defun selecciona-muestra-nombre (nombre)
  (dolist (tupla (selecciona nombre))
    (format t "contacto: ~a~%" tupla)))


; Outras formas de mostrar la base de datos | No hace falta su presencia
;; 1. (muestra-db)
(defun muestra-db ()
  (dolist (tupla *db*)
    (format t "~{~a:~10t~a~%~}~%" tupla)))
;; 2. (muestra-db-nerd)
(defun muestra-db-nerd()
  (format t "~{~{~a:~10t~a~%~}~%~}" *db*))


;; Exemplo de uso
(agrega-telefono (crea-entrada "Luis" 95))
(agrega-telefono (crea-entrada "Luis" 96))
(agrega-telefono (crea-entrada "Alberto" 91))
(agrega-telefono (crea-entrada "Beatriz" 80))

(selecciona-muestra-nombre "Luis")
;; Salida del programa
;; (sb-ext:quit)
:continue