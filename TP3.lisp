;Lenguajes de Programaciom
;Administracion de Tecnologias de Informacion
;Integrantes: Ariel Mora, Joshua Hern�ndez, Yader Morales
						   
(defparameter *contador* 0);Contador que se usar� en la tabla hash						   
						   
(defparameter *ht* (make-hash-table)) ;Realiza la tabla hash que va a contener los valores rescatados del pdf

(defclass file-pdf ()  ;Define la clase para el archivo PDF
	((file-name
		:initarg :file-name
		:initform "")
	 (author-name
		:initarg :author-name
		:initform "")
	 (keywords-name
		:initarg :keywords-name
		:initform "")
	 (CreateDate-name
		:initarg :CreateDate-name
		:initform "")))
		
;Se definen los Gets y Sets de las variables que posee la clase file-pdf
(defmethod getTitleName((file-pdf file-pdf))
	(slot-value file-pdf 'file-name))
	
(defmethod getAuthorName((file-pdf file-pdf))
	(slot-value file-pdf 'author-name))
	
(defmethod getKeyword((file-pdf file-pdf))
	(slot-value file-pdf 'keywords-name))
	
(defmethod getDateName((file-pdf file-pdf))
	(slot-value file-pdf 'CreateDate-name))

(defmethod setTitleName((file-pdf file-pdf) file-name)
	(setf (slot-value file-pdf 'file-name) file-name))
	
(defmethod setAuthorName((file-pdf file-pdf) author-name)
	(setf (slot-value file-pdf 'author-name) author-name))
	
(defmethod setKeyword((file-pdf file-pdf) keywords-name)
	(setf (slot-value file-pdf 'keywords-name) keywords-name))
	
(defmethod setDateName((file-pdf file-pdf) CreateDate-name)
	(setf (slot-value file-pdf 'CreateDate-name) CreateDate-name))
	
(defun component-present-p (value) ;Determina si la direccion pasada por par�metro es v�lida
  (and value (not (eql value :unspecific))))

(defun directory-pathname-p  (p) ;Comprueba si un nombre de ruta ya est� en forma de directorio
  (and(not (component-present-p (pathname-name p)))
  (not (component-present-p (pathname-type p)))p))

(defun pathname-as-directory (name) ;Convierte cualquier ruta de acceso a un directorio
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (if (not (directory-pathname-p name))
      (make-pathname

       :directory (append (or (pathname-directory pathname) (list :relative))
                          (list (file-namestring pathname)))
       :name      nil
       :type      nil
       :defaults pathname)
      pathname)))
      
(defun list-directory (dirname) ;Retorna el nombre de directorio en la forma correcta, para diferenciar subdirectorios de archivos regulares
  (when (wild-pathname-p dirname)
    (error "Can only list concrete directory names."))
  (directory (directory-wildcard dirname)))

(defun directory-wildcard (dirname);Convierte un nombre de directorio a un nombre de ruta v�lido
  (make-pathname
   :name :wild
   :type #-clisp :wild #+clisp nil
   :defaults (pathname-as-directory dirname)))

(defun walk-directory (dirname fn &key directories (test (constantly t))) ;Llama a todas las rutas de acceso de los archivos en el directorio, de forma recursiva
  (labels
      ((walk (name)
         (cond
           ((directory-pathname-p name)
            (when (and directories (funcall test name))
              (funcall fn name))
            (dolist (x (list-directory name)) (walk x)))
           ((funcall test name) (funcall fn name)))))
    (walk (pathname-as-directory dirname))))

(defun pdf-p (file) ;Busca el tipo de archivo en el directorio, que sea PDF
  (and (not (directory-pathname-p file))
  (string-equal "pdf" (pathname-type file))))