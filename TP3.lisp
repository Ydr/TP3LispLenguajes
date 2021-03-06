;Lenguajes de Programaciom
;Administracion de Tecnologias de Informacion
;Integrantes: Ariel Mora, Joshua Hernandez, Yader Morales
						   
(defparameter *contador* 0);Contador que se usara en la tabla hash						   
						   
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
	
(defun component-present-p (value) ;Determina si la direccion pasada por parametro es valida
  (and value (not (eql value :unspecific))))

(defun directory-pathname-p  (p) ;Comprueba si un nombre de ruta ya esta en forma de directorio
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

(defun directory-wildcard (dirname);Convierte un nombre de directorio a un nombre de ruta valido
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

(defun recorrerFichero (file); llama a la funcion recorrerFichero-aux con el archivo como parametro
	(recorrerFichero-aux file))

 
(defun recorrerFichero-aux (file) ;Recorre los archivos en busca de los tags necesarios y crea la tabla hash con los datos recopilados
 (let ((in (open file :direction 
        :input 
        :element-type 'unsigned-byte)))

	(let ((string (make-string (file-length in))))
	(dotimes (i (file-length in))
        (setf (char string i) (code-char (read-byte in))))
        (setf (gethash *contador* *ht*) (make-instance 'file-pdf 
		:file-name (if (NULL (buscarTag "Title" string)) "Ninguno" 
			(buscaCod "Title" "/Author" string))
		:author-name (if (NULL (buscarTag "Author" string)) "Ninguno" 
			(buscaCod "Author" "/Creator" string))					
		:keywords-name (if (NULL (buscarTag "Keywords" string)) "Ninguno" 
			(buscaCod "Keywords" "/APPL:Keywords" string))
		:CreateDate-name (if (NULL (buscarTag "CreationDate" string)) "Ninguno" 
			(buscaCod "CreationDate" "/ModDate" string))))
	(setf *contador* (+ *contador* 1) ))))
     
(defun buscarTag (tag string);Busca de acuerdo al tag en el string, con ayuda de la funcion search de lisp
	(search tag string))

(defun buscaCod (tag tag2 string);corta la informacion que se encuentra en los metadatos
	 (subseq string (+ (buscarTag tag string) (+ (length tag) 1)) (- (buscarTag tag2 string) 1)))
(defun buscar-en (fichero)  ;Funci�n que se debe llamar para pasar por parametro la direccion de los ficheros a buscar
	(walk-directory fichero #'recorrerFichero :test #'pdf-p))

;;;;;;
			
(defun buscar-titulo(titulo);Busca PDFs con un titulo en comun, si el parametro son comillas dobles, mostrara todos los datos
	(maphash #'(lambda (k v) (imprime-titulo v titulo)) *ht*))

(defun imprime-titulo (file titulo); Imprime los metatados referentes al titulo del PDF
	(if (search titulo (getTitleName file)) (format t 
		"Autor: ~a Palabras clave: ~a Fecha de creacion: ~a ~%" (getAuthorName file)(getKeyword file)(getDateName file))))
;;;;;

(defun buscar-creador(creador);Busca PDFs con un creador en comun, si el parametro son comillas dobles, mostrara todos los datos
	(maphash #'(lambda (k v) (imprime-creador v creador)) *ht*))

(defun imprime-creador (file creador) ;Imprime los metatados referentes al creador del PDF
	(if (search creador (getAuthorName file)) (format t 
	"Nombre de archivo: ~a Palabras clave: ~a Fecha de creacion: ~a ~%" (getTitleName file)(getKeyword file)(getDateName file))))

;;;

(defun buscar-fechacreacion(fechacreacion);Busca PDFs con una fecha de creacion en comun, si el parametro son comillas dobles, mostrara todos los datos
	(maphash #'(lambda (k v) (imprime-fecha v fechacreacion)) *ht*))

(defun imprime-fecha(file fechacreacion); Imprime los metatados referentes a la fecha de creacion del PDF
	(if (search fechacreacion (getDateName file)) (format t 
		"Nombre de archivo: ~a Autor: ~a Palabras clave: ~a ~%" (getTitleName file)(getAuthorName file)(getKeyword file))))

;;;
(defun buscar-keyword(keyword) ;Busca PDFs con keywords en comun, si el parametro son comillas dobles, mostrara todos los datos
	(maphash #'(lambda (k v) (imprime-keyword v keyword)) *ht*))

(defun imprime-keyword (file keyword); Imprime los metatados referentes al keyword del PDF
	(if (search keyword (getKeyword file)) (format t 
	"Nombre de archivo: ~s Autor: ~a Fecha de creacion: ~a ~%" (getTitleName file)(getAuthorName file)(getDateName file))))
