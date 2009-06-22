(in-package :mounit)

(defclass tag ()
  ((name :initarg :name :accessor name)
   (attributes :initarg :attributes :initform nil :accessor attributes)
   (children :initarg :children :initform nil :accessor children)))

(defmethod name (x)
  nil)

(defmethod attributes (x)
  nil)

(defmethod children (x)
  nil)

(defmethod name ((x list))
  (car x))

(defmethod attributes ((x list))
  (cadr x))

(defmethod children ((x list))
  (caddr x))

(defun make-tag (name attributes children)
  (make-instance 'tag :name name :attributes attributes :children children))

(defmethod map-attributes (function tag)
  (mapcar function (attributes tag)))

(defmethod map-children (function tag)
  (mapcar function (children tag)))
