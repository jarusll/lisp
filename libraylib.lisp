(ql:quickload 'cffi)
(ql:quickload 'cffi-libffi)

(cffi:define-foreign-library libraylib
    (:unix (:default "/usr/local/lib/libraylib"))
    (t (:default "libraylib")))

(unless (cffi:foreign-library-loaded-p 'libraylib)
    (cffi:use-foreign-library libraylib))

;;;; GLOBALS
(defparameter *draw-transform* (matrix! ((1 0 0 0)
					 (0 1 0 0)
					 (0 0 1 0)
					 (0 0 0 1))))

;;;; MACROS

(defmacro with-gensyms(symbols &body body)
    `(let ,(loop for sym in symbols collect `(,sym (gensym)))
        ,@body))


(defmacro v!(&rest items)
    (destructuring-bind (x y &optional z w) items
        (case (length items)
            (2 `(make-vector2 :x ,x :y ,y))
            (3 `(make-vector3 :x ,x :y ,y :z ,z))
            (4 `(make-vector4 :x ,x :y ,y :z ,z :w ,w)))))


(defmacro with-window(width height title &body body)
  `(progn
     (%init-window ,width ,height ,title)
     (unwind-protect (progn ,@body)
       (%close-window))))

(defmacro with-drawing(&body body)
  `(progn (%begin-drawing)
	  (unwind-protect (progn ,@body)
	    (%end-drawing))))

(defmacro with-bindings(let-form bindings &body body)
  (if bindings
      `(,let-form ,bindings
		  ,@body)
      `(progn
	 ,@body)))

(defmacro with-let(bindings &body body)
  `(with-bindings let ,bindings
     ,@body))

(defmacro with-let*(bindings &body body)
  `(with-bindings let* ,bindings
     ,@body))
    
(defmacro with-keys(keys &body body)
  (labels ((on-key(key &key
		   (up nil up-p)
		   (down nil down-p)
		   (pressed nil pressed-p)
		   (released nil released-p)
		   (pressed-repeat nil pressed-repeat-p))
      (loop for (action value supplied) in
	    `((:up ,up ,up-p)
	      (:down ,down ,down-p)
	      (:pressed ,pressed ,pressed-p) 
	      (:released ,released ,released-p)
	      (:pressed-repeat ,pressed-repeat ,pressed-repeat-p))
	    when supplied
	      collect `(,value (,(ecase action
				   (:up '%is-key-up)
				   (:down '%is-key-down)
				   (:pressed '%is-key-pressed)
				   (:released '%is-key-released)
				   (:pressed-repeat '%is-key-pressed-repeat))
				,key)))))
    `(let ,(loop for key in keys append (apply #'on-key key))
       ,@body)))


(defmacro with-mouse(actions &body body)
  (labels ((mouse-button-p(keyword)
	     (member keyword '(:left :right :middle :side :extra :forward :back)))
	   (mouse-action->function(button)
	     (ecase button
	       (:up '%is-mouse-button-up)
	       (:down '%is-mouse-button-down)
	       (:pressed '%is-mouse-button-pressed)
	       (:released '%is-mouse-button-released)))
	   (mouse-button-bindings(button &key (up nil up-supplied-p)
					   (down nil down-supplied-p)
					   (pressed nil pressed-supplied-p)
					   (released nil released-supplied-p))
	     (loop for (mouse-action binding binding-supplied) in
		   `((:up ,up ,up-supplied-p)
		     (:down ,down ,down-supplied-p)
		     (:pressed ,pressed ,pressed-supplied-p)
		     (:released ,released ,released-supplied-p))
		   when binding-supplied collect `(,binding (,(mouse-action->function mouse-action) ,button))))
	   (position-bindings(&key (x nil x-supplied-p)
				(y nil y-supplied-y))
	     (with-gensyms(position-binding)
	       `((,position-binding (%get-mouse-position))
		 ,@(loop for (axis binding binding-supplied) in
			 `((:x ,x ,x-supplied-p)
			   (:y ,y ,y-supplied-y))
			 when binding-supplied
			   collect `(,binding ,(ecase axis
						 (:x `(vector2-x ,position-binding))
						 (:y `(vector2-y ,position-binding))))))))
	   (delta-bindings(&key (x nil x-supplied-p)
			     (y nil y-supplied-y))
	     (with-gensyms(delta-binding)
	       `((,delta-binding (%get-mouse-delta))
		 ,@(loop for (axis binding binding-supplied) in
			 `((:x ,x ,x-supplied-p)
			   (:y ,y ,y-supplied-y))
			 when binding-supplied
			   collect `(,binding ,(ecase axis
						 (:x `(vector2-x ,delta-binding))
						 (:y `(vector2-y ,delta-binding))))))))
	   (wheel-bindings(&key (x nil x-supplied-p)
			     (y nil y-supplied-p)
			     (move nil move-supplied-p))
	     `(,@(when (or x-supplied-p y-supplied-p)
		   (with-gensyms(wheel-binding)
		     `((,wheel-binding (%get-mouse-wheel-move-v))
		       ,@(loop for (axis binding binding-supplied) in
			       `((:x ,x ,x-supplied-p)
				 (:y ,y ,y-supplied-p))
			       when binding-supplied
				 collect `(,binding ,(ecase axis
						       (:x `(vector2-x ,wheel-binding))
						       (:y `(vector2-y ,wheel-binding))))))))
	       ,@(when move-supplied-p
		   `((,move (%get-mouse-wheel-move)))))))
    (loop for action in actions
	  for action-type = (car action)
	  when (mouse-button-p action-type) append (apply #'mouse-button-bindings action) into mouse-bindings
	    when (eq action-type :position) append (apply #'position-bindings (cdr action)) into position-bindings
	      when (eq action-type :delta) append (apply #'delta-bindings (cdr action)) into delta-bindings
		when (eq action-type :wheel) append (apply #'wheel-bindings (cdr action)) into wheel-bindings
		  finally (return `(with-let ,mouse-bindings
				     (with-let* ,position-bindings
				       (with-let* ,delta-bindings
					 (with-let* ,wheel-bindings
					   ,@body))))))))

(defmacro with-texture(texture-var width height &body body)
  `(let ((,texture-var (%load-render-texture ,width ,height)))
     (unwind-protect (progn ,@body)
       (%unload-render-texture ,texture-var))))

(defmacro with-texture-mode(target &body body)
  `(progn (%begin-texture-mode ,target)
          (unwind-protect (progn ,@body)
            (%end-texture-mode))))

(defmacro with-camera-2d(camera &body body)
  `(progn (%begin-mode-2d ,camera)
          (unwind-protect (progn ,@body)
            (%end-mode-2d))))

(defmacro matrix! (rows)
  "Supply up to 4 rows of up to 4 values. Missing columns and rows are filled with 0."
  (let ((r0 (append (coerce (or (nth 0 rows) '()) 'list)
                    '(1 0 0 0)))
        (r1 (append (coerce (or (nth 1 rows) '()) 'list)
                    '(0 1 0 0)))
        (r2 (append (coerce (or (nth 2 rows) '()) 'list)
                    '(0 0 1 0)))
        (r3 (append (coerce (or (nth 3 rows) '()) 'list)
                    '(0 0 0 1))))
    `(make-matrix :m0 ,(nth 0 r0) :m4 ,(nth 1 r0) :m8 ,(nth 2 r0) :m12 ,(nth 3 r0)
                  :m1 ,(nth 0 r1) :m5 ,(nth 1 r1) :m9 ,(nth 2 r1) :m13 ,(nth 3 r1)
                  :m2 ,(nth 0 r2) :m6 ,(nth 1 r2) :m10 ,(nth 2 r2) :m14 ,(nth 3 r2)
                  :m3 ,(nth 0 r3) :m7 ,(nth 1 r3) :m11 ,(nth 2 r3) :m15 ,(nth 3 r3))))

(defun matrix* (a b)
  (with-members (m0 m4 m8 m12 m1 m5 m9 m13 m2 m6 m10 m14 m3 m7 m11 m15) a matrix
    (with-members ((m0 b-m0) (m4 b-m4) (m8 b-m8) (m12 b-m12)
                    (m1 b-m1) (m5 b-m5) (m9 b-m9) (m13 b-m13)
                    (m2 b-m2) (m6 b-m6) (m10 b-m10) (m14 b-m14)
                    (m3 b-m3) (m7 b-m7) (m11 b-m11) (m15 b-m15)) b matrix
      (make-matrix
        :m0  (+ (* m0 b-m0)  (* m4 b-m1)  (* m8 b-m2)   (* m12 b-m3))
        :m4  (+ (* m0 b-m4)  (* m4 b-m5)  (* m8 b-m6)   (* m12 b-m7))
        :m8  (+ (* m0 b-m8)  (* m4 b-m9)  (* m8 b-m10)  (* m12 b-m11))
        :m12 (+ (* m0 b-m12) (* m4 b-m13) (* m8 b-m14)  (* m12 b-m15))
        :m1  (+ (* m1 b-m0)  (* m5 b-m1)  (* m9 b-m2)   (* m13 b-m3))
        :m5  (+ (* m1 b-m4)  (* m5 b-m5)  (* m9 b-m6)   (* m13 b-m7))
        :m9  (+ (* m1 b-m8)  (* m5 b-m9)  (* m9 b-m10)  (* m13 b-m11))
        :m13 (+ (* m1 b-m12) (* m5 b-m13) (* m9 b-m14)  (* m13 b-m15))
        :m2  (+ (* m2 b-m0)  (* m6 b-m1)  (* m10 b-m2)  (* m14 b-m3))
        :m6  (+ (* m2 b-m4)  (* m6 b-m5)  (* m10 b-m6)  (* m14 b-m7))
        :m10 (+ (* m2 b-m8)  (* m6 b-m9)  (* m10 b-m10) (* m14 b-m11))
        :m14 (+ (* m2 b-m12) (* m6 b-m13) (* m10 b-m14) (* m14 b-m15))
        :m3  (+ (* m3 b-m0)  (* m7 b-m1)  (* m11 b-m2)  (* m15 b-m3))
        :m7  (+ (* m3 b-m4)  (* m7 b-m5)  (* m11 b-m6)  (* m15 b-m7))
        :m11 (+ (* m3 b-m8)  (* m7 b-m9)  (* m11 b-m10) (* m15 b-m11))
         :m15 (+ (* m3 b-m12) (* m7 b-m13) (* m11 b-m14) (* m15 b-m15))))))

(defmacro translate-matrix! (&optional (x 0) (y 0) (z 0))
  `(matrix! ((1 0 0 ,x)
             (0 1 0 ,y)
             (0 0 1 ,z)
             (0 0 0 1))))

(defmacro scale-matrix! (&optional (x 1) (y 1) (z 1))
  `(matrix! ((,x 0 0 0)
             (0 ,y 0 0)
             (0 0 ,z 0)
             (0 0 0 1))))

(defmacro rotate-matrix!(deg)
  (with-gensyms(radian-value)
    `(let ((,radian-value (deg->rad ,deg)))
       (matrix! (((cos ,radian-value) (- (sin ,radian-value)))
		 ((sin ,radian-value) (cos ,radian-value)))))))

(defmacro with-members(members obj type &body body)
  (with-gensyms(obj-var)
    `(let ((,obj-var ,obj))
       (symbol-macrolet
           ,(loop for member in members
		  for binding = (if (atom member) member (second member))
		  for slot = (if (atom member) member (first member))
                  for accessor = (format nil "~a-~a" type slot)
                  collecting `(,binding (,(intern accessor (symbol-package type)) ,obj-var)))
         ,@body))))

;;;; ENUMS

(cffi:defcenum keyboard-key
    (:null             0)        ; key: null used for no key pressed
    (:apostrophe       39)       ; key: '
    (:comma            44)       ; key: )
    (:minus            45)       ; key: -
    (:period           46)       ; key: .
    (:slash            47)       ; key: /
    (:zero             48)       ; key: 0
    (:one              49)       ; key: 1
    (:two              50)       ; key: 2
    (:three            51)       ; key: 3
    (:four             52)       ; key: 4
    (:five             53)       ; key: 5
    (:six              54)       ; key: 6
    (:seven            55)       ; key: 7
    (:eight            56)       ; key: 8
    (:nine             57)       ; key: 9
    (:semicolon        59)       ; key: ;
    (:equal            61)       ; key: =
    (:a                65)       ; key: a | a
    (:b                66)       ; key: b | b
    (:c                67)       ; key: c | c
    (:d                68)       ; key: d | d
    (:e                69)       ; key: e | e
    (:f                70)       ; key: f | f
    (:g                71)       ; key: g | g
    (:h                72)       ; key: h | h
    (:i                73)       ; key: i | i
    (:j                74)       ; key: j | j
    (:k                75)       ; key: k | k
    (:l                76)       ; key: l | l
    (:m                77)       ; key: m | m
    (:n                78)       ; key: n | n
    (:o                79)       ; key: o | o
    (:p                80)       ; key: p | p
    (:q                81)       ; key: q | q
    (:r                82)       ; key: r | r
    (:s                83)       ; key: s | s
    (:t                84)       ; key: t | t
    (:u                85)       ; key: u | u
    (:v                86)       ; key: v | v
    (:w                87)       ; key: w | w
    (:x                88)       ; key: x | x
    (:y                89)       ; key: y | y
    (:z                90)       ; key: z | z
    (:left_bracket     91)       ; key: [
    (:backslash        92)       ; key: '\'
    (:right_bracket    93)       ; key: ]
    (:grave            96)       ; key: `
    (:space            32)       ; key: space
    (:escape           256)      ; key: esc
    (:enter            257)      ; key: enter
    (:tab              258)      ; key: tab
    (:backspace        259)      ; key: backspace
    (:insert           260)      ; key: ins
    (:delete           261)      ; key: del
    (:right            262)      ; key: cursor right
    (:left             263)      ; key: cursor left
    (:down             264)      ; key: cursor down
    (:up               265)      ; key: cursor up
    (:page_up          266)      ; key: page up
    (:page_down        267)      ; key: page down
    (:home             268)      ; key: home
    (:end              269)      ; key: end
    (:caps_lock        280)      ; key: caps lock
    (:scroll_lock      281)      ; key: scroll down
    (:num_lock         282)      ; key: num lock
    (:print_screen     283)      ; key: print screen
    (:pause            284)      ; key: pause
    (:f1               290)      ; key: f1
    (:f2               291)      ; key: f2
    (:f3               292)      ; key: f3
    (:f4               293)      ; key: f4
    (:f5               294)      ; key: f5
    (:f6               295)      ; key: f6
    (:f7               296)      ; key: f7
    (:f8               297)      ; key: f8
    (:f9               298)      ; key: f9
    (:f10              299)      ; key: f10
    (:f11              300)      ; key: f11
    (:f12              301)      ; key: f12
    (:left_shift       340)      ; key: shift left
    (:left_control     341)      ; key: control left
    (:left_alt         342)      ; key: alt left
    (:left_super       343)      ; key: super left
    (:right_shift      344)      ; key: shift right
    (:right_control    345)      ; key: control right
    (:right_alt        346)      ; key: alt right
    (:right_super      347)      ; key: super right
    (:kb_menu          348)      ; key: kb menu
    (:kp_0             320)      ; key: keypad 0
    (:kp_1             321)      ; key: keypad 1
    (:kp_2             322)      ; key: keypad 2
    (:kp_3             323)      ; key: keypad 3
    (:kp_4             324)      ; key: keypad 4
    (:kp_5             325)      ; key: keypad 5
    (:kp_6             326)      ; key: keypad 6
    (:kp_7             327)      ; key: keypad 7
    (:kp_8             328)      ; key: keypad 8
    (:kp_9             329)      ; key: keypad 9
    (:kp_decimal       330)      ; key: keypad .
    (:kp_divide        331)      ; key: keypad /
    (:kp_multiply      332)      ; key: keypad *
    (:kp_subtract      333)      ; key: keypad -
    (:kp_add           334)      ; key: keypad +
    (:kp_enter         335)      ; key: keypad enter
    (:kp_equal         336)      ; key: keypad =
    (:back             4)        ; key: android back button
    (:menu             5)        ; key: android menu button
    (:volume_up        24)       ; key: android volume up button
    (:volume_down      25))      ; key: android volume down button


(cffi:defcenum mouse-key
    (:left     0)       ; mouse button left
    (:right    1)       ; mouse button right
    (:middle   2)       ; mouse button middle (pressed wheel)
    (:side     3)       ; mouse button side (advanced mouse device)
    (:extra    4)       ; mouse button extra (advanced mouse device)
    (:forward  5)       ; mouse button forward (advanced mouse device)
    (:back     6)       ; mouse button back (advanced mouse device)
)

;;;; STRUCTS

; // Vector2, 2 components
; typedef struct Vector2 {
;     float x;                // Vector x component
;     float y;                // Vector y component
; } Vector2;

(cffi:defcstruct (%Vector2 :class vector2-type)
    (x :float)
    (y :float))

(defstruct vector2
    x
    y)

(defmethod cffi:translate-into-foreign-memory
    ((value vector2) (type vector2-type) pointer)
    (cffi:with-foreign-slots ((x y) pointer (:struct %Vector2))
        (setf x (coerce (vector2-x value) 'float)
                y (coerce (vector2-y value) 'float))))

(defmethod cffi:translate-from-foreign (ptr (type vector2-type))
    (cffi:with-foreign-slots ((x y) ptr (:struct %Vector2))
        (make-vector2 :x x :y y)))

; // Vector3, 3 components
; typedef struct Vector3 {
;     float x;                // Vector x component
;     float y;                // Vector y component
;     float z;                // Vector z component
; } Vector3;

(cffi:defcstruct (%Vector3 :class vector3-type)
    (x :float)
    (y :float)
    (z :float))

(defstruct vector3
    x
    y
    z)

(defmethod cffi:translate-into-foreign-memory
    ((value vector3) (type vector3-type) pointer)
  (cffi:with-foreign-slots ((x y z) pointer (:struct %Vector3))
    (with-members ((x lx) (y ly) (z lz)) value vector3
      (setf x lx
            y ly
            z lz))))

(defmethod cffi:translate-from-foreign (ptr (type vector3-type))
  (cffi:with-foreign-slots ((x y z) ptr (:struct %Vector3))
    (make-vector3 :x x :y y :z z)))

; // Vector4, 4 components
; typedef struct Vector4 {
;     float x;                // Vector x component
;     float y;                // Vector y component
;     float z;                // Vector z component
;     float w;                // Vector w component
; } Vector4;

(cffi:defcstruct (%Vector4 :class vector4-type)
    (x :float)
    (y :float)
    (z :float)
    (w :float))

(defstruct vector4
    x
    y
    z
    w)

(defmethod cffi:translate-into-foreign-memory
    ((value vector4) (type vector4-type) pointer)
  (cffi:with-foreign-slots ((x y z w) pointer (:struct %Vector4))
    (with-members ((x lx) (y ly) (z lz) (w lw)) value vector4
      (setf x lx
            y ly
            z lz
            w lw))))

(defmethod cffi:translate-from-foreign (ptr (type vector4-type))
  (cffi:with-foreign-slots ((x y z w) ptr (:struct %Vector4))
    (make-vector4 :x x :y y :z z :w w)))

; // Quaternion, 4 components (Vector4 alias)
; typedef Vector4 Quaternion;
(cffi:defctype %Quaternion (:struct %Vector4))


; // Matrix, 4x4 components, column major, OpenGL style, right-handed
; typedef struct Matrix {
;     float m0, m4, m8, m12;  // Matrix first row (4 components)
;     float m1, m5, m9, m13;  // Matrix second row (4 components)
;     float m2, m6, m10, m14; // Matrix third row (4 components)
;     float m3, m7, m11, m15; // Matrix fourth row (4 components)
; } Matrix;

(cffi:defcstruct (%Matrix :class matrix-type)
    (m0 :float)
    (m4 :float)
    (m8 :float)
    (m12 :float)
    (m1 :float)
    (m5 :float)
    (m9 :float)
    (m13 :float)
    (m2 :float)
    (m6 :float)
    (m10 :float)
    (m14 :float)
    (m3 :float)
    (m7 :float)
    (m11 :float)
    (m15 :float))

(defstruct matrix
    m0
    m4
    m8
    m12
    m1
    m5
    m9
    m13
    m2
    m6
    m10
    m14
    m3
    m7
    m11
    m15)

(defmethod cffi:translate-into-foreign-memory
    ((value matrix) (type matrix-type) pointer)
  (cffi:with-foreign-slots ((m0 m4 m8 m12 m1 m5 m9 m13 m2 m6 m10 m14 m3 m7 m11 m15) pointer (:struct %Matrix))
    (setf m0 (coerce (matrix-m0 value) 'float)
          m4 (coerce (matrix-m4 value) 'float)
          m8 (coerce (matrix-m8 value) 'float)
          m12 (coerce (matrix-m12 value) 'float)
          m1 (coerce (matrix-m1 value) 'float)
          m5 (coerce (matrix-m5 value) 'float)
          m9 (coerce (matrix-m9 value) 'float)
          m13 (coerce (matrix-m13 value) 'float)
          m2 (coerce (matrix-m2 value) 'float)
          m6 (coerce (matrix-m6 value) 'float)
          m10 (coerce (matrix-m10 value) 'float)
          m14 (coerce (matrix-m14 value) 'float)
          m3 (coerce (matrix-m3 value) 'float)
          m7 (coerce (matrix-m7 value) 'float)
          m11 (coerce (matrix-m11 value) 'float)
          m15 (coerce (matrix-m15 value) 'float))))

(defmethod cffi:translate-from-foreign (ptr (type matrix-type))
  (cffi:with-foreign-slots ((m0 m4 m8 m12 m1 m5 m9 m13 m2 m6 m10 m14 m3 m7 m11 m15) ptr (:struct %Matrix))
    (make-matrix :m0 m0 :m4 m4 :m8 m8 :m12 m12
                 :m1 m1 :m5 m5 :m9 m9 :m13 m13
                 :m2 m2 :m6 m6 :m10 m10 :m14 m14
                 :m3 m3 :m7 m7 :m11 m11 :m15 m15)))

; // Color, 4 components, R8G8B8A8 (32bit)
; typedef struct Color {
;     unsigned char r;        // Color red value
;     unsigned char g;        // Color green value
;     unsigned char b;        // Color blue value
;     unsigned char a;        // Color alpha value
; } Color;

(cffi:defcstruct (%Color :class color-type)
    (r :unsigned-char)
    (g :unsigned-char)
    (b :unsigned-char)
    (a :unsigned-char))

(defstruct color
  r
  g
  b
  a)

(defmacro color!(r g b a)
  `(make-color :r ,r :g ,g :b ,b :a ,a))

(defmethod cffi:translate-into-foreign-memory
    ((value color) (type color-type) pointer)
  (cffi:with-foreign-slots ((r g b a) pointer (:struct %Color))
    (with-members ((r lr) (g lg) (b lb) (a la)) value color
      (setf r lr
	    g lg
	    b lb
	    a la))))

(defmethod cffi:translate-from-foreign (ptr (type color-type))
  (cffi:with-foreign-slots ((r g b a) ptr (:struct %Color))
    (make-color :r r :g g :b b :a a)))

(defmethod cffi:translate-into-foreign-memory
    ((value symbol) (type color-type) pointer)
  (cffi:with-foreign-slots ((r g b a) pointer (:struct %Color))
    (ecase value
      (:lightgray  (setf r 200 g 200 b 200 a 255))
      (:gray       (setf r 130 g 130 b 130 a 255))
      (:darkgray   (setf r 80  g 80  b 80  a 255))
      (:yellow     (setf r 253 g 249 b 0   a 255))
      (:gold       (setf r 255 g 203 b 0   a 255))
      (:orange     (setf r 255 g 161 b 0   a 255))
      (:pink       (setf r 255 g 109 b 194 a 255))
      (:red        (setf r 230 g 41  b 55  a 255))
      (:maroon     (setf r 190 g 33  b 55  a 255))
      (:green      (setf r 0   g 228 b 48  a 255))
      (:lime       (setf r 0   g 158 b 47  a 255))
      (:darkgreen  (setf r 0   g 117 b 44  a 255))
      (:skyblue    (setf r 102 g 191 b 255 a 255))
      (:blue       (setf r 0   g 121 b 241 a 255))
      (:darkblue   (setf r 0   g 82  b 172 a 255))
      (:purple     (setf r 200 g 122 b 255 a 255))
      (:violet     (setf r 135 g 60  b 190 a 255))
      (:darkpurple (setf r 112 g 31  b 126 a 255))
      (:beige      (setf r 211 g 176 b 131 a 255))
      (:brown      (setf r 127 g 106 b 79  a 255))
      (:darkbrown  (setf r 76  g 63  b 47  a 255))
      (:white      (setf r 255 g 255 b 255 a 255))
      (:black      (setf r 0   g 0   b 0   a 255))
      (:blank      (setf r 0   g 0   b 0   a 0))
      (:magenta    (setf r 255 g 0   b 255 a 255))
      (:raywhite   (setf r 245 g 245 b 245 a 255)))))

; // Rectangle, 4 components
; typedef struct Rectangle {
;     float x;                // Rectangle top-left corner position x
;     float y;                // Rectangle top-left corner position y
;     float width;            // Rectangle width
;     float height;           // Rectangle height
; } Rectangle;

(cffi:defcstruct (%Rectangle :class rectangle-type)
    (x :float)
    (y :float)
    (width :float)
    (height :float))

(defstruct rectangle
    x
    y
    width
    height)

; // Image, pixel data stored in CPU memory (RAM)
; typedef struct Image {
;     void *data;             // Image raw data
;     int width;              // Image base width
;     int height;             // Image base height
;     int mipmaps;            // Mipmap levels, 1 by default
;     int format;             // Data format (PixelFormat type)
; } Image;

(cffi:defcstruct (%Image :class image-type)
    (data :pointer)
    (width :int)
    (height :int)
    (mipmaps :int)
    (format :int))

(defstruct image
    data
    width
    height
    mipmaps
    format)

; // Texture, tex data stored in GPU memory (VRAM)
; typedef struct Texture {
;     unsigned int id;        // OpenGL texture id
;     int width;              // Texture base width
;     int height;             // Texture base height
;     int mipmaps;            // Mipmap levels, 1 by default
;     int format;             // Data format (PixelFormat type)
; } Texture;

(cffi:defcstruct (%Texture :class texture-type)
    (id :unsigned-int)
    (width :int)
    (height :int)
    (mipmaps :int)
    (format :int))

(defstruct texture
    id
    width
    height
    mipmaps
    format)

(defmethod cffi:translate-into-foreign-memory
    ((value texture) (type texture-type) pointer)
  (cffi:with-foreign-slots ((id width height mipmaps format) pointer (:struct %Texture))
    (with-members ((id l-id) (width l-width) (height l-height) (mipmaps l-mipmaps) (format l-format)) value texture
      (setf id l-id
            width l-width
            height l-height
            mipmaps l-mipmaps
            format l-format))))

(defmethod cffi:translate-from-foreign (ptr (type texture-type))
  (cffi:with-foreign-slots ((id width height mipmaps format) ptr (:struct %Texture))
    (make-texture :id id :width width :height height :mipmaps mipmaps :format format)))

; // Texture2D, same as Texture
; typedef Texture Texture2D;
(cffi:defctype %Texture2D (:struct %Texture))
 
; // TextureCubemap, same as Texture
; typedef Texture TextureCubemap;
(cffi:defctype %TextureCubemap (:struct %Texture))

; // RenderTexture, fbo for texture rendering
; typedef struct RenderTexture {
;     unsigned int id;        // OpenGL framebuffer object id
;     Texture texture;        // Color buffer attachment texture
;     Texture depth;          // Depth buffer attachment texture
; } RenderTexture;

(cffi:defcstruct (%RenderTexture :class render-texture-type)
    (id :unsigned-int)
    (texture (:struct %Texture))
    (depth (:struct %Texture)))

(defstruct render-texture
    id
    texture
    depth)

(defmethod cffi:translate-into-foreign-memory
    ((value render-texture) (type render-texture-type) pointer)
  (cffi:with-foreign-slots ((id texture depth) pointer (:struct %RenderTexture))
    (setf id (render-texture-id value))
    (cffi:translate-into-foreign-memory
        (render-texture-texture value)
        (cffi::parse-type '(:struct %Texture))
        (cffi:foreign-slot-pointer pointer '(:struct %RenderTexture) 'texture))
    (cffi:translate-into-foreign-memory
        (render-texture-depth value)
        (cffi::parse-type '(:struct %Texture))
        (cffi:foreign-slot-pointer pointer '(:struct %RenderTexture) 'depth))))

(defmethod cffi:translate-from-foreign (ptr (type render-texture-type))
  (cffi:with-foreign-slots ((id texture depth) ptr (:struct %RenderTexture))
    (make-render-texture :id id
                         :texture (cffi:translate-from-foreign
                                    (cffi:foreign-slot-pointer ptr '(:struct %RenderTexture) 'texture)
                                    (cffi::parse-type '(:struct %Texture)))
                         :depth (cffi:translate-from-foreign
                                  (cffi:foreign-slot-pointer ptr '(:struct %RenderTexture) 'depth)
                                  (cffi::parse-type '(:struct %Texture))))))

; // RenderTexture2D, same as RenderTexture
; typedef RenderTexture RenderTexture2D;
(cffi:defctype %RenderTexture2D (:struct %RenderTexture))

; // NPatchInfo, n-patch layout info
; typedef struct NPatchInfo {
;     Rectangle source;       // Texture source rectangle
;     int left;               // Left border offset
;     int top;                // Top border offset
;     int right;              // Right border offset
;     int bottom;             // Bottom border offset
;     int layout;             // Layout of the n-patch: 3x3, 1x3 or 3x1
; } NPatchInfo;

(cffi:defcstruct (%NPatchInfo :class n-patch-info-type)
    (source (:struct %Rectangle))
    (left :int)
    (top :int)
    (right :int)
    (bottom :int)
    (layout :int))

(defstruct n-patch-info
    source
    left
    top
    right
    bottom
    layout)

; // GlyphInfo, font characters glyphs info
; typedef struct GlyphInfo {
;     int value;              // Character value (Unicode)
;     int offsetX;            // Character offset X when drawing
;     int offsetY;            // Character offset Y when drawing
;     int advanceX;           // Character advance position X
;     Image image;            // Character image data
; } GlyphInfo;

(cffi:defcstruct (%GlyphInfo :class glyph-info-type)
    (value :int)
    (offsetX :int)
    (offsetY :int)
    (advanceX :int)
    (image (:struct %Image)))


(defstruct glyph-info
    value
    offset-x
    offset-y
    advance-x
    image)

; // Font, font texture and GlyphInfo array data
; typedef struct Font {
;     int baseSize;           // Base size (default chars height)
;     int glyphCount;         // Number of glyph characters
;     int glyphPadding;       // Padding around the glyph characters
;     Texture2D texture;      // Texture atlas containing the glyphs
;     Rectangle *recs;        // Rectangles in texture for the glyphs
;     GlyphInfo *glyphs;      // Glyphs info data
; } Font;

(cffi:defcstruct (%Font :class font-type)
    (baseSize :int)
    (glyphCount :int)
    (glyphPadding :int)
    (texture (:struct %Texture))
    (recs :pointer)
    (glyphs :pointer))

(defstruct font
    base-size
    glyph-count
    glyph-padding
    texture
    recs
    glyphs)

; // Camera, defines position/orientation in 3d space
; typedef struct Camera3D {
;     Vector3 position;       // Camera position
;     Vector3 target;         // Camera target it looks-at
;     Vector3 up;             // Camera up vector (rotation over its axis)
;     float fovy;             // Camera field-of-view aperture in Y (degrees) in perspective, used as near plane height in world units in orthographic
;     int projection;         // Camera projection: CAMERA_PERSPECTIVE or CAMERA_ORTHOGRAPHIC
; } Camera3D;

(cffi:defcstruct (%Camera3D :class camera-3d-type)
    (position (:struct %Vector3))
    (target (:struct %Vector3))
    (up (:struct %Vector3))
    (fovy :float)
    (projection :int))

(defstruct camera-3d
    position
    target
    up
    fovy
    projection)

; typedef Camera3D Camera;    // Camera type fallback, defaults to Camera3D
(cffi:defctype %Camera (:struct %Camera3D))

; // Camera2D, defines position/orientation in 2d space
; typedef struct Camera2D {
;     Vector2 offset;         // Camera offset (screen space offset from window origin)
;     Vector2 target;         // Camera target (world space target point that is mapped to screen space offset)
;     float rotation;         // Camera rotation in degrees (pivots around target)
;     float zoom;             // Camera zoom (scaling around target), must not be set to 0, set to 1.0f for no scale
; } Camera2D;

(cffi:defcstruct (%Camera2D :class camera-2d-type)
    (offset (:struct %Vector2))
    (target (:struct %Vector2))
    (rotation :float)
    (zoom :float))

(defstruct camera-2d
    offset
    target
    rotation
    zoom)

(defmethod cffi:translate-into-foreign-memory
    ((value camera-2d) (type camera-2d-type) pointer)
  (cffi:with-foreign-slots ((offset target rotation zoom) pointer (:struct %Camera2D))
    (cffi:translate-into-foreign-memory
        (camera-2d-offset value)
        (cffi::parse-type '(:struct %Vector2))
        (cffi:foreign-slot-pointer pointer '(:struct %Camera2D) 'offset))
    (cffi:translate-into-foreign-memory
        (camera-2d-target value)
        (cffi::parse-type '(:struct %Vector2))
        (cffi:foreign-slot-pointer pointer '(:struct %Camera2D) 'target))
    (setf rotation (camera-2d-rotation value)
          zoom (camera-2d-zoom value))))

(defmethod cffi:translate-from-foreign (ptr (type camera-2d-type))
  (cffi:with-foreign-slots ((offset target rotation zoom) ptr (:struct %Camera2D))
    (make-camera-2d :offset (cffi:translate-from-foreign
                              (cffi:foreign-slot-pointer ptr '(:struct %Camera2D) 'offset)
                              (cffi::parse-type '(:struct %Vector2)))
                    :target (cffi:translate-from-foreign
                              (cffi:foreign-slot-pointer ptr '(:struct %Camera2D) 'target)
                              (cffi::parse-type '(:struct %Vector2)))
                    :rotation rotation
                    :zoom zoom)))

; // Mesh, vertex data and vao/vbo
; typedef struct Mesh {
;     int vertexCount;        // Number of vertices stored in arrays
;     int triangleCount;      // Number of triangles stored (indexed or not)

;     // Vertex attributes data
;     float *vertices;        // Vertex position (XYZ - 3 components per vertex) (shader-location = 0)
;     float *texcoords;       // Vertex texture coordinates (UV - 2 components per vertex) (shader-location = 1)
;     float *texcoords2;      // Vertex texture second coordinates (UV - 2 components per vertex) (shader-location = 5)
;     float *normals;         // Vertex normals (XYZ - 3 components per vertex) (shader-location = 2)
;     float *tangents;        // Vertex tangents (XYZW - 4 components per vertex) (shader-location = 4)
;     unsigned char *colors;  // Vertex colors (RGBA - 4 components per vertex) (shader-location = 3)
;     unsigned short *indices; // Vertex indices (in case vertex data comes indexed)

;     // Skin data for animation
;     int boneCount;          // Number of bones (MAX: 256 bones)
;     unsigned char *boneIndices; // Vertex bone indices, up to 4 bones influence by vertex (skinning) (shader-location = 6)
;     float *boneWeights;     // Vertex bone weight, up to 4 bones influence by vertex (skinning) (shader-location = 7)

;     // Runtime animation vertex data (CPU skinning)
;     // NOTE: In case of GPU skinning, not used, pointers are NULL
;     float *animVertices;    // Animated vertex positions (after bones transformations)
;     float *animNormals;     // Animated normals (after bones transformations)

;     // OpenGL identifiers
;     unsigned int vaoId;     // OpenGL Vertex Array Object id
;     unsigned int *vboId;    // OpenGL Vertex Buffer Objects id (default vertex data)
; } Mesh;

(cffi:defcstruct (%Mesh :class mesh-type)
    (vertexCount :int)
    (triangleCount :int)
    (vertices :pointer)
    (texcoords :pointer)
    (texcoords2 :pointer)
    (normals :pointer)
    (tangents :pointer)
    (colors :pointer)
    (indices :pointer)
    (boneCount :int)
    (boneIndices :pointer)
    (boneWeights :pointer)
    (animVertices :pointer)
    (animNormals :pointer)
    (vaoId :unsigned-int)
    (vboId :pointer))

(defstruct mesh
    vertex-count
    triangle-count
    vertices
    texcoords
    texcoords2
    normals
    tangents
    colors
    indices
    bone-count
    bone-indices
    bone-weights
    anim-vertices
    anim-normals
    vao-id
    vbo-id)

; // Shader
; typedef struct Shader {
;     unsigned int id;        // Shader program id
;     int *locs;              // Shader locations array (RL_MAX_SHADER_LOCATIONS)
; } Shader;

(cffi:defcstruct (%Shader :class shader-type)
    (id :unsigned-int)
    (locs :pointer))

(defstruct shader
    id
    locs)

; // MaterialMap
; typedef struct MaterialMap {
;     Texture2D texture;      // Material map texture
;     Color color;            // Material map color
;     float value;            // Material map value
; } MaterialMap;

(cffi:defcstruct (%MaterialMap :class material-map-type)
    (texture (:struct %Texture))
    (color (:struct %Color))
    (value :float))

(defstruct material-map
    texture
    color
    value)

; // Material, includes shader and maps
; typedef struct Material {
;     Shader shader;          // Material shader
;     MaterialMap *maps;      // Material maps array (MAX_MATERIAL_MAPS)
;     float params[4];        // Material generic parameters (if required)
; } Material;

(cffi:defcstruct (%Material :class material-type)
    (shader (:struct %Shader))
    (maps :pointer)
    (params (:array :float 4)))

(defstruct material
    shader
    maps
    params)

; // Transform, vertex transformation data
; typedef struct Transform {
;     Vector3 translation;    // Translation
;     Quaternion rotation;    // Rotation
;     Vector3 scale;          // Scale
; } Transform;

(cffi:defcstruct (%Transform :class transform-type)
    (translation (:struct %Vector3))
    (rotation (:struct %Vector4))
    (scale (:struct %Vector3)))

(defstruct transform
    translation
    rotation
    scale)

; // Anim pose, an array of Transform[]
; typedef Transform *ModelAnimPose;
(cffi:defctype %ModelAnimPose (:pointer (:struct %Transform)))

; // Bone, skeletal animation bone
; typedef struct BoneInfo {
;     char name[32];          // Bone name
;     int parent;             // Bone parent
; } BoneInfo;

(cffi:defcstruct (%BoneInfo :class bone-info-type)
    (name (:array :char 32))
    (parent :int))

(defstruct bone-info
    name
    parent)

; // Skeleton, animation bones hierarchy
; typedef struct ModelSkeleton {
;     unsigned int boneCount; // Number of bones
;     BoneInfo *bones;        // Bones information (skeleton)
;     ModelAnimPose bindPose; // Bones base transformation (Transform[])
; } ModelSkeleton;

(cffi:defcstruct (%ModelSkeleton :class model-skeleton-type)
    (boneCount :unsigned-int)
    (bones :pointer)
    (bindPose :pointer))

(defstruct model-skeleton
    bone-count
    bones
    bind-pose)

; // Model, meshes, materials and animation data
; typedef struct Model {
;     Matrix transform;       // Local transform matrix

;     int meshCount;          // Number of meshes
;     int materialCount;      // Number of materials
;     Mesh *meshes;           // Meshes array
;     Material *materials;    // Materials array
;     int *meshMaterial;      // Mesh material number

;     // Animation data
;     ModelSkeleton skeleton; // Skeleton for animation

;     // Runtime animation data (CPU/GPU skinning)
;     ModelAnimPose currentPose; // Current animation pose (Transform[])
;     Matrix *boneMatrices;   // Bones animated transformation matrices
; } Model;

(cffi:defcstruct (%Model :class model-type)
    (transform (:struct %Matrix))
    (meshCount :int)
    (materialCount :int)
    (meshes :pointer)
    (materials :pointer)
    (meshMaterial :pointer)
    (skeleton (:struct %ModelSkeleton))
    (currentPose :pointer)
    (boneMatrices :pointer))

(defstruct model
    transform
    mesh-count
    material-count
    meshes
    materials
    mesh-material
    skeleton
    current-pose
    bone-matrices)

; // ModelAnimation, contains a full animation sequence
; typedef struct ModelAnimation {
;     char name[32];          // Animation name

;     unsigned int boneCount; // Number of bones (per pose)
;     int keyframeCount;      // Number of animation key frames
;     ModelAnimPose *keyframePoses; // Animation sequence keyframe poses [keyframe][pose]
; } ModelAnimation;

(cffi:defcstruct (%ModelAnimation :class model-animation-type)
    (name (:array :char 32))
    (boneCount :unsigned-int)
    (keyframeCount :int)
    (keyframePoses :pointer))

(defstruct model-animation
    name
    bone-count
    keyframe-count
    keyframe-poses)

; // Ray, ray for raycasting
; typedef struct Ray {
;     Vector3 position;       // Ray position (origin)
;     Vector3 direction;      // Ray direction (normalized)
; } Ray;

(cffi:defcstruct (%Ray :class ray-type)
    (position (:struct %Vector3))
    (direction (:struct %Vector3)))

(defstruct ray
    position
    direction)

; // RayCollision, ray hit information
; typedef struct RayCollision {
;     bool hit;               // Did the ray hit something?
;     float distance;         // Distance to the nearest hit
;     Vector3 point;          // Point of the nearest hit
;     Vector3 normal;         // Surface normal of hit
; } RayCollision;

(cffi:defcstruct (%RayCollision :class ray-collision-type)
    (hit :bool)
    (distance :float)
    (point (:struct %Vector3))
    (normal (:struct %Vector3)))

(defstruct ray-collision
    hit
    distance
    point
    normal)

; // BoundingBox
; typedef struct BoundingBox {
;     Vector3 min;            // Minimum vertex box-corner
;     Vector3 max;            // Maximum vertex box-corner
; } BoundingBox;

(cffi:defcstruct (%BoundingBox :class bounding-box-type)
    (min (:struct %Vector3))
    (max (:struct %Vector3)))

(defstruct bounding-box
    min
    max)

; // Wave, audio wave data
; typedef struct Wave {
;     unsigned int frameCount;    // Total number of frames (considering channels)
;     unsigned int sampleRate;    // Frequency (samples per second)
;     unsigned int sampleSize;    // Bit depth (bits per sample): 8, 16, 32 (24 not supported)
;     unsigned int channels;      // Number of channels (1-mono, 2-stereo, ...)
;     void *data;                 // Buffer data pointer
; } Wave;

(cffi:defcstruct (%Wave :class wave-type)
    (frameCount :unsigned-int)
    (sampleRate :unsigned-int)
    (sampleSize :unsigned-int)
    (channels :unsigned-int)
    (data :pointer))

(defstruct wave
    frame-count
    sample-rate
    sample-size
    channels
    data)

; // Opaque structs declaration
; // NOTE: Actual structs are defined internally in raudio module
; typedef struct rAudioBuffer rAudioBuffer;
; typedef struct rAudioProcessor rAudioProcessor;

; // AudioStream, custom audio stream
; typedef struct AudioStream {
;     rAudioBuffer *buffer;       // Pointer to internal data used by the audio system
;     rAudioProcessor *processor; // Pointer to internal data processor, useful for audio effects

;     unsigned int sampleRate;    // Frequency (samples per second)
;     unsigned int sampleSize;    // Bit depth (bits per sample): 8, 16, 32 (24 not supported)
;     unsigned int channels;      // Number of channels (1-mono, 2-stereo, ...)
; } AudioStream;

(cffi:defcstruct (%AudioStream :class audio-stream-type)
    (buffer :pointer)
    (processor :pointer)
    (sampleRate :unsigned-int)
    (sampleSize :unsigned-int)
    (channels :unsigned-int))

(defstruct audio-stream
    buffer
    processor
    sample-rate
    sample-size
    channels)

; // Sound
; typedef struct Sound {
;     AudioStream stream;         // Audio stream
;     unsigned int frameCount;    // Total number of frames (considering channels)
; } Sound;

(cffi:defcstruct (%Sound :class sound-type)
    (stream (:struct %AudioStream))
    (frameCount :unsigned-int))

(defstruct sound
    stream
    frame-count)

; // Music, audio stream, anything longer than ~10 seconds should be streamed
; typedef struct Music {
;     AudioStream stream;         // Audio stream
;     unsigned int frameCount;    // Total number of frames (considering channels)
;     bool looping;               // Music looping enable

;     int ctxType;                // Type of music context (audio filetype)
;     void *ctxData;              // Audio context data, depends on type
; } Music;

(cffi:defcstruct (%Music :class music-type)
    (stream (:struct %AudioStream))
    (frameCount :unsigned-int)
    (looping :bool)
    (ctxType :int)
    (ctxData :pointer))

(defstruct music
    stream
    frame-count
    looping
    ctx-type
    ctx-data)

; // VrDeviceInfo, Head-Mounted-Display device parameters
; typedef struct VrDeviceInfo {
;     int hResolution;                // Horizontal resolution in pixels
;     int vResolution;                // Vertical resolution in pixels
;     float hScreenSize;              // Horizontal size in meters
;     float vScreenSize;              // Vertical size in meters
;     float eyeToScreenDistance;      // Distance between eye and display in meters
;     float lensSeparationDistance;   // Lens separation distance in meters
;     float interpupillaryDistance;   // IPD (distance between pupils) in meters
;     float lensDistortionValues[4];  // Lens distortion constant parameters
;     float chromaAbCorrection[4];    // Chromatic aberration correction parameters
; } VrDeviceInfo;

(cffi:defcstruct (%VrDeviceInfo :class vr-device-info-type)
    (hResolution :int)
    (vResolution :int)
    (hScreenSize :float)
    (vScreenSize :float)
    (eyeToScreenDistance :float)
    (lensSeparationDistance :float)
    (interpupillaryDistance :float)
    (lensDistortionValues (:array :float 4))
    (chromaAbCorrection (:array :float 4)))

(defstruct vr-device-info
    h-resolution
    v-resolution
    h-screen-size
    v-screen-size
    eye-to-screen-distance
    lens-separation-distance
    interpupillary-distance
    lens-distortion-values
    chroma-ab-correction)

; // VrStereoConfig, VR stereo rendering configuration for simulator
; typedef struct VrStereoConfig {
;     Matrix projection[2];           // VR projection matrices (per eye)
;     Matrix viewOffset[2];           // VR view offset matrices (per eye)
;     float leftLensCenter[2];        // VR left lens center
;     float rightLensCenter[2];       // VR right lens center
;     float leftScreenCenter[2];      // VR left screen center
;     float rightScreenCenter[2];     // VR right screen center
;     float scale[2];                 // VR distortion scale
;     float scaleIn[2];               // VR distortion scale in
; } VrStereoConfig;

(cffi:defcstruct (%VrStereoConfig :class vr-stereo-config-type)
    (projection (:array (:struct %Matrix) 2))
    (viewOffset (:array (:struct %Matrix) 2))
    (leftLensCenter (:array :float 2))
    (rightLensCenter (:array :float 2))
    (leftScreenCenter (:array :float 2))
    (rightScreenCenter (:array :float 2))
    (scale (:array :float 2))
    (scaleIn (:array :float 2)))

(defstruct vr-stereo-config
    projection
    view-offset
    left-lens-center
    right-lens-center
    left-screen-center
    right-screen-center
    scale
    scale-in)

; // File path list
; typedef struct FilePathList {
;     unsigned int count;             // Filepaths entries count
;     char **paths;                   // Filepaths entries
; } FilePathList;

(cffi:defcstruct (%FilePathList :class file-path-list-type)
    (count :unsigned-int)
    (paths :pointer))

(defstruct file-path-list
    count
    paths)

; // Automation event
; typedef struct AutomationEvent {
;     unsigned int frame;             // Event frame
;     unsigned int type;              // Event type (AutomationEventType)
;     int params[4];                  // Event parameters (if required)
; } AutomationEvent;

(cffi:defcstruct (%AutomationEvent :class automation-event-type)
    (frame :unsigned-int)
    (type :unsigned-int)
    (params (:array :int 4)))

(defstruct automation-event
    frame
    type
    params)

; // Automation event list
; typedef struct AutomationEventList {
;     unsigned int capacity;          // Events max entries (MAX_AUTOMATION_EVENTS)
;     unsigned int count;             // Events entries count
;     AutomationEvent *events;        // Events entries
; } AutomationEventList;

(cffi:defcstruct (%AutomationEventList :class automation-event-list-type)
    (capacity :unsigned-int)
    (count :unsigned-int)
    (events :pointer))

(defstruct automation-event-list
    capacity
    count
    events)

(defmacro with-members(members obj type &body body)
  (with-gensyms(obj-var)
    `(let ((,obj-var ,obj))
       (symbol-macrolet
           ,(loop for member in members
		  for binding = (if (atom member) member (second member))
		  for slot = (if (atom member) member (first member))
                  for accessor = (format nil "~a-~a" type slot)
                  collecting `(,binding (,(intern accessor (symbol-package type)) ,obj-var)))
         ,@body))))

;;;; FUNCTIONS
; // Window-related functions
; void InitWindow(int width, int height, const char *title);  // Initialize window and OpenGL context
(cffi:defcfun ("InitWindow" %init-window) :void
    (width :int)
    (height :int)
    (title :string))

; void CloseWindow(void);                                     // Close window and unload OpenGL context
(cffi:defcfun ("CloseWindow" %close-window) :void)

; bool WindowShouldClose(void);                               // Check if application should close (KEY_ESCAPE pressed or windows close icon clicked)
(cffi:defcfun ("WindowShouldClose" %window-should-close) :bool)

; bool IsWindowReady(void);                                   // Check if window has been initialized successfully
(cffi:defcfun ("IsWindowReady" %is-window-ready) :bool)

; bool IsWindowFullscreen(void);                              // Check if window is currently fullscreen
(cffi:defcfun ("IsWindowFullscreen" %is-window-fullscreen) :bool)

; bool IsWindowHidden(void);                                  // Check if window is currently hidden
(cffi:defcfun ("IsWindowHidden" %is-window-hidden) :bool)

; bool IsWindowMinimized(void);                               // Check if window is currently minimized
(cffi:defcfun ("IsWindowMinimized" %is-window-minimized) :bool)

; bool IsWindowMaximized(void);                               // Check if window is currently maximized
(cffi:defcfun ("IsWindowMaximized" %is-window-maximized) :bool)

; bool IsWindowFocused(void);                                 // Check if window is currently focused
(cffi:defcfun ("IsWindowFocused" %is-window-focused) :bool)

; bool IsWindowResized(void);                                 // Check if window has been resized last frame
(cffi:defcfun ("IsWindowResized" %is-window-resized) :bool)

; bool IsWindowState(unsigned int flag);                      // Check if one specific window flag is enabled
(cffi:defcfun ("IsWindowState" %is-window-state) :bool
    (flag :unsigned-int)
)

; void SetWindowState(unsigned int flags);                    // Set window configuration state using flags
(cffi:defcfun ("SetWindowState" %set-window-state) :void
    (flags :unsigned-int)
)

; void ClearWindowState(unsigned int flags);                  // Clear window configuration state flags
(cffi:defcfun ("ClearWindowState" %clear-window-state) :void
    (flags :unsigned-int)
)

; void ToggleFullscreen(void);                                // Toggle window state: fullscreen/windowed, resizes monitor to match window resolution
(cffi:defcfun ("ToggleFullscreen" %toggle-fullscreen) :void
)

; void ToggleBorderlessWindowed(void);                        // Toggle window state: borderless windowed, resizes window to match monitor resolution
(cffi:defcfun ("ToggleBorderlessWindowed" %toggle-borderless-windowed) :void
)

; void MaximizeWindow(void);                                  // Set window state: maximized, if resizable
(cffi:defcfun ("MaximizeWindow" %maximize-window) :void
)

; void MinimizeWindow(void);                                  // Set window state: minimized, if resizable
(cffi:defcfun ("MinimizeWindow" %minimize-window) :void
)

; void RestoreWindow(void);                                   // Restore window from being minimized/maximized
(cffi:defcfun ("RestoreWindow" %restore-window) :void
)

; void SetWindowIcon(Image image);                            // Set icon for window (single image, RGBA 32bit)
(cffi:defcfun ("SetWindowIcon" %set-window-icon) :void
    (image (:struct %Image))
)

; void SetWindowIcons(Image *images, int count);              // Set icon for window (multiple images, RGBA 32bit)
(cffi:defcfun ("SetWindowIcons" %set-window-icons) :void
    (images :pointer)
    (count :int)
)

; void SetWindowTitle(const char *title);                     // Set title for window
(cffi:defcfun ("SetWindowTitle" %set-window-title) :void
    (title :string)
)

; void SetWindowPosition(int x, int y);                       // Set window position on screen
(cffi:defcfun ("SetWindowPosition" %set-window-position) :void
    (x :int)
    (y :int)
)

; void SetWindowMonitor(int monitor);                         // Set monitor for the current window
(cffi:defcfun ("SetWindowMonitor" %set-window-monitor) :void
    (monitor :int)
)

; void SetWindowMinSize(int width, int height);               // Set window minimum dimensions (for FLAG_WINDOW_RESIZABLE)
(cffi:defcfun ("SetWindowMinSize" %set-window-min-size) :void
    (width :int)
    (height :int)
)

; void SetWindowMaxSize(int width, int height);               // Set window maximum dimensions (for FLAG_WINDOW_RESIZABLE)
(cffi:defcfun ("SetWindowMaxSize" %set-window-max-size) :void
    (width :int)
    (height :int)
)

; void SetWindowSize(int width, int height);                  // Set window dimensions
(cffi:defcfun ("SetWindowSize" %set-window-size) :void
    (width :int)
    (height :int)
)

; void SetWindowOpacity(float opacity);                       // Set window opacity [0.0f..1.0f]
(cffi:defcfun ("SetWindowOpacity" %set-window-opacity) :void
    (opacity :float)
)

; void SetWindowFocused(void);                                // Set window focused
(cffi:defcfun ("SetWindowFocused" %set-window-focused) :void
)

; void *GetWindowHandle(void);                                // Get native window handle
(cffi:defcfun ("GetWindowHandle" %get-window-handle) :pointer
)

; int GetScreenWidth(void);                                   // Get current screen width
(cffi:defcfun ("GetScreenWidth" %get-screen-width) :int
)

; int GetScreenHeight(void);                                  // Get current screen height
(cffi:defcfun ("GetScreenHeight" %get-screen-height) :int
)

; int GetRenderWidth(void);                                   // Get current render width (it considers HiDPI)
(cffi:defcfun ("GetRenderWidth" %get-render-width) :int
)

; int GetRenderHeight(void);                                  // Get current render height (it considers HiDPI)
(cffi:defcfun ("GetRenderHeight" %get-render-height) :int
)

; int GetMonitorCount(void);                                  // Get number of connected monitors
(cffi:defcfun ("GetMonitorCount" %get-monitor-count) :int
)

; int GetCurrentMonitor(void);                                // Get current monitor where window is placed
(cffi:defcfun ("GetCurrentMonitor" %get-current-monitor) :int
)

; Vector2 GetMonitorPosition(int monitor);                    // Get specified monitor position
(cffi:defcfun ("GetMonitorPosition" %get-monitor-position) (:struct %Vector2)
    (monitor :int)
)

; int GetMonitorWidth(int monitor);                           // Get specified monitor width (current video mode used by monitor)
(cffi:defcfun ("GetMonitorWidth" %get-monitor-width) :int
    (monitor :int)
)

; int GetMonitorHeight(int monitor);                          // Get specified monitor height (current video mode used by monitor)
(cffi:defcfun ("GetMonitorHeight" %get-monitor-height) :int
    (monitor :int)
)

; int GetMonitorPhysicalWidth(int monitor);                   // Get specified monitor physical width in millimetres
(cffi:defcfun ("GetMonitorPhysicalWidth" %get-monitor-physical-width) :int
    (monitor :int)
)

; int GetMonitorPhysicalHeight(int monitor);                  // Get specified monitor physical height in millimetres
(cffi:defcfun ("GetMonitorPhysicalHeight" %get-monitor-physical-height) :int
    (monitor :int)
)

; int GetMonitorRefreshRate(int monitor);                     // Get specified monitor refresh rate
(cffi:defcfun ("GetMonitorRefreshRate" %get-monitor-refresh-rate) :int
    (monitor :int)
)

; Vector2 GetWindowPosition(void);                            // Get window position XY on monitor
(cffi:defcfun ("GetWindowPosition" %get-window-position) (:struct %Vector2)
)

; Vector2 GetWindowScaleDPI(void);                            // Get window scale DPI factor
(cffi:defcfun ("GetWindowScaleDPI" %get-window-scale-dpi) (:struct %Vector2)
)

; const char *GetMonitorName(int monitor);                    // Get the human-readable, UTF-8 encoded name of the specified monitor
(cffi:defcfun ("GetMonitorName" %get-monitor-name) :string
    (monitor :int)
)

; void SetClipboardText(const char *text);                    // Set clipboard text content
(cffi:defcfun ("SetClipboardText" %set-clipboard-text) :void
    (text :string)
)

; const char *GetClipboardText(void);                         // Get clipboard text content
(cffi:defcfun ("GetClipboardText" %get-clipboard-text) :string
)

; Image GetClipboardImage(void);                              // Get clipboard image content
(cffi:defcfun ("GetClipboardImage" %get-clipboard-image) (:struct %Image)
)

; void EnableEventWaiting(void);                              // Enable waiting for events on EndDrawing(), no automatic event polling
(cffi:defcfun ("EnableEventWaiting" %enable-event-waiting) :void
)

; void DisableEventWaiting(void);                             // Disable waiting for events on EndDrawing(), automatic events polling
(cffi:defcfun ("DisableEventWaiting" %disable-event-waiting) :void
)


; // Cursor-related functions
; void ShowCursor(void);                                      // Shows cursor
(cffi:defcfun ("ShowCursor" %show-cursor) :void
)

; void HideCursor(void);                                      // Hides cursor
(cffi:defcfun ("HideCursor" %hide-cursor) :void
)

; bool IsCursorHidden(void);                                  // Check if cursor is not visible
(cffi:defcfun ("IsCursorHidden" %is-cursor-hidden) :bool
)

; void EnableCursor(void);                                    // Enables cursor (unlock cursor)
(cffi:defcfun ("EnableCursor" %enable-cursor) :void
)

; void DisableCursor(void);                                   // Disables cursor (lock cursor)
(cffi:defcfun ("DisableCursor" %disable-cursor) :void
)

; bool IsCursorOnScreen(void);                                // Check if cursor is on the screen
(cffi:defcfun ("IsCursorOnScreen" %is-cursor-on-screen) :bool
)


; // Drawing-related functions
; void ClearBackground(Color color);                          // Set background color (framebuffer clear color)
(cffi:defcfun ("ClearBackground" %clear-background) :void
    (color (:struct %Color))
)

; void BeginDrawing(void);                                    // Setup canvas (framebuffer) to start drawing
(cffi:defcfun ("BeginDrawing" %begin-drawing) :void
)

; void EndDrawing(void);                                      // End canvas drawing and swap buffers (double buffering)
(cffi:defcfun ("EndDrawing" %end-drawing) :void
)

; void BeginMode2D(Camera2D camera);                          // Begin 2D mode with custom camera (2D)
(cffi:defcfun ("BeginMode2D" %begin-mode-2d) :void
    (camera (:struct %Camera2D))
)

; void EndMode2D(void);                                       // Ends 2D mode with custom camera
(cffi:defcfun ("EndMode2D" %end-mode-2d) :void
)

; void BeginMode3D(Camera3D camera);                          // Begin 3D mode with custom camera (3D)
(cffi:defcfun ("BeginMode3D" %begin-mode-3d) :void
    (camera (:struct %Camera3D))
)

; void EndMode3D(void);                                       // Ends 3D mode and returns to default 2D orthographic mode
(cffi:defcfun ("EndMode3D" %end-mode-3d) :void
)

; void BeginTextureMode(RenderTexture2D target);              // Begin drawing to render texture
(cffi:defcfun ("BeginTextureMode" %begin-texture-mode) :void
    (target (:struct %RenderTexture))
)

; void EndTextureMode(void);                                  // Ends drawing to render texture
(cffi:defcfun ("EndTextureMode" %end-texture-mode) :void
)

; void BeginShaderMode(Shader shader);                        // Begin custom shader drawing
(cffi:defcfun ("BeginShaderMode" %begin-shader-mode) :void
    (shader (:struct %Shader))
)

; void EndShaderMode(void);                                   // End custom shader drawing (use default shader)
(cffi:defcfun ("EndShaderMode" %end-shader-mode) :void
)

; void BeginBlendMode(int mode);                              // Begin blending mode (alpha, additive, multiplied, subtract, custom)
(cffi:defcfun ("BeginBlendMode" %begin-blend-mode) :void
    (mode :int)
)

; void EndBlendMode(void);                                    // End blending mode (reset to default: alpha blending)
(cffi:defcfun ("EndBlendMode" %end-blend-mode) :void
)

; void BeginScissorMode(int x, int y, int width, int height); // Begin scissor mode (define screen area for following drawing)
(cffi:defcfun ("BeginScissorMode" %begin-scissor-mode) :void
    (x :int)
    (y :int)
    (width :int)
    (height :int)
)

; void EndScissorMode(void);                                  // End scissor mode
(cffi:defcfun ("EndScissorMode" %end-scissor-mode) :void
)

; void BeginVrStereoMode(VrStereoConfig config);              // Begin stereo rendering (requires VR simulator)
(cffi:defcfun ("BeginVrStereoMode" %begin-vr-stereo-mode) :void
    (config (:struct %VrStereoConfig))
)

; void EndVrStereoMode(void);                                 // End stereo rendering (requires VR simulator)
(cffi:defcfun ("EndVrStereoMode" %end-vr-stereo-mode) :void
)


; // VR stereo config functions for VR simulator
; VrStereoConfig LoadVrStereoConfig(VrDeviceInfo device);     // Load VR stereo config for VR simulator device parameters
(cffi:defcfun ("LoadVrStereoConfig" %load-vr-stereo-config) (:struct %VrStereoConfig)
    (device (:struct %VrDeviceInfo))
)

; void UnloadVrStereoConfig(VrStereoConfig config);           // Unload VR stereo config
(cffi:defcfun ("UnloadVrStereoConfig" %unload-vr-stereo-config) :void
    (config (:struct %VrStereoConfig))
)


; // Shader management functions
; // NOTE: Shader functionality is not available on OpenGL 1.1
; Shader LoadShader(const char *vsFileName, const char *fsFileName);   // Load shader from files and bind default locations
(cffi:defcfun ("LoadShader" %load-shader) (:struct %Shader)
    (vsFileName :string)
    (fsFileName :string)
)

; Shader LoadShaderFromMemory(const char *vsCode, const char *fsCode); // Load shader from code strings and bind default locations
(cffi:defcfun ("LoadShaderFromMemory" %load-shader-from-memory) (:struct %Shader)
    (vsCode :string)
    (fsCode :string)
)

; bool IsShaderValid(Shader shader);                                   // Check if a shader is valid (loaded on GPU)
(cffi:defcfun ("IsShaderValid" %is-shader-valid) :bool
    (shader (:struct %Shader))
)

; int GetShaderLocation(Shader shader, const char *uniformName);       // Get shader uniform location
(cffi:defcfun ("GetShaderLocation" %get-shader-location) :int
    (shader (:struct %Shader))
    (uniformName :string)
)

; int GetShaderLocationAttrib(Shader shader, const char *attribName);  // Get shader attribute location
(cffi:defcfun ("GetShaderLocationAttrib" %get-shader-location-attrib) :int
    (shader (:struct %Shader))
    (attribName :string)
)

; void SetShaderValue(Shader shader, int locIndex, const void *value, int uniformType); // Set shader uniform value
(cffi:defcfun ("SetShaderValue" %set-shader-value) :void
    (shader (:struct %Shader))
    (locIndex :int)
    (value :pointer)
    (uniformType :int)
)

; void SetShaderValueV(Shader shader, int locIndex, const void *value, int uniformType, int count); // Set shader uniform value vector
(cffi:defcfun ("SetShaderValueV" %set-shader-value-v) :void
    (shader (:struct %Shader))
    (locIndex :int)
    (value :pointer)
    (uniformType :int)
    (count :int)
)

; void SetShaderValueMatrix(Shader shader, int locIndex, Matrix mat);  // Set shader uniform value (matrix 4x4)
(cffi:defcfun ("SetShaderValueMatrix" %set-shader-value-matrix) :void
    (shader (:struct %Shader))
    (locIndex :int)
    (mat (:struct %Matrix))
)

; void SetShaderValueTexture(Shader shader, int locIndex, Texture2D texture); // Set shader uniform value and bind the texture (sampler2d)
(cffi:defcfun ("SetShaderValueTexture" %set-shader-value-texture) :void
    (shader (:struct %Shader))
    (locIndex :int)
    (texture (:struct %Texture))
)

; void UnloadShader(Shader shader);                                    // Unload shader from GPU memory (VRAM)
(cffi:defcfun ("UnloadShader" %unload-shader) :void
    (shader (:struct %Shader))
)


; // Screen-space-related functions
; Ray GetScreenToWorldRay(Vector2 position, Camera camera);         // Get a ray trace from screen position (i.e mouse)
(cffi:defcfun ("GetScreenToWorldRay" %get-screen-to-world-ray) (:struct %Ray)
    (position (:struct %Vector2))
    (camera (:struct %Camera3D))
)

; Ray GetScreenToWorldRayEx(Vector2 position, Camera camera, int width, int height); // Get a ray trace from screen position (i.e mouse) in a viewport
(cffi:defcfun ("GetScreenToWorldRayEx" %get-screen-to-world-ray-ex) (:struct %Ray)
    (position (:struct %Vector2))
    (camera (:struct %Camera3D))
    (width :int)
    (height :int)
)

; Vector2 GetWorldToScreen(Vector3 position, Camera camera);        // Get the screen space position for a 3d world space position
(cffi:defcfun ("GetWorldToScreen" %get-world-to-screen) (:struct %Vector2)
    (position (:struct %Vector3))
    (camera (:struct %Camera3D))
)

; Vector2 GetWorldToScreenEx(Vector3 position, Camera camera, int width, int height); // Get size position for a 3d world space position
(cffi:defcfun ("GetWorldToScreenEx" %get-world-to-screen-ex) (:struct %Vector2)
    (position (:struct %Vector3))
    (camera (:struct %Camera3D))
    (width :int)
    (height :int)
)

; Vector2 GetWorldToScreen2D(Vector2 position, Camera2D camera);    // Get the screen space position for a 2d camera world space position
(cffi:defcfun ("GetWorldToScreen2D" %get-world-to-screen-2d) (:struct %Vector2)
    (position (:struct %Vector2))
    (camera (:struct %Camera2D))
)

; Vector2 GetScreenToWorld2D(Vector2 position, Camera2D camera);    // Get the world space position for a 2d camera screen space position
(cffi:defcfun ("GetScreenToWorld2D" %get-screen-to-world-2d) (:struct %Vector2)
    (position (:struct %Vector2))
    (camera (:struct %Camera2D))
)

; Matrix GetCameraMatrix(Camera camera);                            // Get camera transform matrix (view matrix)
(cffi:defcfun ("GetCameraMatrix" %get-camera-matrix) (:struct %Matrix)
    (camera (:struct %Camera3D))
)

; Matrix GetCameraMatrix2D(Camera2D camera);                        // Get camera 2d transform matrix
(cffi:defcfun ("GetCameraMatrix2D" %get-camera-matrix-2d) (:struct %Matrix)
    (camera (:struct %Camera2D))
)


; // Timing-related functions
; void SetTargetFPS(int fps);                       // Set target FPS (maximum)
(cffi:defcfun ("SetTargetFPS" %set-target-fps) :void
    (fps :int)
)

; float GetFrameTime(void);                         // Get time in seconds for last frame drawn (delta time)
(cffi:defcfun ("GetFrameTime" %get-frame-time) :float
)

; double GetTime(void);                             // Get elapsed time in seconds since InitWindow()
(cffi:defcfun ("GetTime" %get-time) :double
)

; int GetFPS(void);                                 // Get current FPS
(cffi:defcfun ("GetFPS" %get-fps) :int
)


; // Custom frame control functions
; // NOTE: Those functions are intended for advanced users that want full control over the frame processing
; // By default EndDrawing() does this job: draws everything + SwapScreenBuffer() + manage frame timing + PollInputEvents()
; // To avoid that behaviour and control frame processes manually, enable in config.h: SUPPORT_CUSTOM_FRAME_CONTROL
; void SwapScreenBuffer(void);                      // Swap back buffer with front buffer (screen drawing)
(cffi:defcfun ("SwapScreenBuffer" %swap-screen-buffer) :void
)

; void PollInputEvents(void);                       // Register all input events
(cffi:defcfun ("PollInputEvents" %poll-input-events) :void
)

; void WaitTime(double seconds);                    // Wait for some time (halt program execution)
(cffi:defcfun ("WaitTime" %wait-time) :void
    (seconds :double)
)


; // Random values generation functions
; void SetRandomSeed(unsigned int seed);            // Set the seed for the random number generator
(cffi:defcfun ("SetRandomSeed" %set-random-seed) :void
    (seed :unsigned-int)
)

; int GetRandomValue(int min, int max);             // Get a random value between min and max (both included)
(cffi:defcfun ("GetRandomValue" %get-random-value) :int
    (min :int)
    (max :int)
)

; int *LoadRandomSequence(unsigned int count, int min, int max); // Load random values sequence, no values repeated
(cffi:defcfun ("LoadRandomSequence" %load-random-sequence) :pointer
    (count :unsigned-int)
    (min :int)
    (max :int)
)

; void UnloadRandomSequence(int *sequence);         // Unload random values sequence
(cffi:defcfun ("UnloadRandomSequence" %unload-random-sequence) :void
    (sequence :pointer)
)


; // Misc. functions
; void TakeScreenshot(const char *fileName);                // Takes a screenshot of current screen (filename extension defines format)
(cffi:defcfun ("TakeScreenshot" %take-screenshot) :void
    (fileName :string)
)

; void SetConfigFlags(unsigned int flags);                  // Setup init configuration flags (view FLAGS)
(cffi:defcfun ("SetConfigFlags" %set-config-flags) :void
    (flags :unsigned-int)
)

; void OpenURL(const char *url);                            // Open URL with default system browser (if available)
(cffi:defcfun ("OpenURL" %open-url) :void
    (url :string)
)


; // Logging system
; void SetTraceLogLevel(int logLevel);                      // Set the current threshold (minimum) log level
(cffi:defcfun ("SetTraceLogLevel" %set-trace-log-level) :void
    (logLevel :int)
)

; void TraceLog(int logLevel, const char *text, ...);       // Show trace log messages (LOG_DEBUG, LOG_INFO, LOG_WARNING, LOG_ERROR...)
(cffi:defcfun ("TraceLog" %trace-log) :void
    (logLevel :int)
    (text :string)
    &rest)

; void SetTraceLogCallback(TraceLogCallback callback);      // Set custom trace log
(cffi:defcfun ("SetTraceLogCallback" %set-trace-log-callback) :void
    (callback :pointer)
)


; // Memory management, using internal allocators
; void *MemAlloc(unsigned int size);                        // Internal memory allocator
(cffi:defcfun ("MemAlloc" %mem-alloc) :pointer
    (size :unsigned-int)
)

; void *MemRealloc(void *ptr, unsigned int size);           // Internal memory reallocator
(cffi:defcfun ("MemRealloc" %mem-realloc) :pointer
    (ptr :pointer)
    (size :unsigned-int)
)

; void MemFree(void *ptr);                                  // Internal memory free
(cffi:defcfun ("MemFree" %mem-free) :void
    (ptr :pointer)
)


; // File system management functions
; unsigned char *LoadFileData(const char *fileName, int *dataSize); // Load file data as byte array (read)
(cffi:defcfun ("LoadFileData" %load-file-data) :pointer
    (fileName :string)
    (dataSize :pointer)
)

; void UnloadFileData(unsigned char *data);                     // Unload file data allocated by LoadFileData()
(cffi:defcfun ("UnloadFileData" %unload-file-data) :void
    (data :pointer)
)

; bool SaveFileData(const char *fileName, void *data, int dataSize); // Save data to file from byte array (write), returns true on success
(cffi:defcfun ("SaveFileData" %save-file-data) :bool
    (fileName :string)
    (data :pointer)
    (dataSize :int)
)

; bool ExportDataAsCode(const unsigned char *data, int dataSize, const char *fileName); // Export data to code (.h), returns true on success
(cffi:defcfun ("ExportDataAsCode" %export-data-as-code) :bool
    (data :pointer)
    (dataSize :int)
    (fileName :string)
)

; char *LoadFileText(const char *fileName);                     // Load text data from file (read), returns a '\0' terminated string
(cffi:defcfun ("LoadFileText" %load-file-text) :string
    (fileName :string)
)

; void UnloadFileText(char *text);                              // Unload file text data allocated by LoadFileText()
(cffi:defcfun ("UnloadFileText" %unload-file-text) :void
    (text :string)
)

; bool SaveFileText(const char *fileName, const char *text);    // Save text data to file (write), string must be '\0' terminated, returns true on success
(cffi:defcfun ("SaveFileText" %save-file-text) :bool
    (fileName :string)
    (text :string)
)


; // File access custom callbacks
; // WARNING: Callbacks setup is intended for advanced users
; void SetLoadFileDataCallback(LoadFileDataCallback callback);  // Set custom file binary data loader
(cffi:defcfun ("SetLoadFileDataCallback" %set-load-file-data-callback) :void
    (callback :pointer)
)

; void SetSaveFileDataCallback(SaveFileDataCallback callback);  // Set custom file binary data saver
(cffi:defcfun ("SetSaveFileDataCallback" %set-save-file-data-callback) :void
    (callback :pointer)
)

; void SetLoadFileTextCallback(LoadFileTextCallback callback);  // Set custom file text data loader
(cffi:defcfun ("SetLoadFileTextCallback" %set-load-file-text-callback) :void
    (callback :pointer)
)

; void SetSaveFileTextCallback(SaveFileTextCallback callback);  // Set custom file text data saver
(cffi:defcfun ("SetSaveFileTextCallback" %set-save-file-text-callback) :void
    (callback :pointer)
)


; int FileRename(const char *fileName, const char *fileRename); // Rename file (if exists)
(cffi:defcfun ("FileRename" %file-rename) :int
    (fileName :string)
    (fileRename :string)
)

; int FileRemove(const char *fileName);                         // Remove file (if exists)
(cffi:defcfun ("FileRemove" %file-remove) :int
    (fileName :string)
)

; int FileCopy(const char *srcPath, const char *dstPath);       // Copy file from one path to another, dstPath created if it doesn't exist
(cffi:defcfun ("FileCopy" %file-copy) :int
    (srcPath :string)
    (dstPath :string)
)

; int FileMove(const char *srcPath, const char *dstPath);       // Move file from one directory to another, dstPath created if it doesn't exist
(cffi:defcfun ("FileMove" %file-move) :int
    (srcPath :string)
    (dstPath :string)
)

; int FileTextReplace(const char *fileName, const char *search, const char *replacement); // Replace text in an existing file
(cffi:defcfun ("FileTextReplace" %file-text-replace) :int
    (fileName :string)
    (search :string)
    (replacement :string)
)

; int FileTextFindIndex(const char *fileName, const char *search); // Find text in existing file
(cffi:defcfun ("FileTextFindIndex" %file-text-find-index) :int
    (fileName :string)
    (search :string)
)

; bool FileExists(const char *fileName);                        // Check if file exists
(cffi:defcfun ("FileExists" %%file-exists) :bool
    (fileName :string)
)

; bool DirectoryExists(const char *dirPath);                    // Check if a directory path exists
(cffi:defcfun ("DirectoryExists" %directory-exists) :bool
    (dirPath :string)
)

; bool IsFileExtension(const char *fileName, const char *ext);  // Check file extension (recommended include point: .png, .wav)
(cffi:defcfun ("IsFileExtension" %is-file-extension) :bool
    (fileName :string)
    (ext :string)
)

; int GetFileLength(const char *fileName);                      // Get file length in bytes (NOTE: GetFileSize() conflicts with windows.h)
(cffi:defcfun ("GetFileLength" %get-file-length) :int
    (fileName :string)
)

; long GetFileModTime(const char *fileName);                    // Get file modification time (last write time)
(cffi:defcfun ("GetFileModTime" %get-file-mod-time) :long
    (fileName :string)
)

; const char *GetFileExtension(const char *fileName);           // Get pointer to extension for a filename string (includes dot: '.png')
(cffi:defcfun ("GetFileExtension" %get-file-extension) :string
    (fileName :string)
)

; const char *GetFileName(const char *filePath);                // Get pointer to filename for a path string
(cffi:defcfun ("GetFileName" %get-file-name) :string
    (filePath :string)
)

; const char *GetFileNameWithoutExt(const char *filePath);      // Get filename string without extension (uses static string)
(cffi:defcfun ("GetFileNameWithoutExt" %get-file-name-without-ext) :string
    (filePath :string)
)

; const char *GetDirectoryPath(const char *filePath);           // Get full path for a given fileName with path (uses static string)
(cffi:defcfun ("GetDirectoryPath" %get-directory-path) :string
    (filePath :string)
)

; const char *GetPrevDirectoryPath(const char *dirPath);        // Get previous directory path for a given path (uses static string)
(cffi:defcfun ("GetPrevDirectoryPath" %get-prev-directory-path) :string
    (dirPath :string)
)

; const char *GetWorkingDirectory(void);                        // Get current working directory (uses static string)
(cffi:defcfun ("GetWorkingDirectory" %get-working-directory) :string
)

; const char *GetApplicationDirectory(void);                    // Get the directory of the running application (uses static string)
(cffi:defcfun ("GetApplicationDirectory" %get-application-directory) :string
)

; int MakeDirectory(const char *dirPath);                       // Create directories (including full path requested), returns 0 on success
(cffi:defcfun ("MakeDirectory" %make-directory) :int
    (dirPath :string)
)

; bool ChangeDirectory(const char *dirPath);                    // Change working directory, return true on success
(cffi:defcfun ("ChangeDirectory" %change-directory) :bool
    (dirPath :string)
)

; bool IsPathFile(const char *path);                            // Check if a given path is a file or a directory
(cffi:defcfun ("IsPathFile" %is-path-file) :bool
    (path :string)
)

; bool IsFileNameValid(const char *fileName);                   // Check if fileName is valid for the platform/OS
(cffi:defcfun ("IsFileNameValid" %is-file-name-valid) :bool
    (fileName :string)
)

; FilePathList LoadDirectoryFiles(const char *dirPath);         // Load directory filepaths, files and directories, no subdirs scan
(cffi:defcfun ("LoadDirectoryFiles" %load-directory-files) (:struct %FilePathList)
    (dirPath :string)
)

; FilePathList LoadDirectoryFilesEx(const char *basePath, const char *filter, bool scanSubdirs); // Load directory filepaths with extension filtering and subdir scan; some filters available: "*.*", "FILES*", "DIRS*"
(cffi:defcfun ("LoadDirectoryFilesEx" %load-directory-files-ex) (:struct %FilePathList)
    (basePath :string)
    (filter :string)
    (scanSubdirs :bool)
)

; void UnloadDirectoryFiles(FilePathList files);                // Unload filepaths
(cffi:defcfun ("UnloadDirectoryFiles" %unload-directory-files) :void
    (files (:struct %FilePathList))
)

; bool IsFileDropped(void);                                     // Check if a file has been dropped into window
(cffi:defcfun ("IsFileDropped" %is-file-dropped) :bool
)

; FilePathList LoadDroppedFiles(void);                          // Load dropped filepaths
(cffi:defcfun ("LoadDroppedFiles" %load-dropped-files) (:struct %FilePathList)
)

; void UnloadDroppedFiles(FilePathList files);                  // Unload dropped filepaths
(cffi:defcfun ("UnloadDroppedFiles" %unload-dropped-files) :void
    (files (:struct %FilePathList))
)

; unsigned int GetDirectoryFileCount(const char *dirPath);      // Get the file count in a directory
(cffi:defcfun ("GetDirectoryFileCount" %get-directory-file-count) :unsigned-int
    (dirPath :string)
)

; unsigned int GetDirectoryFileCountEx(const char *basePath, const char *filter, bool scanSubdirs); // Get the file count in a directory with extension filtering and recursive directory scan. Use 'DIR' in the filter string to include directories in the result
(cffi:defcfun ("GetDirectoryFileCountEx" %get-directory-file-count-ex) :unsigned-int
    (basePath :string)
    (filter :string)
    (scanSubdirs :bool)
)


; // Compression/Encoding functionality
; unsigned char *CompressData(const unsigned char *data, int dataSize, int *compDataSize);        // Compress data (DEFLATE algorithm), memory must be MemFree()
(cffi:defcfun ("CompressData" %compress-data) :pointer
    (data :pointer)
    (dataSize :int)
    (compDataSize :pointer)
)

; unsigned char *DecompressData(const unsigned char *compData, int compDataSize, int *dataSize);  // Decompress data (DEFLATE algorithm), memory must be MemFree()
(cffi:defcfun ("DecompressData" %decompress-data) :pointer
    (compData :pointer)
    (compDataSize :int)
    (dataSize :pointer)
)

; char *EncodeDataBase64(const unsigned char *data, int dataSize, int *outputSize);               // Encode data to Base64 string (includes NULL terminator), memory must be MemFree()
(cffi:defcfun ("EncodeDataBase64" %encode-data-base64) :string
    (data :pointer)
    (dataSize :int)
    (outputSize :pointer)
)

; unsigned char *DecodeDataBase64(const char *text, int *outputSize);                             // Decode Base64 string (expected NULL terminated), memory must be MemFree()
(cffi:defcfun ("DecodeDataBase64" %decode-data-base64) :pointer
    (text :string)
    (outputSize :pointer)
)

; unsigned int ComputeCRC32(unsigned char *data, int dataSize); // Compute CRC32 hash code
(cffi:defcfun ("ComputeCRC32" %compute-crc32) :unsigned-int
    (data :pointer)
    (dataSize :int)
)

; unsigned int *ComputeMD5(unsigned char *data, int dataSize);  // Compute MD5 hash code, returns static int[4] (16 bytes)
(cffi:defcfun ("ComputeMD5" %compute-md5) :pointer
    (data :pointer)
    (dataSize :int)
)

; unsigned int *ComputeSHA1(unsigned char *data, int dataSize); // Compute SHA1 hash code, returns static int[5] (20 bytes)
(cffi:defcfun ("ComputeSHA1" %compute-sha1) :pointer
    (data :pointer)
    (dataSize :int)
)

; unsigned int *ComputeSHA256(unsigned char *data, int dataSize); // Compute SHA256 hash code, returns static int[8] (32 bytes)
(cffi:defcfun ("ComputeSHA256" %compute-sha256) :pointer
    (data :pointer)
    (dataSize :int)
)


; // Automation events functionality
; AutomationEventList LoadAutomationEventList(const char *fileName); // Load automation events list from file, NULL for empty list, capacity = MAX_AUTOMATION_EVENTS
(cffi:defcfun ("LoadAutomationEventList" %load-automation-event-list) (:struct %AutomationEventList)
    (fileName :string)
)

; void UnloadAutomationEventList(AutomationEventList list);   // Unload automation events list from file
(cffi:defcfun ("UnloadAutomationEventList" %unload-automation-event-list) :void
    (list (:struct %AutomationEventList))
)

; bool ExportAutomationEventList(AutomationEventList list, const char *fileName); // Export automation events list as text file
(cffi:defcfun ("ExportAutomationEventList" %export-automation-event-list) :bool
    (list (:struct %AutomationEventList))
    (fileName :string)
)

; void SetAutomationEventList(AutomationEventList *list);     // Set automation event list to record to
(cffi:defcfun ("SetAutomationEventList" %set-automation-event-list) :void
    (list :pointer)
)

; void SetAutomationEventBaseFrame(int frame);                // Set automation event internal base frame to start recording
(cffi:defcfun ("SetAutomationEventBaseFrame" %set-automation-event-base-frame) :void
    (frame :int)
)

; void StartAutomationEventRecording(void);                   // Start recording automation events (AutomationEventList must be set)
(cffi:defcfun ("StartAutomationEventRecording" %start-automation-event-recording) :void
)

; void StopAutomationEventRecording(void);                    // Stop recording automation events
(cffi:defcfun ("StopAutomationEventRecording" %stop-automation-event-recording) :void
)

; void PlayAutomationEvent(AutomationEvent event);            // Play a recorded automation event
(cffi:defcfun ("PlayAutomationEvent" %play-automation-event) :void
    (event (:struct %AutomationEvent))
)


; //------------------------------------------------------------------------------------
; // Input Handling Functions (Module: core)
; //------------------------------------------------------------------------------------

; // Input-related functions: keyboard
; bool IsKeyPressed(int key);                             // Check if a key has been pressed once
(cffi:defcfun ("IsKeyPressed" %is-key-pressed) :bool
    (key keyboard-key)
)

; bool IsKeyPressedRepeat(int key);                       // Check if a key has been pressed again
(cffi:defcfun ("IsKeyPressedRepeat" %is-key-pressed-repeat) :bool
    (key keyboard-key)
)

; bool IsKeyDown(int key);                                // Check if a key is being pressed
(cffi:defcfun ("IsKeyDown" %is-key-down) :bool
    (key keyboard-key)
)

; bool IsKeyReleased(int key);                            // Check if a key has been released once
(cffi:defcfun ("IsKeyReleased" %is-key-released) :bool
    (key keyboard-key)
)

; bool IsKeyUp(int key);                                  // Check if a key is NOT being pressed
(cffi:defcfun ("IsKeyUp" %is-key-up) :bool
    (key keyboard-key)
)

; int GetKeyPressed(void);                                // Get key pressed (keycode), call it multiple times for keys queued, returns 0 when the queue is empty
(cffi:defcfun ("GetKeyPressed" %get-key-pressed) keyboard-key
)

; int GetCharPressed(void);                               // Get char pressed (unicode), call it multiple times for chars queued, returns 0 when the queue is empty
(cffi:defcfun ("GetCharPressed" %get-char-pressed) :int
)

; const char *GetKeyName(int key);                        // Get name of a QWERTY key on the current keyboard layout (eg returns string 'q' for KEY_A on an AZERTY keyboard)
(cffi:defcfun ("GetKeyName" %get-key-name) :string
    (key keyboard-key)
)

; void SetExitKey(int key);                               // Set a custom key to exit program (default is ESC)
(cffi:defcfun ("SetExitKey" %set-exit-key) :void
    (key keyboard-key)
)


; // Input-related functions: gamepads
; bool IsGamepadAvailable(int gamepad);                   // Check if a gamepad is available
(cffi:defcfun ("IsGamepadAvailable" %is-gamepad-available) :bool
    (gamepad :int)
)

; const char *GetGamepadName(int gamepad);                // Get gamepad internal name id
(cffi:defcfun ("GetGamepadName" %get-gamepad-name) :string
    (gamepad :int)
)

; bool IsGamepadButtonPressed(int gamepad, int button);   // Check if a gamepad button has been pressed once
(cffi:defcfun ("IsGamepadButtonPressed" %is-gamepad-button-pressed) :bool
    (gamepad :int)
    (button :int)
)

; bool IsGamepadButtonDown(int gamepad, int button);      // Check if a gamepad button is being pressed
(cffi:defcfun ("IsGamepadButtonDown" %is-gamepad-button-down) :bool
    (gamepad :int)
    (button :int)
)

; bool IsGamepadButtonReleased(int gamepad, int button);  // Check if a gamepad button has been released once
(cffi:defcfun ("IsGamepadButtonReleased" %is-gamepad-button-released) :bool
    (gamepad :int)
    (button :int)
)

; bool IsGamepadButtonUp(int gamepad, int button);        // Check if a gamepad button is NOT being pressed
(cffi:defcfun ("IsGamepadButtonUp" %is-gamepad-button-up) :bool
    (gamepad :int)
    (button :int)
)

; int GetGamepadButtonPressed(void);                      // Get the last gamepad button pressed
(cffi:defcfun ("GetGamepadButtonPressed" %get-gamepad-button-pressed) :int
)

; int GetGamepadAxisCount(int gamepad);                   // Get axis count for a gamepad
(cffi:defcfun ("GetGamepadAxisCount" %get-gamepad-axis-count) :int
    (gamepad :int)
)

; float GetGamepadAxisMovement(int gamepad, int axis);    // Get movement value for a gamepad axis
(cffi:defcfun ("GetGamepadAxisMovement" %get-gamepad-axis-movement) :float
    (gamepad :int)
    (axis :int)
)

; int SetGamepadMappings(const char *mappings);           // Set internal gamepad mappings (SDL_GameControllerDB)
(cffi:defcfun ("SetGamepadMappings" %set-gamepad-mappings) :int
    (mappings :string)
)

; void SetGamepadVibration(int gamepad, float leftMotor, float rightMotor, float duration); // Set gamepad vibration for both motors (duration in seconds)
(cffi:defcfun ("SetGamepadVibration" %set-gamepad-vibration) :void
    (gamepad :int)
    (leftMotor :float)
    (rightMotor :float)
    (duration :float)
)


; // Input-related functions: mouse
; bool IsMouseButtonPressed(int button);                  // Check if a mouse button has been pressed once
(cffi:defcfun ("IsMouseButtonPressed" %is-mouse-button-pressed) :bool
    (button mouse-key)
)

; bool IsMouseButtonDown(int button);                     // Check if a mouse button is being pressed
(cffi:defcfun ("IsMouseButtonDown" %is-mouse-button-down) :bool
    (button mouse-key)
)

; bool IsMouseButtonReleased(int button);                 // Check if a mouse button has been released once
(cffi:defcfun ("IsMouseButtonReleased" %is-mouse-button-released) :bool
    (button mouse-key)
)

; bool IsMouseButtonUp(int button);                       // Check if a mouse button is NOT being pressed
(cffi:defcfun ("IsMouseButtonUp" %is-mouse-button-up) :bool
    (button mouse-key)
)

; int GetMouseX(void);                                    // Get mouse position X
(cffi:defcfun ("GetMouseX" %get-mouse-x) :int
)

; int GetMouseY(void);                                    // Get mouse position Y
(cffi:defcfun ("GetMouseY" %get-mouse-y) :int
)

; Vector2 GetMousePosition(void);                         // Get mouse position XY
(cffi:defcfun ("GetMousePosition" %get-mouse-position) (:struct %Vector2)
)

; Vector2 GetMouseDelta(void);                            // Get mouse delta between frames
(cffi:defcfun ("GetMouseDelta" %get-mouse-delta) (:struct %Vector2)
)

; void SetMousePosition(int x, int y);                    // Set mouse position XY
(cffi:defcfun ("SetMousePosition" %set-mouse-position) :void
    (x :int)
    (y :int)
)

; void SetMouseOffset(int offsetX, int offsetY);          // Set mouse offset
(cffi:defcfun ("SetMouseOffset" %set-mouse-offset) :void
    (offsetX :int)
    (offsetY :int)
)

; void SetMouseScale(float scaleX, float scaleY);         // Set mouse scaling
(cffi:defcfun ("SetMouseScale" %set-mouse-scale) :void
    (scaleX :float)
    (scaleY :float)
)

; float GetMouseWheelMove(void);                          // Get mouse wheel movement for X or Y, whichever is larger
(cffi:defcfun ("GetMouseWheelMove" %get-mouse-wheel-move) :float
)

; Vector2 GetMouseWheelMoveV(void);                       // Get mouse wheel movement for both X and Y
(cffi:defcfun ("GetMouseWheelMoveV" %get-mouse-wheel-move-v) (:struct %Vector2)
)

; void SetMouseCursor(int cursor);                        // Set mouse cursor
(cffi:defcfun ("SetMouseCursor" %set-mouse-cursor) :void
    (cursor :int)
)


; // Input-related functions: touch
; int GetTouchX(void);                                    // Get touch position X for touch point 0 (relative to screen size)
(cffi:defcfun ("GetTouchX" %get-touch-x) :int
)

; int GetTouchY(void);                                    // Get touch position Y for touch point 0 (relative to screen size)
(cffi:defcfun ("GetTouchY" %get-touch-y) :int
)

; Vector2 GetTouchPosition(int index);                    // Get touch position XY for a touch point index (relative to screen size)
(cffi:defcfun ("GetTouchPosition" %get-touch-position) (:struct %Vector2)
    (index :int)
)

; int GetTouchPointId(int index);                         // Get touch point identifier for given index
(cffi:defcfun ("GetTouchPointId" %get-touch-point-id) :int
    (index :int)
)

; int GetTouchPointCount(void);                           // Get number of touch points
(cffi:defcfun ("GetTouchPointCount" %get-touch-point-count) :int
)


; //------------------------------------------------------------------------------------
; // Gestures and Touch Handling Functions (Module: rgestures)
; //------------------------------------------------------------------------------------
; void SetGesturesEnabled(unsigned int flags);            // Enable a set of gestures using flags
(cffi:defcfun ("SetGesturesEnabled" %set-gestures-enabled) :void
    (flags :unsigned-int)
)

; bool IsGestureDetected(unsigned int gesture);           // Check if a gesture have been detected
(cffi:defcfun ("IsGestureDetected" %is-gesture-detected) :bool
    (gesture :unsigned-int)
)

; int GetGestureDetected(void);                           // Get latest detected gesture
(cffi:defcfun ("GetGestureDetected" %get-gesture-detected) :int
)

; float GetGestureHoldDuration(void);                     // Get gesture hold time in seconds
(cffi:defcfun ("GetGestureHoldDuration" %get-gesture-hold-duration) :float
)

; Vector2 GetGestureDragVector(void);                     // Get gesture drag vector
(cffi:defcfun ("GetGestureDragVector" %get-gesture-drag-vector) (:struct %Vector2)
)

; float GetGestureDragAngle(void);                        // Get gesture drag angle
(cffi:defcfun ("GetGestureDragAngle" %get-gesture-drag-angle) :float
)

; Vector2 GetGesturePinchVector(void);                    // Get gesture pinch delta
(cffi:defcfun ("GetGesturePinchVector" %get-gesture-pinch-vector) (:struct %Vector2)
)

; float GetGesturePinchAngle(void);                       // Get gesture pinch angle
(cffi:defcfun ("GetGesturePinchAngle" %get-gesture-pinch-angle) :float
)


; //------------------------------------------------------------------------------------
; // Camera System Functions (Module: rcamera)
; //------------------------------------------------------------------------------------
; void UpdateCamera(Camera *camera, int mode);            // Update camera position for selected mode
(cffi:defcfun ("UpdateCamera" %update-camera) :void
    (camera :pointer)
    (mode :int)
)

; void UpdateCameraPro(Camera *camera, Vector3 movement, Vector3 rotation, float zoom); // Update camera movement/rotation
(cffi:defcfun ("UpdateCameraPro" %update-camera-pro) :void
    (camera :pointer)
    (movement (:struct %Vector3))
    (rotation (:struct %Vector3))
    (zoom :float)
)


; module: rshapes →

; // NOTE: It can be useful when using basic shapes and one single font,
; // defining a font char white rectangle would allow drawing everything in a single draw call
; void SetShapesTexture(Texture2D texture, Rectangle source); // Set texture and rectangle to be used on shapes drawing
(cffi:defcfun ("SetShapesTexture" %set-shapes-texture) :void
    (texture (:struct %Texture))
    (source (:struct %Rectangle))
)

; Texture2D GetShapesTexture(void);                 // Get texture that is used for shapes drawing
(cffi:defcfun ("GetShapesTexture" %get-shapes-texture) (:struct %Texture)
)

; Rectangle GetShapesTextureRectangle(void);        // Get texture source rectangle that is used for shapes drawing
(cffi:defcfun ("GetShapesTextureRectangle" %get-shapes-texture-rectangle) (:struct %Rectangle)
)


; // Basic shapes drawing functions
; void DrawPixel(int posX, int posY, Color color);                                                   // Draw a pixel using geometry [Can be slow, use with care]
(cffi:defcfun ("DrawPixel" %draw-pixel) :void
    (posX :int)
    (posY :int)
    (color (:struct %Color))
)

; void DrawPixelV(Vector2 position, Color color);                                                    // Draw a pixel using geometry (Vector version) [Can be slow, use with care]
(cffi:defcfun ("DrawPixelV" %draw-pixel-v) :void
    (position (:struct %Vector2))
    (color (:struct %Color))
)

; void DrawLine(int startPosX, int startPosY, int endPosX, int endPosY, Color color);                // Draw a line
(cffi:defcfun ("DrawLine" %draw-line) :void
    (startPosX :int)
    (startPosY :int)
    (endPosX :int)
    (endPosY :int)
    (color (:struct %Color))
)

; void DrawLineV(Vector2 startPos, Vector2 endPos, Color color);                                     // Draw a line (using gl lines)
(cffi:defcfun ("DrawLineV" %draw-line-v) :void
    (startPos (:struct %Vector2))
    (endPos (:struct %Vector2))
    (color (:struct %Color))
)

; void DrawLineEx(Vector2 startPos, Vector2 endPos, float thick, Color color);                       // Draw a line (using triangles/quads)
(cffi:defcfun ("DrawLineEx" %draw-line-ex) :void
    (startPos (:struct %Vector2))
    (endPos (:struct %Vector2))
    (thick :float)
    (color (:struct %Color))
)

; void DrawLineStrip(const Vector2 *points, int pointCount, Color color);                            // Draw lines sequence (using gl lines)
(cffi:defcfun ("DrawLineStrip" %draw-line-strip) :void
    (points :pointer)
    (pointCount :int)
    (color (:struct %Color))
)

; void DrawLineBezier(Vector2 startPos, Vector2 endPos, float thick, Color color);                   // Draw line segment cubic-bezier in-out interpolation
(cffi:defcfun ("DrawLineBezier" %draw-line-bezier) :void
    (startPos (:struct %Vector2))
    (endPos (:struct %Vector2))
    (thick :float)
    (color (:struct %Color))
)

; void DrawLineDashed(Vector2 startPos, Vector2 endPos, int dashSize, int spaceSize, Color color);   // Draw a dashed line
(cffi:defcfun ("DrawLineDashed" %draw-line-dashed) :void
    (startPos (:struct %Vector2))
    (endPos (:struct %Vector2))
    (dashSize :int)
    (spaceSize :int)
    (color (:struct %Color))
)

; void DrawCircle(int centerX, int centerY, float radius, Color color);                              // Draw a color-filled circle
(cffi:defcfun ("DrawCircle" %draw-circle) :void
    (centerX :int)
    (centerY :int)
    (radius :float)
    (color (:struct %Color))
)

; void DrawCircleV(Vector2 center, float radius, Color color);                                       // Draw a color-filled circle (Vector version)
(cffi:defcfun ("DrawCircleV" %draw-circle-v) :void
    (center (:struct %Vector2))
    (radius :float)
    (color (:struct %Color))
)

; void DrawCircleGradient(Vector2 center, float radius, Color inner, Color outer);                   // Draw a gradient-filled circle
(cffi:defcfun ("DrawCircleGradient" %draw-circle-gradient) :void
    (center (:struct %Vector2))
    (radius :float)
    (inner (:struct %Color))
    (outer (:struct %Color))
)

; void DrawCircleSector(Vector2 center, float radius, float startAngle, float endAngle, int segments, Color color); // Draw a piece of a circle
(cffi:defcfun ("DrawCircleSector" %draw-circle-sector) :void
    (center (:struct %Vector2))
    (radius :float)
    (startAngle :float)
    (endAngle :float)
    (segments :int)
    (color (:struct %Color))
)

; void DrawCircleSectorLines(Vector2 center, float radius, float startAngle, float endAngle, int segments, Color color); // Draw circle sector outline
(cffi:defcfun ("DrawCircleSectorLines" %draw-circle-sector-lines) :void
    (center (:struct %Vector2))
    (radius :float)
    (startAngle :float)
    (endAngle :float)
    (segments :int)
    (color (:struct %Color))
)

; void DrawCircleLines(int centerX, int centerY, float radius, Color color);                         // Draw circle outline
(cffi:defcfun ("DrawCircleLines" %draw-circle-lines) :void
    (centerX :int)
    (centerY :int)
    (radius :float)
    (color (:struct %Color))
)

; void DrawCircleLinesV(Vector2 center, float radius, Color color);                                  // Draw circle outline (Vector version)
(cffi:defcfun ("DrawCircleLinesV" %draw-circle-lines-v) :void
    (center (:struct %Vector2))
    (radius :float)
    (color (:struct %Color))
)

; void DrawEllipse(int centerX, int centerY, float radiusH, float radiusV, Color color);             // Draw ellipse
(cffi:defcfun ("DrawEllipse" %draw-ellipse) :void
    (centerX :int)
    (centerY :int)
    (radiusH :float)
    (radiusV :float)
    (color (:struct %Color))
)

; void DrawEllipseV(Vector2 center, float radiusH, float radiusV, Color color);                      // Draw ellipse (Vector version)
(cffi:defcfun ("DrawEllipseV" %draw-ellipse-v) :void
    (center (:struct %Vector2))
    (radiusH :float)
    (radiusV :float)
    (color (:struct %Color))
)

; void DrawEllipseLines(int centerX, int centerY, float radiusH, float radiusV, Color color);        // Draw ellipse outline
(cffi:defcfun ("DrawEllipseLines" %draw-ellipse-lines) :void
    (centerX :int)
    (centerY :int)
    (radiusH :float)
    (radiusV :float)
    (color (:struct %Color))
)

; void DrawEllipseLinesV(Vector2 center, float radiusH, float radiusV, Color color);                 // Draw ellipse outline (Vector version)
(cffi:defcfun ("DrawEllipseLinesV" %draw-ellipse-lines-v) :void
    (center (:struct %Vector2))
    (radiusH :float)
    (radiusV :float)
    (color (:struct %Color))
)

; void DrawRing(Vector2 center, float innerRadius, float outerRadius, float startAngle, float endAngle, int segments, Color color); // Draw ring
(cffi:defcfun ("DrawRing" %draw-ring) :void
    (center (:struct %Vector2))
    (innerRadius :float)
    (outerRadius :float)
    (startAngle :float)
    (endAngle :float)
    (segments :int)
    (color (:struct %Color))
)

; void DrawRingLines(Vector2 center, float innerRadius, float outerRadius, float startAngle, float endAngle, int segments, Color color); // Draw ring outline
(cffi:defcfun ("DrawRingLines" %draw-ring-lines) :void
    (center (:struct %Vector2))
    (innerRadius :float)
    (outerRadius :float)
    (startAngle :float)
    (endAngle :float)
    (segments :int)
    (color (:struct %Color))
)

; void DrawRectangle(int posX, int posY, int width, int height, Color color);                        // Draw a color-filled rectangle
(cffi:defcfun ("DrawRectangle" %draw-rectangle) :void
    (posX :int)
    (posY :int)
    (width :int)
    (height :int)
    (color (:struct %Color))
)

; void DrawRectangleV(Vector2 position, Vector2 size, Color color);                                  // Draw a color-filled rectangle (Vector version)
(cffi:defcfun ("DrawRectangleV" %draw-rectangle-v) :void
    (position (:struct %Vector2))
    (size (:struct %Vector2))
    (color (:struct %Color))
)

; void DrawRectangleRec(Rectangle rec, Color color);                                                 // Draw a color-filled rectangle
(cffi:defcfun ("DrawRectangleRec" %draw-rectangle-rec) :void
    (rec (:struct %Rectangle))
    (color (:struct %Color))
)

; void DrawRectanglePro(Rectangle rec, Vector2 origin, float rotation, Color color);                 // Draw a color-filled rectangle with pro parameters
(cffi:defcfun ("DrawRectanglePro" %draw-rectangle-pro) :void
    (rec (:struct %Rectangle))
    (origin (:struct %Vector2))
    (rotation :float)
    (color (:struct %Color))
)

; void DrawRectangleGradientV(int posX, int posY, int width, int height, Color top, Color bottom);   // Draw a vertical-gradient-filled rectangle
(cffi:defcfun ("DrawRectangleGradientV" %draw-rectangle-gradient-v) :void
    (posX :int)
    (posY :int)
    (width :int)
    (height :int)
    (top (:struct %Color))
    (bottom (:struct %Color))
)

; void DrawRectangleGradientH(int posX, int posY, int width, int height, Color left, Color right);   // Draw a horizontal-gradient-filled rectangle
(cffi:defcfun ("DrawRectangleGradientH" %draw-rectangle-gradient-h) :void
    (posX :int)
    (posY :int)
    (width :int)
    (height :int)
    (left (:struct %Color))
    (right (:struct %Color))
)

; void DrawRectangleGradientEx(Rectangle rec, Color topLeft, Color bottomLeft, Color bottomRight, Color topRight); // Draw a gradient-filled rectangle with custom vertex colors
(cffi:defcfun ("DrawRectangleGradientEx" %draw-rectangle-gradient-ex) :void
    (rec (:struct %Rectangle))
    (topLeft (:struct %Color))
    (bottomLeft (:struct %Color))
    (bottomRight (:struct %Color))
    (topRight (:struct %Color))
)

; void DrawRectangleLines(int posX, int posY, int width, int height, Color color);                   // Draw rectangle outline
(cffi:defcfun ("DrawRectangleLines" %draw-rectangle-lines) :void
    (posX :int)
    (posY :int)
    (width :int)
    (height :int)
    (color (:struct %Color))
)

; void DrawRectangleLinesEx(Rectangle rec, float lineThick, Color color);                            // Draw rectangle outline with extended parameters
(cffi:defcfun ("DrawRectangleLinesEx" %draw-rectangle-lines-ex) :void
    (rec (:struct %Rectangle))
    (lineThick :float)
    (color (:struct %Color))
)

; void DrawRectangleRounded(Rectangle rec, float roundness, int segments, Color color);              // Draw rectangle with rounded edges
(cffi:defcfun ("DrawRectangleRounded" %draw-rectangle-rounded) :void
    (rec (:struct %Rectangle))
    (roundness :float)
    (segments :int)
    (color (:struct %Color))
)

; void DrawRectangleRoundedLines(Rectangle rec, float roundness, int segments, Color color);         // Draw rectangle lines with rounded edges
(cffi:defcfun ("DrawRectangleRoundedLines" %draw-rectangle-rounded-lines) :void
    (rec (:struct %Rectangle))
    (roundness :float)
    (segments :int)
    (color (:struct %Color))
)

; void DrawRectangleRoundedLinesEx(Rectangle rec, float roundness, int segments, float lineThick, Color color); // Draw rectangle with rounded edges outline
(cffi:defcfun ("DrawRectangleRoundedLinesEx" %draw-rectangle-rounded-lines-ex) :void
    (rec (:struct %Rectangle))
    (roundness :float)
    (segments :int)
    (lineThick :float)
    (color (:struct %Color))
)

; void DrawTriangle(Vector2 v1, Vector2 v2, Vector2 v3, Color color);                                // Draw a color-filled triangle (vertex in counter-clockwise order!)
(cffi:defcfun ("DrawTriangle" %draw-triangle) :void
    (v1 (:struct %Vector2))
    (v2 (:struct %Vector2))
    (v3 (:struct %Vector2))
    (color (:struct %Color))
)

; void DrawTriangleLines(Vector2 v1, Vector2 v2, Vector2 v3, Color color);                           // Draw triangle outline (vertex in counter-clockwise order!)
(cffi:defcfun ("DrawTriangleLines" %draw-triangle-lines) :void
    (v1 (:struct %Vector2))
    (v2 (:struct %Vector2))
    (v3 (:struct %Vector2))
    (color (:struct %Color))
)

; void DrawTriangleFan(const Vector2 *points, int pointCount, Color color);                          // Draw a triangle fan defined by points (first vertex is the center)
(cffi:defcfun ("DrawTriangleFan" %draw-triangle-fan) :void
    (points :pointer)
    (pointCount :int)
    (color (:struct %Color))
)

; void DrawTriangleStrip(const Vector2 *points, int pointCount, Color color);                        // Draw a triangle strip defined by points
(cffi:defcfun ("DrawTriangleStrip" %draw-triangle-strip) :void
    (points :pointer)
    (pointCount :int)
    (color (:struct %Color))
)

; void DrawPoly(Vector2 center, int sides, float radius, float rotation, Color color);               // Draw a regular polygon (Vector version)
(cffi:defcfun ("DrawPoly" %draw-poly) :void
    (center (:struct %Vector2))
    (sides :int)
    (radius :float)
    (rotation :float)
    (color (:struct %Color))
)

; void DrawPolyLines(Vector2 center, int sides, float radius, float rotation, Color color);          // Draw a polygon outline of n sides
(cffi:defcfun ("DrawPolyLines" %draw-poly-lines) :void
    (center (:struct %Vector2))
    (sides :int)
    (radius :float)
    (rotation :float)
    (color (:struct %Color))
)

; void DrawPolyLinesEx(Vector2 center, int sides, float radius, float rotation, float lineThick, Color color); // Draw a polygon outline of n sides with extended parameters
(cffi:defcfun ("DrawPolyLinesEx" %draw-poly-lines-ex) :void
    (center (:struct %Vector2))
    (sides :int)
    (radius :float)
    (rotation :float)
    (lineThick :float)
    (color (:struct %Color))
)


; // Splines drawing functions
; void DrawSplineLinear(const Vector2 *points, int pointCount, float thick, Color color);            // Draw spline: Linear, minimum 2 points
(cffi:defcfun ("DrawSplineLinear" %draw-spline-linear) :void
    (points :pointer)
    (pointCount :int)
    (thick :float)
    (color (:struct %Color))
)

; void DrawSplineBasis(const Vector2 *points, int pointCount, float thick, Color color);             // Draw spline: B-Spline, minimum 4 points
(cffi:defcfun ("DrawSplineBasis" %draw-spline-basis) :void
    (points :pointer)
    (pointCount :int)
    (thick :float)
    (color (:struct %Color))
)

; void DrawSplineCatmullRom(const Vector2 *points, int pointCount, float thick, Color color);        // Draw spline: Catmull-Rom, minimum 4 points
(cffi:defcfun ("DrawSplineCatmullRom" %draw-spline-catmull-rom) :void
    (points :pointer)
    (pointCount :int)
    (thick :float)
    (color (:struct %Color))
)

; void DrawSplineBezierQuadratic(const Vector2 *points, int pointCount, float thick, Color color);   // Draw spline: Quadratic Bezier, minimum 3 points (1 control point): [p1, c2, p3, c4...]
(cffi:defcfun ("DrawSplineBezierQuadratic" %draw-spline-bezier-quadratic) :void
    (points :pointer)
    (pointCount :int)
    (thick :float)
    (color (:struct %Color))
)

; void DrawSplineBezierCubic(const Vector2 *points, int pointCount, float thick, Color color);       // Draw spline: Cubic Bezier, minimum 4 points (2 control points): [p1, c2, c3, p4, c5, c6...]
(cffi:defcfun ("DrawSplineBezierCubic" %draw-spline-bezier-cubic) :void
    (points :pointer)
    (pointCount :int)
    (thick :float)
    (color (:struct %Color))
)

; void DrawSplineSegmentLinear(Vector2 p1, Vector2 p2, float thick, Color color);                    // Draw spline segment: Linear, 2 points
(cffi:defcfun ("DrawSplineSegmentLinear" %draw-spline-segment-linear) :void
    (p1 (:struct %Vector2))
    (p2 (:struct %Vector2))
    (thick :float)
    (color (:struct %Color))
)

; void DrawSplineSegmentBasis(Vector2 p1, Vector2 p2, Vector2 p3, Vector2 p4, float thick, Color color); // Draw spline segment: B-Spline, 4 points
(cffi:defcfun ("DrawSplineSegmentBasis" %draw-spline-segment-basis) :void
    (p1 (:struct %Vector2))
    (p2 (:struct %Vector2))
    (p3 (:struct %Vector2))
    (p4 (:struct %Vector2))
    (thick :float)
    (color (:struct %Color))
)

; void DrawSplineSegmentCatmullRom(Vector2 p1, Vector2 p2, Vector2 p3, Vector2 p4, float thick, Color color); // Draw spline segment: Catmull-Rom, 4 points
(cffi:defcfun ("DrawSplineSegmentCatmullRom" %draw-spline-segment-catmull-rom) :void
    (p1 (:struct %Vector2))
    (p2 (:struct %Vector2))
    (p3 (:struct %Vector2))
    (p4 (:struct %Vector2))
    (thick :float)
    (color (:struct %Color))
)

; void DrawSplineSegmentBezierQuadratic(Vector2 p1, Vector2 c2, Vector2 p3, float thick, Color color); // Draw spline segment: Quadratic Bezier, 2 points, 1 control point
(cffi:defcfun ("DrawSplineSegmentBezierQuadratic" %draw-spline-segment-bezier-quadratic) :void
    (p1 (:struct %Vector2))
    (c2 (:struct %Vector2))
    (p3 (:struct %Vector2))
    (thick :float)
    (color (:struct %Color))
)

; void DrawSplineSegmentBezierCubic(Vector2 p1, Vector2 c2, Vector2 c3, Vector2 p4, float thick, Color color); // Draw spline segment: Cubic Bezier, 2 points, 2 control points
(cffi:defcfun ("DrawSplineSegmentBezierCubic" %draw-spline-segment-bezier-cubic) :void
    (p1 (:struct %Vector2))
    (c2 (:struct %Vector2))
    (c3 (:struct %Vector2))
    (p4 (:struct %Vector2))
    (thick :float)
    (color (:struct %Color))
)


; // Spline segment point evaluation functions, for a given t [0.0f .. 1.0f]
; Vector2 GetSplinePointLinear(Vector2 startPos, Vector2 endPos, float t);                           // Get (evaluate) spline point: Linear
(cffi:defcfun ("GetSplinePointLinear" %get-spline-point-linear) (:struct %Vector2)
    (startPos (:struct %Vector2))
    (endPos (:struct %Vector2))
    (tt :float)
)

; Vector2 GetSplinePointBasis(Vector2 p1, Vector2 p2, Vector2 p3, Vector2 p4, float t);              // Get (evaluate) spline point: B-Spline
(cffi:defcfun ("GetSplinePointBasis" %get-spline-point-basis) (:struct %Vector2)
    (p1 (:struct %Vector2))
    (p2 (:struct %Vector2))
    (p3 (:struct %Vector2))
    (p4 (:struct %Vector2))
    (tt :float)
)

; Vector2 GetSplinePointCatmullRom(Vector2 p1, Vector2 p2, Vector2 p3, Vector2 p4, float t);         // Get (evaluate) spline point: Catmull-Rom
(cffi:defcfun ("GetSplinePointCatmullRom" %get-spline-point-catmull-rom) (:struct %Vector2)
    (p1 (:struct %Vector2))
    (p2 (:struct %Vector2))
    (p3 (:struct %Vector2))
    (p4 (:struct %Vector2))
    (tt :float))

; Vector2 GetSplinePointBezierQuad(Vector2 p1, Vector2 c2, Vector2 p3, float t);                     // Get (evaluate) spline point: Quadratic Bezier
(cffi:defcfun ("GetSplinePointBezierQuad" %get-spline-point-bezier-quad) (:struct %Vector2)
    (p1 (:struct %Vector2))
    (c2 (:struct %Vector2))
    (p3 (:struct %Vector2))
    (tt :float)
)

; Vector2 GetSplinePointBezierCubic(Vector2 p1, Vector2 c2, Vector2 c3, Vector2 p4, float t);        // Get (evaluate) spline point: Cubic Bezier
(cffi:defcfun ("GetSplinePointBezierCubic" %get-spline-point-bezier-cubic) (:struct %Vector2)
    (p1 (:struct %Vector2))
    (c2 (:struct %Vector2))
    (c3 (:struct %Vector2))
    (p4 (:struct %Vector2))
    (tt :float)
)


; // Basic shapes collision detection functions
; bool CheckCollisionRecs(Rectangle rec1, Rectangle rec2);                                           // Check collision between two rectangles
(cffi:defcfun ("CheckCollisionRecs" %check-collision-recs) :bool
    (rec1 (:struct %Rectangle))
    (rec2 (:struct %Rectangle))
)

; bool CheckCollisionCircles(Vector2 center1, float radius1, Vector2 center2, float radius2);        // Check collision between two circles
(cffi:defcfun ("CheckCollisionCircles" %check-collision-circles) :bool
    (center1 (:struct %Vector2))
    (radius1 :float)
    (center2 (:struct %Vector2))
    (radius2 :float)
)

; bool CheckCollisionCircleRec(Vector2 center, float radius, Rectangle rec);                         // Check collision between circle and rectangle
(cffi:defcfun ("CheckCollisionCircleRec" %check-collision-circle-rec) :bool
    (center (:struct %Vector2))
    (radius :float)
    (rec (:struct %Rectangle))
)

; bool CheckCollisionCircleLine(Vector2 center, float radius, Vector2 p1, Vector2 p2);               // Check if circle collides with a line created betweeen two points [p1] and [p2]
(cffi:defcfun ("CheckCollisionCircleLine" %check-collision-circle-line) :bool
    (center (:struct %Vector2))
    (radius :float)
    (p1 (:struct %Vector2))
    (p2 (:struct %Vector2))
)

; bool CheckCollisionPointRec(Vector2 point, Rectangle rec);                                         // Check if point is inside rectangle
(cffi:defcfun ("CheckCollisionPointRec" %check-collision-point-rec) :bool
    (point (:struct %Vector2))
    (rec (:struct %Rectangle))
)

; bool CheckCollisionPointCircle(Vector2 point, Vector2 center, float radius);                       // Check if point is inside circle
(cffi:defcfun ("CheckCollisionPointCircle" %check-collision-point-circle) :bool
    (point (:struct %Vector2))
    (center (:struct %Vector2))
    (radius :float)
)

; bool CheckCollisionPointTriangle(Vector2 point, Vector2 p1, Vector2 p2, Vector2 p3);               // Check if point is inside a triangle
(cffi:defcfun ("CheckCollisionPointTriangle" %check-collision-point-triangle) :bool
    (point (:struct %Vector2))
    (p1 (:struct %Vector2))
    (p2 (:struct %Vector2))
    (p3 (:struct %Vector2))
)

; bool CheckCollisionPointLine(Vector2 point, Vector2 p1, Vector2 p2, int threshold);                // Check if point belongs to line created between two points [p1] and [p2] with defined margin in pixels [threshold]
(cffi:defcfun ("CheckCollisionPointLine" %check-collision-point-line) :bool
    (point (:struct %Vector2))
    (p1 (:struct %Vector2))
    (p2 (:struct %Vector2))
    (threshold :int)
)

; bool CheckCollisionPointPoly(Vector2 point, const Vector2 *points, int pointCount);                // Check if point is within a polygon described by array of vertices
(cffi:defcfun ("CheckCollisionPointPoly" %check-collision-point-poly) :bool
    (point (:struct %Vector2))
    (points :pointer)
    (pointCount :int)
)

; bool CheckCollisionLines(Vector2 startPos1, Vector2 endPos1, Vector2 startPos2, Vector2 endPos2, Vector2 *collisionPoint); // Check the collision between two lines defined by two points each, returns collision point by reference
(cffi:defcfun ("CheckCollisionLines" %check-collision-lines) :bool
    (startPos1 (:struct %Vector2))
    (endPos1 (:struct %Vector2))
    (startPos2 (:struct %Vector2))
    (endPos2 (:struct %Vector2))
    (collisionPoint :pointer)
)

; Rectangle GetCollisionRec(Rectangle rec1, Rectangle rec2);                                         // Get collision rectangle for two rectangles collision
(cffi:defcfun ("GetCollisionRec" %get-collision-rec) (:struct %Rectangle)
    (rec1 (:struct %Rectangle))
    (rec2 (:struct %Rectangle))
)


; module: rtextures →

; // Image loading functions
; // NOTE: These functions do not require GPU access
; Image LoadImage(const char *fileName);                                                             // Load image from file into CPU memory (RAM)
(cffi:defcfun ("LoadImage" %load-image) (:struct %Image)
    (fileName :string)
)

; Image LoadImageRaw(const char *fileName, int width, int height, int format, int headerSize);       // Load image from RAW file data
(cffi:defcfun ("LoadImageRaw" %load-image-raw) (:struct %Image)
    (fileName :string)
    (width :int)
    (height :int)
    (format :int)
    (headerSize :int)
)

; Image LoadImageAnim(const char *fileName, int *frames);                                            // Load image sequence from file (frames appended to image.data)
(cffi:defcfun ("LoadImageAnim" %load-image-anim) (:struct %Image)
    (fileName :string)
    (frames :pointer)
)

; Image LoadImageAnimFromMemory(const char *fileType, const unsigned char *fileData, int dataSize, int *frames); // Load image sequence from memory buffer
(cffi:defcfun ("LoadImageAnimFromMemory" %load-image-anim-from-memory) (:struct %Image)
    (fileType :string)
    (fileData :pointer)
    (dataSize :int)
    (frames :pointer)
)

; Image LoadImageFromMemory(const char *fileType, const unsigned char *fileData, int dataSize);      // Load image from memory buffer, fileType refers to extension: i.e. '.png'
(cffi:defcfun ("LoadImageFromMemory" %load-image-from-memory) (:struct %Image)
    (fileType :string)
    (fileData :pointer)
    (dataSize :int)
)

; Image LoadImageFromTexture(Texture2D texture);                                                     // Load image from GPU texture data
(cffi:defcfun ("LoadImageFromTexture" %load-image-from-texture) (:struct %Image)
    (texture (:struct %Texture))
)

; Image LoadImageFromScreen(void);                                                                   // Load image from screen buffer and (screenshot)
(cffi:defcfun ("LoadImageFromScreen" %load-image-from-screen) (:struct %Image)
)

; bool IsImageValid(Image image);                                                                    // Check if an image is valid (data and parameters)
(cffi:defcfun ("IsImageValid" %is-image-valid) :bool
    (image (:struct %Image))
)

; void UnloadImage(Image image);                                                                     // Unload image from CPU memory (RAM)
(cffi:defcfun ("UnloadImage" %unload-image) :void
    (image (:struct %Image))
)

; bool ExportImage(Image image, const char *fileName);                                               // Export image data to file, returns true on success
(cffi:defcfun ("ExportImage" %export-image) :bool
    (image (:struct %Image))
    (fileName :string)
)

; unsigned char *ExportImageToMemory(Image image, const char *fileType, int *fileSize);              // Export image to memory buffer, memory must be MemFree()
(cffi:defcfun ("ExportImageToMemory" %export-image-to-memory) :pointer
    (image (:struct %Image))
    (fileType :string)
    (fileSize :pointer)
)

; bool ExportImageAsCode(Image image, const char *fileName);                                         // Export image as code file defining an array of bytes, returns true on success
(cffi:defcfun ("ExportImageAsCode" %export-image-as-code) :bool
    (image (:struct %Image))
    (fileName :string)
)


; // Image generation functions
; Image GenImageColor(int width, int height, Color color);                                           // Generate image: plain color
(cffi:defcfun ("GenImageColor" %gen-image-color) (:struct %Image)
    (width :int)
    (height :int)
    (color (:struct %Color))
)

; Image GenImageGradientLinear(int width, int height, int direction, Color start, Color end);        // Generate image: linear gradient, direction in degrees [0..360], 0=Vertical gradient
(cffi:defcfun ("GenImageGradientLinear" %gen-image-gradient-linear) (:struct %Image)
    (width :int)
    (height :int)
    (direction :int)
    (start (:struct %Color))
    (end (:struct %Color))
)

; Image GenImageGradientRadial(int width, int height, float density, Color inner, Color outer);      // Generate image: radial gradient
(cffi:defcfun ("GenImageGradientRadial" %gen-image-gradient-radial) (:struct %Image)
    (width :int)
    (height :int)
    (density :float)
    (inner (:struct %Color))
    (outer (:struct %Color))
)

; Image GenImageGradientSquare(int width, int height, float density, Color inner, Color outer);      // Generate image: square gradient
(cffi:defcfun ("GenImageGradientSquare" %gen-image-gradient-square) (:struct %Image)
    (width :int)
    (height :int)
    (density :float)
    (inner (:struct %Color))
    (outer (:struct %Color))
)

; Image GenImageChecked(int width, int height, int checksX, int checksY, Color col1, Color col2);    // Generate image: checked
(cffi:defcfun ("GenImageChecked" %gen-image-checked) (:struct %Image)
    (width :int)
    (height :int)
    (checksX :int)
    (checksY :int)
    (col1 (:struct %Color))
    (col2 (:struct %Color))
)

; Image GenImageWhiteNoise(int width, int height, float factor);                                     // Generate image: white noise
(cffi:defcfun ("GenImageWhiteNoise" %gen-image-white-noise) (:struct %Image)
    (width :int)
    (height :int)
    (factor :float)
)

; Image GenImagePerlinNoise(int width, int height, int offsetX, int offsetY, float scale);           // Generate image: perlin noise
(cffi:defcfun ("GenImagePerlinNoise" %gen-image-perlin-noise) (:struct %Image)
    (width :int)
    (height :int)
    (offsetX :int)
    (offsetY :int)
    (scale :float)
)

; Image GenImageCellular(int width, int height, int tileSize);                                       // Generate image: cellular algorithm, bigger tileSize means bigger cells
(cffi:defcfun ("GenImageCellular" %gen-image-cellular) (:struct %Image)
    (width :int)
    (height :int)
    (tileSize :int)
)

; Image GenImageText(int width, int height, const char *text);                                       // Generate image: grayscale image from text data
(cffi:defcfun ("GenImageText" %gen-image-text) (:struct %Image)
    (width :int)
    (height :int)
    (text :string)
)


; // Image manipulation functions
; Image ImageCopy(Image image);                                                                      // Create an image duplicate (useful for transformations)
(cffi:defcfun ("ImageCopy" %image-copy) (:struct %Image)
    (image (:struct %Image))
)

; Image ImageFromImage(Image image, Rectangle rec);                                                  // Create an image from another image piece
(cffi:defcfun ("ImageFromImage" %image-from-image) (:struct %Image)
    (image (:struct %Image))
    (rec (:struct %Rectangle))
)

; Image ImageFromChannel(Image image, int selectedChannel);                                          // Create an image from a selected channel of another image (GRAYSCALE)
(cffi:defcfun ("ImageFromChannel" %image-from-channel) (:struct %Image)
    (image (:struct %Image))
    (selectedChannel :int)
)

; Image ImageText(const char *text, int fontSize, Color color);                                      // Create an image from text (default font)
(cffi:defcfun ("ImageText" %image-text) (:struct %Image)
    (text :string)
    (fontSize :int)
    (color (:struct %Color))
)

; Image ImageTextEx(Font font, const char *text, float fontSize, float spacing, Color tint);         // Create an image from text (custom sprite font)
(cffi:defcfun ("ImageTextEx" %image-text-ex) (:struct %Image)
    (font (:struct %Font))
    (text :string)
    (fontSize :float)
    (spacing :float)
    (tint (:struct %Color))
)

; void ImageFormat(Image *image, int newFormat);                                                     // Convert image data to desired format
(cffi:defcfun ("ImageFormat" %image-format) :void
    (image :pointer)
    (newFormat :int)
)

; void ImageToPOT(Image *image, Color fill);                                                         // Convert image to POT (power-of-two)
(cffi:defcfun ("ImageToPOT" %image-to-pot) :void
    (image :pointer)
    (fill (:struct %Color))
)

; void ImageCrop(Image *image, Rectangle crop);                                                      // Crop an image to a defined rectangle
(cffi:defcfun ("ImageCrop" %image-crop) :void
    (image :pointer)
    (crop (:struct %Rectangle))
)

; void ImageAlphaCrop(Image *image, float threshold);                                                // Crop image depending on alpha value
(cffi:defcfun ("ImageAlphaCrop" %image-alpha-crop) :void
    (image :pointer)
    (threshold :float)
)

; void ImageAlphaClear(Image *image, Color color, float threshold);                                  // Clear alpha channel to desired color
(cffi:defcfun ("ImageAlphaClear" %image-alpha-clear) :void
    (image :pointer)
    (color (:struct %Color))
    (threshold :float)
)

; void ImageAlphaMask(Image *image, Image alphaMask);                                                // Apply alpha mask to image
(cffi:defcfun ("ImageAlphaMask" %image-alpha-mask) :void
    (image :pointer)
    (alphaMask (:struct %Image))
)

; void ImageAlphaPremultiply(Image *image);                                                          // Premultiply alpha channel
(cffi:defcfun ("ImageAlphaPremultiply" %image-alpha-premultiply) :void
    (image :pointer)
)

; void ImageBlurGaussian(Image *image, int blurSize);                                                // Apply Gaussian blur using a box blur approximation
(cffi:defcfun ("ImageBlurGaussian" %image-blur-gaussian) :void
    (image :pointer)
    (blurSize :int)
)

; void ImageKernelConvolution(Image *image, const float *kernel, int kernelSize);                    // Apply custom square convolution kernel to image
(cffi:defcfun ("ImageKernelConvolution" %image-kernel-convolution) :void
    (image :pointer)
    (kernel :pointer)
    (kernelSize :int)
)

; void ImageResize(Image *image, int newWidth, int newHeight);                                       // Resize image (Bicubic scaling algorithm)
(cffi:defcfun ("ImageResize" %image-resize) :void
    (image :pointer)
    (newWidth :int)
    (newHeight :int)
)

; void ImageResizeNN(Image *image, int newWidth, int newHeight);                                     // Resize image (Nearest-Neighbor scaling algorithm)
(cffi:defcfun ("ImageResizeNN" %image-resize-nn) :void
    (image :pointer)
    (newWidth :int)
    (newHeight :int)
)

; void ImageResizeCanvas(Image *image, int newWidth, int newHeight, int offsetX, int offsetY, Color fill); // Resize canvas and fill with color
(cffi:defcfun ("ImageResizeCanvas" %image-resize-canvas) :void
    (image :pointer)
    (newWidth :int)
    (newHeight :int)
    (offsetX :int)
    (offsetY :int)
    (fill (:struct %Color))
)

; void ImageMipmaps(Image *image);                                                                   // Compute all mipmap levels for a provided image
(cffi:defcfun ("ImageMipmaps" %image-mipmaps) :void
    (image :pointer)
)

; void ImageDither(Image *image, int rBpp, int gBpp, int bBpp, int aBpp);                            // Dither image data to 16bpp or lower (Floyd-Steinberg dithering)
(cffi:defcfun ("ImageDither" %image-dither) :void
    (image :pointer)
    (rBpp :int)
    (gBpp :int)
    (bBpp :int)
    (aBpp :int)
)

; void ImageFlipVertical(Image *image);                                                              // Flip image vertically
(cffi:defcfun ("ImageFlipVertical" %image-flip-vertical) :void
    (image :pointer)
)

; void ImageFlipHorizontal(Image *image);                                                            // Flip image horizontally
(cffi:defcfun ("ImageFlipHorizontal" %image-flip-horizontal) :void
    (image :pointer)
)

; void ImageRotate(Image *image, int degrees);                                                       // Rotate image by input angle in degrees (-359 to 359)
(cffi:defcfun ("ImageRotate" %image-rotate) :void
    (image :pointer)
    (degrees :int)
)

; void ImageRotateCW(Image *image);                                                                  // Rotate image clockwise 90deg
(cffi:defcfun ("ImageRotateCW" %image-rotate-cw) :void
    (image :pointer)
)

; void ImageRotateCCW(Image *image);                                                                 // Rotate image counter-clockwise 90deg
(cffi:defcfun ("ImageRotateCCW" %image-rotate-ccw) :void
    (image :pointer)
)

; void ImageColorTint(Image *image, Color color);                                                    // Modify image color: tint
(cffi:defcfun ("ImageColorTint" %image-color-tint) :void
    (image :pointer)
    (color (:struct %Color))
)

; void ImageColorInvert(Image *image);                                                               // Modify image color: invert
(cffi:defcfun ("ImageColorInvert" %image-color-invert) :void
    (image :pointer)
)

; void ImageColorGrayscale(Image *image);                                                            // Modify image color: grayscale
(cffi:defcfun ("ImageColorGrayscale" %image-color-grayscale) :void
    (image :pointer)
)

; void ImageColorContrast(Image *image, float contrast);                                             // Modify image color: contrast (-100 to 100)
(cffi:defcfun ("ImageColorContrast" %image-color-contrast) :void
    (image :pointer)
    (contrast :float)
)

; void ImageColorBrightness(Image *image, int brightness);                                           // Modify image color: brightness (-255 to 255)
(cffi:defcfun ("ImageColorBrightness" %image-color-brightness) :void
    (image :pointer)
    (brightness :int)
)

; void ImageColorReplace(Image *image, Color color, Color replace);                                  // Modify image color: replace color
(cffi:defcfun ("ImageColorReplace" %image-color-replace) :void
    (image :pointer)
    (color (:struct %Color))
    (replace (:struct %Color))
)

; Color *LoadImageColors(Image image);                                                               // Load color data from image as a Color array (RGBA - 32bit)
(cffi:defcfun ("LoadImageColors" %load-image-colors) :pointer
    (image (:struct %Image))
)

; Color *LoadImagePalette(Image image, int maxPaletteSize, int *colorCount);                         // Load colors palette from image as a Color array (RGBA - 32bit)
(cffi:defcfun ("LoadImagePalette" %load-image-palette) :pointer
    (image (:struct %Image))
    (maxPaletteSize :int)
    (colorCount :pointer)
)

; void UnloadImageColors(Color *colors);                                                             // Unload color data loaded with LoadImageColors()
(cffi:defcfun ("UnloadImageColors" %unload-image-colors) :void
    (colors :pointer)
)

; void UnloadImagePalette(Color *colors);                                                            // Unload colors palette loaded with LoadImagePalette()
(cffi:defcfun ("UnloadImagePalette" %unload-image-palette) :void
    (colors :pointer)
)

; Rectangle GetImageAlphaBorder(Image image, float threshold);                                       // Get image alpha border rectangle
(cffi:defcfun ("GetImageAlphaBorder" %get-image-alpha-border) (:struct %Rectangle)
    (image (:struct %Image))
    (threshold :float)
)

; Color GetImageColor(Image image, int x, int y);                                                    // Get image pixel color at (x, y) position
(cffi:defcfun ("GetImageColor" %get-image-color) (:struct %Color)
    (image (:struct %Image))
    (x :int)
    (y :int)
)


; // Image drawing functions
; // NOTE: Image software-rendering functions (CPU)
; void ImageClearBackground(Image *dst, Color color);                                                // Clear image background with given color
(cffi:defcfun ("ImageClearBackground" %image-clear-background) :void
    (dst :pointer)
    (color (:struct %Color))
)

; void ImageDrawPixel(Image *dst, int posX, int posY, Color color);                                  // Draw pixel within an image
(cffi:defcfun ("ImageDrawPixel" %image-draw-pixel) :void
    (dst :pointer)
    (posX :int)
    (posY :int)
    (color (:struct %Color))
)

; void ImageDrawPixelV(Image *dst, Vector2 position, Color color);                                   // Draw pixel within an image (Vector version)
(cffi:defcfun ("ImageDrawPixelV" %image-draw-pixel-v) :void
    (dst :pointer)
    (position (:struct %Vector2))
    (color (:struct %Color))
)

; void ImageDrawLine(Image *dst, int startPosX, int startPosY, int endPosX, int endPosY, Color color); // Draw line within an image
(cffi:defcfun ("ImageDrawLine" %image-draw-line) :void
    (dst :pointer)
    (startPosX :int)
    (startPosY :int)
    (endPosX :int)
    (endPosY :int)
    (color (:struct %Color))
)

; void ImageDrawLineV(Image *dst, Vector2 start, Vector2 end, Color color);                          // Draw line within an image (Vector version)
(cffi:defcfun ("ImageDrawLineV" %image-draw-line-v) :void
    (dst :pointer)
    (start (:struct %Vector2))
    (end (:struct %Vector2))
    (color (:struct %Color))
)

; void ImageDrawLineEx(Image *dst, Vector2 start, Vector2 end, int thick, Color color);              // Draw a line defining thickness within an image
(cffi:defcfun ("ImageDrawLineEx" %image-draw-line-ex) :void
    (dst :pointer)
    (start (:struct %Vector2))
    (end (:struct %Vector2))
    (thick :int)
    (color (:struct %Color))
)

; void ImageDrawCircle(Image *dst, int centerX, int centerY, int radius, Color color);               // Draw a filled circle within an image
(cffi:defcfun ("ImageDrawCircle" %image-draw-circle) :void
    (dst :pointer)
    (centerX :int)
    (centerY :int)
    (radius :int)
    (color (:struct %Color))
)

; void ImageDrawCircleV(Image *dst, Vector2 center, int radius, Color color);                        // Draw a filled circle within an image (Vector version)
(cffi:defcfun ("ImageDrawCircleV" %image-draw-circle-v) :void
    (dst :pointer)
    (center (:struct %Vector2))
    (radius :int)
    (color (:struct %Color))
)

; void ImageDrawCircleLines(Image *dst, int centerX, int centerY, int radius, Color color);          // Draw circle outline within an image
(cffi:defcfun ("ImageDrawCircleLines" %image-draw-circle-lines) :void
    (dst :pointer)
    (centerX :int)
    (centerY :int)
    (radius :int)
    (color (:struct %Color))
)

; void ImageDrawCircleLinesV(Image *dst, Vector2 center, int radius, Color color);                   // Draw circle outline within an image (Vector version)
(cffi:defcfun ("ImageDrawCircleLinesV" %image-draw-circle-lines-v) :void
    (dst :pointer)
    (center (:struct %Vector2))
    (radius :int)
    (color (:struct %Color))
)

; void ImageDrawRectangle(Image *dst, int posX, int posY, int width, int height, Color color);       // Draw rectangle within an image
(cffi:defcfun ("ImageDrawRectangle" %image-draw-rectangle) :void
    (dst :pointer)
    (posX :int)
    (posY :int)
    (width :int)
    (height :int)
    (color (:struct %Color))
)

; void ImageDrawRectangleV(Image *dst, Vector2 position, Vector2 size, Color color);                 // Draw rectangle within an image (Vector version)
(cffi:defcfun ("ImageDrawRectangleV" %image-draw-rectangle-v) :void
    (dst :pointer)
    (position (:struct %Vector2))
    (size (:struct %Vector2))
    (color (:struct %Color))
)

; void ImageDrawRectangleRec(Image *dst, Rectangle rec, Color color);                                // Draw rectangle within an image
(cffi:defcfun ("ImageDrawRectangleRec" %image-draw-rectangle-rec) :void
    (dst :pointer)
    (rec (:struct %Rectangle))
    (color (:struct %Color))
)

; void ImageDrawRectangleLines(Image *dst, Rectangle rec, int thick, Color color);                   // Draw rectangle lines within an image
(cffi:defcfun ("ImageDrawRectangleLines" %image-draw-rectangle-lines) :void
    (dst :pointer)
    (rec (:struct %Rectangle))
    (thick :int)
    (color (:struct %Color))
)

; void ImageDrawTriangle(Image *dst, Vector2 v1, Vector2 v2, Vector2 v3, Color color);               // Draw triangle within an image
(cffi:defcfun ("ImageDrawTriangle" %image-draw-triangle) :void
    (dst :pointer)
    (v1 (:struct %Vector2))
    (v2 (:struct %Vector2))
    (v3 (:struct %Vector2))
    (color (:struct %Color))
)

; void ImageDrawTriangleEx(Image *dst, Vector2 v1, Vector2 v2, Vector2 v3, Color c1, Color c2, Color c3); // Draw triangle with interpolated colors within an image
(cffi:defcfun ("ImageDrawTriangleEx" %image-draw-triangle-ex) :void
    (dst :pointer)
    (v1 (:struct %Vector2))
    (v2 (:struct %Vector2))
    (v3 (:struct %Vector2))
    (c1 (:struct %Color))
    (c2 (:struct %Color))
    (c3 (:struct %Color))
)

; void ImageDrawTriangleLines(Image *dst, Vector2 v1, Vector2 v2, Vector2 v3, Color color);          // Draw triangle outline within an image
(cffi:defcfun ("ImageDrawTriangleLines" %image-draw-triangle-lines) :void
    (dst :pointer)
    (v1 (:struct %Vector2))
    (v2 (:struct %Vector2))
    (v3 (:struct %Vector2))
    (color (:struct %Color))
)

; void ImageDrawTriangleFan(Image *dst, const Vector2 *points, int pointCount, Color color);         // Draw a triangle fan defined by points within an image (first vertex is the center)
(cffi:defcfun ("ImageDrawTriangleFan" %image-draw-triangle-fan) :void
    (dst :pointer)
    (points :pointer)
    (pointCount :int)
    (color (:struct %Color))
)

; void ImageDrawTriangleStrip(Image *dst, const Vector2 *points, int pointCount, Color color);       // Draw a triangle strip defined by points within an image
(cffi:defcfun ("ImageDrawTriangleStrip" %image-draw-triangle-strip) :void
    (dst :pointer)
    (points :pointer)
    (pointCount :int)
    (color (:struct %Color))
)

; void ImageDraw(Image *dst, Image src, Rectangle srcRec, Rectangle dstRec, Color tint);             // Draw a source image within a destination image (tint applied to source)
(cffi:defcfun ("ImageDraw" %image-draw) :void
    (dst :pointer)
    (src (:struct %Image))
    (srcRec (:struct %Rectangle))
    (dstRec (:struct %Rectangle))
    (tint (:struct %Color))
)

; void ImageDrawText(Image *dst, const char *text, int posX, int posY, int fontSize, Color color);   // Draw text (using default font) within an image (destination)
(cffi:defcfun ("ImageDrawText" %image-draw-text) :void
    (dst :pointer)
    (text :string)
    (posX :int)
    (posY :int)
    (fontSize :int)
    (color (:struct %Color))
)

; void ImageDrawTextEx(Image *dst, Font font, const char *text, Vector2 position, float fontSize, float spacing, Color tint); // Draw text (custom sprite font) within an image (destination)
(cffi:defcfun ("ImageDrawTextEx" %image-draw-text-ex) :void
    (dst :pointer)
    (font (:struct %Font))
    (text :string)
    (position (:struct %Vector2))
    (fontSize :float)
    (spacing :float)
    (tint (:struct %Color))
)


; // Texture loading functions
; // NOTE: These functions require GPU access
; Texture2D LoadTexture(const char *fileName);                                                       // Load texture from file into GPU memory (VRAM)
(cffi:defcfun ("LoadTexture" %load-texture) (:struct %Texture)
    (fileName :string)
)

; Texture2D LoadTextureFromImage(Image image);                                                       // Load texture from image data
(cffi:defcfun ("LoadTextureFromImage" %load-texture-from-image) (:struct %Texture)
    (image (:struct %Image))
)

; TextureCubemap LoadTextureCubemap(Image image, int layout);                                        // Load cubemap from image, multiple image cubemap layouts supported
(cffi:defcfun ("LoadTextureCubemap" %load-texture-cubemap) (:struct %Texture)
    (image (:struct %Image))
    (layout :int)
)

; RenderTexture2D LoadRenderTexture(int width, int height);                                          // Load texture for rendering (framebuffer)
(cffi:defcfun ("LoadRenderTexture" %load-render-texture) (:struct %RenderTexture)
    (width :int)
    (height :int)
)

; bool IsTextureValid(Texture2D texture);                                                            // Check if a texture is valid (loaded in GPU)
(cffi:defcfun ("IsTextureValid" %is-texture-valid) :bool
    (texture (:struct %Texture))
)

; void UnloadTexture(Texture2D texture);                                                             // Unload texture from GPU memory (VRAM)
(cffi:defcfun ("UnloadTexture" %unload-texture) :void
    (texture (:struct %Texture))
)

; bool IsRenderTextureValid(RenderTexture2D target);                                                 // Check if a render texture is valid (loaded in GPU)
(cffi:defcfun ("IsRenderTextureValid" %is-render-texture-valid) :bool
    (target (:struct %RenderTexture))
)

; void UnloadRenderTexture(RenderTexture2D target);                                                  // Unload render texture from GPU memory (VRAM)
(cffi:defcfun ("UnloadRenderTexture" %unload-render-texture) :void
    (target (:struct %RenderTexture))
)

; void UpdateTexture(Texture2D texture, const void *pixels);                                         // Update GPU texture with new data (pixels should be able to fill texture)
(cffi:defcfun ("UpdateTexture" %update-texture) :void
    (texture (:struct %Texture))
    (pixels :pointer)
)

; void UpdateTextureRec(Texture2D texture, Rectangle rec, const void *pixels);                       // Update GPU texture rectangle with new data (pixels and rec should fit in texture)
(cffi:defcfun ("UpdateTextureRec" %update-texture-rec) :void
    (texture (:struct %Texture))
    (rec (:struct %Rectangle))
    (pixels :pointer)
)


; // Texture configuration functions
; void GenTextureMipmaps(Texture2D *texture);                                                        // Generate GPU mipmaps for a texture
(cffi:defcfun ("GenTextureMipmaps" %gen-texture-mipmaps) :void
    (texture :pointer)
)

; void SetTextureFilter(Texture2D texture, int filter);                                              // Set texture scaling filter mode
(cffi:defcfun ("SetTextureFilter" %set-texture-filter) :void
    (texture (:struct %Texture))
    (filter :int)
)

; void SetTextureWrap(Texture2D texture, int wrap);                                                  // Set texture wrapping mode
(cffi:defcfun ("SetTextureWrap" %set-texture-wrap) :void
    (texture (:struct %Texture))
    (wrap :int)
)


; // Texture drawing functions
; void DrawTexture(Texture2D texture, int posX, int posY, Color tint);                               // Draw a Texture2D
(cffi:defcfun ("DrawTexture" %draw-texture) :void
    (texture (:struct %Texture))
    (posX :int)
    (posY :int)
    (tint (:struct %Color))
)

; void DrawTextureV(Texture2D texture, Vector2 position, Color tint);                                // Draw a Texture2D with position defined as Vector2
(cffi:defcfun ("DrawTextureV" %draw-texture-v) :void
    (texture (:struct %Texture))
    (position (:struct %Vector2))
    (tint (:struct %Color))
)

; void DrawTextureEx(Texture2D texture, Vector2 position, float rotation, float scale, Color tint);  // Draw a Texture2D with extended parameters
(cffi:defcfun ("DrawTextureEx" %draw-texture-ex) :void
    (texture (:struct %Texture))
    (position (:struct %Vector2))
    (rotation :float)
    (scale :float)
    (tint (:struct %Color))
)

; void DrawTextureRec(Texture2D texture, Rectangle source, Vector2 position, Color tint);            // Draw a part of a texture defined by a rectangle
(cffi:defcfun ("DrawTextureRec" %draw-texture-rec) :void
    (texture (:struct %Texture))
    (source (:struct %Rectangle))
    (position (:struct %Vector2))
    (tint (:struct %Color))
)

; void DrawTexturePro(Texture2D texture, Rectangle source, Rectangle dest, Vector2 origin, float rotation, Color tint); // Draw a part of a texture defined by a rectangle with 'pro' parameters
(cffi:defcfun ("DrawTexturePro" %draw-texture-pro) :void
    (texture (:struct %Texture))
    (source (:struct %Rectangle))
    (dest (:struct %Rectangle))
    (origin (:struct %Vector2))
    (rotation :float)
    (tint (:struct %Color))
)

; void DrawTextureNPatch(Texture2D texture, NPatchInfo nPatchInfo, Rectangle dest, Vector2 origin, float rotation, Color tint); // Draws a texture (or part of it) that stretches or shrinks nicely
(cffi:defcfun ("DrawTextureNPatch" %draw-texture-n-patch) :void
    (texture (:struct %Texture))
    (nPatchInfo (:struct %NPatchInfo))
    (dest (:struct %Rectangle))
    (origin (:struct %Vector2))
    (rotation :float)
    (tint (:struct %Color))
)


; // Color/pixel related functions
; bool ColorIsEqual(Color col1, Color col2);                            // Check if two colors are equal
(cffi:defcfun ("ColorIsEqual" %color-is-equal) :bool
    (col1 (:struct %Color))
    (col2 (:struct %Color))
)

; Color Fade(Color color, float alpha);                                 // Get color with alpha applied, alpha goes from 0.0f to 1.0f
(cffi:defcfun ("Fade" %fade) (:struct %Color)
    (color (:struct %Color))
    (alpha :float)
)

; int ColorToInt(Color color);                                          // Get hexadecimal value for a Color (0xRRGGBBAA)
(cffi:defcfun ("ColorToInt" %color-to-int) :int
    (color (:struct %Color))
)

; Vector4 ColorNormalize(Color color);                                  // Get Color normalized as float [0..1]
(cffi:defcfun ("ColorNormalize" %color-normalize) (:struct %Vector4)
    (color (:struct %Color))
)

; Color ColorFromNormalized(Vector4 normalized);                        // Get Color from normalized values [0..1]
(cffi:defcfun ("ColorFromNormalized" %color-from-normalized) (:struct %Color)
    (normalized (:struct %Vector4))
)

; Vector3 ColorToHSV(Color color);                                      // Get HSV values for a Color, hue [0..360], saturation/value [0..1]
(cffi:defcfun ("ColorToHSV" %color-to-hsv) (:struct %Vector3)
    (color (:struct %Color))
)

; Color ColorFromHSV(float hue, float saturation, float value);         // Get a Color from HSV values, hue [0..360], saturation/value [0..1]
(cffi:defcfun ("ColorFromHSV" %color-from-hsv) (:struct %Color)
    (hue :float)
    (saturation :float)
    (value :float)
)

; Color ColorTint(Color color, Color tint);                             // Get color multiplied with another color
(cffi:defcfun ("ColorTint" %color-tint) (:struct %Color)
    (color (:struct %Color))
    (tint (:struct %Color))
)

; Color ColorBrightness(Color color, float factor);                     // Get color with brightness correction, brightness factor goes from -1.0f to 1.0f
(cffi:defcfun ("ColorBrightness" %color-brightness) (:struct %Color)
    (color (:struct %Color))
    (factor :float)
)

; Color ColorContrast(Color color, float contrast);                     // Get color with contrast correction, contrast values between -1.0f and 1.0f
(cffi:defcfun ("ColorContrast" %color-contrast) (:struct %Color)
    (color (:struct %Color))
    (contrast :float)
)

; Color ColorAlpha(Color color, float alpha);                           // Get color with alpha applied, alpha goes from 0.0f to 1.0f
(cffi:defcfun ("ColorAlpha" %color-alpha) (:struct %Color)
    (color (:struct %Color))
    (alpha :float)
)

; Color ColorAlphaBlend(Color dst, Color src, Color tint);              // Get src alpha-blended into dst color with tint
(cffi:defcfun ("ColorAlphaBlend" %color-alpha-blend) (:struct %Color)
    (dst (:struct %Color))
    (src (:struct %Color))
    (tint (:struct %Color))
)

; Color ColorLerp(Color color1, Color color2, float factor);            // Get color lerp interpolation between two colors, factor [0.0f..1.0f]
(cffi:defcfun ("ColorLerp" %color-lerp) (:struct %Color)
    (color1 (:struct %Color))
    (color2 (:struct %Color))
    (factor :float)
)

; Color GetColor(unsigned int hexValue);                                // Get Color structure from hexadecimal value
(cffi:defcfun ("GetColor" %get-color) (:struct %Color)
    (hexValue :unsigned-int)
)

; Color GetPixelColor(void *srcPtr, int format);                        // Get Color from a source pixel pointer of certain format
(cffi:defcfun ("GetPixelColor" %get-pixel-color) (:struct %Color)
    (srcPtr :pointer)
    (format :int)
)

; void SetPixelColor(void *dstPtr, Color color, int format);            // Set color formatted into destination pixel pointer
(cffi:defcfun ("SetPixelColor" %set-pixel-color) :void
    (dstPtr :pointer)
    (color (:struct %Color))
    (format :int)
)

; int GetPixelDataSize(int width, int height, int format);              // Get pixel data size in bytes for certain format
(cffi:defcfun ("GetPixelDataSize" %get-pixel-data-size) :int
    (width :int)
    (height :int)
    (format :int)
)


; module: rtext →

; // Font loading/unloading functions
; Font GetFontDefault(void);                                                            // Get the default Font
(cffi:defcfun ("GetFontDefault" %get-font-default) (:struct %Font)
)

; Font LoadFont(const char *fileName);                                                  // Load font from file into GPU memory (VRAM)
(cffi:defcfun ("LoadFont" %load-font) (:struct %Font)
    (fileName :string)
)

; Font LoadFontEx(const char *fileName, int fontSize, const int *codepoints, int codepointCount); // Load font from file with extended parameters, use NULL for codepoints and 0 for codepointCount to load the default character set, font size is provided in pixels height
(cffi:defcfun ("LoadFontEx" %load-font-ex) (:struct %Font)
    (fileName :string)
    (fontSize :int)
    (codepoints :pointer)
    (codepointCount :int)
)

; Font LoadFontFromImage(Image image, Color key, int firstChar);                        // Load font from Image (XNA style)
(cffi:defcfun ("LoadFontFromImage" %load-font-from-image) (:struct %Font)
    (image (:struct %Image))
    (key (:struct %Color))
    (firstChar :int)
)

; Font LoadFontFromMemory(const char *fileType, const unsigned char *fileData, int dataSize, int fontSize, const int *codepoints, int codepointCount); // Load font from memory buffer, fileType refers to extension: i.e. '.ttf'
(cffi:defcfun ("LoadFontFromMemory" %load-font-from-memory) (:struct %Font)
    (fileType :string)
    (fileData :pointer)
    (dataSize :int)
    (fontSize :int)
    (codepoints :pointer)
    (codepointCount :int)
)

; bool IsFontValid(Font font);                                                          // Check if a font is valid (font data loaded, WARNING: GPU texture not checked)
(cffi:defcfun ("IsFontValid" %is-font-valid) :bool
    (font (:struct %Font))
)

; GlyphInfo *LoadFontData(const unsigned char *fileData, int dataSize, int fontSize, const int *codepoints, int codepointCount, int type, int *glyphCount); // Load font data for further use
(cffi:defcfun ("LoadFontData" %load-font-data) :pointer
    (fileData :pointer)
    (dataSize :int)
    (fontSize :int)
    (codepoints :pointer)
    (codepointCount :int)
    (type :int)
    (glyphCount :pointer)
)

; Image GenImageFontAtlas(const GlyphInfo *glyphs, Rectangle **glyphRecs, int glyphCount, int fontSize, int padding, int packMethod); // Generate image font atlas using chars info
(cffi:defcfun ("GenImageFontAtlas" %gen-image-font-atlas) (:struct %Image)
    (glyphs :pointer)
    (glyphRecs :pointer)
    (glyphCount :int)
    (fontSize :int)
    (padding :int)
    (packMethod :int)
)

; void UnloadFontData(GlyphInfo *glyphs, int glyphCount);                               // Unload font chars info data (RAM)
(cffi:defcfun ("UnloadFontData" %unload-font-data) :void
    (glyphs :pointer)
    (glyphCount :int)
)

; void UnloadFont(Font font);                                                           // Unload font from GPU memory (VRAM)
(cffi:defcfun ("UnloadFont" %unload-font) :void
    (font (:struct %Font))
)

; bool ExportFontAsCode(Font font, const char *fileName);                               // Export font as code file, returns true on success
(cffi:defcfun ("ExportFontAsCode" %export-font-as-code) :bool
    (font (:struct %Font))
    (fileName :string)
)


; // Text drawing functions
; void DrawFPS(int posX, int posY);                                                     // Draw current FPS
(cffi:defcfun ("DrawFPS" %draw-fps) :void
    (posX :int)
    (posY :int)
)

; void DrawText(const char *text, int posX, int posY, int fontSize, Color color);       // Draw text (using default font)
(cffi:defcfun ("DrawText" %draw-text) :void
    (text :string)
    (posX :int)
    (posY :int)
    (fontSize :int)
    (color (:struct %Color))
)

; void DrawTextEx(Font font, const char *text, Vector2 position, float fontSize, float spacing, Color tint); // Draw text using font and additional parameters
(cffi:defcfun ("DrawTextEx" %draw-text-ex) :void
    (font (:struct %Font))
    (text :string)
    (position (:struct %Vector2))
    (fontSize :float)
    (spacing :float)
    (tint (:struct %Color))
)

; void DrawTextPro(Font font, const char *text, Vector2 position, Vector2 origin, float rotation, float fontSize, float spacing, Color tint); // Draw text using Font and pro parameters (rotation)
(cffi:defcfun ("DrawTextPro" %draw-text-pro) :void
    (font (:struct %Font))
    (text :string)
    (position (:struct %Vector2))
    (origin (:struct %Vector2))
    (rotation :float)
    (fontSize :float)
    (spacing :float)
    (tint (:struct %Color))
)

; void DrawTextCodepoint(Font font, int codepoint, Vector2 position, float fontSize, Color tint); // Draw one character (codepoint)
(cffi:defcfun ("DrawTextCodepoint" %draw-text-codepoint) :void
    (font (:struct %Font))
    (codepoint :int)
    (position (:struct %Vector2))
    (fontSize :float)
    (tint (:struct %Color))
)

; void DrawTextCodepoints(Font font, const int *codepoints, int codepointCount, Vector2 position, float fontSize, float spacing, Color tint); // Draw multiple character (codepoint)
(cffi:defcfun ("DrawTextCodepoints" %draw-text-codepoints) :void
    (font (:struct %Font))
    (codepoints :pointer)
    (codepointCount :int)
    (position (:struct %Vector2))
    (fontSize :float)
    (spacing :float)
    (tint (:struct %Color))
)


; // Text font info functions
; void SetTextLineSpacing(int spacing);                                                 // Set vertical line spacing when drawing with line-breaks
(cffi:defcfun ("SetTextLineSpacing" %set-text-line-spacing) :void
    (spacing :int)
)

; int MeasureText(const char *text, int fontSize);                                      // Measure string width for default font
(cffi:defcfun ("MeasureText" %measure-text) :int
    (text :string)
    (fontSize :int)
)

; Vector2 MeasureTextEx(Font font, const char *text, float fontSize, float spacing);    // Measure string size for Font
(cffi:defcfun ("MeasureTextEx" %measure-text-ex) (:struct %Vector2)
    (font (:struct %Font))
    (text :string)
    (fontSize :float)
    (spacing :float)
)

; Vector2 MeasureTextCodepoints(Font font, const int *codepoints, int length, float fontSize, float spacing); // Measure string size for an existing array of codepoints for Font
(cffi:defcfun ("MeasureTextCodepoints" %measure-text-codepoints) (:struct %Vector2)
    (font (:struct %Font))
    (codepoints :pointer)
    (length :int)
    (fontSize :float)
    (spacing :float)
)

; int GetGlyphIndex(Font font, int codepoint);                                          // Get glyph index position in font for a codepoint (unicode character), fallback to '?' if not found
(cffi:defcfun ("GetGlyphIndex" %get-glyph-index) :int
    (font (:struct %Font))
    (codepoint :int)
)

; GlyphInfo GetGlyphInfo(Font font, int codepoint);                                     // Get glyph font info data for a codepoint (unicode character), fallback to '?' if not found
(cffi:defcfun ("GetGlyphInfo" %get-glyph-info) (:struct %GlyphInfo)
    (font (:struct %Font))
    (codepoint :int)
)

; Rectangle GetGlyphAtlasRec(Font font, int codepoint);                                 // Get glyph rectangle in font atlas for a codepoint (unicode character), fallback to '?' if not found
(cffi:defcfun ("GetGlyphAtlasRec" %get-glyph-atlas-rec) (:struct %Rectangle)
    (font (:struct %Font))
    (codepoint :int)
)


; // Text codepoints management functions (unicode characters)
; char *LoadUTF8(const int *codepoints, int length);                                    // Load UTF-8 text encoded from codepoints array
(cffi:defcfun ("LoadUTF8" %load-utf8) :string
    (codepoints :pointer)
    (length :int)
)

; void UnloadUTF8(char *text);                                                          // Unload UTF-8 text encoded from codepoints array
(cffi:defcfun ("UnloadUTF8" %unload-utf8) :void
    (text :string)
)

; int *LoadCodepoints(const char *text, int *count);                                    // Load all codepoints from a UTF-8 text string, codepoints count returned by parameter
(cffi:defcfun ("LoadCodepoints" %load-codepoints) :pointer
    (text :string)
    (count :pointer)
)

; void UnloadCodepoints(int *codepoints);                                               // Unload codepoints data from memory
(cffi:defcfun ("UnloadCodepoints" %unload-codepoints) :void
    (codepoints :pointer)
)

; int GetCodepointCount(const char *text);                                              // Get total number of codepoints in a UTF-8 encoded string
(cffi:defcfun ("GetCodepointCount" %get-codepoint-count) :int
    (text :string)
)

; int GetCodepoint(const char *text, int *codepointSize);                               // Get next codepoint in a UTF-8 encoded string, 0x3f('?') is returned on failure
(cffi:defcfun ("GetCodepoint" %get-codepoint) :int
    (text :string)
    (codepointSize :pointer)
)

; int GetCodepointNext(const char *text, int *codepointSize);                           // Get next codepoint in a UTF-8 encoded string, 0x3f('?') is returned on failure
(cffi:defcfun ("GetCodepointNext" %get-codepoint-next) :int
    (text :string)
    (codepointSize :pointer)
)

; int GetCodepointPrevious(const char *text, int *codepointSize);                       // Get previous codepoint in a UTF-8 encoded string, 0x3f('?') is returned on failure
(cffi:defcfun ("GetCodepointPrevious" %get-codepoint-previous) :int
    (text :string)
    (codepointSize :pointer)
)

; const char *CodepointToUTF8(int codepoint, int *utf8Size);                            // Encode one codepoint into UTF-8 byte array (array length returned as parameter)
(cffi:defcfun ("CodepointToUTF8" %codepoint-to-utf8) :string
    (codepoint :int)
    (utf8Size :pointer)
)


; // Text strings management functions (no UTF-8 strings, only byte chars)
; // WARNING 1: Most of these functions use internal static buffers[], it's recommended to store returned data on user-side for re-use
; // WARNING 2: Some functions allocate memory internally for the returned strings, those strings must be freed by user using MemFree()
; char **LoadTextLines(const char *text, int *count);                                   // Load text as separate lines ('\n')
(cffi:defcfun ("LoadTextLines" %load-text-lines) :pointer
    (text :string)
    (count :pointer)
)

; void UnloadTextLines(char **text, int lineCount);                                     // Unload text lines
(cffi:defcfun ("UnloadTextLines" %unload-text-lines) :void
    (text :pointer)
    (lineCount :int)
)

; int TextCopy(char *dst, const char *src);                                             // Copy one string to another, returns bytes copied
(cffi:defcfun ("TextCopy" %text-copy) :int
    (dst :string)
    (src :string)
)

; bool TextIsEqual(const char *text1, const char *text2);                               // Check if two text string are equal
(cffi:defcfun ("TextIsEqual" %text-is-equal) :bool
    (text1 :string)
    (text2 :string)
)

; unsigned int TextLength(const char *text);                                            // Get text length, checks for '\0' ending
(cffi:defcfun ("TextLength" %text-length) :unsigned-int
    (text :string)
)

; const char *TextFormat(const char *text, ...);                                        // Text formatting with variables (sprintf() style)
(cffi:defcfun ("TextFormat" %text-format) :string
    (text :string)
    &rest)

; const char *TextSubtext(const char *text, int position, int length);                  // Get a piece of a text string
(cffi:defcfun ("TextSubtext" %text-subtext) :string
    (text :string)
    (position :int)
    (length :int)
)

; const char *TextRemoveSpaces(const char *text);                                       // Remove text spaces, concat words
(cffi:defcfun ("TextRemoveSpaces" %text-remove-spaces) :string
    (text :string)
)

; char *GetTextBetween(const char *text, const char *begin, const char *end);           // Get text between two strings
(cffi:defcfun ("GetTextBetween" %get-text-between) :string
    (text :string)
    (begin :string)
    (end :string)
)

; char *TextReplace(const char *text, const char *search, const char *replacement);     // Replace text string with new string
(cffi:defcfun ("TextReplace" %text-replace) :string
    (text :string)
    (search :string)
    (replacement :string)
)

; char *TextReplaceAlloc(const char *text, const char *search, const char *replacement); // Replace text string with new string, memory must be MemFree()
(cffi:defcfun ("TextReplaceAlloc" %text-replace-alloc) :string
    (text :string)
    (search :string)
    (replacement :string)
)

; char *TextReplaceBetween(const char *text, const char *begin, const char *end, const char *replacement); // Replace text between two specific strings
(cffi:defcfun ("TextReplaceBetween" %text-replace-between) :string
    (text :string)
    (begin :string)
    (end :string)
    (replacement :string)
)

; char *TextReplaceBetweenAlloc(const char *text, const char *begin, const char *end, const char *replacement); // Replace text between two specific strings, memory must be MemFree()
(cffi:defcfun ("TextReplaceBetweenAlloc" %text-replace-between-alloc) :string
    (text :string)
    (begin :string)
    (end :string)
    (replacement :string)
)

; char *TextInsert(const char *text, const char *insert, int position);                 // Insert text in a defined byte position
(cffi:defcfun ("TextInsert" %text-insert) :string
    (text :string)
    (insert :string)
    (position :int)
)

; char *TextInsertAlloc(const char *text, const char *insert, int position);            // Insert text in a defined byte position, memory must be MemFree()
(cffi:defcfun ("TextInsertAlloc" %text-insert-alloc) :string
    (text :string)
    (insert :string)
    (position :int)
)

; char *TextJoin(char **textList, int count, const char *delimiter);                    // Join text strings with delimiter
(cffi:defcfun ("TextJoin" %text-join) :string
    (textList :pointer)
    (count :int)
    (delimiter :string)
)

; char **TextSplit(const char *text, char delimiter, int *count);                       // Split text into multiple strings, using MAX_TEXTSPLIT_COUNT static strings
(cffi:defcfun ("TextSplit" %text-split) :pointer
    (text :string)
    (delimiter :pointer)
    (count :pointer)
)

; void TextAppend(char *text, const char *append, int *position);                       // Append text at specific position and move cursor
(cffi:defcfun ("TextAppend" %text-append) :void
    (text :string)
    (append :string)
    (position :pointer)
)

; int TextFindIndex(const char *text, const char *search);                              // Find first text occurrence within a string, -1 if not found
(cffi:defcfun ("TextFindIndex" %text-find-index) :int
    (text :string)
    (search :string)
)

; char *TextToUpper(const char *text);                                                  // Get upper case version of provided string
(cffi:defcfun ("TextToUpper" %text-to-upper) :string
    (text :string)
)

; char *TextToLower(const char *text);                                                  // Get lower case version of provided string
(cffi:defcfun ("TextToLower" %text-to-lower) :string
    (text :string)
)

; char *TextToPascal(const char *text);                                                 // Get Pascal case notation version of provided string
(cffi:defcfun ("TextToPascal" %text-to-pascal) :string
    (text :string)
)

; char *TextToSnake(const char *text);                                                  // Get Snake case notation version of provided string
(cffi:defcfun ("TextToSnake" %text-to-snake) :string
    (text :string)
)

; char *TextToCamel(const char *text);                                                  // Get Camel case notation version of provided string
(cffi:defcfun ("TextToCamel" %text-to-camel) :string
    (text :string)
)

; int TextToInteger(const char *text);                                                  // Get integer value from text
(cffi:defcfun ("TextToInteger" %text-to-integer) :int
    (text :string)
)

; float TextToFloat(const char *text);                                                  // Get float value from text
(cffi:defcfun ("TextToFloat" %text-to-float) :float
    (text :string)
)


; module: rmodels →

; // Basic geometric 3D shapes drawing functions
; void DrawLine3D(Vector3 startPos, Vector3 endPos, Color color);                                    // Draw a line in 3D world space
(cffi:defcfun ("DrawLine3D" %draw-line-3d) :void
    (startPos (:struct %Vector3))
    (endPos (:struct %Vector3))
    (color (:struct %Color))
)

; void DrawPoint3D(Vector3 position, Color color);                                                   // Draw a point in 3D space, actually a small line
(cffi:defcfun ("DrawPoint3D" %draw-point-3d) :void
    (position (:struct %Vector3))
    (color (:struct %Color))
)

; void DrawCircle3D(Vector3 center, float radius, Vector3 rotationAxis, float rotationAngle, Color color); // Draw a circle in 3D world space
(cffi:defcfun ("DrawCircle3D" %draw-circle-3d) :void
    (center (:struct %Vector3))
    (radius :float)
    (rotationAxis (:struct %Vector3))
    (rotationAngle :float)
    (color (:struct %Color))
)

; void DrawTriangle3D(Vector3 v1, Vector3 v2, Vector3 v3, Color color);                              // Draw a color-filled triangle (vertex in counter-clockwise order!)
(cffi:defcfun ("DrawTriangle3D" %draw-triangle-3d) :void
    (v1 (:struct %Vector3))
    (v2 (:struct %Vector3))
    (v3 (:struct %Vector3))
    (color (:struct %Color))
)

; void DrawTriangleStrip3D(const Vector3 *points, int pointCount, Color color);                      // Draw a triangle strip defined by points
(cffi:defcfun ("DrawTriangleStrip3D" %draw-triangle-strip-3d) :void
    (points :pointer)
    (pointCount :int)
    (color (:struct %Color))
)

; void DrawCube(Vector3 position, float width, float height, float length, Color color);             // Draw cube
(cffi:defcfun ("DrawCube" %draw-cube) :void
    (position (:struct %Vector3))
    (width :float)
    (height :float)
    (length :float)
    (color (:struct %Color))
)

; void DrawCubeV(Vector3 position, Vector3 size, Color color);                                       // Draw cube (Vector version)
(cffi:defcfun ("DrawCubeV" %draw-cube-v) :void
    (position (:struct %Vector3))
    (size (:struct %Vector3))
    (color (:struct %Color))
)

; void DrawCubeWires(Vector3 position, float width, float height, float length, Color color);        // Draw cube wires
(cffi:defcfun ("DrawCubeWires" %draw-cube-wires) :void
    (position (:struct %Vector3))
    (width :float)
    (height :float)
    (length :float)
    (color (:struct %Color))
)

; void DrawCubeWiresV(Vector3 position, Vector3 size, Color color);                                  // Draw cube wires (Vector version)
(cffi:defcfun ("DrawCubeWiresV" %draw-cube-wires-v) :void
    (position (:struct %Vector3))
    (size (:struct %Vector3))
    (color (:struct %Color))
)

; void DrawSphere(Vector3 centerPos, float radius, Color color);                                     // Draw sphere
(cffi:defcfun ("DrawSphere" %draw-sphere) :void
    (centerPos (:struct %Vector3))
    (radius :float)
    (color (:struct %Color))
)

; void DrawSphereEx(Vector3 centerPos, float radius, int rings, int slices, Color color);            // Draw sphere with extended parameters
(cffi:defcfun ("DrawSphereEx" %draw-sphere-ex) :void
    (centerPos (:struct %Vector3))
    (radius :float)
    (rings :int)
    (slices :int)
    (color (:struct %Color))
)

; void DrawSphereWires(Vector3 centerPos, float radius, int rings, int slices, Color color);         // Draw sphere wires
(cffi:defcfun ("DrawSphereWires" %draw-sphere-wires) :void
    (centerPos (:struct %Vector3))
    (radius :float)
    (rings :int)
    (slices :int)
    (color (:struct %Color))
)

; void DrawCylinder(Vector3 position, float radiusTop, float radiusBottom, float height, int slices, Color color); // Draw a cylinder/cone
(cffi:defcfun ("DrawCylinder" %draw-cylinder) :void
    (position (:struct %Vector3))
    (radiusTop :float)
    (radiusBottom :float)
    (height :float)
    (slices :int)
    (color (:struct %Color))
)

; void DrawCylinderEx(Vector3 startPos, Vector3 endPos, float startRadius, float endRadius, int sides, Color color); // Draw a cylinder with base at startPos and top at endPos
(cffi:defcfun ("DrawCylinderEx" %draw-cylinder-ex) :void
    (startPos (:struct %Vector3))
    (endPos (:struct %Vector3))
    (startRadius :float)
    (endRadius :float)
    (sides :int)
    (color (:struct %Color))
)

; void DrawCylinderWires(Vector3 position, float radiusTop, float radiusBottom, float height, int slices, Color color); // Draw a cylinder/cone wires
(cffi:defcfun ("DrawCylinderWires" %draw-cylinder-wires) :void
    (position (:struct %Vector3))
    (radiusTop :float)
    (radiusBottom :float)
    (height :float)
    (slices :int)
    (color (:struct %Color))
)

; void DrawCylinderWiresEx(Vector3 startPos, Vector3 endPos, float startRadius, float endRadius, int sides, Color color); // Draw a cylinder wires with base at startPos and top at endPos
(cffi:defcfun ("DrawCylinderWiresEx" %draw-cylinder-wires-ex) :void
    (startPos (:struct %Vector3))
    (endPos (:struct %Vector3))
    (startRadius :float)
    (endRadius :float)
    (sides :int)
    (color (:struct %Color))
)

; void DrawCapsule(Vector3 startPos, Vector3 endPos, float radius, int slices, int rings, Color color); // Draw a capsule with the center of its sphere caps at startPos and endPos
(cffi:defcfun ("DrawCapsule" %draw-capsule) :void
    (startPos (:struct %Vector3))
    (endPos (:struct %Vector3))
    (radius :float)
    (slices :int)
    (rings :int)
    (color (:struct %Color))
)

; void DrawCapsuleWires(Vector3 startPos, Vector3 endPos, float radius, int slices, int rings, Color color); // Draw capsule wireframe with the center of its sphere caps at startPos and endPos
(cffi:defcfun ("DrawCapsuleWires" %draw-capsule-wires) :void
    (startPos (:struct %Vector3))
    (endPos (:struct %Vector3))
    (radius :float)
    (slices :int)
    (rings :int)
    (color (:struct %Color))
)

; void DrawPlane(Vector3 centerPos, Vector2 size, Color color);                                      // Draw a plane XZ
(cffi:defcfun ("DrawPlane" %draw-plane) :void
    (centerPos (:struct %Vector3))
    (size (:struct %Vector2))
    (color (:struct %Color))
)

; void DrawRay(Ray ray, Color color);                                                                // Draw a ray line
(cffi:defcfun ("DrawRay" %draw-ray) :void
    (ray (:struct %Ray))
    (color (:struct %Color))
)

; void DrawGrid(int slices, float spacing);                                                          // Draw a grid (centered at (0, 0, 0))
(cffi:defcfun ("DrawGrid" %draw-grid) :void
    (slices :int)
    (spacing :float)
)


; //------------------------------------------------------------------------------------
; // Model 3d Loading and Drawing Functions (Module: models)
; //------------------------------------------------------------------------------------

; // Model management functions
; Model LoadModel(const char *fileName);                                                // Load model from files (meshes and materials)
(cffi:defcfun ("LoadModel" %load-model) (:struct %Model)
    (fileName :string)
)

; Model LoadModelFromMesh(Mesh mesh);                                                   // Load model from generated mesh (default material)
(cffi:defcfun ("LoadModelFromMesh" %load-model-from-mesh) (:struct %Model)
    (mesh (:struct %Mesh))
)

; bool IsModelValid(Model model);                                                       // Check if a model is valid (loaded in GPU, VAO/VBOs)
(cffi:defcfun ("IsModelValid" %is-model-valid) :bool
    (model (:struct %Model))
)

; void UnloadModel(Model model);                                                        // Unload model (including meshes) from memory (RAM and/or VRAM)
(cffi:defcfun ("UnloadModel" %unload-model) :void
    (model (:struct %Model))
)

; BoundingBox GetModelBoundingBox(Model model);                                         // Compute model bounding box limits (considers all meshes)
(cffi:defcfun ("GetModelBoundingBox" %get-model-bounding-box) (:struct %BoundingBox)
    (model (:struct %Model))
)


; // Model drawing functions
; void DrawModel(Model model, Vector3 position, float scale, Color tint);               // Draw a model (with texture if set)
(cffi:defcfun ("DrawModel" %draw-model) :void
    (model (:struct %Model))
    (position (:struct %Vector3))
    (scale :float)
    (tint (:struct %Color))
)

; void DrawModelEx(Model model, Vector3 position, Vector3 rotationAxis, float rotationAngle, Vector3 scale, Color tint); // Draw a model with extended parameters
(cffi:defcfun ("DrawModelEx" %draw-model-ex) :void
    (model (:struct %Model))
    (position (:struct %Vector3))
    (rotationAxis (:struct %Vector3))
    (rotationAngle :float)
    (scale (:struct %Vector3))
    (tint (:struct %Color))
)

; void DrawModelWires(Model model, Vector3 position, float scale, Color tint);          // Draw a model wires (with texture if set)
(cffi:defcfun ("DrawModelWires" %draw-model-wires) :void
    (model (:struct %Model))
    (position (:struct %Vector3))
    (scale :float)
    (tint (:struct %Color))
)

; void DrawModelWiresEx(Model model, Vector3 position, Vector3 rotationAxis, float rotationAngle, Vector3 scale, Color tint); // Draw a model wires (with texture if set) with extended parameters
(cffi:defcfun ("DrawModelWiresEx" %draw-model-wires-ex) :void
    (model (:struct %Model))
    (position (:struct %Vector3))
    (rotationAxis (:struct %Vector3))
    (rotationAngle :float)
    (scale (:struct %Vector3))
    (tint (:struct %Color))
)

; void DrawBoundingBox(BoundingBox box, Color color);                                   // Draw bounding box (wires)
(cffi:defcfun ("DrawBoundingBox" %draw-bounding-box) :void
    (box (:struct %BoundingBox))
    (color (:struct %Color))
)

; void DrawBillboard(Camera camera, Texture2D texture, Vector3 position, float scale, Color tint); // Draw a billboard texture
(cffi:defcfun ("DrawBillboard" %draw-billboard) :void
    (camera (:struct %Camera3D))
    (texture (:struct %Texture))
    (position (:struct %Vector3))
    (scale :float)
    (tint (:struct %Color))
)

; void DrawBillboardRec(Camera camera, Texture2D texture, Rectangle source, Vector3 position, Vector2 size, Color tint); // Draw a billboard texture defined by source
(cffi:defcfun ("DrawBillboardRec" %draw-billboard-rec) :void
    (camera (:struct %Camera3D))
    (texture (:struct %Texture))
    (source (:struct %Rectangle))
    (position (:struct %Vector3))
    (size (:struct %Vector2))
    (tint (:struct %Color))
)

; void DrawBillboardPro(Camera camera, Texture2D texture, Rectangle source, Vector3 position, Vector3 up, Vector2 size, Vector2 origin, float rotation, Color tint); // Draw a billboard texture defined by source and rotation
(cffi:defcfun ("DrawBillboardPro" %draw-billboard-pro) :void
    (camera (:struct %Camera3D))
    (texture (:struct %Texture))
    (source (:struct %Rectangle))
    (position (:struct %Vector3))
    (up (:struct %Vector3))
    (size (:struct %Vector2))
    (origin (:struct %Vector2))
    (rotation :float)
    (tint (:struct %Color))
)


; // Mesh management functions
; void UploadMesh(Mesh *mesh, bool dynamic);                                            // Upload mesh vertex data in GPU and provide VAO/VBO ids
(cffi:defcfun ("UploadMesh" %upload-mesh) :void
    (mesh :pointer)
    (dynamic :bool)
)

; void UpdateMeshBuffer(Mesh mesh, int index, const void *data, int dataSize, int offset); // Update mesh vertex data in GPU for a specific buffer index
(cffi:defcfun ("UpdateMeshBuffer" %update-mesh-buffer) :void
    (mesh (:struct %Mesh))
    (index :int)
    (data :pointer)
    (dataSize :int)
    (offset :int)
)

; void UnloadMesh(Mesh mesh);                                                           // Unload mesh data from CPU and GPU
(cffi:defcfun ("UnloadMesh" %unload-mesh) :void
    (mesh (:struct %Mesh))
)

; void DrawMesh(Mesh mesh, Material material, Matrix transform);                        // Draw a 3d mesh with material and transform
(cffi:defcfun ("DrawMesh" %draw-mesh) :void
    (mesh (:struct %Mesh))
    (material (:struct %Material))
    (transform (:struct %Matrix))
)

; void DrawMeshInstanced(Mesh mesh, Material material, const Matrix *transforms, int instances); // Draw multiple mesh instances with material and different transforms
(cffi:defcfun ("DrawMeshInstanced" %draw-mesh-instanced) :void
    (mesh (:struct %Mesh))
    (material (:struct %Material))
    (transforms :pointer)
    (instances :int)
)

; BoundingBox GetMeshBoundingBox(Mesh mesh);                                            // Compute mesh bounding box limits
(cffi:defcfun ("GetMeshBoundingBox" %get-mesh-bounding-box) (:struct %BoundingBox)
    (mesh (:struct %Mesh))
)

; void GenMeshTangents(Mesh *mesh);                                                     // Compute mesh tangents
(cffi:defcfun ("GenMeshTangents" %gen-mesh-tangents) :void
    (mesh :pointer)
)

; bool ExportMesh(Mesh mesh, const char *fileName);                                     // Export mesh data to file, returns true on success
(cffi:defcfun ("ExportMesh" %export-mesh) :bool
    (mesh (:struct %Mesh))
    (fileName :string)
)

; bool ExportMeshAsCode(Mesh mesh, const char *fileName);                               // Export mesh as code file (.h) defining multiple arrays of vertex attributes
(cffi:defcfun ("ExportMeshAsCode" %export-mesh-as-code) :bool
    (mesh (:struct %Mesh))
    (fileName :string)
)


; // Mesh generation functions
; Mesh GenMeshPoly(int sides, float radius);                                            // Generate polygonal mesh
(cffi:defcfun ("GenMeshPoly" %gen-mesh-poly) (:struct %Mesh)
    (sides :int)
    (radius :float)
)

; Mesh GenMeshPlane(float width, float length, int resX, int resZ);                     // Generate plane mesh (with subdivisions)
(cffi:defcfun ("GenMeshPlane" %gen-mesh-plane) (:struct %Mesh)
    (width :float)
    (length :float)
    (resX :int)
    (resZ :int)
)

; Mesh GenMeshCube(float width, float height, float length);                            // Generate cuboid mesh
(cffi:defcfun ("GenMeshCube" %gen-mesh-cube) (:struct %Mesh)
    (width :float)
    (height :float)
    (length :float)
)

; Mesh GenMeshSphere(float radius, int rings, int slices);                              // Generate sphere mesh (standard sphere)
(cffi:defcfun ("GenMeshSphere" %gen-mesh-sphere) (:struct %Mesh)
    (radius :float)
    (rings :int)
    (slices :int)
)

; Mesh GenMeshHemiSphere(float radius, int rings, int slices);                          // Generate half-sphere mesh (no bottom cap)
(cffi:defcfun ("GenMeshHemiSphere" %gen-mesh-hemi-sphere) (:struct %Mesh)
    (radius :float)
    (rings :int)
    (slices :int)
)

; Mesh GenMeshCylinder(float radius, float height, int slices);                         // Generate cylinder mesh
(cffi:defcfun ("GenMeshCylinder" %gen-mesh-cylinder) (:struct %Mesh)
    (radius :float)
    (height :float)
    (slices :int)
)

; Mesh GenMeshCone(float radius, float height, int slices);                             // Generate cone/pyramid mesh
(cffi:defcfun ("GenMeshCone" %gen-mesh-cone) (:struct %Mesh)
    (radius :float)
    (height :float)
    (slices :int)
)

; Mesh GenMeshTorus(float radius, float size, int radSeg, int sides);                   // Generate torus mesh
(cffi:defcfun ("GenMeshTorus" %gen-mesh-torus) (:struct %Mesh)
    (radius :float)
    (size :float)
    (radSeg :int)
    (sides :int)
)

; Mesh GenMeshKnot(float radius, float size, int radSeg, int sides);                    // Generate trefoil knot mesh
(cffi:defcfun ("GenMeshKnot" %gen-mesh-knot) (:struct %Mesh)
    (radius :float)
    (size :float)
    (radSeg :int)
    (sides :int)
)

; Mesh GenMeshHeightmap(Image heightmap, Vector3 size);                                 // Generate heightmap mesh from image data
(cffi:defcfun ("GenMeshHeightmap" %gen-mesh-heightmap) (:struct %Mesh)
    (heightmap (:struct %Image))
    (size (:struct %Vector3))
)

; Mesh GenMeshCubicmap(Image cubicmap, Vector3 cubeSize);                               // Generate cubes-based map mesh from image data
(cffi:defcfun ("GenMeshCubicmap" %gen-mesh-cubicmap) (:struct %Mesh)
    (cubicmap (:struct %Image))
    (cubeSize (:struct %Vector3))
)


; // Material loading/unloading functions
; Material *LoadMaterials(const char *fileName, int *materialCount);                    // Load materials from model file
(cffi:defcfun ("LoadMaterials" %load-materials) :pointer
    (fileName :string)
    (materialCount :pointer)
)

; Material LoadMaterialDefault(void);                                                   // Load default material (Supports: DIFFUSE, SPECULAR, NORMAL maps)
(cffi:defcfun ("LoadMaterialDefault" %load-material-default) (:struct %Material)
)

; bool IsMaterialValid(Material material);                                              // Check if a material is valid (shader assigned, map textures loaded in GPU)
(cffi:defcfun ("IsMaterialValid" %is-material-valid) :bool
    (material (:struct %Material))
)

; void UnloadMaterial(Material material);                                               // Unload material from GPU memory (VRAM)
(cffi:defcfun ("UnloadMaterial" %unload-material) :void
    (material (:struct %Material))
)

; void SetMaterialTexture(Material *material, int mapType, Texture2D texture);          // Set texture for a material map type (MATERIAL_MAP_DIFFUSE, MATERIAL_MAP_SPECULAR...)
(cffi:defcfun ("SetMaterialTexture" %set-material-texture) :void
    (material :pointer)
    (mapType :int)
    (texture (:struct %Texture))
)

; void SetModelMeshMaterial(Model *model, int meshId, int materialId);                  // Set material for a mesh
(cffi:defcfun ("SetModelMeshMaterial" %set-model-mesh-material) :void
    (model :pointer)
    (meshId :int)
    (materialId :int)
)


; // Model animations loading/unloading functions
; ModelAnimation *LoadModelAnimations(const char *fileName, int *animCount);            // Load model animations from file
(cffi:defcfun ("LoadModelAnimations" %load-model-animations) :pointer
    (fileName :string)
    (animCount :pointer)
)

; void UpdateModelAnimation(Model model, ModelAnimation anim, float frame);             // Update model animation pose (vertex buffers and bone matrices)
(cffi:defcfun ("UpdateModelAnimation" %update-model-animation) :void
    (model (:struct %Model))
    (anim (:struct %ModelAnimation))
    (frame :float)
)

; void UpdateModelAnimationEx(Model model, ModelAnimation animA, float frameA, ModelAnimation animB, float frameB, float blend); // Update model animation pose, blending two animations
(cffi:defcfun ("UpdateModelAnimationEx" %update-model-animation-ex) :void
    (model (:struct %Model))
    (animA (:struct %ModelAnimation))
    (frameA :float)
    (animB (:struct %ModelAnimation))
    (frameB :float)
    (blend :float)
)

; void UnloadModelAnimations(ModelAnimation *animations, int animCount);                // Unload animation array data
(cffi:defcfun ("UnloadModelAnimations" %unload-model-animations) :void
    (animations :pointer)
    (animCount :int)
)

; bool IsModelAnimationValid(Model model, ModelAnimation anim);                         // Check model animation skeleton match
(cffi:defcfun ("IsModelAnimationValid" %is-model-animation-valid) :bool
    (model (:struct %Model))
    (anim (:struct %ModelAnimation))
)


; // Collision detection functions
; bool CheckCollisionSpheres(Vector3 center1, float radius1, Vector3 center2, float radius2); // Check collision between two spheres
(cffi:defcfun ("CheckCollisionSpheres" %check-collision-spheres) :bool
    (center1 (:struct %Vector3))
    (radius1 :float)
    (center2 (:struct %Vector3))
    (radius2 :float)
)

; bool CheckCollisionBoxes(BoundingBox box1, BoundingBox box2);                         // Check collision between two bounding boxes
(cffi:defcfun ("CheckCollisionBoxes" %check-collision-boxes) :bool
    (box1 (:struct %BoundingBox))
    (box2 (:struct %BoundingBox))
)

; bool CheckCollisionBoxSphere(BoundingBox box, Vector3 center, float radius);          // Check collision between box and sphere
(cffi:defcfun ("CheckCollisionBoxSphere" %check-collision-box-sphere) :bool
    (box (:struct %BoundingBox))
    (center (:struct %Vector3))
    (radius :float)
)

; RayCollision GetRayCollisionSphere(Ray ray, Vector3 center, float radius);            // Get collision info between ray and sphere
(cffi:defcfun ("GetRayCollisionSphere" %get-ray-collision-sphere) (:struct %RayCollision)
    (ray (:struct %Ray))
    (center (:struct %Vector3))
    (radius :float)
)

; RayCollision GetRayCollisionBox(Ray ray, BoundingBox box);                            // Get collision info between ray and box
(cffi:defcfun ("GetRayCollisionBox" %get-ray-collision-box) (:struct %RayCollision)
    (ray (:struct %Ray))
    (box (:struct %BoundingBox))
)

; RayCollision GetRayCollisionMesh(Ray ray, Mesh mesh, Matrix transform);               // Get collision info between ray and mesh
(cffi:defcfun ("GetRayCollisionMesh" %get-ray-collision-mesh) (:struct %RayCollision)
    (ray (:struct %Ray))
    (mesh (:struct %Mesh))
    (transform (:struct %Matrix))
)

; RayCollision GetRayCollisionTriangle(Ray ray, Vector3 p1, Vector3 p2, Vector3 p3);    // Get collision info between ray and triangle
(cffi:defcfun ("GetRayCollisionTriangle" %get-ray-collision-triangle) (:struct %RayCollision)
    (ray (:struct %Ray))
    (p1 (:struct %Vector3))
    (p2 (:struct %Vector3))
    (p3 (:struct %Vector3))
)

; RayCollision GetRayCollisionQuad(Ray ray, Vector3 p1, Vector3 p2, Vector3 p3, Vector3 p4); // Get collision info between ray and quad
(cffi:defcfun ("GetRayCollisionQuad" %get-ray-collision-quad) (:struct %RayCollision)
    (ray (:struct %Ray))
    (p1 (:struct %Vector3))
    (p2 (:struct %Vector3))
    (p3 (:struct %Vector3))
    (p4 (:struct %Vector3))
)


; module: raudio →

; // Audio device management functions
; void InitAudioDevice(void);                                     // Initialize audio device and context
(cffi:defcfun ("InitAudioDevice" %init-audio-device) :void
)

; void CloseAudioDevice(void);                                    // Close the audio device and context
(cffi:defcfun ("CloseAudioDevice" %close-audio-device) :void
)

; bool IsAudioDeviceReady(void);                                  // Check if audio device has been initialized successfully
(cffi:defcfun ("IsAudioDeviceReady" %is-audio-device-ready) :bool
)

; void SetMasterVolume(float volume);                             // Set master volume (listener)
(cffi:defcfun ("SetMasterVolume" %set-master-volume) :void
    (volume :float)
)

; float GetMasterVolume(void);                                    // Get master volume (listener)
(cffi:defcfun ("GetMasterVolume" %get-master-volume) :float
)


; // Wave/Sound loading/unloading functions
; Wave LoadWave(const char *fileName);                            // Load wave data from file
(cffi:defcfun ("LoadWave" %load-wave) (:struct %Wave)
    (fileName :string)
)

; Wave LoadWaveFromMemory(const char *fileType, const unsigned char *fileData, int dataSize); // Load wave from memory buffer, fileType refers to extension: i.e. '.wav'
(cffi:defcfun ("LoadWaveFromMemory" %load-wave-from-memory) (:struct %Wave)
    (fileType :string)
    (fileData :pointer)
    (dataSize :int)
)

; bool IsWaveValid(Wave wave);                                    // Checks if wave data is valid (data loaded and parameters)
(cffi:defcfun ("IsWaveValid" %is-wave-valid) :bool
    (wave (:struct %Wave))
)

; Sound LoadSound(const char *fileName);                          // Load sound from file
(cffi:defcfun ("LoadSound" %load-sound) (:struct %Sound)
    (fileName :string)
)

; Sound LoadSoundFromWave(Wave wave);                             // Load sound from wave data
(cffi:defcfun ("LoadSoundFromWave" %load-sound-from-wave) (:struct %Sound)
    (wave (:struct %Wave))
)

; Sound LoadSoundAlias(Sound source);                             // Create a new sound that shares the same sample data as the source sound, does not own the sound data
(cffi:defcfun ("LoadSoundAlias" %load-sound-alias) (:struct %Sound)
    (source (:struct %Sound))
)

; bool IsSoundValid(Sound sound);                                 // Checks if a sound is valid (data loaded and buffers initialized)
(cffi:defcfun ("IsSoundValid" %is-sound-valid) :bool
    (sound (:struct %Sound))
)

; void UpdateSound(Sound sound, const void *data, int sampleCount); // Update sound buffer with new data (default data format: 32 bit float, stereo)
(cffi:defcfun ("UpdateSound" %update-sound) :void
    (sound (:struct %Sound))
    (data :pointer)
    (sampleCount :int)
)

; void UnloadWave(Wave wave);                                     // Unload wave data
(cffi:defcfun ("UnloadWave" %unload-wave) :void
    (wave (:struct %Wave))
)

; void UnloadSound(Sound sound);                                  // Unload sound
(cffi:defcfun ("UnloadSound" %unload-sound) :void
    (sound (:struct %Sound))
)

; void UnloadSoundAlias(Sound alias);                             // Unload a sound alias (does not deallocate sample data)
(cffi:defcfun ("UnloadSoundAlias" %unload-sound-alias) :void
    (alias (:struct %Sound))
)

; bool ExportWave(Wave wave, const char *fileName);               // Export wave data to file, returns true on success
(cffi:defcfun ("ExportWave" %export-wave) :bool
    (wave (:struct %Wave))
    (fileName :string)
)

; bool ExportWaveAsCode(Wave wave, const char *fileName);         // Export wave sample data to code (.h), returns true on success
(cffi:defcfun ("ExportWaveAsCode" %export-wave-as-code) :bool
    (wave (:struct %Wave))
    (fileName :string)
)


; // Wave/Sound management functions
; void PlaySound(Sound sound);                                    // Play a sound
(cffi:defcfun ("PlaySound" %play-sound) :void
    (sound (:struct %Sound))
)

; void StopSound(Sound sound);                                    // Stop playing a sound
(cffi:defcfun ("StopSound" %stop-sound) :void
    (sound (:struct %Sound))
)

; void PauseSound(Sound sound);                                   // Pause a sound
(cffi:defcfun ("PauseSound" %pause-sound) :void
    (sound (:struct %Sound))
)

; void ResumeSound(Sound sound);                                  // Resume a paused sound
(cffi:defcfun ("ResumeSound" %resume-sound) :void
    (sound (:struct %Sound))
)

; bool IsSoundPlaying(Sound sound);                               // Check if a sound is currently playing
(cffi:defcfun ("IsSoundPlaying" %is-sound-playing) :bool
    (sound (:struct %Sound))
)

; void SetSoundVolume(Sound sound, float volume);                 // Set volume for a sound (1.0 is max level)
(cffi:defcfun ("SetSoundVolume" %set-sound-volume) :void
    (sound (:struct %Sound))
    (volume :float)
)

; void SetSoundPitch(Sound sound, float pitch);                   // Set pitch for a sound (1.0 is base level)
(cffi:defcfun ("SetSoundPitch" %set-sound-pitch) :void
    (sound (:struct %Sound))
    (pitch :float)
)

; void SetSoundPan(Sound sound, float pan);                       // Set pan for a sound (-1.0 left, 0.0 center, 1.0 right)
(cffi:defcfun ("SetSoundPan" %set-sound-pan) :void
    (sound (:struct %Sound))
    (pan :float)
)

; Wave WaveCopy(Wave wave);                                       // Copy a wave to a new wave
(cffi:defcfun ("WaveCopy" %wave-copy) (:struct %Wave)
    (wave (:struct %Wave))
)

; void WaveCrop(Wave *wave, int initFrame, int finalFrame);       // Crop a wave to defined frames range
(cffi:defcfun ("WaveCrop" %wave-crop) :void
    (wave :pointer)
    (initFrame :int)
    (finalFrame :int)
)

; void WaveFormat(Wave *wave, int sampleRate, int sampleSize, int channels); // Convert wave data to desired format
(cffi:defcfun ("WaveFormat" %wave-format) :void
    (wave :pointer)
    (sampleRate :int)
    (sampleSize :int)
    (channels :int)
)

; float *LoadWaveSamples(Wave wave);                              // Load samples data from wave as a 32bit float data array
(cffi:defcfun ("LoadWaveSamples" %load-wave-samples) :pointer
    (wave (:struct %Wave))
)

; void UnloadWaveSamples(float *samples);                         // Unload samples data loaded with LoadWaveSamples()
(cffi:defcfun ("UnloadWaveSamples" %unload-wave-samples) :void
    (samples :pointer)
)


; // Music management functions
; Music LoadMusicStream(const char *fileName);                    // Load music stream from file
(cffi:defcfun ("LoadMusicStream" %load-music-stream) (:struct %Music)
    (fileName :string)
)

; Music LoadMusicStreamFromMemory(const char *fileType, const unsigned char *data, int dataSize); // Load music stream from data
(cffi:defcfun ("LoadMusicStreamFromMemory" %load-music-stream-from-memory) (:struct %Music)
    (fileType :string)
    (data :pointer)
    (dataSize :int)
)

; bool IsMusicValid(Music music);                                 // Checks if a music stream is valid (context and buffers initialized)
(cffi:defcfun ("IsMusicValid" %is-music-valid) :bool
    (music (:struct %Music))
)

; void UnloadMusicStream(Music music);                            // Unload music stream
(cffi:defcfun ("UnloadMusicStream" %unload-music-stream) :void
    (music (:struct %Music))
)

; void PlayMusicStream(Music music);                              // Start music playing
(cffi:defcfun ("PlayMusicStream" %play-music-stream) :void
    (music (:struct %Music))
)

; bool IsMusicStreamPlaying(Music music);                         // Check if music is playing
(cffi:defcfun ("IsMusicStreamPlaying" %is-music-stream-playing) :bool
    (music (:struct %Music))
)

; void UpdateMusicStream(Music music);                            // Updates buffers for music streaming
(cffi:defcfun ("UpdateMusicStream" %update-music-stream) :void
    (music (:struct %Music))
)

; void StopMusicStream(Music music);                              // Stop music playing
(cffi:defcfun ("StopMusicStream" %stop-music-stream) :void
    (music (:struct %Music))
)

; void PauseMusicStream(Music music);                             // Pause music playing
(cffi:defcfun ("PauseMusicStream" %pause-music-stream) :void
    (music (:struct %Music))
)

; void ResumeMusicStream(Music music);                            // Resume playing paused music
(cffi:defcfun ("ResumeMusicStream" %resume-music-stream) :void
    (music (:struct %Music))
)

; void SeekMusicStream(Music music, float position);              // Seek music to a position (in seconds)
(cffi:defcfun ("SeekMusicStream" %seek-music-stream) :void
    (music (:struct %Music))
    (position :float)
)

; void SetMusicVolume(Music music, float volume);                 // Set volume for music (1.0 is max level)
(cffi:defcfun ("SetMusicVolume" %set-music-volume) :void
    (music (:struct %Music))
    (volume :float)
)

; void SetMusicPitch(Music music, float pitch);                   // Set pitch for a music (1.0 is base level)
(cffi:defcfun ("SetMusicPitch" %set-music-pitch) :void
    (music (:struct %Music))
    (pitch :float)
)

; void SetMusicPan(Music music, float pan);                       // Set pan for a music (-1.0 left, 0.0 center, 1.0 right)
(cffi:defcfun ("SetMusicPan" %set-music-pan) :void
    (music (:struct %Music))
    (pan :float)
)

; float GetMusicTimeLength(Music music);                          // Get music time length (in seconds)
(cffi:defcfun ("GetMusicTimeLength" %get-music-time-length) :float
    (music (:struct %Music))
)

; float GetMusicTimePlayed(Music music);                          // Get current music time played (in seconds)
(cffi:defcfun ("GetMusicTimePlayed" %get-music-time-played) :float
    (music (:struct %Music))
)


; // AudioStream management functions
; AudioStream LoadAudioStream(unsigned int sampleRate, unsigned int sampleSize, unsigned int channels); // Load audio stream (to stream raw audio pcm data)
(cffi:defcfun ("LoadAudioStream" %load-audio-stream) (:struct %AudioStream)
    (sampleRate :unsigned-int)
    (sampleSize :unsigned-int)
    (channels :unsigned-int)
)

; bool IsAudioStreamValid(AudioStream stream);                    // Checks if an audio stream is valid (buffers initialized)
(cffi:defcfun ("IsAudioStreamValid" %is-audio-stream-valid) :bool
    (stream (:struct %AudioStream))
)

; void UnloadAudioStream(AudioStream stream);                     // Unload audio stream and free memory
(cffi:defcfun ("UnloadAudioStream" %unload-audio-stream) :void
    (stream (:struct %AudioStream))
)

; void UpdateAudioStream(AudioStream stream, const void *data, int frameCount); // Update audio stream buffers with data
(cffi:defcfun ("UpdateAudioStream" %update-audio-stream) :void
    (stream (:struct %AudioStream))
    (data :pointer)
    (frameCount :int)
)

; bool IsAudioStreamProcessed(AudioStream stream);                // Check if any audio stream buffers requires refill
(cffi:defcfun ("IsAudioStreamProcessed" %is-audio-stream-processed) :bool
    (stream (:struct %AudioStream))
)

; void PlayAudioStream(AudioStream stream);                       // Play audio stream
(cffi:defcfun ("PlayAudioStream" %play-audio-stream) :void
    (stream (:struct %AudioStream))
)

; void PauseAudioStream(AudioStream stream);                      // Pause audio stream
(cffi:defcfun ("PauseAudioStream" %pause-audio-stream) :void
    (stream (:struct %AudioStream))
)

; void ResumeAudioStream(AudioStream stream);                     // Resume audio stream
(cffi:defcfun ("ResumeAudioStream" %resume-audio-stream) :void
    (stream (:struct %AudioStream))
)

; bool IsAudioStreamPlaying(AudioStream stream);                  // Check if audio stream is playing
(cffi:defcfun ("IsAudioStreamPlaying" %is-audio-stream-playing) :bool
    (stream (:struct %AudioStream))
)

; void StopAudioStream(AudioStream stream);                       // Stop audio stream
(cffi:defcfun ("StopAudioStream" %stop-audio-stream) :void
    (stream (:struct %AudioStream))
)

; void SetAudioStreamVolume(AudioStream stream, float volume);    // Set volume for audio stream (1.0 is max level)
(cffi:defcfun ("SetAudioStreamVolume" %set-audio-stream-volume) :void
    (stream (:struct %AudioStream))
    (volume :float)
)

; void SetAudioStreamPitch(AudioStream stream, float pitch);      // Set pitch for audio stream (1.0 is base level)
(cffi:defcfun ("SetAudioStreamPitch" %set-audio-stream-pitch) :void
    (stream (:struct %AudioStream))
    (pitch :float)
)

; void SetAudioStreamPan(AudioStream stream, float pan);          // Set pan for audio stream (-1.0 to 1.0 range, 0.0 is centered)
(cffi:defcfun ("SetAudioStreamPan" %set-audio-stream-pan) :void
    (stream (:struct %AudioStream))
    (pan :float)
)

; void SetAudioStreamBufferSizeDefault(int size);                 // Default size for new audio streams
(cffi:defcfun ("SetAudioStreamBufferSizeDefault" %set-audio-stream-buffer-size-default) :void
    (size :int)
)

; void SetAudioStreamCallback(AudioStream stream, AudioCallback callback); // Audio thread callback to request new data
(cffi:defcfun ("SetAudioStreamCallback" %set-audio-stream-callback) :void
    (stream (:struct %AudioStream))
    (callback :pointer)
)


; void AttachAudioStreamProcessor(AudioStream stream, AudioCallback processor); // Attach audio stream processor to stream, receives frames x 2 samples as 'float' (stereo)
(cffi:defcfun ("AttachAudioStreamProcessor" %attach-audio-stream-processor) :void
    (stream (:struct %AudioStream))
    (processor :pointer)
)

; void DetachAudioStreamProcessor(AudioStream stream, AudioCallback processor); // Detach audio stream processor from stream
(cffi:defcfun ("DetachAudioStreamProcessor" %detach-audio-stream-processor) :void
    (stream (:struct %AudioStream))
    (processor :pointer)
)


; void AttachAudioMixedProcessor(AudioCallback processor); // Attach audio stream processor to the entire audio pipeline, receives frames x 2 samples as 'float' (stereo)
(cffi:defcfun ("AttachAudioMixedProcessor" %attach-audio-mixed-processor) :void
    (processor :pointer)
)

; void DetachAudioMixedProcessor(AudioCallback processor); // Detach audio stream processor from the entire audio pipeline
(cffi:defcfun ("DetachAudioMixedProcessor" %detach-audio-mixed-processor) :void
    (processor :pointer)
)

;;;; TRANSFORMED DRAWING HELPERS

(defun transform-point (x y)
  "Transform a 2D point by *draw-transform*. Returns two rounded integer values."
  (with-members (m0 m4 m12 m1 m5 m13) *draw-transform* matrix
    (values
      (round (+ (* m0 x) (* m4 y) m12))
      (round (+ (* m1 x) (* m5 y) m13)))))

(defun transform-point-2 (v)
  "Transform a Vector2 by *draw-transform*."
  (with-members (m0 m4 m12 m1 m5 m13) *draw-transform* matrix
    (make-vector2
      :x (+ (* m0 (vector2-x v)) (* m4 (vector2-y v)) m12)
      :y (+ (* m1 (vector2-x v)) (* m5 (vector2-y v)) m13))))

(defun transform-point-3 (v)
  "Transform a Vector3 by *draw-transform* (treated as w = 1)."
  (with-members (m0 m4 m8 m12 m1 m5 m9 m13 m2 m6 m10 m14) *draw-transform* matrix
    (make-vector3
      :x (+ (* m0 (vector3-x v)) (* m4 (vector3-y v)) (* m8 (vector3-z v)) m12)
      :y (+ (* m1 (vector3-x v)) (* m5 (vector3-y v)) (* m9 (vector3-z v)) m13)
      :z (+ (* m2 (vector3-x v)) (* m6 (vector3-y v)) (* m10 (vector3-z v)) m14))))

(defun transform-point-4 (v)
  "Transform a Vector4 by *draw-transform*."
  (with-members (m0 m4 m8 m12 m1 m5 m9 m13 m2 m6 m10 m14 m3 m7 m11 m15) *draw-transform* matrix
    (make-vector4
      :x (+ (* m0 (vector4-x v)) (* m4 (vector4-y v)) (* m8 (vector4-z v)) (* m12 (vector4-w v)))
      :y (+ (* m1 (vector4-x v)) (* m5 (vector4-y v)) (* m9 (vector4-z v)) (* m13 (vector4-w v)))
      :z (+ (* m2 (vector4-x v)) (* m6 (vector4-y v)) (* m10 (vector4-z v)) (* m14 (vector4-w v)))
      :w (+ (* m3 (vector4-x v)) (* m7 (vector4-y v)) (* m11 (vector4-z v)) (* m15 (vector4-w v))))))

(defmacro defdraw (name arg-spec &body body)
  "Define a drawing wrapper that applies *draw-transform* to annotated args.
Annotations:
  position-x / position-y  : scalar 2D point coordinates
  position-2               : Vector2
  position-3               : Vector3
  position-4               : Vector4
Unannotated args pass through unchanged."
  (labels ((normalize (annotation)
             (if (keywordp annotation)
                 annotation
                 (intern (symbol-name annotation) :keyword)))
           (parse (spec)
             (loop with pending-x = nil
                   with result = nil
                   for item in spec
                   do (cond ((atom item)
                             (push `(:plain ,item) result))
                            ((and (consp item) (= (length item) 2))
                             (destructuring-bind (name annotation) item
                               (case (normalize annotation)
                                 ((:position-x) (setf pending-x name))
                                 ((:position-y)
                                  (if pending-x
                                      (progn (push `(:point ,pending-x ,name) result)
                                             (setf pending-x nil))
                                      (error "Unpaired :position-y in defdraw: ~a" item)))
                                 ((:position-2) (push `(:vector2 ,name) result))
                                 ((:position-3) (push `(:vector3 ,name) result))
                                 ((:position-4) (push `(:vector4 ,name) result))
                                 (t (error "Unknown annotation ~a in defdraw" annotation)))))
                            (t (error "Malformed arg spec in defdraw: ~a" item)))
                   finally (when pending-x
                             (error "Unpaired :position-x in defdraw: ~a" pending-x))
                   finally (return (nreverse result))))
           (build (groups body)
             (if (null groups)
                 `(progn ,@body)
                 (let ((group (first groups)))
                   (ecase (first group)
                     (:plain (build (rest groups) body))
                     (:point (let ((x (second group)) (y (third group)))
                               `(multiple-value-bind (,x ,y) (transform-point ,x ,y)
                                  ,(build (rest groups) body))))
                     (:vector2 (let ((name (second group)))
                                 `(let ((,name (transform-point-2 ,name)))
                                    ,(build (rest groups) body))))
                     (:vector3 (let ((name (second group)))
                                 `(let ((,name (transform-point-3 ,name)))
                                    ,(build (rest groups) body))))
                     (:vector4 (let ((name (second group)))
                                 `(let ((,name (transform-point-4 ,name)))
                                    ,(build (rest groups) body)))))))))
    (let ((raw-names (mapcar (lambda (a) (if (atom a) a (first a))) arg-spec)))
      `(defun ,name ,raw-names
         ,(build (parse arg-spec) body)))))


;;;; 2D SHAPE WRAPPERS

(defdraw draw-pixel ((x position-x) (y position-y) color)
  (%draw-pixel x y color))

(defdraw draw-pixel-v ((position position-2) color)
  (%draw-pixel-v position color))

(defdraw draw-line ((startPosX position-x) (startPosY position-y) (endPosX position-x) (endPosY position-y) color)
  (%draw-line startPosX startPosY endPosX endPosY color))

(defdraw draw-line-v ((startPos position-2) (endPos position-2) color)
  (%draw-line-v startPos endPos color))

(defdraw draw-line-ex ((startPos position-2) (endPos position-2) thick color)
  (%draw-line-ex startPos endPos thick color))

(defdraw draw-line-bezier ((startPos position-2) (endPos position-2) thick color)
  (%draw-line-bezier startPos endPos thick color))

(defdraw draw-line-dashed ((startPos position-2) (endPos position-2) dashSize spaceSize color)
  (%draw-line-dashed startPos endPos dashSize spaceSize color))

(defdraw draw-circle ((centerX position-x) (centerY position-y) radius color)
  (%draw-circle centerX centerY radius color))

(defdraw draw-circle-v ((center position-2) radius color)
  (%draw-circle-v center radius color))

(defdraw draw-circle-gradient ((center position-2) radius inner outer)
  (%draw-circle-gradient center radius inner outer))

(defdraw draw-circle-sector ((center position-2) radius startAngle endAngle segments color)
  (%draw-circle-sector center radius startAngle endAngle segments color))

(defdraw draw-circle-sector-lines ((center position-2) radius startAngle endAngle segments color)
  (%draw-circle-sector-lines center radius startAngle endAngle segments color))

(defdraw draw-circle-lines ((centerX position-x) (centerY position-y) radius color)
  (%draw-circle-lines centerX centerY radius color))

(defdraw draw-circle-lines-v ((center position-2) radius color)
  (%draw-circle-lines-v center radius color))

(defdraw draw-ellipse ((centerX position-x) (centerY position-y) radiusH radiusV color)
  (%draw-ellipse centerX centerY radiusH radiusV color))

(defdraw draw-ellipse-v ((center position-2) radiusH radiusV color)
  (%draw-ellipse-v center radiusH radiusV color))

(defdraw draw-ellipse-lines ((centerX position-x) (centerY position-y) radiusH radiusV color)
  (%draw-ellipse-lines centerX centerY radiusH radiusV color))

(defdraw draw-ellipse-lines-v ((center position-2) radiusH radiusV color)
  (%draw-ellipse-lines-v center radiusH radiusV color))

(defdraw draw-ring ((center position-2) innerRadius outerRadius startAngle endAngle segments color)
  (%draw-ring center innerRadius outerRadius startAngle endAngle segments color))

(defdraw draw-ring-lines ((center position-2) innerRadius outerRadius startAngle endAngle segments color)
  (%draw-ring-lines center innerRadius outerRadius startAngle endAngle segments color))

(defdraw draw-rectangle ((posX position-x) (posY position-y) width height color)
  (%draw-rectangle posX posY width height color))

(defdraw draw-rectangle-v ((position position-2) size color)
  (%draw-rectangle-v position size color))

(defdraw draw-rectangle-pro (rec origin rotation color)
  (%draw-rectangle-pro rec origin rotation color))

(defdraw draw-rectangle-gradient-v ((posX position-x) (posY position-y) width height top bottom)
  (%draw-rectangle-gradient-v posX posY width height top bottom))

(defdraw draw-rectangle-gradient-h ((posX position-x) (posY position-y) width height left right)
  (%draw-rectangle-gradient-h posX posY width height left right))

(defdraw draw-rectangle-lines ((posX position-x) (posY position-y) width height color)
  (%draw-rectangle-lines posX posY width height color))

(defdraw draw-rectangle-lines-ex (rec lineThick color)
  (%draw-rectangle-lines-ex rec lineThick color))

(defdraw draw-rectangle-rounded (rec roundness segments color)
  (%draw-rectangle-rounded rec roundness segments color))

(defdraw draw-rectangle-rounded-lines (rec roundness segments color)
  (%draw-rectangle-rounded-lines rec roundness segments color))

(defdraw draw-rectangle-rounded-lines-ex (rec roundness segments lineThick color)
  (%draw-rectangle-rounded-lines-ex rec roundness segments lineThick color))

(defdraw draw-triangle ((v1 position-2) (v2 position-2) (v3 position-2) color)
  (%draw-triangle v1 v2 v3 color))

(defdraw draw-triangle-lines ((v1 position-2) (v2 position-2) (v3 position-2) color)
  (%draw-triangle-lines v1 v2 v3 color))

(defdraw draw-poly ((center position-2) sides radius rotation color)
  (%draw-poly center sides radius rotation color))

(defdraw draw-poly-lines ((center position-2) sides radius rotation color)
  (%draw-poly-lines center sides radius rotation color))

(defdraw draw-poly-lines-ex ((center position-2) sides radius rotation lineThick color)
  (%draw-poly-lines-ex center sides radius rotation lineThick color))

(defdraw draw-spline-segment-linear ((p1 position-2) (p2 position-2) thick color)
  (%draw-spline-segment-linear p1 p2 thick color))

(defdraw draw-spline-segment-basis ((p1 position-2) (p2 position-2) (p3 position-2) (p4 position-2) thick color)
  (%draw-spline-segment-basis p1 p2 p3 p4 thick color))

(defdraw draw-spline-segment-catmull-rom ((p1 position-2) (p2 position-2) (p3 position-2) (p4 position-2) thick color)
  (%draw-spline-segment-catmull-rom p1 p2 p3 p4 thick color))

(defdraw draw-spline-segment-bezier-quadratic ((p1 position-2) (c2 position-2) (p3 position-2) thick color)
  (%draw-spline-segment-bezier-quadratic p1 c2 p3 thick color))

(defdraw draw-spline-segment-bezier-cubic ((p1 position-2) (c2 position-2) (c3 position-2) (p4 position-2) thick color)
  (%draw-spline-segment-bezier-cubic p1 c2 c3 p4 thick color))


;;;; TEXTURE / TEXT WRAPPERS

(defdraw draw-texture (texture (posX position-x) (posY position-y) tint)
  (%draw-texture texture posX posY tint))

(defdraw draw-texture-v (texture (position position-2) tint)
  (%draw-texture-v texture position tint))

(defdraw draw-texture-ex (texture (position position-2) rotation scale tint)
  (%draw-texture-ex texture position rotation scale tint))

(defdraw draw-texture-rec (texture source (position position-2) tint)
  (%draw-texture-rec texture source position tint))

(defdraw draw-texture-pro (texture source dest origin rotation tint)
  (%draw-texture-pro texture source dest origin rotation tint))

(defdraw draw-texture-n-patch (texture nPatchInfo dest origin rotation tint)
  (%draw-texture-n-patch texture nPatchInfo dest origin rotation tint))

(defdraw draw-fps ((posX position-x) (posY position-y))
  (%draw-fps posX posY))

(defdraw draw-text (text (posX position-x) (posY position-y) fontSize color)
  (%draw-text text posX posY fontSize color))

(defdraw draw-text-ex (font text (position position-2) fontSize spacing tint)
  (%draw-text-ex font text position fontSize spacing tint))

(defdraw draw-text-pro (font text (position position-2) origin rotation fontSize spacing tint)
  (%draw-text-pro font text position origin rotation fontSize spacing tint))

(defdraw draw-text-codepoint (font codepoint (position position-2) fontSize tint)
  (%draw-text-codepoint font codepoint position fontSize tint))


;;;; 3D WRAPPERS

(defdraw draw-line-3d ((startPos position-3) (endPos position-3) color)
  (%draw-line-3d startPos endPos color))

(defdraw draw-point-3d ((position position-3) color)
  (%draw-point-3d position color))

(defdraw draw-circle-3d ((center position-3) radius rotationAxis rotationAngle color)
  (%draw-circle-3d center radius rotationAxis rotationAngle color))

(defdraw draw-triangle-3d ((v1 position-3) (v2 position-3) (v3 position-3) color)
  (%draw-triangle-3d v1 v2 v3 color))

(defdraw draw-cube ((position position-3) width height length color)
  (%draw-cube position width height length color))

(defdraw draw-cube-v ((position position-3) size color)
  (%draw-cube-v position size color))

(defdraw draw-cube-wires ((position position-3) width height length color)
  (%draw-cube-wires position width height length color))

(defdraw draw-cube-wires-v ((position position-3) size color)
  (%draw-cube-wires-v position size color))

(defdraw draw-sphere ((centerPos position-3) radius color)
  (%draw-sphere centerPos radius color))

(defdraw draw-sphere-ex ((centerPos position-3) radius rings slices color)
  (%draw-sphere-ex centerPos radius rings slices color))

(defdraw draw-sphere-wires ((centerPos position-3) radius rings slices color)
  (%draw-sphere-wires centerPos radius rings slices color))

(defdraw draw-cylinder ((position position-3) radiusTop radiusBottom height slices color)
  (%draw-cylinder position radiusTop radiusBottom height slices color))

(defdraw draw-cylinder-ex ((startPos position-3) (endPos position-3) startRadius endRadius sides color)
  (%draw-cylinder-ex startPos endPos startRadius endRadius sides color))

(defdraw draw-cylinder-wires ((position position-3) radiusTop radiusBottom height slices color)
  (%draw-cylinder-wires position radiusTop radiusBottom height slices color))

(defdraw draw-cylinder-wires-ex ((startPos position-3) (endPos position-3) startRadius endRadius sides color)
  (%draw-cylinder-wires-ex startPos endPos startRadius endRadius sides color))

(defdraw draw-capsule ((startPos position-3) (endPos position-3) radius slices rings color)
  (%draw-capsule startPos endPos radius slices rings color))

(defdraw draw-capsule-wires ((startPos position-3) (endPos position-3) radius slices rings color)
  (%draw-capsule-wires startPos endPos radius slices rings color))

(defdraw draw-plane ((centerPos position-3) size color)
  (%draw-plane centerPos size color))

(defdraw draw-model (model (position position-3) scale tint)
  (%draw-model model position scale tint))

(defdraw draw-model-ex (model (position position-3) rotationAxis rotationAngle scale tint)
  (%draw-model-ex model position rotationAxis rotationAngle scale tint))

(defdraw draw-model-wires (model (position position-3) scale tint)
  (%draw-model-wires model position scale tint))

(defdraw draw-model-wires-ex (model (position position-3) rotationAxis rotationAngle scale tint)
  (%draw-model-wires-ex model position rotationAxis rotationAngle scale tint))

(defdraw draw-billboard (camera texture (position position-3) scale tint)
  (%draw-billboard camera texture position scale tint))

(defdraw draw-billboard-rec (camera texture source (position position-3) size tint)
  (%draw-billboard-rec camera texture source position size tint))

(defdraw draw-billboard-pro (camera texture source (position position-3) up size origin rotation tint)
  (%draw-billboard-pro camera texture source position up size origin rotation tint))


;;;; NOT WRAPPED BY DEFDRAW
;; These take arrays of points, Rectangle structs, or are not positions:
;;   draw-line-strip, draw-triangle-fan, draw-triangle-strip,
;;   draw-spline-linear, draw-spline-basis, draw-spline-catmull-rom,
;;   draw-spline-bezier-quadratic, draw-spline-bezier-cubic,
;;   draw-text-codepoints, draw-rectangle-rec, draw-rectangle-lines-ex,
;;   draw-rectangle-rounded, draw-rectangle-rounded-lines,
;;   draw-rectangle-rounded-lines-ex, draw-ray, draw-grid,
;;   draw-mesh, draw-mesh-instanced, draw-bounding-box.



; PLAYGROUND

; Vector2 Vector2Add(Vector2 v1, Vector2 v2) // Add two vectors (v1 + v2)
(cffi:defcfun ("Vector2Add" %vector2-add) (:struct %Vector2)
    (v1 (:struct %Vector2))
    (v2 (:struct %Vector2)))

(cffi:defcfun ("Vector2Length" %vector2-length) :float
    (v (:struct %Vector2)))

(%vector2-add (v! 1 1) (v! 2 2))

(with-window 800 600 "Window"
  (%set-target-fps 60)
  (with-texture tex 300 300
    (with-texture-mode tex
      (%clear-background :pink)
      (%draw-rectangle 0 0 150 150 :red))
    (loop until (%window-should-close)
	  with position-x = 0
	  with position-y = 0
	  doing
	     (with-mouse ((:left :down left-down?)
			  (:position :x x-pos :y y-pos)
			  (:delta :x delta-x :y delta-y))
	       (when left-down?
		 (setf position-x (truncate x-pos)
		       position-y (truncate y-pos)))
	       (with-drawing 
		 (%clear-background :raywhite)
		 (%draw-fps 10 10)
		 (%draw-texture (render-texture-texture tex)
				position-x
				position-y
				:white))))))

(defun plot(function &key from to (scale-x 1) (scale-y 1))
  (loop for i from from below to
	for x = (round (* i scale-x))
	for y = (round (* (funcall function i) scale-y))
	doing
	   (%draw-pixel x y :blue)))

(%close-window)

(let ((gen (make-prime-generator)))
  (funcall gen 10000)
  (plot #'(lambda(x) (funcall gen x)) :from 1 :to 10000 :scale-x 799/9999 :scale-y 799/104729))

(plot #'(lambda(x) (* 2 x)) :from 1 :to 1000)

(defun rad->deg (radians)
  (* radians (/ 180.0 pi)))

(defun deg->rad (degrees)
  (* degrees (/ pi 180.0)))

(with-window 800 800 "Plot"
  (with-texture plot-texture 1000 1000
    (with-texture-mode plot-texture
      (%draw-circle 0 0 100.0 :blue)
      (%draw-circle 0 799 100.0 :red)
      (%draw-circle 799 0 100.0 :yellow)
      (%draw-circle 799 799 100.0 :green)
      (plot #'(lambda(x) x) :from 1 :to 1000)
      (plot #'(lambda(x)(* 2 x)) :from 1 :to 1000)
      (plot #'(lambda(x)(sin (rad->deg x))) :from 1 :to 1000))
    (loop until (%window-should-close)
	  doing
	     (with-drawing
	       (%clear-background :white)
	       (%draw-texture (render-texture-texture plot-texture)
			      200
			      -400
			      :white)))))

(defmacro with-transform(matrix &body body)
  (with-gensyms(old-matrix)
    `(let* ((,old-matrix *draw-transform*))
       (setf *draw-transform* (matrix* ,old-matrix ,matrix))
       (unwind-protect (progn ,@body)
	 (setf *draw-transform* ,old-matrix)))))
       
       
(with-window 800 800 "Plot"
  (%set-target-fps 60)
  (loop until (%window-should-close)
	with camera-dx = 0
	with camera-dy = 0
	with scale = 1.0
	with rotate = 0.0
	doing
	   (with-keys ((:w :down is-w-down)
		       (:a :down is-a-down)
		       (:s :down is-s-down)
		       (:d :down is-d-down)
		       (:q :down is-q-down)
		       (:e :down is-e-down)
		       (:minus :down minus-down?)
		       (:equal :down plus-down?))
	     (with-mouse ((:left :down dragging?)
			  (:position :x x-pos :y y-pos)
			  (:delta :x delta-x :y delta-y)
			  (:wheel :move wheel-move))
	       (when is-w-down (decf camera-dy 10))
	       (when is-a-down (decf camera-dx 10))
	       (when is-s-down (incf camera-dy 10))
	       (when is-d-down (incf camera-dx 10))
	       (when is-q-down (incf rotate 2.5))
	       (when is-e-down (decf rotate 2.5))
	       (when minus-down?
		 (decf scale 0.05))
	       (when plus-down?
		 (incf scale 0.05))
	       (when dragging?
		 (decf camera-dx delta-x)
		 (decf camera-dy delta-y))
	       (when wheel-move
		 (incf scale (* 0.01 wheel-move)))
	       (with-drawing
		 (%clear-background :white)
		 (with-transform
		     (reduce #'matrix*
			     (list
			      (scale-matrix! scale scale)
			      (translate-matrix! (- camera-dx) (- camera-dy))
			      (translate-matrix! 0 800)
			      (rotate-matrix! rotate)
			      (scale-matrix! 1 -1)))
		   (draw-circle 0 0 100.0 :blue)
		   (draw-circle 0 799 100.0 :red)
		   (draw-circle 799 0 100.0 :yellow)
		   (draw-circle 799 799 100.0 :green)
		   (draw-rectangle 0 0 100 100 :pink))
		 (draw-fps 10 10)
		 (draw-text (format nil "Position delta (~a,~a)~%" camera-dx camera-dy) 10 40 25 :green)
		 (draw-text (format nil "(~a, ~a)" x-pos y-pos) (+ 20 x-pos) (- y-pos 20) 25 :green)
		 (draw-text (cond (dragging? "Dragging")
				  (is-w-down "W")
				  (is-a-down "A")
				  (is-s-down "S")
				  (is-d-down "D")
				  ((not (zerop wheel-move)) "Scrolling")
				  (t ""))
			    350	400 25 :blue))))))
