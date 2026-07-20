(ql:quickload 'cffi)
(ql:quickload 'cffi-libffi)

(cffi:define-foreign-library libraylib
    (:unix (:default "/usr/local/lib/libraylib"))
    (t (:default "libraylib")))

(unless (cffi:foreign-library-loaded-p 'libraylib)
    (cffi:use-foreign-library libraylib))

;;;; Structs

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
    r g b a)

; typedef struct Vector2 {
;     float x;                // Vector x component
;     float y;                // Vector y component
; } Vector2;

(cffi:defcstruct (%vector2 :class vector2-type)
    (x :float)
    (y :float))

(defstruct vector2
    x y)

; typedef struct Vector3 {
;     float x;                // Vector x component
;     float y;                // Vector y component
;     float z;                // Vector z component
; } Vector3;

(cffi:defcstruct (%vector3 :class vector3-type)
    (x :float)
    (y :float)
    (z :float))

(defstruct vector3
    x y z)

; typedef struct Vector4 {
;     float x;                // Vector x component
;     float y;                // Vector y component
;     float z;                // Vector z component
;     float w;                // Vector w component
; } Vector4;

(cffi:defcstruct (%vector4 :class vector4-type)
    (x :float)
    (y :float)
    (z :float)
    (w :float))

(defstruct vector4
    x y z w)

; Camera2D, defines position/orientation in 2d space
; typedef struct Camera2D {
;     Vector2 offset // Camera offset (screen space offset from window origin)
;     Vector2 target // Camera target (world space target point that is mapped to screen space offset)
;     float rotation // Camera rotation in degrees (pivots around target)
;     float zoom // Camera zoom (scaling around target), must not be set to 0, set to 1.0f for no scale
; } Camera2D;

(cffi:defcstruct (%camera-2d :class camera-2d-type)
    (offset (:struct %vector2))
    (target (:struct %vector2))
    (rotation :float)
    (zoom :float))

(defstruct camera-2d
    offset target rotation zoom)

(make-camera-2d :offset '(1 2) :target '(100 200) :rotation 0.0 :zoom 1.0)
(make-camera-2d :offset (make-vector2 :x 1 :y 2) :target (make-vector2 :x 100 :y 200) :rotation 0.0 :zoom 1.0)

; #define LIGHTGRAY  (Color){ 200, 200, 200, 255 }   // Light Gray
; #define GRAY       (Color){ 130, 130, 130, 255 }   // Gray
; #define DARKGRAY   (Color){ 80, 80, 80, 255 }      // Dark Gray
; #define YELLOW     (Color){ 253, 249, 0, 255 }     // Yellow
; #define GOLD       (Color){ 255, 203, 0, 255 }     // Gold
; #define ORANGE     (Color){ 255, 161, 0, 255 }     // Orange
; #define PINK       (Color){ 255, 109, 194, 255 }   // Pink
; #define RED        (Color){ 230, 41, 55, 255 }     // Red
; #define MAROON     (Color){ 190, 33, 55, 255 }     // Maroon
; #define GREEN      (Color){ 0, 228, 48, 255 }      // Green
; #define LIME       (Color){ 0, 158, 47, 255 }      // Lime
; #define DARKGREEN  (Color){ 0, 117, 44, 255 }      // Dark Green
; #define SKYBLUE    (Color){ 102, 191, 255, 255 }   // Sky Blue
; #define BLUE       (Color){ 0, 121, 241, 255 }     // Blue
; #define DARKBLUE   (Color){ 0, 82, 172, 255 }      // Dark Blue
; #define PURPLE     (Color){ 200, 122, 255, 255 }   // Purple
; #define VIOLET     (Color){ 135, 60, 190, 255 }    // Violet
; #define DARKPURPLE (Color){ 112, 31, 126, 255 }    // Dark Purple
; #define BEIGE      (Color){ 211, 176, 131, 255 }   // Beige
; #define BROWN      (Color){ 127, 106, 79, 255 }    // Brown
; #define DARKBROWN  (Color){ 76, 63, 47, 255 }      // Dark Brown

; #define WHITE      (Color){ 255, 255, 255, 255 }   // White
; #define BLACK      (Color){ 0, 0, 0, 255 }         // Black
; #define BLANK      (Color){ 0, 0, 0, 0 }           // Blank (Transparent)
; #define MAGENTA    (Color){ 255, 0, 255, 255 }     // Magenta
; #define RAYWHITE   (Color){ 245, 245, 245, 255 }   // My own White (raylib logo)

(defmethod cffi:translate-into-foreign-memory
    ((value color) (type color-type) pointer)
    (cffi:with-foreign-slots ((r g b a) pointer (:struct %color))
        (setf r (color-r value)
                g (color-g value)
                b (color-b value)
                a (color-a value))))

(defmethod cffi:translate-into-foreign-memory
    ((value list) (type color-type) pointer)
    (cffi:with-foreign-slots ((r g b a) pointer (:struct %color))
        (setf r (first value)
                g (second value)
                b (third value)
                a (fourth value))))

(defmethod cffi:translate-into-foreign-memory
    ((value vector2) (type vector2-type) pointer)
    (cffi:with-foreign-slots ((x y) pointer (:struct %vector2))
        (setf x (coerce (vector2-x value) 'float)
                y (coerce (vector2-y value) 'float))))

(defmethod cffi:translate-into-foreign-memory
    ((value list) (type vector2-type) pointer)
    (cffi:with-foreign-slots ((x y) pointer (:struct %vector2))
        (setf x (coerce (first value) 'float)
                y (coerce  (second value) 'float))))

(defmethod cffi:translate-from-foreign (ptr (type vector2-type))
    (cffi:with-foreign-slots ((x y) ptr (:struct %vector2))
        (make-vector2 :x x :y y)))

(defmethod cffi:translate-into-foreign-memory
    ((value vector3) (type vector3-type) pointer)
    (cffi:with-foreign-slots ((x y z) pointer (:struct %vector3))
        (setf x (vector3-x value)
                y (vector3-y value)
                z (vector3-z value))))

(defmethod cffi:translate-into-foreign-memory
    ((value list) (type vector3-type) pointer)
    (cffi:with-foreign-slots ((x y z) pointer (:struct %vector3))
        (destructuring-bind (a b c) value
            (setf x a
                    y b
                    z c))))

(defmethod cffi:translate-into-foreign-memory
    ((value vector4) (type vector4-type) pointer)
    (cffi:with-foreign-slots ((x y z w) pointer (:struct %vector4))
        (setf x (vector4-x value)
                y (vector4-y value)
                z (vector4-z value)
                w (vector4-w value))))

(defmethod cffi:translate-into-foreign-memory
    ((value list) (type vector4-type) pointer)
    (cffi:with-foreign-slots ((x y z w) pointer (:struct %vector4))
        (setf x (first value)
                y (second value)
                z (third value)
                w (fourth value))))

(defmethod cffi:translate-into-foreign-memory
    ((value camera-2d) (type camera-2d-type) pointer)
    (cffi:with-foreign-slots ((offset target rotation zoom) pointer (:struct %camera-2d))
        (cffi:translate-into-foreign-memory
            (camera-2d-offset value)
            (cffi::parse-type '(:struct %vector2))
            (cffi:foreign-slot-pointer pointer '(:struct %camera-2d) 'offset))
        (cffi:translate-into-foreign-memory
            (camera-2d-target value)
            (cffi::parse-type '(:struct %vector2))
            (cffi:foreign-slot-pointer pointer '(:struct %camera-2d) 'target))
        (setf rotation (camera-2d-rotation value)
                zoom (camera-2d-zoom value))))

; void InitWindow(int width, int height, const char *title) - Initialize window and OpenGL context
(cffi:defcfun ("InitWindow" init-window) :void
    (width :int)
    (height :int)
    (title :string))

; void SetTargetFPS(int fps);
(cffi:defcfun ("SetTargetFPS" set-target-fps) :void
    (fps :int))

; void ClearBackground(Color color) - Set background color (framebuffer clear color)
(cffi:defcfun ("ClearBackground" clear-background) :void
    (color (:struct %color)))

; void BeginDrawing(void) - Setup canvas (framebuffer) to start drawing
(cffi:defcfun ("BeginDrawing" begin-drawing) :void)

; void EndDrawing(void)
(cffi:defcfun ("EndDrawing" end-drawing) :void)

; void CloseWindow(void) // Close window and unload OpenGL context
(cffi:defcfun ("CloseWindow" close-window) :void)

; bool WindowShouldClose(void) // Check if application should close (KEY_ESCAPE pressed or windows close icon clicked)
(cffi:defcfun ("WindowShouldClose" window-should-close) :bool)

; void DrawCircle(int centerX, int centerY, float radius, Color color) // Draw a color-filled circle
(cffi:defcfun ("DrawCircle" draw-circle) :void
    (centerX :int)
    (centerY :int)
    (radius :float)
    (color (:struct %color)))

; void DrawCircleV(Vector2 center, float radius, Color color) // Draw a color-filled circle (Vector version)
(cffi:defcfun ("DrawCircleV" draw-circle-v) :void
    (center (:struct %vector2))
    (radius :float)
    (color (:struct %color)))

; void DrawLineV(Vector2 startPos, Vector2 endPos, Color color) // Draw a line (using gl lines)
(cffi:defcfun ("DrawLineV" draw-line-v) :void
    (startPos (:struct %vector2))
    (endPos (:struct %vector2))
    (color (:struct %color)))

; void DrawPixel(int posX, int posY, Color color) // Draw a pixel using geometry [Can be slow, use with care]
(cffi:defcfun ("DrawPixel" draw-pixel) :void
    (posX :int)
    (posY :int)
    (color (:struct %color)))

; void DrawPixelV(Vector2 position, Color color) // Draw a pixel using geometry (Vector version) [Can be slow, use with care]
(cffi:defcfun ("DrawPixelV" draw-pixel-v) :void
    (position (:struct %vector2))
    (color (:struct %color)))

; bool IsKeyPressed(int key) // Check if a key has been pressed once
(cffi:defcfun ("IsKeyPressed" is-key-pressed) :bool
    (key :int))

; bool IsKeyDown(int key) // Check if a key is being pressed
(cffi:defcfun ("IsKeyDown" is-key-down) :bool
    (key :int))

; bool IsKeyUp(int key) // Check if a key is being pressed
(cffi:defcfun ("IsKeyUp" is-key-up) :bool
    (key :int))

; float GetMouseWheelMove(void) // Get mouse wheel movement for X or Y, whichever is larger
(cffi:defcfun ("GetMouseWheelMove" get-mouse-wheel-move) :float)

; float GetFrameTime(void) // Get time in seconds for last frame drawn (delta time)
(cffi:defcfun ("GetFrameTime" get-frame-time) :float)

; int GetFPS(void) // Get current FPS
(cffi:defcfun ("GetFPS" get-fps) :int)

; void DrawFPS(int posX, int posY) // Draw current FPS
(cffi:defcfun ("DrawFPS" draw-fps) :void
    (posX :int)
    (posY :int))

; void DrawText(const char *text, int posX, int posY, int fontSize, Color color) // Draw text (using default font)
(cffi:defcfun ("DrawText" draw-text) :void
    (text (:string))
    (posX :int)
    (posY :int)
    (fontSize :int)
    (color (:struct %color)))

; typedef struct Matrix {
;     float m0, m4, m8, m12;  // Matrix first row (4 components)
;     float m1, m5, m9, m13;  // Matrix second row (4 components)
;     float m2, m6, m10, m14; // Matrix third row (4 components)
;     float m3, m7, m11, m15; // Matrix fourth row (4 components)
; } Matrix;

(cffi:defcstruct (%matrix :class matrix-type)
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
        m0 m4 m8 m12
        m1 m5 m9 m13
        m2 m6 m10 m14
        m3 m7 m11 m15)

(defmethod cffi:translate-into-foreign-memory
        ((value matrix) (type matrix-type) pointer)
    (cffi:with-foreign-slots ((m0 m4 m8 m12 m1 m5 m9 m13 m2 m6 m10 m14 m3 m7 m11 m15) pointer (:struct %matrix))
        (setf m0 (matrix-m0 value)
                    m4 (matrix-m4 value)
                    m8 (matrix-m8 value)
                    m12 (matrix-m12 value)
                    m1 (matrix-m1 value)
                    m5 (matrix-m5 value)
                    m9 (matrix-m9 value)
                    m13 (matrix-m13 value)
                    m2 (matrix-m2 value)
                    m6 (matrix-m6 value)
                    m10 (matrix-m10 value)
                    m14 (matrix-m14 value)
                    m3 (matrix-m3 value)
                    m7 (matrix-m7 value)
                    m11 (matrix-m11 value)
                    m15 (matrix-m15 value))))

(defmethod cffi:translate-into-foreign-memory
        ((value list) (type matrix-type) pointer)
    "Accept a list of 4 rows (each a list of 4 floats) and write into C Matrix layout."
    (let ((r0 (first value)) (r1 (second value)) (r2 (third value)) (r3 (fourth value)))
        (cffi:with-foreign-slots ((m0 m4 m8 m12 m1 m5 m9 m13 m2 m6 m10 m14 m3 m7 m11 m15) pointer (:struct %matrix))
            (setf m0 (coerce (nth 0 r0) 'float)
                        m4 (coerce (nth 1 r0) 'float)
                        m8 (coerce (nth 2 r0) 'float)
                        m12 (coerce (nth 3 r0) 'float)

                        m1 (coerce (nth 0 r1) 'float)
                        m5 (coerce (nth 1 r1) 'float)
                        m9 (coerce (nth 2 r1) 'float)
                        m13 (coerce (nth 3 r1) 'float)

                        m2 (coerce (nth 0 r2) 'float)
                        m6 (coerce (nth 1 r2) 'float)
                        m10 (coerce (nth 2 r2) 'float)
                        m14 (coerce (nth 3 r2) 'float)

                        m3 (coerce (nth 0 r3) 'float)
                        m7 (coerce (nth 1 r3) 'float)
                        m11 (coerce (nth 2 r3) 'float)
                        m15 (coerce (nth 3 r3) 'float)))))

(defmethod cffi:translate-from-foreign (ptr (type matrix-type))
    (cffi:with-foreign-slots ((m0 m4 m8 m12 m1 m5 m9 m13 m2 m6 m10 m14 m3 m7 m11 m15) ptr (:struct %matrix))
        (make-matrix :m0 m0 :m4 m4 :m8 m8 :m12 m12
                        :m1 m1 :m5 m5 :m9 m9 :m13 m13
                        :m2 m2 :m6 m6 :m10 m10 :m14 m14
                        :m3 m3 :m7 m7 :m11 m11 :m15 m15)))


; Vector2 Vector2Transform(Vector2 v, Matrix mat) Transforms a Vector2 by a given Matrix
(cffi:defcfun ("Vector2Transform" vector2-transform) (:struct %vector2)
    (v (:struct %vector2))
    (mat (:struct %matrix)))

; Matrix MatrixMultiply(Matrix left, Matrix right) // Get two matrix multiplication NOTE: When multiplying matrices... the order matters!
(cffi:defcfun ("MatrixMultiply" matrix-multiply) (:struct %matrix)
    (left (:struct %matrix))
    (right (:struct %matrix)))

; void BeginMode2D(Camera2D camera) // Begin 2D mode with custom camera (2D)
(cffi:defcfun ("BeginMode2D" begin-mode-2d) :void
    (camera (:struct %camera-2d)))

; void EndMode2D(void) // Ends 2D mode with custom camera
(cffi:defcfun ("EndMode2D" end-mode-2d) :void)

; bool IsMouseButtonPressed(int button) // Check if a mouse button has been pressed once
(cffi:defcfun ("IsMouseButtonPressed" is-mouse-button-pressed) :bool
    (button :int))

; bool IsMouseButtonDown(int button) // Check if a mouse button is being pressed
(cffi:defcfun ("IsMouseButtonDown" is-mouse-button-down) :bool
    (button :int))

; bool IsMouseButtonReleased(int button) // Check if a mouse button has been released once
(cffi:defcfun ("IsMouseButtonReleased" is-mouse-button-released) :bool
    (button :int))

; bool IsMouseButtonUp(int button) // Check if a mouse button is NOT being pressed
(cffi:defcfun ("IsMouseButtonUp" is-mouse-button-up) :bool
    (button :int))

; Vector2 GetMouseDelta(void) // Get mouse delta between frames
(cffi:defcfun ("GetMouseDelta" get-mouse-delta) (:struct %vector2))

; Vector2 GetScreenToWorld2D(Vector2 position, Camera2D camera) // Get the world space position for a 2d camera screen space position
(cffi:defcfun ("GetScreenToWorld2D" get-screen-to-world-2d) (:struct %vector2)
    (position (:struct %vector2))
    (camera (:struct %camera-2d)))

(defun keyboard-key(key)
    (ecase key
    (:null             0)
    (:apostrophe       39)
    (:comma            44)
    (:minus            45)
    (:period           46)
    (:slash            47)
    (:zero             48)
    (:one              49)
    (:two              50)
    (:three            51)
    (:four             52)
    (:five             53)
    (:six              54)
    (:seven            55)
    (:eight            56)
    (:nine             57)
    (:semicolon        59)
    (:equal            61)
    (:a                65)
    (:b                66)
    (:c                67)
    (:d                68)
    (:e                69)
    (:f                70)
    (:g                71)
    (:h                72)
    (:i                73)
    (:j                74)
    (:k                75)
    (:l                76)
    (:m                77)
    (:n                78)
    (:o                79)
    (:p                80)
    (:q                81)
    (:r                82)
    (:s                83)
    (:t                84)
    (:u                85)
    (:v                86)
    (:w                87)
    (:x                88)
    (:y                89)
    (:z                90)
    (:left_bracket     91)
    (:backslash        92)
    (:right_bracket    93)
    (:grave            96)
    (:space            32)
    (:escape           256)
    (:enter            257)
    (:tab              258)
    (:backspace        259)
    (:insert           260)
    (:delete           261)
    (:right            262)
    (:left             263)
    (:down             264)
    (:up               265)
    (:page_up          266)
    (:page_down        267)
    (:home             268)
    (:end              269)
    (:caps_lock        280)
    (:scroll_lock      281)
    (:num_lock         282)
    (:print_screen     283)
    (:pause            284)
    (:f1               290)
    (:f2               291)
    (:f3               292)
    (:f4               293)
    (:f5               294)
    (:f6               295)
    (:f7               296)
    (:f8               297)
    (:f9               298)
    (:f10              299)
    (:f11              300)
    (:f12              301)
    (:left_shift       340)
    (:left_control     341)
    (:left_alt         342)
    (:left_super       343)
    (:right_shift      344)
    (:right_control    345)
    (:right_alt        346)
    (:right_super      347)
    (:kb_menu          348)
    (:kp_0             320)
    (:kp_1             321)
    (:kp_2             322)
    (:kp_3             323)
    (:kp_4             324)
    (:kp_5             325)
    (:kp_6             326)
    (:kp_7             327)
    (:kp_8             328)
    (:kp_9             329)
    (:kp_decimal       330)
    (:kp_divide        331)
    (:kp_multiply      332)
    (:kp_subtract      333)
    (:kp_add           334)
    (:kp_enter         335)
    (:kp_equal         336)
    (:back             4)
    (:menu             5)
    (:volume_up        24)
    (:volume_down      25)))

(defun mouse-key(key)
    (ecase key
        (:left     0)
        (:right    1)
        (:middle   2)
        (:side     3)
        (:extra    4)
        (:forward  5)
        (:back     6)))

(defparameter *points* (make-array 1000000 :fill-pointer 0 :adjustable t :element-type 'fixnum))
(defparameter *primes* (make-array 1000000 :fill-pointer 0 :adjustable t :element-type 'fixnum))
(defparameter *cursor* 0)
(defparameter *sensitivity* 100)

(declaim (optimize (speed 3) (safety 0) (debug 0)))
(defun make-prime-generator()
    (let ((primes (make-array 10000 :fill-pointer 0 :adjustable t :element-type 'fixnum))
            (cursor 1)
            (increments 1))
            (declare (type (vector fixnum) primes)
                (type fixnum cursor increments))
        (labels ((prime-add(p)
                    (vector-push-extend p primes)
                    p)
                    (prime-next()
                        (loop
                            do (incf cursor 1)
                            when (loop for i across primes
                                    with root = (isqrt cursor)
                                    while (<= i root)
                                    never (zerop (mod cursor i)))
                                return (prime-add cursor)))

                    (prime-nth(n)
                        (loop until (<= n (length primes))
                                doing
                                (prime-next))
                        (aref primes (1- n))))
            (lambda (&optional n)
                (cond ((and n (<= n (length primes))) (aref primes (1- n)))
                    (n (prime-nth n))
                    (t (prime-next)))))))

(defmacro with-gensyms(symbols &body body)
    `(let ,(loop for sym in symbols collect `(,sym (gensym)))
        ,@body))

(defmacro do-primes-n ((var count) &body body)
    (with-gensyms(counter prime-generator)
        `(let ((,prime-generator (make-prime-generator)))
            (dotimes (,counter ,count)
                (let ((,var (funcall ,prime-generator)))
                    ,@body)))))

(defun flip-y-axis(y)
    (- *screen-height* 1 y))

(defun normalize-to-screen-height(value maximum)
    (round (* value (/ (1- *screen-height*) maximum))))

(defun clamp(value min max)
    (cond ((< value min) min)
            ((> value max) max)
            (t value)))


(defun push-prime(p)
    (vector-push-extend p *primes*))

(defun primes-horizontal()
    (let ((x 1)
        (gen (make-prime-generator)))
        (setf *points* (make-array 1000 :fill-pointer 0 :adjustable t :element-type 'fixnum))
        (setf *primes* (make-array 1000 :fill-pointer 0 :adjustable t :element-type 'fixnum))
        (setf *cursor* 0)
        (dotimes (_ 2000)
            (let ((p (funcall gen)))
                (push-prime p)
                (incf x)))
        (init-window *screen-width* *screen-height* "Primes!!!")
        (set-target-fps 60)
        (loop until (window-should-close)
            doing
            (begin-drawing)
            (setf *cursor* (clamp (round (+
                                    (* *sensitivity* (get-mouse-wheel-move))
                                    *cursor*))
                                0
                                ( - (max (length *primes*) *screen-width*) (min (length *primes*) *screen-width*))))
            (if (or (>= (+ *cursor* *screen-width*) (length *primes*))
                    (< (length *primes*) *screen-width*))
                (progn
                    (print "Fetching")
                    (dotimes(_ 500)
                        (push-prime (funcall gen)))))
            (clear-background (make-color :r 255 :g 255 :b 0 :a 255))
            (draw-fps 0 0)
            (loop for i from *cursor* below (min (+ *cursor* *screen-width*) (length *primes*))
                for prime = (elt *primes* i)
                doing
                (draw-text (princ-to-string (length *primes*)) 1800 15 20 (make-color :r 0 :g 255 :b 0 :a 255))
                (draw-circle
                    (- i *cursor*)
                    (flip-y-axis (normalize-to-screen-height prime (elt *primes* (1- (length *primes*)))))
                    1.0
                    (make-color :r 255 :g 0 :b 0 :a 255)))
            (end-drawing))
        (close-window)))

(defun deg->rad (degrees)
    (* degrees (/ pi 180)))

(defparameter *screen-width* 800)
(defparameter *screen-height* 600)

(defun plot(function &key start end step (scale-x 1) (scale-y 1))
    (let* ((camera (make-camera-2d
                        :offset (make-vector2 :x 0 :y *screen-height*)
                        :target (make-vector2 :x 0 :y 0)
                        :rotation 0.0
                        :zoom 1.0))
            (is-dragging nil)
            (mouse-delta nil)
            (world-bottom-left (make-vector2 :x 0 :y 0))
            (world-top-right (make-vector2 :x 0 :y 0))
            (matrix-y-flip '((1 0 0 0)
                                (0 -1 0 0)
                                (0 0 1 0)
                                (0 0 0 1)))
            (matrix-x-scale `((,scale-x 0 0 0)
                                (0 1 0 0)
                                (0 0 1 0)
                                (0 0 0 1)))
            (matrix-y-scale `((1 0 0 0)
                                (0 ,scale-y 0 0)
                                (0 0 1 0)
                                (0 0 0 1)))
            (final-matrix (matrix-multiply
                                (matrix-multiply
                                    matrix-y-flip
                                    matrix-x-scale)
                                matrix-y-scale)))
        (init-window *screen-width* *screen-height* "Plot")
        (set-target-fps 60)
        (loop until (window-should-close)
            doing
            (setf mouse-delta (get-mouse-wheel-move))
            ; (let ((screen-from-world-left (get-screen-to-world-2d (list 0 0) camera))
            ;         (screen-from-world-right (get-screen-to-world-2d (list *screen-width* 0) camera))
            ;         (screen-from-world-top (get-screen-to-world-2d (list *screen-width* 0) camera))
            ;         (screen-from-world-bottom (get-screen-to-world-2d (list *screen-width* 0) camera)))
            ;     (setf (vector2-x world-bounds-x) (vector2-x screen-from-world-left)
            ;             (vector2-y world-bounds-x) (vector2-x screen-from-world-right)))
            (cond ((is-key-down (keyboard-key :right)) (decf (vector2-x (camera-2d-target camera)) 10))
                    ((is-key-down (keyboard-key :left)) (incf (vector2-x (camera-2d-target camera)) 10))
                    ((is-key-down (keyboard-key :up)) (incf (vector2-y (camera-2d-target camera)) 10))
                    ((is-key-down (keyboard-key :down)) (decf (vector2-y (camera-2d-target camera)) 10))
                    ((is-key-pressed (keyboard-key :minus)) (decf (camera-2d-zoom camera) 0.1))
                    ((is-key-pressed (keyboard-key :equal)) (incf (camera-2d-zoom camera) 0.1))
                    ((is-key-down (keyboard-key :h)) (decf (first (first matrix-x-scale)) 0.01))
                    ((is-key-down (keyboard-key :j)) (decf (second (second matrix-y-scale)) 0.01))
                    ((is-key-down (keyboard-key :k)) (incf (second (second matrix-y-scale)) 0.01))
                    ((is-key-down (keyboard-key :l)) (incf (first (first matrix-x-scale)) 0.01))
                    ((is-mouse-button-pressed (mouse-key :left)) (setf is-dragging t))
                    ((is-mouse-button-released (mouse-key :left)) (setf is-dragging nil))
                    ((not (zerop mouse-delta)) (setf (camera-2d-zoom camera)
                                                        (max 0.1 (incf (camera-2d-zoom camera) (* mouse-delta 0.1)))))
                    (is-dragging (setf mouse-delta (get-mouse-delta))
                                    (decf (vector2-x (camera-2d-target camera)) (vector2-x mouse-delta))
                                    (decf (vector2-y (camera-2d-target camera)) (vector2-y mouse-delta)))
                    )
            (begin-drawing)
                (clear-background '(255 255 255 255))
                (begin-mode-2d camera)
                    (draw-line-v (vector2-transform '(0 -1000) matrix-y-flip) (vector2-transform '(0 1000) matrix-y-flip) '(255 0 0 255))
                    (draw-line-v (vector2-transform '(-1000 0) matrix-y-flip) (vector2-transform '(1000 0) matrix-y-flip) '(255 0 0 255))
                    (loop for i from start to end by step
                        doing
                        (draw-pixel-v (vector2-transform
                                            (list i (funcall function i))
                                            (matrix-multiply
                                                (matrix-multiply
                                                    matrix-y-flip
                                                    matrix-x-scale)
                                                matrix-y-scale))
                                            '(0 0 255 255))
                        ; (draw-circle-v (vector2-transform
                        ;                     (list i (funcall function i))
                        ;                     final-matrix) 2.0 '(0 0 255 255))
                    )
                (end-mode-2d)
                (draw-fps 0 0)
                (draw-text (format nil "Zoom = ~a" (camera-2d-zoom camera)) 0 50 25 '(255 0 0 255))
            (end-drawing))
        (close-window)))

(defun square(x)
    (* x x))

(let ((gen (make-prime-generator)))
    (funcall gen 100000)
    (plot gen :start 1 :end 100000 :step 1))

(plot #'log :start 1 :end 1000 :step 1)
(plot #'sqrt :start 1 :end 1000 :step 1)

(plot #'square :start 1 :end 1000 :step 1 :scale-y 1/100 :scale-x 1/10)

(first (cons 1 2))
(second (cons 1 '(2)))

(pairlis '(a b c) '(1 2 3))
(pairlis '(a b c) '(1 2 3))

(defmacro with-members ((type members object) &body body)
    (let ((obj (gensym "OBJECT")))
        `(let ((,obj ,object))
        (let ,(loop for member in members
                    collect
                    `(,member
                        (,(intern (format nil "~A-~A" type member)
                                (symbol-package type))
                        ,obj)))
            ,@body))))

(with-members(color (r g b a) (make-color :r 1 :g 2 :b 3 :a 4))
    (print r)
    (print g)
    (print b)
    (print a))
