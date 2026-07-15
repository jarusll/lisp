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

(cffi:defcstruct (%color :class color-type)
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

(defmethod cffi:translate-into-foreign-memory
    ((value vector3) (type vector3-type) pointer)
    (cffi:with-foreign-slots ((x y z) pointer (:struct %vector3))
        (setf x (vector3-x value)
                y (vector3-y value)
                z (vector3-z value))))

(defmethod cffi:translate-into-foreign-memory
    ((value list) (type vector3-type) pointer)
    (cffi:with-foreign-slots ((x y z) pointer (:struct %vector3))
        (setf x (first value)
                y (second value)
                z (third value))))

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

; void DrawPixel(int posX, int posY, Color color) // Draw a pixel using geometry [Can be slow, use with care]
(cffi:defcfun ("DrawPixel" draw-pixel) :void
    (posX :int)
    (posY :int)
    (color (:struct %color)))

; bool IsKeyPressed(int key) // Check if a key has been pressed once
(cffi:defcfun ("IsKeyPressed" is-key-pressed) :bool
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

; void BeginMode2D(Camera2D camera) // Begin 2D mode with custom camera (2D)
(cffi:defcfun ("BeginMode2D" begin-mode-2d) :void
    (camera (:struct %camera-2d)))

; void EndMode2D(void) // Ends 2D mode with custom camera
(cffi:defcfun ("EndMode2D" end-mode-2d) :void)

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

(defparameter *points* (make-array 1000000 :fill-pointer 0 :adjustable t :element-type 'fixnum))
(defparameter *primes* (make-array 1000000 :fill-pointer 0 :adjustable t :element-type 'fixnum))
(defparameter *cursor* 0)
(defparameter *sensitivity* 100)

(setq primes (make-array 10000 :fill-pointer 0 :adjustable t :element-type 'fixnum)
    cursor 1)
(prime-add 2)
(prime-next)

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

(defparameter *screen-width* 800)
(defparameter *screen-height* 600)


(defun push-prime(p)
    (vector-push-extend p *primes*))

(defun main()
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

(main)

(let ((gen (make-prime-generator)))
    (sb-ext:gc :full t)
    (let ((sb-ext:*gc-run-time* 0))
        (time (funcall gen 10000000))
    ))

(let ((camera (make-camera-2d :offset (make-vector2 :x 0 :y *screen-height*) :target (make-vector2 :x 0 :y 0) :rotation 0.0 :zoom 1.0)))
    (init-window *screen-width* *screen-height* "Primes!!!")
    (set-target-fps 60)
    (loop until (window-should-close)
        doing
        (cond ((is-key-pressed (keyboard-key :right)) (decf (vector2-x (camera-2d-target camera)) 10))
                ((is-key-pressed (keyboard-key :left)) (incf (vector2-x (camera-2d-target camera)) 10))
                ((is-key-pressed (keyboard-key :up)) (incf (vector2-y (camera-2d-target camera)) 10))
                ((is-key-pressed (keyboard-key :down)) (decf (vector2-y (camera-2d-target camera)) 10)))
        (begin-drawing)
            (clear-background '(255 255 0 255))
            (begin-mode-2d camera)
                (draw-circle 100 -100 100.0 '(255 0 0 255))
            (end-mode-2d)
            (draw-fps 0 0)
        (end-drawing))
    (close-window))

(describe 'cffi:translate-into-foreign-memory)
(cffi::parse-type '(:struct %vector2))
