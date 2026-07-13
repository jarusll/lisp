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

(defparameter *points* ())
(defparameter *primes* ())

(defun make-prime-generator()
    (let ((primes ())
            (cursor 1))
        (lambda()
            (loop until
                (progn
                    (incf cursor)
                    (loop for i in primes
                        while (<= i (isqrt cursor))
                        never (zerop (mod cursor i)))))
            (setf primes (nconc primes (list cursor)))
            cursor)))
(defmacro with-gensyms(symbols &body body)
    `(let ,(loop for sym in symbols collect `(,sym (gensym)))
        ,@body))
(defmacro do-primes-n ((var count) &body body)
    (with-gensyms(counter prime-generator)
        `(let ((,prime-generator (make-prime-generator)))
            (dotimes (,counter ,count)
                (let ((,var (funcall ,prime-generator)))
                    ,@body)))))

(progn
    (setf *points* ())
    (setf *primes* ())
    (let ((x 1))
        (do-primes-n (p 1000)
            (setf *primes* (nconc *primes* (list p)))
            (setf *points* (nconc *points* (list (list x p))))
            (incf x)))
    (print *primes*)
    (init-window 800 600 "Hello, raylib!")
    (set-target-fps 30)
    (loop until (window-should-close)
        doing
        (begin-drawing)
            (clear-background (make-color :r 255 :g 255 :b 0 :a 255))
            (loop for (x prime) in *points* doing
                (draw-circle
                    x
                    (flip-y-axis (normalize-to-screen-height prime (car (last *primes*))))
                    1.0
                    (make-color :r 255 :g 0 :b 0 :a 255)))
        (end-drawing))
    (close-window))

(defun flip-y-axis(y)
    (- 600 1 y))

(defun normalize-to-screen-height(value maximum)
    (round (* value (/ (1- 600) maximum))))

*primes*
