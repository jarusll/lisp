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

(cffi:defcstruct %color
    (r :unsigned-char)
    (g :unsigned-char)
    (b :unsigned-char)
    (a :unsigned-char))

(defstruct color
    r g b a)

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

(progn
    (init-window 800 600 "Hello, raylib!")
    (set-target-fps 30)
    (loop until (window-should-close)
        doing
        (begin-drawing)
            (cffi:with-foreign-object (c '(:struct %color))
                (setf (cffi:foreign-slot-value c '(:struct %color) 'r) 255
                        (cffi:foreign-slot-value c '(:struct %color) 'g) 0
                        (cffi:foreign-slot-value c '(:struct %color) 'b) 0
                        (cffi:foreign-slot-value c '(:struct %color) 'a) 255)
                (clear-background (cffi:mem-ref c '(:struct %color))))
        (end-drawing))
    (close-window))
