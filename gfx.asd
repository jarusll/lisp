(asdf:defsystem "gfx"
  :description "Software renderer"
  :author "Suraj Yadav"
  :license "MIT"
  :serial t

  :depends-on
  (:cffi
   :cffi-libffi)

  :components
  ((:file "libraylib")
   (:file "renderer")))
