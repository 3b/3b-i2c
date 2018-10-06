(asdf:defsystem 3b-i2c
  :description "wip CL library for using i2c devices"
  :license "MIT"
  :author "Bart Botta <00003b at gmail.com>"
  :depends-on (cffi trivial-features alexandria)
  :serial t
  :components ((:file "package")
	       (:file "i2c")))
