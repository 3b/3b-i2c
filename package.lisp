(defpackage #:3b-i2c
  (:use :cl)
  (:export #:open-i2c
	   #:close-i2c
	   #:read-register-16/be
	   #:write-register-16/be
	   #:swab16
	   #:get-functionality))
