#++
(ql:quickload '(3b-i2c))
(in-package 3b-i2c)

(cffi:defbitfield (i2c-msg-flags :unsigned-short)
  (:RD #x0001)
  (:TEN #x0010)
  (:DMA-SAFE #x0200)
  (:RECV-LEN #x0400)
  (:NO-RD-ACK #x0800)
  (:IGNORE-NAK #x1000)
  (:REV-DIR-ADDR #x2000)
  (:NOSTART #x4000)
  (:STOP #x8000))

(cffi:defbitfield i2c-funcs
  (:I2C #x00000001)
  (:10BIT-ADDR #x00000002)
  (:PROTOCOL-MANGLING #x00000004)
  (:SMBUS-PEC #x00000008)
  (:NOSTART #x00000010)
  (:SLAVE #x00000020)
  (:SMBUS-BLOCK-PROC-CALL #x00008000)
  (:SMBUS-QUICK #x00010000)
  (:SMBUS-READ-BYTE #x00020000)
  (:SMBUS-WRITE-BYTE #x00040000)
  (:SMBUS-READ-BYTE-DATA #x00080000)
  (:SMBUS-WRITE-BYTE-DATA #x00100000)
  (:SMBUS-READ-WORD-DATA #x00200000)
  (:SMBUS-WRITE-WORD-DATA #x00400000)
  (:SMBUS-PROC-CALL #x00800000)
  (:SMBUS-READ-BLOCK-DATA #x01000000)
  (:SMBUS-WRITE-BLOCK-DATA  #x02000000)
  (:SMBUS-READ-I2C-BLOCK #x04000000)
  (:SMBUS-WRITE-I2C-BLOCK #x08000000)
  (:SMBUS-HOST-NOTIFY #x10000000)
  )

(cffi:defcstruct i2c-msg
  (addr :unsigned-short)
  (flags i2c-msg-flags)
  (len :unsigned-short)
  (buf (:pointer :unsigned-char)))

(cffi:defcstruct i2c-rdwr-ioctl-data
  (messages (:pointer (:struct i2c-msg)))
  (count :int))

(defconstant +I2C-RETRIES+ #x0701)
(defconstant +I2C-TIMEOUT+ #x0702)
(defconstant +I2C-SLAVE+ #x0703)
(defconstant +I2C-SLAVE-FORCE+ #x0706)
(defconstant +I2C-TENBIT+ #x0704)
(defconstant +I2C-FUNCS+ #x0705)
(defconstant +I2C-RDWR+ #x0707)
(defconstant +I2C-PEC+ #x0708)
(defconstant +I2C-SMBUS+ #x0720)

(defun device-name (adapter-number)
  (format nil "/dev/i2c-~d" adapter-number))

(defun open-i2c (adapter-number)
  (open (device-name adapter-number) :direction :io 
	:element-type '(unsigned-byte 8)
	:if-exists :overwrite))

(defun close-i2c (i2c)
  (close i2c))

#+sbcl
(defun ioctl (file command &optional arg)
  (sb-posix:ioctl file command arg))

(defmacro with-i2c ((f adapter-number &key device) &body body)
  `(with-open-file (,f (device-name ,adapter-number))
     ,@ (when device
	  (list
	   (alexandria:once-only (device)
	     `(let ((e (ioctl f +i2c-slave+ ,device)))
		(when (minusp e)
		  (error "error ~s connecting to i2c device ~s on adapter ~s"
			 e ,device ,adapter-number))))))
	,@body))


#++
(defun write-i2c (stream reg value)
  (let ((buf (make-array 3 :element-type '(unsigned-byte 8)
			 :initial-contents (list reg
						 (ldb (byte 8 0) value)
						 (ldb (byte 8 8) value)))))
    (write-sequence buf stream)))

#++
(defun read-register (stream device register)
  (cffi:with-foreign-object (p :unsigned-char 2)
    (setf (cffi:mem-ref p) ))
  )

(defun get-functionality (i2c)
  (cffi:with-foreign-object (p 'i2c-funcs)
    (ioctl i2c +i2c-funcs+ (cffi:pointer-address p))
    (cffi:mem-ref p 'i2c-funcs)))


(defun i2c-rdrw (i2c dev &rest messages)
  (when (plusp (length messages))
    (cffi:with-foreign-objects ((m '(:struct i2c-msg) (length messages))
				(d '(:struct i2c-rdwr-ioctl-data)))
      (loop
	 for i from 0
	 for (buf len . .flags) in messages
	 for flags = (cond
		       ((not flags) 0)
		       ((numberp .flags) .flags)
		       (t (cffi:foreign-bitfield-value 'i2c-msg-flags .flags)))
	 do (setf (cffi:mem-aref m '(:struct i2c-msg) i)
		  (list 'addr dev 'flags flags 'len len 'buf buf)))
      (setf (cffi:mem-ref d '(:struct i2c-rdwr-ioctl-data))
	    (list 'messages m 'count (length messages)))
     (ioctl i2c +i2c-rdwr+ (cffi:pointer-address d)))))

(defun write-register-16/ne (i2c dev reg value)
  (cffi:with-foreign-object (buf :unsigned-short 4)
    (setf (cffi:mem-aref buf :unsigned-short 0) reg
	  (cffi:mem-aref buf :unsigned-short 1) value)
    (i2c-rdrw i2c dev (list buf 4))))

(defun read-register-16/ne (i2c dev reg)
  (cffi:with-foreign-objects ((in :unsigned-short 1)
			      (out :unsigned-short 1))
    (setf (cffi:mem-aref out :unsigned-short 0) reg
	  (cffi:mem-aref in :unsigned-short 0) #x3b3b)
    (i2c-rdrw i2c dev
	      (list out 2)
	      (list in 2 :rd :nostart))
    (cffi:mem-ref in :unsigned-short 0)))

(declaim (inline swab16 swab32))
(defun swab16 (x)
  (rotatef (ldb (byte 8 0) x) (ldb (byte 8 8) x))
  x)

(defun write-register-16/be (i2c dev reg value)
  #+:big-endian
  (write-register-16/ne i2c dev reg value)
  #+:little-endian
  (write-register-16/ne i2c dev (swab16 reg) (swab16 value)))

(defun read-register-16/be (i2c dev reg)
  #+:big-endian
  (read-register-16/ne i2c dev reg)
  #+:little-endian
  (swab16 (read-register-16/ne i2c dev (swab16 reg))))

