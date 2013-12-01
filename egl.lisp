
;; the following code must be run once, to parse the egl headers and
;; convert the resulting ffi files into the ccl database that makes
;; the foreign functions available

#+nil
(create-interfaces "raspberry-pi-vc")
#+nil
(require "parse-ffi")
#+nil
(ccl::parse-standard-ffi-files :raspberry-pi-vc)

#.(load "/home/pi/quicklisp/setup.lisp")
(eval-when (:compile-toplevel :execute :load-toplevel)
  (ccl:use-interface-dir "raspberry-pi-vc")
  (progn
    (open-shared-library "libGLESv2.so")
    (open-shared-library "libEGL.so")
    (open-shared-library "libbcm_host.so")))

(eval-when (:compile-toplevel :execute :load-toplevel)
  ;; this needs to be done after libGLESv2 is loaded!
  (ql:quickload "cl-opengl"))

(defpackage :g (:use :cl :gl :ccl))
(in-package :g)


(defmacro init-vars ()
  `(progn ,@(loop for i in '(*surface* *dispman-element* *display* *context* *size-screen*
			     *size* *dispman-update* *dispman-display* *config* *run-gl*) collect
		 `(defvar ,i nil))))

(init-vars)

#.(defparameter *attrib-list* (list #$EGL_RED_SIZE 8
				  #$EGL_GREEN_SIZE 8
				  #$EGL_BLUE_SIZE 8
				  #$EGL_ALPHA_SIZE 8
				  #$EGL_SURFACE_TYPE #$EGL_WINDOW_BIT
				  #$EGL_NONE))

(defun init-egl ()
  (#_bcm_host_init)
  (defparameter *display* (#_eglGetDisplay #$EGL_DEFAULT_DISPLAY))
  (assert (not (= (%ptr-to-int #$EGL_NO_DISPLAY) (%ptr-to-int *display*))))
  (assert (/= #$EGL_FALSE (#_eglInitialize *display* (%null-ptr) (%null-ptr))))
  
  (defparameter *config*
    (rletz ((pconfig #>EGLConfig)
	    (pnum-config #>EGLint)
	    (attribs (:array #>EGLint #.(length *attrib-list*))))
      (loop for i from 0 and e in *attrib-list* do
	   (setf (paref attribs #>EGLint i) e))
  
      (assert (/= #$EGL_FALSE (#_eglChooseConfig *display*
						 attribs
						 pconfig 1 pnum-config)))
      (let ((config (pref pconfig #>EGLConfig)))
	config)))

  (defparameter *context* (#_eglCreateContext *display* *config* #$EGL_NO_CONTEXT (%null-ptr)))
  (assert (not (= (%ptr-to-int #$EGL_NO_CONTEXT) (%ptr-to-int *context*))))

  (defparameter *size-screen*
    (rletz ((pwidth :uint32_t)
	    (pheight :uint32_t))
      (assert (<= 0 (#_graphics_get_display_size 0 pwidth pheight)))
      (list (pref pwidth :uint32_t)
	    (pref pheight :uint32_t))))
  (defparameter *size* (list 840 (second *size-screen*)))

  (defparameter *dispman-display* (#_vc_dispmanx_display_open 0))
  (defparameter *dispman-update* (#_vc_dispmanx_update_start 10))
  (assert (/= #$DISPMANX_NO_HANDLE *dispman-update*))

  (destructuring-bind (width height) *size*
    (rletz ((dst #>VC_RECT_T :x (- (first *size-screen*) width) :y (- (second *size-screen*) height) :width width :height height)
	    (src #>VC_RECT_T :x 0 :y 0 :width (ash width 16) :height (ash height 16)))
      (defparameter *dispman-element* (#_vc_dispmanx_element_add *dispman-update*
								 *dispman-display*
								 2000 dst (%null-ptr) src
								 #$DISPMANX_PROTECTION_NONE
								 (%null-ptr) (%null-ptr) 0))))
  (#_vc_dispmanx_update_submit_sync *dispman-update*)
  (defparameter *run-gl* t))

#+nil
(defparameter *run-gl* nil) ;; execute this, to close the display

(let ((v 0))
 (defun draw ()
   (incf v 6)
   (when (< 360 v) (setf v 0))
   (#_glClearColor (coerce (* .3 (1+ (sin (* pi v (/ 180f0)))))
			   'single-float) .0f0 .0f0 .0f0)
      (#_glClear #$GL_COLOR_BUFFER_BIT)
   (#_glMatrixMode #$GL_MODELVIEW)
   
   (progn
	(#_glLoadIdentity)
	;;	(#_glScalef .2f0 10f0 1f0)
	(scale .2 10 1) ;; this is just an example, that it is possible to use cl-opengl calls

   (#_glTranslatef (coerce (* 20f0 (sin (* pi (/ 180) v))) 'single-float) 0f0 -10f0)
   (rletz ((quadx (:array #>GLbyte #.(* 4 3))))
     (let ((i 0))
      (loop for  (x y) in '((-1 -1)
				       (1 -1)
				       (-1 1)
				       (1 1))
	 do (loop for e in (list x y 0) do
		 (setf (paref quadx #>GLbyte i) e)
		 (incf i))))
     (#_glEnableClientState #$GL_VERTEX_ARRAY)
     (#_glVertexPointer 3 #$GL_BYTE 0 quadx)
     (#_glColor4f .9f0 .3f0 .3f0 1f0)
     (#_glDrawArrays #$GL_TRIANGLE_STRIP 0 4)))
   (#_eglSwapBuffers *display* *surface*)))

(defun init-surface ()
 (destructuring-bind (width height) *size*
   (rletz ((pnative-window #>EGL_DISPMANX_WINDOW_T :element *dispman-element*
			   :width width
			   :height height))
     (defparameter *surface* (#_eglCreateWindowSurface *display* *config* pnative-window (%null-ptr)))
     (assert (/= (%ptr-to-int #$EGL_NO_SURFACE) (%ptr-to-int *surface*)))


     (assert (/= (#_eglMakeCurrent *display* *surface* *surface* *context*) #$EGL_FALSE))
   
     (#_glClearColor .9f0 .25f0 .35f0 1.0f0)
     (#_glMatrixMode #$GL_MODELVIEW)
     (#_glViewport 0 0 (first *size*) (second *size*))
     (#_glMatrixMode #$GL_PROJECTION)
     (#_glLoadIdentity)
     (let* ((nearp 1.0) (farp 500.0) 
	    (hht (coerce (* nearp (tan (* 45 .5 (/ 180) pi))) 'single-float))
	    (hwd (coerce (* hht (first *size*) (/ (second *size*)
						  )) 'single-float)))
       (#_glFrustumf (- hwd) hwd (- hht) hht nearp farp))
   
     (loop while  *run-gl* do 
	  (draw)))))

(defun uninit-egl ()
  (defparameter *run-gl* nil) (sleep .1)  
  (assert (/= #$EGL_FALSE (#_eglMakeCurrent *display* #$EGL_NO_SURFACE 
					    #$EGL_NO_SURFACE #$EGL_NO_CONTEXT)))
  (assert (/= #$EGL_FALSE (#_eglDestroySurface *display* *surface*)))
  (assert (= 0 (#_vc_dispmanx_element_remove *dispman-update* *dispman-element*)))
  (#_vc_dispmanx_update_submit_sync *dispman-update*)
  (assert (= 0 (#_vc_dispmanx_display_close *dispman-display*)))
  (assert (/= #$EGL_FALSE (#_eglDestroyContext *display* *context*)))
  (assert (= #$EGL_TRUE (#_eglTerminate *display*)))
  ;; the following must be run in the same thread that originally initialized egl
  (assert (= #$EGL_TRUE (#_eglReleaseThread))))

#+nil
(progn ;; run this to display a moving bar on the screen
  (init-egl)
  (init-surface)
  (uninit-egl))
