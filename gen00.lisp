(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-rust-generator")
  (ql:quickload "cl-ppcre"))

(in-package :cl-rust-generator)

(declaim (optimize (speed 0)
		   (safety 3)
		   (debug 3)))

(progn
  (defparameter *source-dir* #P"/home/martin/stage/cl-rust-adalm-pluto/code/")
  (defparameter *code-file*
    (merge-pathnames #P"src/main.rs"
		     *source-dir*))
  (defun logprint (msg &optional (rest nil))
    `(progn
       (println! (string ,(format nil "{} {}>{} {}:{} ~a ~{~a~^ ~}"
				  msg
				  (loop for e in rest collect
				       (format nil " ~a={:?}" (emit-rs :code e)))))
		 #+nil (dot (SystemTime--now)
			    (duration_since UNIX_EPOCH)
			    (expect (string "time went backwards"))
			    (as_micros))
		 (Utc--now)
		 (std--process--id)
		 (dot (std--thread--current) (name) (expect (string "can't get thread name"))) 
		 #+nil (dot (std--thread--current)
		      (name))
		 (file!)
		 (line!)
		 ,@(loop for e in rest collect
		      e		;`(dot ,e (display))
			))))


  (with-open-file (s (merge-pathnames #P"Cargo.toml"
				      *source-dir*)
		     
		     :direction :output
		     :if-does-not-exist :create
		     :if-exists :supersede)
    (format s "~a"
	    "[package]
name = \"adalm_pluto_viewer\"
version = \"0.1.0\"
authors = [\"Martin Kielhorn <kielhorn.martin@gmail.com>\"]
edition = \"2018\"

# See more keys and their definitions at https://doc.rust-lang.org/cargo/reference/manifest.html

[dependencies]
glium = \"*\"
imgui = \"*\"
imgui-glium-renderer = \"*\"
imgui-winit-support = \"*\"
chrono = \"*\"
crossbeam-channel = \"*\"
crossbeam-utils = \"*\"
#positioned-io = \"*\"
core_affinity = \"*\"
industrial-io = \"*\" # 0.2.0
fftw = \"*\"
num-complex = \"*\"

# this shaves 1MB off the binary
[profile.release]
panic = \"abort\"
"))

  
  (let ((tex-width 128)
	(tex-height 4096))
   (let ((code
	  `(do0
	    (do0
	     "extern crate core_affinity;"
	     "extern crate industrial_io as iio;"
	     "extern crate imgui;"
	     (use (std thread spawn))
	     (use (std io))
	     (use
	      (chrono (curly DateTime Utc)))
	     "use imgui::*;"
	     ,@(loop for e in `("glutin"
				"glutin::event::{Event,WindowEvent}"
				"glutin::event_loop::{ControlFlow,EventLoop}"
				"glutin::window::WindowBuilder"
				"{Display,Surface}")
		  collect
		    `(use (glium ,e))))
	    (use (glium GlObject)
		 (glium backend Facade)
		 (glium texture (curly ClientFormat
				       RawImage2d)))
	    #+nil (use (imgui render)
		       (imgui render renderer TextureId))
	    (use (imgui (curly Context FontConfig FontGlyphRanges FontSource Ui))
		 (imgui_glium_renderer Renderer)
		 (imgui_winit_support (curly HiDpiMode WinitPlatform)))
	    (use (std (curly thread time fs))
		 (std fs File)
		 (std time Instant)
					;(positioned_io ReadAt)
		 )

	    (use (crossbeam_channel bounded)
		 (std collections VecDeque)
		 (std sync Mutex)
		 (fftw)
		 (fftw plan C2CPlan)
		 (num_complex))
	   

	    (space pub
		   (defstruct0 System
		       (event_loop "EventLoop<()>")
		     ("pub display" "glium::Display")
		     (imgui Context)
		     (platform WinitPlatform)
		     (renderer Renderer)
		     (font_size f32)))

	    #+nil (space "impl<'ui> Ui<'ui>"
			 (progn
			   (space pub
				  (defun "draw_image<'p>" ("&self"
							   "texture_id: &'p usize"
							   "size: &'p [f32; 2]")
				    (declare (values "Image<'ui,'p>"))
				    (return (imgui--widget--image--Image--new self texture_id size))))))
	   
	    (defun init ("title: &str")
	      (declare (values System))
	      (let ((title (case (title.rfind (string "/"))
			     ((Some idx) (dot title (split_at (+ idx 1)) 1))
			     (None title)))
		    (event_loop ("EventLoop::new"))
		    (context (dot ("glutin::ContextBuilder::new")
				  (with_vsync true)))
		    (builder (dot ("WindowBuilder::new")
				  (with_title (title.to_owned))
				  (with_inner_size
				   ("glutin::dpi::LogicalSize::new"
				    "512f64"
				    "512f64"))))
		    (display (dot ("Display::new"
				   builder
				   context
				   &event_loop)
				  (expect (string "failed to initialize display"))))
		    (imgui ("Context::create")))
		(declare (mutable imgui))
		(imgui.set_ini_filename None)
		(let* ((platform ("WinitPlatform::init"
				  "&mut imgui")))
		  (progn
		    (let ((gl_window (display.gl_window))
			  (window (gl_window.window)))
		      (platform.attach_window (imgui.io_mut)
					      &window
					      "HiDpiMode::Rounded"))))
		(let ((renderer (dot ("Renderer::init"
				      "&mut imgui"
				      &display)
				     (expect (string "failed to initialize renderer")))))
		  (return (make-instance System
					 event_loop
					 display
					 imgui
					 platform
					 renderer
					 :font_size 12s0)))))


	    (impl System
		  (defun "main_loop<F: FnMut(&mut bool,&mut Ui,&Display)+'static>"
		      (self "mut run_ui: F")
		    (let (((make-instance System
					  event_loop
					  display
					  "mut imgui"
					  "mut platform"
					  "mut renderer"
					  "..") self)
			  (last_frame ("Instant::now")))
		      (declare (mutable last_frame))
		      (dot event_loop
			   (run
			    (space
			     move
			     (lambda (event _ control_flow)
			       (case event
				 (("Event::NewEvents" _)
				  (setf last_frame (dot imgui
							(io_mut)
							(update_delta_time last_frame)))
				  )
				 ("Event::MainEventsCleared"
				  (let ((gl_window (display.gl_window)))
				    (dot platform
					 (prepare_frame (imgui.io_mut)
							(ref (gl_window.window)))
					 (expect (string "failed to prepare frame")))
				    (dot gl_window
					 (window)
					 (request_redraw))))
				 (("Event::RedrawRequested" _)
				  (let* ((ui (imgui.frame))
					 (run true))
				    (run_ui "&mut run"
					    "&mut ui"
					    "&self.display"
					    )
				    (unless run
				      (setf *control_flow
					    "ControlFlow::Exit"))
				    (let ((gl_window (display.gl_window)))
				      (let* ((target (display.draw)))
					(target.clear_color_srgb 1s0 1s0 1s0 1s0)
					(platform.prepare_render &ui
								 (gl_window.window))
					(let ((draw_data (ui.render)))
					  (dot renderer
					       (render "&mut target"
						       draw_data)
					       (expect (string "rendering failed")))
					  (dot target
					       (finish)
					       (expect (string "swap buffer failed"))))))))
				 ((make-instance "Event::WindowEvent"
						 :event "WindowEvent::CloseRequested"
						 "..")
				  ,(logprint "shutting down" `())
				  (setf *control_flow "ControlFlow::Exit"))
				 (event
				  (let ((gl_window (display.gl_window)))
				    (platform.handle_event (imgui.io_mut)
							   (gl_window.window)
							   &event)))))))))))
	   
	    (defun main ()
	      (let (   ; ((values s r) (crossbeam_channel--bounded 3))
		    #+nil (history (std--sync--Arc--new (Mutex--new (VecDeque--with_capacity 100)))))
		#+nil (progn
			(let ((b (dot (std--thread--Builder--new)
				      (name (dot (string "deque_writer")
						 (into)))))
			      (history (dot history (clone))))
			  (b.spawn
			   (space move
				  (lambda ()
				    (loop
				       (let ((tup (dot r
						       (recv)
						       (ok)
						       (unwrap))))
					 (let* ((h (dot history
							(lock)
							(unwrap))))
					   (dot
					    h
					    (push_back tup))
					   (when (< 100 (h.len))
					     (h.pop_front))))))))))
	       

		(progn
		  (let ((core_ids (dot (core_affinity--get_core_ids)
				       (unwrap))))
		    (for (a core_ids)
			 ,(logprint "affinity" `(a))))
		  (let ((b (dot (std--thread--Builder--new)
				(name (dot (string "pluto_reader")
					   (into)))))
			(reader_thread
			 (b.spawn
			  (space
			   move
			   (lambda ()
			     (do0
			      (core_affinity--set_for_current (make-instance core_affinity--CoreId :id 0))
			      ;; https://github.com/fpagliughi/rust-industrial-io/blob/master/examples/riio_detect.rs
			      (let ((ctx (dot (iio--Context--create_network (string "192.168.2.1"))
					      (unwrap_or_else (lambda (err_)
								,(logprint "couldnt open iio context")
								(std--process--exit 1))))))
				(let* ((trigs (Vec--new)))
				  (for (dev (ctx.devices))
				       (if (dev.is_trigger)
					   (case (dev.id)
					     ((Some id) (trigs.push id))
					     (None "()"))
					   (println! (string "{} [{}]: {} channels")
						     (dot dev
							  (id)
							  (unwrap_or_default))
						     (dot dev
							  (name)
							  (unwrap_or_default))
						     (dot dev
							  (num_channels)))))
				 
				  (if (trigs.is_empty)
				      ,(logprint "no triggers" `())
				      (for (s trigs)
					   (println! (string "trigger {}")
						     s))))

				(let (,@ (loop for (var name) in `((dev cf-ad9361-lpc)
								   (phy ad9361-phy))
					    collect
					      `(,var (dot ctx
							  (find_device (string ,name))
							  (unwrap_or_else
							   (lambda ()
							     ,(logprint (format nil "no device named ~a" name) `())
							     (std--process--exit 2))))))
				      ) 

				  (let* ((nchan 0))
				    (for ("mut chan" (dev.channels))
					 (when (== (Some (std--any--TypeId--of--<i16>))
						   (chan.type_of))
					   (incf nchan)
					   (chan.enable)))
				    (if (== 0 nchan)
					(do0
					 ,(logprint "no 16 bit channels found" `())
					 (std--process--exit 1))
					,(logprint "16 bit channels found" `(nchan))))

				  ;; https://users.rust-lang.org/t/sharing-buffer-between-threads-without-locking/10508
				  ;; https://docs.rs/triple_buffer/5.0.4/triple_buffer/
				  ;; https://medium.com/@polyglot_factotum/rust-concurrency-patterns-communicate-by-sharing-your-sender-11a496ce7791
				  ;; https://wiki.analog.com/resources/tools-software/linux-software/libiio_internals
				  ;; 2017-03 https://users.rust-lang.org/t/spmc-buffer-triple-buffering-for-multiple-consumers/10118
				  ;; 2017-11 https://users.rust-lang.org/t/code-review-triplebuffer-for-sending-huge-objects-between-threads/13787/7
				  ;; https://github.com/HadrienG2/triple-buffer consumer is not in sync with producer
				  ;; https://doc.rust-lang.org/book/ch16-02-message-passing.html
				  ;; https://stjepang.github.io/2019/01/29/lock-free-rust-crossbeam-in-2019.html scoped thread, atomic cell
				  ,(let ((n-buf 3)
					 (n-samples 512))
				     `(do0
				       ;; https://users.rust-lang.org/t/how-can-i-allocate-aligned-memory-in-rust/33293 std::slice::from_raw_parts[_mut]
				       (defstruct0 SendComplex
					   (timestamp "DateTime<Utc>")
				       
					 (ptr
					  "fftw::array::AlignedVec<num_complex::Complex<f64>>"
					;"*mut num_complex::Complex<f64>"
					  ))
				       "unsafe impl Send for SendComplex {}"
				       (let (((values s r) (crossbeam_channel--bounded 3))
					     )

					 (let* ((buf (dot dev
							  ;; cyclic buffer only makes sense for output (to repeat waveform)
							  (create_buffer ,n-samples false)
							  (unwrap_or_else (lambda (err)
									    ,(logprint (format nil "can't create buffer") `(err))
									    (std--process--exit 3)))))
					     
						(fftin (list ,@(loop for i below n-buf collect
								    `(std--sync--Arc--new
								      (Mutex--new
								       (make-instance SendComplex :timestamp (Utc--now) :ptr (fftw--array--AlignedVec--new ,n-samples))
								       )))))
						(fftout (list ,@(loop for i below n-buf collect
								     `(std--sync--Arc--new
								       (Mutex--new
									(make-instance SendComplex :timestamp (Utc--now) :ptr (fftw--array--AlignedVec--new ,n-samples))
									)))))
					     
						(chans (Vec--new))
					;(count 0)
						)

					   (let* ()
					     (do0 
					      (for (ch (dev.channels))
						   (chans.push ch))
					      (dot (crossbeam_utils--thread--scope
						    (lambda (scope)
						      (scope.spawn (lambda (_)
								     ,(logprint "start fftw plan" `())
								     (let* ((plan (dot (fftw--plan--C2CPlan--aligned ,(format nil "&[~a]" n-samples)
														     fftw--types--Sign--Forward
														     fftw--types--Flag--Measure)
										       (unwrap))))
								       (declare (type fftw--plan--C2CPlan64 plan))
								       ,(logprint "finish fftw plan" `())
								       (loop
									  (let ((tup (dot r
											  (recv)
											  (ok)
											  (unwrap))))
									    (declare (type usize tup))
									    (let* ((ha (dot (aref fftin tup)
											    (clone)
											    ))
										   (a (space "&mut" (dot ha
													 (lock)
													 (unwrap)
													 )))
										   (hb (dot (aref fftout tup)
											    (clone)))
										   (b (space "&mut" (dot hb
													 (lock)
													 (unwrap)))))
									      (do0
									       (dot plan
										    (c2c "&mut a.ptr" "&mut b.ptr")
										    (unwrap))
									       (setf b.timestamp (Utc--now)))
									      ,(logprint "" `(tup (- b.timestamp
												     a.timestamp)
												  (aref b.ptr 0)))))))))
						      (let* ((count 0))
							(loop
							   (case (buf.refill)
							     ((Err err)
							      ,(logprint "error filling buffer" `(err))
							      (std--process--exit 4))
							     (t "()"))
							   ;; https://users.rust-lang.org/t/solved-how-to-move-non-send-between-threads-or-an-alternative/19928
							   (progn
							     (let ((time_acquisition (Utc--now)))
							       (let* ((ha (dot (aref fftin count)
									       (clone)))
								      (a (space "&mut" (dot ha
											    (lock)
											    (unwrap)))))
								 (let ((data_i (dot buf
										    (channel_iter--<i16> (ref (aref chans 0)))
										    (collect)))
								       (data_q (dot buf
										    (channel_iter--<i16> (ref (aref chans 1)))
										    (collect))))
								   (declare (type Vec<i16> data_i data_q))
								   (do0
								    (setf a.timestamp time_acquisition)
								    (for (i (slice 0 ,n-samples))
									 (setf (aref a.ptr i) (fftw--types--c64--new (coerce (aref data_i i)
															     f64)
														     (coerce (aref data_q i)
															     f64)))))))))
							   ,(logprint "sender" `(count ))
							   (dot s
								(send count)
								(unwrap))
							   (incf count)
							   (when (<= ,n-buf count)
							     (setf count 0))))))
						   (unwrap)))))))))))))))))))
	      (progn
		(let* ((system (init (file!)))
		       )
		 
		 
		  ;; https://github.com/glium/glium/blob/master/examples/blitting.rs
		  (let* ((data (Vec--with_capacity (* ,tex-width ,tex-height))))
		    ;; https://github.com/Gekkio/imgui-rs/blob/master/imgui-examples/examples/custom_textures.rs
		    (for (i (slice 0 ,tex-width))
			 (for (j (slice 0 ,tex-height))
			      (data.push (coerce j u8))
			      (data.push (coerce i u8))
			      (data.push (coerce (+ i j) u8))
			      ))
		    (let (	   ;(ctx (system.display.get_context))
			  (textures (system.renderer.textures))
			  (raw (make-instance glium--texture--RawImage2d
					      :data (std--borrow--Cow--Owned data)
					      :width (coerce ,tex-width u32)
					      :height (coerce ,tex-height u32)
					      :format glium--texture--ClientFormat--U8U8U8))
			  (gl_texture (dot (glium--texture--Texture2d--new (system.display.get_context)
									   raw)
					   (expect (string "new 2d tex"))))
			  (texture_id (textures.insert (std--rc--Rc--new gl_texture)))
			  (my_texture_id (Some texture_id)))))
		  #+nil (let ((empty_texture (dot (glium--texture--Texture2d--empty_with_format
						   &system.display
						   glium--texture--UncompressedFloatFormat--U8U8U8U8
						   glium--texture--MipmapsOption--NoMipmap
						   128
						   128)
						  (unwrap)))
			      ;; this needs change in glium/src/lib.rs (add pub infront of "mod render" and "mod widget")
			      (texture_id (imgui--render--renderer--TextureId--from
					   (coerce (empty_texture.get_id)
						   usize)))
			      )
		   
		 
			  ,(logprint "generated texture" `((empty_texture.get_id) texture_id))
			  (do0
			   ,(logprint "clear texture" `())
			   (dot empty_texture
				(as_surface)
				(clear_color 0s0 0s0 0s0 1s0))))
		  (system.main_loop
		   (space  move
			   (lambda (_ ui display
				    ;"_ : &bool"
				    ;"ui : &Ui"
				    ;"display : &Display"
				    )
			     ;(declare (type Display display))
			     (dot ("Window::new" (im_str! (string "Hello world")))
				  (size (list 300.0 100.0) "Condition::FirstUseEver")
				  (build ui
					 (lambda ()
					   (ui.text (im_str! (string "Hello World")))
					   (let ((mouse_pos (dot ui
								 (io)
								 mouse_pos)))
					     (ui.text (format!
						       (string "mouse: ({:.1},{:.1})")
						       (aref mouse_pos 0)
						       (aref mouse_pos 1)))))))
			     (dot ("Window::new" (im_str! (string "texture")))
				  (size (list 200.0 100.0)
					"Condition::FirstUseEver")

				  (build ui
					 (lambda ()
					   ;; https://github.com/glium/glium/blob/master/tests/texture_creation.rs
					   ;; https://github.com/ocornut/imgui/wiki/Image-Loading-and-Displaying-Examples
					   ;; https://medium.com/@tomaka/the-glium-library-5be149d87dc1


					   (let* ((data (Vec--with_capacity (* ,tex-width ,tex-height))))
					     ;; https://github.com/Gekkio/imgui-rs/blob/master/imgui-examples/examples/custom_textures.rs
					     (for (i (slice 0 ,tex-width))
						  (for (j (slice 0 ,tex-height))
						       (data.push (coerce j u8))
						       (data.push (coerce i u8))
						       (data.push (coerce (+ i j) u8))
						       ))
					     (let
						 ((raw (make-instance glium--texture--RawImage2d
								      :data (std--borrow--Cow--Owned data)
								      :width (coerce ,tex-width u32)
								      :height (coerce ,tex-height u32)
								      :format glium--texture--ClientFormat--U8U8U8))
						  (data_raw (coerce (raw.data.as_ptr)
								    "*const std::ffi::c_void")))
					       ))

					   ;; make ctx pub in imgui/src/lib.rs
					   ;; make gl pub in glium/src/context/mod.rs
					   ;; also mod gl in glium/src/lib.rs
					   (let ((ctx (display.get_context)))
					    (make-instance unsafe
							   (ctx.gl.TexSubImage2D glium--gl--TEXTURE_2D
										 0 0 0
										 ,tex-width
										 ,tex-height
										 glium--gl--RGBA

										 glium--gl--UNSIGNED_BYTE
										 data_raw
										 )))
					   (let ((img (imgui--widget--image--Image--new texture_id (list ,(* 1s0 tex-width) ,(* 1s0 tex-height)))))
					     (img.build ui))
					   #+nil (let ((system_guard (system_orig.clone))
						       (system (dot system_guard (lock) (unwrap)))
						       (texture (glium--texture--Texture2d--empty_with_format
								 &system.display
								 glium--texture--UncompressedFloatFormat--U8U8U8U8
								 glium--texture--MipmapsOption--NoMipmap
								 8192
								 128))))
					   #+nil (let ((h_guard (dot history (lock) (unwrap)))
						       #+nil
						       (h (dot h_guard
							       (Deref)
					;(iter)
							       )))
						   #+nil (let* ((time "vec![Utc::now();h.len()]")
								(data_time_between_samples_ms "vec![0.0f32;h.len()]")
								)

							   (let* ((i 0))
							     (for (e h)
								  (setf (aref time i) e.0)
								  (if (== 0 i)
								      (setf (aref data_time_between_samples_ms i) 0s0)
								      (let ((duration #+nil
									      (dot (aref time i)
										   (signed_duration_since
										    (aref time (- i 1))))
									      (- (aref time i)
										 (aref time (- i 1)))))
									;; (as_fractional_millis)
									;; (as_seconds)
									(case (dot
									       duration
									       (num_nanoseconds))
									  ((Some a)
									   (setf (aref data_time_between_samples_ms i)
										 (* 1e-6 (coerce
											  a
											  f32))
										 ))
									  (t )))) 
						     
								  (incf i)))
					      
							   ,@(loop for timescale in `(100) appending
								  (loop for (name f) in `(("time_between_samples_ms" "_"))
								     collect
								       (let ((dat (format nil "data_~a" name)))
									 `(progn
									    (let* ((mi (aref ,dat 0))
										   (ma (aref ,dat 0)))
									      (for (e (ref ,dat))
										   (when (< *e mi)
										     (setf mi *e))
										   (when (< ma *e)
										     (setf ma *e)))
									      (let ((label (im_str! (string ,(format nil "~a [unit] {:12.4?} {:12.4?}"
														     name
														     ))
												    mi ma)))
										(dot ui (plot_lines
											 &label
											 (ref (aref ,dat
												    (slice
												     ;;std--cmp--max 0
												     0 ;(- (dot ,dat (len)) ,timescale)
											
												     (dot ,dat (len))))))
										     (build))))))))))))))))))))))

    
    
     (write-source *code-file*
		   code))))
