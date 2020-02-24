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
       (println! (string ,(format nil "{} {}:{} ~a ~{~a~^ ~}"
				  msg
				  (loop for e in rest collect
				       (format nil " ~a={:?}" (emit-rs :code e)))))
		 #+nil (dot (SystemTime--now)
			    (duration_since UNIX_EPOCH)
			    (expect (string "time went backwards"))
			    (as_micros))
		 (Utc--now)
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
#positioned-io = \"*\"
core_affinity = \"*\"
industrial-io = \"*\" # 0.2.0

# this shaves 1MB off the binary
[profile.release]
panic = \"abort\"
"))

  
  
  (let ((code
	 `(do0
	   (do0
	    "extern crate core_affinity;"
	    "extern crate industrial_io as iio;"
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
	   (use (imgui (curly Context FontConfig FontGlyphRanges FontSource Ui))
		(imgui_glium_renderer Renderer)
		(imgui_winit_support (curly HiDpiMode WinitPlatform)))
	   (use (std (curly thread time fs))
		(std fs File)
		(std time Instant)
		(positioned_io ReadAt))

	   (use (crossbeam_channel bounded)
		(std collections VecDeque)
		(std sync Mutex))
	   

	   (defstruct0 System
	       (event_loop "EventLoop<()>")
	     (display "glium::Display")
	     (imgui Context)
	     (platform WinitPlatform)
	     (renderer Renderer)
	     (font_size f32))


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
		 (defun "main_loop<F: FnMut(&mut bool,&mut Ui)+'static>"
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
					   "&mut ui")
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
				 (setf *control_flow "ControlFlow::Exit"))
				(event
				 (let ((gl_window (display.gl_window)))
				   (platform.handle_event (imgui.io_mut)
							  (gl_window.window)
							  &event)))))))))))
	   
	   (defun main ()
	     (let (((values s r) (crossbeam_channel--bounded 4))
		   (history (std--sync--Arc--new (Mutex--new (VecDeque--with_capacity 100)))))
	       (progn
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
			       (loop
				  (dot s
				       (send
					(values (Utc--now)))
				       (unwrap)))))))))))))
	     (progn
	       (let ((system (init (file!)))
		     (history (dot history (clone))))
		 (system.main_loop
		  (space  move
			  (lambda (_ ui)
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
			    (dot ("Window::new" (im_str! (string "recv")))
				 (size (list 200.0 100.0)
				       "Condition::FirstUseEver")
				 ;; https://github.com/Gekkio/imgui-rs/blob/master/imgui-examples/examples/test_window_impl.rs
				 (build ui
					(lambda ()
					  (let ((h_guard (dot history (lock) (unwrap)))
						(h (dot h_guard
							(Deref)
					;(iter)
							)))
					    (let* ((time "vec![Utc::now();h.len()]")
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
					      
					      ,@(loop for timescale in `(1000 8000) appending
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
		  code)))
