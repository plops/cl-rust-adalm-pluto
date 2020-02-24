extern crate core_affinity;
extern crate industrial_io as iio;
use chrono::{DateTime, Utc};
use crossbeam_channel::bounded;
use glium::glutin;
use glium::glutin::event::{Event, WindowEvent};
use glium::glutin::event_loop::{ControlFlow, EventLoop};
use glium::glutin::window::WindowBuilder;
use glium::{Display, Surface};
use imgui::*;
use imgui::{Context, FontConfig, FontGlyphRanges, FontSource, Ui};
use imgui_glium_renderer::Renderer;
use imgui_winit_support::{HiDpiMode, WinitPlatform};
use positioned_io::ReadAt;
use std::collections::VecDeque;
use std::fs::File;
use std::io;
use std::sync::Mutex;
use std::thread::spawn;
use std::time::Instant;
use std::{fs, thread, time};
struct System {
    event_loop: EventLoop<()>,
    display: glium::Display,
    imgui: Context,
    platform: WinitPlatform,
    renderer: Renderer,
    font_size: f32,
}
fn init(title: &str) -> System {
    let title = match title.rfind("/") {
        Some(idx) => title.split_at((idx + 1)).1,
        None => title,
    };
    let event_loop = EventLoop::new();
    let context = glutin::ContextBuilder::new().with_vsync(true);
    let builder = WindowBuilder::new()
        .with_title(title.to_owned())
        .with_inner_size(glutin::dpi::LogicalSize::new(512f64, 512f64));
    let display =
        Display::new(builder, context, &event_loop).expect("failed to initialize display");
    let mut imgui = Context::create();
    imgui.set_ini_filename(None);
    let mut platform = WinitPlatform::init(&mut imgui);
    {
        let gl_window = display.gl_window();
        let window = gl_window.window();
        platform.attach_window(imgui.io_mut(), &window, HiDpiMode::Rounded);
    };
    let renderer = Renderer::init(&mut imgui, &display).expect("failed to initialize renderer");
    return System {
        event_loop,
        display,
        imgui,
        platform,
        renderer,
        font_size: 12.,
    };
}
impl System {
    fn main_loop<F: FnMut(&mut bool, &mut Ui) + 'static>(self, mut run_ui: F) {
        let System {
            event_loop,
            display,
            mut imgui,
            mut platform,
            mut renderer,
            ..
        } = self;
        let mut last_frame = Instant::now();
        event_loop.run(move |event, _, control_flow| match event {
            Event::NewEvents(_) => {
                last_frame = imgui.io_mut().update_delta_time(last_frame);
            }
            Event::MainEventsCleared => {
                let gl_window = display.gl_window();
                platform
                    .prepare_frame(imgui.io_mut(), &(gl_window.window()))
                    .expect("failed to prepare frame");
                gl_window.window().request_redraw();
            }
            Event::RedrawRequested(_) => {
                let mut ui = imgui.frame();
                let mut run = true;
                run_ui(&mut run, &mut ui);
                if !(run) {
                    *control_flow = ControlFlow::Exit;
                };
                let gl_window = display.gl_window();
                let mut target = display.draw();
                target.clear_color_srgb(1.0, 1.0, 1.0, 1.0);
                platform.prepare_render(&ui, gl_window.window());
                let draw_data = ui.render();
                renderer
                    .render(&mut target, draw_data)
                    .expect("rendering failed");
                target.finish().expect("swap buffer failed");
            }
            Event::WindowEvent {
                event: WindowEvent::CloseRequested,
                ..
            } => {
                *control_flow = ControlFlow::Exit;
            }
            event => {
                let gl_window = display.gl_window();
                platform.handle_event(imgui.io_mut(), gl_window.window(), &event);
            }
        });
    }
}
fn main() {
    let (s, r) = crossbeam_channel::bounded(4);
    let history = std::sync::Arc::new(Mutex::new(VecDeque::with_capacity(100)));
    {
        let b = std::thread::Builder::new().name("deque_writer".into());
        let history = history.clone();
        b.spawn(move || {
            loop {
                let tup = r.recv().ok().unwrap();
                let mut h = history.lock().unwrap();
                h.push_back(tup);
                if 100 < h.len() {
                    h.pop_front();
                };
            }
        });
    }
    {
        let core_ids = core_affinity::get_core_ids().unwrap();
        for a in core_ids {
            {
                println!("{} {}:{} affinity  a={:?}", Utc::now(), file!(), line!(), a);
            }
        }
        let b = std::thread::Builder::new().name("pluto_reader".into());
        let reader_thread = b.spawn(move || {
            core_affinity::set_for_current(core_affinity::CoreId { id: 0 });
            let ctx = iio::Context::create_network("192.168.2.1").unwrap_or_else(|err_| {
                {
                    println!(
                        "{} {}:{} couldnt open iio context ",
                        Utc::now(),
                        file!(),
                        line!()
                    );
                }
                std::process::exit(1);
            });
            loop {
                s.send((Utc::now())).unwrap();
            }
        });
    };
    {
        let system = init(file!());
        let history = history.clone();
        system.main_loop(move |_, ui| {
            Window::new(im_str!("Hello world"))
                .size([3.00e+2, 1.00e+2], Condition::FirstUseEver)
                .build(ui, || {
                    ui.text(im_str!("Hello World"));
                    let mouse_pos = ui.io().mouse_pos;
                    ui.text(format!("mouse: ({:.1},{:.1})", mouse_pos[0], mouse_pos[1]));
                });
            Window::new(im_str!("recv"))
                .size([2.00e+2, 1.00e+2], Condition::FirstUseEver)
                .build(ui, || {
                    let h_guard = history.lock().unwrap();
                    let h = h_guard.Deref();
                    let mut time = vec![Utc::now(); h.len()];
                    let mut data_time_between_samples_ms = vec![0.0f32; h.len()];
                    let mut i = 0;
                    for e in h {
                        time[i] = e.0;
                        if (0) == (i) {
                            data_time_between_samples_ms[i] = 0.;
                        } else {
                            let duration = (time[i] - time[(i - 1)]);
                            match duration.num_nanoseconds() {
                                Some(a) => {
                                    data_time_between_samples_ms[i] = ((1.00e-6) * (a as f32));
                                }
                                _ => {}
                            };
                        }
                        i += 1;
                    }
                    {
                        let mut mi = data_time_between_samples_ms[0];
                        let mut ma = data_time_between_samples_ms[0];
                        for e in &(data_time_between_samples_ms) {
                            if *e < mi {
                                mi = *e;
                            };
                            if ma < *e {
                                ma = *e;
                            };
                        }
                        let label =
                            im_str!("time_between_samples_ms [unit] {:12.4?} {:12.4?}", mi, ma);
                        ui.plot_lines(
                            &label,
                            &(data_time_between_samples_ms[0..data_time_between_samples_ms.len()]),
                        )
                        .build();
                    }
                    {
                        let mut mi = data_time_between_samples_ms[0];
                        let mut ma = data_time_between_samples_ms[0];
                        for e in &(data_time_between_samples_ms) {
                            if *e < mi {
                                mi = *e;
                            };
                            if ma < *e {
                                ma = *e;
                            };
                        }
                        let label =
                            im_str!("time_between_samples_ms [unit] {:12.4?} {:12.4?}", mi, ma);
                        ui.plot_lines(
                            &label,
                            &(data_time_between_samples_ms[0..data_time_between_samples_ms.len()]),
                        )
                        .build();
                    };
                });
        });
    }
}
