extern crate core_affinity;
extern crate imgui;
extern crate industrial_io as iio;
use chrono::{DateTime, Utc};
use crossbeam_channel::bounded;
use fftw;
use fftw::plan::C2CPlan;
use glium::backend::Facade;
use glium::glutin;
use glium::glutin::event::{Event, WindowEvent};
use glium::glutin::event_loop::{ControlFlow, EventLoop};
use glium::glutin::window::WindowBuilder;
use glium::texture::{ClientFormat, RawImage2d};
use glium::GlObject;
use glium::{Display, Surface};
use imgui::*;
use imgui::{Context, FontConfig, FontGlyphRanges, FontSource, Ui};
use imgui_glium_renderer::Renderer;
use imgui_winit_support::{HiDpiMode, WinitPlatform};
use num_complex;
use std::collections::VecDeque;
use std::fs::File;
use std::io;
use std::sync::Mutex;
use std::thread::spawn;
use std::time::Instant;
use std::{fs, thread, time};
pub struct System {
    event_loop: EventLoop<()>,
    pub display: glium::Display,
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
    fn main_loop<F: FnMut(&mut bool, &mut Ui, &Display) + 'static>(self, mut run_ui: F) {
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
                run_ui(&mut run, &mut ui, &self.display);
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
                {
                    println!(
                        "{} {}>{} {}:{} shutting down ",
                        Utc::now(),
                        std::process::id(),
                        std::thread::current()
                            .name()
                            .expect("can't get thread name"),
                        file!(),
                        line!()
                    );
                }
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
    {
        let core_ids = core_affinity::get_core_ids().unwrap();
        for a in core_ids {
            {
                println!(
                    "{} {}>{} {}:{} affinity  a={:?}",
                    Utc::now(),
                    std::process::id(),
                    std::thread::current()
                        .name()
                        .expect("can't get thread name"),
                    file!(),
                    line!(),
                    a
                );
            }
        }
        let b = std::thread::Builder::new().name("pluto_reader".into());
        let reader_thread  = b.spawn(move ||{
                                    core_affinity::set_for_current(core_affinity::CoreId {id: 0});
                        let ctx  = iio::Context::create_network("192.168.2.1").unwrap_or_else(| err_|{
                                {
                                        println!("{} {}>{} {}:{} couldnt open iio context ", Utc::now(), std::process::id(), std::thread::current().name().expect("can't get thread name"), file!(), line!());
}
                                std::process::exit(1);
});
                        let mut trigs  = Vec::new();
            for  dev in ctx.devices() {
                                if  dev.is_trigger()  {
                                        match dev.id() {
                                                Some(id) => {
                                                trigs.push(id)
},
                                                None => {
                                                ()
},
}
} else {
                                        println!("{} [{}]: {} channels", dev.id().unwrap_or_default(), dev.name().unwrap_or_default(), dev.num_channels());
}
}
            if  trigs.is_empty()  {
                                {
                                        println!("{} {}>{} {}:{} no triggers ", Utc::now(), std::process::id(), std::thread::current().name().expect("can't get thread name"), file!(), line!());
}
} else {
                                for  s in trigs {
                                        println!("trigger {}", s);
}
};
                        let dev  = ctx.find_device("cf-ad9361-lpc").unwrap_or_else(||{
                                {
                                        println!("{} {}>{} {}:{} no device named cf-ad9361-lpc ", Utc::now(), std::process::id(), std::thread::current().name().expect("can't get thread name"), file!(), line!());
}
                                std::process::exit(2);
});
            let phy  = ctx.find_device("ad9361-phy").unwrap_or_else(||{
                                {
                                        println!("{} {}>{} {}:{} no device named ad9361-phy ", Utc::now(), std::process::id(), std::thread::current().name().expect("can't get thread name"), file!(), line!());
}
                                std::process::exit(2);
});
                        let mut nchan  = 0;
            for  mut chan in dev.channels() {
                                if  (Some(std::any::TypeId::of::<i16>()))==(chan.type_of())  {
                                                            nchan += 1 ;
                    chan.enable();
};
}
            if  (0)==(nchan)  {
                                                {
                                        println!("{} {}>{} {}:{} no 16 bit channels found ", Utc::now(), std::process::id(), std::thread::current().name().expect("can't get thread name"), file!(), line!());
}
                std::process::exit(1);
} else {
                                {
                                        println!("{} {}>{} {}:{} 16 bit channels found  nchan={:?}", Utc::now(), std::process::id(), std::thread::current().name().expect("can't get thread name"), file!(), line!(), nchan);
}
};
                                    struct SendComplex {
                                timestamp: DateTime<Utc>,
                                ptr: fftw::array::AlignedVec<num_complex::Complex<f64>>,
}
            unsafe impl Send for SendComplex {}
                        let (s, r)  = crossbeam_channel::bounded(3);
                        let mut buf  = dev.create_buffer(512, false).unwrap_or_else(| err|{
                                {
                                        println!("{} {}>{} {}:{} can't create buffer  err={:?}", Utc::now(), std::process::id(), std::thread::current().name().expect("can't get thread name"), file!(), line!(), err);
}
                                std::process::exit(3);
});
            let mut fftin  = [std::sync::Arc::new(Mutex::new(SendComplex {timestamp: Utc::now(), ptr: fftw::array::AlignedVec::new(512)})), std::sync::Arc::new(Mutex::new(SendComplex {timestamp: Utc::now(), ptr: fftw::array::AlignedVec::new(512)})), std::sync::Arc::new(Mutex::new(SendComplex {timestamp: Utc::now(), ptr: fftw::array::AlignedVec::new(512)}))];
            let mut fftout  = [std::sync::Arc::new(Mutex::new(SendComplex {timestamp: Utc::now(), ptr: fftw::array::AlignedVec::new(512)})), std::sync::Arc::new(Mutex::new(SendComplex {timestamp: Utc::now(), ptr: fftw::array::AlignedVec::new(512)})), std::sync::Arc::new(Mutex::new(SendComplex {timestamp: Utc::now(), ptr: fftw::array::AlignedVec::new(512)}))];
            let mut chans  = Vec::new();
                                    for  ch in dev.channels() {
                                chans.push(ch);
}
            crossbeam_utils::thread::scope(| scope|{
                                scope.spawn(| _|{
                                        {
                                                println!("{} {}>{} {}:{} start fftw plan ", Utc::now(), std::process::id(), std::thread::current().name().expect("can't get thread name"), file!(), line!());
}
                                                            let mut plan: fftw::plan::C2CPlan64  = fftw::plan::C2CPlan::aligned(&[512], fftw::types::Sign::Forward, fftw::types::Flag::Measure).unwrap();
                    {
                                                println!("{} {}>{} {}:{} finish fftw plan ", Utc::now(), std::process::id(), std::thread::current().name().expect("can't get thread name"), file!(), line!());
}
                    loop {
                                                                        let tup: usize  = r.recv().ok().unwrap();
                                                let mut ha  = fftin[tup].clone();
                        let mut a  = &mut ha.lock().unwrap();
                        let mut hb  = fftout[tup].clone();
                        let mut b  = &mut hb.lock().unwrap();
                                                plan.c2c(&mut a.ptr, &mut b.ptr).unwrap();
                                                b.timestamp=Utc::now();
                        {
                                                        println!("{} {}>{} {}:{}   tup={:?}  (b.timestamp-a.timestamp)={:?}  b.ptr[0]={:?}", Utc::now(), std::process::id(), std::thread::current().name().expect("can't get thread name"), file!(), line!(), tup, (b.timestamp-a.timestamp), b.ptr[0]);
};
};
});
                                                let mut count  = 0;
                loop {
                                        match buf.refill() {
                                                Err(err) => {
                                                {
                                                println!("{} {}>{} {}:{} error filling buffer  err={:?}", Utc::now(), std::process::id(), std::thread::current().name().expect("can't get thread name"), file!(), line!(), err);
}
                                                std::process::exit(4)
},
                                                _ => {
                                                ()
},
}
                                        {
                                                                        let time_acquisition  = Utc::now();
                                                let mut ha  = fftin[count].clone();
                        let mut a  = &mut ha.lock().unwrap();
                                                let data_i: Vec<i16>  = buf.channel_iter::<i16>(&(chans[0])).collect();
                        let data_q: Vec<i16>  = buf.channel_iter::<i16>(&(chans[1])).collect();
                                                                        a.timestamp=time_acquisition;
                        for  i in 0..512 {
                                                                                    a.ptr[i]=fftw::types::c64::new((data_i[i] as f64), (data_q[i] as f64));
};
}
                                        {
                                                println!("{} {}>{} {}:{} sender  count={:?}", Utc::now(), std::process::id(), std::thread::current().name().expect("can't get thread name"), file!(), line!(), count);
}
                                        s.send(count).unwrap();
                                        count += 1 ;
                                        if  (3)<=(count)  {
                                                                                                count=0;
};
};
}).unwrap();
});
    };
    {
        let mut system = init(file!());
        let mut data = Vec::with_capacity(((128) * (4096)));
        for i in 0..128 {
            for j in 0..4096 {
                data.push((j as u8));
                data.push((i as u8));
                data.push(((i + j) as u8));
            }
        }
        let textures = system.renderer.textures();
        let raw = glium::texture::RawImage2d {
            data: std::borrow::Cow::Owned(data),
            width: (128 as u32),
            height: (4096 as u32),
            format: glium::texture::ClientFormat::U8U8U8,
        };
        let gl_texture =
            glium::texture::Texture2d::new(system.display.get_context(), raw).expect("new 2d tex");
        let texture_id = textures.insert(std::rc::Rc::new(gl_texture));
        let my_texture_id = Some(texture_id);
        system.main_loop(move |_, ui, display| {
            Window::new(im_str!("Hello world"))
                .size([3.00e+2, 1.00e+2], Condition::FirstUseEver)
                .build(ui, || {
                    ui.text(im_str!("Hello World"));
                    let mouse_pos = ui.io().mouse_pos;
                    ui.text(format!("mouse: ({:.1},{:.1})", mouse_pos[0], mouse_pos[1]));
                });
            Window::new(im_str!("texture"))
                .size([2.00e+2, 1.00e+2], Condition::FirstUseEver)
                .build(ui, || {
                    let mut data = Vec::with_capacity(((128) * (4096)));
                    for i in 0..128 {
                        for j in 0..4096 {
                            data.push((j as u8));
                            data.push((i as u8));
                            data.push(((i + j) as u8));
                        }
                    }
                    let raw = glium::texture::RawImage2d {
                        data: std::borrow::Cow::Owned(data),
                        width: (128 as u32),
                        height: (4096 as u32),
                        format: glium::texture::ClientFormat::U8U8U8,
                    };
                    let data_raw = (raw.data.as_ptr() as *const std::ffi::c_void);
                    let ctx = display.get_context();
                    unsafe {
                        ctx.gl.TexSubImage2D(
                            glium::gl::TEXTURE_2D,
                            0,
                            0,
                            0,
                            128,
                            4096,
                            glium::gl::RGBA,
                            glium::gl::UNSIGNED_BYTE,
                            data_raw,
                        )
                    };
                    let img = imgui::widget::image::Image::new(texture_id, [128., 4096.]);
                    img.build(ui);
                });
        });
    }
}
