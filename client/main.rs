use sword::noun::{Atom, D, T};
use crown::kernel::boot;
use crown::kernel::form::Kernel;
use crown::kernel::boot::Cli as BootCli;
use crown::utils::make_tas;
use crown::{AtomExt, Noun, NounExt, Bytes};
use clap::{arg, command, ColorChoice, Parser};
use crossterm::terminal;
use crossterm::event::{read, Event, KeyEvent, KeyCode, KeyModifiers};

static KERNEL_JAM: &[u8] =
    include_bytes!(concat!(env!("CARGO_MANIFEST_DIR"), "/jam/homunculus.jam"));

#[derive(Parser, Debug)]
#[command(about = "", author = "", version, color = ColorChoice::Auto)]
struct TestCli {
    #[command(flatten)]
    boot: BootCli,
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cli = TestCli::parse();
    let mut kernel = boot::setup_form(KERNEL_JAM, Some(cli.boot))?;
    terminal::enable_raw_mode()?;
    println!("\x1b[2J");
    let (width, height) = terminal::size()?;
    let size_init_poke = make_resize_poke(&mut kernel, width, height);
    kernel.poke(size_init_poke)?;
    loop {
        let event = read()?;
        match event {
            Event::Resize(width, height) => {
                let poke = make_resize_poke(&mut kernel, width, height);
                kernel.poke(poke)?;
            }
            Event::Mouse(mouse_event) => {
                println!("mouse event");
            }
            Event::Key(KeyEvent {
                code: KeyCode::Char('c'),
                modifiers: KeyModifiers::CONTROL,
                kind: _,
                state: _
            }) => {
                terminal::disable_raw_mode()?;
                println!("\x1b[2J");
                break;
            }
            Event::Key(key_event) => {
                // println!("code: {}, modifiers: {}", key_event.code, key_event.modifiers);
                let poke = make_input_poke(&mut kernel, key_event);
                let poke_result = kernel.poke(poke)?;
                // let effect_list = poke_result.as_cell()?;
                // let effect = effect_list.head().as_cell()?;
                // let effect_data = effect.tail().as_atom()?;
                // let effect_str = std::str::from_utf8(effect_data.as_bytes())?;
                // println!("product: {}", effect_str);
            }
            _ => {}
        }
    }
    Ok(())
}

fn create_poke(kernel: &mut Kernel, args: &[Noun]) -> Noun {
    if args.len() < 2 {
        panic!("args must have at least 2 elements");
    }
    T(kernel.serf.stack(), args)
}

fn make_resize_poke(kernel: &mut Kernel, width: u16, height: u16) -> Noun {
    let tag = make_tas(kernel.serf.stack(), "homunculus");
    let jon = make_tas(kernel.serf.stack(), "resize");
    let wid = Atom::new(kernel.serf.stack(), width.into());
    let hei = Atom::new(kernel.serf.stack(), height.into());
    create_poke(kernel, &[tag.as_noun(), jon.as_noun(), wid.as_noun(), hei.as_noun()])
}

fn make_input_poke(kernel: &mut Kernel, key_event: KeyEvent) -> Noun {
    let input = handle_key_input(key_event);
    let tag = make_tas(kernel.serf.stack(), "homunculus");
    let jon = make_tas(kernel.serf.stack(), "input");
    let dat = make_tas(kernel.serf.stack(), &input);
    create_poke(kernel, &[tag.as_noun(), jon.as_noun(), dat.as_noun()])
}

fn handle_key_input(key_event: KeyEvent) -> String {
    match key_event.code {
        KeyCode::Char(char) =>  char.to_string(),
        KeyCode::Tab              =>  "\\t".to_string(),
        KeyCode::Enter            =>  "\\n".to_string(),
        KeyCode::Esc              =>  "\\e".to_string(),
        KeyCode::Backspace        =>  "\\177".to_string(),
        KeyCode::Delete           =>  "\\e[3~".to_string(),
        KeyCode::Up               =>  "\\e[A".to_string(),
        KeyCode::Down             =>  "\\e[B".to_string(),
        KeyCode::Right            =>  "\\e[C".to_string(),
        KeyCode::Left             =>  "\\e[D".to_string(),
        _                         =>  "".to_string()
    }
}
