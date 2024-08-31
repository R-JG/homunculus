use sword::noun::{D, T};
use crown::kernel::boot;
use crown::kernel::form::Kernel;
use crown::kernel::boot::Cli as BootCli;
use crown::utils::make_tas;
use crown::Noun;
use clap::{arg, command, ColorChoice, Parser};
use crossterm::terminal;
use crossterm::event::{read, Event, KeyEvent, KeyCode, KeyModifiers, KeyEventKind, KeyEventState, MouseEvent};

static KERNEL_JAM: &[u8] =
    include_bytes!(concat!(env!("CARGO_MANIFEST_DIR"), "/jam/homunculus.jam"));

#[derive(Parser, Debug)]
#[command(about = "", author = "", version, color = ColorChoice::Auto)]
struct TestCli {
    #[command(flatten)]
    boot: BootCli,
}

// #[tokio::main]
// async fn main() -> Result<(), Box<dyn std::error::Error>> {
fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cli = TestCli::parse();
    let mut kernel = boot::setup_form(KERNEL_JAM, Some(cli.boot))?;
    terminal::enable_raw_mode();
    loop {
        let event = read()?;
        match event {
            Event::FocusGained => {
                println!("Focus gained");
            }
            Event::FocusLost => {
                println!("Focus lost");
            }
            Event::Paste(content) => {
                println!("Pasted content: {}", content);
            }
            Event::Resize(width, height) => {
                println!("Terminal resized to width: {}, height: {}", width, height);
            }
            Event::Mouse(mouse_event) => {
                println!("mouse event");
            }
            Event::Key(KeyEvent {
                code: KeyCode::Char('c'),
                modifiers: KeyModifiers::CONTROL,
                kind: KeyEventKind,
                state: KeyEventState
            }) => {
                terminal::disable_raw_mode();
                println!("\x1b[2J");
                break;
            }
            Event::Key(key_event) => {
                // println!("code: {}, modifiers: {}", key_event.code, key_event.modifiers);
                let poke = {
                    let input = 
                        match key_event.code {
                            KeyCode::Char(char) => char.to_string(),
                            _ => "default".to_string()
                        };
                    let tag = make_tas(kernel.serf.stack(), "test");
                    let dat = make_tas(kernel.serf.stack(), &input);
                    create_poke(&mut kernel, &[tag.as_noun(), dat.as_noun()])
                };
                let mut poke_result = kernel.poke(poke)?;
                let effect_list = poke_result.as_cell()?;
                let effect = effect_list.head().as_cell()?;
                let effect_data = effect.tail().as_atom()?;
                let effect_str = std::str::from_utf8(effect_data.as_bytes())?;
                println!("product: {}", effect_str);
            }
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
