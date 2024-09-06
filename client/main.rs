use std::time::{SystemTime, UNIX_EPOCH};
use crossterm::terminal;
use crossterm::event::{read, Event, KeyEvent, KeyCode, KeyModifiers};
use reqwest;


#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let channel_id = SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_secs().to_string();
    let ship = "zod";
    let code = "password=lidlut-tabwed-pillex-ridrup";
    let base_url = "http://localhost:8080/";
    let login_url = format!("{}/~/login", base_url);
    let channel_url = format!("{}/~/channel/{}", base_url, channel_id);

    let reqw = reqwest::Client::new();
    let resp = reqw.post(login_url)
        .body(code)
        .send()
        .await?;
    let auth = resp.headers().get("set-cookie").unwrap().to_str()?.to_string();
    stream_output(channel_url, auth);

    //reqw = reqwest::Client::builder()
    //let eventsource = Client::new_with_client(
    //    reqwest::Url::parse(&channel_url)?,
    //    reqw
    //);




    // terminal::enable_raw_mode()?;
    // println!("\x1b[2J");
    // let (width, height) = terminal::size()?;
    // loop {
    //     let event = read()?;
    //     match event {
    //         Event::Resize(width, height) => {
    //             //
    //         }
    //         Event::Mouse(mouse_event) => {
    //             println!("mouse event");
    //         }
    //         Event::Key(KeyEvent {
    //             code: KeyCode::Char('c'),
    //             modifiers: KeyModifiers::CONTROL,
    //             kind: _,
    //             state: _
    //         }) => {
    //             terminal::disable_raw_mode()?;
    //             println!("\x1b[2J");
    //             break;
    //         }
    //         Event::Key(key_event) => {
    //             //
    //         }
    //         _ => {}
    //     }
    // }
    Ok(())
}

async fn stream_output(channel_url: String, auth: String) -> Result<(), Box<dyn std::error::Error>>  {
    let client = reqwest::Client::new();
    let mut resp = client
        .get(channel_url)
        .header("cookie", auth)
        .send()
        .await?;
    while let Some(chunk) = resp.chunk().await? {
        println!("Chunk: {chunk:?}");
    }
    Ok(())
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
