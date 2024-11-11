use std::io::{Write, stdout};
use std::sync::Arc;
use std::sync::atomic::{AtomicU32, Ordering};
use std::time::{SystemTime, UNIX_EPOCH};
use crossterm::execute;
use crossterm::terminal;
use crossterm::terminal::{Clear, ClearType};
use crossterm::cursor::MoveTo;
use crossterm::style::{ResetColor, Attribute, SetAttribute};
use crossterm::event::{read, DisableMouseCapture, EnableMouseCapture, Event, KeyCode, KeyEvent, KeyModifiers, MouseEvent, MouseEventKind};
use reqwest::Client;
use futures::stream::StreamExt;
use serde::{Serialize, Deserialize};
use serde_json::{to_string, from_str};

#[derive(Serialize)]
struct Subscribe {
  id: u32,
  action: String,
  ship: String,
  app: String,
  path: String
}

#[derive(Serialize)]
struct Poke {
  id: u32,
  action: String,
  ship: String,
  app: String,
  mark: String,
  json: PokeData
}

#[derive(Serialize)]
struct ReqAck {
  id: u32,
  action: String,
  #[serde(rename = "event-id")]
  eventid: u32
}

#[derive(Serialize)]
struct Delete {
  id: u32,
  action: String,
}

#[derive(Deserialize)]
struct Diff {
  // id: u32,
  // response: String,
  json: String
}

#[derive(Deserialize)]
struct Quit {
  // id: u32,
  // response: String
}

#[derive(Deserialize)]
struct ResAck {
  // ok: String,
  // id: u32,
  // response: String
}

#[derive(Serialize)]
struct TerminalSize(u16, u16);

#[derive(Serialize)]
#[serde(untagged)]
enum PokeData {
  Input(String),
  Size(TerminalSize)
}

#[derive(Serialize)]
#[serde(untagged)]
enum ReqOptions {
  Subscribe(Subscribe),
  Poke(Poke),
  Ack(ReqAck),
  Delete(Delete)
}

#[derive(Serialize)]
#[serde(transparent)]
struct ReqBody {
  body: Vec<ReqOptions>
}

#[derive(Deserialize)]
#[serde(untagged)]
enum ResBody {
  Diff(Diff),
  Quit(Quit),
  Ack(ResAck)
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
  let ship = "zod".to_string();
  let code = "lidlut-tabwed-pillex-ridrup";
  let base_url = "http://localhost:8080";
  //
  let msg_id = Arc::new(AtomicU32::new(0));
  let now = SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_secs();
  let channel_id = format!("homunculus-{}", now.to_string());
  let login_url = format!("{}/~/login", base_url);
  let channel_url = format!("{}/~/channel/{}", base_url, channel_id);
  // get auth
  let reqw = Client::new();
  let resp = reqw.post(login_url)
    .body(format!("password={}", code))
    .send()
    .await?;
  let auth = resp
    .headers().get("set-cookie").unwrap().to_str()?
    .split(";").next().unwrap().to_string();
  // create channel
  reqw.put(&channel_url)
    .body(make_subscribe_body(&ship, &msg_id)?)
    .header("cookie", &auth)
    .send()
    .await?;
  // stream output
  setup_terminal()?;
  let reqw_clone = reqw.clone();
  let msg_id_clone = Arc::clone(&msg_id);
  let channel_url_clone = channel_url.clone();
  let auth_clone = auth.clone();
  tokio::spawn(async {
    let _ = stream_output(reqw_clone, msg_id_clone, channel_url_clone, auth_clone).await;
  });
  // send input
  let (width, height) = terminal::size()?;
  reqw.put(&channel_url)
    .body(make_poke_body(&ship, &msg_id, PokeData::Size(TerminalSize(width, height)))?)
    .header("cookie", &auth)
    .send()
    .await?;
  loop {
    let event = read()?;
    match event {
      Event::Key(KeyEvent {
        code: KeyCode::Char('c'),
        modifiers: KeyModifiers::CONTROL,
        kind: _,
        state: _
      }) => {
        reqw.put(&channel_url)
          .body(make_delete_body(&msg_id)?)
          .header("cookie", &auth)
          .send()
          .await?;
        reset_terminal()?;
        break;
      }
      Event::Key(key_event) => {
        let input = handle_key(key_event);
        if input != "" {
          reqw.put(&channel_url)
            .body(make_poke_body(&ship, &msg_id, PokeData::Input(input))?)
            .header("cookie", &auth)
            .send()
            .await?;
        }
      }
      Event::Mouse(mouse_event) => {
        let input = handle_mouse(mouse_event);
        if input != "" {
          reqw.put(&channel_url)
            .body(make_poke_body(&ship, &msg_id, PokeData::Input(input))?)
            .header("cookie", &auth)
            .send()
            .await?;
        }
      }
      Event::Resize(width, height) => {
        let size = TerminalSize(width, height);
        reqw.put(&channel_url)
          .body(make_poke_body(&ship, &msg_id, PokeData::Size(size))?)
          .header("cookie", &auth)
          .send()
          .await?;
      }
      _ => {}
    }
  }
  Ok(())
}

async fn stream_output(reqw: Client, msg_id: Arc<AtomicU32>, channel_url: String, auth: String) -> Result<(), Box<dyn std::error::Error>>  {
  let resp = reqw
    .get(&channel_url)
    .header("Content-Type", "text/event-stream")
    .header("cookie", &auth)
    .send()
    .await?;
  let mut stream = resp.bytes_stream();
  let mut buffer = Vec::<u8>::new();
  let mut bufstr = String::new();
  while let Some(chunk) = stream.next().await {
    match chunk {
      Ok(bytes) => {
        buffer.append(&mut bytes.to_vec());
        let Ok(newstr) = String::from_utf8(buffer.clone())
          else { continue; };
        buffer.clear();
        bufstr.push_str(&newstr);
        while let Some(pos) = bufstr.find("\n\n") {
          let message = bufstr[..pos].to_string();
          bufstr.drain(..pos + 2);
          let mut event_id: Option<u32> = None;
          let mut dat: Option<&str> = None;
          for line in message.lines() {
            if line.starts_with(':') {
              continue;
            } else if let Some(id_str) = line.strip_prefix("id: ") {
              let Ok(num) = id_str.trim().parse::<u32>()
                else { continue; };
              event_id = Some(num);
            } else if let Some(dat_str) = line.strip_prefix("data: ") {
              dat = Some(dat_str);
            } else {
              continue;
            }
          }
          if event_id == None || dat == None {
            continue;
          }
          let event = from_str::<ResBody>(dat.unwrap());
          match event {
            Ok(ResBody::Diff(diff)) => {
              let out = diff.json.replace("\\x1b", "\x1b");
              print!("{}", out);
              std::io::stdout().flush().unwrap();
              let Ok(ack_body) = make_ack_body(&msg_id, &event_id.unwrap())
                else { continue; };
              let Ok(_) = reqw.put(&channel_url)
                .header("cookie", &auth)
                .body(ack_body)
                .send()
                .await
              else { continue; };
            },
            _ => continue
          }
        }
      }
      Err(_err) => continue
    }
  }
  Ok(())
}

fn make_subscribe_body(ship: &String, msg_id: &Arc<AtomicU32>) -> Result<String, serde_json::Error> {
  let subscribe = Subscribe{
    id: msg_id.load(Ordering::SeqCst),
    action: "subscribe".to_string(),
    ship: ship.to_owned(),
    app: "homunculus".to_string(),
    path: "/homunculus-http".to_string()
  };
  let body = ReqBody{
    body: vec![ReqOptions::Subscribe(subscribe)]
  };
  msg_id.fetch_add(1, Ordering::SeqCst);
  return to_string(&body);
}

fn make_poke_body(ship: &String, msg_id: &Arc<AtomicU32>, dat: PokeData) -> Result<String, serde_json::Error> {
  let poke = Poke{
    id: msg_id.load(Ordering::SeqCst),
    action: "poke".to_string(),
    ship: ship.to_owned(),
    app: "homunculus".to_string(),
    mark: "json".to_string(),
    json: dat
  };
  let body = ReqBody{
    body: vec![ReqOptions::Poke(poke)]
  };
  msg_id.fetch_add(1, Ordering::SeqCst);
  return to_string(&body);
}

fn make_ack_body(msg_id: &Arc<AtomicU32>, eventid: &u32) -> Result<String, serde_json::Error> {
  let ack = ReqAck{
    id: msg_id.load(Ordering::SeqCst),
    action: "ack".to_string(),
    eventid: *eventid
  };
  let body = ReqBody{
    body: vec![ReqOptions::Ack(ack)]
  };
  msg_id.fetch_add(1, Ordering::SeqCst);
  return to_string(&body);
}

fn make_delete_body(msg_id: &Arc<AtomicU32>) -> Result<String, serde_json::Error> {
  let delete = Delete{
    id: msg_id.load(Ordering::SeqCst),
    action: "delete".to_string(),
  };
  let body = ReqBody{
    body: vec![ReqOptions::Delete(delete)]
  };
  msg_id.fetch_add(1, Ordering::SeqCst);
  return to_string(&body);
}

fn handle_key(key_event: KeyEvent) -> String {
  match key_event.modifiers {
    KeyModifiers::NONE =>
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
      },
    KeyModifiers::CONTROL =>
      match key_event.code {
        KeyCode::Up               =>  "\\e[1;5A".to_string(),
        KeyCode::Down             =>  "\\e[1;5B".to_string(),
        KeyCode::Right            =>  "\\e[1;5C".to_string(),
        KeyCode::Left             =>  "\\e[1;5D".to_string(),
        _                         =>  "".to_string()
      },
    KeyModifiers::ALT =>
      match key_event.code {
        KeyCode::Char(char) =>  format!("\\e{char}"),
        KeyCode::Up               =>  "\\e[1;3A".to_string(),
        KeyCode::Down             =>  "\\e[1;3B".to_string(),
        KeyCode::Right            =>  "\\e[1;3C".to_string(),
        KeyCode::Left             =>  "\\e[1;3D".to_string(),
        _                         =>  "".to_string()
      },
    KeyModifiers::SHIFT =>
      match key_event.code {
        KeyCode::Up               =>  "\\e[1;2A".to_string(),
        KeyCode::Down             =>  "\\e[1;2B".to_string(),
        KeyCode::Right            =>  "\\e[1;2C".to_string(),
        KeyCode::Left             =>  "\\e[1;2D".to_string(),
        _                         =>  "".to_string()
      },
    _                             =>  "".to_string()
  }
}

fn handle_mouse(mouse_event: MouseEvent) -> String {
  let x = mouse_event.column + 1;
  let y = mouse_event.row + 1;
  match mouse_event.kind {
    MouseEventKind::Down(_)    =>  format!("\\e[<0;{};{}M", x, y),
    MouseEventKind::ScrollDown =>  format!("\\e[<64;{};{}M", x, y),
    MouseEventKind::ScrollUp   =>  format!("\\e[<65;{};{}M", x, y),
    _                          =>  "".to_string()
  }
}

fn setup_terminal() -> Result<(), Box<dyn std::error::Error>> {
  terminal::enable_raw_mode()?;
  execute!(
    stdout(),
    EnableMouseCapture
  )?;
  Ok(())
}

fn reset_terminal() -> Result<(), Box<dyn std::error::Error>> {
  terminal::disable_raw_mode()?;
  execute!(
    stdout(),
    MoveTo(0, 0),
    ResetColor,
    SetAttribute(Attribute::Reset),
    DisableMouseCapture,
    Clear(ClearType::All),
    Clear(ClearType::Purge),
  )?;
  Ok(())
}
