use std::time::{SystemTime, UNIX_EPOCH};
use crossterm::terminal;
use crossterm::event::{read, Event, KeyEvent, KeyCode, KeyModifiers};
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
  json: String
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
struct Diff {
  id: u32,
  response: String,
  json: String
}

#[derive(Deserialize)]
struct Quit {
  id: u32,
  response: String
}

#[derive(Deserialize)]
struct ResAck {
  ok: String,
  id: u32,
  response: String
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
  let mut msg_id = 0;
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
    .body(make_subscribe_body(&ship, &mut msg_id)?)
    .header("cookie", &auth)
    .send()
    .await?;
  // start sse
  stream_output(&reqw, &mut msg_id, &channel_url, &auth).await?;



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

async fn stream_output(reqw: &Client, msg_id: &mut u32, channel_url: &String, auth: &String) -> Result<(), Box<dyn std::error::Error>>  {
  let resp = reqw
    .get(channel_url)
    .header("Content-Type", "text/event-stream")
    .header("cookie", auth)
    .send()
    .await?;
  let mut stream = resp.bytes_stream();
  let mut buffer = String::new();
  while let Some(chunk) = stream.next().await {
    match chunk {
      Ok(bytes) => {
        let chunk_str = String::from_utf8(bytes.to_vec())?;
        buffer.push_str(&chunk_str);
        while let Some(pos) = buffer.find("\n\n") {
          let message = buffer[..pos].to_string();
          buffer.drain(..pos + 2);
          let mut event_id: u32 = 0;
          let mut dat: &str = "";
          for line in message.lines() {
            if line.starts_with(':') {
              continue;
            } else if let Some(id_str) = line.strip_prefix("id: ") {
              event_id = id_str.trim().parse::<u32>()?;
            } else if let Some(dat_str) = line.strip_prefix("data: ") {
              dat = dat_str;
            }
          }
          let event = from_str::<ResBody>(dat);
          match event {
            Ok(ResBody::Diff(diff)) => {
              print!("{}", diff.json);
              reqw.put(channel_url)
                .header("cookie", auth)
                .body(make_ack_body(msg_id, &event_id)?)
                .send();
            },
            _ => ()
          }
        }
      }
      Err(_err) => ()
    }
  }
  Ok(())
}

fn make_subscribe_body(ship: &String, msg_id: &mut u32) -> Result<String, serde_json::Error> {
  let subscribe = Subscribe{
    id: *msg_id,
    action: "subscribe".to_string(),
    ship: ship.to_owned(),
    app: "homunculus".to_string(),
    path: "/homunculus-http".to_string()
  };
  let body = ReqBody{
    body: vec![ReqOptions::Subscribe(subscribe)]
  };
  *msg_id += 1;
  return to_string(&body);
}

fn make_ack_body(msg_id: &mut u32, eventid: &u32) -> Result<String, serde_json::Error> {
  let ack = ReqAck{
    id: *msg_id,
    action: "ack".to_string(),
    eventid: *eventid
  };
  let body = ReqBody{
    body: vec![ReqOptions::Ack(ack)]
  };
  *msg_id += 1;
  return to_string(&body);
}

fn handle_key(key_event: KeyEvent) -> String {
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
