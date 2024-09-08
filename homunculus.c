#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <termios.h>
#include <ctype.h>
#include <errno.h>
#include <string.h>
#include <curl/curl.h>

#define CTRL_KEY(k) ((k) & 0x1f)

struct termios old_termios;

void
die(const char *s)
{
  perror(s);
  exit(1);
}

void
disable_raw_mode()
{
  if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &old_termios) == -1)
    die("tcsetattr");
}

void
enable_raw_mode()
{
  if (tcgetattr(STDIN_FILENO, &old_termios) == -1) die("tcgetattr");
  atexit(disable_raw_mode);
  struct termios raw = old_termios;
  raw.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
  raw.c_oflag &= ~(OPOST);
  raw.c_cflag |= (CS8);
  raw.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);
  if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw) == -1) die("tcsetattr");
}

void
clear_screen()
{
  write(STDOUT_FILENO, "\x1b[2J", 4);
}

int
get_size(int *rows, int *cols)
{
  struct winsize ws;
  if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == -1 || ws.ws_col == 0) {
    return -1;
  } else {
    *cols = ws.ws_col;
    *rows = ws.ws_row;
    return 0;
  }
}

void
read_keys(char keys[6])
{
  int nread;
  while ((nread = read(STDIN_FILENO, &keys[0], 1)) != 1) {
    if (nread == -1 && errno != EAGAIN) die("read");
  }
  if (keys[0] != '\x1b') return;
  if (read(STDIN_FILENO, &keys[1], 1) != 1) return;
  if (keys[1] != '[') return;
  if (read(STDIN_FILENO, &keys[2], 1) != 1) return;
  if ((keys[2] == 'A') || (keys[2] == 'B') || (keys[2] == 'C') || (keys[2] == 'D')) {
    // printf("aro");
    return;
  } else if (keys[2] == '1') {
    if (read(STDIN_FILENO, &keys[3], 1) != 1) return;
    if (keys[3] != ';')  return;
    if (read(STDIN_FILENO, &keys[4], 1) != 1) return;
    if (read(STDIN_FILENO, &keys[5], 1) != 1) return;
    // printf("mod aro");
    return;
  } else if (keys[2] == '3') {
    if (read(STDIN_FILENO, &keys[3], 1) != 1) return;
    if (keys[3] != '~')  return;
    // printf("del");
    return;
  }
}

void
handle_input(char keys[6])
{
  read_keys(keys);
  if (keys[0] == CTRL_KEY('c')) {
    clear_screen();
    exit(0);
  }
  // if (keys[0] != '\x1b') printf(keys);
  // printf("\r\n");

  // send req

  memset(keys, '\0', 6);
}

char*
extract_auth(char *hed_val)
{
  return strtok(hed_val, ";");
}

size_t
handle_sse(char *buffer, size_t itemsize, size_t nitems, void* na)
{
  size_t bytes = itemsize * nitems;
  //
  // process json from buffer and then print the rendered output
  //
  printf("!!! Server Sent Event !!!");
  printf(buffer);
  printf("\r\n");
  return bytes;
}

int
main(void)
{                          // hide cursor: "\x1b[?25l", 6    show cursor: "\x1b[?25h", 6
  char ship[] = "zod";
  char base_url[] = "http://localhost:8080";
  char code[28] = "lidlut-tabwed-pillex-ridrup";
  //
  char *test_login_url = "http://localhost:8080/~/login";
  char *test_channel_url = "http://localhost:8080/~/channel/randomlygeneratedid";
  char *test_password = "password=lidlut-tabwed-pillex-ridrup";
  // curl init
  struct curl_header *hed;
  CURL *curl_sse = curl_easy_init();
  CURL *curl_req = curl_easy_init();
  if (!curl_sse || !curl_req) die("curl");
  // curl auth
  curl_easy_setopt(curl_req, CURLOPT_URL, test_login_url);
  curl_easy_setopt(curl_req, CURLOPT_POSTFIELDS, test_password);
  curl_easy_perform(curl_req);
  curl_easy_header(curl_req, "set-cookie", 0, CURLH_HEADER, -1, &hed);
  char *auth = extract_auth(hed->value);
  struct curl_slist *slist = NULL;
  // curl create sse channel
  
  // curl stream sse channel
  curl_easy_setopt(curl_sse, CURLOPT_URL, test_channel_url);
  curl_easy_setopt(curl_sse, CURLOPT_COOKIE, auth);
  curl_easy_setopt(curl_sse, CURLOPT_WRITEFUNCTION, handle_sse);
  curl_easy_perform(curl_sse);
  
  enable_raw_mode();
  // clear_screen();
  char keys[6] = {'\0'};
  while (1) {
    handle_input(keys);
  }
  return 0;
}
