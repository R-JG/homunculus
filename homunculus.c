#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <termios.h>
#include <ctype.h>
#include <errno.h>
// #include <curl/curl.h>

#define CTRL_KEY(k) ((k) & 0x1f)

struct termios old_termios;

void die(const char *s) {
  perror(s);
  exit(1);
}

void disableRawMode() {
  if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &old_termios) == -1)
    die("tcsetattr");
}

void enableRawMode() {
  if (tcgetattr(STDIN_FILENO, &old_termios) == -1) die("tcgetattr");
  atexit(disableRawMode);
  struct termios raw = old_termios;
  raw.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
  raw.c_oflag &= ~(OPOST);
  raw.c_cflag |= (CS8);
  raw.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);
  if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw) == -1) die("tcsetattr");
}

void clearScreen() {
  write(STDOUT_FILENO, "\x1b[2J", 4);
}

char readKey() {
  int cs;
  char c;
  while ((cs = read(STDIN_FILENO, &c, 1)) != 1) {
    if (cs == -1 && errno != EAGAIN) die("read");
  }
  return c;
}

void handleInput() {
  char c = readKey();

  if (iscntrl(c)) {
    printf("%d\r\n", c);
  } else {
    printf("%d ('%c')\r\n", c, c);
  }

  switch (c) {
    case CTRL_KEY('q'):
      exit(0);
      break;
  }
}

int main(void) {
  clearScreen();
  enableRawMode();
  char c;
  while (1) {
    handleInput();
  }
  return 0;
}
