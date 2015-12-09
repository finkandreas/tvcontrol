// std includes
#include <iostream>
#include <memory>
#include <string>
#include <vector>

// system header includes
#include <linux/input.h>
#include <linux/uinput.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <unistd.h>

#include "epapi.h"

#ifdef DEBUG
  #error "DEBUG is defined!!!"
#endif

namespace {
  std::vector<struct input_event> eventsToSend;
}


void check_failed(bool failed, const std::string& sMsg) {
  if (failed) {
    std::cerr << sMsg << std::endl;
    exit(1);
  }
}


void pressKey(int fd, const unsigned int key) {
  constexpr int neededEvents = 3;
  if (::eventsToSend.size() < neededEvents) ::eventsToSend.resize(neededEvents);
  memset(::eventsToSend.data(), 0, sizeof(struct input_event)*neededEvents);
  ::eventsToSend[0].type = EV_KEY;
  ::eventsToSend[0].code = key;
  ::eventsToSend[0].value = 1; // press key

  ::eventsToSend[1].type = EV_KEY;
  ::eventsToSend[1].code = key;
  ::eventsToSend[1].value = 0;  // release key

  ::eventsToSend[2].type = EV_SYN;

  check_failed(write(fd, ::eventsToSend.data(), sizeof(struct input_event)*neededEvents) < 0, "Failed to press the key");
}



int main() {
  std::unique_ptr<PktHandler> ph(new PktHandler(3,4));
  std::unique_ptr<MsgHandler> mh(new MsgHandler(ph.release()));

  //Register a message type
  mh->registerType(1, "stop", "s");
  mh->registerType(2, "keypress", "l");
  mh->registerType(3, "typetext", "s");

  int uinputFd = open("/dev/uinput", O_WRONLY|O_NONBLOCK);
  check_failed(uinputFd < 0, "Could not open /dev/uinput for writing. Do you have permissions?");
  check_failed( ioctl(uinputFd, UI_SET_EVBIT, EV_KEY) < 0, "Could not set EV_KEY bit" );
  check_failed( ioctl(uinputFd, UI_SET_EVBIT, EV_SYN) < 0, "Could not set EV_SYN bit" );
  for (int i=1; i<247; ++i) check_failed( ioctl(uinputFd, UI_SET_KEYBIT, i) < 0, "Could not add the key" );
  for (int i : {0x160}) check_failed( ioctl(uinputFd, UI_SET_KEYBIT, i) < 0, "Could not add the key" );

  struct uinput_user_dev uidev;
  memset(&uidev, 0, sizeof(uidev));
  snprintf(uidev.name, UINPUT_MAX_NAME_SIZE, "myuinput");
  uidev.id.bustype = BUS_USB;
  uidev.id.vendor  = 0x1234;
  uidev.id.product = 0xfedc;
  uidev.id.version = 1;

  check_failed( write(uinputFd, &uidev, sizeof(uidev)) < 0, "Failed to write uinput_user_dev structure to the filedescriptor" );
  check_failed( ioctl(uinputFd, UI_DEV_CREATE) < 0, "Could not create the dev file" );

  bool loop = true;
  while (loop) {
    //Wait for a message
    Msg *m=0;
    int result = mh->rx(&m);

    //Verify return code
    if (result) {
      //handle error
      std::cerr << "ERROR, message: " << mh->strerror() << std::endl;
      return 1;
    }

    if (m->getType() == 1) {
      loop = false;
    }

    if (m->getType() == 2) {
      char format;
      long button;
      m->getParam(0, &format, &button);
      pressKey(uinputFd, button);
    }

    if (m->getType() == 3) {
      char *text, format;
      m->getParam(0, &format, &text);
    }

    //Destroy message
    delete m;
  }

  check_failed( ioctl(uinputFd, UI_DEV_DESTROY) < 0, "Failed to destroy the dev file" );
  return 0;
}
