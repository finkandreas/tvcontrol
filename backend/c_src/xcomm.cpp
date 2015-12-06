// std includes
#include <iostream>
#include <memory>

// X11 includes
extern "C"{
  #include "xdo.h"
}
#include "epapi.h"

#ifdef DEBUG
  #error "DEBUG is defined!!!"
#endif

int main() {
  std::unique_ptr<PktHandler> ph(new PktHandler(3,4));
  std::unique_ptr<MsgHandler> mh(new MsgHandler(ph.release()));

  xdo_t* myXdo = xdo_new(0);

  //Register a message type
  mh->registerType(1, "stop", "s");
  mh->registerType(2, "keypress", "s");
  mh->registerType(3, "typetext", "s");

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
      char *button, format;
      m->getParam(0, &format, &button);
      xdo_keysequence(myXdo, CURRENTWINDOW, button, 12000);
    }

    if (m->getType() == 3) {
      char *text, format;
      m->getParam(0, &format, &text);
      xdo_type(myXdo, CURRENTWINDOW, text, 12000);
    }

    //Destroy message
    delete m;
  }

  xdo_free(myXdo);
  return 0;
}
