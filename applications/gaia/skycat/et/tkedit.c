/*
** This file implements a simple ASCII text editor
*/
void Et_Main(int argc, char **argv){
  ET_INSTALL_COMMANDS;
  ET_INCLUDE( tkedit.tcl );
}

#ifdef UNIX
int main(int argc, char **argv){
  Et_Init(&argc,argv);
  Et_Main(argc,argv);
  Et_MainLoop();
  return 0;
}
#endif

/* The "bell" command in Tk4.0 is busted.  This is a replacement */
ET_PROC( bell ){
#ifdef UNIX
  XBell(Et_Display,0);
#endif
  return ET_OK;
}
