/*
** This file implements a simple web server
*/
void Et_Main(int argc, char **argv){
  ET_INSTALL_COMMANDS;
  ET_INCLUDE( httpd.tcl );
}

#if UNIX
int main(int argc, char **argv){
  Et_Init(&argc,argv);
  Et_Main(argc,argv);
  Et_MainLoop();
  return 0;
}
#endif
