/* A C wrapper for the TkBrowser code
*/
void main(int argc, char **argv){
  Et_Init(&argc,argv);
  ET_INCLUDE( browser.tcl );
  Et_MainLoop();
  exit(0);
}
