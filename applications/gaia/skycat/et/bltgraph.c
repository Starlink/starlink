/*
** This program shows an example of how to use ET with extensions
** packages for Tcl/Tk, such as BLT.
*/
#include <stdio.h>

int main(int argc, char **argv){
  Et_Init(&argc,argv);
  if( Blt_Init(Et_Interp)!=ET_OK ){
    fprintf(stderr,"Can't initialize the BLT extension.\n");
    exit(1);
  }
  ET_INCLUDE( graph2 );
  Et_MainLoop();
  return 0;
}
