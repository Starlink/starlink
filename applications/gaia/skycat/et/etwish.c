/*
** This program implements the the interactive "wish" program of Tcl/Tk
** using ET.  The difference between this progarm and the standard "wish"
** is that this program has the startup scripts compiled in, and is
** thus completely stand-alone.
**
** You can use this program as a template to build your own "wish" with
** additional commands written in C.  Just use the ET_PROC construct to
** code your commands, then insert a single ET_INSTALL_COMMANDS statement
** after the Et_Init().
*/
void main(int argc, char **argv){
  Et_Init(&argc,argv);
  if( argc>2 && (strcmp(argv[1],"-f")==0 || strcmp(argv[1],"-file")==0) ){
    ET( source "%q(argv[2])" );
  }else if( argc>1 ){
    ET( source "%q(argv[1])" );
  }else{
    Et_ReadStdin();
  }
  Et_MainLoop();
}
