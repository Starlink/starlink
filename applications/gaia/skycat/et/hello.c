/*
** The classic "Hello, World!" program written using ET.
*/
void main(int argc, char **argv){
   Et_Init(&argc,argv);
   ET( button .x -text "Hello, World!" -command exit; pack .x );
   Et_MainLoop();
}
