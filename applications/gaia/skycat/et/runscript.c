/*
** This ET program is used to run a stand-alone Tcl/Tk script.  The
** Tcl/Tk script which is run must be in the same directory as this
** executable, and must have the same name except with the suffix
** ".tcl" appended.
**
** For example, to run the Tcl/Tk script named "myscript.tcl", make
** a link of the executable for this program to a file called
** "myscript" in the same directory with the "myscript.tcl" script.
** To invoke the Tcl/Tk script, type "myscript".
*/
void
main(int argc, char **argv){
  Et_Init(&argc,argv);
  ET( source $cmd_dir/$cmd_name.tcl );
  Et_MainLoop();
}
