/*
 filename: ast_ftool_main.c
 purpose:  c - wrapper for host ftool task
 author:   DJA
*/

#include <stdio.h>
#include "f77.h"
#include "sae_par.h"

#ifdef unix
#define RETURN 0
#endif
#ifdef vms
#define RETURN 1
#endif

#ifdef sun4
int MAIN_; /* work around SunOS 4.1.3 bug */
#endif

main (argc,argv)
int argc;
char **argv;
{
 OpenDefaultPF(argc, argv);
 F77_CALL(ast_ftool_applic)();
 CloseDefaultPF();
 return(RETURN);
}
