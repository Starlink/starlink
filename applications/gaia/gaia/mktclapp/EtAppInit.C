/*
 * E.S.O. - VLT project/ESO Archive
 * $Id$
 *
 * EtAppInit.C -- This version is based on mktclapp.
 *
 * This file is processed with mktclapp to produce the C++ source file
 * containing all of the necessary Tcl scripts to form a single binary
 * version of the skycat.
 *
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton 14 Nov 97 Created */

#ifndef lint
/* static char sccsid[] = "@(#) tkAppInit.c 1.12 94/12/17 16:30:56"; */
#endif /* not lint */

#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <stdio.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <iostream.h>

/* declare command procedures here */
extern "C" {
#include "tk.h"
#include "gaiainit.h"
extern int Blt_Init(Tcl_Interp *interp);
extern int Itcl_Init(Tcl_Interp *interp);
extern int Itk_Init(Tcl_Interp *interp);
extern int Tclx_Init(Tcl_Interp *interp);

extern int Skycat_Init(Tcl_Interp *interp);
extern int Gaia_Init(Tcl_Interp *interp);
}


/*
 *----------------------------------------------------------------------
 *
 * Et_AppInit --
 *
 *	This is the main program for ET application initialisation.
 *
 *----------------------------------------------------------------------
 */
extern "C" 
int Et_AppInit( Tcl_Interp *interp )
{
  //  Do each extension initialisation, note we add the environment
  //  variable fro each package as used when the single binary is built.
  if (Itcl_Init(interp) == TCL_ERROR) {
    return TCL_ERROR;
  }
  Tcl_StaticPackage(interp, "Itcl", Itcl_Init, (Tcl_PackageInitProc *) NULL);
  //  Tcl_SetVar( interp, "itcl_library", ET_ITCL_LIBRARY, TCL_GLOBAL_ONLY );
  //  Tcl_SetVar2( interp, "env", "ITCL_LIBRARY", ET_ITCL_LIBRARY, TCL_GLOBAL_ONLY);

  if (Itk_Init(interp) == TCL_ERROR) {
    return TCL_ERROR;
  }
  Tcl_StaticPackage(interp, "Itk", Itk_Init, (Tcl_PackageInitProc *) NULL);
  //  Tcl_SetVar( interp, "itk_library", ET_ITK_LIBRARY, TCL_GLOBAL_ONLY );
  //  Tcl_SetVar2( interp, "env", "ITK_LIBRARY", ET_ITK_LIBRARY, TCL_GLOBAL_ONLY);

  if (Blt_Init(interp) == TCL_ERROR) {
    return TCL_ERROR;
  }
  //  Tcl_SetVar( interp, "blt_library", ET_BLT_LIBRARY, TCL_GLOBAL_ONLY );
  //  Tcl_SetVar2( interp, "env", "BLT_LIBRARY", ET_BLT_LIBRARY, TCL_GLOBAL_ONLY);

  if (Tclx_Init(interp) == TCL_ERROR) {
    return TCL_ERROR;
  }
  Tcl_StaticPackage (interp, "Tclx", Tclx_Init, (Tcl_PackageInitProc *) NULL);
  //  Tcl_SetVar( interp, "tclx_library", ET_ITCLX_LIBRARY, TCL_GLOBAL_ONLY );
  //  Tcl_SetVar2( interp, "env", "TCLX_LIBRARY", ET_ITCLX_LIBRARY, TCL_GLOBAL_ONLY);

  // install the Skycat package 
  if (Skycat_Init(interp) == TCL_ERROR) {
    return TCL_ERROR;
  }
  Tcl_StaticPackage (interp, "Skycat", Skycat_Init, (Tcl_PackageInitProc *) NULL);    // install the Skycat package 
  //  Tcl_SetVar( interp, "skycat_library", ET_SKYCAT_LIBRARY, TCL_GLOBAL_ONLY );
  //  Tcl_SetVar2( interp, "env", "SKYCAT_LIBRARY", ET_SKYCAT_LIBRARY, TCL_GLOBAL_ONLY);

  if (Gaia_Init(interp) == TCL_ERROR) {
    return TCL_ERROR;
  }
  Tcl_StaticPackage (interp, "Gaia", Gaia_Init, (Tcl_PackageInitProc *) NULL);
  //  Tcl_SetVar( interp, "tclx_library", ET_GAIA_LIBRARY, TCL_GLOBAL_ONLY );
  //  Tcl_SetVar2( interp, "env", "TCLX_LIBRARY", ET_GAIA_LIBRARY, TCL_GLOBAL_ONLY);

  return TCL_OK;
}


