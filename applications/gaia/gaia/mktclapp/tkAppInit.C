/*
 * E.S.O. - VLT project/ESO Archive
 * $Id$
 *
 * EtAppInit.C -- This version is based on mktclapp.

 *  Copyright:
 *     Copyright (C) 2000-2005 Central Laboratory of the Research Councils.
 *     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
 *     All Rights Reserved.
 
 *  Licence:
 *     This program is free software; you can redistribute it and/or
 *     modify it under the terms of the GNU General Public License as
 *     published by the Free Software Foundation; either version 2 of the
 *     License, or (at your option) any later version.
 *
 *     This program is distributed in the hope that it will be
 *     useful, but WITHOUT ANY WARRANTY; without even the implied warranty
 *     of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with this program; if not, write to the Free Software
 *     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
 *     02111-1307, USA
 
 * This file is processed with mktclapp to produce the C++ source file
 * containing all of the necessary Tcl scripts to form a single binary
 * version of the skycat.
 *
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  14 Nov 97  Created 
 * Peter W. Draper 2000       Added to GAIA.
 */

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
#include "gaia_swish.h"
extern int Blt_Init(Tcl_Interp *interp);
extern int Itcl_Init(Tcl_Interp *interp);
extern int Itk_Init(Tcl_Interp *interp);
extern int Tclx_Init(Tcl_Interp *interp);
extern int Tkhtml_Init(Tcl_Interp *interp);

extern int Skycat_Init(Tcl_Interp *interp);
extern int Gaia_Init(Tcl_Interp *interp);
}

/* Fortran initialisation. */
extern "C" {
void initFortran( int argc, char *argv[] );
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

  if (Itk_Init(interp) == TCL_ERROR) {
    return TCL_ERROR;
  }
  Tcl_StaticPackage(interp, "Itk", Itk_Init, (Tcl_PackageInitProc *) NULL);

  if (Blt_Init(interp) == TCL_ERROR) {
    return TCL_ERROR;
  }

  if (Tclx_Init(interp) == TCL_ERROR) {
    return TCL_ERROR;
  }
  Tcl_StaticPackage (interp, "Tclx", Tclx_Init, (Tcl_PackageInitProc *) NULL);

  if (Tkhtml_Init(interp) == TCL_ERROR ) {
      return TCL_ERROR;
      }
  Tcl_StaticPackage (interp, "TkHTML", Tkhtml_Init, (Tcl_PackageInitProc *) NULL);

  // install the Skycat package 
  if (Skycat_Init(interp) == TCL_ERROR) {
    return TCL_ERROR;
  }
  Tcl_StaticPackage (interp, "Skycat", Skycat_Init, (Tcl_PackageInitProc *) NULL);    // install the Skycat package 

  if (Gaia_Init(interp) == TCL_ERROR) {
    return TCL_ERROR;
  }
  Tcl_StaticPackage (interp, "Gaia", Gaia_Init, (Tcl_PackageInitProc *) NULL);

  return TCL_OK;
}

/*
 * Main function. Provide our own so we can do the necessary Fortran
 * initialisations.
 */
int
main( int argc, char *argv[] )
{
    /* Do any work to initialise the Fortran runtime */
    initFortran( argc, argv );

    return ( Et_Init( argc, argv ) != TCL_OK );
}
