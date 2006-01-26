#if !defined( GRP_INCLUDED )  /* Include this file only once */
#define GRP_INCLUDED
/*
*  Name:
*     grp.h

*  Purpose:
*     Define the C interface to the GRP library.

*  Description:
*     This module defines the C interface to the functions of the GRP
*     library. The file grp.c contains C wrappers for the Fortran 
*     GRP routines.

*  Notes:
*     - Given the size of the GRP library, providing a complete C
*     interface is probably not worth the effort. Instead, I suggest that 
*     people who want to use GRP from C extend this file (and
*     grp.c) to include any functions which they need but which are
*     not already included.

*  Authors:
*     DSB: David .S. Berry (UCLan)
*     TIMJ: Tim Jenness (JAC, Hawaii)

*  History:
*     30-SEP-2005 (DSB):
*        Original version.
*     03-NOV-2005 (TIMJ):
*        Use enum for constants rather than #define.
*        Use an opaque struct for the C interface rather than the bare
*        int.
*     24-NOV-2006 (TIMJ):
*        Add grpInfoI

*  Copyright:
*     Copyright (C) 2005-2006 Particle Physics and Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

*/

/* We need CNF to define Fortran */

#include <f77.h>

/* Public Constants */
/* ---------------- */

/* An illegal GRP_ identifier value. This value can sometimes be
   specified by an application in place of a GRP_ identifier in order
   to supress some operation. */
enum { GRP__NOID  = 0 };

/* Maximum length of a group expression. */
enum { GRP__SZGEX  = 255 };

/* Length of a name within a group. */
enum { GRP__SZNAM  = 255 };

/* Max. length of a group type */
enum { GRP__SZTYP  = 80 };

/* Max. length of a file name. */
enum { GRP__SZFNM  = 256 };

/* Type definitions for GRP C interface */
/* ------------------------------------ */

/* The contents of this struct are not public */
typedef struct Grp {
   F77_INTEGER_TYPE igrp; /* Currently refers to the Fortran GRP ID */  
} Grp;


/* Public function prototypes */
/* -------------------------- */
Grp *grpInit( int * );
void grpFree( Grp **, int * );

void grpDelet( Grp **, int * );
void grpGrpsz( Grp *, int *, int * );
void grpGet( Grp *, int, int, char *const *, int, int * );
void grpInfoI( Grp *grp, int index, const char * item, int * value, 
	       int *status);
Grp *grpNew( const char *, int * );
void grpPut1( Grp *, const char *, int, int * );
void grpValid( Grp *, int *, int * );

/* Semi-Public function prototypes: For Fortran interface wrappers only */
/* -------------------------------------------------------------------- */
void grp1Setid( Grp *, F77_INTEGER_TYPE, int *);
F77_INTEGER_TYPE grp1Getid( Grp *, int * );



#endif
