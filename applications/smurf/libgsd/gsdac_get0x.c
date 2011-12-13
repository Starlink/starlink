/*
*+
*  Name:
*     gsdac_get0x.c

*  Purpose:
*     Return contents of a scalar GSD header

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     gsdac_get0{blwirdc} ( const gsd *gsd,
*                           const char *name, <type> *value , int *status )

*  Arguments:
*     gsd = const gsd* (Given)
*        GSD file access parameters
*     name = const char* (Given)
*        The name of the item. This should be an array of 16 characters (char
*        name[16]) and a null-terminated string.
*     value = <type>* (Returned)
*        The data value.  For gsdac_get0c the value should be declared with
*        length 17 at least.  The returned string is null-terminated in
*        value[16].
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     These routines return the value stored in a scalar GSD header item
*     given by name.  The different data types are:
*
*    <t> <type>     GSD
*     b   char      byte
*     c   char[17]  char
*     d   double    double
*     i   int       integer
*     l   char      logical
*     r   float     real
*     w   short     word

*  Authors:
*     J.Balfour (UBC)
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History :
*     2008-02-04 (JB):
*        Original
*     2008-03-19 (JB):
*        Removed unused variables.
*     2008-04-04 (JB):
*        Wrap gsd calls in macro for error checking.
*     2008-07-04 (TIMJ):
*        Use const.

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 3 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* Starlink includes */
#include "gsd.h"
#include "sae_par.h"
#include "mers.h"

/* SMURF includes */
#include "gsdac.h"

void gsdac_get0b ( const gsd *gsd,
                   const char *name, char *value, int *status ) {

  /* Local variables */
  char array;                  /* array flag (should always be false) */
  int itemno;                  /* item number of the GSD header */
  char type;                   /* data type of the item (should always be B) */
  char unit[11];               /* unit of the GSD header */

  /* Check inherited status */
  if ( *status != SAI__OK ) return;

  /* Get the item number. */
  CALLGSD( gsdFind ( gsd->fileDsc, gsd->itemDsc, name, &itemno,
		     unit, &type, &array ),
           status,
           msgSetc ( "NAME", name ); errRep ( "gsdac_get0b", "gsdFind : Could not find element ^NAME in file", status ); );

  if ( *status != SAI__OK ) return;

  /* Check that the array flag is false and the data type is B. */
  if ( array ) {
    *status = SAI__ERROR;
    msgSetc ( "NAME", name );
    errRep ( "gsdac_get0b", "Expected scalar data for ^NAME, got an array", status );
    return;
  } else if (  type != 'B' ) {
    *status = SAI__ERROR;
    msgSetc ( "NAME", name );
    errRep ( "gsdac_get0b", "Header type of ^NAME did not match requested type B (CHAR).", status );
    return;
  }

  CALLGSD( gsdGet0b ( gsd->fileDsc, gsd->itemDsc, gsd->dataPtr,
		      itemno, value ),
           status,
           msgSetc ( "NAME", name ); errRep ( "gsdac_get0b", "gsdGet0b : Could not get ^NAME from GSD file", status ); );

}

void gsdac_get0c ( const gsd *gsd,
                   const char *name, char *value, int *status ) {

  /* Local variables */
  char array;                  /* array flag (should always be false) */
  int itemno;                  /* item number of the GSD header */
  char type;                   /* data type of the item (should always be C) */
  char unit[11];               /* unit of the GSD header */

  /* Check inherited status */
  if ( *status != SAI__OK ) return;

  /* Get the item number. */
  CALLGSD( gsdFind ( gsd->fileDsc, gsd->itemDsc, name, &itemno,
		     unit, &type, &array ),
           status,
           msgSetc ( "NAME", name ); errRep ( "gsdac_get0c", "gsdFind : Could not find element ^NAME in file", status ); );

  if ( *status != SAI__OK ) return;

  /* Check that the array flag is false and the data type is C. */
  if ( array ) {
    *status = SAI__ERROR;
    msgSetc ( "NAME", name );
    errRep ( "gsdac_get0c", "Expected scalar data for ^NAME, got an array", status );
    return;
  } else if (  type != 'C' ) {
    *status = SAI__ERROR;
    msgSetc ( "NAME", name );
    errRep ( "gsdac_get0c", "Header type of ^NAME did not match requested type C (CHAR[17]).", status );
    return;
  }

  CALLGSD( gsdGet0c ( gsd->fileDsc, gsd->itemDsc, gsd->dataPtr,
		      itemno, value ),
           status,
           msgSetc ( "NAME", name ); errRep ( "gsdac_get0c", "gsdGet0c : Could not get ^NAME from GSD file", status ); );

}

void gsdac_get0d ( const gsd *gsd,
                   const char *name, double *value, int *status ) {

  /* Local variables */
  char array;                  /* array flag (should always be false) */
  int itemno;                  /* item number of the GSD header */
  char type;                   /* data type of the item (should always be D) */
  char unit[11];               /* unit of the GSD header */

  /* Check inherited status */
  if ( *status != SAI__OK ) return;

  /* Get the item number. */
  CALLGSD( gsdFind ( gsd->fileDsc, gsd->itemDsc, name, &itemno,
		     unit, &type, &array ),
           status,
           msgSetc ( "NAME", name ); errRep ( "gsdac_get0d", "gsdFind : Could not find element ^NAME in file", status ); );

  if ( *status != SAI__OK ) return;

  /* Check that the array flag is false and the data type is D. */
  if ( array ) {
    *status = SAI__ERROR;
    msgSetc ( "NAME", name );
    errRep ( "gsdac_get0d", "Expected scalar data for ^NAME, got an array", status );
    return;
  } else if (  type != 'D' ) {
    *status = SAI__ERROR;
    msgSetc ( "NAME", name );
    errRep ( "gsdac_get0d", "Header type of ^NAME did not match requested type D (DOUBLE).", status );
    return;
  }

  CALLGSD( gsdGet0d ( gsd->fileDsc, gsd->itemDsc, gsd->dataPtr,
		      itemno, value ),
           status,
           msgSetc ( "NAME", name ); errRep ( "gsdac_get0d", "gsdGet0d : Could not get ^NAME from GSD file", status ); );

}

void gsdac_get0i ( const gsd *gsd,
                   const char *name, int *value, int *status ) {

  /* Local variables */
  char array;                  /* array flag (should always be false) */
  int itemno;                  /* item number of the GSD header */
  char type;                   /* data type of the item (should always be I) */
  char unit[11];               /* unit of the GSD header */

  /* Check inherited status */
  if ( *status != SAI__OK ) return;

  /* Get the item number. */
  CALLGSD( gsdFind ( gsd->fileDsc, gsd->itemDsc, name, &itemno,
		     unit, &type, &array ),
           status,
           msgSetc ( "NAME", name ); errRep ( "gsdac_get0i", "gsdFind : Could not find element ^NAME in file", status ); );

  if ( *status != SAI__OK ) return;

  /* Check that the array flag is false and the data type is I. */
  if ( array ) {
    *status = SAI__ERROR;
    msgSetc ( "NAME", name );
    errRep ( "gsdac_get0i", "Expected scalar data for ^NAME, got an array", status );
    return;
  } else if (  type != 'I' ) {
    *status = SAI__ERROR;
    msgSetc ( "NAME", name );
    errRep ( "gsdac_get0i", "Header type of ^NAME did not match requested type I (INTEGER).", status );
     return;
  }

  CALLGSD( gsdGet0i ( gsd->fileDsc, gsd->itemDsc, gsd->dataPtr,
		      itemno, value ),
           status,
           msgSetc ( "NAME", name ); errRep ( "gsdac_get0i", "gsdGet0i : Could not get ^NAME from GSD file", status ); );

}

void gsdac_get0l ( const gsd *gsd,
                   const char *name, char *value, int *status ) {

  /* Local variables */
  char array;                  /* array flag (should always be false) */
  int itemno;                  /* item number of the GSD header */
  char type;                   /* data type of the item (should always be L) */
  char unit[11];               /* unit of the GSD header */

  /* Check inherited status */
  if ( *status != SAI__OK ) return;

  /* Get the item number. */
  CALLGSD( gsdFind ( gsd->fileDsc, gsd->itemDsc, name, &itemno,
		     unit, &type, &array ),
           status,
           msgSetc ( "NAME", name ); errRep ( "gsdac_get0l", "gsdFind : Could not find element ^NAME in file", status ); );

  if ( *status != SAI__OK ) return;

  /* Check that the array flag is false and the data type is L. */
  if ( array ) {
    *status = SAI__ERROR;
    msgSetc ( "NAME", name );
    errRep ( "gsdac_get0l", "Expected scalar data for ^NAME, got an array", status );
    return;
  } else if (  type != 'L' ) {
    *status = SAI__ERROR;
    msgSetc ( "NAME", name );
    errRep ( "gsdac_get0l", "Header type of ^NAME did not match requested type L (CHAR).", status );
    return;
  }

  CALLGSD( gsdGet0l ( gsd->fileDsc, gsd->itemDsc, gsd->dataPtr,
		      itemno, value ),
           status,
           msgSetc ( "NAME", name ); errRep ( "gsdac_get0l", "gsdGet0l : Could not get ^NAME from GSD file", status ); );

}

void gsdac_get0r ( const gsd *gsd,
                   const char *name, float *value, int *status ) {

  /* Local variables */
  char array;                  /* array flag (should always be false) */
  int itemno;                  /* item number of the GSD header */
  char type;                   /* data type of the item (should always be R) */
  char unit[11];               /* unit of the GSD header */

  /* Check inherited status */
  if ( *status != SAI__OK ) return;

  /* Get the item number. */
  CALLGSD( gsdFind ( gsd->fileDsc, gsd->itemDsc, name, &itemno,
		     unit, &type, &array ),
           status,
           msgSetc ( "NAME", name ); errRep ( "gsdac_get0r", "gsdFind : Could not find element ^NAME in file", status ); );

  if ( *status != SAI__OK ) return;

  /* Check that the array flag is false and the data type is R. */
  if ( array ) {
    *status = SAI__ERROR;
    msgSetc ( "NAME", name );
    errRep ( "gsdac_get0r", "Expected scalar data for ^NAME, got an array", status );
    return;
  } else if (  type != 'R' ) {
    *status = SAI__ERROR;
    msgSetc ( "NAME", name );
    errRep ( "gsdac_get0r", "Header type of ^NAME did not match requested type R (FLOAT).", status );
    return;
  }

  CALLGSD( gsdGet0r ( gsd->fileDsc, gsd->itemDsc, gsd->dataPtr,
		      itemno, value ),
           status,
           msgSetc ( "NAME", name ); errRep ( "gsdac_get0r", "gsdGet0r : Could not get ^NAME from GSD file", status ); );

}

void gsdac_get0w ( const gsd *gsd,
                   const char *name, short *value, int *status ) {

  /* Local variables */
  char array;                  /* array flag (should always be false) */
  int itemno;                  /* item number of the GSD header */
  char type;                   /* data type of the item (should always be W) */
  char unit[11];               /* unit of the GSD header */

  /* Check inherited status */
  if ( *status != SAI__OK ) return;

  /* Get the item number. */
  CALLGSD( gsdFind ( gsd->fileDsc, gsd->itemDsc, name, &itemno,
		     unit, &type, &array ),
           status,
           msgSetc ( "NAME", name ); errRep ( "gsdac_get0w", "gsdFind : Could not find element ^NAME in file", status ); );

  if ( *status != SAI__OK ) return;

  /* Check that the array flag is false and the data type is W. */
  if ( array ) {
    *status = SAI__ERROR;
    msgSetc ( "NAME", name );
    errRep ( "gsdac_get0w", "Expected scalar data for ^NAME, got an array", status );
    return;
  } else if (  type != 'W' ) {
    *status = SAI__ERROR;
    msgSetc ( "NAME", name );
    errRep ( "gsdac_get0w", "Header type of ^NAME did not match requested type W (SHORT).", status );
    return;
  }

  CALLGSD( gsdGet0w ( gsd->fileDsc, gsd->itemDsc, gsd->dataPtr,
		      itemno, value ),
           status,
           msgSetc ( "NAME", name ); errRep ( "gsdac_get0w", "gsdGet0w : Could not get ^NAME from GSD file", status ); );

}
