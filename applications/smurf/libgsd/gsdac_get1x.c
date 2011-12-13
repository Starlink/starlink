/*
*+
*  Name:
*     gsdac_get1x.c

*  Purpose:
*     Return contents of a GSD array.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     gsdac_get1{blwirdc} ( const gsd *gsd,
*                           const char *name, <type> *values , int *status )

*  Arguments:
*     gsd = const gsd* (Given)
*        GSD file access parameters
*     name = const char* (Given)
*        The name of the item. This should be an array of 16 characters (char
*        name[16]) and a null-terminated string.
*     values = <type>* (Returned)
*        The data values.  For gsdac_get1c the routine returns a byte array of
*        the strings concatenated (no terminators).
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     These routines return the value stored in an array GSD header item
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
*
*     An appropriate amount of memory must be allocated to the array
*     values prior to calling gsdac_get1x (use gsdac_getArraySize to
*     determine how much memory to allocate).

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

#define MAXDIMS 3

void gsdac_get1b ( const gsd *gsd,
                   const char *name, char *values, int *status ) {

  /* Local variables */
  int actDims;                 /* actual number of dimensions */
  int actVals;                 /* actual number of values retrieved */
  char array;                  /* array flag (should always be true) */
  char dimMem[MAXDIMS][16];    /* actual memory for dimension names */
  char *dimNames[MAXDIMS];     /* pointers to dimension names */
  int dimVals[MAXDIMS];        /* array dimensions */
  int i;                       /* loop counter */
  int itemno;                  /* item number of the GSD header */
  int start;                   /* start index of the array values */
  int size;                    /* number of elements in the array */
  char type;                   /* data type of the item (should always be B) */
  char unit[11];               /* unit of the GSD header */
  char unitMem[MAXDIMS][11];   /* actual memory for dimension units */
  char *unitNames[MAXDIMS];    /* pointers to unit names */

  /* Check inherited status */
  if ( *status != SAI__OK ) return;

  /* Get the item number. */
  CALLGSD( gsdFind ( gsd->fileDsc, gsd->itemDsc, name, &itemno,
		     unit, &type, &array ),
           status,
           msgSetc ( "NAME", name ); errRep ( "gsdac_get1b", "gsdFind : Could not find element ^NAME in file", status ); );

  if ( *status != SAI__OK ) return;

  /* Check that the array flag is true and the data type is B. */
  if ( !array ) {
    *status = SAI__ERROR;
    msgSetc ( "NAME", name );
    errRep ( "gsdac_get1b", "Expected array data for ^NAME, got a scalar",
             status );
    return;
  } else if (  type != 'B' ) {
    *status = SAI__ERROR;
    msgSetc ( "NAME", name );
    errRep ( "gsdac_get1b", "Header type of ^NAME did not match requested type B (CHAR).", status );
    return;
  }

  /* Set up pointers for the dimension names (see NOTES in gsdInqSize for
     explanation). */
  for ( i = 0; i < MAXDIMS; i++ )
    dimNames[i] = dimMem[i];

  /* Set up pointers for the unit names (see NOTES in gsdInqSize for
     explanation). */
  for ( i = 0; i < MAXDIMS; i++ )
    unitNames[i] = unitMem[i];

  /* Get the dimensionality. */
  CALLGSD( gsdInqSize ( gsd->fileDsc, gsd->itemDsc, gsd->dataPtr,
                         itemno, MAXDIMS, dimNames, unitNames,
                         dimVals, &actDims, &size ),
           status,
           msgSetc ( "NAME", name ); errRep ( "gsdac_get1b", "gsdinqSize : Error retrieving array dimensionality for ^NAME", status ); );

  if ( *status != SAI__OK ) return;

  start = 1;

  /* Get the array data. */
  CALLGSD( gsdGet1b ( gsd->fileDsc, gsd->itemDsc, gsd->dataPtr,
		      itemno, 1, &size, &start, &size,
                      values, &actVals ),
           status,
           msgSetc ( "NAME", name ); errRep ( "gsdac_get1b", "gsdGet1b : Could not get ^NAME from GSD file", status ); );

}

void gsdac_get1c ( const gsd *gsd,
                   const char *name, char *values, int *status ) {

  /* Local variables */
  int actDims;                 /* actual number of dimensions */
  int actVals;                 /* actual number of values retrieved */
  char array;                  /* array flag (should always be true) */
  char dimMem[MAXDIMS][16];    /* actual memory for dimension names */
  char *dimNames[MAXDIMS];     /* pointers to dimension names */
  int dimVals[MAXDIMS];        /* array dimensions */
  int i;                       /* loop counter */
  int itemno;                  /* item number of the GSD header */
  int start;                   /* start index of the array values */
  int size;                    /* number of elements in the array */
  char type;                   /* data type of the item (should always be B) */
  char unit[11];               /* unit of the GSD header */
  char unitMem[MAXDIMS][11];   /* actual memory for dimension units */
  char *unitNames[MAXDIMS];    /* pointers to unit names */

  /* Check inherited status */
  if ( *status != SAI__OK ) return;

  /* Get the item number. */
  CALLGSD( gsdFind ( gsd->fileDsc, gsd->itemDsc, name, &itemno,
		     unit, &type, &array ),
           status,
           msgSetc ( "NAME", name ); errRep ( "gsdac_get1c", "gsdFind : Could not find element ^NAME in file", status ); );

  if ( *status != SAI__OK ) return;

  /* Check that the array flag is true and the data type is C. */
  if ( !array ) {
    *status = SAI__ERROR;
    msgSetc ( "NAME", name );
    errRep ( "gsdac_get1c", "Expected array data for ^NAME, got a scalar",
             status );
    return;
  } else if (  type != 'C' ) {
    *status = SAI__ERROR;
    msgSetc ( "NAME", name );
    errRep ( "gsdac_get1c", "Header type of ^NAME did not match requested type C (CHAR[16]).", status );
    return;
  }

  /* Set up pointers for the dimension names (see NOTES in gsdInqSize for
     explanation). */
  for ( i = 0; i < MAXDIMS; i++ )
    dimNames[i] = dimMem[i];

  /* Set up pointers for the unit names (see NOTES in gsdInqSize for
     explanation). */
  for ( i = 0; i < MAXDIMS; i++ )
    unitNames[i] = unitMem[i];

  /* Get the dimensionality. */
  CALLGSD( gsdInqSize ( gsd->fileDsc, gsd->itemDsc, gsd->dataPtr,
                         itemno, MAXDIMS, dimNames, unitNames,
                         dimVals, &actDims, &size ),
           status,
           msgSetc ( "NAME", name ); errRep ( "gsdac_get1c", "gsdinqSize : Error retrieving array dimensionality for ^NAME", status ); );

  if ( *status != SAI__OK ) return;

  start = 1;

  /* Get the array data. */
  CALLGSD( gsdGet1c ( gsd->fileDsc, gsd->itemDsc, gsd->dataPtr,
		      itemno, 1, &size, &start, &size,
                      values, &actVals ),
           status,
           msgSetc ( "NAME", name ); errRep ( "gsdac_get1c", "gsdGet1c : Could not get ^NAME from GSD file", status ); );

}

void gsdac_get1d ( const gsd *gsd,
                   const char *name, double *values, int *status ) {

  /* Local variables */
  int actDims;                 /* actual number of dimensions */
  int actVals;                 /* actual number of values retrieved */
  char array;                  /* array flag (should always be true) */
  char dimMem[MAXDIMS][16];    /* actual memory for dimension names */
  char *dimNames[MAXDIMS];     /* pointers to dimension names */
  int dimVals[MAXDIMS];        /* array dimensions */
  int i;                       /* loop counter */
  int itemno;                  /* item number of the GSD header */
  int start;                   /* start index of the array values */
  int size;                    /* number of elements in the array */
  char type;                   /* data type of the item (should always be B) */
  char unit[11];               /* unit of the GSD header */
  char unitMem[MAXDIMS][11];   /* actual memory for dimension units */
  char *unitNames[MAXDIMS];    /* pointers to unit names */

  /* Check inherited status */
  if ( *status != SAI__OK ) return;

  /* Get the item number. */
  CALLGSD( gsdFind ( gsd->fileDsc, gsd->itemDsc, name, &itemno,
		     unit, &type, &array ),
           status,
           msgSetc ( "NAME", name ); errRep ( "gsdac_get1d", "gsdFind : Could not find element ^NAME in file", status ); );

  if ( *status != SAI__OK ) return;

  /* Check that the array flag is true and the data type is D. */
  if ( !array ) {
    *status = SAI__ERROR;
    msgSetc ( "NAME", name );
    errRep ( "gsdac_get1d", "Expected array data for ^NAME, got a scalar",
             status );
    return;
  } else if (  type != 'D' ) {
    *status = SAI__ERROR;
    msgSetc ( "NAME", name );
    errRep ( "gsdac_get1d", "Header type of ^NAME did not match requested type D (DOUBLE).", status );
    return;
  }

  /* Set up pointers for the dimension names (see NOTES in gsdInqSize for
     explanation). */
  for ( i = 0; i < MAXDIMS; i++ )
    dimNames[i] = dimMem[i];

  /* Set up pointers for the unit names (see NOTES in gsdInqSize for
     explanation). */
  for ( i = 0; i < MAXDIMS; i++ )
    unitNames[i] = unitMem[i];

  /* Get the dimensionality. */
  CALLGSD( gsdInqSize ( gsd->fileDsc, gsd->itemDsc, gsd->dataPtr,
                         itemno, MAXDIMS, dimNames, unitNames,
                         dimVals, &actDims, &size ),
           status,
           msgSetc ( "NAME", name ); errRep ( "gsdac_get1d", "gsdinqSize : Error retrieving array dimensionality for ^NAME", status ); );

  if ( *status != SAI__OK ) return;

  start = 1;

  /* Get the array data. */
  CALLGSD( gsdGet1d ( gsd->fileDsc, gsd->itemDsc, gsd->dataPtr,
		      itemno, 1, &size, &start, &size,
                      values, &actVals ),
           status,
           msgSetc ( "NAME", name ); errRep ( "gsdac_get1d", "gsdGet1d : Could not get ^NAME from GSD file", status ); );

}

void gsdac_get1i ( const gsd *gsd,
                   const char *name, int *values, int *status ) {

  /* Local variables */
  int actDims;                 /* actual number of dimensions */
  int actVals;                 /* actual number of values retrieved */
  char array;                  /* array flag (should always be true) */
  char dimMem[MAXDIMS][16];    /* actual memory for dimension names */
  char *dimNames[MAXDIMS];     /* pointers to dimension names */
  int dimVals[MAXDIMS];        /* array dimensions */
  int i;                       /* loop counter */
  int itemno;                  /* item number of the GSD header */
  int start;                   /* start index of the array values */
  int size;                    /* number of elements in the array */
  char type;                   /* data type of the item (should always be B) */
  char unit[11];               /* unit of the GSD header */
  char unitMem[MAXDIMS][11];   /* actual memory for dimension units */
  char *unitNames[MAXDIMS];    /* pointers to unit names */

  /* Check inherited status */
  if ( *status != SAI__OK ) return;

  /* Get the item number. */
  CALLGSD( gsdFind ( gsd->fileDsc, gsd->itemDsc, name, &itemno,
		     unit, &type, &array ),
           status,
           msgSetc ( "NAME", name ); errRep ( "gsdac_get1i", "gsdFind : Could not find element ^NAME in file", status ); );

  if ( *status != SAI__OK ) return;

  /* Check that the array flag is true and the data type is I. */
  if ( !array ) {
    *status = SAI__ERROR;
    msgSetc ( "NAME", name );
    errRep ( "gsdac_get1i", "Expected array data for ^NAME, got a scalar",
             status );
    return;
  } else if (  type != 'I' ) {
    *status = SAI__ERROR;
    msgSetc ( "NAME", name );
    errRep ( "gsdac_get1i", "Header type of ^NAME did not match requested type I (INTEGER).", status );
    return;
  }

  /* Set up pointers for the dimension names (see NOTES in gsdInqSize for
     explanation). */
  for ( i = 0; i < MAXDIMS; i++ )
    dimNames[i] = dimMem[i];

  /* Set up pointers for the unit names (see NOTES in gsdInqSize for
     explanation). */
  for ( i = 0; i < MAXDIMS; i++ )
    unitNames[i] = unitMem[i];

  /* Get the dimensionality. */
  CALLGSD( gsdInqSize ( gsd->fileDsc, gsd->itemDsc, gsd->dataPtr,
                         itemno, MAXDIMS, dimNames, unitNames,
                         dimVals, &actDims, &size ),
           status,
           msgSetc ( "NAME", name ); errRep ( "gsdac_get1i", "gsdinqSize : Error retrieving array dimensionality for ^NAME", status ); );

  if ( *status != SAI__OK ) return;

  start = 1;

  /* Get the array data. */
  CALLGSD( gsdGet1i ( gsd->fileDsc, gsd->itemDsc, gsd->dataPtr,
		      itemno, 1, &size, &start, &size,
                      values, &actVals ),
           status,
           msgSetc ( "NAME", name ); errRep ( "gsdac_get1i", "gsdGet1i : Could not get ^NAME from GSD file", status ); );

}


void gsdac_get1l ( const gsd *gsd,
                   const char *name, char *values, int *status ) {

  /* Local variables */
  int actDims;                 /* actual number of dimensions */
  int actVals;                 /* actual number of values retrieved */
  char array;                  /* array flag (should always be true) */
  char dimMem[MAXDIMS][16];    /* actual memory for dimension names */
  char *dimNames[MAXDIMS];     /* pointers to dimension names */
  int dimVals[MAXDIMS];        /* array dimensions */
  int i;                       /* loop counter */
  int itemno;                  /* item number of the GSD header */
  int start;                   /* start index of the array values */
  int size;                    /* number of elements in the array */
  char type;                   /* data type of the item (should always be B) */
  char unit[11];               /* unit of the GSD header */
  char unitMem[MAXDIMS][11];   /* actual memory for dimension units */
  char *unitNames[MAXDIMS];    /* pointers to unit names */

  /* Check inherited status */
  if ( *status != SAI__OK ) return;

  /* Get the item number. */
  CALLGSD( gsdFind ( gsd->fileDsc, gsd->itemDsc, name, &itemno,
		     unit, &type, &array ),
           status,
           msgSetc ( "NAME", name ); errRep ( "gsdac_get1l", "gsdFind : Could not find element ^NAME in file", status ); );

  if ( *status != SAI__OK ) return;

  /* Check that the array flag is true and the data type is L. */
  if ( !array ) {
    *status = SAI__ERROR;
    msgSetc ( "NAME", name );
    errRep ( "gsdac_get1l", "Expected array data for ^NAME, got a scalar",
             status );
    return;
  } else if (  type != 'L' ) {
    *status = SAI__ERROR;
    msgSetc ( "NAME", name );
    errRep ( "gsdac_get1l", "Header type of ^NAME did not match requested type L (CHAR).", status );
    return;
  }

  /* Set up pointers for the dimension names (see NOTES in gsdInqSize for
     explanation). */
  for ( i = 0; i < MAXDIMS; i++ )
    dimNames[i] = dimMem[i];

  /* Set up pointers for the unit names (see NOTES in gsdInqSize for
     explanation). */
  for ( i = 0; i < MAXDIMS; i++ )
    unitNames[i] = unitMem[i];

  /* Get the dimensionality. */
  CALLGSD( gsdInqSize ( gsd->fileDsc, gsd->itemDsc, gsd->dataPtr,
                         itemno, MAXDIMS, dimNames, unitNames,
                         dimVals, &actDims, &size ),
           status,
           msgSetc ( "NAME", name ); errRep ( "gsdac_get1l", "gsdinqSize : Error retrieving array dimensionality for ^NAME", status ); );

  if ( *status != SAI__OK ) return;

  start = 1;

  /* Get the array data. */
  CALLGSD( gsdGet1l ( gsd->fileDsc, gsd->itemDsc, gsd->dataPtr,
		      itemno, 1, &size, &start, &size,
                      values, &actVals ),
           status,
           msgSetc ( "NAME", name ); errRep ( "gsdac_get1l", "gsdGet1l : Could not get ^NAME from GSD file", status ); );

}

void gsdac_get1r ( const gsd *gsd,
                   const char *name, float *values, int *status ) {

  /* Local variables */
  int actDims;                 /* actual number of dimensions */
  int actVals;                 /* actual number of values retrieved */
  char array;                  /* array flag (should always be true) */
  char dimMem[MAXDIMS][16];    /* actual memory for dimension names */
  char *dimNames[MAXDIMS];     /* pointers to dimension names */
  int dimVals[MAXDIMS];        /* array dimensions */
  int i;                       /* loop counter */
  int itemno;                  /* item number of the GSD header */
  int start;                   /* start index of the array values */
  int size;                    /* number of elements in the array */
  char type;                   /* data type of the item (should always be B) */
  char unit[11];               /* unit of the GSD header */
  char unitMem[MAXDIMS][11];   /* actual memory for dimension units */
  char *unitNames[MAXDIMS];    /* pointers to unit names */

  /* Check inherited status */
  if ( *status != SAI__OK ) return;

  /* Get the item number. */
  CALLGSD( gsdFind ( gsd->fileDsc, gsd->itemDsc, name, &itemno,
		     unit, &type, &array ),
           status,
           msgSetc ( "NAME", name ); errRep ( "gsdac_get1r", "gsdFind : Could not find element ^NAME in file", status ); );

  if ( *status != SAI__OK ) return;

  /* Check that the array flag is true and the data type is R. */
  if ( !array ) {
    *status = SAI__ERROR;
    msgSetc ( "NAME", name );
    errRep ( "gsdac_get1r", "Expected array data for ^NAME, got a scalar",
             status );
    return;
  } else if (  type != 'R' ) {
    *status = SAI__ERROR;
    msgSetc ( "NAME", name );
    errRep ( "gsdac_get1r", "Header type of ^NAME did not match requested type R (FLOAT).", status );
    return;
  }

  /* Set up pointers for the dimension names (see NOTES in gsdInqSize for
     explanation). */
  for ( i = 0; i < MAXDIMS; i++ )
    dimNames[i] = dimMem[i];

  /* Set up pointers for the unit names (see NOTES in gsdInqSize for
     explanation). */
  for ( i = 0; i < MAXDIMS; i++ )
    unitNames[i] = unitMem[i];

  /* Get the dimensionality. */
  CALLGSD( gsdInqSize ( gsd->fileDsc, gsd->itemDsc, gsd->dataPtr,
                         itemno, MAXDIMS, dimNames, unitNames,
                         dimVals, &actDims, &size ),
           status,
           msgSetc ( "NAME", name ); errRep ( "gsdac_get1r", "gsdinqSize : Error retrieving array dimensionality for ^NAME", status ); );

  if ( *status != SAI__OK ) return;

  start = 1;

  /* Get the array data. */
  CALLGSD( gsdGet1r ( gsd->fileDsc, gsd->itemDsc, gsd->dataPtr,
		      itemno, 1, &size, &start, &size,
                      values, &actVals ),
           status,
           msgSetc ( "NAME", name ); errRep ( "gsdac_get1r", "gsdGet1r : Could not get ^NAME from GSD file", status ); );

}

void gsdac_get1w ( const gsd *gsd,
                   const char *name, short *values, int *status ) {

  /* Local variables */
  int actDims;                 /* actual number of dimensions */
  int actVals;                 /* actual number of values retrieved */
  char array;                  /* array flag (should always be true) */
  char dimMem[MAXDIMS][16];    /* actual memory for dimension names */
  char *dimNames[MAXDIMS];     /* pointers to dimension names */
  int dimVals[MAXDIMS];        /* array dimensions */
  int i;                       /* loop counter */
  int itemno;                  /* item number of the GSD header */
  int start;                   /* start index of the array values */
  int size;                    /* number of elements in the array */
  char type;                   /* data type of the item (should always be B) */
  char unit[11];               /* unit of the GSD header */
  char unitMem[MAXDIMS][11];   /* actual memory for dimension units */
  char *unitNames[MAXDIMS];    /* pointers to unit names */

  /* Check inherited status */
  if ( *status != SAI__OK ) return;

  /* Get the item number. */
  CALLGSD( gsdFind ( gsd->fileDsc, gsd->itemDsc, name, &itemno,
		     unit, &type, &array ),
           status,
           msgSetc ( "NAME", name ); errRep ( "gsdac_get1w", "gsdFind : Could not find element ^NAME in file", status ); );

  if ( *status != SAI__OK ) return;

  /* Check that the array flag is true and the data type is W. */
  if ( !array ) {
    *status = SAI__ERROR;
    errRep ( "gsdac_get1w", "Expected array data for ^NAME, got a scalar",
             status );
    return;
  } else if (  type != 'W' ) {
    *status = SAI__ERROR;
    msgSetc ( "NAME", name );
    errRep ( "gsdac_get1l", "Header type of ^NAME did not match requested type W (SHORT).", status );
    return;
  }

  /* Set up pointers for the dimension names (see NOTES in gsdInqSize for
     explanation). */
  for ( i = 0; i < MAXDIMS; i++ )
    dimNames[i] = dimMem[i];

  /* Set up pointers for the unit names (see NOTES in gsdInqSize for
     explanation). */
  for ( i = 0; i < MAXDIMS; i++ )
    unitNames[i] = unitMem[i];

  /* Get the dimensionality. */
  CALLGSD( gsdInqSize ( gsd->fileDsc, gsd->itemDsc, gsd->dataPtr,
                         itemno, MAXDIMS, dimNames, unitNames,
                         dimVals, &actDims, &size ),
           status,
           msgSetc ( "NAME", name ); errRep ( "gsdac_get1w", "gsdinqSize : Error retrieving array dimensionality for ^NAME", status ); );

  if ( *status != SAI__OK ) return;

  start = 1;

  /* Get the array data. */
  CALLGSD( gsdGet1w ( gsd->fileDsc, gsd->itemDsc, gsd->dataPtr,
		      itemno, 1, &size, &start, &size,
                      values, &actVals ),
           status,
           msgSetc ( "NAME", name ); errRep ( "gsdac_get1w", "gsdGet1w : Could not get ^NAME from GSD file", status ); );

}
