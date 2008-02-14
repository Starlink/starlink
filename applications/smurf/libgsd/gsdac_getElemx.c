/*
*+
*  Name:
*     gsdac_getElemx.c

*  Purpose:
*     Return a single element of a GSD array.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     gsdac_getElem{blwirdc} ( const gsd *gsd,
*                              char *name, const int index, 
*                              <type> *value, int *status )

*  Arguments:
*     gsd = const gsd* (Given)
*        GSD file access parameters
*     name = char* (Given)
*        The name of the item. This should be an array of 16 characters (char
*        name[16]) and a null-terminated string.
*     index = const int (Given)
*        The index of the requested value.
*     values = <type>* (Returned)
*        The data value.  For gsdac_getElemc the value should be declared with
*        length 17 at least.  The returned string is null-terminated in 
*        value[16].
*     status = int* (Given and Returned)
*        Pointer to global status.  

*  Description:
*     These routines return the value stored in an index of an array GSD 
*     header item given by name.  The different data types are:
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
*     {enter_new_authors_here}

*  History :
*     2008-02-04 (JB):
*        Original

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
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* Starlink includes */
#include "gsd.h"
#include "sae_par.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "gsdac.h"

#define MAXDIMS 3

void gsdac_getElemb ( const gsd *gsd, char *name, 
                      const int index, char *value, 
                      int *status ) {
 
  /* Local variables */
  int size;                    /* size of array */
  char *data;                  /* array data */

  /* Check inherited status */
  if ( *status != SAI__OK ) return;

  /* Get the size of the data array. */
  gsdac_getArraySize ( gsd, name, &size, status );

  if ( *status != SAI__OK ) {
    msgSetc ( "NAME", name );
    errRep ( "gsdac_getElemb", "Error retrieving array size for ^NAME", 
             status );
    return;
  }

  /* Check for a sensible index. */
  if ( index > ( size - 1 ) || index < 0 ) {
    msgSetc ( "NAME", name );
    errRep ( "gsdac_getElemb", "Index value not in range for ^NAME", 
             status );
    return;
  }  

  /* Allocate memory. */
  data = smf_malloc ( size, sizeof( char ), 0, status );

  /* Get the data. */
  gsdac_get1b ( gsd, name, data, status );

  if ( *status != SAI__OK ) {
    smf_free ( data, status );
    msgSetc ( "NAME", name );
    errRep ( "gsdac_getElemb", "Error retrieving array values for ^NAME", 
              status );
    return;
  }  

  /* Get the indexed value. */
  *value = data[index];

  /* Free allocated memory. */
  smf_free ( data, status );

}

void gsdac_getElemc ( const gsd *gsd,
                      char *name, const int index, char *value, 
                      int *status ) {

  /* Local variables */
  int size;                    /* size of array */
  char *data;                  /* array data */

  /* Check inherited status */
  if ( *status != SAI__OK ) return;

  /* Get the size of the data array. */
  gsdac_getArraySize ( gsd, name, &size, status );

  if ( *status != SAI__OK ) {
    msgSetc ( "NAME", name );
    errRep ( "gsdac_getElemc", "Error retrieving array size for ^NAME", 
             status );
    return;
  }

  /* Check for a sensible index. */
  if ( index > ( size - 1 ) || index < 0 ) {
    msgSetc ( "NAME", name );
    errRep ( "gsdac_getElemc", "Index value not in range for ^NAME", 
             status );
    return;
  }  

  /* Allocate memory. */
  data = smf_malloc ( size, sizeof( char[17] ), 0, status );

  /* Get the data. */
  gsdac_get1c ( gsd, name, data, status );

  if ( *status != SAI__OK ) {
    smf_free ( data, status );
    msgSetc ( "NAME", name );
    errRep ( "gsdac_getElemc", "Error retrieving array values for ^NAME", 
              status );
    return;
  }

  /* Get the indexed value. */
  strncpy ( value, &(data[index]), 16 );
  value[16] = '\0';

  /* Free allocated memory. */
  smf_free ( data, status );

}

void gsdac_getElemd ( const gsd *gsd,
                      char *name, const int index, double *value, 
                      int *status ) {

  /* Local variables */
  int size;                    /* size of array */
  double *data;                  /* array data */

  /* Check inherited status */
  if ( *status != SAI__OK ) return;

  /* Get the size of the data array. */
  gsdac_getArraySize ( gsd, name, &size, status );

  if ( *status != SAI__OK ) {
    msgSetc ( "NAME", name );
    errRep ( "gsdac_getElemd", "Error retrieving array size for ^NAME", 
             status );
    return;
  }

  /* Check for a sensible index. */
  if ( index > ( size - 1 ) || index < 0 ) {
    msgSetc ( "NAME", name );
    errRep ( "gsdac_getElemd", "Index value not in range for ^NAME", 
             status );
    return;
  }  

  /* Allocate memory. */
  data = smf_malloc ( size, sizeof( double ), 0, status );

  /* Get the data. */
  gsdac_get1d ( gsd, name, data, status );

  if ( *status != SAI__OK ) {
    smf_free ( data, status );
    msgSetc ( "NAME", name );
    errRep ( "gsdac_getElemd", "Error retrieving array values for ^NAME", 
              status );
    return;
  }

  /* Get the indexed value. */
  *value = data[index];

  /* Free allocated memory. */
  smf_free ( data, status );

}

void gsdac_getElemi ( const gsd *gsd,
                      char *name, const int index, int *value, 
                      int *status ) {

  /* Local variables */
  int size;                    /* size of array */
  int *data;                  /* array data */

  /* Check inherited status */
  if ( *status != SAI__OK ) return;

  /* Get the size of the data array. */
  gsdac_getArraySize ( gsd, name, &size, status );

  if ( *status != SAI__OK ) {
    errRep ( "gsdac_getElemi", "Error retrieving array size for ^NAME", 
             status );
    return;
  }

  /* Check for a sensible index. */
  if ( index > ( size - 1 ) || index < 0 ) {
    msgSetc ( "NAME", name );
    errRep ( "gsdac_getElemi", "Index value not in range for ^NAME", 
             status );
    return;
  }  

  /* Allocate memory. */
  data = smf_malloc ( size, sizeof( int ), 0, status );

  /* Get the data. */
  gsdac_get1i ( gsd, name, data, status );

  if ( *status != SAI__OK ) {
    smf_free ( data, status );
    msgSetc ( "NAME", name );
    errRep ( "gsdac_getElemi", "Error retrieving array values for ^NAME", 
              status );
    return;
  }

  /* Get the indexed value. */
  *value = data[index];

  /* Free allocated memory. */
  smf_free ( data, status );

}

void gsdac_getEleml ( const gsd *gsd,
                      char *name, const int index, char *value, 
                      int *status ) {

  /* Local variables */
  int size;                    /* size of array */
  char *data;                  /* array data */

  /* Check inherited status */
  if ( *status != SAI__OK ) return;

  /* Get the size of the data array. */
  gsdac_getArraySize ( gsd, name, &size, status );

  if ( *status != SAI__OK ) {
    msgSetc ( "NAME", name );
    errRep ( "gsdac_getEleml", "Error retrieving array size for ^NAME", 
             status );
    return;
  }

  /* Check for a sensible index. */
  if ( index > ( size - 1 ) || index < 0 ) {
    msgSetc ( "NAME", name );
    errRep ( "gsdac_getEleml", "Index value not in range for ^NAME", 
             status );
    return;
  }  

  /* Allocate memory. */
  data = smf_malloc ( size, sizeof( char ), 0, status );

  /* Get the data. */
  gsdac_get1l ( gsd, name, data, status );

  if ( *status != SAI__OK ) {
    smf_free ( data, status );
    msgSetc ( "NAME", name );
    errRep ( "gsdac_getEleml", "Error retrieving array values for ^NAME", 
              status );
    return;
  }

  /* Get the indexed value. */
  *value = data[index];

  /* Free allocated memory. */
  smf_free ( data, status );

}

void gsdac_getElemr ( const gsd *gsd,
                      char *name, const int index, float *value, 
                      int *status ) {

  /* Local variables */
  int size;                    /* size of array */
  float *data;                  /* array data */

  /* Check inherited status */
  if ( *status != SAI__OK ) return;

  /* Get the size of the data array. */
  gsdac_getArraySize ( gsd, name, &size, status );
 
  if ( *status != SAI__OK ) {
    msgSetc ( "NAME", name );
    errRep ( "gsdac_getElemr", "Error retrieving array size for ^NAME", 
             status );
    return;
  }
 
  /* Check for a sensible index. */
  if ( index > ( size - 1 ) || index < 0 ) {
    msgSetc ( "NAME", name );
    errRep ( "gsdac_getElemr", "Index value not in range for ^NAME", 
             status );
    return;
  }  

  /* Allocate memory. */
  data = smf_malloc ( size, sizeof( float ), 0, status );

  /* Get the data. */
  gsdac_get1r ( gsd, name, data, status );

  if ( *status != SAI__OK ) {
    smf_free ( data, status );
    msgSetc ( "NAME", name );
    errRep ( "gsdac_getElemr", "Error retrieving array values for ^NAME", 
              status );
    return;
  }

  /* Get the indexed value. */
  *value = data[index];

  /* Free allocated memory. */
  smf_free ( data, status );

}

void gsdac_getElemw ( const gsd *gsd,
                      char *name, const int index, short *value, 
                      int *status ) {

  /* Local variables */
  int size;                    /* size of array */
  short *data;                  /* array data */

  /* Check inherited status */
  if ( *status != SAI__OK ) return;

  /* Get the size of the data array. */
  gsdac_getArraySize ( gsd, name, &size, status );

  if ( *status != SAI__OK ) {
    msgSetc ( "NAME", name );
    errRep ( "gsdac_getElemw", "Error retrieving array size for ^NAME", 
             status );
    return;
  }

  /* Check for a sensible index. */
  if ( index > ( size - 1 ) || index < 0 ) {
    msgSetc ( "NAME", name );
    errRep ( "gsdac_getElemw", "Index value not in range for ^NAME", 
             status );
    return;
  }  

  /* Allocate memory. */
  data = smf_malloc ( size, sizeof( short ), 0, status );

  /* Get the data. */
  gsdac_get1w ( gsd, name, data, status );

  if ( *status != SAI__OK ) {
    smf_free ( data, status );
    msgSetc ( "NAME", name );
    errRep ( "gsdac_getElemw", "Error retrieving array values for ^NAME", 
              status );
    return;
  }

  /* Get the indexed value. */
  *value = data[index];

  /* Free allocated memory. */
  smf_free ( data, status );

}



