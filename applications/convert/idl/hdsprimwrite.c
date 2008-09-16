/*+
* Name:
*    hdsprimwrite

*  Purpose:
*     To write an IDL primitive to an HDS file.

*  Language:
*     C

*  Invocation:
*     Call from C
*     int hdsprimwrite(HDSLoc *toploc, char *hdstype, int ndims, int *dims,
*         void *data, int *status ) {

*  Arguments:
*     toploc = HDSLoc * (Given)
*        An HDS locator to an HDS component to which the data are to be written
*     hdstype = char * (Given)
*        The HDS type to be written
*     ndims = int (Given)
*        The number of dimensions
*     dims = int * (Given)
*        The dimensions
*     data = void * (Given)
*        Pointer to the IDL data
*     status = int * (Given and Returned)
*        The Starlink global status

*  Description:
*     Character data is a special case, requiring  conversion from the IDL
*     form of strings.
*     The data is put into the HDS component, other type conversion is done
*     by HDS if necessary.

*  Pitfalls:
*     [pitfall_description]...

*  Notes:
*     -  {noted_item}
*     [routine_notes]...

*  External Routines Used:
*     SAE
*        sae_par.h
*     HDS
*        hds.h
*        dat_name
*        dat_vec
*        dat_size
*        dat_cell
*        dat_ncomp
*        dat_type
*        dat_index
*        dat_mapv
*        dat_unmap
*        dat_annul

*  Implementation Deficiencies:
*     -  {deficiency}
*     [routine_deficiencies]...

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*     All Rights Reserved.

*  Authors:
*     AJC: A.J.Chipperfield (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*      12-OCT-1999 (AJC):
*        Original version.
*      22-NOV-2005 (TIMJ):
*        Use modern C HDS interface
*      15-SEP-2008 (TIMJ):
*        3 arg emsSetc is deprecated.
*     {enter_further_changes_here}

*  Bugs:
*     -  {description_of_bug}
*     {note_new_bugs_here}
*-
*/
#include <stdio.h>
#include <string.h>
#include "export.h"
#include "sae_par.h"
#include "hds.h"
#include "ems.h"
#include "ems_par.h"
#include "star/hds.h"
#include "hds2idl.h"

void hdsprimwrite(HDSLoc *toploc, char *hdstype, int ndims, int *dims,
                    void *data, int *status ) {
int clen;       /* length of strings in array */
char **carray;  /* Pointer to an array of C strings */
int i;
 hdsdim hdims[MAXDIMS];

   if ( *status != SAI__OK ) return;

   /* Copy dims to HDS type */
   for (i = 0; i < ndims ; i ++ ) {
     hdims[i] = dims[i];
   }

   if ( strncmp( hdstype, "_CHAR", 5 ) ) {
      if ( *hdstype == '_' ) {
         datPut( toploc, hdstype, ndims, dims, data, status );
      } else {
         *status = SAI__ERROR;
         emsSetnc( "TYPE", hdstype, EMS__SZTOK );
         emsRep(" ",
           "Cannot handle primitive data type ^TYPE.", status );
      }
   } else {
      clen = atoi( hdstype+6 )+1;
      carray = getstringarray( ndims, dims, data );
      idlDatPutc( toploc, ndims, dims, carray, clen, status );
      retstringarray(carray);
   }
return;
}
