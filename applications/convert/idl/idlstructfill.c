/*+
* Name:
*    idlstructfill

*  Purpose:
*     To read data from an HDS file into an IDL structure

*  Language:
*     C

*  Invocation:
*     Call from C
*     void idlstructfill( HDSLoc *sloc, IDL_SREF sref, int *status ) {

*  Arguments:
*     sloc = HDSLoc * (Given);
*        An HDS locator to the structure component of the HDS file.
*     sref = IDL_SREF (Given);
*        An IDL structure reference
*     status = int * (Given and returned)
*        The Starlink global status

*  Description:

*  External Routines Used:
*     SAE
*        sae_par.h
*     EMS
*        ems.h
*        emsRep
*     HDS
*        hds.h
*        datShape
*        datVec
*        datSize
*        datType
*        datCell
*        datNcomp
*        datIndex
*        datAnnul

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils

*  Authors:
*     AJC: A.J.Chipperfield (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     20-JUL-1999 (AJC):
*        Original version.
*     28-FEB-2000 (AJC):
*        Handle _BYTE->integer, _UWORD->long
*     29-FEB-2000 (AJC):
*        Handle array of structures at top level
*     22-NOV-2005 (TIMJ):
*        Use modern C API
*     {enter_further_changes_here}

*  Bugs:
*     -  {description_of_bug}
*     {note_new_bugs_here}
*-
*/

#include <stdio.h>
#include "export.h"
#include "sae_par.h"
#include "hds2idl.h"
#include "star/hds.h"
#include "ems.h"

int typecheck( char *hdstype, UCHAR idltype ) {
return SAI__OK;
}

void idlstructfill( HDSLoc *sloc, IDL_SREF sref, int *status ) {
int i, j;                 /* Loop counters */
hdsdim k;
int nscomp;               /* Number of structure components */
int ncomp;                /* Number of hds components */
int ndims;                /* Number of hds dimensions */
hdsdim dims[DAT__MXDIM];  /* dimensions */
char type[DAT__SZTYP+1];  /* type as C string */
char hdsstructype[DAT__SZTYP+30];
UCHAR idltype;            /* IDL type code */
unsigned char *sdata = NULL;     /* pointer to structure data */
unsigned char *sdatac = NULL;    /* pointer to component structure data */
IDL_LONG toffset;         /* offset to tag data */
unsigned char *tdata;     /* pointer to tag data */
IDL_VPTR datav;           /* pointer to tag data variable */

int bpix;                 /* Number of bytes/pixel */
IDL_StructDefPtr sdef = NULL;  /* pointer to the structure definition */
IDL_StructDefPtr sdefc = NULL; /* pointer to the structure component definition */

HDSLoc * vloc = NULL;    /* Vector locator */
HDSLoc * aloc = NULL;    /* Array structure element locator */
HDSLoc * cloc = NULL;    /* Component locator */

size_t nsels;                /* Number of structure elements */

IDL_SREF sref2;           /* sub-directory sref */
IDL_ARRAY arr;            /* subdirectory array structure */

   if ( *status != SAI__OK ) return;
/* Get pointer to the structure data */
   sdata = sref.arr->data;
   sdef = sref.sdef;

/* and set up a dummy sref in case of recursion */
   sref2.arr = &arr;

/* Get the HDS structure shape */
   datShape( sloc, MAXDIMS, dims, &ndims, status );
/* Treat it as a vector of structures */
   datVec( sloc, &vloc, status );
/* Get its size */
   datSize( vloc, &nsels, status );
/* Set the HDS structure type */
   datType(  sloc, type, status );

/* Get the IDL structure tag info */
   if ((toffset=IDL_StructTagInfoByIndex( sdef, 0, IDL_MSG_RET, &datav ))
     == -1 ) {
      *status = SAI__ERROR;
      emsRep(" ", "Failed to get tag info", status );

   } else {
/* Save the structure type */
      tdata = sdata + (int)toffset;
/* If it's a structure array, add the dimensions to the HDSTYPE */
      strcpy( hdsstructype, type);
      if( ndims ) {
         strcat( hdsstructype,"(");
         i=strlen(hdsstructype);
         for(j=0;j<ndims;j++) {
            k = sprintf(hdsstructype+i,"%d,",dims[j]);
            i+=k;
         }
         strcpy(hdsstructype+i-1,")");
      }
      IDL_StrStore( (IDL_STRING *)tdata, hdsstructype );

/* Go through structure vector filling each element */
      for ( k=1; (*status==SAI__OK) && (k<=nsels); k++ ) {
/* If it's a true structure array, get the first tag corresponding to */
/* an array element; otherwise just use the given structure           */
         if( ndims ) {
            if ((toffset =
               IDL_StructTagInfoByIndex( sdef, k, IDL_MSG_RET, &datav ))
                  == -1 ) {
               *status = SAI__ERROR;
               emsRep(" ", "Failed to get tag info", status );
            } else {
               sdefc = datav->value.s.sdef;
               sdatac = sdata + toffset;
            }
         } else {
            sdefc = sref.sdef;
            sdatac = sdata;
         }

/* Get the number of tags in the sub-structure */
         nscomp = IDL_StructNumTags( sdefc );

/* Get locator to the corresponding HDS structure array cell */
         datCell( vloc, 1, &k, &aloc, status );
         if ( *status != SAI__OK ) {
            emsRep( " ",
            "Failed to get locator to structure array element", status );
         } else {
            datNcomp( aloc, &ncomp, status );
            if ( ncomp + 1 != nscomp ) {
               if ( *status == SAI__OK )
                  *status = SAI__ERROR;
               emsRep( " ", "Structure mismatch", status );

            } else {
/* Now go through the structure, filling it in */
/* starting with the HDSSTRUCTYPE component    */
               if ((toffset =
                  IDL_StructTagInfoByIndex( sdefc, 0, IDL_MSG_RET, &datav ))
                  == -1 ) {
                  *status = SAI__ERROR;
                  emsRep(" ", "Failed to get tag info", status );
               } else {
/* calculate pointer to data */
                  tdata = sdatac + (int)toffset;
/* and insert the structure type */
                  IDL_StrStore( (IDL_STRING *)tdata, type );
               }
               for ( i=1; (*status==SAI__OK) && (i<=ncomp); i++ ) {
                  if ((toffset =
                     IDL_StructTagInfoByIndex( sdefc, i, IDL_MSG_RET, &datav ))
                       == -1 ) {
                     *status = SAI__ERROR;
                     emsRep(" ", "Failed to get tag info", status );
                  } else {
/* calculate pointer to data */
                     tdata = sdatac + (int)toffset;
/* and get IDL type */
                     idltype = datav->type;

/* Get locator to i'th component of the HDS structure */
                     datIndex( aloc, i, &cloc, status );
                     if( *status != SAI__OK ) {
                        emsRep(" ", "Failed locator to next index", status );
                     } else {
/* check type compatibility */
                        if ( idltype == IDL_TYP_STRUCT ) {
/* It's another structure */
/* Set up sref2 */
/* Complete constructing a structure reference (as much required) */
                           bpix = 0;
                           sref2.sdef = datav->value.s.sdef;
                           arr.data = tdata;
                           arr.elt_len = datav->value.s.arr->elt_len;
                           (void)idlstructfill(cloc, sref2, status);

                        } else {
/* It's primitive data */
                           idlprimfill( cloc, datav, tdata, status );
                        }
                     } /* end index  got OK */
                     datAnnul( &cloc, status );
                  } /* end offset got OK */
               } /* end for each component */
            } /* end of component number agreement OK */
         datAnnul( &aloc, status );
         } /* end of structure cell got OK */
/* increment pointer to structure start */
      } /* end of for each structure vector element */
   datAnnul( &vloc, status );
   } /* end of got tag0 info */
   return;

}


