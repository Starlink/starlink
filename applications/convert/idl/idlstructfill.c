/*+
* Name:
*    idlstructfill

*  Purpose:
*     To read data from an HDS file into an IDL structure

*  Language:
*     C

*  Invocation:
*     Call from C
*     void idlstructfill( char *sloc, IDL_SREF sref, int *status ) {

*  Arguments:
*     sloc = char * (Given);
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
*     {enter_further_changes_here}

*  Bugs:
*     -  {description_of_bug}
*     {note_new_bugs_here}
*-
*/

#include <stdio.h>
#include "export.h"
#include "sae_par.h"
#include "hds.h"
#include "ems.h"

#define MAXDIMS 7

int typecheck( char *hdstype, UCHAR idltype ) {
return SAI__OK;
}

void idlstructfill( char *sloc, IDL_SREF sref, int *status ) {
int i, j, k;              /* Loop counters */
int nscomp;               /* Number of structure components */
int ncomp;                /* Number of hds components */
int ndims;                /* Number of hds dimensions */
int dims[DAT__MXDIM];     /* dimensions */
char type[DAT__SZTYP+1];  /* type as C string */
char hdsstructype[DAT__SZTYP+30];
UCHAR idltype;            /* IDL type code */
unsigned char *sdata;     /* pointer to structure data */
unsigned char *sdatac;    /* pointer to component structure data */
IDL_LONG toffset;         /* offset to tag data */
unsigned char *tdata;     /* pointer to tag data */
IDL_VPTR datav;           /* pointer to tag data variable */
IDL_VPTR chptr;           /* Scratch variable pointer */

int bpix;                 /* Number of bytes/pixel */
IDL_StructDefPtr sdef;    /* pointer to the structure definition */
IDL_StructDefPtr sdefc;   /* pointer to the structure component definition */
IDL_STRING *strings;      /* pointer to array of string structures */
char *chars;              /* pointer to imported characters */

char vloc[DAT__SZLOC];    /* Vector locator */
char aloc[DAT__SZLOC];    /* Array structure element locator */
char cloc[DAT__SZLOC];    /* Component locator */

int nsels;                /* Number of structure elements */

IDL_SREF sref2;           /* sub-directory sref */
IDL_ARRAY arr;            /* subdirectory array structure */

   if ( *status != SAI__OK ) return;
/* Get pointer to the structure data */
   sdata = sref.arr->data;
   sdef = sref.sdef;

/* and set up a dummy sref in case of recursion */
   sref2.arr = &arr;

/* Get the HDS structure shape */
   idlDatShape( sloc, MAXDIMS, dims, &ndims, status );
/* Treat it as a vector of structures */
   idlDatVec( sloc, vloc, status );
/* Get its size */
   idlDatSize( vloc, &nsels, status );
/* Set the HDS structure type */
   idlDatType(  sloc, type, status );

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
            sprintf(hdsstructype+i,"%d,%n",dims[j],&k);
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
         idlDatCell( vloc, 1, &k, aloc, status );
         if ( *status != SAI__OK ) {
            emsRep( " ", 
            "Failed to get locator to structure array element", status );
         } else {
            idlDatNcomp( aloc, &ncomp, status );
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
                     idlDatIndex( aloc, i, cloc, status );
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
                        idlDatAnnul( cloc, status );
                     } /* end index  got OK */
                  } /* end offset got OK */
               } /* end for each component */
            } /* end of component number agreement OK */
         idlDatAnnul( aloc, status );
         } /* end of structure cell got OK */
/* increment pointer to structure start */
      } /* end of for each structure vector element */
   idlDatAnnul( vloc, status );
   } /* end of got tag0 info */
   return;

}


