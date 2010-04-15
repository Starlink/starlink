/*+
*  Name:
*    idlprimfill

*  Purpose:
*     To copy HDS primitive data to IDL

*  Language:
*     C

*  Invocation:
*     Call from C
*     void idlprimfill( HDSLoc*cloc, IDL_VPTR datav, void *datptr, int *status )

*  Arguments:
*     cloc = HDSLoc * (Given);
*        An HDS locator to the HDS primitive component
*     datav = IDL_VPTR (Given);
*        VPTR describing the data (not necessarily containing the data)
*     datptr = void * (Given);
*        Pointer to the IDL data area
*     status = int * (Given and returned)
*        The Starlink global status

*  Description:

*  External Routines Used:
*     SAE
*        sae_par.h
*     EMS
*        ems.h
*        emsSeti
*        emsRep
*     HDS
*        hds.h
*        datState
*        datType
*        datMapv
*        datUnmap

*  Copyright:
*     Copyright (C) 2000 Central Laboratory of the Research Councils

*  Authors:
*     AJC: A.J.Chipperfield (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*      2-MAR-2000 (AJC):
*        Original version.
*      22-NOV-2005 (TIMJ):
*        Use new HDS interface
*     {enter_further_changes_here}

*  Bugs:
*     -  {description_of_bug}
*     {note_new_bugs_here}
*-
*/

#include <stdio.h>
#include <stdlib.h>
#include "export.h"
#include "sae_par.h"
#include "ems.h"
#include "cnf.h"
#include "hds2idl.h"

void idlprimfill( HDSLoc *cloc, IDL_VPTR datav, void *datptr, int *status ) {

int j;                  /* loop counters */
UCHAR idltype;            /* The IDL type */
char type[DAT__SZTYP+1];  /* Type in which to to map HDS data */
int bpix;                 /* Number of bytes/value */
int defined;              /* If HDS value defined */
void *cpntr;              /* True C pointer to mapped data */
size_t nels;              /* Number of mapped elements */
int nels_i;
size_t nbytes;            /* Number of bytes in array */
int flen;                 /* length of HDS strings */
int clen;                 /* length of corresponding C string */
char *chars;              /* pointer to imported characters */
IDL_STRING *strings;      /* pointer to array of string structures */
IDL_VPTR chptr;           /* Scratch variable pointer */

if ( *status != SAI__OK ) return;

/* check type compatibility */
/* Get the number of bytes per element */
/* and the HDS type in which to  map the data */
   idltype = datav->type;

   switch (idltype) {
   case IDL_TYP_FLOAT:
      strcpy( type, "_REAL" );
      bpix = 4;
      break;
   case IDL_TYP_LONG:
      strcpy( type, "_INTEGER" );
      bpix = 4;
      break;
   case IDL_TYP_INT:
      strcpy( type, "_WORD" );
      bpix = 2;
      break;
   case IDL_TYP_DOUBLE:
      strcpy( type, "_DOUBLE" );
      bpix = 8;
      break;
   case IDL_TYP_BYTE:
      strcpy( type, "_UBYTE" );
      bpix = 1;
      break;
   case IDL_TYP_STRING:
      datType( cloc, type, status);
      bpix = 1;
      break;
   default:
/* flag no data to copy */
      bpix = 0;
      *status = SAI__ERROR;
      emsSeti( "TYPE", idltype );
      emsRep( " ", "Illegal IDL type ^TYPE", status );
      break;
   } /* end of case */

   if ( (*status == SAI__OK ) && bpix ) {
/* Map the data as if a vector - provided it is defined */
      datState( cloc, &defined, status );
      if ( defined ) {
         datMapV( cloc, type, "READ", &cpntr, &nels, status );
         if ( *status != SAI__OK ) {
            emsRep(" ", "Failed to map HDS component", status );
         } else {
            if ( idltype == IDL_TYP_STRING ) {
               flen = atoi( type + 6 );
               clen = flen + 1;

/* Import the Fortran strings to C */
               nels_i = (int)nels;
               chars = IDL_GetScratch( &chptr, nels_i, clen );
               cnfImprta( cpntr, flen, chars, clen, 1, &nels_i );

/* set strings to be a pointer to the IDL_STRING structure(s) */
               strings = (IDL_STRING *)datptr;

/* store the imported strings into the STRING structures */
               for ( j=0; j<nels; j++ )
                  IDL_StrStore( strings+j, &chars[j*clen] );
               IDL_Deltmp( chptr );

            } else {
/* Type other than string */
               if ( datav->flags & IDL_V_ARR ) {
/* Number Array */
/* copy the data to the array */
                  nbytes = bpix * nels;
                  memcpy( datptr, cpntr, nbytes );
               } else {
/* Number Scalar */
                  switch (idltype) {
                  case IDL_TYP_FLOAT:
                     ((IDL_ALLTYPES *)datptr)->f = *(float *)cpntr;
                     break;
                  case IDL_TYP_LONG:
                     ((IDL_ALLTYPES *)datptr)->l = *(int *)cpntr;
                     break;
                  case IDL_TYP_INT:
                     ((IDL_ALLTYPES *)datptr)->i = *(short *)cpntr;
                     break;
                  case IDL_TYP_DOUBLE:
                     ((IDL_ALLTYPES *)datptr)->d = *(double *)cpntr;
                     break;
                  case IDL_TYP_BYTE:
                     ((IDL_ALLTYPES *)datptr)->c = *(UCHAR *)cpntr;
                     break;
                  } /* end of case */
               } /* end of if array */
            } /* end if string */
            datUnmap( cloc, status );
         } /* end of mapped data */
      } /* end of if defined */
   } /* end of bpix non-zero */

   return;

}

