/*+
* Name:
*    getcomp

*  Purpose:
*     Get a locator to an HDS object

*  Language:
*     C

*  Invocation:
*     Call from C
*     getcomp(char *object, char *acmode, HDSLoc **loc, int *status )

*  Arguments:
*     object = char * (Given)
*        String giving the object path
*     acmode = char * (Given)
*        The access mode required, READ, WRITE or UPDATE
*     loc = HDSLoc ** (Returned)
*        The locator
*     status = int * (Given and Returned)
*        The Starlink global status

*  Description:
*     The associated file is opened and the path followed to obtain the
*     required locator.

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
*        hdsOpen
*        datFind
*        datSlice
*        datCell
*        datAnnul
*        datPrmry
*        datClone

*  Implementation Deficiencies:
*     -  {deficiency}
*     [routine_deficiencies]...

*  Copyright:
*     Copyright (C) 2000 Central Laboratory of the Research Councils

*  Authors:
*     AJC: A.J.Chipperfield (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*      29-FEB-2000 (AJC):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     -  {description_of_bug}
*     {note_new_bugs_here}
*-
*/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "sae_par.h"
#include "ems.h"
#define FALSE 0
#define TRUE 1

#include "hds2idl.h"

void getcomp( char *object, const char *acmode, HDSLoc ** loc, int *status ) {

HDSLoc * botloc = NULL;
HDSLoc * tmploc = NULL;
char *comp;
char *name = NULL;
char *saveptr;
char *tempstr = NULL;
hdsdim ends[DAT__MXDIM];
int file_opened=FALSE;
int ndims;
int slice;
hdsdim starts[DAT__MXDIM];
int true=TRUE;

   if ( ( (tempstr = (char *)calloc(strlen(object)+2,sizeof(char))) != NULL )
	&& ( (name = (char *)malloc(strlen(object))) != NULL ) ) {

      saveptr = strcpy( tempstr, object );

/* Only the last component spec can be a slice so set slice false here */
/* and test initially and after each but the last                      */
      slice = FALSE;
      while ( ( *status == SAI__OK ) &&
           ( (comp=strtok( saveptr, "." )) != NULL ) ) {

         botloc=NULL;
         tmploc=NULL;
         saveptr = comp + strlen(comp) + 1;
/* Check if previous was an array slice form of component spec */
         if ( slice ) {
            *status = SAI__ERROR;
            emsRep(" ", "Only the last component can be sliced", status );

         } else {
/* Get component spec details */
/* and act accordingly        */
            if (checkarr( comp, name, &slice, &ndims, starts, ends, status )) {
/* only part of array component required */
               if( file_opened ) {
                  datFind( *loc, name, &tmploc, status );
               } else {
		 hdsOpen( name, (char *)acmode, &tmploc, status );
               }

               if ( slice ) {
                  datSlice( tmploc, ndims, starts, ends, &botloc, status );
               } else {
                  datCell( tmploc, ndims, starts, &botloc, status );
               }
               datPrmry( TRUE , &botloc, &true, status );
               datAnnul( &tmploc, status );

            } else {
/* Whole component required */
               if( file_opened ) {
                  datFind( *loc, name, &botloc, status );
                  datPrmry( TRUE , &botloc, &true, status );
               } else {
		 hdsOpen( name, (char *)acmode, &botloc, status );
               }
            }

/* Switch locators for next loop.                            */
/* Ensure that the object is not closed when loc is annulled */
/* by making botloc a primary locator first.                  */
            if ( !file_opened ) {
               file_opened = TRUE;
            } else {
               datAnnul( loc, status );
            }
            datClone( botloc, loc, status );
            datPrmry( TRUE, loc, &true, status );
            datAnnul( &botloc, status );
         }
      }
      free( tempstr );
      free( name );

   } else {
      *status = SAI__ERROR;
      emsRep(" ", "GETCOMP: failed to malloc space", status );
      if ( tempstr != NULL ) {
	free( tempstr );
      }
   }
}
