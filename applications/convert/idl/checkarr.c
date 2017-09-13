/*+
* Name:
*
* Purpose:
*    Check an HDS component specification for array index specifiers

* Language:
*    C

* Invocation:
*    Call from C
*    isarr = checkarr( comp, name, slice, ndims, starts, ends, status );

* Arguments:
*    comp = char * (Given)
*       The component specifier - can include index or slice info
*    name = char[] (Returned)
*       The component name stripped of any index or slice info, must
*       be at least as big as "comp".
*    slice = int * (Returned)
*       If the component spec is a slice
*    ndims = int * (Returned)
*       The number of dimensions of any any index or slice info
*    starts = hdsdim[] (Returned)
*       The start indices of any index or slice info
*    ends = hdsdim[] (Returned)
*       The end indices of any index or slice info
*    status = int * (Given and Returned)
*       The Starlink global status

* Returned Value:
*    isarr = int
*       True if the component spec has any index or slice info

* Description:
*    The component spec is analysed and constituent parts returned.

* Pitfalls:
*    [pitfall_description]...

* Notes:
*    -  {noted_item}
*    [routine_notes]...

* External Routines Used:
*    SAE
*       sae_par.h

* Implementation Deficiencies:
*    -  {deficiency}
*    [routine_deficiencies]...

* Copyright:
*    Copyright (C) 2000 Central Laboratory of the Research Councils

* Authors:
*    AJC: A.J.Chipperfield (Starlink, RAL)
*    {enter_new_authors_here}

* History:
*     3-MAR-2000 (AJC):
*       Original version.
*    {enter_further_changes_here}

* Bugs:
*    -  {description_of_bug}
*    {note_new_bugs_here}
*-
*/
#include <stdlib.h>
#include <string.h>
#include "sae_par.h"
#include "ems.h"
#include "hds.h"

#define TRUE 1
#define FALSE 0

int checkarr( char *comp, char name[], int *slice, int *ndims,
              hdsdim starts[], hdsdim ends[], int *status ) {
int i,j;              /* counters */
int isarr;            /* temp return value */
int len;              /* length of remainder of index string */
char *tempstr;        /* temporary malloced string */
char *index;          /* pointer to index portion */
char *comma;          /* pointer to comma within an index spec */
char *colon;          /* pointer to colon within an index spec */

   if ( *status != SAI__OK ) return 0;

   *slice = FALSE;
   if ( (index=strchr(comp,'(')) != NULL ) {
      isarr = TRUE;
      if ( (tempstr=calloc(strlen(index)+2,sizeof(char))) != NULL ) {
         strncpy( name, comp, index-comp );
         name[index-comp] = '\0';
/* Copy index part of component spec, removing spaces and parentheses */
         for ( i=1,j=0; index[i]!='\0'; i++ ) {
            if ( index[i] != ' ' ) tempstr[j++] = index[i];
         }
         if ( tempstr[j-1] == ')' ) {
            tempstr[j-1] = ',';
            tempstr[j] = '\0';
/* tempstr contains the index information 'i:j, k:l, ...)' */
/* i, k etc may be omitted, default 1                      */
/* :j, :l etc may be omitted, j,l etc default to i,k etc   */
/* j, l etc may be omitted, default to 0 (upper bound)     */
            index = tempstr;
            for ( *ndims=0; strlen( index ); (*ndims)++ ) {
               if ( ( comma = strchr( index,',') ) != NULL ) *comma = '\0';
               len = strlen( index );
               if ( (colon=strchr( index, ':' )) != NULL ) {
/* Colon present */
                  *colon = '\0';
                  *slice = TRUE;
                  if ( strlen( index ) ) {
                     starts[*ndims] = atoi(index);
                  } else {
                     starts[*ndims] = 1;
                  }
                  if ( strlen( ++colon ) ) {
                     ends[*ndims] = atoi( colon );
                  } else {
                     ends[*ndims] = 0;
                  }

               } else {
/* No colon */
                  if ( strlen( index) ) {
                     starts[*ndims] = ends[*ndims] = atoi(index);
                  } else {
                     *slice = TRUE;
                     starts[*ndims] = 1;
                     ends[*ndims] = 0;
                  }
               }
               index+=len+1;
            }

         } else {
            *status = SAI__ERROR;
            emsRep( " ", "CHECKARR: Invalid object name", status );
            isarr = FALSE;
         }
         free( tempstr );

      } else {
         *status = SAI__ERROR;
         emsRep( " ", "CHECKARR: Failed to malloc space", status );
         isarr = FALSE;
      }

   } else {
/* No array subscript */
      isarr = FALSE;
      strcpy( name, comp );
      *ndims = 0;
   }

   return isarr;
}
