/*+
* Name:
*    getobjectdetails

*  Purpose:
*     To get the type, shape and size of an object.

*  Language:
*     C

*  Invocation:
*     Call from C
*     getobjectdetails( IDL_VPTR var, void *data, char *taglist[],
*          char hdstype[], int *numtags, int *ndims, hdsdim dims[],
*          int *elt_len, int *status ) {

*  Arguments:
*     var = IDL_VPTR (Given)
*        Variable containing data
*     data = *void (Given)
*        Pointer to data for the variable (needed for getidlstringsize)
*     taglist = char *[] (Given)
*        Pointer to array tagnames
*     hdstype = char[] (Returned)
*        The HDS type to be created
*     numtags = int * (Returned)
*        Number of tags (0 implies non-structure)
*     ndims = int * (Returned)
*        The number of dimensions
*     dims = hdsdim[] (Returned)
*        The dimensions
*     elt_len = int * (Returned)
*        Length of an array element
*     status = int * (Given and Returned)
*        The Starlink global status

*  Description:
*     The HDS type, shape and size of the HDS object which will correspond
*     to the given IDL variable are returned as arguments. If the first tag
*     in the given taglist for the structure is HDSSTRUCTYPE, the value of
*     that component of the IDL structure is returned as the type of the
*     HDS structure; otherwise, the value returned by gethdstype (i.e. "")
*     is used.

*  Pitfalls:
*     [pitfall_description]...

*  Notes:
*     -  {noted_item}
*     [routine_notes]...

*  External Routines Used:
*     SAE
*        sae_par.h

*  Implementation Deficiencies:
*     -  {deficiency}
*     [routine_deficiencies]...

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils

*  Authors:
*     AJC: A.J.Chipperfield (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*      12-OCT-1999 (AJC):
*        Original version.
*      29-NOV-1999 (AJC):
*        Handle structure arrays
*     {enter_further_changes_here}

*  Bugs:
*     -  {description_of_bug}
*     {note_new_bugs_here}
*-
*/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "export.h"
#include "sae_par.h"
#include "hds.h"

int getidlstringsize( IDL_VPTR var, IDL_STRING *data );

char *gethdstype( UCHAR idltype );

void getobjectdetails( IDL_VPTR var, void *data, char *taglist[],
                    char hdstype[], int *numtags,
                    int *ndims, hdsdim dims[], int *elt_len, int *status ) {

UCHAR idltype;     /* Type of variable */
int i;             /* counter */
int *vardim;
IDL_VPTR tmpvar;
IDL_LONG offset;
char *dimptr;
char *tok;

      if ( *status != SAI__OK ) return;
/* Type */
      idltype = var->type;
      strcpy(hdstype, gethdstype( idltype ));

      if ( var->flags & IDL_V_STRUCT ) {
/* Structure */
         *numtags = IDL_StructNumTags( var->value.s.sdef );
         *elt_len = var->value.s.arr->elt_len;
         if ( var->value.s.arr->n_elts == 1 )
            *ndims = 0;
         else
            *ndims = (int)var->value.s.arr->n_dim;
            vardim = (int *)var->value.s.arr->dim;
            for ( i=0; i<*ndims; i++ ) dims[i] = vardim[i];

         if ( !strcmp( *taglist, "HDSSTRUCTYPE" ) ) {
/* Get the tag info */
            offset = IDL_StructTagInfoByIndex(
               var->value.s.sdef, 0, IDL_MSG_LONGJMP, &tmpvar );
            strcpy( hdstype, IDL_STRING_STR((IDL_STRING *)data+offset) );
         }

/* If it's an array structure -    */
         if ( dimptr=strpbrk(hdstype,"(") ) {
/*    terminate hdstype at the (   */
            *dimptr++ = '\0';
/*    and get the array dimensions */
            for ( i=0; tok=strtok(i?NULL:dimptr,",)"); i++ ){
               dims[i] = atoi(tok);
            }
         }
         *ndims = i;

      } else if ( var->flags & IDL_V_ARR ) {
/* Primitive Array */
         *numtags = 0;;
         *ndims = (int)var->value.arr->n_dim;
         vardim = (int *)var->value.s.arr->dim;
         for ( i=0; i<*ndims; i++ ) dims[i] = vardim[i];

      } else {
/* Primitive scalar */
         *numtags = 0;
         *ndims = 0;
      }

/*    if it's IDL_STRING find the largest string size */
      if ( idltype == 7 ) {
         strcat(hdstype,"*");
         i = getidlstringsize(var,(IDL_STRING *)data);
         sprintf(hdstype+6,"%d",i);
      }

      return;
}
