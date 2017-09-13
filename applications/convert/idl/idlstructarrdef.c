/*+
* Name:
*    idlstructarrdef
*
* Purpose:
*    To Create an IDL structure based on an HDS file array of structures
*
* Language:
*    C
*
* Invocation:
*    Call from C
*      struct = idlstructarrdef( sloc, name, ndims, dims, status );
*
* Arguments:
*    sloc = HDSLoc * (Given)
*       Locator to the HDS structure
*
*    name = char * (Given)
*       Name of the structure
*
*    ndims = int (Given)
*       Number of dimensions
*
*    dims = hdsdim[] (Given)
*       Array of dimensions
*
*    status = *int (Given and Returned)
*       Global status
*
* Returned Value:
*    sdef = IDL_StructDefPtr
*       A pointer to the created IDL structure definition
*
* Method:
*    For IDL arrays of structures each element must have the same structure
*    so HDS arrays of structures are represented by an IDL structure with
*    HDSSTRUCTYPE hdstype(dim1,dim2...) and a separate structure component
*    named NAME_index1_index2... representing each element of the array of
*    structures.
*    The HDS structure is analysed and a list of corresponding IDL tags
*    created. The function IDL_MakeStruct is then called to create the
*    structure. If the tag is itself a structure, this function is called
*    recursively.
*
* Copyright:
*   Copyright (C) 1999 Central Laboratory of the Research Councils
*
* Authors:
*    AJC: A.J.Chipperfield (Starlink, RAL)
*    {enter_new_authors_here}
*
* History:
*     30-NOV-1999 (AJC):
*       Original version
*     22-NOV-2005 (TIMJ):
*       Use modern HDS interface
*    {enter_further_changes_here}
*-
*/
#include <stdio.h>
#include "export.h"
#include "sae_par.h"
#include "hds2idl.h"

#define TAG__SZNAM DAT__SZNAM+30

IDL_StructDefPtr idlstructarrdef(
   HDSLoc *sloc, char *name, int ndims, hdsdim dims[], int *status ) {

static char hdsstructype[13] =  "HDSSTRUCTYPE";
static IDL_STRUCT_TAG_DEF niltagdef={0};

IDL_STRUCT_TAG_DEF *tags;
IDL_VPTR tagvar;
IDL_VPTR tagnamevar;
hdsdim i;                 /* Loop counter */
int j;                    /* Loop counters */
size_t nels;              /* Number of elements */
char suffix[5]="_";       /* Name suffix component */
int index[DAT__MXDIM]={0};/* current indices */

char *tagnames;           /* pointer to tagnames */
char *tagname;            /* pointer within tagnames */

void *s = NULL;          /* pointer to the created structure */
HDSLoc * vloc = NULL;    /* Vector locator */
HDSLoc * cloc = NULL;    /* Array cell locator */

 if ( !(*status == SAI__OK) ) return (NULL);

/* Get memory for tags */
   datSize ( sloc, &nels, status );
   tags = (IDL_STRUCT_TAG_DEF *)IDL_GetScratch( &tagvar, (IDL_LONG)nels+2,
            (IDL_LONG)sizeof(IDL_STRUCT_TAG_DEF) );

/* Set tag for the HDS type */
   tags[0].name = hdsstructype;
   tags[0].dims = 0;
   tags[0].type = (void *)IDL_TYP_STRING;
   tags[0].flags = '\0';

/* Get memory for tagnames - allow for 7 3-digit suffixes (highly unlikely) */
/* nels is bound to be at least 1.                                           */
   tagnames = (char *)IDL_GetScratch( &tagnamevar, (IDL_LONG)nels,
              (IDL_LONG)TAG__SZNAM );
   tagname = tagnames;

/* Get the array as a vector */
   datVec( sloc, &vloc, status );

/* Now go through the structure setting up the tags array */
/* for each element. Each will be a scalar structure      */
   for ( i=1; (*status==SAI__OK) && (i<=nels); i++ ) {
      datCell( vloc, 1, &i, &cloc, status );

      if ( *status == SAI__OK ) {

/* Construct the tagname */
         strcpy( tagname, name );
         for( j=0; j<ndims; j++ ) {
            sprintf( suffix+1, "%d", index[j] );
            strcat( tagname, suffix );
         }
         tags[i].name = tagname;
         tagname += TAG__SZNAM;

/* Enter the dimensions (scalar) */
         tags[i].dims = 0;

/* Set up the tag type */
/* It's a structure    */
         tags[i].type = (void*)idlstructdef(cloc, status);

/* Set the flags */
         tags[i].flags = '\0';

/* Increment the index array */
         index[0]++;
         for ( j=1; j<ndims; j++ ) {
            if( index[j-1] == dims[j-1] ) {
               index[j-1] = 0;
               index[j]++;
            }
         }
      } /* end cell got OK */

      datAnnul( &cloc, status );

   } /* end for each element */

   datAnnul( &vloc, status );

   if ( *status == SAI__OK ) {
/* Terminate the tags */
      tags[i] = niltagdef;

/* Create a structure */
      s = IDL_MakeStruct( NULL, tags );
   }

/* Clean up */
   IDL_Deltmp(tagvar);
   IDL_Deltmp(tagnamevar);

/* and return the structure */
   return( s );
}
