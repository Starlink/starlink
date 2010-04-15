/*+
* Name:
*    idlstructdef
*
* Purpose:
*    To Create an IDL structure based on an HDS file structure
*
* Language:
*    C
*
* Invocation:
*    Call from C
*      struct = idlstructdef( sloc, status );
*
* Arguments:
*    sloc = HDSLoc* (Given)
*       Locator to the HDS structure
*
*    status = *int (Given and Returned)
*       Global status
*
* Returned Value:
*    sdef = IDL_StructDefPtr
*       A pointer to the created IDL structure definition
*
* Method:
*    The HDS structure is analysed and a list of corresponding IDL tags
*    created. The function IDL_MakeStruct is then called to create the
*    structure. If the tag is itself a structure, this function is called
*    recursively.

*  External Routines Used:
*     SAE
*        sae_par.h
*     EMS
*        ems.h
*        ems_par.h
*        emsSetc
*        emsRep
*     HDS
*        hds.h
*        datNcomp
*        datType
*        datIndex
*        datShape
*        datName
*        datAnnul
*
* Copyright:
*   Copyright (C) 2008 Science and Technology Facilities Council.
*   Copyright (C) 1999 Central Laboratory of the Research Councils
*   All Rights Reserved.
*
* Authors:
*    TIMJ: Tim Jenness (JAC, Hawaii)
*    AJC: A.J.Chipperfield (Starlink, RAL)
*    {enter_new_authors_here}
*
* History:
*     1-JUL-1999 (AJC):
*       Original version
*     1-SEP-1999 (AJC):
*       Change TYPE component of structure to HDSTYPE
*    25-SEP-1999 (AJC):
*       Add LOGICAL_ to tagname for _LOGICAL components
*    28-FEB-2000 (AJC):
*       Handle _BYTE->integer, _UWORD->long
*    29-FEB-2000 (AJC):
*       Handle array of structures at top level
*    22-NOV-2005 (TIMJ):
*       Use modern HDS
*    15-SEP-2008 (TIMJ):
*       3 arg emsSetc is deprecated.
*    {enter_further_changes_here}
*
*-
*/
#include <stdio.h>
#include "export.h"
#include "sae_par.h"
#include "ems.h"
#include "ems_par.h"
#include "hds2idl.h"

#define TAG__SZNAM DAT__SZNAM+9       /* Max length of tag name */


IDL_StructDefPtr idlstructdef( HDSLoc *sloc, int *status ) {

static char hdsstructype[13] =  "HDSSTRUCTYPE";
static IDL_STRUCT_TAG_DEF niltagdef={0};

IDL_VPTR tagvar;
IDL_VPTR tagnamevar;
IDL_VPTR tagdimsvar;

IDL_STRUCT_TAG_DEF *tags;

int i, j;                 /* Loop counters */
int ncomp;                /* Number of components */
int prim;                 /* If component is primitive */
int ndims;                /* Number of dimensions */
hdsdim dims[DAT__MXDIM];     /* dimensions */
char type[DAT__SZTYP+1];    /* type as C string */
char name[DAT__SZNAM+1];    /* name as C string */
char *tagnames;           /* pointer to tagnames */
char *tagname;            /* pointer within tagnames */
IDL_LONG *tagdims;        /* pointer to tagdims */
IDL_LONG *tagdim;         /* pointer within tagdims */

void *s = NULL;          /* pointer to the created structure */
HDSLoc * cloc = NULL;    /* Component locator */

/* Fortran variables */
int maxdims=DAT__MXDIM;

 if ( !(*status == SAI__OK) ) return (NULL);

datShape( sloc, maxdims, dims, &ndims, status );
if ( !ndims ) {
   datNcomp ( sloc, &ncomp, status );
   tags = (IDL_STRUCT_TAG_DEF *)IDL_GetScratch( &tagvar, (IDL_LONG)ncomp+2,
            (IDL_LONG)sizeof(IDL_STRUCT_TAG_DEF) );

/* Set tag for the HDS type */
   datType( sloc, type, status );
   tags[0].name = hdsstructype;
   tags[0].dims = 0;
   tags[0].type = (void *)IDL_TYP_STRING;
   tags[0].flags = '\0';

/* Get memory for tagnames. We get space for ncomp+1 names so it works */
/* even if ncomp == 0, thus simplifying code.                           */
   tagnames = (char *)IDL_GetScratch( &tagnamevar, (IDL_LONG)ncomp+1,
              (IDL_LONG)TAG__SZNAM );
   tagname = tagnames;

/* Get memory for tag dimensions - only some or none may be needed */
/* Again we get ncomp+1 elements for simplicity.                   */
   tagdims = (IDL_LONG *)IDL_GetScratch( &tagdimsvar,
              (IDL_LONG)(ncomp+1), (IDL_LONG)(DAT__MXDIM+1)*sizeof(IDL_LONG) );
   tagdim = tagdims;

/* Now go through the structure setting up the tags array */
   for ( i=1; (*status==SAI__OK) && (i<=ncomp); i++ ) {
      datIndex( sloc, i, &cloc, status );
      datName( cloc, name, status );
      datPrim( cloc, &prim, status );
      if ( !prim ) {
/* It's another structure                                                    */
/* For IDL arrays of structures each element must have the same structure    */
/* so HDS arrays of structures are represented by an IDL structure with      */
/* HDSSTRUCTYPE hdstype(dim1,dim2...) and a separate structure component     */
/* named NAME_index1_index2... representing each element of the array of     */
/* structures.                                                               */
         strcpy( tagname, name );
         tags[i].name = tagname;
         tags[i].dims = 0;
         tags[i].type = (void*)idlstructdef(cloc, status);

      } else {
         datShape( cloc, maxdims, dims, &ndims, status );
         datType( cloc, type, status );

         if ( *status == SAI__OK ) {

/* Set the tag name */
            strcpy( tagname, strcmp( type, "_LOGICAL" )?"":"LOGICAL_" );
            strcat ( tagname, name );
            tags[i].name = tagname;

/* Set up the tag dims array */
            if( ndims ) {
/* array */
               tags[i].dims = tagdim;
               tags[i].dims[0] = (IDL_LONG)ndims;;
               for (j=0;j<ndims;j++)
                  tags[i].dims[j+1] = (IDL_LONG)dims[j];
               tagdim += DAT__MXDIM + 1;
            } else
/* scalar */
               tags[i].dims = 0;

/* Set up the tag type */
            if (!strcmp(type, "_REAL")) {
               tags[i].type = (void *)IDL_TYP_FLOAT;
            } else if (!strcmp(type, "_INTEGER")) {
               tags[i].type = (void *)IDL_TYP_LONG;
            } else if (!strcmp(type, "_WORD")) {
               tags[i].type = (void *)IDL_TYP_INT;
            } else if (!strcmp(type, "_DOUBLE")) {
               tags[i].type = (void *)IDL_TYP_DOUBLE;
            } else if (!strcmp(type, "_UBYTE")) {
               tags[i].type = (void *)IDL_TYP_BYTE;
            } else if (!strcmp(type, "_LOGICAL")) {
               tags[i].type = (void *)IDL_TYP_LONG;
            } else if (!strncmp(type, "_CHAR", 5)) {
               tags[i].type = (void *)IDL_TYP_STRING;
            } else if (!strcmp(type, "_BYTE")) {
               tags[i].type = (void *)IDL_TYP_INT;
            } else if (!strcmp(type, "_UWORD")) {
               tags[i].type = (void *)IDL_TYP_LONG;
            } else if ( type[0] == '_' ) {
               *status = SAI__ERROR;
               emsSetnc( "TYPE", type, EMS__SZTOK );
               emsRep( " ", "Illegal type ^TYPE", status );
            }

         } /* endif cloc OK */

      } /* end if structure */
      datAnnul( &cloc, status );

/* Set the flags */
      tags[i].flags = (UCHAR)0;

      tagname += TAG__SZNAM;
   } /* end for each component */

   if ( *status == SAI__OK ) {
/* Terminate the tags */
      tags[i] = niltagdef;

/* Create a structure */
      s = IDL_MakeStruct( NULL, tags );
   }

/* Clean up */
   IDL_Deltmp(tagvar);
   IDL_Deltmp(tagnamevar);
   IDL_Deltmp(tagdimsvar);

} else {
/* It's an array of structures */
   datName( sloc, name, status );
   s = idlstructarrdef( sloc, name, ndims, dims, status);

}
/* and return the structure */
return( s );

}
