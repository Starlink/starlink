/*+
* Name:
*    crehds

*  Purpose:
*     To write an IDL structure to an HDS object with the same structure.

*  Language:
*     C

*  Invocation:
*     From IDL via CALL_EXTERNAL

*  Arguments:
*     argc = int (Given);
*        The number of elements in argv.
*     argv[] = void * (Given)
*        Pointers to the arguments given in the CALL_EXTERNAL call:
*           struct = IDL_VPTR (Given)
*              Variable to be written to hds
*           taglist = IDL_VPTR (Given)
*              Pointer to a list of tagnames of the variable to be written
*           hds_name = IDL_VPTR (Given)
*              The name of the HDS object to be read

*  Description:

*  Pitfalls:
*     [pitfall_description]...

*  Notes:
*     -  {noted_item}
*     [routine_notes]...

*  External Routines Used:
*     SAE
*        sae_par.h
*     EMS
*        ems.h
*        ems_par.h
*        emsMark
*        emsRlse
*        emsRep
*        emsEload
*     HDS
*        hds.h
*        hdsNew
*        datAnnul

*  Implementation Deficiencies:
*     -  {deficiency}
*     [routine_deficiencies]...

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils

*  Authors:
*     AJC: A.J.Chipperfield (Starlink, RAL)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     20-JUL-1999 (AJC):
*        Original version.
*     22-NOV-2005 (TIMJ):
*        Migrate to new C HDS interface
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
#include "star/hds.h"
#include "ems.h"
#include "ems_par.h"
#include "hds2idl.h"


void crehds( int argc, IDL_VPTR argv[] ) {
/*
** Declare variables
*/
char *hdsname;    /* The name of the HDS container file to create */
char *strucname;  /* The name of the top-level structure */
IDL_VPTR var;

int ndims;
hdsdim dims[DAT__MXDIM];

int status;        /* Starlink status */

HDSLoc* toploc = NULL;      /* Locator to top object */
int numtags;                /* Number of tags = 0 if not a structure */
char hdstype[DAT__SZTYP+1]; /* corresponding HDS type */
int fstat;                  /* Final status (before emsEload) */
char param[EMS__SZPAR+1];     /* Error message parameter name */
int parlen;                 /* Length of error message parameter name */
char opstr[EMS__SZMSG+1];     /* Error message */
int oplen;                  /* Length of error message */
char **taglist;             /* Pointer to taglist */
char *data;                 /* Pointer to data */
IDL_LONG nvals;             /* Number of values in var */
int elt_len;                /* Length of element of structure */
IDL_STRING *IDL_tags;
hdsdim IDL_ntags;

char defname[12]="IDL2HDS_OUT";

/* Start Error context */
   status = SAI__OK;
   emsMark();

/* Check that the correct number of arguments were passed in */
   if ( ( argc == 2 ) | (argc == 3 ) ) {
      var = argv[0];

      if ( argv[1]->flags & IDL_V_ARR ) {
         IDL_tags = (IDL_STRING *)argv[1]->value.arr->data;
         IDL_ntags = (hdsdim)argv[1]->value.arr->n_elts;
      } else {
         IDL_tags = &argv[1]->value.str;
         IDL_ntags = 1;
      }
      taglist = getstringarray( 1, &IDL_ntags, IDL_tags );

      if ( argc == 3 )
         hdsname = argv[2]->value.str.s;
      else
         hdsname = defname;
      strucname = hdsname;

      IDL_VarGetData( var, &nvals, &data, 0 );

      getobjectdetails( var, data, taglist,
         hdstype, &numtags, &ndims, dims, &elt_len, &status );
      hdsNew( hdsname, strucname, hdstype, ndims, dims, &toploc, &status );
      if ( numtags ) {
/* is a structure - invoke the structure handler */
         hdsstructwrite(
            toploc, data, taglist, numtags, ndims, dims, var, &status );
         retstringarray( taglist );
      } else {
         hdsprimwrite( toploc, hdstype, ndims, dims, data, &status );
      }

      datAnnul( &toploc, &status );

   } else {
      status = SAI__ERROR;
      emsRep( " ", "crehds: Incorrect number of arguments", &status );
   }

/*  Report any error messages */
/*  Adding Starlink-style !! and ! prefix */
   if ( status != SAI__OK ) {
      fstat = status;
      while ( status != SAI__OK ) {
         emsEload(
            param, &parlen, opstr, &oplen, &status );
         if ( status != SAI__OK )
            IDL_Message( IDL_M_NAMED_GENERIC, IDL_MSG_INFO, opstr );
      }
   }
   emsRlse();

/*  That's it, return to the calling routine */
   return;
}
