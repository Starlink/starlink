/*+
* Name:
*    probe_ndf

*  Purpose:
*     To obtain the shape and size of an NDF and the type of a specified
*     component of it.

*  Language:
*     C

*  Invocation:
*     From IDL via CALL_EXTERNAL

*  Description:
*     Obtains the size and type of a specified NDF component

*  Arguments:
*     argc = int (Given);
*        The number of elements in argv.
*     argv[] = void * (Given)
*        Pointers to the arguments given in the CALL_EXTERNAL call:
*           ndf_name = STRING * (Given)
*              The name of the NDF to be probed.
*           comp = STRING * (Given)
*              The name of the NDF component to be probed.
*           type = STRING * (Returned)
*              The full type of the component.
*           ndims = int * (Returned)
*              The number of dimensions.
*           dims = int * (Returned)
*              The dimensions vector.

*  Examples:
*     {routine_example_text}
*        {routine_example_description}
*     [routine_example]...

*  Pitfalls:
*     -  {pitfall}
*     [pitfall_description]...

*  Notes:
*     -  {noted_item}
*     [routine_notes]...

*  External Routines Used:
*     SAE
*        sae_par.h
*     MERS
*        mers.h
*           merswrap.h
*           err_par.h
*           err_err.h
*           msg_par.h
*           msg_err.h
*        errMark
*        errRlse
*        errRep
*        errLoad
*     HDS
*        dat_par.h
*     NDF
*        ndf.h
*        ndfBegin
*        ndfEnd
*        ndfOpen
*        ndfDim
*        ndfFtype
*
*     {name_of_facility_or_package}:
*        {routine_used}...
*     [facility_or_package]...

*  Implementation Deficiencies:
*     -  {deficiency}
*     [routine_deficiencies]...

*  Copyright:
*     Copyright (C) {year} Central Laboratory of the Research Councils

*  Authors:
*     AJC: A.J.Chipperfield (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*      5-JAN-1999 (AJC):
*        Original version.
*      7-JAN-1999 (AJC):
*        Add component argument
*        Use NDF_FTYPE not NDF_TYPE (to trap COMPLEX)
*     14-FEB-2002 (AJC):
*        IDL has re-defined IDL_STRING - use updated header file export.h
*     {enter_further_changes_here}

*  Bugs:
*     -  {description_of_bug}
*     {note_new_bugs_here}
*-
*/

#include <stdio.h>
#include <string.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf.h"
#include "mers.h"
#include "export.h"

int probe_ndf( argc, argv)
int argc;
void *argv[];
{
/*
** Declare variables
*/
IDL_STRING *ndf_name;  /* The name of the NDF to be probed */
IDL_STRING *comp;      /* The component to be probed */
IDL_STRING *type;      /* The HDS type of the component */
int *ndims;        /* The number of dimensions */
int *dims;         /* The dimensions */

int status;        /* Starlink status */

int ndf;           /* NDF identifier */
int place;         /* NDF placeholder */

int fstat;               /* Final status (before errLoad) */
int errn=0;             /* Error sequence number */
char param[ERR__SZPAR]; /* Error message parameter name */
int parlen;             /* Length of error message parameter name */
char opstr[ERR__SZMSG]; /* Error message */
int oplen;              /* Length of error message */

/*
** Start Error context
*/
   status = SAI__OK;
   errMark();

/*
** Check that the correct number of arguments were passed in
*/
   if(argc != 5) {
   /*
   ** Print an error message and return
   */
      status = SAI__ERROR;
      errRep( " ", "probe_ndf: Incorrect number of arguments", &status );
   } else {
   /*
   ** Extract the arguments to comprehensible names
   */
      ndf_name = (IDL_STRING *)argv[0];
      comp = (IDL_STRING *)argv[1];
      type = (IDL_STRING *)argv[2];
      ndims = (int *)argv[3];
      dims = (int *)argv[4];
   /*
   ** Enable NDF calls
   */
      ndfBegin();
   /*
   ** Open the NDF
   */
      ndfOpen( NULL, ndf_name->s, "READ", "OLD", &ndf, &place, &status );
   /*
   **  Obtain the NDF dimension info
   */
      ndfDim( ndf, 7, dims, ndims, &status );
   /*
   ** and Type
   */
      ndfFtype( ndf, comp->s, type->s, DAT__SZTYP, &status );
      type->slen = strlen(type->s);
   /*
   **  Close NDF
   */
      ndfEnd( &status );
   }

/*
**  Report any error messages
**  Adding Starlink-style !! and ! prefix
*/
   fstat = status;
   while ( status != SAI__OK ) {
      errLoad(
         param, ERR__SZPAR, &parlen, opstr, ERR__SZMSG, &oplen, &status );
      if ( status != SAI__OK )
         printf( "%s %s\r\n", errn++?"! ":"!!", opstr );
   }
   errRlse();

/*
**  That's it, return to the calling routine
*/
   return( fstat == SAI__OK );

}
