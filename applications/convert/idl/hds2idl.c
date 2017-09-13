/*+
* Name:
*    hds2idl

*  Purpose:
*     Convert HDS to IDL data.

*  Language:
*     C

*  Invocation:
*     From IDL as a system routine:
*     IDL> struct = hds2idl("object_name")

*  Arguments:
*     argc = int (Given);
*        The number of elements in argv.
*     argv[] = void * (Given)
*        Pointers to the arguments given in the CALL_EXTERNAL call:
*           hds_name = VPTR * (Given)
*              The name of the HDS object to be read

*  Returned value:
*     struct = IDL_VPTR
*        Pointer to an IDL variable containing the data converted from
*        the HDS object.

*  Description:
*     This IDL system routine will construct an IDL variable corresponding
*     to the specified HDS object, which may be either a structure or
*     primitive object. Array elements and slices may be specified in the
*     object name as for example: dataset.axis(1).data_array. A
*     dataset.data_array(50:200,100).  If a lower bound is omitted, 1 is
*     assumed. If an upper bound is omitted, the upper limit is assumed.
*

*  Pitfalls:
*     The numbering of elements of HDS arrays starts at 1 whereas IDL arrays
*     start at 0. Therefore if an array element or slice is specified in
*     the HDS object name, some confusion may occur.
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
*        datStruc
*        datType
*        datShape
*        datAnnul

*  Implementation Deficiencies:
*     -  {deficiency}
*     [routine_deficiencies]...

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils

*  Authors:
*     AJC: A.J.Chipperfield (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*      5-JAN-1999 (AJC):
*        Original version.
*     29-FEB-2000 (AJC):
*        Open object, not necessarily a file.
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
#include "ems.h"
#include "ems_par.h"
#include "star/hds.h"
#include "hds2idl.h"


IDL_VPTR hds2idl( int argc, IDL_VPTR argv[] ) {
/*
** Declare variables
*/
IDL_VPTR hds_name;       /* IDL_VPTR to name of the HDS object to be read */
IDL_VPTR var;            /* Variable pointer to IDL object */
IDL_VARIABLE temp;       /* Temporary storage for primitive scalar variable */
IDL_StructDefPtr sdef;   /* Structure definition of sub-structure */
IDL_STRING *objname;     /* Pointer to object name as IDL string */
HDSLoc *objloc = NULL;   /* Locator of file */

int status;             /* Starlink status */

char type[DAT__SZTYP+1];/* HDS type of component */
UCHAR idltype;          /* IDl type of component */
int ndims;              /* Number of dimensions of object */
hdsdim dims[DAT__MXDIM];/* Dimensions of object HDS style */
IDL_LONG idldims[DAT__MXDIM];/* Dimensions of object IDL style */
int i;                  /* loop index */
int fstat;              /* Final status (before emsEload) */
int isstruct;           /* Whether object is structure */
void *tdata;            /* Pointer to data area of IDL variable or array */

char param[EMS__SZPAR+1]; /* Error message parameter name */
int parlen;             /* Length of error message parameter name */
char opstr[EMS__SZMSG+1]; /* Error message */
int oplen;              /* Length of error message */

IDL_LONG one[IDL_MAX_ARRAY_DIM]={1};

/* Start Error context */
   status = SAI__OK;
   emsMark();

/* Check that the correct number of arguments were passed in */
   if(argc != 1) {

   /* Print an error message and return */
      status = SAI__ERROR;
      emsRep( " ", "hds2idl: Incorrect number of arguments", &status );

   } else {
   /* Extract the arguments to comprehensible names */
      hds_name = argv[0];
      objname = &hds_name->value.str;

   /* Open the HDS object */
      getcomp( IDL_STRING_STR(objname), "READ", &objloc, &status );

   /* Check for structure or primitive */
      datStruc( objloc, &isstruct, &status );
      if (status == SAI__OK) {
         if ( isstruct ) {
         /* Create a structure */
            sdef = idlstructdef( objloc, &status );

         /* Create a temporary variable */
            if ( status == SAI__OK ) {
               (void *)IDL_MakeTempStruct( sdef, 1, one, &var, TRUE );
               idlstructfill( objloc, var->value.s, &status );
            }

         } else {
         /* Object is primitive */
            datType( objloc, type, &status );
            idltype = getidltype( type );
            datShape( objloc, DAT__MXDIM, dims, &ndims, &status );
            if ( status == SAI__OK ) {
               if (ndims) {
               /* Get dimensions IDL style */
                  for (i=0;i<ndims;i++) idldims[i] = (IDL_LONG)dims[i];
               /* Object is primitive array */
                  tdata = IDL_MakeTempArray( (int)idltype, ndims, idldims,
                    IDL_BARR_INI_ZERO , &var );

               } else {
               /* Object is primitive scalar */
                  var = &temp;
                  var->type = idltype;
                  var->flags = 0;
                  tdata = &var->value;
               }

               idlprimfill( objloc, var, tdata, &status );
            }

         }

      /* Annul the object (and close the file) */
         datAnnul( &objloc, &status );
      }
   }

   if ( status != SAI__OK ) {
   /*  Report any error messages             */
   /*  Adding Starlink-style !! and ! prefix */
      fstat = status;
      while ( status != SAI__OK ) {
         emsEload(
            param, &parlen, opstr, &oplen, &status );
         if ( status != SAI__OK )
            IDL_Message( IDL_M_NAMED_GENERIC, IDL_MSG_INFO, opstr );
      }

   /*  Set to return undefined variable */
      var = IDL_Gettmp();

   /*  and close error context */
      emsRlse();
   }

/*  That's it, return to the calling routine */
   return var;
}

