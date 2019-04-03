#include "star/subpar.h"
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "par_err.h"
#include "ndf.h"
#include "mers.h"

void ndfCreat_( const char *param, const char *ftype, int ndim,
               const hdsdim lbnd[], const hdsdim ubnd[], int *indf,
               int *status ){
/*
*+
*  Name:
*     ndfCreat

*  Purpose:
*     Create a new simple NDF via the ADAM parameter system.

*  Synopsis:
*     void ndfCreat( const char *param, const char *ftype, int ndim,
*                    const hdsdim lbnd[], const hdsdim ubnd[], int *indf,
*                    int *status )

*  Description:
*     This function creates a new simple NDF via the ADAM parameter system,
*     associates it with a parameter, and returns an NDF identifier for it.

*  Parameters:
*     param
*        Pointer to a null terminated string holding the name of the ADAM
*        parameter.
*     ftype
*        Pointer to a null terminated string holding the full data type of
*        the NDF's DATA component (e.g. "_DOUBLE" or "COMPLEX_REAL").
*     ndim
*        Number of NDF dimensions.
*     lbnd
*        Lower pixel-index bounds of the NDF.
*     ubnd
*        Upper pixel-index bounds of the NDF.
*     *indf
*        Returned holding the NDF identifier.
*     *status
*        The global status.

*  Notes:
*     -  This function creates a "simple" NDF, i.e. one whose array
*     components will be stored in "simple" form by default (see SGP/38).
*     -  The full data type of the DATA component is specified via the
*     "ftype" parameter and the data type of the VARIANCE component
*     defaults to the same value. These data types may be set individually
*     with the ndfStype function if required.
*     -  If this function is called with "status" set, then a value of
*     NDF__NOID will be returned for the "indf" parameter, although no
*     further processing will occur. The same value will also be returned
*     if the function should fail for any reason. The NDF__NOID constant is
*     defined in the header file "ndf.h".

*  Copyright:
*     Copyright (C) 2018 East Asian Observatory
*     All rights reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or modify
*     it under the terms of the GNU General Public License as published by
*     the Free Software Foundation; either version 2 of the License, or (at
*     your option) any later version.
*
*     This program is distributed in the hope that it will be useful,but
*     WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
*     General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     DSB: David S. Berry (EAO)

*  History:
*     xxx (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Local Variables: */
   NdfACB *acb;          /* Pointer to NDF entry in the ACB */
   NdfPCB *pcb;          /* Pointer to placeholder entry in the PCB */
   char name[ NDF__SZPAR + 1 ];    /* NDF name string */
   char type[ NDF__SZTYP + 1 ];    /* Numeric data type */
   int cmplx;            /* Whether data type is complex */
   int tstat;            /* Temporary status variable */
   size_t ipar;          /* Parameter table index */

/* Set an initial value for the "indf" parameter. */
   *indf = NDF__NOID;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Mark the error stack, so that annulling error messages doesn't
   disturb any pre-existing error stack contents. */
   errMark();

/* Find the parameter index in the parameter tables. */
   subParFindpar( param, &ipar, status );

/* Check the data type and bounds information for validity. */
   ndf1Chftp( ftype, type, sizeof( type ), &cmplx, status );
   ndf1Vbnd( ndim, lbnd, ubnd, status );
   if( *status == SAI__OK ) {

/* Obtain the NDF name via the parameter. */
      subParGetname( ipar, name, sizeof(name), status );
      acb = NULL;

/* Loop until a valid NDF structure has been created or a
   non-recoverable error occurs. */
      while( *status == SAI__OK ){

/* Create a placeholder entry for the object in the PCB and use this to
   create the new NDF. */
         ndf1Plfor( NULL, name, &pcb, status );
         ndf1Dcre( ftype, ndim, lbnd, ubnd, pcb, &acb, status );

/* Annul the PCB entry when done, erasing the object if there has been
   an error. */
         ndf1Annpl( *status != SAI__OK, &pcb, status );

/* If this failed, then the user must be re-prompted. Report contextual
   information and flush any error messages. */
         if( *status != SAI__OK ) {
            msgSetc( "PARAM", param );
            errRep( " ", "ndfCreat: Unable to create a new simple NDF via "
                    "the '%^PARAM' parameter.", status );
            errFlush( status );

/* Cancel the parameter association, annulling any further error
   messages this may generate. */
            subParCancl( ipar, status );
            errAnnul( status );

/* Obtain the NDF name via the parameter. */
            subParGetname( ipar, name, sizeof(name), status );
            acb = NULL;

         } else {
            break;
         }
      }
   }

/* Export an NDF identifier */
   *indf = ndf1Expid( ( NdfObject * ) acb, status );

/* If an error occurred, then annul any ACB entry which might have been
   acquired. */
   if( *status != SAI__OK ) ndf1Anl( &acb, status );

/* If an error occurred, then classify it... */

/* If an "abort" was requested, then annul any error messages and issue
   an appropriate new one. */
   if( *status == PAR__ABORT ) {
      tstat = *status;
      errAnnul( &tstat );
      msgSetc( "PARAM", param );
      errRep( " ", "Aborted creation of a new NDF structure via the "
              "'%^PARAM' parameter.", status );

/* If an "null" NDF was specified, then annul any error messages and
   issue an appropriate new one. */
   } else if( *status == PAR__NULL ) {
      tstat = *status;
      errAnnul( &tstat );
      msgSetc( "PARAM", param );
      errRep( " ", "Null NDF structure specified for the '%^PARAM' "
              "parameter.", status );

/* For other errors, add context information and call the error tracing
   function. */
   } else if( *status != SAI__OK ) {
      msgSetc( "PARAM", param );
      errRep( " ", "ndfCreat: Error creating a new simple NDF via the "
              "'%^PARAM' parameter.", status );
      ndf1Trace( "ndfCreat", status );
   }

/* Release the error stack. */
   errRlse();

/* Restablish the original AST status pointer */
   NDF_FINAL

}

