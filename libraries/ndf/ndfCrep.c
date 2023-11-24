#include "star/subpar.h"
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "par_err.h"
#include "ndf.h"
#include "mers.h"

void ndfCrep_( const char *param, const char *ftype, int ndim,
              const hdsdim ubnd[], int *indf, int *status ){
/*
*+
*  Name:
*     ndfCrep

*  Purpose:
*     Create a new primitive NDF via the ADAM parameter system.

*  Synopsis:
*     void ndfCrep( const char *param, const char *ftype, int ndim,
*                   const hdsdim ubnd[], int *indf, int *status )

*  Description:
*     This function creates a new primitive NDF via the ADAM parameter
*     system, associates it with a parameter, and returns an NDF identifier
*     for it.

*  Parameters:
*     param
*        Pointer to a null terminated string holding the name of the ADAM
*        parameter.
*     ftype
*        Pointer to a null terminated string holding the type of the NDF's
*        DATA component (e.g. "_REAL"). Note that complex types are not
*        permitted when creating a primitive NDF.
*     ndim
*        Number of NDF dimensions.
*     ubnd
*        Upper pixel-index bounds of the NDF (the lower bound of each
*        dimension is taken to be 1).
*     *indf
*        Returned holding the NDF identifier.
*     *status
*        The global status.

*  Notes:
*     -  This function creates a "primitive" NDF, i.e. one whose array
*     components will be stored in "primitive" form by default (see
*     SGP/38).
*     -  The data type of the DATA component is specified via the "ftype"
*     parameter and the data type of the VARIANCE component defaults to the
*     same value. These data types may be set individually with the
*     ndfStype function if required.
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
*     3-APR-2019 (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Local Variables: */
   NdfACB *acb;          /* Pointer to NDF entry in the ACB */
   NdfPCB *pcb;          /* Pointer to placeholder entry in the PCB */
   char name[ NDF__SZPAR + 1 ];    /* NDF name string */
   char type[ NDF__SZTYP + 1 ];    /* Numeric data type */
   hdsdim lbnd[ NDF__MXDIM ];      /* Lower bounds of NDF */
   int cmplx;            /* Whether data type is complex */
   int i;                /* Loop counter for dimensions */
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

/* Check the data type information for validity. Report an error if a
   complex data type has been specified. */
   ndf1Chftp( ftype, type, sizeof( type ), &cmplx, status );
   if( *status == SAI__OK ) {
      if( cmplx ) {
         *status = NDF__FTPIN;
         msgSetc( "BADTYPE", ftype );
         errRep( " ", "The complex type '^BADTYPE' is not valid for a "
                 "primitive NDF (possible programming error).", status );
      }
   }

/* Check the NDF bounds for validity. */
   if( *status == SAI__OK ) {
      for( i = 0; i < NDF_MIN( ndim, NDF__MXDIM ); i++ ){
         lbnd[ i ] = 1;
      }
      ndf1Vbnd( ndim, lbnd, ubnd, status );
   }
   if( *status == SAI__OK ) {

/* Obtain the NDF name via the parameter. */
      subParGetname( ipar, name, sizeof(name), status );
      acb = NULL;

/* Loop until a valid NDF structure has been created or a
   non-recoverable error occurs. */
      while( *status == SAI__OK ){

/* Create a placeholder entry for the object in the PCB and use this to
   create the new NDF. */
         NDF__DCB_LOCK_MUTEX;
         ndf1Plfor( NULL, name, &pcb, status );
         ndf1Dcrep( ftype, ndim, ubnd, pcb, &acb, status );
         NDF__DCB_UNLOCK_MUTEX;

/* Annul the PCB entry when done, erasing the object if there has been
   an error. */
         NDF__PCB_LOCK_MUTEX;
         ndf1Annpl( *status != SAI__OK, &pcb, status );
         NDF__PCB_UNLOCK_MUTEX;

/* If this failed, then the user must be re-prompted. Report contextual
   information and flush any error messages. */
         if( *status != SAI__OK ) {
            msgSetc( "PARAM", param );
            errRep( " ", "ndfCrep: Unable to create a new primitive NDF "
                    "via the '%^PARAM' parameter.", status );
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
      errRep( " ", "ndfCrep: Error creating a new primitive NDF via the "
              "'%^PARAM' parameter.", status );
      ndf1Trace( "ndfCrep", status );
   }

/* Release the error stack. */
   errRlse();

/* Restablish the original AST status pointer */
   NDF_FINAL

}

