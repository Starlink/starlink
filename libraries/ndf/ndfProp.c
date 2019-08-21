#include "star/subpar.h"
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "par_err.h"
#include "ndf.h"
#include "mers.h"

void ndfProp_( int indf1, const char *clist, const char *param, int *indf2,
              int *status ){
/*
*+
*  Name:
*     ndfProp

*  Purpose:
*     Propagate NDF information to create a new NDF via the ADAM parameter
*     system.

*  Synopsis:
*     void ndfProp( int indf1, const char *clist, const char *param,
*                   int *indf2, int *status )

*  Description:
*     This function creates a new NDF data structure through the ADAM
*     parameter system, associates it with a parameter and returns an
*     identifier for it. The shape, data type, etc. of this new NDF are
*     based on a existing "template" NDF, and the values of components of
*     this template may be selectively propagated to initialise the new
*     data structure.

*  Parameters:
*     indf1
*        Identifier for an existing NDF (or NDF section) to act as a
*        template.
*     clist
*        Pointer to a null terminated string holding A comma-separated list
*        of the NDF components which are to be propagated to the new data
*        structure. By default, the HISTORY, LABEL and TITLE components are
*        propagated. All extensions are also propagated by default except
*        for any that have had a zero value assigned to the corresponding
*        "PXT..." tuning parameter using ndfTune. See the "Component
*        Propagation" section for further details.
*     param
*        Pointer to a null terminated string holding the name of the ADAM
*        parameter for the new NDF.
*     *indf2
*        Returned holding the identifier for the new NDF.
*     *status
*        The global status.

*  Notes:
*     -  If this function is called with "status" set, then a value of
*     NDF__NOID will be returned for the "indf2" parameter, although no
*     further processing will occur. The same value will also be returned
*     if the function should fail for any reason. The NDF__NOID constant is
*     defined in the header file "ndf.h".

*  Component Propagation:
*     -  The template components whose values are to be propagated to
*     initialise the new data structure are specified via the "clist"
*     parameter. Thus "clist"="DATA,QUALITY" would cause the new NDF to
*     inherit its DATA and QUALITY values (if available) from the template
*     structure, in addition to those propagated by default. Component
*     propagation may be suppressed by supplying a component name with the
*     prefix "NO". Thus "clist"="DATA,NOHISTORY" would propagate the DATA
*     component, but suppress propagation of HISTORY. If component names
*     appear more than once in the "clist" value, then the last occurrence
*     takes precedence.
*     -  Propagation of specific NDF extensions may be suppressed by using
*     "NOEXTENSION()" as one of the items in the "clist" parameter; a list
*     of the extensions to be suppressed should appear between the
*     parentheses. Thus "clist"="AXIS,NOEXTENSION(IRAS,ASTERIX)" would
*     propagate the AXIS component, but suppress propagation of the IRAS
*     and ASTERIX extensions (if present). Propagation of suppressed
*     extensions may be re-enabled by specifying "EXTENSION()" in a similar
*     manner at a later point in the "clist" value.
*     -  An asterisk (*) may be used as a wild card to match all extension
*     names. Thus "NOEXTENSION(*),EXTENSION(IRAS)" may be used to indicate
*     that only the IRAS extension should be propagated.
*     -  Whether or not a named extension is propagated by default can be
*     controlled via an NDF tuning parameter (see ndfTune). The defaults
*     established using ndfTune can be over-ridden by specifying the
*     extension explicitly within the "clist" parameter; e.g.
*     "EXTENSION(FITS)" or "NOEXTENSION(FITS)" can be used to over-ride the
*     default established by the PXTFITS tuning parameter.
*     -  Component names in the "clist" parameter may be abbreviated to 3
*     characters, but extension names must appear in full.

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
   NdfACB *acb1;         /* Pointer to input NDF entry in the ACB */
   NdfACB *acb2;         /* Pointer to output NDF entry in the ACB */
   NdfPCB *pcb;          /* Pointer to placeholder entry in the PCB */
   char extn[ NDF__MXEXT ][ DAT__SZNAM + 1 ];/* Excluded ext. list */
   char name[ NDF__SZPAR + 1 ];    /* NDF name string */
   int cpf[ NDF__NCPF ];/* Component propagation flags */
   int nextn;            /* Number of excluded extensions */
   int tstat;            /* Temporary status variable */
   size_t ipar;          /* Parameter table index */

/* Set an initial value for the "indf2" parameter. */
   *indf2 = NDF__NOID;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Mark the error stack, so that annulling error messages doesn't
   disturb any pre-existing error stack contents. */
   errMark();

/* Import the input NDF identifier. */
   ndf1Impid( indf1, &acb1, status );

/* Get a list of all available extensions in the input NDF. */
   ndf1Xlst( acb1, NDF__MXEXT, extn, &nextn, status );

/* Parse the component propagation expression. */
   ndf1Pscpx( clist, NDF__MXEXT, extn, &nextn, cpf, status );

/* Find the parameter index in the parameter tables. */
   subParFindpar( param, &ipar, status );
   if( *status == SAI__OK ) {

/* Obtain the new NDF name via the parameter. */
      subParGetname( ipar, name, sizeof(name), status );
      acb2 = NULL;

/* Loop until a valid output NDF structure has been created or a
   non-recoverable error occurs. */
      while( *status == SAI__OK ){

/* Create a placeholder entry for the object in the PCB and selectively
   propagate the components of the input NDF to create a new base NDF. */
         ndf1Plfor( NULL, name, &pcb, status );
         ndf1Prp( acb1, nextn, extn, cpf, pcb, &acb2, status );

/* Annul the PCB entry when done, erasing the object if there has been
   an error. */
         ndf1Annpl( *status != SAI__OK, &pcb, status );

/* If this failed, then the user must be re-prompted. Report contextual
   information and flush any error messages. */
         if( *status != SAI__OK ) {
            msgSetc( "PARAM", param );
            errRep( " ", "ndfProp: Unable to propagate NDF information to "
                    "create a new NDF via the '%^PARAM' parameter.", status );
            errFlush( status );

/* Cancel the parameter association, annulling any further error
   messages this may generate. */
            subParCancl( ipar, status );
            errAnnul( status );

/* Obtain the new NDF name via the parameter. */
            subParGetname( ipar, name, sizeof(name), status );
            acb2 = NULL;

         } else {
            break;
         }
      }
   }

/* Export an NDF identifier */
   *indf2 = ndf1Expid( ( NdfObject * ) acb2, status );

/* If an error occurred, then annul any ACB entry which might have been
   acquired. */
   if( *status != SAI__OK ) ndf1Anl( &acb2, status );

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
      errRep( " ", "ndfProp: Error propagating NDF information to create a "
              "new NDF via the '%^PARAM' parameter.", status );
      ndf1Trace( "ndfProp", status );
   }

/* Release the error stack. */
   errRlse();

/* Restablish the original AST status pointer */
   NDF_FINAL

}

