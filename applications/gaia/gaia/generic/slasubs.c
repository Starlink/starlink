/*
 *  Name:
 *     slasubs.c

 *  Purpose:
 *     Implement a C interface to some of the Fortran SLALIB library.

 *  Description:
 *     This file implements a C interface to the Fortran version of the
 *     SLALIB library. It only supplies the functions that are used by
 *     GAIA, so is far from complete. In fact it has one call SLA_OBS.
 *     See the SLALIB documentation about the call sequence.

 *  Copyright:
 *     Copyright (C) 2003 Central Laboratory of the Research Councils

 *  Authors:
 *     PWD: Peter W. Draper (Starlink, Durham University)

 *  History:
 *     12-SEP-2003 (PWD):
 *        Original version.
 */

#include "f77.h"
#include "slasubs.h"

F77_SUBROUTINE(sla_obs)( INTEGER( N ),
                         CHARACTER( C ),
                         CHARACTER( NAME ),
                         DOUBLE( W ),
                         DOUBLE( P ),
                         DOUBLE( H )
                         TRAIL( C )
                         TRAIL( NAME ) );
void slaObs ( int n, char *c, char *name, double *w, double *p, double *h )
{
   DECLARE_INTEGER( fn );
   DECLARE_CHARACTER( fname, 42 );
   DECLARE_CHARACTER( fc, 12 );
   DECLARE_DOUBLE( fw );
   DECLARE_DOUBLE( fp );
   DECLARE_DOUBLE( fh );

   F77_EXPORT_INTEGER( n, fn );
   if ( c[0] != '\0' ) {
       F77_EXPORT_CHARACTER( c, fc, fc_length );
   } 
   else {
       F77_EXPORT_CHARACTER( " ", fc, fc_length );
   }


   F77_CALL( sla_obs )( INTEGER_ARG( &fn ),
                        CHARACTER_ARG( fc ),
                        CHARACTER_ARG( fname ),
                        DOUBLE_ARG( &fw ),
                        DOUBLE_ARG( &fp ),
                        DOUBLE_ARG( &fh )
                        TRAIL_ARG( fc )
                        TRAIL_ARG( fname ) );

   F77_IMPORT_CHARACTER( fc, fc_length, c );
   F77_IMPORT_CHARACTER( fname, fname_length, name );
   F77_IMPORT_DOUBLE( fw, *w );
   F77_IMPORT_DOUBLE( fp, *p );
   F77_IMPORT_DOUBLE( fh, *h );
}
