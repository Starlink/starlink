/* A simple test of the C EMS installation */

/* ensure it doesn't look in this directory for the header files */
#include <stdio.h>
#include <sae_par.h>
#include <ems.h>
#include <ems_par.h>
#include <f77.h>

int main(){
   int status;
   DECLARE_INTEGER( fstatus );
   DECLARE_CHARACTER( err, 5 );
   DECLARE_CHARACTER( mess, EMS__SZMSG );

/* The basic C interface */
   printf( "\nTest the basic C interface\n" );
   status = SAI__ERROR;
   emsRep( "ERR1", "Error message 1", &status );
   emsMark();
   emsRep( "NOERR", "This message should not appear", &status );
   emsAnnul( &status );
   status = SAI__ERROR;
   emsRep( "ERR2", "Error message 2", &status );
   printf( "This should appear between the two error messages\n" );
   emsRlse();

/* Now the Fortran interface (from C) */
/* Pretend we are calling Fortran routines */
   printf( "\nTest the Fortran interface\n" );
   fstatus = SAI__ERROR;
   cnfExprt( "ERR1", err, err_length );
   cnfExprt( "Error message 1", mess, mess_length );
   F77_CALL(ems_rep)( CHARACTER_ARG( err ),
                      CHARACTER_ARG( mess ),
                      INTEGER_ARG( &fstatus )
                      TRAIL_ARG( err )
                      TRAIL_ARG( mess ) );
   F77_CALL(ems_mark)();
   cnfExprt( "ERR1", err, err_length );
   cnfExprt( "This message should not appear", mess, mess_length );
   F77_CALL(ems_rep)(  CHARACTER_ARG( err ),
                      CHARACTER_ARG( mess ),
                      INTEGER_ARG( &fstatus )
                      TRAIL_ARG( err )
                      TRAIL_ARG( mess ) );
   cnfExprt( "ERR2", err, err_length );
   cnfExprt( "Error message 2", mess, mess_length );
   F77_CALL(ems_annul)( INTEGER_ARG( &fstatus ) );
   fstatus = SAI__ERROR;
   F77_CALL(ems_rep)(  CHARACTER_ARG( err ),
                      CHARACTER_ARG( mess ),
                      INTEGER_ARG( &fstatus )
                      TRAIL_ARG( err )
                      TRAIL_ARG( mess ) );

   printf( "This should appear between the two error messages\n" );
   F77_CALL(ems_rlse)();
   printf( "\n" );

   exit( 0 );
}   
