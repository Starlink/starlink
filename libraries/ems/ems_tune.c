/*
*+
*  Name:
*     emsTune
 *
 *  Fortran callable routine
*/
#define MAXKEYSZ  6                    /* Maximum key length */

#include "ems.h"                       /* ems_ function prototypes */
#include "f77.h"                       /* CNF macros and prototypes */

F77_SUBROUTINE (ems_tune)( CHARACTER(key), INTEGER(value), INTEGER(status)
        TRAIL(key) )
{
   char ckey[MAXKEYSZ+1];   /* Imported keyword */

   GENPTR_CHARACTER(key)
   GENPTR_INTEGER(value)
   GENPTR_INTEGER(status)

   cnfImpn( key, key_length, MAXKEYSZ, ckey );

   emsTune( ckey, *value, status );
   
   return;
}
