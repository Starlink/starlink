/*+
 *  Name:
 *     emsRep
 *
 *  Fortran callable routine
*/
#include <stdlib.h>
#include "ems.h"                       /* ems_ function prototypes */
#include "ems_par.h"                   /* EMS constatnts           */
#include "f77.h"                       /* CNF macros and prototypes */

F77_SUBROUTINE(ems_rep) ( CHARACTER(err), CHARACTER(text), INTEGER(status)
         TRAIL(err) TRAIL(text) ) {

   char cerr[EMS__SZPAR+1];  /* Imported message name */
   char *ctext;              /* Imported text string */

   GENPTR_CHARACTER(err)
   GENPTR_CHARACTER(text)
   GENPTR_INTEGER(status)

/* Import the given strings
*/    
   cnfImpn( err, err_length, EMS__SZPAR, cerr );
   ctext = cnfCreim( text, text_length ); 

   emsRep( cerr, ctext, status);

   free( ctext );
   return;
}
