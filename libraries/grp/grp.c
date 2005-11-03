/*
*  Name:
*     grp.c

*  Purpose:
*     Implement the C interface to the GRP library.

*  Description:
*     This module implements C-callable wrappers for the public
*     routines in the GRP library. The interface to these wrappers
*     is defined in grp.h.

*  Notes:
*     - Given the size of the GRP library, providing a complete C
*     interface is probably not worth the effort. Instead, I suggest that 
*     people who want to use GRP from C extend this file (and
*     grp.h) to include any functions which they need but which are
*     not already included.

*  Authors:
*     DSB: David S Berry
*     {enter_new_authors_here}

*  History:
*     30-SEP-2005 (DSB):
*        Original version.
*     {enter_further_changes_here}
*/

/* Header files. */
/* ============= */
#include "f77.h"                 
#include "grp.h"

/* Wrapper function implementations. */
/* ================================= */


F77_SUBROUTINE(grp_grpsz)( INTEGER(IGRP), INTEGER(SIZE), INTEGER(STATUS) );

void grpGrpsz( int igrp, int *size, int *status ){
   DECLARE_INTEGER(IGRP);
   DECLARE_INTEGER(SIZE);
   DECLARE_INTEGER(STATUS);

   F77_EXPORT_INTEGER( igrp, IGRP );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_CALL(grp_grpsz)( INTEGER_ARG(&IGRP),
                        INTEGER_ARG(&SIZE),
                        INTEGER_ARG(&STATUS) );

   F77_IMPORT_INTEGER( SIZE, *size );
   F77_IMPORT_INTEGER( STATUS, *status );

   return;
}


F77_SUBROUTINE(grp_delet)( INTEGER(IGRP), INTEGER(STATUS) );

void grpDelet( int *igrp, int *status ){
   DECLARE_INTEGER(IGRP);
   DECLARE_INTEGER(STATUS);

   F77_EXPORT_INTEGER( *igrp, IGRP );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_CALL(grp_delet)( INTEGER_ARG(&IGRP),
                        INTEGER_ARG(&STATUS) );

   F77_IMPORT_INTEGER( IGRP, *igrp );
   F77_IMPORT_INTEGER( STATUS, *status );

   return;
}



F77_SUBROUTINE(grp_get)( INTEGER(IGRP), INTEGER(INDEX), INTEGER(SIZE), 
                         CHARACTER_ARRAY(NAMES), INTEGER(STATUS)
                         TRAIL(NAMES) );

/* Note the addition of a "len" parameter following the "names" array.
   This should be supplied equal to the allocated length of the shortest 
   string for which a pointer has been supplied in "names". This length
   should include room for the trailing null. */

void grpGet( int igrp, int index, int size, char *const *names, int len, 
             int *status ){
   DECLARE_INTEGER(IGRP);
   DECLARE_INTEGER(INDEX);
   DECLARE_INTEGER(SIZE);
   DECLARE_CHARACTER_ARRAY_DYN(NAMES);
   DECLARE_INTEGER(STATUS);

   F77_EXPORT_INTEGER( igrp, IGRP );
   F77_EXPORT_INTEGER( index, INDEX );
   F77_EXPORT_INTEGER( size, SIZE );
   F77_CREATE_CHARACTER_ARRAY(NAMES,len-1,size);
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_CALL(grp_get)( INTEGER_ARG(&IGRP),
                      INTEGER_ARG(&INDEX),
                      INTEGER_ARG(&SIZE),
                      CHARACTER_ARRAY_ARG(NAMES),
                      INTEGER_ARG(&STATUS) 
                      TRAIL_ARG(NAMES) );

   F77_IMPORT_CHARACTER_ARRAY_P(NAMES,NAMES_length,names,len,size);
   F77_FREE_CHARACTER(NAMES);
   F77_IMPORT_INTEGER( STATUS, *status );

   return;
}


