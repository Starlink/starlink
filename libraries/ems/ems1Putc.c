/*
*+
*  Name:
*     ems1Putc

*  Purpose:
*     Put a CHARACTER string into another at a given position.

*  Language:
*     Starlink ANSI C

*  Invocation:
*     ems1Putc( cvalue, maxlen, string, iposn, status )

*  Description:
*     The string CVALUE (or as much of it as there is room for) is
*     copied into the part of STRING beginning at position IPOSN+1.
*     IPOSN is updated to indicate the end position of the copy of
*     CVALUE within STRING after this operation. If the resulting
*     string is truncated because STRING is too short, then the string
*     is terminated with an ellipsis and STATUS is returned set to 
*     EMS__STROV. The sizes of CVALUE and STRING are based on the 
*     declared Fortran 77 size given by the intrinsic function LEN. 

*  Implementation Notes:
*     The coding of this routine assumes that the need to append an 
*     ellipsis is rare: i.e. it is less efficient when it has to
*     append an ellipsis.

*  Arguments:
*     cvalue = char* (Given)
*        The string to be copied.
*     maxlen = int (Given)
*        Maximum length of output string
*     string = char* (Given and Returned)
*        The string into which CVALUE is to be copied.
*     iposn = int* (Given and Returned)
*        The position pointer within STRING.
*     status = int* (Returned)
*        The global status.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     ACD: A.C. Davenhall (ROE)
*     AJC: A.J. Chipperfield (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     RTP: R.T. Platon (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-JAN-1983 (JRG):
*        Original FORTRAN version.
*     14-FEB-2001 (RTP)
*        Rewritten in C based on the Fortran routine EMS1_PUTC
*      5-MAR-2001 (AJC):
*        Add maxlen argument
*        Use *iposn not iposn internally 
*     21-SEP-2001 (AJC):
*        Correct calculation of allow
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "sae_par.h"                 /* SAE public constants */
#include "ems1.h"                    /* EMS_ Internal functions */
#include "ems_sys.h"                 /* SAE public constants */

void ems1Putc( const char *cvalue, const int maxlen, char *string, int *iposn,
               int *status ) {
   int allow;              /* Allowed length of CVALUE for copying */
   int idx;                /* Character index */
   int size1;              /* Used length of CVALUE */

   TRACE("ems1Putc");

   /*  Initialise the returned status. */
   *status = SAI__OK;

/*  Get the size of target string. */
   size1 = strlen( cvalue );

/*  Check that the pointer is within string. */
   if ( *iposn < maxlen ) {

/*     Get the length that can be copied. */
      allow = MIN( size1, maxlen - *iposn - 1 );

/*     Copy the string. */
      strncpy( &string[*iposn+1] , cvalue, allow+1 );

/*     Check if an ellipsis is required. */
      if ( allow < size1 ) {

/*        Append an ellipsis. */
         idx = MAX( 0, maxlen - 3 );
         strcpy( &string[idx], "..." );
         *status = SAI__WARN;
      }

/*     Update the pointer value. */
         *iposn = *iposn + allow;

   } else {
/*     The pointer is beyond the declared length of the string, so 
 *     append an ellipsis. */
      idx = MAX( 0, maxlen - 3 );
      strcpy( &string[idx], "..." );
      *status = SAI__WARN;

/*     Update the pointer value. */
      *iposn = maxlen;
   }

   return;
}
