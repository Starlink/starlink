      SUBROUTINE SST_CNTAC( STR, NCH )
*+
*  Name:
*     SST_CNTAC

*  Purpose:
*     Count the number of alphanumeric characters in a string.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SST_CNTAC( STR, NCH )

*  Description:
*     The routine returns an integer giving the number of alphanumeric
*     characters in the string supplied.

*  Arguments:
*     STR = CHARACTER * ( * ) (Given)
*        String whose characters are to be counted.
*     NCH = INTEGER (Returned)
*        Number of alphanumeric characters.

*  Notes:
*     -  Underscore '_' is counted as an alphanumeric character by this
*     routine.

*  Algorithm:
*     -  Loop, looking at each character and counting it if it is
*     alphanumeric.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15-AUG-1989 (RFWS):
*        Original version.
*     3-AUG-1990 (RFWS):
*        Changed routine name.
*     8-AUG-1990 (RFWS):
*        Changed to call CHR_ISALM to identify alphanumeric characters.
*     9-AUG-1990 (RFWS):
*        Removed use of CHR_LEN.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      CHARACTER * ( * ) STR

*  Arguments Returned:
      INTEGER NCH

*  External References:
      LOGICAL CHR_ISALM          ! Is character alphanumeric?

*  Local Variables:
      INTEGER I                  ! Loop counter for characters

*.

*  Initialise, then test each character for being alphanumeric.
      NCH = 0
      DO 1 I = 1, LEN( STR )
         IF ( CHR_ISALM( STR( I : I ) ) ) THEN

*  Count those which are.
            NCH = NCH + 1
         END IF
1     CONTINUE

      END
* @(#)sst_cntac.f   1.1   94/12/05 11:31:23   96/07/05 10:27:26
