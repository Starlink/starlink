      SUBROUTINE DSA_PUT_HIST (SLOT, NTEXT, TEXT, STATUS)
*+
*  Name:
*     DSA_PUT_HIST

*  Purpose:
*     Append history text to a data structure.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA_PUT_HIST (SLOT, NTEXT, TEXT; STATUS)

*  Description:
*     Append history text to a data structure.

*  Arguments:
*     SLOT = INTEGER (Given)
*        The reference slot associated with the NDF.
*     NTEXT = INTEGER (Given)
*        Number of lines of text to be appended.
*     TEXT(NTEXT)*(*) (Given)
*        Text to be appended.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     acd: Clive Davenhall (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     27 Sep 2001 (acd):
*        Original version.
*     28 Oct 2001 (acd):
*        First stable version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants

*  Global Variables:
      INCLUDE 'DSA_COMMON'       ! DSA global variables

*  Arguments Given:
      INTEGER   SLOT
      INTEGER   NTEXT
      CHARACTER TEXT(NTEXT)*(*)

*  Status:
      INTEGER   STATUS           ! Global status

*  Local Variables:
      INTEGER   NDFID            ! NDF identifer for the output structure.

*.

*  Check inherited global status.
      IF ( STATUS .NE. 0 ) RETURN

*
*    Begin error context and translate status.

      CALL ERR_MARK
      STATUS = SAI__OK

*
*    Look up the NDF identifier for the given slot.

C      ndfid = dsa__refid1(slot)
      NDFID = DSA__MAPID1(SLOT)

*
*    Create the HISTORY component, if necessary, and add the history
*    text.  Then disable the addition of further history comonents.

      CALL NDF_HCRE (NDFID, STATUS)
      CALL NDF_HPUT ('NORMAL', ' ', .FALSE., NTEXT, TEXT, .TRUE.,
     :  .FALSE., .FALSE., NDFID, STATUS)
      CALL NDF_HSMOD ('DISABLED', NDFID, STATUS)

*
*    Translate the status and end the error context.

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_FLUSH (STATUS)
         STATUS = 1
      ELSE
         STATUS = 0
      END IF
      CALL ERR_RLSE

      END
