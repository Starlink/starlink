      SUBROUTINE NDF_END( STATUS )
*+
*  Name:
*     NDF_END

*  Purpose:
*     End the current NDF context.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_END( STATUS )

*  Description:
*     The routine ends the current NDF context, causing all NDF
*     identifiers and placeholders created within that context (i.e.
*     since a matching call to NDF_BEGIN) to be annulled. Any mapped
*     values associated with these identifiers are unmapped, and any
*     temporary NDFs which no longer have identifiers associated with
*     them are deleted.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  Matching pairs of calls to NDF_BEGIN and NDF_END may be
*     nested. An error will be reported if NDF_END is called without a
*     corresponding call to NDF_BEGIN.
*     -  This routine attempts to execute even if STATUS is set on
*     entry, although no further error report will be made if it
*     subsequently fails under these circumstances.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     6-OCT-1989 (RFWS):
*        Original version.
*     17-OCT-1989 (RFWS):
*        Corrected error in argument list of NDF1_ANNPL.
*     1-DEC-1989 (RFWS):
*        Sorted out error status handling.
*     29-JAN-1990 (RFWS):
*        Corrected bug in test for missing call to NDF_BEGIN.
*     3-JUN-1993 (RFWS):
*        Added error message logging for history recording.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Global Variables:
      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_CTX( NDF__MXACB ) = INTEGER (Read)
*           Context level at which ACB entry was made.
*        ACB_IDCTX = INTEGER (Read and Write)
*           Current identifier context level.

      INCLUDE 'NDF_PCB'          ! NDF_ Placeholder Control Block
*        PCB_CTX( NDF__MXPCB ) = INTEGER (Read)
*           Context level at which PCB entry was made.

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER ASTAT              ! Status for annulling table entries
      INTEGER IACBA              ! ACB slot to be annulled
      INTEGER IACBT              ! ACB slot to test
      INTEGER IPCBA              ! PCB slot to be annulled
      INTEGER IPCBT              ! PCB slot to test
      INTEGER NEXT               ! Next common block slot to consider
      INTEGER TSTAT              ! Temporary status variable

*.

*  Log any pending error message information for subsequent recording
*  in NDF history records.
      CALL NDF1_HLERR( STATUS )

*  Initialise a status value for annulling ACB and PCB entries.
      ASTAT = STATUS

*  Save the STATUS value and mark the error stack.
      TSTAT = STATUS
      CALL ERR_MARK

*  If the current identifier context level is not positive, then a
*  matching call to NDF_BEGIN has been omitted, so report an error.
      STATUS = SAI__OK
      IF ( ACB_IDCTX .LE. 1 ) THEN
         STATUS = NDF__MSBEG
         CALL ERR_REP( 'NDF_END_INV',
     :   'NDF_END called without a corresponding call to NDF_BEGIN ' //
     :   '(possible programming error).', STATUS )

*  Otherwise, decrement the current context level.
      ELSE
         ACB_IDCTX = ACB_IDCTX - 1

*  Loop to examine each active entry in the ACB.
         NEXT = 0
         IACBT = 0
1        CONTINUE                ! Start of 'DO WHILE' loop
         CALL NDF1_NXTSL( NDF__ACB, IACBT, NEXT, STATUS )
         IF ( ( STATUS .EQ. SAI__OK ) .AND. ( NEXT .NE. 0 ) ) THEN
            IACBT = NEXT

*  If the context level at which the entry was made exceeds the new
*  "current" context level, then annul the ACB entry.
            IF ( ACB_CTX( IACBT ) .GT. ACB_IDCTX ) THEN
               IACBA = IACBT
               CALL NDF1_ANL( IACBA, ASTAT )
            END IF
            GO TO 1
         END IF

*  Similarly, loop to examine all the active entries in the PCB.
         NEXT = 0
         IPCBT = 0
2        CONTINUE                ! Start of 'DO WHILE' loop
         CALL NDF1_NXTSL( NDF__PCB, IPCBT, NEXT, STATUS )
         IF ( ( STATUS .EQ. SAI__OK ) .AND. ( NEXT .NE. 0 ) ) THEN
            IPCBT = NEXT

*  If the context level at which the entry was made exceeds the new
*  "current" context level, then annul the PCB entry.
            IF ( PCB_CTX( IPCBT ) .GT. ACB_IDCTX ) THEN
               IPCBA = IPCBT
               CALL NDF1_ANNPL( .TRUE., IPCBA, ASTAT )
            END IF
            GO TO 2
         END IF
      END IF

*  If an error occurred while annulling any entry, but STATUS has not
*  been set, then transfer the bad ASTAT value to STATUS.
      IF ( STATUS .EQ. SAI__OK ) THEN
         IF ( ASTAT .NE. SAI__OK ) THEN
            STATUS = ASTAT
         END IF
      END IF

*  Annul any error if STATUS was previously bad, otherwise let the new
*  error report stand.
      IF ( STATUS .NE. SAI__OK ) THEN
         IF ( TSTAT .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
            STATUS = TSTAT

*  If appropriate, report context information and call the error tracing
*  routine.
         ELSE
            CALL ERR_REP( 'NDF_END_ERR',
     :      'NDF_END: Error ending an NDF context.', STATUS )
            CALL NDF1_TRACE( 'NDF_END', STATUS )
         END IF
      ELSE
         STATUS = TSTAT
      END IF

*  Release error stack.
      CALL ERR_RLSE

      END
