      SUBROUTINE NDF1_UMP( IACB, COMP, STATUS )
*+
*  Name:
*     NDF1_UMP

*  Purpose:
*     Unmap an ACB entry or one of its components.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_UMP( IACB, COMP, STATUS )

*  Description:
*     The routine unmaps components of an NDF which have previously
*     been mapped for access. The NDF is identified by its ACB entry.
*     The routine will either unmap a specified list of components of
*     the NDF (in which case an error will be reported if any of those
*     components has not previously been mapped), or can perform a
*     "wild carded" unmapping operation on all those components which
*     have been mapped (which could be none).

*  Arguments:
*     IACB = INTEGER (Given)
*        Index to the ACB entry to be unmapped.
*     COMP = CHARACTER * ( * ) (Given)
*        Name(s) of the NDF component(s) to be unmapped; 'DATA',
*        'QUALITY', 'VARIANCE' or '*' (case insensitive). The last
*        value acts as a wild card, causing all mapped components to be
*        unmapped.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine attempts to execute even if STATUS is set on
*     entry, although no further error report will be made if it
*     subsequently fails under these circumstances.
*     -  If the 'AXIS' component is specified, then all mapped axis
*     array will be unmapped. In this case, no error will occur if
*     there are no mapped axis arrays.

*  Algorithm:
*     -  Save the error context on entry.
*     -  Initialise the component count.
*     -  Initialise the character pointer to the start of the component
*     list. Then loop to extract each component from the list.
*     -  Find the final character of the next element in the list.
*     -  Locate the first and last non-blank characters in the element,
*     checking that it is not entirely blank.
*     -  Increment the component count.
*     -  If a wild card component name has been given then consider all
*     the NDF's components in turn to see which are mapped. Call the
*     appropriate routine to unmap each one.
*     -  Otherwise, test the component name against each permitted
*     value in turn, calling the appropriate unmapping routine.
*     -  If the component name was not recognised, then report an
*     error.
*     -  Increment the character pointer to the start of the next
*     element in the component list and return to process it.
*     -  If no error has occurred, but the number of components
*     processed is zero, then report an error.
*     -  Restore the error context.

*  Copyright:
*     Copyright (C) 1989, 1990 Science & Engineering Research Council.
*     All Rights Reserved.

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
*     5-OCT-1989 (RFWS):
*        Original version.
*     15-DEC-1989 (RFWS):
*        Added support for the variance component.
*     8-JAN-1990 (RFWS):
*        Changed wild card character to '*'.
*     11-JAN-1990 (RFWS):
*        Changed error code.
*     15-JAN-1990 (RFWS):
*        Added capability for processing comma-separated component
*        lists.
*     17-JAN-1990 (RFWS):
*        Added explicit error messages if invalid component names are
*        given.
*     31-JAN-1990 (RFWS):
*        Added support for the quality component.
*     1-MAR-1990 (RFWS):
*        Fixed illegal character string concatenation.
*     16-MAR-1990 (RFWS):
*        Set the quality masking flag to .TRUE. when the quality
*        component is successfully unmapped.
*     10-OCT-1990 (RFWS):
*        Installed unmapping of axis arrays.
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

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_DMAP( NDF__MXACB ) = LOGICAL (Read)
*           Whether the NDF's data array is mapped for access.
*        ACB_QMF( NDF__MXACB ) = LOGICAL (Write)
*           Quality masking flag.
*        ACB_VMAP( NDF__MXACB ) = LOGICAL (Read)
*           Whether the NDF's variance array is mapped for access.

*  Arguments Given:
      INTEGER IACB
      CHARACTER * ( * ) COMP

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL NDF1_SIMLR         ! String compare with abbreviation

*  Local Variables:
      INTEGER F                  ! Position of first non-blank character
      INTEGER I1                 ! Position of first component character
      INTEGER I2                 ! Position of last component character
      INTEGER L                  ! Position of last non-blank character
      INTEGER NCOMP              ! Number non-blank components specified
      INTEGER TSTAT              ! Temporary status variable

*.

*  Save the STATUS value and mark the error stack.
      TSTAT = STATUS
      CALL ERR_MARK

*  Initialise the component count.
      STATUS = SAI__OK
      NCOMP = 0

*  Initialise the character pointer to the start of the component list.
*  Then loop to extract each element from the component list.
      I1 = 1
1     CONTINUE                ! Start of "DO WHILE" loop
      IF ( ( STATUS .EQ. SAI__OK ) .AND.
     :     ( I1 .LE. LEN( COMP ) ) ) THEN

*  Find the final character of the next element in the component list
*  (the last character before a comma or end of string).
         I2 = INDEX( COMP( I1 : ), ',' )
         IF ( I2 .EQ. 0 ) THEN
            I2 = LEN( COMP )
         ELSE
            I2 = I2 + I1 - 2
         END IF
         IF ( I2 .GE. I1 ) THEN

*  Locate the first and last non-blank characters in the element,
*  checking that it is not entirely blank.
            CALL CHR_FANDL( COMP( I1 : I2 ), F, L )
            IF ( L .GE. F ) THEN
               F = F + I1 - 1
               L = L + I1 - 1

*  Increment the component count.
               NCOMP = NCOMP + 1

*  If a wild card component name was given, then consider all the NDF's
*  components to see if they are mapped.  Call the appropriate routine
*  to unmap each one.
               IF ( COMP( F : L ) .EQ. '*' ) THEN

*  ...unmap the data array.
                  IF ( ACB_DMAP( IACB ) ) THEN
                     CALL NDF1_DUMP( IACB, STATUS )
                  END IF

*  ...unmap the quality array and set the quality masking flag to true
*  if successful.
                  IF ( ACB_QMAP( IACB ) ) THEN
                     CALL NDF1_QUMP( IACB, STATUS )
                     IF ( STATUS .EQ. SAI__OK ) THEN
                        ACB_QMF( IACB ) = .TRUE.
                     END IF
                  END IF

*  ...unmap the variance array.
                  IF ( ACB_VMAP( IACB ) ) THEN
                     CALL NDF1_VUMP( IACB, STATUS )
                  END IF

*  ...unmap any axis arrays which may be mapped.
                  CALL NDF1_AUMP( 0, IACB, '*', STATUS )

*  If the component name is not a wild card, then test it against each
*  permitted value in turn, calling the appropriate unmapping routine.

*  AXIS component:
*  ==============
*  Unmap any axis arrays which may be mapped.
               ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'AXIS',
     :                               NDF__MINAB ) ) THEN
                  CALL NDF1_AUMP( 0, IACB, '*', STATUS )

*  DATA component:
*  ==============
*  Unmap the data array.
               ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'DATA',
     :                               NDF__MINAB ) ) THEN
                  CALL NDF1_DUMP( IACB, STATUS )

*  EXTENSION:
*  =========
*  Report an error, as an extension cannot be umapped.
               ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'EXTENSION',
     :                               NDF__MINAB ) ) THEN
                  STATUS = NDF__CNMIN
                  CALL ERR_REP( 'NDF1_UMP_EXT',
     :            'An EXTENSION cannot be unmapped (possible ' //
     :            'programming error).', STATUS )

*  HISTORY component:
*  =================
*  Report an error, as this component cannot be unmapped.
               ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'HISTORY',
     :                               NDF__MINAB ) ) THEN
                  STATUS = NDF__CNMIN
                  CALL ERR_REP( 'NDF1_UMP_HIS',
     :            'A HISTORY component cannot be unmapped (possible ' //
     :            'programming error).', STATUS )

*  LABEL component:
*  ===============
*  Report an error, as this component cannot be unmapped.
               ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'LABEL',
     :                               NDF__MINAB ) ) THEN
                  STATUS = NDF__CNMIN
                  CALL ERR_REP( 'NDF1_UMP_LAB',
     :            'A LABEL component cannot be unmapped (possible '//
     :            'programming error).', STATUS )

*  QUALITY component:
*  =================
*  Unmap the quality array and set the quality masking flag to .TRUE.
*  if successful.
               ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'QUALITY',
     :                               NDF__MINAB ) ) THEN
                  CALL NDF1_QUMP( IACB, STATUS )
                  IF ( STATUS .EQ. SAI__OK ) THEN
                     ACB_QMF( IACB ) = .TRUE.
                  END IF

*  TITLE component:
*  ===============
*  Report an error, as this component cannot be unmapped.
               ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'TITLE',
     :                               NDF__MINAB ) ) THEN
                  STATUS = NDF__CNMIN
                  CALL ERR_REP( 'NDF1_UMP_TIT',
     :            'A TITLE component cannot be unmapped (possible ' //
     :            'programming error).', STATUS )

*  UNITS component:
*  ===============
*  Report an error, as this component cannot be unmapped.
               ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'UNITS',
     :                               NDF__MINAB ) ) THEN
                  STATUS = NDF__CNMIN
                  CALL ERR_REP( 'NDF1_UMP_UNI',
     :            'A UNITS component cannot be unmapped (possible ' //
     :            'programming error).', STATUS )

*  VARIANCE component:
*  ==================
*  Unmap the variance array.
               ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'VARIANCE',
     :                               NDF__MINAB ) ) THEN
                  CALL NDF1_VUMP( IACB, STATUS )

*  If the component name was not recognised, then report an error.
               ELSE
                  STATUS = NDF__CNMIN
                  CALL MSG_SETC( 'BADCOMP', COMP( F : L ) )
                  CALL ERR_REP( 'NDF1_UMP_COMP',
     :                          'Invalid array component name ' //
     :                          '''^BADCOMP'' specified (possible ' //
     :                          'programming error).', STATUS )
               END IF
            END IF
         END IF

*  Increment the character pointer to the start of the next element in
*  the component list and return to process the next element.
         I1 = I2 + 2
         GO TO 1
      END IF

*  If no error has occurred, but no non-blank component names have been
*  processed, then report an error.
      IF ( ( STATUS .EQ. SAI__OK ) .AND. ( NCOMP .EQ. 0 ) ) THEN
         STATUS = NDF__NOCMP
         CALL ERR_REP( 'NDF1_UMP_NONE',
     :                 'No array component name specified (possible ' //
     :                 'programming error).', STATUS )
      END IF

*  Annul any error if STATUS was previously bad, otherwise let the new
*  error report stand.
      IF ( STATUS .NE. SAI__OK ) THEN
         IF ( TSTAT .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
            STATUS = TSTAT

*  Call error tracing routine if appropriate.
         ELSE
            CALL NDF1_TRACE( 'NDF1_UMP', STATUS )
         END IF
      ELSE
         STATUS = TSTAT
      END IF

*  Release the error stack.
      CALL ERR_RLSE

      END
