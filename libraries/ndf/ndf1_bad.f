      SUBROUTINE NDF1_BAD( IACB, COMP, CHECK, BAD, STATUS )
*+
*  Name:
*     NDF1_BAD

*  Purpose:
*     Determine if an array component of an ACB entry may contain bad
*     pixels.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_BAD( IACB, COMP, CHECK, BAD, STATUS )

*  Description:
*     The routine returns a logical value indicating whether an array
*     component of an NDF may contain bad pixels for which checks must
*     be made when the array's values are processed. Only if the
*     returned value is .FALSE. can such checks be omitted. If the
*     CHECK argument to this routine is set .TRUE., then it will also
*     perform an explicit check (if necessary) to see whether bad
*     pixels are actually present.

*  Arguments:
*     IACB = INTEGER (Given)
*        Index to the NDF entry in the ACB.
*     COMP = CHARACTER * ( * ) (Given)
*        Name of the NDF array component; 'DATA', 'QUALITY' or
*        'VARIANCE'.
*     CHECK = LOGICAL (Given)
*        Whether to perform an explicit check to see whether bad pixels
*        are actually present.
*     BAD = LOGICAL (Returned)
*        Whether it is necessary to check for bad pixels when
*        processing the array's values.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  A comma-separated list of component names may also be
*     supplied, in which case the routine returns the logical "OR" of
*     the results for each component.
*     -  If CHECK is set .FALSE., then the returned value of BAD will
*     indicate whether bad pixels might be present and should therefore
*     be checked for during subsequent processing. However, even if BAD
*     is returned .TRUE. in such circumstances, it is still possible
*     that there may not actually be any bad pixels present (for
*     instance, in an NDF section, the region of the array accessed
*     might happen to avoid all the bad pixels).
*     -  If CHECK is set .TRUE., then an explicit check will be made,
*     if necessary, to ensure that BAD is only returned .TRUE. if bad
*     pixels are actually present in the data.
*     -  A .TRUE. result will be returned for any components whose
*     value is undefined, except in the case of the QUALITY component,
*     for which a .FALSE. result is always returned in these
*     circumstances.
*     -  If a component is mapped for access, then the value of BAD
*     will refer to the actual mapped values. It may differ from its
*     original (unmapped) value if conversion errors occurred during
*     the mapping process or if an initialisation option of '/ZERO' was
*     specified for a component whose value was initially undefined.

*  Algorithm:
*     -  Initialise the result and the component count.
*     -  Initialise the character pointer to the start of the component
*     list. Then loop to extract each component from the list.
*     -  Find the final character of the next element in the list.
*     -  Locate the first and last non-blank characters in the element,
*     checking that it is not entirely blank.
*     -  Increment the component count.
*     -  Compare the component name with each value in turn, taking the
*     appropriate action or reporting an error message if an
*     inappropriate component name has been given.
*     -  If the component name was not recognised, then report an
*     error.
*     -  Increment the character pointer to the start of the next
*     element in the component list and return to process it.
*     -  If no error has occurred, but the number of components
*     processed is zero, then report an error.

*  Copyright:
*     Copyright (C) 1989, 1990, 1991 Science & Engineering Research Council.
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
*     22-NOV-1989 (RFWS):
*        Original version.
*     18-DEC-1989 (RFWS):
*        Added support for the variance component.
*     11-JAN-1990 (RFWS):
*        Added more specific error messages if invalid component names
*        are supplied. Also added support for comma-separated component
*        lists.
*     22-FEB-1990 (RFWS):
*        Renamed from NDF_BAD to NDF1_BAD and changed to access the NDF
*        via its ACB index rather than its identifier.
*     1-MAR-1990 (RFWS):
*        Fixed illegal character concatenation.
*     23-MAR-1990 (RFWS):
*        Changed to call NDF1_DBAD.
*     2-JAN-1991 (RFWS):
*        Removed unnecessary use of NDF_ACB include file.
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

*  Arguments Given:
      INTEGER IACB
      CHARACTER * ( * ) COMP
      LOGICAL CHECK

*  Arguments Returned:
      LOGICAL BAD

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

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the result and the component count.
      BAD = .FALSE.
      NCOMP = 0

*  Initialise the character pointer to the start of the component list.
*  Then loop to extract each element from the component list.
      I1 = 1
1     CONTINUE                   ! Start of "DO WHILE" loop
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

*  Compare the component name with each value in turn (allowing
*  abbreviation), and take the appropriate action, or report an error
*  if an inappropriate component name has been given.

*  AXIS component.
*  ==============
*  Report an error, since this component cannot have bad pixels.
               IF ( NDF1_SIMLR( COMP( F : L ), 'AXIS',
     :                          NDF__MINAB ) ) THEN
                  STATUS = NDF__CNMIN
                  CALL ERR_REP( 'NDF1_BAD_AXI',
     :            'An AXIS component cannot have bad pixels ' //
     :            '(possible programming error).', STATUS )

*  DATA component:
*  ==============
*  Determine the bad pixel flag value for the data component.
               ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'DATA',
     :                               NDF__MINAB ) ) THEN
                  IF ( .NOT. BAD ) THEN
                     CALL NDF1_DBAD( IACB, CHECK, BAD, STATUS )
                  END IF

*  EXTENSION.
*  =========
*  Report an error, since extensions cannot have bad pixels.
               ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'EXTENSION',
     :                               NDF__MINAB ) ) THEN
                  STATUS = NDF__CNMIN
                  CALL ERR_REP( 'NDF1_BAD_EXT',
     :            'An EXTENSION cannot have bad pixels ' //
     :            '(possible programming error).', STATUS )

*  HISTORY component.
*  =================
*  Report an error, since this component cannot have bad pixels.
               ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'HISTORY',
     :                               NDF__MINAB ) ) THEN
                  STATUS = NDF__CNMIN
                  CALL ERR_REP( 'NDF1_BAD_HIS',
     :            'A HISTORY component cannot have bad pixels ' //
     :            '(possible programming error).', STATUS )

*  LABEL component.
*  ===============
*  Report an error, since this component cannot have bad pixels.
               ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'LABEL',
     :                               NDF__MINAB ) ) THEN
                  STATUS = NDF__CNMIN
                  CALL ERR_REP( 'NDF1_BAD_LAB',
     :            'A LABEL component cannot have bad pixels ' //
     :            '(possible programming error).', STATUS )

*  QUALITY component.
*  =================
*  If the QUALITY component was specified, then set a .FALSE. result,
*  since bad pixels are disregarded in this component. In practice,
*  this amounts to doing nothing.
               ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'QUALITY',
     :                   NDF__MINAB ) ) THEN
                  CONTINUE

*  TITLE component.
*  ===============
*  Report an error, since this component cannot have bad pixels.
               ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'TITLE',
     :                               NDF__MINAB ) ) THEN
                  STATUS = NDF__CNMIN
                  CALL ERR_REP( 'NDF1_BAD_TIT',
     :            'A TITLE component cannot have bad pixels ' //
     :            '(possible programming error).', STATUS )

*  UNITS component.
*  ===============
*  Report an error, since this component cannot have bad pixels.
               ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'UNITS',
     :                               NDF__MINAB ) ) THEN
                  STATUS = NDF__CNMIN
                  CALL ERR_REP( 'NDF1_BAD_UNI',
     :            'A UNITS component cannot have bad pixels ' //
     :            '(possible programming error).', STATUS )

*  VARIANCE component:
*  ==================
*  Determine the bad pixel flag value for the variance array.
               ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'VARIANCE',
     :                               NDF__MINAB ) ) THEN
                  IF ( .NOT. BAD ) THEN
                     CALL NDF1_VBAD( IACB, CHECK, BAD, STATUS )
                  END IF

*  If the NDF component name was not recognised, then report an error.
               ELSE
                  STATUS = NDF__CNMIN
                  CALL MSG_SETC( 'BADCOMP', COMP( F : L ) )
                  CALL ERR_REP( 'NDF1_BAD_CMP',
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
         CALL ERR_REP( 'NDF1_BAD_NONE',
     :                 'No array component name specified (possible ' //
     :                 'programming error).', STATUS )
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_BAD', STATUS )

      END
