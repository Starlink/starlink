      SUBROUTINE COF_GETFC( PARNAM, MAXVAL, GRPID, STATUS )
*+
*  Name:
*     COF_GETFC

*  Purpose:
*     Obtains format-conversion flags through an ADAM parameter,
*     stored in a GRP group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_GETFC( PARNAM, MAXVAL, GRPID, STATUS )

*  Description:
*     This routine serves FITS2NDF.  It requests format-conversion
*     (i.e. use BSCALE and BZERO to convert integers to floating point)
*     values through parameter PARNAM, up to a maximum of MAXVAL values,
*     that are stored in a GRP group with the identifier GRPID.  Values
*     can come via indirection from files, and repeated prompting if the
*     previous values entered ended with the "-" continuation character.
*
*     The values are validated.  The values should be boolean or
*     "Native" (the latter can be abbrevated to "Na").  An error report
*     is made should an invalid value be supplied and the group cleared.
*     A null value (!) equates to a TRUE value.
*
*     If fewer than MAXVAL values are supplied, the missing values take
*     the value of the last supplied flag.  If more than MAXVAL values
*     are supplied, a SAI_ERROR STATUS is returned and the group
*     deleted.

*  Arguments:
*     PARNAM = CHARACTER * ( * ) (Given)
*        The name of the parameter through which the values are
*        obtained.
*     MAXVAL = INTEGER (Given)
*        The maximum number of values in the group.  This corresponds
*        to the number of input files supplied to FITS2NDF.
*     GRPID = INTEGER (Returned)
*        The group identifier.
*     STATUS = INTEGER (Given & Returned)
*        Global status value.

*  Notes:
*     -  In the error message when the number of values supplied exceeds
*     MAXVAL, the report refers to exceeding the the number of input
*     files rather than using a generic phrase.

*  Copyright:
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     Copyright (C) 2007 Science & Technology Facilities Council. All
*     Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2006 April 7 (MJC):
*        Original version.
*     2007 January 3 (MJC):
*        Allow for Native value.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Data-system constants
      INCLUDE 'GRP_PAR'          ! GRP_ constants
      INCLUDE 'PAR_ERR'          ! PAR_ error constants

*  Arguments Given:
      CHARACTER * ( * ) PARNAM
      INTEGER MAXVAL

*  Arguments Returned:
      INTEGER GRPID

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      CHARACTER * ( 2 ) CHR_NTH  ! Ordinal abbreviation

*  Local Variables:
      INTEGER ADDED              ! Number of items added to a group
      LOGICAL CFLAG              ! A group requires further input via
                                 ! continuation lines?
      CHARACTER * ( 6 ) FMTCON   ! Character form of a FMTCNV value
      LOGICAL FMTCNV             ! Apply scale and zero?
      LOGICAL GOOD               ! All group values are valid?
      INTEGER I                  ! Loop counter
      INTEGER NFC                ! Number of format-conversion values

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop until all values are acceptable.
      GOOD = .FALSE.
  100 CONTINUE
      IF ( .NOT. GOOD ) THEN

*  Create a new group to contain the input flags.
         CALL GRP_NEW( 'Format-conversion flags', GRPID, STATUS )

*  Allow for continuation lines.
         CFLAG = .TRUE.
         DO WHILE ( CFLAG .AND. STATUS .EQ. SAI__OK )

*  Get the list of FMTCNVs from the environment.
            CALL GRP_GROUP( PARNAM, GRP__NOID, GRPID, NFC, ADDED,
     :                      CFLAG, STATUS )

*  A null value is equivalent to a blank.
            IF ( STATUS .EQ. PAR__NULL ) THEN
               CALL ERR_ANNUL( STATUS )
               CALL GRP_PUT1( GRPID, 'TRUE', 0, STATUS )
            END IF

*  Cancel the parameter association in order to get more group values
*  through the parameter, unless there are no more to obtain.
            IF ( CFLAG ) CALL PAR_CANCL( PARNAM, STATUS )
         END DO

*  Tidy up and exit.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL GRP_DELET( GRPID, STATUS )
            GOTO 999
         END IF

*  Assume that the values are good for the moment.
         GOOD = .TRUE.

*  Validate the values.  First get each value.  Command-line
*  keywords can appear instead of the values.  So recognise the
*  command-line keyword presence meaning TRUE and no<keyword> meaning
*  FALSE.  Update the group values.
         DO I = 1, NFC
            CALL GRP_GET( GRPID, I, 1, FMTCON, STATUS )
            CALL CHR_UCASE( FMTCON )
            IF ( FMTCON .EQ. PARNAM( 1:5 ) ) THEN
               FMTCON = 'TRUE'
            ELSE IF ( FMTCON .EQ. 'NO'//PARNAM( 1:3 ) ) THEN
               FMTCON = 'FALSE'
            ELSE IF ( FMTCON( 1:2 ) .EQ. 'NA' ) THEN
               FMTCON = 'NATIVE'
            END IF
            CALL ERR_MARK
            CALL GRP_PUT( GRPID, 1, FMTCON, I, STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_ANNUL( STATUS )
               FMTCON = 'TRUE'
            ENDIF
            CALL ERR_RLSE( STATUS )

*  Convert the group value to a logical and then test that the
*  conversion was successful.
            IF ( FMTCON .NE. 'NATIVE' ) THEN
               CALL CHR_CTOL( FMTCON, FMTCNV, STATUS )
               IF ( STATUS .NE. SAI__OK ) THEN

*  Annul the error status.
                  CALL ERR_ANNUL( STATUS )

*  Display an informational message, including the incorrect string.
                  CALL MSG_SETI( 'I', I )
                  CALL MSG_SETC( 'TH', CHR_NTH( I ) )
                  CALL MSG_SETC( 'GM', FMTCON )
                  CALL MSG_SETC( 'PARNAM', PARNAM )
                  CALL MSG_OUT( 'FMTCNV_ERR',
     :             'The ^I^TH value "^GM" is not one of the '/
     :             /'acceptable logical values for ^PARNAM .', STATUS )

*  Let the user have another go.  So cancel the parameter value and
*  Delete the group.
                  CALL PAR_CANCL( PARNAM, STATUS )
                  CALL GRP_DELET( GRPID, STATUS )
                  GOOD = .FALSE.
                  GOTO 100
	       END IF
            END IF
         END DO
      END IF

*  There are some special cases.  A single value means apply it to all
*  files.  If there are too few, the last value is used for the
*  remainder.  If there are too many, an error results.
      IF ( NFC .GT. MAXVAL ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'NI', MAXVAL )
         CALL MSG_SETI( 'NFC', NFC )
         CALL MSG_SETC( 'PARNAM', PARNAM )
         CALL ERR_REP( 'COF_GETFC_VALCOUNT',
     :     'The number of ^PARNAM values (^NFC) exceeds '/
     :     /'the number of input files (^NI).', STATUS )

*  Tidy up and exit.
         CALL GRP_DELET( GRPID, STATUS )
         GOTO 999

*  Extend the group by duplication to give the same number of values
*  as input files.  The last value is duplicated.
      ELSE IF ( NFC .LT. MAXVAL ) THEN

*  Obtain the last value.
         CALL GRP_GET( GRPID, NFC, 1, FMTCON, STATUS )

*  Extend the original group by adding the required number of values.
*  This may not be as efficient as having an array but it avoids getting
*  workspace or having a fixed-length array.
         DO I = NFC + 1, MAXVAL
            CALL GRP_GRPEX( FMTCON, GRP__NOID, GRPID, NFC, ADDED,
     :                      CFLAG, STATUS )
         END DO
      END IF

  999 CONTINUE

      END
