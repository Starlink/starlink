      SUBROUTINE COF_GETYP( PARNAM, MAXVAL, GRPID, STATUS )
*+
*  Name:
*     COF_GETYP

*  Purpose:
*     Obtains HDS numeric data-type values through an ADAM parameter,
*     stored in a GRP group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_GETYP( PARNAM, MAXVAL, GRPID, STATUS )

*  Description:
*     This routine serves FITS2NDF.  It requests HDS numeric primitive
*     data-type values through parameter PARNAM, up to a maximum of
*     MAXVAL values, that are stored in a GRP group with the identifier
*     GRPID.  Values can come via indirection from files, and repeated
*     prompting if the previous values entered ended with the "-"
*     continuation character.
*
*     The values are validated, and each may have up to one character
*     mistyped.  An error report is made should an invalid value be
*     supplied and the group cleared.  Unambiguous abbreviations may be
*     given.  A blank value (within quotes) is also permitted.  A null
*     value (!) equates to a blank value.
*
*     If fewer than MAXVAL values are supplied, the missing values take
*     the value of the last supplied type.  If more than MAXVAL values
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
*     All Rights Reserved.

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

*  External References:
      CHARACTER * ( 2 ) CHR_NTH  ! Ordinal abbreviation

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER ADDED              ! Number of items added to a group
      LOGICAL CFLAG              ! A group requires further input via
                                 ! continuation lines?
      LOGICAL GOOD               ! All group values are valid?
      CHARACTER * ( DAT__SZTYP ) HTYPE ! HDS data type in group
      INTEGER I                  ! Loop counter
      INTEGER NC                 ! Number of characters
      INTEGER NTYPE              ! Number of TYPE values
      INTEGER PENALT             ! String-matching penalty (not used)
      CHARACTER * ( DAT__SZTYP ) TYPE ! Data type for processing

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop until all values are acceptable.
      GOOD = .FALSE.
  100 CONTINUE
      IF ( .NOT. GOOD ) THEN

*  Create a new group to contain the input FMTCNVs.
         CALL GRP_NEW( 'Type values', GRPID, STATUS )

*  Allow for continuation lines.
         CFLAG = .TRUE.
         DO WHILE ( CFLAG .AND. STATUS .EQ. SAI__OK )

*  Get the list of FMTCNVs from the environment.
            CALL GRP_GROUP( PARNAM, GRP__NOID, GRPID, NTYPE, ADDED,
     :                      CFLAG, STATUS )

*  A null value is equivalent to a blank.
            IF ( STATUS .EQ. PAR__NULL ) THEN
               CALL ERR_ANNUL( STATUS )
               CALL GRP_PUT1( GRPID, ' ', 0, STATUS )
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

*  Validate the values.  First get each value, and then test that the
*  value is one the allowed types.
         DO I = 1, NTYPE
            CALL GRP_GET( GRPID, I, 1, HTYPE, STATUS )

*  This a bit naughty using a PAR internal routine.  This functionality
*  arguably should be part of CHR.  Allow one-character typing error,
*  and abbreviations.
            IF ( HTYPE .NE. ' ' ) THEN
               CALL PAR1_MENU( HTYPE, '_BYTE,_DOUBLE,_INTEGER,_REAL,'/
     :                         /',_UBYTE,_UWORD,_WORD', ',', 1, TYPE,
     :                         NC, PENALT, STATUS )
               IF ( STATUS .NE. SAI__OK ) THEN

*  Annul the error status.
                  CALL ERR_ANNUL( STATUS )

*  Display an informational message, including the incorrect string.
                  CALL MSG_SETI( 'I', I )
                  CALL MSG_SETC( 'TH', CHR_NTH( I ) )
                  CALL MSG_SETC( 'T', HTYPE )
		  CALL MSG_SETC( 'PARNAM', PARNAM )
                  CALL MSG_OUT( 'BADTYPE_ERR',
     :              'The ^I^TH value "^T" is not one of the '/
     :              /'acceptable HDS primitive types values for '/
     :              /'^PARNAM .', STATUS )

*  Let the user have another go.  So cancel the parameter value and
*  delete the group.
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
      IF ( NTYPE .GT. MAXVAL ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'NI', MAXVAL )
         CALL MSG_SETI( 'NTYPE', NTYPE )
         CALL MSG_SETC( 'PARNAM', PARNAM )
         CALL ERR_REP( 'COF_GETYP_VALCOUNT',
     :     'The number of ^PARNAM values (^NTYPE) exceeds '/
     :     /'the number of input files (^NI).', STATUS )

*  Tidy up and exit.
         CALL GRP_DELET( GRPID, STATUS )
         GOTO 999

*  Extend the group by duplication to give the same number of values
*  as input files.  The last value is duplicated.
      ELSE IF ( NTYPE .LT. MAXVAL ) THEN

*  Obtain the last value.
         CALL GRP_GET( GRPID, NTYPE, 1, HTYPE, STATUS )

*  Extend the original group by adding the required number of values.
*  This may not be as efficient as having an array but it avoids getting
*  workspace or having a fixed-length array.
         DO I = NTYPE + 1, MAXVAL
            CALL GRP_GRPEX( HTYPE, GRP__NOID, GRPID, NTYPE, ADDED,
     :                      CFLAG, STATUS )
         END DO
      END IF

  999 CONTINUE

      END
