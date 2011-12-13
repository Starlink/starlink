      SUBROUTINE TRN1_ERROR( ROUTIN, TEXT, STATUS )
*+
*  Name:
*     TRN1_ERROR

*  Purpose:
*     Report a TRANSFORM error message.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TRN1_ERROR( ROUTIN, TEXT, STATUS )

*  Description:
*     The routine reports an error message issued by the TRN_ facility.
*     All error messages from this facility are reported via this
*     routine, which may be substituted to change the way error reports
*     are issued to the user.  This version reports error messages via
*     the ERR_ facility, by calling the routine ERR_REP.  The error
*     message has the form:

*            <routine>: <text> - <message>

*     where <text> is the text supplied, <message> is the error message
*     associated with the STATUS value supplied and <routine> is the
*     name of the routine issuing the error messsage.  If <text> is
*     blank, the abbreviated form:

*           <routine>: <message>

*     is used. This routine will execute regardless of the STATUS value
*     set on entry.

*  Copyright:
*     Copyright (C) 1988, 1992 Science & Engineering Research Council.
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
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     16-FEB-1988 (RFWS):
*        Original version.
*     12-FEB-1992 (RFWS):
*        Changed format of error messages. Also changed to explicitly
*        supply the error message to be used for each TRANSFORM error
*        status value.
*     17-FEB-1992 (RFWS):
*        Removed non-standard string concatenation.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'TRN_ERR'          ! TRN_ error codes
      INCLUDE 'ERR_PAR'          ! ERR_ public constants

*  Arguments Given:
      CHARACTER * ( * ) ROUTIN   ! Name of the calling routine
      CHARACTER * ( * ) TEXT     ! The text of the error message

*  Status:
      INTEGER STATUS             ! Error status

*  External References:
      INTEGER CHR_LEN            ! Significant length of a string

*  Local Variables:
      CHARACTER * ( ERR__SZMSG ) MSG ! Copy of error message text
      INTEGER LMSG               ! Length of text supplied

*.

*  Define a message token for the routine name.
      CALL MSG_SETC( 'ROUTINE', ROUTIN )

*  Define a message token for the error message associated with the
*  status value.
      IF ( STATUS .EQ. TRN__BADCN ) THEN
         CALL MSG_SETC( 'MESSAGE',
     :      '"Bad" constant explicitly specified (TRN__BADCN)' )

      ELSE IF ( STATUS .EQ. TRN__OPNOS ) THEN
         CALL MSG_SETC( 'MESSAGE',
     :      'Operation not supported (TRN__OPNOS)' )

      ELSE IF ( STATUS .EQ. TRN__CLSIN ) THEN
         CALL MSG_SETC( 'MESSAGE',
     :      'Classification information invalid (TRN__CLSIN)' )

      ELSE IF ( STATUS .EQ. TRN__CMPER ) THEN
         CALL MSG_SETC( 'MESSAGE',
     :      'Compilation error (TRN__CMPER)' )

      ELSE IF ( STATUS .EQ. TRN__CMTOF ) THEN
         CALL MSG_SETC( 'MESSAGE',
     :      'Compiled mapping table overflow (TRN__CMTOF)' )

      ELSE IF ( STATUS .EQ. TRN__CONIN ) THEN
         CALL MSG_SETC( 'MESSAGE',
     :      'Constant syntax invalid (TRN__CONIN)' )

      ELSE IF ( STATUS .EQ. TRN__DELIN ) THEN
         CALL MSG_SETC( 'MESSAGE',
     :      'Delimiting comma invalid (TRN__DELIN)' )

      ELSE IF ( STATUS .EQ. TRN__DIMIN ) THEN
         CALL MSG_SETC( 'MESSAGE',
     :      'Dimensions invalid (TRN__DIMIN)' )

      ELSE IF ( STATUS .EQ. TRN__DSTIN ) THEN
         CALL MSG_SETC( 'MESSAGE',
     :      'Definition status invalid (TRN__DSTIN)' )

      ELSE IF ( STATUS .EQ. TRN__DUVAR ) THEN
         CALL MSG_SETC( 'MESSAGE',
     :      'Duplicate variable name (TRN__DUVAR)' )

      ELSE IF ( STATUS .EQ. TRN__EXPUD ) THEN
         CALL MSG_SETC( 'MESSAGE',
     :      'Expression undefined (TRN__EXPUD)' )

      ELSE IF ( STATUS .EQ. TRN__ICDIR ) THEN
         CALL MSG_SETC( 'MESSAGE',
     :      'Incompatible transformation directions (TRN__ICDIR)' )

      ELSE IF ( STATUS .EQ. TRN__MAPUD ) THEN
         CALL MSG_SETC( 'MESSAGE',
     :      'Mapping undefined (TRN__MAPUD)' )

      ELSE IF ( STATUS .EQ. TRN__MIDIN ) THEN
         CALL MSG_SETC( 'MESSAGE',
     :      'Compiled mapping identifier invalid (TRN__MIDIN)' )

      ELSE IF ( STATUS .EQ. TRN__MIOPA ) THEN
         CALL MSG_SETC( 'MESSAGE',
     :      'Missing or invalid operand (TRN__MIOPA)' )

      ELSE IF ( STATUS .EQ. TRN__MIOPR ) THEN
         CALL MSG_SETC( 'MESSAGE',
     :      'Missing or invalid operator (TRN__MIOPR)' )

      ELSE IF ( STATUS .EQ. TRN__MISVN ) THEN
         CALL MSG_SETC( 'MESSAGE',
     :      'Missing variable name (TRN__MISVN)' )

      ELSE IF ( STATUS .EQ. TRN__MLPAR ) THEN
         CALL MSG_SETC( 'MESSAGE',
     :      'Missing left parenthesis (TRN__MLPAR)' )

      ELSE IF ( STATUS .EQ. TRN__MRPAR ) THEN
         CALL MSG_SETC( 'MESSAGE',
     :      'Missing right parenthesis (TRN__MRPAR)' )

      ELSE IF ( STATUS .EQ. TRN__NDCMM ) THEN
         CALL MSG_SETC( 'MESSAGE',
     :      'Number of data coordinates mis-matched (TRN__NDCMM)' )

      ELSE IF ( STATUS .EQ. TRN__NMVMM ) THEN
         CALL MSG_SETC( 'MESSAGE',
     :      'Number of module variables mis-matched (TRN__NMVMM)' )

      ELSE IF ( STATUS .EQ. TRN__NTVMM ) THEN
         CALL MSG_SETC( 'MESSAGE',
     :   'Number of transformation variables mis-matched (TRN__NTVMM)' )

      ELSE IF ( STATUS .EQ. TRN__NVRIN ) THEN
         CALL MSG_SETC( 'MESSAGE',
     :      'Number of variables invalid (TRN__NVRIN)' )

      ELSE IF ( STATUS .EQ. TRN__OPCIN ) THEN
         CALL MSG_SETC( 'MESSAGE',
     :      'Operation code invalid (TRN__OPCIN)' )

      ELSE IF ( STATUS .EQ. TRN__PRCIN ) THEN
         CALL MSG_SETC( 'MESSAGE',
     :      'Precision invalid (TRN__PRCIN)' )

      ELSE IF ( STATUS .EQ. TRN__TOKIN ) THEN
         CALL MSG_SETC( 'MESSAGE',
     :      'Token name invalid (TRN__TOKIN)' )

      ELSE IF ( STATUS .EQ. TRN__TRNUD ) THEN
         CALL MSG_SETC( 'MESSAGE',
     :      'Transformation undefined (TRN__TRNUD)' )

      ELSE IF ( STATUS .EQ. TRN__TRUNC ) THEN
         CALL MSG_SETC( 'MESSAGE',
     :      'Character string truncated (TRN__TRUNC)' )

      ELSE IF ( STATUS .EQ. TRN__TYPIN ) THEN
         CALL MSG_SETC( 'MESSAGE',
     :      'Type invalid (TRN__TYPIN)' )

      ELSE IF ( STATUS .EQ. TRN__VARIN ) THEN
         CALL MSG_SETC( 'MESSAGE',
     :      'Variable name invalid (TRN__VARIN)' )

      ELSE IF ( STATUS .EQ. TRN__VARUD ) THEN
         CALL MSG_SETC( 'MESSAGE',
     :      'Variable name undefined (TRN__VARUD)' )

      ELSE IF ( STATUS .EQ. TRN__VERMM ) THEN
         CALL MSG_SETC( 'MESSAGE',
     :      'Software version mis-match (TRN__VERMM)' )

      ELSE IF ( STATUS .EQ. TRN__WRNFA ) THEN
         CALL MSG_SETC( 'MESSAGE',
     :      'Wrong number of function arguments (TRN__WRNFA)' )

*  If the error status was not a TRANSFORM error status, then attempt
*  to interpret it as an operating system status value.
      ELSE
         CALL ERR_SYSER( 'MESSAGE', STATUS )
      END IF

*  Report the error, including a copy of the error message text
*  supplied, if not blank.
      LMSG = CHR_LEN( TEXT )
      IF( LMSG .NE. 0 ) THEN
         MSG( : LMSG ) = TEXT( : LMSG )
         CALL ERR_REP( 'TRN_ERR',
     :                 '^ROUTINE: ' // MSG( : LMSG ) // ' - ^MESSAGE.',
     :                 STATUS )
      ELSE
         CALL ERR_REP( 'TRN_ERR',
     :                 '^ROUTINE: ^MESSAGE.', STATUS )
      END IF

*  Exit routine.
      END
