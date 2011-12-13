      SUBROUTINE MSG_FMTC( TOKEN, FORMAT, CVALUE )
*+
*  Name:
*     MSG_FMTC

*  Purpose:
*     Assign a CHARACTER value to a message token (formatted).

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL MSG_FMTC( TOKEN, FORMAT, CVALUE )

*  Description:
*     A given CHARACTER value is encoded using the supplied Fortran 77
*     format field and the result assigned to the named message token.
*     If the token is already defined, the result is appended to the
*     existing token value. If this subroutine fails, the token remains
*     unmodified. This will be apparent in any messages which refer to
*     this token.

*  Arguments:
*     TOKEN = CHARACTER * ( * ) (Given)
*        The message token name.
*     FORMAT = CHARACTER * ( * ) (Given)
*        The Fortran 77 FORMAT field used to encode the supplied value.
*     CVALUE = CHARACTER * ( * ) (Given)
*        The value to be assigned to the message token.

*  Algorithm:
*     -  Convert the value into a string.
*     -  Use EMS_SETC to set the message token.

*  Implementation Details:
*     We allocate enough buffer space for both the input string itself and any format
*     statement so that the formatted WRITE will almost certainly work (using X in a
*     format can add many spaces). The input buffer is truncated at EMS__SZTOK+1 so
*     that it will be truncated by EMS_SETC using ellipsis. The output buffer is sized
*     at 2*EMS__SZTOK+1 to account for the format. This is not full proof but should
*     allow all but the most odd formatting (where the output string requires more than
*     double the amount of space used by the input string) to pass through the formatted
*     internal WRITE.

*  Copyright:
*     Copyright (C) 2009 Science and Technology Facilities Council.
*     Copyright (C) 1983, 1989, 1990 Science & Engineering Research Council.
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
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
*     JRG: Jack Giddings (UCL)
*     PCTR: P.C.T. Rees (STARLINK)
*     AJC: A.J.Chipperfield (STARLINK, RAL)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     3-JAN-1983 (JRG):
*        Original version.
*     13-SEP-1989 (PCTR):
*        Converted to new prologue and layout.
*     6-NOV-1989 (PCTR):
*        Further prologue changes.
*     13-DEC-1989 (PCTR):
*        EMS_ version adapted from MSG_FMTC.
*     19-MAR-1990 (PCTR):
*        Trap zero length tokens.
*     9-APR-1990 (PCTR):
*        Converted to Standard Fortran 77 CHARACTER concatenation.
*     15-FEB-2001 (AJC):
*        Renamed to EMS_FMTx to MSG_FMTx
*     10-FEB-2009 (TIMJ):
*        Formatted WRITE fails even if the supplied string has many trailing spaces
*        but the full string exceeds the buffer space. Ignore those spaces.
*        Since EMS_SETC does handle string truncation, allocate
*        2 * EMS__SZTOK for local storage and let EMS handle truncation.
*     11-FEB-2009 (TIMJ):
*        Handle truncation properly even if the supplied message is very very large.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE                     ! No implicit typing

*  Global Constants:
      INCLUDE 'EMS_PAR'                 ! EMS_ public constants

*  Arguments Given:
      CHARACTER * ( * ) TOKEN
      CHARACTER * ( * ) FORMAT
      CHARACTER * ( * ) CVALUE

*  External References:
      INTEGER CHR_LEN                   ! String length

*  Local Variables:
      INTEGER ALLOW                     ! Allowed length of FMT string
      INTEGER CLEN                      ! Length of CVALUE
      INTEGER FLEN                      ! Length of FORMAT string
      INTEGER IOSTAT                    ! Fortran I/O status
      INTEGER NCHAR                     ! Character count

      CHARACTER * ( EMS__SZTOK ) FMT    ! String to contain FORMAT
      CHARACTER * ( ( 2 * EMS__SZTOK ) + 1 ) STR ! String to contain value

*.

*  Find the length of the FMT string.
      FLEN = CHR_LEN( FORMAT )
      ALLOW = MIN( FLEN, EMS__SZTOK )

*  Abort if the FORMAT string is empty.
      IF ( ALLOW .GT. 0 ) THEN

*     Load FMT.
         FMT = FORMAT( 1 : ALLOW )

*     Find the length of the input string but cap it at the size of the output buffer
         CLEN = CHR_LEN( CVALUE )
         CLEN = MAX( 1, CLEN )
         CLEN = MIN( EMS__SZTOK + 1, CLEN )

*     Construct the message token string.
         WRITE ( STR, '( '//FMT//' )',
     :        IOSTAT = IOSTAT ) CVALUE( 1 : CLEN )

*     Check the Fortran I/O status.
         IF ( IOSTAT .EQ. 0 ) THEN
            NCHAR = CHR_LEN( STR )
            NCHAR = MAX( 1, NCHAR )
            CALL EMS_SETC( TOKEN, STR( 1 : NCHAR ) )
         END IF
      END IF

      END
