      SUBROUTINE MSG_FMTL( TOKEN, FORMAT, LVALUE )
*+
*  Name:
*     MSG_FMTL

*  Purpose:
*    Assign a LOGICAL value to a message token (formatted).

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL MSG_FMTL( TOKEN, FORMAT, LVALUE )

*  Description:
*     A given LOGICAL value is encoded using the supplied Fortran 77
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
*     LVALUE = LOGICAL (Given)
*        The value to be assigned to the message token.

*  Algorithm:
*     -  Convert the value into a string.
*     -  Use EMS_SETC to set the message token.

*  Copyright:
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
*     {enter_new_authors_here}

*  History:
*     3-JAN-1983 (JRG):
*        Original version.
*     13-SEP-1989 (PCTR):
*        Converted to new prologue and layout.
*     13-DEC-1989 (PCTR):
*        EMS_ version adapted from MSG_FMTL.
*     9-APR-1990 (PCTR):
*        Converted to Standard Fortran 77 CHARACTER concatenation.
*     15-FEB-2001 (AJC):
*        Renamed to EMS_FMTx to MSG_FMTx
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

      LOGICAL LVALUE

*  External References:
      INTEGER CHR_LEN                   ! String length

*  Local Variables:
      INTEGER ALLOW                     ! Allowed length of FMT string
      INTEGER FLEN                      ! Length of FORMAT string
      INTEGER IOSTAT                    ! Fortran I/O status
      INTEGER NCHAR                     ! Character count

      CHARACTER * ( EMS__SZTOK ) FMT    ! String to contain FORMAT
      CHARACTER * ( EMS__SZTOK ) STR    ! String to contain value

*.

*  Find the length of the FMT string.
      FLEN = CHR_LEN( FORMAT )
      ALLOW = MIN( FLEN, EMS__SZTOK )

*  Abort if the FORMAT string is empty.
      IF ( ALLOW .GT. 0 ) THEN

*     Load FMT.
         FMT = FORMAT( 1 : ALLOW )

*     Construct the message token string.
         WRITE ( STR, '( '//FMT//' )', IOSTAT = IOSTAT ) LVALUE

*     Check the Fortran I/O status.
         IF ( IOSTAT .EQ. 0 ) THEN
            NCHAR = CHR_LEN( STR )
            CALL EMS_SETC( TOKEN, STR( 1 : NCHAR ) )
         END IF
      END IF

      END
