      SUBROUTINE SUBPAR_VWHLP ( TOPIC, LIBRARY, FLAG, STATUS)
*+
*  Name:
*     SUBPAR_VWHLP

*  Purpose:
*     output help.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_VWHLP (TOPIC, LIBRARY, FLAG, STATUS)

*  Description:
*     This is the Unix version of the routine which, for VMS, outputs
*     help information to the terminal using the VMS help system.
*     The routine should never be called. If it is, it indicates either
*     the file extension was not .shl, .hlb or blank, or that a .shl
*     file did not exist.

*  Arguments:
*     LIBRARY=CHARACTER*(*) (given)
*        the VMS help library to be accessed
*     TOPIC  =CHARACTER*(*) (given)
*        the topic within the library on which help is sought
*     FLAG   =INTEGER       (given)
*        non-zero if help library search is required
*     STATUS =INTEGER       (returned)
*        the return status

*  Algorithm:
*     Report an error

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

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council.
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Authors:
*     AJC: A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     07-MAY-1992 (AJC):
*        Original version
*     11-JUN-1992 (AJC):
*        Use status SUBPAR__HLPER
*     28-JUN-1995 (AJC):
*        Correct illegal concatenation of character argument
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'SUBPAR_ERR'

*  Arguments Given:
      CHARACTER*(*)  TOPIC,LIBRARY
      INTEGER FLAG

*  Status:
      INTEGER        STATUS

*  External References:
      INTEGER STRING_IANYR     ! Search for character backwards
      EXTERNAL STRING_IANYR

*  Local Variables:
      INTEGER LIBLEN           ! Length of library name
      INTEGER STNM             ! index to start of file name
      INTEGER ENDNM            ! index to end of file name

*  Find library file extension
      LIBLEN = LEN( LIBRARY )
      STNM = STRING_IANYR( LIBRARY, '/' ) + 1
      ENDNM = STRING_IANYR( LIBRARY(STNM:), '.') - 1
      IF ( ENDNM .LE. 0 ) THEN
         ENDNM = LIBLEN
      ELSE
         ENDNM = STNM + ENDNM - 1
      ENDIF

      STATUS = SUBPAR__HLPER
      IF ( ( ENDNM .EQ. LIBLEN )
     :.OR. ( LIBRARY(ENDNM+1:) .EQ. '.hlb' ) ) THEN
*     The .shl file is missing
         CALL EMS_SETC( 'LIB', LIBRARY(1:ENDNM) )
         CALL EMS_SETC( 'LIB', '.shl' )
         CALL EMS_REP( 'SUP_VWHLP1',
     :   'SUBPAR: Help library ^LIB - not found', STATUS )

      ELSE
*     The file spec is wrong
         CALL EMS_SETC( 'LIB', LIBRARY )
         CALL EMS_REP( 'SUP_VWHLP2',
     :   'SUBPAR: Illegal help library name ^LIB', STATUS )
      ENDIF

      END
