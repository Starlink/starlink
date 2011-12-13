      SUBROUTINE CCD1_RMSK( GOTMSK, MSKNAM, NOQUAL, STATUS )
*+
*  Name:
*     CCD1_RMSK

*  Purpose:
*     To write out the name and section heading for MASKING section.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_RMSK( GOTMSK, MSKNAM, NOQUAL, STATUS )

*  Description:
*     If gotmsk is true this routines writes out the masking section
*     header for the routine calling routine, which consists of
*             Data MASKING
*             ------------
*             Mask file: MSKNAM.............
*             Used quality or Used BAD values
*     Otherwise it does nothing. If echo is set true then any output is
*     written to the log file.

*  Arguments:
*     GOTMSK = LOGICAL (Given)
*        If true then the mask file has been given.
*     MSKNAM = CHARACTER * ( * ) (Given)
*        The name of the mask file and a type description, probably
*        returned from CCD1_GTMSK.
*     NOQUAL = LOGICAL (Given)
*        If true then quality has not been used.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council. All
*     Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     11-JUN-1991 (PDRAPER):
*        Original version.
*     21-JUN-1991 (PDRAPER):
*        Changed to use log system
*     29-OCT-1991 (PDRAPER):
*        Added mask file string - updated system to use ARD.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'MSG_PAR'          ! Defines MSG__SZMSG - size of output
                                 ! string

*  Arguments Given:
      LOGICAL GOTMSK
      LOGICAL NOQUAL
      CHARACTER * ( * ) MSKNAM

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER BUFFER*( MSG__SZMSG ) ! Output line buffer
      INTEGER IAT                ! Pointer to string position

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the mask file has been given then write out the MSKNAM string.
      IF ( GOTMSK ) THEN
         CALL CCD1_MSG( ' ', ' ' , STATUS )

*  Report the section name.
         IAT = 4
         BUFFER = ' '
         CALL CHR_PUTC( 'Bad data MASKING', BUFFER,
     :                   IAT )
         CALL CCD1_MSG( ' ', BUFFER( :IAT ), STATUS )

*  Underline it.
         IAT = 4
         CALL CHR_PUTC( '----------------', BUFFER, IAT )
         CALL CCD1_MSG( ' ', BUFFER( :IAT ), STATUS )

*  Write out mask file name.
         CALL MSG_SETC( 'MSKNAM', MSKNAM )
         CALL CCD1_MSG( ' ', '  Mask file: ^MSKNAM', STATUS )

*  Report which data has been used for the BAD value information.
         IAT = 2
         IF ( NOQUAL ) THEN
            CALL CHR_PUTC( 'Used BAD values', BUFFER, IAT )
         ELSE
            CALL CHR_PUTC( 'Used QUALITY', BUFFER, IAT )
         END IF
         CALL CCD1_MSG( ' ', BUFFER( :IAT ), STATUS )
      END IF

      END
* $Id$
