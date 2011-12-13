       SUBROUTINE CCD1_REND( LBND, UBND, LBND2, UBND2, EXTSEC,
     :                      DTYPE, IDOUT, FORI, STATUS )
*+
*  Name:
*     CCD1_REND

*  Purpose:
*     To report the final parameters as used by DEBIAS.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_REND( LBND, UBND, LBND2, UBND2, EXTSEC,
*                     DTYPE, IDOUT, FORI, STATUS )

*  Description:
*     The routine reports the extent of the input NDF which is
*     propagated to the output NDF. The actual output data type and
*     the name of the out NDF. The output is logged to current log file
*     if it has been requested.

*  Arguments:
*     LBND( 2 ) = INTEGER (Given)
*        The lower bounds of the input NDF.
*     UBND( 2 ) = INTEGER (Given)
*        The upper bounds of the input NDF.
*     LBND2( 2 ) = INTEGER (Given)
*        The lower bounds of the output NDF.
*     UBND2( 2 ) = INTEGER (Given)
*        The upper bounds of the output NDF.
*     EXTSEC = LOGICAL (Given)
*        Whether or not the output NDF section was obtained from an NDF
*        extension.
*     DTYPE = CHARACTER * ( * ) (Given)
*        The NDF type of the output NDF.
*     IDOUT = INTEGER (Given)
*        Identifier of the output NDF.
*     FORI = LOGICAL (Given)
*        Whether or not the output NDF origin has been fixed to 1,1.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1991, 1994 Science & Engineering Research Council.
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
*     All Rights Reserved.

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
*     12-JUN-1991 (PDRAPER):
*        Original version.
*     21-JUN-1991 (PDRAPER):
*        Changed to use new log file system.
*     18-JAN-1994 (PDRAPER):
*        Added EXTSEC argument.
*     21-OCT-1995 (PDRAPER):
*        Added FORI argument & message.
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
      INTEGER LBND( 2 )
      INTEGER UBND( 2 )
      INTEGER LBND2( 2 )
      INTEGER UBND2( 2 )
      LOGICAL EXTSEC
      CHARACTER DTYPE * ( * )
      INTEGER IDOUT
      LOGICAL FORI

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER BUFFER*( MSG__SZMSG ) ! Output line buffer
      INTEGER IAT                ! Pointer to string position

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Write out blank line.
      CALL CCD1_MSG( ' ', ' ', STATUS )

*  Write out section introduction.
      IAT = 4
      BUFFER = ' '
      CALL CHR_PUTC(
     : 'Output NAME, data TYPE and section EXTENT',
     :               BUFFER, IAT )
      CALL CCD1_MSG( ' ', BUFFER( :IAT ), STATUS )

*  Underline it.
      IAT = 4
      CALL CHR_PUTC(
     : '-----------------------------------------',
     :               BUFFER, IAT )
      CALL CCD1_MSG( ' ', BUFFER( :IAT ), STATUS )

*  Capture the output NDF name and write it out.
      BUFFER = ' '
      CALL NDF_MSG( 'REND_NDF', IDOUT )
      CALL CCD1_MSG( ' ', '  Output NDF: ^REND_NDF', STATUS )

*  Write out the extents of the input NDF.
      CALL MSG_SETI( 'REND_B1', LBND( 1 ) )
      CALL MSG_SETI( 'REND_B2', UBND( 1 ) )
      CALL MSG_SETI( 'REND_B3', LBND( 2 ) )
      CALL MSG_SETI( 'REND_B4', UBND( 2 ) )
      CALL CCD1_MSG( ' ','  Input NDF extent  (xmin:xmax,ymin:ymax):'//
     :         ' (^REND_B1:^REND_B2,^REND_B3:^REND_B4)',
     :         STATUS )

*  And for the output NDF, correcting if the origin is fixed?
      IF ( FORI ) THEN
         CALL MSG_SETI( 'REND_B1', 1 )
         CALL MSG_SETI( 'REND_B2', UBND2( 1 ) - LBND2( 1 ) + 1 )
         CALL MSG_SETI( 'REND_B3', 1 )
         CALL MSG_SETI( 'REND_B4', UBND2( 2 ) - LBND2( 2 ) + 1 )
      ELSE
         CALL MSG_SETI( 'REND_B1', LBND2( 1 ) )
         CALL MSG_SETI( 'REND_B2', UBND2( 1 ) )
         CALL MSG_SETI( 'REND_B3', LBND2( 2 ) )
         CALL MSG_SETI( 'REND_B4', UBND2( 2 ) )
      END IF
      IF ( EXTSEC ) THEN
         CALL CCD1_MSG( ' ','  Output NDF extent (xmin:xmax,ymin:ymax):'
     :         //' (^REND_B1:^REND_B2,^REND_B3:^REND_B4)*',STATUS )
      ELSE
         CALL CCD1_MSG( ' ','  Output NDF extent (xmin:xmax,ymin:ymax):'
     :         //' (^REND_B1:^REND_B2,^REND_B3:^REND_B4)',STATUS )
      END IF

*  Write out the final data type.
      CALL MSG_SETC( 'REND_TYPE', DTYPE )
      CALL CCD1_MSG( ' ','  Output data type                       :'//
     :                    ' ^REND_TYPE', STATUS )



      END
* $Id$
