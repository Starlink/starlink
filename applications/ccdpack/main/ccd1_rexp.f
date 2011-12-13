      SUBROUTINE CCD1_REXP( EXPAND, SATUR, SETSAT, SATVAL, EXTSAT,
     :                      NSAT, DEFER, EXTDEF, STATUS )
*+
*  Name:
*     CCD1_REXP

*  Purpose:
*     To report the parameters used in the DEBIAS data expansion and
*     saturation section.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_REXP( EXPAND, SATUR, SETSAT, SATVAL, EXTSAT,
*                     NSAT, DEFER, EXTDEF, STATUS )

*  Description:
*     The routine writes out the values used in the data expansion,
*     saturation and deferred charge correction section. So:
*
*     Data EXPANSION, SATURATION and DEFERRED charge correction.
*     ----------------------------------------------------------
*
*     Data expanded to counts (electrons)
*     or
*     Data not expanded (output in ADUs)
*     Data has been corrected for any saturated pixels
*     or
*     Data has not been corrected for saturated pixels
*     Saturation value          :
*     Number of saturated pixels:
*     Saturated pixels set BAD
*     or
*     Saturated pixels set to saturation value
*     Deferred charge correction:
*
*     The output is echoed to the log file if requested. Values obtained
*     from NDF extensions are indicated with an "*".

*  Arguments:
*     EXPAND = LOGICAL (Given)
*        Set true if the data has been expanded to counts.
*     SATUR = LOGICAL (Given)
*        Set true if the data has been saturated.
*     SETSAT = LOGICAL (Given)
*        Set true if the data has been set to the saturation value.
*        False if the saturated data has been set BAD.
*     SATVAL = DOUBLE PRECISION (Given)
*        The saturation value.
*     EXTSAT = LOGICAL (Given)
*        Whether the saturation value has been obtained from an NDF
*        extension or not.
*     NSAT = INTEGER (Given)
*        The number of saturated pixels.
*     DEFER = DOUBLE PRECISION (Given)
*        The deferred charge value.
*     EXTDEF = LOGICAL (Given)
*        Whether the deferred charge has been obtained from an NDF
*        extension or not.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1991, 1994 Science & Engineering Research Council.
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
*        Added arguments for item obtained from NDF extensions.
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
      LOGICAL EXPAND
      LOGICAL SATUR
      LOGICAL SETSAT
      DOUBLE PRECISION SATVAL
      LOGICAL EXTSAT
      INTEGER NSAT
      DOUBLE PRECISION DEFER
      LOGICAL EXTDEF

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER BUFFER*( MSG__SZMSG ) ! Output line buffer
      INTEGER IAT                ! Pointer to string position

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Write out blank line.
      CALL CCD1_MSG( ' ', ' ' , STATUS )

*  Report the section name.
      IAT = 4
      BUFFER = ' '
      CALL CHR_PUTC(
     : 'Data EXPANSION, SATURATION and DEFERRED charge correction',
     :               BUFFER, IAT )
      CALL CCD1_MSG( ' ', BUFFER( :IAT ), STATUS )

*  Underline it.
      IAT = 4
      CALL CHR_PUTC(
     : '---------------------------------------------------------',
     :               BUFFER, IAT )
      CALL CCD1_MSG( ' ', BUFFER( :IAT ), STATUS )

*  Has the data been expanded to counts?
      IAT = 2
      BUFFER = ' '
      IF ( EXPAND ) THEN
          CALL CHR_PUTC( 'Data expanded to counts '//
     :    ' (output in electrons)', BUFFER, IAT )
      ELSE
          CALL CHR_PUTC( 'Data not expanded (output in ADUs)', BUFFER,
     :                   IAT )
      END IF
      CALL CCD1_MSG( ' ', BUFFER( :IAT ), STATUS )

*  Has the data been saturated ?
      IAT = 2
      BUFFER = ' '
      IF ( SATUR ) THEN
          CALL CHR_PUTC(
     :'Data has been corrected for any saturated pixels ', BUFFER, IAT )
         CALL CCD1_MSG( ' ', BUFFER( :IAT ), STATUS )

*  At what value ?
         CALL MSG_SETR( 'REPEXP_SATVAL', REAL( SATVAL ) )
         IF ( EXTSAT ) THEN
            CALL CCD1_MSG( ' ', '  Saturation value          :'//
     :                       ' ^REPEXP_SATVAL*' , STATUS )
         ELSE
            CALL CCD1_MSG( ' ', '  Saturation value          :'//
     :                       ' ^REPEXP_SATVAL' , STATUS )
         END IF

*  Number of saturated values.
         CALL MSG_SETI( 'REPEXP_NSAT', NSAT)
         CALL CCD1_MSG( ' ', '  Number of saturated pixels:'//
     :                 ' ^REPEXP_NSAT' , STATUS )

*  Where the saturated values set BAD or to the saturation value?
         IAT = 2
         BUFFER = ' '
         IF ( SETSAT ) THEN
            CALL CHR_PUTC( 'Saturated pixels set to saturation value.',
     :                      BUFFER, IAT )
         ELSE
            CALL CHR_PUTC( 'Saturated pixels set BAD', BUFFER, IAT )
         END IF
      ELSE

*  No saturation occurred.
          CALL CHR_PUTC(
     :'Data has not been corrected for any saturated pixels ', BUFFER,
     :IAT )
      END IF
      CALL CCD1_MSG( ' ', BUFFER( :IAT ), STATUS )

*  Write out the deferred charge value.
      CALL MSG_SETR( 'REPEXP_DEFER', REAL( DEFER ) )
      IF ( EXTDEF ) THEN
         CALL CCD1_MSG( ' ', '  Deferred charge value        :'//
     :                    ' ^REPEXP_DEFER*', STATUS )
      ELSE
         CALL CCD1_MSG( ' ', '  Deferred charge value        :'//
     :                    ' ^REPEXP_DEFER', STATUS )
      END IF

      END
* $Id$
