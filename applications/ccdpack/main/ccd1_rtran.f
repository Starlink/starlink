      SUBROUTINE CCD1_RTRAN( TRTYPE, INEXT, TR, XMAP, YMAP, LOCTR,
     :                       FORWRD, FR1PAR, FR2PAR, IWCSF, STATUS )
*+
*  Name:
*     CCD1_RTRAN

*  Purpose:
*     Reports the various parameters as used by TRANLIST

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_RTRAN( TRTYPE, INEXT, TR, XMAP, YMAP, LOCTR, FORWRD,
*                      FR1PAR, FR2PAR, IWCSF, STATUS )

*  Description:
*     This routine writes a report about the transformation options
*     used by TRANLIST.

*  Arguments:
*     TRTYPE = CHARACTER * ( * ) (Given)
*        The type of transformation information supplied by the user.
*        One of COEFF, EXPRES or STRUCT.
*     INEXT = LOGICAL (Given)
*        Whether the TRANSFORM structures are to be found in the
*        NDF extensions.
*     TR( 6 ) = DOUBLE PRECISION (Given)
*        If TRTYPE = 'COEFF' then these values are the six linear
*        transformation coefficients.
*     XMAP = CHARACTER * ( * ) (Given)
*        The X transformation expression used if TRTYPE = 'EXPRES'.
*     XMAP = CHARACTER * ( * ) (Given)
*        The Y transformation expression used if TRTYPE = 'EXPRES'.
*     LOCTR = CHARACTER * ( * ) (Given)
*        Locator to the transform structure used if TRTYPE = 'STRUCT'.
*     FORWRD = LOGICAL (Given)
*        If true then the forward transformation was used. Otherwise the
*        inverse transformation.
*     FR1PAR = CHARACTER * ( * ) (Given)
*        Name of an ADAM parameter supplying the name of the source frame
*        if TRTYPE is WCS.
*     FR2PAR = CHARACTER * ( * ) (Given)
*        Name of an ADAM parameter supplying the name of the destination
*        frame if TRTYPE is WCS.
*     IWCSF = INTEGER (Given)
*        NDF identifier for NDF containing WCS information if TRTYPE is
*        WCS and INEXT is false.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1992-1993 Science & Engineering Research Council.
*     Copyright (C) 1999 Central Laboratory of the Research Councils.
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
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     9-OCT-1992 (PDRAPER):
*        Original version.
*     8-FEB-1993 (PDRAPER):
*        Changed to not report file names, just transformation
*        information.
*     14-JUN-1993 (PDRAPER):
*        Added INEXT parameter + associated changes.
*     1-APR-1999 (MBT):
*        Added WCS mode and farmed out TR output to CCD1_TROUT.
*     1-NOV-1999 (MBT):
*        Changed calling sequence for CCD1_TROUT.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'MSG_PAR'          ! Message system parameters
      INCLUDE 'PRM_PAR'          ! PRIMDAT constants
      INCLUDE 'PAR_ERR'          ! PAR system error constants

*  Arguments Given:
      CHARACTER * ( * ) TRTYPE
      LOGICAL INEXT
      DOUBLE PRECISION TR( 6 )
      CHARACTER * ( * ) XMAP
      CHARACTER * ( * ) YMAP
      CHARACTER * ( * ) LOCTR
      LOGICAL FORWRD
      CHARACTER * ( * ) FR1PAR
      CHARACTER * ( * ) FR2PAR
      INTEGER IWCSF

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( MSG__SZMSG ) FR1 ! Value of source frame parameter
      CHARACTER * ( MSG__SZMSG ) FR2 ! Value of destination frame parameter

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start with a blank.
      CALL CCD1_MSG( ' ', ' ', STATUS )

      IF ( TRTYPE .EQ. 'COEFF' ) THEN

*  Transformation given as a series of linear coefficients.
         CALL CCD1_MSG( ' ',
     : '  Transformation defined by a series of linear coefficients:',
     :   STATUS )

*  Output coefficients.
         CALL CCD1_TROUT( TR, 0, .FALSE., STATUS )

*  Transformation given as a expression.
      ELSE IF ( TRTYPE .EQ. 'EXPRES' ) THEN
         CALL CCD1_MSG( ' ',
     : '  Transformation defined by the expressions:', STATUS )
         CALL MSG_SETC( 'XMAP', XMAP )
         CALL CCD1_MSG( ' ',
     : '    ^XMAP', STATUS )
         CALL MSG_SETC( 'YMAP', YMAP )
         CALL CCD1_MSG( ' ',
     : '    ^YMAP', STATUS )

      ELSE IF ( TRTYPE .EQ. 'WCS' ) THEN

*  Transformation derived from a WCS component of an NDF.  This is
*  either the WCS component of each NDF or from one which serves all.
*  Print frame identifiers.
         CALL PAR_GET0C( FR1PAR, FR1, STATUS )
         CALL CHR_UCASE( FR1 )
         CALL CHR_RMBLK( FR1 )
         IF ( STATUS .NE. SAI__OK ) GO TO 99
         CALL PAR_GET0C( FR2PAR, FR2, STATUS )
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            CALL MSG_SETC( 'FR2', 'Current frame' )
         ELSE
            CALL CHR_UCASE( FR2 )
            CALL CHR_RMBLK( FR2 )
            CALL MSG_SETC( 'FR2', 'frame ' // FR2 )
         END IF
         CALL MSG_SETC( 'FR1', 'frame ' // FR1 )
         CALL CCD1_MSG( ' ',
     : '  Transformation is the mapping from ^FR1 to ^FR2', STATUS )

*  Print name of single WCS containing file or alternative as appropriate.
         IF ( INEXT ) THEN
            CALL CCD1_MSG( ' ',
     : '  in the WCS component of each NDF.', STATUS )
         ELSE
            CALL NDF_MSG( 'WCSFIL', IWCSF )
            CALL CCD1_MSG( ' ',
     : '  in the WCS component of NDF ^WCSFIL.', STATUS )
         END IF

      ELSE IF ( TRTYPE .EQ. 'STRUCT' ) THEN

*  Transformation given as a TRN_TRANSFORM structure. This is either in
*  the NDF extension or directly.
         IF ( INEXT ) THEN
            IF ( FORWRD ) THEN
               CALL CCD1_MSG( ' ',
     : '  Transformation defined by forward mapping in NDF extensions',
     :                        STATUS )
            ELSE
               CALL CCD1_MSG( ' ',
     : '  Transformation defined by inverse mapping in NDF extensions',
     :                        STATUS )
            END IF
         ELSE
            IF ( FORWRD ) THEN
               CALL CCD1_MSG( ' ',
     : '  Transformation defined by forward mapping in:', STATUS )
            ELSE
               CALL CCD1_MSG( ' ',
     : '  Transformation defined by inverse mapping in:', STATUS )
            END IF

*  Get the name of the object which has the TRN_TRANSFORM structure.
            CALL DAT_MSG( 'OBJ', LOCTR )
            CALL CCD1_MSG( ' ', '    ^OBJ', STATUS )
         END IF
      END IF

*  Error status exit.
 99   CONTINUE
      END
* $Id$
