      SUBROUTINE FTS1_GPARM( NCARD, HEADER, PCOUNT, PARAMS, PTYPE,
     :                       PSCALE, PZERO, BAD, BLANK, LOGHDR, FD,
     :                       STATUS )
*+
*  Name:
*     FTS1_GPARM

*  Purpose:
*     Adds the group parameters to the FITS header records.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FTS1_GPARM( NCARD, HEADER, PCOUNT, PARAMS, PTYPE, PSCALE,
*    :                 PZERO, BAD, BLANK, LOGHDR, FD, STATUS )

*  Description:
*     This is a server routine for FITSIN.  It packages up the
*     operations required to add the group parameters to the HDS
*     structure that contains the FITS header cards.  It also writes
*     the group parameters to a log file is required.

*     Each group parameter is evaluated in double precision from its
*     scale and offset, and with the parameter name a pseudo FITS
*     card is generated.  In the header structure the last card---has
*     the END keyword---is overwritten. Once all the group parameters
*     have been copied into the header structure, an END card is placed
*     after them to preserve a valid FITS header section.

*  Arguments:
*     NCARD = INTEGER (Given)
*        The number of header 80-character cards in the original FITS
*        header.  It excludes the number of group parameters.
*     HEADER( NCARD + PCOUNT ) = CHARACTER * 80 (Given)
*        The FITS headers in 80-character cards.  The aditional PCOUNT
*        elements are for the group parameters.
*     PCOUNT = INTEGER (Given)
*        The number of group parameters in each group.
*     PARAMS( PCOUNT ) = INTEGER (Given)
*        The values of the group parameters in the header, i.e. before
*        any scale and offset have been applied.
*     PTYPE( PCOUNT ) = CHARACTER * ( * ) (Given)
*        The type (descriptive name) of each group parameter.
*     PSCALE( PCOUNT ) = DOUBLE PRECISION (Given)
*        The scale factors of the group parameters so that the
*        parameters may be converted to the true floating-point values.
*     PZERO( PCOUNT ) = DOUBLE PRECISION (Given)
*        The offsets of the group parameters so that the parameters may
*        be converted to the true floating-point values.
*     BAD = LOGICAL (Given)
*        If true, testing and replacement of undefined parameters is to
*        occur.  A blank value, as specified by %BLANK, is replaced
*        by the standard magic value.  If false, the value of %BLANK
*        is ignored.
*     BLANK = INTEGER (Given)
*        Value of an undefined parameter.
*     LOGHDR = LOGICAL (Given)
*        If true the evaluated group parameters written in FITS-card
*        format will be written to the log file.
*     FD = INTEGER (Given)
*        The file descriptor for the log file.  It is ignored if %LOGHDR
*        is false.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1990 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This programme is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This programme is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE.  See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this programme; if not, write to the Free Software
*     Foundation, Inc., 51, Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1990 November 18 (MJC):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_ERR'          ! Parameter-system errors
      INCLUDE 'PRM_PAR'          ! Bad-value constants

*  Arguments Given:
      INTEGER
     :  NCARD,
     :  PCOUNT,
     :  BLANK,
     :  FD,
     :  PARAMS( PCOUNT )

      CHARACTER * ( * )
     :  HEADER( NCARD + PCOUNT ) * 80,
     :  PTYPE( PCOUNT )

      DOUBLE PRECISION
     :  PSCALE( PCOUNT ),
     :  PZERO( PCOUNT )

      LOGICAL
     :  BAD,
     :  LOGHDR

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER
     :  I                        ! Loop counter

      DOUBLE PRECISION
     :  TPARAM                   ! True parameter value

      CHARACTER * 80
     :  PAVAL * 20               ! I20 format of a parameter value
*.

*    Check inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Loop for each group parameter.

      DO  I = 1, PCOUNT

*       See if the parameter is undefined.

         IF ( BAD .AND. PARAMS( I ) .EQ. BLANK ) THEN
            TPARAM = VAL__BADD
         ELSE

*          Apply the scale and offset to get the true parameter value.

            TPARAM = DBLE( PARAMS( I ) ) * PSCALE( I ) + PZERO( I )
         END IF
         WRITE( PAVAL, '(G20.12)' ) TPARAM

*       Create the Pseudo-FITS cards for the parameters.

         HEADER( NCARD + I - 1 ) = PTYPE( I )( :8 )//'= '//PAVAL

         IF ( LOGHDR ) THEN

*          Write remainder of the 'header', i.e. the parameters, to the
*          logfile if required.

            CALL FIO_WRITE( FD, HEADER( NCARD + I - 1 ), STATUS )
         END IF

*       Report the error context.

         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'FTS1_GPARM_WLOGP',
     :        'Error writing parameter cards to the file $LOGFILE',
     :        STATUS )
            GOTO 999
         END IF

      END DO

*    Move the end card to last element in the header array.

      HEADER( NCARD + PCOUNT ) = 'END'


  999 CONTINUE

      END
