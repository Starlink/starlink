      SUBROUTINE CVG_SHOWHEADER( FUNIT, ALL, STATUS )
*+
*  Name:
*     CVG_SHOWHEADER

*  Purpose:
*     Displays all headers from one or all HDUs on standard output

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CVG_SHOWHEADER( FUNIT, ALL, STATUS )

*  Description:
*     This routine displays headers from the current HDU, or all HDSUs,
*     on standard output.

*  Arguments:
*     FUNIT = INTEGER (Given)
*        The FITSIO unit number for the FITS file.
*     ALL = LOGICAL (Given)
*        If .TRUE., list headers from all HDUs. Otherwise, list only the
*        current HDU.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*     Copyright (C) 2013 Science & Technology Facilities Council.
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
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     19-NOV-2013 (DSB):
*        Original version.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and functions
      INCLUDE 'CVG_PAR'          ! CVG constants

*  Arguments Given:
      INTEGER FUNIT
      LOGICAL ALL

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER HEADER*( CVG__HEDLEN )
      CHARACTER PATH*( CVG__MXPTH )
      INTEGER FIRST
      INTEGER FSTAT
      INTEGER HDUTYPE
      INTEGER IHDU
      INTEGER IHDU0
      INTEGER IHEAD
      INTEGER KEYADD
      INTEGER LAST
      INTEGER NHDU
      INTEGER NHEAD

*.

*  Check the inherited global status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Initialise the FITSIO status.  It's not the same as the Starlink
*  status, which is reset by the fixed part.
      FSTAT = CVG__FITSOK

*  Display the name of the file attached to the logical unit.
      CALL FTFLNM( FUNIT, PATH, FSTAT )
      CALL MSG_SETC( 'P', PATH )
      CALL MSG_BLANK( STATUS )
      CALL MSG_OUT( ' ', '>> Displaying headers for FITS file ^P:',
     :              STATUS )

*  Record the index of the current HDU.
      CALL FTGHDN( FUNIT, IHDU0 )

*  Choose the first and last HDU to display.
      IF( ALL ) THEN
         FIRST = 1
         CALL FTTHDU( FUNIT, LAST, STATUS )
      ELSE
         FIRST = IHDU0
         LAST = IHDU0
      END IF

*  Loop round all required HDUs.
      DO IHDU = FIRST, LAST

*  Select the next HDU.
         CALL FTMAHD( FUNIT, IHDU, HDUTYPE, STATUS )

*  Describe the header that is about to be displayed.
         CALL MSG_BLANK( STATUS )
         CALL MSG_SETI( 'I', IHDU )
         IF( HDUTYPE .EQ. 0 )  THEN
            IF( IHDU .EQ. 1 ) THEN
               CALL MSG_SETC( 'T', 'primary' )
            ELSE
               CALL MSG_SETC( 'T', 'image' )
            END IF
         ELSE IF( HDUTYPE .EQ. 1 )  THEN
            CALL MSG_SETC( 'T', 'ASCII table' )
         ELSE IF( HDUTYPE .EQ. 2 )  THEN
            CALL MSG_SETC( 'T', 'binary table' )
         ELSE
            CALL MSG_SETC( 'T', 'unknown' )
         END IF

         IF( IHDU .EQ. IHDU0 ) THEN
            CALL MSG_OUT( ' ', '>> Header for ^T HDU ^I (the current '//
     :                    'HDU):',STATUS )
         ELSE
            CALL MSG_OUT( ' ', '>> Header for ^T HDU ^I:',STATUS )
         END IF
         CALL MSG_BLANK( STATUS )

*  Find the number of headers (not including the final END).
         CALL FTGHSP( FUNIT, NHEAD, KEYADD, FSTAT )
         IF( FSTAT .NE. CVG__FITSOK ) THEN
            CALL CVG_FIOER( FSTAT, 'CVG_SHOWHEADER_NHEAD', 'FTGHSP',
     :                      'Error obtaining the number of header '//
     :                      'Cards.', STATUS )
            GO TO 999
         END IF

*  Loop through the headers.
         DO IHEAD = 1, NHEAD

*  Obtain the header. If an error occurred getting the header, flush
*  the FITSIO error stack, but carry on to process any remaining headers.
            HEADER = ' '
            CALL FTGREC( FUNIT, IHEAD, HEADER, FSTAT )
            IF( FSTAT .NE. CVG__FITSOK ) THEN
               FSTAT = CVG__FITSOK
               CALL FTCMSG

*  Display this header.
            ELSE
               CALL MSG_OUT( "", HEADER, STATUS )
            END IF

         END DO

      END DO

 999  CONTINUE

*  Re-instate the original HDU.
      CALL FTMAHD( FUNIT, IHDU0, HDUTYPE, STATUS )

*  Report an error if anything went wrong in a FITSIO routine.
      IF( FSTAT .GT. CVG__FITSOK .AND. STATUS .EQ. SAI__OK ) THEN
         CALL CVG_FIOER( FSTAT, ' ', ' ', 'Failed to display HDU '//
     :                   'headers.', STATUS )

      ELSE IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( ' ', 'Failed to display HDU headers.', STATUS )

      END IF

      END
