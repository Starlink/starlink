      SUBROUTINE ATL_MOCFT( MOC, FILE, STATUS )
*+
*  Name:
*     ATL_MOCFT

*  Purpose:
*     Store an AST Moc as a binary table in a FITS file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ATL_MOCFT( MOC, FILE, STATUS )

*  Arguments:
*     MOC = INTEGER (Given)
*        The AST identifier for the MOC
*     FILE = CHARACTER * ( * ) (Given)
*        The path to the FITS file to create. Any existing file with the
*        same name is first deleted.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2019 East Asian Observatory
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
*     DSB: David Berry (EAO)
*     {enter_new_authors_here}

*  History:
*     14-MAY-2019 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants
      INCLUDE 'CNF_PAR'          ! CNF functions etc

*  Arguments Given:
      INTEGER MOC
      CHARACTER FILE*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER CARD*80
      CHARACTER ERRTEXT*30
      CHARACTER ERRMESS*80
      CHARACTER FPARAM*100
      INTEGER FC
      INTEGER FSTAT
      INTEGER IP
      INTEGER LN
      INTEGER NB
      INTEGER UNIT
      LOGICAL DONE
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the length and width of the MOC and reserve space to hold the data.
      NB = AST_GETI( MOC, 'MOCTYPE', STATUS )
      LN = AST_GETI( MOC, 'MOCLENGTH', STATUS )
      CALL PSX_CALLOC( NB*LN, '_BYTE', IP, STATUS )

* Get the data to be put in the UNIQ column of the binary table.
      CALL AST_GETMOCDATA( MOC, NB*LN, %VAL( CNF_PVAL( IP ) ), STATUS )

*  Get the FitsChan holding the required headers for the binary table.
      FC = AST_GETMOCHEADER( MOC, STATUS )
      IF( STATUS .EQ. SAI__OK ) THEN

*  Use FITSIO to create the FITS file and put the data into a binary table
*  extension. Delete any pre-existing file with the same name.
         FSTAT = 0
         CALL FTGIOU( UNIT, FSTAT )
         OPEN( UNIT=UNIT, IOSTAT=FSTAT, FILE=FILE, STATUS='OLD' )
         IF( FSTAT == 0 ) CLOSE( UNIT, STATUS='DELETE' )

         FSTAT = 0
         CALL FTGIOU( UNIT, FSTAT )
         CALL FTINIT( UNIT, FILE, 1, FSTAT )

         CALL FTIKYL( UNIT, 'simple', .true.,  ' ', FSTAT )
         CALL FTIKYJ( UNIT, 'bitpix', 8,  ' ', FSTAT )
         CALL FTIKYJ( UNIT, 'naxis', 0,  ' ', FSTAT )
         CALL FTIKYL( UNIT, 'extend', .true.,  ' ', FSTAT )

         CALL FTCRHD( UNIT, FSTAT )

         CALL AST_CLEAR( FC, 'Card', STATUS )
         DO WHILE( AST_FINDFITS( FC, '%f', CARD, .TRUE., STATUS) )
            CALL FTPREC( UNIT, CARD, FSTAT )
         END DO

         IF( NB .EQ. 4 ) THEN
            CALL FTPCLJ( UNIT, 1, 1, 1, LN,
     :                   %VAL( CNF_PVAL( IP ) ), FSTAT )
         ELSE
            CALL FTPCLK( UNIT, 1, 1, 1, LN,
     :                   %VAL( CNF_PVAL( IP ) ), FSTAT )
         END IF
         CALL FTCLOS( UNIT, FSTAT )
         CALL FTFIOU( UNIT, FSTAT )

         IF( FSTAT .GT. 0 ) THEN
            STATUS = SAI__ERROR
            CALL FTGERR( FSTAT, ERRTEXT )
            CALL MSG_SETC( 'T', ERRTEXT )
            CALL ERR_REP( ' ', 'FITSIO error: ^T', STATUS )
            CALL FTGMSG( ERRMESS )
            DO WHILE( ERRMESS .NE. ' ' )
               CALL MSG_SETC( 'T', ERRMESS )
               CALL ERR_REP( ' ', '^T', STATUS )
               CALL FTGMSG(ERRMESS)
            END DO
         ELSE
            DONE = .TRUE.
         END IF
      END IF

*  Free resources.
      CALL AST_ANNUL( FC, STATUS )
      CALL PSX_FREE( IP, STATUS )

      END
