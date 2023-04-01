      SUBROUTINE CVG_OPEN( PATH, MODE, FUNIT, BLOCKF, STATUS )
*+
*  Name:
*     CVG_OPEN

*  Purpose:
*     Opens an existing FITS file for read or update access.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CVG_OPEN( PATH, MODE, FUNIT, BLOCKF, STATUS )

*  Description:
*     This function opens an existing FITS file with a given path,
*     and returns a logical unit number that can be used to access it
*     using CVG and FITSIO functions.

*  Arguments:
*     PATH = CHARACTER * ( * ) (Given)
*        The path to the file to be created. A file type of ".fit" will be
*        added if there is no file type in the supplied string.
*     MODE = CHARACTER * ( * ) (Given)
*        The access mode: 'READ' or 'UPDATE. Case insensitive.
*        Abbreviations can be used.
*     FUNIT = INTEGER (Returned)
*        The logical unit number of the FITS file. Returned equal to
*        CVG_NOLUN if an error occurs.
*     BLOCKF = INTEGER (Returned)
*        The logical record blocking factor.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2013 Science & Technology Facilities Council. All
*     Rights Reserved.

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
*     DSB: David S. Berry (JAC, Hawaii)
*     {enter_newauthors_here}

*  History:
*     14-NOV-2013 (DSB):
*        Original version, based on code from COF_FTOPR.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CVG_PAR'          ! CVG constants

*  Arguments Given:
      CHARACTER PATH*(*)
      CHARACTER MODE*(*)

*  Arguments Returned:
      INTEGER FUNIT
      INTEGER BLOCKF

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Effective string length

*  Local Variables:
      CHARACTER EXTRA*( CVG__MXPTH )
      CHARACTER LMODE*20
      CHARACTER LPATH*( CVG__MXPTH )
      INTEGER FSTAT
      INTEGER IAT
      INTEGER LNROOT
      INTEGER NCF
      INTEGER RWMODE
*.

*  Initialise the returned logical unit number.
      FUNIT = CVG__NOLUN

*  Check inherited status
      IF( STATUS .NE. SAI__OK ) RETURN

*  Initialise the FITSIO status.
      FSTAT = CVG__FITSOK
      LPATH = ' '

*  Get the length of the filename.
      NCF = CHR_LEN( PATH )
      IF( NCF .EQ. 0 .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'CVG_OPEN: Zero-length path supplied for '//
     :                'new FITS file.', STATUS )
      END IF

*  Get the path excluding any HDU name or number, or any filtering
*  specifications.
      CALL FTRTNM( PATH, LPATH, FSTAT )

*  Extract any HDU name or number, or any filtering specifications into
*  another variable.
      LNROOT = CHR_LEN( LPATH )
      IF( LNROOT .LT. NCF ) THEN
         EXTRA = PATH( LNROOT + 1 : NCF )
      ELSE
         EXTRA = ' '
      END IF

*  If the section of the root following the final "/" contains no dot,
*  append ".FIT" to the root, and then append any any HDU name or number,
*  or any filtering specifications.
      CALL CHR_LASTO( LPATH, '/', IAT )
      CALL CHR_LASTO( LPATH( IAT + 1: ), '.', IAT )
      IF( IAT .EQ. 0 ) THEN
         CALL CHR_APPND( '.fit', LPATH, LNROOT )
         IF( EXTRA .NE. ' ' ) CALL CHR_APPND( EXTRA, LPATH, LNROOT )
         NCF = LNROOT

*  Otherwise just use the supplied path.
      ELSE
         LPATH = PATH
      END IF

*  Check the access mode and get the corresponding FITSIO access code.
      LMODE = MODE
      CALL CHR_UCASE( LMODE )
      CALL CHR_RMBLK( LMODE )
      IF( INDEX( LMODE, 'READ' ) .EQ. 1 ) THEN
         RWMODE = 0

      ELSE IF( INDEX( LMODE, 'UPDATE' ) .EQ. 1 ) THEN
         RWMODE = 1

      ELSE IF( STATUS .EQ. SAI__OK ) THEN
         CALL MSG_SETC( 'M', MODE )
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'CVG_OPEN: Illegal mode ''^M'' supplied: '//
     :                 'must be ''READ'' or ''UPDATE''.', STATUS )
      END IF

*  Find a free logical-unit.
      CALL FIO_GUNIT( FUNIT, STATUS )

*  Open the FITS file.
      CALL FTOPEN( FUNIT, LPATH( : NCF ), RWMODE, BLOCKF, FSTAT )

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.
      IF( FSTAT .GT. CVG__FITSOK ) THEN
         CALL MSG_SETC( 'F', LPATH( :NCF ) )
         CALL CVG_FIOER( FSTAT, ' ', 'FTINIT', 'Error opening '//
     :                   'FITS file ^F.', STATUS )
      END IF

*  If an error has occurred, attempt to close the file.
      IF( STATUS .NE. SAI__OK ) CALL CVG_CLOSE( FUNIT, STATUS )

      END
