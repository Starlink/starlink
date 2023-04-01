      SUBROUTINE CVG_NEW( PATH, BLOCKF, OVRWRT, FUNIT, STATUS )
*+
*  Name:
*     CVG_NEW

*  Purpose:
*     Creates a new FITS file and return a unit number for it.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CVG_NEW( PATH, BLOCKF, OVRWRT, FUNIT, STATUS )

*  Description:
*     This function creates a new FITS file with a given path, and
*     returns a logical unit number that can be used to access it
*     using CVG and FITSIO functions.

*  Arguments:
*     PATH = CHARACTER * ( * ) (Given)
*        The path to the file to be created. A file type of ".fit" will be
*        added if there is no file type in the supplied string.
*     BLOCKF = INTEGER (Given)
*        The blocking factor for the new file. It must be a positive
*        integer between 1 and 10.
*     OVRWRT = LOGICAL (Returned)
*        If .TRUE., any existing file with the given name is silently
*        over-written. Otherwise, an error is reported if the file
*        already exists.
*     FUNIT = INTEGER (Returned)
*        The logical unit number of the FITS file. Returned equal to
*        CVG_NOLUN if an error occurs.
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
*     {enter_new_authors_here}

*  History:
*     14-NOV-2013 (DSB):
*        Original version, based on code from COF_NDF2F.
*     19-NOV-2013 (DSB):
*        Use a file type of ".fit" if the supplied path does not include
*        any file type.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CVG_PAR'          ! CVG constants

*  Arguments Given:
      CHARACTER PATH*(*)
      INTEGER BLOCKF
      LOGICAL OVRWRT

*  Arguments Returned:
      INTEGER FUNIT

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Effective string length

*  Local Variables:
      CHARACTER EXTRA*( CVG__MXPTH )
      CHARACTER LPATH*( CVG__MXPTH )
      INTEGER FSTAT
      INTEGER IAT
      INTEGER LNROOT
      INTEGER NAXES(1)
      INTEGER NCF
      LOGICAL FEXIST
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
         CALL ERR_REP( ' ', 'CVG_NEW: Zero-length path supplied for '//
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

*  Find a free logical-unit.
      CALL FIO_GUNIT( FUNIT, STATUS )

*  If required, remove any existing file with the given path. Do
*  this in a new error reporting context so that we can annul any
*  error that arises from it.
      IF( OVRWRT .AND. STATUS .EQ. SAI__OK ) THEN
         CALL ERR_MARK
         CALL PSX_REMOVE( LPATH, STATUS )
         IF( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )
         CALL ERR_RLSE
      END IF

*  Open the FITS file.
      CALL FTINIT( FUNIT, LPATH( : NCF ), BLOCKF, FSTAT )

*  Write the mandatory keywords for the primary header. These can be
*  changed later as needed.
      NAXES(1) = 0
      CALL FTPHPR( FUNIT, .TRUE., 8, 0, NAXES, 0, 1, .TRUE., FSTAT )

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.  To simplify the error message, test to see if the file
*  exists, and if it does make a shorter report (note that the status
*  must be set bad too) and clear the FITSIO error-message stack.
*  Record whether the file was actually opened or not.
      IF( FSTAT .GT. CVG__FITSOK ) THEN
         CALL MSG_SETC( 'F', LPATH( :NCF ) )
         INQUIRE( FILE=LPATH( : NCF ), EXIST=FEXIST )
         IF( FEXIST ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'Error creating the FITS file ^F '//
     :                    ' because it already exists.', STATUS )
            CALL FTCMSG
         ELSE
            CALL CVG_FIOER( FSTAT, ' ', 'FTINIT', 'Error creating '//
     :                      'the output FITS file ^F.', STATUS )
         END IF
      END IF

*  Return a logical unit number of CVG__NOLUN if an error has occurred.
      IF( STATUS .NE. SAI__OK ) FUNIT = CVG__NOLUN

      END
