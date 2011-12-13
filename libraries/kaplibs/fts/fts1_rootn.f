      SUBROUTINE FTS1_ROOTN( MEDIUM, CFN, SUBFIL, PREFIX, FILROO,
     :                       NCROOT, STATUS )
*+
*  Name:
*     FTS1_ROOTN

*  Purpose:
*     Creates the rootname for an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FTS1_ROOTN( MEDIUM, FN, SUBFIL, PREFIX, FILROO, NCROOT,
*    :                 STATUS )

*  Description:
*     This is a server routine for FITSIN.  It packages up the
*     operations required to define the rootname for NDFs in
*     automatic mode.  The rootname is the prefix followed by the
*     file number underscore sub-file number (if present).

*  Arguments:
*     MEDIUM = CHARACTER * ( * ) (Given)
*        The medium containing the FITS file.  Currently supported are
*        'DISK' for a disk file, and 'TAPE' for standard magnetic tape.
*     CFN = CHARACTER * ( * ) (Given)
*        The number on the tape of the FITS file being processed if
*        MEDIUM is 'TAPE', or the input disk-FITS filename if MEDIUM
*        is 'TAPE'.
*     SUBFIL = INTEGER (Given)
*        The number of the sub-file/extension within the current FITS
*        file being processed.
*     PREFIX = CHARACTER * ( * ) (Given)
*        The prefix to be given to the file name of catalogues produced
*        in automatic mode.  It is ignored when %MEDIUM = 'DISK'.
*     FILROO = CHARACTER * ( * ) (Returned)
*        The root file name of NDFs (to be used in automatic mode).
*     NCROOT = INTEGER (Returned)
*        The length in characters of the rootname.
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
*     1990 November 26 (MJC):
*        Original version.
*     1990 December 4 (MJC):
*        Added MEDIUM argument, modified file number to a character
*        argument and added disk-filename handling.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER
     :  SUBFIL

      CHARACTER * ( * )
     :  CFN,
     :  MEDIUM,
     :  PREFIX

*  Arguments Returned:
      INTEGER
     :  NCROOT

      CHARACTER * ( * )
     :  FILROO

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER
     :  CHR_LEN                  ! String length less trailing blanks

*  Local Variables:
      INTEGER
     :  IBR,                     ! Index of a right-hand bracket
     :  IEX,                     ! Index of a period (first character in
                                 ! a file extension)
     :  NCFILN,                  ! Number of characters in file number
     :  NCPREF                   ! Number of characters in file prefix

      CHARACTER
     :  FILNO * 6                ! Number of HDS container file for
                                 ! automatic mode

*.

*    Check status on entry - return if not o.k.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Get the length of the file name or number.

      NCFILN = CHR_LEN( CFN )

*    Generate the file name or prefix if a group-format file.
*    ========================================================

*    The file is specified by a number for a tape.

      IF ( MEDIUM .EQ. 'TAPE' ) THEN

*       The output filename is the prefix followed by the file number.

         NCPREF = CHR_LEN( PREFIX )
         FILROO = PREFIX( :NCPREF )//CFN( :NCFILN )

*       Find the new length of the name string.

         NCROOT = NCPREF + NCFILN

*    The file is specified by name for disk file.

      ELSE IF ( MEDIUM .EQ. 'DISK' ) THEN

*       Extract the filename.  First look for a right bracket for a
*       directory.  The filename will start one character after that
*       or from the first character.  The name extends to one character
*       less than the extension.
*       **  VMS specific  **

         IBR = INDEX( CFN, ']' )
         IEX = INDEX( CFN( MAX( 1, IBR ): ), '.' )

*       Find the new length of the name string and extract the name.

         IF ( IEX .EQ. 0 ) THEN
            NCROOT = NCFILN
            FILROO = CFN
         ELSE IF ( IBR .EQ. 0 ) THEN
            NCROOT = IEX - 1
            FILROO = CFN( 1:NCROOT )
         ELSE
            NCROOT = IEX - 2
            FILROO = CFN( IBR+1:IBR+NCROOT )
         END IF
      END IF

*    Special case when more than one NDF per FITS file
*    so have to distinguish between sub-files.

      IF ( SUBFIL .GT. 1 ) THEN
         CALL CHR_ITOC( SUBFIL, FILNO, NCFILN )
         CALL CHR_APPND( '_'//FILNO( :NCFILN ), FILROO, NCROOT )

*       Find the new length of the name string.

         NCROOT = NCROOT + NCFILN + 1
      END IF

      END
