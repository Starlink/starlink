      SUBROUTINE KPG1_FLPTH( ROOT, INSTR, OUTSTR, NC, STATUS )
*+
*  Name:
*     KPG1_FLPTH

*  Purpose:
*     Gets the full path to a file given relative to a specified root
*     directory.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_FLPTH( INSTR, OUTSTR, NC, STATUS )

*  Description:
*     This routine returns the full path to a file specified relative to
*     the $ROOT directory. The supplied file spec should be a UNIX-style
*     file spec., but the returned full file path will be translated into
*     the VMS equivalent if required.

*  Arguments:
*     ROOT = CHARACTER * ( * ) (Given)
*        An environment variable which is to be translated to get the
*        full path to the root directory, such as HOME, KAPPA_DIR.  If a
*        blank value is supplied, then the current working directory is
*        used ($PWD on Unix and $PATH on VMS).
*     INSTR = CHARACTER * ( * ) (Given)
*        The file spec relative to the root directory. E.g. fred.dat,
*        adam/display.sdf, etc.
*     OUTSTR = CHARACTER * ( * ) (Returned)
*        The full file path. For example /home/dsb/fred.dat
*        (DISK$USER:[DSB]FRED.DAT on VMS), or /home/dsb/adam/display.sdf
*        (DISK$USER:[DSB.ADAM]DISPLAY.SDF on VMS).  Returned blank if an
*        error occurs.
*     NC = INTEGER (Returned)
*        Number of characters used in OUTSTR. Returned equal to 0 if an
*        error occurs.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1998 CLRC
*     Copyright (C) 2005 Particle Physics and Astronomy Research Council.
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
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     24-SEP-1998 (DSB):
*        Original version.
*     27-DEC-2005 (TIMJ):
*        Call CHR_LASTO rather than KPG1_LASTO.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER ROOT*(*)
      CHARACTER INSTR*(*)

*  Arguments Returned:
      CHARACTER OUTSTR*(*)
      INTEGER NC

*  Status:

      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN

*  Local Variables:
      CHARACTER DUMMY1           ! Un-used argument
      CHARACTER DUMMY2           ! Un-used argument
      CHARACTER DUMMY3           ! Un-used argument
      CHARACTER DUMMY4           ! Un-used argument
      CHARACTER SYSNAM*15        ! The name of the operating system
      INTEGER I                  ! Index of last "/"
*.


*  Initialise
      OUTSTR = ' '
      NC = 0

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  See if we are using VMS or UNIX.
      CALL PSX_UNAME( SYSNAM, DUMMY1, DUMMY2, DUMMY3, DUMMY4, STATUS )

*  Initialise the returned string to hold $ROOT, and get its length.
      IF( ROOT .NE. ' ' ) THEN
         CALL PSX_GETENV( ROOT, OUTSTR, STATUS )

      ELSE IF( SYSNAM( : 3 ) .EQ. 'VMS' ) THEN
         CALL PSX_GETENV( 'PATH', OUTSTR, STATUS )

      ELSE
         CALL PSX_GETENV( 'PWD', OUTSTR, STATUS )

      END IF

      NC = CHR_LEN( OUTSTR )

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  If on VMS...
      IF( SYSNAM( : 3 ) .EQ. 'VMS' ) THEN

*  If the supplied file name contains a "/" then replace the trailing "]"
*  in the translation of $HOME with ".".
         IF( INDEX( INSTR, '/' ) .NE. 0 ) THEN
            OUTSTR( NC : ) = '.'

*  Append the supplied string.
            CALL CHR_APPND( INSTR, OUTSTR, NC )

*  Replace the last "/" with "]".
            CALL CHR_LASTO( OUTSTR( : NC ), '/', I )
            OUTSTR( I : I ) = ']'

*  Replace any remaining "/" with "."
            CALL CHR_TRCHR( '/', '.', OUTSTR( : NC ), STATUS )

*  If the supplied file name does not include a directory part, just
*  append it to the translation of $HOME.
         ELSE
            CALL CHR_APPND( INSTR, OUTSTR, NC )
         ENDIF

*  Convert returned file path to upper case.
         CALL CHR_UCASE( OUTSTR( : NC ) )

*  If on UNIX, append a "/" to the $HOME value, and then append the
*  supplied file name.
      ELSE
         CALL CHR_APPND( '/', OUTSTR, NC )
         CALL CHR_APPND( INSTR, OUTSTR, NC )
      END IF

*  Remove spaces.
      CALL CHR_RMBLK( OUTSTR( : NC ) )


 999  CONTINUE

*  If an error has occurred, return null values.
      IF( STATUS .NE. SAI__OK ) THEN
         OUTSTR = ' '
         NC = 0
      END IF

      END
