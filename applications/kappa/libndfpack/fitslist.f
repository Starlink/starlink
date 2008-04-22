      SUBROUTINE FITSLIST( STATUS )
*+
*  Name:
*     FITSLIST

*  Purpose:
*     Lists the FITS extension of an NDF.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL FITSLIST( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application lists the FITS header stored in an NDF FITS
*     extension.  The list may either be reported directly to you,
*     or written to a text file.

*  Usage:
*     fitslist in [logfile]

*  ADAM Parameters:
*     IN = NDF (Read)
*        The NDF whose FITS extension is to be listed.
*     LOGFILE = FILENAME (Read)
*        The name of the text file to store a list of the FITS
*        extension.  If it is null (!) the list of the FITS extension
*        is reported directly to you. [!]

*  Examples:
*     fitslist saturn
*        The contents of the FITS extension in NDF saturn are
*        reported to you.
*     fitslist ngc205 logfile=ngcfits.lis
*        The contents of the FITS extension in NDF ngc205 are
*        written to the text file ngcfits.lis.

*  Notes:
*     -  If the NDF does not have a FITS extension the application will
*     exit.

*  Related Applications:
*     KAPPA: FITSEDIT, FITSHEAD; Figaro: FITSKEYS.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
*     Copyright (C) 2004 Central Laboratory of the Research Councils.
*     Copyright (C) 2006 Particle Physics and Astronomy Research Council.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     1991 February 28 (MJC):
*        Original version.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL.
*     2006 October 17 (TIMJ):
*        Enable STREAM mode when dumping FITS contents.  We do not want
*        to word wrap at 79 characters.
*     2006 October 24 (MJC):
*        Use modern commenting style.
*     {enter_further_changes_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Data-system constants
      INCLUDE 'PAR_ERR'          ! Parameter-system errors
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER EL                 ! Number of cards in the header
      INTEGER FD                 ! Logfile descriptor
      INTEGER LENGTH             ! Length of a character being mapped
      CHARACTER * ( DAT__SZLOC ) LOC ! Locator to the FITS extension
      LOGICAL LOGF               ! The log file is open?
      INTEGER NDF                ! NDF identifier
      INTEGER PNTR( 1 )          ! Pointer to the card images

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Obtain an identifier for the input NDF.
      CALL LPG_ASSOC( 'IN', 'READ', NDF, STATUS )

*  Obtain a locator to the FITS extension.
      CALL NDF_XLOC( NDF, 'FITS', 'READ', LOC, STATUS )

*  Map the 80-character FITS card images stored within the extension.
      CALL DAT_MAPV( LOC, '_CHAR*80', 'READ', PNTR( 1 ), EL, STATUS )
      LENGTH = 80

*  Open the log file.  If null is returned from the parameter system,
*  the list of FITS headers are reported to the user directly.
      CALL ERR_MARK
      LOGF = .FALSE.
      CALL FIO_ASSOC( 'LOGFILE', 'WRITE', 'LIST', 80, FD, STATUS )

      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
      ELSE IF ( STATUS .EQ. SAI__OK ) THEN
         LOGF = .TRUE.
      END IF
      CALL ERR_RLSE

*  Check the status before accessing EL.
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Tell MSG that it should not wrap at 79 characters since we know we
*  are in blocks of 80.  Given that we can not query MSG for the current
*  values it is difficult to ensure that we reset to the correct value.
*  value.  We make a guess that we are not in STREAM mode and simply 
*  re-enable formatted mode afterwards.
         CALL MSG_TUNE( 'STREAM', 1, STATUS )

*  Write the headers to the log file or report them directly.  Note that
*  the length of the character-array elements is passed by value after
*  the last genuine argument.  This is for UNIX and does no harm on VMS.
*  The second character argument is no problem since it is not passed by
*   pointer.
         CALL KPG1_LISTC( FD, EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ), 
     :                    LOGF, STATUS, %VAL( CNF_CVAL( LENGTH ) ) )

*  Reset the MSG tuning parameter
         CALL MSG_TUNE( 'STREAM', 0, STATUS )

      END IF

*  Close the log file if one has been opened.
      IF ( LOGF ) CALL FIO_ANNUL( FD, STATUS )

*  Tidy the locator to the FITS extension.
      CALL DAT_ANNUL( LOC, STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'FITSLIST_ERR',
     :     'FITSLIST: Error listing a FITS extension.', STATUS )
      END IF

      END
