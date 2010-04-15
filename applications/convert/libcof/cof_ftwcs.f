      SUBROUTINE COF_FTWCS( FUNIT, INDF, NENCOD, ENCODS, FILE, WCSATT,
     :                      STATUS )
*+
*  Name:
*     COF_FTWCS

*  Purpose:
*     Uses coordinate system information in the FITS headers of the current
*     header and data unit to create WCS and AXIS components in an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_FTWCS( FUNIT, INDF, NENCOD, ENCODS, FILE, WCSATT, STATUS )

*  Description:
*     This constructs an AST FrameSet from the FITS headers of the current
*     header and data unit and adds it into the existing WCS information in
*     the supplied NDF. It can also create AXIS structures (see below).
*
*     The information needed to create the FrameSet can be stored several
*     times in a single FITS header, using different keywords each time.
*     Each of these descriptions is known as an "encoding" and AST supports
*     several different encoding schemes (i.e. FITS-WCS, DSS, NATIVE).
*     If the supplied FITS header contains more than one encoding then we
*     need to choose which one to use. This decision is important because
*     is is possible for encodings to be inconsistent (i.e. software may
*     modify one encoding without making equivalent modifications to the
*     other encodings). The simplest way to make this decision is to hand
*     responsibility for it over to the user. In this case, the user
*     supplies a list of preferred encodings, and the first of these encodings
*     which exists in the FITS header gets used. If the user does not
*     know which encoding to use, then we can make an intelligent guess by
*     comparing the encodings to see which ones are consistent and which
*     ones are not.
*
*     In addition to the WCS component, this routine also creates AXIS
*     Centre, Label and Units components in the NDF, but only if they do
*     not already exist, and if the FrameSet read from the FITS header
*     contains an AXIS Frame. NDF2FITS does not write out the AXIS Frame
*     if it is equivalent to pixel coordinates, and so no AXIS structures
*     will be created by this routine in this case. Also, if the AXIS Frame
*     represents linear axis coordinates, then there will already be AXIS
*     structures in the NDF (created earlier within FITS2NDF), and so again
*     no AXIS  structures will be created by this routine. Thus, this routine
*     will only create AXIS structures in the cases where the axis coordinates
*     are non-linear.

*  Arguments:
*     FUNIT = INTEGER (Given)
*        The FITSIO unit number for the FITS file.
*     INDF = INTEGER (Given)
*        The NDF identifier.
*     NENCOD = INTEGER (Given)
*        The number of encodings supplied in ENCODS.
*     ENCODS( NENCOD ) = CHARACTER * ( * ) (Given)
*        The user's preferred AST encodings. If NENCOD is zero, then this
*        is ignored, and an intelligent guess is made as to which encoding
*        to use (see COF_WCSIM).
*     FILE = CHARACTER * ( * ) (Given)
*        The name of the FITS file or tape device to appear in error
*        messages.
*     WCSATT = CARACTER * ( * ) (Given)
*        Extra attribute settings for the WCS FitsChan.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Prior Requirements:
*     The FITS file must already be opened with the FITSIO library.

*  Copyright:
*     Copyright (C) 1997, 2000-2002, 2004 Central Laboratory of the
*     Research Councils. All Rights Reserved.

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
*     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     11-NOV-1997 (DSB):
*        Original version.
*     11-JAN-2000 (DSB):
*        Add call to COF_ASTWN to display any warning messages generated
*        by AST.
*     3-APR-2001 (DSB):
*        Clarified the errors reported if no WCS is created.
*     20-FEB-2002 (DSB):
*        Added argument WCSATT.
*     2004 September 10 (TIMJ):
*        Fix valgrind warning with uninitialised HEADER on entry
*        to fitsio routine
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and functions

*  Arguments Given:
      INTEGER FUNIT
      INTEGER INDF
      INTEGER NENCOD
      CHARACTER ENCODS( NENCOD )*(*)
      CHARACTER FILE*(*)
      CHARACTER WCSATT*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER FITSOK             ! Value of good FITSIO status
      PARAMETER( FITSOK = 0 )

      INTEGER HEDLEN             ! FITS header length
      PARAMETER( HEDLEN = 80 )

*  Local Variables:
      CHARACTER * ( HEDLEN ) HEADER ! A FITS header
      INTEGER FC                 ! Identifier for AST FitsChan
      INTEGER FSTAT              ! FITSIO status
      INTEGER IHEAD              ! Loop counter for headers
      INTEGER KEYADD             ! Number of headers that can be added
      INTEGER NBAD               ! Number of bad header cards
      INTEGER NHEAD              ! Number of FITS headers

*.

*  Check the inherited global status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin as AST context.
      CALL AST_BEGIN( STATUS )

*  Create an AST FitsChan. This is an object which acts as a buffer to
*  hold a set of FITS header cards to be used by other AST routines.
      FC = AST_FITSCHAN( AST_NULL, AST_NULL, WCSATT, STATUS )
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Initialise the FITSIO status.  It's not the same as the Starlink
*  status, which is reset by the fixed part.
      FSTAT = FITSOK

*  Find the number of headers (not including the final END).
      CALL FTGHSP( FUNIT, NHEAD, KEYADD, FSTAT )
      IF( FSTAT .NE. FITSOK ) THEN
         CALL COF_FIOER( FSTAT, 'COF_FTWCS_NHEAD', 'FTGHSP',
     :                   'Error obtaining the number of header cards.',
     :                   STATUS )
         GO TO 999
      END IF

*  Initialise the number of bad header cards.
      NBAD = 0

*  Loop through the headers.
      DO IHEAD = 1, NHEAD

*  Obtain the header. If an error occurred getting the header, flush
*  the FITSIO error stack, increment the number of bad headers, but
*  carry on to process any remaining headers.
         HEADER = ' '
         CALL FTGREC( FUNIT, IHEAD, HEADER, FSTAT )
         IF( FSTAT .NE. FITSOK ) THEN
            FSTAT = FITSOK
            CALL FTCMSG
            NBAD = NBAD + 1

*  Add this header into the FitsChan. If an error occurs, annul the
*  error, increment the number of bad headers, and continue to process any
*  remaining headers.
         ELSE
            CALL AST_PUTFITS( FC, HEADER, 1, STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_ANNUL( STATUS )
               NBAD = NBAD + 1
            END IF
         END IF

      END DO

*  Issue a warning if any bad header cards were encountered.
      IF ( ( STATUS .EQ. SAI__OK ) .AND. ( NBAD .NE. 0 ) ) THEN
         CALL ERR_MARK
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'NBAD', NBAD )
         CALL ERR_REP( 'COF_FTWCS_WARN', 'WARNING: ^NBAD FITS header '//
     :                 'cards could not be read and were not used.',
     :                 STATUS )
         CALL ERR_FLUSH( STATUS )
         CALL ERR_RLSE
      END IF

*  Rewind the FitsChan by clearing the Card attribute so that the first
*  header card will be read by COF_WCSIM.
      CALL AST_CLEAR( FC, 'Card', STATUS )

*  Now import any WCS information from the FitsChan into the NDF.
      CALL COF_WCSIM( FC, INDF, NENCOD, ENCODS, STATUS )

*  Report any warnings stored in the FitsChan by AST.
      CALL COF_ASTWN( FC, INDF, STATUS )

*  Jump to here if an error occurs. Add a context message.
  999 CONTINUE
      IF( STATUS .NE. SAI__OK )THEN
         CALL MSG_SETC( 'FL', FILE )
         CALL ERR_REP( 'COF_FTWCS_ERR1', 'The output NDF will still '//
     :                 'be created but will not contain any WCS '//
     :                 'information.', STATUS )

*  A useful conversion may still be possible even if the WCS
*  information cannot be imported. So if an error occurred, flush it.
         CALL ERR_FLUSH( STATUS )
      END IF

*  End the AST context.
      CALL AST_END( STATUS )

      END
