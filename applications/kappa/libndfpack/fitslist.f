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
*     or written to a text file.  The displayed list of headers can
*     be augmented, if required, by the inclusion of FITS headers
*     representing the current World Coordinate System defined by the
*     WCS component in the NDF (see Parameter ENCODING).

*  Usage:
*     fitslist in [logfile]

*  ADAM Parameters:
*     ENCODING = LITERAL (Read)
*        If a non-null value is supplied, the NDF WCS component is used
*        to generate a set of FITS headers describing the WCS, and these
*        headers are added into the displayed list of headers (any WCS
*        headers inherited from the FITS extension are first removed).
*        The value supplied for ENCODING controls the FITS keywords
*        that will be used to represent the WCS.  The value supplied
*        should be one of the encodings listed in the "World Co-ordinate
*        Systems" section below.  An error is reported if the WCS cannot
*        be represented using the supplied encoding.  A trailing minus
*        sign appended to the encoding indicates that only the WCS
*        headers should be displayed (that is, the contents of the FITS
*        extension are not displayed if the encoding ends with a minus
*        sign).  Also see the FULLWCS parameter.  [!]
*     FULLWCS = _LOGICAL (Read)
*        Only accessed if ENCODING is non-null.  If TRUE, then all
*        co-ordinate frames in the WCS component are written out.
*        Otherwise, only the current Frame is written out.  [FALSE]
*     IN = NDF (Read)
*        The NDF whose FITS extension is to be listed.
*     LOGFILE = FILENAME (Read)
*        The name of the text file to store a list of the FITS
*        extension.  If it is null (!) the list of the FITS extension
*        is reported directly to you.  [!]

*  Examples:
*     fitslist saturn
*        The contents of the FITS extension in NDF saturn are
*        reported to you.
*     fitslist saturn fullwcs encoding=fits-wcs
*        As above but it also lists the standard FITS world-co-ordinate
*        headers derived from saturn's WCS component, provided such
*        information exists.
*     fitslist saturn fullwcs encoding=fits-wcs-
*        As the previous example except that it only lists the standard
*        FITS world-co-ordinate headers derived from saturn's WCS
*        component.  The headers in the FITS extension are not listed.
*     fitslist ngc205 logfile=ngcfits.lis
*        The contents of the FITS extension in NDF ngc205 are
*        written to the text file ngcfits.lis.

*  Notes:
*     -  If the NDF does not have a FITS extension the application will
*     exit unless the value supplied for ENCODING ends with a minus sign.

*  World Co-ordinate Systems:
*     The ENCODING parameter can take any of the following values.
*
*        "FITS-IRAF" --- This uses keywords CRVALi CRPIXi, CDi_j, and
*        the system commonly used by IRAF.  It is described in the
*        document "World Coordinate Systems Representations Within the
*        FITS Format" by R.J. Hanisch and D.G. Wells, 1988, available by
*        ftp from fits.cv.nrao.edu /fits/documents/wcs/wcs88.ps.Z.
*
*        "FITS-WCS" --- This is the FITS standard WCS encoding
*        scheme described in the paper "Representation of celestial
*        coordinates in FITS"
*        (http://www.atnf.csiro.au/people/mcalabre/WCS/).  It is
*        very similar to "FITS-IRAF" but supports a wider range of
*        projections and co-ordinate systems.
*
*        "FITS-WCS(CD)" --- This is the same as "FITS-WCS" except that
*        the scaling and rotation of the data array is described by a
*        CD matrix instead of a PC matrix with associated CDELT values.
*
*        "FITS-PC" --- This uses keywords CRVALi, CDELTi, CRPIXi,
*        PCiiijjj, etc., as described in a previous (now superseded)
*        draft of the above FITS world co-ordinate system paper by
*        E.W.Greisen and M.Calabretta.
*
*        "FITS-AIPS" --- This uses conventions described in the document
*        "Non-linear Coordinate Systems in AIPS" by Eric W. Greisen
*        (revised 9th September, 1994), available by ftp from
*        fits.cv.nrao.edu /fits/documents/wcs/aips27.ps.Z.  It is
*        currently employed by the AIPS data-analysis facility (amongst
*        others), so its use will facilitate data exchange with AIPS.
*        This encoding uses CROTAi and CDELTi keywords to describe axis
*        rotation and scaling.
*
*        "FITS-AIPS++" --- This is an extension to FITS-AIPS which
*        allows the use of a wider range of celestial projections, as
*        used by the AIPS++ project.
*
*        "FITS-CLASS" --- This uses the conventions of the CLASS
*        project.  CLASS is a software package for reducing single-dish
*        radio and sub-mm spectroscopic data.  It supports
*        double-sideband spectra.  See
*        http://www.iram.fr/IRAMFR/GILDAS/doc/html/class-html/class.html.
*
*        "DSS" --- This is the system used by the Digital Sky Survey,
*        and uses keywords AMDXn, AMDYn, PLTRAH, etc.
*
*        "NATIVE" --- This is the native system used by the AST library
*        (see SUN/210) and provides a loss-free method for transferring
*        WCS information between AST-based application.  It allows more
*        complicated WCS information to be stored and retrieved than any
*        of the other encodings.

*  Related Applications:
*     KAPPA: FITSEDIT, FITSHEAD; Figaro: FITSKEYS.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
*     Copyright (C) 2004 Central Laboratory of the Research Councils.
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     Copyright (C) 2009 Science & Technology Facilities Council.
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
*     MJC: Malcolm J. Currie (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     DSB: David Berry (JAC, Hawaii)
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
*     4-AUG-2009 (DSB):
*        Added parameters ENCODING and FULLWCS, and modified to use an AST
*        FitsChan as a staging area for the displayed headers.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and functions
      INCLUDE 'PAR_ERR'          ! Parameter-system errors

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string

*  Local Variables:
      CHARACTER CARD*80          ! A single FITS header card
      CHARACTER ENCOD*16         ! FITS encoding requested for WCS info
      INTEGER FCHAN              ! Pointer to FitsChan
      INTEGER FD                 ! Logfile descriptor
      INTEGER I                  ! Frame index
      INTEGER IB                 ! Index of base Frame
      INTEGER IC                 ! Index of current Frame
      INTEGER IWCS               ! Pointer to NDF WCS FrameSet
      INTEGER LN                 ! Used length of ENCOD
      INTEGER NDF                ! NDF identifier
      INTEGER NF                 ! Number of Frames in WCS FrameSet
      INTEGER OBJ                ! Object read from FITS extension
      LOGICAL FULLWCS            ! Display all Frames?
      LOGICAL LOGF               ! The log file is open?
      LOGICAL SHOWEX             ! Show contents of the FITS extension?
      LOGICAL THERE              ! Does the NDF have a FITS extension?
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an ast context.
      CALL AST_BEGIN( STATUS )

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Obtain an identifier for the input NDF.
      CALL LPG_ASSOC( 'IN', 'READ', NDF, STATUS )

*  Assume the FITS extension is to be displayed.
      SHOWEX = .TRUE.

*  Get the AST encoding to use when converting WCS information to FITS
*  headers.  If null is supplied, WCS keywords are not added to the
*  displayed list.
      CALL PAR_CHOIC( 'ENCODING', 'FITS-WCS', 'FITS-IRAF,FITS-WCS,'//
     :                'FITS-PC,FITS-AIPS,FITS-AIPS++,FITS-CLASS,DSS,'//
     :                'FITS-WCS(CD),NATIVE,FITS-IRAF-,FITS-WCS-,'//
     :                'FITS-PC-,FITS-AIPS-,FITS-AIPS++-,FITS-CLASS-,'//
     :                'DSS-,FITS-WCS(CD)-,NATIVE-', .FALSE., ENCOD,
     :                STATUS )

*  If a null value was supplied for ENCODING, annul the error and set the
*  encoding blank to indicate that no WCS keywords should be added to the
*  displayed header.
      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         ENCOD = ' '

*  Otherwise see if the encoding ends with a minus sign. If so, remove
*  the minus sign and set a flag indicating that the FITS extension should
*  not be displayed.
      ELSE IF( STATUS .EQ. SAI__OK ) THEN
         LN = CHR_LEN( ENCOD )
         IF( ENCOD( LN : LN ) .EQ. '-' ) THEN
            ENCOD( LN : LN ) = ' '
            SHOWEX = .FALSE.
         END IF
      END IF

*  If the NDF has a FITS extension, and we are displaying the FITS
*  extension, create an AST FitsChan containing the cards in the FITS
*  extension.
      FCHAN = AST__NULL
      IF( SHOWEX ) THEN
         CALL NDF_XSTAT( NDF, 'FITS', THERE, STATUS )
         IF( THERE ) CALL KPG_GTFTS( NDF, FCHAN, STATUS )
      END IF

*  If WCS information is to be displayed, ensure we have a FitsChan.
      IF( ENCOD .NE. ' ' ) THEN
         IF( FCHAN .EQ. AST__NULL ) FCHAN = AST_FITSCHAN( AST_NULL,
     :                                           AST_NULL, ' ', STATUS )

*  If we have a FitsChan (containing the contents of the FITS extension),
*  attempt to read FrameSets from the header. We do not require the
*  resulting objects, but this will delete from the FitsChan any WCS
*  keywords inherited from the FITS extension.
         IF( FCHAN .NE. AST__NULL ) THEN
            OBJ = AST_READ( FCHAN, STATUS )

            DO WHILE( OBJ .NE. AST__NULL )
               OBJ = AST_READ( FCHAN, STATUS )
            END DO

*  If we do not have a FitsChan, create one now.
         ELSE
            FCHAN = AST_FITSCHAN( AST_NULL, AST_NULL, ' ', STATUS )
         END IF

*  Get the WCS FrameSet from the NDF.
         CALL KPG1_GTWCS( NDF, IWCS, STATUS )

*  If only the current coordinate system is required, remove all others
*  from the FrameSet.
         CALL PAR_GET0L( 'FULLWCS', FULLWCS, STATUS )
         IF( .NOT. FULLWCS ) THEN
            NF = AST_GETI( IWCS,' NFrame', STATUS )
            IC = AST_GETI( IWCS, 'Current', STATUS )
            IB = AST_GETI( IWCS, 'Base', STATUS )
            DO I = NF, 1, -1
               IF( I .NE. IC .AND. I .NE. IB ) THEN
                  CALL AST_REMOVEFRAME( IWCS, I, STATUS )
               END IF
            END DO
         END IF

*  Attempt to write the WCS out to the FitsChan. Set the Encoding first
*  to the requested value. Report an error if the conversion fails.
         CALL AST_SETC( FCHAN, 'Encoding', ENCOD, STATUS )
         IF( AST_WRITE( FCHAN, IWCS, STATUS ) .EQ. 0 .AND.
     :       STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'ENC', ENCOD )
            CALL ERR_REP( ' ', 'Cannot convert the NDF WCS to FITS '//
     :                    'using the ''^ENC'' encoding.', STATUS )
         END IF

      END IF

*  Skip to the end if there is nothing to display or an error has
*  occurred.
      IF( FCHAN .NE. AST__NULL .AND. STATUS .EQ. SAI__OK ) THEN

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

*  Tell MSG that it should not wrap at 79 characters since we know we
*  are in blocks of 80.  Given that we can not query MSG for the current
*  values it is difficult to ensure that we reset to the correct value.
*  value.  We make a guess that we are not in STREAM mode and simply
*  re-enable formatted mode afterwards.
         CALL MSG_TUNE( 'STREAM', 1, STATUS )

*  Reset the FitsChan so that the first card is the current card.
         CALL AST_CLEAR( FCHAN, 'Card', STATUS )

*  Loop round all cards in the FitsChan.
         DO WHILE( AST_FINDFITS( FCHAN, '%f', CARD, .TRUE., STATUS ) )

*  Write the card to the log file or to the screen.
            IF( LOGF ) THEN
               CALL FIO_WRITE( FD, CARD, STATUS )
            ELSE
               CALL MSG_OUT( ' ', CARD, STATUS )
            END IF

         END DO

*  Reset the MSG tuning parameter
         CALL MSG_TUNE( 'STREAM', 0, STATUS )

      END IF

*  Close the log file if one has been opened.
      IF ( LOGF ) CALL FIO_ANNUL( FD, STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'FITSLIST_ERR',
     :     'FITSLIST: Error listing a FITS extension.', STATUS )
      END IF

      END
