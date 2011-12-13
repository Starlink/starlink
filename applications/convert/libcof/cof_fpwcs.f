      SUBROUTINE COF_FPWCS( FUNIT, INDF, ENCOD, NATIVE, USEAXS, STATUS )
*+
*  Name:
*     COF_FPWCS

*  Purpose:
*     Uses co-ordinate system information from the NDF WCS component
*     to create FITS headers in the current header and data unit.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_FPWCS( FUNIT, INDF, ENCOD, NATIVE, USEAXS, STATUS )

*  Description:
*     The AST FrameSet (see SUN/210) describing the co-ordinate systems
*     of the supplied NDF is obtained. Any Frames that can be generated
*     automatically are removed from this FrameSet (i.e. the PIXEL
*     Frame, and also the AXIS Frame if it is equivalent to the PIXEL
*     Frame). If more than one Frame (i.e. the GRID frame) remains, a
*     FITS header is created containing desriptions (known as
*     "encodings") of the FrameSet.

*  Arguments:
*     FUNIT = INTEGER (Given)
*        The FITSIO unit number to which the FITS header cards are
*         written.
*     INDF = INTEGER (Given)
*        The NDF identifier.
*     ENCOD = CHARACTER * ( * ) (Given)
*        The encoding to use. If this is blank, then a default encoding
*        is chosen based on the contents of the FITS extension. The
*        supplied string should be a recognised AST encoding such as
*        'DSS', 'FITS-WCS', 'NATIVE', etc. (or a blank string).
*     NATIVE = LOGICAL (Given)
*        Include a NATIVE encoding in the header?
*     USEAXS = CHARACTER * ( * ) (Given)
*        Whether or not to export AXIS co-ordinates to an alternate
*        world co-ordinate representation in the FITS headers.  Such an
*        alternate may require a FITS extension to store lookup tables
*        of co-ordinates using the -TAB projection type.  The allowed
*        options are as follows.
*
*        "CHECK" --- requests no AXIS information be stored unless the
*                    current NDF contains AXIS information but no WCS.
*        "YES"   --- May create an alternate world co-ordinate
*                    representation irrespective of how the current
*                    NDF stores co-ordinate information.
*        "NO"    --- Must not create an alternate world co-ordinate
*                    representation in the current NDF.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Prior Requirements:
*     The FITS file must already be opened with the FITSIO library.

*  Copyright:
*     Copyright (C) 1997-2000, 2003-2004 Central Laboratory of the
*     Research Councils. Copyright (C) 2008, 2011 Science & Technology
*     Facilities Council. All Rights Reserved.

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
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     11-NOV-1997 (DSB):
*        Original version.
*     9-NOV-1998 (DSB):
*        Replaced arguments NENCOD and ENCODS by NATIVE.
*     22-JUN-1999 (DSB):
*        Added ENCOD argument.
*     2-FEB-2000 (DSB):
*        Get the current number of header cards before emptying the
*        header.
*     11-APR-2000 (DSB):
*        Updated description of ENCOD argument.
*     12-FEB-2003 (DSB):
*        Reduce the value of the FitsDigits attribute from 20 to 10
*        to avoid lots of spurious decimal places.
*     20-MAY-2003 (DSB):
*        Issue warning if WCS headers cannot be produced.
*     10-SEP-2004 (TIMJ):
*        Initialise HEADER to fix valgrind warning
*     30-JUN-2008 (DSB):
*        Delete cards from end of list to avoid massive CFITSIO
*        overheads.
*     20-JAN-2011 (DSB):
*        - Increase the value of the FitsDigits attribute from 10 to 15
*        to get sub-second accuracy on MJD-OBS.
*        - Support FITS-WCS Paper III "-TAB" algorithm.
*     11-FEB-2011 (DSB):
*        Prevent multiple copies (i.e. one for each NDF array component)
*        of a -TAB bintable being created.
*     2011 February 25 (MJC):
*        Added USEAXS argument.
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
      CHARACTER ENCOD*(*)
      LOGICAL NATIVE
      CHARACTER*( * ) USEAXS

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER COF_WCSEX

*  Local Constants:
      INTEGER FITSOK             ! Value of good FITSIO status
      PARAMETER( FITSOK = 0 )

      INTEGER HEDLEN             ! FITS header length
      PARAMETER( HEDLEN = 80 )

      INTEGER ASTVER             ! Version number for new tables
      PARAMETER( ASTVER = 143526 )

*  Local Variables:
      CHARACTER EXCLUD*4         ! Frame (AXIS) to exclude
      INTEGER FC                 ! Identifier for AST FitsChan
      INTEGER FS                 ! NDF's FrameSet identifier
      INTEGER FSTAT              ! FITSIO status
      LOGICAL GOTAXI             ! Does NDF have an AXIS component?
      LOGICAL GOTWCS             ! Does NDF have a WCS component?
      CHARACTER HEADER*( HEDLEN )! A FITS header
      INTEGER IHEAD              ! Loop counter for headers
      INTEGER ITABLE             ! Index of current entry in KEYMAP
      CHARACTER KEY*30           ! Key name
      INTEGER KEYADD             ! Number of headers that can be added
      INTEGER KEYMAP             ! KeyMap holding FitsTables
      INTEGER NHEAD              ! Number of FITS headers
      INTEGER NTABLE             ! No. of entries in KEYMAP
      LOGICAL PPGTAB             ! Propagate a table?
      INTEGER TABLE              ! Pointer to a FitsTable
      LOGICAL THERE              ! Does the object exist?
*.

*  Check the inherited global status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Initialisation
      HEADER = ' '

*  Begin as AST context.
      CALL AST_BEGIN( STATUS )

*  Create an AST FitsChan. This is an object which acts as a buffer to
*  hold a set of FITS header cards to be used by other AST routines.
*  Setting FitsDigits to a negative value ensures that FitsChan never
*  uses more than the number of digits allowed by the FITS standard when
*  formatting floating point values.
      FC = AST_FITSCHAN( AST_NULL, AST_NULL, 'FITSDIGITS=-15', STATUS )

*  Indicate that the FitsChan can describe axes using the -TAB algorithm
*  defined in FITS-WCS Paper III. The value assigned to the TabOK
*  attribute (ASTVER) is used as the table version number for any tables
*  created by the AST_WRITE method. The value used is a "magic value"
*  that is used to identify tables created by AST.
      CALL AST_SETI( FC, 'TABOK', ASTVER, STATUS )

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Initialise the FITSIO status.  It's not the same as the Starlink
*  status, which is reset by the fixed part.
      FSTAT = FITSOK

*  Find the number of headers (not including the final END) in the
*  current FITSIO header.
      CALL FTGHSP( FUNIT, NHEAD, KEYADD, FSTAT )
      IF( FSTAT .NE. FITSOK ) THEN
         CALL COF_FIOER( FSTAT, 'COF_FPWCS_NHEAD', 'FTGHSP',
     :                   'Error obtaining the number of header cards.',
     :                   STATUS )
         GO TO 999
      END IF

*  Increment the number of cards by 1 to include the final END card.
      NHEAD = NHEAD + 1

*  Copy each card from the FITSIO header into the FitsChan.
      DO IHEAD = 1, NHEAD

*  Obtain the header. If an error occurred getting the header, flush
*  the FITSIO error stack, but carry on to process any remaining
*  headers.
         CALL FTGREC( FUNIT, IHEAD, HEADER, FSTAT )
         IF( FSTAT .NE. FITSOK ) THEN
            FSTAT = FITSOK
            CALL FTCMSG

*  Add this header into the FitsChan. If an error occurs, annul the
*  error, and continue to process any remaining headers.
         ELSE
            CALL AST_PUTFITS( FC, HEADER, 1, STATUS )
            IF ( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )
         END IF
      END DO

*  Decide whether the AXIS -TAB table is to be created.  Default to
*  USEAXS=NO.
      PPGTAB = .FALSE.
      IF ( USEAXS .EQ. 'YES' ) THEN
         PPGTAB = .TRUE.

      ELSE IF ( USEAXS .EQ. 'CHECK' ) THEN

*  If the NDF has a WCS component do not propagate the AXIS information
*  in a table.
         CALL NDF_STATE( INDF, 'WCS', GOTWCS, STATUS )

*  If the NDF has an AXIS component without a WCS componen, do propagate
*  the AXIS information in a table.
         CALL NDF_STATE( INDF, 'AXIS', GOTAXI, STATUS )
         PPGTAB = GOTAXI .AND. .NOT. GOTWCS
      END IF

*  Remove the unwanted AXIS from the NDF FrameSet
      EXCLUD = ' '
      IF ( .NOT. PPGTAB ) EXCLUD = 'AXIS'

*  Now export any WCS information from the NDF into the FitsChan. This
*  may overwrite any WCS information which already existed in the
*  FITSIO header on entry. Only modify the supplied FITSIO header if at
*  least one Object was written to the FitsChan.
      IF( COF_WCSEX( FC, INDF, ENCOD, NATIVE, EXCLUD, STATUS ) .GE.
     :    1 ) THEN

*  Copy the contents of the FitsChan into the CHDU header.
         CALL COF_FC2HD( FC, FUNIT, STATUS )

*  If any axes were successfully described using the -TAB algorithm,
*  there will be one or more FitsTables stored in the FitsChan. For each
*  such FitsTable, create a corresponding extension in the FITS file
*  containing a binary table. Get a pointer to a KeyMap that holds
*  copies of the FitsTables in the FitsChan.
         KEYMAP = AST_GETTABLES( FC, STATUS )
         IF( KEYMAP .NE. AST__NULL ) THEN

*  Get the number of tables in the KeyMap, and loop round them all.
            NTABLE = AST_MAPSIZE( KEYMAP, STATUS )
            DO ITABLE = 1, NTABLE

*  Get the key for the KeyMap entry with the current index, and use it
*  to retrieve a pointer to the FitsTable.
               KEY = AST_MAPKEY( KEYMAP, ITABLE, STATUS )
               IF( AST_MAPGET0A( KEYMAP, KEY, TABLE, STATUS ) ) THEN

                  IF ( PPGTAB ) THEN

*  Create an extension in the FITS file holding a binary table
*  containing values copied from the FitsTable, and then annul the
*  FitsTable pointer. Use the table key in the KeyMap as the extension
*  name.
                     CALL COF_FT2BT( TABLE, FUNIT, KEY, ASTVER, STATUS )
                     CALL AST_ANNUL( TABLE, STATUS )

                  END IF
               END IF
            END DO

*  Annul the pointer to the KeyMap holding the tables.
            CALL AST_ANNUL( KEYMAP, STATUS )
         END IF

*  If the NDF has a WCS component but it could not be written out, issue
*  a warning.
      ELSE

         CALL NDF_STATE( INDF, 'WCS', THERE, STATUS )
         IF( THERE ) THEN
            IF( ENCOD .NE. ' ' ) THEN
               CALL MSG_SETC( 'ENC', ENCOD )
               CALL MSG_OUT( 'COF_FPWCS_MSG1', '  WARNING: Unable '//
     :                       'to export WCS information using the '//
     :                       'specified FITS encoding ''^ENC''.',
     :                       STATUS )
            ELSE
               CALL MSG_OUT( 'COF_FPWCS_MSG2', '  WARNING: Unable '//
     :                       'to export WCS information using any '//
     :                       'of the supported FITS encodings.',
     :                       STATUS )
            END IF

         END IF
      END IF

*  Jump to here if an error occurs.
  999 CONTINUE

*  End the AST context.
      CALL AST_END( STATUS )

      END
