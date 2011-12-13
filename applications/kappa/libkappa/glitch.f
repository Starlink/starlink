      SUBROUTINE GLITCH( STATUS )
*+
*  Name:
*     GLITCH

*  Purpose:
*     Replaces bad pixels in a 2-d NDF with the local median

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL GLITCH( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine removes bad pixels from a 2-d NDF, replacing them with
*     the median of the eight (or less at the edges) neighbouring pixels.
*     At least three of these eight neighbouring pixels must have good
*     values (that is, they must not set to the bad value) otherwise the
*     resultant pixel becomes bad.
*
*     The positions of the pixels to be removed can be supplied in four
*     ways (see parameter MODE):
*
*     - In response to parameter prompts. A single bad pixel position is
*     supplied at each prompt, and the user is re-prompted until a null value
*     is supplied.
*
*     - Within a positions list such as produced by applications CURSOR,
*     LISTMAKE, etc.
*
*     - Within a simple text file. Each line contains the position of a
*     pixel to be replaced.
*
*     - Alternatively, each bad pixel in the input NDF can be used
*     (subject to the above requirement that at least three out of the
*     eight neighbouring pixels are not bad).

*  Usage:
*     glitch in out [title] { incat=?
*                           { infile=?
*                           { pixpos=?
*                           mode

*  ADAM Parameters:
*     IN  =  NDF (Read)
*        The input image.
*     INCAT = FILENAME (Read)
*        A catalogue containing a positions list giving the pixels
*        to be replaced, such as produced by applications CURSOR, LISTMAKE,
*        etc. Only accessed if parameter MODE is given the value "Catalogue".
*     INFILE = FILENAME (Read)
*        The name of a text file containing the positions of the pixels
*        to be replaced. The positions should be given in the current
*        co-ordinate Frame of the input NDF, one per line. Spaces or
*        commas can be used as delimiters between axis values. The file
*        may contain comment lines with the first character # or !. This
*        parameter is  only used if parameter MODE is set to "File".
*     MODE =  LITERAL (Read)
*        The method used to obtain the positions of the pixels to be
*        replaced.  The supplied string can be one of the following
*        options.
*
*        - "Bad"       --  The bad pixels in the input NDF are used.
*
*        - "Catalogue" --  Positions are obtained from a positions list
*                          using parameter INCAT.
*
*        - "File"      --  The pixel positions are read from a text file
*                          specified by parameter INFILE.
*
*        - "Interface" --  The position of each pixel is obtained using
*                          parameter PIXPOS.  The number of positions
*                          supplied must not exceed 200.
*
*        [current value]
*     OUT  =  NDF (Write)
*        The output image.
*     PIXPOS = LITERAL (Read)
*        The position of a pixel to be replaced, in the current
*        co-ordinate Frame of the input NDF. Axis values should be
*        separated by spaces or commas. This parameter is only used if
*        parameter MODE is set to "Interface". If a value is supplied on
*        the command line, then the application exits after processing the
*        single specified pixel. Otherwise, the application loops to
*        obtain multiple pixels to replace, until a null (!) value is
*        supplied. Entering a colon (":") will result in a description of
*        the required co-ordinate Frame being displayed, followed by a
*        prompt for a new value.
*     TITLE  =  LITERAL (Read)
*        Title for the output image.  A null value (!) propagates the title
*        from the input image to the output image. [!]

*  Examples:
*     glitch m51 cleaned mode=cat incat=badpix.FIT
*        Reads pixel positions from the positions list stored in the FITS
*        file badpix.FIT, and replaces the corresponding pixels in the
*        2-d NDF m51.sdf by the median of the surrounding neighbouring
*        pixels. The cleaned image is written to cleaned.sdf.

*  Notes:
*     -  If the current co-ordinate Frame of the input NDF is not PIXEL,
*     then the supplied positions are first mapped into the PIXEL Frame
*     before being used.

*  Related Applications:
*     KAPPA: ARDMASK, CHPIX, FILLBAD, ZAPLIN, NOMAGIC, REGIONMASK,
*     SEGMENT, SETMAGIC; Figaro: CSET, ICSET, NCSET, TIPPEX.

*  Implementation Status:
*     -  This routine correctly processes the AXIS, DATA, QUALITY,
*     VARIANCE, LABEL, TITLE, UNITS, WCS and HISTORY components of the
*     input NDF and propagates all extensions.
*     -  Processing of bad pixels and automatic quality masking are
*     supported.
*     -  Only single and double precision floating point data can be
*     processed directly. All integer data will be converted to floating
*     point before being processed.

*  Copyright:
*     Copyright (C) 2000, 2004 Central Laboratory of the Research
*     Councils. All Rights Reserved.

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
*     {enter_new_authors_here}

*  History:
*     7-MAY-2000 (DSB):
*        First NDF version.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE            ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'        ! Global SSE parameters
      INCLUDE 'NDF_PAR'        ! NDF definitions
      INCLUDE 'SUBPAR_PAR'     ! SUBPAR constants
      INCLUDE 'PAR_ERR'        ! Parameter-system errors
      INCLUDE 'AST_PAR'        ! AST constants and function declarations
      INCLUDE 'CNF_PAR'        ! For CNF_PVAL function

*  Status:
      INTEGER STATUS

*  Local Constants:
      INTEGER MAXPIX            ! Max. no. of pixels which can be supplied in
      PARAMETER ( MAXPIX = 200 )! interface mode.

*  Local Variables:
      CHARACTER DTYPE*( NDF__SZFTP ) ! Data type for output components
      CHARACTER ITYPE*( NDF__SZTYP ) ! Data type for processing
      CHARACTER MODE*10         ! Mode in which positions are to be obtained
      CHARACTER TITLE*80        ! Catalogue title
      INTEGER CFRM              ! Current Frame from input NDF
      INTEGER EL                ! Number of elements in mapped array
      INTEGER INDF1             ! Input-NDF identifier
      INTEGER INDF2             ! Output-NDF identifier
      INTEGER INDIM             ! Length of the 1st array dimension
      INTEGER IPDIN             ! Pointer to input DATA component
      INTEGER IPDOUT            ! Pointer to output DATA component
      INTEGER IPID              ! Pointer to position identifiers
      INTEGER IPIX              ! Index of PIXEL Frame within IWCS
      INTEGER IPPIX             ! Pointer to pixel positions
      INTEGER IPPOS             ! Pointer to supplied positions
      INTEGER IPVIN             ! Pointer to input VARIANCE component
      INTEGER IPVOUT            ! Pointer to output VARIANCE component
      INTEGER IWCS              ! WCS FrameSet from input NDF
      INTEGER IWCSC             ! WCS FrameSet from input catalogue
      INTEGER MAP               ! Mapping from Current to PIXEL Frame
      INTEGER NAX               ! No. of axes in current Frame of NDF
      INTEGER NAXCAT            ! No. of axes in required Frame of catalogue
      INTEGER NPOS              ! No. of positions supplied
      INTEGER NREP              ! No. of positions replaced
      INTEGER SDIM( NDF__MXDIM )! Significant dimensions of the NDF
      INTEGER SLBND( 2 )        ! Significant lower bounds of the image
      INTEGER SUBND( 2 )        ! Significant upper bounds of the image
      LOGICAL VAR               ! Variance is present in the NDF?
*.

*  Check inherited global status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Obtain an identifier for the input NDF.
      CALL LPG_ASSOC( 'IN', 'READ', INDF1, STATUS )

*  Now get the WCS FrameSet from the NDF, ensuring we have exactly
*  two axes in the Base Frame.
      CALL KPG1_ASGET( INDF1, 2, .TRUE., .FALSE., .TRUE., SDIM,
     :                 SLBND, SUBND, IWCS, STATUS )

*  Get a pointer to the current Frame. This is the Frame in which
*  positions are supplied by the user or in the file, and in which they
*  are written to the output log file.
      CFRM = AST_GETFRAME( IWCS, AST__CURRENT, STATUS )

*  Save the number of current Frame axes.
      NAX = AST_GETI( CFRM, 'NAXES', STATUS )

*  Find the index of the PIXEL Frame in the WCS FrameSet.
      CALL KPG1_ASFFR( IWCS, 'PIXEL', IPIX, STATUS )

*  Create the output NDF. This is initially a copy of the input NDF, which
*  will then be altered.
      CALL LPG_PROP( INDF1, 'WCS,Data,Variance,Quality,Axis,Units',
     :               'OUT', INDF2, STATUS )

*  Obtain a new title for the output NDF.
      CALL NDF_CINP( 'TITLE', INDF2, 'Title', STATUS )

*  This application supports only non-complex floating point types directly.
*  Therefore for the given type of the image find in which type it should
*  be processed.
      CALL NDF_MTYPE( '_REAL,_DOUBLE', INDF2, INDF2, 'Data', ITYPE,
     :                DTYPE, STATUS )

*  Map the input and output data arrays.
      CALL NDF_MAP( INDF1, 'DATA', ITYPE, 'READ', IPDIN, EL, STATUS )
      CALL NDF_MAP( INDF2, 'DATA', ITYPE, 'UPDATE', IPDOUT, EL, STATUS )

*  See whether variance exists.
      VAR = .FALSE.
      CALL NDF_STATE( INDF2, 'Variance', VAR, STATUS )

*  If so, map the input and output variance arrays.
      IF( VAR ) THEN
         CALL NDF_MAP( INDF1, 'VARIANCE', ITYPE, 'READ', IPVIN, EL,
     :                 STATUS )
         CALL NDF_MAP( INDF2, 'VARIANCE', ITYPE, 'UPDATE', IPVOUT, EL,
     :                 STATUS )
      ELSE
         IPVIN = IPDIN
         IPVOUT = IPDOUT
      END IF

*  Find which mode of operation is to be employed.
      CALL PAR_CHOIC( 'MODE', 'Interface', 'Bad,File,Catalogue,'//
     :                'Interface', .TRUE., MODE, STATUS )

*  In "Catalogue" mode, open a positions list catalogue and read its
*  contents. A pointer to a FrameSet is returned, together with pointers
*  to positions and identifiers, and a title. The positions are returned
*  in the Base Frame of this FrameSet.
      IF( MODE .EQ. 'CATALOGUE' ) THEN
         IWCSC = AST__NULL
         CALL KPG1_RDLST( 'INCAT', .FALSE., IWCSC, NPOS, NAXCAT, IPPOS,
     :                    IPID, TITLE, ' ', STATUS )

*  Make the PIXEL Frame the current Frame in the NDFs FrameSet.
         CALL AST_SETI( IWCS, 'CURRENT', IPIX, STATUS )

*  Attempt to merge the NDFs FrameSet into the FrameSet read from the
*  positions list. The Current Frame in the merged FrameSet (IWCSC) is
*  inherited from the NDFs FrameSet, and is therefore the NDF PIXEL Frame.
         CALL KPG1_ASMRG( IWCSC, IWCS, ' ', .FALSE., 2, STATUS )

*  Get the mapping which transforms the positions supplied in the catalogue
*  (i.e. Base Frame positions) into PIXEL positions (i.e. Current Frame
*  positions).
         MAP = AST_SIMPLIFY( AST_GETMAPPING( IWCSC, AST__BASE,
     :                                       AST__CURRENT, STATUS ),
     :                       STATUS )

*  Allocate memory to hold the corresponding PIXEL positions.
         CALL PSX_CALLOC( 2*NPOS, '_DOUBLE', IPPIX, STATUS )

*  Transform the supplied Current Frame positions into the PIXEL Frame.
         CALL AST_TRANN( MAP, NPOS, NAXCAT, NPOS,
     :                   %VAL( CNF_PVAL( IPPOS ) ), .TRUE.,
     :                   2, NPOS, %VAL( CNF_PVAL( IPPIX ) ), STATUS )

*  Free the memory holding the supplied positions and identifiers.
         CALL PSX_FREE( IPPOS, STATUS )
         CALL PSX_FREE( IPID, STATUS )

*  The stride between axis values is NPOS.
         INDIM = NPOS

*  If we are in "File" mode, obtain the file and read the positions,
*  interpreting them as positions within the Current Frame of the NDF.
*  A pointer to memory holding the positions is returned.
      ELSE IF( MODE .EQ. 'FILE' ) THEN
         CALL KPG1_ASFIL( 'INFILE', ' ', CFRM, NPOS, IPPOS, ' ',
     :                    STATUS )

*  Get the Mapping from the current NDF Frame to the PIXEL Frame.
         MAP = AST_SIMPLIFY( AST_GETMAPPING( IWCS, AST__CURRENT, IPIX,
     :                                       STATUS ), STATUS )

*  Allocate memory to hold the corresponding PIXEL positions.
         CALL PSX_CALLOC( 2*NPOS, '_DOUBLE', IPPIX, STATUS )

*  Transform the supplied Current Frame positions into the PIXEL Frame.
         CALL AST_TRANN( MAP, NPOS, NAX, NPOS,
     :                   %VAL( CNF_PVAL( IPPOS ) ), .TRUE.,
     :                   2, NPOS, %VAL( CNF_PVAL( IPPIX ) ), STATUS )

*  Free the memory holding the supplied positions.
         CALL PSX_FREE( IPPOS, STATUS )

*  The stride between axis values is NPOS.
         INDIM = NPOS

*  If in "Interface" mode, allocate memory to store the maximum number of
*  positions, and then get them.
      ELSE IF( MODE .EQ. 'INTERFACE' ) THEN
         CALL PSX_CALLOC( MAXPIX*2, '_DOUBLE', IPPIX, STATUS )
         CALL KPS1_GLIGT( IWCS, IPIX, 'PIXPOS', MAXPIX,
     :                    %VAL( CNF_PVAL( IPPIX ) ),
     :                    NPOS, STATUS )

*  The stride between axis values is MAXPIX.
         INDIM = MAXPIX

*  If in "Bad" mode, get the pixel positions of all the bad pixels.
      ELSE
         IF( ITYPE .EQ. '_REAL' ) THEN
            CALL KPS1_GLIBR( SLBND( 1 ), SLBND( 2 ), SUBND( 1 ),
     :                       SUBND( 2 ), %VAL( CNF_PVAL( IPDIN ) ),
     :                       IPPIX, NPOS,
     :                       STATUS )

         ELSE IF( ITYPE .EQ. '_DOUBLE' ) THEN
            CALL KPS1_GLIBD( SLBND( 1 ), SLBND( 2 ), SUBND( 1 ),
     :                       SUBND( 2 ), %VAL( CNF_PVAL( IPDIN ) ),
     :                       IPPIX, NPOS,
     :                       STATUS )

         ELSE IF( STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'I', ITYPE )
            CALL ERR_REP( 'GLITCH_ERR1', 'NDF data type ^I not '//
     :                    'supported (programming error).', STATUS )
         END IF

*  The stride between axis values is NPOS.
         INDIM = NPOS

      END IF

*  Do nothing more if no pixels are to be de-glitched.
      IF( NPOS .GT. 0 ) THEN

*  Do the work, using a subroutine for each different data type.
         IF( ITYPE .EQ. '_REAL' ) THEN
            CALL KPS1_GLIWR( SLBND( 1 ), SLBND( 2 ), SUBND( 1 ),
     :                    SUBND( 2 ), VAR, NPOS, INDIM,
     :                    %VAL( CNF_PVAL( IPPIX ) ),
     :                    %VAL( CNF_PVAL( IPDIN ) ),
     :                    %VAL( CNF_PVAL( IPVIN ) ),
     :                    %VAL( CNF_PVAL( IPDOUT ) ),
     :                    %VAL( CNF_PVAL( IPVOUT ) ), NREP, STATUS )

         ELSE IF( ITYPE .EQ. '_DOUBLE' ) THEN
            CALL KPS1_GLIWD( SLBND( 1 ), SLBND( 2 ), SUBND( 1 ),
     :                    SUBND( 2 ), VAR, NPOS, INDIM,
     :                    %VAL( CNF_PVAL( IPPIX ) ),
     :                    %VAL( CNF_PVAL( IPDIN ) ),
     :                    %VAL( CNF_PVAL( IPVIN ) ),
     :                    %VAL( CNF_PVAL( IPDOUT ) ),
     :                    %VAL( CNF_PVAL( IPVOUT ) ), NREP, STATUS )

         ELSE IF( STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'I', ITYPE )
            CALL ERR_REP( 'GLITCH_ERR2', 'NDF data type ^I not '//
     :                    'supported (programming error).', STATUS )
         END IF

      ELSE
         NREP = 0
      END IF

*  Report the number of pixel replaced.
      IF( NREP .EQ. 0 ) THEN
         CALL MSG_OUT( 'GLITCH_MSG1', '   No pixels replaced.',
     :                  STATUS )

      ELSE IF( NREP .EQ. 1 ) THEN
         CALL MSG_OUT( 'GLITCH_MSG2', '   One pixel replaced.',
     :                  STATUS )

      ELSE
         CALL MSG_SETI( 'N', NREP )
         CALL MSG_OUT( 'GLITCH_MSG3', '   ^N pixels replaced.',
     :                  STATUS )
      END IF

      CALL MSG_BLANK( STATUS )

*  If an error occurred, attempt to delete the output NDF.
      IF ( STATUS .NE. SAI__OK ) CALL NDF_DELET( INDF2, STATUS )

*  Free workspace.
      CALL PSX_FREE( IPPIX, STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  Add a context report if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'GLITCH_ERR3', 'GLITCH: Failed to replace '//
     :                 'selected pixels within a 2-dimensional NDF.',
     :                 STATUS )
      END IF

      END
