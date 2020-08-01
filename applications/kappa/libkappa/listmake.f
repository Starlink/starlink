      SUBROUTINE LISTMAKE( STATUS )
*+
*  Name:
*     LISTMAKE

*  Purpose:
*     Creates a catalogue holding a positions list.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL LISTMAKE( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application creates a catalogue containing a list of
*     positions supplied by the user, together with information
*     describing the co-ordinate Frames in which the positions are
*     defined.  Integer position identifiers which allow positions to be
*     distinguished are also stored in the catalogue.  The catalogue may
*     be manipulated using the CURSA package (SUN/190), and is stored in
*     either FITS binary format or the "Small Text List" (STL) format
*     defined by CURSA.
*
*     If an NDF is specified using Parameter NDF, then the positions
*     should be given in the current co-ordinate Frame of the NDF.
*     Information describing the co-ordinate Frames available within the
*     NDF will be copied to the output positions list.  Subsequent
*     applications can use this information in order to align the
*     positions with other data sets.
*
*     If no NDF is specified, then the user must indicate the
*     co-ordinate Frame in which the positions will be supplied using
*     Parameter FRAME.  A description of this Frame will be written to
*     the output positions list for use by subsequent applications.
*
*     The positions themselves may be supplied within a text file,
*     or may be given in response to repeated prompts for a parameter.
*     Alternatively, pixel centres in the NDF supplied for parameter
*     NDF can be used (see Parameter MODE).
*
*     The output can be initialised by copying positions from an
*     existing positions list.  Any positions supplied directly by the
*     user are then appended to the end of this initial list (see
*     Parameter INCAT).

*  Usage:
*     listmake outcat [ndf] [mode] { file=?
*                                  { position=?
*                                  mode

*  ADAM Parameters:
*     CATFRAME = LITERAL (Read)
*        A string determining the co-ordinate Frame in which positions
*        are to be stored in the output catalogue associated with
*        Parameter OUTCAT.  The string supplied for CATFRAME can be one
*        of the following options.
*
*        - A Domain name such as SKY, AXIS, PIXEL, etc.
*
*        - An integer value giving the index of the required Frame.
*
*        - An IRAS90 Sky Co-ordinate System (SCS) values such as
*        EQUAT(J2000) (see SUN/163).
*
*        If a null (!) value is supplied, the positions will be stored
*        in the current Frame. [!]
*     CATEPOCH = DOUBLE PRECISION (Read)
*        The epoch at which the sky positions stored in the output
*        catalogue were determined.  It will only be accessed if an
*        epoch value is needed to qualify the co-ordinate Frame
*        specified by COLFRAME.  If required, it should be given as a
*        decimal years value, with or without decimal places ("1996.8"
*        for example).  Such values are interpreted as a Besselian epoch
*        if less than 1984.0 and as a Julian epoch otherwise.
*     DESCRIBE = LOGICAL (Read)
*        If TRUE, a detailed description of the co-ordinate Frame in
*        which positions are required will be displayed before the
*        positions are obtained using either Parameter POSITION or FILE.
*        [Current value]
*     DIM = _INTEGER (Read)
*        The number of axes for each position.  It is only accessed if
*        a null value is supplied for Parameter NDF.
*     EPOCH = _DOUBLE (Read)
*        If an IRAS90 Sky Co-ordinate System specification is supplied
*        (using Parameter DOMAIN) for a celestial co-ordinate system,
*        then an epoch value is needed to qualify it.  This is the epoch
*        at which the supplied sky positions were determined.  It should
*        be given as a decimal years value, with or without decimal
*        places ("1996.8" for example).  Such values are interpreted as
*        a Besselian epoch if less than 1984.0 and as a Julian epoch
*        otherwise.
*     FILE = FILENAME (Read)
*        A text file containing the positions to be stored in the output
*        positions list. Each line should contain the formatted axis
*        values for a single position, separated by white space.  It is
*        only accessed if Parameter MODE is given the value "File".
*     FRAME = LITERAL (Read)
*        Specifies the co-ordinate Frame of the positions supplied through
*        Parameters POSITION or FILE.  There is a cascade of allowed
*        interpretations of this parameter value; the search for the
*        co-ordinate Frame ends once there is a successful interpretation,
*        otherwise the search moves on to the next possible meaning in
*        the following order.
*
*        -  An HDS path containing a WCS FrameSet, whose current
*        Frame defines the co-ordinate Frame.
*
*        -  The name of an NDF, whose current WCS co-ordinate Frame
*        is used.
*
*        -  If the parameter value ends with ".FIT", an attempt is
*        made to interpret the parameter value as the name of a FITS
*        file.  If successful, the primary WCS co-ordinate system from
*        the primary HDU headers is used.
*
*        -  A text file containing either an AST Frame dump (such as
*        produced by commands in the ATOOLS package), or a set of FITS
*        WCS headers.
*
*        -  An IRAS90 "Sky Co-ordinate System" (SCS) string such as
*        "EQUAT(J2000)" (see SUN/163}), whereupon the positions are
*        assumed to be two-dimensional celestial co-ordinates in the
*        specified system.
*
*        -  Domain name without any interpretation.  Any Domain name may
*        be supplied, but normally one of the standard Domain names,
*        such as GRID, PIXEL, GRAPHICS should be given.  Parameter DIM
*        is used to determine the number of axes in the Frame.
*
*        This parameter is only accessed if the Parameter NDF is given a
*        null value.
*     INCAT = FILENAME (Read)
*        A catalogue containing an existing positions list which is to
*        be included at the start of the output positions list.  These
*        positions are mapped into the current co-ordinate Frame of the
*        supplied NDF, or into the Frame specified by Parameter FRAME if
*        no NDF was supplied.  A message is displayed indicating the
*        Frame in which alignment occurred.  They are then stored in the
*        output list before any further positions are added.  A null
*        value may be supplied if there is no input positions list.  [!]
*     MODE = LITERAL (Read)
*        The mode by which the positions are to be obtained.  The
*        options are as follows.
*
*        - "Interface" -- The positions are obtained using Parameter
*        POSITION.
*
*        - "File" -- The positions are to be read from a text file
*        specified using Parameter FILE.
*
*        - "Good" -- The positions used are the pixel centres in the
*        data file specified by Parameter NDF.  Only the pixels that
*        have good values in the Data array of the NDF are used.
*
*        - "Pixel" -- The positions used are the pixel centres in the
*        data file specified by Parameter NDF.  All pixel are used,
*        whether the pixel values are good or not.
*
*        ["Interface"]
*     NDF = NDF (READ)
*        The NDF which defines the available co-ordinate Frames in the
*        output positions list.  If an NDF is supplied, the positions
*        obtained using Parameters POSITION or FILE are assumed to be in
*        the current co-ordinate Frame of the NDF, and the WCS component
*        of the NDF is copied to the output positions list.  If a null
*        value is supplied, the single co-ordinate Frame defined by
*        Parameter FRAME is stored in the output positions list, and
*        supplied positions are assumed to be in the same Frame.  [!]
*     OUTCAT = FILENAME (Write)
*        The catalogue holding the output positions list.  See also
*        Parameter CATFRAME.
*     POSITION = LITERAL (Read)
*        The co-ordinates of a single position to be stored in the
*        output positions list.  Supplying ":" will display details of
*        the co-ordinate Frame in which the position is required.  The
*        position should be given as a list of formatted axis values
*        separated by white space.  You are prompted for new values for
*        this parameter until a null value is entered.   It is only
*        accessed if Parameter MODE is given the value "Interface".
*     TITLE = LITERAL (Read)
*        A title for the output positions list.  If a null (!) value is
*        supplied, the value used is obtained from the input positions
*        list if one is supplied.  Otherwise, it is obtained from the
*        NDF if one is supplied.  Otherwise, it is "Output from
*        LISTMAKE".  [!]

*  Examples:
*     listmake newlist frame=pixel dim=2
*        This creates a FITS binary catalogue called newlist.FIT
*        containing a list of positions, together with a description of
*        a single two-dimensional pixel co-ordinate Frame.  The
*        positions are supplied as a set of space-separated pixel
*        co-ordinates in response to repeated prompts for the Parameter
*        POSITION.
*     listmake stars.txt frame=equat(B1950) epoch=1962.3
*        This creates a catalogue called stars.txt containing a list of
*        positions, together with a description of a single FK4
*        equatorial RA/DEC co-ordinate Frame (referenced to the B1950
*        equinox).  The catalogue is stored in a text file using the
*        CAT "Small Text List" format ("STL" - see SUN/190).  The
*        positions were determined at epoch B1962.3.  The epoch of
*        observation is required since the underlying model on which the
*        FK4 system is based is non-inertial and rotates slowly with
*        time, introducing fictitious proper motions.  The positions are
*        supplied hours and degrees values in reponse to repeated
*        prompts for Parameter POSITIONS.
*     listmake outlist ndf=allsky mode=file file=stars catframe=gal
*        This creates a FITS binary catalogue called outlist.FIT
*        containing a list of positions, together with descriptions of
*        all the co-ordinate Frames contained in the NDF allsky.  The
*        positions are supplied as co-ordinates within the current
*        co-ordinate Frame of the NDF.  Application WCSFRAME can be used
*        to find out what this Frame is.  The positions are supplied in
*        a text file called stars.  The positions are transformed into
*        galactic co-ordinates before being stored in the output.
*     listmake out.txt incat=old.fit frame=gal
*        This creates an STL format catalogue stored in a text file
*        called out.txt containing a list of positions, together with a
*        description of a single galactic co-ordinate Frame.  The
*        positions contained in the existing binary FITS catalogue
*        old.fit are mapped into galactic co-ordinates (if possible)
*        and stored in the output positions list.  Further galactic
*        co-ordinate positions are then obtained by repeated prompting
*        for the Parameter POSITION.  These positions are appended to
*        the positions obtained from file old.fit.
*     listmake out.txt incat=old.fit ndf=cobe
*        As above but the output positions list contains copies of all
*        the Frames in the NDF cobe.  The positions in old.fit are
*        mapped into the current co-ordinate Frame of the NDF (if
*        possible) before being stored in the output positions list.
*        The new positons must also be supplied in the same Frame (using
*        Parameter POSITION).
*     listmake profpos.fit ndf=prof1 mode=pixel
*        This creates a positions list called profpos.fit containing the
*        positions of all the pixel centres in the one-dimensional NDF
*        called prof.  This could for instance be used as input to
*        application PROFILE in order to produce another profile in
*        which the samples are at the same positions as those in NDF
*        prof.

*  Notes:
*     -  This application uses the conventions of the CURSA package
*     for determining the formats of input and output catalogues.  If a
*     file type of .fit is given, then the catalogue is assumed to be a
*     FITS binary table.  If a file type of .txt is given, then the
*     catalogue is assumed to be stored in a text file in STL format.
*     If no file type is given, then ".fit" is assumed.
*     -  There is a limit of 200 on the number of positions which can be
*     given using Parameter POSITION.  There is no limit on the number
*     of positions which can be given using Parameter FILE.
*     -  Position identifiers are asigned to the supplied positions in
*     the order in which they are supplied.  If no input positions list
*     is given using Parameter INCAT, then the first supplied position
*     will be assigned the identifier "1".  If an input positions list
*     is given, then the first supplied position is assigned an
*     identifier one greater than the largest identifier in the input
*     positions list.

*  Related Applications:
*     KAPPA: CURSOR, LISTSHOW; CURSA: XCATVIEW, CATSELECT.

*  Copyright:
*     Copyright (C) 1998-1999, 2001, 2004 Central Laboratory of the
*     Research Councils.
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
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     16-SEP-1998 (DSB):
*        Original version.
*     3-SEP-1999 (DSB):
*        Added NULL argument to KPG1_GTPOS call.
*     13-DEC-2001 (DSB):
*        Added Parameters CATFRAME and CATEPOCH.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL
*     20-JAN-2006 (DSB):
*        Added option "Good" for Parameter MODE.
*     8-JUN-2009 (DSB):
*        Document new options for Parameter FRAME.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations
      INCLUDE 'PAR_ERR'          ! PAR error constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MXPOS              ! Max. no. of positions which can be
      PARAMETER ( MXPOS=200 )    ! given in interface mode.

*  Local Variables:
      CHARACTER MODE*10          ! Mode for getting supplied positions
      CHARACTER TITLE*80         ! Title for output positions list
      DOUBLE PRECISION BC( NDF__MXDIM )! Supplied Base Frame position
      DOUBLE PRECISION CC( NDF__MXDIM )! Supplied position
      DOUBLE PRECISION POS( MXPOS, NDF__MXDIM )! Positions given using POSITION
      INTEGER DIM( NDF__MXDIM )  ! Dimensions of supplied NDF
      INTEGER EL                 ! Number of mapped data array elements
      INTEGER FRM                ! Frame in which positions must be supplied
      INTEGER I                  ! Loop count
      INTEGER ICURR0             ! Index of original current Frame
      INTEGER ID0                ! First identifier for supplied positions
      INTEGER IFRM               ! Index of Frame for required positions
      INTEGER INDF               ! Reference NDF identifier
      INTEGER IPDAT              ! Pointer to NDF data array
      INTEGER IPFIL              ! Pointer to supplied positions
      INTEGER IPID               ! Pointer to output identifiers
      INTEGER IPIDIN             ! Pointer to input identifiers
      INTEGER IPIN               ! Pointer to input positions
      INTEGER IPPOS              ! Pointer to output positions
      INTEGER IPW1               ! Pointer to work array
      INTEGER IVAL               ! Dummy integer variable
      INTEGER IWCS               ! AST pointer to output FrameSet
      INTEGER IWCSIN             ! AST pointer to input FrameSet
      INTEGER MAP                ! Pointer to simplified Base->Current Mapping
      INTEGER NAX                ! No. of axes in required Frame
      INTEGER NAXIN              ! No. of axes in input list Frame
      INTEGER NBAD               ! No. of bad pixels in the NDF data array
      INTEGER NDIM               ! No. of pixel axes in supplied NDF
      INTEGER NP                 ! Number of supplied positions
      INTEGER NPIN               ! Number of input positions
      INTEGER NPOUT              ! Number of output positions
      LOGICAL DESC               ! Describe the Frame?
      LOGICAL USEFIL             ! Positions supplied using Parameter FILE?
*.

*  Check the inherited global status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Initialise pointers.
      IPPOS = 0
      IPFIL = 0
      IPIN = 0
      IPIDIN = 0
      IPID = 0
      IPW1 = 0

*  Start a new AST context.
      CALL AST_BEGIN( STATUS )

*  Start a new NDF context.
      CALL NDF_BEGIN

*  See where the positions are to be obtained from.
      CALL PAR_CHOIC( 'MODE', 'Interface', 'Interface,File,Pixel,Good',
     :                .TRUE., MODE, STATUS )

*  Get a FrameSet defining the available co-ordinate Frames, and identify
*  the Frame within this FrameSet in which positions are required.
*  ===================================================================

*  Get the NDF which defines the available co-ordinate Frames.
      CALL LPG_ASSOC( 'NDF', 'READ', INDF, STATUS )

*  If an NDF was supplied, get its WCS FrameSet.
      IF( STATUS .EQ. SAI__OK ) THEN
         CALL KPG1_GTWCS( INDF, IWCS, STATUS )

*  Save the index of the original Current Frame.
         ICURR0 = AST_GETI( IWCS, 'CURRENT', STATUS )

*  Ensure the Current Frame in the FrameSet is the Frame in which positions
*  must be supplied. Use the Base (GRID) Frame if NDF pixel positions are
*  being used, and the original Current Frame otherwise.
         IF( MODE .EQ. 'PIXEL' .OR. MODE. EQ. 'GOOD' ) THEN
            CALL AST_SETI( IWCS, 'CURRENT', AST_GETI( IWCS, 'BASE',
     :                                                STATUS ),
     :                     STATUS )
         END IF

*  Get a pointer to the Frame and note the number of axes.
         FRM = AST_GETFRAME( IWCS, AST__CURRENT, STATUS )
         NAX = AST_GETI( FRM, 'NAXES', STATUS )

*  If no NDF was supplied, annul the error, and use Parameters FRAME
*  and DIM to define the (single) available Frame.
      ELSE IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )

*  Report an error if we are using pixel centres.
         IF( MODE .EQ. 'PIXEL' .OR. MODE .EQ. 'GOOD' ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'LISTMAKE_ERR1', 'Parameter %MODE has been '//
     :                    'set to a value which requires an NDF, '//
     :                    'but a null value was given for Parameter '//
     :                    '%NDF.', STATUS )
         END IF

*  Get the required Frame.
         CALL KPG1_ASFGT( 'FRAME', 'DIM', 'EPOCH', FRM, NAX, STATUS )

*  Create a FrameSet holding this single Frame.
         IWCS = AST_FRAMESET( FRM, ' ', STATUS )

*  Save the index of the original Current Frame.
         ICURR0 = 1

      END IF

*  Get any input positions list.
*  =============================
*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Read the contents of the input positions list. The returned positions
*  are in the Base Frame of the returned FrameSet.
      TITLE = ' '
      CALL KPG1_RDLST( 'INCAT', .FALSE., IWCSIN, NPIN, NAXIN, IPIN,
     :                 IPIDIN, TITLE, ' ', STATUS )

*  If an input list was supplied...
      IF( STATUS .EQ. SAI__OK ) THEN

*  Get work space to hold the input positions after mapping into the
*  required Frame.
         CALL PSX_CALLOC( NPIN*NAX, '_DOUBLE', IPW1, STATUS )

*  Attempt to merge the output FrameSet into the FrameSet read from the
*  input positions list, aligning them in a suitable common Frame.
         CALL KPG1_ASMRG( IWCSIN, IWCS, ' ', .FALSE., 2, STATUS )

*  Get the Mapping from Base to Current Frames in the merged FrameSet.
         MAP = AST_GETMAPPING( IWCSIN, AST__BASE, AST__CURRENT, STATUS )
         MAP = AST_SIMPLIFY( MAP, STATUS )

*  Map the input Base Frame positions into the required Frame using this
*  Mapping.
         CALL AST_TRANN( MAP, NPIN, NAXIN, NPIN,
     :                   %VAL( CNF_PVAL( IPIN ) ), .TRUE.,
     :                   NAX, NPIN, %VAL( CNF_PVAL( IPW1 ) ), STATUS )

*  Find the maximum identifier value in the input list.
         CALL KPG1_MXMNI( .FALSE., NPIN, %VAL( CNF_PVAL( IPIDIN ) ),
     :                    IVAL, ID0,
     :                    IVAL, IVAL, IVAL, STATUS )

*  The first supplied position will have an identifier one more than the
*  largest identifier in the supplied positions list.
         ID0 = ID0 + 1

*  If no input list was supplied, annul the error.
      ELSE IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         NPIN = 0
         ID0 = 1
      END IF

*  Obtain the new positions to store in the output positions list.
*  ===============================================================

*  If a description of the co-ordinate Frame is required, give one.
      CALL PAR_GET0L( 'DESCRIBE', DESC, STATUS )
      IF( DESC ) THEN
         CALL MSG_BLANK( STATUS )
         CALL KPG1_DSFRM( FRM, 'Positions must be supplied in '//
     :                    'the following co-ordinate Frame:', AST__BAD,
     :                    AST__BAD, .TRUE., STATUS )
      END IF

*  If required, attempt to read positions from a file specified by the
*  user.
      IF( MODE .EQ. 'FILE' ) THEN
         CALL KPG1_ASFIL( 'FILE', ' ', FRM, NP, IPFIL, ' ', STATUS )

*  If succesful, indicate that the IPFIL pointer should be used.
         USEFIL = ( NP .GT. 0 .AND. STATUS .EQ. SAI__OK )

*  If all the pixel centres are to be used...
      ELSE IF( MODE .EQ. 'PIXEL' ) THEN

*  Get the dimensions and size of the NDF.
         CALL NDF_DIM( INDF, NDF__MXDIM, DIM, NDIM, STATUS )
         CALL NDF_SIZE( INDF, NP, STATUS )

*  Get workspace to hold the pixel centres in GRID positions.
         CALL PSX_CALLOC( NP*NDIM, '_DOUBLE', IPFIL, STATUS )

*  Abort if an error has occurred.
         IF( STATUS .NE. SAI__OK ) GO TO 999

*  Store the pixel centres in IPFIL.
         CALL KPS1_LMKPC( NDIM, DIM, NP, %VAL( CNF_PVAL( IPFIL ) ),
     :                    STATUS )

*  Indicate that we should use the IPFIL pointer.
         USEFIL = .TRUE.

*  If the good pixel centres are to be used...
      ELSE IF( MODE .EQ. 'GOOD' ) THEN

*  Get the dimensions of the NDF.
         CALL NDF_DIM( INDF, NDF__MXDIM, DIM, NDIM, STATUS )

*  Map the NDF's Data component.
         CALL NDF_MAP( INDF, 'DATA', '_REAL', 'READ', IPDAT, EL,
     :                 STATUS )

*  Count the number of bad values in the data array.
         CALL KPG1_NBADR( EL, %VAL( CNF_PVAL( IPDAT ) ), NBAD, STATUS )

*  Convert this to the number of good values.
         NP = EL - NBAD

*  Get workspace to hold the good pixel centres in GRID positions.
         CALL PSX_CALLOC( NP*NDIM, '_DOUBLE', IPFIL, STATUS )

*  Abort if an error has occurred.
         IF( STATUS .NE. SAI__OK ) GO TO 999

*  Store the good pixel centres in IPFIL.
         CALL KPS1_LMKGD( %VAL( CNF_PVAL( IPDAT ) ), NDIM, DIM, NP,
     :                    %VAL( CNF_PVAL( IPFIL ) ), STATUS )

*  Unmap the NDF's Data component.
         CALL NDF_UNMAP( INDF, 'DATA', STATUS )

*  Indicate that we should use the IPFIL pointer.
         USEFIL = .TRUE.

*  Otherwise, get up to MXPOS positions from the user.
      ELSE IF( STATUS .EQ. SAI__OK ) THEN

*  Loop until the array is full or an error occurs.
         NP = 0
         DO WHILE( NP .LT. MXPOS .AND. STATUS .EQ. SAI__OK )

            CC( 1 ) = AST__BAD
            CALL KPG1_GTPOS( 'POSITION', FRM, .FALSE., CC, BC, STATUS )

            IF( STATUS .EQ. SAI__OK ) THEN
               NP = NP + 1
               DO I = 1, NAX
                  POS( NP, I ) = CC( I )
               END DO

               CALL PAR_CANCL( 'POSITION', STATUS )

            END IF

         END DO

*  Annul the error if a null parameter value was supplied.
         IF( STATUS .EQ. PAR__NULL ) CALL ERR_ANNUL( STATUS )

*  Indicate that we should use the POS array.
         USEFIL = .FALSE.

      END IF

*  Concatenate the input positions with the new positions.
*  =======================================================

*  Find the total number of positions in the output positions list.
      NPOUT = NPIN + NP

*  Report an error if there are no output positions.
      IF( NPOUT .EQ. 0 .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'LISTMAKE_ERR2', 'Output positions list would '//
     :                 'be empty.', STATUS )
         GO TO 999
      END IF

*  Allocate room to hold the output positions and identifiers.
      CALL PSX_CALLOC( NAX*NPOUT, '_DOUBLE', IPPOS, STATUS )
      CALL PSX_CALLOC( NPOUT, '_INTEGER', IPID, STATUS )

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Copy any input positions and identifiers into the list, starting at
*  element 1.
      IF( NPIN .GT. 0 ) THEN
         CALL KPS1_LMKST( NAX, NPIN, NPIN, %VAL( CNF_PVAL( IPW1 ) ), 0,
     :                    %VAL( CNF_PVAL( IPIDIN ) ), 1, NPOUT,
     :                    %VAL( CNF_PVAL( IPPOS ) ),
     :                    %VAL( CNF_PVAL( IPID ) ), STATUS )
      END IF

*  Copy any new positions into the list, following the input positions.
*  Identifiers increase monotonically following the highest identifier in
*  the input positions list.
      IF( NP .GT. 0 ) THEN
         IF( USEFIL ) THEN
            CALL KPS1_LMKST( NAX, NP, NP, %VAL( CNF_PVAL( IPFIL ) ),
     :                       ID0, 0,
     :                       NPIN + 1, NPOUT, %VAL( CNF_PVAL( IPPOS ) ),
     :                       %VAL( CNF_PVAL( IPID ) ), STATUS )
         ELSE
            CALL KPS1_LMKST( NAX, NP, MXPOS, POS, ID0, 0, NPIN + 1,
     :                       NPOUT, %VAL( CNF_PVAL( IPPOS ) ),
     :                       %VAL( CNF_PVAL( IPID ) ),
     :                       STATUS )
         END IF
      END IF

*  Get the title to store in the output positions list.
      IF( TITLE .EQ. ' ' .AND. INDF .NE. NDF__NOID ) THEN
         CALL NDF_CGET( INDF, 'TITLE', TITLE, STATUS )
      END IF
      IF( TITLE .EQ. ' ' ) TITLE = 'Output from LISTMAKE'
      CALL PAR_DEF0C( 'TITLE', TITLE, STATUS )

      IF( STATUS .NE. SAI__OK ) GO TO 999

      CALL PAR_GET0C( 'TITLE', TITLE, STATUS )
      IF( STATUS .EQ. PAR__NULL ) CALL ERR_ANNUL( STATUS )


*  Get the index of the Frame within IWCS in which we have positions.
      IFRM = AST_GETI( IWCS, 'CURRENT', STATUS )

*  Ensure the original Current Frame is re-instated.
      CALL AST_SETI( IWCS, 'CURRENT', ICURR0, STATUS )

*  Create the output positions list.
      CALL KPG1_WRLST( 'OUTCAT', NPOUT, NPOUT, NAX,
     :                 %VAL( CNF_PVAL( IPPOS ) ),
     :                 IFRM, IWCS, TITLE, 0, %VAL( CNF_PVAL( IPID ) ),
     :                 .FALSE., STATUS )

*  Shutdown procedure.
*  ===================
  999 CONTINUE

*  Release work arrays.
      IF( IPPOS .NE. 0 ) CALL PSX_FREE( IPPOS, STATUS )
      IF( IPID .NE. 0 ) CALL PSX_FREE( IPID, STATUS )
      IF( IPFIL .NE. 0 ) CALL PSX_FREE( IPFIL, STATUS )
      IF( IPIN .NE. 0 ) CALL PSX_FREE( IPIN, STATUS )
      IF( IPIDIN .NE. 0 ) CALL PSX_FREE( IPIDIN, STATUS )
      IF( IPW1 .NE. 0 ) CALL PSX_FREE( IPW1, STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  Give a contextual error message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'LISTMAKE_ERR3', 'LISTMAKE: Failed to create '//
     :                 'a positions list.', STATUS )
      END IF

      END
