      SUBROUTINE FINDOFF( STATUS )
*+
*  Name:
*     FINDOFF

*  Purpose:
*     Performs pattern-matching between position lists related by 
*     simple offsets.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL FINDOFF( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine is designed to determine which positions in many
*     unaligned and unlabeled lists match, subject to the condition
*     that the transformations between the lists are well modelled by
*     simple translations. 
*
*     The results from this routine are labelled position lists (one
*     for each input list) which may be used to complete image
*     registration using the REGISTER routine. The estimated offsets are
*     also reported.

*  Usage:
*     findoff inlist error outlist

*  ADAM Parameters:
*     COMPLETE = _DOUBLE (Read)
*        A completeness threshold for rejecting matched position 
*        list pairs. A completeness factor is estimated by counting the
*        number of objects in the overlap region of two lists, taking
*        the minimum of these two values (this adjusts for
*        incompleteness due to a different object detection threshold)
*        and comparing this with the number of objects actually
*        matched. Ideally a completeness of 1 should be found, the lower
*        this value the lower the quality of the match.
*        [0.5]
*     ERROR = _DOUBLE (Read)
*        The error in the X and Y positions. This value is used to
*        determine which positions match within an error box (SLOW) or
*        as a bin size (FAST). An inaccurate value may result in 
*        excessive false or null matches.
*        [1.0]
*     FAILSAFE = _LOGICAL (Read)
*        If FAST is TRUE then this parameter indicates whether the SLOW
*        algorithm is to be used when FAST fails.
*        [TRUE]
*     FAST = _LOGICAL (Read)
*        If TRUE then the FAST matching algorithm is used, otherwise
*        just the SLOW algorithm is used.
*        [TRUE]
*     INLIST = LITERAL (Read)
*        This parameter is used to access the names of the lists
*        which contain the positions and, if NDFNAMES is TRUE, the names
*        of the associated NDFs. If NDFNAMES is TRUE the names of the
*        position lists are assumed to be stored in the extension of the
*        NDFs (in the CCDPACK extension item CURRENT_LIST) and the names
*        of the NDFs themselves should be given in response (and may
*        include wildcards).
*
*        If NDFNAMES is FALSE then the actual names of the position
*        lists should be given. These may not use wildcards but may be
*        specified using indirection (other CCDPACK position list
*        processing routines will write the names of their results file
*        into files suitable for use in this manner) the indirection
*        character is "^". 
*     LOGFILE = FILENAME (Read)
*        Name of the CCDPACK logfile.  If a null (!) value is given for
*        this parameter then no logfile will be written, regardless of
*        the value of the LOGTO parameter.
*
*        If the logging system has been initialised using CCDSETUP
*        then the value specified there will be used. Otherwise, the
*        default is "CCDPACK.LOG".
*        [CCDPACK.LOG]
*     LOGTO = LITERAL (Read)
*        Every CCDPACK application has the ability to log its output
*        for future reference as well as for display on the terminal.
*        This parameter controls this process, and may be set to any
*        unique abbreviation of the following:
*           -  TERMINAL  -- Send output to the terminal only
*           -  LOGFILE   -- Send output to the logfile only (see the
*                           LOGFILE parameter)
*           -  BOTH      -- Send output to both the terminal and the
*                           logfile
*           -  NEITHER   -- Produce no output at all
*
*        If the logging system has been initialised using CCDSETUP
*        then the value specified there will be used. Otherwise, the
*        default is "BOTH".
*        [BOTH]
*     MAXDISP = _DOUBLE (Read)
*        This parameter gives the maximum acceptable displacement in 
*        pixels between the original alignment of the NDFs and the
*        alignment in which the objects are matched.  If frames have
*        to be displaced more than this value to obtain a match, the
*        match is rejected.  This will be of use when USEWCS is set 
*        and the alignment defined by the WCS components of the 
*        NDFs is fairly accurate.  It should be set to the maximum
*        expected inaccuracy in that alignment.  If null, arbitrarily 
*        large displacements are allowed, although note that a 
*        similar restriction is effectively imposed by setting the
*        RESTRICT parameter.
*        [!]
*     MINMATCH = _INTEGER (Read)
*        This parameter specifies the minimum number of positions
*        which must be matched for a comparison of two lists to be 
*        deemed successful.  Small values (especially less than 3) of
*        this parameter can lead to a high probability of false matches,
*        and are only advisable for very sparsely populated lists
*        and/or small values of the MAXDISP parameter (presumably in
*        conjunction with USEWCS).
*        [3]
*     MINSEP = _DOUBLE (Read)
*        Positions which are very close may cause false matches by being
*        within the error box of other positions. The value of this
*        parameter controls how close objects may be before they are
*        both rejected (this occurs before pattern-matching).
*        [Dynamic -- 5.0*ERROR]
*     NAMELIST = LITERAL (Read)
*        The name of a file to contain the names of the output
*        position lists. The names written to this file are those
*        generated using the expression given to the OUTLIST parameter.
*        This file may be used in an indirection expression to input
*        all the position lists output from this routine into another
*        routine (say REGISTER), if the associating position lists with
*        NDFs option is not being used.
*        [FINDOFF.LIS]
*     NDFNAMES = _LOGICAL (Read)
*        If TRUE then the routine will assume that the names of the
*        position lists are stored in the NDF CCDPACK extensions under
*        the item "CURRENT_LIST". The names will be present in the
*        extension if the positions were located using a CCDPACK
*        application (such as FINDOBJ). Using this facility allows the
*        transparent propagation of position lists through processing
*        chains.
*
*        If a global value for this parameter has been set using
*        CCDSETUP then that value will be used.
*        [TRUE]
*     OUTLIST = FILENAME (Write)
*        A list of names specifying the result files. These contain
*        labelled positions which can be used in registration.
*        The names of the lists may use modifications of the
*        input names (NDF names if available otherwise the names of the
*        position lists). So if you want to call the output lists
*        the same name as the input NDFs except to add a type use.
*
*           OUTLIST > *.find
*        
*        If no NDF names are given (NDFNAMES is FALSE) then if you want
*        to change the extension of the files (from ".find" to ".off"
*        in this case) use
*
*           OUTLIST > *|find|off|
*
*        Or alternatively you can use an explicit list of names.
*        These may use indirection elements as well as names separated
*        by commas.
*        [*]
*     OVERRIDE = LOGICAL (Read)
*        This parameter controls whether to continue and create an
*        incomplete solution. Such solutions will result when only a
*        subset of the input position lists have been matched.
*
*        This situation would ideally indicate that one, or more, of the
*        input lists are from positions not coincident with the others,
*        in which case it is perfectly legimate to proceed, however, the
*        more likely scenario is that they have too few positions and
*        have consequently been rejected.  
*        [TRUE]
*     RESTRICT = LOGICAL (Read)
*        This parameter determines whether the mapping between coordinate
*        systems of NDF pairs is used to restrict the choice of objects
*        to match with each other.  If set TRUE, then only objects
*        which would appear in the overlap of two frames if the
*        mapping determined by the WCS components of each NDF pair 
*        were accurate are tested for matching.  If it is set FALSE, then
*        no such restriction is made.
*
*        This parameter should therefore be set TRUE if the translational
*        part of the information about the coordinate frames in the 
*        WCS components is fairly accurate (especially in the case that
*        there are many objects and a small overlap), and FALSE if it 
*        is not.
*
*        This parameter is ignored if USEWCS is FALSE.
*        [FALSE]
*     USECOM = LOGICAL (Read)
*        This parameter specifies whether the completeness value will
*        be used to weight the number of matches between a pair, when
*        determining the graph connecting all input datasets. Using
*        a completeness weight increases the chance of selecting high
*        quality matches, but may reduce the chance of selecting matches
*        with the highest counts in favour of those with lower counts.
*        [TRUE]
*     USEWCS = LOGICAL (Read)
*        This parameter specifies whether the World Coordinate System
*        information in the NDF should be used to make guesses about
*        how to align the frames.  If set true, and if a mapping
*        between the coordinates of each pair of NDFs can be found in 
*        the domain of the Current coordinate frame of either of the pair
*        then this mapping is applied before the match is attempted.
*        Additionally, if the parameter RESTRICT is TRUE, the objects 
*        chosen for matching up between each pair of images are only 
*        those which would overlap in that pair if the WCS coordinate 
*        information in both NDFs were accurate.  No other restriction 
*        is made on the basis of this information however.  Since the
*        algorithms rely on shifting images in the X and Y directions
*        therefore, they will be capable of matching up objects even if 
*        there is a significant (if RESTRICT is TRUE) or an arbitrarily
*        large (if RESTRICT is FALSE) translational error in the 
*        coordinate system of the chosen domain, but other large errors
*        in the mapping (e.g. rotational alignment) will not be
*        tolerated.
*       
*        If this parameter is set FALSE, then object matching proceeds
*        on the assumption that the coordinate frames of all NDFs
*        are the same apart from a translation.
*
*        This parameter is ignored if NDFNAMES is false.
*        [TRUE]

*  Notes:
*     - Position list formats. 
*       
*       CCDPACK supports data in two formats.
*
*       CCDPACK format - the first three columns are interpreted as the
*       following.
*
*          - Column 1: an integer identifier
*          - Column 2: the X position
*          - Column 3: the Y position
*
*       The column one value must be an integer and is used to identify
*       positions which are the same but which have different locations
*       on different images. Values in any other (trailing) columns are
*       usually ignored.
*
*       EXTERNAL format - positions are specified using just an X 
*       and a Y entry and no other entries.
*
*          - Column 1: the X position
*          - Column 2: the Y position
*
*       This format is used by KAPPA applications such as CURSOR.
*
*       Comments may be included in a file using the characters "#" and
*       "!". Columns may be separated by the use of commas or spaces.
*
*     - NDF extension items. 
*
*       If NDFNAMEs is TRUE then the names of the input position lists
*       will be gotten from the item "CURRENT_LIST" of the CCDPACK 
*       extension of the input NDFs. On exit this item will be updated
*       to contain the name of the appropriate output lists.

*  Notes on Algorithms:
*     The pattern-matching process uses two main algorithms, one which
*     matches all the point pair-offsets between any two input lists, 
*     looking for the matches with the most common positions, and one 
*     which uses a statistical method based on a histogram of the 
*     differences in the offsets (where the peak in the histogram is 
*     assumed the most likely difference). In each case an estimate of 
*     the positional error must be given as it is used when deciding 
*     which positions match (given an offset) or is used as the bin 
*     size when forming histograms.
*
*     Which algorithm you should use depends on the number of points
*     your positions lists contain and the expected size of the overlaps
*     between the datasets. Obviously it is much easier to detect two
*     lists with most of their positions in common. With small overlaps
*     a serious concern is the likelihood of finding a `false' match.
*     False matches must be more likely the larger the datasets and the
*     smaller the overlap.
*
*     The first algorithm (referred to as SLOW) is more careful and is
*     capable of selecting out positions when small overlaps in the
*     data are present (although a level of false detections will
*     always be present) but the process is inherently slow (scaling as
*     n**3log2(n)).  The second algorithm (referred to as FAST) is an
*     n*n process so is much quicker, but requires much better
*     overlapping.
*
*     Because the FAST process takes so little CPU time it is better to
*     try this first (without the SLOW process as a backup), only use
*     the SLOW algorithm when you have small datasets and do not
*     expect large areas (numbers of positions) of overlap.
*
*     A third algorithm, referred to as SNGL, is used automatically if 
*     one or both of the lists in a pair contains only a single object.
*     In this case object matching is trivial and, of course, may 
*     easily be in error.  SNGL can only be used if the MINMATCH
*     parameter has been set to 1, which should be done with care.  The
*     SNGL algorithm may be useful if there really is only one object, 
*     correctly identified, in all the frames.  If this is not the 
*     case, it should only be used when USEWCS is true and MAXDISP is 
*     set to a low value, indicating that the alignment of the NDFs in
*     the Current frames of their WCS components is already fairly 
*     accurate.
*
*     The global registration process works by forming a graph with
*     each position list at a node and with connecting edges of weight
*     the number of matched position-pairs. The edge weights may be
*     modified by a completeness factor which attempts to assess the
*     quality of the match (this is based on the ratio of the expected
*     number of matches in the overlap region to the actual number,
*     random matches shouldn't return good statistics when compared
*     with genuine ones). This still leaves a possibility of false
*     matches disrupting any attempt to register the datasets so a
*     single "spanning tree" is chosen (this is a graph which just
*     visits each node the minimum number of times required to get
*     complete connectivity, no loops allowed) which has the highest
*     possible number of matched positions (rejecting edges with few
*     matched positions/low completenesses where possible). This gives
*     a most likely solution to the offsets between the position lists,
*     rather than the "best" solution which could well include false
*     matches; compare this solution with a median as opposed to a
*     mean. The final registration is then used to identify all the
*     objects which are the same in all datasets (using a relaxation
*     method), resulting in labelled position lists which are output
*     for use by REGISTER.

*  Examples:
*     findoff inlist='*' error=1 outlist='*.off'
*        In this example all the NDFs in the current directory are
*        accessed and their associated position lists are used.  
*        Before object matching is attempted any transformations 
*        implied by aligning the images in the Current frames
*        of the NDFs' WCS components are applied to the position list
*        coordinates.  The matched position lists are named *.off.
*        The method used is to try the FAST algorithm, switching to SLOW 
*        if FAST fails. The completeness measure is used when forming 
*        the spanning tree.  Matches with completenesses less than 
*        0.5 and or with less than three positions, are rejected.
*
*     findoff fast nofailsafe
*        In this example the only the FAST algorithm is used.
*
*     findoff usecom=false
*        In this example the completeness factor is derived but not used
*        to weight the edges of the spanning tree.
*
*     findoff error=0.002 minsep=0.008
*        In this example very precise measurements (or small units)
*        are being used. The intrinsic error in the measurements is
*        around 0.002 and positions within a box 0.008 of each other are
*        rejected.
*
*     findoff inlist='data*' outlist='*.off' restrict=true
*        This form would be used if the NDFs 'data*' have WCS components
*        which are already approximately correct.  The Current domain 
*        of each should be set to the domain in which they are all 
*        (approximately) registered.  Setting the RESTRICT parameter
*        then tells FINDOFF to consider only objects in the region which 
*        the WCS components say ought to overlap between pairs of frames.
*        This can save a lot of time if there are many objects and a 
*        small overlap, but will result in failure of the program if 
*        the WCS components are not translationally registered 
*        reasonably well.
*
*     findoff inlist='data*' outlist='*.off' restrict minmatch=2
*             maxdisp=20 minsep=30
*        In this example the NDFs are sparsely populated, and a pair 
*        will be considered to match if as few as two matching objects
*        can be found.  The NDFs have been initially aligned in the 
*        Current frames of their WCS components to an accuracy of 20
*        or better.  As an additional safeguard, no objects within 
*        30 pixels of each other in the same NDF are used for matching.

*  Behaviour of parameters:
*     Most parameters retain their current value as default. The
*     "current" value is the value assigned on the last run of the
*     application. If the application has not been run then the
*     "intrinsic" defaults, as shown in the parameter help, apply.
*
*     Retaining parameter values has the advantage of allowing you to
*     define the default behaviour of the application but does mean
*     that additional care needs to be taken when re-using the
*     application after a break of sometime. The intrinsic default
*     behaviour of the application may be restored by using the RESET
*     keyword on the command line.
*
*     Certain parameters (LOGTO, LOGFILE and NDFNAMES) have global
*     values. These global values will always take precedence, except
*     when an assignment is made on the command line.  Global values may
*     be set and reset using the CCDSETUP and CCDCLEAR commands.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     29-SEP-1992 (PDRAPER):
*        Original version.
*     12-MAY-1993 (PDRAPER):
*        Added new techniques for SLOW.
*     6-OCT-1995 (PDRAPER):
*        Updated for CCDPACK 2.0.
*     16-SEP-1996 (PDRAPER):
*        Removed all NAG calls.
*     3-MAR-1997 (PDRAPER):
*        Removed top-level locator control (foreign data access upgrade).
*     23-MAR-1998 (PDRAPER):
*        Changed to open input formatted files as required (rather than
*        all at once). This works around the FIO limit of 40 open files
*        and achieves the CCDPACK limit of 100 instead.
*     16-DEC-1998 (PDRAPER):
*        Added OVERRIDE parameter to control the behaviour when
*        only some of the datasets are paired.
*     20-JAN-1999 (PDRAPER):
*        Sorted out workspace problems for incomplete matching
*        (IPBEEN and IPQUE increased to NOPEN from NMATCH).
*     30-MAR-1999 (MBT):
*        Changed to deal with WCS components of NDFs.
*     26-APR-1999 (MBT):
*        Now erases .MORE.CCPACK.CURRENT_LIST component for unmatched
*        NDFs.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS/DAT parameterisations
      INCLUDE 'FIO_PAR'          ! FIO parameters
      INCLUDE 'CCD1_PAR'         ! CCDPACK parameterisations
      INCLUDE 'AST_PAR'          ! AST constants
      INCLUDE 'PAR_ERR'          ! PAR error codes

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 12 ) DOMAIN ! WCS domain used for provisional registration
      CHARACTER * ( CCD1__BLEN ) LINE ! Line buffer for reading in data
      CHARACTER * ( CCD1__BLEN ) LINE1 ! Line buffer for writing out text
      CHARACTER * ( CCD1__BLEN ) LINE2 ! Line buffer for writing out text
      CHARACTER * ( DAT__SZLOC ) LOCEXT ! HDS locator for CCDPACK extension
      CHARACTER * ( 4 ) METHOD  ! Last method attempted for object matching
      CHARACTER * ( FIO__SZFNM ) FNAME ! Buffer to store filenames
      DOUBLE PRECISION BNDX( 4, CCD1__MXLIS ) ! X coords of bounding boxes
      DOUBLE PRECISION BNDY( 4, CCD1__MXLIS ) ! Y coords of bounding boxes
      DOUBLE PRECISION CBNDX( 4 ) ! Converted X coords of list 2 bounding box
      DOUBLE PRECISION CBNDY( 4 ) ! Converted Y coords of list 2 bounding box
      DOUBLE PRECISION COMFAC   ! Completeness factor
      DOUBLE PRECISION ERROR    ! Error in input positions
      DOUBLE PRECISION MINSEP   ! Minimum input data separation
      DOUBLE PRECISION MAXDIS   ! Maximum displacement for matching
      DOUBLE PRECISION NEDFAC   ! Minimum completeness factor required
      DOUBLE PRECISION XOFF( CCD1__MXLIC ) ! Determined X translation
      DOUBLE PRECISION XOFFN( CCD1__MXLIS ) ! Final X translation
      DOUBLE PRECISION YOFF( CCD1__MXLIC ) ! Determined Y translation
      DOUBLE PRECISION YOFFN( CCD1__MXLIS ) ! Final Y translation
      INTEGER CNV               ! AST mapping between frames of compared lists
      INTEGER COUNT             ! Dummy loop counter
      INTEGER FDIN              ! Input FIO descriptor
      INTEGER FDOUT             ! Output FIO descriptor
      INTEGER FIOGR             ! Input IRH group identifier
      INTEGER FRBASE            ! Base frame of WCS frameset
      INTEGER FSCNV             ! AST pointer to framet containing mapping
      INTEGER I                 ! Loop variable
      INTEGER IAT               ! Position in CHR_ string
      INTEGER IDIN              ! NDF identifier
      INTEGER IPBEEN            ! Pointer to workspace
      INTEGER IPDAT             ! Pointer to input data
      INTEGER IPGRA             ! Pointer to graph
      INTEGER IPID( CCD1__MXLIS ) ! Pointer to Output X positions
      INTEGER IPIND             ! Pointer to input indices in file 1
      INTEGER IPQUE             ! Pointer to workspace
      INTEGER IPRAN1            ! Pointer to sort ranks (w/s)
      INTEGER IPRAN2            ! Pointer to sort ranks (w/s)
      INTEGER IPRBN1            ! Intermediate pointer to sort ranks
      INTEGER IPRBN2            ! Intermediate pointer to sort ranks
      INTEGER IPSPAN            ! Pointer to graph (spanning)
      INTEGER IPSUB             ! Pointer to sub-graph (spanning)
      INTEGER IPWRK1            ! Workspace pointers
      INTEGER IPWRK2            ! Workspace pointers
      INTEGER IPWRK3            ! Workspace pointers
      INTEGER IPWRK4            ! Workspace pointers
      INTEGER IPWRK5            ! Workspace pointers
      INTEGER IPX( CCD1__MXLIS ) ! Pointer to out/input X positions
      INTEGER IPXC2             ! Pointer to list 2 X coords in list 1 space
      INTEGER IPXI1             ! Pointer to list 1 X coords in list 2 box
      INTEGER IPXI2             ! Pointer to list 2 X coords in list 1 box
      INTEGER IPYC2             ! Pointer to list 2 Y coords in list 1 space
      INTEGER IPYI1             ! Pointer to list 1 Y coords in list 2 box
      INTEGER IPYI2             ! Pointer to list 2 Y coords in list 1 box
      INTEGER IPXN              ! Pointer to input X positions
      INTEGER IPXO1( CCD1__MXLIC ) ! Pointer to output X positions
      INTEGER IPXO2( CCD1__MXLIC ) ! Pointer to output X positions
      INTEGER IPY( CCD1__MXLIS ) ! Pointer to out/input Y positions
      INTEGER IPYN              ! Pointer to input Y positions
      INTEGER IPYO1( CCD1__MXLIC ) ! Pointer to output Y positions
      INTEGER IPYO2( CCD1__MXLIC ) ! Pointer to output Y positions
      INTEGER J                 ! Loop variable
      INTEGER JBAS1             ! Index of Base frame in AST frameset 1
      INTEGER JBAS2             ! Index of Base frame in AST frameset 2
      INTEGER JCUR              ! Index of Current frame in AST frameset
      INTEGER JPIX              ! Index of frame in Pixel domain
      INTEGER LBND( 2 )         ! Lower pixel-index bounds of NDF 
      INTEGER LOOPS             ! Number of comparison loops
      INTEGER MINMAT            ! Minimum number of positions for match
      INTEGER NDFGR             ! Input NDF IRG group
      INTEGER NDIM              ! Number of dimensions in NDF
      INTEGER NEDGES            ! Number of edges in graph
      INTEGER NEWED             ! Number of edges in spanning graph
      INTEGER NMAT( CCD1__MXLIC ) ! Number of matched positions
      INTEGER NMATCH            ! Number of matches
      INTEGER NNODE             ! Number of nodes in spanning graph
      INTEGER NOPEN             ! Number of input files opened
      INTEGER NOUT( CCD1__MXLIS ) ! Number of output positions
      INTEGER NPOSS             ! Number of possible point pairs
      INTEGER NREC( CCD1__MXLIS ) ! Number of records
      INTEGER NRECN             ! Current number of records 
      INTEGER NRET              ! Dummy variable
      INTEGER NUMI1             ! Number of list 1 points in list 2 box
      INTEGER NUMI2             ! Number of list 2 points in list 1 box
      INTEGER NVAL( CCD1__MXLIS ) ! Number of values per-record
      INTEGER OFFS( CCD1__MXLIS + 1 ) ! Offsets into extended lists
      INTEGER OUTGRP            ! Output IRH group identifier
      INTEGER TOTNOD            ! Total number of nodes in graph
      INTEGER UBND( 2 )         ! Upper pixel-index bounds of NDF
      INTEGER WCS( CCD1__MXLIS ) ! AST WCS pointers
      LOGICAL ALLOK             ! Trur no input positions removed in preselection phase
      LOGICAL COMPL             ! True if graph is complete
      LOGICAL CYCLIC            ! True if graph is cyclic
      LOGICAL FAILED            ! True if FAST and FSAFe are true and error occurs during match
      LOGICAL FAST              ! True if n**2 method used
      LOGICAL FSAFE             ! True if n**4 method used if n**2 fails
      LOGICAL NDFS              ! True if position list names are stored in NDF extensions
      LOGICAL OK                ! Match is ok
      LOGICAL RSTRCT            ! True if only objects in overlap considered
      LOGICAL USECOM            ! Use completeness measure as a weight
      LOGICAL USEWCS            ! True if we will attempt to use WCS extension
      LOGICAL OVERRD            ! True if partial selection is allowed
      LOGICAL PAIRED( CCD1__MXNDF ) ! Whether list is paired with someone

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start the CCDPACK logging system.
      CALL CCD1_START( 'FINDOFF', STATUS )

*  Begin NDF context.
      CALL NDF_BEGIN( STATUS )

*  Begin AST context.
      CALL AST_BEGIN( STATUS )

*  Find out what is to be used for the source of the position list
*  names. Are they stored in NDF extensions or will just straight list
*  names be given.
      NDFS = .TRUE.
      CALL PAR_GET0L( 'NDFNAMES', NDFS, STATUS )

*  Get the lists of of positions.
      CALL CCD1_GTLIG( NDFS, 'CURRENT_LIST', 'INLIST', 1, CCD1__MXLIS, 
     :                 NOPEN, FIOGR, NDFGR, STATUS )

*  Get the error in the position measurements.
      CALL PAR_GET0D( 'ERROR', ERROR, STATUS )

*  Get the minimum separation of points which is allowed. This should be
*  several times the point spread function (error).
      CALL PAR_DEF0D( 'MINSEP', ERROR * 5.0D0, STATUS )
      CALL PAR_GET0D( 'MINSEP', MINSEP, STATUS )

*  Get the maximum displacement allowed between frames, relative to
*  their original positioning, either using WCS information or not.
*  A zero value of MAXDIS represents a null value of the parameter.
      IF ( STATUS .NE. SAI__OK ) GO TO 99
      CALL PAR_GET0D( 'MAXDISP', MAXDIS, STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN
         MAXDIS = 0D0
         CALL ERR_ANNUL( STATUS )
      END IF

*  Get the minimum completeness factor which is ok.
      CALL PAR_GET0D( 'COMPLETE', NEDFAC, STATUS )
      NEDFAC = MAX( 0.0D0, MIN( NEDFAC, 1.0D0 ) )

*  Is the completeness to be used to weight the edges when picking out
*  the spanning graph?
      CALL PAR_GET0L( 'USECOMP', USECOM, STATUS )

*  Get the minimum number of positions which are required for a match.
      CALL PAR_GET0I( 'MINMATCH', MINMAT, STATUS )
      MINMAT = MAX( 1, MINMAT )

*  Get the operation mode. FAST which uses the statistical analysis or
*  .NOT.FAST which uses the straight-forward distance comparison.
      FAST = .FALSE.
      CALL PAR_GET0L( 'FAST', FAST, STATUS )

*  If the mode is fast does the user want to use the none fast option
*  when fast intercomparison fails?
      FSAFE = .FALSE.
      IF ( FAST ) CALL PAR_GET0L( 'FAILSAFE', FSAFE, STATUS )

*  See if we should continue with registration if only a few of the 
*  datasets have been matched.
      OVERRD = .FALSE.
      CALL PAR_GET0L( 'OVERRIDE', OVERRD, STATUS )

*  See if we should use WCS extension information in NDFs.
      IF ( NDFS ) THEN
         USEWCS = .TRUE.
         CALL PAR_GET0L( 'USEWCS', USEWCS, STATUS )
      ELSE
         USEWCS = .FALSE.
      END IF

*  See if we should use WCS information to restrict lists of objects
*  to be matched.
      RSTRCT = .FALSE.
      IF ( USEWCS ) CALL PAR_GET0L( 'RESTRICT', RSTRCT, STATUS )

*  Get World Coordinate System information from NDFs.  After this,
*  the WCS array will contain a pointer to a valid WCS frameset for 
*  each member of the group.
      DO 12 I = 1, NOPEN
         IF ( USEWCS ) THEN

*  Get pointer to WCS frameset.
            CALL IRG_NDFEX( NDFGR, I, IDIN, STATUS )
            CALL CCD1_GTWCS( IDIN, WCS( I ), STATUS )

*  Set Base frame to what is now the Current frame, and Current 
*  frame to a frame in the Pixel domain.  This has the effect that
*  later calls to AST_CONVERT trying to find a mapping between pairs
*  of WCS framesets, if one of the fields in the domainlist is empty,
*  will convert between Pixel domains and via one or other of the 
*  domains which were current as submitted to the application.
*  This is the behaviour advertised.
            JCUR = AST_GETI( WCS( I ), 'Current', STATUS )
            CALL CCD1_FRDM( WCS( I ), 'Pixel', JPIX, STATUS )
            CALL AST_SETI( WCS( I ), 'Base', JCUR, STATUS )

*  Get bounding box: BNDX and BNDY contain the X and Y pixel coordinates
*  of the corners of the NDF's DATA array, for the purpose of determining
*  where overlaps are expected once WCS information has been obtained.  
*  They must be listed in BNDX and BNDY in a clockwise, or anti-clockwise, 
*  order.  They are modified here by the ERROR parameter so that pixels 
*  outside the box by that distance are considered for matching.
            IF ( RSTRCT ) THEN
               CALL NDF_BOUND( IDIN, 2, LBND, UBND, NDIM, STATUS )
               BNDX( 1, I ) = DBLE( LBND( 1 ) - 1 ) - ERROR
               BNDX( 2, I ) = DBLE( UBND( 1 ) ) + ERROR
               BNDX( 3, I ) = DBLE( UBND( 1 ) ) + ERROR
               BNDX( 4, I ) = DBLE( LBND( 1 ) - 1 ) - ERROR
               BNDY( 1, I ) = DBLE( LBND( 2 ) - 1 ) - ERROR
               BNDY( 2, I ) = DBLE( LBND( 2 ) - 1 ) - ERROR
               BNDY( 3, I ) = DBLE( UBND( 2 ) ) + ERROR
               BNDY( 4, I ) = DBLE( UBND( 2 ) ) + ERROR
            END IF

         ELSE 
            WCS( I ) = AST__NULL
         END IF

 12   CONTINUE

*  Output names of the positions lists and labels.
      CALL CCD1_MSG( ' ', ' ', STATUS )
      CALL CCD1_MSG( ' ', '    Input position lists:', STATUS )
      CALL CCD1_MSG( ' ', '    ---------------------', STATUS )
      DO 6 I = 1, NOPEN
         CALL IRH_GET( FIOGR, I, 1, FNAME, STATUS )
         CALL MSG_SETC( 'FNAME', FNAME )
         CALL MSG_SETI( 'N', I )
         CALL CCD1_MSG( ' ', '  ^N) ^FNAME', STATUS )
 6    CONTINUE

*  Where the position list names originated.
      IF ( NDFS ) THEN 
         CALL CCD1_MSG( ' ',
     :'  Position list names extracted from NDF extensions.', STATUS )

*  Write the names of the associated NDFs out to the user.
         CALL CCD1_MSG( ' ', ' ', STATUS )
         CALL CCD1_MSG( ' ', '    Associated NDFs:', STATUS )
         CALL CCD1_MSG( ' ', '    ----------------', STATUS )
         DO 7 I = 1, NOPEN
            CALL IRH_GET( NDFGR, I, 1, FNAME, STATUS )
            CALL MSG_SETC( 'FNAME', FNAME )
            CALL MSG_SETI( 'N', I )
            CALL CCD1_MSG( ' ', '  ^N) ^FNAME', STATUS )
 7       CONTINUE
      END IF

*  Report the initial parameters.
      CALL CCD1_MSG( ' ', ' ', STATUS )
      CALL CCD1_MSG( ' ', '     Initial parameters', STATUS )
      CALL CCD1_MSG( ' ', '     ------------------', STATUS )

*  And the rest of the parameters.
      CALL MSG_SETD( 'ERROR', ERROR )
      CALL CCD1_MSG( '  ', '  Error in positions: ^ERROR', STATUS )
      IF ( MAXDIS .NE. 0D0 ) THEN
         CALL MSG_SETD( 'MAXDIS', MAXDIS )
         CALL CCD1_MSG( ' ', 
     :    '  Maximum displacement allowed: ^MAXDIS', STATUS )
      ELSE
         CALL CCD1_MSG( ' ', 
     :    '  Arbitrarily large displacements allowed', STATUS )
      END IF
      CALL MSG_SETD( 'MINSEP', MINSEP )
      CALL CCD1_MSG( '  ',
     : '  Minimum distance between positions: ^MINSEP', STATUS )
      CALL MSG_SETI( 'MINMAT', MINMAT )
      CALL CCD1_MSG( ' ', '  Minimum number of positions required'//
     : ' for positive match: ^MINMAT', STATUS )
      CALL MSG_SETD( 'COMPL', NEDFAC )
      CALL CCD1_MSG( ' ', '  Minimum completeness level'//
     : ' for positive match: ^COMPL', STATUS )
      IF ( USECOM ) THEN
         CALL CCD1_MSG( ' ','  Completeness estimates will be used to'//
     :   ' weight connections', STATUS )
      ELSE
         CALL CCD1_MSG( ' ','  Completeness estimates will not be'//
     :   ' used to weight connections', STATUS )
      END IF
      IF ( USEWCS ) THEN
         CALL CCD1_MSG( ' ', '  WCS information will be used for'//
     :   ' provisional alignment', STATUS )
      ELSE
         CALL CCD1_MSG( ' ', '  WCS information will not be used for'//
     :   ' provisional alignment', STATUS )
      END IF
      IF ( RSTRCT ) THEN 
         CALL CCD1_MSG( ' ', '  Attempted matches will be'//
     :   ' restricted to apparent overlap zones', STATUS )
      ELSE 
         CALL CCD1_MSG( ' ', '  All objects will be considered'//
     :   ' for possible matches', STATUS )
      END IF

*  What sort of comparison will be performed.
      IF ( FAST ) THEN 
         CALL CCD1_MSG( ' ', '  Using FAST matching algorithm', STATUS )
         IF ( FSAFE ) THEN
            CALL CCD1_MSG( ' ', '  Failsafe in operation', STATUS )
         ELSE
            CALL CCD1_MSG( ' ', '  Failsafe not in operation', STATUS )
         END IF
      ELSE
         CALL CCD1_MSG( ' ', '  Using SLOW matching algorithm',
     :   STATUS )
      END IF

*=======================================================================
*  Data extraction section. Reject positions which are too close.
*=======================================================================
      CALL CCD1_MSG( ' ', ' ', STATUS )
      CALL CCD1_MSG( ' ','    Data PRESELECTION', STATUS )
      CALL CCD1_MSG( ' ','    -----------------', STATUS )
      CALL CCD1_MSG( ' ', ' ', STATUS )

*  Set flag to indicate that no positions have been unremoved.
      ALLOK = .TRUE.

*  Extract the data present in the input files.
      DO 2 I = 1, NOPEN 

*  Open the input files and test the number of entries.
         CALL IRH_GET( FIOGR, I, 1, FNAME, STATUS )
         CALL CCD1_OPFIO( FNAME, 'READ', 'LIST', 0, FDIN, STATUS )
         CALL CCD1_LTEST( FDIN, LINE, CCD1__BLEN, 2, 0, NVAL( I ), 
     :                    STATUS )
         IF ( NVAL( I ) .EQ. 2 ) THEN 

*  Map in X and Y positions only (non-standard file)
            CALL CCD1_NLMAP( FDIN, LINE, CCD1__BLEN, IPDAT, NREC( I ), 
     :                       NVAL( I ), STATUS )
         ELSE

*  Standard file format map these in.
            CALL CCD1_LMAP( FDIN, LINE, CCD1__BLEN, IPIND, IPDAT, 
     :                      NREC( I ), NVAL( I ), STATUS )
            CALL CCD1_MFREE( IPIND, STATUS )            
         END IF

*  Check file has at least MINMAT values, otherwise it isn't possible
*  to proceed.
         IF ( NREC( I ) .LT. MINMAT ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'FNAME', FNAME )
            CALL MSG_SETI( 'NREC', NREC( I ) )
            CALL MSG_SETI( 'MINMAT', MINMAT )
            CALL ERR_REP( 'TOOFEW','  The file ''^FNAME'' '//
     :'only contains ^NREC positions. At least ^MINMAT are required '//
     :'per file.',STATUS )
            CALL CCD1_MFREE( IPDAT, STATUS )
            GO TO 99
         END IF

*  Get workspace for storing the X and Y values.
         CALL CCD1_MALL( NREC( I ), '_DOUBLE', IPX( I ), STATUS )
         CALL CCD1_MALL( NREC( I ), '_DOUBLE', IPY( I ), STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Extract the values from the mapped data array.
         CALL CCD1_LEXT( %VAL( IPDAT ), NREC( I ), NVAL( I ), 1,
     :                   %VAL( IPX( I ) ), STATUS )
         CALL CCD1_LEXT( %VAL( IPDAT ), NREC( I ), NVAL( I ), 2,
     :                   %VAL( IPY( I ) ), STATUS )

*  Ok. Now select which of these point will be considered for matching.
*  Remove points which are too close.
         IF ( STATUS .NE. SAI__OK ) GO TO 99
         CALL CCD1_MALL( NREC( I ), '_INTEGER', IPRAN1, STATUS )
         CALL CCD1_MALL( NREC( I ), '_DOUBLE', IPXN, STATUS )
         CALL CCD1_MALL( NREC( I ), '_DOUBLE', IPYN, STATUS )
         CALL CCD1_PRMIN( %VAL( IPX( I ) ), %VAL( IPY( I ) ), NREC( I ),
     :                    MINSEP, %VAL( IPXN ), %VAL( IPYN ),
     :                    %VAL( IPRAN1 ), NRECN, STATUS )

*  If any points have been reject, report this and set ALLOK false
*  to indicate that at least some input positions have been rejected.
         IF ( NREC( I ) .NE. NRECN ) THEN
            ALLOK = .FALSE.
            CALL MSG_SETI( 'NREJ', NREC( I ) - NRECN )
            CALL MSG_SETI( 'N', I )
            CALL CCD1_MSG( ' ',
     :'  ^NREJ positions too close in list ^N); positions removed',
     : STATUS )

*  Free unused workspace.
            CALL CCD1_MFREE( IPX( I ), STATUS )
            CALL CCD1_MFREE( IPY( I ), STATUS )

*  Swap pointers to new data.
            IPX( I ) = IPXN
            IPY( I ) = IPYN
            NREC( I ) = NRECN
         ELSE

*  Release unused workspace.
            CALL CCD1_MFREE( IPXN, STATUS )
            CALL CCD1_MFREE( IPYN, STATUS )
         END IF

*  Release memory no longer in use.
         CALL CCD1_MFREE( IPRAN1, STATUS )
         CALL CCD1_MFREE( IPDAT, STATUS )

*  And close the file.
         CALL FIO_CLOSE( FDIN, STATUS )
 2    CONTINUE
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Comment if no positions have been removed.
      IF ( ALLOK )
     :   CALL CCD1_MSG( ' ',
     :   '  No positions removed in pre-selection phase', STATUS )

*=======================================================================
*  Main loop for intercomparisons of position data.
*=======================================================================
      CALL CCD1_MSG( ' ', ' ', STATUS )
      CALL CCD1_MSG( ' ', '    INTERCOMPARISON and MATCHING', STATUS )
      CALL CCD1_MSG( ' ', '    ----------------------------', STATUS )

*  Determine the number of loops we will perform.
      LOOPS = 0
      DO I = 1, NOPEN - 1
        DO J = I + 1, NOPEN
           LOOPS = LOOPS + 1
        END DO
      END DO

*  Inform user of the number of intercomparisons.
      CALL CCD1_MSG( ' ', ' ', STATUS )
      CALL MSG_SETI( 'LOOPS', LOOPS )
      CALL CCD1_MSG( ' ', '  No. of intercomparisons: ^LOOPS', STATUS )
      CALL CCD1_MSG( ' ', ' ', STATUS )

*  Set and output header labels.
      LINE1 = '    List  List  No. matches  Completeness  Status   '
      LINE2 = '    ----  ----  -----------  ------------  ------   '
      IF ( ( FAST .AND. FSAFE ) .OR. MINMAT .EQ. 1 ) THEN
         LINE1( 53: ) = 'Algorithm  '
         LINE2( 53: ) = '---------  '
      END IF
      IF ( USEWCS ) THEN
         LINE1( 64: ) = 'WCS domain  '
         LINE2( 64: ) = '----------  '
      END IF
      CALL CCD1_MSG( ' ', LINE1, STATUS )
      CALL CCD1_MSG( ' ', LINE2, STATUS )
         
*  Loop comparing each list with all the lists which follow it.
      COUNT = 0 
      NMATCH = 0
      DO 3 I = 1, NOPEN - 1
         DO 4 J = I + 1, NOPEN 

*  Increment counters and set number of matched positions.
            COUNT = COUNT + 1
            NMATCH = NMATCH + 1
            NMAT( COUNT ) = 0

*  Is this match ok?
            OK = .TRUE.
            FAILED = .FALSE.
            METHOD = ' '

*  Now get workspace arrays for matching routines (plus 2 for CCD1_SOFF
*  sorting routines).
            NPOSS = NREC( I ) * ( NREC( J ) + 2 )
            CALL CCD1_MALL( NPOSS, '_DOUBLE', IPWRK1, STATUS )

*  Workspace for the output X and Y values.
            CALL CCD1_MALL( NREC( I ), '_DOUBLE', IPXO1( COUNT ),
     :                      STATUS )
            CALL CCD1_MALL( NREC( I ), '_DOUBLE', IPYO1( COUNT ),
     :                      STATUS )
            CALL CCD1_MALL( NREC( J ), '_DOUBLE', IPXO2( COUNT ),
     :                      STATUS )
            CALL CCD1_MALL( NREC( J ), '_DOUBLE', IPYO2( COUNT ),
     :                      STATUS )

*  And for remembering the original positions in input data sets.
            CALL CCD1_MALL( NREC( I ), '_INTEGER', IPRAN1, STATUS )
            CALL CCD1_MALL( NREC( J ), '_INTEGER', IPRAN2, STATUS )
            CALL CCD1_MALL( NREC( I ), '_INTEGER', IPRBN1, STATUS )
            CALL CCD1_MALL( NREC( J ), '_INTEGER', IPRBN2, STATUS )

*  Check memory allocations succeeded.
            IF ( STATUS .NE. SAI__OK ) GO TO 99

            IF ( USEWCS ) THEN

*  Store the Base frame indices since the call to AST_CONVERT will 
*  overwrite them.
               JBAS1 = AST_GETI( WCS( I ), 'Base', STATUS )
               JBAS2 = AST_GETI( WCS( J ), 'Base', STATUS )

*  Try to obtain an AST mapping between coordinate systems (Current and
*  Base frames have previously been set for each frameset so that an 
*  empty domainlist achieves the desired effect).
               FSCNV = AST_CONVERT( WCS( J ), WCS( I ), ' ', STATUS )
               CNV = AST_GETMAPPING( FSCNV, AST__BASE, AST__CURRENT, 
     :                               STATUS )
               CALL AST_ANNUL( FSCNV, STATUS )

*  Store the domain in which the alignment actually occurred.
               FRBASE = AST_GETFRAME( WCS( I ), AST__BASE, STATUS )
               DOMAIN = AST_GETC( FRBASE, 'Domain', STATUS )
               CALL AST_ANNUL( FRBASE, STATUS ) 

*  Restore the original Base frames to their former values.
               CALL AST_SETI( WCS( I ), 'Base', JBAS1, STATUS )
               CALL AST_SETI( WCS( J ), 'Base', JBAS2, STATUS )

            ELSE
               CNV = AST__NULL 
            END IF

*  Check whether mapping was obtained successfully.
            IF ( CNV .EQ. AST__NULL ) THEN

*  We are not making use of WCS information.  
*  Set up arrays as if there is a unit mapping between the coordinate 
*  systems of the two lists, with no bounding box restrictions.
               IPXI1 = IPX( I )
               IPYI1 = IPY( I )
               IPXI2 = IPX( J )
               IPYI2 = IPY( J ) 
               IPXC2 = IPX( J )
               IPYC2 = IPY( J )
               NUMI1 = NREC( I )
               NUMI2 = NREC( J )

*  Fill sort lists with unit mapping (since they are mapping from one
*  list to itself).
               CALL CCD1_GISEQ( 1, 1, NUMI1, %VAL( IPRBN1 ), STATUS )
               CALL CCD1_GISEQ( 1, 1, NUMI2, %VAL( IPRBN2 ), STATUS )

            ELSE

*  We are making use of WCS information.
*  Allocate temporary workspace for all X and Y coordinates of points
*  in list J, converted to the coordinate system of list I.
               CALL CCD1_MALL( NREC( J ), '_DOUBLE', IPXC2, STATUS )
               CALL CCD1_MALL( NREC( J ), '_DOUBLE', IPYC2, STATUS )

*  Allocate space for X and Y coordinates of points in both lists, in
*  the same coordinate system (that of list I), to contain only those
*  points in the (WCS-estimated) overlap between the two sets.
               CALL CCD1_MALL( NREC( I ), '_DOUBLE', IPXI1, STATUS )
               CALL CCD1_MALL( NREC( I ), '_DOUBLE', IPYI1, STATUS )
               CALL CCD1_MALL( NREC( J ), '_DOUBLE', IPXI2, STATUS )
               CALL CCD1_MALL( NREC( J ), '_DOUBLE', IPYI2, STATUS )

*  Check memory allocations succeeded.
               IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Map positions in list J to coordinate system of list I.
               CALL AST_TRAN2( CNV, NREC( J ), 
     :                         %VAL( IPX( J ) ), %VAL( IPY( J ) ),
     :                         .TRUE., %VAL( IPXC2 ), %VAL( IPYC2 ),
     :                         STATUS )

               IF ( RSTRCT ) THEN

*  Map bounding box vertices applying to list J into coordinate system
*  of list I.
                  CALL AST_TRAN2( CNV, 4, BNDX( 1, J ), BNDY( 1, J ),
     :                            .TRUE., CBNDX, CBNDY, STATUS )

*  Select only points in list I which fall inside bounding box of list J.
                  CALL CCD1_INPLY( CBNDX, CBNDY, 4,
     :                             %VAL( IPX( I ) ), %VAL( IPY( I ) ),
     :                             NREC( I ), %VAL( IPXI1 ), 
     :                             %VAL( IPYI1 ), %VAL( IPRBN1 ), 
     :                             NUMI1, STATUS )

*  Select only points in list J which fall inside bounding box of list I.
                  CALL CCD1_INPLY( BNDX( 1, I ), BNDY( 1, I ), 4,
     :                             %VAL( IPXC2 ), %VAL( IPYC2 ),
     :                             NREC( J ), %VAL( IPXI2 ), 
     :                             %VAL( IPYI2 ), %VAL( IPRBN2 ), 
     :                             NUMI2, STATUS )

               ELSE

*  No restrictions on which points to consider - copy all to the 
*  arrays for matching.
                  CALL CCG1_COPAD( NREC( I ), %VAL( IPX( I ) ), 
     :                             %VAL( IPXI1 ), STATUS )
                  CALL CCG1_COPAD( NREC( I ), %VAL( IPY( I ) ),
     :                             %VAL( IPYI1 ), STATUS )
                  CALL CCG1_COPAD( NREC( J ), %VAL( IPXC2 ), 
     :                             %VAL( IPXI2 ), STATUS )
                  CALL CCG1_COPAD( NREC( J ), %VAL( IPYC2 ),
     :                             %VAL( IPYI2 ), STATUS )
                  NUMI1 = NREC( I )
                  NUMI2 = NREC( J )

               END IF

            END IF

*  Check if we have enough points in each list to satisfy the matching
*  criteria.
            IF ( NUMI1 .LT. MINMAT .OR. NUMI2 .LT. MINMAT )
     :         OK = .FALSE.

*  Generate the offset statistics.
            IF ( STATUS .NE. SAI__OK ) GO TO 99
            IF ( ( NUMI1 .EQ. 1 .OR. NUMI2 .EQ. 1 ) .AND. OK ) THEN

*  One or both lists have only one object.  Treat as a special case.
               METHOD = 'SNGL'
               CALL CCD1_SNGL( MAXDIS,
     :                         %VAL( IPXI1 ), %VAL( IPYI1 ),
     :                         %VAL( IPRBN1 ), NUMI1,
     :                         %VAL( IPXI2 ), %VAL( IPYI2 ),
     :                         %VAL( IPRBN2 ), NUMI2,
     :                         %VAL( IPXO1( COUNT ) ),
     :                         %VAL( IPYO1( COUNT ) ),
     :                         %VAL( IPXO2( COUNT ) ),
     :                         %VAL( IPYO2( COUNT ) ), NMAT( COUNT ),
     :                         XOFF( COUNT ), YOFF( COUNT ),
     :                         %VAL( IPRAN1 ), %VAL( IPRAN2 ), STATUS )

*  If the match was successful, completeness must be unity.
               IF ( STATUS .EQ. SAI__OK ) THEN
                  COMFAC = 1D0
               ELSE
                  COMFAC = 0D0
               END IF

            ELSE IF ( FAST .AND. OK ) THEN

*  Perform matching using histogram of X and Y offsets refined through
*  iteration.
               METHOD = 'FAST'
               CALL CCD1_STAO( ERROR, MAXDIS,
     :                         %VAL( IPXI1 ), %VAL( IPYI1 ), 
     :                         %VAL( IPRBN1 ), NUMI1,
     :                         %VAL( IPXI2 ), %VAL( IPYI2 ),
     :                         %VAL( IPRBN2 ), NUMI2,
     :                         %VAL( IPWRK1 ),
     :                         %VAL( IPXO1( COUNT ) ),
     :                         %VAL( IPYO1( COUNT ) ),
     :                         %VAL( IPXO2( COUNT ) ),
     :                         %VAL( IPYO2( COUNT ) ), NMAT( COUNT ),
     :                         XOFF( COUNT ), YOFF( COUNT ),
     :                         %VAL( IPRAN1 ), %VAL( IPRAN2 ), STATUS )
               IF ( STATUS .NE. SAI__OK ) THEN
               
*  This mode has failed. If failsafe is set set FAILED to TRUE. Annul
*  the error and continue in any case.
                  IF ( FSAFE ) THEN
                     FAILED = .TRUE.
                  ELSE

*  Match has failed.
                    OK = .FALSE.
                  END IF
                  CALL ERR_ANNUL( STATUS )
               ELSE

*  Match is a success. If failsafe is in operation then check that this
*  has also passed the other success criteria, such as minimum number
*  and completeness.
                  IF ( FSAFE ) THEN 

*  Now do the test.
                     CALL CCD1_OVCOM( %VAL( IPX( I ) ),
     :                                %VAL( IPY( I ) ), NREC( I ),
     :                                %VAL( IPXC2 ),
     :                                %VAL( IPYC2 ), NREC( J ),
     :                                NMAT( COUNT ), XOFF( COUNT ),
     :                                YOFF( COUNT ), ERROR, COMFAC,
     :                                STATUS )

*  Now check for minimum number match and completeness. Set failed if
*  this match fails now.
                     IF ( COMFAC .LT. NEDFAC .OR.
     :                    NMAT( COUNT ) .LT. MINMAT ) THEN
                        FAILED = .TRUE.
                     END IF
                  END IF
               END IF
            END IF
                  
*  Perform matching using the straight-forward distance comparisons
*  if this is required.
            IF ( ( .NOT. FAST .OR. ( FSAFE .AND. FAILED ) ) 
     :         .AND. METHOD .NE. 'SNGL' .AND. OK ) THEN 
            
*  Get workspace and perform comparison.
               METHOD = 'SLOW'
               CALL CCD1_MALL( NPOSS, '_DOUBLE', IPWRK2, STATUS )
               CALL CCD1_MALL( NPOSS, '_INTEGER', IPWRK3, STATUS )
               CALL CCD1_MALL( NREC( J ) + 2, '_INTEGER', IPWRK4, 
     :                         STATUS )
               CALL CCD1_MALL( NREC( J ) + 2, '_INTEGER', IPWRK5, 
     :                         STATUS )
               CALL CCD1_SOFF( ERROR, MAXDIS, 
     :                         %VAL( IPXI1 ), %VAL( IPYI1 ),
     :                         %VAL( IPRBN1 ), NUMI1,
     :                         %VAL( IPXI2 ), %VAL( IPYI2 ),
     :                         %VAL( IPRBN2 ), NUMI2,
     :                         %VAL( IPWRK1 ), %VAL( IPWRK2 ),
     :                         %VAL( IPWRK3 ), %VAL( IPWRK4 ),
     :                         %VAL( IPWRK5 ),
     :                         %VAL( IPXO1( COUNT ) ),
     :                         %VAL( IPYO1( COUNT ) ),
     :                         %VAL( IPXO2( COUNT ) ),
     :                         %VAL( IPYO2( COUNT ) ), NMAT( COUNT ),
     :                         XOFF( COUNT ), YOFF( COUNT ),
     :                         %VAL( IPRAN1 ), %VAL( IPRAN2 ), STATUS )
               CALL CCD1_MFREE( IPWRK2, STATUS )
               CALL CCD1_MFREE( IPWRK3, STATUS )
               CALL CCD1_MFREE( IPWRK4, STATUS )
               CALL CCD1_MFREE( IPWRK5, STATUS )
            END IF
            IF ( STATUS .NE. SAI__OK ) THEN

*  Match has failed.
               CALL ERR_ANNUL( STATUS )
               OK = .FALSE.
            END IF

*  Now (re-) determine the completeness of the match. If no positions
*  have been matched then set the completeness factor to 0.
            IF ( .NOT. OK ) THEN
               COMFAC = 0.0D0
            ELSE

*  Now do the test.
               CALL CCD1_OVCOM( %VAL( IPX( I ) ), %VAL( IPY( I ) ), 
     :                          NREC( I ), %VAL( IPXC2 ), 
     :                          %VAL( IPYC2 ), NREC( J ), 
     :                          NMAT( COUNT ), XOFF( COUNT ), 
     :                          YOFF( COUNT ), ERROR, COMFAC, STATUS )
            END IF

*  Final check for ok match.
            IF ( OK ) THEN
               IF ( COMFAC .LT. NEDFAC .OR.
     :              NMAT( COUNT ) .LT. MINMAT ) THEN
                 OK = .FALSE.
               END IF
            END IF

*  Write information about this loop.
            CALL CHR_ITOC( I, LINE( 6: ), IAT )
            CALL CHR_ITOC( J, LINE( 12: ), IAT )
            CALL CHR_ITOC( NMAT( COUNT ), LINE( 19: ), IAT )
            CALL CHR_RTOC( REAL( COMFAC ), LINE( 30: ), IAT )
            IF ( OK ) THEN
                LINE( 44: ) = 'accepted'
            ELSE
                LINE( 44: ) = 'rejected'
            END IF
            IF ( ( FAST .AND. FSAFE ) .OR. MINMAT .EQ. 1 )
     :         LINE( 56: ) = METHOD
            IF ( CNV .NE. AST__NULL ) LINE( 64: ) = DOMAIN

*  And write the line of information
            CALL CCD1_MSG( ' ', LINE, STATUS )

*  If matching process failed then reset values for this pass and
*  release workspace which will not now be used.
            IF ( .NOT. OK ) THEN 
               NMAT( COUNT ) = 0
               NMATCH = NMATCH - 1
               CALL CCD1_MFREE( IPXO1( COUNT ), STATUS )
               CALL CCD1_MFREE( IPYO1( COUNT ), STATUS )
               CALL CCD1_MFREE( IPXO2( COUNT ), STATUS )
               CALL CCD1_MFREE( IPYO2( COUNT ), STATUS )
            ELSE 

*  If using completeness as a weight modify the number of matches.
               IF ( USECOM ) THEN 
                  NMAT( COUNT ) = NINT( DBLE( NMAT( COUNT ) ) * COMFAC )
                  NMAT( COUNT ) = MAX( 1, NMAT( COUNT ) )
               END IF

*  If using WCS information, construct list of matched positions in the
*  original coordinate frame from the lists which are in transformed
*  frames.  We use IPXC2 and IPYC2 as workspace and leave the result
*  where the original values were, in IPXO2 and IPYO2.
               IF ( CNV .NE. AST__NULL ) THEN
                  CALL AST_TRAN2( CNV, NMAT( COUNT ), 
     :                            %VAL( IPXO2( COUNT ) ),
     :                            %VAL( IPYO2( COUNT ) ), .FALSE.,
     :                            %VAL( IPXC2 ), %VAL( IPYC2 ), STATUS )
                  CALL CCD1_MFREE( IPXO2( COUNT ), STATUS )
                  CALL CCD1_MFREE( IPYO2( COUNT ), STATUS )
                  IPXO2( COUNT ) = IPXC2
                  IPYO2( COUNT ) = IPYC2
               END IF

            END IF

*  Free resources used for handling WCS information.
            CALL CCD1_MFREE( IPRBN1, STATUS )
            CALL CCD1_MFREE( IPRBN2, STATUS )
            IF ( CNV .NE. AST__NULL ) THEN
               CALL AST_ANNUL( CNV, STATUS )
               CALL CCD1_MFREE( IPXI1, STATUS )
               CALL CCD1_MFREE( IPYI1, STATUS )
               CALL CCD1_MFREE( IPXI2, STATUS )
               CALL CCD1_MFREE( IPYI2, STATUS )
            END IF

*  Now compare next set of positions.
            CALL CCD1_MFREE( IPWRK1, STATUS )
            CALL CCD1_MFREE( IPRAN1, STATUS )
            CALL CCD1_MFREE( IPRAN2, STATUS )
 4       CONTINUE 
 3    CONTINUE    

*  End AST context.
      CALL AST_END( STATUS )

*  End NDF context.
      CALL NDF_END( STATUS )
                  
*  Comment on the success or overwise of the intercomparisons. If no
*  intercomparisons were successful set status and abort, otherwise, if
*  requested, push on to check the connectivity of graph of
*  intercomparisons. To make this test definitive we need to check all
*  lists, as some may be matched more than once.
      DO 10 I = 1, NOPEN
         PAIRED( I ) = .FALSE.
 10   CONTINUE
      COUNT = 0
      DO 8 I = 1, NOPEN - 1
         DO 9 J = I + 1, NOPEN 
            COUNT = COUNT + 1
            IF ( NMAT( COUNT ) .NE. 0 ) THEN
               PAIRED( I ) = .TRUE.
               PAIRED( J ) = .TRUE.
            END IF
 9       CONTINUE
 8    CONTINUE
      OK = .TRUE.
      DO 11 I = 1, NOPEN
         IF ( .NOT. PAIRED( I ) ) OK = .FALSE.
 11   CONTINUE
      IF ( .NOT. OK ) THEN
         IF ( NMATCH .EQ. 0 ) THEN
            STATUS = SAI__ERROR
            CALL CCD1_ERREP( 'TOTAL_FAILURE',
     :'  No positions were matched between any dataset', STATUS )
            GO TO 99
         ELSE     
            IF ( OVERRD ) THEN 
               CALL CCD1_MSG( ' ', ' ', STATUS )
               CALL CCD1_MSG( ' ',
     :'  Warning - not all position datasets have been successfully'//
     :' matched. Continuing with those that have.', STATUS) 
               CALL CCD1_MSG( ' ', ' ', STATUS )
            ELSE
               STATUS = SAI__ERROR
               CALL CCD1_ERREP( 'TOTAL_FAILURE',
     :'  Not all position datasets have been successfully matched', 
     :                          STATUS )
               GO TO 99
            END IF
         END IF   
      END IF      

*  If not all the position lists have been matched but we are going to 
*  continue, we should remove the associated position lists from any
*  NDFs which have not been matched, otherwise casual use of them by 
*  later applications may give the impression that the original position
*  lists are matched ones.
      IF ( .NOT. OK .AND. NDFS ) THEN
         DO 13 I = 1, NOPEN
            IF ( .NOT. PAIRED( I ) ) THEN
               CALL IRG_NDFEX( NDFGR, I, IDIN, STATUS )
               CALL CCD1_CEXT( IDIN, .FALSE., 'UPDATE', LOCEXT, STATUS )
               CALL DAT_ERASE( LOCEXT, 'CURRENT_LIST', STATUS )
               CALL CCD1_MSG( ' ', 
     :'  Removing associated position list from CCDPACK extension of',
     :                        STATUS )
               CALL NDF_MSG( 'NDF', IDIN )
               CALL CCD1_MSG( ' ', '      unmatched NDF ^NDF.', STATUS )
               CALL CCD1_MSG( ' ', ' ', STATUS )
            END IF
 13      CONTINUE
      END IF
                  
*=======================================================================
*  End of data intercomparison and offset estimation
*=======================================================================
*  Spanning graph determination section
*=======================================================================
      CALL CCD1_MSG( ' ', ' ', STATUS )
      CALL CCD1_MSG( ' ','    CONNECTIONS and OFFSETS', STATUS )
      CALL CCD1_MSG( ' ','    -----------------------', STATUS )
      CALL CCD1_MSG( ' ', ' ', STATUS )

*  Get workspace for the graph testing section.
      CALL CCD1_MALL( NMATCH * 4, '_INTEGER', IPGRA, STATUS )
      CALL CCD1_MALL( NMATCH * 4, '_INTEGER', IPSPAN, STATUS )
      CALL CCD1_MALL( NMATCH * 4, '_INTEGER', IPSUB, STATUS )
      CALL CCD1_MALL( MAX( NOPEN, NMATCH ), '_INTEGER', IPQUE, STATUS )
      CALL CCD1_MALL( MAX( NOPEN, NMATCH ), '_LOGICAL', IPBEEN, STATUS )
                           
*  Look for a maximum likelihood span of the graph of positions (each
*  position is treated as a node, the node-node transformations are
*  the edges with weights the number of positions matched).
*  First create the graph. 
      CALL CCD1_CRGR( NMAT, COUNT, NOPEN, %VAL( IPGRA ), NEDGES,
     :                STATUS )
                           
*  Call routine to determine if the graph is complete.
      CALL CCD1_GRAPC( %VAL( IPGRA ), NEDGES, 1, %VAL( IPQUE ),
     :                 %VAL( IPBEEN ), COMPL, CYCLIC, %VAL( IPSUB ),
     :                 NEWED, TOTNOD, STATUS )
      IF ( COMPL ) THEN    
                           
*  Graph is complete -- all nodes connected. Determine the most likely
*  spanning sub-graph. The most likely one is the graph which is most
*  strongly connected (largest count of matched pairs).
         CALL CCD1_MLSPG( %VAL( IPGRA ), NEDGES, TOTNOD, %VAL( IPQUE ),
     :                    %VAL( IPBEEN ), %VAL( IPSPAN ),
     :                    %VAL( IPSUB ), NEWED, NNODE, STATUS )

*  Determine the "complete" solution.
*  Find the offsets of all positions to the `reference' set (first
*  node of first edge of spanning graph is assumed to be the reference
*  set).
         CALL CCD1_GROFF( %VAL( IPSUB ), NEWED, XOFF, YOFF,
     :                    NOPEN, %VAL( IPBEEN ), %VAL( IPQUE ),
     :                    XOFFN, YOFFN, STATUS )

*  Generate the ID's for the output lists. Matching positions between
*  the lists and final merging all positions for each node.
         CALL CCD1_GMMP( %VAL( IPSUB ), NEWED, NNODE, IPXO1, IPYO1,
     :                   IPXO2, IPYO2, NMAT, OFFS, IPX, IPY, IPID, NOUT,
     :                   STATUS )

      ELSE

*  Set STATUS and issue error (may change this to cope with error
*  by forming most likely connection).
         STATUS = SAI__ERROR
         CALL CCD1_ERREP( 'FINDOFF_NOTCON',
     :'  Intercomparison of positions does not produce a complete'//
     :' registration of all frames -- graph incomplete', STATUS )
         GO TO 99
      END IF

*  Free memory used in matching (this may have overflowed the internal 
*  resources of CCD1_MALL/MFREE, so need to be more careful than just 
*  using -1 in MFREE call).
      DO 1 I = 1, COUNT
         IF ( NMAT( I ) .NE. 0 ) THEN 
            CALL CCD1_MFREE( IPXO1( I ), STATUS )
            CALL CCD1_MFREE( IPYO1( I ), STATUS )
            CALL CCD1_MFREE( IPXO2( I ), STATUS )
            CALL CCD1_MFREE( IPYO2( I ), STATUS )
         END IF
 1    CONTINUE
      CALL CCD1_MFREE( IPGRA, STATUS )
      CALL CCD1_MFREE( IPSPAN, STATUS )
      CALL CCD1_MFREE( IPSUB, STATUS )
      CALL CCD1_MFREE( IPQUE, STATUS )
      CALL CCD1_MFREE( IPBEEN, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99 

*=======================================================================
*   End of spanning graph section
*=======================================================================
*   Writing output lists and updating NDF extensions
*=======================================================================
*  Get the output position lists file names. Use the input NDF names
*  as possible modification elements when possible otherwise use the
*  input position list names.
      IF ( NDFS ) THEN 
         CALL CCD1_STRGR( 'OUTLIST', NDFGR, NOPEN, NOPEN, OUTGRP, NRET,
     :                    STATUS )
      ELSE
         CALL CCD1_STRGR( 'OUTLIST', FIOGR, NOPEN, NOPEN, OUTGRP, NRET,
     :                    STATUS )
      END IF

*  Tell user the names.
      CALL CCD1_MSG( ' ', ' ', STATUS )
      CALL CCD1_MSG( ' ', '    Output position lists:', STATUS )
      CALL CCD1_MSG( ' ', '    ----------------------', STATUS )

*  Create each of these files and write the appropriate information
*  to them.
      DO I = 1, NOPEN
         IF ( NOUT( I ) .GT. 0 ) THEN 
            CALL IRH_GET( OUTGRP, I, 1, FNAME, STATUS )
            CALL CCD1_OPFIO( FNAME, 'WRITE', 'LIST', 0, FDOUT,
     :                       STATUS )
            CALL CCD1_FIOHD( FDOUT, 'Output from FINDOFF', STATUS )
            CALL CCD1_WRIXY( FDOUT, %VAL( IPID( I ) ),
     :                       %VAL( IPX( I ) ), %VAL( IPY( I ) ),
     :                       NOUT( I ), LINE, CCD1__BLEN, STATUS )
            CALL FIO_CLOSE( FDOUT, STATUS )

*  Output name.
            CALL MSG_SETC( 'FNAME', FNAME )
            CALL CCD1_MSG( ' ', '  ^FNAME', STATUS )

*  If the names of the positions lists were accessed using NDF extension
*  information then update the extension.
            IF ( NDFS ) THEN 
               CALL IRG_NDFEX( NDFGR, I, IDIN, STATUS )
               CALL CCG1_STO0C( IDIN, 'CURRENT_LIST', FNAME, STATUS )

*  Close the NDF.
               CALL NDF_ANNUL( IDIN, STATUS )
            END IF
         END IF

*  Finally release the positions extracted from the input files.
         CALL CCD1_MFREE( IPID( I ), STATUS )
         CALL CCD1_MFREE( IPX( I ), STATUS )
         CALL CCD1_MFREE( IPY( I ), STATUS )
      END DO 

*  If the filenames were supplied directly then write an output list of
*  the names for other applications to use.
      IF ( .NOT. NDFS .AND. STATUS .EQ. SAI__OK ) THEN 

*  Write the names of the output files to a file which may be used for
*  indirection into other applications.
         CALL CCD1_LNAM( 'NAMELIST', 1, NOPEN,
     :   '# FINDOFF - output position lists', OUTGRP, .TRUE., STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
            CALL CCD1_MSG( ' ', '  No namelist written ', STATUS )
         END IF
      END IF

*  Abort on error label.
 99   CONTINUE

*  Close IRH
      CALL IRH_ANNUL( FIOGR, STATUS )
      IF ( NDFS ) CALL IRH_ANNUL( NDFGR, STATUS )
      CALL IRH_CLOSE( STATUS )

*  Free all workspace.
      CALL CCD1_MFREE( -1, STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL CCD1_ERREP( 'FINDOFF_ERR',
     :   'FINDOFF: Error determining position list matches',
     :   STATUS )
      END IF

*  Finally close the logging system.
      CALL CCD1_END( STATUS )

      END
* $Id$
