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
*     MINMATCH = _INTEGER (Read)
*        This parameter specifies the minimum number of positions
*        which must be matched for a comparison of two lists to be 
*        deemed successful. This must be greater than 2.
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
*     USECOM = LOGICAL (Read)
*        This parameter specifies whether the completeness value will
*        be used to weight the number of matches between a pair, when
*        determining the graph connecting all input datasets. Using
*        a completeness weight increases the chance of selecting high
*        quality matches, but may reduce the chance of selecting matches
*        with the highest counts in favour of those with lower counts.
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
*     The pattern-matching process uses two algorithms, one which
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
*        accessed and their associated position lists are used. The
*        matched positions are named *.off. The method used is to try 
*        the FAST algorithm, switching to SLOW if FAST fails. The
*        completeness measure is used when forming the spanning tree.
*        Matches with completenesses less than 0.5 and with less than
*        three positions are rejected.
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

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( CCD1__BLEN ) LINE ! Line buffer for reading in data
      CHARACTER * ( FIO__SZFNM ) FNAME ! Buffer to store filenames
      DOUBLE PRECISION COMFAC   ! Completeness factor
      DOUBLE PRECISION ERROR    ! Error in input positions
      DOUBLE PRECISION MINSEP   ! Minimum input data separation
      DOUBLE PRECISION NEDFAC   ! Minimum completeness factor required
      DOUBLE PRECISION XOFF( CCD1__MXLIC ) ! Determined X translation
      DOUBLE PRECISION XOFFN( CCD1__MXLIS ) ! Final X translation
      DOUBLE PRECISION YOFF( CCD1__MXLIC ) ! Determined Y translation
      DOUBLE PRECISION YOFFN( CCD1__MXLIS ) ! Final Y translation
      INTEGER COUNT             ! Dummy loop counter
      INTEGER FDIN              ! Input FIO descriptor
      INTEGER FDOUT             ! Output FIO descriptor
      INTEGER FIOGR             ! Input IRH group identifier
      INTEGER I                 ! Loop variable
      INTEGER IAT               ! Dummy 
      INTEGER IDIN              ! NDF identifier
      INTEGER IPBEEN            ! Pointer to workspace
      INTEGER IPDAT             ! Pointer to input data
      INTEGER IPGRA             ! Pointer to graph
      INTEGER IPID( CCD1__MXLIS ) ! Pointer to Output X positions
      INTEGER IPIND             ! Pointer to input indices in file 1
      INTEGER IPQUE             ! Pointer to workspace
      INTEGER IPRAN1            ! Pointer to sort ranks (w/s)
      INTEGER IPRAN2            ! Pointer to sort ranks (w/s)
      INTEGER IPSPAN            ! Pointer to graph (spanning)
      INTEGER IPSUB             ! Pointer to sub-graph (spanning)
      INTEGER IPWRK1            ! Workspace pointers
      INTEGER IPWRK2            ! Workspace pointers
      INTEGER IPWRK3            ! Workspace pointers
      INTEGER IPWRK4            ! Workspace pointers
      INTEGER IPWRK5            ! Workspace pointers
      INTEGER IPX( CCD1__MXLIS ) ! Pointer to out/input X positions
      INTEGER IPXN              ! Pointer to input X positions
      INTEGER IPXO1( CCD1__MXLIC ) ! Pointer to output X positions
      INTEGER IPXO2( CCD1__MXLIC ) ! Pointer to output X positions
      INTEGER IPY( CCD1__MXLIS ) ! Pointer to out/input Y positions
      INTEGER IPYN              ! Pointer to input Y positions
      INTEGER IPYO1( CCD1__MXLIC ) ! Pointer to output Y positions
      INTEGER IPYO2( CCD1__MXLIC ) ! Pointer to output Y positions
      INTEGER J                 ! Loop variable
      INTEGER LOOPS             ! Number of comparison loops
      INTEGER MINMAT            ! Minimum number of positions for match
      INTEGER NDFGR             ! Input NDF IRG group
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
      INTEGER NVAL( CCD1__MXLIS ) ! Number of values per-record
      INTEGER OFFS( CCD1__MXLIS + 1 ) ! Offsets into extended lists
      INTEGER OUTGRP            ! Output IRH group identifier
      INTEGER TOTNOD            ! Total number of nodes in graph
      LOGICAL ALLOK             ! Trur no input positions removed in preselection phase
      LOGICAL COMPL             ! True if graph is complete
      LOGICAL CYCLIC            ! True if graph is cyclic
      LOGICAL FAILED            ! True if FAST and FSAFe are true and error occurs during match
      LOGICAL FAST              ! True if n**2 method used
      LOGICAL FSAFE             ! True if n**4 method used if n**2 fails
      LOGICAL NDFS              ! True if position list names are stored in NDF extensions
      LOGICAL OK                ! Match is ok
      LOGICAL USECOM            ! Use completeness measure as a weight
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start the CCDPACK logging system.
      CALL CCD1_START( 'FINDOFF', STATUS )

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

*  Get the minimum completeness factor which is ok.
      CALL PAR_GET0D( 'COMPLETE', NEDFAC, STATUS )
      NEDFAC = MAX( 0.0D0, MIN( NEDFAC, 1.0D0 ) )

*  Is the completeness to be used to weight the edges when picking out
*  the spanning graph?
      CALL PAR_GET0L( 'USECOMP', USECOM, STATUS )

*  Get the minimum number of positions which are required for a match.
      CALL PAR_GET0I( 'MINMATCH', MINMAT, STATUS )
      MINMAT = MAX( 2, MINMAT )

*  Get the operation mode. FAST which uses the statistical analysis or
*  .NOT.FAST which uses the straight-forward distance comparison.
      FAST = .FALSE.
      CALL PAR_GET0L( 'FAST', FAST, STATUS )

*  If the mode is fast does the user want to use the none fast option
*  when fast intercomparison fails?
      FSAFE = .FALSE.
      IF ( FAST ) CALL PAR_GET0L( 'FAILSAFE', FSAFE, STATUS )

*  Name of the positions lists and labels.
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
      CALL MSG_SETD( 'MINSEP', MINSEP )
      CALL CCD1_MSG( '  ',
     : '  Minimum distance between positions: ^MINSEP', STATUS )
      CALL MSG_SETI( 'MINMAT', MINMAT )
      CALL CCD1_MSG( ' ', '  Minimum number of positions required'//
     : ' for positive match: ^MINMAT', STATUS )
      CALL MSG_SETD( 'COMPL', NEDFAC )
      CALL CCD1_MSG( ' ', '  Minimum completeness level'//
     : ' for positive match: ^COMPL', STATUS )
      IF (  USECOM ) THEN
         CALL CCD1_MSG( ' ','  Completeness estimates will be used to'//
     :   ' weight connections', STATUS )
      ELSE
         CALL CCD1_MSG( ' ','  Completeness estimates will not be'//
     :   ' used to weight connections', STATUS )
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

*  Set header labels and offsets.
      IF ( FAST .AND. FSAFE ) THEN
         CALL CCD1_MSG( ' ',
     :'    List  List  No. of matches   Completeness    Status   '//
     : 'Algorithm',
     : STATUS )
         CALL CCD1_MSG( ' ',
     :'    ----  ----  --------------   ------------    ------   '//
     : '---------',
     : STATUS )
      ELSE
         CALL CCD1_MSG( ' ',
     :'    List  List  No. of matches   Completeness    Status   ',
     : STATUS )
         CALL CCD1_MSG( ' ',
     :'    ----  ----  --------------   ------------    ------   ',
     : STATUS )
      END IF

*  Loop comparing each list with all the lists which follow it.
      COUNT = 0 
      NMATCH = 0
      DO 3 I = 1, NOPEN -1
         DO 4 J = I + 1, NOPEN 

*  Increment counters and set number of matched positions.
            COUNT = COUNT + 1
            NMATCH = NMATCH + 1
            NMAT( COUNT ) = 0

*  Is this match ok?
            OK = .TRUE.

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

*  Generate the offset statistics.
            IF ( STATUS .NE. SAI__OK ) GO TO 99
            FAILED = .FALSE.
            IF ( FAST ) THEN 
            
*  Perform matching using histogram of X and Y offsets refined through
*  iteration.
               CALL CCD1_STAO( ERROR,
     :                         %VAL( IPX( I ) ), %VAL( IPY( I ) ),
     :                         NREC( I ),
     :                         %VAL( IPX( J ) ), %VAL( IPY( J ) ),
     :                         NREC( J ),
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
     :                                %VAL( IPX( J ) ), 
     :                                %VAL( IPY( J ) ), NREC( J ), 
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
            IF ( .NOT. FAST .OR. ( FSAFE .AND. FAILED ) ) THEN 
            
*  Get workspace and perform comparison.
               CALL CCD1_MALL( NPOSS, '_DOUBLE', IPWRK2, STATUS )
               CALL CCD1_MALL( NPOSS, '_INTEGER', IPWRK3, STATUS )
               CALL CCD1_MALL( NREC( J ) + 2, '_INTEGER', IPWRK4, 
     :                         STATUS )
               CALL CCD1_MALL( NREC( J ) + 2, '_INTEGER', IPWRK5, 
     :                         STATUS )
               CALL CCD1_SOFF( ERROR,
     :                         %VAL( IPX( I ) ), %VAL( IPY( I ) ),
     :                         NREC( I ),
     :                         %VAL( IPX( J ) ), %VAL( IPY( J ) ),
     :                         NREC( J ),
     :                         %VAL( IPWRK1 ), %VAL( IPWRK2 ),
     :                         %VAL( IPWRK3 ), %VAL( IPWRK4 ),
     :                         %VAL( IPWRK5 ),
     :                         %VAL( IPXO1( COUNT ) ),
     :                         %VAL( IPYO1( COUNT ) ),
     :                         %VAL( IPXO2( COUNT ) ),
     :                         %VAL( IPYO2( COUNT ) ), NMAT( COUNT ),
     :                         XOFF( COUNT ), YOFF( COUNT ),
     :                         %VAL( IPRAN1 ), %VAL( IPRAN2 ),
     :                         STATUS )
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
     :                          NREC( I ), %VAL( IPX( J ) ),
     :                          %VAL( IPY( J ) ), NREC( J ),
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
            CALL CHR_ITOC( NMAT( COUNT ), LINE( 22: ), IAT )
            CALL CHR_RTOC( REAL( COMFAC ), LINE( 34: ), IAT )
            IF ( OK ) THEN
                LINE( 49: ) = 'accepted'
            ELSE
                LINE( 49: ) = 'rejected'
            END IF
            IF ( FAST .AND. FSAFE ) THEN
               IF ( FAILED ) THEN
                  LINE( 62: ) = 'SLOW'
               ELSE
                  LINE( 62: ) = 'FAST'
               END IF
            END IF

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
            ELSE IF ( USECOM ) THEN 

*  If using completeness as a weight modify the number of matches.
               NMAT( COUNT ) = NINT( DBLE( NMAT( COUNT ) ) * COMFAC )
               NMAT( COUNT ) = MAX( 1, NMAT( COUNT ) )
            END IF

*  Now compare next set of positions.
            CALL CCD1_MFREE( IPWRK1, STATUS )
            CALL CCD1_MFREE( IPRAN1, STATUS )
            CALL CCD1_MFREE( IPRAN2, STATUS )
 4       CONTINUE 
 3    CONTINUE    
                  
*  Comment on the success or overwise of the intercomparisons. If no
*  intercomparisons were successful set status and abort, otherwise
*  push on to check the connectivity of graph of intercomparisons.
      IF ( NMATCH .LT. NOPEN - 1 ) THEN
         IF ( NMATCH .EQ. 0 ) THEN
            STATUS = SAI__ERROR
            CALL CCD1_ERREP( 'TOTAL_FAILURE',
     :'  No positions were matched between any dataset', STATUS )
            GO TO 99
         ELSE     
            CALL CCD1_MSG( ' ', ' ', STATUS )
            CALL CCD1_MSG( ' ',
     :'  Warning - not all position datasets have been successfully'//
     :' matched. Continuing those which have.', STATUS) 
            CALL CCD1_MSG( ' ', ' ', STATUS )
         END IF   
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
      CALL CCD1_MALL( NMATCH, '_INTEGER', IPQUE, STATUS )
      CALL CCD1_MALL( NMATCH, '_LOGICAL', IPBEEN, STATUS )
                           
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
     :   'FINDOFF: Error determining positions matches',
     :   STATUS )
      END IF

*  Finally close the logging system.
      CALL CCD1_END( STATUS )

      END
* $Id$
