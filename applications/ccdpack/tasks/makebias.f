      SUBROUTINE MAKEBIAS( STATUS )
*+
*  Name:
*     MAKEBIAS

*  Purpose:
*     Produces a master from a set of bias frames.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL MAKEBIAS( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine processes a series of bias frames (stored in NDFs),
*     so as to produce a single "master bias" frame in which the noise
*     levels are reduced. This master bias frame can then be used to
*     de-bias other CCD frames (using DEBIAS). Using the given readout
*     noise an, optional, variance component may be produced for the
*     output data. The use of a variance component allows the effects
*     of noise in bias subtraction to be properly monitored.
*
*     MAKEBIAS also performs other functions during processing, such as
*     estimating the readout noise (which it displays for comparison
*     with the nominal value), estimating the data levels, zeroing the
*     average value of the input data before combination (to more
*     closely follow any drifts in the zero level) and also supports
*     many different methods for performing the bias-frame data
*     combination. The combination methods offer a mixture of very
*     robust (median) to very efficient (mean) estimators.

*  Usage:
*     makebias in out rnoise method  { alpha=?
*                                    { sigmas=? niter=?
*                                    { niter=?
*                                    { min=? max=?

*  ADAM Parameters:
*     ALPHA = _REAL (Read)
*        The fraction of extreme values to remove before combining
*        the data at any pixel. This fraction is removed from each
*        extreme so can only take a value in the range 0 to 0.5.
*        Only used if METHOD="TRIMMED"
*        [0.2]
*     GENVAR = _LOGICAL (Read)
*        If TRUE then a variance component representative of the
*        readout noise will be generated. If FALSE then no variance
*        component will be generated. If a variance component is not
*        generated then any future estimates of variance made using the
*        output NDF will be underestimates, however, disk space savings
*        can be made using this option, if future error analyses are
*        not important. If this parameter is set FALSE then a readout
*        noise estimate will not be requested.
*
*        If a global value has been set using CCDSETUP this value
*        will be used, and will be shown as the default.
*        [FALSE]
*     IN = LITERAL (Read)
*        A list of NDF names which contain the raw bias frame data.
*        The NDF names should be separated by commas and may include wildcards.
*     KEEPIN = _LOGICAL (Read)
*        Whether to keep (i.e. not delete) the input NDFs or not.
*        Deleting the input NDFs has the advantage of saving disk
*        space, but since the NDFs input to this routine are raw data
*        files (rather than processed intermediary files) they should be
*        always be keep unless space considerations are at a very high
*        premium.
*
*        The default for this parameter is TRUE and this cannot be
*        overridden except by assignment on the command line or in
*        reponse to a forced prompt.
*        [TRUE]
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
*     MAX = _REAL (Read)
*        If METHOD = "THRESH" then this value defines the upper limit
*        for values which can be used when combining data. Note that the
*        value used for this parameter will not be corrected for zero
*        pointing. Hence if the output NDF is to be zeroed then the
*        maximum value should be a offset from zero (say some positive
*        number 2 or 3 sigmas large). This could be used as a form of
*        sigma clipping if no variances are to be generated.
*     METHOD = LITERAL (Read)
*        The method to be used to combine the data components of
*        the input NDFs. This may be set to any unique abbreviation of
*        the following:
*           -  MEAN      -- Mean of the input data values
*           -  MEDIAN    -- Weighted median of the input data values
*           -  TRIMMED   -- An "alpha trimmed mean" in which a fraction
*                           alpha of the values are removed from
*                           each extreme
*           -  MODE      -- An iteratively "sigma clipped" mean which
*                           approximates to the modal value
*           -  SIGMA     -- A sigma clipped mean
*           -  THRESHOLD -- Mean with values above and below given
*                           limits removed
*           -  MINMAX    -- Mean with the highest and lowest values
*                           removed
*           -  BROADENED -- A broadened median (the mean of a small
*                           number of central values)
*           -  CLIPMED   -- A sigma clipped median (like SIGMA except
*                           that the median of the clipped values is used)
*           -  FASTMED   -- Unweighted median of the input data values
*        [MEDIAN]
*     MIN = _REAL (Read)
*        If METHOD = "THRESH" then this value defines the lower limit
*        for values which can be used when combining the data. Note that
*        the value used for this parameter will not be corrected for zero
*        pointing. Hence if the output NDF is to be zeroed then the
*        minimum value should be a offset from zero (say some negative
*        number 2 or 3 sigmas large). This could be used as a form of
*        sigma clipping if no variances are to be generated.
*     MINPIX = _INTEGER (Read)
*        The minimum number of good (i.e. not BAD) pixels required
*        to contribute to the value of an output pixel. Output pixels
*        not meeting this requirement are set BAD.
*        [1]
*     NITER = _INTEGER (Read)
*        The number of refining iterations performed if METHOD = "MODE".
*        [7]
*     OUT = LITERAL (Read)
*        Name of the output NDF. This has the master bias frame and
*        the estimated variances.  If USESET is true and multiple Sets
*        are represented in the IN list, then this name will be used
*        as the name of an HDS container file containing one NDF for
*        each Set Index value.
*        This name may be specified using indirection through a file.
*     PRESERVE = _LOGICAL (Read)
*        If TRUE then this indicates that the input data type is to be
*        used for processing. If not then the output type will either
*        be _REAL or _DOUBLE, the precision at which the combinations
*        are performed.
*
*        If a global value has been set using CCDSETUP then this will
*        be used.
*        [TRUE]
*     RNOISE = _DOUBLE (Read)
*        The readout-noise standard deviation. This should be in the
*        input data units (ADUs). A value for this will be worked out
*        for each frame and reported at the end of the task. The
*        average of these values is reported immediately before this
*        parameter is accessed and can be used if a better estimate is
*        not known. Note that the supplied estimate has some resilience
*        to large-scale structure in the input frames, but will be
*        incorrect if the input-frame backgrounds are severely sloped.
*        If variances are not generated then this value will not be
*        accessed.
*
*        The value of this parameter may not be used if the USEEXT
*        parameter is TRUE and will not be used if GENVAR is FALSE
*        (i.e. no variances are being generated). If USEEXT is TRUE
*        then readout noise values will be extracted from the NDFs
*        CCDPACK extensions. Only if a suitable value is not present
*        will the value associated with this parameter be used.
*
*        If a global value has been set up using CCDSETUP this value
*        will be used, and will be shown as the default.  If USESET
*        is true, a global value specific to each image's Set Index
*        value will be sought.
*        [Dynamically derived value]
*     SIGMAS = _REAL (Read)
*        Number of standard deviations to reject data at. Used for
*        "MODE", "SIGMA" and "CLIPMED" methods. For METHOD = "MODE" the
*        standard deviation is estimated from the population of values. For
*        METHOD = "SIGMA" and "CLIPMED" this value is the readout noise.
*        [4]
*     TITLE = LITERAL (Read)
*        Title for the output NDF
*        [Output from MAKEBIAS].
*     USEEXT = _LOGICAL (Read)
*        If TRUE then the parameter RNOISE of this program will not
*        be used and the required values will be obtained from the
*        CCDPACK extensions of the input NDFs instead. This method can
*        only be used if the NDFs have been "imported" using the
*        programs PRESENT or IMPORT. Typically it is used when
*        processing using CCDPACK's "automated" methods.
*
*        Values obtained from the CCDPACK extension are identified in
*        the output log by the presence of a trailing asterisk (*).
*        [FALSE]
*     USESET = _LOGICAL (Read)
*        Whether to use Set header information or not.  If USESET is
*        false then any Set header information will be ignored.
*
*        If USESET is true, then input files will be considered in
*        groups; a separate master bias frame will be constructed for
*        each group of corresponding input frames (i.e. those sharing
*        the same Set Index attribute).  If this results in multiple
*        output master bias frames, they will be written as separate
*        NDFs into a single HDS container file.  If no Set header
*        information is present in the input files, then all the
*        input files are combined together to form the master bias,
*        so USESET can usually be safely set to TRUE.
*
*        If a global value for this parameter has been set using
*        CCDSETUP then that value will be used.
*        [FALSE]
*     ZERO = _LOGICAL (Read)
*        Flag indicating whether the output master bias is to have a
*        mean value of zero or not. If TRUE the input data components
*        are ZERO-ed before combination, of the data. Note that if
*        this option is chosen then it will be necessary to offset the
*        master bias to the data before subtraction. This option is
*        not allowed for unsigned input data type (unless PRESERVE is
*        FALSE) as zeroing will make around half the data values
*        invalid.
*        [TRUE]

*  Examples:
*     makebias in='"b1,b2,b3,b4,b5"' method=median out=mbias rnoise=10
*        This forms a master bias from the data components of the NDFs
*        b1-b5. The combination mode chosen is the median. The output
*        NDF is mbias whose variance has values based on a readout
*        noise of 10 data units. Note the quotes when entering a comma
*        separated list on the command line.
*
*     makebias in=^bias_frames.lis out=master_bias
*        In this example the list of NDFs is read from the file
*        bias_frames.lis. This file may contain indirection to other files
*        up to a depth of 7.
*
*     makebias in='*' out=master_bias
*        In this example all the NDFs in the directory are used.

*  Implementation Status:
*     - The routine supports BAD pixels and all numeric data types
*       except COMPLEX.  All combinational arithmetic is performed using
*       floating values.  The UNITS, AXIS and TITLE components
*       are correctly propagated. Any input variances are ignored.

*  Notes:
*     - If a variance component is present it will not be propagated.

*  Behaviour of Parameters:
*     Most parameters retain their current value as default. The
*     "current" value is the value assigned on the last run of the
*     application. If the application has not been run then the
*     "intrinsic" defaults, as shown in the parameter help, apply.
*     The exceptions to this rule are:
*        - RNOISE  -- dynamic value (but see below)
*        - TITLE   -- always "Output from MAKEBIAS"
*        - KEEPIN  -- always TRUE
*
*     Retaining parameter values has the advantage of allowing you to
*     define the default behaviour of the application but does mean
*     that additional care needs to be taken when using the application
*     on new datasets/different devices, or after a break of sometime.
*     The intrinsic default behaviour of the application may be
*     restored by using the RESET keyword on the command line.
*
*     Certain parameters (LOGTO, LOGFILE, RNOISE, GENVAR, PRESERVE and
*     USESET) have global values. These global values will always take
*     precedence, except when an assignment is made on the command line.
*     Global values may be set and reset using the CCDSETUP and
*     CCDCLEAR commands.  If USESET is true then a global value
*     of RNOISE specific to the Set Index of each image will be used
*     if one is available.
*
*     The parameter RNOISE will not be used if the USEEXT parameter is
*     set TRUE. In this case values will be obtained from the input NDFs
*     CCDPACK extensions.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils

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
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     19-MAR-1991 (PDRAPER):
*        Original version.
*     21-JUN-1991 (PDRAPER):
*        Added log system calls.
*     23-JUL-1991 (PDRAPER):
*        Added IRG system and comments.
*     19-AUG-1991 (PDRAPER):
*        Changed to suppress generation of variances - considerable
*        modifications.
*     5-AUG-1993 (PDRAPER):
*        Change to dynamically allocate certain workspace.
*     29-SEP-1993 (PDRAPER):
*        Added input NDF type checking and use of extension items
*        for getting readout noise.
*     15-JAN-1994 (PDRAPER):
*        Now uses CCDPACK extension information.
*     2-FEB-1994 (PDRAPER):
*        Added option to delete input NDFs.
*     12-SEP-1995 (PDRAPER):
*        Added prologue information on KEEPIN and USEEXT.
*     6-OCT-1995 (PDRAPER):
*        Updated for CCDPACK 2.0.
*     31-JAN-1998 (PDRAPER):
*        Added clipmed combination method.
*     25-JUN-1998 (PDRAPER):
*        Stopped the propagation of quality from the first NDF to the
*        output. This was not the right thing to do when the NDFs are
*        padded to match bounds (regions of BAD quality are introduced).
*     18-NOV-1998 (PDRAPER):
*        Added fastmed combination method.
*     13-FEB-1999 (MBT):
*        Modified to propagate WCS component.
*     29-JUN-2000 (MBT):
*        Replaced use of IRH/IRG with GRP/NDG.
*     14-FEB-2001 (MBT):
*        Upgraded for use with Sets.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF status codes and buffer sizes
      INCLUDE 'CCD1_PAR'         ! CCDPACK internal constants
      INCLUDE 'GRP_PAR'          ! GRP system constants
      INCLUDE 'AST_PAR'          ! AST system constants
      INCLUDE 'CNF_PAR'          ! CNF functions

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MAXPIX             ! Maximum number of pixel mapped from
                                 ! input data sets.
      PARAMETER ( MAXPIX = 500000 )

*  Local Variables:
      CHARACTER * ( 25 ) CMODE   ! Combination mode
      CHARACTER * ( NDF__SZFTP ) DTYPE ! Output data type
      CHARACTER * ( NDF__SZTYP ) ITYPE ! Lowest common precision type of input NDFs
      CHARACTER * ( NDF__SZTYP ) PTYPE ! Actual processing precision
      CHARACTER * ( GRP__SZNAM ) NAME ! Set Name attribute value
      CHARACTER * ( GRP__SZNAM ) OUTNAM ! Name of output HDS container file
      DOUBLE PRECISION AVEACC( CCD1__MXNDF ) ! Average value in an NDF
      DOUBLE PRECISION NCON( CCD1__MXNDF ) ! NUmber of contributing pixels.
      DOUBLE PRECISION NOISE( CCD1__MXNDF ) ! Noise estimate in an NDF
      DOUBLE PRECISION RNOISE    ! Nominal readout noise value.
      DOUBLE PRECISION RNOISX    ! Readout noise from extension information
      DOUBLE PRECISION STATS( CCD1__MXNDF ) ! Image contribution statistics
      DOUBLE PRECISION VARS( CCD1__MXNDF ) ! Variances for NDFs (readout noise**2)
      DOUBLE PRECISION WRK1( CCD1__MXNDF ) ! Workspace for combination
      DOUBLE PRECISION WRK2( CCD1__MXNDF ) ! routines.
      DOUBLE PRECISION WRK3( CCD1__MXNDF ) !
      INTEGER EL                 ! Number of pixels in mapped NDF section
      INTEGER I                  ! Loop variable
      INTEGER ICHUNK             ! Loop variable for number of chunks
      INTEGER IDWRK4             ! Identifier to workspace
      INTEGER IMETH              ! The combination method
      INTEGER INDEX              ! Set Index attribute value
      INTEGER INGRP              ! NDG identifier for group of all input NDFs
      INTEGER INSTA              ! Number of pixels in accessable workspace stack.
      INTEGER INWORK             ! NDF identifier for workspace
      INTEGER IPOINT             ! Pointer to currently mapped NDF section
      INTEGER IPVAR              ! Pointer to variance of ""
      INTEGER IPWORK             ! Pointer to workspace
      INTEGER IPWRK4             ! Pointer to workspace for order stats covariances (size number_ndfs**3)
      INTEGER ISUB               ! Subgroup loop index
      INTEGER JSET               ! Set alignment frame index (dummy)
      INTEGER KEYGRP             ! GRP identifier for subgroup Index keys
      INTEGER MAPINT             ! Current HDS mapping mode
      INTEGER MINPIX             ! Minimum number of contributing pixels
      INTEGER MMXPIX             ! Maximum number of pixels accessed from one NDF
      INTEGER MXPIX              ! Maximum number of pixels in processing stack at one time
      INTEGER NCHUNK             ! Number of chunks an input NDF is to be accessed in.
      INTEGER NDFCUR             ! Identifier of currently mapped input NDF
      INTEGER NDFOUT             ! identifier for output NDF
      INTEGER NITER              ! Number of clipping iterations
      INTEGER NNDF               ! The number of input NDFs accessed
      INTEGER NOK                ! Number of ok values obtained from extension
      INTEGER NPIX               ! The number of pixels in an NDF
      INTEGER NSUB               ! Number of subgroups
      INTEGER NTOT               ! Total number of input NDFs
      INTEGER NWRK4              ! Number of elements of covariance w/s
      INTEGER OPLACE( CCD1__MXNDF ) ! Placeholders for output NDFs
      INTEGER PLACE              ! Place holder for an NDF
      INTEGER POINT( CCD1__MXNDF ) ! Workspace for pointers to sorted data
      INTEGER SINDEX             ! Common Set Index value in subgroup
      INTEGER STACK( CCD1__MXNDF ) ! Stack of input NDF identifiers
      INTEGER SUBGRP( CCD1__MXNDF ) ! NDG identifiers for subgroup NDF lists
      LOGICAL DELETE             ! Delete input NDFs?
      LOGICAL GENVAR             ! If true then variances are generated.
      LOGICAL OK                 ! Ok flag
      LOGICAL PRESER             ! If true the user wants to preser the input precision.
      LOGICAL USED( CCD1__MXNDF ) ! Workspace for flagging image useage
      LOGICAL USEEXT             ! Get information from NDF extensions
      LOGICAL USESET             ! Use Set header information?
      LOGICAL ZERO               ! Set if the output NDF is to have a mean of zero.
      REAL ALPHA                 ! Trimming fraction
      REAL NSIGMA                ! Number of sigma to clip at
      REAL RMAX, RMIN            ! Maximum and minimum values (in stack)

*  Internal references:
      INCLUDE 'NUM_DEC_CVT'      ! Conversion declarations
      INCLUDE 'NUM_DEF_CVT'      ! Conversion definitions
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Switch on HDS I/O instead of mapping. Record the present status and
*  restore this on exit.
      CALL HDS_GTUNE( 'MAP', MAPINT, STATUS )
      CALL HDS_TUNE( 'MAP', 0, STATUS )

*  Start up the CCDPACK log system and write out the introduction for
*  this task.
      CALL CCD1_START( 'MAKEBIAS', STATUS )

*  Initialise GRP identifiers, so that a later call of CCD1_GRDEL on
*  an unintialised group cannot cause trouble.
      INGRP = GRP__NOID
      KEYGRP = GRP__NOID
      DO I = 1, CCD1__MXNDF
         SUBGRP( I ) = GRP__NOID
      END DO

*  See if the user wants to save disk space by deleting the input NDFs
*  when MAKEBIAS is finished with them. This will use the NDF_DELET
*  call which will delete container files if the NDF is associated with
*  the top-level object, otherwise the NDF itself will just be deleted.
*  In the latter case the space used by the NDF in the container file
*  will be released, the size of the file will probably not reduce.
      CALL PAR_GET0L( 'KEEPIN', DELETE, STATUS )
      DELETE = .NOT. DELETE

*  Access the input NDFs
      CALL NDF_BEGIN
      CALL CCD1_NDFGL( 'IN', 1, CCD1__MXNDF, INGRP, NTOT, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Find out if we are using Set header information.
      CALL PAR_GET0L( 'USESET', USESET, STATUS )

*  Split the group of input NDFs up by Set Index if necessary.
      NSUB = 1
      IF ( USESET ) THEN
         CALL CCD1_SETSP( INGRP, 'INDEX', CCD1__MXNDF, SUBGRP, NSUB,
     :                    KEYGRP, STATUS )
      ELSE
         SUBGRP( 1 ) = INGRP
         KEYGRP = GRP__NOID
      END IF

*  Get placeholders for the output NDFs we want to create from the OUT
*  parameter.
      CALL CCD1_NDFPL( 'OUT', NSUB, 'i', KEYGRP, OPLACE, OUTNAM,
     :                 STATUS )

*  Are we to generate variances or not ?
      GENVAR = .FALSE.
      CALL PAR_GET0L( 'GENVAR', GENVAR, STATUS )

*  Find out if the user wants to get the readout noise values from the
*  extensions of the NDFs. If this is true then each input NDF will have
*  a different readout noise value. NDFs with missing values will use
*  the value obtained through the parameter system. (This parameter is
*  only relevant if GENVAR is TRUE.)
      IF ( GENVAR ) THEN
         CALL PAR_GET0L( 'USEEXT', USEEXT, STATUS )
      END IF

*  Loop over subgroups performing calculations separately for each one.
      DO ISUB = 1, NSUB

*  Write a header unless this is the only subgroup.
         IF ( NSUB .GT. 1 ) THEN
            CALL CCD1_SETHD( KEYGRP, ISUB, 'Producing master bias',
     :                       'Index', STATUS )
         END IF

*  Get the number of NDFs in this subgroup.
         CALL GRP_GRPSZ( SUBGRP( ISUB ), NNDF, STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Get the stack of NDF identifiers for this subgroup,and find a common
*  Set Index attribute if one exists.
         SINDEX = 0
         DO I = 1, NNDF
            CALL NDG_NDFAS( SUBGRP( ISUB ), I, 'READ', STACK( I ),
     :                      STATUS )
            CALL CCD1_SETRD( STACK( I ), AST__NULL, NAME, INDEX, JSET,
     :                       STATUS )
            IF ( I .EQ. 1 ) THEN
               SINDEX = INDEX
            ELSE
               IF ( INDEX .NE. SINDEX ) SINDEX = CCD1__BADSI
            END IF
         END DO

*  Check the frame types of the input NDFs. These should be unnamed
*  or be of type BIAS.
         CALL CCD1_CKTYP( STACK, NNDF, 'BIAS', STATUS )

*  Make sure that all the input data is accessed at a precision
*  sufficient for all of the input NDFs and get the processing
*  precision. Since all of the processing is 'averaging', integer
*  values make little sense, except as a finally rounded result.
*  Therefore although all types are supported, arithmetic will generally
*  be done in real or double precision, even when in real large amounts
*  of work will still be performed in double precision. The precisions
*  will be converted internally to the combination routines so that the
*  mapped space is limited to only that which is necessary; actual work
*  usually occurs within the workspace buffers declared above. All
*  integer precisions, except _INTEGER will be processed as reals,
*  _INTEGER and _DOUBLE will be processed purely in double precision.
*  The output precision will be that of the input data array and
*  variance array, unless the user wants the extra precision.
         CALL NDF_MTYPN( '_BYTE,_UBYTE,_WORD,_UWORD,_INTEGER,_REAL,'//
     :                   '_DOUBLE', NNDF, STACK, 'Data', ITYPE, DTYPE,
     :                   STATUS )
         IF( ITYPE .EQ.'_INTEGER' .OR. ITYPE .EQ. '_DOUBLE' ) THEN
            PTYPE = '_DOUBLE'
         ELSE
            PTYPE = '_REAL'
         END IF

*  Find out if the user wants to preserve the input precision.
         CALL PAR_GET0L( 'PRESERVE', PRESER, STATUS )

*  Determine if the users wants an average of zero output, if so the
*  bias values are modified by subtracting the mean value before
*  combining the data. If the output type is unsigned then do not
*  allow an output average of zero!
         IF( PRESER .AND. ITYPE( 1 : 2 ) .EQ. '_U' ) THEN
            ZERO = .FALSE.
         ELSE
            CALL PAR_GET0L( 'ZERO', ZERO, STATUS )
            IF ( ZERO ) THEN

*  User wants to zero the bias data before combination. If preserve
*  is false and the ITYPE is unsigned then zero pointing is allowed but
*  this means that the data must be mapped in so as to allow negative
*  numbers when the zero is subtracted.
               IF ( ITYPE( 1 : 2 ) .EQ. '_U' ) THEN
                  IF ( ITYPE .EQ. '_UBYTE' ) THEN
                     ITYPE = '_WORD'
                  ELSE
                     ITYPE = '_REAL'
                  END IF
               END IF
            END IF
         END IF

*  Find out the average values in each of the NDFs, work in the
*  input precision. Also derive an estimate for the noise level in each
*  data component. These will be combined to form an estimate for the
*  readout noise. The algorithm used to derive the noise has some
*  protection against a varying background, but will still give an
*  incorrect value for steep slopes, or non uniform variations.
         VARS( 1 ) = 0.0D0
         DO 4 I = 1, NNDF

*  Map in the NDF, note that unmapping occurs in this loop, this is to
*  avoid problems with the blocking section.
            CALL NDF_MAP( STACK( I ), 'Data', ITYPE, 'READ', IPOINT,
     :                    EL, STATUS )

*  Form the mean and standard deviation.
            CALL CCD1_RMSD( ITYPE, .TRUE., IPOINT, EL, AVEACC( I ),
     :                      NOISE( I ), NPIX, STATUS )

*  Unmap current Data component.
            CALL NDF_UNMAP( STACK( I ), 'Data', STATUS )
            VARS( 1 ) = VARS( 1 ) + NOISE( I )
 4       CONTINUE

*  Form an estimate of the 'global' readout noise on the basis of the
*  input NDFs variations.
         IF ( STATUS .NE. SAI__OK ) GO TO 99
         RNOISE = VARS( 1 ) / REAL( NNDF )

*  Find out the stacking mode.
         CALL CCD1_GTCPC( GENVAR, IMETH, CMODE, NITER, NSIGMA, ALPHA,
     :                    RMIN, RMAX, STATUS )

*  Match the pixel-index bounds of the input NDFs (makes them all look
*  the same size), padding out to largest required extent.
         CALL NDF_MBNDN( 'Pad', NNDF, STACK, STATUS )

*  Find out how many pixels a single padded NDF contains.
         CALL NDF_SIZE( STACK( 1 ), NPIX, STATUS )

*  Find out how many pixels of a single NDF we are to access at any
*  one time. Chunking the data onto the stack and using HDS I/O instead
*  of mapping should decrease the elapsed times.
         MMXPIX = MAXPIX / ( NNDF + 2 ) ! Two extra arrays to access
                                        ! try to keep memory constant.
         MXPIX = MAX( 1, MIN( MMXPIX, NPIX ) )

*  Find out how many chunks this corresponds too.
         CALL NDF_NCHNK( STACK( 1 ), MXPIX, NCHUNK, STATUS )

*  The stack to contain all the NDFs, sectioned to contain this number
*  of pixels, this needs size -
         INSTA = MXPIX * NNDF

*  Get a temporary NDF big enough to contain all possible input NDF
*  sections in a stack NNDF big.
         CALL NDF_TEMP( PLACE, STATUS )
         CALL NDF_NEW( ITYPE, 1, 1, INSTA, PLACE, INWORK, STATUS )

*  Map the stack in.
         CALL NDF_MAP( INWORK, 'Data', ITYPE, 'WRITE', IPWORK, EL,
     :                 STATUS )

*  Create the output NDF to contain the result. Propagating axis,
*  label, history, units from the first NDF. Do NOT propagate
*  the CCDPACK extension (most of the information in this only
*  applies to the input NDF) or the QUALITY component.
         CALL NDF_SCOPY( STACK( 1 ), 'Axis,Units,WCS,Noext(CCDPACK)',
     :                   OPLACE( ISUB ), NDFOUT, STATUS )

*  If we can sensibly do so, create a Set header in the output.
*  if all the input NDFs in this subgroup have the same (non-zero)
*  Set Index, then use that as the Set Index of the output NDF.
*  In that case, use the name of the output NDF itself as the Set
*  Name attribute.
         IF ( SINDEX .NE. CCD1__BADSI ) THEN
            CALL CCD1_SETWR( NDFOUT, OUTNAM, SINDEX, AST__NOFRAME,
     :                       STATUS )
         END IF

*  Set the output types (as explained above) to the processing
*  precision.
         IF ( .NOT. PRESER ) CALL NDF_STYPE( PTYPE, NDFOUT,
     :                                       'Data,Variance', STATUS )

*  Get the read out noise level in ADUs. Report possible value for
*  readout noise.
         CALL MSG_SETR( 'READOUT_NOISE', REAL( RNOISE ) )
         CALL MSG_OUT( 'READOUT_VAL',
     :   '  Readout noise estimated from combined NDFs'//
     :   ' ^READOUT_NOISE (ADUs)', STATUS )
         IF ( GENVAR ) THEN

*  Generating variances - need a readout noise estimate. Otherwise have
*  just reported what we derived for reference. If the extension values
*  for the readout noise are to be used then try to get a value for each
*  NDF. If all NDFs do not contain RNOISE then "just" use the value
*  from the environment.
            IF ( USEEXT ) THEN

*  Try to get a value from the extension of the NDFs.
               NOK = 0
               DO 9 I = 1, NNDF
                  CALL CCG1_FCH0D( STACK( I ), 'RNOISE', RNOISX,
     :                             OK, STATUS )

*  If OK is TRUE then we have a value.
                  IF ( OK ) THEN
                     VARS( I ) = RNOISX * RNOISX
                     NOK = NOK + 1
                  END IF
 9             CONTINUE

*  Check the number of values actually used from the extensions and
*  comment on this is if it seems unusual. Need all NDFs to have a value
*  otherwise we will use a value from the environment.
               IF ( NOK .NE. NNDF ) THEN
                  CALL MSG_SETD( 'RNOISE', RNOISE )
                  CALL CCD1_MSG( ' ', ' Warning - extension item'//
     :' RNOISE not present in all NDFs, using parameter value',
     : STATUS )

*  Set USEEXT to false, no extension information is used.
                  USEEXT = .FALSE.
               END IF
            END IF
            IF ( .NOT. USEEXT ) THEN

*  If we are dealing with a specific Set Index, load the appropriately
*  keyed value of the global parameter into the GLOBAL parameter file.
               IF ( USESET ) CALL CCD1_KPLD( 'RNOISE', SINDEX, STATUS )

*  Get a readout noise estimate from the environment. This supercedes
*  all extension items.
               CALL PAR_DEF0D( 'RNOISE', RNOISE, STATUS )
               CALL PAR_GET0D( 'RNOISE', RNOISE, STATUS )

*  Make up the variances for all NDFs from the "default".
               DO 6 I = 1, NNDF
                  VARS( I ) = RNOISE * RNOISE
 6             CONTINUE
            END IF
         ELSE

*  No variances are required, use unity weighting.
            DO 7 I = 1, NNDF
               VARS( I ) = 1.0D0
 7          CONTINUE
         END IF

*  Get the minimum number of pixels per output pixel
         CALL PAR_GET0I( 'MINPIX', MINPIX, STATUS )

*  Zero statistics buffer.
         DO 5 I = 1, NNDF
            NCON( I ) = 0.0D0
 5       CONTINUE

*  If we're generating covariances then get the required workspace.
         IF ( GENVAR ) THEN
            NWRK4 = MAX( 1, ( ( NNDF+ 1 )**3 ) / 2 )
            CALL CCD1_MKTMP( NWRK4, '_DOUBLE', IDWRK4, STATUS )
            NWRK4 = MAX( 1, NWRK4 / NNDF )
            CALL CCD1_MPTMP( IDWRK4, 'WRITE', IPWRK4, STATUS )
         ELSE
            NWRK4 = 1
            CALL CCD1_MALL( NWRK4, '_INTEGER', IPWRK4, STATUS )
         ENDIF

*  For each chunk of an NDF, find other chunks and enter them onto the
*  stack.
         DO 2 ICHUNK = 1, NCHUNK

*  Map in the NDFs one at a time, transfering them into the 3D stack.
            DO 3 I = 1, NNDF

*  Get the current chunk for each data_array.
               CALL NDF_CHUNK( STACK( I ), MXPIX, ICHUNK, NDFCUR,
     :                         STATUS )

*  Map this section in.
               CALL NDF_MAP( NDFCUR, 'Data', ITYPE, 'READ', IPOINT, EL,
     :                       STATUS )

*  Transfer this to the stack. Use the routine to transfer this at the
*  precision ITYPE.
               CALL CCD1_PTINS( ITYPE, IPWORK, EL, NNDF, I, IPOINT,
     :                          STATUS )

*  Unmap the NDF data array and return for next.
               CALL NDF_UNMAP( NDFCUR, 'Data', STATUS )
               CALL NDF_ANNUL( NDFCUR, STATUS )
 3          CONTINUE

*  Subtract the mean values from the data on the stack (if requested).
            IF( ZERO ) THEN
               CALL CCD1_SUBCS( ITYPE, .TRUE., IPWORK, EL, NNDF, AVEACC,
     :                          STATUS )
            END IF

*  Do the work on the stack transfering the result to the chunk of the
*  output NDF, corresponding to the chunk of the input NDFs. Map in the
*  variance as well.
            CALL NDF_CHUNK( NDFOUT, MXPIX, ICHUNK, NDFCUR, STATUS)
            CALL NDF_MAP( NDFCUR, 'Data', PTYPE, 'WRITE', IPOINT, EL,
     :                    STATUS )
            IF ( GENVAR ) CALL NDF_MAP( NDFCUR, 'Variance', PTYPE,
     :                                  'WRITE', IPVAR, EL, STATUS )

*  Combine all the NDFs in the stack using the method given.
            CALL CCD1_MKBC( ITYPE, GENVAR, IPWORK, EL, NNDF, VARS,
     :                      IMETH, MINPIX, NITER, NSIGMA, ALPHA,
     :                      RMIN, RMAX, IPOINT, IPVAR, WRK1, WRK2,
     :                      WRK3, %VAL( CNF_PVAL( IPWRK4 ) ),
     :                      NWRK4, NCON, POINT,
     :                      USED, STATUS )

*  Unmap the output section, ready for next chunks.
            IF( GENVAR ) CALL NDF_UNMAP( NDFCUR, 'Variance', STATUS )
            CALL NDF_UNMAP( NDFCUR, 'Data', STATUS )
            CALL NDF_ANNUL( NDFCUR, STATUS )
 2       CONTINUE

*  Work out contribution statistics.
         DO 8 I = 1, NNDF
            STATS( I ) = NCON( I ) / DBLE( NPIX ) * 100.0D0
 8       CONTINUE

*  Set bad pixel flag in output NDF
         CALL NDF_SBAD( .TRUE., NDFOUT, 'Data,Variance', STATUS )

*  Set title of output NDF, propagating it if requested.
         CALL NDF_CINP( 'TITLE', NDFOUT, 'TITLE', STATUS )

*  Add the frame type to the output NDF.
         CALL CCG1_STO0C( NDFOUT, 'FTYPE', 'MASTER_BIAS', STATUS )

*  If the output NDF has been zeroed then record this in the NDF
*  extension for future reference.
         CALL CCG1_STO0L( NDFOUT, 'ZEROED', ZERO, STATUS )

*  Touch frame to leave processing trail.
         CALL CCD1_TOUCH( NDFOUT, 'MAKEBIAS', STATUS )

*  Report MAKEBIAS parameters
         IF ( .NOT. GENVAR ) VARS( 1 ) =  RNOISE
         CALL CCD1_RBIA( STACK, NNDF, ZERO, AVEACC, NOISE, CMODE, IMETH,
     :                   MINPIX, ALPHA, NSIGMA, NITER, RMIN, RMAX,
     :                   STATS, VARS, NDFOUT, PRESER, DTYPE, PTYPE,
     :                   GENVAR, USEEXT, DELETE, STATUS )
      END DO

*  If requested delete all the input NDFs.
      IF ( DELETE .AND. STATUS .EQ. SAI__OK ) THEN
         DO 10 I = 1, NTOT
            CALL CCD1_NGDEL( INGRP, I, .TRUE., STATUS )
 10      CONTINUE
      END IF

*  End of NDF context, release all identifiers etc.
 99   CONTINUE

*  End the NDF context.
      CALL NDF_END( STATUS )

*  Make sure all workspace is released (ok to call these at any time as
*  CCD1_START initialises).
      CALL CCD1_FRTMP( -1, STATUS )
      CALL CCD1_MFREE( -1, STATUS )

*  Release GRP resources.
      CALL CCD1_GRDEL( INGRP, STATUS )
      CALL CCD1_GRDEL( KEYGRP, STATUS )
      DO I = 1, MIN( NSUB, CCD1__MXNDF )
         CALL CCD1_GRDEL( SUBGRP( I ), STATUS )
      END DO

*  Restore the HDS input mapping mode.
      CALL HDS_TUNE( 'MAP', MAPINT, STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL CCD1_ERREP( 'MAKEBIAS_ERR',
     :   'MAKEBIAS: Error making master bias frame.',
     :   STATUS )
      END IF

*  Close down logfile system (after error message).
      CALL CCD1_END( STATUS )

      END
* $Id$
