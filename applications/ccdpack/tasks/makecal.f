      SUBROUTINE MAKECAL( STATUS )
*+
*  Name:
*     MAKECAL

*  Purpose:
*     Produces a dark or pre-flash calibration NDF.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL MAKECAL( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine performs the combination of a series of dark count
*     or pre-flash exposure frames. The input NDFs should have been
*     bias subtracted. The input data are divided by the exposure
*     factors before combination into a calibration "master", giving an
*     output NDF whose data represent one unit of the given exposure
*     time per pixel. Thus the calibration frame should be multiplied
*     by the appropriate factor before subtracting from other frames
*     (i.e. by the dark time or the flash-exposure time). This can be
*     performed by CALCOR and should be done prior to the production of
*     a flatfield and flatfield correction. The data combination methods
*     give a mixture of very robust (median) to very efficient (mean)
*     methods to suit the data.

*  Usage:
*     makecal in expose out method { alpha=?
*                                  { sigmas=? niter=?
*                                  { niter=?
*                                  { min=? max=?

*  ADAM Parameters:
*     ALPHA = _REAL (Read)
*        The fraction of extreme values to remove before combining
*        the data at any pixel. This fraction is removed from each
*        extreme so can only take a value in the range 0 to 0.5.
*        Only used if METHOD="TRIMMED"
*        [0.2]
*     EXPOSE = LITERAL (Read)
*        Either:
*        An exact number of exposure factors for the input NDFs. The
*        values must be in the same order as the input NDFs.
*
*        Or:
*        A single value which applies to all the input NDFs.
*
*        Indirection through an ASCII file may be used to specify these
*        values. If more than one line is required at prompt time then
*        a continuation line may be requested by adding "-" to the end
*        of the line.
*
*        This parameter will not be used if USEEXT is set TRUE.
*     IN = LITERAL (Read)
*        A list of NDF names which contain the calibration data. The
*        NDF names should be separated by commas and may include
*        wildcards.
*
*        NOTE the use of wildcards with this program is not recommended
*        unless the input NDFs all have the same calibration exposure
*        factors. The order of processing of any wildcarded NDFs cannot
*        be guaranteed.
*     KEEPIN = _LOGICAL (Read)
*        Whether to keep (i.e. not delete) the input NDFs or
*        not. Deleting the input NDFs has the advantage of saving disk
*        space, but should probably only be used if this program is part
*        of a sequence of commands and the intermediary data used by
*        it are not important.
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
*        for values which can be used when combining the data. This
*        limit applies to the range of the output data (i.e. the values
*        after the exposure factors have been divided into the input
*        data).
*     METHOD = LITERAL (Read)
*        The method to be used to combine the data components of
*        the input NDFs. This may be set to any unique abbreviation of
*        the following:
*           -  MEAN      -- Mean of the input data values
*           -  MEDIAN    -- Weighted median of the input data values
*           -  TRIMMED   -- An "alpha trimmed mean" in which a fraction
*                           alpha/2 of the values are removed from
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
*           -  FASTMED   -- Unweighted median of input data values
*        [MEDIAN]
*     MIN = _REAL (Read)
*        If METHOD = "THRESH" then this value defines the lower limit
*        for values which can be used when combining the data. This
*        limit applies to the range of the output data (i.e. the values
*        after the exposure factors have been divided into the input
*        data).
*     MINPIX = _INTEGER (Read)
*        The minimum number of good (ie. not BAD) pixels required
*        to contribute to the value of an output pixel. Output pixels
*        not meeting this requirement are set BAD.
*        [1]
*     NITER = _INTEGER (Read)
*        The number of refining iterations performed if METHOD = "MODE".
*        [7]
*     OUT = LITERAL (Write)
*        Name of the output NDF to contain the calibration data.
*        Note this NDF will have a type of at least _REAL.
*        If USESET is true and multiple Sets are represented in the IN
*        list then this name will be used as the name of an HDS
*        container file containing one NDF for each Set Index value.
*        This name may be specified using indirection through a file.
*     SIGMAS = _REAL (Read)
*        Number of standard deviations to reject data at. Used for
*        "MODE", "SIGMA" and "CLIPMED" methods. For METHOD = "MODE" the
*        standard deviation is estimated from the population of values,
*        for METHOD = "SIGMA" the variances are used. If no variances
*        exist then a population estimate is used.
*        [4.0]
*     TITLE = LITERAL (Read)
*        Title for the output NDF.
*        [Output from MAKECAL]
*     USESET = _LOGICAL (Read)
*        Whether to use Set header information or not.  If USESET is
*        false then any Set header information will be ignored.
*        If USESET is true, then input files will be considered in
*        groups; a separate calibration frame will be constructed
*        for each group of corresponding input frames (i.e. those
*        sharing the same Set Index attribute).  If this results in
*        multiple output calibration files, they will be written as
*        separate NDFs into a single HDS container file.
*        If no Set header information is present in the input files,
*        then calibration is done on all the input files together,
*        so USESET can usually be safely set to TRUE.
*
*        If a global value for this parameter has been set using
*        CCDSETUP then that value will be used.
*        [FALSE]
*     TYPE = LITERAL (Read)
*        The frame types of the input data. This should be a recognised
*        name "FLASH", "DARK" or "NONE". The value of this parameter
*        affects the output NDF frame type which will be set to
*        "MASTER_FLASH" or "MASTER_DARK" or "MASTER_?".
*        [NONE]
*     USEEXT = _LOGICAL (Read)
*        If TRUE then the EXPOSE parameter of this program will not
*        be used and the required values will be obtained from the
*        CCDPACK extensions of the input NDFs instead. This method can
*        only be used if the NDFs have been "imported" using the
*        programs PRESENT or IMPORT. Typically it is used when
*        processing using CCDPACK's "automated" methods.
*
*        Values obtained from the CCDPACK extension are identified in
*        the output log by the presence of a trailing asterisk (*).
*        [FALSE]

*  Examples:
*     makecal in='"f1,f2,f3,f4"' expose='"100,200,300,400"' method=median
*             out=master_flash
*        This example forms a flash calibration NDF from the data in
*        NDFs f1,f2,f3 and f4. The data are divided by the relative
*        exposure factors before combination. The combination method
*        used is the (weighted) median, the resultant data are written
*        to the NDF master_flash.
*
*     makecal '"d1,d2,d3,d4"' 1 master_dark trimmed alpha=0.2
*        This example produces a dark-count-calibration frame from the
*        data in NDFs d1,d2,d3 and d4. The exposure factors are given
*        as 1 which probably indicates that the dark-exposure times in
*        these datasets are exactly right to correct any subsequent
*        data frames. The combination mode used is the trimmed mean with
*        trimming fraction 0.2 and the output data are written to NDF
*        master_dark.
*
*     makecal ^flash_frames ^flash_exposures flash_master
*        In this example a list of frames to be processed is passed to
*        the program by indirection through an ASCII file
*        flash_frames.dat, the corresponding exposure times are passed
*        from the file flash_exposures.dat. This is probably the only
*        safe method for entering NDFs to this routine other than as in
*        the above examples. Using wildcards for the file
*        specifications will mean that the exposures cannot be
*        associated correctly. Thus wildcards should not be used except
*        when the input NDFs have the same exposure times.

*  Implementation Status:
*     - The routine supports BAD pixels and all data types except
*       COMPLEX. All combinational arithmetic is performed in floating
*       point. The AXIS and TITLE components are correctly propagated.
*       The variances are propagated through the combination
*       processing, assuming that the input data have a normal
*       distribution.

*  Behaviour of Parameters:
*     Most parameters retain their current value as default. The
*     "current" value is the value assigned on the last run of the
*     application. If the application has not been run then the
*     "intrinsic" defaults, as shown in the parameter help, apply.
*     The exceptions to this rule are:
*        - TITLE  -- always "Output from MAKECAL"
*        - KEEPIN -- always TRUE
*
*     Retaining parameter values has the advantage of allowing you to
*     define the default behaviour of the application but does mean
*     that additional care needs to be taken when using the application
*     on new datasets/different devices, or after a break of sometime.
*     The intrinsic default behaviour of the application may be
*     restored by using the RESET keyword on the command line.
*
*     Certain parameters (LOGTO, LOGFILE and USESET) have global values.
*     These global values will always take precedence, except when an
*     assignment is made on the command line.  Global values may be set
*     and reset using the CCDSETUP and CCDCLEAR commands.
*
*     The parameter EXPOSE will not be used if the USEEXT parameter is
*     set TRUE. In this case the necessary values will be extracted from
*     the CCDPACK extensions of the input NDFs.

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
*     28-MAY-1991 (PDRAPER):
*        Original Version.
*     5-JUL-1993 (PDRAPER):
*        Made some workspace dynamic.
*     29-SEP-1993 (PDRAPER):
*        Added the TYPE parameter. Incorporated input frame type
*        checking etc.
*     2-FEB-1994 (PDRAPER):
*        Added deletion of input NDFs.
*     19-JUL-1995 (PDRAPER):
*        Removed AIF_ calls and replace with PAR_.
*     12-SEP-1995 (PDRAPER):
*        Added prologue descriptions of KEEPIN and USEEXT.
*     6-OCT-1995 (PDRAPER):
*        Updated to CCDPACK 2.0.
*     31-JAN-1998 (PDRAPER):
*        Added clipmed combination method.
*     25-JUN-1998 (PDRAPER):
*        Stopped the propagation of quality from the first NDF to the
*        output. This was not the right thing to do when the NDFs are
*        padded to match bounds (regions of BAD quality are introduced).
*     18-NOV-1998 (PDRAPER):
*        Added FASTMED combination option.
*     23-FEB-1999 (MBT):
*        Modified to propagate WCS component.
*     29-JUN-2000 (MBT):
*        Replaced use of IRH/IRG with GRP/NDG.
*     13-FEB-2001 (MBT):
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
      INCLUDE 'GRP_PAR'          ! Standard GRP system constants
      INCLUDE 'AST_PAR'          ! Standard AST system constants
      INCLUDE 'CNF_PAR'          ! CNF functions

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MAXPIX             ! Maximum number of pixel mapped from
                                 ! input data sets.
      PARAMETER ( MAXPIX = 500000 )

*  Local Variables:
      CHARACTER * ( 25 )  CMODE  ! Combination mode
      CHARACTER * ( CCD1__NMLEN) FTYPE ! Expect frame type
      CHARACTER * ( NDF__SZFTP ) DTYPE ! Output data type
      CHARACTER * ( NDF__SZTYP ) ITYPE ! Lowest common precision type of input NDFs
      CHARACTER * ( GRP__SZNAM ) NAME ! Set Name attribute value
      CHARACTER * ( GRP__SZNAM ) OUTNAM ! Name of output container file
      CHARACTER * ( NDF__SZTYP ) PTYPE ! Actual processing precision
      DOUBLE PRECISION CVAL      ! Mean value scaling factor
      DOUBLE PRECISION EFACIN( CCD1__MXNDF ) ! Exposure times for IN NDFs
      DOUBLE PRECISION EFACT( CCD1__MXNDF ) ! Exposure times for subgroup
      DOUBLE PRECISION INFACT( CCD1__MXNDF ) ! Inverse exposure times
      DOUBLE PRECISION NCON( CCD1__MXNDF ) ! Number of contributing pixels.
      DOUBLE PRECISION STATS( CCD1__MXNDF ) ! Image contribution statistics
      DOUBLE PRECISION WRK1( CCD1__MXNDF ) ! Workspace for combination
      DOUBLE PRECISION WRK2( CCD1__MXNDF ) ! routines.
      DOUBLE PRECISION WRK3( CCD1__MXNDF ) !
      INTEGER EL                 ! Number of pixels in mapped NDF section
      INTEGER I                  ! Loop variable
      INTEGER ICHUNK             ! Loop variable for number of chunks
      INTEGER IDWRK4             ! Workspace identifier
      INTEGER IMETH              ! The combination method
      INTEGER INDEX              ! Set Index attribute value
      INTEGER INGRP              ! NDG identifier for group of input NDFs
      INTEGER INSTA              ! Number of pixels in accessible workspace stack.
      INTEGER INWORK             ! NDF identifier for workspace
      INTEGER IPDIN              ! Pointer to current input NDF chunk
      INTEGER IPDOUT             ! Pointer to output Data component
      INTEGER IPDWRK             ! Pointer to Data stack
      INTEGER IPVIN              ! Pointer to current input NDF chunk
      INTEGER IPVOUT             ! Pointer to output Variance component
      INTEGER IPVWRK             ! Pointer to Variance stack
      INTEGER IPWRK4             ! Pointer to covariances workspace
      INTEGER ISUB               ! Subgroup loop index
      INTEGER JSET               ! Set Frame index (dummy)
      INTEGER KEYGRP             ! GRP identifier for subgroup Index attribute
      INTEGER MAPINT             ! Input HDS mapping mode
      INTEGER MINPIX             ! Minimum number of contributing pixels
      INTEGER MMXPIX             ! Maximum value that MXPIX can take
      INTEGER MXPIX              ! Maximum number of pixels in processing stack at one time
      INTEGER NCHUNK             ! Number of chunks an input NDF is to be accessed in.
      INTEGER NDFCUR             ! Identifier of currently mapped input NDF
      INTEGER NDFOUT             ! identifier for output NDF
      INTEGER NITER              ! Number of clipping iterations
      INTEGER NNDF               ! The number of input NDFs in the subgroup
      INTEGER NPIX               ! Number of pixels in an NDF
      INTEGER NSUB               ! Number of subgroups
      INTEGER NTOT               ! Total number of input NDFs
      INTEGER NVAR               ! Number of input NDFs with variances
      INTEGER NWRK4              ! Number of elements of covariance w/s
      INTEGER OWORK              ! Chunk of output NDF.
      INTEGER OPLACE( CCD1__MXNDF ) ! Placeholders for subgroup output NDFs
      INTEGER PLACE              ! Place holder for an NDF
      INTEGER POINT( CCD1__MXNDF ) ! Workspace for pointers to sorted data
      INTEGER STACK( CCD1__MXNDF ) ! Stack of input NDF identifiers
      INTEGER SUBGRP( CCD1__MXNDF ) ! NDG identifiers for subgroup input NDFs
      INTEGER SINDEX             ! Common subgroup Index value
      LOGICAL BAD                ! Set if BAD pixels are present
      LOGICAL DELETE             ! Delete input NDFs before exit
      LOGICAL HAVVAR             ! Set if all variances components are present.
      LOGICAL THSVAR             ! This variance - used for testing variance presence.
      LOGICAL USED( CCD1__MXNDF )     ! Workspace for flagging image usage
      LOGICAL USEEXT             ! Use extension values
      LOGICAL USESET             ! Are we using Set header information?
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

*  Startup logging - write introduction.
      CALL CCD1_START( 'MAKECAL', STATUS )

*  Initialise GRP identifiers, so that a later call of CCD1_GRDEL on
*  an uninitialised group cannot cause trouble.
      INGRP = GRP__NOID
      KEYGRP = GRP__NOID
      DO I = 1, CCD1__MXNDF
         SUBGRP( I ) = GRP__NOID
      END DO

*  See if the user wants to save disk space by deleting the input NDFs
*  when MAKECAL is finished with them. This will use the NDF_DELET
*  call which will delete container files if the NDF is associated with
*  the top-level object, otherwise the NDF itself will just be deleted.
*  In the latter case the space used by the NDF in the container file
*  will be released, the size of the file will probably not reduce.
      CALL PAR_GET0L( 'KEEPIN', DELETE, STATUS )
      DELETE = .NOT. DELETE

*  See if we are to use the NDF extensions to look for suitable
*  exposure times.
      CALL PAR_GET0L( 'USEEXT', USEEXT, STATUS )

*  Get the frame type which is expected. This reflects whether the
*  input data are dark or flash frames, or if this isn't a useful
*  operation (default).
      CALL PAR_CHOIC( 'TYPE', 'NONE', 'NONE,DARK,FLASH', .TRUE., FTYPE,
     :                STATUS )

*  Access the input NDFs and the associated exposure times. If USEEXT is
*  true then we will look in the NDF extensions for the exposure
*  factors, defaulting to the value(s) supplied by the parameter.
      CALL NDF_BEGIN
      IF ( FTYPE .EQ. 'NONE' ) USEEXT = .FALSE.
      CALL CCD1_NDFAB( FTYPE, USEEXT, 'IN', CCD1__MXNDF, 'EXPOSE',
     :                 INGRP, EFACIN, NTOT, STATUS )
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

*  Loop over subgroups performing the calculations separately for each
*  one.
      DO ISUB = 1, NSUB

*  Write a header unless this is the only subgroup.
         IF ( NSUB .GT. 1 ) THEN
            CALL CCD1_SETHD( KEYGRP, ISUB,
     :                       'Producing calibration master', 'Index',
     :                       STATUS )
         END IF

*  Get the number of NDFs in this subgroup.
         CALL GRP_GRPSZ( SUBGRP( ISUB ), NNDF, STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Get the stack of NDF identifiers for this subgroup, and find a common
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

*  Get the exposure values for this subgroup.
         CALL CCG1_ORDD( INGRP, EFACIN, SUBGRP( ISUB ), EFACT, STATUS )

*  Check the frame types of the input NDFs, these should either be DARK
*  or FLASH frames. Do not check if no preference for a type is given.
         IF ( FTYPE .NE. 'NONE' ) THEN
            CALL CCD1_CKTYP( STACK, NNDF, FTYPE, STATUS )
         END IF

*  All NDFs should be debiassed before being combined (except maybe
*  some forms of Infra-red data?).
         CALL CCD1_CKDEB( STACK, NNDF, STATUS )

*  Find a suitable data type for accessing the input data. The output
*  type will always be one of _REAL or _DOUBLE as the data may be
*  normalised.
         CALL NDF_MTYPN( '_BYTE,_UBYTE,_WORD,_UWORD,_INTEGER,_REAL,'
     :                   //'_DOUBLE', NNDF, STACK, 'Data',
     :                   ITYPE, DTYPE, STATUS )

*  Set the result type, the actual stacking arithmetic will be performed
*  using this type and the output will remain in this. The input data is
*  accessed at this precision as adjustment of the values occurs prior
*  to stacking. Memory should not be a problem as a fixed chunking size
*  is used.
         IF( ITYPE .EQ.'_INTEGER' .OR. ITYPE .EQ. '_DOUBLE' ) THEN
            PTYPE = '_DOUBLE'
         ELSE
            PTYPE = '_REAL'
         END IF

*  Find out which input NDFs have variances. Will require all inputs to
*  have variances, otherwise the exposure values will be used as
*  weights. If we do not have all variances then none will be accessed.
         NVAR = 0
         DO 4  I = 1, NNDF
            CALL NDF_STATE( STACK( I ), 'Variance', THSVAR, STATUS )
            IF ( THSVAR ) NVAR = NVAR + 1
 4       CONTINUE

*  Set HAVVAR flag to show if all variances are present. If not all
*  variances are present and NVAR is greater than zero issue a warning.
         IF ( NVAR .GT. 0 ) THEN
            IF ( NVAR .EQ. NNDF ) THEN
                HAVVAR = .TRUE.
            ELSE
                HAVVAR = .FALSE.
                CALL MSG_OUT( 'MAKECAL_NVAR',
     :          ' Warning - only some input NDFs have a variance'//
     :          ' component, variance analysis not performed.',
     :          STATUS )
            END IF

         ELSE

*  No variances to process, need inverse weights as pseudo variances.
            DO 7 I = 1, NNDF
               INFACT( I ) = 1.0D0 / EFACT( I )
 7          CONTINUE
            HAVVAR = .FALSE.
         END IF

*  Find out the stacking mode.
         CALL CCD1_GTCPB( IMETH, CMODE, NITER, NSIGMA, ALPHA, RMIN,
     :                    RMAX, STATUS )

*  Get the minimum number of contributing pixels per output pixel
         CALL PAR_GET0I( 'MINPIX', MINPIX, STATUS )

*  Match the pixel-index bounds of the input NDFs (makes them all look
*  the same size), padding out to largest required extent.
         CALL NDF_MBNDN( 'Pad', NNDF, STACK, STATUS )

*  Find out how many pixels a single padded NDF contains.
         CALL NDF_SIZE( STACK( 1 ), NPIX, STATUS )

*  Have we got BAD pixels?
         CALL NDF_MBADN( .TRUE., NNDF, STACK, 'Data,Variance', .FALSE.,
     :                   BAD, STATUS )

*  Find out how many pixels of a single NDF we are to access at any
*  one time. Chunking the data onto the stack and using HDS I/O instead
*  of mapping should decrease the elapsed times.
         MMXPIX = MAXPIX / ( NNDF + 4 ) ! Four extra arrays to access
                                        ! try to keep memory constant.
         MXPIX = MAX( 1, MIN( MMXPIX, NPIX ) )

*  Find out how many chunks this corresponds too.
         CALL NDF_NCHNK( STACK( 1 ), MXPIX, NCHUNK, STATUS )

*  The stack to contain all the NDFs, sectioned to contain this number
*  of pixels, this needs size -
         INSTA = MXPIX * NNDF

*  Create the output NDF to contain the result. Propagating axis,
*  label and history, do not propagate units these may no
*  longer apply. Do NOT propagate the CCDPACK extension (most of
*  the information in this only applies to the input NDF). or the
*  QUALITY.
         CALL NDF_SCOPY( STACK( 1 ), 'Axis,NoUnits,WCS,Noext(CCDPACK)',
     :                   OPLACE( ISUB ), NDFOUT, STATUS )

*  If we can sensibly do so, create a Set header in the output.  If all
*  the input NFDs in this subgroup have the same (non-zero) Set Index,
*  then use that as the Set Index of the output NDF.  In that case,
*  use the name of the output NDF itself as the Set Name attribute.
         IF ( SINDEX .NE. CCD1__BADSI ) THEN
            CALL CCD1_SETWR( NDFOUT, OUTNAM, SINDEX, AST__NOFRAME,
     :                       STATUS )
         END IF

*  Make sure that its data are in the processing precision.
         CALL NDF_STYPE( PTYPE, NDFOUT, 'Data,Variance', STATUS )

*  Get a temporary NDF big enough to contain all possible input NDF
*  sections in a stack NNDF big.
         CALL NDF_TEMP( PLACE, STATUS )
         CALL NDF_NEW( PTYPE, 1, 1, INSTA, PLACE, INWORK, STATUS )

*  Map the stack in.
         CALL NDF_MAP( INWORK, 'Data', PTYPE, 'WRITE', IPDWRK, EL,
     :                 STATUS )
         IF ( HAVVAR ) CALL NDF_MAP( INWORK, 'Variance', PTYPE, 'WRITE',
     :                               IPVWRK, EL, STATUS )

*  Initialise frames statistics buffer.
         DO 1 I = 1, NNDF
            NCON( I ) = 0.0D0
 1       CONTINUE

*  If we have variances then will process covariances - get the required
*  workspace.
         IF ( HAVVAR ) THEN
            NWRK4 = MAX( 1, ( ( NNDF+ 1 )**3 ) / 2 )
            CALL CCD1_MKTMP( NWRK4, '_DOUBLE', IDWRK4, STATUS )
            NWRK4 = MAX( 1, NWRK4 / NNDF )
            CALL CCD1_MPTMP( IDWRK4, 'WRITE', IPWRK4, STATUS )
         ENDIF

*  For each chunk of an NDF, find other chunks and enter them onto the
*  stack.
         DO 2 ICHUNK = 1, NCHUNK

*  Map in the corresponding output NDF to use as workspace.
            CALL NDF_CHUNK( NDFOUT, MXPIX, ICHUNK, OWORK, STATUS)
            CALL NDF_MAP( OWORK, 'Data', PTYPE, 'WRITE', IPDOUT, EL,
     :                    STATUS )
            IF ( HAVVAR ) CALL NDF_MAP( OWORK, 'Variance', PTYPE,
     :                                  'WRITE', IPVOUT, EL, STATUS )

*  Map in the NDFs one at a time, transferring them into the 3D stack.
            DO 3 I = 1, NNDF

*  Get the current chunk for each data_array.
               CALL NDF_CHUNK( STACK( I ), MXPIX, ICHUNK, NDFCUR,
     :                         STATUS )

*  Map this section in.
               CALL NDF_MAP( NDFCUR, 'Data', PTYPE, 'READ', IPDIN, EL,
     :                       STATUS )
               IF ( HAVVAR ) CALL NDF_MAP( NDFCUR, 'Variance', PTYPE,
     :                                     'READ', IPVIN, EL, STATUS )

*  Divide all the input data by the exposure factors.
               IF ( STATUS .NE. SAI__OK ) GO TO 99
               CVAL = 1.0D0 / EFACT( I )
               CALL CCD1_CMUL( BAD, PTYPE, IPDIN, EL, CVAL, IPDOUT,
     :                         STATUS )

*  Transfer this data to the data stack.
               CALL CCD1_PTINS( PTYPE, IPDWRK, EL, NNDF, I, IPDOUT,
     :                          STATUS )

*  If all variances exist then scale the variances appropriately,
*  otherwise use the inverse exposure factors as weights.
               IF ( HAVVAR ) THEN
                  CVAL = CVAL * CVAL
                  CALL CCD1_CMUL( BAD, PTYPE, IPVIN, EL, CVAL, IPVOUT,
     :                            STATUS )

*  Transfer this to the variances stack.
                  CALL CCD1_PTINS( PTYPE, IPVWRK, EL, NNDF, I, IPVOUT,
     :                             STATUS )
               END IF

*  Unmap the NDF data and variance arrays, and return for next.
               CALL NDF_UNMAP( NDFCUR, '*', STATUS )
               CALL NDF_ANNUL( NDFCUR, STATUS )
 3          CONTINUE

*  Do the work on the stack transferring the result to the chunk of the
*  output NDF, corresponding to the chunk of the input NDFs.
*  Combine all the NDFs in the stack using the method given.
            IF ( HAVVAR ) THEN
               IF ( PTYPE .EQ. '_REAL' ) THEN
                  CALL CCG1_CM1RR( %VAL( CNF_PVAL( IPDWRK ) ), EL, NNDF,
     :                             %VAL( CNF_PVAL( IPVWRK ) ),
     :                             IMETH, MINPIX, NITER,
     :                             NSIGMA, ALPHA, RMIN, RMAX,
     :                             %VAL( CNF_PVAL( IPDOUT ) ),
     :                             %VAL( CNF_PVAL( IPVOUT ) ), WRK1,
     :                             WRK2, WRK3,
     :                             %VAL( CNF_PVAL( IPWRK4 ) ), NWRK4,
     :                             NCON, POINT, USED, STATUS )
               ELSE IF ( PTYPE .EQ. '_DOUBLE ' ) THEN
                  CALL CCG1_CM1DD( %VAL( CNF_PVAL( IPDWRK ) ), EL, NNDF,
     :                             %VAL( CNF_PVAL( IPVWRK ) ),
     :                             IMETH, MINPIX, NITER,
     :                             NSIGMA, ALPHA, RMIN, RMAX,
     :                             %VAL( CNF_PVAL( IPDOUT ) ),
     :                             %VAL( CNF_PVAL( IPVOUT ) ), WRK1,
     :                             WRK2, WRK3,
     :                             %VAL( CNF_PVAL( IPWRK4 ) ), NWRK4,
     :                             NCON, POINT, USED, STATUS )
               END IF
            ELSE

*  No variances just use inverse weights.
               IF ( PTYPE .EQ. '_REAL' ) THEN
                  CALL CCG1_CM3RR( %VAL( CNF_PVAL( IPDWRK ) ),
     :                             EL, NNDF, INFACT,
     :                             IMETH, MINPIX, NITER, NSIGMA, ALPHA,
     :                             RMIN, RMAX,
     :                             %VAL( CNF_PVAL( IPDOUT ) ), WRK1,
     :                             WRK2, NCON, POINT, USED, STATUS )
               ELSE IF ( PTYPE .EQ. '_DOUBLE ' ) THEN
                  CALL CCG1_CM3DD( %VAL( CNF_PVAL( IPDWRK ) ),
     :                             EL, NNDF, INFACT,
     :                             IMETH, MINPIX, NITER, NSIGMA, ALPHA,
     :                             RMIN, RMAX,
     :                             %VAL( CNF_PVAL( IPDOUT ) ), WRK1,
     :                             WRK2, NCON, POINT, USED, STATUS )
               END IF
            END IF

*  Unmap the output section, ready for next chunks.
            CALL NDF_UNMAP( OWORK, '*', STATUS )
            CALL NDF_ANNUL( OWORK, STATUS )
 2       CONTINUE

*  Work out contribution statistics.
         DO 5 I = 1, NNDF
            STATS( I ) = NCON( I ) / DBLE( NPIX ) * 100.0D0
 5       CONTINUE

*  Set bad pixel flag in output NDF
         CALL NDF_SBAD( BAD, NDFOUT, 'Data', STATUS )

*  Set title of output NDF, propagating it if requested.
         CALL NDF_CINP( 'TITLE', NDFOUT, 'TITLE', STATUS )

*  Add the frame type to the output NDF.
         IF (  FTYPE .EQ. 'DARK' ) THEN
            CALL CCG1_STO0C( NDFOUT, 'FTYPE', 'MASTER_DARK', STATUS )
         ELSE IF ( FTYPE .EQ. 'FLASH' ) THEN
            CALL CCG1_STO0C( NDFOUT, 'FTYPE', 'MASTER_FLASH', STATUS )
         ELSE
            CALL CCG1_STO0C( NDFOUT, 'FTYPE', 'MASTER_CAL', STATUS )
         END IF

*  Touch the output NDF to leave audit-trail.
         CALL CCD1_TOUCH( NDFOUT, 'MAKECAL', STATUS )

*  Report MAKECAL parameters
         CALL CCD1_RCAL( FTYPE, STACK, NNDF, EFACT, USEEXT, STATS,
     :                   CMODE, IMETH, MINPIX, ALPHA, NSIGMA, NITER,
     :                   RMIN, RMAX, NDFOUT, PTYPE, DELETE, STATUS )

*  Release the NDF identifiers for this subgroup.
         DO I = 1, NNDF
            CALL NDF_ANNUL( STACK( I ), STATUS )
         END DO
      END DO

*  If requested delete all the input NDFs.
      IF ( DELETE .AND. STATUS .EQ. SAI__OK ) THEN
         DO 10 I = 1, NTOT
            CALL CCD1_NGDEL( INGRP, I, .TRUE., STATUS )
 10      CONTINUE
      END IF

*  Exit label - tidy up if possible.
 99   CONTINUE

*  End of NDF context, release all identifiers etc.
      CALL NDF_END( STATUS )

*  Make sure all workspace is released (ok to call these at any time as
*  CCD1_START initialises).
      CALL CCD1_FRTMP( -1, STATUS )
      CALL CCD1_MFREE( -1, STATUS )

*  Release group resources.
      CALL CCD1_GRDEL( INGRP, STATUS )
      CALL CCD1_GRDEL( KEYGRP, STATUS )
      DO I = 1, MIN( NSUB, CCD1__MXNDF )
         CALL CCD1_GRDEL( SUBGRP( I ), STATUS )
      END DO

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL CCD1_ERREP( 'MAKECAL_ERR',
     : 'MAKECAL: Error making master dark or flash calibration frame.',
     :   STATUS )
      END IF

*  Close down logging system write terminator.
      CALL CCD1_END( STATUS )

*  Restore the HDS input mapping mode.
      CALL HDS_TUNE( 'MAP', MAPINT, STATUS )

      END
* $Id$
