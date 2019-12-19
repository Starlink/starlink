      SUBROUTINE STATS( STATUS )
*+
*  Name:
*     STATS

*  Purpose:
*     Computes simple statistics for an NDF's pixels.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL STATS( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application computes and displays simple statistics for the
*     pixels in an NDF's data, quality or variance array.  The
*     statistics available are:
*     -  the pixel sum,
*     -  the pixel mean,
*     -  the pixel population standard deviation,
*     -  the pixel population skewness and excess kurtosis,
*     -  the value and position of the minimum- and maximum-valued
*     pixels,
*     -  the total number of pixels in the NDF,
*     -  the number of pixels used in the statistics, and
*     -  the number of pixels omitted.
*
*     Iterative K-sigma clipping may also be applied as an option
*     (see Parameter CLIP).
*
*     Order statistics (median and percentiles) may optionally be
*     derived and displayed (see Parameters ORDER and PERCENTILES).
*     Although this can be a relatively slow operation on large arrays,
*     unlike application HISTAT the reported order statistics are
*     accurate, not approximations, irrespective of the distribution of
*     values being analysed.

*  Usage:
*     stats ndf [comp] [clip] [logfile]

*  ADAM Parameters:
*     CLIP( ) = _REAL (Read)
*        An optional one-dimensional array of clipping levels to be
*        applied, expressed as standard deviations.  If a null value is
*        supplied for this parameter (the default), then no iterative
*        clipping will take place and the statistics computed will
*        include all the valid NDF pixels.
*
*        If an array of clipping levels is given, then the routine will
*        first compute statistics using all the available pixels. It
*        will then reject all those pixels whose values lie outside K
*        standard deviations of the mean (where K is the first value
*        supplied) and will then re-evaluate the statistics. This
*        rejection iteration is repeated in turn for each value in the
*        CLIP array.  A maximum of five values may be supplied, all of
*        which must be positive. [!]
*     COMP = LITERAL (Read)
*        The name of the NDF array component for which statistics are
*        required: "Data", "Error", "Quality" or "Variance" (where
*        "Error" is the alternative to "Variance" and causes the square
*        root of the variance values to be taken before computing the
*        statistics).  If "Quality" is specified, then the quality
*        values are treated as numerical values (in the range 0 to
*        255).  ["Data"]
*     KURTOSIS = _DOUBLE (Write)
*        The population excess kurtosis of all the valid pixels in the
*        NDF array.  This is the normal kurtosis minus 3, such that a
*        Gaussian distribution of values would generate an excess
*        kurtosis of 0.
*     LOGFILE = FILENAME (Write)
*        A text file into which the results should be logged.  If a null
*        value is supplied (the default), then no logging of results
*        will take place. [!]
*     MAXCOORD( ) = _DOUBLE (Write)
*        A one-dimensional array of values giving the WCS co-ordinates
*        of the centre of the (first) maximum-valued pixel found in the
*        NDF array.  The number of co-ordinates is equal to the number
*        of NDF dimensions.
*     MAXIMUM = _DOUBLE (Write)
*        The maximum pixel value found in the NDF array.
*     MAXPOS( ) = _INT64 (Write)
*        A one-dimensional array of pixel indices identifying the
*        (first) maximum-valued pixel found in the NDF array.  The
*        number of indices is equal to the number of NDF dimensions.
*     MAXWCS = LITERAL (Write)
*        The formatted WCS co-ordinates at the maximum pixel value.  The
*        individual axis values are comma separated.
*     MEAN = _DOUBLE (Write)
*        The mean value of all the valid pixels in the NDF array.
*     MEDIAN = _DOUBLE (Write)
*        The median value of all the valid pixels in the NDF array when
*        ORDER is TRUE.
*     MINCOORD( ) = _DOUBLE (Write)
*        A one-dimensional array of values giving the WCS co-ordinates
*        of the centre of the (first) minimum-valued pixel found in the
*        NDF array.  The number of co-ordinates is equal to the number
*        of NDF dimensions.
*     MINIMUM = _DOUBLE (Write)
*        The minimum pixel value found in the NDF array.
*     MINPOS( ) = _INT64 (Write)
*        A one-dimensional array of pixel indices identifying the
*        (first) minimum-valued pixel found in the NDF array.  The
*        number of indices is equal to the number of NDF dimensions.
*     MINWCS = LITERAL (Write)
*        The formatted WCS co-ordinates at the minimum pixel value.  The
*        individual axis values are comma separated.
*     NDF = NDF (Read)
*        The NDF data structure to be analysed.
*     NUMBAD = _INT64 (Write)
*        The number of pixels which were either not valid or were
*        rejected from the statistics during iterative K-sigma
*        clipping.
*     NUMGOOD = _INT64 (Write)
*        The number of NDF pixels which actually contributed to the
*        computed statistics.
*     NUMPIX = _INT64 (Write)
*        The total number of pixels in the NDF (both good and bad).
*     ORDER = _LOGICAL (Read)
*        Whether or not to calculate order statistics.  If set TRUE
*        the median and optionally percentiles are determined and
*        reported.  [FALSE]
*     PERCENTILES( 100 ) = _REAL (Read)
*         A list of percentiles to be found.  None are computed if this
*         parameter is null (!).  The percentiles must be in the range
*         0.0 to 100.0   This parameter is ignored unless ORDER is TRUE.
*         [!]
*     PERVAL() = _DOUBLE (Write)
*         The values of the percentiles of the good pixels in the NDF
*         array.  This parameter is only written when one or more
*         percentiles have been requested.
*     SIGMA = _DOUBLE (Write)
*        The population standard deviation of the pixel values in the
*        NDF array.
*     SKEWNESS = _DOUBLE (Write)
*        The population skewness of all the valid pixels in the NDF
*        array.
*     TOTAL = _DOUBLE (Write)
*        The sum of the pixel values in the NDF array.

*  Examples:
*     stats image
*        Computes and displays simple statistics for the data array in
*        the NDF called image.
*     stats image order percentiles=[25,75]
*        As the previous example but it also reports the median, 25 and
*        75 percentiles.
*     stats ndf=spectrum variance
*        Computes and displays simple statistics for the variance array
*        in the NDF called spectrum.
*     stats spectrum error
*        Computes and displays statistics for the variance array in the
*        NDF called spectrum, but takes the square root of the variance
*        values before doing so.
*     stats halley logfile=stats.dat
*        Computes statistics for the data array in the NDF called
*        halley, and writes the results to a logfile called stats.dat.
*     stats ngc1333 clip=[3.0,2.8,2.5]
*        Computes statistics for the data array in the NDF called
*        NGC1333, applying three iterations of K-sigma clipping.  The
*        statistics are first calculated for all the valid pixels in
*        the data array.  Those pixels with values lying more than 3.0
*        standard deviations from the mean are then rejected, and the
*        statistics are re-computed.  This process is then repeated
*        twice more, rejecting pixel values lying more than 2.8 and 2.5
*        standard deviations from the mean.  The final statistics are
*        displayed.

*  Related Applications:
*     KAPPA: HISTAT, NDFTRACE; Figaro: ISTAT.

*  Implementation Status:
*     -  This routine correctly processes the AXIS, DATA, VARIANCE,
*     QUALITY, TITLE, and HISTORY components of the NDF.
*     -  Processing of bad pixels and automatic quality masking are
*     supported.
*     -  All non-complex numeric data types can be handled.  Arithmetic
*     is performed using double-precision floating point.
*     -  Any number of NDF dimensions is supported.
*     -  Huge NDF are supported.

*  Copyright:
*     Copyright (C) 1991-1992 Science & Engineering Research Council.
*     Copyright (C) 2004 Central Laboratory of the Research Councils.
*     Copyright (C) 2007, 2009, 2010. 2013 Science & Technology
*     Facilities Council.
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
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     MJC: Malcolm J. Currie  STARLINK
*     DSB: David S. Berry STARLINK
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     19-MAR-1991 (RFWS):
*        Complete re-write of earlier routine to use NDF_ routines and
*        the extra facilities these provide.
*     11-APR-1991 (RFWS):
*        Improved the prologue.
*     1992 March 3 (MJC):
*        Replaced AIF parameter-system calls by the extended PAR
*        library.
*     6-AUG-2004 (DSB):
*        Display current Frame WCS coords at max and min pixel
*        positions.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL.
*     18-MAY-2007 (DSB):
*        Added parameters MINWCS and MAXWCS.
*     2009 June 19 (MJC):
*        Added order statistics.
*     20-AUG-2009 (DSB):
*        Changed calculation of order statistics to ignore bad pixels.
*     2009 August 20 (MJC):
*        Call new KPG_STOSx subroutine to evaluate order statistics.
*     2009 August 21 (MJC):
*        Do not map signed integer types directly to enable the ordered
*        statistics to be calculated with KPG_STOSx now restricted to
*        BDIRW instantiations.
*     2010 July 22 (MJC):
*        Uses a recursive algorithm.  It calculates the population
*        skewness and kurtosis.  The standard deviation is now a
*        population statistic, rather than the sample statistic
*        formerly.  In practice this will only make a significant
*        difference for small NDF arrays.
*     2013 August 23 (MJC):
*        Ordered statistics now respect sigma clipping.
*     5-DEC-2019 (DSB):
*        Support huge NDFs.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST functions and constants
      INCLUDE 'PAR_ERR'          ! PAR_ error codes
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT primitive data constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MXCLIP             ! Max. number of clipping levels
      PARAMETER ( MXCLIP = 5 )

      INTEGER NPRCTL             ! Maximum number of percentiles
      PARAMETER( NPRCTL = 100 )

      INTEGER SZBUF              ! Size of text buffer
      PARAMETER ( SZBUF = 200 )

*  Local Variables:
      BYTE BQUANT( NPRCTL )      ! Byte quantile values
      CHARACTER * ( 8 ) COMP     ! Name of array component to analyse
      CHARACTER * ( NDF__SZFTP ) DTYPE ! Data type for output components
      CHARACTER * ( 8 ) MCOMP    ! Component name for mapping arrays
      CHARACTER * ( NDF__SZTYP ) TYPE ! Numeric type for processing
      CHARACTER * ( SZBUF ) BUF  ! Text buffer
      CHARACTER * ( 255 ) MAXWCS ! Formatted max WCS position
      CHARACTER * ( 255 ) MINWCS ! Formatted max WCS position
      DOUBLE PRECISION CRANGE( 2 ) ! Clipping range
      DOUBLE PRECISION DMAX      ! Max. value of pixels in array
      DOUBLE PRECISION DMAXC     ! Max. pixel value after clipping
      DOUBLE PRECISION DMIN      ! Min. value of pixels in array
      DOUBLE PRECISION DMINC     ! Min. pixel value after clipping
      DOUBLE PRECISION DSTAT( 7 ) ! Array of floating-point  statistics
      DOUBLE PRECISION DSTATC( 7 ) ! Array of clipped f.p. statistics
      DOUBLE PRECISION KURT      ! Kurtosis
      DOUBLE PRECISION KURTC     ! Kurtosis (after clipping)
      DOUBLE PRECISION MAXC( NDF__MXDIM ) ! Co-ordinates of max. pixel
      DOUBLE PRECISION MAXCC( NDF__MXDIM ) ! Max. pixel coords (clipped)
      DOUBLE PRECISION MEAN      ! Mean of pixels in array
      DOUBLE PRECISION MEANC     ! Mean of pixels after clipping
      DOUBLE PRECISION MEDIAN( 2 ) ! Medians of pixels in array
      DOUBLE PRECISION MINC( NDF__MXDIM ) ! Co-ordinates of min. pixel
      DOUBLE PRECISION MINCC( NDF__MXDIM ) ! Min. pixel coords (clipped)
      DOUBLE PRECISION MODE      ! Mode of pixels in array (dummy)
      DOUBLE PRECISION PERVAL( NPRCTL * 2 ) ! Values at the percentiles
      DOUBLE PRECISION SKEW      ! Skewness
      DOUBLE PRECISION SKEWC     ! Skewness (after clipping)
      DOUBLE PRECISION STDEV     ! Standard devn. of pixels in array
      DOUBLE PRECISION STDEVC    ! Std. devn. of pixels after clipping
      DOUBLE PRECISION SUM       ! Sum of pixels in array
      DOUBLE PRECISION SUMC      ! Sum of pixels after clipping
      INTEGER I                  ! Loop counter for NDF dimensions
      INTEGER ICLIP              ! Loop counter for clipping levels
      INTEGER IFIL               ! File descriptor for logfile
      INTEGER IWCS               ! Pointer to WCS FrameSet
      INTEGER J                  ! Loop counter for clipped order stats
      INTEGER NC                 ! No. characters in text buffer
      INTEGER NCLIP              ! Number of clipping iterations
      INTEGER NDF                ! NDF identifier
      INTEGER NDIM               ! Number of NDF dimensions
      INTEGER NUMPER             ! Number of percentiles
      INTEGER NWCS               ! Number of WCS axes
      INTEGER PNTR( 1 )          ! Pointer to mapped NDF array
      INTEGER*8 EL               ! Number of array elements mapped
      INTEGER*8 IMAX( 1 )        ! Vector index of max. pixel
      INTEGER*8 IMAXC( 1 )       ! Vector index of max. clipped pixel
      INTEGER*8 IMIN( 1 )        ! Vector index of min. pixel
      INTEGER*8 IMINC( 1 )       ! Vector index of min. clipped pixel
      INTEGER*8 ISTAT( 3 )       ! Array of integer statistics
      INTEGER*8 ISTATC( 3 )      ! Array of clipped integer statistics
      INTEGER*8 LBND( NDF__MXDIM ) ! NDF lower bounds
      INTEGER*8 MAXP( NDF__MXDIM ) ! Indices of maximum-valued pixel
      INTEGER*8 MAXPC( NDF__MXDIM )! Maximum pixel indices after clipping
      INTEGER*8 MINP( NDF__MXDIM ) ! Indices of minimum-valued pixel
      INTEGER*8 MINPC( NDF__MXDIM )! Minimum pixel indices after clipping
      INTEGER*8 NGOOD            ! No. valid pixels in array
      INTEGER*8 NGOODC           ! No. valid pixels after clipping
      INTEGER*8 UBND( NDF__MXDIM )! NDF upper bounds
      LOGICAL BAD                ! Bad-pixel flag
      LOGICAL DOPRCT             ! Percentiles have been supplied?
      LOGICAL LOGFIL             ! Log file required?
      LOGICAL ORDER              ! Calculate order statistics?
      LOGICAL THERE              ! Array component exists?
      REAL CLIP( MXCLIP )        ! Array of clipping limits
      REAL PERCNT( NPRCTL )      ! Percentiles

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! Declarations of conversion routines
      INCLUDE 'NUM_DEF_CVT'      ! Definitions of conversion routines

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Initialise.
      LOGFIL = .FALSE.

*  Obtain the NDF to be analysed.
      CALL LPG_ASSOC( 'NDF', 'READ', NDF, STATUS )

*  Get its WCS FrameSet
      CALL KPG1_GTWCS( NDF, IWCS, STATUS )

*  Get the number of WCS axes.
      NWCS = AST_GETI( IWCS, 'Nout', STATUS )

*  Determine which array component is to be analysed, converting
*  'ERROR' into 'VARIANCE'.
      CALL PAR_CHOIC( 'COMP', 'Data', 'Data,Error,Quality,Variance',
     :                .FALSE., COMP, STATUS )
      MCOMP = COMP
      IF ( COMP .EQ. 'ERROR' ) COMP = 'VARIANCE'

*  Check that the required component exists and report an error if it
*  does not.
      CALL NDF_STATE( NDF, COMP, THERE, STATUS )
      IF ( ( STATUS .EQ. SAI__OK ) .AND. ( .NOT. THERE ) ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'COMP', COMP )
         CALL NDF_MSG( 'NDF', NDF )
         CALL ERR_REP( 'STATS_NOCOMP',
     :                 'The ^COMP component is undefined in the NDF ' //
     :                 'structure ^NDF', STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Defer error reporting and obtain an array of clipping limits to be
*  applied. Constrain the values to be positive.
      NCLIP = 0
      CALL ERR_MARK
      CALL PAR_GDRVR( 'CLIP', MXCLIP, VAL__SMLR, VAL__MAXR, CLIP,
     :                NCLIP, STATUS )

*  Interpret a null value as indicating that no clipping should be
*  applied.
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         NCLIP = 0
      END IF
      CALL ERR_RLSE

*  Are order statistics required?
      DOPRCT = .FALSE.
      ORDER = .FALSE.
      CALL PAR_GET0L( 'ORDER', ORDER, STATUS )
      IF ( ORDER ) THEN

*  Defer error reporting and obtain an array of percentiles to be
*  calculated.  Constrain the values to be between the minimum and
*  maximum data values.
         CALL ERR_MARK
         CALL PAR_GDRVR( 'PERCENTILES', NPRCTL, 0.0, 100.0, PERCNT,
     :                   NUMPER, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN

*  Null is a valid response to say do not compute percentiles.  Make
*  the number of percentiles one and flag the value, so that the
*  calculation and display routines can handle and recognise that
*  there are no percentile values to calculate and report.
            IF ( STATUS .EQ. PAR__NULL ) THEN
               CALL ERR_ANNUL( STATUS )
               NUMPER = 1
            END IF

         ELSE
            DOPRCT = .TRUE.
         END IF
         CALL ERR_RLSE
      END IF

*  Obtain an optional file for logging the results. A null value,
*  meaning no logfile is required, is handled invisibly.
      LOGFIL = .FALSE.
      CALL ERR_MARK
      CALL FIO_ASSOC( 'LOGFILE', 'WRITE', 'LIST', SZBUF, IFIL, STATUS )

      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
      ELSE IF ( STATUS .EQ. SAI__OK ) THEN
         LOGFIL = .TRUE.
      END IF
      CALL ERR_RLSE

*  Display the NDF name, also sending it to the logfile if necessary.
      CALL MSG_BLANK( STATUS )
      IF ( LOGFIL ) CALL FIO_WRITE( IFIL, ' ', STATUS )
      CALL NDF_MSG( 'NDF', NDF )
      CALL MSG_LOAD( 'NDFNAME',
     :               '   Pixel statistics for the NDF structure ^NDF',
     :               BUF, NC, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL MSG_SETC( 'MESSAGE', BUF( : NC ) )
         CALL MSG_OUT( ' ', '^MESSAGE', STATUS )
         IF ( LOGFIL ) CALL FIO_WRITE( IFIL, BUF( : NC ), STATUS )
      END IF

*  Display (and log) the NDF's title.
      CALL MSG_BLANK( STATUS )
      IF ( LOGFIL ) CALL FIO_WRITE( IFIL, ' ', STATUS )
      CALL NDF_CMSG( 'TITLE', NDF, 'Title', STATUS )
      CALL MSG_LOAD( 'NDFTITLE',
     :               '      Title                     : ^TITLE',
     :               BUF, NC, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL MSG_SETC( 'MESSAGE', BUF( : NC ) )
         CALL MSG_OUT( ' ', '^MESSAGE', STATUS )
         IF ( LOGFIL ) CALL FIO_WRITE( IFIL, BUF( : NC ), STATUS )
      END IF

*  Display (and log) the name of the component being analysed.
      CALL MSG_SETC( 'COMP', MCOMP )
      CALL MSG_LOAD( 'NDFCOMP',
     :               '      NDF array analysed        : ^COMP',
     :               BUF, NC, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL MSG_SETC( 'MESSAGE', BUF( : NC ) )
         CALL MSG_OUT( ' ', '^MESSAGE', STATUS )
         IF ( LOGFIL ) CALL FIO_WRITE( IFIL, BUF( : NC ), STATUS )
      END IF

*  If a logfile is in use, display its name.
      IF ( LOGFIL ) CALL MSG_OUT( 'LOG',
     :              '      Logging to file           : $LOGFILE',
     :                            STATUS )

*  Obtain the numeric type of the NDF array component to be analysed.
      TYPE = '_REAL'
      CALL NDF_MTYPE( '_BYTE,_WORD,_INTEGER,_INT64,_REAL,_DOUBLE',
     :                NDF, NDF, COMP, TYPE, DTYPE, STATUS )

*  Map the array using this numeric type and see whether there may be
*  bad pixels present.
      CALL KPG1_MAP8( NDF, MCOMP, TYPE, 'READ', PNTR, EL, STATUS )
      IF ( COMP .EQ. 'QUALITY' ) THEN
         BAD = .FALSE.
      ELSE
         CALL NDF_BAD( NDF, COMP, .FALSE., BAD, STATUS )
      END IF

*  Call the appropriate routine to compute the statistics.
      IF ( TYPE .EQ. '_BYTE' ) THEN
         CALL KPG_OSTA8B( BAD, EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                   NCLIP, CLIP, ISTAT, DSTAT,
     :                   ISTATC, DSTATC, STATUS )

      ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
         CALL KPG_OSTA8D( BAD, EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                   NCLIP, CLIP, ISTAT, DSTAT,
     :                   ISTATC, DSTATC, STATUS )

      ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
         CALL KPG_OSTA8I( BAD, EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                   NCLIP, CLIP, ISTAT, DSTAT,
     :                   ISTATC, DSTATC, STATUS )

      ELSE IF ( TYPE .EQ. '_INT64' ) THEN
         CALL KPG_OSTA8K( BAD, EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                   NCLIP, CLIP, ISTAT, DSTAT,
     :                   ISTATC, DSTATC, STATUS )

      ELSE IF ( TYPE .EQ. '_REAL' ) THEN
         CALL KPG_OSTA8R( BAD, EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                   NCLIP, CLIP, ISTAT, DSTAT,
     :                   ISTATC, DSTATC, STATUS )

      ELSE IF ( TYPE .EQ. '_WORD' ) THEN
         CALL KPG_OSTA8W( BAD, EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                   NCLIP, CLIP, ISTAT, DSTAT,
     :                   ISTATC, DSTATC, STATUS )
      END IF

*  Extract the individual statistics from the arrays.
      NGOOD = ISTAT( 1 )
      IMIN( 1 ) = ISTAT( 2 )
      IMAX( 1 ) = ISTAT( 3 )
      DMIN = DSTAT( 1 )
      DMAX = DSTAT( 2 )
      SUM = DSTAT( 3 )
      MEAN = DSTAT( 4 )
      STDEV = DSTAT( 5 )
      SKEW = DSTAT( 6 )
      KURT = DSTAT( 7 )

      NGOODC = ISTATC( 1 )
      IMINC( 1 ) = ISTATC( 2 )
      IMAXC( 1 ) = ISTATC( 3 )
      DMINC = DSTATC( 1 )
      DMAXC = DSTATC( 2 )
      SUMC = DSTATC( 3 )
      MEANC = DSTATC( 4 )
      STDEVC = DSTATC( 5 )
      SKEWC = DSTATC( 6 )
      KURTC = DSTATC( 7 )

*  Obtain the NDF bounds and initialise the indices of the minimum and
*  maximim pixel positions and co-ordinates.
      CALL NDF_BOUND8( NDF, NDF__MXDIM, LBND, UBND, NDIM, STATUS )
      DO 1 I = 1, NDIM
         MINP( I ) = VAL__BADK
         MINPC( I ) = VAL__BADK
         MAXP( I ) = VAL__BADK
         MAXPC( I ) = VAL__BADK
         MINC( I ) = VAL__BADD
         MINCC( I ) = VAL__BADD
         MAXC( I ) = VAL__BADD
         MAXCC( I ) = VAL__BADD
 1    CONTINUE

*  If available, convert the minimum and maximum pixel locations into
*  N-dimensional indices and then into co-ordinate values.
      IF ( NGOOD .NE. 0 ) THEN
         CALL KPG1_VEC2N8( 1, IMIN, NDIM, LBND, UBND, MINPC, STATUS )
         CALL KPG1_VEC2N8( 1, IMAX, NDIM, LBND, UBND, MAXPC, STATUS )
         CALL KPG1_PX2AX8( NDIM, MINPC, NDF, MINCC, STATUS )
         CALL KPG1_PX2AX8( NDIM, MAXPC, NDF, MAXCC, STATUS )

         DO I = 1, NDIM
            MINP( I ) = MINPC( I )
            MAXP( I ) = MAXPC( I )
         END DO

         DO I = 1, NWCS
            MINC( I ) = MINCC( I )
            MAXC( I ) = MAXCC( I )
         END DO

      END IF

*  Assign undefined values to the ordered statistics so by default
*  they are not reported.
      MEDIAN( 1 ) = VAL__BADD
      MEDIAN( 2 ) = VAL__BADD
      IF ( .NOT. DOPRCT ) THEN
         PERCNT( 1 ) = VAL__BADR
         PERVAL( 1 ) = VAL__BADD
      END IF
      MODE = VAL__BADD

*  Calculate the ordered statistics.
*  =================================
      IF ( ORDER ) THEN

*  Use a brute-force sort of the data.

*  Call the appropriate routine to quicksort the array and then find the
*  order statistics.
         CRANGE( 1 ) = DMINC
         CRANGE( 2 ) = DMAXC
         IF ( TYPE .EQ. '_BYTE' ) THEN
            CALL KPG_STOC8B( EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ), NGOOD,
     :                      NUMPER, PERCNT, CRANGE, MEDIAN, PERVAL,
     :                      STATUS )

         ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
            CALL KPG_STOC8D( EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ), NGOOD,
     :                      NUMPER, PERCNT, CRANGE, MEDIAN, PERVAL,
     :                      STATUS )

         ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
            CALL KPG_STOC8I( EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ), NGOOD,
     :                      NUMPER, PERCNT, CRANGE, MEDIAN, PERVAL,
     :                      STATUS )

         ELSE IF ( TYPE .EQ. '_INT64' ) THEN
            CALL KPG_STOC8K( EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ), NGOOD,
     :                      NUMPER, PERCNT, CRANGE, MEDIAN, PERVAL,
     :                      STATUS )

         ELSE IF ( TYPE .EQ. '_REAL' ) THEN
            CALL KPG_STOC8R( EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ), NGOOD,
     :                      NUMPER, PERCNT, CRANGE, MEDIAN, PERVAL,
     :                      STATUS )

         ELSE IF ( TYPE .EQ. '_WORD' ) THEN
            CALL KPG_STOC8W( EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ), NGOOD,
     :                      NUMPER, PERCNT, CRANGE, MEDIAN, PERVAL,
     :                      STATUS )

         END IF
      END IF

*  Display the statistics, using the most appropriate floating-point
*  precision.
      IF ( TYPE .EQ. '_DOUBLE' .OR. TYPE .EQ. '_INT64' ) THEN
         CALL KPG1_STDS8D( IWCS, NDIM, EL, NGOOD, DMIN, MINP, MINC,
     :                    DMAX, MAXP, MAXC, SUM, MEAN, STDEV, SKEW,
     :                    KURT, MEDIAN( 1 ), MODE, MAX( 1, NUMPER ),
     :                    PERCNT, PERVAL, MAXWCS, MINWCS, STATUS )
      ELSE
         CALL KPG1_STDS8R( IWCS, NDIM, EL, NGOOD, DMIN, MINP, MINC,
     :                    DMAX, MAXP, MAXC, SUM, MEAN, STDEV, SKEW,
     :                    KURT, MEDIAN( 1 ), MODE, MAX( 1, NUMPER ),
     :                    PERCNT, PERVAL, MAXWCS, MINWCS, STATUS )
      END IF

*  Also write the statistics to the logfile, if used.
      IF ( LOGFIL ) THEN
         IF ( TYPE .EQ. '_DOUBLE' .OR. TYPE .EQ. '_INT64' ) THEN
            CALL KPG1_STFL8D( IWCS, NDIM, EL, NGOOD, DMIN, MINP, MINC,
     :                       DMAX, MAXP, MAXC, SUM, MEAN, STDEV, SKEW,
     :                       KURT, MEDIAN(  1 ), MODE, MAX( 1, NUMPER ),
     :                       PERCNT, PERVAL, IFIL, STATUS )
         ELSE
            CALL KPG1_STFL8R( IWCS, NDIM, EL, NGOOD, DMIN, MINP, MINC,
     :                       DMAX, MAXP, MAXC, SUM, MEAN, STDEV, SKEW,
     :                       KURT, MEDIAN( 1 ), MODE, MAX( 1, NUMPER ),
     :                       PERCNT, PERVAL, IFIL, STATUS )
         END IF
      END IF

*  If clipping was performed, then determine the N-dimensional
*  co-ordinates of the minimum and maximum pixel values after clipping
*  and convert them into co-ordinate values.
      IF ( NCLIP .NE. 0 ) THEN
         IF ( NGOODC .NE. 0 ) THEN
            CALL KPG1_VEC2N8( 1, IMINC, NDIM, LBND, UBND, MINPC,
     :                        STATUS )
            CALL KPG1_VEC2N8( 1, IMAXC, NDIM, LBND, UBND, MAXPC,
     :                        STATUS )
            CALL KPG1_PX2AX8( NDIM, MINPC, NDF, MINCC, STATUS )
            CALL KPG1_PX2AX8( NDIM, MAXPC, NDF, MAXCC, STATUS )
         END IF

*  Generate a heading for the clipped statistics, including a list of
*  the clipping levels.
         NC = 0
         CALL CHR_PUTC( '      After clipping at ', BUF, NC )
         DO 3 ICLIP = 1, NCLIP
            IF ( ( ICLIP .GT. 1 ) .AND. ( ICLIP .EQ. NCLIP ) ) THEN
               CALL CHR_PUTC( ' & ', BUF, NC )
            ELSE IF ( ICLIP .GT. 1 ) THEN
               CALL CHR_PUTC( ', ', BUF, NC )
            END IF
            CALL CHR_PUTR( CLIP( ICLIP ), BUF, NC )
 3       CONTINUE
         CALL CHR_PUTC( ' standard deviations:', BUF, NC )

*  Display the heading, followed by the clipped statistics using the
*  most appropriate floating-point precision.
         CALL MSG_OUT( 'CLIPDONE', BUF( : NC ), STATUS )
         IF ( TYPE .EQ. '_DOUBLE' .OR. TYPE .EQ. '_INT64' ) THEN
            CALL KPG1_STDS8D( IWCS, NDIM, EL, NGOODC, DMINC, MINPC,
     :                       MINCC, DMAXC, MAXPC, MAXCC, SUMC, MEANC,
     :                       STDEVC, SKEWC, KURTC, MEDIAN( 2 ), MODE,
     :                       MAX( 1, NUMPER ), PERCNT,
     :                       PERVAL( 1 + NUMPER ), MAXWCS, MINWCS,
     :                       STATUS )
         ELSE
            CALL KPG1_STDS8R( IWCS, NDIM, EL, NGOODC, DMINC, MINPC,
     :                       MINCC, DMAXC, MAXPC, MAXCC, SUMC, MEANC,
     :                       STDEVC, SKEWC, KURTC, MEDIAN( 2 ), MODE,
     :                       MAX( 1, NUMPER ), PERCNT,
     :                       PERVAL( 1 + NUMPER ), MAXWCS, MINWCS,
     :                       STATUS )
         END IF

*  Also write the statistics to the log file, if used.
         IF ( LOGFIL ) THEN
            CALL FIO_WRITE( IFIL, BUF( : NC ), STATUS )
            IF ( TYPE .EQ. '_DOUBLE' .OR. TYPE .EQ. '_INT64' ) THEN
               CALL KPG1_STFL8D( IWCS, NDIM, EL, NGOODC, DMINC, MINPC,
     :                          MINCC, DMAXC, MAXPC, MAXCC, SUMC, MEANC,
     :                          STDEVC, SKEWC, KURTC, MEDIAN( 2 ), MODE,
     :                          MAX( 1, NUMPER ), PERCNT, PERVAL, IFIL,
     :                          MAX( 1, NUMPER ), PERCNT,
     :                          PERVAL( 1 + NUMPER ), IFIL, STATUS )
            ELSE
               CALL KPG1_STFL8R( IWCS, NDIM, EL, NGOODC, DMINC, MINPC,
     :                          MINCC, DMAXC, MAXPC, MAXCC, SUMC, MEANC,
     :                          STDEVC, SKEWC, KURTC, MEDIAN( 2 ), MODE,
     :                          MAX( 1, NUMPER ), PERCNT,
     :                          PERVAL( 1 + NUMPER ), IFIL, STATUS )
            END IF
         END IF
      END IF

*  Write the final results to the output parameters.
      CALL PAR_PUT0D( 'MAXIMUM', DMAXC, STATUS )
      CALL PAR_PUT0C( 'MAXWCS', MAXWCS, STATUS )
      CALL PAR_PUT0C( 'MINWCS', MINWCS, STATUS )
      CALL PAR_PUT0D( 'MEAN', MEANC, STATUS )
      CALL PAR_PUT0D( 'MINIMUM', DMINC, STATUS )
      CALL PAR_PUT0D( 'SIGMA', STDEVC, STATUS )
      CALL PAR_PUT0D( 'SKEWNESS', SKEWC, STATUS )
      CALL PAR_PUT0D( 'KURTOSIS', KURTC, STATUS )
      CALL PAR_PUT0D( 'TOTAL', SUMC, STATUS )
      CALL PAR_PUT0K( 'NUMBAD', EL - NGOODC, STATUS )
      CALL PAR_PUT0K( 'NUMGOOD', NGOODC, STATUS )
      CALL PAR_PUT0K( 'NUMPIX', EL, STATUS )
      CALL PAR_PUT1D( 'MAXCOORD', NWCS, MAXCC, STATUS )
      CALL PAR_PUT1D( 'MINCOORD', NWCS, MINCC, STATUS )
      CALL PAR_PUT1K( 'MAXPOS', NDIM, MAXPC, STATUS )
      CALL PAR_PUT1K( 'MINPOS', NDIM, MINPC, STATUS )
      IF ( ORDER ) THEN
         IF ( NCLIP .NE. 0 ) THEN
            CALL PAR_PUT0D( 'MEDIAN', MEDIAN( 2 ), STATUS )
         ELSE
            CALL PAR_PUT0D( 'MEDIAN', MEDIAN( 1 ), STATUS )
         END IF
      END IF

*  Only write percentiles values if any percentiles were given.
      IF ( DOPRCT ) THEN
         IF ( NCLIP .NE. 0 ) THEN
            CALL PAR_PUT1D( 'PERVAL', NUMPER, PERVAL( 1 + NUMPER ),
     :                      STATUS )
         ELSE
            CALL PAR_PUT1D( 'PERVAL', NUMPER, PERVAL, STATUS )
         END IF
      END IF

*  Arrive here if an error occurs.
 99   CONTINUE

*  Annul the WCS FrameSet
      CALL AST_ANNUL( IWCS, STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  Close the logfile, if used.
      IF ( LOGFIL ) CALL FIO_ANNUL( IFIL, STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'STATS_ERR',
     :   'STATS: Error computing simple statistics for an NDF''s ' //
     :   'pixels.', STATUS )
      END IF

      END
