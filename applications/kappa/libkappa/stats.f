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
*     -  the pixel standard deviation,
*     -  the value and position of the minimum- and maximum-valued
*     pixels,
*     -  the total number of pixels in the NDF,
*     -  the number of pixels used in the statistics, and
*     -  the number of pixels omitted.
*
*     Iterative K-sigma clipping may also be applied as an option.

*  Usage:
*     stats ndf [comp] [clip] [logfile]

*  ADAM Parameters:
*     CLIP( ) = _REAL (Read)
*        An optional 1-dimensional array of clipping levels to be
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
*        CLIP array.  A maximum of 5 values may be supplied, all of
*        which must be positive. [!]
*     COMP = LITERAL (Read)
*        The name of the NDF array component for which statistics are
*        required: "Data", "Error", "Quality" or "Variance" (where
*        "Error" is the alternative to "Variance" and causes the square
*        root of the variance values to be taken before computing the
*        statistics).  If "Quality" is specified, then the quality
*        values are treated as numerical values (in the range 0 to
*        255).  ["Data"]
*     LOGFILE = FILENAME (Write)
*        A text file into which the results should be logged.  If a null
*        value is supplied (the default), then no logging of results
*        will take place. [!]
*     MAXCOORD( ) = _DOUBLE (Write)
*        A 1-dimensional array of values giving the WCS co-ordinates of
*        the centre of the (first) maximum-valued pixel found in the
*        NDF array.  The number of co-ordinates is equal to the number of
*        NDF dimensions.
*     MAXIMUM = _DOUBLE (Write)
*        The maximum pixel value found in the NDF array.
*     MAXPOS( ) = _INTEGER (Write)
*        A 1-dimensional array of pixel indices identifying the (first)
*        maximum-valued pixel found in the NDF array.  The number of
*        indices is equal to the number of NDF dimensions.
*     MEAN = _DOUBLE (Write)
*        The mean value of all the valid pixels in the NDF array.
*     MINCOORD( ) = _DOUBLE (Write)
*        A 1-dimensional array of values giving the WCS co-ordinates of
*        the centre of the (first) minimum-valued pixel found in the
*        NDF array.  The number of co-ordinates is equal to the number
*        of NDF dimensions.
*     MINIMUM = _DOUBLE (Write)
*        The minimum pixel value found in the NDF array.
*     MINPOS( ) = _INTEGER (Write)
*        A 1-dimensional array of pixel indices identifying the (first)
*        minimum-valued pixel found in the NDF array.  The number of
*        indices is equal to the number of NDF dimensions.
*     NDF = NDF (Read)
*        The NDF data structure to be analysed.
*     NUMBAD = _INTEGER (Write)
*        The number of pixels which were either not valid or were
*        rejected from the statistics during iterative K-sigma
*        clipping.
*     NUMGOOD = _INTEGER (Write)
*        The number of NDF pixels which actually contributed to the
*        computed statistics.
*     NUMPIX = _INTEGER (Write)
*        The total number of pixels in the NDF (both good and bad).
*     SIGMA = _DOUBLE (Write)
*        The standard deviation of the pixel values in the NDF array.
*     TOTAL = _DOUBLE (Write)
*        The sum of the pixel values in the NDF array.

*  Examples:
*     stats image
*        Computes and displays simple statistics for the data array in
*        the NDF called image.
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

*  Copyright:
*     Copyright (C) 1991-1992 Science & Engineering Research Council.
*     Copyright (C) 2004 Central Laboratory of the Research Councils.
*     All Rights Reserved.

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

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
*        Display current Frame WCS coords at max and min pixel positions.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL
*     {enter_further_changes_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_ERR'          ! PAR_ error codes
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT primitive data constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MXCLIP             ! Max. number of clipping levels
      PARAMETER ( MXCLIP = 5 )

      INTEGER SZBUF              ! Size of text buffer
      PARAMETER ( SZBUF = 132 )

*  Local Variables:
      CHARACTER * ( 8 ) COMP     ! Name of array component to analyse
      CHARACTER * ( 8 ) MCOMP    ! Component name for mapping arrays
      CHARACTER * ( NDF__SZTYP ) TYPE ! Numeric type for processing
      CHARACTER * ( SZBUF ) BUF  ! Text buffer
      DOUBLE PRECISION DMAX      ! Max. value of pixels in array
      DOUBLE PRECISION DMAXC     ! Max. pixel value after clipping
      DOUBLE PRECISION DMIN      ! Min. value of pixels in array
      DOUBLE PRECISION DMINC     ! Min. pixel value after clipping
      DOUBLE PRECISION MAXC( NDF__MXDIM ) ! Co-ordinates of max. pixel
      DOUBLE PRECISION MAXCC( NDF__MXDIM ) ! Max. pixel coords (clipped)
      DOUBLE PRECISION MEAN      ! Mean of pixels in array
      DOUBLE PRECISION MEANC     ! Mean of pixels after clipping
      DOUBLE PRECISION MEDIAN    ! Median of pixels in array (dummy)
      DOUBLE PRECISION MINC( NDF__MXDIM ) ! Co-ordinates of min. pixel
      DOUBLE PRECISION MINCC( NDF__MXDIM ) ! Min. pixel coords (clipped)
      DOUBLE PRECISION MODE      ! Mode of pixels in array (dummy)
      REAL PERCNT( 1 )           ! Percentile level of pixels (dummy)
      DOUBLE PRECISION PERVAL( 1 ) ! Percentile value of pixels (dummy)
      DOUBLE PRECISION STDEV     ! Standard devn. of pixels in array
      DOUBLE PRECISION STDEVC    ! Std. devn. of pixels after clipping
      DOUBLE PRECISION SUM       ! Sum of pixels in array
      DOUBLE PRECISION SUMC      ! Sum of pixels after clipping
      INTEGER EL                 ! Number of array elements mapped
      INTEGER I                  ! Loop counter for NDF dimensions
      INTEGER ICLIP              ! Loop counter for clipping levels
      INTEGER IFIL               ! File descriptor for logfile
      INTEGER IMAX( 1 )          ! Vector index of max. pixel
      INTEGER IMAXC( 1 )         ! Vector index of max. clipped pixel
      INTEGER IMIN( 1 )          ! Vector index of min. pixel
      INTEGER IMINC( 1 )         ! Vector index of min. clipped pixel
      INTEGER IWCS               ! Pointer to WCS FrameSet
      INTEGER LBND( NDF__MXDIM ) ! NDF lower bounds
      INTEGER MAXP( NDF__MXDIM ) ! Indices of maximum-valued pixel
      INTEGER MAXPC( NDF__MXDIM ) ! Maximum pixel indices after clipping
      INTEGER MINP( NDF__MXDIM ) ! Indices of minimum-valued pixel
      INTEGER MINPC( NDF__MXDIM ) ! Minimum pixel indices after clipping
      INTEGER NC                 ! No. characters in text buffer
      INTEGER NCLIP              ! Number of clipping iterations
      INTEGER NDF                ! NDF identifier
      INTEGER NDIM               ! Number of NDF dimensions
      INTEGER NGOOD              ! No. valid pixels in array
      INTEGER NGOODC             ! No. valid pixels after clipping
      INTEGER PNTR( 1 )          ! Pointer to mapped NDF array
      INTEGER UBND( NDF__MXDIM ) ! NDF upper bounds
      LOGICAL BAD                ! Bad-pixel flag
      LOGICAL LOGFIL             ! Log file required?
      LOGICAL THERE              ! Array component exists?
      REAL CLIP( MXCLIP )        ! Array of clipping limits

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

*  Obtain an optional file for logging the results. A null value,
*  meaning no logfile is required, is handled invisibly.

      LOGFIL = .FALSE.
      CALL ERR_MARK
      CALL FIO_ASSOC( 'LOGFILE', 'WRITE', 'LIST', 132, IFIL, STATUS )

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
      CALL NDF_TYPE( NDF, COMP, TYPE, STATUS )

*  Map the array using this numeric type and see whether there may be
*  bad pixels present.
      CALL KPG1_MAP( NDF, MCOMP, TYPE, 'READ', PNTR, EL, STATUS )
      IF ( COMP .EQ. 'QUALITY' ) THEN
         BAD = .FALSE.
      ELSE
         CALL NDF_BAD( NDF, COMP, .FALSE., BAD, STATUS )
      END IF
      
*  Call the appropriate routine to compute the statistics.
      IF ( TYPE .EQ. '_BYTE' ) THEN
         CALL KPG1_STATB( BAD, EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ), 
     :                    NCLIP, CLIP,
     :                    NGOOD, IMIN( 1 ), DMIN, IMAX( 1 ),
     :                    DMAX, SUM, MEAN, STDEV, NGOODC,
     :                    IMINC( 1 ), DMINC, IMAXC( 1 ), DMAXC, SUMC,
     :                    MEANC, STDEVC, STATUS )
 
      ELSE IF ( TYPE .EQ. '_UBYTE' ) THEN
         CALL KPG1_STATUB( BAD, EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ), 
     :                     NCLIP, CLIP,
     :                     NGOOD, IMIN( 1 ), DMIN, IMAX( 1 ),
     :                     DMAX, SUM, MEAN, STDEV, NGOODC,
     :                     IMINC( 1 ), DMINC, IMAXC( 1 ), DMAXC, SUMC,
     :                     MEANC, STDEVC, STATUS )
 
      ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
         CALL KPG1_STATD( BAD, EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ), 
     :                    NCLIP, CLIP,
     :                    NGOOD, IMIN( 1 ), DMIN, IMAX( 1 ),
     :                    DMAX, SUM, MEAN, STDEV, NGOODC,
     :                    IMINC( 1 ), DMINC, IMAXC( 1 ), DMAXC, SUMC,
     :                    MEANC, STDEVC, STATUS )
 
      ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
         CALL KPG1_STATI( BAD, EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ), 
     :                    NCLIP, CLIP,
     :                    NGOOD, IMIN( 1 ), DMIN, IMAX( 1 ),
     :                    DMAX, SUM, MEAN, STDEV, NGOODC,
     :                    IMINC( 1 ), DMINC, IMAXC( 1 ), DMAXC, SUMC,
     :                    MEANC, STDEVC, STATUS )
 
      ELSE IF ( TYPE .EQ. '_REAL' ) THEN
         CALL KPG1_STATR( BAD, EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ), 
     :                    NCLIP, CLIP,
     :                    NGOOD, IMIN( 1 ), DMIN, IMAX( 1 ),
     :                    DMAX, SUM, MEAN, STDEV, NGOODC,
     :                    IMINC( 1 ), DMINC, IMAXC( 1 ), DMAXC, SUMC,
     :                    MEANC, STDEVC, STATUS )
 
      ELSE IF ( TYPE .EQ. '_WORD' ) THEN
         CALL KPG1_STATW( BAD, EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ), 
     :                    NCLIP, CLIP,
     :                    NGOOD, IMIN( 1 ), DMIN, IMAX( 1 ),
     :                    DMAX, SUM, MEAN, STDEV, NGOODC,
     :                    IMINC( 1 ), DMINC, IMAXC( 1 ), DMAXC, SUMC,
     :                    MEANC, STDEVC, STATUS )
 
      ELSE IF ( TYPE .EQ. '_UWORD' ) THEN
         CALL KPG1_STATUW( BAD, EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ), 
     :                     NCLIP, CLIP,
     :                     NGOOD, IMIN( 1 ), DMIN, IMAX( 1 ),
     :                     DMAX, SUM, MEAN, STDEV, NGOODC,
     :                     IMINC( 1 ), DMINC, IMAXC( 1 ), DMAXC, SUMC,
     :                     MEANC, STDEVC, STATUS )
      END IF

*  Obtain the NDF bounds and initialise the indices of the minimum and
*  maximim pixel positions and co-ordinates.
      CALL NDF_BOUND( NDF, NDF__MXDIM, LBND, UBND, NDIM, STATUS )
      DO 1 I = 1, NDIM
         MINP( I ) = VAL__BADI
         MINPC( I ) = VAL__BADI
         MAXP( I ) = VAL__BADI
         MAXPC( I ) = VAL__BADI
         MINC( I ) = VAL__BADD
         MINCC( I ) = VAL__BADD
         MAXC( I ) = VAL__BADD
         MAXCC( I ) = VAL__BADD
 1    CONTINUE

*  If available, convert the minimum and maximum pixel locations into
*  N-dimensional indices and then into co-ordinate values.
      IF ( NGOOD .NE. 0 ) THEN
         CALL KPG1_VEC2N( 1, IMIN, NDIM, LBND, UBND, MINPC, STATUS )
         CALL KPG1_VEC2N( 1, IMAX, NDIM, LBND, UBND, MAXPC, STATUS )
         CALL KPG1_PX2AX( NDIM, MINPC, NDF, MINCC, STATUS )
         CALL KPG1_PX2AX( NDIM, MAXPC, NDF, MAXCC, STATUS )
         DO 2 I = 1, NDIM
            MINP( I ) = MINPC( I )
            MAXP( I ) = MAXPC( I )
            MINC( I ) = MINCC( I )
            MAXC( I ) = MAXCC( I )
 2       CONTINUE
      END IF

*  Assign undefined values to the ordered statistics.
      MEDIAN = VAL__BADD
      MODE = VAL__BADD
      PERCNT( 1 ) = VAL__BADR
      PERVAL( 1 ) = VAL__BADD

*  Display the statistics, using the most appropriate floating-point
*  precision.
      IF ( TYPE .EQ. '_DOUBLE' ) THEN
         CALL KPG1_STDSD( IWCS, NDIM, EL, NGOOD, DMIN, MINP, MINC,
     :                    DMAX, MAXP, MAXC, SUM, MEAN, STDEV,
     :                    MEDIAN, MODE, 1, PERCNT, PERVAL, STATUS )
      ELSE
         CALL KPG1_STDSR( IWCS, NDIM, EL, NGOOD, DMIN, MINP, MINC,
     :                    DMAX, MAXP, MAXC, SUM, MEAN, STDEV,
     :                    MEDIAN, MODE, 1, PERCNT, PERVAL, STATUS )
      END IF

*  Also write the statistics to the logfile, if used.
      IF ( LOGFIL ) THEN
         IF ( TYPE .EQ. '_DOUBLE' ) THEN
            CALL KPG1_STFLD( IWCS, NDIM, EL, NGOOD, DMIN, MINP, MINC,
     :                       DMAX, MAXP, MAXC, SUM, MEAN, STDEV,
     :                       MEDIAN, MODE, 1, PERCNT, PERVAL,
     :                       IFIL, STATUS )
         ELSE
            CALL KPG1_STFLR( IWCS, NDIM, EL, NGOOD, DMIN, MINP, MINC,
     :                       DMAX, MAXP, MAXC, SUM, MEAN, STDEV,
     :                       MEDIAN, MODE, 1, PERCNT, PERVAL,
     :                       IFIL, STATUS )
         END IF
      END IF

*  If clipping was performed, then determine the N-dimensional
*  co-ordinates of the minimum and maximum pixel values after clipping
*  and convert them into co-ordinate values.
      IF ( NCLIP .NE. 0 ) THEN
         IF ( NGOODC .NE. 0 ) THEN
            CALL KPG1_VEC2N( 1, IMINC, NDIM, LBND, UBND, MINPC, STATUS )
            CALL KPG1_VEC2N( 1, IMAXC, NDIM, LBND, UBND, MAXPC, STATUS )
            CALL KPG1_PX2AX( NDIM, MINPC, NDF, MINCC, STATUS )
            CALL KPG1_PX2AX( NDIM, MAXPC, NDF, MAXCC, STATUS )
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
         IF ( TYPE .EQ. '_DOUBLE' ) THEN
            CALL KPG1_STDSD( IWCS, NDIM, EL, NGOODC, DMINC, MINPC, 
     :                       MINCC,
     :                       DMAXC, MAXPC, MAXCC, SUMC, MEANC, STDEVC,
     :                       MEDIAN, MODE, 1, PERCNT, PERVAL, STATUS )
         ELSE
            CALL KPG1_STDSR( IWCS, NDIM, EL, NGOODC, DMINC, MINPC, 
     :                       MINCC,
     :                       DMAXC, MAXPC, MAXCC, SUMC, MEANC, STDEVC,
     :                       MEDIAN, MODE, 1, PERCNT, PERVAL, STATUS )
         END IF

*  Also write the statistics to the log file, if used.
         IF ( LOGFIL ) THEN
            CALL FIO_WRITE( IFIL, BUF( : NC ), STATUS )
            IF ( TYPE .EQ. '_DOUBLE' ) THEN
               CALL KPG1_STFLD( IWCS, NDIM, EL, NGOODC, DMINC, MINPC, 
     :                          MINCC, DMAXC, MAXPC, MAXCC, SUMC, MEANC,
     :                          STDEVC, MEDIAN, MODE, 1, PERCNT, PERVAL,
     :                          IFIL, STATUS )
            ELSE
               CALL KPG1_STFLR( IWCS, NDIM, EL, NGOODC, DMINC, MINPC, 
     :                          MINCC, DMAXC, MAXPC, MAXCC, SUMC, MEANC,
     :                          STDEVC, MEDIAN, MODE, 1, PERCNT, PERVAL,
     :                          IFIL, STATUS )
            END IF
         END IF
      END IF

*  Write the final results to the output parameters.
      CALL PAR_PUT0D( 'MAXIMUM', DMAXC, STATUS )
      CALL PAR_PUT0D( 'MEAN', MEANC, STATUS )
      CALL PAR_PUT0D( 'MINIMUM', DMINC, STATUS )
      CALL PAR_PUT0D( 'SIGMA', STDEVC, STATUS )
      CALL PAR_PUT0D( 'TOTAL', SUMC, STATUS )
      CALL PAR_PUT0I( 'NUMBAD', EL - NGOODC, STATUS )
      CALL PAR_PUT0I( 'NUMGOOD', NGOODC, STATUS )
      CALL PAR_PUT0I( 'NUMPIX', EL, STATUS )
      CALL PAR_PUT1D( 'MAXCOORD', NDIM, MAXCC, STATUS )
      CALL PAR_PUT1D( 'MINCOORD', NDIM, MINCC, STATUS )
      CALL PAR_PUT1I( 'MAXPOS', NDIM, MAXPC, STATUS )
      CALL PAR_PUT1I( 'MINPOS', NDIM, MINPC, STATUS )

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
