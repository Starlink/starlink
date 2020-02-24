      SUBROUTINE HISTAT( STATUS )
*+
*  Name:
*     HISTAT

*  Purpose:
*     Computes ordered statistics for an NDF's pixels using an
*     histogram.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL HISTAT( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application computes and displays simple ordered statistics
*     for the pixels in an NDF's data, quality, error, or variance
*     array.  The statistics available are:
*     -  the pixel sum,
*     -  the pixel mean,
*     -  the pixel median,
*     -  the pixel mode,
*     -  the pixel value at selected percentiles,
*     -  the value and position of the minimum- and maximum-valued
*     pixels,
*     -  the total number of pixels in the NDF,
*     -  the number of pixels used in the statistics, and
*     -  the number of pixels omitted.
*
*     The mode may be obtained in different ways (see Parameter METHOD).

*  Usage:
*     histat ndf [comp] [percentiles] [logfile]

*  ADAM Parameters:
*     COMP = LITERAL (Read)
*        The name of the NDF array component for which statistics are
*        required.  The options are limited to the arrays within the
*        supplied NDF.  In general the value may "Data", "Error",
*        "Quality" or "Variance" (note that "Error" is the alternative
*        to "Variance" and causes the square root of the variance
*        values to be taken before computing the statistics).  If
*        "Quality" is specified, then the quality values are treated as
*        numerical values (in the range 0 to 255).  ["Data"]
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
*     MAXPOS( ) = _INTEGER (Write)
*        A one-dimensional array of pixel indices identifying the
*        (first) maximum-valued pixel found in the NDF array.  The
*        number of indices is equal to the number of NDF dimensions.
*     MAXWCS = LITERAL (Write)
*        The formatted WCS co-ordinates at the maximum pixel value. The
*        individual axis values are comma separated.
*     MEAN = _DOUBLE (Write)
*        The mean value of all the valid pixels in the NDF array.
*     MEDIAN = _DOUBLE (Write)
*        The median value of all the valid pixels in the NDF array.
*     METHOD = LITERAL (Read)
*        The method used to evaluate the mode.  The choices are as
*        follows.
*
*        - "Histogram" -- This finds the peak of an optimally binned
*        histogram, the mode being the central value of that bin.  The
*        number of bins may be altered given through Parameter NUMBIN,
*        however it is recommended to use the optimal binsize derived
*        from the prescription of Freedman & Diatonis.
*
*        - "Moments" -- As "Histogram" but the mode is the weighted
*        centroid from the moments of the peak bin and its neighbours.
*        The neighbours are those bins either side of the peak in a
*        continuous sequence whose membership exceeds the peak value
*        less three times the Poisson error of the peak bin.  Thus it
*        gives an interpolated mode and does reduce the effect of
*        noise.
*
*        - "Pearson" -- This uses the 3 * median $-$ 2 * mean formula
*        devised by Pearson.  See the first two References.  This
*        assumes that the median is bracketed by the mode and mean and
*        only a mildly skew unimodal distribution.  This often applies
*        to an image of the sky.
*
*        ["Moments"]
*     MINCOORD( ) = _DOUBLE (Write)
*        A one-dimensional array of values giving the WCS co-ordinates
*        of the centre of the (first) minimum-valued pixel found in the
*        NDF array.  The number of co-ordinates is equal to the number
*        of NDF dimensions.
*     MINIMUM = _DOUBLE (Write)
*        The minimum pixel value found in the NDF array.
*     MINPOS( ) = _INTEGER (Write)
*        A one-dimensional array of pixel indices identifying the
*        (first) minimum-valued pixel found in the NDF array.  The
*        number of indices is equal to the number of NDF dimensions.
*     MINWCS = LITERAL (Write)
*        The formatted WCS co-ordinates at the minimum pixel value. The
*        individual axis values are comma separated.
*     MODE = _DOUBLE (Write)
*        The modal value of all the valid pixels in the NDF array.
*        The method used to obtain the mode is governed by Parameter
*        METHOD.
*     NDF = NDF (Read)
*        The NDF data structure to be analysed.
*     NUMBAD = _INTEGER (Write)
*        The number of pixels which were either not valid or were
*        rejected from the statistics during iterative K-sigma
*        clipping.
*     NUMBIN = _INTEGER (Read)
*        The number of histogram bins to be used for the coarse
*        histogram to evaluate the mode.  It is only accessed when
*        METHOD="Histogram" or "Moments".  This must lie in the range
*        10 to 10000.  The suggested default is calculated dynamically
*        depending on the data spread and number of values (using the
*        prescription of Freedman & Diaconis).  For integer data it is
*        advisble to use the dynamic default or an integer multiple
*        thereof to avoid creating non-integer wide bins.  []
*     NUMGOOD = _INTEGER (Write)
*        The number of NDF pixels which actually contributed to the
*        computed statistics.
*     NUMPIX = _INTEGER (Write)
*        The total number of pixels in the NDF (both good and bad).
*     PERCENTILES( 100 ) = _REAL (Read)
*         A list of percentiles to be found.  None are computed if this
*         parameter is null (!).  The percentiles must be in the range
*         0.0 to 100.0 [!]
*     PERVAL() = _DOUBLE (Write)
*         The values of the percentiles of the good pixels in the NDF
*         array.  This parameter is only written when one or more
*         percentiles have been requested.
*     TOTAL = _DOUBLE (Write)
*        The sum of the pixel values in the NDF array.

*  Examples:
*     histat image
*        Computes and displays simple ordered statistics for the data
*        array in the NDF called image.
*     histat image method=his
*        As above but the mode is the centre of peak bin in the
*        optimally distributed histogram rather than sub-bin
*        interpolated using neighbouring bins.
*     histat ndf=spectrum variance
*        Computes and displays simple ordered statistics for the
*        variance array in the NDF called spectrum.
*     histat spectrum error
*        Computes and displays ordered statistics for the variance
*        array in the NDF called spectrum, but takes the square root of
*        the variance values before doing so.
*     histat halley logfile=stats.dat method=pearson
*        Computes ordered statistics for the data array in the NDF
*        called halley, and writes the results to a logfile called
*        stats.dat.  The mode is derived using the Pearson formula.
*     histat ngc1333 percentiles=[0.25,0.75]
*        Computes ordered statistics for the data array in the NDF
*        called ngc1333, including the quartile values.

*  Notes:
*     -  Where the histogram contains a few extreme outliers, the
*     histogram limits are adjusted to reduce greatly the bias upon
*     the statistics, even if a chosen percentile corresponds to an
*     extreme outlier.  The outliers are still accounted in the median
*     and percentiles.  The histogram normally uses 10000 bins.  For
*     small arrays the number of bins is at most a half of the number
*     of array elements.  Integer arrays have a minimum bin width of
*     one; this can also reduce the number of bins.  The goal is to
*     avoid most histogram bins being empty artificially, since the
*     sparseness of the histogram is the main criterion for detecting
*     outliers.  Outliers can also be removed (flagged) via application
*     THRESH prior to using this application.
*     -  There is quantisation bias in the statistics, but for
*     non-pathological distributions this should be insignificant.
*     Accuracy to better than 0.01 of a percentile is normal.  Linear
*     interpolation within a bin is used, so the largest errors arise
*     near the median.

*  References:
*     Moroney, M.J., 1957, "Facts from Figures" (Pelican)
*     Goad, L.E. 1980, "Statistical Filtering of Cosmic-Ray Events
*       from Astronomical CCD Images in "Applications of Digital Image
*       Processing to Astronomy", SPIE 264, 136.
*     Freedman, D. & Diaconis, P. 1981, "On the histogram as a density
*        estimator: L2 theory", Zeitschrift fur
*        Wahrscheinlichkeitstheorie und verwandte Gebiete 57, 453.

*  Related Applications:
*     KAPPA: HISTOGRAM, MSTATS, NDFTRACE, NUMB, STATS; ESP: HISTPEAK;
*     Figaro: ISTAT.

*  Implementation Status:
*     -  This routine correctly processes the AXIS, WCS, DATA, VARIANCE,
*     QUALITY, TITLE, and HISTORY components of the NDF.
*     -  Processing of bad pixels and automatic quality masking are
*     supported.
*     -  All non-complex numeric data types can be handled.  Arithmetic
*     is performed using single- or double-precision floating point,
*     as appropriate.
*     -  Any number of NDF dimensions is supported.

*  Copyright:
*     Copyright (C) 1991, 1994 Science & Engineering Research Council.
*     Copyright (C) 2000, 2004 Central Laboratory of the Research
*     Councils.
*     Copyright (C) 2007, 2009, 2010, 2012 Science & Technology
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
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David S. Berry STARLINK
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     1991 November 13 (MJC):
*        Original NDF version.
*     1994 September 27 (MJC):
*        Replaced AIF calls with PAR, FIO, and PSX.  Made messages
*        conditional and used modern names for subroutines (_$ to 1_).
*     2000 June 13 (MJC):
*        Removed NUMBIN parameter.  No longer obtains dynamic space
*        for the histogram.  Uses improved algorithm for calculating
*        the median and percentiles.
*     6-AUG-2004 (DSB):
*        Display current Frame WCS coords at max and min pixel
*        positions.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL.
*     18-MAY-2007 (DSB):
*        Added parameters MINWCS and MAXWCS.
*     2007 June 29 (MJC):
*        Extend to calculate the mode from an optimally binned
*        histogram through new parameters METHOD and NUMBIN.  Added
*        References.
*     2009 June 25 (MJC):
*        Initialise PERVAL for valgrind.
*     15-APR-2010 (DSB):
*        Ensure that bad percentile values introduced by KPS1_HMSOx are
*        not included in the value written to output parameter PERVAL.
*     2010 August 4 (MJC):
*        Use extended APIs for KPG1_STDSx and KPG1_STFLx.
*     2012 May 8 (MJC):
*        Add _INT64 support.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_ERR'          ! PAR_ error codes
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT primitive data constants
      INCLUDE 'MSG_PAR'          ! Message constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
      INCLUDE 'AST_PAR'          ! AST functions and constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER NPRCTL             ! Maximum number of percentiles
      PARAMETER( NPRCTL = 100 )

      INTEGER SZBUF              ! Size of text buffer
      PARAMETER ( SZBUF = 200 )

*  Local Variables:
      LOGICAL BAD                ! There may be bad values in the array
      CHARACTER * ( SZBUF ) BUF  ! Text buffer
      CHARACTER*8 COMP           ! Name of array component to analyse
      CHARACTER*255 MAXWCS       ! Formatted max WCS position
      CHARACTER*255 MINWCS       ! Formatted max WCS position
      DOUBLE PRECISION DMAX      ! Max. value of pixels in array
      DOUBLE PRECISION DMIN      ! Min. value of pixels in array
      LOGICAL DOPRCT             ! Percentiles have been supplied
      INTEGER EL                 ! Number of array elements mapped
      INTEGER*8 EL8              ! Number of array elements mapped
      INTEGER I                  ! Loop counter for percentiles
      INTEGER IFIL               ! File descriptor for logfile
      INTEGER IMAX( 1 )          ! Vector index of max. pixel
      INTEGER IMIN( 1 )          ! Vector index of min. pixel
      INTEGER IWCS               ! Pointer to WCS FrameSet
      INTEGER J                  ! Counter for percentiles
      INTEGER LBND( NDF__MXDIM ) ! NDF lower bounds
      LOGICAL LOGFIL             ! Log file is required
      DOUBLE PRECISION MAXC( NDF__MXDIM ) ! Co-ordinates of max. pixel
      CHARACTER*8 MCOMP          ! Component name for mapping arrays
      INTEGER MAXP( NDF__MXDIM ) ! Indices of maximum-valued pixel
      INTEGER MAXPER             ! Max. no. of prcntles to get from user
      INTEGER MINP( NDF__MXDIM ) ! Indices of minimum-valued pixel
      DOUBLE PRECISION MEAN      ! Mean of pixels in array
      DOUBLE PRECISION MEDIAN    ! Median of pixels in array
      CHARACTER*9 METHOD         ! Method for determining the mode
      DOUBLE PRECISION MINC( NDF__MXDIM ) ! Co-ordinates of min. pixel
      DOUBLE PRECISION MODE      ! Mode of pixels in array
      INTEGER NC                 ! No. characters in text buffer
      INTEGER NDF                ! NDF identifier
      INTEGER NDIM               ! Number of NDF dimensions
      INTEGER NGOOD              ! No. valid pixels in array
      INTEGER*8 NGOOD8           ! No. valid pixels in array
      INTEGER NUMPER             ! Number of percentiles
      INTEGER NWCS               ! Number of WCS axes
      REAL PERCNT( NPRCTL )      ! Percentiles
      DOUBLE PRECISION PERVAL( NPRCTL ) ! Values at the percentiles
      INTEGER PNTR( 1 )          ! Pointer to mapped NDF array
      DOUBLE PRECISION STDEV     ! Standard devn. of pixels in array
      DOUBLE PRECISION SUM       ! Sum of pixels in array
      LOGICAL THERE              ! Array component exists
      CHARACTER * ( NDF__SZTYP ) TYPE ! Numeric type for processing
      INTEGER UBND( NDF__MXDIM ) ! NDF upper bounds
      LOGICAL USEHIS             ! Use histogram to find the mode

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain the NDF array component.
*  ===============================

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Initialise whether or not the logfile is required.
      LOGFIL = .FALSE.

*  Obtain the NDF to be analysed.
      CALL LPG_ASSOC( 'NDF', 'READ', NDF, STATUS )

*  Determine which array component is to be analysed, converting
*  'ERROR' into 'VARIANCE'.
      CALL PAR_CHOIC( 'COMP', 'Data', 'Data,Error,Quality,Variance',
     :                .FALSE., COMP, STATUS )
      MCOMP = COMP
      IF ( COMP .EQ. 'ERROR' ) COMP = 'VARIANCE'

*  Check that the required component exists and report an error,
*  including the NDF's name, if it does not.
      CALL NDF_STATE( NDF, COMP, THERE, STATUS )
      IF ( ( STATUS .EQ. SAI__OK ) .AND. ( .NOT. THERE ) ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'COMP', COMP )
         CALL NDF_MSG( 'NDF', NDF )
         CALL ERR_REP( 'HISTAT_NOCOMP',
     :     'The ^COMP component is undefined in the NDF structure ^NDF',
     :     STATUS )
      END IF

*  Exit if something has gone wrong.  Good status is required before
*  obtaining the percentiles.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Percentiles
*  ===========

*  Inquire the method used to calculate the mode.
      CALL PAR_CHOIC( 'METHOD', 'Moments', 'Histogram,Moments,Pearson',
     :                .TRUE., METHOD, STATUS )
      USEHIS = METHOD .EQ. 'HISTOGRAM' .OR. METHOD .EQ. 'MOMENTS'

*  If required, ensure we leave two slots spare in the PERCNT array.
      IF( USEHIS ) THEN
         MAXPER = NPRCTL - 2
      ELSE
         MAXPER = NPRCTL
      END IF

*  Defer error reporting and obtain an array of percentiles to be
*  calculated.  Constrain the values to be between the minimum and
*  maximum data values.
      CALL ERR_MARK
      CALL PAR_GDRVR( 'PERCENTILES', MAXPER, 0.0, 100.0, PERCNT, NUMPER,
     :                STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN

*  Null is a valid response to say do not compute percentiles.  Make
*  the number of percentiles one and flag the value, so that the
*  display routines can handle and recognise there are no percentile
*  values to report.  The exception is when we need an inter-quartile
*  range.
         DOPRCT = .FALSE.
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            IF ( USEHIS ) THEN
               NUMPER = 0
            ELSE
               NUMPER = 1
               PERCNT( 1 ) = VAL__BADR
            END IF
         END IF

      ELSE
         DOPRCT = .TRUE.
      END IF
      CALL ERR_RLSE

*  We need extra percentiles to derive the inter-quartile range for
*  the histogram method to derive the mode. We know there will be room
*  in PERCNT for these extra values, so no need to check.
      IF ( USEHIS ) THEN
         PERCNT( NUMPER + 1 ) = 25.0
         PERCNT( NUMPER + 2 ) = 75.0
         NUMPER = NUMPER + 2
         DOPRCT = .TRUE.
      END IF

*  Obtain the statistics.
*  ======================

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

      DO I = 1, NPRCTL
         PERVAL(I) = VAL__BADD
      END DO

*  Call the appropriate routine to compute the histogram and hence the
*  statistics.
      IF ( TYPE .EQ. '_BYTE' ) THEN
         CALL KPG1_HSTAB( BAD, EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                    NUMPER, PERCNT,
     :                    NGOOD, IMIN( 1 ), DMIN, IMAX( 1 ), DMAX,
     :                    SUM, MEAN, MEDIAN, MODE, PERVAL, STATUS )

      ELSE IF ( TYPE .EQ. '_UBYTE' ) THEN
         CALL KPG1_HSTAUB( BAD, EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                     NUMPER, PERCNT,
     :                     NGOOD, IMIN( 1 ), DMIN, IMAX( 1 ), DMAX,
     :                     SUM, MEAN, MEDIAN, MODE, PERVAL, STATUS )

      ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
         CALL KPG1_HSTAD( BAD, EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                    NUMPER, PERCNT,
     :                    NGOOD, IMIN( 1 ), DMIN, IMAX( 1 ), DMAX,
     :                    SUM, MEAN, MEDIAN, MODE, PERVAL, STATUS )

      ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
         CALL KPG1_HSTAI( BAD, EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                    NUMPER, PERCNT,
     :                    NGOOD, IMIN( 1 ), DMIN, IMAX( 1 ), DMAX,
     :                    SUM, MEAN, MEDIAN, MODE, PERVAL, STATUS )

      ELSE IF ( TYPE .EQ. '_INT64' ) THEN
         CALL KPG1_HSTAK( BAD, EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                    NUMPER, PERCNT,
     :                    NGOOD, IMIN( 1 ), DMIN, IMAX( 1 ), DMAX,
     :                    SUM, MEAN, MEDIAN, MODE, PERVAL, STATUS )

      ELSE IF ( TYPE .EQ. '_REAL' ) THEN
         CALL KPG1_HSTAR( BAD, EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                    NUMPER, PERCNT,
     :                    NGOOD, IMIN( 1 ), DMIN, IMAX( 1 ), DMAX,
     :                    SUM, MEAN, MEDIAN, MODE, PERVAL, STATUS )

      ELSE IF ( TYPE .EQ. '_WORD' ) THEN
         CALL KPG1_HSTAW( BAD, EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                    NUMPER, PERCNT,
     :                    NGOOD, IMIN( 1 ), DMIN, IMAX( 1 ), DMAX,
     :                    SUM, MEAN, MEDIAN, MODE, PERVAL, STATUS )

      ELSE IF ( TYPE .EQ. '_UWORD' ) THEN
         CALL KPG1_HSTAUW( BAD, EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                     NUMPER, PERCNT,
     :                     NGOOD, IMIN( 1 ), DMIN, IMAX( 1 ), DMAX,
     :                     SUM, MEAN, MEDIAN, MODE, PERVAL, STATUS )
      END IF

*  Determine the mode using an histogram-peak approach.
      IF ( USEHIS ) THEN

*  Call the appropriate routine to compute the histogram and hence the
*  derive the mode.  The optimum bin width is derived from which the
*  optimum number of bins is derived but the user is allowed to modify
*  that through Parameter NUMBIN.
         NGOOD8 = NGOOD
         EL8 = EL
         IF ( TYPE .EQ. '_BYTE' ) THEN
             CALL KPS1_HSMOB( BAD, EL8, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                        'NUMBIN', METHOD, DMAX, DMIN, NGOOD8,
     :                        NUMPER, PERCNT, PERVAL, MODE, STATUS )

         ELSE IF ( TYPE .EQ. '_UBYTE' ) THEN
            CALL KPS1_HSMOUB( BAD, EL8, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                        'NUMBIN', METHOD, DMAX, DMIN, NGOOD8,
     :                        NUMPER, PERCNT, PERVAL, MODE, STATUS )

         ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
            CALL KPS1_HSMOD( BAD, EL8, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                       'NUMBIN', METHOD, DMAX, DMIN, NGOOD8,
     :                       NUMPER, PERCNT, PERVAL, MODE, STATUS )

         ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
            CALL KPS1_HSMOI( BAD, EL8, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                       'NUMBIN', METHOD, DMAX, DMIN, NGOOD8,
     :                       NUMPER, PERCNT, PERVAL, MODE, STATUS )

         ELSE IF ( TYPE .EQ. '_INT64' ) THEN
            CALL KPS1_HSMOK( BAD, EL8, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                       'NUMBIN', METHOD, DMAX, DMIN, NGOOD8,
     :                       NUMPER, PERCNT, PERVAL, MODE, STATUS )

         ELSE IF ( TYPE .EQ. '_REAL' ) THEN
            CALL KPS1_HSMOR( BAD, EL8, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                       'NUMBIN', METHOD, DMAX, DMIN, NGOOD8,
     :                       NUMPER, PERCNT, PERVAL, MODE, STATUS )

         ELSE IF ( TYPE .EQ. '_WORD' ) THEN
             CALL KPS1_HSMOW( BAD, EL8, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                        'NUMBIN', METHOD, DMAX, DMIN, NGOOD8,
     :                        NUMPER, PERCNT, PERVAL, MODE, STATUS )

         ELSE IF ( TYPE .EQ. '_UWORD' ) THEN
            CALL KPS1_HSMOUW( BAD, EL8, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                        'NUMBIN', METHOD, DMAX, DMIN, NGOOD8,
     :                        NUMPER, PERCNT, PERVAL, MODE, STATUS )

         END IF

*  Shuffle the percentile values down to remove the bad values
*  introduced by KPS1_HSMOx.
         J = 1
         DO I = 1, NUMPER
            IF( PERVAL( I ) .NE. VAL__BADD ) THEN
               PERVAL( J ) = PERVAL ( I )
               PERCNT( J ) = PERCNT ( I )
               J = J + 1
            END IF
         END DO
         NUMPER = J - 1

      END IF

*  Logging
*  =======

*  Obtain an optional file for logging the results.  Start a new error
*  context as null is a valid value.
      CALL ERR_MARK
      CALL FIO_ASSOC( 'LOGFILE', 'WRITE', 'LIST', SZBUF, IFIL, STATUS )

*  Null is a valid response to say do create a logfile.  Record this
*  fact.  Close the new error context.
      IF ( STATUS .NE. SAI__OK ) THEN

         LOGFIL = .FALSE.
         IF ( STATUS .EQ. PAR__NULL ) CALL ERR_ANNUL( STATUS )
      ELSE
         LOGFIL = .TRUE.
      END IF
      CALL ERR_RLSE

*  Display the NDF name, also sending it to the logfile if necessary.
*  Cannot use MSG_BLANK because the message is optional.
      CALL MSG_BLANKIF( MSG__NORM, STATUS )
      IF ( LOGFIL ) CALL FIO_WRITE( IFIL, ' ', STATUS )
      CALL NDF_MSG( 'NDF', NDF )
      CALL MSG_LOAD( 'NDFNAME',
     :               '   Pixel ordered statistics for the NDF '/
     :               /'structure ^NDF', BUF, NC, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL MSG_SETC( 'MESSAGE', BUF( : NC ) )
         CALL MSG_OUTIF( MSG__NORM, ' ', '^MESSAGE', STATUS )
         IF ( LOGFIL ) CALL FIO_WRITE( IFIL, BUF( : NC ), STATUS )
      END IF

*  Display (and log) the NDF's title.  Cannot use MSG_BLANK because
*  the message is optional.
      CALL MSG_BLANKIF( MSG__NORM, STATUS )
      IF ( LOGFIL ) CALL FIO_WRITE( IFIL, ' ', STATUS )
      CALL NDF_CMSG( 'TITLE', NDF, 'Title', STATUS )
      CALL MSG_LOAD( 'NDFTITLE',
     :               '      Title                     : ^TITLE',
     :               BUF, NC, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL MSG_SETC( 'MESSAGE', BUF( : NC ) )
         CALL MSG_OUTIF( MSG__NORM, ' ', '^MESSAGE', STATUS )
         IF ( LOGFIL ) CALL FIO_WRITE( IFIL, BUF( : NC ), STATUS )
      END IF

*  Display (and log) the name of the component being analysed.
      CALL MSG_SETC( 'COMP', MCOMP )
      CALL MSG_LOAD( 'NDFCOMP',
     :               '      NDF array analysed        : ^COMP',
     :               BUF, NC, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL MSG_SETC( 'MESSAGE', BUF( : NC ) )
         CALL MSG_OUTIF( MSG__NORM, ' ', '^MESSAGE', STATUS )
         IF ( LOGFIL ) CALL FIO_WRITE( IFIL, BUF( : NC ), STATUS )
      END IF

*  If a logfile is in use, display its name.
      IF ( LOGFIL ) CALL MSG_OUTIF( MSG__NORM, 'LOG',
     :              '      Logging to file           : $LOGFILE',
     :                            STATUS )


*  Report the statistics.
*  ======================

*  Obtain the NDF bounds and initialise the indices of the minimum and
*  maximim pixel positions and co-ordinates.
      CALL NDF_BOUND( NDF, NDF__MXDIM, LBND, UBND, NDIM, STATUS )
      DO 1 I = 1, NDIM
         MINP( I ) = VAL__BADI
         MAXP( I ) = VAL__BADI
         MINC( I ) = VAL__BADD
         MAXC( I ) = VAL__BADD
    1 CONTINUE

*  If available, convert the minimum and maximum pixel locations into
*  N-dimensional indices and then into co-ordinate values.
      IF ( NGOOD .NE. 0 ) THEN
         CALL KPG1_VEC2N( 1, IMIN, NDIM, LBND, UBND, MINP, STATUS )
         CALL KPG1_VEC2N( 1, IMAX, NDIM, LBND, UBND, MAXP, STATUS )
         CALL KPG1_PX2AX( NDIM, MINP, NDF, MINC, STATUS )
         CALL KPG1_PX2AX( NDIM, MAXP, NDF, MAXC, STATUS )
      END IF

*  The standard deviation is undefined, but needs to be passed to the
*  display routines, so give it the flagged value.
      STDEV = VAL__BADD

*  Get the WCS FrameSet.
      CALL KPG1_GTWCS( NDF, IWCS, STATUS )

*  Get the number of WCS axes.
      NWCS = AST_GETI( IWCS, 'Nout', STATUS )

*  Display the statistics, using the most appropriate floating-point
*  precision.
      IF ( TYPE .EQ. '_DOUBLE' .OR. TYPE .EQ. '_INT64' ) THEN
         CALL KPG1_STDSD( IWCS, NDIM, EL, NGOOD, DMIN, MINP, MINC,
     :                    DMAX, MAXP, MAXC, SUM, MEAN, STDEV,
     :                    VAL__BADD, VAL__BADD, MEDIAN, MODE, NUMPER,
     :                    PERCNT, PERVAL, MAXWCS, MINWCS, STATUS )
      ELSE
         CALL KPG1_STDSR( IWCS, NDIM, EL, NGOOD, DMIN, MINP, MINC,
     :                    DMAX, MAXP, MAXC, SUM, MEAN, STDEV,
     :                    VAL__BADD, VAL__BADD, MEDIAN, MODE, NUMPER,
     :                    PERCNT, PERVAL, MAXWCS, MINWCS, STATUS )
      END IF

*  Also write the statistics to the logfile, if used.
      IF ( LOGFIL ) THEN
         IF ( TYPE .EQ. '_DOUBLE' .OR. TYPE .EQ. '_INT64' ) THEN
            CALL KPG1_STFLD( IWCS, NDIM, EL, NGOOD, DMIN, MINP, MINC,
     :                       DMAX, MAXP, MAXC, SUM, MEAN, STDEV,
     :                       VAL__BADD, VAL__BADD, MEDIAN, MODE,
     :                       NUMPER, PERCNT, PERVAL, IFIL, STATUS )
         ELSE
            CALL KPG1_STFLR( IWCS, NDIM, EL, NGOOD, DMIN, MINP, MINC,
     :                       DMAX, MAXP, MAXC, SUM, MEAN, STDEV,
     :                       VAL__BADD, VAL__BADD, MEDIAN, MODE,
     :                       NUMPER, PERCNT, PERVAL, IFIL, STATUS )
         END IF
      END IF

*  Annul the WCS FrameSet.
      CALL AST_ANNUL( IWCS, STATUS )

*  Write the final results to the output parameters.
      CALL PAR_PUT0D( 'MAXIMUM', DMAX, STATUS )
      CALL PAR_PUT0D( 'MEAN', MEAN, STATUS )
      CALL PAR_PUT0D( 'MEDIAN', MEDIAN, STATUS )
      CALL PAR_PUT0D( 'MODE', MODE, STATUS )
      CALL PAR_PUT0D( 'MINIMUM', DMIN, STATUS )
      CALL PAR_PUT0D( 'TOTAL', SUM, STATUS )
      CALL PAR_PUT0I( 'NUMBAD', EL - NGOOD, STATUS )
      CALL PAR_PUT0I( 'NUMGOOD', NGOOD, STATUS )
      CALL PAR_PUT0I( 'NUMPIX', EL, STATUS )
      CALL PAR_PUT1D( 'MAXCOORD', NDIM, MAXC, STATUS )
      CALL PAR_PUT1D( 'MINCOORD', NDIM, MINC, STATUS )
      CALL PAR_PUT1I( 'MAXPOS', NDIM, MAXP, STATUS )
      CALL PAR_PUT1I( 'MINPOS', NDIM, MINP, STATUS )
      CALL PAR_PUT0C( 'MAXWCS', MAXWCS, STATUS )
      CALL PAR_PUT0C( 'MINWCS', MINWCS, STATUS )

*  Only write percentiles values if any percentiles are left.
      IF ( NUMPER .GT. 0 ) CALL PAR_PUT1D( 'PERVAL', NUMPER, PERVAL,
     :                                    STATUS )

*  Arrive here if an error occurs.
  999 CONTINUE

*  End the NDF context.
      CALL NDF_END( STATUS )

*  Close the logfile, if used.
      IF ( LOGFIL ) CALL FIO_CLOSE( IFIL, STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'HISTAT_ERR',
     :     'HISTAT: Error computing simple ordered statistics for an '/
     :     /'NDF''s pixels.', STATUS )
      END IF

      END
