      SUBROUTINE KSTEST( STATUS )
*+
*  Name:
*     KSTEST

*  Purpose:
*     Compares data sets using the Kolmogorov-Smirnov test.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL KSTEST( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status

*  Description:
*     This routine reads in a data array and performs a two sided
*     Kolmogorov-Smirnov test on the vectorised data.  It does this in
*     two ways:
*
*       1) If only one dataset is to be tested the data array is
*          divided into subsamples.  First it compares subsample 1 with
*          subsample 2, if they are thought to be from the same sample
*          they are concatenated.  This enlarged sample is then
*          compared with subsample 3 etc., concatenating if consistent,
*          until no more subsamples remain.
*
*       2) If more than one dataset is specified, the datasets are
*          compared to the reference dataset in turn.  If the
*          probability the two are from the same sample is greater than
*          the specified confidence level, the datasets are
*          concatenated, and the next sample is tested against this
*          enlarged reference dataset.
*
*     The probability and maximum separation of the cumulative
*     distribution function is displayed for each comparison (at the
*     normal reporting level).  The mean value of the consistent data
*     and its error are also reported.  In all cases the consistent
*     data can be output to a new dataset.  The statistics and
*     probabilities are written to results parameters.

*  Usage:
*     kstest in out [limit]

*  ADAM Parameters:
*     COMP = LITERAL (Read)
*        The name of the NDF array component to be tested for
*        consistency: "Data", "Error", "Quality" or "Variance" (where
*        "Error" is the alternative to "Variance" and causes the square
*        root of the variance values to be taken before performing the
*        comparisons).  If "Quality" is specified, then the quality
*        values are treated as numerical values (in the range 0 to
*        255).  ["Data"]
*     DIST() = _REAL (Write)
*        Maximum separation found in the cumulative distributions for
*        each comparison subsample.  Note that it excludes the
*        reference dataset.
*     ERRMEAN = _DOUBLE (Write)
*        Error in the mean value of the consistent data.
*     FILES() = LITERAL (Write)
*        The names of the datasets intercompared.  The first is the
*        reference dataset.
*     LIMIT = _REAL (Read)
*        Confidence level at which samples are thought to be
*        consistent.  This must lie in the range 0 to 1. [0.05]
*     IN = LITERAL (Read)
*        The names of the NDFs to be tested.  If just one dataset is
*        supplied, it is divided into subsamples, which are compared
*        (see parameter NSAMPLE).  When more than one dataset is
*        provided, the first becomes the reference dataset to which all
*        the remainder are compared.
*
*        It may be a list of NDF names or direction specifications
*        separated by commas.  If a list is supplied on the command
*        line, the list must be enclosed in double quotes.  NDF names
*        may include the regular expressions ("*", "?", "[a-z]" etc.).
*        Indirection may occur through text files (nested up to seven
*        deep).  The indirection character is "^".  If extra prompt
*        lines are required, append the continuation character "-" to
*        the end of the line.  Comments in the indirection file begin
*        with the character "#".
*     MEAN = _DOUBLE (Write)
*        Mean value of the consistent data.
*     NKEPT = _INTEGER (Write)
*        Number of consistent data.
*     NSAMPLE = _INTEGER (Read)
*        The number of the subsamples into which to divide the reference
*        dataset.  This parameter is only requested when a single NDF
*        is to be analysed, i.e. when only one dataset name is supplied
*        via parameter IN.  The allowed range is 2 to 20.  [3]
*     OUT = NDF (Write)
*        Output 1-dimensional NDF to which the consistent data is
*        written.  A null value (!)---the suggested default---prevents
*        creation of this output dataset.
*     PROB() = _REAL (Write)
*        Probability that each comparison subsample is drawn from the
*        same sample.  Note that this excludes the reference sample.
*     SIGMA = _DOUBLE (Write)
*        Standard deviation of the consistent data.

*  Examples:
*     kstest arlac accept
*        This tests the NDF called arlac for self-consistency at the 95%
*        confidence level using three subsamples.  No output dataset is
*        created.
*
*        The following applies to all the examples.  If the reference
*        dataset and a comparison subsample are consistent, the two
*        merge to form an expanded reference dataset, which is then
*        used for the next comparison.  Details of the comparisons are
*        presented.
*     kstest arlac arlac_filt 0.10 nsample=10
*        As above except data are retained if they exceed the 90%
*        probability level, the comparisons are made with ten
*        subsamples, and the consistent data are written to the
*        one-dimensional NDF called arlac_filt.
*     kstest in="ref,obs*" comp=v out=master
*        This compares the variance in the NDF called ref with that in
*        a series of other NDFs whose names begin "obs".  The variance
*        consistent with the reference dataset are written to the data
*        array in the NDF called master.  To be consistent, they must be
*        the same at 95% probability.
*     kstest "ref,^96lc.lis,obs*" master comp=v
*        As the previous example, except the comparison files include
*        those listed in the text file 96lc.lis.

*  Notes:
*     - The COMP array MUST exist in each NDF to be compared.  The
*     COMP array becomes the data array in the output dataset.  When
*     COMP="Data", the variance values corresponding to consistent data
*     are propagated to the output dataset.
*     - Pixel bounds are ignored for the comparisons.
*     - The internal comparison of a single dataset follows the method
*     outlined in Hughes D., 1993, JCMT-UKIRT Newsletter, #4, p32.
*     - The maximum number of files is 20.

*  Algorithm:
*     - Read in one or more datasets
*     - If one NDF split into subsamples and compare internally
*     - If more than one NDF compare directly and return statistic
*     - Write output file of consistent data

*  Implementation Status:
*     - This routine correctly processes DATA, VARIANCE, HISTORY, LABEL,
*     TITLE, and UNITS components, and propagates all extensions.  AXIS
*     information is lost.  Propagation is from the reference dataset.
*     - Processing of bad pixels and automatic quality masking are
*     supported.
*     - All numeric data types are supported, however, processing uses
*     the _REAL data type, and the output dataset has this type.

*  Copyright:
*     Copyright (C) 1996-1998, 2004 Central Laboratory of the Research
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
*     TIMJ: T. Jenness (JACH)
*     MJC: Malcolm J. Currie (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     1996 September (TIMJ):
*        Original version.
*     1996 September 19 (MJC):
*        Completed the prologue and modified for consistency within
*        KAPPA.
*     1997 May 12 (MJC):
*        Redesigned to use GRP and NDF chunks, thus removing the REF
*        parameter and renaming NDF to IN.  Better propagation of NDF
*        components.  Improved the table formatting.  Made efficiency
*        improvements.  Replaced SUBSAMPLE parameter with NSAMPLE.
*     7-OCT-1998 (DSB):
*        Changed (irg based) KPG1_NWILD call to (ndg based) KPG1_RGNDF.
*        Also changed to display the NDF name if the NDF Title is blank.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! SSE global definitions
      INCLUDE 'MSG_PAR'          ! MSG__ constants
      INCLUDE 'NDF_PAR'          ! NDF__ constants
      INCLUDE 'DAT_PAR'          ! DAT__ constants
      INCLUDE 'GRP_PAR'          ! GRP__ constants
      INCLUDE 'PAR_ERR'          ! PAR__ constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER          STATUS    ! Global status

*  Local Constants:
      INTEGER          MXFILE    ! Maximum number of files/subsamples to
      PARAMETER ( MXFILE = 20 )  ! compare

*  Local Variables:
      REAL CLIP( 1 )             ! Array of clipping limits
      CHARACTER * ( 28 ) COMLIS  ! List of available array components
      INTEGER COMLN              ! Length of component list
      CHARACTER * ( 8 ) COMP     ! Name of array component to analyse
      REAL D( MXFILE )           ! Maximum separation of cumulative
                                 ! distributions
      DOUBLE PRECISION DMAX      ! Maximum value of pixels in array
      DOUBLE PRECISION DMAXC     ! Maximum pixel value after clipping
      DOUBLE PRECISION DMIN      ! Minimum value of pixels in array
      DOUBLE PRECISION DMINC     ! Minimum pixel value after clipping
      INTEGER EL                 ! Number of data points
      CHARACTER * ( 256 ) FNAMES( MXFILE ) ! Array of filenames read in
      INTEGER GDPTR( MXFILE )    ! Pointer to good data
      INTEGER GVPTR( MXFILE )    ! Pointer to good variance
      INTEGER I                  ! Loop counter
      INTEGER IERR               ! Index of first conversion error
                                 ! (unused)
      INTEGER IGRP               ! GRP identifier of list of files
      INTEGER IMAX( 1 )          ! Vector index of maximum pixel
      INTEGER IMAXC( 1 )         ! Vector index of max clipped pixel
      INTEGER IMIN( 1 )          ! Vector index of minimum pixel
      INTEGER IMINC( 1 )         ! Vector index of min clipped pixel
      CHARACTER * ( 256 ) INFILE ! Name of an input filename
      INTEGER INPTR( 2 )         ! Pointer to mapped input data/variance
      LOGICAL ISVAR              ! Variance is in NDF?
      INTEGER ITEMP              ! Scratch integer
      INTEGER KSDPTR             ! Pointer to KS output array
      INTEGER KSVPTR             ! Pointer to KS output variance
      INTEGER LBND( 1 )          ! Lower bound of array
      REAL LIMIT                 ! Confidence limit
      CHARACTER * ( 8 ) MCOMP    ! Component name for mapping arrays
      DOUBLE PRECISION MEAN      ! Mean of pixels in array
      DOUBLE PRECISION MEANC     ! Mean of pixels after clipping
      INTEGER NCLIP              ! Number of clipping iterations
      INTEGER NDFI               ! Input NDF identifier
      INTEGER NDFO               ! Output NDF identifier
      INTEGER NDFR               ! Reference-dataset NDF identifier
      INTEGER NERR               ! Number of conversion errors (unused)
      INTEGER NFAIL              ! Number of failed subsamples
      INTEGER NFILES             ! Number of files read in
      INTEGER NGOOD( MXFILE )    ! Number of good data points
      INTEGER NKEPT              ! Number of points which pass KS
      INTEGER NPTS               ! Number of points in subsample
      INTEGER NTOTAL             ! Total number of good points
      INTEGER OUTPTR( 2 )        ! Output data and variance pointers
      REAL PROB( MXFILE )        ! Prob. that two samples identical
      LOGICAL SINGLE             ! Only one dataset provided?
      DOUBLE PRECISION STDEV     ! Standard devn. of pixels in array
      DOUBLE PRECISION STDEVC    ! Std. devn. of pixels after clipping
      DOUBLE PRECISION SUM       ! Sum of pixels in array
      DOUBLE PRECISION SUMC      ! Sum of pixels after clipping
      INTEGER SUBSAM             ! Size of subsample
      CHARACTER * ( 40 ) TITLE   ! Title of dataset
      INTEGER UBND( 1 )          ! Upper bound of array
      LOGICAL USEVAR             ! Processing variance arrays?
      REAL VAR                   ! Coadded variance
      INTEGER WNPTR              ! Pointer to scratch array
      INTEGER WPTR               ! Pointer to scratch array

*.

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the confidence limit.
      CALL PAR_GDR0R( 'LIMIT', 0.05, 0.0, 1.0, .FALSE., LIMIT, STATUS )

*  Start up the NDF system and read in the input data.
      CALL NDF_BEGIN

*  Obtain a list of NDFs of recognised foreign data files.
      CALL KPG1_RGNDF( 'IN', 0, 0, ' ', IGRP, NFILES, STATUS )

*  Abort if no files were found.  Make a helpful contextual error
*  report.
      IF ( IGRP .EQ. GRP__NOID .OR. NFILES .EQ. 0 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'KSTEST_ERR1',
     :     'No files could be found.  If you are using foreign-format '/
     :     /'files ensure that automatic conversion is enabled.',
     :     STATUS )
         GOTO 999
      END IF

*  When there is but one dataset, the comparisons are made on subsamples
*  within this dataset.  So find the number of subsamples subject to
*  the constraints that there must be at least two and no more than the
*  maximum number of `files'.
      SINGLE = NFILES .EQ. 1
      IF ( SINGLE ) THEN

*  Get a file specification from the group.
         CALL GRP_GET( IGRP, 1, 1, INFILE, STATUS )

*  Open the named file.
         CALL NDF_FIND( DAT__ROOT, INFILE, NDFR, STATUS )

*  Store the name.
         IF ( STATUS .EQ. SAI__OK ) FNAMES( 1 ) = INFILE

*  Inquire the dataset size in pixels
         CALL NDF_SIZE( NDFR, EL, STATUS )

*  Obtain the number of samples.
         CALL PAR_GDR0I( 'NSAMPLE', 3, 2, MXFILE, .FALSE., NFILES,
     :                   STATUS )

*  Find the maximum size of the subsamples.
         SUBSAM = ( EL - 1 ) / NFILES + 1
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 999

      USEVAR = .FALSE.

      DO I = 1, NFILES

         IF ( SINGLE ) THEN

*  Obtain a chunk from the reference dataset, which has already been
*  opened.
            CALL NDF_CHUNK( NDFR, SUBSAM, I, NDFI, STATUS )
         ELSE

*  Get a file specification from the group.
            CALL GRP_GET( IGRP, I, 1, INFILE, STATUS )

*  Open the named file.
            CALL NDF_FIND( DAT__ROOT, INFILE, NDFI, STATUS )

*  Store the name.
            IF ( STATUS .EQ. SAI__OK ) FNAMES( I ) = INFILE

*  Retain the first NDF from which to produce the output.
            IF ( I .EQ. 1 ) CALL NDF_CLONE( NDFI, NDFR, STATUS )
         END IF

         IF ( STATUS .EQ. SAI__OK ) THEN

*  Read in the title from the dataset.
            CALL NDF_CGET( NDFI, 'Title', TITLE, STATUS )

*  Find which array component to use.
*  ==================================
*
*  Inquire which arrays are available and form a comma-separated list
*  of them.  The required component must be present in all the datasets.
            IF ( I .EQ. 1 ) THEN
               CALL KPG1_ARCOL( NDFI, 'Data,Quality,Error,Variance',
     :                          COMLIS, COMLN, STATUS )

*  Find which component to plot.  No need to inquire the value, if the
*  only array component is Data.  Note the mixed-case returned in the
*  list is for attractive error reports.  See below why there is a
*  MCOMP.
               IF ( COMLIS .EQ. 'Data' ) THEN
                  COMP = 'DATA'
                  MCOMP = COMP
               ELSE
                  CALL PAR_CHOIC( 'COMP', 'Data', COMLIS( :COMLN ),
     :                            .FALSE., COMP, STATUS )

*  Most NDF routines with a component argument don't recognise 'ERROR',
*  so we need two variables.  Thus convert 'ERROR' into 'VARIANCE' in
*  the variable needed for such routines.  The original value is held
*  in a variable with the prefix M for mapping, as one of the few
*  routines that does support 'ERROR' is NDF_MAP.
                  MCOMP = COMP
                  IF ( COMP .EQ. 'ERROR' ) COMP = 'VARIANCE'

               END IF
            END IF

*  Does the variance exist?
            CALL NDF_STATE( NDFI, 'VARIANCE', ISVAR, STATUS )

*  If the chosen component is the data array then we also want to
*  propagate the VARIANCE array (for the output dataset), if it exists.
*  If we have mapped a different array (QUALITY or VARIANCE) we don't
*  need to propagate anything else.

*  If we map any variance arrays then we shall propagate variance
*  using USEVAR = .TRUE..
            IF ( MCOMP .EQ. 'DATA' .AND. ISVAR ) THEN

*  Map the data and variance arrays (as REAL arrays regardless of
*  actual type).
               CALL KPG1_MAP( NDFI, 'Data,Variance', '_REAL', 'READ',
     :                       INPTR, EL, STATUS )
               USEVAR = .TRUE.

*  Otherwise just set the VARIANCE array to 0 (if a further NDF might
*  have a variance array).  Note that need to supply a zero-filled
*  array for KPG1_KGODR.
            ELSE

*  Map the data array (as a REAL array regardless of actual type).
               CALL KPG1_MAP( NDFI, MCOMP, '_REAL', 'READ',
     :                       INPTR, EL, STATUS )
            END IF

*  Get some memory to remove bad DATA points.
            CALL PSX_MALLOC( EL * VAL__NBR, GDPTR( I ), STATUS )
            IF ( USEVAR )
     :        CALL PSX_MALLOC( EL * VAL__NBR, GVPTR( I ), STATUS )

*  Need to go through the chosen array and possibly the variance array
*  to extract good data.
            CALL KPG1_KGODR( ISVAR, EL, %VAL( CNF_PVAL( INPTR( 1 ) ) ),
     :                       %VAL( CNF_PVAL( INPTR( 2 ) ) ), NGOOD( I ),
     :                       %VAL( CNF_PVAL( GDPTR( I ) ) ),
     :                       %VAL( CNF_PVAL( GVPTR( I ) ) ),
     :                       STATUS )

*  Write out some information.
            IF( TITLE .NE. ' ' ) THEN
               CALL MSG_SETC( 'TITLE', TITLE )
            ELSE
               CALL NDF_MSG( 'TITLE', NDFI )
            END IF

            CALL MSG_SETI( 'NGOOD', NGOOD( I ) )
            CALL MSG_SETI( 'NEL', EL )
            CALL MSG_OUTIF( MSG__NORM, 'RES1',
     :        'Data for "^TITLE" contain ^NEL points, of which ^NGOOD '/
     :        /'are good.', STATUS )

*  Now unmap the input data.
            CALL NDF_UNMAP( NDFI, '*', STATUS )
            CALL NDF_ANNUL( NDFI, STATUS )

         END IF
      END DO

*  Find the total number of points.
      NTOTAL = 0
      DO I = 1, NFILES
         NTOTAL = NTOTAL + NGOOD( I )
      END DO

*  Need to store the output somewhere.
      CALL PSX_MALLOC( NTOTAL * VAL__NBR, KSDPTR, STATUS )
      IF ( USEVAR ) CALL PSX_MALLOC( NTOTAL * VAL__NBR, KSVPTR, STATUS )

*  Store the reference sample
      NKEPT = NGOOD( 1 )
      CALL VEC_RTOR( .FALSE., NKEPT, %VAL( CNF_PVAL( GDPTR( 1 ) ) ),
     :               %VAL( CNF_PVAL( KSDPTR ) ), IERR, NERR, STATUS )

      IF ( USEVAR ) THEN
         CALL VEC_RTOR( .FALSE., NKEPT, %VAL( CNF_PVAL( GVPTR( 1 ) ) ),
     :                  %VAL( CNF_PVAL( KSVPTR ) ), IERR, NERR, STATUS )
      END IF

* Set up the scratch output dummy arrays (for sorted data).
      CALL PSX_MALLOC( NTOTAL * VAL__NBR, WPTR, STATUS )

*  Loop around.
      NFAIL = 0
      DO I = 2, NFILES

         NPTS = NGOOD( I )

*  Set up the scratch (sorted) output array.
         CALL PSX_MALLOC( NPTS * VAL__NBR, WNPTR, STATUS )

*  Now compare the two samples.
         CALL KPS1_KS2TR( NKEPT, NPTS, %VAL( CNF_PVAL( KSDPTR ) ),
     :                    %VAL( CNF_PVAL( GDPTR( I ) ) ),
     :                    D( I - 1 ), PROB( I - 1 ),
     :                    %VAL( CNF_PVAL( WPTR ) ),
     :                    %VAL( CNF_PVAL( WNPTR ) ), STATUS )

*  If they are from the same sample then copy them.
         IF ( PROB( I - 1 ) .GT. LIMIT ) THEN
            CALL VEC_RTOR( .FALSE., NPTS,
     :                     %VAL( CNF_PVAL( GDPTR( I ) ) ),
     :                    %VAL( CNF_PVAL( KSDPTR + NKEPT * VAL__NBR ) ),
     :                     IERR, NERR, STATUS )

            IF ( USEVAR ) THEN
               CALL VEC_RTOR( .FALSE., NPTS,
     :                        %VAL( CNF_PVAL( GVPTR( I ) ) ),
     :                    %VAL( CNF_PVAL( KSVPTR + NKEPT * VAL__NBR ) ),
     :                        IERR, NERR, STATUS )
            END IF

*  Increase the reference-sample size.
            NKEPT = NKEPT + NPTS

         ELSE
            NFAIL = NFAIL + 1

         END IF

*  Display the result of the test unless the MSG level is set
*  MSG__QUIET.
         IF ( I .EQ. 2 ) THEN
            CALL MSG_BLANKIF( MSG__NORM, STATUS )
            CALL MSG_OUTIF( MSG__NORM, 'HEADING',
     :        '                  Probability        Max. Sep.', STATUS )
            CALL MSG_OUTIF( MSG__NORM, 'LINE',
     :        '------------------------------------------------------'/
     :        /'--------', STATUS )
         END IF

*  Keep or reject (this must be done here since the MSG_OUTIF above
*  annuls the KEEP message token if it is put in the previous IF
*  (PROB...) THEN.
         IF ( PROB( I - 1 ) .GT. LIMIT ) THEN
            CALL MSG_SETC( 'KEEP', 'Accepted' )
         ELSE
            CALL MSG_SETC( 'KEEP', 'Rejected' )
         END IF

*  General information on each comparison.
         CALL MSG_FMTR( 'PROB', 'E13.6', PROB( I - 1 ) )
         CALL MSG_FMTR( 'DIST', 'E13.6', D( I - 1 ) )
         CALL MSG_FMTI( 'SUB', 'I3', I )
         CALL MSG_OUTIF( MSG__NORM, 'RESULTS',
     :        ' cf. with ^SUB :  ^PROB      ^DIST  (^KEEP)', STATUS )

*  Free up the scratch array.
         CALL PSX_FREE( WNPTR, STATUS )

      END DO

*  Write the PROB and D arrays to parameters (and list of filenames).
*  By definition the number of files must be more than one (either
*  because that was supplied, or the number of subsamples must be at
*  least two).
      IF ( SINGLE ) THEN
         CALL PAR_PUT1C( 'FILES', 1, FNAMES, STATUS )
      ELSE
         CALL PAR_PUT1C( 'FILES', NFILES, FNAMES, STATUS )
      END IF
      CALL PAR_PUT1R( 'PROB', NFILES - 1, PROB, STATUS )
      CALL PAR_PUT1R( 'DIST', NFILES - 1, D, STATUS )

*  Let's be friendly and tell the user something.
      CALL MSG_BLANKIF( MSG__NORM, STATUS )
      IF ( NFAIL .EQ. NFILES - 1 ) THEN
         CALL MSG_OUTIF( MSG__NORM, 'FINAL',
     :     'All subsamples were rejected', STATUS )

      ELSE

         IF ( NFAIL .GT. 0 ) THEN
            CALL MSG_SETI( 'FAIL', NFAIL )
            CALL MSG_OUTIF( MSG__NORM, 'FINAL',
     :        'Number of subsamples rejected: ^FAIL', STATUS )
         ELSE
            CALL MSG_OUTIF( MSG__NORM, 'FINAL',
     :        'All subsamples were accepted.', STATUS )
         END IF

*  The mean value of the consistent dataset is very useful.
*  Compute the statistics with no clipping.
         NCLIP = 0
         CLIP( 1 ) = 0

         CALL KPG1_STATR( .FALSE., NKEPT, %VAL( CNF_PVAL( KSDPTR ) ),
     :                    NCLIP, CLIP( I ), ITEMP, IMIN( 1 ), DMIN,
     :                    IMAX( 1 ), DMAX, SUM, MEAN, STDEV, ITEMP,
     :                    IMINC( 1 ), DMINC, IMAXC( 1 ), DMAXC, SUMC,
     :                    MEANC, STDEVC, STATUS )

         VAR = REAL( STDEV ) / SQRT( REAL( NKEPT ) )

*  Report the statistics.
         CALL MSG_SETR( 'MEAN', REAL( MEAN ) )
         CALL MSG_SETR( 'VAR', VAR )

         CALL MSG_OUTIF( MSG__NORM, 'COADD',
     :     'Coadded result is ^MEAN +/- ^VAR', STATUS )
         CALL MSG_BLANKIF( MSG__NORM, STATUS )

*  Write to the results parameters.
         CALL PAR_PUT0I( 'NKEPT', NKEPT, STATUS )
         CALL PAR_PUT0D( 'MEAN', MEAN, STATUS )
         CALL PAR_PUT0D( 'SIGMA', STDEV, STATUS )
         CALL PAR_PUT0D( 'ERRMEAN', STDEV / SQRT( DBLE( NKEPT ) ),
     :                   STATUS )

*  Now create the output file.  Start a new error context for the
*  acceptable null reponse.
         CALL ERR_MARK
         LBND( 1 ) = 1
         UBND( 1 ) = NKEPT
         CALL LPG_PROP( NDFR, 'NOAXIS,UNITS', 'OUT', NDFO, STATUS )

*  Change the bounds and type.
         CALL NDF_SBND( 1, LBND, UBND, NDFO, STATUS )
         CALL NDF_STYPE( '_REAL', NDFO, 'Data', STATUS )
         IF ( USEVAR )
     :     CALL NDF_STYPE( '_REAL', NDFO, 'Variance', STATUS )

         IF ( STATUS .EQ. SAI__OK .AND. NDFO .NE. NDF__NOID ) THEN

*  Map the output data array.  Copy the processed data to the output
*  file.
            CALL KPG1_MAP( NDFO, 'DATA', '_REAL', 'WRITE',
     :                    OUTPTR( 1 ), ITEMP, STATUS )

            CALL VEC_RTOR( .FALSE., NKEPT, %VAL( CNF_PVAL( KSDPTR ) ),
     :                     %VAL( CNF_PVAL( OUTPTR( 1 ) ) ),
     :                     IERR, NERR, STATUS )

*  Map the variance array.  Copy the processed variance to the output
*  file.
            IF ( USEVAR ) THEN
               CALL KPG1_MAP( NDFO, 'VARIANCE', '_REAL', 'WRITE',
     :                       OUTPTR( 2 ), ITEMP, STATUS )

               CALL VEC_RTOR( .FALSE., NKEPT,
     :                        %VAL( CNF_PVAL( KSVPTR ) ),
     :                        %VAL( CNF_PVAL( OUTPTR( 2 ) ) ),
     :                        IERR, NERR, STATUS )
            END IF

*  We do not write any axis information since this cannot be known
*  for a merged dataset.

*  Tidy the output NDF.
            CALL NDF_UNMAP( NDFO, '*', STATUS )
            CALL NDF_ANNUL( NDFO, STATUS )

*  Treat null as meaning do not create an output NDF.
         ELSE IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
         END IF
         CALL ERR_RLSE
      END IF

*  Tidy up.
      CALL PSX_FREE( WPTR, STATUS )

      DO I = 1, NFILES
         CALL PSX_FREE( GDPTR( I ), STATUS )
         CALL PSX_FREE( GVPTR( I ), STATUS )
      END DO

      CALL PSX_FREE( KSDPTR, STATUS )
      CALL PSX_FREE( KSVPTR, STATUS )

 999  CONTINUE
      CALL GRP_DELET( IGRP, STATUS )
      CALL NDF_END( STATUS )

*  Make the closing error report if something has gone wrong.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'KSTEST_ERR',
     :     'KSTEST: Error performing a two-sided Kolmogorov-Smirnoff '/
     :     /'test.', STATUS )
      END IF

      END
