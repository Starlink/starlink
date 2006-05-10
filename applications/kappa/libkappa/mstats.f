      SUBROUTINE MSTATS( STATUS )
*+
*  Name:
*     MSTATS

*  Purpose:
*     Calculate statistics over a group of data arrays or points.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL MSTATS( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)

*  Description:
*     This application calculates cumulative statistics over a group
*     of NDFs.  It can either generate the statistics of each 
*     corresponding pixel in the input array components and output
*     a new NDF with array components containing the result, or
*     calculate statistics at a single point specified in the 
*     current co-ordinate Frame of the input NDFs.
*
*     In array mode (SINGLE=FALSE), statistics are calculated for each
*     pixel in one of the array components (DATA, VARIANCE or QUALITY)
*     accumulated over all the input NDFs and written to an output
*     NDF; each pixel of the output NDF is a result of combination
*     of pixels with the same Pixel co-ordinates in all the input NDFs.
*     If SMODE="Mean" then the output NDF has a DATA component giving
*     the pixel averages of the input NDFs, and a VARIANCE component
*     giving the variances.  If SMODE="Median", then the output NDF 
*     has only a DATA component, which gives the medians.  The data type
*     of the output arrays will be of a floating point type even if
*     the input arrays were of an integer type.  The input NDFs 
*     must all have the same number of dimensions, but need not all 
*     be the same shape.  The shape of the output NDF can be set
*     to either the intersection or the union of the shapes of the
*     input NDFs using the TRIM parameter.
*
*     In single pixel mode (SINGLE=TRUE) a position in the current
*     co-ordinate Frame of all the NDFs is given, and the value at
*     the pixel covering this point in each of the input NDFs is
*     accumulated to form the result.  The result, and if ILEVEL = 3,
*     the value of each contributing pixel, is reported directly 
*     to the user.

*  Usage:
*     mstats in out

*  ADAM Parameters:
*     COMP = LITERAL (Read)
*        The NDF array component to be analysed.  It may be "Data",
*        "Quality", "Variance", or "Error" (where "Error" is an
*        alternative to "Variance" and causes the square root of the
*        variance values to be used).  If "Quality" is specified,
*        then the quality values are treated as numerical values (in
*        the range 0 to 255).  In cases other than "Data", which is 
*        always present, a missing component will be treated as having 
*        all pixels set to the `bad' value.  ["Data"]
*     ILEVEL = _INTEGER (Read)
*        The interactive level of the routine (only used if 
*        SINGLE=TRUE).  If it is 1, nothing is reported on the screen. 
*        If is is 2 or 3, the  statistics are reported.  If is is 3, 
*        the individual pixel values are also reported.  It should lie 
*        between 1 and 3.  [2]
*     IN = NDF (Read)
*        A group of input NDFs.  They may have different shapes, but 
*        must all have the same number of dimensions.  This should 
*        be given as a comma separated list, in which each list element 
*        can be one of the following.
*
*        - An NDF name, optionally containing wild-cards and/or regular 
*        expressions ("*", "?", "[a-z]" etc.).
*
*        - The name of a text file, preceded by an up-arrow character 
*        "^".  Each line in the text file should contain a 
*        comma-separated list of elements, each of which can in turn be 
*        an NDF name (with optional wild-cards, etc.), or another file 
*        specification (preceded by an up-arrow).  Comments can be 
*        included in the file by commencing lines with a hash character 
*        "#".
*
*        If the value supplied for this parameter ends with a minus
*        sign "-", then the user is re-prompted for further input until
*        a value is given which does not end with a minus sign.  All the
*        images given in this way are concatenated into a single group.
*     MEAN = _DOUBLE (Write)
*        An output parameter to which is written the mean pixel value,
*        if SINGLE=TRUE.
*     MEDIAN = _DOUBLE (Write)
*        An output parameter to which is written the median pixel value,
*        if SINGLE=TRUE.
*     OUT = NDF (Read)
*        The name of an NDF to receive the results.  Each pixel of
*        the DATA (and perhaps VARIANCE) component represents the
*        statistics of the corresponding pixels of the input NDFs.
*        Only used if SINGLE=FALSE.
*     POS = LITERAL (Read)
*        In Single pixel mode (SINGLE=TRUE), this parameter gives the
*        position in the current co-ordinate Frame at which the
*        statistics should be calculated (supplying a colon ":" will 
*        display details of the required co-ordinate Frame).  The 
*        position should be supplied as a list of formatted axis values 
*        separated by spaces or commas.  The pixel covering this point
*        in each input array, if any, will be used.
*     SINGLE = _LOGICAL (Read)
*        Whether the statistics should be calculated in Single pixel
*        mode or Array mode.  If SINGLE=TRUE, then the POS parameter
*        will be used to get the point to which the statistics refer,
*        but if SINGLE=FALSE an output NDF will be generated containing
*        the results for all the pixels.  [FALSE]
*     SMODE = LITERAL (Read)
*        The way in which the input arrays should be processed in order
*        to generate the output NDF.  It may be "Mean" or "Median".
*        If it is "Mean" (the default) then OUT will be an NDF in which
*        the DATA component gives the averages of the pixels of the 
*        selected component of the input NDFs, and the VARIANCE 
*        component gives their variances.  If it is "Median" then then
*        OUT will have a DATA component giving the medians of the input 
*        pixels, and it will have no VARIANCE component.  ["Mean"]
*     TITLE = LITERAL (Read)
*        Title for the output NDF.  ["KAPPA - Mstats"]
*     TRIM = _LOGICAL (Read)
*        This parameter controls the shape of the output NDF.  If 
*        TRIM=TRUE, then the output NDF is the shape of the intersection
*        of all the input NDFs, i.e. only pixels which appear in all the
*        input arrays will be represented in the output.  If TRIM=FALSE,
*        the output is the shape of the union of the inputs, i.e. every
*        pixel which appears in the input arrays will be represented in 
*        the output.  [TRUE]
*     VAR = _DOUBLE (Write)
*        An output parameter to which is written the variance of the 
*        pixel values, if SINGLE=TRUE.

*  Examples:
*     mstats idat* ostats
*        This calculates the mean and variance of each pixel in the 
*        Data arrays of all the NDFs in the current directory with names
*        which start "idat", and writes the result in a new NDF called
*        "ostats".  The shape of ostats will be the intersection of the 
*        volumes of all the indat* NDFs.
*     mstats idat* ostats trim=false
*        This does the same as the previous example, except that the
*        output NDF will be the `union' of the volumes of the input
*        NDFs, that is a cuboid with lower bounds as low as the
*        lowest pixel bound of the input NDFs in each dimension and
*        with upper bounds as high as the highest pixel bound in
*        each dimension.
*     mstats idat* ostats comp=variance
*        This does the same as the first example except that statistics
*        are calculated on the VARIANCE components of all the input
*        NDFs.  Thus the pixels of the VARIANCE component of "ostats" 
*        will be the variances of the variances of the input data.
*     mstats m31* single=true pos="0:42:38,40:52:20" 
*        This example is analysing the pixel brightness at the indicated
*        sky position in a number of NDFs whose name start with "m31",
*        which all have SKY as their current co-ordinate Frame.
*        The mean and variance of the pixels at that position in all
*        the NDFs are printed to the screen.
*     mstats m31* single pos="0:42:38,40:52:20" ilevel=3
*        This does the same as the previous example, but also prints
*        out the value of the sampled pixel in each of the NDFs.
*        For those in which the pixel at the selected position is
*        bad or falls outside the NDF, this is also indicated.
*     mstats in="arr1,arr2,arr3" out=middle smode=median
*        This example calculates the medians of the DATA components of
*        the three named NDFs and writes them into a new NDF called
*        "middle".

*  Related Applications:
*     CCDPACK: MAKEMOS, MAKECAL, MAKEFLAT.

*  Copyright:
*     Copyright (C) 2001, 2004 Central Laboratory of the Research
*     Councils. Copyright (C) 2006 Particle Physics & Astronomy
*     Research Council. All Rights Reserved.

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
*     MBT: Mark Taylor (Starlink)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     26-NOV-2001 (MBT):
*        Original version.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL.
*     2006 April 12 (MJC):
*        Remove unused variables, correct punctuation, and wrapped long
*        lines.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No default typing allowed

*  Global Constants:
      INCLUDE  'SAE_PAR'         ! Global SSE definitions
      INCLUDE  'AST_PAR'         ! AST constants and functions
      INCLUDE  'NDF_PAR'         ! NDF constants
      INCLUDE  'DAT_PAR'         ! HDS system constants
      INCLUDE  'CNF_PAR'         ! For CNF_PVAL function

*  Status:
      INTEGER STATUS

*  Local Variables:
      CHARACTER COMP * ( DAT__SZNAM ) ! Name of NDF component for stats
      CHARACTER ITYPE * ( NDF__SZTYP ) ! HDS type of output data arrays
      CHARACTER SMODE * ( 8 )    ! Calculation mode - 'mean' or 'median'
      CHARACTER STRIM * ( 8 )    ! Trim parameter to pass to NDF_MBNDN
      DOUBLE PRECISION DATUM     ! Value of pixel
      DOUBLE PRECISION MEAN      ! Average value
      DOUBLE PRECISION MED       ! Median value
      DOUBLE PRECISION SUM       ! Running total of pixel values
      DOUBLE PRECISION SUM2      ! Running total of pixel values squared
      DOUBLE PRECISION VAR       ! Variance value
      INTEGER EL                 ! Number of pixels in mapped array
      INTEGER I                  ! Loop variable
      INTEGER IGRP               ! GRP identifier for input-NDFs group
      INTEGER ILEVEL             ! The interaction level
      INTEGER IPDAT              ! Pointer to data array
      INTEGER IPNDF              ! Pointer to array of NDF identifiers
      INTEGER IPODAT             ! Pointer to output data array
      INTEGER IPOVAR             ! Pointer to output variance array
      INTEGER IPPTR              ! Pointer to array of pointers to 
                                 ! mapped arrays
      INTEGER IPWORK             ! Pointer to workspace array
      INTEGER LBND( NDF__MXDIM ) ! Lower pixel bounds of output NDF
      INTEGER NGOOD              ! Number of non-bad pixels
      INTEGER NGOOD1             ! Number of pixels used
      INTEGER NNDF               ! The number of input NDFs
      INTEGER ONDF               ! NDF identifier of output NDF
      LOGICAL SINGLE             ! Are we in SINGLE or ARRAY mode?
      LOGICAL TRIM               ! Will be trim or pad arrays?

*.

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start an AST context.
      CALL AST_BEGIN( STATUS )

*  Start an NDF context.
      CALL NDF_BEGIN

*  Get a group containing the names of the NDFs to be processed.
      NNDF = 0
      CALL KPG1_RGNDF( 'IN', 0, 1, '  Give more NDFs...', IGRP, 
     :                 NNDF, STATUS )

*  Determine whether we are in single pixel or array mode.
      CALL PAR_GET0L( 'SINGLE', SINGLE, STATUS )

*  Get the component we require.
      CALL PAR_CHOIC( 'COMP', 'DATA', 'DATA,VARIANCE,QUALITY,ERROR',
     :                .FALSE., COMP, STATUS )

*  Single pixel mode.
*  ==================
      IF ( SINGLE ) THEN

*  Allocate some work space.
         CALL PSX_CALLOC( NNDF, '_DOUBLE', IPDAT, STATUS )

*  Get interaction level for rejection algorithm.
         CALL PAR_GDR0I( 'ILEVEL', 2, 1, 3, .TRUE., ILEVEL, STATUS )

*  Get the pixels from which to calculate the statistics.
         CALL KPS1_MSS( IGRP, NNDF, COMP, ILEVEL, NGOOD, 
     :                  %VAL( CNF_PVAL( IPDAT ) ),
     :                  STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 999

*  See if we have found any pixels at the specified point.
         IF ( NGOOD .GT. 0 ) THEN

*  If so, calculate the median.
*  (Note: the first argument should properly be .FALSE., but this 
*  currently falls foul of a bug in KPG1_MEDUD.  This version is
*  correct, and is hardly a performance bottleneck).
            CALL KPG1_MEDUD( .TRUE., NGOOD, %VAL( CNF_PVAL( IPDAT ) ), 
     :                       MED,
     :                       NGOOD1, STATUS )

*  And calculate the mean and variance.
            SUM = 0D0
            SUM2 = 0D0
            DO I = 1, NGOOD
               CALL KPG1_RETRD( NGOOD, I, %VAL( CNF_PVAL( IPDAT ) ), 
     :                          DATUM, STATUS )
               SUM = SUM + DATUM
               SUM2 = SUM2 + DATUM * DATUM
            END DO
            MEAN = SUM / DBLE( NGOOD )
            VAR = SUM2 / DBLE( NGOOD ) - MEAN * MEAN

*  Output the results, if required.
            IF( ILEVEL .GT. 1 ) THEN
               CALL MSG_SETD( 'MED', MED )
               CALL MSG_OUT( ' ', '    Pixel median:      ^MED',
     :                       STATUS )
               CALL MSG_SETD( 'MEAN', MEAN )
               CALL MSG_OUT( ' ', '    Pixel mean:        ^MEAN',
     :                       STATUS )
               CALL MSG_SETD( 'VAR', VAR )
               CALL MSG_OUT( ' ', '    Pixel variance:    ^VAR',
     :                       STATUS )
               CALL MSG_BLANK( STATUS )
            END IF

*  Write them to output parameters.
            CALL PAR_PUT0D( 'MEDIAN', MED, STATUS )
            CALL PAR_PUT0D( 'MEAN', MEAN, STATUS )
            CALL PAR_PUT0D( 'VAR', VAR, STATUS )

*  There were no good pixels - write an error message.
         ELSE
            STATUS = SAI__ERROR
            CALL ERR_REP( 'MSTATS_ERR1', 'There are no good pixels.',
     :                    STATUS )
         END IF

*  Release the work space.
         CALL PSX_FREE( IPDAT, STATUS )

*  Array mode.
*  ===========
      ELSE

*  See whether we want mean and variance or median.
         CALL PAR_CHOIC( 'SMODE', 'MEAN', 'MEAN,MEDIAN', .FALSE., SMODE,
     :                   STATUS )

*  See whether the output NDF will be the union or intersection of the 
*  inputs.
         CALL PAR_GET0L( 'TRIM', TRIM, STATUS )
         IF ( TRIM ) THEN
            STRIM = 'TRIM'
         ELSE
            STRIM = 'PAD'
         END IF

*  Allocate some work space.
         CALL PSX_CALLOC( NNDF, '_INTEGER', IPNDF, STATUS )
         CALL PSX_CALLOC( NNDF, '_INTEGER', IPPTR, STATUS )

*  Map pixel-matched requested components of all the NDFs.
         CALL KPS1_MSA( IGRP, NNDF, COMP, STRIM, 'OUT', ITYPE,
     :                  %VAL( CNF_PVAL( IPNDF ) ), 
     :                  %VAL( CNF_PVAL( IPPTR ) ), ONDF, STATUS )

*  Get a title for it from the parameter system.
         CALL NDF_CINP( 'TITLE', ONDF, 'Title', STATUS )

*  Map the Data and possibly Variance components of the output NDF.
         CALL NDF_MAP( ONDF, 'DATA', ITYPE, 'WRITE', IPODAT, EL,
     :                 STATUS )
         IF ( SMODE .EQ. 'MEAN' ) THEN
            CALL NDF_MAP( ONDF, 'VARIANCE', ITYPE, 'WRITE', IPOVAR, EL,
     :                    STATUS )
         END IF

*  Calculate the median if so requested.
         IF ( SMODE .EQ. 'MEDIAN' ) THEN

*  Allocate more workspace.
            CALL PSX_CALLOC( NNDF, ITYPE, IPWORK, STATUS )

*  Get the median.
            IF ( ITYPE .EQ. '_REAL' ) THEN
               CALL KPS1_MEDR( NNDF, EL, %VAL( CNF_PVAL( IPPTR ) ), 
     :                         %VAL( CNF_PVAL( IPWORK ) ),
     :                         %VAL( CNF_PVAL( IPODAT ) ), STATUS )
            ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
               CALL KPS1_MEDD( NNDF, EL, %VAL( CNF_PVAL( IPPTR ) ), 
     :                         %VAL( CNF_PVAL( IPWORK ) ),
     :                         %VAL( CNF_PVAL( IPODAT ) ), STATUS )
            END IF

*  Release the workspace.
            CALL PSX_FREE( IPWORK, STATUS )

*  Otherwise calculate the mean and variance.
         ELSE
            IF ( ITYPE .EQ. '_REAL' ) THEN
               CALL KPS1_MMVR( NNDF, EL, %VAL( CNF_PVAL( IPPTR ) ), 
     :                         %VAL( CNF_PVAL( IPODAT ) ),
     :                         %VAL( CNF_PVAL( IPOVAR ) ), STATUS )
            ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
               CALL KPS1_MMVD( NNDF, EL, %VAL( CNF_PVAL( IPPTR ) ), 
     :                         %VAL( CNF_PVAL( IPODAT ) ),
     :                         %VAL( CNF_PVAL( IPOVAR ) ), STATUS )
            END IF
         END IF

*  Release workspace.
         CALL PSX_FREE( IPNDF, STATUS )
         CALL PSX_FREE( IPPTR, STATUS )

*  Annul the output NDF.
         CALL NDF_ANNUL( ONDF, STATUS )
      END IF

*  Come here if something has gone wrong.
  999 CONTINUE

*  Release GRP resources.
      CALL GRP_DELET( IGRP, STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  Report a contextual message if anything went wrong.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'MSTATS_ERR2', 
     :                 'MSTATS: Unable to calculate statistics.',
     :                 STATUS )
      END IF

      END
