      SUBROUTINE CCD1_DOMOS( NIN, NDF, LBND, UBND, ADJUST, SCALE,
     :                       DSCALE, ZERO, DZERO, ORIGIN, MODIFY,
     :                       DOOUT, USEVAR, GENVAR, USEWT, WEIGHT,
     :                       IMETH, ALPHA, NSIGMA, NITER, RMIN, RMAX,
     :                       NDFOUT, BAD, BADIN, IBOUND, LIST, PNTR,
     :                       VARS, VARLIN, STATUS )
*+
*  Name:
*     CCD1_DOMOS

*  Purpose:
*     Form a mosaic from a set of NDFs.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_DOMOS( NIN, NDF, LBND, UBND, ADJUST, SCALE, DSCALE,
*                      ZERO, DZERO, ORIGIN, MODIFY, DOOUT, USEVAR,
*                      GENVAR, USEWT, WEIGHT, IMETH, ALPHA, NSIGMA,
*                      NITER, RMIN, RMAX, NDFOUT, BAD, BADIN, IBOUND,
*                      LIST, PNTR, VARS, VARLIN, STATUS )

*  Description:
*     The routine merges an arbitrary set of "input" NDFs into a mosaic
*     by combining their data (and variance) values pixel-by-pixel
*     (matching pixels with the same indices between the input NDFs).
*     It will optionally apply scale factor and zero point corrections
*     to the input data before combining them and, if required, will
*     modify the "input" NDFs themselves using the same corrections.
*     Corrections may be applied to the input NDFs without producing an
*     output mosaic if necessary.
*
*     This routines is designed to limit its use of memory and to
*     access all NDF arrays near-sequentially, regardless of the number
*     of input NDFs or the size of mosaic being produced. It should
*     therefore work reasonably efficiently, in terms of memory usage
*     and I/O, in a wide range of situations.

*  Arguments:
*     NIN = INTEGER (Given)
*        Number of "input" NDFs to be combined into the mosaic.
*     NDF( NIN ) = INTEGER (Given)
*        Array of identifiers for the NDFs to be combined.
*     LBND( NDF__MXDIM, NIN ) = INTEGER (Given)
*        Array containing the lower pixel index bounds for each
*        dimension of each input NDF.
*     UBND( NDF__MXDIM, NIN ) = INTEGER (Given)
*        Array containing the upper pixel index bounds for each
*        dimension of each input NDF.
*     ADJUST = LOGICAL (Given)
*        Whether to apply scale factor and zero point corrections to
*        the input data.
*     SCALE( NIN ) = DOUBLE PRECISION (Given)
*        Array of scale factor corrections to be applied to the input
*        NDFs' data values (only used if ADJUST is .TRUE.).
*     DSCALE( NIN ) = DOUBLE PRECISION (Given)
*        Array of error estimates on the scale factor corrections (only
*        used if ADJUST is .TRUE. and variance values are being
*        corrected, modified or used as weights).
*     ZERO( NIN ) = DOUBLE PRECISION (Given)
*        Array of zero point corrections to be applied to the input
*        NDFs' data values (only used if ADJUST is .TRUE.).
*     DZERO( NIN ) = DOUBLE PRECISION (Given)
*        Array of error estimates on the zero point corrections (only
*        used if ADJUST is .TRUE. and variance values are being
*        corrected, modified or used as weights).
*     ORIGIN( NIN ) = DOUBLE PRECISION (Given)
*        Array of false origin values to be used in applying the scale
*        factor and zero point corrections (only used if ADJUST is
*        .TRUE.). The correction applied to the data in input NDF I is:
*
*           new = ( old - ORIGIN( I ) ) * SCALE( I ) + ZERO( I )
*
*        The origin values should be chosen so as to make the errors on
*        the scale factor and zero point corrections statistically
*        independent.
*     MODIFY = LOGICAL (Given)
*        Whether the input NDFs' contents are to be modified by the
*        scale factor and zero point corrections (if so, then WRITE
*        access must be available for these "input" NDFs). This value
*        is only significant if if ADJUST is .TRUE.. Otherwise, the
*        input NDFs will not be modified.
*     DOOUT = LOGICAL (Given)
*        Whether an output mosaic NDF is to be produced. Note that
*        either MODIFY or DOOUT (or both) should be .TRUE..
*     USEVAR = LOGICAL (Given)
*        Whether variance information present in the input NDFs should
*        be used to weight their data values when they are combined
*        (note that input variance information will only actually be
*        used if all the input NDFs contain this information).
*     GENVAR = LOGICAL (Given)
*        Whether to generate a variance component in the output NDF
*        (note that output variances will only actually be generated if
*        all the input NDFs contain this information and if USEVAR is
*        also .TRUE.).
*     USEWT = LOGICAL (Given)
*        Whether to apply an explicit list of weighting factors to the
*        input NDFs (this will only be done if USEVAR is .FALSE. and
*        USEWT is .TRUE.).
*     WEIGHT( NIN ) = REAL (Given)
*        Array of explicit positive weighting factors to be applied to
*        the input NDFs if USEVAR is .FALSE. and USEWT is .TRUE..
*        Otherwise they will not be used. Note that if both USEVAR and
*        USEWT are .FALSE. and (a) ADJUST is .TRUE., then the scale
*        factor adjustments will be used to derive weights, otherwise
*        if (b) ADJUST is .FALSE, then unit weights will be used for
*        all input NDFs.
*     IMETH = INTEGER (Given)
*        An integer representing the required method for combining the
*        input data into a mosaic:
*           2 = MEAN
*           3 = WEIGHTED MEDIAN
*           4 = TRIMMED MEAN
*           5 = MODE
*           6 = SIGMA CLIPPED MEAN
*           7 = THRESHOLD EXCLUSION MEAN
*           8 = MINMAX MEAN
*           9 = BROADENED MEDIAN
*           10 = SIGMA CLIPPED MEDIAN
*           11 = UNWEIGHTED MEDIAN
*        This, and the following associated arguments, are only used if
*        DOOUT is .TRUE..
*     ALPHA = REAL (Given)
*        Fraction of values to remove from data (appropriate to IMETH =
*        4).
*     NSIGMA = REAL (Given)
*        Number of standard deviations at which to clip the data
*        (appropriate to IMETH = 5 and 6).
*     NITER = INTEGER (Given)
*        Maximum number of clipping iterations (appropriate to IMETH =
*        5 and 6).
*     RMIN = REAL (Given)
*        Minimum allowed data value (appropriate to IMETH = 7).
*     RMAX = REAL (Given)
*        Maximum allowed data value (appropriate to IMETH = 7).
*     NDFOUT = INTEGER (Given)
*        Identifier for the output NDF into which the results are to be
*        written. The characteristics of this NDF also determine the
*        shape and data type of the output mosaic. It should not be
*        mapped for access prior to calling this routine. This NDF is
*        required even if DOOUT is .FALSE. (although in this case it
*        will not be written to and may be a "super-set" section of a
*        smaller NDF). If MODIFY is .TRUE., then the extent of this NDF
*        should encompass all the input NDFs (otherwise only part of
*        them will have corrections applied).
*     BAD( 2, NIN ) = LOGICAL (Returned)
*        Workspace.
*     BADIN( 2, NIN ) = LOGICAL (Returned)
*        Workspace. This array is only used if (MODIFY.AND.ADJUST) is
*        .TRUE..
*     IBOUND( 2 * ( NIN + 1 ) ) = INTEGER (Returned)
*        Workspace.
*     LIST( 2, NIN ) = INTEGER (Returned)
*        Workspace.
*     PNTR( 2, NIN ) = INTEGER (Returned)
*        Workspace.
*     VARS( NIN ) = DOUBLE PRECISION (Returned)
*        Workspace.
*     VARLIN( NIN ) = DOUBLE PRECISION (Returned)
*        Workspace.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     The main difficulty this routine has to overcome occurs when a
*     large number of input NDFs contribute to a large output mosaic,
*     and where these NDFs overlap in arbitrary ways. The mosaic to be
*     produced is then divided into a large number of "regions" by the
*     boundaries of the input NDFs. Within any region the number of
*     contributing input NDFs is constant, but the number and size of
*     the regions is arbitrary and difficult to cater for.
*
*     In this situation it is not possible to access all the input data
*     and combine it explicitly (too much memory required). Nor is it
*     possible to process each region separately, as this will generate
*     many scattered (hence inefficient) references to the input data,
*     frequently in regions which are also too small for efficiency.
*
*     The approach used here is involves a two-level partitioning
*     scheme. In this, "NDF chunking" (see SUN/33) is used to limit
*     memory usage by coarsely partitioning the mosaic. A finer level
*     of partitioning along a single dimension is then used to break
*     each chunk efficiently into regions where the number of input
*     NDFs may be considered constant (so that data combination can
*     take place). It operates as follows:
*
*     -  The first dimension of the output mosaic having an extent of
*     more than one pixel is identified and the NDF's extent along this
*     dimension (dimension IFIRST) is treated as a "line" of data
*     (normally this will be dimension 1, although not always).
*     -  The output NDF is partitioned using "NDF chunking", the chunk
*     size being chosen both to be an integral number of "lines" and
*     also to limit the amount of memory used at once (taking account
*     of all the workspace arrays to be allocated). In general, each
*     chunk will cross many of the separate "regions" defined by the
*     input NDFs.
*     -  A section is then selected from each of those input NDFs which
*     can possibly contribute to the current chunk. This section is
*     chosen to match the chunk bounds in all dimensions except IFIRST,
*     where it retains the original bounds of the input NDF. The data
*     in each of these sections are then accessed.
*     -  This strategy has the effect of preventing access to the input
*     data from being broken in dimension IFIRST, and hence of giving
*     near-sequential access to the input NDFs as each chunk is
*     processed.  Some padding of input data may still occur
*     (associated with input NDF bounds which cause breaks in
*     sequential access in higher dimensions) but this is relatively
*     inexpensive and the chunk size is restricted to prevent this
*     becoming excessive.
*     -  The above process moves the contributing input data for a
*     chunk into memory in such a way (by virtue of padding) that it is
*     now possible to handle the separate input "regions" simply by
*     partitioning each chunk along dimension IFIRST. This is done by
*     sorting the pixel index bounds for this dimension (both lower and
*     upper) for all the contributing NDFs into ascending order, and
*     considering each resulting dimension interval in turn.
*     -  Within each of these intervals, the number of contributing
*     input NDFs may now be considered constant. Producing the output
*     mosaic is then simply a process of assembling all the
*     contributing data (and variances if appropriate) for each
*     interval into a workspace stack and combining them. The results
*     are then copied to the appropriate part of the output chunk. It
*     is at this point that scattered access to input and output data
*     takes place, but the data now reside in a limited amount of
*     memory, so the cost is small.
*     -  Scale factor and zero point adjustments, if required, are
*     applied to the input data as they are accessed, either directly
*     (if the input NDFs are to be modified) or as values are assembled
*     into workspace for combining.

*  Implementation Deficiencies:
*     If combination routines return information about the production
*     of bad pixels, then handling of this information will need
*     revision.

*  Notes:
*     This routine will correctly set the bad pixel flag value(s) of
*     the output NDF (if generated) and the "input" NDFs (if modified).

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council

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
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     PDRAPER: Peter Draper (STARLINK)
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-AUG-1992 (RFWS):
*        Original version.
*     5-AUG-1992 (RFWS):
*        Handle output bad pixel flag values.
*     6-AUG-1992 (RFWS):
*        Eliminated the output mosaic bounds as arguments - now found
*        from the output NDF supplied.
*     6-AUG-1992 (RFWS):
*        Added status checks.
*     6-AUG-1992 (RFWS):
*        Fixed bug: array dimension sizes in wrong order.
*     23-SEP-1992 (RFWS):
*        Changed handling of bad pixel flags for mapped input arrays to
*        allow for possible data conversion errors during mapping (this
*        may happen even if the numeric type in use is "safe" because
*        of potential overflow if HDS data were originally written on
*        another machine with a wider number range).
*     20-OCT-1992 (PDRAPER):
*        Changed calls to CCDPACK version 0.1 standard. This involved
*        modifications so that no variances are produced when UVAR is
*        false. Did not change NARY as handling of output variance stack
*        not effected. Modified deficiencies section to remove these
*        items. Note that if GVAR is true and UVAR false then no output
*        variances are produced when the combination routines are used.
*     2-NOV-1992 (PDRAPER):
*        Removed ITYPE2 variable. This was commented as the output
*        processing data type, but was not initialised before use
*        and had only one reference. The reference was changed to ITYPE1
*        as the processing type is always the output type (I hope).
*     18-JAN-1993 (PDRAPER):
*        Changed call to NDF_SBAD when MODIFY to .TRUE. (was .FALSE.)
*        to correctly reset bad pixel flag in NDF.
*     20-JAN-1993 (RFWS):
*        Undid the previous change and instead added the BADIN
*        workspace array. Arranged that if "input" NDFs are being
*        modified, their bad pixel flags are set to their original
*        values before they are mapped for update access, and restored
*        to their modified values before being unmapped again. This is
*        to allow for the fact that "input" (unprocessed) pixels may
*        have a different flag value to "output" (processed) pixels,
*        even although they are stored in the same array.
*     20-JAN-1993 (RFWS):
*        Changed GVAR to be .FALSE. if UVAR is .FALSE. and propagated
*        this knowledge to logical expressions which use these values.
*        Modified the description of GENVAR to reflect this fact.
*     27-JUL-1993 (PDRAPER):
*        Added a better estimate of the workspace requirements for
*        covariance matrix. Changed from NII**3 to 0.5*(NNI+1)**3.
*        This reflects the increase of CCD1__MXNDF to 100. 8Mbytes
*        are needed for this array if left at NI**3, this reduces to
*        just over 4 under the new scheme.
*     26-JAN-1994 (PDRAPER):
*        Fixed arguments to ccg1_cm3r and ccg1_cm3d to remove
*        RESVAR, this is not present in subroutines.
*     19-SEP-1996 (PDRAPER):
*        Replaced NAG sorting routine with PDA equivalent.
*     14-JAN-1999 (PDRAPER):
*        Added changes to estimate variances from data stack. This
*        happens if GENVAR is TRUE and the input NDFs do not have any
*        variances. Related changes are increasing the minimum number of
*        contributing pixels to 2, when variances are estimated (the
*        associated variance of such regions would be BAD anyway).
*     27-NOV-2000 (MBT):
*        Fixed bug; the weight array being passed to CCG1_CM3 routines
*        was not indexed correctly.
*     5-JAN-2004 (MBT):
*        Fixed bug; attempt could be made to unmap non-mapped component
*        if the first chunk had no contributing pixels.  Think I've done
*        this correctly, but it is slightly surprising that it hasn't
*        been spotted before.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF global constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER NIN
      INTEGER NDF( NIN )
      INTEGER LBND( NDF__MXDIM, NIN )
      INTEGER UBND( NDF__MXDIM, NIN )
      LOGICAL ADJUST
      DOUBLE PRECISION SCALE( NIN )
      DOUBLE PRECISION DSCALE( NIN )
      DOUBLE PRECISION ZERO ( NIN )
      DOUBLE PRECISION DZERO( NIN )
      DOUBLE PRECISION ORIGIN( NIN )
      LOGICAL MODIFY
      LOGICAL DOOUT
      LOGICAL USEVAR
      LOGICAL GENVAR
      LOGICAL USEWT
      REAL WEIGHT( NIN )
      INTEGER IMETH
      REAL ALPHA
      REAL NSIGMA
      INTEGER NITER
      REAL RMIN
      REAL RMAX
      INTEGER NDFOUT

*  Arguments Returned:
      LOGICAL BAD( 2, NIN )
      LOGICAL BADIN( 2, NIN )
      INTEGER IBOUND( 2 * ( NIN + 1 ) )
      INTEGER LIST( 2, NIN )
      INTEGER PNTR( 2, NIN )
      DOUBLE PRECISION VARS( NIN )
      DOUBLE PRECISION VARLIN( NIN )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MXPIX0             ! Max. pixels in memory at once
      PARAMETER ( MXPIX0 = 500000 )

*  Local Variables:
      CHARACTER * ( 13 ) COMP1  ! Input NDF component list
      CHARACTER * ( 13 ) COMP2  ! Output NDF component list
      CHARACTER * ( 6 ) ACMODE  ! Access mode string
      CHARACTER * ( NDF__SZFTP ) DTYPE1 ! Input storage data type
      CHARACTER * ( NDF__SZTYP ) ITYPE1 ! Input processing data type
      INTEGER DATSTK            ! Pointer to workspace (data stack)
      INTEGER EL                ! Number of array elements
      INTEGER I                 ! Loop counter for input NDFs
      INTEGER ICHUNK            ! Loop counter for NDF chunks
      INTEGER IEND              ! Upper dimension bound of interval
      INTEGER IFAIL             ! NAG library error flag
      INTEGER IFIRST            ! Index of first significant dimension
      INTEGER II                ! Index into contributing NDF list
      INTEGER INDF              ! ID for input NDF section
      INTEGER INTVL             ! Loop counter for dimension intervals
      INTEGER ISTART            ! Lower dimension bound of interval
      INTEGER LBC               ! Saved lower chunk dimension limit
      INTEGER LBNDC( NDF__MXDIM ) ! Lower bounds of NDF chunk
      INTEGER LBNDX( NDF__MXDIM ) ! Lower bounds of output mosaic
      INTEGER MINBAD            ! Minimum no. bad pixels expected
      INTEGER MINPIX            ! Minimum no. contributing pixels
      INTEGER MXPIX             ! Maximum number of pixels in a chunk
      INTEGER NARY              ! Number of workspace arrays
      INTEGER NBADD             ! Number of bad data pixels
      INTEGER NBADV             ! Number of bad variance pixels
      INTEGER NBASE             ! Base NDF identifier
      INTEGER NCHUNK            ! Number of NDF chunks available
      INTEGER NDFC              ! ID for NDF chunk
      INTEGER NDIMC             ! Number of NDF chunk dimensions
      INTEGER NDIMX             ! Number of output dimensions
      INTEGER NIC               ! No. of NDFs contributing to a chunk
      INTEGER NII               ! No. NDFs contributing to an interval
      INTEGER NLINES            ! Number of "lines" in chunk
      INTEGER NVAR              ! Number of input NDFs with variance
      INTEGER NWRK4             ! Size of WRK4 workspace
      INTEGER OPTLIN            ! Optimum number of lines to process
      INTEGER PIC               ! Number of pixels in common
      INTEGER PNTR2( 2 )        ! Pointers to mapped output arrays
      INTEGER RESDAT            ! Pointer to result data array
      INTEGER RESVAR            ! Pointer to result variance array
      INTEGER UBC               ! Saved upper chunk dimension limit
      INTEGER UBNDC( NDF__MXDIM ) ! Upper bounds of NDF chunk
      INTEGER UBNDX( NDF__MXDIM ) ! Upper bounds of output mosaic
      INTEGER VARSTK            ! Pointer to workspace (variance stack)
      INTEGER WRK1              ! Pointer to workspace
      INTEGER WRK2              ! Pointer to workspace
      INTEGER WRK3              ! Pointer to workspace
      INTEGER WRK4              ! Pointer to workspace
      INTEGER WRK5              ! Pointer to workspace
      INTEGER WRK6              ! Pointer to workspace
      INTEGER WRK7              ! Pointer to workspace
      LOGICAL BADCHK            ! Check for bad pixels needed?
      LOGICAL BDOUTD            ! Output data has bad pixels?
      LOGICAL BDOUTV            ! Output variance has bad pixels?
      LOGICAL GVAR              ! Generate output variances?
      LOGICAL OUT1              ! First output chunk to be written?
      LOGICAL UVAR              ! Use variances for weighting?
      LOGICAL VAR               ! Input NDF has variance information?
      LOGICAL EVAR              ! Create variances from data

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise.
*  ==========
      BDOUTV = .FALSE.
      BDOUTD = .FALSE.
      NBADV = 0
      NBADD = 0

*  Minimum number of contributing pixels.
      MINPIX = 1



*  Determine the shape of the output NDF.
      CALL NDF_BOUND( NDFOUT, NDF__MXDIM, LBNDX, UBNDX, NDIMX, STATUS )

*  Set up an access mode string for accessing the "input" NDFs.
      ACMODE = 'READ'
      IF ( MODIFY .AND. ADJUST ) ACMODE = 'UPDATE'

*  Loop through each input NDF to determine whether variance
*  information is present. Count the number where it is.
      NVAR = 0
      DO 1 I = 1, NIN
         CALL NDF_STATE( NDF( I ), 'Variance', VAR, STATUS )
         IF ( VAR ) NVAR = NVAR + 1

*  If the "input" NDFs are being modified, then record the original bad
*  pixel flag value for the data and variance arrays of the associated
*  base NDF. Annul the base NDF identifier when finished.
         IF ( MODIFY .AND. ADJUST ) THEN
            CALL NDF_BASE( NDF( I ), NBASE, STATUS )
            CALL NDF_BAD( NBASE, 'Data', .FALSE., BADIN( 1, I ),
     :                    STATUS )
            CALL NDF_BAD( NBASE, 'Variance', .FALSE., BADIN( 2, I ),
     :                    STATUS )
            CALL NDF_ANNUL( NBASE, STATUS )

*  Reset the "input" bad pixel flags (these will be re-determined as
*  corrections are applied).
            CALL NDF_SBAD( .FALSE., NDF( I ), 'Data,Variance', STATUS )
         END IF
         IF ( STATUS .NE. SAI__OK ) GO TO 99
 1    CONTINUE

*  Determine whether to use variances for weighting when the input data
*  are combined, and whether to generate an output variance component.
*  Both of these depend on the settings of input arguments and on
*  whether all the input NDFs contain variance information. Note if
*  input NDFs do not have enough variances and GENVAR is true then
*  variances will be created from the variation in the input data.
      UVAR = ( USEVAR .AND. ( NVAR .EQ. NIN ) )
      GVAR = ( GENVAR .AND. ( NVAR .EQ. NIN ) .AND. UVAR )
      EVAR = ( GENVAR .AND. .NOT. UVAR )

*  Set up a list of input NDF array components to be accessed and a
*  similar list of output NDF components.
      IF ( UVAR ) THEN
         COMP1 = 'Data,Variance'
      ELSE
         COMP1 = 'Data'
      ENDIF
      IF ( GVAR .OR. EVAR ) THEN
         COMP2 = 'Data,Variance'
      ELSE
         COMP2 = 'Data'
      END IF

*  Merge the numeric types of the input NDFs to determine which data
*  type to use for processing. If input NDFs are to be modified, then
*  take account of any input variance information, regardless of the
*  UVAR and GVAR settings.
      IF ( MODIFY .AND. ADJUST ) THEN
         CALL NDF_MTYPN( '_REAL,_DOUBLE', NIN, NDF, 'Data,Variance',
     :                   ITYPE1, DTYPE1, STATUS )
      ELSE
          CALL NDF_MTYPN( '_REAL,_DOUBLE', NIN, NDF, COMP1, ITYPE1,
     :                    DTYPE1, STATUS )
      END IF

*  If input variance information is not being used to weight the input
*  data, then loop to set up reciprocal weighting factors for each
*  input NDF in the VARS array.
      IF ( .NOT. UVAR ) THEN
         DO 2 I = 1, NIN

*  If user-supplied weights have been supplied, then use those.
            IF ( USEWT ) THEN
               VARS( I ) = 1.0 / WEIGHT( I )

*  Otherwise, use the scale factor correction to be applied, if
*  available.
            ELSE IF ( ADJUST ) THEN
               VARS( I ) = SCALE( I )

*  If there's no alternative, use weights of unity.
            ELSE
               VARS( I ) = 1.0D0
            END IF
 2       CONTINUE
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Decide how to partition the output NDF.
*  ======================================
*  Find the first dimension of the output NDF which has a size greater
*  than unity. We call this dimension size a "line", ignoring any
*  earlier dimensions which may have unit size.
      DO 3 IFIRST = 1, NDIMX - 1
         IF ( UBNDX( IFIRST ) .GT. LBNDX( IFIRST ) ) GO TO 4
 3    CONTINUE
 4    CONTINUE

*  If IFIRST is not the last output dimension, then estimate the
*  optimum number of "lines" to process at once. This is derived from
*  the average input NDF extent in the next dimension. One eighth of
*  the resulting average extent is used, this being an estimate of the
*  typical extent of overlap between NDFs when forming mosaics (the
*  exact result is far from critical; erring on the low side seems to
*  give best performance).
      OPTLIN = 1
      IF ( IFIRST .LT. NDIMX ) THEN
         OPTLIN = 0
         DO 5 I = 1, NIN
            OPTLIN = OPTLIN + ( UBND( IFIRST + 1, I ) -
     :                          LBND( IFIRST + 1, I ) + 1 )
 5       CONTINUE
         OPTLIN = MAX( 1, OPTLIN / ( NIN * 8 ) )
      END IF

*  Calculate the maximum number of work arrays that might be needed to
*  form the mosaic. From this, determine the maximum number of pixels
*  that can be processed in each NDF chunk so as to use the optimum
*  number of "lines" of data at a time, subject to not exceeding the
*  overall limit on the number of pixels in memory at once. (Take
*  account of the worst case, where all the input NDFs contribute to
*  the whole of a chunk.)
      NARY = 2 * NIN + 2
      IF ( UVAR ) NARY = NARY + ( 2 * NIN )
      IF ( GVAR .OR. EVAR ) NARY = NARY + 2
      MXPIX = MIN( MAX( 1, MXPIX0 / NARY ),
     :             OPTLIN * ( UBNDX( IFIRST ) - LBNDX( IFIRST ) + 1 ) )

*  Loop to partition the output NDF into chunks.
*  ============================================
*  Divide the output NDF into chunks of contiguous pixels for
*  processing.
      CALL NDF_NCHNK( NDFOUT, MXPIX, NCHUNK, STATUS )
      OUT1 = .TRUE.
      DO 10 ICHUNK = 1, NCHUNK

*  Begin an NDF context for each chunk and select the chunk.
         CALL NDF_BEGIN
         CALL NDF_CHUNK( NDFOUT, MXPIX, ICHUNK, NDFC, STATUS )

*  Determine the shape of the chunk and save its bounds in the first
*  significant dimension (dimension IFIRST).
         CALL NDF_BOUND( NDFC, NDF__MXDIM, LBNDC, UBNDC, NDIMC, STATUS )
         LBC = LBNDC( IFIRST )
         UBC = UBNDC( IFIRST )

*  Determine how many "lines" of output data there are in the chunk.
         CALL NDF_SIZE( NDFC, EL, STATUS )
         NLINES = MAX( 1, EL / ( UBC - LBC + 1 ) )
         IF ( STATUS .NE. SAI__OK ) GO TO 98

*  Identify and access NDF sections contributing to each chunk.
*  ===========================================================
*  Consider each input NDF to see if it intersects with this chunk (and
*  hence contributes to the output) and determine the number of pixels
*  in common.
         NIC = 0
         DO 6 I = 1, NIN
            CALL CCD1_OVLAP( NDF( I ), NDFC, PIC, STATUS )

*  Count the number of NDFs which contribute and form a list of their
*  indices, so they may be re-accessed.
            IF ( PIC .NE. 0 ) THEN
               NIC = NIC + 1
               LIST( 1, NIC ) = I

*  Select a section from each contributing NDF which matches the bounds
*  of the current chunk in all except the first significant dimension
*  (IFIRST). Allow the section to retain its original bounds in this
*  dimension, subject to not exceeding the chunk bounds. (This process
*  selects just those pixels which contribute to the results for this
*  chunk.)
               LBNDC( IFIRST ) = MAX( LBND( IFIRST, I ), LBC )
               UBNDC( IFIRST ) = MIN( UBND( IFIRST, I ), UBC )
               CALL NDF_SECT( NDF( I ), NDIMC, LBNDC, UBNDC, INDF,
     :                        STATUS )

*  Store the lower and upper bounds of the first significant dimension
*  (IFIRST) for the contributing part of each input NDF in an overall
*  list. This will later be used to partition this dimension. (Note
*  that a value of 1 is subtracted from the lower bound here and added
*  back later to allow the bounds to be sorted correctly.)
               IBOUND( 2 * NIC - 1 ) = LBNDC( IFIRST ) - 1
               IBOUND( 2 * NIC ) = UBNDC( IFIRST )

*  If the "input" NDFs are being modified, then check to see if there
*  is a variance array present in the current input NDF (if not known
*  already).
               VAR = ( NVAR .EQ. NIN )
               IF ( MODIFY .AND. ADJUST .AND. ( .NOT. VAR ) ) THEN
                  CALL NDF_STATE( INDF, 'Variance', VAR, STATUS )
               END IF

*  If the input NDFs are being modified, then obtain the current bad
*  pixel flag value for the data array of the associated base NDF.
*  This flag will be appropriate to the "output" array values (i.e.
*  those parts of the array which have already been processed).
*  Substitute the original bad pixel flag value appropriate to the
*  "input" (unprocessed) parts of the array before the array is mapped.
               IF ( MODIFY .AND. ADJUST ) THEN
                  CALL NDF_BASE( NDF( I ), NBASE, STATUS )
                  CALL NDF_BAD( NBASE, 'Data', .FALSE., BDOUTD, STATUS )
                  CALL NDF_SBAD( BADIN( 1, I ), NBASE, 'Data', STATUS )

*  Repeat this process for the variance array.
                  CALL NDF_BAD( NBASE, 'Variance', .FALSE., BDOUTV,
     :                          STATUS )
                  CALL NDF_SBAD( BADIN( 2, I ), NBASE, 'Variance',
     :                           STATUS )
               END IF

*  Map the NDF section, saving the pointer(s) for use later.  Use an
*  appropriate access mode and ensure that if the input NDF is to be
*  modified and it contains a variance array, then this is also mapped.
               IF ( MODIFY .AND. ADJUST .AND. VAR ) THEN
                  CALL NDF_MAP( INDF, 'Data,Variance', ITYPE1, ACMODE,
     :                          PNTR( 1, I ), EL, STATUS )
               ELSE
                  CALL NDF_MAP( INDF, COMP1, ITYPE1, ACMODE,
     :                          PNTR( 1, I ), EL, STATUS )
               END IF

*  Determine whether bad pixels may be present in the mapped section
*  array(s).
               CALL NDF_BAD( INDF, 'Data', .FALSE., BAD( 1, I ),
     :                       STATUS )
               CALL NDF_BAD( INDF, 'Variance', .FALSE., BAD( 2, I ),
     :                       STATUS )

*  Calculate the number of pixels in this section which do not lie
*  inside the bounds of the input NDF. This gives the minimum number of
*  bad pixel values we expect to find in the mapped arrays (there may
*  actually be more than this).
               MINBAD = EL - PIC

*  Modify the "input" NDFs if required.
*  ===================================
*  If the input NDFs are to be modified, then restore the "output" bad
*  pixel flag values for the data and variance arrays of the associated
*  base NDF and annul the base NDF identifier.
               IF ( MODIFY .AND. ADJUST ) THEN
                  CALL NDF_SBAD( BDOUTD, NBASE, 'Data', STATUS )
                  CALL NDF_SBAD( BDOUTV, NBASE, 'Variance', STATUS )
                  CALL NDF_ANNUL( NBASE, STATUS )

*  Apply the scale factor and zero point corrections to the mapped
*  values.
                  BADCHK = ( BAD( 1, I ) .OR.
     :                       ( VAR .AND. BAD( 2, I ) ) )
                  CALL CCD1_CRDAT( ITYPE1, BADCHK, VAR, EL, SCALE( I ),
     :                             DSCALE( I ), ZERO( I ), DZERO( I ),
     :                             ORIGIN( I ), PNTR( 1, I ),
     :                             PNTR( 2, I ), NBADD, NBADV, STATUS )

*  Update the bad pixel flags for the mapped arrays.
                  BAD( 1, I ) = ( NBADD .GT. 0 )
                  BAD( 2, I ) = ( NBADV .GT. 0 )

*  If the number of bad pixel values generated exceeds that expected,
*  then bad pixels must now be present in the input NDF, so set its bad
*  pixel flags accordingly.
                  CALL NDF_SBAD( ( NBADD .GT. MINBAD ), INDF,
     :                           'Data', STATUS )
                  CALL NDF_SBAD( ( NBADV .GT. MINBAD ), INDF,
     :                           'Variance', STATUS )
               END IF
            END IF
            IF ( STATUS .NE. SAI__OK ) GO TO 98
 6       CONTINUE

*  Add the overall first dimension bounds of the output chunk to the
*  list of bounds to ensure that the entire output chunk is covered.
         IBOUND( 2 * NIC + 1 ) = LBC - 1
         IBOUND( 2 * NIC + 2 ) = UBC

*  Partition the current chunk along dimension IFIRST.
*  ==================================================
*  Only perform the following if an output NDF is being generated.
         IF ( DOOUT ) THEN

*  If the number of contributing input NDFs for the current output
*  chunk is zero, then map it, initialising it to the bad-pixel value,
*  then unmap it. Set bad pixel flag values indicating that this chunk
*  now contains bad pixels.
            IF ( NIC .EQ. 0 ) THEN
               CALL NDF_MAP( NDFC, COMP2, ITYPE1, 'WRITE/BAD', PNTR2,
     :                       EL, STATUS )
               CALL NDF_UNMAP( NDFC, COMP2, STATUS )
               BDOUTD = .TRUE.
               BDOUTV = .TRUE.

*  Otherwise, map it for writing without initialisation.
            ELSE
               CALL NDF_MAP( NDFC, COMP2, ITYPE1, 'WRITE', PNTR2, EL,
     :                       STATUS )

*  Sort the IFIRST dimension limits of the input NDFs contributing to
*  this chunk into ascending order. Set default bad pixel flag values
*  for the current chunk and loop through each interval to partition
*  the chunk along this dimension.
               IFAIL = 1
               CALL PDA_QSAI( 2 * ( NIC + 1 ), IBOUND )
               BDOUTD = .FALSE.
               BDOUTV = .FALSE.
               DO 9 INTVL = 2, 2 * ( NIC + 1 )

*  Determine the lower and upper bounds of the current interval,
*  ignoring intervals with zero extent.
                  ISTART = IBOUND( INTVL - 1 ) + 1
                  IEND = IBOUND( INTVL )
                  IF ( IEND .GE. ISTART ) THEN

*  Identify and access data contributing to each dimension interval.
*  ================================================================
*  Loop to to inspect those input NDFs which contribute to the current
*  output chunk. Test each to determine if it contributes within the
*  current dimension interval. Count the number which do and form a
*  list of them.
                     NII = 0
                     DO 7 I = 1, NIC
                        II = LIST( 1, I )
                        IF ( ( LBND( IFIRST, II ) .LE. IEND ) .AND.
     :                       ( UBND( IFIRST, II ) .GE. ISTART ) ) THEN
                           NII = NII + 1
                           LIST( 2, NII ) = II
                        END IF
 7                   CONTINUE

*  If no input NDFs can contribute to this interval, then simply fill
*  the appropriate region of the output array(s) with the bad pixel
*  value.  Set bad pixel flag values to indicate this.
                     IF ( NII .EQ. 0 ) THEN
                        CALL CCD1_PTBAD( ITYPE1, LBC, UBC, NLINES,
     :                                   ISTART, IEND, PNTR2( 1 ),
     :                                   STATUS )
                        IF ( GVAR .OR. EVAR )
     :                     CALL CCD1_PTBAD( ITYPE1, LBC, UBC, NLINES,
     :                                      ISTART, IEND, PNTR2( 2 ),
     :                                      STATUS )
                        BDOUTD = .TRUE.
                        BDOUTV = .TRUE.

*  If only one input NDF contributes and MINPIX is 1, then copy the
*  input values directly to the appropriate part of the output array(s),
*  including a scale factor and/or zero point adjustment if required and
*  not already applied to the input values.
                     ELSE IF ( NII .EQ. 1 .AND. MINPIX .EQ. 1 ) THEN
                        II = LIST( 2, 1 )
                        CALL CCD1_MVDAT( ITYPE1,
     :                     ( ADJUST .AND. ( .NOT. MODIFY ) ),
     :                     BAD( 1, II ), .FALSE.,
     :                     MAX( LBND( IFIRST, II ), LBC ),
     :                     MIN( UBND( IFIRST, II ), UBC ), NLINES,
     :                     PNTR( 1, II ), PNTR( 2, II ), ISTART,
     :                     IEND, 1, SCALE( II ), DSCALE( II ),
     :                     ZERO( II ), DZERO( II ), ORIGIN( II ), LBC,
     :                     UBC, PNTR2( 1 ), NBADD, STATUS )
                        IF ( GVAR ) CALL CCD1_MVDAT( ITYPE1,
     :                     ( ADJUST .AND. ( .NOT. MODIFY ) ),
     :                     BAD( 2, II ), .TRUE.,
     :                     MAX( LBND( IFIRST, II ), LBC ),
     :                     MIN( UBND( IFIRST, II ), UBC ), NLINES,
     :                     PNTR( 1, II ), PNTR( 2, II ), ISTART,
     :                     IEND, 1, SCALE( II ), DSCALE( II ),
     :                     ZERO( II ), DZERO( II ), ORIGIN( II ), LBC,
     :                     UBC, PNTR2( 2 ), NBADV, STATUS )

*  If creating variances and input NDF has none then set bad.
                        IF ( EVAR .AND. .NOT. GVAR )
     :                     CALL CCD1_PTBAD( ITYPE1, LBC, UBC,
     :                                      NLINES, ISTART, IEND,
     :                                      PNTR2( 2 ),STATUS )


*  Note if the output chunk now contains bad pixels.
                        BDOUTD = ( BDOUTD .OR. ( NBADD .GT. 0 ) )
                        BDOUTV = ( BDOUTV .OR. ( NBADV .GT. 0 ) )

*  If more than one input NDF contributes to this interval, then
*  allocate a stack large enough to hold copies of all the contributing
*  input data for this interval. Also allocate a stack for variance
*  values if required. (Note we use PSX_ routines here since the memory
*  requirements are limited by the value of MXPIX0.)
                     ELSE
                        EL = NLINES * ( IEND - ISTART + 1 )
                        CALL PSX_CALLOC( EL * NII, ITYPE1, DATSTK,
     :                                   STATUS )
                        IF ( UVAR ) CALL PSX_CALLOC( EL * NII, ITYPE1,
     :                                               VARSTK, STATUS )

*  Loop to process the contributing NDFs.
                        DO 8 I = 1, NII
                           II = LIST( 2, I )

*  Copy the contributing part of each input array into the input
*  processing buffer, applying scale factor and/or zero point
*  adjustments if required and not already applied to the input values.
                           CALL CCD1_MVDAT( ITYPE1,
     :                        ( ADJUST .AND. ( .NOT. MODIFY ) ),
     :                        BAD( 1, II ), .FALSE.,
     :                        MAX( LBND( IFIRST, II ), LBC ),
     :                        MIN( UBND( IFIRST, II ), UBC ), NLINES,
     :                        PNTR( 1, II ), PNTR( 2, II ), ISTART,
     :                        IEND, I, SCALE( II ), DSCALE( II ),
     :                        ZERO( II ), DZERO( II ), ORIGIN( II ),
     :                        ISTART, IEND, DATSTK, NBADD, STATUS )
                           IF ( UVAR ) THEN
                              CALL CCD1_MVDAT( ITYPE1,
     :                        ( ADJUST .AND. ( .NOT. MODIFY ) ),
     :                        BAD( 2, II ), .TRUE.,
     :                        MAX( LBND( IFIRST, II ), LBC ),
     :                        MIN( UBND( IFIRST, II ), UBC ), NLINES,
     :                        PNTR( 1, II ), PNTR( 2, II ), ISTART,
     :                        IEND, I, SCALE( II ), DSCALE( II ),
     :                        ZERO( II ), DZERO( II ), ORIGIN( II ),
     :                        ISTART, IEND, VARSTK, NBADV, STATUS )
                           ELSE

*  Set up an array of variances indexed by line using the array of
*  variances indexed by NDF.
                              VARLIN( I ) = VARS( II )
                           END IF
                           IF ( STATUS .NE. SAI__OK ) GO TO 98
 8                      CONTINUE

*  Combine input data for the current interval.
*  ===========================================
*  Allocate workspace for combining the input data.
                        CALL PSX_CALLOC( EL, ITYPE1, RESDAT, STATUS )
                        CALL PSX_CALLOC( EL, ITYPE1, RESVAR, STATUS )
                        CALL PSX_CALLOC( NII, ITYPE1, WRK1, STATUS )
                        CALL PSX_CALLOC( NII, ITYPE1, WRK2, STATUS )
                        IF ( UVAR ) THEN
                           CALL PSX_CALLOC( NII, '_DOUBLE', WRK3,
     :                                      STATUS )
                           NWRK4 = MAX( 1, ( ( NII + 1 )**3 ) / 2 )
                           CALL PSX_CALLOC( NWRK4, '_DOUBLE', WRK4,
     :                                      STATUS )
                           NWRK4 = MAX( 1, NWRK4 / NII )
                        END IF
                        CALL PSX_CALLOC( NII, '_DOUBLE', WRK5, STATUS )
                        CALL PSX_CALLOC( NII, '_INTEGER', WRK6,
     :                                   STATUS )
                        CALL PSX_CALLOC( NII, '_LOGICAL', WRK7, STATUS )

*  Call the appropriate combining routine, taking account of the
*  processing data type and whether variance values are to be used to
*  weight the input data.

*  _REAL processing, no variances.
                        IF ( ITYPE1 .EQ. '_REAL' ) THEN
                           IF ( .NOT. UVAR ) THEN
                              CALL CCG1_CM3RR(
     :   %VAL( CNF_PVAL( DATSTK ) ), EL, NII,
     :                                  VARLIN, IMETH, MINPIX, NITER,
     :                                  NSIGMA, ALPHA, RMIN, RMAX,
     :                                  %VAL( CNF_PVAL( RESDAT ) ),
     :                                  %VAL( CNF_PVAL( WRK1 ) ),
     :                                  %VAL( CNF_PVAL( WRK2 ) ),
     :                                  %VAL( CNF_PVAL( WRK5 ) ),
     :                                  %VAL( CNF_PVAL( WRK6 ) ),
     :                                  %VAL( CNF_PVAL( WRK7 ) ),
     :                                  STATUS )

*  _REAL processing, with variances, variances always generated.
                           ELSE
                              CALL CCG1_CM1RR(
     :   %VAL( CNF_PVAL( DATSTK ) ), EL,
     :                                  NII, %VAL( CNF_PVAL( VARSTK ) ),
     :                                  IMETH,
     :                                  MINPIX, NITER, NSIGMA, ALPHA,
     :                                  RMIN, RMAX,
     :                                  %VAL( CNF_PVAL( RESDAT ) ),
     :                                  %VAL( CNF_PVAL( RESVAR ) ),
     :                                  %VAL( CNF_PVAL( WRK1 ) ),
     :                                  %VAL( CNF_PVAL( WRK2 ) ),
     :                                  %VAL( CNF_PVAL( WRK3 ) ),
     :                                  %VAL( CNF_PVAL( WRK4 ) ), NWRK4,
     :                                  %VAL( CNF_PVAL( WRK5 ) ),
     :                                  %VAL( CNF_PVAL( WRK6 ) ),
     :                                  %VAL( CNF_PVAL( WRK7 ) ),
     :                                  STATUS )
                           END IF

*  _DOUBLE processing, no variances.
                        ELSE IF ( ITYPE1 .EQ. '_DOUBLE' ) THEN
                           IF ( .NOT. UVAR ) THEN
                              CALL CCG1_CM3DD(
     :   %VAL( CNF_PVAL( DATSTK ) ), EL, NII,
     :                                  VARLIN, IMETH, MINPIX, NITER,
     :                                  NSIGMA, ALPHA, RMIN, RMAX,
     :                                  %VAL( CNF_PVAL( RESDAT ) ),
     :                                  %VAL( CNF_PVAL( WRK1 ) ),
     :                                  %VAL( CNF_PVAL( WRK2 ) ),
     :                                  %VAL( CNF_PVAL( WRK5 ) ),
     :                                  %VAL( CNF_PVAL( WRK6 ) ),
     :                                  %VAL( CNF_PVAL( WRK7 ) ),
     :                                  STATUS )
*  _DOUBLE processing, with variances.
                           ELSE
                              CALL CCG1_CM1DD(
     :   %VAL( CNF_PVAL( DATSTK ) ), EL,
     :                                  NII, %VAL( CNF_PVAL( VARSTK ) ),
     :                                  IMETH,
     :                                  MINPIX, NITER, NSIGMA, ALPHA,
     :                                  RMIN, RMAX,
     :                                  %VAL( CNF_PVAL( RESDAT ) ),
     :                                  %VAL( CNF_PVAL( RESVAR ) ),
     :                                  %VAL( CNF_PVAL( WRK1 ) ),
     :                                  %VAL( CNF_PVAL( WRK2 ) ),
     :                                  %VAL( CNF_PVAL( WRK3 ) ),
     :                                  %VAL( CNF_PVAL( WRK4 ) ), NWRK4,
     :                                  %VAL( CNF_PVAL( WRK5 ) ),
     :                                  %VAL( CNF_PVAL( WRK6 ) ),
     :                                  %VAL( CNF_PVAL( WRK7 ) ),
     :                                  STATUS )
                           END IF
                        END IF

*  Generate estimated variances, if required.
                        IF ( EVAR ) THEN
                           IF ( ITYPE1 .EQ. '_REAL' ) THEN
                              CALL CCG1_EVARR(
     :   %VAL( CNF_PVAL( RESDAT )),
     :
     :   %VAL( CNF_PVAL( DATSTK ) ),
     :                                         EL, NII,
     :
     :   %VAL( CNF_PVAL( RESVAR ) ),
     :                                         STATUS )
                           ELSE IF ( ITYPE1 .EQ. '_DOUBLE' ) THEN
                              CALL CCG1_EVARD(
     :   %VAL( CNF_PVAL( RESDAT )),
     :
     :   %VAL( CNF_PVAL( DATSTK ) ),
     :                                         EL, NII,
     :
     :   %VAL( CNF_PVAL( RESVAR ) ),
     :                                         STATUS )
                           END IF
                        END IF


*  Release the workspace.
                        CALL PSX_FREE( DATSTK, STATUS )
                        IF ( UVAR ) CALL PSX_FREE( VARSTK, STATUS )
                        CALL PSX_FREE( WRK1, STATUS )
                        CALL PSX_FREE( WRK2, STATUS )
                        IF ( UVAR ) THEN
                           CALL PSX_FREE( WRK3, STATUS )
                           CALL PSX_FREE( WRK4, STATUS )
                        END IF
                        CALL PSX_FREE( WRK5, STATUS )
                        CALL PSX_FREE( WRK6, STATUS )
                        CALL PSX_FREE( WRK7, STATUS )

*  After combined results have been obtained for each interval, copy
*  them into the appropriate part of the mapped output array(s). No
*  further corrections are required at this point.  (N.B. We have to
*  assume bad pixels may be present at this point because the data
*  combination routines do not return information about this.)
                        CALL CCD1_MVDAT( ITYPE1, .FALSE., .TRUE.,
     :                     .FALSE., ISTART, IEND, NLINES, RESDAT,
     :                     RESVAR , ISTART, IEND, 1, 1.0D0, 0.0D0,
     :                     0.0D0, 0.0D0, 0.0D0, LBC, UBC, PNTR2( 1 ),
     :                     NBADD, STATUS )
                        IF ( GVAR .OR. EVAR )
     :                     CALL CCD1_MVDAT( ITYPE1, .FALSE.,
     :                     .TRUE., .TRUE., ISTART, IEND, NLINES,
     :                     RESDAT, RESVAR, ISTART, IEND, 1, 1.0D0,
     :                     0.0D0, 0.0D0, 0.0D0, 0.0D0, LBC, UBC,
     :                     PNTR2( 2 ), NBADV, STATUS )

*  Note if the output chunk now contains bad pixels.
                        BDOUTD = ( BDOUTD .OR. ( NBADD .GT. 0 ) )
                        BDOUTV = ( BDOUTV .OR. ( NBADV .GT. 0 ) )

*  De-allocate workspace used for processing the current interval.
                        CALL PSX_FREE( RESDAT, STATUS )
                        CALL PSX_FREE( RESVAR, STATUS )
                     END IF
                  END IF
                  IF ( STATUS .NE. SAI__OK ) GO TO 98
 9             CONTINUE
            END IF

*  Handle the output bad pixel flag(s).
*  ===================================
*  After assigning pixel values to each output chunk, set its bad pixel
*  flag(s) appropriately (while it is still mapped).
            CALL NDF_SBAD( BDOUTD, NDFC, 'Data', STATUS )
            IF ( GVAR ) CALL NDF_SBAD( BDOUTV, NDFC, 'Variance',
     :                                 STATUS )

*  If this is the first output chunk to be written, then unmap it,
*  causing the values assigned to be written back to the data
*  structure.
            IF ( OUT1 .AND. NIC .NE. 0 ) THEN
               OUT1 = .FALSE.
               CALL NDF_UNMAP( NDFC, COMP2, STATUS )

*  If no bad pixels were assigned to either of its arrays, then check
*  explicitly to determine whether there are any bad pixels in them now
*  (this is to detect any possible data conversion errors during
*  unmapping).
               IF ( .NOT. BDOUTD )
     :            CALL NDF_BAD( NDFC, 'Data', .TRUE., BDOUTD, STATUS )
               IF ( GVAR .AND. ( .NOT. BDOUTV ) )
     :            CALL NDF_BAD( NDFC, 'Variance', .TRUE., BDOUTV,
     :                          STATUS )

*  After unmapping the first output chunk, set the bad pixel flag(s)
*  for the whole of the output NDF appropriately (this cannot be done
*  earlier because the output arrays have an undefined state until this
*  point). Assignments made to subsequent output chunks will now
*  accumulate the correct bad pixel flag value(s) for the output NDF as
*  a whole.
               CALL NDF_SBAD( BDOUTD, NDFOUT, 'Data', STATUS )
               IF ( GVAR ) CALL NDF_SBAD( BDOUTV, NDFOUT, 'Variance',
     :                                    STATUS )
            END IF
         END IF

*  Clean up.
*  ========
*  Arrive here if an error occurs within this NDF context.
 98      CONTINUE

*  End the NDF context, cleaning up after processing each output chunk.
         CALL NDF_END( STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 99
 10   CONTINUE

*  Arrive here if any other error occurs.
 99   END
* $Id$
