      SUBROUTINE CCD1_DOCMP( GETS, GETZ, VAR, SKYSUP, TOLS, TOLZ,
     :                       MAXIT, NNDF, NDF, IREF, NCMP, IPAIR, NPIX,
     :                       DIFS, DDIFS, DIFZ, DDIFZ, ORIGIN, NCMP1,
     :                       STATUS )
*+
*  Name:
*     CCD1_DOCMP

*  Purpose:
*     Inter-compare NDFs to determine scale factor and/or zero point
*     differences.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_DOCMP( GETS, GETZ, VAR, SKYSUP, TOLS, TOLZ, MAXIT,
*                      NNDF, NDF, IREF, NCMP, IPAIR, NPIX, DIFS, DDIFS,
*                      DIFZ, DDIFZ, ORIGIN, NCMP1, STATUS )

*  Description:
*     This routine is invoked by the MAKEMOS application to run the
*     CCD1_CMPRx routines repeatedly to inter-compare a series of NDF
*     data arrays to determine their scale factor and/or zero point
*     differences. Its purpose is to handle workspace allocation, NDF
*     access, message output, error conditions and other housekeeping
*     tasks for CCD1_CMPRx.

*  Arguments:
*     GETS = LOGICAL (Given)
*        Whether estimates of scale factor differences are required.
*     GETZ = LOGICAL (Given)
*        Whether estimates of zero point differences are required.
*     VAR = LOGICAL (Given)
*        Whether variance information (if available in the NDFs
*        supplied) should be used to weight the data during the
*        inter-comparison process.
*     SKYSUP = REAL (Given)
*        Sky noise suppression factor (passed to CCD1_CMPRx).
*     TOLS = REAL (Given)
*        Accuracy tolerance for scale factor estimates (passed to
*        CCD1_CMPRx).
*     TOLZ = REAL (Given)
*        Accuracy tolerance for zero point estimates (passed to
*        CCD1_CMPRx).
*     MAXIT = INTEGER (Given)
*        Maximum number of iterations to be used in the
*        inter-comparison process (passed to CCD1_CMPRx).
*     NNDF = INTEGER (Given)
*        Number of NDFs (should be at least 2).
*     NDF( NNDF ) = INTEGER (Given)
*        Array of identifiers for the NDFs.
*     IREF = INTEGER (Given)
*        Index in the NDF array of the "reference NDF" (this is simply
*        used in message output and does not affect processing). A
*        value of zero should be given if there is no reference NDF.
*     NCMP = INTEGER (Given)
*        Number of inter-comparisons to be made.
*     IPAIR( 2, NCMP ) = INTEGER (Given and Returned)
*        On entry this should contain a list of pairs of indices (into
*        the NDF array) identifying which NDFs are to be used in each
*        inter-comparison. Each NDF supplied should appear at least
*        once in this list. On exit, this list is updated to omit any
*        index pairs for inter-comparisons which failed. The new number
*        of entries is returned via the NCMP1 argument.
*     NPIX( NCMP ) = INTEGER (Given and Returned)
*        On entry this array should contain the number of overlapping
*        pixels involved in each of the inter-comparisons specified in
*        the IPAIR array. On exit, this list is updated to omit any
*        elements for inter-comparisons which failed. The new number of
*        entries is returned via the NCMP1 argument.
*     DIFS( NCMP ) = DOUBLE PRECISION (Returned)
*        Array of scale factor differences (unity is returned if GETS
*        is .FALSE.).
*     DDIFS( NCMP ) = DOUBLE PRECISION (Returned)
*        Array of error estimates on the scale factor differences (zero
*        is returned if GETS is .FALSE.).
*     DIFZ( NCMP ) = DOUBLE PRECISION (Returned)
*        Array of zero point differences (zero is returned if GETZ is
*        .FALSE.).
*     DDIFZ( NCMP ) = DOUBLE PRECISION (Returned)
*        Array of error estimates on the zero point differences (zero
*        is returned if GETZ is .FALSE.).
*     ORIGIN( NCMP ) = DOUBLE PRECISION (Returned)
*        Array of false origin values used in the inter-comparisons.
*        These are chosen to minimise the inter-dependence of the scale
*        factor and zero point errors (see CCD1_CMPRx for further
*        details).
*     NCMP1 = INTEGER (Returned)
*        Number of successful inter-comparisons. This gives the number
*        of valid values returned in the IPAIR, NPIX, DIFS, DDIFS,
*        DIFZ, DDIFZ and ORIGIN arrays. An error status will be
*        returned if this is not at least equal to NNDF-1.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

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
*     {enter_new_authors_here}

*  History:
*     31-JUL-1992 (RFWS):
*        Original version.
*     3-AUG-1992 (RFWS):
*        Improved formatting of results and error reporting.
*     20-OCT-1992 (PDRAPER):
*        Changed calls to CCDPACK version 0.1.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS/DAT constants
      INCLUDE 'NDF_PAR'          ! NDF public constants
      INCLUDE 'USER_ERR'         ! General purpose "user" error codes
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      LOGICAL GETS
      LOGICAL GETZ
      LOGICAL VAR
      REAL SKYSUP
      REAL TOLS
      REAL TOLZ
      INTEGER MAXIT
      INTEGER NNDF
      INTEGER NDF( NNDF )
      INTEGER IREF
      INTEGER NCMP

*  Arguments Given and Returned:
      INTEGER IPAIR( 2, NCMP )
      INTEGER NPIX( NCMP )

*  Arguments Returned:
      DOUBLE PRECISION DIFS( NCMP )
      DOUBLE PRECISION DDIFS( NCMP )
      DOUBLE PRECISION DIFZ( NCMP )
      DOUBLE PRECISION DDIFZ( NCMP )
      DOUBLE PRECISION ORIGIN( NCMP )
      INTEGER NCMP1

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 12 ) TXT     ! Buffer for formatting text
      CHARACTER * ( 13 ) COMP    ! NDF component list
      CHARACTER * ( DAT__SZLOC ) LCWRK1 ! Workspace locator
      CHARACTER * ( DAT__SZLOC ) LCWRK2 ! Workspace locator
      CHARACTER * ( DAT__SZLOC ) LCWRK3 ! Workspace locator
      CHARACTER * ( DAT__SZLOC ) LCWRK4 ! Workspace locator
      CHARACTER * ( NDF__SZFTP ) DTYPE ! Data type for storage (junk)
      CHARACTER * ( NDF__SZTYP ) ITYPE ! Data type for processing
      DOUBLE PRECISION DS        ! Final scale factor change
      DOUBLE PRECISION DZ        ! Final zero point change
      INTEGER CSTAT              ! Inter-comparison status return
      INTEGER EL                 ! Number of mapped array elements
      INTEGER ELWRK1( 1 )        ! Number of workspace elements
      INTEGER ELWRK2( 1 )        ! Number of workspace elements
      INTEGER ELWRK3( 1 )        ! Number of workspace elements
      INTEGER ELWRK4( 1 )        ! Number of workspace elements
      INTEGER I                  ! Index of first compared NDF
      INTEGER ICMP               ! Loop counter for inter-comparisons
      INTEGER IDIM               ! Loop counter for dimensions
      INTEGER IGNORE             ! I/O error status (ignored)
      INTEGER ITER               ! Number of iterations used
      INTEGER J                  ! Index of second compared NDF
      INTEGER LBND( NDF__MXDIM ) ! Lower bounds of overlap region
      INTEGER NDFI               ! ID for first NDF section to compare
      INTEGER NDFJ               ! ID for second NDF section to compare
      INTEGER NDIM               ! No. dimensions of overlap region
      INTEGER NGOOD              ! Number of good pixels used
      INTEGER PNTRI( 2 )         ! Pointers to mapped NDF arrays
      INTEGER PNTRJ( 2 )         ! Pointers to mapped NDF arrays
      INTEGER UBND( NDF__MXDIM ) ! Upper bounds of overlap region
      INTEGER WRK1               ! Pointer to workspace
      INTEGER WRK2               ! Pointer to workspace
      INTEGER WRK3               ! Pointer to workspace
      INTEGER WRK4               ! Pointer to workspace
      LOGICAL BAD                ! Bad pixels may be present?
      LOGICAL VARC               ! Use variance during inter-comparison?

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Display a heading.
      CALL CCD1_MSG( ' ', ' ', STATUS )
      CALL CCD1_MSG( ' ', '   Inter-comparing NDFs in pairs:', STATUS )
      CALL CCD1_MSG( ' ', '   -----------------------------', STATUS )

*  Acquire workspace.
*  =================
*  Initialise counts of the number of elements required in each
*  workspace array. Then loop to consider each inter-comparison,
*  calculating the amount of workspace required in each array.
      ELWRK1( 1 ) = 0
      ELWRK2( 1 ) = 0
      ELWRK3( 1 ) = 0
      ELWRK4( 1 ) = 0
      DO 1 ICMP = 1, NCMP

*  Identify the two NDFs contributing to each inter-comparison.
         I = IPAIR( 1, ICMP )
         J = IPAIR( 2, ICMP )

*  Determine if variance values should (and can) be used to weight the
*  data arrays being inter-compared.
         VARC = .FALSE.
         IF ( VAR ) THEN
            CALL NDF_STATE( NDF( I ), 'Variance', VARC, STATUS )
            IF ( VARC ) THEN
               CALL NDF_STATE( NDF( J ), 'Variance', VARC, STATUS )
            END IF
         END IF

*  Maximise the workspace requirements in each array over all
*  inter-comparisons. Take account of arrays which may not actually be
*  used in any one inter-comparison (the conditions governing these
*  workspace requirements are dictated by the CCD1_CMPRx routines).
         IF ( GETS .AND. GETZ ) THEN
            ELWRK1( 1 )= MAX( ELWRK1( 1 ), 3 * NPIX( ICMP ) )
         ELSE
            ELWRK1( 1 ) = MAX( ELWRK1( 1 ), NPIX( ICMP ) )
         END IF
         ELWRK2( 1 ) = MAX( ELWRK2( 1 ), NPIX( ICMP ) )
         IF ( VARC .AND. GETS .AND. ( SKYSUP .GT. 0.0 ) ) THEN
            ELWRK3( 1 ) = MAX( ELWRK3( 1 ), 2 * NPIX( ICMP ) )
            ELWRK4( 1 ) = MAX( ELWRK4( 1 ), NPIX( ICMP ) )
         ELSE IF ( VARC .OR.
     :             ( GETS .AND. ( SKYSUP .GT. 0.0 ) ) ) THEN
            ELWRK3( 1 ) = MAX( ELWRK3( 1 ), NPIX( ICMP ) )
         END IF
 1    CONTINUE

*  Match the numeric types of all the input NDFs to determine the
*  floating point data type to be used for processing the "worst case"
*  intercomparison.
      IF ( VAR ) THEN
         CALL NDF_MTYPN( '_REAL,_DOUBLE', NNDF, NDF, 'Data,Variance',
     :                   ITYPE, DTYPE, STATUS )
      ELSE
         CALL NDF_MTYPN( '_REAL,_DOUBLE', NNDF, NDF, 'Data', ITYPE,
     :                   DTYPE, STATUS )
      END IF

*  Allocate those workspace arrays which are required (note we use HDS
*  to provide this workspace because the requirements may be large).
      IF ( ELWRK1( 1 ) .GT. 0 ) THEN
         CALL AIF_TEMP( '_INTEGER', 1, ELWRK1, LCWRK1, STATUS )
         CALL DAT_MAP( LCWRK1, '_INTEGER', 'WRITE', 1, ELWRK1, WRK1,
     :                 STATUS )
      ELSE
         WRK1 = 0
      END IF
      IF ( ELWRK2( 1 ) .GT. 0 ) THEN
         CALL AIF_TEMP( ITYPE, 1, ELWRK2, LCWRK2, STATUS )
         CALL DAT_MAP( LCWRK2, ITYPE, 'WRITE', 1, ELWRK2, WRK2, STATUS )
      ELSE
         WRK2 = 0
      END IF
      IF ( ELWRK3( 1 ) .GT. 0 ) THEN
         CALL AIF_TEMP( ITYPE, 1, ELWRK3, LCWRK3, STATUS )
         CALL DAT_MAP( LCWRK3, ITYPE, 'WRITE', 1, ELWRK3, WRK3, STATUS )
      ELSE
         WRK3 = 0
      END IF
      IF ( ELWRK4( 1 ) .GT. 0 ) THEN
         CALL AIF_TEMP( ITYPE, 1, ELWRK4, LCWRK4, STATUS )
         CALL DAT_MAP( LCWRK4, ITYPE, 'WRITE', 1, ELWRK4, WRK4, STATUS )
      ELSE
         WRK4 = 0
      END IF

*  Perform the inter-comparisons.
*  =============================
*  Loop through each possible inter-comparison, counting the number
*  which are successful in NCMP1.
      NCMP1 = 0
      DO 3 ICMP = 1, NCMP
         NCMP1 = NCMP1 + 1

*  Identify the two NDFs contributing to each inter-comparison.
         I = IPAIR( 1, ICMP )
         J = IPAIR( 2, ICMP )

*  Display a message indicating which pair of NDFs is being
*  inter-compared.
         CALL CCD1_MSG( ' ', ' ', STATUS )
         CALL MSG_SETI( 'ICMP', ICMP )
         CALL MSG_SETI( 'NCMP', NCMP )
         CALL MSG_SETI( 'I', I )
         CALL MSG_SETI( 'J', J )
         CALL CCD1_MSG( ' ',
     : '   --> ^ICMP (of ^NCMP): Inter-comparing NDFs ^I and ^J:',
     :                 STATUS )

*  Display the NDF names.
         CALL CCD1_MSG( ' ', ' ', STATUS )
         CALL NDF_MSG( 'NDFI', NDF( I ) )
         IF ( I .EQ. IREF ) CALL MSG_SETC( 'NDFI', ' (reference NDF)' )
         CALL CCD1_MSG( ' ', '         Datasets: a) ^NDFI', STATUS )
         CALL NDF_MSG( 'NDFJ', NDF( J ) )
         IF ( J .EQ. IREF ) CALL MSG_SETC( 'NDFJ', ' (reference NDF)' )
         CALL CCD1_MSG( ' ', '                   b) ^NDFJ', STATUS )
         CALL CCD1_MSG( ' ', ' ', STATUS )

*  Begin a new NDF context and clone the identifiers of the NDFs to be
*  compared. Match their bounds by trimming (i.e. selecting only the
*  pixels they have in common).
         CALL NDF_BEGIN
         CALL NDF_CLONE( NDF( I ), NDFI, STATUS )
         CALL NDF_CLONE( NDF( J ), NDFJ, STATUS )
         CALL NDF_MBND( 'TRIM', NDFI, NDFJ, STATUS )

*  Obtain and format the pixel index bounds of the overlap region.
         CALL NDF_BOUND( NDFI, NDF__MXDIM, LBND, UBND, NDIM, STATUS )
         DO 2 IDIM = 1, NDIM
            IF ( IDIM .NE. 1 ) CALL MSG_SETC( 'BOUNDS', ',' )
            CALL MSG_SETI( 'BOUNDS', LBND( IDIM ) )
            CALL MSG_SETC( 'BOUNDS', ':' )
            CALL MSG_SETI( 'BOUNDS', UBND( IDIM ) )
 2       CONTINUE

*  Display the bounds of the overlap region along with the number of
*  pixels it contains.
         CALL CCD1_MSG( ' ',
     : '         Pixel bounds of overlap region:  (^BOUNDS)', STATUS )
         CALL MSG_SETI( 'NPIX', NPIX( ICMP ) )
         CALL CCD1_MSG( ' ',
     : '         Number of pixels in overlap:     ^NPIX', STATUS )

*  Determine if variance values should (and can) be used to weight the
*  data arrays being inter-compared.
         VARC = .FALSE.
         IF ( VAR ) THEN
            CALL NDF_STATE( NDFI, 'Variance', VARC, STATUS )
            IF ( VARC ) THEN
               CALL NDF_STATE( NDFJ, 'Variance', VARC, STATUS )
            END IF
         END IF

*  Indicate whether variance weighting is in use.
         CALL MSG_SETL( 'VARC', VARC )
         CALL CCD1_MSG( ' ',
     : '         Variance weighting in use:       ^VARC', STATUS )

*  Set up a list of components to be used in the inter-comparison.
         IF ( VARC ) THEN
            COMP = 'Data,Variance'
         ELSE
            COMP = 'Data'
         END IF

*  Match the numeric types of the array components of these two NDFs to
*  determine the floating point data type to be used for
*  inter-comparing them.
         CALL NDF_MTYPE( '_REAL,_DOUBLE',
     :                   NDFI, NDFJ, COMP, ITYPE, DTYPE, STATUS )

*  Map each NDF section for reading using the appropriate floating
*  point data type and determine if it is necessary to check for the
*  presence of bad pixel values.
         PNTRI( 1 ) = 0
         PNTRI( 2 ) = 0
         PNTRJ( 1 ) = 0
         PNTRJ( 2 ) = 0
         CALL NDF_MAP( NDFI, COMP, ITYPE, 'READ', PNTRI, EL, STATUS )
         CALL NDF_MAP( NDFJ, COMP, ITYPE, 'READ', PNTRJ, EL, STATUS )
         CALL NDF_MBAD( .TRUE., NDFI, NDFJ, COMP, .FALSE., BAD, STATUS )

*  If OK so far, mark the error stack and inter-compare the NDFs' data
*  arrays, using the appropriate numeric type.
         IF ( STATUS .EQ. SAI__OK ) THEN
            CALL ERR_MARK
            IF ( ITYPE .EQ. '_REAL' ) THEN
               CALL CCD1_CMPRR( BAD, VARC, EL,
     :                          %VAL( CNF_PVAL( PNTRJ( 1 ) ) ),
     :                          %VAL( CNF_PVAL( PNTRJ( 2 ) ) ),
     :                          %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                          %VAL( CNF_PVAL( PNTRI( 2 ) ) ),
     :                          GETS, GETZ, TOLS, TOLZ, MAXIT, SKYSUP,
     :                          DIFS( NCMP1 ), DDIFS( NCMP1 ),
     :                          DIFZ( NCMP1 ), DDIFZ( NCMP1 ),
     :                          ORIGIN( NCMP1 ), NGOOD, ITER, DS, DZ,
     :                          %VAL( CNF_PVAL( WRK1 ) ),
     :                          %VAL( CNF_PVAL( WRK2 ) ),
     :                          %VAL( CNF_PVAL( WRK3 ) ),
     :                          %VAL( CNF_PVAL( WRK4 ) ), STATUS )
            ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
               CALL CCD1_CMPRD( BAD, VARC, EL,
     :                          %VAL( CNF_PVAL( PNTRJ( 1 ) ) ),
     :                          %VAL( CNF_PVAL( PNTRJ( 2 ) ) ),
     :                          %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                          %VAL( CNF_PVAL( PNTRI( 2 ) ) ),
     :                          GETS, GETZ, TOLS, TOLZ, MAXIT, SKYSUP,
     :                          DIFS( NCMP1 ), DDIFS( NCMP1 ),
     :                          DIFZ( NCMP1 ), DDIFZ( NCMP1 ),
     :                          ORIGIN( NCMP1 ), NGOOD, ITER, DS, DZ,
     :                          %VAL( CNF_PVAL( WRK1 ) ),
     :                          %VAL( CNF_PVAL( WRK2 ) ),
     :                          %VAL( CNF_PVAL( WRK3 ) ),
     :                          %VAL( CNF_PVAL( WRK4 ) ), STATUS )
            END IF

*  If there were insufficient good data values, or an iterative fit
*  failed to converge, then annul the error, but note the original
*  status value. Release the error stack.
            CSTAT = STATUS
            IF ( ( STATUS .EQ. USER__001 ) .OR.
     :           ( STATUS .EQ. USER__002 ) ) THEN
               CALL ERR_ANNUL( STATUS )
            END IF
            CALL ERR_RLSE
         END IF

*  If results have been omitted previously because of inter-comparisons
*  which failed, then move other inter-comparison information up to
*  eliminate the omitted entries, thus maintaining correspondence with
*  the results arrays.
         IF ( STATUS .EQ. SAI__OK ) THEN
            IF ( NCMP1 .LT. ICMP ) THEN
               IPAIR( 1, NCMP1 ) = IPAIR( 1, ICMP )
               IPAIR( 2, NCMP1 ) = IPAIR( 2, ICMP )
               NPIX( NCMP1 ) = NPIX( ICMP )
            END IF

*  Display the results of each inter-comparison.
*  ============================================
*  Display the number of (good) pixels actually used.
            CALL MSG_SETI( 'NGOOD', NGOOD )
            WRITE( TXT( : 5 ), '(F5.1)' ) 100.0 * REAL( NGOOD ) /
     :                                            REAL( NPIX( NCMP1 ) )
            CALL CHR_LDBLK( TXT( : 5 ) )
            CALL MSG_SETC( 'PCENT', TXT( : 5 ) )
            CALL CCD1_MSG( ' ',
     : '         Number of (good) pixels used:    ^NGOOD ' //
     : '(^PCENT% of overlap)', STATUS )

*  If there were insufficient good pixels, then report this fact and
*  discard the results of this inter-comparison.
            IF ( CSTAT .EQ. USER__001 ) THEN
               CALL CCD1_MSG( ' ', ' ', STATUS )
               CALL CCD1_MSG( ' ',
     : '     *** WARNING: Insufficient good pixels to obtain a ' //
     : 'satisfactory fit', STATUS )
               CALL CCD1_MSG( ' ',
     : '                  These results will not be used', STATUS )
               NCMP1 = NCMP1 - 1

*  Otherwise, show the number of iterations used, if appropriate.
            ELSE
               IF ( ITER .GT. 0 ) THEN
                  CALL MSG_SETI( 'ITER', ITER )
                  IF ( ITER .EQ. MAXIT )
     :               CALL MSG_SETC( 'ITER', ' (maximum allowed)' )
                  CALL CCD1_MSG( ' ',
     : '         Number of iterations used:       ^ITER', STATUS )
               END IF

*  Display the scale factor result and its error.
               CALL CCD1_MSG( ' ', ' ', STATUS )
               IF ( GETS ) THEN
                  WRITE( TXT( : 12 ), '(G12.5)', IOSTAT = IGNORE )
     :               SNGL( DIFS( NCMP1 ) )
                  CALL CHR_LDBLK( TXT( : 12 ) )
                  CALL MSG_SETC( 'SCALE', TXT( : 12 ) )
                  WRITE( TXT( : 9 ), '(G9.2)', IOSTAT = IGNORE )
     :               SNGL( DDIFS( NCMP1 ) )
                  CALL CHR_LDBLK( TXT( : 9 ) )
                  CALL MSG_SETC( 'DSCALE', TXT( : 9 ) )
                  CALL CCD1_MSG( ' ',
     : '         Relative scale-factor S (dS):    ^SCALE (^DSCALE)',
     :               STATUS  )
               END IF

*  Display the zero point result and its error (correcting for the
*  false origin for the purposes of display).
               IF ( GETZ) THEN
                  WRITE( TXT( : 12 ), '(G12.5)', IOSTAT = IGNORE )
     :              SNGL( DIFZ( NCMP1 ) -
     :                    DIFS( NCMP1 ) * ORIGIN( NCMP1 ) )
                  CALL CHR_LDBLK( TXT( : 12 ) )
                  CALL MSG_SETC( 'ZERO', TXT( : 12 ) )
                  WRITE( TXT( : 9 ), '(G9.2)', IOSTAT = IGNORE )
     :               SNGL( DDIFZ( NCMP1 ) )
                  CALL CHR_LDBLK( TXT( : 9 ) )
                  CALL MSG_SETC( 'DZERO', TXT( : 9 ) )
                  CALL CCD1_MSG( ' ',
     : '         Zero-point shift Z (dZ):         ^ZERO (^DZERO)',
     :               STATUS )
               END IF

*  If the scale factor estimate is not positive, then display a warning
*  message and discard the results.
               IF ( DIFS( NCMP1 ) .LE. 0.0D0 ) THEN
                  CALL CCD1_MSG( ' ', ' ', STATUS )
                  CALL CCD1_MSG( ' ',
     : '     *** WARNING: Scale-factor estimate is not positive',
     :                          STATUS )
                  CALL CCD1_MSG( ' ',
     : '                  These results will not be used', STATUS )
                  NCMP1 = NCMP1 - 1

*  If an iterative inter-comparison failed to converge, then display a
*  warning message.
               ELSE IF ( CSTAT .EQ. USER__002 ) THEN
                  CALL CCD1_MSG( ' ', ' ', STATUS )
                  CALL CCD1_MSG( ' ',
     : '     *** WARNING: Iterations failed to converge to the ' //
     : 'required accuracy', STATUS )
                  CALL CCD1_MSG( ' ',
     : '                  Check the results!!', STATUS )
                  CALL CCD1_MSG( ' ', ' ', STATUS )

*  Also display the amount by which each parameter changed in the final
*  iteration along with its associated tolerance.
                  IF ( GETS ) THEN
                     CALL MSG_SETR( 'DS', SNGL( DS ) )
                     CALL MSG_SETR( 'TOLS',
     :                        SNGL( ABS( TOLS * DIFS( NCMP1 ) ) ) )
                     CALL CCD1_MSG( ' ',
     : '         Final scale-factor change: ^DS (cf. ^TOLS required)',
     :                  STATUS )
                  END IF
                  IF ( GETZ ) THEN
                     CALL MSG_SETR( 'DZ', SNGL( DZ ) )
                     CALL MSG_SETR( 'TOLZ',
     :                        SNGL( ABS( TOLZ * DIFS( NCMP1 ) ) ) )
                     CALL CCD1_MSG( ' ',
     : '         Final zero-point change:   ^DZ (cf. ^TOLZ required)',
     :                             STATUS )
                  END IF
               END IF
            END IF
         END IF

*  Clean up after inter-comparing each pair of NDFs and quit the
*  inter-comparison loop if there has been an error.
         CALL NDF_END( STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 4
 3    CONTINUE
 4    CONTINUE

*  Release those workspace arrays which were allocated before the main
*  inter-comparison loop.
      IF ( ELWRK1( 1 ) .GT. 0 ) THEN
         CALL DAT_UNMAP( LCWRK1, STATUS )
         CALL AIF_ANTMP( LCWRK1, STATUS )
      END IF
      IF ( ELWRK2( 1 ) .GT. 0 ) THEN
         CALL DAT_UNMAP( LCWRK2, STATUS )
         CALL AIF_ANTMP( LCWRK2, STATUS )
      END IF
      IF ( ELWRK3( 1 ) .GT. 0 ) THEN
         CALL DAT_UNMAP( LCWRK3, STATUS )
         CALL AIF_ANTMP( LCWRK3, STATUS )
      END IF
      IF ( ELWRK4( 1 ) .GT. 0 ) THEN
         CALL DAT_UNMAP( LCWRK4, STATUS )
         CALL AIF_ANTMP( LCWRK4, STATUS )
      END IF

*  Report an error if no inter-comparisons succeeded.
      IF ( STATUS .EQ. SAI__OK ) THEN
         IF ( NCMP1 .EQ. 0 ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'CCD1_DOCMP_NONE',
     :                    'No successful inter-comparisons; cannot ' //
     :                    'determine the required ' //
     :                    'scale-factor/zero-point corrections.',
     :                    STATUS )

*  Also check that the minimum number of successful inter-comparisons
*  was achieved and report an error if not.
         ELSE IF ( NCMP1 .LT. ( NNDF - 1 ) ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'NCMP1', NCMP1 )
            CALL MSG_SETI( 'NNDF', NNDF )
            CALL ERR_REP( 'CCD1_DOCMP_2FEW',
     :                    'Only ^NCMP1 inter-comparison(s) ' //
     :                    'succeeded; not adequate to determine the ' //
     :                    'required scale-factor/zero-point ' //
     :                    'corrections for a set of ^NNDF NDFs.',
     :                    STATUS )
         ELSE

*  Display the number of successful inter-comparisons.
            CALL CCD1_MSG( ' ', ' ', STATUS )
            CALL MSG_SETI( 'NCMP1', NCMP1 )
            CALL CCD1_MSG( ' ',
     : '      Number of successful NDF inter-comparisons = ^NCMP1',
     :                    STATUS )
         END IF
      END IF

      END
* $Id$
