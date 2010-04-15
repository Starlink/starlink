      SUBROUTINE SPD_CAAA( NDFCUB, ROW, SPVXST, VARXST, COVRSX,
     :   PTRCX, PTRCD, PTRCV, PTRCC, PTRSX, PTRSD, PTRSV, PTRSC,
     :   SNELM, FIRST, STATUS )
*+
*  Name:
*     SPD_CAAA

*  Purpose:
*     Locate spectrum in cube.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_CAAA( NDFCUB, ROW, SPVXST, VARXST, COVRSX,
*        PTRCX, PTRCD, PTRCV, PTRCC, PTRSX, PTRSD, PTRSV, PTRSC,
*        SNELM, FIRST, STATUS )

*  Description:
*     This routine locates a spectrum (a row) in N-dimensional arrays of
*     an NDF.

*  Arguments:
*     NDFCUB = INTEGER (Given)
*        The identifier of the cube NDF. Its bounds are taken to be the
*        array bounds.
*     ROW( 2 : NDF__MXDIM ) = INTEGER (Given)
*        The row specification. ROW(2), ROW(3) ... ROW(NDF__MXDIM) are
*        taken to be the position of the row within the NDF bounds of
*        the second, third, ... NDF__MXDIM-th axis.
*     SPVXST = INTEGER (Given)
*        Non-zero if the x array is actually of the same N-D shape as
*        the data array. Zero if the x array actually is a vector common
*        to all rows of the data array.
*     VARXST = INTEGER (Given)
*        Non-zero if there is a variance array.
*     COVRSX = INTEGER (Given)
*        Non-zero if there is an array of covariance row sums.
*     PTRCX = INTEGER (Given)
*        The pointer to the x array or x vector.
*     PTRCD = INTEGER (Given)
*        The pointer to the data array.
*     PTRCV = INTEGER (Given)
*        The pointer to the variance array, if available.
*     PTRCC = INTEGER (Given)
*        The pointer to the array of covariance row sums, if available.
*     PTRSX = INTEGER (Returned)
*        The pointer to the x vector.
*     PTRSD = INTEGER (Returned)
*        The pointer to the data vector.
*     PTRSV = INTEGER (Returned)
*        The pointer to the variance vector, if available.
*     PTRSC = INTEGER (Returned)
*        The pointer to the vector of covariance row sums, if available.
*     FIRST = INTEGER (Returned)
*        If the array is treated as 1-D, then ARRAY(FIRST) is the same
*        as VECTOR(1).
*     SNELM = INTEGER (Returned)
*        The length of the vector.
*     STATUS = INTEGER (Given and Returned)
*        The global status. Status is set and a report made if FIRST
*        turns out to be less than 1, or greater than the size of the
*        NDF, or if the row specification lies outside the NDF bounds.

*  Note:
*     Degenerate first axes in the NDF are ignored. That is, rows are
*     presumed to extend along the first axis that is longer than one
*     pixel.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     22 Apr 1994 (hme):
*        Original version.
*     06 May 1994 (hme):
*        Return more information than just FIRST.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! Standard PAR constants

*  Arguments Given:
      INTEGER NDFCUB
      INTEGER ROW( 2 : NDF__MXDIM )
      INTEGER SPVXST
      INTEGER VARXST
      INTEGER COVRSX
      INTEGER PTRCX, PTRCD, PTRCV, PTRCC

*  Arguments Returned:
      INTEGER PTRSX, PTRSD, PTRSV, PTRSC
      INTEGER SNELM
      INTEGER FIRST

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER REALSZ             ! Bytes per real number
      PARAMETER ( REALSZ = 4 )

*  Local Variables:
      LOGICAL ERROR              ! Keep track of row outside bounds
      INTEGER I, J               ! Temporary integers
      INTEGER INCR               ! Array index increment
      INTEGER NDIM               ! NDF dimensionality
      INTEGER NELM               ! NDF size
      INTEGER DIM(  NDF__MXDIM ) ! NDF dimensions
      INTEGER LBND( NDF__MXDIM ) ! NDF lower bounds
      INTEGER UBND( NDF__MXDIM ) ! NDF upper bounds

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Default return values.
      FIRST = 1
      SNELM = 1

*  Get the NDF's bounds, dimensions, and size.
      CALL NDF_BOUND( NDFCUB, NDF__MXDIM, LBND, UBND, NDIM, STATUS )
      CALL NDF_DIM(   NDFCUB, NDF__MXDIM, DIM,        NDIM, STATUS )
      CALL NDF_SIZE(  NDFCUB, NELM, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  Correct bounds for leading degenerate axes.
*  If the first few axes are of length 1, then we want to ignore them.
*  for ( i=1; dim(i)<=1; i++ );
*  if ( i>1 )
*  {  for ( j=1; i<=NDF__MXDIM; i++, j++ )
*     { dim(j)=dim(i); lbnd(j)=lbnd(i); ubnd(j)=ubnd(i); }
*     for ( ; j<=NDF__MXDIM; j++ ) { dim(j)=1; lbnd(j)=1; ubnd(j)=1; }
*  }
      I = 1
 1    CONTINUE                        ! Start of 'DO WHILE' loop
      IF ( I .LE. NDF__MXDIM ) THEN
         IF ( DIM(I) .LE. 1 ) THEN
            I = I + 1
            GO TO 1
         END IF
      END IF
      J = 1
 2    CONTINUE                        ! Start of 'DO WHILE' loop
      IF ( I .GT. 1 ) THEN
         IF ( I .LE. NDF__MXDIM ) THEN
            DIM(J)  = DIM(I)
            LBND(J) = LBND(I)
            UBND(J) = UBND(I)
            I = I + 1
            J = J + 1
            GO TO 2
         END IF
 3       CONTINUE                        ! Start of 'DO WHILE' loop
         IF ( J .LE. NDF__MXDIM ) THEN
            DIM(J)  = 1
            LBND(J) = 1
            UBND(J) = 1
            J = J + 1
            GO TO 3
         END IF
      END IF

*  Spectrum length.
      SNELM = DIM(1)

*  Apply lower bounds to row specification.
*  The given row specification counts from the lower bound, below we
*  count from 1.
*  Build up the index offset.
*  Check that row is within bounds.
      ERROR = .FALSE.
      INCR  = 1
      FIRST = 1
      DO 4 I = 2, NDF__MXDIM
         INCR  = INCR  * DIM(I-1)
         FIRST = FIRST + ( ROW(I) - LBND(I) ) * INCR
         IF ( ROW(I) .LT. LBND(I) .OR. ROW(I) .GT. UBND(I) )
     :      ERROR = .TRUE.
 4    CONTINUE

*  Check the returned index offset against 1 and the NDF size.
      IF ( ERROR .OR. FIRST .LT. 1 .OR. FIRST .GT. NELM ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPD_CAAA_E01', 'SPD_CAAA: Error: ' //
     :      'The row to be located is outside the array.', STATUS )
         GO TO 500
      END IF

*  Work out the spectral pointers by applying offset to cube pointers.
      PTRSX = PTRCX
      IF ( SPVXST .NE. 0 ) PTRSX = PTRCX + REALSZ * ( FIRST - 1 )
      PTRSD = PTRCD + REALSZ * ( FIRST - 1 )
      PTRSV = PTRCV
      IF ( VARXST .NE. 0 ) PTRSV = PTRCV + REALSZ * ( FIRST - 1 )
      PTRSC = PTRCC
      IF ( COVRSX .NE. 0 ) PTRSC = PTRCC + REALSZ * ( FIRST - 1 )

*  Return.
 500  CONTINUE
      END
