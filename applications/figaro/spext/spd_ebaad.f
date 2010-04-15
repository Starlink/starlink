      SUBROUTINE SPD_EBAAD( MAXDIM, SPAXIS, ODIM, INELM, ONELM,
     :   VECTOR, ARRAY, STATUS )
*+
*  Name:
*     SPD_EBAA{DR}

*  Purpose:
*     Grow vector to array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_EBAAD( MAXDIM, SPAXIS, ODIM, INELM, ONELM,
*        VECTOR, ARRAY, STATUS )

*  Description:
*     This routine is a simplified version of SPABHD, which can grow
*     an N-M-dimensional array several times into part of an
*     N-dimensional array. The input is one-dimensional and is copied
*     several times to fill the whole output array. The vector extends
*     along the specified axis of the output array.
*
*     This simplified routine exists mainly to tidy up the call tree. It
*     is to be used by SPD_E* routines to grow the main NDF's AXIS
*     centres or widths into the Specdre Extension's SPECVALS or
*     SPECWIDS. It is really a routine internal to the SPEXT library or
*     Specdre.

*  Arguments:
*     MAXDIM = INTEGER (Given)
*        The maximum no. of axes supported by calling routine. Must be
*        equal to corresponding local value, which is 7.
*     SPAXIS = INTEGER (Given)
*        The number of the spectroscopic axis. The vector will extend
*        along this axis in the array.
*     ODIM( MAXDIM ) = INTEGER (Given)
*        7-D dimensions in target data.
*     INELM = INTEGER (Given)
*        Size of the source data, its declared length.
*     ONELM = INTEGER (Given)
*        Total size of the target data, its declared 1-D length.
*     VECTOR( INELM ) = DOUBLE PRECISION (Given)
*        Source data vector.
*     ARRAY( ONELM ) = DOUBLE PRECISION (Returned)
*        Target data array. It is declared one-dimensional, it is
*        formally seven-dimensional, but some trailing dimensions may be
*        degenerate (of length 1).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     The routine loops through the 7-D target data ARRAY, since this
*     is larger than the source data VECTOR. The array indices are
*     N1 ... Ni ... N7. However we use 1-D arrays for addressing the
*     array elements, the indices being IIND and OIND for source and
*     target respectively. OIND increases always by 1 as we loop through
*     ARRAY. IIND could be calculated from:
*
*     IIND =   MIN(N1,LDIM(1))
*            + SUM{i=2,MAXDIM}
*                 [ (MIN(Ni,LDIM(i)) - 1) * PROD{j=1,i-1} [LDIM(j)] ]
*
*     LDIM is the 7-D size of VECTOR, its values are 1 for all axes
*     except LDIM(SPAXIS)=INELM=ODIM(SPAXIS). Thus the MIN functions
*     take values of 1 (grown) or Ni (retained).
*
*     It is faster to increment IIND instead of recalculating it. For
*     this, we work out the increments necessary when any index Ni is
*     increased by 1. These increments are:
*
*     IINC(1) = 0 for collapsed axis
*               1 for retained axis
*
*     IINC(i) =   ( MIN(Ni+1,LDIM(i)) - MIN(Ni,LDIM(i)) )
*               * PROD{j=1,i-1} [LDIM(j)]
*
*             = 0                       for collapsed axis
*               PROD{j=1,i-1} [LDIM(j)] for retained axis

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     08 Mar 1991 (hme):
*        Original, adapted form EXTRCR (GROWRL).
*     26 Mar 1991 (hme):
*        Enable copying of IN into only part of OUT.
*     11 Apr 1991 (hme):
*        Update the prologue.
*     13 Mar 1992 (hme):
*        Adapted from GROWRL to SPABH{DR}. Reduced from 10-D to 7-D.
*     24 Feb 1994 (hme):
*        Simplify SPABHD for 1-D input that fills the whole output
*        array.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER MAXDIM, INELM, ONELM
      INTEGER SPAXIS
      INTEGER ODIM( MAXDIM )
      DOUBLE PRECISION VECTOR( INELM )

*  Arguments Returned:
      DOUBLE PRECISION ARRAY( ONELM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local constants:
      INTEGER LMXDIM
      PARAMETER ( LMXDIM = 7 )

*  Local Variables:
      INTEGER IINC( LMXDIM )     ! IIND-increments
      INTEGER OINC( LMXDIM )     ! OIND-increments
      INTEGER LENGTH( LMXDIM )   ! Size of target area
      INTEGER IIND, OIND         ! Index for IN and OUT
      INTEGER N1, N2, N3, N4, N5, N6, N7

*.

*  Check status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check MAXDIM.
      IF ( MAXDIM .NE. LMXDIM  ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPABH_MXDIM', 'SPD_EBAAD: Error: ' //
     :      'Incompatible number of supported dimensions.', STATUS )
         GO TO 500
      ENDIF

*  Check ODIM(SPAXIS).
      IF ( ODIM(SPAXIS) .NE. INELM ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPD_EBAA_E01', 'SPD_EBAAD: Error: Vector' //
     :      'length does not match length of spectroscopic axis.',
     :      STATUS )
         GO TO 500
      ENDIF

*  IIND increments, preliminary values. OIND increments.
      IINC(1) = 1
      OINC(1) = 1
      DO 2 N1 = 2, LMXDIM
         IF ( N1 .EQ. SPAXIS+1 ) THEN
            IINC(N1) = IINC(N1-1) * INELM
         ELSE
            IINC(N1) = IINC(N1-1)
         END IF
         OINC(N1) = OINC(N1-1) * ODIM(N1-1)
 2    CONTINUE

*  IIND increments, actual values.
*  IIND and OIND start values.
*  LENGTH of target area.
      IIND = 1
      OIND = 1
      DO 3 N1 = 1, LMXDIM
         IF ( N1 .NE. SPAXIS ) IINC(N1) = 0
         LENGTH(N1) = ODIM(N1)
 3    CONTINUE

*  7-dimensional loop through target area of ARRAY to copy data.
      DO 10 N7 = 1, ODIM(7)
         DO  9 N6 = 1, ODIM(6)
            DO  8 N5 = 1, ODIM(5)
               DO  7 N4 = 1, ODIM(4)
                  DO  6 N3 = 1, ODIM(3)
                     DO  5 N2 = 1, ODIM(2)
                        DO  4 N1 = 1, ODIM(1)
                           ARRAY(OIND) = VECTOR(IIND)
                           OIND = OIND + OINC(1)
                           IIND = IIND + IINC(1)
    4                   CONTINUE
                        OIND = OIND - LENGTH(1) * OINC(1) + OINC(2)
                        IIND = IIND - LENGTH(1) * IINC(1) + IINC(2)
    5                CONTINUE
                     OIND = OIND - LENGTH(2) * OINC(2) + OINC(3)
                     IIND = IIND - LENGTH(2) * IINC(2) + IINC(3)
    6             CONTINUE
                  OIND = OIND - LENGTH(3) * OINC(3) + OINC(4)
                  IIND = IIND - LENGTH(3) * IINC(3) + IINC(4)
    7          CONTINUE
               OIND = OIND - LENGTH(4) * OINC(4) + OINC(5)
               IIND = IIND - LENGTH(4) * IINC(4) + IINC(5)
    8       CONTINUE
            OIND = OIND - LENGTH(5) * OINC(5) + OINC(6)
            IIND = IIND - LENGTH(5) * IINC(5) + IINC(6)
    9    CONTINUE
         OIND = OIND - LENGTH(6) * OINC(6) + OINC(7)
         IIND = IIND - LENGTH(6) * IINC(6) + IINC(7)
   10 CONTINUE

*  That's all.
  500 CONTINUE

      END
