      SUBROUTINE SPD_UAAMR( INDAT, OUTDAT, STAPIX, ENDPIX,
     :   MAXDIM, LDIM, ODIM, INELM, ONELM, STATUS )
*+
*  Name:
*     SPD_UAAM{DR}

*  Purpose:
*     Grow action.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_UAAMR( INDAT, OUTDAT, STAPIX, ENDPIX,
*        MAXDIM, LDIM, ODIM, INELM, ONELM, STATUS )

*  Description:
*     This routine copies the source data several times into the target
*     data. Both are formally seven-dimensional. Actually the source
*     data have less dimensions than the target data. The (complete)
*     source are inserted as subsets (or slices) into (part of) the
*     target data.
*
*     For example, two-dimensional source data could be copied into
*     several two-dimensional slices of three-dimensional target data.
*     This would require two target axes to be equal in length the to
*     two source axes.
*
*     More specifically, the source data could have dimensions
*     (NX,1,NZ,1,1,1,1) and the target data could have dimensions
*     (NX,NY,NZ,1,1,1,1). The target area within the target data could
*     have start pixel (1,NY1,1,1,1,1,1) and end pixel
*     (NX,NY2,NZ,1,1,1,1). Then the source data would be copied
*     NY2-NY1+1 times into the xz-planes NY1 to NY2 of the target data.
*
*     In this example, the first and third axes are retained - or common
*     to source and target, and the second axis is grown - or expanded
*     from length 1 in the source to length > 1 in the target data. The
*     fourth to seventh axes are degenerate in both source and target,
*     but must formally be given as length 1 for this routine to run.
*
*     Note that any source axis is either retained or grown. Retained
*     axes must be of equal length in source and target data, grown axes
*     must be of length 1 in source data.

*  Arguments:
*     INDAT( INELM ) = REAL (Given)
*        Source data array. It is declared one-dimensional, it is
*        formally seven-dimensional, but several dimensions will be
*        degenerate (of length 1).
*     OUTDAT( ONELM ) = REAL (Returned)
*        Target data array. It is declared one-dimensional, it is
*        formally seven-dimensional, but some trailing dimensions may be
*        degenerate (of length 1).
*     STAPIX( MAXDIM ) = INTEGER (Given)
*        7-D start pixel of target area in target data. Must be 1 for
*        each retained axis. Must be greater than 0 and less than or
*        equal to ENDPIX for each axis.
*     ENDPIX( MAXDIM ) = INTEGER (Given)
*        7-D end pixel of target area in target data. Must be ODIM for
*        each retained axis. Must be greater than or equal to STAPIX and
*        less than or equal to ODIM for each axis.
*     MAXDIM = INTEGER (Given)
*        The maximum no. of axes supported by calling routine. Must be
*        equal to corresponding local value, which is 7.
*     LDIM( MAXDIM ) = INTEGER (Given)
*        7-D dimensions in source data. Must be 1 for grown axis and
*        ODIM for retained axis.
*     ODIM( MAXDIM ) = INTEGER (Given)
*        7-D dimensions in target data.
*     INELM = INTEGER (Given)
*        Total size of the source data, its declared 1-D length.
*     ONELM = INTEGER (Given)
*        Total size of the target data, its declared 1-D length.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     The routine loops through the 7-D target data OUTDAT, since this
*     is larger than the source data INDAT. The array indices are
*     N1 ... Ni ... N7. However we use 1-D arrays for addressing the
*     array elements, the indices being IIND and OIND for source and
*     target respectively. OIND increases always by 1 as we loop through
*     OUTDAT. IIND could be calculated from:
*
*     IIND =   MIN(N1,LDIM(1))
*            + SUM{i=2,MAXDIM}
*                 [ (MIN(Ni,LDIM(i)) - 1) * PROD{j=1,i-1} [LDIM(j)] ]
*
*     LDIM is the 7-D size of INDAT, its values are 1 for a grown axis
*     and ODIM for a retained axis. Thus the MIN functions take values
*     of 1 (grown) or Ni (retained).
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
*     25 Nov 1994 (hme):
*        Renamed from SPABHR.
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
      INTEGER STAPIX( MAXDIM )
      INTEGER ENDPIX( MAXDIM )
      INTEGER LDIM( MAXDIM )
      INTEGER ODIM( MAXDIM )
      REAL INDAT( INELM )

*  Arguments Returned:
      REAL OUTDAT( ONELM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local constants:
      INTEGER LMXDIM
      PARAMETER ( LMXDIM = 7 )

*  Local Variables:
      LOGICAL ERROR              ! Used to check dimensions etc.
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
         CALL ERR_REP( 'SPD_UAAM_MXDIM', 'SPD_UAAMR: Error: ' //
     :      'Incompatible number of supported dimensions.', STATUS )
         GO TO 500
      ENDIF

*  Check STAPIX, ENDPIX, LDIM.
      ERROR = .FALSE.
      DO 1 N1 = 1, LMXDIM
         IF ( STAPIX(N1) .LT. 1          ) ERROR = .TRUE.
         IF ( ENDPIX(N1) .LT. STAPIX(N1) ) ERROR = .TRUE.
         IF ( ODIM(N1)   .LT. ENDPIX(N1) ) ERROR = .TRUE.
         IF ( LDIM(N1) .NE. ODIM(N1) .AND. LDIM(N1) .NE. 1)
     :      ERROR = .TRUE.
         IF ( LDIM(N1) .EQ. ODIM(N1) .AND.
     :         ( STAPIX(N1) .NE. 1 .OR. ENDPIX(N1) .NE. ODIM(N1) ) )
     :      ERROR = .TRUE.
 1    CONTINUE
      IF ( ERROR ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPD_UAAM_INCON', 'SPD_UAAMR: Error: ' //
     :      'Inconsistent dimensions or invalid target area.', STATUS )
         GO TO 500
      ENDIF

*  IIND increments, preliminary values. OIND increments.
      IINC(1) = 1
      OINC(1) = 1
      DO 2 N1 = 2, LMXDIM
         IINC(N1) = IINC(N1-1) * LDIM(N1-1)
         OINC(N1) = OINC(N1-1) * ODIM(N1-1)
 2    CONTINUE

*  IIND increments, actual values.
*  IIND and OIND start values.
*  LENGTH of target area.
      IIND = 1
      OIND = 1
      DO 3 N1 = 1, LMXDIM
         IF ( LDIM(N1) .EQ. 1 ) IINC(N1) = 0
         OIND = OIND + ( STAPIX(N1) - 1 ) * OINC(N1)
         LENGTH(N1) = ENDPIX(N1) - STAPIX(N1) + 1
 3    CONTINUE

*  7-dimensional loop through target area of OUTDAT to copy data.
      DO 10 N7 = STAPIX(7), ENDPIX(7)
         DO  9 N6 = STAPIX(6), ENDPIX(6)
            DO  8 N5 = STAPIX(5), ENDPIX(5)
               DO  7 N4 = STAPIX(4), ENDPIX(4)
                  DO  6 N3 = STAPIX(3), ENDPIX(3)
                     DO  5 N2 = STAPIX(2), ENDPIX(2)
                        DO  4 N1 = STAPIX(1), ENDPIX(1)
                           OUTDAT(OIND) = INDAT(IIND)
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
