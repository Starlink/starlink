      SUBROUTINE ECH_GET_PROFILE(
     :           IMAGE,
     :           NX,
     :           NY,
     :           N_ORDERS,
     :           MAXIMUM_POLY,
     :           TRACE_POLYNOMIAL,
     :           INTERACTIVE,
     :           PROFILING_MODE,
     :           NX_FRACTION,
     :           PFL_SUBSAMPLES,
     :           MAX_SKY_PIXELS,
     :           DEK_THRESH,
     :           SKY_LOLIM,
     :           DEK_BELOW,
     :           DEK_ABOVE,
     :           PRO_BELOW,
     :           PRO_ABOVE,
     :           OBJ_MASK,
     :           SKY_MASK,
     :           SUBSAMPLED_PROFILE,
     :           X_TRACE_COORD,
     :           Y_TRACE_COORD,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_GET_PROFILE

*  Purpose:
*     Determines subsampled spatial profile of an echellogram.

*  Description:
*     This routine is a top-level task control routine which decides
*     whether single- or all-order profiling is required.
*     No other operation is done in this routine.
*     Arguments are passed through unchanged.

*  Authors:
*     MJC: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     06-SEP-1996 (MJC):
*       Initial version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Arguments:
      INTEGER NX
      INTEGER NY
      INTEGER N_ORDERS
      INTEGER MAXIMUM_POLY
      REAL IMAGE( NX, NY )
      REAL NX_FRACTION
      DOUBLE PRECISION TRACE_POLYNOMIAL( MAXIMUM_POLY, N_ORDERS )
      LOGICAL INTERACTIVE
      CHARACTER*( * ) PROFILING_MODE
      INTEGER DEK_BELOW( N_ORDERS )
      INTEGER DEK_ABOVE( N_ORDERS )
      REAL DEK_THRESH
      REAL SKY_LOLIM
      INTEGER MAX_SKY_PIXELS
      INTEGER PFL_SUBSAMPLES
      INTEGER PRO_BELOW
      INTEGER PRO_ABOVE
      INTEGER OBJ_MASK( -MAX_SKY_PIXELS/2: MAX_SKY_PIXELS/2, N_ORDERS )
      INTEGER SKY_MASK( -MAX_SKY_PIXELS/2: MAX_SKY_PIXELS/2, N_ORDERS )
      REAL SUBSAMPLED_PROFILE( -PFL_SUBSAMPLES/2 : PFL_SUBSAMPLES/2 )
      DOUBLE PRECISION X_TRACE_COORD( NX )
      DOUBLE PRECISION Y_TRACE_COORD( NX )

*  Status:
      INTEGER STATUS
*.
      IF ( NX_FRACTION .GE. 1.0 ) THEN
         CALL ECH_SPATIAL_PROFILE(
     :        IMAGE,
     :        NX,
     :        NY,
     :        N_ORDERS,
     :        MAXIMUM_POLY,
     :        TRACE_POLYNOMIAL,
     :        INTERACTIVE,
     :        PROFILING_MODE,
     :        NX_FRACTION,
     :        PFL_SUBSAMPLES,
     :        MAX_SKY_PIXELS,
     :        DEK_THRESH,
     :        SKY_LOLIM,
     :        DEK_BELOW,
     :        DEK_ABOVE,
     :        PRO_BELOW,
     :        PRO_ABOVE,
     :        OBJ_MASK,
     :        SKY_MASK,
     :        SUBSAMPLED_PROFILE,
     :        X_TRACE_COORD,
     :        Y_TRACE_COORD,
     :        STATUS
     :       )

      ELSE
         CALL ECH_SPATIAL_AVGPROF(
     :        IMAGE,
     :        NX,
     :        NY,
     :        N_ORDERS,
     :        MAXIMUM_POLY,
     :        TRACE_POLYNOMIAL,
     :        INTERACTIVE,
     :        PROFILING_MODE,
     :        NX_FRACTION,
     :        PFL_SUBSAMPLES,
     :        MAX_SKY_PIXELS,
     :        DEK_THRESH,
     :        SKY_LOLIM,
     :        DEK_BELOW,
     :        DEK_ABOVE,
     :        PRO_BELOW,
     :        PRO_ABOVE,
     :        OBJ_MASK,
     :        SKY_MASK,
     :        SUBSAMPLED_PROFILE,
     :        X_TRACE_COORD,
     :        Y_TRACE_COORD,
     :        STATUS
     :       )
      END IF

      END
