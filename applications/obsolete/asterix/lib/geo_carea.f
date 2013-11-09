*+  GEO_CAREA - Finds area of overlap of circle on a rectangle, exactly
      REAL FUNCTION GEO_CAREA( RAD, CX, CY, LOX, HIX, LOY, HIY )
*
*    Description :
*
*     Returns the area of the circle of radius RAD centred at (CX,CY)
*     which overlaps a rectangle defined by LOX,HIX,LOY and HIY. The
*     centre of the circle is assumed to lie within the rectangle.
*
*    Method
*
*     The area of overlap is found by subtracting off those areas excluded
*     by an edge. The left and right edges are imagined to continue to minus
*     and plus infinity for this purpose, and the top bottom edges from the
*     lhs to the rhs of the field. The formula SEG describes the area of the
*     segments to be cut off (the #'s below).
*
*             _________________ radius
*            /  |#########|    \
*      -----/-------------------\----- edge of rectangle, distance 'd'
*          /    A \  |  / B      \                        from centre
*                  \1|2/
*                   \|/  two angular arguments
*                    C
*
*    Deficiencies :
*    Bugs :
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     12 Sep 91 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'MATH_PAR'
*
*    Import :
*
      REAL                        RAD            ! Radius of circle
      REAL                        CX,CY          ! Centre of circle
      REAL                        LOX, HIX       ! Range of rectangle in X
      REAL                        LOY, HIY       ! Range of rectangle in Y
*
*    Local variables :
*
      REAL                        ANG1,ANG2      ! Angles to segment edges
      REAL                        AREA           ! The running area variable
      REAL                        D              !
      REAL                        LHS_D, RHS_D   ! Distance from edges to centre
      REAL                        BOT_D, TOP_D   ! Ditto
*
*    Inline functions :
*
      REAL                        SEG
      SEG(ANG1,ANG2,D) = RAD*RAD*(SIN(2.0*ANG2)+2.0*(ANG2-ANG1)
     :              -SIN(2.0*ANG1))/4.0 - RAD*(SIN(ANG2)-SIN(ANG1))*D
*-

*    The area of the whole circle
      AREA = MATH__PI*RAD*RAD

*    Find the distances from the centre to the edges
      BOT_D = CY - LOY
      TOP_D = HIY - CY
      LHS_D = CX - LOX
      RHS_D = HIX - CX

*    Does circle extend over any edge of the rectangle?
      IF ( (BOT_D.LT.RAD) .OR. (TOP_D.LT.RAD) .OR.
     :     (LHS_D.LT.RAD) .OR. (RHS_D.LT.RAD) ) THEN

*      Off the lhs?
        IF ( LHS_D .LT. RAD ) THEN
          ANG1 = ACOS(LHS_D/RAD)
          AREA = AREA - SEG( -ANG1, ANG1, LHS_D )
        END IF

*      Off the rhs?
        IF ( RHS_D .LT. RAD ) THEN
          ANG1 = ACOS(RHS_D/RAD)
          AREA = AREA - SEG( -ANG1, ANG1, RHS_D )
        END IF

*      Off the top?
        IF ( TOP_D .LT. RAD ) THEN
          D = SQRT( RAD**2 - TOP_D**2 )
          ANG1 = ASIN(MIN(D,LHS_D)/RAD)
          ANG2 = ASIN(MIN(D,RHS_D)/RAD)
          AREA = AREA - SEG( -ANG1, ANG2, TOP_D )
        END IF

*      Off the bottom?
        IF ( BOT_D .LT. RAD ) THEN
          D = SQRT( RAD**2 - BOT_D**2 )
          ANG1 = ASIN(MIN(D,LHS_D)/RAD)
          ANG2 = ASIN(MIN(D,RHS_D)/RAD)
          AREA = AREA - SEG( -ANG1, ANG2, BOT_D )
        END IF

      END IF

*    Set return value
      GEO_CAREA = AREA

      END
