      SUBROUTINE  CPSECR( IMAGE, DIM1, DIM2, X1, Y1, X2, Y2, RXDIM,
     :                      RYDIM, REGION, STATUS )
*
*    Description :
*
*     This routine copies the 2-D data of a specified region of an
*     image.
*
*    Invocation :
*
*     CALL  CPSECR( IMAGE, DIM1, DIM2, X1, Y1, X2, Y2, RXDIM, RYDIM,
*    :                REGION, STATUS )
*
*    Arguments :
*
*     IMAGE( DIM1, DIM2 ) = REAL( READ )
*         The array containing the raw image data.
*     DIM1 = INTEGER( READ )
*         The first dimension of the 2-d array.
*     DIM2 = INTEGER( READ )
*         The second dimension of the 2-d array.
*     X1 = INTEGER( READ )
*         The X co-ordinate of the first point from which the region
*           is determined.
*     Y1 = INTEGER( READ )
*         The Y co-ordinate of the first point.
*     X2 = INTEGER( READ )
*         The X co-ordinate of the second point.
*     Y2 = INTEGER( READ )
*         The Y co-ordinate of the second point.
*     RXDIM = INTEGER( READ )
*         The X-dimension of the region.
*     RYDIM = INTEGER( READ )
*         The Y-dimension of the region.
*     REGION( RXDIM, RYDIM ) = REAL( WRITE )
*         The array into which the region is copied.
*     STATUS = INTEGER( READ )
*         The status value on entry to this routine.
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     The maximum and minimum co-ordinates of the region are found.
*     The region is copied by a direct assignment of the data elements
*     concerned to the elements of the new array.
*
*    Authors :
*
*     S.Chan ( RGVAD::KFH )
*     Malcolm Currie RAL (UK.AC.RL.STAR::CUR)
*
*    History :
*
*     28 October 1983:
*     1986 Sep 22: Renamed from KFH_TRANSFER2. Standardised to RAPI2D
*                  style; renamed parameters section to arguments and
*                  added access; region limits now unaltered within
*                  routine; argument reordered (8 -> 10); made generic;
*                  removed tabs; relocated 'local' variables to import
*                  etc.; stripped of trailing blanks and tidied
*                  (RL.STAR::CUR).
*     1989 Aug  7: Passed array dimensions as separate variables
*                  (RL.STAR::CUR).
*
*    Type Definitions :
 
      IMPLICIT NONE
 
*    Global Constants :
 
      INCLUDE 'SAE_PAR'
 
*    Import :
 
      INTEGER
     :    RXDIM, RYDIM,
     :    DIM1, DIM2,
     :    X1, Y1,
     :    X2, Y2
 
      REAL IMAGE( DIM1, DIM2 )
 
*    Export :
 
      REAL REGION( RXDIM, RYDIM )
 
*    Status :
 
      INTEGER STATUS
 
*    Local variables :
 
      INTEGER
     :    I, J,                 ! General variables
     :    XX1, YY1,             ! Lower x,y bound of the region
     :    XX2, YY2              ! Upper x,y bound of the region
 
*-
 
*    If status value is bad, then return to the main program.
 
      IF ( STATUS .NE. SAI__OK ) GOTO 999
 
*    Copy input co-ordinates a pair are transposed
 
      XX1 = MIN( X1, X2 )
      YY1 = MIN( Y1, Y2 )
      XX2 = MAX( X1, X2 )
      YY2 = MAX( Y1, Y2 )
 
*    Copy the elements of the region.
 
      DO  I = YY1, YY2, 1
         DO  J = XX1, XX2, 1
 
            REGION( J - XX1 + 1, I - YY1 + 1 ) = IMAGE( J, I )
 
         END DO
      END DO
 
 999  CONTINUE
 
      END
