*+  SQSHS - sets up the squashing limits and weights for SQSHX and SQSHY

      SUBROUTINE SQSHS( INLEN, OUTLEN, RATIO, PIXELS, WEIGHT, STATUS )
*
*    Description :
*
*     Given the input array dimension, INLEN, and the output array
*     dimension, OUTLEN, the ratio of the two is returned in RATIO along
*     with two arrays, PIXELS, which contains the pixel limits in the
*     input array corresponding to each pixel in the output array, and
*     WEIGHT, which contains the weights corresponding to the limiting
*     pixels in PIXELS. The second dimension of these two arrays is used
*     to differentiate between the left (1) and right (2) limits. Each
*     pixel in the output array can be calculated from the input array
*     as follows :
*        OUTPUT<N> = [ ( WEIGHT(N,1) * INPUT<PIXEL(N,1)> ) +
*                        INPUT<PIXEL(N,1)+1> +
*                        .
*                        .
*                        INPUT<PIXEL(N,2)-1> +
*                      ( WEIGHT(N,2) * INPUT<PIXEL(N,2)> ) ] / RATIO
*     where the index inside the <> brakets indicates the index for the
*     dimension being squashed, ignoring any other dimensions, i.e. the
*     sum of the values of interior pixels (not at a boundary) plus the
*     weighted values of the limiting pixels .
*     An immediate return will occur if STATUS has an error value on
*     entry.
*
*    Invocation :
*
*     CALL SQSHS( INLEN, OUTLEN, RATIO, PIXELS, WEIGHT, STATUS )
*
*    Arguments :
*
*     INLEN = INTEGER( READ )
*           Input array dimension under consideration.
*     OUTLEN = INTEGER( READ )
*           Output array dimension under consideration.
*     RATIO = REAL( WRITE )
*           The ratio of input array dimension to output array
*           dimension.
*     PIXELS( OUTLEN, 2 ) = INTEGER( WRITE )
*           Will contains the pixel limits over which averaging will be
*           performed in the input array to form a single pixel in the
*           output array.
*     WEIGHT( OUTLEN, 2 ) = REAL( WRITE )
*           Will contain the weights which correspond to the pixel
*           limits in PIXELS.
*     STATUS = INTEGER( READ )
*           This is the global status, if this variable has an error
*           value on entry then an immediate return will occur.
*
*    Method :
*
*     If no error on entry then
*        Calculate ratio of input array dimension to output array
*          dimension
*        Get the integer truncated ratio, INTRAT and fractional part of
*          ratio, FRACT
*        Set up edge conditions
*        For first pixel of output array
*           First pixel for averaging is first pixel in input array
*           Last pixel for averaging is INTRAT+1'th pixel in input array
*           Weight for first pixel is 1.0, weight for last pixel is
*             FRACT
*        Endfor
*        For last pixel of output array
*           First pixel for averaging is (last-INTRAT)'th pixel in
*             input array
*           Last pixel for averaging is last pixel in input array
*           Weight for first pixel is FRACT, weight for last pixel is
*             1.0
*        Endfor
*        For second to last-1 pixels of output array
*           Weight for new first pixel is 1.0 - previous last pixel
*             weight
*           New first pixel for averaging is previous last pixel
*           If weight for first pixel is effectively zero then
*              Set weight to 1.0 and first pixel becomes next pixel
*                along
*           Endif
*           Calculate remaining weight after weight for first pixel is
*             removed from the total weight
*           Calculate number of pixels between first and last pixels
*             this is also the number of unit weights
*           Last pixel weight is remaining weight minus number of unit
*             weights
*           If last pixel weight is effectively zero then
*              Set weight to 1.0
*           Else
*              Number of pixels between first and last pixels
*                incremented to take account of pixel with fractional
*                weight
*           Endif
*           New last pixel is 1st pixel + no. of pixels between 1st and
*             last
*        Endfor
*     Endif
*
*    Authors :
*
*     Dave Baines (ROE::ASOC5)
*     Malcolm Currie RAL (UK.AC.RL.STAR::CUR)
*
*    History :
*
*     18/08/1983 : Original version                     (ROE::ASOC5)
*     19/02/1984 : Documentation brought up to standard (ROE::ASOC5)
*     1986 Sep 10 : Renamed parameters section to arguments, tried to
*                   make description comprehensible and tidied
*                  (RL.STAR::CUR).
*
*    Type Definitions :

      IMPLICIT NONE

*    Global constants :

      INCLUDE 'SAE_PAR'

*    Import :

      INTEGER
     :  INLEN,
     :  OUTLEN

*    Export :

      INTEGER
     :  PIXELS( OUTLEN, 2 )

      REAL
     :  WEIGHT( OUTLEN, 2 ),
     :  RATIO

*    Status :

      INTEGER STATUS

*    Local Constants :

      REAL WTMIN                    ! minimum weighting value allowed
      PARAMETER ( WTMIN = 0.000001 )

*    Local variables :

      INTEGER
     :  INTRAT, ! integer truncated ratio INLEN/OUTLEN
     :  INDEX,  ! index to elements of PIXELS and WEIGHT
     :  INCREM  ! number of pixels between 1st and last pixels

      REAL
     :  FRACT, ! fractional part of RATIO
     :  REMWT  ! weight left over after 1st weight calculated
*-

*    check for error on entry

      IF( STATUS .EQ. SAI__OK ) THEN

*       calculate ratio of input dimension to output dimension

         RATIO = REAL( INLEN ) / REAL( OUTLEN )

*       calculate integer truncated ratio

         INTRAT = INT( RATIO )

*       calculate fractional part of ratio

         FRACT = RATIO - INTRAT

*       set up edge conditions

         PIXELS( 1, 1 ) = 1
         PIXELS( 1, 2 ) = INTRAT + 1
         WEIGHT( 1, 1 ) = 1.0
         WEIGHT( 1, 2 ) = FRACT
         PIXELS( OUTLEN, 1 ) = INLEN - INTRAT
         PIXELS( OUTLEN, 2 ) = INLEN
         WEIGHT( OUTLEN, 1 ) = FRACT
         WEIGHT( OUTLEN, 2 ) = 1.0

*       set up rest of squash parameters

         DO  INDEX = 2, OUTLEN-1

*          weight for new first pixel is 1.0 - previous last pixel
*          weight

            WEIGHT( INDEX, 1 ) = 1.0 - WEIGHT( INDEX-1, 2 )

*          new first pixel is previous last pixel

            PIXELS( INDEX, 1 ) = PIXELS( INDEX-1, 2 )

            IF( WEIGHT( INDEX, 1 ) .LT. WTMIN ) THEN

*             weight is effectively zero, set weight to one and

               WEIGHT( INDEX, 1 ) = 1.0

*             move on to the next pixel

               PIXELS( INDEX, 1 ) = PIXELS( INDEX, 1 ) + 1
            ENDIF

*          what weight remains after removing first pixel weight

            REMWT = RATIO - WEIGHT( INDEX, 1 )

*          how many unit weights

            INCREM = INT( REMWT )

*          last pixel weight is left over weight - no. of unit weights

            WEIGHT( INDEX, 2 ) = REMWT - REAL( INCREM )

            IF( WEIGHT( INDEX, 2 ) .LT. WTMIN ) THEN

*             last pixel weight is effectively zero so set it to 1.0

               WEIGHT( INDEX, 2 ) = 1.0
            ELSE

*             have a fractional weight so increase pixel increment by 1

               INCREM = INCREM + 1
            ENDIF

*          new last pixel is first pixel plus the increment

            PIXELS( INDEX, 2 ) = PIXELS( INDEX, 1 ) + INCREM
         ENDDO
      ENDIF

      END
