*+  LAPLSB - subtracts Laplacian of input 2-d image to form output

      SUBROUTINE LAPLSB ( INARR, DIM1, DIM2, NUMBER, OUTARR, STATUS )
*
*    Description :
*
*     This routine subtracts NUMBER times the Laplacian of the input
*     array from that array to create the output array. This can be
*     thought of as a convolution by
*  
*                             -N   -N   -N
*                             -N   +8N  -N
*                             -N   -N   -N
*
*     where N is the integer number NUMBER. The convolution acts as
*     a unidirectional edge detector, and for a flat areas in the 
*     input array, the convolution sums to zero. 
*
*    Invocation :
*
*     CALL LAPLSB( INARR, DIM1, DIM2, NUMBER, OUTARR, STATUS )
*
*    Arguments :
*
*     INARR( DIM1, DIM2 ) = REAL( READ )
*         The input data array
*     DIM1 = INTEGER( READ )
*         The first dimension of the 2-d arrays.
*     DIM2 = INTEGER( READ )
*         The second dimension of the 2-d arrays.
*     NUMBER = INTEGER( READ )
*         The number of times the Laplacian is subtracted
*     OUTARR( DIM1, DIM2 ) = REAL( WRITE )
*         The output compressed data array
*     STATUS  =  INTEGER( READ )
*         Global status value
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     If o.k. then
*     For all lines in array
*        Find top and bottom mask positions
*        For all pixels in current line
*           Find left and right mask positions
*           If the central pixel is in the first column then
*              For all applicable mask positions
*                 If pixel is invalid then
*                    Mask is zero (weight) for this pixel
*                 Else
*                    Mask is -NUMBER
*                 Endif
*                 If pixel is not central sum mask value to central 
*                   weight
*              End for
*           Else
*              For all applicable mask positions
*                 Sum central weights less central pixel
*              End for
*              If there is a right-hand pixel to be 'read' then
*                 For all column mask positions
*                    If right-hand pixel is invalid then
*                       Mask is zero (weight) for this pixel
*                    Else
*                       Mask is -NUMBER
*                       Sum central weight
*                    Endif
*                 Endfor
*              End if
*           End if
*           If central pixel not valid or central weight is zero then
*              Set outarray pixel = bad
*           Else
*              Evaluate approximate convolution from pixels in box
*                around current pixel in input array as the product
*                of input array and mask, except for central pixel,
*                which is weighted by the central weight
*           End if
*        Endfor
*     Endfor
*     Return
*
*    Bugs :
*
*     None known.
*
*    Authors :
*
*     Mark McCaughrean UoE ( REVA::MJM )
*     Malcolm Currie RAL (UK.AC.RL.STAR::CUR)
*
*    History :
*
*     10-12-1985 : First implementation (UKTH::MARK)
*     17-04-1986 : Tidied and modified (REVA::MJM)
*     1986 Aug 13: Renamed from LAPLACESUB, completed the prologue and
*                  conformed to Starlink programming standards
*                  (RL.STAR::CUR).
*     1986 Sept 7: Major rewrite to account for edge pixels and invalid
*                  pixels, and renamed the parameters section of the
*                  prologue to arguments (RL.STAR::CUR).
*     1989 Aug  7: Passed array dimensions as separate variables
*                  (RL.STAR::CUR).
*
*    Type definitions :

      IMPLICIT  NONE              ! no default typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'          ! global SSE definitions
      INCLUDE 'PRM_PAR'           ! PRIMDAT public constants

*    Import :

      INTEGER
     :     DIM1, DIM2,
     :     NUMBER
 
      REAL
     :     INARR( DIM1, DIM2 )

*    Export :

      REAL
     :     OUTARR( DIM1, DIM2 )

*    Status :

      INTEGER  STATUS

*    Local constants :

      INTEGER
     :      BOX,                  ! Neighbourhood box size
     :      HBOX                  ! Half box size

      PARAMETER ( BOX  =  3 )
      PARAMETER ( HBOX = BOX/2 )

*    Local variables :

      INTEGER
     :     CENWHT,                ! normalisation weighting of central 
                                  ! pixel
     :     IL, IR, JB, JT,        ! left, right, bottom and top extents
     :                            ! of the mask respectively
     :     MASK( -HBOX:HBOX, -HBOX:HBOX ), ! running mask
     :     I, J, II, JJ           ! general counter variables

      REAL
     :     SUM                    ! integration sum
     
*-
*    check status on entry - return if not o.k.

      IF ( STATUS .NE. SAI__OK ) GOTO 999

*    now perform the Laplacian convolution - loop round all lines

      DO  JJ  =  1 , DIM2

*       define top and bottom mask positions

         JT = MIN( DIM2, JJ + HBOX ) - JJ
         JB = MAX( 1, JJ - HBOX ) - JJ

*       loop round all pixels in current line

         DO  II  =  1, DIM1

*          define left and right mask positions

            IR = MIN( DIM1, II + HBOX ) - II
            IL = MAX( 1, II - HBOX ) - II

            CENWHT  =  0

*          special case for first column - need to assign initial
*          mask values

            IF ( II .EQ. 1 ) THEN

*             for all mask positions

               DO  J = JB, JT
                  DO  I = IL, IR

*                   test for invalid pixels

                     IF ( INARR( II+I, JJ+J ) .EQ. VAL__BADR ) THEN
                        MASK( I, J )  =  0
                     ELSE
                        MASK( I, J )  =  -NUMBER
                     END IF

*                   sum central weights less central pixel

                     IF ( I .NE. 0 .OR. J .NE. 0 ) THEN
                        CENWHT  =  CENWHT - MASK( I, J )
                     END IF
                  END DO
               END DO
            ELSE

*             slide mask one column

               DO  J = JB, JT
                  DO  I = IL, MAX( IR-1, 0 )
                     MASK( I, J ) = MASK( I+1, J )

*                   sum central weights less central pixel

                     IF ( I .NE. 0 .OR. J .NE. 0 ) THEN
                        CENWHT  =  CENWHT - MASK( I, J )
                     END IF
                  END DO
               END DO

*             read in next right-hand pixel, if there is one to be
*             read, i.e. not at right-hand edge

               IF ( II + HBOX .LE. DIM1 ) THEN

*                set mask in all of its lines and revise central weight

                  DO  J  = JB, JT
                     IF ( INARR( IR+II, J+JJ ) .EQ. VAL__BADR ) THEN
                        MASK( IR, J )  =  0
                     ELSE
                        MASK( IR, J )  =  -NUMBER
                        CENWHT  =  CENWHT - MASK( IR, J )
                     END IF
                  END DO
               END IF

*          end of first-column condition
            END IF

*          central pixel must be valid

            IF ( INARR( II, JJ ) .EQ. VAL__BADR .OR.
     :           CENWHT .EQ. 0 ) THEN

*             surrounding pixels are invalid

               OUTARR( II, JJ )  =  VAL__BADR
            ELSE

               SUM =  0.0

*             compute approximation to the Laplacian

               DO  J = JB, JT
                  DO  I = IL, IR
                     IF ( I .NE. 0 .OR. J .NE. 0 ) THEN
                        SUM  =  SUM + INARR( II+I, JJ+J ) * 
     :                          REAL( MASK( I, J ) )
                     ELSE

*                      use central weighting equals minus sum weights
*                      of neighbouring pixels

                        SUM =  SUM + INARR( II, JJ ) * REAL( CENWHT )
                     END IF
                  END DO
               END DO

               OUTARR( II, JJ )  =  SUM 

*          end of condition for invalid central pixel or no valid
*          neighbouring pixels
            END IF
                     
*       end of loop round pixels in current line
         END DO

*    end of loop round lines
      END DO

 999  CONTINUE

*    end and return

      END
