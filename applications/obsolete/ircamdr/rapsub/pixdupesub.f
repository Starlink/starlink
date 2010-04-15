
*+  PIXDUPESUB - expands an image by pixel duplication

      SUBROUTINE PIXDUPESUB ( INARRAY, IDIMS1, IDIMS2, OUTARRAY,
     :                        ODIMS1, ODIMS2, EXPAND, STATUS )

*    Description :
*
*     This routine expands an input image by pixel duplication, where
*     the duplication factor has been obtained from the calling routine.
*     This routine is designed for 2-d arrays only.
*
*    Invocation :
*
*     CALL PIXDUPESUB( INARRAY, IDIMS, OUTARRAY, ODIMS, EXPAND, STATUS )
*
*    Parameters :
*
*     INARRAY( IDIMS( 1 ), IDIMS( 2 ) ) = REAL( READ )
*           The input data array
*     IDIMS( 2 ) = INTEGER( READ )
*           The dimensions of the input array
*     OUTARRAY( ODIMS( 1 ), ODIMS( 2 ) ) = REAL( WRITE )
*           The output expanded data array
*     ODIMS( 2 ) = INTEGER( READ )
*           The dimensions of the output array
*     EXPAND = INTEGER( READ )
*           The linear expansion factor applied
*
*    Method :
*
*     Check status on entry - return if not o.k.
*     For all rows in input image
*        Work out which rows of the output image come from the current
*         input image row
*        For all pixels in the current input array row
*           Work out which pixels in the current output rows come
*            from the current input pixel
*           Get value for current input array pixel
*           For all rows in output corresponding to current input row
*              For all pixels in output corresponding to current input pixel
*                 Output array pixel value  =  Input array pixel value
*              Endfor
*           Endfor
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
*     Mark McCaughrean UoE (REVA::MJM)
*
*    History :
*
*     19-09-1985 : First implementation (REVA::MJM)
*     03-07-1986 : Tidied up and debugged (REVA::MJM)
*     15-AUG-1994  Changed DIM arguments so routine will compile (SKL@JACH)
*
*    Type Definitions :

      IMPLICIT  NONE               ! no default typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'           ! global SSE definitions

*    Import :

      INTEGER
     :    IDIMS1, IDIMS2,              ! dimensions of input array
     :    ODIMS1, ODIMS2,              !      "      " output  "
     :    EXPAND                   ! linear expansion factor to be used

      REAL
     :    INARRAY( IDIMS1, IDIMS2 )    ! input data array

*    Export :

      REAL
     :    OUTARRAY( ODIMS1, ODIMS2 )   ! output data array

*    Status :

      INTEGER  STATUS              ! global status parameter

*    Local variables :

      INTEGER
     :    STARTROW,            ! current start row in output image
     :    ENDROW,              !    "     end   "   "    "     "
     :    STARTCOL,            !    "    start column    "     "
     :    ENDCOL,              !    "     end     "      "     "
     :    I, J, K, L           ! counters

      REAL
     :    CURRENT              ! value of pixel currently being duplicated

*-
*    check status on entry - return if not ok
      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      ENDIF


*    loop around all rows of the input array
      DO  J  =  1, IDIMS2

*       calculate the starting and finishing y indices of the pixels
*       in the output image that will be formed from pixels with the
*       current y index in the input image

         STARTROW  =  ( ( J - 1 ) * EXPAND ) + 1
         ENDROW    =  STARTROW + EXPAND - 1

*       loop round all pixels in current input image row
         DO  I  =  1, IDIMS1

*          calculate the starting and finishing x indices of the pixels
*          in the output image which will be formed by duplication of
*          the current pixel in the input image
            STARTCOL  =  ( ( I - 1 ) * EXPAND ) + 1
            ENDCOL    =  STARTCOL + EXPAND - 1

*          set a dummy variable to be the value of the pixel currently
*          under duplication
            CURRENT  =  INARRAY( I, J )

*          loop round all the output pixels that are to have this value
            DO  L  =  STARTROW, ENDROW
               DO  K  =  STARTCOL, ENDCOL
                  OUTARRAY( K, L )  =  CURRENT
               END DO
            END DO

*       end of loop round all pixels in current input array row
         END DO

*    end of loop round all rows in input array
      END DO


*    end and return
      END
