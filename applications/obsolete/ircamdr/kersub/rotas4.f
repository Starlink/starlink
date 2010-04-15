*+  ROTAS4 - rotates input array into output array for ROTATE
      SUBROUTINE ROTAS4( NUMRA, LONG, SHORT, ROTSIZ, XLARGE, IDIMS1,
     :  IDIMS2, ARRIN, ODIMS1, ODIMS2, ARROUT, WORK, STATUS )
*    Description :
*     The input array, ARRIN, is rotated through NUMRA right angles in the
*     clockwise direction as a number of ROTSIZ by ROTSIZ sections. The
*     rotated array is put into ARROUT.
*     An immediate return will occur if STATUS has an error value on entry.
*    Invocation :
*      CALL ROTAS4( NUMRA, LONG, SHORT, ROTSIZ, XLARGE, IDIMS, ARRIN, ODIMS,
*     :  ARROUT, WORK, STATUS )
*    Parameters :
*     NUMRA = INTEGER( READ )
*           Number of right-angles through which data will be rotated.
*     LONG = INTEGER( READ )
*           Longest dimension of the array to be rotated.
*     SHORT = INTEGER( READ )
*           Shortest dimension of the array to be rotated.
*     ROTSIZ = INTEGER( READ )
*           Size of the subsections to be rotated.
*     XLARGE = LOGICAL( READ )
*           Should be .TRUE. if first dimension of the array to be rotated is
*           greater than the second.
*     IDIMS( 2 ) = INTEGER( READ )
*           Dimensions of input data array to be rotated.
*     ARRIN( IDIMS(1), IDIMS(2) ) = REAL( READ )
*           Data to be rotated.
*     ODIMS = INTEGER( READ )
*           Dimensions of the output data array.
*     ARROUT( ODIMS(1), ODIMS(2) ) = REAL( WRITE )
*           Will contain the rotated data.
*     WORK( ROTSIZ, ROTSIZ ) = REAL( WRITE )
*           Workspace to hold the subsection for rotation.
*     STATUS = INTEGER( UPDATE )
*           This is the global status, if this variable has an error value on
*           entry then an immediate return will occur. If an error occurs
*           during the execution of this routine STATUS will be returned
*           containing the appropriate error value.
*    Method :
*     If no error on entry then
*        ENDL is the position of the start of the last rotation box along the
*          longer side of the input array
*        ENDS is the position of the start of the last rotation box along the
*          shorter side of the input array
*        INDEXL is a pointer to the position of the start of the rotation
*          box along the longer side of the input array
*        INDEXS is a pointer to the position of the start of the rotation
*          box along the shorter side of the input array
*        ROTSIZ is the size of the rotation box
*        Move along longest side of input array
*        For INDEXL from 1, in steps of ROTSIZ, to a value less than or equal
*          to ENDL
*           Move along shortest side of input array
*           For INDEXS from 1, in steps or ROTSIZ, to a value less than or
*             equal to ENDS
*              Rotate box pointed at by INDEXL, INDEXS
*           End for
*           If shorter dimension of input array is not exactly divisible by
*             ROTSIZ then
*              There will be some data left unrotated so set INDEXS to point
*                at ENDS
*              Rotate box
*           Endif
*        Endfor
*        If the longer dimension of the input array is not exactly divisible
*          by ROTSIZ then
*           There will be some data left unrotated so set INDEXL to point
*             at ENDL
*           Move along shortest side of input array
*           For INDEXS from 1, in steps or ROTSIZ, to a value less than or
*             equal to ENDS
*              Rotate box pointed at by INDEXL, INDEXS
*           End for
*           If shorter dimension of input array is not exactly divisible by
*             ROTSIZ then
*              There will be some data left unrotated so set INDEXS to point
*                at ENDS
*              Rotate box
*           Endif
*        Endif
*     Endif
*    Authors :
*     Dave Baines (ROE::ASOC5)
*    History :
*     27/07/1983 : Original version                     (ROE::ASOC5)
*     17/02/1984 : Documentation brought up to standard (ROE::ASOC5)
*     12-Aug-1994  Changed DIM arguments so that routine will compile(SKL@JACH)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER
     :  IDIMS1, IDIMS2, ! array of dimensions of input array
     :  ODIMS1, ODIMS2, !   "   "       "     "  output array
     :  NUMRA,  ! number of clockwise right-angle rotates
     :  LONG,   ! longest dimension of input array
     :  SHORT,  ! shortest    "      "   "     "
     :  ROTSIZ  ! size of subsection to be rotated
      LOGICAL
     :  XLARGE ! true if IDIMS1 is greater than or equal to IDIMS2
      REAL
     :  ARRIN( IDIMS1, IDIMS2 ) ! input array
*    Import-Export :
      REAL
     :  WORK( ROTSIZ, ROTSIZ ) ! workspace
*    Export :
      REAL
     :  ARROUT( ODIMS1, ODIMS2 ) ! output array
*    Status :
      INTEGER STATUS
*    Local variables :
      INTEGER
     :  ENDL,   ! position of last rotation box along longest side
     :  ENDS,   !     "     "   "      "     "    "   shortest  "
     :  INDEXL, ! pointer to subsection, long dimension
     :  INDEXS  !    "     "      "      short    "
*-

*    check for error on entry
      IF( STATUS .EQ. SAI__OK ) THEN

*       set up positions of last rotation box along long and short dimensions
         ENDL = LONG  + 1 - ROTSIZ
         ENDS = SHORT + 1 - ROTSIZ

*       rotate the array as a number of ROTSIZ by ROTSIZ boxes
*       move along longer side
         DO INDEXL = 1, ENDL, ROTSIZ

*          move along shorter side
            DO INDEXS = 1, ENDS, ROTSIZ

               CALL ROTAS1( NUMRA, ROTSIZ, XLARGE, ENDL, ENDS, INDEXL,
     :           INDEXS, IDIMS1, IDIMS2, ARRIN, ODIMS1, ODIMS2, ARROUT,
     :           WORK, STATUS )

            ENDDO

*          check for unrotated data along shorter side of input array
            IF( MOD( SHORT, ROTSIZ ) .NE. 0 ) THEN

*             pointer set to position af last rotation box along shorter side
               INDEXS = ENDS

               CALL ROTAS1( NUMRA, ROTSIZ, XLARGE, ENDL, ENDS, INDEXL,
     :           INDEXS, IDIMS1, IDIMS2, ARRIN, ODIMS1, ODIMS2, ARROUT,
     :           WORK, STATUS )

            ENDIF
         ENDDO

*       check for unrotated data along longer side of input array
         IF( MOD( LONG, ROTSIZ ) .NE. 0 ) THEN

*          pointer set to position of last rotaton box along longer side
            INDEXL = ENDL

*          move along shorter side
            DO INDEXS = 1, ENDS, ROTSIZ

               CALL ROTAS1( NUMRA, ROTSIZ, XLARGE, ENDL, ENDS, INDEXL,
     :           INDEXS, IDIMS1, IDIMS2, ARRIN, ODIMS1, ODIMS2,
     :           ARROUT, WORK, STATUS )

            ENDDO

*          check for unrotated data along shorter side of input array
            IF( MOD( SHORT, ROTSIZ ) .NE. 0 ) THEN

*             pointer set to position af last rotation box along shorter side
               INDEXS = ENDS

               CALL ROTAS1( NUMRA, ROTSIZ, XLARGE, ENDL, ENDS, INDEXL,
     :           INDEXS, IDIMS1, IDIMS2, ARRIN, ODIMS1, ODIMS2, ARROUT,
     :           WORK, STATUS )
            ENDIF
         ENDIF
      ENDIF

      END
