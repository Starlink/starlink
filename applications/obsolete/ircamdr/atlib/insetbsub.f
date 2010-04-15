*+  INSETBSUB - replaces all pixels inside box with specified value

      SUBROUTINE INSETBSUB ( INARRAY, OUTARRAY, DIMS1, DIMS2, XSTART,
     :                       YSTART, XSIZE, YSIZE, NEWVAL, STATUS )

*    Description :
*
*     This routine copies the data from one array to another except
*     inside the bounds of a specified box, where a given fixed
*     value is inserted as a replacement.
*
*    Invocation :
*
*     CALL INSETBSUB( INARRAY, OUTARRAY, DIMS, XSTART, YSTART,
*                     XSIZE, YSIZE, NEWVAL, STATUS )
*
*    Method :
*
*    Bugs :
*
*     None known.
*
*    Authors :
*
*     Mark McCaughrean UoE (REVA::MJM)
*     Colin Aspin (JACH::CAA)
*
*    History :
*
*     17-09-1985 : First implementation (REVA::MJM)
*     03-07-1986 : Revised implementation (REVA::MJM)
*     29-10-1988 : my version for INSETB (JACH::CAA)
*     11-Aug-1994  Changed DIM arguments so that routine will compile (SKL@JACH)
*
*    Type Definitions :

      IMPLICIT  NONE        ! no default typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'    ! global SSE parameters

*    Import :

      INTEGER
     :    DIMS1,      ! dimensions of input array
     :    DIMS2,      ! dimensions of input array
     :    XSTART,         ! x coord of box start
     :    YSTART,         ! y coord of box start
     :    XSIZE,          ! x size of box
     :    YSIZE           ! y size of box

      REAL
     :    INARRAY( DIMS1, DIMS2 ),   ! input data
     :    NEWVAL                             ! replacement value

*    Export :

      REAL
     :    OUTARRAY( DIMS1, DIMS2 )   ! output data

*    Status :

      INTEGER  STATUS      ! global status parameter

*    Local variables :

      INTEGER
     :    I, J             ! counters

*-
*    check status on entry - return if not ok

      IF ( STATUS .NE. SAI__OK ) THEN

         RETURN

      ENDIF

*    loop round all rows in array

      DO  J  =  1, DIMS2

*       loop round all pixels in current row

         DO  I  =  1, DIMS1

*         test if current pixel inside box and set to new value if it is ...

	    IF( J .GE. YSTART .AND. J .LE. ( YSTART+YSIZE-1) .AND.
     :	        I .GE. XSTART .AND. I .LE. ( XSTART+XSIZE-1)) THEN

               OUTARRAY( I, J )  =  NEWVAL

*         set to old value if current pixel outside box

            ELSE

               OUTARRAY( I, J )  =  INARRAY( I, J )

            ENDIF

         END DO

      END DO

      END
