*  History:
*     20 July 2000 (ajc):
*        Change TYPE * to PRINT *
*        Unused in RELEASE_NEW_CUBE: IFAIL
*-----------------------------------------------------------------------
      SUBROUTINE MAKE_CUBE2 (NCUBE, NINDEX, CUBE_ADDRESS,
     &                                      INDEX_ADDRESS, IFAIL)

*  Routine to get virtual memory for a cube and an index

      IMPLICIT  NONE

*     Formal parameters:

      INTEGER   NCUBE
      INTEGER   NINDEX
      INTEGER   CUBE_ADDRESS
      INTEGER   INDEX_ADDRESS
      INTEGER   IFAIL

*     Local variables:

      INTEGER   ISTAT

*     Functions:

      INTEGER   IGETVM
      INTEGER   IFREEVM

*  Ok, go...

      IFAIL = 0

*     Create virtual memory (use LIB$ routines rather than IGET/IFREE so
*     that cube is not deleted by ^C)

      PRINT *,NCUBE,' bytes required for data cube'

CD    PRINT *, ' -- make_cube2 --'

      ISTAT = IGETVM (NCUBE, .FALSE., 'MAKE_CUBE2', CUBE_ADDRESS)

CD    PRINT *,
CD   &'    Got ', NCUBE, ' bytes of VM for cube @ ',CUBE_ADDRESS
      IF (ISTAT.NE.0) THEN
        PRINT *,
     &  ' -- make_cube2 -- Failed to get virtual memory for cube'
        IFAIL = 51
        GO TO 99
      END IF

      ISTAT = IGETVM (NINDEX, .FALSE., 'MAKE_CUBE2', INDEX_ADDRESS)

CD    PRINT *,
CD   &'    Got ',NINDEX,' bytes of VM for index @ ',INDEX_ADDRESS
      IF (ISTAT.NE.0) THEN
        PRINT *,' -- make_cube2 -- Failed to get v. memory for index'
        IFAIL = 51
        GO TO 99
      END IF

      RETURN

*------------------
*     Error return

   99 CONTINUE
      ISTAT = IFREEVM (CUBE_ADDRESS)
      IF (ISTAT.NE.0) THEN
        PRINT *, ' -- make_cube2 (error) -- didn''t release cube VM'
      END IF

      ISTAT = IFREEVM (INDEX_ADDRESS)
      IF (ISTAT.NE.0) THEN
        PRINT *, ' -- make_cube2 (error) -- didn''t release index VM'
      END IF

      RETURN
*------------------

      END



      SUBROUTINE RELEASE_NEW_CUBE

C  Routine to release virtual memory allocated to rotated data cube, and
C  to mark the cube as non-existent.

      IMPLICIT  NONE

*     Include files:

      INCLUDE 'CUBE'
      INCLUDE 'MAPS'
      INCLUDE 'MAPHD'

*     Local variables:

      INTEGER    ISTAT

*     Functions:

      INTEGER    IFREEVM

*  Ok, go...

CD    PRINT *, ' -- Release new cube --'

      IF (NEW_CUBE_LOADED .AND. NCUBE.NE.0) THEN

CD      PRINT *, '    Releasing ', NCUBE, ' bytes of index @ ',
CD   &               CURRENT_CUBE_ADDRESS
        ISTAT = IFREEVM (CURRENT_CUBE_ADDRESS)
        IF (ISTAT.NE.0) THEN
          PRINT *, '    Error releasing VM for new cube, ISTAT = ',ISTAT
        END IF

CD      PRINT *, '    Releasing ',NINDEX,' bytes of index@ ',
CD   &               CURRENT_INDEX_ADDRESS
        ISTAT = IFREEVM (CURRENT_INDEX_ADDRESS)
        IF (ISTAT.NE.0) THEN
          PRINT *,
     &    '    Error releasing VM for new index, ISTAT = ',ISTAT
        END IF

      END IF

*     Reset pointers back to original (unmodified) cube:

      CURRENT_INDEX_ADDRESS = INDEX_ADDRESS
      CURRENT_CUBE_ADDRESS  = CUBE_ADDRESS

*     Reset flags:

      MAP_INTERPOLATED  = .FALSE.
      MAP_ROTATED       = .FALSE.
      NEW_CUBE_LOADED   = .FALSE.
      MAP_ANGLE         =   0.0

*     Reset map titles:

      CALL SET_MAPTITLE (.FALSE., 0.0, POS_ANGLE)

      RETURN
      END



      SUBROUTINE READ_CUBE_DATA (IIN, JIN, LDATA, DATA)

C  Routine to read spectrum from data cube at (x,y) position = (iin,jin)

      IMPLICIT NONE

C  Formal parameters:

      INTEGER*4 IIN, JIN          ! Cube indices
      INTEGER*4 LDATA             ! Length or array to be read (words)
      REAL*4    DATA(LDATA)       ! Where to return the result

C  Include files:

      INCLUDE 'CUBE'
      INCLUDE 'MAPHD'
      INCLUDE 'CNF_PAR'

C  Other variables:

      INTEGER*4 INS               ! Effective 1-d index in (x,y) plane

      INS = IIN + MSTEP*(JIN-1)

      CALL XCOPY (4*LDATA,
     :     %VAL(CNF_PVAL(CUBE_ADDRESS)+4*LDATA*(INS-1)), DATA)

      RETURN
      END



      SUBROUTINE READ_NEW_CUBE2 (IIN, JIN, LDATA, I1, I2, DATA)

C  Routine to read spectrum from modified data
C  cube at (x,y) position = (iin,jin)

      IMPLICIT NONE

C  Formal parameters:

      INTEGER*4 IIN, JIN          ! Cube indices
      INTEGER*4 LDATA             ! Length or array to be read (words)
      INTEGER*4 I1,  I2           ! First and last elements of spectrum to get
      REAL*4    DATA(LDATA)       ! Where to return the result

C  Include files:

      INCLUDE 'CUBE'
      INCLUDE 'MAPHD'
      INCLUDE 'CNF_PAR'

C  Other variables:

      INTEGER*4 INS               ! Effective 1-d index in (x,y) plane

      INS = IIN + MSTEP*(JIN-1)

      CALL XCOPY (4*(I2+1-I1),
     :     %VAL(CNF_PVAL(CURRENT_CUBE_ADDRESS)
     :     +4*LDATA*(INS-1)+4*(I1-1)),
     :     DATA)

      RETURN
      END
