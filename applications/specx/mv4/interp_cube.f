*  History:
*     22 Nov 1993 (hme):
*        Replace LIB$FREE_VM with PSX_FREE.
*     17 Dec 1993 (hme):
*        In order to adapt to new STACKCOMM/PROTOTYPE, no longer use
*        TSYS as start of scan header, since it isn't any more.
*     20 July 2000 (ajc):
*        Missing commas in format
*        Change TYPE * to PRINT *
*        Unused I, J, K, NMIN, NMAX
C-----------------------------------------------------------------------

      SUBROUTINE INTERPOLATE_CUBE (BUF1, BUF2, IFAIL)

C   Routine to take the data cube and interpolate the spectra which aren't
C   there but are close enough to others.

C   Updated version 6/7/89 (RP) interpolates all spectra from one cube
C   onto a new one, then just swaps the pointers and releases the VM
C   held for the original cube. Consequence is that the interpolation cannot
C   now be undone just by resetting the indices.

      IMPLICIT NONE

C     Formal parameter:

      REAL      BUF1(*)           ! Workspace
      REAL      BUF2(*)           ! Workspace
      INTEGER   IFAIL             ! SPECX error return

C     Include files:

      INCLUDE 'CUBE'
      INCLUDE 'FLAGCOMM'
      INCLUDE 'MAPS'
      INCLUDE 'MAPHD'
      INCLUDE 'PROTOTYPE'
      INCLUDE 'PLOT2D'
      INCLUDE 'WEIGHTS'
      INCLUDE 'CNF_PAR'

C     Miscellaneous variables

      LOGICAL   INTERP            ! True if one or more points contain "good" data
      LOGICAL   HIT_DATA          ! True if *some* data found in beam
      INTEGER   M,N               ! Counters
      INTEGER   ISTAT             ! Status return for GEN_ routines
      INTEGER   INTCUBE_ADDRESS   ! Pointer to new (interpolated) cube.
      INTEGER   INTINDEX_ADDRESS  ! Pointer to new index array
      INTEGER   MNPOS             ! Value from INDEX array
      INTEGER   MNOFFSET          ! Offset bytes in (m,n) space
      INTEGER   NDATA             ! Length of spectrum

C     Functions:

      INTEGER   IFREEVM

C  Ok, go..

      IFAIL = 0

C     Check cube is in memory

      IF (.NOT. CUBE_IN_MEMORY) THEN
        IFAIL = 68
        RETURN
      END IF

C     Check not trivial interpolation (delta function)

      IF (IXMAX.EQ.0 .AND. IYMAX.EQ.0) THEN
        PRINT *
        PRINT *,'Trivial interpolation, zero width beam:'
        PRINT *,'Cube left as is!'
        PRINT *
        RETURN
      ELSE
        PRINT *
        PRINT *, 'Interpolating function has size (x,y) =',IXMAX,IYMAX
        PRINT *
      END IF

C    Check cube not already interpolated and rotated

      IF (MAP_INTERPOLATED .AND. MAP_ROTATED) THEN
        PRINT *, ' -- interpolate_cube -- Map already'
     &           //' rotated and interpolated'
        IFAIL = 72
        RETURN
      END IF

C     If already interpolated, clear out the modified cube and
C     start again... (resets cube and index pointers)

      IF (MAP_INTERPOLATED) THEN
        PRINT *, ' -- interpolate_cube -- '
        PRINT *, '    cube already interpolated - do over from scratch'
        CALL RELEASE_NEW_CUBE
      END IF

C     Determine the map header

      CALL PUSH
      CALL EXTRACT_HEADER (SCAN_HEADER)
      NDATA = NPTS(1)
      CALL POP

C     Get virtual memory for interpolated cube.

      PRINT *,'Getting virtual memory for new cube...'
      PRINT *, NCUBE,  ' bytes required for cube'
      PRINT *, NINDEX, ' bytes required for index array'

      CALL MAKE_CUBE2 (NCUBE, NINDEX, INTCUBE_ADDRESS,
     &                 INTINDEX_ADDRESS, IFAIL)
      CALL XCOPY      (NINDEX, %VAL(CNF_PVAL(CURRENT_INDEX_ADDRESS)),
     &                 %VAL(CNF_PVAL(INTINDEX_ADDRESS)))
      NEW_CUBE_LOADED = .TRUE.

C     Debug INDEX array

*     CALL LDEBUG (%VAL(CNF_PVAL(CURRENT_INDEX_ADDRESS)), MSTEP, NSTEP)

C     Then do the interpolation

      PRINT *
      PRINT *, 'This may take a little while:'
      PRINT '('' Row being interpolated (out of'', I4,'')'')', NSTEP
      PRINT *

      INTERP   = .FALSE.
      HIT_DATA = .FALSE.

      DO N = 1, NSTEP

        WRITE( *, '(I5)' ) N

        DO M = 1, MSTEP

          MNOFFSET = 4*((N-1)*MSTEP + (M-1))
          CALL XCOPY (4,
     :         %VAL(CNF_PVAL(CURRENT_INDEX_ADDRESS)+MNOFFSET),
     :         MNPOS)

          CALL READ_INTERP_DATA (NDATA, M, N, BUF1, BUF2, 1, NDATA,
     &                           HIT_DATA, INTERP)

C         If we have data on all sides of the point to be interpolated, then
C         go ahead and put properly weighted data back into the cube.

          IF (INTERP) THEN

            CALL XCOPY (4*NDATA, BUF2,
     :           %VAL(CNF_PVAL(INTCUBE_ADDRESS)+MNOFFSET*NDATA))

*           PRINT *,'All logicals set'
*           PRINT *,'Interpolated array placed in pixel ',M,N
*           PRINT *,'Final sum of weights =',SIGMA_W

*           Update the index array (make entry for an interpolated
*           spectrum 0 rather than -1000). Note -- retain positive
*           values for positions where spectra exist in original so
*           that valid points can be shown on plots.

            CALL XCOPY (4,
     :           %VAL(CNF_PVAL(INTINDEX_ADDRESS)+MNOFFSET),
     :           MNPOS)
            IF (MNPOS.LT.0) THEN
              MNPOS    = 0
              CALL XCOPY (4, MNPOS,
     :             %VAL(CNF_PVAL(INTINDEX_ADDRESS)+MNOFFSET))
            END IF

          END IF
        END DO
      END DO
      PRINT *

C     Debug new INDEX array

*     CALL LDEBUG (%VAL(CNF_PVAL(INTINDEX_ADDRESS)), MSTEP, NSTEP)

      IF (HIT_DATA) THEN

*       Release any *intermediate* cube that may be present
        IF (CURRENT_CUBE_ADDRESS.NE.CUBE_ADDRESS) THEN

*         PRINT *, ' -- interpolate_cube --'
*         PRINT *, '    Releasing ', NCUBE, ' bytes of cube @ ',
*    &                 CURRENT_CUBE_ADDRESS

          ISTAT = IFREEVM (CURRENT_CUBE_ADDRESS)
          IF (ISTAT.NE.0) THEN
            PRINT *, ' -- interpolate_cube -- Error releasing cube'
          END IF

*         PRINT *, '    Releasing ', NINDEX, ' bytes of index @ ',
*    &                 CURRENT_INDEX_ADDRESS

          ISTAT = IFREEVM (CURRENT_INDEX_ADDRESS)
          IF (ISTAT.NE.0) THEN
            PRINT *, ' -- interpolate_cube -- Error releasing index'
          END IF
        END IF

*       Transfer the address of the new cube to the /CUBE/ common area
        CURRENT_CUBE_ADDRESS  = INTCUBE_ADDRESS
        CURRENT_INDEX_ADDRESS = INTINDEX_ADDRESS

*       Set "interpolated" flag
        MAP_INTERPOLATED = .TRUE.

      ELSE
        IFAIL = 69      ! No "raw" data points found in current map

        ISTAT = IFREEVM (INTCUBE_ADDRESS)
        IF (ISTAT.NE.0) THEN
          PRINT *, ' -- interpolate_cube (error releasing cube ',
     &            NCUBE, ' bytes @ ', INTCUBE_ADDRESS
        END IF

        ISTAT = IFREEVM (INTINDEX_ADDRESS)
        IF (ISTAT.NE.0) THEN
          PRINT *, ' -- interpolate_cube (error releasing index ',
     &            NINDEX, ' bytes @ ', INTINDEX_ADDRESS
        END IF

      END IF

      RETURN
      END



      SUBROUTINE LDEBUG (INDEX, MSTEP, NSTEP)

      IMPLICIT   NONE

      INTEGER    MSTEP
      INTEGER    NSTEP
      INTEGER    INDEX (MSTEP,NSTEP)

      INTEGER    I, J
      CHARACTER  DEBUG*80

      DEBUG = ' '

      DO J = 1, NSTEP

        DO I = 1,  MIN (MSTEP, 80)
          DEBUG(I:I) = '.'
          IF (INDEX(I,J).GT.0) THEN
            DEBUG(I:I) = 'X'
          ELSE IF(INDEX(I,J).EQ.0) THEN
            DEBUG(I:I) = '0'
          END IF
        END DO
        PRINT *, DEBUG

      END DO

      RETURN
      END
