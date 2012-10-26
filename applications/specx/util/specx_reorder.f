*  History:
*     20 July 2000 (ajc):
*        Change TYPE * to PRINT *
*------------------------------------------------------------------------

      SUBROUTINE SPECX_REORDER (N, ISOURCE)

C  Routine to rearrange individual receiver spectra according to
C  source array ISOURCE. Modified by JFL from SPECX_REORDER,
C  DATA array with variable sized quadrants catered for.

      IMPLICIT NONE

*     formal parameters:

      INTEGER   N
      INTEGER   ISOURCE(*)

*     include files:

      INCLUDE   'FLAGCOMM'
      INCLUDE   'STACKCOMM'
      INCLUDE   'CNF_PAR'

*     local variables:

      INTEGER   IPTR
      INTEGER   LENTGT
      INTEGER   NQ, K, OFFSET
      INTEGER   NTOTAL
      INTEGER   TARGET
      INTEGER   DSTART, DEND
      INTEGER   ISTAT

*     functions:

      INTEGER   NTOT
      INTEGER   IGETVM
      INTEGER   IFREEVM

*  Ok, go...

CD    PRINT '('' # of sectors, sort array = '',I4.1,8I3.1)',
CD   &          N, (ISOURCE(K),K=1,N)

C     First reorder data array
C     Get virtual memory and take copy of data

      NTOTAL = NTOT(N)

      ISTAT = IGETVM (4*NTOTAL, .TRUE., 'SPECX_REORDER', IPTR)
      IF (ISTAT .ne. 0) THEN
        PRINT *, 'Trouble getting virtual memory for reordering:'
        PRINT *, ' ---', 4*NTOTAL, ' bytes requested'
        PRINT *, ' --- reordering not done'
        RETURN
      END IF

      CALL XCOPY  (4*NTOTAL, DATA, %VAL(CNF_PVAL(IPTR)))

      OFFSET = 1

C     and copy it back in the desired order

      DO NQ = 1, N

         TARGET = ISOURCE(NQ)

C        find start, end and length of target section

         DSTART = NTOT (TARGET-1)
         DEND   = NTOT (TARGET)
         LENTGT = NPTS (TARGET)

C        copy target section into output array

CD       PRINT *, 'Copy of array segment from: start, end, length = ',
CD   &            DSTART, DEND, LENTGT

         CALL XCOPY (4*LENTGT, %VAL(CNF_PVAL(IPTR)+ 4*DSTART),
     :               DATA(OFFSET))

C        and update offset

         OFFSET = OFFSET + LENTGT

      END DO

C     release virtual memory

      ISTAT = IFREEVM (IPTR)

C     What will happen to IQCEN?

      DO NQ = 1,N
        IF (ISOURCE(NQ) .EQ. IQCEN) IQCEN = NQ
      END DO

C     Then rearrange quadrant dependent parameters

CD    PRINT *, 'Sort of header variables: NQ = 1 to ', N-1

      DO NQ = 1, N-1

        IF (NQ.NE.ISOURCE(NQ)) THEN

          CALL EXCHNGE (NQ, ISOURCE(NQ), NPTS,   4)
          CALL EXCHNGE (NQ, ISOURCE(NQ), JFINC,  4)
          CALL EXCHNGE (NQ, ISOURCE(NQ), JFREST, 4)
          CALL EXCHNGE (NQ, ISOURCE(NQ), LOFREQ, 8)
          CALL EXCHNGE (NQ, ISOURCE(NQ), IFFREQ, 8)
          CALL EXCHNGE (NQ, ISOURCE(NQ), ITREC,  4)
          CALL EXCHNGE (NQ, ISOURCE(NQ), ITSKY,  4)
          CALL EXCHNGE (NQ, ISOURCE(NQ), ITTEL,  4)

          DO K = NQ,NQUAD
            IF (ISOURCE(K).EQ.NQ) THEN
              ISOURCE(K)  = ISOURCE(NQ)
              ISOURCE(NQ) = NQ
            END IF
          END DO

        END IF
      END DO


      END

*------------------------------------------------------------------------

