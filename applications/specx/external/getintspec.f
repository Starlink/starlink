C  History:
C      8-May-2000
C        Replace 'Type *' with 'PRINT *'
C        Unused in EXTRNL10: IPOS
C        Unused in GET_NEWSPEC PDIFFS, MCEN, I, NBYTES, APDEC, STATUS
*-----------------------------------------------------------------------

      SUBROUTINE EXTRNL10 (IFAIL)

C  Routine to get nominated spectrum from the current map.
C  Positions passed to GET_SPECTRUM are in the map coordinate frame - therefore
C  have to convert R.A. and Dec. if map position angle .ne. 0 degrees.
C-

      IMPLICIT  NONE

*     Formal parameters:

      INTEGER   IFAIL

*     Include files:

      INCLUDE   'MAPHD'

*     Local variables:

      INTEGER   ILS
      INTEGER   ISTAT
      REAL      OFFSET(2)
      REAL      X1OFF, X2OFF
      REAL      CP,    SP
      CHARACTER PROMPT*80

*     Functions

      INTEGER   GEN_ILEN

*  Ok, go...

      IFAIL = 0

      PROMPT = 'R.A. and Dec. offset? (arcsec)'
      IF (POS_ANGLE .NE. 0.0) THEN
        PROMPT = 'R.A. and Dec. offset? (arcsec) '//
     &           '(<CR> to select by (X,Y)'
      END IF

      ILS = GEN_ILEN (PROMPT)
      CALL GEN_GETR4A (PROMPT(:ILS), OFFSET, 2, ' ', OFFSET, ISTAT)

      IF (ISTAT.GT.0) THEN
        CALL GEN_GETR4A ('X and Y offsets? (arcsec) ',
     &                    OFFSET, 2, ' ', OFFSET, ISTAT)
        X1OFF = OFFSET(1)
        X2OFF = OFFSET(2)
      ELSE
        CP    = COS (POS_ANGLE/57.29578)
        SP    = SIN (POS_ANGLE/57.29578)
        X1OFF = OFFSET(1)*CP - OFFSET(2)*SP
        X2OFF = OFFSET(1)*SP + OFFSET(2)*CP
        PRINT *,'Offsets in map coordinates: ',X1OFF, X2OFF
      END IF

      CALL GET_NEWSPEC (X1OFF, X2OFF, IFAIL)

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE GET_NEWSPEC (X, Y, IFAIL)

C  Routine to fetch the spectrum from the current cube at the nominated
C  position, and place it in the scan stack.
C  Input positions X and Y are in arcseconds in the map coordinate frame.
C  Thus restored values of DRA and DDEC need to be converted from X and Y
C  offsets.

      IMPLICIT NONE

C     Formal parameters:

      REAL*4    X,Y         !Position of desired point on MAP array
      INTEGER*4 IFAIL       !Error return

C     Include files

      INCLUDE 'FLAGCOMM'
      INCLUDE 'MAPHD'
      INCLUDE 'CUBE'
      INCLUDE 'MAPS'
      INCLUDE 'PLOT2D'
      INCLUDE 'STAKPAR'
      INCLUDE 'STACKCOMM'
      INCLUDE 'CNF_PAR'

C     Other variables

      INTEGER*4 IX,IY,IZ          ! Link array elements
      INTEGER*4 IFRAC
      INTEGER*4 IPOS              ! Position in map file of required data
      INTEGER*4 M, N              ! X and Y offsets (Cells) from start of map
      INTEGER*4 MNPOS
      INTEGER*4 MNOFFSET          ! Offset in bytes into cube
      INTEGER*4 NDATA             ! Number of bytes in the data array
      INTEGER*4 XOFF2, YOFF2      ! (~Double the) X and Y offsets (cells)
      REAL*4    FRAC
      REAL*4    XOFF, YOFF        ! Offsets in cells
      REAL*4    CP, SP

      IX = LINK(1)
      IY = LINK(2)
      IZ = LINK(3)

*  ok, go...

C     Make room on the stack for the new spectrum

      IF (.NOT.XCLEAR)  CALL PUSH

C     Mark the X-register as now containing a spectrum

      IF (JTOP.EQ.0) JTOP = 1
      XCLEAR = .FALSE.

C     Work out offsets in cells if position
C     in file is not known.

      IPOS = 0

      XOFF = X/CELL_XSIZE
      YOFF = Y/CELL_YSIZE

C     Locate appropriate bin on map
C     Both coordinates run + to - (i.e. map starts at top left = NE)

      M     = NINT (0.5 * FLOAT (MSTEP+1) - XOFF)
      FRAC  = SIGN (1.0,XOFF) * MOD(MSTEP+1,2)
      IFRAC = NINT (FRAC)
      XOFF2 = 2 * NINT (XOFF+FRAC*0.5) - IFRAC

      N     = NINT (0.5 * FLOAT (NSTEP+1) - YOFF)
      FRAC  = SIGN (1.0,YOFF) * MOD(NSTEP+1,2)
      IFRAC = NINT (FRAC)
      YOFF2 = 2 * NINT (YOFF+FRAC*0.5) - IFRAC

C     Get the spectrum: By now we have have worked out appropriate sky offsets.
C     Get it from the cube directly as long as it is at least
C     there in interpolated form.

      IFAIL = 0
      CALL EXTRACT_HEADER (SCAN_HEADER)

      IF (      M.GE.1 .AND. M.LE.MSTEP
     &    .AND. N.GE.1 .AND. N.LE.NSTEP) THEN

         NDATA    = 4 * NPTS(1)
         MNOFFSET = 4 * ((N-1)*MSTEP + (M-1))
         CALL XCOPY (4,
     :        %VAL(CNF_PVAL(CURRENT_INDEX_ADDRESS)+MNOFFSET),
     :        MNPOS)

         IF (MNPOS.GE.0) THEN
            CALL XCOPY (NDATA,
     :           %VAL(CNF_PVAL(CURRENT_CUBE_ADDRESS)+MNOFFSET*NPTS(1) ),
     :           DATA)
         ELSE
            IFAIL = 56
         END IF

      ELSE
         IFAIL = 56
      END IF

      IF (IFAIL.NE.0) GO TO 99

C     Reconstitute R.A. and Dec. offsets from map centre

      LSCAN = 0
      CP    = COS (POS_ANGLE/57.29578)
      SP    = SIN (POS_ANGLE/59.29578)
      DRA   = 0.5*CELL_XSIZE*( + XOFF2*CP + YOFF2*SP)
      DDEC  = 0.5*CELL_YSIZE*( - XOFF2*SP + YOFF2*CP)

C     ...and then correct offset to header RA/Dec.

      CALL COPY_MAPPOS (RA, DEC, RAM, DECM, RA, DEC)

C  Tidy up and return

   99 CONTINUE

      IF (IFAIL.NE.0) THEN
         CALL POP
      END IF

      RETURN
      END

C-----------------------------------------------------------------------

