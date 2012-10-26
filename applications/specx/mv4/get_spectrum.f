*  History:
*     17 Dec 1993 (hme):
*        In order to adapt to new STACKCOMM/PROTOTYPE, no longer use
*        TSYS as start of scan header, since it isn't any more.
*     4 March 1997 (timj):
*        Set LSCAN to be non-zero to protect scans written to files
*      1 Aug 2000 (ajc):
*        Change TYPE * to PRINT *
*        Unused MCEN, I, NBYTES, APDEC, STATUS
C-----------------------------------------------------------------------

      SUBROUTINE GET_SPECTRUM (X, Y, POS, IFAIL)

C  Routine to fetch the spectrum from the map file at the nominated position
C  and place it in the scan stack. If IPOS.NE.0 then gets spectrum from
C  nominated position in file instead.
C  Input positions X and Y are in arcseconds in the map coordinate frame.
C  Thus restored values of DRA and DDEC need to be converted from X and Y
C  offsets.

      IMPLICIT NONE

*     Formal parameters:

      REAL*4    X,Y         !Position of desired point on MAP array
      INTEGER*4 POS         !Position in file to read from, if known
      INTEGER*4 IFAIL       !Error return

*     Include files

      INCLUDE 'FLAGCOMM'
      INCLUDE 'MAPHD'
      INCLUDE 'CUBE'
      INCLUDE 'MAPS'
      INCLUDE 'PLOT2D'
      INCLUDE 'STAKPAR'
      INCLUDE 'STACKCOMM'
      INCLUDE 'CNF_PAR'

*     Other variables

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

CD    Print *,'-- Get_spectrum --'
CD    Print *,'Current stack: JTOP,XCLEAR =',JTOP,XCLEAR

*     Make room on the stack for the new spectrum

      IF (.NOT.XCLEAR)  CALL PUSH

*     Mark the X-register as now containing a spectrum

      IF (JTOP.EQ.0) JTOP = 1
      XCLEAR = .FALSE.

CD    PRINT *, ' -- get_spectrum --'
CD    PRINT *, '    Stack pushed --'
CD    PRINT *, '    Current stack: JTOP,XCLEAR =',JTOP,XCLEAR

*     Work out offsets in cells if position
*     in file is not known.

CD    PRINT *, '    X, Y, POS: ', X, Y, POS

      IPOS = POS
      IF (IPOS.EQ.0) THEN

        XOFF = X/CELL_XSIZE
        YOFF = Y/CELL_YSIZE

CD      PRINT *, '    XOFF, YOFF =', XOFF, YOFF

*       Locate appropriate bin on map
*       Both coordinates run + to - (i.e. map starts at top left = NE)

        M     = NINT (0.5 * FLOAT (MSTEP+1) - XOFF)
        FRAC  = SIGN (1.0,XOFF) * MOD(MSTEP+1,2)
        IFRAC = NINT (FRAC)
        XOFF2 = 2 * NINT (XOFF+FRAC*0.5) - IFRAC

        N     = NINT (0.5 * FLOAT (NSTEP+1) - YOFF)
        FRAC  = SIGN (1.0,YOFF) * MOD(NSTEP+1,2)
        IFRAC = NINT (FRAC)
        YOFF2 = 2 * NINT (YOFF+FRAC*0.5) - IFRAC

CD      PRINT *, '    IPOS = 0, M,N =', M, N

      END IF

*     Get the spectrum: By now we have either got the input value of IPOS, or
*     have worked out appropriate sky offsets.
*     If the position in the file is given (in IPOS) then get the spectrum from
*     the map - else get it from the cube directly as long as it is at least
*     there in interpolated form.

      IFAIL = 0
      CALL EXTRACT_HEADER (SCAN_HEADER)

*     Get the spectrum from the map file

      IF (IPOS.NE.0) THEN
        CALL FROMAP (MCHAN, IPOS, XOFF2, YOFF2, DATA, NPTS(1),
     &               %VAL(CNF_PVAL(INDEX_ADDRESS)), IFAIL)

*     Get the spectrum from the cube (provided that the corresponding
*     spectrum is either there or interpolated).

      ELSE IF (      M.GE.1 .AND. M.LE.MSTEP
     &         .AND. N.GE.1 .AND. N.LE.NSTEP) THEN

        NDATA    = 4 * NPTS(1)
        MNOFFSET = 4 * ((N-1)*MSTEP + (M-1))
        CALL XCOPY (4, %VAL(CNF_PVAL(INDEX_ADDRESS)+MNOFFSET), MNPOS)

CD      PRINT *, '    M,N,NDATA;',  M, N, NDATA
CD      PRINT *, '    MNPOS; ', MNPOS

        IF (MNPOS.GE.0) THEN
          CALL XCOPY (NDATA,
     :         %VAL(CNF_PVAL(CUBE_ADDRESS)+MNOFFSET*NPTS(1)), DATA)
        ELSE
          IFAIL = 56
        END IF

      ELSE
        IFAIL = 56
      END IF

      IF (IFAIL.NE.0) GO TO 99

*     Reconstitute R.A. and Dec. offsets from map centre

*     Set scan number to position in file (if use 0 then scans are treated
*     as being deleted as soon as they are written to data files)
      IF (IPOS .NE. 0) THEN
         LSCAN = IPOS
      ELSE
         LSCAN = 1
      ENDIF
      CP    = COS (POS_ANGLE/57.29578)
      SP    = SIN (POS_ANGLE/59.29578)
      DRA   = 0.5*CELL_XSIZE*( + XOFF2*CP + YOFF2*SP)
      DDEC  = 0.5*CELL_YSIZE*( - XOFF2*SP + YOFF2*CP)

*     And then correct offset to header RA/Dec.
*     Note: use prototype header positions only if map positions
*           are still zero --- otherwise COPY_MAPPOS will reset RA and DEC.

      CALL COPY_MAPPOS (RA, DEC, RAM, DECM, RA, DEC)

*     Tidy up and return

   99 CONTINUE

      IF (IFAIL.NE.0) THEN
        CALL POP
      END IF

CD    PRINT *, '    Before return: IFAIL = ', IFAIL
CD    PRINT *, '    Current stack: JTOP,XCLEAR =',JTOP,XCLEAR

      RETURN
      END

C-----------------------------------------------------------------------
