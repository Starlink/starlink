C-----------------------------------------------------------------

      SUBROUTINE SPECX_GSD_V4_DATA (INDEX, IRX, IERR)

C   Routine to select data for specified spectrum (# in file) and
C   place in SPECX stack for processing

C   RMP 18 Nov 1991 - put in Rachael's latest mods to John's code
C   AJC  8 May 2000 - Port to Unix
C                     Replace TYPE with PRINT
C                     Don't split string constants across lines
C                     Unused I
C   TIMJ 22 Aug 2005 - Init using DATA statements
C   26-OCT-2012 TIMJ: Use CNF
      IMPLICIT  NONE

C   Formal parameters

      INTEGER*4 INDEX
      INTEGER*4 IRX
      INTEGER*4 IERR

      INCLUDE  'CNF_PAR'
      INCLUDE  'STAKPAR'
      INCLUDE  'STACKCOMM'
      INCLUDE  'FLAGCOMM'
      INCLUDE  'SPECX_PARS'
      INCLUDE  'GSD_VAR.INC'
      INCLUDE  'GSD_FILE.INC'
      INCLUDE  'GSD_INDEX.INC'

      INTEGER*4 ADAM__OK
      PARAMETER (ADAM__OK=0)

      INTEGER*4 IBEGIN(3)
      INTEGER*4 DIMVAL_DATA(3)
      INTEGER*4 IENDS(3)

      INTEGER*4 IPTR
      INTEGER*4 ISOURCE(NQMAX)
      INTEGER*4 J
      INTEGER*4 JOFF
      INTEGER*4 KNT
      INTEGER*4 LVAL
      INTEGER*4 NBYTES
      INTEGER*4 NO_QUADS
      INTEGER*4 NQ
      INTEGER*4 NS
      INTEGER*4 NSEG
      INTEGER*4 QUAD
      INTEGER*4 STATUS
      REAL*4    X2Y_RAD
      REAL*4    V2Y_RAD
      REAL*4    X_OFFSET
      REAL*4    Y_OFFSET
      REAL*8    CF
      REAL*8    RF
      REAL*8    FR

      LOGICAL   ARRAY
      INTEGER*4 NO
      CHARACTER UNITS*15
      CHARACTER TYPE*1
      CHARACTER ALPHABET*8
      CHARACTER NUMBERS*8

      DATA ALPHABET /'ABCDEFGH'/
      DATA NUMBERS  /'12345678' /

      IERR = 0
      STATUS = ADAM__OK

CD    WRITE (ILOUT,*) '-- SPECX_GSD_V4_DATA --'
CD    WRITE (ILOUT,*) '    INDEX =',INDEX,' IRX =',IRX

C  Test we are not asking for a scan which is not there

      IF (INDEX.GT.NGSDSPEC .OR. INDEX.LE.0) THEN
        IERR = 18
        RETURN
      END IF

C  Clear the data array

      DO J = 1, LSTK-128
        DATA(J) = 0.0
      END DO

C  Find indices to all arrays

      CALL GSD_FIND (IFD, 'c13dat',
     &               NO, UNITS, TYPE, ARRAY, IND_DATA, STATUS)
      CALL GSD_FIND (IFD, 'C12SST',
     &               NO, UNITS, TYPE, ARRAY, IND_TSYS, STATUS)
      CALL GSD_FIND (IFD, 'C12RT',
     &               NO, UNITS, TYPE, ARRAY, IND_TREC, STATUS)
      CALL GSD_FIND (IFD, 'c12ttel',
     &               NO, UNITS, TYPE, ARRAY, IND_TTEL, STATUS)
      CALL GSD_FIND (IFD, 'c12tsky',
     &               NO, UNITS, TYPE, ARRAY, IND_TSKY, STATUS)
      CALL GSD_FIND (IFD, 'C12RF',
     &               NO, UNITS, TYPE, ARRAY, IND_RF, STATUS)
      CALL GSD_FIND (IFD, 'C12FR',
     &               NO, UNITS, TYPE, ARRAY, IND_FR, STATUS)
      CALL GSD_FIND (IFD, 'C12RF',
     &               NO, UNITS, TYPE, ARRAY, IND_RF, STATUS)
      CALL GSD_FIND (IFD, 'C12CF',
     &               NO, UNITS, TYPE, ARRAY, IND_CF, STATUS)


C  How many quadrants per stack position?

      NO_QUADS = NRC/NRF

C  Set offsets in spectrum header
C    ..first get x and y offsets in arcsecs

      X_OFFSET = PHIST(1,MAX(1,INDEX))
      Y_OFFSET = PHIST(2,MAX(1,INDEX))
*---------------------
*     Bug in next 4 lines (discovered by Per Friberg and Richard Prestage)
*     Can't remember why they were ever put in, so just comment out for now.
*     RP - 18-8-90
*     IF (X_OFFSET.EQ.0.0 .AND. Y_OFFSET.EQ.0.0) THEN
*       X_OFFSET = XMAP_OFF
*       Y_OFFSET = YMAP_OFF
*     END IF
*---------------------

C  (This para is all messed up thanks to indiscriminate changes of data
C   formats from JACH end. However above code, as suggested by RMP, looks as
C   though it will do the trick for now!)

CD    WRITE (ILOUT,*) 'x,y cell sizes      ', sngl(dx), sngl(dy)
CD    WRITE (ILOUT,*) 'x,y offsets (cells) ', x_offset, y_offset
      X_OFFSET = X_OFFSET * DX
      Y_OFFSET = Y_OFFSET * DY
      WRITE (ILOUT,'(10X,''(x,y) offset = ('',F6.1,'','',F6.1,'//
     &               ''') arcsec'')')  x_offset, y_offset
      WRITE (ILOUT,'(10X,''rotation angles: x2y = '',F6.1,'//
     &               ''' deg.; v2y = '',F6.1,'' deg.'')') X2Y, V2Y

C    ..then convert to R.A. and Dec. offsets

      V2Y_RAD = 1.74533e-2 * V2Y                               ! Radians
      X2Y_RAD = 1.74533e-2 * X2Y
      DRA     =   SIN (V2Y_RAD - X2Y_RAD) * X_OFFSET
     &          + SIN (V2Y_RAD)           * Y_OFFSET
      DDEC    =   COS (V2Y_RAD - X2Y_RAD) * X_OFFSET
     &          + COS (V2Y_RAD)           * Y_OFFSET
      WRITE (ILOUT,'(10X,''(r,d) offset = ('',F6.1,'','',F6.1,'//
     &               ''') arcsec'')')  dra, ddec

C  Fix up integration time for last spectrum

      IF (INDEX .EQ. NGSDSPEC) INTT = NINT(1000.*INT_TIME_LAST)

C  Now get appropriate array elements for each quadrant within each spectrum
C  ( system temperatures etc are the same for all spectra )

      NO_NEWSPEC = 0

      KNT = 0

      DO NS = 1, NRF
        NQUAD = 0
        DO QUAD = 1, MIN (NO_QUADS,NRC-NO_QUADS*(NS-1))
          NSEG = (NS-1)*NO_QUADS + QUAD

          IF (IRX.EQ.0 .OR. NSEG.EQ.IRX) THEN

            IF (IRX.EQ.0) THEN
              NQ    = QUAD
              NQUAD = NQUAD + 1
            ELSE
              NQ    = 1
              NQUAD = 1
            END IF

            CALL GSD_GET1R (IND_TSYS, 1, NRC, NSEG, NSEG,
     &                      TSYS(NQ), LVAL, STATUS)
            CALL GSD_GET1I (IND_TREC, 1, NRC, NSEG, NSEG,
     &                      ITREC(NQ), LVAL, STATUS)
            CALL GSD_GET1I (IND_TTEL, 1, NRC, NSEG, NSEG,
     &                      ITTEL(NQ), LVAL, STATUS)
            CALL GSD_GET1I (IND_TSKY, 1, NRC, NSEG, NSEG,
     &                      ITSKY(NQ), LVAL, STATUS)
            CALL GSD_GET1D (IND_FR, 1, NRC, NSEG, NSEG,
     &                      FR, LVAL, STATUS)
            CALL GSD_GET1D (IND_RF, 1, NRC, NSEG, NSEG,
     &                      RF, LVAL, STATUS)
            CALL GSD_GET1D (IND_CF, 1, NRC, NSEG, NSEG,
     &                      CF, LVAL, STATUS)

            JFREST(NQ) = 10**6 * RF     ! GHz -> kHz
            JFCEN(NQ)  = 10**6 * CF     ! GHz -> kHz
            JFINC(NQ)  = 10**6 * FR     ! MHz -> Hz
            NPTS(NQ)   = LSPC

            JOFF      = (NQ-1)*LSPC + 1

            IF (NQ*LSPC.GT.LSTK-LHEAD) THEN
              WRITE (ILOUT,*) 'Error # 25'
              IERR = 25
              GO TO 99
            END IF

            DIMVAL_DATA(1) = LSPC
            DIMVAL_DATA(2) = NRC
            DIMVAL_DATA(3) = INDEX

            IBEGIN(1)  = 1        ! 1 -> LSPC
            IBEGIN(2)  = NSEG     ! 1 -> NRC
            IBEGIN(3)  = INDEX    ! 1 -> NGSDSPEC
            IENDS(1)   = LSPC
            IENDS(2)   = NSEG
            IENDS(3)   = INDEX

            CALL GSD_GET1R (IND_DATA, 3, DIMVAL_DATA, IBEGIN, IENDS,
     &                      DATA(JOFF), LVAL, STATUS)

            IF (IRX.EQ.NSEG) THEN
              WRITE (ITITLE(10:11),'(''_'',A1)') NUMBERS(NS:NS)
              GO TO 90
            END IF

          END IF
        END DO

C  Order quadrants by increasing frequency (necessary so that you can
C  average ChA and ChB data for example)

        IF (IRX.EQ.0) THEN
          IF (NQUAD .GT. 1) THEN
            CALL SPECX_JSORT   (NQUAD, JFCEN, ISOURCE)
CD          WRITE (ILOUT,*) 'I.F. channels reordered'
CD          WRITE (ILOUT,'('' New channel order: '',8(1X,I2))')
CD   &                (ISOURCE(J),J=1,NQUAD)
            CALL SPECX_REORDER (NQUAD, ISOURCE)
          END IF
        END IF

C  Label scan with receiver number if required

        IF (NRF.GT.1 .AND. IRX.EQ.0) THEN
          WRITE (ITITLE(10:11),'(''_'',A1)') ALPHABET(NS:NS)
        END IF

C  Then make room for another scan if necessary

        NO_NEWSPEC = NO_NEWSPEC + 1

        IF (IRX.EQ.0 .AND. NS.LT.NRF) THEN
          CALL PUSH
*         Old 1000 print statement
          KNT = KNT + 1
        END IF

      END DO
*     Old 1001 print statement
   90 KNT = KNT + 1

C     Print out a picture of the stack with new spectra showing...

      NBYTES = 4*LSTK
      CALL IGETVM  (NBYTES, .TRUE., 'SPECX_GSD_V4_DATA', IPTR)
      CALL DISPST  (KNT, .FALSE., %VAL(CNF_PVAL(IPTR)))
      CALL IFREEVM (IPTR)

C  Standard return

   99 IF (STATUS.NE.ADAM__OK) THEN
        PRINT '('' Status'',2X,I10,4X,''($'',Z8.8,'')'')', status,status
      END IF

*     WRITE (ILOUT,1000) ITITLE, NQUAD
*1000 FORMAT (' Scan pushed onto stack:  ',A26,' (',I2,' sector(s))')
*1001 FORMAT (' Scan now in X-register:  ',A26,' (',I2,' sector(s))')

      RETURN
      END

C-------------------------------------------------------------------------

