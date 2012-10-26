C-----------------------------------------------------------------

      SUBROUTINE SPECX_GSD_V51_DATA (INDEX, IRX, IERR)

C   Routine to select data for specified spectrum (# in file) and
C   place in SPECX stack for processing. Works on data files in
C   GSD version 5.1 format, where the stack position of each input
C   quadrant is specified by the GSD array C3BESSPEC.

C   22-NOV-1991 REVAD::JFL Original version.
C   18-NOV-1991 JCMT::RMP  Incorporate Rachael's latest mods (to IDRA)
C   04-AUG-1992 JCMT::CJM  Minor mod to get round problem of missing C10LO
C    8-MAY-2000 AJC Port to Linux
C               Replace TYPE with PRINT
C               Don't split strings across lines
C               Unused NO_QUADS, NQ, NSEG, JJ, BES_CONN, CSTART, CEND, CDIMVAL
C   22-AUG-2005 TIMJ: Init using DATA
C   26-OCT-2012 TIMJ: Use CNF
C-
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

      INTEGER*4 I
      INTEGER*4 IPTR
      INTEGER*4 ISOURCE(NQMAX)
      INTEGER*4 J
      INTEGER*4 JOFF, JBEG, JEND
      INTEGER*4 KNT
      INTEGER*4 LVAL
      INTEGER*4 NBYTES
      INTEGER*4 NS
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
      LOGICAL   MATCH
      INTEGER*4 NO
      CHARACTER UNITS*15
      CHARACTER TYPE*1
      CHARACTER ALPHABET*8
      CHARACTER NUMBERS*8

      INTEGER*4 IND_GSD(GSD__SZINDEX)
      INTEGER*4 IND_BESSPEC(GSD__SZINDEX)
      INTEGER*4 NSPEC
      INTEGER*4 NO_BE_O_CH
      INTEGER*4 BES_SPECTRUM (16)
      INTEGER*4 NO_BES_O_CH (16)
      INTEGER*4 SUBSYS (16)
      INTEGER*4 ACTVALS (2)

      DATA ALPHABET /'ABCDEFGH'/
      DATA NUMBERS  /'12345678'/

      IERR = 0
      STATUS = ADAM__OK

CD    WRITE (ILOUT,*) '-- SPECX_GSD_V5_DATA --'
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

C  Find and read in the array specifying which subsystem each quadrant
C  (subband)is connected

      CALL GSD_FIND (IFD, 'C3BESSPEC',
     :   NO, UNITS, TYPE, ARRAY, IND_BESSPEC, STATUS)
      CALL GSD_GET1I (IND_BESSPEC, 1, NRC, 1, NRC, BES_SPECTRUM,
     :   ACTVALS, STATUS)

C  Find how many subsystems there are

      NSPEC = 1
      SUBSYS (1) = BES_SPECTRUM (1)
      IF (NRC .GT. 1) THEN
         DO J = 2, NRC
            MATCH = .FALSE.
            DO I = 1, NSPEC
               IF (BES_SPECTRUM(J) .EQ. SUBSYS(I)) THEN
                  MATCH = .TRUE.
               END IF
            END DO
            IF (.NOT. MATCH) THEN
               NSPEC = NSPEC + 1
               SUBSYS (NSPEC) = BES_SPECTRUM (J)
            END IF
         END DO
      END IF

CD    write (ilout,*) 'nspec ', nspec
CD    write (ilout,*) 'subsys ', (subsys(i), i = 1, nspec)
CD    write (ilout,*) 'bes_spectrum ', (bes_spectrum(i), i = 1, nrc)

C     Find the total number of output channels from the frontend

      CALL GSD_FIND (IFD, 'C3NFOC',
     :               NO, UNITS, TYPE, ARRAY, IND_GSD, STATUS)
      CALL GSD_GET0I (IND_GSD, NRF, STATUS)

C     Find the total number of output channels from the backend

      CALL GSD_FIND (IFD, 'C3NCH',
     :               NO, UNITS, TYPE, ARRAY, IND_GSD, STATUS)
      CALL GSD_GET0I (IND_GSD, NO_BE_O_CH, STATUS)

C     Find indices to all other arrays

      CALL GSD_FIND (IFD, 'C13DAT',
     &               NO, UNITS, TYPE, ARRAY, IND_DATA, STATUS)
      CALL GSD_FIND (IFD, 'C12SST',
     &               NO, UNITS, TYPE, ARRAY, IND_TSYS, STATUS)
      CALL GSD_FIND (IFD, 'C12RT',
     &               NO, UNITS, TYPE, ARRAY, IND_TREC, STATUS)
      CALL GSD_FIND (IFD, 'C12TTEL',
     &               NO, UNITS, TYPE, ARRAY, IND_TTEL, STATUS)
      CALL GSD_FIND (IFD, 'C12TSKY',
     &               NO, UNITS, TYPE, ARRAY, IND_TSKY, STATUS)
      CALL GSD_FIND (IFD, 'C12RF',
     &               NO, UNITS, TYPE, ARRAY, IND_RF, STATUS)
      CALL GSD_FIND (IFD, 'C12CF',
     &               NO, UNITS, TYPE, ARRAY, IND_CF, STATUS)
      CALL GSD_FIND (IFD, 'C12FR',
     &               NO, UNITS, TYPE, ARRAY, IND_FR, STATUS)
      IF (STATUS.NE.ADAM__OK) THEN
        STATUS = ADAM__OK
      END IF

C  find and read in the number of points per section array

      CALL GSD_FIND (IFD, 'C3LSPC',
     :               NO, UNITS, TYPE, ARRAY, IND_GSD, STATUS)
      CALL GSD_GET1I (IND_GSD, 1, NRC, 1, NRC,
     :               NO_BES_O_CH, LVAL, STATUS)

C  Set offsets in spectrum header
C    ..first get x and y offsets in arcsecs

      X_OFFSET = PHIST(1,MAX(1,INDEX))
      Y_OFFSET = PHIST(2,MAX(1,INDEX))

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
     &               ''') arcsec'')')  DRA, DDEC

C  Fix up integration time for last spectrum

      IF (INDEX .EQ. NGSDSPEC) INTT = NINT(1000.*INT_TIME_LAST)

      NO_NEWSPEC = 0
      KNT = 0

C     Now get the data: loop through sub-bands/sectors/quadrants, which
C     are assumed to be sequential starting at 0.

      DO NS = 0, NSPEC-1


        NQUAD = 0
        JOFF = 1

C       Loop through all the backend quadrants

        DO QUAD = 1, NRC

C           see if this quadrant was connected to the current spectrum (subsystem)

            IF (BES_SPECTRUM (QUAD) .EQ. NS) THEN

C              check that 8 quadrants haven't already been read in

               IF (NQUAD .EQ. 8) THEN
                  PRINT *, 'Only first 8 quadrants read in'
                  GOTO 100
               END IF

               NQUAD = NQUAD + 1

C              Now get appropriate array elements for this quadrant within
C              each spectrum  ( system temperatures etc are the same for
C              all spectra )

               CALL GSD_GET1R (IND_TSYS, 1, NRC, QUAD, QUAD,
     :           TSYS(NQUAD), LVAL, STATUS)
               CALL GSD_GET1I (IND_TREC, 1, NRC, QUAD, QUAD,
     :           ITREC(NQUAD), LVAL, STATUS)
               CALL GSD_GET1I (IND_TTEL, 1, NRC, QUAD, QUAD,
     :           ITTEL(NQUAD), LVAL, STATUS)
               CALL GSD_GET1I (IND_TSKY, 1, NRC, QUAD, QUAD,
     :           ITSKY(NQUAD), LVAL, STATUS)
               CALL GSD_GET1D (IND_FR, 1, NRC, QUAD, QUAD,
     :           FR, LVAL, STATUS)
               CALL GSD_GET1D (IND_RF, 1, NRC, QUAD, QUAD,
     :           RF, LVAL, STATUS)
               CALL GSD_GET1D (IND_CF, 1, NRC, QUAD, QUAD,
     :           CF, LVAL, STATUS)

               JFREST(NQUAD) = 1.0E6 * RF     ! GHz -> kHz
               JFCEN(NQUAD)  = 1.0E6 * CF     ! GHz -> kHz
               JFINC(NQUAD)  = 1.0E6 * FR     ! MHz -> Hz
               NPTS(NQUAD)   = NO_BES_O_CH(QUAD)

C              check not going to overflow data array

               IF ((JOFF+NPTS(NQUAD)-1) .GT. LSTK-LHEAD) THEN
                  WRITE (ILOUT,*) 'Error # 25'
                  IERR = 25
                  GO TO 99
               END IF

C              dimensions of stored data array

               DIMVAL_DATA(1) = NO_BE_O_CH
               DIMVAL_DATA(2) = 1
               DIMVAL_DATA(3) = INDEX

CD             write (ilout,*) 'no_be_o_ch, index ', no_be_o_ch, index


C              section of stored data array that we want

               IF (QUAD .EQ. 1) THEN
                  JBEG = 1
                  JEND = NO_BES_O_CH (1)
               ELSE
                  JBEG = 1
                  JEND = NO_BES_O_CH (1)
                  DO I = 2, QUAD
                    JBEG = JBEG + NO_BES_O_CH(I-1)
                    JEND = JEND + NO_BES_O_CH(I)
                  END DO
               END IF
               IBEGIN(1)  = JBEG                 ! area of spectrum we want
               IENDS(1) = JEND
               IBEGIN(2) = 1
               IENDS(2) = 1
               IBEGIN(3) = INDEX                 ! spectrum number in file
               IENDS(3) = INDEX

CD             write(ilout,*) 'index, nquad, quad, jbeg, jend ', index,
CD   :            nquad, quad, jbeg, jend

C              get the data

               CALL GSD_GET1R (IND_DATA, 3, DIMVAL_DATA, IBEGIN, IENDS,
     :           DATA(JOFF), LVAL, STATUS)

C              increment the data pointer to point to the next free position
C              in the data array

               JOFF = JOFF + NPTS(NQUAD)

            END IF
         END DO

 100     CONTINUE

C        Order quadrants by increasing frequency (necessary so that you can
C        average ChA and ChB data for example)

         IF (NQUAD .GT. 1) THEN
            CALL SPECX_JSORT   (NQUAD, JFCEN, ISOURCE)
            CALL SPECX_REORDER (NQUAD, ISOURCE)
         END IF

C        Label scan with IF number if required

         IF (NSPEC .GT. 1) THEN
            WRITE (ITITLE(10:11),'(''_'',A1)') ALPHABET(NS+1:NS+1)
         END IF

C        Then make room for another scan if necessary

         NO_NEWSPEC = NO_NEWSPEC + 1

         IF (NS .LT. NSPEC-1) THEN
           CALL PUSH
           KNT = KNT + 1
         END IF

      END DO

   90 KNT = KNT + 1

C     Print out a picture of the stack with new spectra showing...

      NBYTES = 4*LSTK
      CALL IGETVM  (NBYTES, .TRUE., 'SPECX_GSD_V51_DATA', IPTR)
      CALL DISPST  (KNT, .FALSE., %VAL(CNF_PVAL(IPTR)))
      CALL IFREEVM (IPTR)

C     Standard return

   99 IF (STATUS.NE.ADAM__OK) THEN
        PRINT '('' Status'',2X,I10,4X,''($'',Z8.8,'')'')', status,status
      END IF

      END
