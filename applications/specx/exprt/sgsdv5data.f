C-----------------------------------------------------------------

      SUBROUTINE SPECX_GSD_V5_DATA (INDEX, IRX, IERR)

C   Routine to select data for specified spectrum (# in file) and
C   place in SPECX stack for processing. Works on GSD version 5 data
C   data files, whereby it fills stack positions according to IF
C   inputs measured and output frequency resolution.

C   22-NOV-1991: REVAD::JFL Original version.
C   18-Nov-1991: JCMT::RMP  Incorporate Rachaels latest fixes (to IDRA)
C   08-MAY-2000 AJC
C      Port to Linux
C      Replace TYPE with PRINT
C      Don't split strings across lines
C      Unused NO_QUADS, NQ, NSEG
C      Initialised CONNECTED to .FALSE. and NIF to 0 (so Linux behaves
C       the same as other platforms)
C   22-AUG-2005 TIMJ: Use DATA for init
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
      INTEGER*4 NO
      CHARACTER UNITS*15
      CHARACTER TYPE*1
      CHARACTER ALPHABET*8
      CHARACTER NUMBERS*8

      LOGICAL MATCH
      LOGICAL CONNECTED
      INTEGER*4 IND_GSD(GSD__SZINDEX)
      INTEGER*4 JJ
      INTEGER*4 NO_BE_O_CH
      INTEGER*4 NO_IF_PER_BES
      INTEGER*4 NIF
      INTEGER*4 IFIN (2,16)
      INTEGER*4 BES_CONN (32)
      INTEGER*4 NO_BES_O_CH (16)
      INTEGER*4 CSTART (2)
      INTEGER*4 CEND (2)
      INTEGER*4 CDIMVAL (2)
      INTEGER*4 ACTVALS (2)
      DOUBLE PRECISION FR_TEMP (16)
      DOUBLE PRECISION RESIN (16)

      DATA ALPHABET /'ABCDEFGH'/
      DATA NUMBERS  /'12345678'/

      IERR = 0
      STATUS = ADAM__OK
      CONNECTED = .FALSE.
      NIF = 0

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

C  Find the total number of output channels from the backend

      CALL GSD_FIND (IFD, 'C3NCH',
     :               NO, UNITS, TYPE, ARRAY, IND_GSD, STATUS)
      CALL GSD_GET0I (IND_GSD, NO_BE_O_CH, STATUS)

C  Find the number of IF inputs to each back-end section

      CALL GSD_FIND (IFD, 'C3NOIFPBES',
     :               NO, UNITS, TYPE, ARRAY, IND_GSD, STATUS)
      CALL GSD_GET0I (IND_GSD, NO_IF_PER_BES, STATUS)

C  Find and read in the backend section connection array

      CALL GSD_FIND (IFD, 'C3BESCONN',
     :               NO, UNITS, TYPE, ARRAY, IND_GSD, STATUS)
      CDIMVAL (1) = NO_IF_PER_BES
      CDIMVAL (2) = NRC
      CSTART (1) = 1
      CSTART (2) = 1
      CEND (1) = NO_IF_PER_BES
      CEND (2) = NRC
      CALL GSD_GET1I (IND_GSD, 2, CDIMVAL, CSTART, CEND, BES_CONN,
     :   ACTVALS, STATUS)

C  Find and read in the backend section resolution array

      CALL GSD_FIND (IFD, 'C12FR',
     :   NO, UNITS, TYPE, ARRAY, IND_FR, STATUS)
      CALL GSD_GET1D (IND_FR, 1, NRC, 1, NRC, FR_TEMP, LVAL, STATUS)


C  Find how many different IF inputs / spectrum resolution (or combinations
C  of IF inputs if NOBESIN = 2) combinations were used, and note the `input/
C  resolution' s that were used in the IFIN and RESIN arrays.

      IF (NO_IF_PER_BES .EQ. 1) THEN

         NIF = 1
         IFIN (1,NIF) = BES_CONN (1)
         IFIN (2,NIF) = 0
         RESIN (NIF) = FR_TEMP (1)

         DO J = 2, NRC
            MATCH = .FALSE.
            DO I = 1, NIF
               IF ((BES_CONN(J) .EQ. IFIN(1,I)) .AND.
     :             (FR_TEMP(J) .EQ. RESIN(I)))  THEN
                  MATCH = .TRUE.
               END IF
            END DO
            IF (.NOT. MATCH) THEN
               NIF = NIF + 1
               IFIN (1,NIF) = BES_CONN (J)
               IFIN (2,NIF) = 0
               RESIN (NIF) = FR_TEMP (J)
            END IF
         END DO

      ELSE IF (NO_IF_PER_BES .EQ. 2) THEN

         NIF = 1
         IFIN (1,NIF) = BES_CONN (1)
         IFIN (2,NIF) = BES_CONN (2)
         RESIN (NIF) = FR_TEMP (1)

         DO J = 2, NRC
            MATCH = .FALSE.
            JJ = 1 + 2*(J-1)
            DO I = 1, NIF
               IF ((BES_CONN(JJ) .EQ. IFIN(1,I))   .AND.
     :             (BES_CONN(JJ+1) .EQ. IFIN(2,I)) .AND.
     :             (FR_TEMP(J) .EQ. RESIN (I)))    THEN
                  MATCH = .TRUE.
               END IF
            END DO
            IF (.NOT. MATCH) THEN
               NIF = NIF + 1
               IFIN (1,NIF) = BES_CONN (JJ)
               IFIN (2,NIF) = BES_CONN (JJ+1)
               RESIN (NIF) = FR_TEMP (J)
            END IF
         END DO

      END IF

CD    write (ilout,*) 'no_if_per_bes, nrc ', no_if_per_bes, nrc
CD    write (ilout,*) 'bes_conn ', (bes_conn(i), i = 1,
CD   :   no_if_per_bes*nrc)
CD    write (ilout,*) 'fr_temp ', (fr_temp(i), i = 1, nrc)
CD    write (ilout,*) 'nif ', nif
CD    write (ilout,*) 'ifin, resin ', ((ifin(i,j), i=1,2),
CD   :   resin(j), j=1,nif)

C  Find indices to all other arrays

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
     &               ''') arcsec'')')  dra, ddec

C  Fix up integration time for last spectrum

      IF (INDEX .EQ. NGSDSPEC) INTT = NINT(1000.*INT_TIME_LAST)

      NO_NEWSPEC = 0
      KNT = 0

      DO NS = 1, NIF                    ! loop through IF inputs

         NQUAD = 0                      ! the spectra associated with each IF
         JOFF = 1                       ! input go into a separate spectrum on
                                        ! the stack

         DO QUAD = 1, NRC               ! loop through all back-end quadrants

*  see if this quadrant was connected to the current IF input and has matching
*  frequency resolution
            IF (NO_IF_PER_BES .EQ. 1) THEN
               IF ((BES_CONN(QUAD) .EQ. IFIN(1,NS)) .AND.
     :             (FR_TEMP(QUAD) .EQ. RESIN(NS)))  THEN
                  CONNECTED = .TRUE.
               ELSE
                  CONNECTED = .FALSE.
               END IF
            ELSE IF (NO_IF_PER_BES .EQ. 2) THEN
               JJ = 1 + 2*(QUAD-1)
               IF ((BES_CONN(JJ) .EQ. IFIN(1,NS))    .AND.
     :             (BES_CONN(JJ+1) .EQ. IFIN(2,NS))  .AND.
     :             (FR_TEMP(QUAD) .EQ. RESIN(NS)))   THEN
                  CONNECTED = .TRUE.
               ELSE
                  CONNECTED = .FALSE.
               END IF
            END IF

*  go on to next quadrant if this one is not connected to the current IF,
*  otherwise...

            IF (CONNECTED) THEN

*  issue warning if 8 quadrants (current SPECX limit) have already been
*  read in

               IF (NQUAD .EQ. 8) THEN
                  PRINT *, 'Only first 8 quadrants read in'
                  GOTO 100
               END IF

               NQUAD = NQUAD + 1

*  Now get appropriate array elements for this quadrant within each spectrum
*  ( system temperatures etc are the same for all spectra )

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

*  check not going to overflow data array

               IF ((JOFF+NPTS(NQUAD)-1) .GT. LSTK-LHEAD) THEN
                  WRITE (ILOUT,*) 'Error # 25'
                  IERR = 25
                  GO TO 99
               END IF

*  dimensions of stored data array

               DIMVAL_DATA(1) = NO_BE_O_CH
               DIMVAL_DATA(2) = 1
               DIMVAL_DATA(3) = INDEX

CD             write (ilout,*) 'no_be_o_ch, index ', no_be_o_ch, index


*  section of stored data array that we want

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

*  get the data

               CALL GSD_GET1R (IND_DATA, 3, DIMVAL_DATA, IBEGIN, IENDS,
     :           DATA(JOFF), LVAL, STATUS)

*  increment the data pointer to point to the next free position in the data
*  array

               JOFF = JOFF + NPTS(NQUAD)

            END IF
         END DO

 100     CONTINUE

C  Order quadrants by increasing frequency (necessary so that you can
C  average ChA and ChB data for example)

         IF (NQUAD .GT. 1) THEN
            CALL SPECX_JSORT   (NQUAD, JFCEN, ISOURCE)
            CALL SPECX_REORDER (NQUAD, ISOURCE)
         END IF

C  Label scan with IF number if required

         IF (NIF .GT. 1) THEN
            WRITE (ITITLE(10:11),'(''_'',A1)') ALPHABET(NS:NS)
         END IF

C  Then make room for another scan if necessary

         NO_NEWSPEC = NO_NEWSPEC + 1

         IF (NS .LT. NIF) THEN
           CALL PUSH
           KNT = KNT + 1
         END IF

      END DO

   90 KNT = KNT + 1

C     Print out a picture of the stack with new spectra showing...

      NBYTES = 4*LSTK
      CALL IGETVM  (NBYTES, .TRUE., 'SPECX_GSD_V5_DATA', IPTR)
      CALL DISPST  (KNT, .FALSE., %VAL(CNF_PVAL(IPTR)))
      CALL IFREEVM (IPTR)

C  Standard return

   99 IF (STATUS.NE.ADAM__OK) THEN
        PRINT '('' Status'',2X,I10,4X,''($'',Z8.8,'')'')', status,status
      END IF

      END
