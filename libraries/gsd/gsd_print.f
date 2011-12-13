      PROGRAM GSD_PRINT
*+
*  Name:
*     gsd_print

*  Purpose:
*     Program to PRINT a GSD file

* Copyright:
*    Copyright (C) 1986-1999 Particle Physics and Astronomy Research Council.
*    All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*    Jon Fairclough
*    Horst Meyerdierks

*  History:
*    11-Nov-1986 (JF):
*        Original.
*    06-Dec-1994 (HME):
*        Ported to Unix, as tutorial.

*  Notes:
*    This programme is being ported as a test and tutorial for the new
*    GSD library. There is a C version of this programme, which should
*    be used instead.
*
*    Firstly, ADAMERRS should not be included. ADAM__OK is zero.
*    Secondly, GSD_PAR must be made a symbolic link to the actual
*    include file, e.g. "ln -s gsd_par GSD_PAR". The link can be
*    removed after compilation of the Fortran source code.
*    Thirdly, GSD_ERR does not exist. GSD returns zero as good status
*    and something else as bad status. That is usually enough for the
*    GSD-calling code to know.
*
*    With those changes this programme compiles. It must be linked with
*    the GSD library, and also with Starlink's CNF library, like:
*    "f77 -g gsd_print.f libgsd.a -L/star/lib `cnf_link`"
*
*    As it was the code would then have unresolved symbols:
*    lib$get_foreign, lib$get_lun, lib$signal.
*
*    With these calls eliminated, the programme links. But it will not
*    run properly, because the new library is lacking some features
*    used. Here are the problems encountered with this programme.
*
*    The new library opens a file given its full name, .DAT is no longer
*    appended by GSD_OPEN_READ. In this programme that upsets the
*    construction of the list file, which would become file.dat.LIS.
*
*    This programme uses the feature whereby any type of data could be
*    converted to any other type within the GSD library. These days
*    programmes must use the information they have about the data types,
*    use the correct GSD_GETnx routine, and convert themselves.
*
*    This programme also uses strings that are 25 characters long. With
*    the new library a string array can be returned only if the strings
*    are declared exactly 16 characters long.
*

*-
      IMPLICIT NONE
*
      INCLUDE 'PRM_PAR'       ! Bad values
      INCLUDE 'GSD_PAR'
*
*    Print file
      INTEGER LUN
      CHARACTER*80 PRINTFILE
      INTEGER PRLEN
*    Arguments for GSD_OPEN_READ
      CHARACTER*80 FILENAME
      INTEGER FILELEN
      INTEGER FD
      REAL VERSION
      CHARACTER*30 LABEL
      INTEGER NITEM
      INTEGER STATUS/0/
*    Arguments for GSD_ITEM
      INTEGER NUMBER/0/
      CHARACTER*(GSD__SZNAME) NAME
      CHARACTER*(GSD__SZUNIT) UNIT
      CHARACTER TYPE
      LOGICAL TABLE
      INTEGER GSDINDEX(GSD__SZINDEX)        ! 5
*    Arguments for GSD_INQ_SIZE
      INTEGER SIZE
      INTEGER MAXDIMS/GSD__MXDIM/        ! GSD__MXDIM=5
      CHARACTER*(GSD__SZNAME) DIMNAMES(GSD__MXDIM)
      CHARACTER*(GSD__SZUNIT) DIMUNITS(GSD__MXDIM)
      INTEGER DIMVALS(GSD__MXDIM)
      INTEGER ACTDIMS
*    Strings
      BYTE BVALUE
      LOGICAL*1 LVALUE
      INTEGER*2 WVALUE
      INTEGER IVALUE
      REAL RVALUE
      DOUBLE PRECISION DVALUE
      CHARACTER*25 CVALUE            ! 25 bytes required for double precision
      INTEGER MAXTRANS
      PARAMETER (MAXTRANS=512)
      BYTE BVALUES(MAXTRANS)
      LOGICAL*1 LVALUES(MAXTRANS)
      INTEGER*2 WVALUES(MAXTRANS)
      INTEGER IVALUES(MAXTRANS)
      REAL RVALUES(MAXTRANS)
      DOUBLE PRECISION DVALUES(MAXTRANS)
      CHARACTER*16 CVALUES (MAXTRANS)
      INTEGER ACTVALS
*    Transfer data...
      INTEGER TRANSFERS
      INTEGER TRANSFER_COUNT
      INTEGER START
      INTEGER END
*    ADAM message
      CHARACTER*80 MESSAGE/' '/
*    Counter
      INTEGER I
      INTEGER FORCEPROMPT/0/
*
*    Prompt for filename
*      DO WHILE (FILELEN .LE. 0)
*         CALL LIB$GET_FOREIGN(FILENAME, 'I : Name of GSD file > ',
*     :                        FILELEN, FORCEPROMPT)
*         FORCEPROMPT = 1
*      ENDDO
      WRITE( *, * ) 'I : Name of GSD file > '
      READ( *, '(A)' ) FILENAME
      FILELEN = INDEX( FILENAME, ' ' ) - 1
      IF ( FILELEN .LE. 0 ) STOP 'Error: No file name given'
*
*    Open GSD file
      CALL GSD_OPEN_READ(FILENAME(:FILELEN),
     :                   FD,
     :                   VERSION,
     :                   LABEL,
     :                   NITEM,
     :                   STATUS)

      IF (STATUS .EQ. 0) THEN
*
*       Set the printfile name
*         PRINTFILE=FILENAME(:FILELEN)//'.LIS'
*         PRLEN = FILELEN + 4
      WRITE( *, * ) 'I : Name of list file > '
      READ( *, '(A)' ) PRINTFILE
      PRLEN = INDEX( PRINTFILE, ' ' ) - 1
      IF ( PRLEN .LE. 0 ) STOP 'Error: No file name given'
*
*       Open the print file...
*         CALL LIB$GET_LUN(LUN)
         LUN = 41
         OPEN (UNIT=LUN, FILE=PRINTFILE(:PRLEN), STATUS='NEW',
     :         CARRIAGECONTROL='LIST')
         PRINT *,'O : Writing contents to ', PRINTFILE(:PRLEN)
*
*       Write header
         WRITE (UNIT=LUN, FMT=400) FILENAME(:FILELEN), VERSION,
     :                             LABEL, NITEM
400      FORMAT (80('-') /' G S D     P R I N T'/
     :           80('-')//' FILENAME     : ', A/
     :                    ' GSD VERSION  : ', F6.3/
     :                    ' LABEL        : ', A/
     :                    ' NITEMS       : ', I3///)
*
         WRITE (UNIT=LUN, FMT=500)
*                  111111111122222222223333333333444444444455555555556
*         123456789012345678901234567890123456789012345678901234567890
500      FORMAT (
     :   ' NAME            UNIT         TYPE      TABLE     ',
     :   'VALUE or SIZE'/
     :     80('-')//)
*
      ENDIF
*
*    Loop reading descriptors
      DO WHILE (NUMBER .LT. NITEM .AND. STATUS .EQ. 0)
         NUMBER = NUMBER + 1
*
*       Get information on the next item
         CALL GSD_ITEM (FD, NUMBER,            ! Input
     :                  NAME, UNIT, TYPE, TABLE, GSDINDEX, STATUS)  ! Output
*
         IF (STATUS .EQ. 0) THEN
            IF (.NOT. TABLE) THEN
*
*             Get scalar value as character string GSD routine does type
*             conversion)
*               CALL GSD_GET0C (GSDINDEX, CVALUE, STATUS)
*
*               WRITE (UNIT=LUN, FMT=1000) NAME, UNIT, TYPE, TABLE,
*     :                                    CVALUE
*1000           FORMAT (T2, A15, T18, A10, T31, A1, T41, L1, T51, A25)
*
               IF ( TYPE .EQ. 'B' ) THEN
                  CALL GSD_GET0B (GSDINDEX, BVALUE, STATUS)
                  WRITE(LUN,1001) NAME, UNIT, TYPE, TABLE, BVALUE
1001              FORMAT (T2, A15, T18, A10, T31, A1, T41, L1, T51, I4)
               ELSE IF ( TYPE .EQ. 'L' ) THEN
                  CALL GSD_GET0L (GSDINDEX, LVALUE, STATUS)
                  WRITE(LUN,1002) NAME, UNIT, TYPE, TABLE, LVALUE
1002              FORMAT (T2, A15, T18, A10, T31, A1, T41, L1, T51, L1)
               ELSE IF ( TYPE .EQ. 'W' ) THEN
                  CALL GSD_GET0W (GSDINDEX, WVALUE, STATUS)
                  WRITE(LUN,1003) NAME, UNIT, TYPE, TABLE, WVALUE
1003              FORMAT (T2, A15, T18, A10, T31, A1, T41, L1, T51, I6)
               ELSE IF ( TYPE .EQ. 'I' ) THEN
                  CALL GSD_GET0I (GSDINDEX, IVALUE, STATUS)
                  WRITE(LUN,1004) NAME, UNIT, TYPE, TABLE, IVALUE
1004              FORMAT (T2, A15, T18, A10, T31, A1, T41, L1, T51, I10)
               ELSE IF ( TYPE .EQ. 'R' ) THEN
                  CALL GSD_GET0R (GSDINDEX, RVALUE, STATUS)
                  WRITE(LUN,1005) NAME, UNIT, TYPE, TABLE, RVALUE
1005              FORMAT (T2, A15, T18, A10, T31, A1, T41, L1,T51,G14.8)
               ELSE IF ( TYPE .EQ. 'D' ) THEN
                  CALL GSD_GET0D (GSDINDEX, DVALUE, STATUS)
                  WRITE(LUN,1006) NAME, UNIT, TYPE, TABLE, DVALUE
1006              FORMAT (T2, A15, T18, A10, T31, A1, T41,L1,T51,G21.15)
               ELSE IF ( TYPE .EQ. 'C' ) THEN
                  CALL GSD_GET0C (GSDINDEX, CVALUE, STATUS)
                  WRITE(LUN,1007) NAME, UNIT, TYPE, TABLE, CVALUE
1007              FORMAT (T2, A15, T18, A10, T31, A1, T41, L1, T51, A25)
               END IF
*
            ELSE
*
*             Get the dimensional information on the array...
               CALL GSD_INQ_SIZE (FD, NUMBER,
     :                            GSD__MXDIM, DIMNAMES, DIMUNITS,
     :                            DIMVALS, ACTDIMS, SIZE, STATUS)

               WRITE (UNIT=LUN, FMT=2000) NAME, UNIT, TYPE, TABLE,
     :                                    SIZE, ACTDIMS
2000           FORMAT (T2, A15, T18, A10, T31, A1, T41, L1, T51, 2I10)
               DO I = 1, ACTDIMS
                  WRITE (UNIT=LUN, FMT=3000) DIMNAMES(I),
     :                                       DIMUNITS(I),
     :                                       DIMVALS(I)
3000              FORMAT (T2, ' DIMNAMES = ', A15,
     :                        ' DIMUNITS = ', A10,
     :                        ' DIMVALS = ', I10)
               ENDDO
*
*             Map the array as a 1D array and write out the values
*             Transfer the data in sections equal to the size of the
*             locally declared array...
               TRANSFERS = (SIZE + MAXTRANS - 1 ) / MAXTRANS
               TRANSFER_COUNT = 0
               END = 0
*
               DO WHILE (STATUS .EQ. 0 .AND.
     :                   TRANSFER_COUNT .LT. TRANSFERS)
*
                  TRANSFER_COUNT = TRANSFER_COUNT + 1
                  START = END + 1
                  END = MIN (SIZE, END + MAXTRANS)
*
*                  CALL GSD_GET1C (GSDINDEX, 1, SIZE, START, END,
*     :                            CVALUES, ACTVALS, STATUS)
*
*                  WRITE (UNIT=LUN, FMT=4000)
*     :                  (CVALUES(I), I = 1, ACTVALS)
*4000              FORMAT (1X, 3A)
*
                  IF ( TYPE .EQ. 'B' ) THEN
                     CALL GSD_GET1B (GSDINDEX, 1, SIZE, START, END,
     :                            BVALUES, ACTVALS, STATUS)
                     WRITE (LUN, 4001) (BVALUES(I), I = 1, ACTVALS)
4001                 FORMAT (1X, 12(1X,I4,1X))
                  ELSE IF ( TYPE .EQ. 'L' ) THEN
                     CALL GSD_GET1L (GSDINDEX, 1, SIZE, START, END,
     :                            LVALUES, ACTVALS, STATUS)
                     WRITE (LUN, 4002) (LVALUES(I), I = 1, ACTVALS)
4002                 FORMAT (1X, 12(1X,L1,1X))
                  ELSE IF ( TYPE .EQ. 'W' ) THEN
                     CALL GSD_GET1W (GSDINDEX, 1, SIZE, START, END,
     :                            WVALUES, ACTVALS, STATUS)
                     WRITE (LUN, 4003) (WVALUES(I), I = 1, ACTVALS)
4003                 FORMAT (1X, 8(1X,I6,1X))
                  ELSE IF ( TYPE .EQ. 'I' ) THEN
                     CALL GSD_GET1I (GSDINDEX, 1, SIZE, START, END,
     :                            IVALUES, ACTVALS, STATUS)
                     WRITE (LUN, 4004) (IVALUES(I), I = 1, ACTVALS)
4004                 FORMAT (1X, 6(1X,I10,1X))
                  ELSE IF ( TYPE .EQ. 'R' ) THEN
                     CALL GSD_GET1R (GSDINDEX, 1, SIZE, START, END,
     :                            RVALUES, ACTVALS, STATUS)
                     WRITE (LUN, 4005) (RVALUES(I), I = 1, ACTVALS)
4005                 FORMAT (1X, 4(1X,G14.8,1X))
                  ELSE IF ( TYPE .EQ. 'D' ) THEN
                     CALL GSD_GET1D (GSDINDEX, 1, SIZE, START, END,
     :                            DVALUES, ACTVALS, STATUS)
                     WRITE (LUN, 4006) (DVALUES(I), I = 1, ACTVALS)
4006                 FORMAT (1X, 3(1X,G21.15,1X))
                  ELSE IF ( TYPE .EQ. 'C' ) THEN
                     CALL GSD_GET1C (GSDINDEX, 1, SIZE, START, END,
     :                            CVALUES, ACTVALS, STATUS)
                     WRITE (LUN, 4007) (CVALUES(I), I = 1, ACTVALS)
4007                 FORMAT (1X, 4(1X,A16,1X))
                  END IF
               ENDDO
            ENDIF
         ENDIF
      ENDDO
*
*      IF (STATUS .NE. 0) CALL LIB$SIGNAL(%VAL(STATUS))
      IF ( STATUS .NE. 0 ) STOP 'Error reading GSD file'
*
      END

