      SUBROUTINE ECH_IMPORT_ECHARC(
     :           IOUT,
     :           ARFILE,
     :           MAX_PERM_FTRS,
     :           PREV,
     :           N_ORDERS,
     :           IDEN_FTR_POSITION,
     :           IDEN_FTR_WAVELENGTH,
     :           IDENTIFIED_FTRS,
     :           IDEN_FTR_STATUS,
     :           FORDER,
     :           FSIGMA
     :          )
*+
*  Name:
*     ECHOMOP - ECH_IMPORT_ECHARC

*  Purpose:
*     Reads ECHARC format output files

*  Description:
*     ECHARC utility. Either opens an existing arc line file and
*     reads in the channel numbers and wavelengths it contains,
*     and then opens a new arc line file, 'ARLINES.ECH', or else
*     just opens a new file.

*  Invocation:
*     CALL ECH_IMPORT_ECHARC(
*    :     IOUT,
*    :     ARFILE,
*    :     MAX_PERM_FTRS,
*    :     PREV,
*    :     N_ORDERS,
*    :     IDEN_FTR_POSITION,
*    :     IDEN_FTR_WAVELENGTH,
*    :     IDENTIFIED_FTRS,
*    :     IDEN_FTR_STATUS,
*    :     FORDER,
*    :     FSIGMA
*    :     )

*  Arguments:
*     IOUT = INTEGER (Given)
*        Fortran unit number to be used.
*     N_ORDERS = INTEGER (Given)
*        Number of orders in echellogram.
*     IDENTIFIED_FTRS = INTEGER (Given)
*        Number of identified features.
*     ARFILE = CHAR (Given)
*        Name of previous line list.
*     MAX_PERM_FTRS = INTEGER (Given)
*        Maximum possible number of lines.
*     PREV = LOGICAL (Given)
*        If true, routine is to read previous line list.
*     ORDER = INTEGER (Returned)
*        Order in which identified lines were found.
*     IDEN_FTR_POSITION = REAL (Returned)
*        Channel numbers of identified lines.
*     IDEN_FTR_WAVELENGTH = REAL (Returned)
*        Wavelengths of identified lines.
*     WWEIGHTS = REAL (Returned)
*        The weights for the identified arc lines.
*     IDEN_FTR_STATUS = INTEGER (Returned)
*        The status of the identified arc lines.
*     FSIGMA = REAL (Returned)
*        The sigma read from the file.
*     FORDER = INTEGER (Returned)
*        The order read from the file (the actual
*        order value in the file, not that plus 1).
*     NLID = INTEGER (Returned)
*        Number of identified lines.

*  Bugs:
*     None known.

*  Authors:
*     Dave Mills STARLINK (ZUVAD::DMILLS)

*  History:
*     1992 Sept 1 : Initial release

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'ECH_REPORT.INC'

*  Parameters:
      LOGICAL PREV
      INTEGER IOUT,max_perm_ftrs,NLID,n_orders ,i,
     :              identified_ftrs(n_orders),
     :              iden_ftr_status(max_perm_ftrs,n_orders),FORDER
      REAL iden_ftr_position(max_perm_ftrs,n_orders),
     :     iden_ftr_wavelength(max_perm_ftrs,n_orders),
     :   FSIGMA

      CHARACTER*( * ) ARFILE

*  Local Variables:
      REAL POSN
      REAL WAVE

      INTEGER IBRACK
      INTEGER IDOT
      INTEGER NFILE
      INTEGER STATUS
      INTEGER IORD

      CHARACTER*64 FILE
      CHARACTER*4 AUTO

*  Functions:
      INTEGER CHR_LEN
*.

*  See if we are to read the old file.
      NLID=0
      IF (PREV) THEN
         FILE=ARFILE
         IBRACK=MAX(1,INDEX(ARFILE,']'))
         IDOT=INDEX(ARFILE(IBRACK:),'.')
         IF (IDOT.EQ.0) FILE=ARFILE(:CHR_LEN(ARFILE))//'.ECH'
         OPEN (UNIT=IOUT,FILE=FILE,STATUS='OLD',IOSTAT=STATUS)
         IF (STATUS.NE.0) THEN
            CALL ECH_REPORT(0,'Unable to open old line list file')
         ELSE
            READ (IOUT,'(I5//)',IOSTAT=STATUS) NLID
            DO NFILE=1,NLID
               READ (IOUT,'(6X,I3,2F13.4,7X,A4)',IOSTAT=STATUS)
     :                 iord,posn,wave,auto
               IF (STATUS.NE.0) THEN
                  CALL ECH_REPORT(0,'I/O error reading arcline file')
                  GO TO 320
               END IF
               identified_ftrs(iord) = identified_ftrs(iord)+1
               iden_ftr_wavelength(identified_ftrs(iord),iord) = wave
               iden_ftr_position(identified_ftrs(iord),iord) = posn
               IF (AUTO.EQ.'    ') THEN
                  iden_ftr_status(identified_ftrs(iord),iord) = 256
               ELSE IF (AUTO.EQ.' (A)') THEN
                  iden_ftr_status(identified_ftrs(iord),iord) = 512
               ELSE IF (AUTO.EQ.' (E)') THEN
                  iden_ftr_status(identified_ftrs(iord),iord) = 1024
               ELSE
                  iden_ftr_status(identified_ftrs(iord),iord) = 2048
               END IF
            END DO
  320       CONTINUE
            IF (STATUS.EQ.0) THEN
               READ (IOUT,'(/41X,F5.2)',IOSTAT=STATUS) FSIGMA
               IF (STATUS.EQ.0) THEN
                  READ (IOUT,'(/15X,I3/)',IOSTAT=STATUS) FORDER
               END IF
               IF (STATUS.NE.0) THEN
                  CALL ECH_REPORT(0,
     :                'I/O error reading SIGMA and NCOEFF from file')
               END IF
            END IF
            CLOSE (IOUT,IOSTAT=STATUS)
            WRITE ( report_string , 1000 ) nlid,file
            CALL ECH_REPORT ( 0 , report_string )
            DO i = 1 , n_orders
               IF ( identified_ftrs ( i ) .GT. 0 ) THEN
                  WRITE ( report_string , 1001 ) i,identified_ftrs(i)
                  CALL ECH_REPORT ( 0 , report_string )
               ENDIF
            END DO
            CALL ECH_REPORT ( 0 , ' ' )
         END IF
      END IF

 1000 FORMAT ( 1X , 'Reading ',I5,' features from file ',A64 )
 1001 FORMAT ( 1X , 'Order ',I3,' now has ',I3,' identified features')

      END
