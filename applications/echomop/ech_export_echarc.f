      SUBROUTINE ECH_EXPORT_ECHARC(
     :           IOUT,
     :           ARFILE,
     :           N_ORDERS,
     :           IDENTIFIED_FTRS,
     :           MAX_PERM_FTRS,
     :           IDEN_FTR_POSITION,
     :           IDEN_FTR_WAVELENGTH,
     :           IDEN_FTR_STATUS,
     :           MAX_NPOLY,
     :           SIGMA
     :          )
*+
*  Name:
*     ECHOMOP - ECH_EXPORT_ECHARC

*  Purpose:
*     Writes out identified lines in ECHARC format.

C     ECHARC utility.  Writes out the lines and wavelengths to a
C     disk file in a simple format suitable for a program (either
C     ARC itself or some automatic fitter) to read later.  The
C     file is assumed to be already open, and ARLIST does not
C     close it.
C
C     Parameters  (">" input)
C
C     (>) IOUT     (Integer) Fortran unit number for output file
C     (>) max_perm_ftrs    (Integer) Dimension of arrays ORDER,iden_ftr_position,etc.
C     (>) ORDER    (Integer array ORDER(max_perm_ftrs))  The order number
C                  in which identified lines were found.
C     (>) iden_ftr_position    (Real array iden_ftr_position(max_perm_ftrs)) The channel numbers for
C                  the lines.
C     (>) iden_ftr_wavelength    (Real array iden_ftr_wavelength(max_perm_ftrs)) The wavelengths of the
C                  lines.
C     (>) iden_ftr_status    (Integer array iden_ftr_status(max_perm_ftrs)) The iden_ftr_status codes for
C                  the identified arc lines.
C     (>) NLID     (Integer) The number of the lines identified.
C     (>) max_npoly   (Integer) The number of polynomial coefficients used.
C     (>) SIGMA    (Real) The line width value used for the fit
C
C                                             KS / CIT 14th June 1984
C     Modified:
C
C     4th Sept 1985  KS / AAO  Line number added to output format
C                    iden_ftr_status parameter added.  Auto flag added.
C     30TH JUNE 1986 KS / AAO  Now allows for possibility NCEOFF>NLID
*-

*  Type Definitions:
      IMPLICIT NONE
C
C     Parameters   (dimension 1 used in case NLID=0)
C
      INTEGER IOUT,max_perm_ftrs,NLID,n_orders,
     :        iden_ftr_status(max_perm_ftrs,n_orders),
     :        max_npoly,identified_ftrs(n_orders)
      REAL    iden_ftr_position(max_perm_ftrs,n_orders),
     :        iden_ftr_wavelength(max_perm_ftrs,n_orders),
     :        SIGMA

*  Local Variables:
      INTEGER I
      INTEGER STATUS
      INTEGER IORD
      INTEGER IBRACK
      INTEGER IDOT
      CHARACTER*( * ) ARFILE
      CHARACTER*64 FILE
      CHARACTER*4 AUTO

*  Functions Called:
      INTEGER CHR_LEN
*.
      NLID = 0
      DO I = 1, N_ORDERS
         NLID = NLID + IDENTIFIED_FTRS( I )
      END DO

      IF ( NLID .GT. 0 ) THEN

*     Open new file.
         FILE = ARFILE
         IBRACK = MAX( 1, INDEX( ARFILE, ']' ) )
         IDOT = INDEX( ARFILE( IBRACK: ), '.' )
         IF ( IDOT .EQ. 0 )
     :      FILE = ARFILE( :CHR_LEN( ARFILE ) ) // '.ECH'
         OPEN ( UNIT = IOUT, FILE = FILE, STATUS = 'NEW',
     :          IOSTAT = STATUS )
         IF ( STATUS .NE. 0 ) THEN
            CALL ECH_REPORT( 0, 'Unable to open new arc line file' )
            GO TO 999
         END IF

         WRITE ( IOUT, '( I5, A )', IOSTAT = STATUS )
     :         NLID,' lines identified, as follows'
         WRITE ( IOUT, '( /, 6X, A )', IOSTAT = STATUS )
     :         'Order   Channel     Wavelength   Line#'
         NLID = 0
         DO IORD = 1, N_ORDERS
            DO I = 1, IDENTIFIED_FTRS( IORD )
               NLID = NLID + 1
               IF ( IDEN_FTR_STATUS( I, IORD ) .EQ. 256 ) THEN
                  AUTO = ' '

               ELSE IF ( IDEN_FTR_STATUS( I, IORD ) .EQ. 512 ) THEN
                  AUTO = ' (A)'

               ELSE IF ( IDEN_FTR_STATUS( I, IORD ) .EQ. 1024 ) THEN
                  AUTO = ' (E)'

               ELSE
                  AUTO = ' (?)'
               END IF
               WRITE ( IOUT, '( 6X, I3, 2F13.4, I7, A4 )',
     :                 IOSTAT = STATUS )
     :               IORD,IDEN_FTR_POSITION( I, IORD ),
     :               IDEN_FTR_WAVELENGTH( I, IORD ), I, AUTO
            END DO
         END DO
         WRITE ( IOUT, '( /, A, F5.2 )', IOSTAT = STATUS )
     :         ' Line width used for identifying lines:  ', SIGMA
         WRITE ( IOUT, '( /, A, I3, / )', IOSTAT = STATUS )
     :         ' Order of fit: ', MAX_NPOLY - 1
      END IF

  999 CONTINUE

      END
