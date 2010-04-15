C+
      SUBROUTINE ECH_ARLIST (IOUT,NLMAX,ORDER,CHANS,WAVES,CLASS,
     :                       NLID,NCOEFF,SIGMA)
C
C     E C H _ A R L I S T
C
C     ECHARC utility.  Writes out the lines and wavelengths to a
C     disk file in a simple format suitable for a program (either
C     ARC itself or some automatic fitter) to read later.  The
C     file is assumed to be already open, and ARLIST does not
C     close it.
C
C     Parameters  (">" input)
C
C     (>) IOUT     (Integer) Fortran unit number for output file
C     (>) NLMAX    (Integer) Dimension of arrays ORDER,CHANS,etc.
C     (>) ORDER    (Integer array ORDER(NLMAX))  The order number
C                  in which identified lines were found.
C     (>) CHANS    (Real array CHANS(NLMAX)) The channel numbers for
C                  the lines.
C     (>) WAVES    (Real array WAVES(NLMAX)) The wavelengths of the
C                  lines.
C     (>) CLASS    (Integer array CLASS(NLMAX)) The class codes for
C                  the identified arc lines.
C     (>) NLID     (Integer) The number of the lines identified.
C     (>) NCOEFF   (Integer) The number of polynomial coefficients used.
C     (>) SIGMA    (Real) The line width value used for the fit
C
C                                             KS / CIT 14th June 1984
C     Modified:
C
C     4th Sept 1985  KS / AAO  Line number added to output format
C                    CLASS parameter added.  Auto flag added.
C     30TH JUNE 1986 KS / AAO  Now allows for possibility NCEOFF>NLID
C+
      IMPLICIT NONE
C
C     Parameters   (dimension 1 used in case NLID=0)
C
      INTEGER IOUT,NLMAX,NLID,ORDER(NLMAX),CLASS(NLMAX),NCOEFF
      REAL CHANS(NLMAX),WAVES(NLMAX),SIGMA
C
C     Local variables
C
      INTEGER I,STATUS
      CHARACTER*4 AUTO
C
      IF (NLID.GT.0) THEN
         REWIND (IOUT,IOSTAT=STATUS)
         WRITE (IOUT,'(I5,A)',IOSTAT=STATUS)
     :                         NLID,' lines identified, as follows'
         WRITE (IOUT,'(/,6X,A)',IOSTAT=STATUS)
     :      'Order   Channel     Wavelength   Line#'
         DO I=1,NLID
            IF (CLASS(I).EQ.0) THEN
               AUTO=' '
            ELSE IF (CLASS(I).EQ.1) THEN
               AUTO=' (A)'
            ELSE IF (CLASS(I).EQ.2) THEN
               AUTO=' (E)'
            ELSE
               AUTO=' (?)'
            END IF
            WRITE (IOUT,'(6X,I3,2F13.4,I7,A4)',IOSTAT=STATUS)
     :                ORDER(I),CHANS(I),WAVES(I),I,AUTO
         END DO
         WRITE (IOUT,'(/,A,F5.2)',IOSTAT=STATUS)
     :      ' Line width used for identifying lines:  ',SIGMA
         WRITE (IOUT,'(/,A,I3,/)',IOSTAT=STATUS)
     :             ' Order of fit: ',NCOEFF-1
      END IF
C
      RETURN
      END
