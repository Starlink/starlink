C+
      SUBROUTINE ARLIST (IOUT,FILE,CHANS,WAVES,CLASS,NLID,COEFFS,
     :                                        ORDER,FITTED,SIGMA)
C
C     A R L I S T
C
C     ARC utility.  Writes out the lines and wavelengths to a
C     disk file in a simple format suitable for a program (either
C     ARC itself or some automatic fitter) to read later.  The
C     file is assumed to be already open, and ARLIST does not
C     close it.
C
C     Parameters  (">" input)
C
C     (>) IOUT     (Integer) Fortran unit number for output file
C     (>) FILE     (Character) The full name of the data file
C                  containing the arc.
C     (>) CHANS    (Real array CHANS(NLID)) The channel numbers for
C                  the lines.
C     (>) WAVES    (Real array WAVES(NLID)) The wavelengths of the
C                  lines.
C     (>) CLASS    (Integer array CLASS(NLID)) The class codes for
C                  the identified arc lines.
C     (>) NLID     (Integer) The number of the lines identified.
C     (>) COEFFS   (Double precision COEFFS(ORDER)) The current
C                  wavelength coefficients.
C     (>) ORDER    (Integer) The number of coefficients used.
C     (>) FITTED   (Logical) True if a wavelength fit has been obtained.
C     (>) SIGMA    (Real) The line width value used for the fit
C
C     Subroutines / functions used -
C
C     GEN_EPOLYD   (GEN_ package) Evaluate a polynomial
C     ICH_LEN      (ICH_    "   ) Position of last non-blank char in string
C
C                                             KS / CIT 14th June 1984
C     Modified:
C
C     4th Sept 1985  KS / AAO  Line number added to output format
C                    CLASS parameter added.  Auto flag added.
C     30th June 1986 KS / AAO  Now allows for possibility ORDER>NLID
C     19th Mar 1991  KS / AAO  FILE parameter added, and written to file.
C+
      IMPLICIT NONE
C
C     Parameters   (dimension 1 used in case NLID=0)
C
      LOGICAL FITTED
      INTEGER IOUT,NLID,ORDER,CLASS(1)
      REAL CHANS(1),WAVES(1),SIGMA
      DOUBLE PRECISION COEFFS(ORDER)
      CHARACTER*(*) FILE
C
C     Functions
C
      INTEGER ICH_LEN
      DOUBLE PRECISION GEN_EPOLYD
C
C     Local variables
C
      INTEGER I,NORDER,STATUS
      REAL ERR,FITVAL,RMS
      CHARACTER*4 AUTO
C
      NORDER=MIN(NLID,ORDER)
      IF (NLID.GT.0) THEN
         REWIND (IOUT,IOSTAT=STATUS)
         WRITE (IOUT,'(I5,A,A)',IOSTAT=STATUS)
     :             NLID,' lines identified from ',FILE(:ICH_LEN(FILE))
         IF (FITTED) THEN
            RMS=0.
            WRITE (IOUT,'(/,6X,A)',IOSTAT=STATUS)
     :         'Channel   Wavelength   Calculated  Discrepancy  Line#'
            DO I=1,NLID
               FITVAL=GEN_EPOLYD(DBLE(CHANS(I)),COEFFS,NORDER)
               ERR=FITVAL-WAVES(I)
               RMS=RMS+ERR*ERR
               IF (CLASS(I).EQ.0) THEN
                  AUTO=' '
               ELSE
                  AUTO=' (A)'
               END IF
               WRITE (IOUT,'(4F13.4,I7,A4)',IOSTAT=STATUS)
     :                         CHANS(I),WAVES(I),FITVAL,ERR,I,AUTO
            END DO
            RMS=SQRT(RMS/FLOAT(NLID))
            WRITE (IOUT,'(/,A,F10.2,A,F5.2)',IOSTAT=STATUS)
     :                ' RMS error: ',RMS,', Line width used: ',SIGMA
            WRITE (IOUT,'(/,A,I3,/)',IOSTAT=STATUS)
     :                ' Order of fit: ',NORDER-1
            WRITE (IOUT,'(3D23.16)',IOSTAT=STATUS)
     :                                (COEFFS(I),I=1,NORDER)
         ELSE
            WRITE (IOUT,'(/,6X,A)',IOSTAT=STATUS)
     :         'Channel   Wavelength   -- No fit performed --   Line#'
            DO I=1,NLID
               IF (CLASS(I).EQ.0) THEN
                  AUTO=' '
               ELSE
                  AUTO=' (A)'
               END IF
               WRITE (IOUT,'(2F13.4,26X,I7,A4)',IOSTAT=STATUS)
     :                             CHANS(I),WAVES(I),I,AUTO
            END DO
         END IF
      END IF
C
      END
