      SUBROUTINE FIG_READSDIST(COFILE,MAXTRACKS,NTRACKS,NX,NY,START,END,
     :                                          YAVE,WIDTH,COEFF,STATUS)
*
*                            F I G _ R E A D S D I S T
*
*  Routine name:
*     FIG_READSDIST
*
*  Function:
*     Read a coefficient file of the SDIST.DAT format, as written by SDIST.
*
*  Description:
*     Obtain a free logical unit, open the file, read it directly into the
*     supplied arrays, checking only I/O status and not that the data are
*     self-consistent, close it, and release the logical unit. Report any
*     errors directly using PAR_WRUSER.
*
*     The file has the following format (the term "spectrum" is here used
*     for "spectrum or order"):
*
*     Three header lines, conventionally all beginning with "*".
*     One line giving the number of spectra traced and the dimensions of the
*     original input image, in the format 20X,I5,25X,I8,4X,I8.
*     Then, for each spectrum traced,
*     One record giving the spectrum number and the leftmost and rightmost
*     pixels covered by the trace, in format 11X,I5,17X,I5,4X,I5
*     One record giving the average Y value in the spectrum and the average
*     width of the spectrum, in format 16X,F13.7,10X,F9.2
*     Four records giving the 11 polynomial coefficients for the fit, in
*     3D23.16 format (coefficients are given constant first, with any
*     unused ones set to zero - note that they have been adjusted to give a
*     zero mean Y value, so the average Y value must be added to the
*     constant term).
*
*  Language:
*     FORTRAN
*
*  Call:
*      CALL FIG_READSDIST(COFILE,MAXTRACKS,NTRACKS,NX,NY,START,END,
*                                          YAVE,WIDTH,COEFF,STATUS)
*
*  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
*
*     (>) COFILE        (Fixed string,descr) The name of the coefficient
*                       file, default file extension .DAT.
*     (>) MAXTRACKS     (Integer,ref) The maximum number of spectra or orders
*                       that can be accomodated. If the actual number exceeds
*                       MAXTRACKS, only the first MAXTRACKS elements in the
*                       arrays will be returned but NTRACKS will be returned
*                       as the actual number.
*     (<) NTRACKS       (Integer,ref) Actual number of spectra or orders.
*     (<) NX,NY         (Integer,ref) Dimensions of image on which the original
*                       spectra or orders were tracked.
*     (<) START(MAXTRACKS) (Integer array,ref) The starting (X) pixel number for
*                       which this spectrum or order was tracked
*     (<) END(MAXTRACKS) (Integer array,ref) The ending (X) pixel number for
*                       which this spectrum or order was tracked
*     (<) YAVE(MAXTRACKS) (Real*8 array,ref) The average (Y) value along this
*                       spectrum or order.
*     (<) WIDTH(MAXTRACKS) (Real array,ref) The average width of this spectrum
*                       or order.
*     (<) COEFF(11,MAXTRACKS) (Real*8 array,ref) The 10th order polynomial fit
*                       coefficients to the spectra or orders, constant term
*                       first and with any unused coefficients zero.
*     (<) STATUS        (Integer,ref) Fortran i/o status, zero for success
*
*  External variables used:
*
*     None
*
*  External subroutines / functions used:
*
*     PAR_WRUSER
*     DSA_FREE_LU, DSA_GET_LU
*     Standard Fortran Functions
*
*  Prior requirements:
*     None
*
*  Support: William Lupton, AAO
*
*  Version date: 04-Aug-88
*-
*  History:
*     28 Sep 1992 (hme):
*        TABs removed.
*     21 Jul 1993 (hme):
*        Use DSA_*_LU.
*     9  Jun 1999 (tdca):
*        Removed unsupported keywords from OPEN statements.

*
*     Parameters
*
      CHARACTER*(*) COFILE
      INTEGER MAXTRACKS
      INTEGER NTRACKS
      INTEGER NX
      INTEGER NY
      INTEGER START(MAXTRACKS)
      INTEGER END(MAXTRACKS)
      REAL*8  YAVE(MAXTRACKS)
      REAL    WIDTH(MAXTRACKS)
      REAL*8  COEFF(11,MAXTRACKS)
      INTEGER STATUS
*
*     Local variables
*
      INTEGER LU,IGNORE,TRACK,I
*
*     Obtain a logical unit number
*
      IGNORE=0
      CALL DSA_GET_LU(LU,IGNORE)
*
*     Open the coefficient file
*
      OPEN(UNIT=LU,FILE=COFILE,STATUS='OLD',IOSTAT=STATUS)
      IF (STATUS.NE.0) THEN
         CALL PAR_WRUSER('Error opening coeff file',IGNORE)
         GOTO 9999
      ENDIF
*
*     Read and ignore header lines.
*
      READ(LU,'()',IOSTAT=STATUS)
      READ(LU,'()',IOSTAT=STATUS)
      READ(LU,'()',IOSTAT=STATUS)
      IF (STATUS.NE.0) THEN
         CALL PAR_WRUSER('Error reading coeff file header lines',IGNORE)
         GOTO 9999
      ENDIF
*
*     Read number of spectra or orders and size of original image.
*
      READ(LU,'(20X,I5,15X,I8,4X,I8)',IOSTAT=STATUS) NTRACKS,NX,NY
      IF (STATUS.NE.0) THEN
         CALL PAR_WRUSER('Error reading coeff file number of spectra '//
     :                             'or orders and size of image',IGNORE)
         GOTO 9999
      ENDIF
*
*     For each spectrum or order, read start and end in X direction, average
*     Y position, average width and polynomial coefficients.
*
      DO TRACK = 1,MIN(MAXTRACKS,NTRACKS)
         READ(LU,'(33X,I5,4X,I5)',IOSTAT=STATUS) START(TRACK),
     :                                             END(TRACK)
         IF (STATUS.NE.0) THEN
            CALL PAR_WRUSER('Error reading coeff file start and end '//
     :                       'positions of a spectrum or order',IGNORE)
            GOTO 9999
         ENDIF
         READ(LU,'(16X,F13.7,10X,F9.2)',IOSTAT=STATUS) YAVE(TRACK),
     :                                                 WIDTH(TRACK)
         IF (STATUS.NE.0) THEN
            CALL PAR_WRUSER('Error reading coeff file average '//
     :        'position and width of a spectrum or order',IGNORE)
            GOTO 9999
         ENDIF
         READ(LU,'(3D23.16)',IOSTAT=STATUS) (COEFF(I,TRACK),I=1,11)
         IF (STATUS.NE.0) THEN
            CALL PAR_WRUSER('Error reading coeff file coefficients '//
     :                               'for a spectrum or order',IGNORE)
            GOTO 9999
         ENDIF
      ENDDO
*
*     Jump here on error or on normal exit. Close the file and free the
*     logical unit.
*
9999  CONTINUE
      CLOSE(UNIT=LU,IOSTAT=IGNORE)
      IGNORE=0
      CALL DSA_FREE_LU(LU,IGNORE)
      END
