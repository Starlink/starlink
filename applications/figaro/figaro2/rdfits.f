C+
      SUBROUTINE RDFITS
C
C     R D F I T S
C
C     Figaro routine to read file in a 'Disk FITS' format,
C     creating a Figaro data structure file that contains all
C     the information from the disk file (although not necessarily in
C     an ideal form, since the program cannot guess at the meaning
C     of all the various FITS keywords).  For more details, see the
C     listing for FIG_FITIN.
C
C     Command parameters -
C
C     FILE       (Character) The name of the 'Disk Fits' file.
C
C     IMAGE      (Character) The name of the Figaro output file.
C
C     Command keywords -
C
C     SWAP       Swap bytes.  This should be true if the data has
C                been stored in the proper FITS format (IBM-style)
C                and the program is running on a non-IBM byte order
C                machine such as a VAX.  On a VAX, you should use:
C                SWAP = .TRUE. for AAO de facto 'disk FITS'.
C                SWAP = .FALSE. for WJT 'disk FITS'.
C
C     FLOAT      Convert the data to floating point numbers. This is
C                normally what will be required, although note that only
C                single precision is supported.  If FLOAT=NO is specified,
C                FITS will still convert to floating point if the data has
C                scaling and offset factors that are not 1.0 and 0.0
C                respectively.  The case where FLOAT=NO, BITPIX=16,
C                BSCALE=1.0, BZERO=32768 is treated as a special case and
C                will generate an array of unsigned 16 bit integers.
C                FLOAT=NO is usually only useful in this special case and
C                in the case where BSCALE=1.0, BZERO=0.0, BITPIX=16, where
C                it will create a smaller data file with no loss of precision.
C
C     User Variables used -   None
C
C     Note:  Most of the various 'disk FITS' formats differ only in
C            whether or not they swap bytes, and in the details of the
C            way the disk data is organised in records.  For example,
C            a VAX VMS file may have a 'FIXED', 'VARIABLE', or 'SEGMENTED'
C            format, whereas UNIX files are generally simpler.  Also on
C            a machine that has a record-based file structure (like a VAX)
C            the record lengths may or may not be the 2880 bytes that would
C            match a FITS tape. This program determines the disk format
C            (fixed, variable,segmented) and record length for itself, and
C            uses the SWAP keyword to indicate whether the data bytes are to
C            be treated as swapped or not.  It should therefore be able to
C            handle most of the available 'disk FITS' formats.
C
C                                             KS / AAO 17th June 1986
C     Modified:
C
C     13th Oct 1987 PWH/ StA. Generalised for any record type and byte
C                   order.  Storage for file and image names increased.
C     29th Dec 1987 KS / AAO. PWH's version made the standard Figaro
C                   version.
C     26th Oct 1988 KS / AAO. BLOCK parameter added to FIG_FITIN call.
C     6th  May 1990 KS / AAO.  Following reworking of FIG_FITIN to use
C                   DSA routines, amongst some other changes, comments
C                   modified to reflect these changes (mostly to the
C                   way FLOAT=NO is handled).  SETERR call changed to
C                   FIG_SETERR.
C     21st Aug 1990 KS / AAO. Revised to use the new FIG_DFITS_ routines
C                   for the actual disk I/O.  FITS_OPEN has now been
C                   reworked to become the VAX version of FIG_DFITS_OPEN
C                   and has been removed from this file.
C     21st Jul 1993 HME / UoE, Starlink.  Use DSA_*_LU.
C      2nd Aug 1993 HME / UoE, Starlink.  Since this routine must not
C                   call DSA_OPEN or DSA_CLOSE, we cannot use DSA to get
C                   a logical unit. Use FIO instead.
C      6th Jul 1994 HME / UoE, Starlink.  Disable IEEE format (BITPIX -32).
C     18th Jul 1996 MJCL / Starlink, UCL.  Set variables for storage of
C                   file names to 132 chars.
C     29th Jul 1996 MJCL / Starlink, UCL.  PAR_ABORT checking.
C+
      IMPLICIT NONE
C
C     Local variables
C
      LOGICAL FAULT,FLOAT,FOPEN,GOTLU,SWAP
      INTEGER BLOCK,LU,MTCHAN,STATUS,IGNORE
      CHARACTER ERROR*64,FILE*132,IMAGE*132
C
C     Functions called
C
      LOGICAL PAR_ABORT            ! (F)PAR abort flag
C
C     Initial values
C
      STATUS=0
      FOPEN=.FALSE.
      FAULT=.FALSE.
      GOTLU=.FALSE.
C
C     Get name of 'Disk FITS' format file and open it
C
      CALL PAR_RDCHAR('FILE',' ',FILE)
      IF ( PAR_ABORT() ) GO TO 500
      IGNORE=0
      CALL FIO_GUNIT(LU,IGNORE)
      GOTLU=.TRUE.
      CALL FIG_DFITS_OPEN (LU,FILE,STATUS)
      IF (STATUS.NE.0) THEN
         FAULT=.TRUE.
         GO TO 500
      END IF
      FOPEN=.TRUE.
C
C     Get the value of IMAGE
C
      CALL PAR_RDCHAR('IMAGE',' ',IMAGE)
C
C     Get the value of SWAP
C
      CALL PAR_RDKEY('SWAP',.TRUE.,SWAP)
C
C     Get the value of FLOAT
C
      CALL PAR_RDKEY('FLOAT',.TRUE.,FLOAT)
      IF ( PAR_ABORT() ) GO TO 500
C
C     Now call FITIN to do all the hard work
C
      MTCHAN=0
      BLOCK=0
      CALL FIG_FITIN(MTCHAN,LU,SWAP,BLOCK,IMAGE,FLOAT,STATUS,ERROR)
      IF (STATUS.NE.0) THEN
         CALL PAR_WRUSER('Error converting ''Disk FITS'' data',STATUS)
         CALL PAR_WRUSER(ERROR,STATUS)
         FAULT=.TRUE.
      END IF
C
C     Tidy up
C
  500 CONTINUE
C
      IF (FOPEN) CALL FIG_DFITS_CLOSE(STATUS)
      IGNORE=0
      IF (GOTLU) CALL FIO_PUNIT(LU,IGNORE)
C
      IF (FAULT) CALL FIG_SETERR
C
      END
