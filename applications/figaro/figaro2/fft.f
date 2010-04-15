      SUBROUTINE FFT
*+
*  Name:
*     FFT / BFFT

*  Purpose:
*     Takes the forward FFT of a complex data structure
*     Takes the reverse FFT of a complex data structure

*  Language:
*     Fortran 77

*  Type of Module:
*     Figaro application

*  Invocation:
*     CALL FFT

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     These Figaro functions take the FFT of the data in a file.
*     FFT performs a forward transform, BFFT performs an inverse
*     transform.  The input file must contain a complex data
*     structure, i.e. one with IMAGINARY and DATA components.
*
*     The data may be multi-dimensional; if it is, a multi-dimensional
*     FFT is performed.  Note that the Figaro routine R2CMPLX will turn
*     an existing real data structure into a complex one acceptable to
*     this routine. FFT does NOT perform any cosine belling or other
*     tapering of the data, nor does it reduce it to a zero mean.

*  Usage:
*     fft spatial_data frequency_data
*     bfft frequency_data spatial_data

*  ADAM Parameters:
*     CDATA = FILE (Read)
*        CDATA is the name of a complex data structure. Such structures
*        for the spatial domain are most easily produced using the
*        R2CMPLX command. For the frequency domain, such data were
*        usually created by R2CMPLX and transformed by FFT.
*     OUTPUT = FILE (Write)
*        OUTPUT is the name of the resulting Fourier transformed data.
*        If OUTPUT is the same as CDATA then the transform is performed
*        in situ; otherwise, a new file is created.

*  Notes:
*     The fourier transform routines available in the various math
*     libraries (NAG, IMSL, etc) all have slightly different
*     characteristics, which show up in the programs that use them.
*     This routine has been written around the NAG library (mainly
*     the routines C06FAF and C06FJF), so many of its characteristics
*     may be deduced by reading the relevant parts of the NAG manuals.
*     In version 5.0 this routine was changed to use the PDA library,
*     effectively FFTPACK routines. The data is re-ordered by FFT after
*     the transform so that the zero frequency component is in the
*     center of the resulting array, and this re-ordering is reversed by
*     BFFT before the transform. This means that after FFT has been run,
*     the various axes all go from -N to +N where N is the Nyquist
*     frequency.  New axis data structures that reflect this are created
*     by FFT and will be deleted by BFFT.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     jm: Jo Murray (RAL, Starlink)
*     jms: ??? (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     08 Oct 1986 (ks):
*        Original version.
*     20 Feb 1989 (jm):
*        Modified to use DSA_ routines Dynamic memory handling changed
*        to use DYN_ routines
*     19 Feb 1991 (jms):
*        Added STATUS checks to support user requested aborts. No longer
*        checks if complex data is double precision (DSA_MAP_DATA can
*        handle it if it isn't).
*     20 Feb 1991 (jms):
*        Now uses DSA routines to create new axis structures that
*        reflect the transformed data.
*     06 Oct 1992 (hme):
*        INCLUDE changed.
*     18 Mar 1993 (hme):
*        Call DSA_DATA_SIZE with maximum dimensionality 2 rather than
*        10. The array for the dimensions had no more space than that.
*     20 Apr 1995 (hme):
*        No longer use NAG. At this level it means that more work space
*        is required.
*     13 Mar 1996 (hme):
*        Adapt to the FDA library.
*        Map complex in single call.
*        No saving and restoring of axes. The forward transform will still
*        change the axes to Nyquist frequency. The backward transform
*        will delete these.
*     2005 June 14 (MJC):
*         Use CNF_PVAL for pointers to mapped data.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions
C
      CHARACTER GEN_NTH*2        ! Returns 'st','th','rd' etc.
                                 ! appropriate to N
      CHARACTER ICH_CI*12        ! Return an integer as a
                                 ! character string
      INTEGER  ICH_LEN           ! Position of last non-blank char in
                                 ! string
      INTEGER  DSA_TYPESIZE      ! Returns byte size of data type
C
C     Local variables
C
      INTEGER   BDOUB            ! Number of bytes per item of type 'DOUBLE'
      CHARACTER CITEMS(2)*32     ! Axis character items retrieved
      DOUBLE PRECISION NITEMS(1) ! Axis numeric items retrieved
      INTEGER   DIMS(2)          ! Image dimensions
      INTEGER   IPTR             ! Dynamic-memory pointer for image data
      INTEGER   NDIM             ! Number of image dimensions
      INTEGER   NELM             ! Number of elements in image - ignored
      INTEGER   SLOT             ! Slot number for mapped data - ignored
      INTEGER   STATUS           ! Running status for DSA routines

      LOGICAL CHANGE, FAULT, FORWARD
      LOGICAL RFLAG
      INTEGER DPTR, I
      INTEGER MAXDIM, NEXT
      INTEGER NPTR, NWORK, PREV, ROTS(10), FSTAT, WPTR
      CHARACTER COMMAND*8
      CHARACTER STRING*80
C
C     Parameters controlling the way DSA_OUTPUT opens the spectrum file
C
      INTEGER   NEW_FILE, NO_DATA
      PARAMETER (NEW_FILE=1, NO_DATA=1)
C
C     Initial values
C
      FAULT=.FALSE.
      STATUS=0
C
C     Get command name
C
      CALL PAR_COMMAND(COMMAND)
      FORWARD=COMMAND.EQ.'FFT'
C
C     Open DSA routines
C
      CALL DSA_OPEN(STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Open input data file
C
      CALL DSA_INPUT ('CDATA','CDATA',STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Get the dimensions of the data array and check that its dimensions
C     are acceptable.
C
      CALL DSA_DATA_SIZE ('CDATA',2,NDIM,DIMS,NELM,STATUS)
      IF (STATUS.NE.0) GOTO 500

      NELM=1
      MAXDIM=1
      CHANGE=.FALSE.
      DO I=1,NDIM
         CALL FIG_C06CHK(DIMS(I),NEXT,PREV)
         IF (DIMS(I).NE.NEXT) THEN
            IF (.NOT.CHANGE) CALL PAR_WRUSER(' ',STATUS)
            STRING=ICH_CI(I)
            NPTR=ICH_LEN(STRING)+1
            STRING(NPTR:)=GEN_NTH(I)//' dimension of input data ('//
     :                                                  ICH_CI(DIMS(I))
            NPTR=ICH_LEN(STRING)+1
            STRING(NPTR:)=') cannot be used for an FFT'
            CALL PAR_WRUSER(STRING(:NPTR+26),STATUS)
            CALL PAR_WRUSER('because of the way it factorises.',STATUS)
            STRING='(The closest acceptable values are '//ICH_CI(PREV)
            NPTR=ICH_LEN(STRING)+1
            STRING(NPTR:)=' and '//ICH_CI(NEXT)
            NPTR=ICH_LEN(STRING)+1
            STRING(NPTR:)='.)'
            CALL PAR_WRUSER(STRING(:NPTR+1),STATUS)
            CHANGE=.TRUE.
         END IF
         NELM=NELM*DIMS(I)
         IF (MAXDIM.LT.DIMS(I)) MAXDIM=DIMS(I)
      END DO
      IF (CHANGE) THEN
         FAULT=.TRUE.
         GO TO 500
      END IF
C
C     Get name of resulting file and copy original if transform
C     is not to be in situ.
C
      CALL DSA_OUTPUT('OUTPUT','OUTPUT','CDATA',0,0,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Map the output data array.
C     Map the real and imaginary arrays.
C
C     CALL DSA_MAP_DATA('OUTPUT','UPDATE','DOUBLE',DPTR,SLOT,STATUS)
C     CALL DSA_MAP_IMAGINARY('OUTPUT','UPDATE','DOUBLE',IPTR,SLOT,
C    :                        STATUS)
      CALL DSA_MAP_COMPLEX('OUTPUT','UPDATE','DOUBLE',DPTR,IPTR,SLOT,
     :                     STATUS)
C
C     Work out how much workspace is needed and grab it.  The FFT
C     routines need a) for 1D data, an array the same size as the
C     data, or b) for nD data, an array 3 times the length of the
C     largest dimension.  The data rotator needs an array the same
C     size as the data arrays.
C
      IF (NDIM.EQ.1) THEN
         NWORK=6*DIMS(1)+15
      ELSE
         NWORK=6*MAXDIM+15
      END IF
      NWORK=MAX(NWORK,NELM)

      BDOUB=DSA_TYPESIZE('DOUBLE',STATUS)
      CALL DSA_GET_WORK_ARRAY(NWORK,'DOUBLE',WPTR,SLOT,STATUS)
      IF(STATUS.NE.0)GOTO 500
C
C     For BFFT, re-order the data so the zero frequency term is at
C     the start, before applying the transform.
C
      IF (.NOT.FORWARD) THEN
         DO I=1,NDIM
            ROTS(I)=-(DIMS(I)-1)/2
         END DO
         CALL GEN_DROTND(NELM,NDIM,DIMS,ROTS,%VAL(CNF_PVAL(DPTR)),
     :                   %VAL(CNF_PVAL(WPTR)))
         CALL GEN_MOVE(NELM*BDOUB,%VAL(CNF_PVAL(WPTR)),
     :                 %VAL(CNF_PVAL(DPTR)))
         CALL GEN_DROTND(NELM,NDIM,DIMS,ROTS,%VAL(CNF_PVAL(IPTR)),
     :                   %VAL(CNF_PVAL(WPTR)))
         CALL GEN_MOVE(NELM*BDOUB,%VAL(CNF_PVAL(WPTR)),
     :                 %VAL(CNF_PVAL(IPTR)))
      END IF
C
C     Perform the FFT
C
      RFLAG=.FALSE.
      CALL FIG_BFFT(NDIM,DIMS,FORWARD,RFLAG,%VAL(CNF_PVAL(DPTR)),
     :              %VAL(CNF_PVAL(IPTR)),%VAL(CNF_PVAL(WPTR)),NWORK,
     :              FSTAT)
      IF (FSTAT.NE.0) THEN
         FAULT=.TRUE.
         GO TO 500
      END IF
C
C     For FFT, re-order the data so the zero frequency term of the
C     result array is in the middle.
C
      IF (FORWARD) THEN
         DO I=1,NDIM
            ROTS(I)=(DIMS(I)-1)/2
         END DO
         CALL GEN_DROTND(NELM,NDIM,DIMS,ROTS,%VAL(CNF_PVAL(DPTR)),
     :                   %VAL(CNF_PVAL(WPTR)))
         CALL GEN_MOVE(NELM*BDOUB,%VAL(CNF_PVAL(WPTR)),
     :                 %VAL(CNF_PVAL(DPTR)))
         CALL GEN_DROTND(NELM,NDIM,DIMS,ROTS,%VAL(CNF_PVAL(IPTR)),
     :                   %VAL(CNF_PVAL(WPTR)))
         CALL GEN_MOVE(NELM*BDOUB,%VAL(CNF_PVAL(WPTR)),
     :                 %VAL(CNF_PVAL(IPTR)))
      END IF
C
C     If this is a forward transform, then any .X etc structures are
C     no longer applicable, so rename them and create new axis
C     structures that reflect the transformed data.  On a reverse
C     transform, delete the new structures and rename the old ones back.
C
      IF (FORWARD) THEN
         DO I=1,NDIM
C           CALL DSA_SAVE_AXIS('OUTPUT',I,STATUS)
            CALL DSA_COERCE_AXIS_DATA('OUTPUT',I,'FLOAT',1,DIMS(I),
     :                                STATUS)
            CALL DSA_MAP_AXIS_DATA('OUTPUT',I,'WRITE','FLOAT',
     :                             WPTR,SLOT,STATUS)
            CALL FIG_NQVALS(DIMS(I),%VAL(CNF_PVAL(WPTR)))
            CITEMS(1)='Nyquist units  '
            CITEMS(2)='Frequency      '
            CALL DSA_SET_AXIS_INFO('OUTPUT',I,2,CITEMS,0,NITEMS,STATUS)
         END DO
      ELSE
         DO I=1,NDIM
            CALL DSA_DELETE_AXIS('OUTPUT',I,STATUS)
C           CALL DSA_RESTORE_AXIS('OUTPUT',I,STATUS)
         END DO
      END IF

  500 CONTINUE
C
C     Close down everything
C
      CALL DSA_CLOSE(STATUS)
      IF (FAULT) CALL FIG_SETERR

      END
C+
      SUBROUTINE FIG_NQVALS (NELM,DATA)
C
C     F I G _ N Q V A L S
C
C     FFT/BFFT utility.  Fills the elements of an axis array with
C     values in the range -1 to +1, as required by an axis data array
C     following its Fourier transformation.
C
C     Parameters -  (">" input, "<" output)
C
C     (>) NELM    (Integer) The number of elements in the array.
C     (<) DATA    (Real array DATA(NELM)) The axis data array.
C
C     Common variables - None
C
C     Subroutines / functions used - None
C
C                                                    KS / AAO 8th Oct 1986
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NELM
      REAL DATA(NELM)
C
C     Local variables
C
      INTEGER I
      REAL STEP
C
C     Set axis values.  Note that the final element is always +1.0,
C     zero is the middle element (if NELM is odd) or the element on
C     the low side of the middle (if NELM is even).  The first element
C     will be -1.0 if NELM is odd and -1.0+step is NELM is even.  This
C     all follows from the way the C06 routines handle the transform
C     and how the data is rotated afterwards.
C
      STEP=1.0/FLOAT(INT(NELM/2))
      DO I=1,NELM
         DATA(I)=1.0-FLOAT(NELM-I)*STEP
      END DO
C
      END
