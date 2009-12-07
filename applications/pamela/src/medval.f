      REAL FUNCTION MEDVAL( DATA, NPTS )
*
*  Computes the median of an array of real numbers
*
*  Input:
*      DATA      = REAL*4 ARRAY OF DATA VALUES
*      NPTS      = INTEGER*4 NUMBER OF DATA VALUES
*  Output:
*      MEDVAL      = REAL*4 MEDIAN VALUE OF DATA
*
      INTEGER NPTS
      REAL DATA(NPTS)
      PARAMETER (MAXPTS=10000)
      INTEGER IRANK(MAXPTS)
*
*  Compute rank of each data value
*
      IF(NPTS.GT.MAXPTS) THEN
        WRITE(*,*) 'Too many points in MEDVAL'
        NPOINT = MAXPTS
      ELSE
        NPOINT = NPTS
      END IF
      CALL HEAPSORT(NPOINT, DATA, IRANK)
      MEDRANK = (NPOINT+1)/2
      MEDVAL = DATA(IRANK(MEDRANK))
*
*  Average two values if the number of points is even
*
      IF(MEDRANK*2 .EQ. NPOINT) THEN
        MEDVAL = 0.5* ( MEDVAL + DATA(IRANK(MEDRANK+1)) )
      END IF
C
      RETURN
      END
