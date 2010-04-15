*+WFC_SURV         Returns the estimated survey exposure for the ecl. lat.
*  Aug 1992	M. Duesterhaus	Remove VAX specific code
*************************************************************************
      REAL FUNCTION WFC_SURV (DECLAT)

      IMPLICIT NONE

*  Calling Arguments
      DOUBLE PRECISION DECLAT

*  Global Variables
      INCLUDE 'zpidata.inc'
      INCLUDE 'com_form_files.inc'

*  Local Variables, saved
      LOGICAL SURV_READ/.FALSE./		! True when data read
      REAL SURVEXP(-90:89)
      SAVE SURV_READ, SURVEXP

      INTEGER LUN         			! Logical unit number
      INTEGER I, STATUS, ILAT

*-

      IF (.NOT. SURV_READ) THEN

*      Open file with exposure data
	 CALL GETLUN (LUN)
         OPEN(LUN, FILE=DSCFRPS_DATA(:len_dscfrps)//'rps_wfc_survexp',
     &		STATUS='OLD', IOSTAT=STATUS)
*	write(16,*) ' wfc_suirv open status ',status
         IF (STATUS.NE.0) THEN
            WFC_SURV = -1.
            GOTO 90
         END IF

*      Read in exposure for each ecl. lat.

         DO I= -90, 89
            READ(LUN,'(9X,I3,F11.2)') ILAT, SURVEXP(I)
*		if (abs(i).gt.80) write(16,*) ilat,survexp(i)

            IF (ILAT .NE.I) THEN
               WFC_SURV = -2.
               GOTO 90
            END IF
         END DO
         CLOSE(LUN)
	 CALL FRELUN (LUN)
         SURV_READ = .TRUE.
      END IF

      I = MIN ( MAX ( INT( DECLAT * DRTOD) , -90 ), 89 )
      WFC_SURV = SURVEXP(I)

90    CONTINUE
      END



