*+  IPEAKS - find peaks in image
      SUBROUTINE IPEAKS(STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*      2-May-1989      Richard Saxton
*    History :
*     23 May 90        Imported to ASTERIX (RJV)
*     21 Oct 93 : Removed extra STATUS arguments to MSG_SETx (DJA)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      REAL THRESH
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'IPEAKS Version 1.2-0')
*-
      CALL MSG_PRNT(VERSION)

      IF (.NOT.I_OPEN) THEN
        CALL MSG_PRNT('AST_ERR: image processing system not active')

      ELSEIF (.NOT.I_DISP) THEN
        CALL MSG_PRNT('AST_ERR: no image currently displayed')

        CALL GTR_RESTORE(STATUS)

* Calculate the maximum number of peaks possible, =one every other pixel.
      ELSEIF ((I_IX2-I_IX1+1)*(I_IY2-I_IY1+1)/2.LT.2) THEN
           CALL MSG_PRNT('AST_ERR: too few data points to work on')

      ELSE

* get threshold value
        CALL PAR_GET0R('THRESH',THRESH, STATUS)

        CALL IPEAKS_DOIT(%VAL(I_DPTR),THRESH,STATUS)


      ENDIF

      END


*+  IPEAKS_DOIT - Find local maxima in array
	SUBROUTINE IPEAKS_DOIT(ARRAY,THRESH,STATUS)
* Description :
*     Search for peaks in slices of the first dimension.   eg. if the
*    input array is an X,Y image, the program will take the first value
*    of Y and search the corresponding strip of X values for peaks, then
*    take the next value of Y and search that strip etc..
*    A peak is only counted if it is above a certain counts threshold.
* History :
*   27 June 85:  Original for point source search program (LTVAD::RW)
*   2 May  89:  ISIS version (LTVAD::RDS)
*   23 May 90:  Imported into ASTERIX (RJV)
* Type Definitions :
        IMPLICIT NONE
* Global constants :
        INCLUDE 'SAE_PAR'
        INCLUDE 'DAT_PAR'
* Global variables :
        INCLUDE 'IMG_CMN'
* Import :
	REAL ARRAY(I_NX,I_NY)            ! Input array
        REAL THRESH                      ! Minimum threshold for peak
* Import-Export :
* Export :
* Status :
        INTEGER STATUS
* Local constants :
* Local variables :
        INTEGER J,K,JJ,KK                ! Loop counters
        INTEGER IP                       ! Set to 1 if peak found
        INTEGER NPEAKS
        REAL VAL                         ! local array values
        REAL X,Y
*-

      IF (STATUS.NE.SAI__OK) RETURN

        CALL PGSCH(1.5)

* Look for local peaks
	NPEAKS=0
*
* Loop over the second dimension
	DO J = I_IY1+1,I_IY2-1
*
*     Loop over the first dimension
           DO K = I_IX1+1,I_IX2-1
*
              VAL=ARRAY(K,J)
*
*     Check against threshold
	      IF (VAL.LT.THRESH) THEN
*
	         IP=0
*
              ELSE
*
*     Find if local maximum by testing surrounding pixels
*
                 IP=1
*
                 DO JJ=J-1,J+1
		    DO KK=K-1,K+1
*
	               IF ( .NOT. (KK.EQ.K .AND. JJ.EQ.J) ) THEN
*
	                  IF (ARRAY(KK,JJ) .GE. VAL) THEN
                            IP=0
                          ENDIF
*
	               ENDIF
*
	            ENDDO
		 ENDDO
*
	     ENDIF
*
*     Local maximum if IP set to 1
	     IF (IP.NE.0) THEN
*
	        NPEAKS=NPEAKS+1

*     Get world coords. of peak pixel
                CALL IMG_PIXTOWORLD(REAL(K),REAL(J),X,Y,STATUS)

*     Write position to terminal
                CALL MSG_SETR('X',X)
                CALL MSG_SETR('Y',Y)
                CALL MSG_PRNT('Peak at ^X , ^Y')

*     Mark position on image
                CALL PGPOINT(1,X,Y,2)

	     ENDIF
	  ENDDO
	ENDDO

*     Write number of peaks found
        CALL MSG_PRNT(' ')
        CALL MSG_SETI('N',NPEAKS)
        CALL MSG_PRNT('Total of ^N peaks found')
*
        CALL GCB_SETDEF(STATUS)
*
        END
