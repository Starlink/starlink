*  History:
*     19 Nov 1993 (hme):
*        Replace STR$UPCASE with CHR_UCASE.
*     15 Jan 1994 (rp):
*        Replace CHR_UCASE with UUCASE
*     31 Jan 1994 (hme):
*        Disuse <> in formats.
*     21 Sep 2000 (ajc):
*        Unused I, IREP, STRING, FORMAT
*-----------------------------------------------------------------------

      SUBROUTINE SET_MAPSCALE (IFAIL)

      IMPLICIT   NONE

*     Formal parameters

      INTEGER    IFAIL

*     Include files

      INCLUDE   'MAPS'
      INCLUDE   'MAPHD'
      INCLUDE   'PLOT2D'
      INCLUDE   'FLAGCOMM'

*     Local variables

      INTEGER   J, K
      INTEGER   IAP
      INTEGER   ILXU
      INTEGER   ISTAT
      INTEGER   JDEF

      REAL*4    TEMP(3)

      CHARACTER MAPTIT(3)*11, TEMPT(2)*5,  AXTIT(3)*6,
     &          SUMST(2)*1, PROMPT*256, APROMPT*32
      CHARACTER BOUNDS(2)*10
      DATA      BOUNDS      /'left,right', 'top,bottom'/

*     Other common blocks

      COMMON /TITLES/ MAPTIT, AXTIT

*     Functions

      INTEGER  GEN_ILEN

*  Ok, go...

      IFAIL=0

      SUMST(1)  = 'A'
      SUMST(2)  = 'I'
      CALL SET_MAPTITLE (MAP_ROTATED, THETA, POS_ANGLE)
      MAPTIT(3) = XAXIS_NAME
      AXTIT(3)  = XAXIS_UNITS

*     First associate co-ordinates with particular axes

      PROMPT =  '"'' Horizontal and vertical map axes?''/'
     &        // ''' Choose from: Longitude('//MAPTIT(1)(:1)
     &        // '), Latitude('//MAPTIT(2)(:1)//'), '
     &        // XAXIS_NAME//'('//XAXIS_NAME(:1)//')'
     &        // '  ['
     &        // MAPTIT(LINK(1))(:1)//','//MAPTIT(LINK(2))(:1)
     &        // ']   ''"'

      CALL GEN_GETSTR2 (1, PROMPT, MAPTIT(LINK(3))(:1),
     &                  ' ', TEMPT(1), JDEF)
      IF (JDEF.EQ.0) THEN
        CALL GEN_GETSTR2 (1,'Please specify the second axis too...',
     &                    MAPTIT(LINK(3))(:1), ' ', TEMPT(2), JDEF)
      END IF

      IF (JDEF.EQ.0)   THEN
        DO J = 1,2
          CALL UUCASE(TEMPT(J))
        END DO
        DO K = 1,2
          LINK(K) = 0
          DO J = 1,3
            IF (TEMPT(K)(:1).EQ.MAPTIT(J)(:1)) LINK(K) = J
          END DO

*         Check legal and different

          IF (LINK(K).EQ.0)   THEN
            IFAIL = 21
            RETURN
          END IF
        END DO
        IF (LINK(2).EQ.LINK(1))   THEN
          IFAIL = 22
          RETURN
        END IF
      ELSE IF (JDEF.GT.2 .OR. JDEF.LT.0) THEN
        IFAIL = 38
        RETURN
      END IF

*     Deduce remaining axis

      LINK(3) = 6 - LINK(1) - LINK(2)
      WRITE (6,'('' Third co-ordinate is ''A4,/)') MAPTIT(LINK(3))

*     Now get range of x and y axes

      DO K = 1,2

        IF (LINK(K).EQ.3) THEN
          APROMPT = ' '
        ELSE
          APROMPT = '(0,0 for auto scaling)'
        END IF

        IAP  = GEN_ILEN (APROMPT)
        ILXU = GEN_ILEN (AXTIT(LINK(K)))

        PROMPT = ' '
        WRITE (*,1020) MAPTIT(LINK(K)), BOUNDS(K),APROMPT(:IAP)
 1020   FORMAT (A4, ' limits (', A10, ') ', A)

        WRITE (PROMPT,1021,IOSTAT=ISTAT) AXTIT(LINK(K))(:ILXU),
     +       QBEG(LINK(K)), QEND(LINK(K))
 1021   FORMAT (' Range? (', A, ') [', F11.4, ',', F11.4,'] ')

        CALL GEN_GETR4A (PROMPT, TEMP, 2, ' ', TEMP, JDEF)
        IF (JDEF.EQ.0) THEN
          QBEG(LINK(K)) = TEMP(1)
          QEND(LINK(K)) = TEMP(2)
        ELSE IF (JDEF.GT.2 .OR. JDEF.LT.0) THEN
          IFAIL = 38
          RETURN
        END IF

      END DO

      CHANGE_PLOT = .FALSE.

      RETURN
      END

*-----------------------------------------------------------------------
