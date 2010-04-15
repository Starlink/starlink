*+DBS_READ_DSCF  Reads dscf data
*     1987 Aug	M Harris	1st version, part of DBS_OPEN
*     1989 Jan	   ::		Separate routine
*     1992 Apr	M. Duesterhaus  remove VAX RTL calls
*     1993 May  P. Brisco       We now calculate the field positions.
*     1993 Jun  P. Brisco       If the RECORDSIZE parameter has a length of
*                               less than 128, we calculate it for the user.
*     1994	M Ricketts	Mods to skip null fields
*******************************************************************
      SUBROUTINE DBS_READ_DSCF(REF_NO, IDSCF, POINTER )
      IMPLICIT NONE


* Calling Arguments
      INTEGER       REF_NO	! Reference number of data set.
      INTEGER IDSCF
      INTEGER POINTER		! if .le. 0 then new dscf, else gives ref no.

*  Global Variables
      INCLUDE 'aaa_dbs_params.inc'
      INCLUDE 'com_dbs_field.inc'
      INCLUDE 'com_dbs_chars.inc'
      INCLUDE 'com_dbs_iof.inc'
      INCLUDE 'com_dbs_bytes.inc'

*  Local Variables
      INTEGER MAXFILES_PLUS1
      PARAMETER(MAXFILES_PLUS1=MAX_FILE_COUNT + 1)
      CHARACTER*20 COMMENT	! Value of comment field.
      CHARACTER*17 FLD        	! Temporary character variable.
     & ,           NULV		! Null value for field data.
     & ,           TMPFLD	! Last field name.
      CHARACTER*1  KEYWORD	! Type of input information.
      INTEGER current_start_pos ! Starting position of current field.
      INTEGER	EXPONENT    	! Exponent for field data.
     & ,    I, I1 , J , K , N	! Loop variables.
     & ,           NK , P1 , P2	! Temporary variables.
     & ,           SIZ         	! Array size.
      CHARACTER*60 BLANK
      integer LENC, NB		! length comment, dummy
      CHARACTER*1 COND_CHAR
      LOGICAL LOCL		! If Located logical field

*  Functions
      INTEGER MDH_ENDWORD
      INTEGER  MDH_CTOI
*  ________________________  Executable Code  ______________________________
      DO  J=1,60
         BLANK(J:J) = ' '
      END DO

      IF (POINTER .LE. 0) THEN
         recsize(ref_no) = 0    !   Set number of bytes to zero.
         N  = 1                 !   Set number of fields indicator.
         NK = 0                 !   Set number of keyfields.
         KEYWORD = ' '
         current_start_pos = 1

         DO WHILE (KEYWORD .NE. 'E' ) ! Process all fields and parameters.
            start(n,ref_no) = current_start_pos
            READ(IDSCF, '(X,A1,X,A17,I6,X,A6,I4,X,A10,X,A6,X,A17,X,A20)')
     &           KEYWORD,FLD,LENTH(N,REF_NO),FORMAT(N,REF_NO),EXPONENT
     &           ,UNIT(N,REF_NO),NULFORMAT(N,REF_NO),NULV,COMMENT
                                !  Read in first description from DSCF file.
            LENC = MDH_ENDWORD(COMMENT)
            p1 = INDEX(fld , '(') + 1
            p2 = INDEX(fld , ')') - 1

            IF (p1 .GT. 0 .AND. p2 .GE. p1) THEN
               siz = MDH_CTOI(fld(p1:p2))
            ELSE
               siz = 1
            ENDIF

            current_start_pos = current_start_pos + (lenth(n,ref_no)*siz)

            IF ( KEYWORD .EQ. 'C' ) FLD = TMPFLD !  Get last field.

            IF ( KEYWORD .EQ. 'F' ) THEN !  If descriptor is for a field.

               IF (NULV.EQ.' ') THEN
                  NULVALUE ( N, REF_NO) = BLANK(1:LENTH(N,REF_NO))
               ELSE
                  NULVALUE( N , REF_NO ) = NULV
               END IF
               FIELD( N , REF_NO ) = FLD !   Get field from temporary variable.

               CONDITIONAL(N, REF_NO) = ' '
               COND_OFFSET(N, REF_NO) = 0
               IF (LENC .EQ.20) THEN						!   may be conditional read
                 IF (COMMENT(18:18) .EQ. ':' .AND. COMMENT(20:20) .EQ. ':') CONDITIONAL(N, REF_NO) = COMMENT(19:19)
               END IF

               IF ( CONDITIONAL(N, REF_NO) .EQ. 'L') THEN
                 LOCL = .FALSE.
                 NB = N-1
                 DO WHILE ((NB .GE.1) .AND. .NOT. LOCL)
                   IF ( FORMAT(NB, REF_NO) .EQ. 'L*1') THEN
                      LOCL = .TRUE.
                      COND_OFFSET(N, REF_NO) = N - NB				! skip if prev logical false
                   END IF
                   NB = NB-1
                 END DO
                 IF ((.NOT.LOCL) .AND. NB .EQ.1) THEN
                   WRITE(*,*) ' ERROR decoding dscf, no logical before ',field(n,ref_no)
                   CONDITIONAL(N, REF_NO) = ' '
                 END IF
               END IF

               N = N + 1        !   Increase number of fields indicator by 1.

            ELSE IF ( KEYWORD .EQ. 'G' ) THEN !  Else if descriptor for vector.

               P1 = INDEX( FLD , '(' ) + 1 !  Find position of first bracket.
               P2 = INDEX( FLD , ')' ) - 1 !  Find position of second bracket.
               SIZ = MDH_CTOI( FLD( P1:P2 ) )
               FIELD( N , REF_NO )( : P1 - 1 ) = FLD !  Get field from temp var
               FIELD( N , REF_NO )( P1 : P1 )  = '1' !  Make 1st field of vect
               FIELD( N , REF_NO )( P1 + 1 : ) = ')' !  Replace the bracket.

               IF (NULV.EQ.' ') THEN
                  NULVALUE ( N, REF_NO) = BLANK(1:LENTH(N,REF_NO))
               ELSE
                  NULVALUE( N , REF_NO ) = NULV
               END IF

               COND_CHAR = ' '
               IF (LENC .EQ.20) THEN						!   may be conditional read
                IF (COMMENT(18:18) .EQ. ':' .AND. COMMENT(20:20) .EQ. ':') COND_CHAR = COMMENT(19:19)
               END IF
               CONDITIONAL(N, REF_NO) = COND_CHAR
               COND_OFFSET(N, REF_NO) = 0
               IF ( CONDITIONAL(N, REF_NO) .EQ. 'L') THEN
                 LOCL = .FALSE.
                 NB = N-1
                 DO WHILE ((NB .GE.1) .AND. .NOT. LOCL)
                   IF ( FORMAT(NB, REF_NO) .EQ. 'L*1') THEN
                      LOCL = .TRUE.
                      COND_OFFSET(N, REF_NO) = N - NB				! skip if prev logical false
                   END IF
                   NB = NB-1
                 END DO
                 IF ((.NOT.LOCL) .AND. NB .EQ.1) THEN
                    WRITE(*,*) ' ERROR decoding dscf, no logical before ',field(n,ref_no)
                    CONDITIONAL(N, REF_NO) = ' '
                 END IF
               END IF

               DO I1 = 2 , SIZ  !  Do for each member of the vector.
                  K = N + I1 - 1 !   Get field number.
                  FIELD( K , REF_NO ) = FLD ! First part of name in common.
                  P2 = P1 + INT( LOG10( REAL( I1 ) ) ) ! Find end of num pos.
                  IF (I1.LT.10) THEN
                     WRITE( FIELD( K , REF_NO )( P1 : P2 )
     &                    ,         '( I1 )' ) I1 !   Put number in field name.
                  ELSE
                     WRITE( FIELD( K , REF_NO )( P1 : P2 )
     &                    ,         '( I2 )' ) I1 !   Put number in field name.
                  END IF
                  FIELD( K , REF_NO )( P2 + 1 : ) = ')' ! Replace the bracket.
                  NULVALUE( K , REF_NO )  = NULV
                  NULFORMAT( K , REF_NO ) = NULFORMAT( N , REF_NO )
                                !   Put null format in common.
                  FORMAT( K , REF_NO )    = FORMAT( N , REF_NO )
                                !   Put input format in common.
                  START(K,REF_NO) = START(K - 1,REF_NO) + LENTH(N,REF_NO)
                                !   Put start in common.
                  LENTH( K , REF_NO )     = LENTH( N , REF_NO )
                                !   Put length in common.
                  UNIT( K , REF_NO )      = UNIT( N , REF_NO )
                                !   Put units in common.
                  CONDITIONAL(K, REF_NO)  = COND_CHAR
                  IF ( COND_CHAR .EQ. 'A' .OR. COND_CHAR .EQ. 'L') THEN
                     COND_OFFSET(K,REF_NO)     = -1  ! Skip if it's got anything
                  ELSE
                     COND_OFFSET(N, REF_NO) = 0
                  END IF

               END DO           !  End of vector loop.

               N = K + 1        !  Update number of fields.

            ELSE IF ( KEYWORD .EQ. 'P' ) THEN ! Else if keyword is a parameter.

               IF ( FLD .EQ. 'NRECORDS' ) THEN ! If param is num of records.
                  READ ( NULV , '(' // NULFORMAT( N , REF_NO )
     &                 //           ')' ) NRECORDS( REF_NO ) ! Get num of recs

               ELSE IF (FLD .EQ. 'RECORDSIZE') THEN ! If param is size of recs
                  READ ( NULV , '(' // NULFORMAT( N , REF_NO )
     &                 //           ')' ) RECSIZE( REF_NO ) ! Get num of bytes

               ELSE IF ( FLD(:8) .EQ. 'KEYFIELD' ) THEN ! If keyfield param
                  NK = NK + 1   !   Increment number of keyfields.
                  KEYFIELD( NK , REF_NO ) = NULV !   Get keyfield.

                  IF( COMMENT .EQ. 'DESCENDING' ) THEN !   If comment etc.
                     ASCEND( NK , REF_NO ) = 'F' !    Store indicator.
                  ELSE
                     ASCEND( NK , REF_NO ) = 'T'
                  END IF

               ELSE IF ( FLD .EQ. 'ORDERING' ) THEN !   Obvious for a while.
                  ORDER( REF_NO ) = NULV

               ELSE IF ( FLD .EQ. 'JOIN_CRITERION' .OR.
     &                 FLD .EQ. 'CRITERION' ) THEN

                  IF ( FLD .EQ. TMPFLD ) THEN !  If criterion not started.
                     CRITERION( REF_NO ) = COMMENT !   Store criterion.
                  ELSE          !  Else.
                     P1 = INDEX( CRITERION( REF_NO ) , ' ' )
                                ! Find end of criterion.
                     CRITERION( REF_NO )( P1 : ) = COMMENT
                                ! Put continuation on end.
                  END IF        ! fld .eq. tmpfld

               END IF           ! fld .eq. 'JOIN_CRITERION .or.

            END IF		! keword .eq. 'P'

            TMPFLD = FLD	! Store last field.
            IF (recsize(ref_no) .LT. current_start_pos .OR.
	1	recsize(ref_no) .LT. 128) recsize(ref_no) = current_start_pos

         END DO                 ! End of outer loop.

         CLOSE ( IDSCF )        !  Close the DSCF file.
         CALL FRELUN ( IDSCF )  !  Free the logical unit number.
         NFIELDS( REF_NO ) = N - 1 !  Allow for the extra N = N+1.
         NKEYS( REF_NO ) = NK   ! Set number of keyfields.

      ELSE                      ! Else if DSCF already in store.
         CRITERION( REF_NO ) = CRITERION( POINTER )
         NRECORDS ( REF_NO ) = NRECORDS ( POINTER )
         RECSIZE  ( REF_NO ) = RECSIZE  ( POINTER )
         ORDER    ( REF_NO ) = ORDER    ( POINTER )
         NFIELDS  ( REF_NO ) = NFIELDS  ( POINTER )

         DO J = 1 , NFIELDS( POINTER )
            FIELD    ( J , REF_NO ) = FIELD    ( J , POINTER )
            UNIT     ( J , REF_NO ) = UNIT     ( J , POINTER )
            FORMAT   ( J , REF_NO ) = FORMAT   ( J , POINTER )
            NULFORMAT( J , REF_NO ) = NULFORMAT( J , POINTER )
            LENTH    ( J , REF_NO ) = LENTH    ( J , POINTER )
            START    ( J , REF_NO ) = START    ( J , POINTER )
         END DO

         NKEYS( REF_NO ) = NKEYS( POINTER )

         DO J = 1 , NKEYS( POINTER )
            KEYFIELD( J , REF_NO ) = KEYFIELD( J , POINTER )
            ASCEND  ( J , REF_NO ) = ASCEND  ( J , POINTER )
         END DO

      END IF                    !  pointer .le. 0

      END
