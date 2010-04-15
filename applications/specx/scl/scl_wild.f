*-----------------------------------------------------------------------

      SUBROUTINE SCL_MATCH_WILD (TABLE, TSYMBOL, MATCH_OK)
*  Description:
*     Return MATCH_OK = .TRUE. if string TSYMBOL matches string TABLE;
*     .FALSE. otherwise. TSYMBOL may contain * to match zero or more
*     characters of TABLE

*  History:
*     20-SEP-2000 (AJC):
*        Unused J, END
*        Set LSTR before it's used
*        Handle no wildcard case
*-

      IMPLICIT  NONE

*     Formal parameters:

      CHARACTER TABLE*(*)
      CHARACTER TSYMBOL*(*)
      LOGICAL   MATCH_OK

*     Local variables:

      INTEGER   STAR,    OLDSTAR
      INTEGER   START,   NEXT
      INTEGER   LSUB,    LVAR,    LSTR
      CHARACTER SUBSTRING*32

*     Functions:

      INTEGER   GEN_ILEN

*  Ok, go...

      LVAR = GEN_ILEN (TSYMBOL)
      LSTR = GEN_ILEN (TABLE)

      MATCH_OK = .TRUE.

*     Select starting position in symbol name being tested
*     --- skip right if 'readonly'

      START  =  1
      IF (TABLE(:1).eq.'*') START = 2

*     Find first wildcard in input variable, and if it is not
*     the first character then do exact match for first part of string.
*     Move on start position in current name to immediately after match

      STAR = INDEX (TSYMBOL, '*')
      IF ( STAR .EQ. 0 ) STAR = LVAR + 1

      IF (STAR.NE.1) THEN
        LSUB = STAR - 1
        MATCH_OK = START+LSUB-1.le.LSTR  .and.
     &         TSYMBOL(:LSUB).eq.TABLE(START:START+LSUB-1)
        START = START + LSUB
      END IF

      DO WHILE (MATCH_OK .and. STAR.LT.LVAR)

*       Now we can guarantee that we are working from the wildcard and only
*       need to search for a match of the following bit of the variable name.
*       First find next wildcard (if any)

        OLDSTAR = STAR
        STAR    = INDEX (TSYMBOL(STAR+1:), '*')
        IF (STAR.EQ.0) THEN
          STAR = LVAR+1
        ELSE
          STAR = STAR + OLDSTAR
        END IF
        LSUB = STAR - OLDSTAR - 1

*       Check that there is a further substring; in the case of a final
*       '*' found last time round then everything else matches and we
*       can skip the rest of this loop.

        IF (LSUB.ge.1) THEN
          SUBSTRING = TSYMBOL(OLDSTAR+1:STAR-1)

          IF (START + LSUB - 1 .gt. LSTR) THEN
            MATCH_OK = .FALSE.

          ELSE IF (STAR .le. LVAR) THEN
            NEXT = INDEX (TABLE(START:), SUBSTRING(:LSUB))

*           In the case that a match is found the table symbol is still 'live'
*           -- move to next character after the matching bit

            IF (NEXT .gt. 0) THEN
              START    = START + NEXT + LSUB - 1
              MATCH_OK = START .le. LSTR + 1

*           Otherwise the table symbol is dead and we can try another

            ELSE
              MATCH_OK = .FALSE.
            END IF

          ELSE IF (STAR.gt.LVAR) THEN

*         No final star, so have to match end of string exactly
*         (including not leaving any of the current name over and
*          not needing to begin before current pointer)

            MATCH_OK = LSTR-START+1 .ge. LSUB
     &                 .and. TABLE(LSTR-LSUB+1:LSTR)
     &                     .eq. SUBSTRING(1:LSUB)

          END IF
        END IF
      END DO

      RETURN
      END

*-----------------------------------------------------------------------
