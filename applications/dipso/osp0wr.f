*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
*
*   SP format write subroutines
*
*   "0"  flags breaks in data
*
*   History:
*      30/8/94 - Name changed from SP0WR to OSP0WR (D. Berry)
*
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!

       SUBROUTINE OSP0WR(PARAMS,TITLE,IHHEAD,WORKSZ,WORK,NPOINT,WAVE,
     :                  FLUX,NBREAK,BREAK,SUBCHK,STATUS)

       IMPLICIT NONE
       INCLUDE 'SAE_PAR'
       INTEGER STATUS

       CHARACTER*80 PARAMS
       CHARACTER*79 TITLE, IHHEAD
       INTEGER WORKSZ, NPOINT, NBREAK
       INTEGER BREAK(NBREAK)
       INTEGER INUM, IX, MAXOUT

       REAL WORK(WORKSZ), WAVE(NPOINT), FLUX(NPOINT)

       LOGICAL SUBCHK

*
       IF( STATUS .NE. SAI__OK ) RETURN

       SUBCHK = .TRUE.

       CALL SPLOAD('OSP0WR',PARAMS,TITLE,IHHEAD,WORKSZ,WORK,NPOINT,WAVE,
     :             FLUX,NBREAK,BREAK,INUM,MAXOUT,SUBCHK,STATUS)
       IF( STATUS .NE. SAI__OK ) GO TO 100

       IF (.NOT.SUBCHK ) THEN
          CLOSE (61)
          GOTO 100
       ENDIF

       TITLE(2:79) = TITLE(1:78)
       TITLE(1:1) = ' '
       WRITE (61) TITLE(1:79)
       TITLE(1:) = TITLE(2:)
       IHHEAD(1:79) = ' '
       IHHEAD(1:) = ' Written in SPECTRUM 0 format'
       WRITE (61) IHHEAD(1:79)
       WRITE (61) INUM
       WRITE (61) (WORK(IX),WORK(IX+MAXOUT),IX=1,INUM)
       CLOSE (61)

  100  CONTINUE

       END
