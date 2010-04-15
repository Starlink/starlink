*  History:
*     17 Nov 1993 (hme):
*        For the moment assume /NODUMP and /NOMAP.
*     07 Feb 1994 (hme):
*        Unix vesion of this routine.
*     14 Dec 1999 (tim Jenness)
*        Note that this routine will not work on a vax (untested)
*        The old VMS routine had different calling arguments

*-----------------------------------------------------------------------
      SUBROUTINE QUALIFIERS (NO_DUMP, NEW_DUMP, DUMPNAME,
     &                       NO_MAP,  NEW_MAP,  MAPNAME)

*  Routine to fetch dump and map qualifiers from SPECX command line.
      IMPLICIT NONE

*  Formal parameters:
      LOGICAL NO_DUMP,  NEW_DUMP
      CHARACTER * ( * ) DUMPNAME
      LOGICAL NO_MAP,   NEW_MAP
      CHARACTER * ( * ) MAPNAME

*  Local variables:

      LOGICAL USED( 4 )             ! Block double interpretation
      INTEGER I                     ! Loop variable
      INTEGER NWORD                 ! So many parameters given
      CHARACTER * ( 80 ) WORD( 4 )  ! The parameter values

*  Internal references:

      INTEGER IARGC                 ! Tell how many parameters

*  Set the defaults. (The file names are given and returned.)
*  Using old map is permitted only if a dump is used as well.

      NO_DUMP     = .FALSE.
      NO_MAP      = .FALSE.
      NEW_DUMP    = .FALSE.
      NEW_MAP     = .FALSE.

*  See how many words there are on the command line.

      NWORD = IARGC()

*  If there are more than four words, give a warning and ignore them.

      IF ( NWORD .GT. 4 ) THEN
         WRITE( *, * ) 'Warning: Too many command parameters, ' //
     :      'will use default settings.'

*  Else (not more than four words).

      ELSE

*     Get the words.

         DO 1 I = 1, NWORD
            CALL GETARG( I, WORD(I) )
            USED(I) = .FALSE.
 1       CONTINUE

*     Look out for a nodump option "-nd".

         DO 2 I = 1, NWORD
            IF ( WORD(I) .EQ. '-nd' ) THEN
               NO_DUMP     = .TRUE.
               USED(I)     = .TRUE.
            END IF
 2       CONTINUE

*     Look out for a dump specification "-d <dumpfile>".
         DO 3 I = 1, NWORD - 1
            IF ( .NOT. USED(I) .AND. .NOT. USED(I+1) .AND.
     :            WORD(I) .EQ. '-d' ) THEN
               NEW_MAP     = .TRUE.
               DUMPNAME    =  WORD(I+1)
               USED(I)     = .TRUE.
               USED(I+1)   = .TRUE.
            END IF
 3       CONTINUE

*     Look out for a nomap option "-nm".
         DO 4 I = 1, NWORD
            IF ( .NOT. USED(I) .AND. WORD(I) .EQ. '-nm' ) THEN
               NO_MAP      = .TRUE.
               USED(I)     = .TRUE.
            END IF
 4       CONTINUE

*     Look out for a dump specification "-m <mapfile>".
         DO 5 I = 1, NWORD - 1
            IF ( .NOT. USED(I) .AND. .NOT. USED(I+1) .AND.
     :            WORD(I) .EQ. '-m' ) THEN
               NEW_MAP     = .TRUE.
               MAPNAME     =  WORD(I+1)
               USED(I)     = .TRUE.
               USED(I+1)   = .TRUE.
            END IF
 5       CONTINUE
      END IF

*  Return.
      END
