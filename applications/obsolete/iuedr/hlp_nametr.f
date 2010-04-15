      SUBROUTINE hlp_NAMETR (KMD, BEFORE, AFTER, J)
*+
*  - - - - - - -
*   H L P _ N A M E T R
*  - - - - - - -
*
*  Translate a HELP library name into an actual file name for use
*  in Fortran OPEN statements.
*
*  This is a SPECIFIC IMPLEMENTATION of a routine supplied by the
*  calling package.  It sandwiches the given name between a prefix
*  and a suffix.  As well as providing the mandatory "translate"
*  function (K=0), this implementation also provides for the
*  setting up and enquiring of the prefix and suffix BEFOREgs (K=1-4).
*
*  Given:
*     KMD       i        command:  0 = translate
*                                  1 = specify prefix
*                                  2 = specify suffix
*                                  3 = enquire prefix
*                                  4 = enquire suffix
*     BEFORE    c*(*)    input BEFOREg: for KMD=0, HELP library name
*                                          KMD=1, prefix
*                                          KMD=2, suffix
*                                          KMD=3, not used
*                                          KMD=4, not used
*
*  Returned:
*     AFTER     c*(*)    output BEFOREg: for KMD=0, filename for OPEN
*                                       for KMD=1, not used
*                                       for KMD=2, not used
*                                       for KMD=3, prefix
*                                       for KMD=4, suffix
*     J         i        status:   0 = OK
*                                -16 = destination BEFOREg too small
*                                -17 = illegal KMD, or no translation
*                                      possible
*
*  Notes:
*
*  1)  See the declarations below for the maximum allowed prefix and
*      suffix sizes LPMAX, LSMAX.
*
*  2)  If the prefix is all spaces, the output BEFOREg returned is
*      the same as the input BEFOREg.
*
*  3)  Other implementations, which exploit environment variables,
*      handle uppercase/lowercase, use lookup tables, and so on,
*      will be developed as the need arises.
*
*  Called:  hlp_LENGTH
*
*  P.T.Wallace   Starlink   30 July 1992
*  M.J.Clayton   Starlink   02 September 1994 IUEDR Vn. 3.1-3
*     Removed PSX_UNAME Call.
*-

      IMPLICIT NONE

      INCLUDE 'CMPSX'

      INTEGER KMD
      CHARACTER*(*) BEFORE,AFTER
      INTEGER J

      INTEGER hlp_LENGTH

      INTEGER L

*  Prefix and suffix, and their lengths excluding trailing spaces
      INTEGER LPMAX,LSMAX
      PARAMETER (LPMAX=100,LSMAX=20)
      CHARACTER PREFIX*(LPMAX),SUFFIX*(LSMAX)
      INTEGER LP,LS
      SAVE PREFIX,SUFFIX,LP,LS
      DATA PREFIX,SUFFIX,LP,LS / ' ',' ',0,0 /

      INTEGER status,i

      CHARACTER*8   type

      INTEGER        filenamesize
      PARAMETER      ( filenamesize = 255 )
      CHARACTER*(filenamesize)   fn
      CHARACTER*(filenamesize)   lfn
      CHARACTER*(filenamesize)   env_path
      LOGICAL        ftrans
      INTEGER        nchar
      INTEGER        lindex


*
*  Switch according to command value
*  ---------------------------------

      GO TO (1000,1100,1200,1300,1400),KMD+1
*            tran setp sets enqp enqs

*  Illegal KMD value.
      GO TO 9910

*
*  KMD=0: Translate library name to filename
*  -----------------------------------------

 1000 CONTINUE



*  Compatible lengths?
      IF (hlp_LENGTH(BEFORE).LE.LEN(AFTER)) THEN
        fn = before
        nchar = hlp_length ( before )
        CALL CHR_LCASE ( fn )
        LFN = FN
        type = ' '
        ftrans = .FALSE.
!        IF ( sysname .NE. 'VMS' ) THEN
         DO i = 1 , nchar
           IF ( lfn(i:i) .EQ. ':' ) THEN
            ftrans = .TRUE.
            lindex = i
            type = 'LOGICAL'
           END IF
         END DO
!        END IF
        IF ( lfn(1:1) .EQ. '$' ) THEN
         DO i = 1 , nchar
           IF ( lfn(i:i) .EQ. '/' ) THEN
            ftrans = .TRUE.
            lindex = i
            type = 'ENVIR'
           END IF
         END DO
        END IF
        IF ( ftrans ) THEN
           CALL PSX_GETENV ( fn(1:lindex-1), env_path , status )
           IF ( STATUS .NE. 0 ) THEN
              CALL CHR_UCASE ( fn )
              CALL PSX_GETENV ( fn(1:lindex-1), env_path , status )
           END IF
           IF ( STATUS .NE. 0 ) THEN
              env_path = fn(1:lindex)
              lindex = lindex + 1
           END IF
           i = filenamesize
           DO WHILE ( env_path(i:i) .EQ. ' ' )
             i = i - 1
           END DO
           IF ( type .EQ. 'LOGICAL' .AND. sysname .NE. 'VMS') THEN
             fn = env_path(1:i) // '/' // fn(lindex+1:)
             nchar = nchar - lindex + i + 1
             IF ( fn(nchar:nchar) .EQ. '/' ) THEN
               fn=fn(1:nchar-1)
               nchar = nchar-1
             END IF
           ELSE IF ( type .EQ. 'ENVIR' ) THEN
             fn = env_path(1:i) // fn(lindex+1:)
             nchar = nchar - lindex + i
           END IF
        ELSE IF ( sysname .NE. 'VMS' ) THEN
           fn = PREFIX(1:HLP_LENGTH(PREFIX)) // fn
           fn = fn(1:HLP_LENGTH(fn)) // SUFFIX
        END IF
        IF ( fn(nchar-3:nchar) .NE. '.shl' ) fn=fn(1:nchar)//'.shl'
        after = fn
!!!        type *,'help library ',after
        status = 0
      ELSE

*     No: error.
         status=-16
      END IF

*  OK exit.
      GO TO 9990

*
*  KMD=1: Specify prefix
*  ---------------------

 1100 CONTINUE

*  Length of prefix.
      L=hlp_LENGTH(BEFORE)

*  Error if insufficient room in internal BEFOREg.
      IF (L.GT.LPMAX) GO TO 9900

*  Store the prefix.
      IF (BEFORE.EQ.' ') THEN
         PREFIX=' '
         LP=0
      ELSE
         PREFIX=BEFORE
         LP=L
      END IF

*  OK exit.
      GO TO 9990

*
*  KMD=2: Specify suffix
*  ---------------------

 1200 CONTINUE

*  Length of suffix.
      L=hlp_LENGTH(BEFORE)

*  Error if insufficient room in internal BEFOREg.
      IF (L.GT.LSMAX) GO TO 9900

*  Store the prefix.
      IF (BEFORE.EQ.' ') THEN
         SUFFIX=' '
         LS=0
      ELSE
         SUFFIX=BEFORE
         LS=L
      END IF

*  OK exit.
      GO TO 9990

*
*  KMD=3: Enquire prefix
*  ---------------------

 1300 CONTINUE

*  Copy the prefix.
      AFTER=PREFIX

*  Error if truncation has occurred.
      IF (AFTER.NE.PREFIX) GO TO 9900

*  OK exit.
      GO TO 9990

*
*  KMD=4: Enquire suffix
*  ---------------------

 1400 CONTINUE

*  Copy the suffix.
      AFTER=SUFFIX

*  Error if truncation has occurred.
      IF (AFTER.NE.SUFFIX) GO TO 9900

*  OK exit.
      GO TO 9990

*
*  Exits
*  -----

*  BEFOREg overflow.
 9900 CONTINUE
      J=-16
      GO TO 9999

*  General error.
 9910 CONTINUE
      J=-17
      GO TO 9999

*  OK.
 9990 CONTINUE
      J=0

 9999 CONTINUE

      END
