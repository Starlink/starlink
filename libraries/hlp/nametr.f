      SUBROUTINE hlp_NAMETR (KMD, STRIN, STROUT, J)
*+
*  - - - - - - -
*   N A M E T R
*  - - - - - - -
*
*  Translate a help library name into an actual file name for use
*  in Fortran OPEN statements.
*
*  This is a SPECIFIC IMPLEMENTATION of a routine supplied by the
*  calling package.  It sandwiches the given name between a prefix
*  and a suffix.  As well as providing the mandatory "translate"
*  function (K=0), this implementation also provides for the
*  setting up and enquiring of the prefix and suffix strings (K=1-4).
*
*  Given:
*     KMD       i        command:  0 = translate
*                                  1 = specify prefix
*                                  2 = specify suffix
*                                  3 = enquire prefix
*                                  4 = enquire suffix
*     STRIN     c*(*)    input string: for KMD=0, help library name
*                                          KMD=1, prefix
*                                          KMD=2, suffix
*                                          KMD=3, not used
*                                          KMD=4, not used
*
*  Returned:
*     STROUT    c*(*)    output string: for KMD=0, filename for OPEN
*                                       for KMD=1, not used
*                                       for KMD=2, not used
*                                       for KMD=3, prefix
*                                       for KMD=4, suffix
*     J         i        status:   0 = OK
*                                -16 = destination string too small
*                                -17 = illegal KMD, or no translation
*                                      possible
*
*  Notes:
*
*  1)  See the declarations below for the maximum allowed prefix and
*      suffix sizes LPMAX, LSMAX.
*
*  2)  If the prefix is all spaces, the output string returned is
*      the same as the input string.
*
*  3)  Other implementations, which exploit environment variables,
*      handle uppercase/lowercase, use lookup tables, and so on,
*      will be developed as the need arises.
*
*  Called:  hlp_LENGTH
*
*  P.T.Wallace   Starlink   29 August 1995
*-

      IMPLICIT NONE

      INTEGER KMD
      CHARACTER*(*) STRIN,STROUT
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

*  Length of library name.
      L=hlp_LENGTH(STRIN)

*  Error if insufficient room in output string.
      IF (LP+L+LS.GT.LEN(STROUT)) GO TO 9900

*  OK: translate the string.
      IF (LP.EQ.0) THEN
         STROUT=STRIN(:L)//SUFFIX
      ELSE
         STROUT=PREFIX(:LP)//STRIN(:L)//SUFFIX
      END IF

*  OK exit.
      GO TO 9990

*
*  KMD=1: Specify prefix
*  ---------------------

 1100 CONTINUE

*  Length of prefix.
      L=hlp_LENGTH(STRIN)

*  Error if insufficient room in internal string.
      IF (L.GT.LPMAX) GO TO 9900

*  Store the prefix.
      IF (STRIN.EQ.' ') THEN
         PREFIX=' '
         LP=0
      ELSE
         PREFIX=STRIN
         LP=L
      END IF

*  OK exit.
      GO TO 9990

*
*  KMD=2: Specify suffix
*  ---------------------

 1200 CONTINUE

*  Length of suffix.
      L=hlp_LENGTH(STRIN)

*  Error if insufficient room in internal string.
      IF (L.GT.LSMAX) GO TO 9900

*  Store the prefix.
      IF (STRIN.EQ.' ') THEN
         SUFFIX=' '
         LS=0
      ELSE
         SUFFIX=STRIN
         LS=L
      END IF

*  OK exit.
      GO TO 9990

*
*  KMD=3: Enquire prefix
*  ---------------------

 1300 CONTINUE

*  Copy the prefix.
      STROUT=PREFIX

*  Error if truncation has occurred.
      IF (STROUT.NE.PREFIX) GO TO 9900

*  OK exit.
      GO TO 9990

*
*  KMD=4: Enquire suffix
*  ---------------------

 1400 CONTINUE

*  Copy the suffix.
      STROUT=SUFFIX

*  Error if truncation has occurred.
      IF (STROUT.NE.SUFFIX) GO TO 9900

*  OK exit.
      GO TO 9990

*
*  Exits
*  -----

*  String overflow.
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
