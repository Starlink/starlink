      SUBROUTINE hlp_HREADD (STRING, NC, J)
*+
*  - - - - - - -
*   H R E A D D
*  - - - - - - -
*
*  Sequential-access read from the data region of the help library.
*
*  Given (in COMMON):
*     NEXTD      i       address for this sequential access
*
*  Returned (in COMMON):
*     NEXTD      i       address for next sequential access
*
*  Returned (arguments)
*     STRING    c*(*)    input record (not including end-of-string)
*     NC        i        length of record (0 or more)
*     J         i        status: +1 = OK, but quasi-end-of-file
*                                 0 = OK
*                                -1 = HLP system in wrong state
*                                -4 = read error
*                                -7 = attempt to read outside file
*                                -8 = record overflows STRING (see note)
*
*  Notes:
*
*  1)  If the record overflows STRING, the first LEN(STRING) characters
*      are stored in STRING, NC is set to LEN(STRING) and J is set to
*      -8.
*
*  2)  See the source for hlp_HDREAD for side-effects involving
*      locations in COMMON.
*
*  3)  The condition "quasi-end-of-file" means that a zero-length record
*      has been read.
*
*  4)  See also the routine hlp_HREADX, which allows rapid reading of
*      only those records with entries in the index portion of the help
*      library.
*
*  Called:  hlp_HDREAD
*
*  P.T.Wallace   Starlink   13 June 1995
*-

      IMPLICIT NONE

      CHARACTER*(*) STRING
      INTEGER NC,J

      INCLUDE 'helpic'



*  Perform direct-access read
      CALL hlp_HDREAD(NEXTD,STRING,NC,J)

*  Detect quasi-end-of-file
      IF (J.EQ.0.AND.NC.EQ.0) J=1

      END
