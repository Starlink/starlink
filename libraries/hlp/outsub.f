      INTEGER FUNCTION hlp_OUTSUB (STRING)
*+
*  - - - - - - -
*   O U T S U B
*  - - - - - - -
*
*  A minimalist example of the user-supplied OUTSUB routine, which is
*  called by the hlp_HELP routine to output a line of help text.
*
*  !!! This version is suitable for Sun SPARC, DECstation etc, !!!
*  !!! platforms which do not require printer vertical format  !!!
*  !!! codes on WRITEs to the user's terminal.                 !!!
*
*  Given:
*     STRING      c*(*)    help text string
*
*  Returned:
*     hlp_OUTSUB  i        status: always 1=OK
*
*  I/O unit:
*     LUMES   output   prompt
*
*  Note:  The calling sequence for this routine is the same as that for
*         the VAX/VMS routine LIB$PUT_OUTPUT.
*
*  P.T.Wallace   Starlink   25 August 1995
*-

      IMPLICIT NONE

      CHARACTER*(*) STRING

*  I/O unit number
      INTEGER LUMES
      PARAMETER (LUMES=6)     !!! Machine dependent !!!


      WRITE (LUMES,'(A)') STRING
      hlp_OUTSUB=1

      END
