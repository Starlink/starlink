      SUBROUTINE SPD_UAAK( UNIT, MAXCOL, NCOLS, DUMMY, COLI, VALI,
     :   COMM, ISTAT )
*+
*  Name:
*     SPD_UAAK

*  Purpose:
*     Read values from specified columns in a line of an ASCII file.

*  Language:
*     Fortran

*  Invocation:
*     CALL SPD_UAAK( UNIT, MAXCOL, NCOLS, DUMMY, COLI, VALI, COMM,
*        ISTAT )

*  Description:
*     This routine reads a comment (flagged by ! in the first or second
*     character) or a number of REAL values from a line of an ASCII
*     table. We must allow for the commenting character to be in the
*     second character, since the first character might be needed as
*     Fortran carriage control.

*  Arguments:
*     UNIT = INTEGER (Given)
*        FORTRAN unit from which to read.
*     MAXCOL = INTEGER (Given)
*        How many columns are checked. May be up to 50.
*     NCOLS = INTEGER (Given)
*        How many values to be read.
*     DUMMY = REAL (Given)
*        Value if read failed.
*     COLI( NCOLS ) = INTEGER (Given)
*        The numbers of the columns to be read from.
*     VALI( NCOLS ) = REAL (Returned)
*        The values read. VALI(I) is read from column no. COLI(I),
*        I = 1 ... NCOLS.
*     COMM = CHARACTER * ( * ) (Returned)
*        Comment text if a comment was read.
*     ISTAT = INTEGER (Returned)
*        Error status: 0 for ok, 1 for EOF, 2 for !comment,
*        3 for too few columns, 4 for illegal MAXCOL.

*  Implementation Status:
*     This routine uses a list-directed internal read, and insofar
*     violates the Fortran 77 standard.

*  Authors:
*     hme: Horst Meyerdierks (RAIUB, UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     28 Sep 1988 (hme):
*        Original version.
*     10 Nov 1988 (hme):
*        Don't know what.
*     18 May 1991 (hme):
*        STARLSE prologue.
*     21 Nov 1991 (hme):
*        Allow ! to be in first or second character.
*     27 Nov 1995 (hme):
*        Comply with Fortran standard and no longer use internal
*        list-directed read (FMT=*). Now use FMT='(50F)' where 50 is the
*        limit imposed by the local buffer anyway.
*     07 Dec 1995 (hme):
*        The F format without width or precision is not exactly permitted.
*        It does in fact fail on input that has an E+xx exponent. So we're
*        back with the list-directed internal read.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      INTEGER UNIT
      INTEGER MAXCOL
      INTEGER NCOLS
      REAL DUMMY
      INTEGER COLI( NCOLS )

*  Arguments Returned:
      REAL VALI( NCOLS )
      CHARACTER * ( * ) COMM

*  Status:
      INTEGER ISTAT

*  Local Constants:
*  The number of columns is arbitrarily limitied to 50. This is
*  necessary to dimension the local buffer for all numbers.
      INTEGER COLLIM
      PARAMETER ( COLLIM = 50 )


*  Local Variables:
      INTEGER I                  ! Loop index
      REAL INDATA( COLLIM )      ! Buffer array

*.

*  Defaults for values returned.
      ISTAT=0
      COMM='!!'
      DO I=1,NCOLS
         VALI(I)=DUMMY
      ENDDO

*  Check MAXCOL.
      IF ( MAXCOL .GT. COLLIM ) GO TO 4
      DO I = 1, NCOLS
         IF ( MAXCOL .LT. COLI(I) ) GO TO 4
      ENDDO

*  Read the next record.
      READ( UNIT, '(A)', END = 1 ) COMM
      IF ( COMM(1:1) .EQ. '!' .OR. COMM(2:2) .EQ. '!' ) GO TO 2

*  If not a comment, extract the requested data.
*  First read as many columns from the beginning as are necessary to
*  include the rightmost of the requested column numbers. These are read
*  into the buffer array INDATA.
*  Then pick the required columns (numbers COLI) from the buffer array
*  and store them into the returned array VALI.
      READ( COMM, *, ERR = 3 ) ( INDATA(I), I = 1, MAXCOL )
      DO I = 1, NCOLS
         IF ( COLI(I) .GT. 0 ) VALI(I) = INDATA(COLI(I))
      ENDDO

*  Normal return with status 0.
      RETURN

*  Return with some other status.
    1 ISTAT=1
      RETURN
    2 ISTAT=2
      RETURN
    3 ISTAT=3
      RETURN
    4 ISTAT=4
      RETURN
      END
