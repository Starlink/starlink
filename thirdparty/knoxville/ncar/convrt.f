      PROGRAM CONVRT
*+
*  Name:
*     CONVRT

*  Purpose:
*     To convert ASCII file EZMAPDAT on UNIT 1 to binary on UNIT 2.

*  Language:
*     Starlink Fortran 77

*  Description:

*  Authors:
*     NCAR: National Center for Atmospheric Research, Boulder.
*     PCTR: P.C.T. Rees (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1987 (NCAR):
*        Original version.
*     22-JAN-1992 (PCTR):
*        Tidy-up and UNIX version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Local Variables:
      INTEGER I
      INTEGER ICOUNT
      INTEGER IDLS
      INTEGER IDRS
      INTEGER IGID
      INTEGER NPTS

      REAL FLIM( 4 )
      REAL PNTS( 200 )

      CHARACTER * 80 CHAR

*.

*  Initialise the line counter.
      ICOUNT = 0

*  Loop to read past the NCAR banner.
      DO 1 I = 1, 10
         READ( 1, 4 ) CHAR
    1 CONTINUE

*  Loop to read each line of source.
    2 CONTINUE

*     Read the line of data.
         READ( 1, 5, END=3 ) NPTS, IGID, IDLS, IDRS,
     :                       ( FLIM( I ), I = 1, 4 )
         IF ( NPTS .GT. 1 ) READ( 1, 6, END=3 ) ( PNTS( I ), 
     :                                            I = 1, NPTS )

*     Increment the line counter.
         ICOUNT = ICOUNT + 1

*     Write the line of data.
*         WRITE( *, * ) NPTS, IGID, IDLS, IDRS, ( FLIM( I ), I = 1, 4 )
         WRITE( 2 ) NPTS, IGID, ( FLIM( I ), I = 1, 4 ),
     :             ( PNTS( I ), I = 1, NPTS )
      GO TO 2
    3 STOP

    4 FORMAT( A80 )
    5 FORMAT( 4I4, 4F8.3 )
    6 FORMAT( 10F8.3 )

      END
