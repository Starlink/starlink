*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
*
*
*    SUBROUTINE IPSET
*
*    INCREMENTS PLOTTING COLOUR
*
*    IMPORTS:
*      PALMAX (INTEGER) MAXIMUM NO. OF COLOURS IN PALLETTE
*
*    UPDATES:
*      IPAL   (INTEGER) INDEX OF CURRENT PALLETTE COLOUR
*
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
       SUBROUTINE IPSET (IPAL, MAXPAL)
*
*
*    DECLARATIONS
*
*
       INTEGER MAXPAL, IPAL
*
*
*    INCREMENT
*
*
       IPAL = IPAL + 1
       IF (IPAL.GT.MAXPAL) IPAL = 1
*
*
       RETURN
       END
