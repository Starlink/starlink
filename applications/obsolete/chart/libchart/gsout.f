      SUBROUTINE GSOUT( STATUS )
*
*   This subroutine provides an alternative way of creating
*   formatted output.
*
*   In this cas the output includes information about the stars
*   found relating to their use as Guide Stars for the RGO
*   astrometric refractors.
*
*   It is an adaption of subroutine STARS in the ICL1903T version
*   of Chart.
*
*   It was adapted by K F Hartley at RGO on 1-2-83
*
*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*   History:

*   Adapted by R W Argyle at RGO on 26-1-84 to include guide star
*   coordinates for the LPO 1 metre camera
*     3-MAR-1993 (AJJB):
*       STATUS argument added.
*     5-MAR-1993 (Andrew Broderick (AJJB)):
*        STATUS argument added to all calls to routines within Chart
*        which did'nt already have one.
*
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

      INCLUDE 'MAIN'

*  Status:
      INTEGER STATUS             ! Global status

      DOUBLE PRECISION TWOPI,HALFPI,RDSA,RDST,RDDG
      COMMON /CONVF/TWOPI,HALFPI,RDSA,RDST,RDDG
      DOUBLE PRECISION RAO,DECO,X,Y
*-

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*
*   Find out the length of a page
*
      IPAGEL = 36

      IPAGE = 1
*
*   Print report on number of stars found.
*
      WRITE (7,900) NUM
  900 FORMAT (/,' ',10X,'Number of catalogue stars in area =',I5)
*
*   First print the Column headings.
*
      LNUM=38
      CALL PRCOL (0, STATUS )

      JBLOCK = 0
*
*   Now loop around printing details for each star.
*
      DO J=1,NUM
*-
         LNUM=LNUM+1
         JBLOCK = JBLOCK + 1
         IF (LNUM.GT.IPAGEL) THEN
             IPAGE=IPAGE + 1
             LNUM = 1
             JBLOCK = 1
             CALL PRCOL(IPAGE, STATUS )
          ENDIF
         IF (JBLOCK.GT.5) THEN
             WRITE (7,910)
910   FORMAT(' ')
             JBLOCK = 1
         ENDIF
*
*   Now extract the details for this star.
*
         RAO=STAR(1,IP(J))
         DECO=STAR(2,IP(J))
         RMAG=FLOAT(NSTAR(1,IP(J)))/10.0
         NCAT=NSTAR(3,IP(J))
         CALL STARP(J,NCAT,RMAG,RAO,DECO, STATUS )
         CALL PROJ(1,RAO,DECO,X,Y, STATUS )
         CALL OFFS(X,Y, STATUS )
         XC= REAL ( X/RDDG*60.0 )
         YC= REAL( Y/RDDG*60.0 )
*
*   Having found the position, compute the various guide probe
*   positions.
*
         CALL GUID13(XC,YC, STATUS )
         CALL MERZ26(XC,YC, STATUS )
         CALL LPO1M(XC,YC, STATUS )
*
*   This is the end of the loop for the stars
*
      END DO
      END
