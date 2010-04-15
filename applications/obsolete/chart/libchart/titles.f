      SUBROUTINE TITLES( STATUS )
*
*   Subroutine to Plot Titles on Starcharts
*
*   Arguments:
*
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*   History:
*     4-MAR-1993 (AJJB):
*       STATUS argument added.
*     5-MAR-1993 (Andrew Broderick (AJJB)):
*        STATUS argument added to all calls to routines within Chart
*        which did'nt already have one.
*

      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'MAIN'
      INCLUDE 'PLOTDAT'

*  Status:
      INTEGER STATUS             ! Global status

      CHARACTER*1 ISIGN

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Set Zone for Title Block

      CALL SGS_ZONE (0.0,TITLEW,YDIAM,YMAX,IZTITLE,ISTAT)

*   Set World Co-ordinates for this zone

      CALL SGS_SW (0.0,TITLEW,0.0,TITLEH,ISTAT)

*   Select Zone IZTITLE

      CALL SGS_SELZ (IZTITLE,ISTAT)

*   If ARGS Set Pen Colour

      IF (COLOUR) CALL SGS_SPEN (1)

*   Set Beginning of All Strings to Centre Left

      CALL SGS_STXJ ('CL')

      XP = CW
      YP = TITLEH - 3.0 - (CH * 0.5)

*   Draw lines of titles according to block height

      IF ( TWOLINE ) THEN

*   Output First Line of Titles
*   XP and YP are co-ords of beginnings of character strings

         CALL SGS_BTEXT (XP,YP)
         CALL SGS_ATEXT (IDFLD)
         CALL SGS_OTEXT

         XP=XP+13.0*CW
         CALL SGS_BTEXT (XP,YP)
         CALL SGS_ATEXT ('CENTRE : ')
         CALL SGS_OTEXT

         XP=XP+9.0*CW
         CALL TCONV (2,A,0,ISIGN,MHRS,MINS,MSECS,SECS, STATUS )
         CALL SGS_BTEXT (XP,YP)
         IF (MHRS.GE.10) THEN
            CALL SGS_ATXI (MHRS,-2)
         ELSE
            CALL SGS_ATEXT ('0')
            CALL SGS_ATXI (MHRS,-1)
         ENDIF
         CALL SGS_OTEXT

         XP=XP+2.0*CW
         CALL SGS_BTEXT (XP,YP)
         IF (MINS.GE.10) THEN
            CALL SGS_ATXI (MINS,-3)
         ELSE
            CALL SGS_ATEXT (' 0')
            CALL SGS_ATXI (MINS,-1)
         ENDIF
         CALL SGS_OTEXT

         XP=XP+3.0*CW
         CALL SGS_BTEXT (XP,YP)
         IF (MSECS.GE.10) THEN
            CALL SGS_ATXI (MSECS,-3)
         ELSE
            CALL SGS_ATEXT (' 0')
            CALL SGS_ATXI (MSECS,-1)
         ENDIF
         CALL SGS_OTEXT

         XP=XP+3.0*CW
         CALL SGS_BTEXT (XP,YP)
         CALL SGS_ATEXT (' , ')
         CALL SGS_OTEXT

         XP=XP+3.0*CW
         CALL TCONV (1,D,0,ISIGN,MDEGS,MINS,MSECS,X, STATUS )
         CALL SGS_BTEXT (XP,YP)
         CALL SGS_ATEXT (ISIGN)
         CALL SGS_OTEXT

         XP=XP+CW
         CALL SGS_BTEXT (XP,YP)
         IF (MDEGS.GE.10) THEN
            CALL SGS_ATXI (MDEGS,-2)
         ELSE
            CALL SGS_ATEXT ('0')
            CALL SGS_ATXI (MDEGS,-1)
         ENDIF
         CALL SGS_OTEXT

         XP=XP+2.0*CW
         CALL SGS_BTEXT (XP,YP)
         DSECS=FLOAT(MSECS)/60.0
         DMINS=FLOAT(MINS)+DSECS
         IF (DMINS.GE.10.0) THEN
            CALL SGS_ATXR (DMINS,-5,1)
         ELSE
            CALL SGS_ATEXT (' 0')
            CALL SGS_ATXR (DMINS,-3,1)
         ENDIF
         CALL SGS_OTEXT

         XP=XP+7.0*CW
         CALL SGS_BTEXT (XP,YP)
         CALL SGS_ATEXT ('(')
         CALL SGS_OTEXT

         XP=XP+CW
         CALL SGS_BTEXT (XP,YP)
         CALL SGS_ATXR  (EQUIN,0,1)
         CALL SGS_OTEXT

         XP=XP+6.0*CW
         CALL SGS_BTEXT (XP,YP)
         CALL SGS_ATEXT (')')
         CALL SGS_OTEXT

*   Output Second Line of Titles

         YP=YP-3.0-CH
         XP=CW

         CALL SGS_BTEXT (XP,YP)
         CALL SGS_ATEXT (IDCHAR)
         CALL SGS_OTEXT

         XP=XP+13.0*CW
         CALL SGS_BTEXT (XP,YP)
         CALL SGS_ATEXT ('EPOCH : ')
         CALL SGS_OTEXT

         XP=XP+8.0*CW
         CALL SGS_BTEXT (XP,YP)
         CALL SGS_ATXR  (EPOCH,0,1)
         CALL SGS_OTEXT

         IF (GRID(1:1).EQ.'Y'.OR.GRID(1:1).EQ.'M') THEN

            XP=XP+8.0*CW
            CALL SGS_BTEXT (XP,YP)
            CALL SGS_ATEXT ('EQUINOX : ')
            CALL SGS_OTEXT

            XP=XP+10.0*CW
            CALL SGS_BTEXT (XP,YP)
            CALL SGS_ATXR  (EQUOUT,0,1)
            CALL SGS_OTEXT

         ENDIF

      ELSE

*   Three lines output :-
*
*   Line One

         CALL SGS_BTEXT (XP,YP)
         CALL SGS_ATEXT (IDCHAR)
         CALL SGS_OTEXT

         XP = XP + 13.0 * CW

         CALL SGS_BTEXT (XP,YP)
         CALL SGS_ATEXT (IDFLD)
         CALL SGS_OTEXT

*   Line two

         XP = CW
         YP = YP - 3.0 - CH

         CALL SGS_BTEXT (XP,YP)
         CALL SGS_ATEXT ('CENTRE : ')
         CALL SGS_OTEXT

         XP=XP+9.0*CW
         CALL TCONV (2,A,0,ISIGN,MHRS,MINS,MSECS,SECS, STATUS )
         CALL SGS_BTEXT (XP,YP)
         IF (MHRS.GE.10) THEN
            CALL SGS_ATXI (MHRS,-2)
         ELSE
            CALL SGS_ATEXT ('0')
            CALL SGS_ATXI (MHRS,-1)
         ENDIF
         CALL SGS_OTEXT

         XP=XP+2.0*CW
         CALL SGS_BTEXT (XP,YP)
         IF (MINS.GE.10) THEN
            CALL SGS_ATXI (MINS,-3)
         ELSE
            CALL SGS_ATEXT (' 0')
            CALL SGS_ATXI (MINS,-1)
         ENDIF
         CALL SGS_OTEXT

         XP=XP+3.0*CW
         CALL SGS_BTEXT (XP,YP)
         IF (MSECS.GE.10) THEN
            CALL SGS_ATXI (MSECS,-3)
         ELSE
            CALL SGS_ATEXT (' 0')
            CALL SGS_ATXI (MSECS,-1)
         ENDIF
         CALL SGS_OTEXT

         XP=XP+3.0*CW
         CALL SGS_BTEXT (XP,YP)
         CALL SGS_ATEXT (' , ')
         CALL SGS_OTEXT

         XP=XP+3.0*CW
         CALL TCONV (1,D,0,ISIGN,MDEGS,MINS,MSECS,X, STATUS )
         CALL SGS_BTEXT (XP,YP)
         CALL SGS_ATEXT (ISIGN)
         CALL SGS_OTEXT

         XP=XP+CW
         CALL SGS_BTEXT (XP,YP)
         IF (MDEGS.GE.10) THEN
            CALL SGS_ATXI (MDEGS,-2)
         ELSE
            CALL SGS_ATEXT ('0')
            CALL SGS_ATXI (MDEGS,-1)
         ENDIF
         CALL SGS_OTEXT

         XP=XP+2.0*CW
         CALL SGS_BTEXT (XP,YP)
         DSECS=FLOAT(MSECS)/60.0
         DMINS=FLOAT(MINS)+DSECS
         IF (DMINS.GE.10.0) THEN
            CALL SGS_ATXR (DMINS,-5,1)
         ELSE
            CALL SGS_ATEXT (' 0')
            CALL SGS_ATXR (DMINS,-3,1)
         ENDIF
         CALL SGS_OTEXT

         XP=XP+7.0*CW
         CALL SGS_BTEXT (XP,YP)
         CALL SGS_ATEXT ('(')
         CALL SGS_OTEXT

         XP=XP+CW
         CALL SGS_BTEXT (XP,YP)
         CALL SGS_ATXR  (EQUIN,0,1)
         CALL SGS_OTEXT

         XP=XP+6.0*CW
         CALL SGS_BTEXT (XP,YP)
         CALL SGS_ATEXT (')')
         CALL SGS_OTEXT

*   Line Three

         XP = CW
         YP = YP - 3.0 - CH

         CALL SGS_BTEXT (XP,YP)
         CALL SGS_ATEXT ('EPOCH : ')
         CALL SGS_OTEXT

         XP=XP+8.0*CW
         CALL SGS_BTEXT (XP,YP)
         CALL SGS_ATXR  (EPOCH,0,1)
         CALL SGS_OTEXT

         IF (GRID(1:1).EQ.'Y'.OR.GRID(1:1).EQ.'M') THEN

            XP=XP+8.0*CW
            CALL SGS_BTEXT (XP,YP)
            CALL SGS_ATEXT ('EQUINOX : ')
            CALL SGS_OTEXT

            XP=XP+10.0*CW
            CALL SGS_BTEXT (XP,YP)
            CALL SGS_ATXR  (EQUOUT,0,1)
            CALL SGS_OTEXT

         ENDIF

      ENDIF

      CALL SGS_FLUSH
      CALL SGS_SELZ (IZBASE,ISTAT)
      CALL SGS_RELZ (IZTITLE,ISTAT)

      END
