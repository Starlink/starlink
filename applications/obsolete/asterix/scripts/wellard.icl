PROC WELLARD AFILE
;
; Check image processing active
    IMG = " "
    ICURRENT SUPPRESS NAME=(IMG)
    IF ( LEN(IMG) = 0 )
      SIGNAL IP_NOT_STARTED
    ENDIF
;
; Get name of ARD file
    SET NOCHECKPARS
    IF UNDEFINED(AFILE)
       INPUT 'Enter name of output ARD file > ' (AFILE)
    ENDIF
;
; Create this file
    CREATE AFILE2 (AFILE)
    CLOSE AFILE2
;
; Set the global filename
    SETGLOBAL ARDFILE (AFILE)
;
; Create the exclude global and initialise it
    CREATEGLOBAL EXCLUDE _LOGICAL
;
    SETGLOBAL EXCLUDE FALSE

; Load the menufile
    FNAME = GETENV("AST_DAT_MENU")&"/wellard.mnu"
    LOADMENU ((FNAME)&" ")
;
; Execute the menu
    RUNMENU 1
;
; Reset the whole image
    IWHOLE
;
; Tell user what's happened
    PRINT 'Created ARD file' (AFILE)
;
    EXCEPTION IP_NOT_STARTED
    ENDEXCEPTION

ENDPROC

PROC ARDCIRC 
;
; initialise variables
     AFILE=0.0
     X=0.0
     Y=0.0
     X1=0.0
     X2=0.0
     EXCL=0.0
     RAD=0.0
;
        GETGLOBAL ARDFILE (AFILE)
        AFILE = "@" & AFILE
;
        ICIRCLE 
;
;  Retrieve the current position from the system
        ICURRENT X=(X) Y=(Y) X1=(X1) X2=(X2) SUPPRESS=YES
;
        RAD = ABS(X2-X1) / 2.0
;
;  Find if this is an exclude region
        GETGLOBAL EXCLUDE (EXCL)
;
        REGIONS NEW=NO ARDFILE=(AFILE) EXCLUDE=(EXCL) SHAPE='CIRCLE'~
         XCENT=(X) YCENT=(Y) RADIUS=(RAD) MORE=NO
;
;        IWHOLE
ENDPROC

PROC ARDBOX 
;
; initialise variables
      AFILE=0.0
      X=0.0
      Y=0.0
      X1=0.0
      X2=0.0
      Y1=0.0
      Y=0.0
      EXCL=0.0
      XWID=0.0
      YWID=0.0
;
        GETGLOBAL ARDFILE (AFILE)
        AFILE = "@" & AFILE
;
        IBOX
;
;  Retrieve the current position from the system
        ICURRENT X=(X) Y=(Y) X1=(X1) X2=(X2) Y1=(Y1) Y2=(Y2) SUPPRESS=YES
;
        XWID = ABS(X2-X1) 
        YWID = ABS(Y2-Y1) 
;
;  Find if this is an exclude region
        GETGLOBAL EXCLUDE (EXCL)
;
        REGIONS NEW=NO ARDFILE=(AFILE) EXCLUDE=(EXCL) SHAPE='BOX'~
 XCENT=(X) YCENT=(Y) XWIDTH=(XWID) YWIDTH=(YWID) MORE=NO
;
;        IWHOLE
ENDPROC

PROC ARDANN 
;
; initialise varibales
     AFILE=0.0
     XC=0.0
     YC=0.0
     X1=0.0
     X2=0.0
     XP=0.0
     YP=0.0
     ORAD=0.0
     EXCL=0.0
     IRAD=0.0
;
        GETGLOBAL ARDFILE (AFILE)
        AFILE = "@" & AFILE
;
;  Define inner circle
        PRINT '** INNER CIRCLE **'
        ICIRCLE
;
;  Retrieve the current position from the system
        ICURRENT X=(XC) Y=(YC) X1=(X1) X2=(X2) SUPPRESS=YES
;
        IRAD = ABS(X2-X1) / 2.0
;
;  Define outer circle
        PRINT '** CHOOSE OUTER RADIUS **'
        IPOSIT
;
;  Retrieve the current position from the system
        ICURRENT X=(XP) Y=(YP) SUPPRESS=YES
;
        ORAD = SQRT((XP-XC)**2 + (YP-YC)**2) 
;
;  Draw on the second circle
        IMODE KEY
        ICIRCLE XCENT=(XC) YCENT=(YC) RAD=(ORAD)
        IMODE CURS
;
;  Find if this is an exclude region
        GETGLOBAL EXCLUDE (EXCL)
;
        REGIONS NEW=NO ARDFILE=(AFILE) EXCLUDE=(EXCL) SHAPE='ANNULUS'~
 XCENT=(XC) YCENT=(YC) INNRAD=(IRAD) OUTRAD=(ORAD) MORE=NO
;
;        IWHOLE
ENDPROC
;
PROC ARDPSS
;
; initialise variables
     AFILE=0.0
     SRCFILE=0.0
     NSOURCE=0.0
     ORAD=0.0
     EL=0.0
     AZ=0.0
     OFFAX=0.0
     STR=0.0
; Get the name of the ARD file to write to
     GETGLOBAL ARDFILE (AFILE)
     AFILE = "@" & AFILE
;
     INPUT 'Enter name of source list file >' (SRCFILE)
;
; Find number of sources
     SSGET (SRCFILE) NSRC ATTR=(NSOURCE)
;
     PRINT 'Found ' (NSOURCE) 'sources'
;
; Set keyboard mode
     IMODE KEY
;
; Loop over all sources
     LOOP FOR I=1 TO NSOURCE 
;
;  Read data points from file AZ and EL are azimuth and elevation 
;  Create string
       SSGET (SRCFILE) X_CORR ISRC=(I) ATTR=(AZ)
       SSGET (SRCFILE) Y_CORR ISRC=(I) ATTR=(EL)
;
;  Calculate the off-axis angle in arcmins
       OFFAX = SQRT(AZ**2 + EL**2) * 60.0
;
;  Calculate the 95% radius at 0.2 keV (returned in degrees).
       XRAD OFFAX=(OFFAX) PFRAC=0.95 ENERGY=0.2 DISP=NO PSFRAD=(ORAD)
;
;  Draw the circle
       ICIRCLE XCENT=(AZ) YCENT=(EL) RAD=(ORAD)
;
       REGIONS NEW=NO ARDFILE=(AFILE) EXCLUDE=NO SHAPE='CIRCLE'~
 XCENT=(AZ) YCENT=(EL) RADIUS=(ORAD) MORE=NO
;
     ENDLOOP
;
; Reset the cursor mode and the whole image
     IMODE CURS
;     IWHOLE
;
ENDPROC
;
PROC SPOKES
;
; initialise variables
    AFILE=0.0
;
    GETGLOBAL ARDFILE (AFILE)
;
    XSPOKES OUTSIDE=NO NEW=NO ARDFILE=(AFILE) 
;
ENDPROC    

PROC SETBAD
;
; Sets a region in an image permanently bad
   WELLARD DEL.ARD
;
   ARDQUAL ARDFILE=DEL.ARD OVER=YES
;
; delete temporary file
;VAX: $DELETE DEL.ARD;0
;UNIX: $rm del.ard
;
ENDPROC

PROC XPSF90 
;
; initialise variables
    X=0.0
    Y=0.0
    OFFAX=0.0
    RAD=0.0
;
    IPOSIT
;
    ICURRENT X=(X) Y=(Y) SUPPRESS=YES

    OFFAX = SQRT(X*X + Y*Y) * 60.
    XRAD90 OFFAX=(OFFAX) PSFRAD=(RAD)
;
    IMODE KEY
;
    ICIRCLE XCENT=(X) YCENT=(Y) RAD=(RAD)
;
ENDPROC
PROC KEEPANN IRAD ORAD AFILE
;
; Get name of ARD file
    IF UNDEFINED(IRAD)
       INPUT 'Enter inner radius (degrees) >' (IRAD)
    ENDIF
;
    IF UNDEFINED(ORAD)
       INPUT 'Enter outer radius (degrees) >' (ORAD)
    ENDIF
;
    IF UNDEFINED(AFILE)
       INPUT 'Enter name of output ARD file >' (AFILE)
    ENDIF
;
; Create this file
    CREATE LUN (AFILE)
;
; Write in the exclude annulus structure
    WRITE LUN  COMPOSITE EDGE
;
    WRITE LUN  BOX 0.0 0.0 2.134 2.134
    WRITE LUN    .AND. .NOT. CIRCLE 0.0 0.0 (ORAD)
;
    WRITE LUN  END COMPOSITE
;
; Write in the inner circle
    WRITE LUN  CIRCLE  0.0  0.0  (IRAD)
;
    CLOSE LUN
;
ENDPROC
