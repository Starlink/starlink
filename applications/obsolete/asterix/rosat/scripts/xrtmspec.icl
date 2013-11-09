;+  XRTMSPEC - Extract spectrum for each source in source list
;
;    Description :
;
;     Procedure to extract spectra from a small box around 
;     sources found by PSS in an XRT field. 
;
;    History :
;
;     ?? ??? ?? : V1.5-0  Original (RDS)
;     22 Feb 94 : V1.7-0  Uses 95% enclosed energy radius for event
;                         extraction, and more sophisticated handling of
;                         of parameters (DJA)
;
;-
PROC XRTMSPEC SOURCE_LIST ENCPSF EMEAN

;Version id
  PRINT 'XRTMSPEC Version 1.7-0'

;Initialise
  NS=0
  RA=0.0
  DEC=0.0
  X_C=0.0
  Y_C=0.0
  R90=0.0

;Choose input source file
  IF UNDEFINED(SOURCE_LIST)
    IF FILE_EXISTS('srclist.sdf')
      SOURCES = 'srclist'
      PRINT 'Input source list defaulting to'(SOURCES)
    ELSE
      SOURCES = ' '
      INPUT 'Enter name of source results file > ' (SOURCES)
    ENDIF
  ELSE
    SOURCES = SOURCE_LIST
  ENDIF
  IF NOT FILE_EXISTS((SOURCES)&'.sdf')
    SIGNAL NOSRCFILE
  ENDIF

;Enclosed energy defined?
  IF UNDEFINED(ENCPSF)
    P_ENCPSF = 0.95
    PRINT 'Enclosed energy defaulting to '((P_ENCPSF)*100.0)'%'
  ELSE
    P_ENCPSF = ENCPSF
  ENDIF

;Enclosed energy defined?
  IF UNDEFINED(EMEAN)
    P_EMEAN = 0.5
    PRINT 'Energy for psf evaluation defaulting to '(P_EMEAN)'keV'
  ELSE
    P_EMEAN = EMEAN
  ENDIF

;Read in the no of sources found by PSS
  STRING='@'&(SOURCES)&'.POSIT.NSRC'
  HGET (STRING) VALUE ATTR=(NS) 

;Output the number of spectra being produced
  PRINT 'Producing ' (NS) 'spectra'

;Loop over each source
  LOOP FOR I = 1 TO NS
;
;  Create the output filename for this spectrum
    FILE = '@spec' & (I)

;  Read in X_CORR and Y_CORR 
    STRING='@'&(SOURCES)&'.POSIT.IMG_COORDS.X.DATA_ARRAY(' & (I) & ')' 
    HGET (STRING) VALUE ATTR=(X_C) 
    STRING='@'&(SOURCES)&'.POSIT.IMG_COORDS.Y.DATA_ARRAY(' & (I) & ')' 
    HGET (STRING) VALUE ATTR=(Y_C)

;  Off-axis angle in arcmin
    ROFF = SQRT(X_C*X_C+Y_C*Y_C)*60.0

;  Get P_ENCPSF enclosed energy radius in degrees
    XRAD (ROFF) (P_EMEAN) PFRAC=(P_ENCPSF) NODISP PSFRAD=(R90)

;  Read in RA and DEC of this source from the srclist file produced by PSS. 
    STRING='@'&(SOURCES)&'.POSIT.CEL_COORDS.RA.DATA_ARRAY(' & (I) & ')' 
    HGET (STRING) VALUE ATTR=(RA) 
    STRING='@'&(SOURCES)&'.POSIT.CEL_COORDS.DEC.DATA_ARRAY(' & (I) & ')' 
    HGET (STRING) VALUE ATTR=(DEC)

;  Announce this source
    PRINT 'Source found at ' (RA) (DEC) ' being written to '(FILE) 

;  Produce a spectrum from a circular source box of radius R90 degrees
;  around this particular RA, DEC position.
    XSORT SRCFILE=(FILE&' ') BACK=NO BIN_AXES=7 SHAPE=C GRP_RA=(RA+0.0)~
      GRP_DEC=(DEC+0.0) RADIUS=(R90) ENBIN=1 DATASET='B' \

; The following are possible extensions to the procedure

;  Subtract a standard background file, BACKY.SDF . This file must 
;  have been created already and have the same number of PHA channels
;  as the spectrum created above, i.e. 256.
;   FILES = FILE & '_S'
;   XRTSUB (FILE) BACKY (FILES)
;
;  Correct the file
;   FILEC = FILES & '_C'
;   XRTCORR INP=(FILES) OUT=(FILEC) \
;
;  Write spectral response into datafile
;   XRTRESP (FILES) ERASE=Y 
;
  ENDLOOP
;
  EXCEPTION NOSRCFILE
    PRINT '!! Source file '(SOURCES)' does not exist'
    PRINT '!  ...from XRTMSPEC'
  ENDEXCEPTION

ENDPROC
