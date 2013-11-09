;+  XRTMTIME - Extract time series for each source in source list
;
;    Description :
;
;     Procedure to extract time series from a small box around 
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
PROC XRTMTIME SOURCE_LIST ENCPSF EMEAN TBIN
;
;Version id
  PRINT 'XRTMTIME Version 1.7-0'
;
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

;Get time bin size from the user
  IF UNDEFINED(TBIN)
    INPUTR 'Enter width of time bin >' (P_TBIN)
  ELSE
    P_TBIN = TBIN
  ENDIF

;Output the number of time series being produced
  PRINT 'Producing ' (NS) 'time series'

;Loop over each source
  LOOP FOR I = 1 TO NS

;  Create the output filename for this time series
    FILE = '@time' & (I)

;  Read in X_CORR and Y_CORR 
    STRING='@'&(SOURCES)&'.POSIT.IMG_COORDS.X.DATA_ARRAY(' & (I) & ')' 
    HGET (STRING) VALUE ATTR=(X_C) 
    STRING='@'&(SOURCES)&'.POSIT.IMG_COORDS.Y.DATA_ARRAY(' & (I) & ')' 
    HGET (STRING) VALUE ATTR=(Y_C)

;  Off-axis angle in arcmin
    ROFF = SQRT(X_C*X_C+Y_C*Y_C)*60.0

;  Get ENCPSF enclosed energy radius in degrees
    XRAD (ROFF) (P_EMEAN) PFRAC=(P_ENCPSF) NODISP PSFRAD=(R90)

;  Read in RA and DEC of this source from the srclist file
;  produced by PSS. 
    STRING='@'&(SOURCES)&'.POSIT.CEL_COORDS.RA.DATA_ARRAY(' & (I) & ')' 
    HGET (STRING) VALUE ATTR=(RA) 
    STRING='@'&(SOURCES)&'.POSIT.CEL_COORDS.DEC.DATA_ARRAY(' & (I) & ')' 
    HGET (STRING) VALUE ATTR=(DEC)

;  Announce source number
    PRINT 'Source found at ' (RA) (DEC) ' being written to '(FILE) 

;  Produce a time series from a circular source box of radius R90 degrees
;  around this particular RA, DEC position.
    XSORT SRCFILE=(FILE&' ') BACK=NO BIN_AXES=5 SHAPE=C GRP_RA=(RA+0.0)~
      GRP_DEC=(DEC+0.0) RADIUS=(R90) TIMBIN=(P_TBIN) DATASET='B' \

;Next source
  ENDLOOP

;Handle missing source list
  EXCEPTION NOSRCFILE
    PRINT '!! Source file '(SOURCES)' does not exist'
    PRINT '!  ...from XRTMTIME'
  ENDEXCEPTION

ENDPROC
