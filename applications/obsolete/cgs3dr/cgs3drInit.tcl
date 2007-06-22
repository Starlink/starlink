proc cgs3drInit {} {
  global env
  global FigTask
  global Red3Task
  global Cgs3drTask
  global TspTask
  global Cgs3drWidgets
  global Red3Widgets

# Initialise some global variables
  set Cgs3drWidgets(TASK_NAME)      $Cgs3drTask
  set Cgs3drWidgets(TASK_ACTION)    init
  set Cgs3drWidgets(TASK_PARAMS)    ""
  set Cgs3drWidgets(PAR_PLOTTING)   1
  set Cgs3drWidgets(PAR_VERBOSE)    1
  set Cgs3drWidgets(PAR_DIVBYSKY)   1
  set Cgs3drWidgets(PAR_DIVBYSTD)   0
  set Cgs3drWidgets(PAR_CYCBYCYC)   0
  set Cgs3drWidgets(PAR_CYCBEG)     1
  set Cgs3drWidgets(PAR_CYCEND)     0
  set Cgs3drWidgets(PAR_NSIGMA)     3.0
  set Cgs3drWidgets(PAR_STANDARD)   " "
  set Cgs3drWidgets(PAR_ICHANBEG)   1
  set Cgs3drWidgets(PAR_ICHANEND)   32
  set Cgs3drWidgets(PAR_VERBOSE_PH) 0
  set Cgs3drWidgets(PAR_GDEVICE)    "xwindows"

  set Red3Widgets(INPUT)            " "
  set Red3Widgets(OUTPUT)           " "
  set Red3Widgets(STARTSCAN)        1
  set Red3Widgets(ENDSCAN)          0
  set Red3Widgets(NSIGMA)           3.0
  set Red3Widgets(BAD_CYCLE)        1
  set Red3Widgets(FACTOR)           1
  set Red3Widgets(CONSTANT)         0
  set Red3Widgets(DETECTOR)         1
  set Red3Widgets(IMAGE)            " "
  set Red3Widgets(YSTART)           1
  set Red3Widgets(YEND)             1
  set Red3Widgets(IST)              1
  set Red3Widgets(IEN)              0
  set Red3Widgets(SPECTRUM)         " "
  set Red3Widgets(SPECT)            " "
  set Red3Widgets(FILE)             " "
  set Red3Widgets(ERRORS)           1
  set Red3Widgets(SPECTRUM1)        " "
  set Red3Widgets(TEMPLATE)         " "
  set Red3Widgets(BB_TEMP)          5000.0
  set Red3Widgets(REFWAVE)          10.0
  set Red3Widgets(REFFLUX)          0.0
  set Red3Widgets(VERBOSE_PH)       0


# Do the obey sequence
  cgs3drClear
  cgs3drInform "Initialising ..."
  set params "date=$env(TODAY2) datadir=$env(DATADIR) rodir=$env(RODIR) reduction_task=$Red3Task figaro_task=$FigTask"
  set params "${params} tsp_task=$TspTask plotting=T verbose=T divbysky=T divbystd=F cycbycyc=F cycbeg=1 cycend=0 nsigma=3.0"
  set params "${params} standard=' ' ichanbeg=1 ichanend=32 verbose_ph=F gdevice='xwindows'"
  $Cgs3drTask obey init "${params}" -inform "cgs3drInform %V" -endmsg {set init_done 1}
  tkwait variable init_done

# Now plot a spectrum
  set params "spectrum='\$RED3_DIR/cgs3' whole=T autoscale=T"
  set params "${params} label='Portable-CGS3DR VPKG_VERS DATADIR=$env(DATADIR) DATE=$env(TODAY2)' hardcopy=F"
  set params "${params} soft='xwindows' colour='red'"
  $FigTask obey splot "${params}" -inform "cgs3drInform %V" -endmsg {set done 1}
  tkwait variable done
  set params "spectrum='\$RED3_DIR/cgs3' whole=T autoscale=T"
  set params "${params} label='Portable-CGS3DR VPKG_VERS DATADIR=$env(DATADIR) DATE=$env(TODAY2)' hardcopy=F"
  set params "${params} soft='xwindows' colour='red'"
  $FigTask obey esplot "${params}" -inform "cgs3drInform %V"

# Issue a message
  cgs3drClear
  cgs3drInform "Initialised for :"
  cgs3drInform "  Date    : $env(TODAY2)"
  cgs3drInform "  Datadir : $env(DATADIR)"
  cgs3drInform "  Rodir   : $env(RODIR)"
}
