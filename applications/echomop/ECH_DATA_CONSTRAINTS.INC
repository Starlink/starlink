C  Include file to define the limitations place upon input data images
C  by the ECH_ package

C!!!! Any change to max_allowed_orders MUST be made to max_curs_positions
C!!!! in ECH_ENVIRONMENT.INC as well.

      INTEGER MAX_ALLOWED_ORDERS
      PARAMETER ( MAX_ALLOWED_ORDERS = 200 )

      INTEGER MAX_ALLOWED_RF_FEAT
      PARAMETER ( MAX_ALLOWED_RF_FEAT = 500 )

      INTEGER MAX_ALLOWED_ORDNUM
      PARAMETER ( MAX_ALLOWED_ORDNUM = 200 )

      INTEGER MAX_SLICE_PIXELS
      PARAMETER ( MAX_SLICE_PIXELS = 101 )

      REAL V0_ALPHA
      PARAMETER ( V0_ALPHA = 3.0 )

      REAL V0_BETA
      PARAMETER ( V0_BETA = 2.0 )

!  Calculated using a maximum value of x=TUNE_MXSKYPIX=41 this
!  parameter dimensions arrays which will not be used if
!  this value is exceeded. The size is (x*(5*x+1)) and is used
!  for NAG workspace.
      INTEGER MAX_COVAR_SIZE
      PARAMETER ( MAX_COVAR_SIZE = 8446 )
