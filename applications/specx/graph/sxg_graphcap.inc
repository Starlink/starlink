*-----------------------------------------------------------------------
*
*      SXG_GRAPHCAP.INC
*
*-----------------------------------------------------------------------

      INTEGER      MAXDEVS
      PARAMETER   (MAXDEVS=40)

      INTEGER      NDEVS

      STRUCTURE /GCAPS/
        INTEGER   DEV_NO
        CHARACTER PROMPT*16
        CHARACTER DEVNAME*16
        LOGICAL   TERM
        LOGICAL   DUAL
        LOGICAL   COLOUR
        LOGICAL   HARD
        CHARACTER FILE*80
      END STRUCTURE

      RECORD /GCAPS/ DEVICE

      COMMON /SXG_GRAPHCAP/ DEVICE(MAXDEVS), NDEVS
      SAVE   /SXG_GRAPHCAP/

*-----------------------------------------------------------------------
