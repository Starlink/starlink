*+CIN_ALIGN_LOW Structure to hold alignment data, Low level
         STRUCTURE /ALIGN_REC/
* Original M.J. Ricketts
* Modified for CALLIB M Denby
*-
            DOUBLE PRECISION MJD			! Date
            REAL EULER(3)
            REAL ERROR(3)
            REAL DELTA_EULER
            CHARACTER*20 CAL_TYPE
         END STRUCTURE
