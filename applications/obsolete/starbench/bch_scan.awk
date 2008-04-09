#+
#  Name:
#    bch_scan.awk
#
#  Purpose:
#    Scan a benchmark log file to get the statistics and system load.
#
#  Language:
#    awk
#
#  Invocation:
#    awk -f bch_scan.awk <filename>
#
#  Authors:
#    Tim Gledhill (tmg), University of Hertfordshire
#
#  History:
#    15-SEP-1994 (tmg):
#       Original version.
#    31-OCT-1994 (tmg):
#       Host field number changes after including benchmark version number.
#    23-NOV-1994 (tmg):
#       Add C benchmark.
#    06-FEB-1995 (tmg):
#       Remove LaTeX b/m
#    22-FEB-1995 (tmg):
#       Remove disk b/m.
#    22-MAY-1996 (tmg):
#       Process the elapsed time. v0.9-1
#    18-JUL-1996 (tmg):
#       V1.0 incorporating IRAF benchmarks.
#-
#
# Initialize all of the output strings to non-blank fields since blanks are
# used as field delimiters.
#
BEGIN {
        etim_fft = "-"
        etim_cfp = "-"  
        etim_kap = "-"
        etim_pis = "-"
        etim_spe = "-"
        etim_ccd = "-"
        etim_cdr = "-"
        etim_dao = "-"
        etim_ima = "-"
}

#
# Define the position of the various elements in the benchmark log files.
#
  {
#
# Get the host name from the file.
#
      if ( $1 == "Hostname" ) 
         host = $3
#
# Get the system load from the file.
#
      if ( $1 == "Current" )
        load = $4
#
# Get the ucpu, kcpu and elapsed times and form a total cpu time for each 
# benchmark.
#
      if ( $1 == "FFT" )
      {
        ucpu_fft = $2
        kcpu_fft = $3
        tcpu_fft = ucpu_fft + kcpu_fft
        etim_fft = $4
      }
      if ( $1 == "SLA" )
      {
        ucpu_cfp = $2
        kcpu_cfp = $3
        tcpu_cfp = ucpu_cfp + kcpu_cfp
        etim_cfp = $4
      }
      if ( $1 == "KAPPA" )
      {
        ucpu_kap = $2
        kcpu_kap = $3
        tcpu_kap = ucpu_kap + kcpu_kap
        etim_kap = $4
      }
      if ( $1 == "PISA" )
      {
        ucpu_pis = $2
        kcpu_pis = $3
        tcpu_pis = ucpu_pis + kcpu_pis
        etim_pis = $4
      }
      if ( $1 == "SPECDRE" )
      {
        ucpu_spe = $2
        kcpu_spe = $3
        tcpu_spe = ucpu_spe + kcpu_spe
        etim_spe = $4
      }
      if ( $1 == "CCDPACK" )
      {
        ucpu_ccd = $2
        kcpu_ccd = $3
        tcpu_ccd = ucpu_ccd + kcpu_ccd
        etim_ccd = $4
      }
      if ( $1 == "CCDRED" )
      {
        ucpu_cdr = $2
        kcpu_cdr = $3
        tcpu_cdr = ucpu_cdr + kcpu_cdr
        etim_cdr = $4
      }
      if ( $1 == "DAOPHOT" )
      {
        ucpu_dao = $2
        kcpu_dao = $3
        tcpu_dao = ucpu_dao + kcpu_dao
        etim_dao = $4
      }
      if ( $1 == "IMAGES" )
      {
        ucpu_ima = $2
        kcpu_ima = $3
        tcpu_ima = ucpu_ima + kcpu_ima
        etim_ima = $4
      }
  }
#
# Print out the results at the end.
#
END {  
      fmt1 = "%.8s %8.2f%8.2f%8.2f %.8s %8.2f%8.2f" \
             "%8.2f %.8s %8.2f%8.2f%8.2f %.8s " \
             "%8.2f%8.2f%8.2f %0.8s %8.2f%8.2f" \
             "%8.2f %.8s %8.2f%8.2f%8.2f %.8s "
      printf( fmt1,  \
      host, ucpu_fft, kcpu_fft, tcpu_fft, etim_fft, ucpu_cfp, kcpu_cfp, \
            tcpu_cfp, etim_cfp, ucpu_kap, kcpu_kap, tcpu_kap, etim_kap, \
            ucpu_pis, kcpu_pis, tcpu_pis, etim_pis, ucpu_spe, kcpu_spe, \
            tcpu_spe, etim_spe, ucpu_ccd, kcpu_ccd, tcpu_ccd, etim_ccd )

      fmt2 = "%8.2f%8.2f%8.2f %.8s %8.2f%8.2f%8.2f %.8s %8.2f%8.2f" \
             "%8.2f %.8s %8.2f"
      printf( fmt2, \
             ucpu_cdr, kcpu_cdr, tcpu_cdr, etim_cdr, ucpu_dao, \
             kcpu_dao, tcpu_dao, etim_dao, ucpu_ima, kcpu_ima, \
             tcpu_ima, etim_ima, \
      load )
}
