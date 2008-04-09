#+
#  Name:
#    bch_result.awk
#
#  Purpose:
#    Scan the node file and compute the mean and standard deviation for 
#    each column. Print the results.
#
#  Language:
#    awk
#
#  Invocation:
#    awk -f bch_result.awk <filename>
#
#  Authors:
#    Tim Gledhill (tmg), University of Hertfordshire
#
#  History:
#    15-SEP-1994 (tmg):
#       Original version.
#    23-NOV-1994 (tmg):
#       Include C benchmark.
#    06-FEB-1995 (tmg)
#       Remove the LaTeX b/m.
#    22-FEB-1995 (tmg)
#       Remove disk b/m.
#    22-MAY-1996 (tmg)
#       Process the elapsed time statistics. v0.9
#    18-JUL-1996 (tmg)
#       v1.0 incorporating IRAF benchmarks.
#-
BEGIN {
      NHEAD = 13       # number of lines in the header
}
#
  {
#
# Skip the header.
#
      if ( NR > NHEAD ) {
#
# Read in the columns corresponding to the tcpu figure for each benchmark. 
#
        col3 = $3                     # fft tcpu
        col7 = $7                     # cfp tcpu
        col11 = $11                   # kappa tcpu
        col15 = $15                   # pisa tcpu
        col19 = $19                   # specdre tcpu
        col23 = $23                   # ccdpack tcpu
        col27 = $27                   # ccdred tcpu
        col31 = $31                   # daophot tcpu
        col35 = $35                   # images tcpu
#
# If the tcpu figure is not 0.0 then this record contains information on
# the corresponding benchmark so process the other columns - otherwise this 
# benchmark has not been run. Keep a count on valid records for each
# benchmark. Form sums to calculate the mean and s.d.
#
        if ( col3 > 0.0 ) {           # fft
          split($4,a,":")
          col4 = 60.0 * a[1] + a[2]    
          sum1 = sum1 + $1                       
          sum2 = sum2 + $2
          sum3 = sum3 + col3
          ssum3 = ssum3 + ( col3 * col3 )
          sum4 = sum4 + col4
          ssum4 = ssum4 + ( col4 * col4 )
          nfft++
        }
        if ( col7 > 0.0 ) {           #cfp
          split($8,a,":")
          col8 = 60.0 * a[1] + a[2]     
          sum5 = sum5 + $5                  
          sum6 = sum6 + $6
          sum7 = sum7 + col7
          ssum7 = ssum7 + ( col7 * col7 )
          sum8 = sum8 + col8
          ssum8 = ssum8 + ( col8 * col8 )
          ncfp++
        }
        if ( col11 > 0.0 ) {          #kappa
          split($12,a,":")
          col12 = 60.0 * a[1] + a[2]    
          sum9 = sum9 + $9                     
          sum10 = sum10 + $10
          sum11 = sum11 + col11
          ssum11 = ssum11 + ( col11 * col11 )
          sum12 = sum12 + col12
          ssum12 = ssum12 + ( col12 * col12 )
          nkap++
        }
        if ( col15 > 0.0 ) {           #pisa
          split($16,a,":")
          col16 = 60.0 * a[1] + a[2]   
          sum13 = sum13 + $13    
          sum14 = sum14 + $14
          sum15 = sum15 + col15
          ssum15 = ssum15 + ( col15 * col15 )
          sum16 = sum16 + col16
          ssum16 = ssum16 + ( col16 * col16 )
          npis++
        }
        if ( col19 > 0.0 ) {           #specdre
          split($20,a,":")
          col20 = 60.0 * a[1] + a[2]  
          sum17 = sum17 + $17                
          sum18 = sum18 + $18
          sum19 = sum19 + col19
          ssum19 = ssum19 + ( col19 * col19 )
          sum20 = sum20 + col20
          ssum20 = ssum20 + ( col20 * col20 )
          nspe++
        }
        if ( col23 > 0.0 ) {            #ccdpack
          split($24,a,":")
          col24 = 60.0 * a[1] + a[2]    
          sum21 = sum21 + $21   
          sum22 = sum22 + $22
          sum23 = sum23 + col23
          ssum23 = ssum23 + ( col23 * col23 )
          sum24 = sum24 + col24
          ssum24 = ssum24 + ( col24 * col24 )
          nccd++
        }
        if ( col27 > 0.0 ) {            #ccdred
          split($28,a,":")
          col28 = 60.0 * a[1] + a[2]   
          sum25 = sum25 + $25  
          sum26 = sum26 + $26
          sum27 = sum27 + col27
          ssum27 = ssum27 + ( col27 * col27 )
          sum28 = sum28 + col28
          ssum28 = ssum28 + ( col28 * col28 )
          ncdr++
        }
        if ( col31 > 0.0 ) {            #daophot
          split($32,a,":")
          col32 = 60.0 * a[1] + a[2]  
          sum29 = sum29 + $29             
          sum30 = sum30 + $30
          sum31 = sum31 + col31
          ssum31 = ssum31 + ( col31 * col31 )
          sum32 = sum32 + col32
          ssum32 = ssum32 + ( col32 * col32 )
          ndao++
        }
        if ( col35 > 0.0 ) {            #images
          split($36,a,":")
          col36 = 60.0 * a[1] + a[2]    
          sum33 = sum33 + $33                  
          sum34 = sum34 + $34
          sum35 = sum35 + col35
          ssum35 = ssum35 + ( col35 * col35 )
          sum36 = sum36 + col36
          ssum36 = ssum36 + ( col36 * col36 )
          nima++
        }
        sum37 = sum37 + $37            #load
        ssum37 = ssum37 + ( $37 * $37 ) 
      }
  }

END {
#
# Consistency checks. Check that there are some FFT and CFP benchmarks. These
# are run in both ussc and iraf modes and so should be there unless something
# has gone drastically wrong. Also check that the ussc benchmarks have the same 
# number of valid entries. If this fails then one of the benchmarks failed also.
# Do the same for iraf.
#
      if ( nfft == 0 || ncfp == 0 ) {
        print "FFT/CFP consistency check failed"
        exit 1
      }
      if ( nkap != npis || nkap != nspe || nkap != nccd ) {
        print "USSC consistency check failed"
        exit 1
      }
      if ( ncdr != ndao || ncdr != nima ) {
        print "IRAF consistency check failed"
        exit 1
      }
#
# Calculate the mean and S.D. of each column.
#
      m1 = sum1 / nfft                                    #fft
      m2 = sum2 / nfft
      m3 = sum3 / nfft
      sd3 = ( ssum3 / nfft ) - ( m3 * m3 )
      sd3 = sqrt( sd3 ) 
      m4 = sum4 / nfft
      sd4 = ( ssum4 / nfft ) - ( m4 * m4 )
      sd4 = sqrt( sd4 )
      m5 = sum5 / ncfp                                    #sla
      m6 = sum6 / ncfp
      m7 = sum7 / ncfp
      sd7 = ( ssum7 / ncfp ) - ( m7 * m7 )
      sd7 = sqrt( sd7 ) 
      m8 = sum8 / ncfp
      sd8 = ( ssum8 / ncfp ) - ( m8 * m8 )
      sd8 = sqrt( sd8 )
      if ( nkap > 0 ) {
        m9 = sum9 / nkap                                  #kappa
        m10 = sum10 / nkap
        m11 = sum11 / nkap
        sd11 = ( ssum11 / nkap ) - ( m11 * m11 )
        sd11 = sqrt( sd11 ) 
        m12 = sum12 / nkap
        sd12 = ( ssum12 / nkap ) - ( m12 * m12 )
        sd12 = sqrt( sd12 ) 
        m13 = sum13 / nkap                                #pisa
        m14 = sum14 / nkap
        m15 = sum15 / nkap
        sd15 = ( ssum15 / nkap ) - ( m15 * m15 )
        sd15 = sqrt( sd15 ) 
        m16 = sum16 / nkap
        sd16 = ( ssum16 / nkap ) - ( m16 * m16 )
        sd16 = sqrt( sd16 )
        m17 = sum17 / nkap                                #specdre
        m18 = sum18 / nkap
        m19 = sum19 / nkap
        sd19 = ( ssum19 / nkap ) - ( m19 * m19 )
        sd19 = sqrt( sd19 ) 
        m20 = sum20 / nkap
        sd20 = ( ssum20 / nkap ) - ( m20 * m20 )
        sd20 = sqrt( sd20 )
        m21 = sum21 / nkap                                #ccdpack
        m22 = sum22 / nkap
        m23 = sum23 / nkap
        sd23 = ( ssum23 / nkap ) - ( m23 * m23 )
        sd23 = sqrt( sd23 ) 
        m24 = sum24 / nkap
        sd24 = ( ssum24 / nkap ) - ( m24 * m24 )
        sd24 = sqrt( sd24 ) 
      }
      if ( ncdr > 0 ) {
        m25 = sum25 / ncdr                                #ccdred
        m26 = sum26 / ncdr
        m27 = sum27 / ncdr
        sd27 = ( ssum27 / ncdr ) - ( m27 * m27 )
        sd27 = sqrt( sd27 ) 
        m28 = sum28 / ncdr
        sd28 = ( ssum28 / ncdr ) - ( m28 * m28 )
        sd28 = sqrt( sd28 )
        m29 = sum29 / ncdr                                #daophot
        m30 = sum30 / ncdr
        m31 = sum31 / ncdr
        sd31 = ( ssum31 / ncdr ) - ( m31 * m31 )
        sd31 = sqrt( sd31 ) 
        m32 = sum32 / ncdr
        sd32 = ( ssum32 / ncdr ) - ( m32 * m32 )
        sd32 = sqrt( sd32 )
        m33 = sum33 / ncdr                               #images
        m34 = sum34 / ncdr
        m35 = sum35 / ncdr
        sd35 = ( ssum35 / ncdr ) - ( m35 * m35 )
        sd35 = sqrt( sd35 ) 
        m36 = sum36 / ncdr
        sd36 = ( ssum36 / ncdr ) - ( m36 * m36 )
        sd36 = sqrt( sd36 )
      } 
      m37 = sum37 / nfft                                 #load
      sd37 = ( ssum37 / nfft ) - ( m37 * m37 )
      sd37 = sqrt( sd37 )
#
# Print out the results at the end. Also print out the number of measurements
# the ussc and iraf figures are based on.
#
      fmt = "%8.3f%8.3f%8.3f%8.3f%8.3f%8.3f" \
            "%8.3f%8.3f%8.3f%8.3f%8.3f%8.3f" \
            "%8.3f%8.3f%8.3f%8.3f%8.3f%8.3f" \
            "%8.3f%8.3f%8.3f%8.3f%8.3f%8.3f" \
            "%8.3f%8.3f%8.3f%8.3f%8.3f%8.3f" \
            "%8.3f%8.3f%8.3f%8.3f%8.3f%8.3f" \
            "%8.3f%8.3f%8.3f%8.3f%8.3f%8.3f" \
            "%8.3f%8.3f%8.3f%8.3f%8.3f%8.3f" \
            "%8.3f%8.3f%8.3f%8.3f%8.3f%8.3f" \
            "%8.3f%8.3f %d %d\n"

      printf( fmt, m1, m2, m3, sd3, m4, sd4, \
                   m5, m6, m7, sd7, m8, sd8, \
                   m9, m10, m11, sd11, m12, sd12, \
                   m13, m14, m15, sd15, m16, sd16, \
                   m17, m18, m19, sd19, m20, sd20, \
                   m21, m22, m23, sd23, m24, sd24, \
                   m25, m26, m27, sd27, m28, sd28, \
                   m29, m30, m31, sd31, m32, sd32, \
                   m33, m34, m35, sd35, m36, sd36, \
                   m37, sd37, nkap, ncdr )
}
