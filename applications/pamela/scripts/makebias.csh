#!/bin/csh
#
# !!begin
# !!title makebias
# !!author T.R. Marsh
# !!created 10 January  2005
# !!revised 24 July 2007
# !!root makebias
# !!index makebias.csh
# !!descr combines bias frames together
# !!head1 Script which combines bias frames together.
#
# When combining bias frames one needs to reject cosmic rays.
# A good way to do this is through median averaging, but the
# end result is digitised. !!emph{makebias} avoids this by
# median combining small groups and then averaging the results.
# The small group need to be large enough to ensure good removal
# of cosmic rays. Another problem with combining bias frames is 
# that they may suffer overall offsets such that one frame hardly
# contributes to the median (e.g. if it is 20 counts higher than the others
# but readout noise is only 3 counts). This routine accounts for this using
# the program 'picstat' to compute the mean level over a region 
# which it subtracts prior to combining the frames, but adds back to
# the final result before averaging. The result is a mean bias with about the
# the right level and little digitisation noise. NB the routine takes care
# to combine the groups with the correct weight. This normally means a slightly
# larger weight for the final group as it will usually have a few more frames than the
# others.
#
#
# !!head2 Invocation
#
# makebias list nmedian bias_region output
# 
# !!head2 Arguments
#
# !!table
# !!arg{list}{List of files to be combined.}
# !!arg{nmedian}{The size of the small groups to be medianed. Does not have to divide
# into the total number of files.}
# !!arg{bias_region}{Region file for picstat to compute mean level of each frame which
# will be subtracted before they are medianed together. The mean value of the last frame
# of each frame is added onto the final combined frame of the group}
# !!arg{output}{name of output bias frame}
# !!table
#
# !!end
#
# Written for ITP run December 2004 / January 2005 by TRM
#
#

if($#argv != 4) then
  echo "usage: list nmedian bias_region output"
  exit
endif

set list        = $1
set nmedian     = $2
set bias_region = $3
set output      = $4

# check for existence of files

if(! ( -e $list && -e $bias_region ) ) then
  echo "One or both of $list and $bias_region do not exist"
  exit
endif
 
if ( ! $?ADAM_USER ) then
  setenv ADAM_USER ~/adam
endif

# compute number of files

set nfile = `cat $list | egrep -v '^[ \t]*$' | wc -l`

@ ngroup = $nfile / $nmedian
 
if($ngroup == 0) then
  set ngroup = 1
endif

source $STARLINK_DIR/etc/cshrc
source $STARLINK_DIR/etc/login
figaro > /dev/null
pamela

set ng    = 0
set flist = `cat $list`

@ ntest = $ngroup - 1

while ($ng < $ngroup)

  @ nf1 = $nmedian * $ng + 1
  @ nf2 = $nf1 + $nmedian

  if( $ng == $ntest) then
    @ nf2 = $nfile + 1
  endif

  @ ngp = $ng + 1

  set nf = $nf1
  \rm -f zzz_makebias 
  while($nf < $nf2)
    echo $flist[$nf] >> zzz_makebias
    @ nf++
  end

  echo "Combining group $ng"
  echo "First subtracting constant offsets determined with picstat"
  echo " "

  foreach file (`cat zzz_makebias`)
    if(-e $file.sdf) then
      echo "Processing file = $file"
      picstat $file $bias_region clip=3 plot=no
      icsub   $file factor=@$ADAM_USER/GLOBAL.PIC_MEAN output=zzz_junk
      mv zzz_junk.sdf $file.sdf
    else
      echo "Could not find $file.sdf"
      exit
    endif
  end

  echo " "
  echo "Now applying medsky"

  @ nmult = $nf2 - $nf1

# after medsky we add back the most recetly determined constant
# which applies to the last frame of each group

  if($ng == 0) then
    medsky zzz_makebias noscaled $output
    icadd  image=$output factor=@$ADAM_USER/GLOBAL.PIC_MEAN output=$output
    icmult $output $nmult $output
  else
    medsky zzz_makebias noscaled zzz_bias
    icadd  image=zzz_bias factor=@$ADAM_USER/GLOBAL.PIC_MEAN output=zzz_bias
    icmult zzz_bias $nmult zzz_bias
    iadd $output zzz_bias $output
  endif
  @ ng++
end

icdiv $output $nfile $output
\rm -f zzz_${output}.sdf
echo "Finished, with final result dumped to ${output}.sdf"

exit


