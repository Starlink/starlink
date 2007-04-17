#  "@(#) $Id: ittasc.sh,v 1.1.1.1 2006/01/12 16:37:41 abrighto Exp $" 
# @(#)ittasc.sh	8.1.1.1 (ESO-IPG) 8/31/94 15:51:58
# Bourne shell script  ittasc.sh
# to delete lines 1-4 and last line of ascii table file
# and to remove 1. field
# 
sed '1,4d
     $d' $1 >temp.temp
# 
# now we use awk to throw out the sequence field
# 
awk '{ printf "                   %s\n", $2 }' temp.temp >temp.final
# 
mv temp.final $2
rm temp.temp
