#!/usr/bin/csh -f
#
# Creates opt.lis etc for molly input. Run
# in whatever directory has the 1D files

ls r*opt.sdf  >! opt.lis
ls r*nor.sdf  >! nor.lis
ls r*arco.sdf >! arco.lis
ls r*arcn.sdf >! arcn.lis
ls r*skyo.sdf >! skyo.lis
ls r*skyn.sdf >! skyn.lis
