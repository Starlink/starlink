$!
$! Test procedure for NDPROGS.
$! ---------------------------
$!
$! Run this, have a cup of tea, wait till a picture appears, 
$! and check that FINAL.DST contains nothing but zeros.
$! If it does, NDPROGS is probably installed OK. 
$!
$! Remove the comments on the $delete lines to get rid of intermediate 
$! files during processsing.
$!
$! Julian Gold  RGO  CAVAD::GOLDJIL or GOLDJIL@UK.AC.CAM.AST-STAR
$!
$test OUT=image_qe NDIM=3 SIZE=(256 128 4) DTY=s EXTRA=qe AXES -
      AXKEY=(1 1 0) AXST=(128 -64 0) AXEN=(-128 64 0) AXLOG=(0 0 0)
$axflip IM=image_qe AXIS=1 OUT=image_1
$!delete image_qe.dst;
$transpose IM=image_1 ORDER=(2 1 3) OUT=image_2 
$!delete image_1.dst;
$subset IM=image_2 ST=(-64 -64 1) EN=(64 64 4) OUT=image_3
$!delete image_2.dst;
$collapse IM=image_3 AXKEY=(0 0 1) WH OUT=image_4 FL
$!delete image_3.dst;
$transform IM=image_4 ERR_VAL=0 SHIFT=(0 0) CENTRE=(64 64) ANGLE=(90) -
           NOAXES RESAMPLE=(1 1) INTERP=1 OUT=image_5
$typecon IM=image_4 OUT=image_6
$!delete image_4.dst;
$typecon IM=image_5 OUT=image_7
$!delete image_5.dst;
$logic2 IM=image_6 IMAGE1=image_7 WH OP=or OUT=image_8
$!delete image_6.dst;
$!delete image_7.dst;
$arith1 IM=image_8 WH OP=- VAL=340 OUT=image_9
$!delete image_8.dst;
$stretch IMAGE1=image_9 IMAGE2=dummy OUTPUT=image_a INTERP=LINEAR
$!delete image_9.dst;
$arith2 IM=image_a IMAGE1=my_image_a WH OP=- OUT=final
$depict IM=image_a LO=0 HI=680 PLACE=c MAG=1 LAB=Test AX RAMP -
        WH NOCONT NOHA TAB=rainbow ER
