#  Keywords for use by SCCS.
#  $Id$

#  File to define groups for use by the SDT grp command.
#  ====================================================
#  This is a description file for the "make" utility.
#  Original created by the SDT newdev command on:
#
#           Wed Jul  6 13:16:31 BST 1994


#  Define new groups here...

#  The routines which belong to the interface of IMG.

interface = hdr_delet.f hdr_in.f hdr_inx.gsc hdr_name.f hdr_numb.f \
            hdr_out.f hdr_outx.gsc img_cancl.f img_delet.f img_free.f \
            img_in.f img_in1.f img_in1x.gen img_in2.f img_in2x.gen \
            img_in3.f img_in3x.gen img_indf.f img_inx.gen img_new.f \
            img_new1.f img_new1x.gen img_new2.f img_new2x.gen img_new3.f \
            img_new3x.gen img_newx.gen img_out.f img_outx.gen img_tmp.f \
            img_tmp1.f img_tmp1x.gen img_tmp2.f img_tmp2x.gen img_tmp3.f \
            img_tmp3x.gen img_tmpx.gen

#  Target for use by the grp command.
$(action)
