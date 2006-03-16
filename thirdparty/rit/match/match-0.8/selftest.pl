#!/usr/bin/perl
#
# run a test of the "match" program, and compare its outputs to some
#   known values.  
#
# The test input files were created using one set of points which were
#   transformed in a cubic manner with the following coefficients:
#    
#       x'                       y'
#     a =  0.0                  i =  0.0
#     b =  0.707                j =  0.707
#     c = -0.707                k =  0.707
#     d =  0.00005              l =  0.00005
#     e = -0.00005              m = -0.00005
#     f =  0.00005              n =  0.00005
#     g =  0.0000001            o =  0.0000001
#     h =  0.0000001            p =  0.0000001
#
#   This corresponds roughly to a 45-degree rotation, with a little
#   distortion thrown in.  We check the result of the "match" program
#   to verify that it derives coefficients which are close to these values.
#   
#   Print error messages as we go (if errors occur),
#   and exit with code
#
#        0           if all goes well
#        1           if error(s) occur
#        
# MWR 6/12/2000
#
#   Added a third test file, contining points which are related to
#   'selfa.dat' by a simple translation (with a little non-random scatter
#   thrown in).  We need that set to test the new "medtf" option.
#   Also, added some checks for proper handling command-line options.
#
# MWR 12/14/2001
# 
#   Added new test files
#         selfd.dat    selfe.dat
#   to test the input TRANS routines.  Each of these files has
#   70 objects: the first 20 in each are 'good', matching the first
#   20 in the other file after a translation of (+30, -50) units.
#   The final 50 in each file are 'bad', simply random points which
#   should not match at all.
#   Added new test files
#         intransa.dat intransb.dat intransc.dat 
#         intransd.dat intranse.dat intransf.dat
#   for the new 'intrans=' option.
#   Also, added some checks for proper handling of new command-line
#   arguments.
#   Modified "check_value" so that it only prints out diagnostic messages
#   if 'debug' is non-zero.
# MWR 12/31/2001
#
#   Modify the code so it expects to see the 2 new members of the 
#   TRANS structre, "sx" and "sy".
# MWR 6/18/2002
#
#   Add test of the "project_coords" program.
# MWR 10/25/2003
#

# set this to 1 to enable lots of debugging messages
$debug = 0;

# When we're running 'make', the "srcdir" is where the input files 
#   for the self-test live.
# But when we're running the test manually, the input files are
#   in the current directory
$srcdir = $ENV{"srcdir"};
if (length($srcdir) < 1) {
  $srcdir = ".";
}
if ($debug > 0) {
  printf "srcdir is $srcdir\n";
}
$input_a = "$srcdir/selfa.dat";
$input_b = "$srcdir/selfb.dat";
$input_c = "$srcdir/selfc.dat";
$input_d = "$srcdir/selfd.dat";
$input_e = "$srcdir/selfe.dat";
$intrans_a = "$srcdir/intransa.dat";
$intrans_b = "$srcdir/intransb.dat";
$intrans_c = "$srcdir/intransc.dat";
$intrans_d = "$srcdir/intransd.dat";
$intrans_e = "$srcdir/intranse.dat";
$intrans_f = "$srcdir/intransf.dat";
$project_a = "$srcdir/self_m57.dat";
$project_b = "$srcdir/self_m57_rad.dat";
$project_c = "$srcdir/self_m57_asec.dat";

# make sure that all the data files exist
if (!(-r $input_a) || !(-r $input_b) || !(-r $input_c) ||
    !(-r $input_d) || !(-r $input_e)) {
  printf STDERR "can't open input file(s) $input_a and $input_b and $input_c\n";
  printf STDERR "    and $input_d and $input_e \n";
  exit(1);
}
if (!(-r $intrans_a) || !(-r $intrans_b) || !(-r $intrans_c) || 
    !(-r $intrans_d) || !(-r $intrans_e) || !(-r $intrans_f)) {
  printf STDERR "can't open intrans file(s) $intrans_a and $intrans_b \n";
  printf STDERR "    and $intrans_c and $intrans_d \n";
  printf STDERR "    and $intrans_e and $intrans_f \n";
  exit(1);
}
if (!(-r $project_a) || !(-r $project_b) || !(-r $project_c)) {
  printf STDERR "can't open project_coords file(s) $project_a \n";
  printf STDERR "    and/or $project_b and/or $project_c \n";
  exit(1);
}


# this will be the final exit code -- if 0, all is well.  
#   We increment it every time a test fails
$final_code = 0;

# we're going to run three tests: one each for linear, quadratic,
#    and cubic transformations.  
#    
# first, the linear: we run two tests, for "transonly" and "recalc" options
$teststr = "./match $input_a 1 2 3 $input_b 1 2 3 trirad=0.002 nobj=20 linear transonly";
$retval = `./match $input_a 1 2 3 $input_b 1 2 3 trirad=0.002 nobj=20 linear transonly`;
if ($debug > 0) {
  printf "running linear test, transonly.  \n";
  printf "$teststr\n";
  printf "$retval\n";
}
$final_code += check_linear($retval);


$retval = 
  `./match $input_a 1 2 3 $input_b 1 2 3 trirad=0.002 nobj=20 linear recalc`;
if ($debug > 0) {
  printf "running linear test, recalc  Result is:\n";
  printf "$retval";
}
$final_code += check_linear($retval);


# now, the tests with a quadratic plate solution
$retval = `./match $input_a 1 2 3 $input_b 1 2 3 trirad=0.002 nobj=20 quadratic transonly`;
if ($debug > 0) {
  printf "running quadratic test, transonly.  Result is:\n";
  printf "$retval";
}
$final_code += check_quadratic($retval);

$retval = `./match $input_a 1 2 3 $input_b 1 2 3 trirad=0.002 nobj=20 quadratic recalc`;
if ($debug > 0) {
  printf "running quadratic test, recalc  Result is:\n";
  printf "$retval";
}
$final_code += check_quadratic($retval);


# now, the tests with a cubic plate solution
$retval = `./match $input_a 1 2 3 $input_b 1 2 3 trirad=0.002 nobj=20 cubic transonly`;
if ($debug > 0) {
  printf "running cubic test, transonly.  Result is:\n";
  printf "$retval";
}
$final_code += check_cubic($retval);

$retval = `./match $input_a 1 2 3 $input_b 1 2 3 trirad=0.002 nobj=20 cubic recalc`;
if ($debug > 0) {
  printf "running cubic test, recalc  Result is:\n";
  printf "$retval";
}
$final_code += check_cubic($retval);



# check the "id1=" and "id2=" options
$retval = `./match $input_a 1 2 3 $input_b 1 2 3 trirad=0.002 nobj=20 linear id1=0 id2=0`; 
if ($debug > 0) {
  printf "running test of id1=, id2= options.  Results is:\n";
  printf "$retval";
}
$final_code += check_id("matched.mtA", "matched.mtB");


# Check the 'medtf' option
$retval = `./match $input_a 1 2 3 $input_c 1 2 3 trirad=0.002 nobj=20 linear medtf 2>&1 `;
if ($debug > 0) {
  printf "running test of medtf.  Results is:\n";
  printf "$retval";
}
$final_code += check_medtf($retval);

# Check the 'medsigclip' option
$retval = `./match $input_a 1 2 3 $input_c 1 2 3 trirad=0.002 nobj=20 linear medsigclip=2.5 2>&1 `;
if ($debug > 0) {
  printf "running test of medsigclip.  Results is:\n";
  printf "$retval";
}
$final_code += check_medsigclip($retval);


# Now check that the 'scale', 'min_scale' and 'max_scale' options 
#    are handled properly.  Use a simple linear translation as the
#    test case.
#    
# This should work -- scale is 1.0
$retval = `./match $input_a 1 2 3 $input_b 1 2 3 trirad=0.002 nobj=20 linear transonly scale=1.0`;
if ($debug > 0) {
  printf "running scale test A.  \n";
  printf "$retval\n";
}
$final_code += check_linear($retval);
#
# This should also work
$retval = `./match $input_a 1 2 3 $input_b 1 2 3 trirad=0.002 nobj=20 linear transonly min_scale=0.8 max_scale=1.2`;
if ($debug > 0) {
  printf "running scale test B.  \n";
  printf "$retval\n";
}
$final_code += check_linear($retval);
#
# This should NOT work, as the true scale is 1.0
$retval = `./match $input_a 1 2 3 $input_b 1 2 3 trirad=0.002 nobj=20 linear transonly scale=5.0 2>&1`;
if ($debug > 0) {
  printf "running scale test C.  \n";
  printf "$retval\n";
}
$final_code += check_failed($retval, "unable to create a valid TRANS",
                                "test of invalid scale factor");
#
#
# This should NOT work, as the true scale is 1.0
$retval = `./match $input_a 1 2 3 $input_b 1 2 3 trirad=0.002 nobj=20 linear transonly min_scale=5.0 max_scale=10.0 2>&1`;
if ($debug > 0) {
  printf "running scale test D.  \n";
  printf "$retval\n";
}
$final_code += check_failed($retval, "unable to create a valid TRANS",
                                "test of invalid min_scale, max_scale factors");
#
#
# Now check some invalid combinations of command-line options
$retval = `./match $input_a 1 2 3 $input_b 1 2 3 trirad=0.002 nobj=20 linear transonly scale=1.0 min_scale=5.0 max_scale=10.0 2>&1`;
if ($debug > 0) {
  printf "running scale test E.  \n";
  printf "$retval\n";
}
$final_code += check_failed($retval, "invalid combination",
                                "test of invalid combination of scale factors");
#
$retval = `./match $input_a 1 2 3 $input_b 1 2 3 trirad=0.002 nobj=20 linear transonly scale=1.0 min_scale=5.0 2>&1`;
if ($debug > 0) {
  printf "running scale test F.  \n";
  printf "$retval\n";
}
$final_code += check_failed($retval, "invalid combination",
                                "test of invalid combination of scale factors");
#
$retval = `./match $input_a 1 2 3 $input_b 1 2 3 trirad=0.002 nobj=20 linear transonly scale=1.0 max_scale=5.0 2>&1`;
if ($debug > 0) {
  printf "running scale test G.  \n";
  printf "$retval\n";
}
$final_code += check_failed($retval, "invalid combination",
                                "test of invalid combination of scale factors");
#
$retval = `./match $input_a 1 2 3 $input_b 1 2 3 trirad=0.002 nobj=20 linear transonly max_scale=5.0 2>&1`;
if ($debug > 0) {
  printf "running scale test H.  \n";
  printf "$retval\n";
}
$final_code += check_failed($retval, "invalid combination",
                                "test of invalid combination of scale factors");
#
$retval = `./match $input_a 1 2 3 $input_b 1 2 3 trirad=0.002 nobj=20 linear transonly min_scale=5.0 2>&1`;
if ($debug > 0) {
  printf "running scale test I.  \n";
  printf "$retval\n";
}
$final_code += check_failed($retval, "invalid combination",
                                "test of invalid combination of scale factors");
#
$retval = `./match $input_a 1 2 3 $input_b 1 2 3 trirad=0.002 nobj=20 linear transonly min_scale=5.0 max_scale=1.0 2>&1`;
if ($debug > 0) {
  printf "running scale test J.  \n";
  printf "$retval\n";
}
$final_code += check_failed($retval, "min_scale must be smaller",
                                "test of invalid combination of scale factors");
#
$retval = `./match $input_a 1 2 3 $input_b 1 2 3 trirad=0.002 nobj=20 linear transonly scale=-5.0 2>&1`;
if ($debug > 0) {
  printf "running scale test K.  \n";
  printf "$retval\n";
}
$final_code += check_failed($retval, "must be > 0",
                                "test of negative scale factor");
#
$retval = `./match $input_d 1 2 3 $input_e 1 2 3 trirad=0.002 nobj=70 identity intrans=$intrans_a 2>&1`;
if ($debug > 0) {
  printf "running identity test A.  \n";
  printf "$retval\n";
}
$final_code += check_failed($retval, "Cannot specify both",
                                "test of identity/intrans args");
#
#
# Now check that program can distinguish valid from invalid input TRANS files
#
$retval = `./match $input_d 1 2 3 $input_e 1 2 3 trirad=0.002 nobj=70 intrans=$intrans_c 2>&1`;
if ($debug > 0) {
  printf "running intrans test A.  \n";
  printf "$retval\n";
}
$final_code += check_failed($retval, "for Norder",
                                "test of intrans parsing");
#
$retval = `./match $input_d 1 2 3 $input_e 1 2 3 trirad=0.002 nobj=70 intrans=$intrans_d 2>&1`;
if ($debug > 0) {
  printf "running intrans test B.  \n";
  printf "$retval\n";
}
$final_code += check_failed($retval, "invalid coefficient spec",
                                "test of intrans parsing");
#


#
# Now check that the 'identity' keyword causes us to get the right
#   answer (or wrong answer, if misused)
#
#   this test should fail
$retval = `./match $input_d 1 2 3 $input_e 1 2 3 trirad=0.002 nobj=70 identity`;
if ($debug > 0) {
  printf "running identity test A.  \n";
  printf "$retval\n";
}
$retcode = check_identity($retval);
$final_code += ($retcode == 0);
#
#   but this test should succeed
$retval = `./match $input_d 1 2 3 $input_e 1 2 3 trirad=0.002 nobj=70 identity xsh=30 ysh=-50`;
if ($debug > 0) {
  printf "running identity test B.  \n";
  printf "$retval\n";
}
$retcode = check_identity($retval);
$final_code += ($retcode != 0);
#


#
# Now check that the 'intrans' keyword causes us to get the right
#   answer (or wrong answer, if misused)
#
#   this test should succeed
$retval = `./match $input_d 1 2 3 $input_e 1 2 3 trirad=0.002 nobj=70 intrans=$intrans_a`;
if ($debug > 0) {
  printf "running intrans test A.  \n";
  printf "$retval\n";
}
$retcode = check_identity($retval);
$final_code += ($retcode != 0);
#
#   but this test should fails
$retval = `./match $input_d 1 2 3 $input_e 1 2 3 trirad=0.002 nobj=70 intrans=$intrans_b`;
if ($debug > 0) {
  printf "running intrans test B.  \n";
  printf "$retval\n";
}
$retcode = check_identity($retval);
$final_code += ($retcode == 0);
#
#   this test of quadratic model should succeed
$retval = `./match $input_d 1 2 3 $input_e 1 2 3 trirad=0.002 nobj=70 intrans=$intrans_e`;
if ($debug > 0) {
  printf "running intrans test C.  \n";
  printf "$retval\n";
}
$retcode = check_identity_quadratic($retval);
$final_code += ($retcode != 0);
#
#   this test of cubic model should succeed
$retval = `./match $input_d 1 2 3 $input_e 1 2 3 trirad=0.002 nobj=70 intrans=$intrans_f`;
if ($debug > 0) {
  printf "running intrans test D.  \n";
  printf "$retval\n";
}
$retcode = check_identity_cubic($retval);
$final_code += ($retcode != 0);
#



# run a simple check of the "project_coords" program
if ($debug > 0) {
  printf "running project_coords test \n";
}
$retcode = check_project_coords();
$final_code += ($retcode != 0);




# remove some temp files we created during the tests
unlink("matched.mtA");
unlink("matched.mtB");
unlink("matched.unA");
unlink("matched.unB");


if ($final_code == 0) {
  printf "match: passed all tests\n";
} else {
  printf "match: failed some test(s)\n";
}
exit($final_code);



#############################################################################
# PROCEDURE: check_linear
# 
# This procedure examines the output TRANS structure returned by the
#   "match" code for a linear transformation.  We make sure that each
#   of the TRANS coefficients is in the proper range.
#
# The TRANS should have 11 elements, and look something like this:
#
#   TRANS: a=0.043661422     b=0.707484673     c=-0.707367056    
#          d=0.077633855     e=0.708066513     f=0.706880146    
#          sig=7.2292e-03    Nr=20             Nm=30
#          sx=5.8231e-01     sy=1.0441e-01
#
# Returns:
#   0           if all goes well
#   > 0         if one or more coeffs are outside their proper range
#                    (the value is the number of coeffs which fail test)
#
sub check_linear {
  my($string, 
     @words,
     $a, $b, $c, $d, $e, $f,
     $ret_code);

  $string = $_[0];
  $ret_code = 0;

  @words = split(/\s+/, $string);
  if ($#words != 11) {
    printf STDERR "check_linear: wrong number of words in $string\n";
    return(1);
  }

  $ret_code += check_value($words[1], 0.0,   0.10, "coeff a");
  $ret_code += check_value($words[2], 0.707, 0.02, "coeff b");
  $ret_code += check_value($words[3],-0.707, 0.02, "coeff c");
  $ret_code += check_value($words[4], 0.0,   0.10, "coeff d");
  $ret_code += check_value($words[5], 0.707, 0.02, "coeff e");
  $ret_code += check_value($words[6], 0.707, 0.02, "coeff f");
  $ret_code += check_value($words[7], 0.01,  0.01, "coeff sig");
  $ret_code += check_value($words[8], 15,    5   , "coeff Nr");
  $ret_code += check_value($words[10], 0.0,  0.5 , "coeff sx");
  $ret_code += check_value($words[11], 0.0,  0.5 , "coeff sy");

  return($ret_code);
}


#############################################################################
# PROCEDURE: check_quadratic
# 
# This procedure examines the output TRANS structure returned by the
#   "match" code for a quadratic transformation.  We make sure that each
#   of the TRANS coefficients is in the proper range.
#
# The TRANS should have 17 elements, and look something like this:
#
#   TRANS: a=-0.003196734    b=0.706512474     c=-0.706392173    
#          d=0.000033325     e=-0.000078381    f=0.000063959 
#          g=0.033404956     h=0.707162877     i=0.707694690  
#          j=0.000031248     k=-0.000084174    l=0.000056344    
#          sig=7.2292e-03    Nr=20             Nm=30
#          sx=5.8231e-01     sy=1.0441e-01
#
# Returns:
#   0           if all goes well
#   > 0         if one or more coeffs are outside their proper range
#                    (the value is the number of coeffs which fail test)
#
sub check_quadratic {
  my($string, 
     @words,
     $a, $b, $c, $d, $e, $f, $g, $h, $i, $j, $k, $l,
     $ret_code);

  $string = $_[0];
  $ret_code = 0;

  @words = split(/\s+/, $string);
  if ($#words != 17) {
    printf STDERR "check_quadratic: wrong number of words in $string\n";
    return(1);
  }

  $ret_code += check_value($words[1], 0.0,   0.10, "coeff a");
  $ret_code += check_value($words[2], 0.707, 0.02, "coeff b");
  $ret_code += check_value($words[3],-0.707, 0.02, "coeff c");
  $ret_code += check_value($words[4], 0.00005, 0.00010, "coeff d");
  $ret_code += check_value($words[5],-0.00005, 0.00010, "coeff e");
  $ret_code += check_value($words[6], 0.00005, 0.00010, "coeff f");
  $ret_code += check_value($words[7], 0.0,   0.10, "coeff g");
  $ret_code += check_value($words[8], 0.707, 0.02, "coeff h");
  $ret_code += check_value($words[9], 0.707, 0.02, "coeff i");
  $ret_code += check_value($words[10], 0.00005, 0.00010, "coeff j");
  $ret_code += check_value($words[11],-0.00005, 0.00010, "coeff k");
  $ret_code += check_value($words[12], 0.00005, 0.00010, "coeff l");
  $ret_code += check_value($words[13], 0.01,  0.01, "coeff sig");
  $ret_code += check_value($words[14], 15,    5   , "coeff Nr");
  $ret_code += check_value($words[16], 0.0,   0.1 , "coeff sx");
  $ret_code += check_value($words[17], 0.0,   0.1 , "coeff sy");

  return($ret_code);
}


#############################################################################
# PROCEDURE: check_cubic
# 
# This procedure examines the output TRANS structure returned by the
#   "match" code for a cubic transformation.  We make sure that each
#   of the TRANS coefficients is in the proper range.
#
# The TRANS should have 21 elements, and look something like this:
#
#   TRANS: a=-0.003196734    b=0.706512474     c=-0.706392173    
#          d=0.000033325     e=-0.000078381    f=0.000063959 
#          g=0.033404956     h=0.707162877     i=0.707694690  
#          j=0.000031248     k=-0.000084174    l=0.000056344    
#          sig=7.2292e-03    Nr=20             Nm=30
#          sx=5.8231e-01     sy=1.0441e-01
#
# Returns:
#   0           if all goes well
#   > 0         if one or more coeffs are outside their proper range
#                    (the value is the number of coeffs which fail test)
#
sub check_cubic {
  my($string, 
     @words,
     $a, $b, $c, $d, $e, $f, $g, $h, $i, $j, $k, $l, $m, $n, $o, $p,
     $ret_code);

  $string = $_[0];
  $ret_code = 0;

  @words = split(/\s+/, $string);
  if ($#words != 21) {
    printf STDERR "check_cubic: wrong number of words in $string\n";
    return(1);
  }

  $ret_code += check_value($words[1], 0.0,   0.10, "coeff a");
  $ret_code += check_value($words[2], 0.707, 0.02, "coeff b");
  $ret_code += check_value($words[3],-0.707, 0.02, "coeff c");
  $ret_code += check_value($words[4], 0.00005, 0.00010, "coeff d");
  $ret_code += check_value($words[5],-0.00005, 0.00010, "coeff e");
  $ret_code += check_value($words[6], 0.00005, 0.00010, "coeff f");
  $ret_code += check_value($words[7], 0.0000001, 0.00001, "coeff g");
  $ret_code += check_value($words[8], 0.0000001, 0.00001, "coeff h");
  $ret_code += check_value($words[9], 0.0,   0.10, "coeff i");
  $ret_code += check_value($words[10], 0.707, 0.02, "coeff j");
  $ret_code += check_value($words[11], 0.707, 0.02, "coeff k");
  $ret_code += check_value($words[12], 0.00005, 0.00010, "coeff l");
  $ret_code += check_value($words[13],-0.00005, 0.00010, "coeff m");
  $ret_code += check_value($words[14], 0.00005, 0.00010, "coeff n");
  $ret_code += check_value($words[15], 0.0000001, 0.00001, "coeff o");
  $ret_code += check_value($words[16], 0.0000001, 0.00001, "coeff p");
  $ret_code += check_value($words[17], 0.01,  0.01, "coeff sig");
  $ret_code += check_value($words[18], 15,    5   , "coeff Nr");
  $ret_code += check_value($words[20], 0.0,   0.1 , "coeff sx");
  $ret_code += check_value($words[21], 0.0,   0.1 , "coeff sy");

  return($ret_code);
}


#############################################################################
# PROCEDURE: check_medtf
# 
# This procedure examines the output MEDTF structure returned by the
#   "match" code for a pure translation.  We make sure that each
#   of the MEDTF coefficients is in the proper range.
#
# The MEDTF should have 7 elements, and look something like this:
#
#   TRANS: mdx=-1.010000000    mdy=-1.020000000   
#          adx=-1.002000000    ady=-1.002000000
#          sdx=0.015684387     sdy=0.031559468     n=20
#
# Returns:
#   0           if all goes well
#   > 0         if one or more coeffs are outside their proper range
#                    (the value is the number of coeffs which fail test)
#
sub check_medtf {
  my($string, 
	  @lines,
     @words,
	  $index,
	  $mdx, $mdy, $adx, $ady, $sdx, $sdy, $n,
     $ret_code);

  $string = $_[0];
  $ret_code = 0;

  # we check only the line with MEDTF, which should be the first line
  #   in the output which isn't a WARNING
  @lines = split(/\n+/, $string);
  $index = 0;
  while ($lines[$index] =~ /WARNING/) {
    $index++;
  }
  @words = split(/\s+/, $lines[$index]);
  if ($#words != 7) {
    printf STDERR "check_medtf: wrong number of words in $lines[$index]\n";
    return(1);
  }

  $ret_code += check_value($words[1], -1.0,  0.05, "mdx");
  $ret_code += check_value($words[2], -1.0,  0.05, "mdy");
  $ret_code += check_value($words[3], -1.0,  0.05, "adx");
  $ret_code += check_value($words[4], -1.0,  0.05, "ady");
  $ret_code += check_value($words[5],  0.03, 0.03, "sdx");
  $ret_code += check_value($words[6],  0.03, 0.03, "sdy");
  $ret_code += check_value($words[7], 20,    1,    "n");

  return($ret_code);
}


#############################################################################
# PROCEDURE: check_medsigclip
# 
# This procedure examines the output MEDTF structure returned by the
#   "match" code for a pure translation -- discarding all pairs of
#   items which are more than 2.5 stdev from the mean.  We make sure that each
#   of the MEDTF coefficients is in the proper range.
#
# The MEDTF should have 7 elements, and look something like this:
#
#   TRANS: mdx=-1.010000000    mdy=-1.020000000   
#          adx=-1.002000000    ady=-1.002000000
#          sdx=0.010684387     sdy=0.008559468     n=13
#
# Returns:
#   0           if all goes well
#   > 0         if one or more coeffs are outside their proper range
#                    (the value is the number of coeffs which fail test)
#
sub check_medsigclip {
  my($string, 
	  @lines,
     @words,
	  $index,
	  $mdx, $mdy, $adx, $ady, $sdx, $sdy, $n,
     $ret_code);

  $string = $_[0];
  $ret_code = 0;

  # we check only the line with MEDTF, which should be the first line
  #   in the output which isn't a WARNING
  @lines = split(/\n+/, $string);
  $index = 0;
  while ($lines[$index] =~ /WARNING/) {
    $index++;
  }
  @words = split(/\s+/, $lines[$index]);
  if ($#words != 7) {
    printf STDERR "check_medsigclip: wrong number of words in $lines[$index]\n";
    return(1);
  }

  $ret_code += check_value($words[1], -1.0,  0.05, "mdx");
  $ret_code += check_value($words[2], -1.0,  0.05, "mdy");
  $ret_code += check_value($words[3], -1.0,  0.05, "adx");
  $ret_code += check_value($words[4], -1.0,  0.05, "ady");
  $ret_code += check_value($words[5],  0.01, 0.01, "sdx");
  $ret_code += check_value($words[6],  0.01, 0.01, "sdy");
  $ret_code += check_value($words[7], 13,    1,    "n");

  return($ret_code);
}


#############################################################################
# PROCEDURE: check_failed
# 
# Usage:
#          check_failed   (program output)   error_string   explanation
#
# This procedure checks the (stdout + stderr) result of running
#   the "match" program.  It expects that the program SHOULD have
#   failed (because we gave improper arguments, or no match could
#   be found).  It looks for the given "error_string" in the 
#   captured output.
#
# If the expected string isn't found, the final argument "explanation"
#   is printed to stdout.
#
# Returns:
#   0           if it can find the expected "error_string" in output
#   > 0         if it can't
#
sub check_failed {
  my($string, 
     $error_string,
	  $explanation,
	  @lines,
     $ret_code);

  $string = $_[0];
  $error_string = $_[1];
  $explanation = $_[2];
  $ret_code = 0;

  # we check line by line until we find the expected string, then quit
  @lines = split(/\n+/, $string);
  foreach $line (@lines) {
    if ($line =~ /$error_string/) {
	   return(0);
    }
  }

  # nope, we never found the expected string.  Rats.
  printf "unexpected result in $explanation\n";
  return(1);
}





##############################################################################
# PROCEDURE: check_value
# 
# usage: check_value    value_string correct_value slop name
# 
# This procedure compares the value within "value_string" to "correct_value"; 
# if the two are equal within the "slop"
#
#                correct_value - slop < value < correct_value + slop
#
#   then the function returns 0.  Otherwise, it prints an error message
#   describing the actual and expected values, using the "name" given
#   as the final arg
#
# The "value_string" has a name, an equals sign, and then a
#   numerical value, like this:
#    
#             a=0.4342300         or      sig=0.3534e-03
#   
#            
#   We need to grab the number from this string as a first step, before
#   we can compare it to the correct value.
#  
# RETURNS
#    0            if all goes well
#    1            if value is outside range (plus prints error message)
#
sub check_value {
  my($value_string, $value, $correct_value, $slop, $name,
     $min, $max);

  $value_string = $_[0];
  $value = $value_string;
  $value =~ s/^[A-Za-z]+=//;
  $correct_value = $_[1];
  $slop = $_[2];
  $name = $_[3];

  $min = $correct_value - $slop;
  $max = $correct_value + $slop;

  if (($value < $min) || ($value > $max)) {
    if ($debug > 0) {
      printf STDERR "%s has value %10.6e outside range %10.6e %10.6e\n",
                    $name, $value, $min, $max;
    }
    return(1);
  }

  return(0);
}




#############################################################################
# PROCEDURE: check_id
# 
# This procedure looks at the output files created by a match of 
#   the test data.  Each of the two files in the test data set
#   contains an ID code in column 0.  Matching stars in the two
#   input files have the same ID value, so the corresponding lines
#   in the output files ought to have the same ID values, too.
#
# Returns:
#   0           if all goes well
#   > 0         if one or more IDs in the two output files don't match
#
sub check_id {
  my($output_a, $output_b, 
     @words,
     $nid_a, $nid_b, 
     @id_a, @id_b,
     $ret_code);

  $output_a = $_[0];
  $output_b = $_[1];
  $ret_code = 0;

  # first, make sure that the output files exist
  if (!(-r $input_a) || !(-r $input_b)) {
    printf STDERR "check_id: can't open output file(s) $output_a and $output_b";
    return(1);
  }

  # now, walk through the two output files.  They should have the same
  #   number of lines, and the first word in each line (the ID value)
  #   ought to be the same.
  open(OUTPUT_A, $output_a) || die("check_id: can't open file $output_a");
  open(OUTPUT_B, $output_b) || die("check_id: can't open file $output_b");

  $nid_a = 0;
  while (<OUTPUT_A>) {
    @words = split(/\s+/, $_);
    $id_a[$nid_a] = $words[1];
    $nid_a++;
  }
  close(OUTPUT_A);
    
  $nid_b = 0;
  while (<OUTPUT_B>) {
    @words = split(/\s+/, $_);
    $id_b[$nid_b] = $words[1];
    $nid_b++;
  }
  close(OUTPUT_B);
    
  if ($nid_a != $nid_b) {
    printf STDERR "check_id: output files have different number of lines\n";
    return(1);
  }
  
  for ($i = 0; $i < $nid_a; $i++) {
    if ($debug > 0) {
      printf "  check_id: line %3d  A %6d   B %6d \n", 
                                       $i, $id_a[$i], $id_b[$i];
    }
    if ($id_a[$i] != $id_b[$i]) {
      printf STDERR "check_id: ID mismatch for line $i\n";
      $ret_code++;
    }
  }

  return($ret_code);
}


#############################################################################
# PROCEDURE: check_identity
# 
# This procedure examines the output TRANS structure returned by the
#   "match" code for tests of the 'identity' argument.  We make sure that each
#   of the TRANS coefficients is in the proper range.
#
# The TRANS should have 11 elements, and look something like this:
#
#   TRANS: a=30.00000000     b=1.000000000     c=0.000000000     
#          d=-50.000000000   e=0.000000000     f=1.000000000    
#          sig=1.0000e-10    Nr=20             Nm=23
#          sx=5.0000e-01     sy=5.0000e-01
#
# Returns:
#   0           if all goes well
#   > 0         if one or more coeffs are outside their proper range
#                    (the value is the number of coeffs which fail test)
#
sub check_identity {
  my($string, 
     @words,
     $a, $b, $c, $d, $e, $f,
     $ret_code);

  $string = $_[0];
  $ret_code = 0;

  @words = split(/\s+/, $string);
  if ($#words != 11) {
    printf STDERR "check_identity: wrong number of words in $string\n";
    return(1);
  }

  $ret_code += check_value($words[1], 30.0,     1.0e-6, "coeff a");
  $ret_code += check_value($words[2], 1.000,    1.0e-6, "coeff b");
  $ret_code += check_value($words[3], 0.000,    1.0e-6, "coeff c");
  $ret_code += check_value($words[4], -50.0,    1.0e-6, "coeff d");
  $ret_code += check_value($words[5], 0.000,    1.0e-6, "coeff e");
  $ret_code += check_value($words[6], 1.000,    1.0e-6, "coeff f");
  $ret_code += check_value($words[7], 1.0e-10,  1.0e-6, "coeff sig");
  $ret_code += check_value($words[8], 20,       1     , "coeff Nr");
  $ret_code += check_value($words[10], 0.50,     0.50  , "coeff sx");
  $ret_code += check_value($words[11], 0.50,    0.50  , "coeff sy");

  return($ret_code);
}


#############################################################################
# PROCEDURE: check_identity_quadratic
# 
# This procedure examines the output TRANS structure returned by the
#   "match" code for tests of the 'identity' argument.  We make sure that each
#   of the TRANS coefficients is in the proper range.
#
# The TRANS should have 17 elements, and look something like this:
#
#   TRANS: a=30.00000000     b=1.000000000     c=0.000000000     
#          d=0.00000000      e=0.000000000     f=0.000000000
#          g=-50.000000000   h=0.000000000     i=1.000000000    
#          j=0.00000000      k=0.000000000     l=0.000000000
#          sig=1.0000e-10    Nr=20             Nm=23
#          sx=5.0000e-01     sy=5.0000e-01
#
# Returns:
#   0           if all goes well
#   > 0         if one or more coeffs are outside their proper range
#                    (the value is the number of coeffs which fail test)
#
sub check_identity_quadratic {
  my($string, 
     @words,
     $a, $b, $c, $d, $e, $f, $g, $h, $i, $j, $k, $l,
     $ret_code);

  $string = $_[0];
  $ret_code = 0;

  @words = split(/\s+/, $string);
  if ($#words != 17) {
    printf STDERR "check_identity: wrong number of words in $string\n";
    return(1);
  }

  $ret_code += check_value($words[1], 30.0,     1.0e-6, "coeff a");
  $ret_code += check_value($words[2], 1.000,    1.0e-6, "coeff b");
  $ret_code += check_value($words[3], 0.000,    1.0e-6, "coeff c");
  $ret_code += check_value($words[4], 0.000,    1.0e-6, "coeff d");
  $ret_code += check_value($words[5], 0.000,    1.0e-6, "coeff e");
  $ret_code += check_value($words[6], 0.000,    1.0e-6, "coeff f");
  $ret_code += check_value($words[7], -50.0,    1.0e-6, "coeff g");
  $ret_code += check_value($words[8], 0.000,    1.0e-6, "coeff h");
  $ret_code += check_value($words[9], 1.000,    1.0e-6, "coeff i");
  $ret_code += check_value($words[10], 0.000,   1.0e-6, "coeff j");
  $ret_code += check_value($words[11], 0.000,   1.0e-6, "coeff k");
  $ret_code += check_value($words[12], 0.000,   1.0e-6, "coeff l");
  $ret_code += check_value($words[13], 1.0e-10, 1.0e-6, "coeff sig");
  $ret_code += check_value($words[14], 18,      1     , "coeff Nr");
  $ret_code += check_value($words[16], 0.50,    0.50  , "coeff sx");
  $ret_code += check_value($words[17], 0.50,    0.50  , "coeff sy");

  return($ret_code);
}


#############################################################################
# PROCEDURE: check_identity_cubic
# 
# This procedure examines the output TRANS structure returned by the
#   "match" code for tests of the 'identity' argument.  We make sure that each
#   of the TRANS coefficients is in the proper range.
#
# The TRANS should have 21 elements, and look something like this:
#
#   TRANS: a=30.00000000     b=1.000000000     c=0.000000000     
#          d=0.00000000      e=0.000000000     f=0.000000000
#          g=0.00000000      h=0.000000000     
#          i=-50.000000000   j=0.000000000     k=1.000000000    
#          l=0.00000000      m=0.000000000     n=0.000000000
#          o=0.00000000      p=0.000000000     
#          sig=1.0000e-10    Nr=18             Nm=23
#          sx=0.5000e-01     sy=0.5000e-01
#
# Returns:
#   0           if all goes well
#   > 0         if one or more coeffs are outside their proper range
#                    (the value is the number of coeffs which fail test)
#
sub check_identity_cubic {
  my($string, 
     @words,
     $a, $b, $c, $d, $e, $f, $g, $h, $i, $j, $k, $l, $m, $n, $o, $p,
     $ret_code);

  $string = $_[0];
  $ret_code = 0;

  @words = split(/\s+/, $string);
  if ($#words != 21) {
    printf STDERR "check_identity: wrong number of words in $string\n";
    return(1);
  }

  $ret_code += check_value($words[1], 30.0,     1.0e-6, "coeff a");
  $ret_code += check_value($words[2], 1.000,    1.0e-6, "coeff b");
  $ret_code += check_value($words[3], 0.000,    1.0e-6, "coeff c");
  $ret_code += check_value($words[4], 0.000,    1.0e-6, "coeff d");
  $ret_code += check_value($words[5], 0.000,    1.0e-6, "coeff e");
  $ret_code += check_value($words[6], 0.000,    1.0e-6, "coeff f");
  $ret_code += check_value($words[7], 0.000,    1.0e-6, "coeff g");
  $ret_code += check_value($words[8], 0.000,    1.0e-6, "coeff h");
  $ret_code += check_value($words[9], -50.0,    1.0e-6, "coeff i");
  $ret_code += check_value($words[10], 0.000,    1.0e-6, "coeff j");
  $ret_code += check_value($words[11], 1.000,    1.0e-6, "coeff k");
  $ret_code += check_value($words[12], 0.000,   1.0e-6, "coeff l");
  $ret_code += check_value($words[13], 0.000,   1.0e-6, "coeff m");
  $ret_code += check_value($words[14], 0.000,   1.0e-6, "coeff n");
  $ret_code += check_value($words[15], 0.000,   1.0e-6, "coeff o");
  $ret_code += check_value($words[16], 0.000,   1.0e-6, "coeff p");
  $ret_code += check_value($words[17], 1.0e-10, 1.0e-6, "coeff sig");
  $ret_code += check_value($words[18], 18,      1     , "coeff Nr");
  $ret_code += check_value($words[20], 0.50,    0.50  , "coeff sx");
  $ret_code += check_value($words[21], 0.50,    0.50  , "coeff sy");

  return($ret_code);
}


#############################################################################
# PROCEDURE: check_project_coords
# 
# This procedure runs the "project_coords" program on a test input
#   file (which contains positions of M57 and four stars around it).
#   It tries both regular (radians) and arcsec output formats.
#   It compares the results to a set of correct results.
#
# Returns:
#   0           if all goes well
#   > 0         if output doesn't match the expected output
#
sub check_project_coords {
  my($input_file, 
     $radians_answer_file,
     $arcsec_answer_file,
     $output_file,
     $cmd, $ret,
     $ret_code);

  $input_file = $project_a;
  $radians_output_file = $project_b;
  $arcsec_output_file = $project_c;
  $output_file = "$srcdir/project_coords.out";

  $retcode = 0;

  # first, make sure that all the input and output files exist
  if (!(-r $input_file) || !(-r $radians_output_file) ||
      !(-r $arcsec_output_file)) {
    printf STDERR "check_project_coords: can't open input/output files";
    return(1);
  }

  # run the program, producing output in radians
  $cmd = "./project_coords $input_file 0 1 283.39603 33.02769 outfile=$output_file";
  if ($debug > 0) {
    printf "check_project_coords: about to run ..$cmd..\n";
  }
  $ret = `$cmd`;
  # compare the output to the expected output
  $cmd = "diff $output_file $project_b";
  if ($debug > 0) {
    printf "check_project_coords: about to check ..$cmd.. \n";
  }
  $ret = `$cmd`;
  if ($ret != "") {
    printf STDERR "check_project_coords: radians test diff returns ..$ret..\n";
    $ret_code++;
  }
    
  # run the program, producing output in arcsec
  $cmd = "./project_coords $input_file 0 1 283.39603 33.02769 asec outfile=$output_file";
  if ($debug > 0) {
    printf "check_project_coords: about to run ..$cmd..\n";
  }
  $ret = `$cmd`;
  # compare the output to the expected output
  $cmd = "diff $output_file $project_c";
  if ($debug > 0) {
    printf "check_project_coords: about to check ..$cmd.. \n";
  }
  $ret = `$cmd`;
  if ($ret != "") {
    printf STDERR "check_project_coords: arcsec test diff returns ..$ret..\n";
    $ret_code++;
  }

  # if the tests succeeded, remove the output file
  if ($ret_code == 0) {
    unlink($output_file);
  }


  return($ret_code);
}

