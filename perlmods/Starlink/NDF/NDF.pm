package NDF;
require 5.002;

use strict;
use Carp;
use vars qw($VERSION @ISA @EXPORT $AUTOLOAD %EXPORT_TAGS);


#grep "dat_.*(" NDF.xs | grep -v ';' | awk -F'(' '{print $1}' | grep -v '_r$' | sort | fmt -50
 
require Exporter;
require DynaLoader;
require AutoLoader;
 
@ISA = qw(Exporter DynaLoader); 

# Version 1.2
'$Revision$ ' =~ /.*:\s(.*)\s\$/ && ($VERSION = "$1");

# Add the following to the 'ndf'=> associative array if you want to
# use ADAM PARAMETERS. Remove the comment field from the entries in the XS
# file:
# ndf_assoc ndf_cinp ndf_creat ndf_crep ndf_exist ndf_prop
# You will also need to link against ndf_link_adam

%EXPORT_TAGS = ( 
		'ndf'=>[qw/
			ndf_acget ndf_aclen ndf_acmsg ndf_acput ndf_acre
			ndf_aform ndf_amap ndf_annul ndf_anorm ndf_arest
			ndf_asnrm ndf_astat ndf_astyp ndf_atype
			ndf_aunmp ndf_bad ndf_base ndf_bb ndf_begin
			ndf_block ndf_bound ndf_cget ndf_chunk 
			ndf_clen ndf_clone ndf_cmplx ndf_cmsg ndf_copy
			ndf_cput ndf_delet ndf_dim
			ndf_end ndf_find ndf_form ndf_ftype
			ndf_gtune ndf_happn ndf_hcre ndf_hdef ndf_hend
			ndf_hfind ndf_hinfo ndf_hnrec ndf_hout ndf_hpurg
			ndf_hput ndf_hsmod ndf_isacc ndf_isbas ndf_istmp
			ndf_loc ndf_map ndf_mapql ndf_mapz ndf_mbad
			ndf_mbadn ndf_mbnd ndf_mbndn ndf_msg ndf_mtype
			ndf_mtypn ndf_nbloc ndf_nchnk ndf_new ndf_newp
			ndf_noacc ndf_open ndf_place ndf_qmask ndf_qmf
			ndf_reset ndf_same ndf_sbad ndf_sbb ndf_sbnd
			ndf_scopy ndf_sect ndf_shift ndf_size ndf_sqmf
			ndf_ssary ndf_state ndf_stype ndf_temp ndf_tune
			ndf_type ndf_unmap ndf_valid ndf_xdel ndf_xgt0c
			ndf_xgt0d ndf_xgt0i ndf_xgt0l ndf_xgt0r ndf_xiary
			ndf_xloc ndf_xname ndf_xnew ndf_xnumb ndf_xpt0c
			ndf_xpt0d ndf_xpt0i ndf_xpt0l ndf_xpt0r ndf_xstat
			/],

		'ary'=>[qw/ary_annul ary_dim ary_find ary_map ary_ndim
			 ary_size ary_unmap/],

		'msg'=>[qw/msg_bell msg_blank msg_fmtc msg_fmtd msg_fmti
			msg_fmtl msg_fmtr msg_iflev msg_ifset msg_load
			msg_out msg_outif msg_renew msg_setc msg_setd
			msg_seti msg_setl msg_setr/],

		'err'=>[qw/err_annul err_begin err_end err_facer err_fioer
			err_flbel err_flush err_level err_load err_mark
			err_rep err_rlse err_stat err_syser/],

		'hds'=>[qw/hds_copy hds_erase hds_ewild hds_flush hds_free
			hds_group hds_gtune hds_link hds_lock hds_new
			hds_open hds_show hds_state hds_stop hds_trace
			hds_tune hds_wild
			/],

		'dat'=>[qw/dat_alter dat_annul dat_basic dat_ccopy dat_cctyp
			dat_cell dat_clen dat_clone dat_coerc dat_copy
			dat_drep dat_erase dat_ermsg dat_find dat_get0c
			dat_get0d dat_get0i dat_get0l dat_get0r dat_get1c
			dat_get1d dat_get1i dat_get1r dat_getvc dat_getvd
			dat_getvi dat_getvr dat_index dat_len dat_map
			dat_mapc dat_mapd dat_mapi dat_mapl dat_mapr
			dat_mapv dat_mould dat_move dat_msg dat_name
			dat_ncomp dat_new dat_new0c dat_new0d dat_new0i
			dat_new0l dat_new0r dat_new1c dat_new1d dat_new1i
			dat_new1l dat_new1r dat_newc dat_paren dat_prec
			dat_prim dat_prmry dat_put0c dat_put0d dat_put0d
			dat_put0l dat_put0r dat_put1d dat_put1i dat_put1r
			dat_put1c dat_putc dat_putd dat_puti dat_putr dat_putvc
			dat_putvd dat_putvi
			dat_putvr dat_ref dat_refct dat_renam dat_reset
			dat_retyp dat_shape dat_size dat_slice dat_state
			dat_struc dat_temp dat_there dat_type dat_unmap
			dat_valid dat_vec dat_where
			/],

		'cmp'=>[qw/ cmp_get0c cmp_get0d cmp_get0i cmp_get0l cmp_get0r
			cmp_get1c cmp_get1d cmp_get1i cmp_get1r cmp_getvc
			cmp_getvd cmp_getvi cmp_getvr cmp_len cmp_mapv
			cmp_mod cmp_modc cmp_prim cmp_put0c cmp_put0d
			cmp_put0i cmp_put0l cmp_put0r cmp_put1c cmp_put1d 
			cmp_put1i cmp_put1r 
			cmp_putni cmp_putvc cmp_putvd cmp_putvi cmp_putvr
			cmp_shape cmp_size cmp_struc cmp_type cmp_unmap
			/],

		'misc'=>[qw/mem2string string2mem array2mem mem2array
			 par_get
			 fits_get_nth_item fits_get_item fits_extract_key_val
			 fits_construct_string/]
	       );

Exporter::export_tags('ndf','ary','msg','err','hds','dat','cmp','misc');

# Autoload constants when required

sub AUTOLOAD {
    # This AUTOLOAD is used to 'autoload' constants from the constant()
    # XS function.  If a constant is not found then control is passed
    # to the AUTOLOAD in AutoLoader.
 
    my $constname;
    ($constname = $AUTOLOAD) =~ s/.*:://;
    my $val = constant($constname, @_ ? $_[0] : 0);
    if ($! != 0) {
        if ($! =~ /Invalid/) {
            $AutoLoader::AUTOLOAD = $AUTOLOAD;
            goto &AutoLoader::AUTOLOAD;
        }
        else {
                croak "Your vendor has not defined NDF macro $constname";
        }
    }
    eval "sub $AUTOLOAD { $val }";
    goto &$AUTOLOAD;
}

bootstrap NDF $VERSION;

# Character arrays need to be packed before being passed to C
# The int and float arrays are dealt with automatically by KGB's arrays.c

sub pack1Dchar {
  my ($arg) = shift;
  return $arg if ref(\$arg) eq "SCALAR"; # packed char string
  my $array_len = $#{$arg} + 1; # How many members in array
  my ($maxlength, $size);
  # Find maximum length of an array member
  $maxlength = 0;
  foreach (@$arg) {
    $size = length($_);
    $maxlength = $size if $size > $maxlength; 
  }

  return ($maxlength, pack("A$maxlength" x $array_len, @$arg))
    if ref(\$arg) eq "GLOB" ||
      (ref($arg) eq "ARRAY" && ref(\$$arg[0]) eq "SCALAR");
  croak "Something has gone wrong";
}
  


# Now deal with the calls which use character arrays

sub ndf_hput ($$$$\@$$$$$) {
  croak 'Usage: ndf_hput(hmode, appn, repl, nlines, @text, trans, wrap, rjust, indf, status)' if (scalar(@_)!=10);
  my($hmode, $appn, $repl, $nlines, $text, $trans, $wrap, $rjust,
     $indf, $status) = @_;
  $nlines = ($nlines > $#{$text} + 1 ? $#{$text} + 1 : $nlines);
  ndf_hput_r($hmode, $appn, $repl, $nlines, pack1Dchar($text), $trans, 
	     $wrap, $rjust, $indf, $status);
  $_[9] = $status;
}

sub dat_putc ($$\@\@$) {
  croak 'Usage: dat_putc(loc, ndim, @dim, @value, status)' if (scalar(@_)!=5);
  my ($loc, $ndim, $dim, $value, $status) = @_;
  dat_putc_r($loc, $ndim, $dim, pack1Dchar($value), $status);
  $_[4] = $status;
}

sub dat_put1c ($$\@$) {
  croak 'Usage: dat_put1c(loc, el, @value, status)' if (scalar(@_)!=4);
  my ($loc, $el, $value, $status) = @_;
  dat_put1c_r($loc, $el, pack1Dchar($value), $status);
  $_[3] = $status;
}

sub dat_putvc ($$\@$) {
  croak 'Usage: dat_putvc(loc, el, @value, status)' if (scalar(@_)!=4);
  my ($loc, $el, $value, $status) = @_;
  dat_putvc_r($loc, $el, pack1Dchar($value), $status);
  $_[3] = $status;
}

sub cmp_put1c ($$$\@$) {
  croak 'Usage: cmp_putvc(loc, name, el, @value, status)' if (scalar(@_)!=5);
  my ($loc, $name, $el, $value, $status) = @_;
  cmp_put1c_r($loc, $name, $el, pack1Dchar($value), $status);
  $_[4] = $status;
}

sub cmp_putvc ($$$\@$) {
  croak 'Usage: cmp_putvc(loc, name, el, @value, status)' if (scalar(@_)!=5);
  my ($loc, $name, $el, $value, $status) = @_;
  cmp_putvc_r($loc, $name, $el, pack1Dchar($value), $status);
  $_[4] = $status;
}

# Add routines which deal with bytes

# NDF_QMASK(QUAL, BADBIT)

	      
sub ndf_qmask ($$) {
  croak 'Usage: ndf_qmask(qual, badbit)' if (scalar(@_)!=2);
  return(1) if (($_[0] & $_[1]) == 0);
  return(0);
}


# Add routines to simplify array mapping

# Map an array to a memory pointer
#   INPUT: ARRAY, Pack type, memory address of mapped array
#   To map arrays need to
#       1.  Pack the array into a string
#       2.  Find out byte size of int, float etc
#       3.  Copy this string to the memory location with string2mem

sub array2mem  (\@$$) {
  my ($ref, $packtype, $pntr) = @_;

  string2mem(pack($packtype, @$ref), &byte_size($packtype) * ($#{$ref}+1), 
	     $pntr);
}

# Need to know the size of the array to map it from the pointer to perl
#  INPUT: pointer to mapped memory, pack type, array, size of array

sub mem2array ($$$) {
  my ($pntr, $packtype, $nel) = @_;
  my $mapped;

  mem2string($pntr, &byte_size($packtype) * $nel, $mapped);
  return unpack($packtype, $mapped);

}



# Now put in some general PERL sub routines

#+
#  Name:
#    par_get

#  Purpose:
#    To find the values of parameters stored in ~/adam

#  Type of module:
#    Perl 5 script

#  Description:
#    This procedure is a perl5 implementation of Kappa PARGET.
#    The current value of a parameter associated with a Starlink application
#    is returned.
#

#  Arguments:
#    PAR (Given)
#      The required parameter
#    FILE (Given)
#      The requested application

#  Example:
#     pget  nsigma drawsig
#        Returns the value of the NSIGMA parameter (an array)

#  Prior requirements:
#    - The NDF perl module must exist
#-

sub par_get ($$$) {

  croak 'Usage: par_get(param, application, status)' if (scalar(@_)!=3);
  my  ($parname, $pkg, $status, $path, $filename, $tempstat, $prim,
	 $loc, $there, $loco, $type, $locs, @cvalues, $el, $size);

  ($parname, $pkg, $status) = @_;

  # Just in case a reference isn't passed
  if (!ref($status)) {
    print "Warning! STATUS will not be returned unless you pass the reference\n";
    $tempstat = $status;
    $status = \$tempstat;

  } elsif (ref($status) ne "SCALAR") {
    print "ref($status) is not a valid type for STATUS\n";
    return;
  }

  return if $$status != &NDF::SAI__OK;

  # Find the location of the SDF files

  $path = $ENV{'ADAM_USER'};

  if ($path !~ /./) {
    $path = $ENV{'HOME'};
    if ($path !~ /./) {
      $$status = &NDF::SAI__ERROR;
      return;
    }
  }

  $filename = "$path/adam/$pkg";

  # Open the file
  hds_open($filename, 'READ', $loc, $$status);
  return if $$status != &NDF::SAI__OK;

  # Find the object
  dat_there($loc, $parname, $there, $$status);

  if (! $there) {
    $$status = &NDF::SAI__ERROR;
    msg_setc('PAR', $parname);
    msg_setc('PATH', $filename);
    err_rep("PARGET_NOOBJ", "There is no parameter ^PAR in file ^PATH", $$status);
    return;
  }

  # Obtain a locator to the desired value from the primitive object

  dat_find($loc, $parname, $loco, $$status);
  
  # Find out if the object is primitive
  dat_prim($loco, $prim, $$status);

  if (! $prim) {
    dat_annul($loco, $$status);
    dat_find($loc, $parname, $locs, $$status);
    dat_type($locs, $type, $$status);

    if ($type ne "ADAM_PARNAME") {
      $$status = &NDF::SAI__ERROR;
      msg_setc('PAR', $parname);
      msg_setc('PATH', $filename);
      err_rep('PARGET_NOOBJ', 'Object ^PAR in file ^PATH is an arbitrary structure.', $$status);
      dat_annul($locs, $$status);
      dat_annul($loc, $$status);
      return;

    } else {

      dat_find($locs, 'NAMEPTR', $loco, $$status);

    }
    dat_annul($locs, $$status);
  }

  # Find the number of elements associated with the object

  dat_size($loco, $size, $$status);

  # Get the values as a character array
  dat_getvc($loco, $size, \@cvalues, $el, $$status); # Need to pass reference
                                                     # when in module
  # Finish off
  dat_annul($loco, $$status);
  dat_annul($loc, $$status);

  return (@cvalues);

}

#-----------------------------------------------------------------------

######### F I T S #########

 
# Routine to extract nth FITS value and keyword from FITS array
#
#  Input:   Array reference to FITS array
#           Number of FITS item in array

#  Return:  Keyword and value

sub fits_get_nth_item (\@$) {

  my ($keyword, $value, $comment);
  my ($fitsref, $n) = @_;

  # Split up keyword and value

  ($keyword, $value, $comment) = fits_extract_key_val($$fitsref[$n]);

  # Now return keyword and value
  return ($keyword, $value, $comment);

}

# Routine to find FITS value corresponding to given keyword
#
# INPUT:  Array reference to FITS array
#         Keyword

# OUTPUT: Value

sub fits_get_item (\@$) {

  my ($fitsref, $keyword) = @_;
  my ($name, $value, $nfits, $comment);
  my (@results) = ();

  # Look at each member of the FITS array
  (@results) = grep(/^$keyword/i,@$fitsref);

  ($#results > -1) && do {

    ($name, $value, $comment) = fits_extract_key_val($results[0]);

  } || ($value = "NOT FOUND");

  return($value);
}

# Routine to extract a value and keyword from a FITS-like string
 
sub fits_extract_key_val ($) {

  my $fits_entry = shift;
  my ($keyword, $value, $comment);

  $keyword = $value = $comment = undef;
  # Extract the value
  ($keyword, $value, $comment) = split(/=|\s\//,$fits_entry);

  # Tidy up the keyword
  $keyword =~ s/\s//g;    # Remove white space

  # Tidy up value
  $value =~ s/'//g;    # Remove quotes(') from strings;
  $value =~ s/^\s+//;  # Remove leading whitespace
  $value =~ s/\s+$//;  # Remove trailing whitespace

  $value = "NOT FOUND" unless ($value =~ /./);
  $keyword = "NONE" unless ($keyword =~ /./);

  return($keyword, $value, $comment);
}

# Routine to construct a FITS-like string from a keyword, value and comment
# Packed string is returned

sub fits_construct_string ($$$) {
  my ($keyword, $value, $comment) = @_;

  my ($fitsent);

  $fitsent = substr($keyword,0,8); # Key must be <= 8 characters
  $fitsent .= (' 'x(8-length($fitsent)))."= ";

  # Check whether we have a number or character string
  if ($value =~ /^(-?)(\d*)(\.?)(\d*)([Ee][-\+]?\d+)?$/) {
    # Number (chop to 67 characters)
    $value = substr($value,0,67);
    $value = (' 'x(20-length($value))).$value;

  } else {
    # Character (chop to 65 characters)
    $value = substr($value,0, 65);
    $value = "'$value'";
    $value = $value.(' 'x(20-length($value)));
  }

  # Add comment
  $fitsent .= $value.' / '.$comment;

  # Fix at 80 characters
  $fitsent = substr($fitsent,0,80);
  $fitsent .= ' 'x(80-length($fitsent));

  return $fitsent;
}
 
 
1;
__END__
# This is the documentation

=head1 NAME

NDF - Perl extension for accessing Starlink N-dimensional data structures
(NDFs)

=head1 SYNOPSIS

  use NDF;
  use NDF qw(:ndf :dat :ary :hds :cmp :misc :err :msg);

=head1 DESCRIPTION

This module gives access to the Starlink extensible N-dimensional data format (NDF) and related error and message handling routines. Basic hierarchical
data structure (HDS) access routines are also supplied.

The first form of calling the module imports all the function calls
into the main:: namespace. The different sub-packages are:

=over 4

=item :ndf

This imports all the NDF_ functions for use.

=item :msg

This imports all the MSG_ functions.

=item :err

This imports all the Starlink error handling functions (ERR_)

=item :ary

This imports some ARY_ functions.

=item :hds

This imports the HDS_ functions

=item :dat

This imports most of the  DAT_ functions

=item :cmp

This imports most of the CMP_ functions

=item :misc

This imports routines for converting DATA pointers to arrays and routines for 
easy access to FITS arrays commonly found in NDFs. A routine for accessing
parameters stored by ADAM tasks is also available.

=back


Demo programs are available in the C<t> directory of the installation
tree.

=head2 Access to compiler constants

Compiler constants can be accessed via NDF::(constant) and are autoloaded.
e.g. SAI__OK can be accessed as &NDF::SAI__OK, as in:

C<
  S<ndf_find(&NDF::DAT__ROOT, $filename, $access, $status);
  print "An error occurred\n" if ($status != &NDF::SAI__OK);>
>

At present all the DAT__, ERR__, EMS__, MSG__ and SAI__ constants are 
available. More can be included as more C include files are created.
In the absence of the include files a request for a specific compiler
constant will be refused.

=head2 Dealing with MAPPED arrays

Routines such as ndf_map and dat_mapv return pointers to the data
arrays. These pointers are handled differently in perl than in Fortran.

To read a mapped array you must first I<copy> the data from the mapped
array into a perl array. Modifiying this perl array I<will not> change
the mapped array. The perl array must be copied back into the mapped 
array for any changes to become permanent. To reduce memory requirements
you may want to unmap the array as soon as it has been copied to the 
perl array.

In perl the array must first be packed into a single string and this
string must then be copied to the mapped memory location.
For example:

C<
  $mapped_string = pack("i*", @data);
  ndf_map($indf, 'DATA', '_CHARACTER*20', 'WRITE', $pntr, $el, $status);
  string2mem($mapped_string, 4 * $el, $pntr);
>

This maps a perl data array to the NDF DATA_ARRAY. If $el does not equal
$#data + 1 then odd things will happen. 

The reverse process can be achieved using mem2string as follows:

C<
  S<ndf_map($indf, 'DATA', '_INTEGER', 'READ', $pntr, $el, $status);>
  mem2string($pntr, 4*$el, $mapped_string);
  @data = unpack("i*", $mapped_string);
>

Note that both mem2string and string2mem require the I<number of bytes>
to be mapped. The data then has to be unpacked into an array by the
perl unpack command.

Character arrays can be dealt with as follows:

C<
  $mapped_string = pack("A20" x ($#data +1), @data);
  ndf_map($indf, 'DATA', '_CHARACTER*20', 'WRITE', $pntr, $el, $status);
  string2mem($mapped_string, 20*($#data +1), $pntr);
>

This packs an array of perl strings into a fixed length (space
padded) string of length 20 x (size of array) characters. The string is
then copied to the mapped array with string2mem.

Dealing with arrays is made easier by the array2mem and mem2array
routines, which handle the packing for the programmer. At present these
routines do not work with character arrays and the lower level
string2mem and mem2string routines must still be used.

To copy from a mapped array to a perl array you use mem2array. The
routine requires the pointer to the mapped array, the pack type (as
used in the the perl C<pack> command, i.e. "i*" for ints, "f*" for
floats) and the number of elements, and returns the array.

For example:
C<
  ndf_map($indf, 'DATA', '_INTEGER', 'READ', $pntr, $el, $status);
  @data = mem2array($pntr, "i*", $el);
>

To write to a mapped array, you use array2mem:

C<
  S<ndf_map($indf, 'DATA', '_INTEGER', 'WRITE', $pntr, $el, $status);>
  array2mem(@data, "i*", $pntr);
>

Remember that, at present, these routines I<do not> work with
_CHARACTER arrays. In that case you must use the mem2string and
string2mem routines directly.



=head2 Accessing FITS entries

FITS entries are stored in the FITS extension of an NDF. The FITS array
must be accessed by using raw HDS commands as follows:

C<
  ndf_xloc($indf, 'FITS', 'READ', $floc, $status);
  dat_get1c($floc, 200, @fits, $nfits, $status);
>

(The 200 indicates the maximum size of the array - this can be any
value greater than or equal to the actual size of the array).  The FITS
header information is now in the perl @fits array and can be accessed
via fits_get_item and fits_get_nth_item.

For example, to list all the values and keywords:


   for ($i=0; $i <= $#fits; $i++) {
    ($keyword, $value, $comment) = fits_get_nth_item(@fits, $i);
    print "$i: $keyword\t $value\n";
   }

You can also use fits_extract_key_val to access the nth entry via

C<
  ($keyword, $value, $comment) = fits_extract_key_val($fits[$n]);
>

To find out the value that goes with a keyword:

C<
  $inst = fits_get_item(@fits, 'INSTRUME');
  print "Instrument is $inst\n";
>

Remember that perl has associative arrays, such that


  %fitsitem = ();
  for ($i=0; $i <= $#fits; $i++) {
    ($keyword, $value, $comment) = fits_get_nth_item(@fits, $i);
    $fitsitem{$keyword} = $value;
  }

So that C<'print "$fitsitem{'INSTRUME'}\n";'> will print SCUBA.


=head1 NOTES

=head2 Function calls

The commands in this module use  perl prototypes to check argument 
passing. This means that arrays do not have to be referenced when being
used as arguments I<unless> the routine is called with a &.

For example, both

   $value = fits_get_item(@fits, $keyword);
and
   $value = &fits_get_item(\@fits, $keyword);

are correct but 

   $value = &fits_get_item(@fits, $keyword);

will fail because perl will pass all the members of the array and not the
reference to the array. 

=head2 Arrays

Note that the GetNx/PutNx routines have not been implemented in this
module because plain Perl does not really support N-dimensional
arrays.  All data in plain perl is vectorised.  If you wish to use
N-dimensional data structures in perl you must use the PDL module in
conjunction with a vectorised read. (see eg. the rndf command in
perldl which stores all arrays as true N-dimensional data sets). The
PDL module is available from CPAN. Also note that the Get1x/Put1x
routines are implemented but are not necessary since the vectorised
routines can always be used instead.

=head2 Accessing TYPEd data

Although routines are available for accessing data of any type (eg
int, real, double) the untyped nature of perl and the type conversion
inherent in the NDF routines mean that, in practice, only character
handling routines are needed to access data (eg via ndf_xgt0c or
dat_get1c). This does not apply to mapped data arrays where the size
of the data type must be known before unpacking. The double precision
routines may be necessary since the type conversion routines truncate
to REAL precision when converting to CHAR.


=head2 Printing History information

Whilst all the NDF history calls are implemented, NDF_HOUT has only a
minimal functionality since NDF_HOUT requires the name of a Fortran
subroutine.  Calling a perl subroutine from Fortran has not been
implemented and NDF_HECHO is called by default.

=head2 Accessing ADAM parameters from A-tasks

ADAM A-task store their parameters in HDS files in the ADAM_USER
directory (usually ~/adam). This routine retrieves the value of 
a parameter for any ADAM task that uses the ADAM_USER directory.

For example

      ($in) = par_get("data_array","GLOBAL", \$status);

returns the current data set (stored in GLOBAL.sdf). Note that the 
data is returned in an array context. The routine returns without
action if $status is not set to SAI__OK on entry.


=head1 Implemented routines

The following routines are available from this module:

=over 6

=item :ndf

All ndf_ routines are implemented except those dealing with ADAM parameters
(eg ndf_assoc). 

=item :ary

ary_annul ary_dim ary_find ary_map ary_ndim ary_size ary_unmap

=item :msg

All msg_ routines are implemented:
msg_bell msg_blank msg_fmtc msg_fmtd msg_fmti msg_fmtl msg_fmtr
msg_iflev msg_ifset msg_load msg_out msg_outif msg_renew msg_setc
msg_setd msg_seti msg_setl msg_setr

=item :err

All err_ routines are implemented:
err_annul err_begin err_end err_facer err_fioer err_flbel err_flush
err_level err_load err_mark err_rep err_rlse err_stat err_syser

=item :hds

All hds_ routines are implemented:
hds_copy hds_erase hds_ewild hds_flush hds_free hds_group hds_gtune
hds_link hds_lock hds_new hds_open hds_show hds_state hds_stop
hds_trace hds_tune hds_wild

=item :dat

dat_alter dat_annul dat_basic dat_ccopy dat_cctyp dat_cell dat_clen
dat_clone dat_coerc dat_copy dat_drep dat_erase dat_ermsg dat_find
dat_get0c dat_get0d dat_get0i dat_get0l dat_get0r dat_get1c dat_get1d
dat_get1i dat_get1r dat_getvc dat_getvd dat_getvi dat_getvr dat_index
dat_len dat_map dat_mapc dat_mapd dat_mapi dat_mapl dat_mapr dat_mapv
dat_mould dat_move dat_msg dat_name dat_ncomp dat_new dat_new0c
dat_new0d dat_new0i dat_new0l dat_new0r dat_new1c dat_new1d dat_new1i
dat_new1l dat_new1r dat_newc dat_paren dat_prec dat_prim dat_prmry
dat_put0c dat_put0d dat_put0d dat_put0l dat_put0r dat_put1c dat_put1d
dat_put1i dat_put1r dat_putd dat_puti dat_putr dat_putvc dat_putvd
dat_putvi dat_putvr dat_ref dat_refct dat_renam dat_reset dat_retyp
dat_shape dat_size dat_slice dat_state dat_struc dat_temp dat_there
dat_type dat_unmap dat_valid dat_vec dat_where


=item :cmp

cmp_get0c cmp_get0d cmp_get0i cmp_get0l cmp_get0r cmp_get1c cmp_get1d
cmp_get1i cmp_get1r cmp_getvc cmp_getvd cmp_getvi cmp_getvr cmp_len
cmp_mapv cmp_mod cmp_modc cmp_prim cmp_put0c cmp_put0d cmp_put0i
cmp_put0l cmp_put0r cmp_put1c cmp_put1d cmp_put1i cmp_put1r cmp_putni
cmp_putvc cmp_putvd cmp_putvi cmp_putvr cmp_shape cmp_size cmp_struc
cmp_type cmp_unmap

=item :misc

mem2string string2mem array2mem mem2array fits_get_nth_item
fits_get_item fits_extract_key_val fits_construct_string par_get


=back

=head1 AUTHOR

Module created by T. Jenness, timj@jach.hawaii.edu
(with help from F. Economou, frossie@jach.hawaii.edu)

=head1 STARLINK

The NDF library is by Rodney Warren-Smith and is part of the Starlink
Software Collection (Starlink Project, Rutherford Appleton Lab).
For more information on Starlink go to http://star-www.rl.ac.uk.
See Starlink User Note 33 for details on using the NDF library routines.


=head1 SEE ALSO

L<perl(1)> for an introduction to Perl,
L<perlfunc(1))> for a description of the pack and unpack commands,
L<perlxs(1)> for details of how this module accesses external libraries.

=cut
