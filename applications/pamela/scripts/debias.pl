#!/usr/bin/perl 
#
# !!begin 
# !!title    Debiassing script
# !!author   T.R. Marsh
# !!created  14 January 2001
# !!revised  08 December 2005
# !!root     debias 
# !!index    debias.pl
# !!descr    Perl script for debiassing multiple files
# !!head1    Debiassing script
#
# !!emph{debias} runs 'picstat' followed by 'icsub' to remove the bias
# level from frames. Note that reduce can also do this but !!emph{debias}
# may be useful for preparation of flat fields etc. For safety, it creates
# a header item '.more.debias' which if it exists means that it will not attempt
# to subtract the bias frame a second time, although it will carry on with the
# action of a constant determined from the bias region.
#
# !!head2 Invocation
#
# debias bias_frame bias_region file1 file2 ...
#
# !!head2 Arguments
# 
# !!table
# !!arg{bias_frame}{a mean bias frame to subtract. specify a null frame if
# you don't want to do this}
# !!arg{bias_region}{is a file specifying the bias region(s) for picstat.}
# !!arg{file1, file2}{are individual files.}
# !!table
#
# !!end

use strict;

(@ARGV > 2) or die "usage: bias_frame bias_region file1 file2 ..\n";

my $bias_frame  = shift or die "No bias frame supplied.\n";
my $bias_region = shift or die "No bias region file supplied.\n";

-e "$bias_frame.sdf" or die "Failed to find $bias_frame.sdf\n";
-e $bias_region or die "Failed to find $bias_region.\n";
-f $bias_region or die "$bias_region is not a plain file.\n";
-r $bias_region or die "$bias_region not readable.\n";

# Strip off trailing .sdf, check that files exist, 
# are not directories, and are readable

my $file;
foreach $file (@ARGV){
    $file =~ s/\.sdf$//;
    -e "${file}.sdf" or die "${file}.sdf does not exist!\n";
    -f "${file}.sdf" or die "${file}.sdf is not a plain file!\n";
    -r "${file}.sdf" or die "${file}.sfd is not readable!\n";
    -w "${file}.sdf" or die "${file}.sdf is not writeable!\n";
}

# OK down to business

print "Generating debiassing script zzz_debias ...\n\n";
open(DEBIAS, ">zzz_debias") || die "Can't open zzz_debias\n";

print DEBIAS "#!/bin/csh\n\n";
print DEBIAS "if ( \$?ADAM_USER ) then\n";
print DEBIAS "   echo \"ADAM_USER: \$ADAM_USER\"\n";
print DEBIAS " else\n";
print DEBIAS "   setenv ADAM_USER ~/adam\n";
print DEBIAS "endif\n\n";

print DEBIAS "source \$STARLINK_DIR/etc/login; source \$STARLINK_DIR/etc/cshrc\n";
print DEBIAS "figaro\n";
print DEBIAS "pamela\n\n";
print DEBIAS "set verbose\n\n";
    
foreach $file (@ARGV){
    $file =~ s/\.sdf//;

    print DEBIAS "echo \" \"\n";

    print DEBIAS "echo \"Processing file = $file\"\n\n";
    
    print DEBIAS "set test = \`hdstrace $file.more | awk \'/DEBIAS/{print \$1}\'\`\n";
    
# subtraction of constant frame
    
    print DEBIAS "if ( \"\$test\" != \"DEBIAS\" ) then\n";
    
    print DEBIAS "   creobj type=_LOGICAL dims=0 object=$file.MORE.DEBIAS\n";
    
    print DEBIAS "   isub image=$file image1=$bias_frame output=$file\n\n";
    
    print DEBIAS "else\n";
    
    print DEBIAS "   echo \"$file has already had a bias frame subtracted\"\n\n";
    
    print DEBIAS "endif\n\n";
    
# subtraction of constant
    
    print DEBIAS "picstat image=$file streg=$bias_region clip=3 plot=no\n\n";
    
    print DEBIAS "icsub image=$file factor=@\$ADAM_USER/GLOBAL.PIC_MEAN output=zzz_junk\n\n";
    
    print DEBIAS "\\mv -v zzz_junk.sdf $file.sdf\n\n";
}

print DEBIAS "exit\n\n";

close(DEBIAS);

print "Debiassing ...\n\n";

system("chmod +x zzz_debias; ./zzz_debias >& zzz_debias.log");

print "Finished debiassing. Script: zzz_debias, output: zzz_debias.log.\n\n";

exit;





