#!/usr/bin/perl
#  Edit the next three lines before running the program
$mvtbindir = "/full/path/name";
$mvtdatadir = "/Users/hector/Music/MIDIviaTEXT";
$mvttmpdir = "/full/path/name";

# Usage:  cat myfile.csv | csv2mvt.pl  (that is: pipe a CSV file)

$comment_overlaps=1;     # put comments in the file if overlaps are found
$goal_pp_beat=960;       # desired number of units per beat

$ind = 0; %uses=();
while ($a = <>) {
    if ($a =~ m/^\s*[\#\;]/) { # comments
        $ev[$ind]=$a;
        $ind+=1;
        next;
    }
    $a =~ s/(\d+),\s*(\d+),\s*// || die("Missing track or time in CSV file");
    $track=$1; $clock=$2;
    if ($a =~ m/Header,\s*(\d+),\s*(\d+),\s*(\d+)/) {
        $format=$1; $ntracks=$2; $ppqn=$3;
        $ev[$ind]="$clock, Start_file, ";  # the rest below
        $ind+=1;
    } elsif ($a =~ m/Time_signature,\s*(\d+),\s*(\d+)/) {    # TimeSig
        ($clock eq 0) || die ("No Time Signature changes allowed.");
        $numer=$1;  $denom=2**$2;
    } elsif ($a =~ m/Title_t,\s*"([^"]*)"/) {  # Title
        ($clock eq 0) || die ("Titles only at start of track.");
        $song_title=$1 if ($song_title eq "");
        $title[$track]="$1$uses{$1}";
        $uses{$1}+=1;
    } elsif ($a=~s/Note_(\w+)_c,\s*(\d+),\s*(\d+),\s*(\d+)//) { # Note xx
        $which=$1; $chan=$2; $pitch=$3; $vel=$4;
        if (($which eq "off") || ($vel eq 0)) {  # Note off 
            for($i=$ind-1;$i>-1;$i--) {
                if (($ev[$i] =~ m/(\d+),\s*Note_c,\s*(\d+),\s*(\d+)/) 
                    && ($2 eq $chan) && ($3 eq $pitch)) {
                    $dur = ($clock-$1);
                    if ($ev[$i] =~ m/\d+,\s*\d+,\s*\d+,\s*\d+/) {
                        $ev[$i] = "# Overlap on track $track: $ev[$i]" 
                            if ($comment_overlaps);  
                        next;
                    }
                    $ev[$i]="$ev[$i], $dur\n";
                    last;
                }
            }
            die("No matching noteon found for $chan $pitch $vel at $clock")
                if ($i < 0);
        } else { # Note on
            $ev[$ind]="$clock, Note_c, $chan, $pitch, $vel";
            $ind+=1;
        }
    } else { # anything else passes as is
        $ev[$ind]="$clock, $a";
        $ind+=1;
    }
}

# for printing nice records
# $pitchstring=" CC# DD# E FF# GG# AA# B";    # using sharps
$pitchstring=" CDb DEb E FGb GAb ABb B";    # using flats
$tempofmt="tempo %.2f\n";
$locfmt="%+3s:%+1s:%-3s ";  # this ones uses the colon notation

# get the names of all the MIDI controllers
open (FILE,"<", "$mvtdatadir/MIDIControllers.txt")  or die;
while ($a = <FILE>) {
    $a =~ m/^(\d+)\s+(\S+)\n/;
    $ctlname[$1]=$2;
}
close(FILE);

# Set values for conversion of MIDI units to bar:beat:part
$numer=4 if ($numer eq "");
$denom=4 if ($denom eq "");
$pp_beat=int(4*$ppqn/$denom);
$pp_bar=$pp_beat*$numer;
$pp_scale=$goal_pp_beat/$pp_beat;

# Convert arg to bar, beat, part according to ppqn and timesig
# With optional second argument, use 1-based origin for bat and beat
sub ConvertTime{
    $midiclock=$_[0];
    $bar=int($midiclock/$pp_bar);
    $beat=int(($midiclock%$pp_bar)/$pp_beat);
    $part=int((($midiclock%$pp_bar)%$pp_beat)*$pp_scale);
    if ($_[1]) {
        $bar+= 1;
        $beat+=1; 
    }
}

$track=0;
for($i=0;$i<$ind;$i++) {
    if ($ev[$i] =~ s/(^\s*)[\#\;]//) {
        print("$1;$ev[$i]");
    } elsif ($ev[$i] =~ m/.*End_track/) {
        print("end-track\n");
    } elsif ($ev[$i] =~ m/.*End_of_file/) {
        print("end-file\n");
    } elsif ($ev[$i] =~ m/Start_file/) {
        print("start-file $numer $denom\n");
    } elsif ($ev[$i] =~ m/(\d+),\s*Start_track/) {
        $clock=$1; $track+=1;
        $tit = $title[$track];
        $tit ="Track$track" if ($tit eq "");
        print("start-track \"$tit\"\n");
    } else {
        if ($ev[$i] =~ s/(.*_c,\s*)(\d+)//) {
            $chan=$2+1;
            $ev[$i]="$1$chan$ev[$i]";
        }
        $ev[$i] =~ s/\s*(\d+),\s*//;
        $clock=$1;
        &ConvertTime($clock,1);
        printf($locfmt, $bar, $beat, $part);
        if ($ev[$i] =~ m/Tempo,\s*(\d+)/) {
            $tempo=60000000/$1;
            printf($tempofmt, $tempo);
        } elsif ($ev[$i] =~ m/Control_c,\s*(\d+),\s*([^ ]+),\s*(\d+)/) {
            $chan=$1;  $ctl=$2;  $val=$3;
			if ($ctl eq 64) {
				print("sustain $chan $val\n");
			} elsif ($ctl eq 7) {
				print("volume $chan $val\n");
			} elsif ($ctl eq 1) {
				print("wheel $chan $val\n");
			} elsif ($ctl eq 11) {
				print("expression $chan $val\n");
			} else {
				$name=$ctlname[$ctl];
				if ($name eq "") {
					$name="MIDI_Control_$ctl";
				}
				print("control $chan $name $val\n");
			}
        } elsif ($ev[$i] =~ m/Pitch_bend_c,\s*(\d+),\s*(\d+)/) {
            $chan=$1;  $val=$2-8192;
            print("bend $chan $val\n");
        } elsif ($ev[$i] =~ m/Program_c,\s*(\d+),\s*(\d+)/) {
            $chan=$1;  $val=$2;
            print("program $chan $val\n");
        } elsif ($ev[$i] =~ m/Channel_aftertouch_c,\s*(\d+),\s*(\d+)/) {
            $chan=$1;  $val=$2;
            print("after $chan $val\n");
        } elsif ($ev[$i] =~ m/Note_c,\s*(\d+),\s*(\d+),\s*(\d+),\s*(\d+)/) {
            $chan=$1;  $pitch=$2;  $vel=sprintf("%3s",$3);
            &ConvertTime($4);
			if ($bar eq 0) {
				if ($beat eq 0) {
					$dur="$part";
				} else {
					$dur="$beat:$part";
				}
			} else {
				$dur="$bar:$beat:$part";
			}
            $oct=int($pitch/12)-1;
            $sc=substr($pitchstring,2*($pitch%12),2);
			print("note $chan $sc$oct $vel $dur\n");
        }  else {
			$ev[$i] =~ s/,//g;
			$ev[$i] =~ s/\n//g;
            print("meta $ev[$i]\n");
        }
    }   
}
