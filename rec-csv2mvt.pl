#!/usr/bin/perl

# This reads a midi-io.scm recorded track in CSV format in /tmp/track.csv
# and writes a track in MVT format to the file /tmp/track.mvt

# Usage:  rec-csv2mvt.pl <numer> <denom> <trackname>
#   where <numer> and <denom> give the time signature

# Set values for conversion of MIDI units to bar:beat:part and track name
$numer=$ARGV[0] || 4;
$denom=$ARGV[1] || 4;
$title=$ARGV[2] || "Record";

$csv_file="/tmp/track.csv";
$mvt_file="/tmp/track.mvt";
$pp_beat=960;
$pp_bar=$pp_beat*$numer;

# go through CSV file replacing Note_off_c and Note_on_c by single Note_c
open (FILE,"<", $csv_file) or die("Cannot open $csv_file");
$ind = 0; %uses=();
while ($a = <FILE>) {
    $a =~ s/^(\d+),\s*(\d+),\s*// || die("Missing track or time in CSV file");
    $track=$1; $clock=$2;
	if ($a=~s/Note_(\w+)_c,\s*(\d+),\s*(\d+),\s*(\d+)//) { # Note xx
        $which=$1; $chan=$2; $pitch=$3; $vel=$4;
        if (($which eq "off") || ($vel eq 0)) {  # Note off 
            for($i=$ind-1;$i>-1;$i--) {
                if (($ev[$i] =~ m/(\d+),\s*Note_c,\s*(\d+),\s*(\d+)/) 
                    && ($2 eq $chan) && ($3 eq $pitch)) {
                    $dur = ($clock-$1);
                    $ev[$i]="$ev[$i], $dur\n";
                    last;
                }
            }
            die("No noteon found for $chan $pitch $vel at $clock") if ($i < 0);
        } else { # Note on
            $ev[$ind]="$clock, Note_c, $chan, $pitch, $vel";
            $ind+=1;
        }
    } else { # anything else passes as is
        $ev[$ind]="$clock, $a";
        $ind+=1;
    }
}
close(FILE);

# for printing nice records
# $pitchstring=" CC# DD# E FF# GG# AA# BB#";
$pitchstring=" CDb DEb E FGb GAb ABb B";    # using flats 
$locfmt="%+3s:%+1s:%-3s "; 

# get the names of all the MIDI controllers
open (FILE,"<", "MIDIControllers.txt")  or die;
while ($a = <FILE>) {
    $a =~ m/^(\d+)\s+(\S+)\n/;
    $ctlname[$1]=$2;
}
close(FILE);

# Convert arg to bar, beat, part according to time signature.
# With optional second argument, use 1-based origin for bar and beat
sub ConvertTime{
    $midiclock=$_[0];
    $bar=int($midiclock/$pp_bar);
    $beat=int(($midiclock%$pp_bar)/$pp_beat);
    $part=int(($midiclock%$pp_bar)%$pp_beat);
    if ($_[1]) {
        $bar+= 1;
        $beat+=1; 
    }
}

# go through ev array printing MVT events to MVT file
open (FILE,">", $mvt_file) or die("Cannot open $mvt_file");
for($i=0;$i<$ind;$i++) {
    if ($ev[$i] =~ m/.*End_track/) {
        # print FILE ( "end-track\n");
    } elsif ($ev[$i] =~ m/.*Start_track/) {
        # print FILE ("start-track \"$title\"\n");
    } else {
        $ev[$i] =~ s/^\s*(\d+),\s*//;
        &ConvertTime($1,1);
        printf FILE ($locfmt, $bar, $beat, $part);
		$ev[$i]="$1$chan$ev[$i]";
        if ($ev[$i] =~ m/Control_c,\s*(\d+),\s*([^ ]+),\s*(\d+)/) {
            $chan=$1+1;  $ctl=$2;  $val=$3;
			if ($ctl eq 64) {
				print FILE ("sustain $chan $val\n");
			} elsif ($ctl eq 7) {
				print FILE ("volume $chan $val\n");
			} elsif ($ctl eq 1) {
				print FILE ("wheel $chan $val\n");
			} elsif ($ctl eq 11) {
				print FILE ("expression $chan $val\n");
			} else {
				$name=$ctlname[$ctl];
				if ($name eq "") {
					$name="MIDI_Control_$ctl";
				}
				print FILE ("control $chan $name $val\n");
			}
        } elsif ($ev[$i] =~ m/Pitch_bend_c,\s*(\d+),\s*(\d+)/) {
            $chan=$1;  $val=$2-8192;
            print FILE ("bend $chan $val\n");
        } elsif ($ev[$i] =~ m/Program_c,\s*(\d+),\s*(\d+)/) {
            $chan=$1+1;  $val=$2;
            print FILE ("program $chan $val\n");
        } elsif ($ev[$i] =~ m/Channel_aftertouch_c,\s*(\d+),\s*(\d+)/) {
            $chan=$1+1;  $val=$2;
            print FILE ("after $chan $val\n");
        } elsif ($ev[$i] =~ m/Note_c,\s*(\d+),\s*(\d+),\s*(\d+),\s*(\d+)/) {
            $chan=$1+1;  $pitch=$2;  $vel=sprintf("%3s",$3);
            &ConvertTime($4);
			if ($bar eq 0) {
				if ($beat eq 0) { 
					$dur="$part";
				} else { $dur="$beat:$part"; }
			} else { $dur="$bar:$beat:$part"; }
            $oct=int($pitch/12)-1;
            $sc=substr($pitchstring,2*($pitch%12),2);
			print FILE ("note $chan $sc$oct $vel $dur\n");
        }  else { die("Error: Track has illegal event: $ev[$i]!"); }
    }   
}
close(FILE);
