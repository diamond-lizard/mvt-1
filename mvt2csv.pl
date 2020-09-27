#!/usr/bin/perl
#  Edit the next three lines before running the program
$mvtbindir = "/full/path/name";
$mvtdatadir = "/Users/hector/Music/MIDIviaTEXT";
$mvttmpdir = "/full/path/name";

# Usage:  cat myfile.mvt | mvt2csv.pl [ bar ]  (that is: pipe an MVT file)
# The optional argument is the first bar (except for time=0 events)

$start_bar = $ARGV[0];
$start_bar=1 if ($start_bar < 1);
$pp_beat = 960;

$offer = "Note_off_c";     # Use Note-off events (vs. Note-on with velocity 0)


# find out how many tracks and if solo_mode
$ntracks=0;  $ind=0;  $solo_mode=0;
while ($a = <STDIN>) {
    $ev[$ind]=$a;
    $ind += 1;
    if ($a =~ m/start-track/) {
        $ntracks += 1;
        $solo_mode=1 if ($a =~ m/solo/);
    }
}

%pitchval= ("C", 0, "D", 2, "E", 4, "F", 5, "G", 7, "A", 9, "B", 11);
%ctlval= ("volume", 7, "wheel", 1, "sustain", 64, "expression", 11);

# Convert bar:beat:part pos to miditime according to current ppqn and timesig
# With optional second argument, return value for 1-based origin.
sub ConvertPosition{
    $base = $_[0];
    return ($bar-$base)*$pp_bar + ($beat-$base)*$pp_beat +  $part;
}

# get the names of all the MIDI controllers
%ctlname=();
open (FILE,"<", "$mvtdatadir/MIDIControllers.txt")  or die;
while ($a = <FILE>) {
        $a =~ m/(\d+)\s+(.*)\n/;
        $ctlname{$2}=$1;
}
close(FILE);

# go through the entire file, emitting CVS lines
$track=0; $clock=0; %noff =();
for($i=0;$i<$ind;$i++) {
    $a=$ev[$i];
    if ($a =~ s/(^\s*)[\#\;]//) {
        print("$1#$a");
    } elsif ($a =~ m:start-file\s*(\d+)\s*(\d+):) {
        $numer=$1; $denom=$2; 
        $ppqn= int($denom*$pp_beat/4);
        $pp_bar=$pp_beat*$numer;
        print("0, 0, Header, 1, $ntracks, $ppqn\n");
    } elsif ($a =~ m/end-file/) {
        print("0, 0, End_of_file\n");
    } elsif ($a =~ m/end-track/) {
        foreach $key (sort { $a <=> $b} keys %noff) {
            print($noff{$key});
            $clock=$key;
        }
        %noff=();
        printf("$track, %d, End_track\n",$clock);
    } elsif ($a =~ m/start-track\s+("[^"]*")/) {
        $title=$1; $track+=1;
        print("$track, 0, Start_track\n$track, 0, Title_t, $title\n");
        if ($track eq 1) {
            $power2 = log($denom)/log(2);
            print("1, 0, Time_signature, $numer, $power2, 24, 8\n");
        }
        $voice = (($a =~ m/mute/) || 
                  ($solo_mode && (!($a =~ m/solo/)))) ? 0 : 1;
    } else {
        next if (!$voice);
        $a =~ s/\s*(\d+):(\d+):(\d+)\s+//;
        $bar=$1; $beat=$2; $part=$3;
        next if (!(($bar eq 1) && ($beat eq 1) && ($part eq 0)) 
                 && ($bar < $start_bar));
        $bar = 1+$bar-$start_bar;
        $bar=1 if ($bar<1);
        $clock=&ConvertPosition(1);
        foreach $key (sort { $a <=> $b} keys %noff) {
            if ($key <= $clock) {
                print($noff{$key});
                delete $noff{$key};
            }
        }
        print("$track, $clock, ");

        if ($a =~ m/tempo\s+([.0123456789]+)/) {
            $tempo=int(.5+60000000/$1);
            print("Tempo, $tempo\n");
		} elsif ($a =~ s/^meta\s+//) {
			$b="";
			while ($a =~ s/^(\w+)\s+|^("[^"]*")\s+//) { $b="$b$1$2, "; }
			$a =~ s/\n//;
			print("$b$a\n");
		} else {
			$a =~ s/(\w*)\s+(\d+)\s*//;
			$type=$1; $chan=$2-1;   # fix channel
			if (($type eq "volume") || ($type eq "wheel") ||
				($type eq "expression") || ($type eq "sustain")) {
				$a =~ m/(\d+)/;
				$val=$1;
				$num=$ctlval{$type};
				print("Control_c, $chan, $num, $val\n");
			} elsif ($type eq "control") {
				$a =~ m/([^ ]+)\s+(\d+)/;
				$ctl=$1; $val=$2;
				if ($ctl =~ m/MIDI_Control_(\d+)/) {
					$num=$1;
				} else {
					$num=$ctlname{$ctl};
				}
				print("Control_c, $chan, $num, $val\n");
			} elsif ($type eq "bend") {
				$a =~ m/(\d+)/;
				$val=$1+8192;   # fix pitch bend
				print("Pitch_bend_c, $chan, $val\n");
			} elsif ($type eq "after") {
				$a =~ m/(\d+)/;
				$val=$1;
				print("Channel_aftertouch_c, $chan, $val\n");
			} elsif ($type eq "program") {
				$a =~ m/(\d+)/;
				$val=$1;
				print("Program_c, $chan, $val\n");
			} else {  # $type eq "note"
				$a =~ m/^([^ ]+)\s+(\d+)\s+(.+)/;
				$name=$1;  $vel=$2; $bbp=$3;
				# get pitch
				$acc = ($name =~ s/#//) ? 1 : 0;
				$acc -= ($name =~ s/b//) ? 1 : 0;
				$name =~ m/(\w)([-0123456789]+)/;
				$pt=($2+1)*12+$pitchval{$1}+$acc;
				print("Note_on_c, $chan, $pt, $vel\n");
				# get duration
				if ($bbp =~ m/(\d+):(\d+):(\d+)/) {
					$bar=$1; $beat=$2; $part=$3;
				} elsif ($bbp =~ m/(\d+):(\d+)/) {
					$bar=0; $beat=$1; $part=$2;
				} else {
					$bbp =~ m/(\d+)/;
					$bar=0; $beat=0; $part=$1;
				}
				$off=$clock+&ConvertPosition(0);
				$noff{$off}="$track, $off, $offer, $chan, $pt, 0\n$noff{$off}";
			}
		}
	}
}
