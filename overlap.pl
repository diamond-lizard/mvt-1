#!/usr/bin/perl

# Check a CSV file for possible note overlaps and die if there are any

# Usage: pipe a CSV file to it over stdin.

# Background: the MIDI file spec allows two Note-on events to occur (for the
# same pitch on the same channel) without an intervening Note-off.  Bad news 
# for duration-based programs like MVT.

$ind=0;
while ($a = <>) {
    if ($a =~ m/\d+,\s*(\d+)/) {
        $time=$1;
        if ($a =~ m/.*Note_(\w+)_c,\s*(\d+),\s*(\d+),\s*(\d+)/) {
            $which=$1;
            $chan=$2;
            $pitch=$3;
            $vel=$4;
            for($i=0;$i<$ind;$i++) {
                if ($ev[$i] =~ m/(\d+),\s*(\d+),\s*(\d+)/ &&
                    ($chan eq $2) && ($pitch eq $3)) {
                    if (($which eq "on") && ($vel > 0)) {
                        die("Overlap time=$time and $1 for note $pitch");
                    } else {
                        $ev[$i]="";
                        last;
                    }
                }
            }
            if (($which eq "on") && ($vel > 0)) {
                $ev[$ind]="$time, $chan, $pitch";   
                $ind+=1;
            }
        }
    } 
}
print("No overlaps found\n");
