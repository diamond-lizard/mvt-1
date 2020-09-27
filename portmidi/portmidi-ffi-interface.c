/* This defines a PortMidi dynamic library with these procedures:     */
/*   StartMidi, StopMidi, ListDevices, MidiMessageOut, SetReroute,    */
/*   OpenMidiIn, CloseMidiIn, OpenMidiOut, CloseMidiOut               */
/* with very minimal error checking                                   */

#include "stdio.h" 
#include "portmidi.h"
#include "porttime.h"

PmStream *midi_in;       /* the MIDI in stream */
PmStream *midi_out;      /* the MIDI out stream */
PmError err;             /* for errors in opening devices */
int midi_in_open = 0;    /* non-0 indicates port is open */
int midi_out_open = 0;   /* non-0 indicates port is open */
int reroute = 0;         /* non-0 indicates rerouting of MIDI in */

void (*MidiReader)(int time, int x, int y, int z);  /* callback for MIDI in */

/* This procedure is called regularly for MIDI input */
void ReceivePoll(PtTimestamp timestamp, void *userData) {
  PmEvent event;
  int count;
  if (!midi_in_open) return;
  while ((count = Pm_Read(midi_in, &event, 1))) {
    if (count == 1) {
      /* possibly reroute the channel of the input */
      if (reroute) event.message = (event.message & 0xF0FFFF) | (reroute-1);
      /* possibly echo the input to the output */
      if (midi_out_open) Pm_WriteShort(midi_out, 0, event.message);
      /* call the callback procedure with the input data */
      MidiReader(timestamp, Pm_MessageStatus(event.message),
         Pm_MessageData1(event.message), Pm_MessageData2(event.message));
    } else fprintf(stderr,"MIDI Read error: %s\n",Pm_GetErrorText(count));
  }
}

/* This allows MIDI in messages to be rerouted to another MIDI channel */
void SetReroute(int x) {
  reroute = x;
}

/* Initiate MIDI.  Argument is procedure to callback for MIDI input */
void StartMidi(void (*readfn)(int time, int x, int y, int z)){
  MidiReader = readfn;
  Pt_Start(1,ReceivePoll,0);
  Pm_Initialize();
}

/* Terminate midi */
void StopMidi() {
  Pt_Stop();
  Pm_Terminate();
}

/* Close the output device */
void CloseMidiOut() {
  Pm_Close(midi_out);
  midi_out_open = 0;
}

/* Close the input device */
void CloseMidiIn() {
  Pm_Close(midi_in);
  midi_in_open = 0;
}

/* Open the Nth portmidi output device (first = 0)*/
void OpenMidiOut(int n) {
  int i;
  for (i = 0; i < Pm_CountDevices(); i++) {
    const PmDeviceInfo *info = Pm_GetDeviceInfo(i);
    if (info->output) {
      if (n == 0) {
        break;
      } else { n -= 1; }
    }
  }
  if (n > 0) fprintf(stderr,"Open MIDI out: Not enough output devices\n");
  if (err=Pm_OpenOutput(&midi_out, i, NULL, 512, NULL, NULL,1)) {
    fprintf(stderr,"Open MIDI out: %s\n",Pm_GetErrorText(err));
  } else midi_out_open = 1;
}

/* Open the Nth portmidi input device (first = 0)*/
void OpenMidiIn(int n) {
  int i;
  for (i = 0; i < Pm_CountDevices(); i++) {
    const PmDeviceInfo *info = Pm_GetDeviceInfo(i);
    if (info->input) {
      if (n == 0) {
        break;
      } else { n -= 1; }
    }
  }
  if (n > 0) fprintf(stderr,"Open MIDI in: Not enough input devices\n");
  if (err=Pm_OpenInput(&midi_in, i, NULL, 512, NULL, NULL)) {
    fprintf(stderr,"Open MIDI in: %s\n",Pm_GetErrorText(err));
  } else midi_in_open = 1;
}

/* Send a midi message out via the output device */
void MidiMessageOut(int x, int y, int z) {
  Pm_WriteShort(midi_out, 0, Pm_Message(x, y, z));
}


/* Print MIDI device info to standard out */
void ListDevices() {
  int i,j;
  printf("The INPUT devices seen by PortMidi:\n");                            
  j=0;                                                                        
  for (i = 0; i < Pm_CountDevices(); i++) {                                   
    const PmDeviceInfo *info = Pm_GetDeviceInfo(i);                           
    if (info->input) {                                                        
      printf("  %d: %s, %s \n", j, info->interf, info->name);                 
      j += 1;                                                                 
    }                                                                         
  }                                                                           
  printf("The OUTPUT devices seen by PortMidi:\n");                           
  j=0;                                                                        
  for (i = 0; i < Pm_CountDevices(); i++) {                                   
    const PmDeviceInfo *info = Pm_GetDeviceInfo(i);                           
    if (info->output) {                                                       
      printf("  %d: %s, %s \n", j, info->interf, info->name);                 
      j += 1;                                                                 
    }
  }
}
