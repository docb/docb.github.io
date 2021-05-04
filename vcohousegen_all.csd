<CsoundSynthesizer>

<CsOptions>
-odac -dm0
</CsOptions>

<CsInstruments>
sr        = 48000
ksmps     = 32
nchnls    = 2
0dbfs = 1

;#include "master.orc"
opcode master,0,aakkk
  ainL,ainR,kamp,krev,kdel xin
  aL = ainL*kamp
  aR = ainR*kamp
  adL = aL*kdel
  adR = aR*kdel
  arL = aL*krev
  arR = aR*krev
  chnmix adL,"delInL"
  chnmix adR,"delInR"
  chnmix arL,"revInL"
  chnmix arR,"revInR"
  chnmix aL,"mixL"
  chnmix aR,"mixR"
endop

;; Channel Helper from Steven YI's livecode.orc

/** Sets i-rate value into channel and sets initialization to true. Works together
  with xchan */
opcode xchnset, 0, Si
  SchanName, ival xin
  Sinit = sprintf("%s_initialized", SchanName)
  chnset(1, Sinit)
  chnset(ival, SchanName)
endop

/** xchan
  Initializes a channel with initial value if channel has default value of 0 and
  then returns the current value from the channel. Useful in live coding to define
  a dynamic point that will be automated or set outside of the instrument that is
  using the channel.

  Opcode is overloaded to return i- or k- value. Be sure to use xchan:i or xchan:k
  to specify which value to use.
*/
opcode xchan, i,Si
  SchanName, initVal xin

  Sinit = sprintf("%s_initialized", SchanName)
  if(chnget:i(Sinit) == 0) then
    chnset(1, Sinit)
    chnset(initVal, SchanName)
  endif
  xout chnget:i(SchanName)
endop

/** xchan
  Initializes a channel with initial value if channel has default value of 0 and
  then returns the current value from the channel. Useful in live coding to define
  a dynamic point that will be automated or set outside of the instrument that is
  using the channel.

  Opcode is overloaded to return i- or k- value. Be sure to use xchan:i or xchan:k
  to specify which value to use.
*/
opcode xchan, k,Si
  SchanName, initVal xin

  Sinit = sprintf("%s_initialized", SchanName)
  if(chnget:i(SchanName) == 0) then
    chnset(1, Sinit)
    chnset(initVal, SchanName)
  endif
  xout chnget:k(SchanName)
endop


;based on Kickblast by Micah Frank under GPLv3
;https://github.com/chronopolis5k/Kickblast

gitab1 ftgen 0,0,129,10,1 ;sine
gitab2 ftgen 0,0,129,10,1,0,1,0,1,0,1,0,1 ;odd partials
gitab3 ftgen 0,0,16384,10,1,0,.33,0,.2,0,.14,0 ,.11,0,.09 ;odd harmonics
gitab4 ftgen 0,0,16384,10,0,.2,0,.4,0,.6,0,.8,0,1,0,.8,0,.6,0,.4,0,.2 ; saw
gitab5 ftgen 0,0,129,21,1 ;white noise
gitab7 ftgen 0,0,129,9,.5,1,0 ;half sine
gitab10 ftgen 0, 0, 2^10, 10, 1, 0, -1/9, 0, 1/25, 0, -1/49, 0, 1/81

instr drumskick
  ifreq = p4
  iamp = p5
  isusfrq = 59
  isusres = 0.07
  isusidx = 0
  isusinit = 2.749
  isusdecay = 2
  isusgain = 1.58

  iattfrq = 59
  iattidx  = 0
  iattdur = 23.45
  iattlvl = 0.17
  iattinit = 2250
  iattdur = iattdur/1000
  ikickSusArray[] fillarray gitab1, gitab7, gitab10
  isuswave = ikickSusArray[isusidx]

  ikickAtkArray[] fillarray gitab2, gitab3, gitab4, gitab5
  iattwave = ikickAtkArray[iattidx]
  ;print isusinit, iattdur,isusdecay,iattlvl,iattdur,isusfrq,iattfrq,iattinit,isusres
  kpenv expseg isusinit, iattdur, 1, isusdecay, 0.01  ;modulate pitch.

  kamp expseg 0.9, isusdecay, 0.001

;;kick attack
  katkenv expseg iattlvl, iattdur, 0.01 ;attack envelope

  asus oscili kamp, isusfrq*kpenv, isuswave
  aatk oscili katkenv, iattfrq, iattwave

  kfiltenv expseg iattinit, isusdecay*0.25, 20

  afilt moogvcf2 asus+aatk, kfiltenv, isusres
  a1 = afilt*isusgain
  a1 limit a1, -0.9, 0.9 ;limiter
  a1 dcblock2 a1
  abass vco2,0.2,ifreq
  abass butterlp abass,150
  aenvb madsr 0.001,0.2,0.1,0.1

  aenv1 madsr 0.001,0,1,0.01
  aout = (a1*aenv1+abass*aenvb)*4
  master aout,aout,iamp,0,0
endin

gisinesn ftgen 0,0,1024,10,1 ;A SINE WAVE
/** Snare Drum - From Iain McCurdy's TR-808.csd */
instr snare
  
  iamp = p5
  idecay = 0.48
  itune = -1.2
  kpan = 0.5
  ifrq    = 342   ;FREQUENCY OF THE TONES
  iNseDur = 0.3 * idecay ;DURATION OF THE NOISE COMPONENT
  iPchDur = 0.1 * idecay ;DURATION OF THE SINE TONES COMPONENT

  ;SINE TONES COMPONENT
  aenv1 expseg  1,iPchDur,0.0001,iNseDur-iPchDur,0.0001    ;AMPLITUDE ENVELOPE
  apitch1 oscili  1,ifrq*octave(itune),gisinesn     ;SINE TONE 1
  apitch2 oscili  0.25,ifrq*0.5*octave(itune),gisinesn   ;SINE TONE 2 (AN OCTAVE LOWER)
  apitch  = (apitch1+apitch2)*0.75        ;MIX THE TWO SINE TONES

  ;NOISE COMPONENT
  aenv2 expon 1,iNseDur,0.0005         ;AMPLITUDE ENVELOPE
  anoise  noise 0.75,0            ;CREATE SOME NOISE
  anoise  butbp anoise,10000*octave(itune),10000    ;BANDPASS FILTER THE NOISE SIGNAL
  anoise  buthp anoise,1000         ;HIGHPASS FILTER THE NOISE SIGNAL
  kcf expseg  5000,0.1,3000,iNseDur-0.2,3000     ;CUTOFF FREQUENCY FOR A LOWPASS FILTER
  anoise  butlp anoise,kcf          ;LOWPASS FILTER THE NOISE SIGNAL
  amix  = (apitch*aenv1+anoise*aenv2)*4 ;MIX AUDIO SIGNALS AND SCALE ACCORDING TO GUI 'Level'
  if p4 > 0 then
    amix moogvcf2 amix,p4,0.1
  endif
  aL,aR pan2  amix,kpan
  aenv init 1
  master aL,aR,iamp,0.2,0
endin
/** Closed HH - From Iain McCurdy's TR-808.csd */
instr cl
  iamp = p5
  idecay = 0.28
  itune = -0.1
  kpan = 0.5
  kFrq1 = 296*octave(itune)   ;FREQUENCIES OF THE 6 OSCILLATORS
  kFrq2 = 285*octave(itune)
  kFrq3 = 365*octave(itune)
  kFrq4 = 348*octave(itune)
  kFrq5 = 420*octave(itune)
  kFrq6 = 835*octave(itune)
  idur  = 0.088*idecay   ;DURATION OF THE NOTE
  idur  limit idur,0.1,10   ;LIMIT THE MINIMUM DURATION OF THE NOTE (VERY SHORT NOTES CAN RESULT IN THE INDICATOR LIGHT ON-OFF NOTE BEING TO0 SHORT)

;  iactive active  p1-1      ;SENSE ACTIVITY OF PREVIOUS INSTRUMENT (OPEN HIGH HAT)
;  if iactive>0 then     ;IF 'OPEN HIGH HAT' IS ACTIVE...
;   turnoff2 p1-1,0,0    ;TURN IT OFF (CLOSED HIGH HAT TAKES PRESIDENCE)
;   chnset gkoff,"Act3"        ;TURN OFF ACTIVE LIGHT FOR OPEN HIGH HAT
;  endif

  ;PITCHED ELEMENT
  aenv  expsega 1,idur,0.001,1,0.001    ;AMPLITUDE ENVELOPE FOR THE PULSE OSCILLATORS
  ipw = 0.25        ;PULSE WIDTH
  a1  vco2  0.5,kFrq1,2,ipw     ;PULSE OSCILLATORS...
  a2  vco2  0.5,kFrq2,2,ipw
  a3  vco2  0.5,kFrq3,2,ipw
  a4  vco2  0.5,kFrq4,2,ipw
  a5  vco2  0.5,kFrq5,2,ipw
  a6  vco2  0.5,kFrq6,2,ipw
  amix  sum a1,a2,a3,a4,a5,a6   ;MIX THE PULSE OSCILLATORS
  amix  reson amix,5000*octave(itune),5000,1  ;BANDPASS FILTER THE MIXTURE
  amix  buthp amix,5000     ;HIGHPASS FILTER THE SOUND...
  amix  buthp amix,5000     ;...AND AGAIN
  amix  = amix*aenv     ;APPLY THE AMPLITUDE ENVELOPE

  ;NOISE ELEMENT
  anoise  noise 0.8,0       ;GENERATE SOME WHITE NOISE
  aenv  expsega 1,idur,0.001,1,0.001    ;CREATE AN AMPLITUDE ENVELOPE
  kcf expseg  20000,0.7,9000,idur-0.1,9000  ;CREATE A CUTOFF FREQ. ENVELOPE
  anoise  butlp anoise,kcf      ;LOWPASS FILTER THE NOISE SIGNAL
  anoise  buthp anoise,8000     ;HIGHPASS FILTER THE NOISE SIGNAL
  anoise  = anoise*aenv     ;APPLY THE AMPLITUDE ENVELOPE

  ;MIX PULSE OSCILLATOR AND NOISE COMPONENTS
  amix  = (amix+anoise)*6
  aL,aR pan2  amix,kpan
  aenv init 1
  master aL,aR,iamp,0.1,0
endin

/** MRCA - From Iain McCurdy's TR-808.csd */
instr mrca
  iamp = p5
  idecay = 0.92
  itune = 1.5
  kpan = 0.5
  idur  = 0.07*idecay   ;DURATION 3
  idur  limit idur,0.1,10
  iHPF  limit 6000*octave(itune),20,sr/2  ;HIGHPASS FILTER FREQUENCY
  iLPF  limit 12000*octave(itune),20,sr/3
  ;AMPLITUDE ENVELOPE
  iBP1  = 0.4     ;BREAK-POINT 1
  iDur1 = 0.014*idecay    ;DURATION 1
  iBP2  = 1     ;BREAKPOINT 2
  iDur2 = 0.01 *idecay    ;DURATION 2
  iBP3  = 0.05      ;BREAKPOINT 3
  idur  limit idur,0.1,10
  aenv  expsega iBP1,iDur1,iBP2,iDur2,iBP3  ;CREATE AMPLITUDE ENVELOPE
  anoise  noise 0.75,0      ;CREATE A NOISE SIGNAL
  anoise  buthp anoise,iHPF     ;HIGHPASS FILTER THE SOUND
  anoise  butlp anoise,iLPF     ;LOWPASS FILTER THE SOUND
  anoise  = (anoise*aenv)*6  ;SCALE THE AMPLITUDE
  aL,aR pan2  anoise,kpan  ;PAN THE MONOPONIC SIGNAL
  aenv init 1
  master aL,aR,iamp,0.1,0
endin


giratio0 ftgen 0,0,32,-2,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32
giratio1 ftgen 0,0,32,-2,1,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50,52,54,56,58,60,64
giratio2 ftgen 0,0,32,-2,1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55,57,59,61,63
giratio3 ftgen 0,0,32,-2,1,2,3,4,21/4,6,7,8,8,9,21/2,12,49/4,14,16,16,18,21,24,49/2,27,28,63/2,32,32,36,42,48,49,56,63,64
giratio4 ftgen 0,0,32,-2,1,9/8,5/4,21/16,3/2,7/4,2,9/4,5/2,21/8,3,7/2,4,9/2,5,21/4,6,7,8,9,10,21/2,12,14,16,18,20,21,24,28,32,36
giratio5 ftgen 0,0,32,-2,1,9/8,5/4,11/8,3/2,15/8,2,9/4,5/2,11/4,3,15/4,4,9/2,5,11/2,6,15/2,8,9,10,11,12,15,16,18,20,22,24,30,32,36
giratio6 ftgen 0,0,32,-2,1,10/9,7/6,21/16,3/2,7/4,2,20/9,7/3,21/8,3,7/2,4,40/9,14/3,21/4,6,7,8,80/9,28/3,21/2,12,14,16,160/9,56/3,21,24,28,32,320/9
giratio7 ftgen 0,0,32,-2,1,9/8,7/6,21/16,3/2,14/9,7/4,2,9/4,7/3,21/8,3,28/9,7/2,4,9/2,14/3,21/4,6,56/9,7,8,9,28/3,21/2,12,112/9,14,16,18,56/3,21
giratio8 ftgen 0,0,32,-2,1,21/20,14/11,27/20,40/27,11/7,16/9,2,21/10,28/11,27/10,80/27,22/7,32/9,4,21/5,56/11,27/5,160/27,44/7,64/9,8,42/5,112/11,54/5,320/27,88/7,128/9,16,84/5,224/11,104/5
giratio[] fillarray giratio0,giratio1,giratio2,giratio3,giratio4,giratio5,giratio6,giratio7,giratio8

giwave0 ftgen 0,0,2^16,10,1
giwave1 ftgen 0,0,2^16,10,1,0.2,0,0.1
giwave2 ftgen 0,0,2^16,10,1,0.5,1/3,1/4,1/5,1/6,1/7,1/8,1/9,1/10,1/11
giwave3 ftgen 0,0,2^16,10,1,0,1/3,0,1/5,0,1/7,0,1/9,0,1/11
giwave4 ftgen 0,0,2^16,10,1,0.3,0.4,0.2,0,0.05,0,0.1
giwave5 ftgen 0,0,2^16,10,1,1/2,0,1/4,0,1/6,0,1/8,0,1/10
giwave[] fillarray giwave0,giwave1,giwave2,giwave3,giwave4,giwave5

gidists[] fillarray 8,9,10
opcode gensnap,i,iiiiii
   ilen,icube,id,ip1,ip2,iseed xin
   idist = gidists[id]
   iparam = ip1
   if id == 2 then
     iparam += 0.15
   endif
   seed iseed
   itablen = ilen*icube*icube*icube
   print itablen,idist,iparam,ip2,iseed
   itab ftgentmp 0,0,itablen,21,idist,1,iparam,ip2
   ;ftprint itab,1,0,itablen,1,ilen
   xout itab
endop

opcode vcosynt,a,iiiiiki
  iamp,ifreq,iratio,iamps,imode,kpw,isyn xin
  asig[] init 32
  kindx = 0
  asig[kindx] vco2 table(kindx,iamps), ifreq*table(kindx,iratio),imode,kpw,0,isyn
  kindx += 1
  asig[kindx] vco2 table(kindx,iamps), ifreq*table(kindx,iratio),imode,kpw,0,isyn
  kindx += 1
  asig[kindx] vco2 table(kindx,iamps), ifreq*table(kindx,iratio),imode,kpw,0,isyn
  kindx += 1
  asig[kindx] vco2 table(kindx,iamps), ifreq*table(kindx,iratio),imode,kpw,0,isyn
  kindx += 1
  asig[kindx] vco2 table(kindx,iamps), ifreq*table(kindx,iratio),imode,kpw,0,isyn
  kindx += 1
  asig[kindx] vco2 table(kindx,iamps), ifreq*table(kindx,iratio),imode,kpw,0,isyn
  kindx += 1
  asig[kindx] vco2 table(kindx,iamps), ifreq*table(kindx,iratio),imode,kpw,0,isyn
  kindx += 1
  asig[kindx] vco2 table(kindx,iamps), ifreq*table(kindx,iratio),imode,kpw,0,isyn
  kindx += 1
  asig[kindx] vco2 table(kindx,iamps), ifreq*table(kindx,iratio),imode,kpw,0,isyn
  kindx += 1
  asig[kindx] vco2 table(kindx,iamps), ifreq*table(kindx,iratio),imode,kpw,0,isyn
  kindx += 1
  asig[kindx] vco2 table(kindx,iamps), ifreq*table(kindx,iratio),imode,kpw,0,isyn
  kindx += 1
  asig[kindx] vco2 table(kindx,iamps), ifreq*table(kindx,iratio),imode,kpw,0,isyn
  kindx += 1
  asig[kindx] vco2 table(kindx,iamps), ifreq*table(kindx,iratio),imode,kpw,0,isyn
  kindx += 1
  asig[kindx] vco2 table(kindx,iamps), ifreq*table(kindx,iratio),imode,kpw,0,isyn
  kindx += 1
  asig[kindx] vco2 table(kindx,iamps), ifreq*table(kindx,iratio),imode,kpw,0,isyn
  kindx += 1
  asig[kindx] vco2 table(kindx,iamps), ifreq*table(kindx,iratio),imode,kpw,0,isyn
  kindx += 1
  asig[kindx] vco2 table(kindx,iamps), ifreq*table(kindx,iratio),imode,kpw,0,isyn
  kindx += 1
  asig[kindx] vco2 table(kindx,iamps), ifreq*table(kindx,iratio),imode,kpw,0,isyn
  kindx += 1
  asig[kindx] vco2 table(kindx,iamps), ifreq*table(kindx,iratio),imode,kpw,0,isyn
  kindx += 1
  asig[kindx] vco2 table(kindx,iamps), ifreq*table(kindx,iratio),imode,kpw,0,isyn
  kindx += 1
  asig[kindx] vco2 table(kindx,iamps), ifreq*table(kindx,iratio),imode,kpw,0,isyn
  kindx += 1
  asig[kindx] vco2 table(kindx,iamps), ifreq*table(kindx,iratio),imode,kpw,0,isyn
  kindx += 1
  asig[kindx] vco2 table(kindx,iamps), ifreq*table(kindx,iratio),imode,kpw,0,isyn
  kindx += 1
  asig[kindx] vco2 table(kindx,iamps), ifreq*table(kindx,iratio),imode,kpw,0,isyn
  kindx += 1
  asig[kindx] vco2 table(kindx,iamps), ifreq*table(kindx,iratio),imode,kpw,0,isyn
  kindx += 1
  asig[kindx] vco2 table(kindx,iamps), ifreq*table(kindx,iratio),imode,kpw,0,isyn
  kindx += 1
  asig[kindx] vco2 table(kindx,iamps), ifreq*table(kindx,iratio),imode,kpw,0,isyn
  kindx += 1
  asig[kindx] vco2 table(kindx,iamps), ifreq*table(kindx,iratio),imode,kpw,0,isyn
  kindx += 1
  asig[kindx] vco2 table(kindx,iamps), ifreq*table(kindx,iratio),imode,kpw,0,isyn
  kindx += 1
  asig[kindx] vco2 table(kindx,iamps), ifreq*table(kindx,iratio),imode,kpw,0,isyn
  kindx += 1
  asig[kindx] vco2 table(kindx,iamps), ifreq*table(kindx,iratio),imode,kpw,0,isyn
  kindx += 1
  asig[kindx] vco2 table(kindx,iamps), ifreq*table(kindx,iratio),imode,kpw,0,isyn
  kindx += 1
  xout iamp*sumarray(asig)
endop

gipdf ftgen 0,0,65536,-42,-1,-0.4,0.5,0.4,1,0.5
opcode yvals,k[],k[]i
  karr[],ifreq xin
  ktrig metro ifreq
  if ktrig == 1 then
  ks[] init 4
  kindx = 0
  while kindx < 4 do
    ks[kindx] duserrnd gipdf 
    karr[kindx*2] = ks[kindx]<0?random:k(-ks[kindx],1):random:k(0,1-ks[kindx])
    karr[kindx*2+1] = karr[kindx*2]+ks[kindx]
    kindx = kindx + 1
  od
  ;printarray ks
  ;printarray karr
  printks "%f %f %f %f\n",0,ks[0],ks[1],ks[2],ks[3]
  printks "%f %f %f %f %f %f %f %f\n",0,karr[0],karr[1],karr[2],karr[3],karr[4],karr[5],karr[6],karr[7]
  endif
  xout karr
endop

givcomode[] fillarray 0,2,4
chnset 5000,"hvsfilt"
chnset 0.6,"hvsamp"
chnset 0.6,"hvsrev"
chnset 0.1,"hvsdel"
instr hvs
  ibasefrq = p4
  ipathseed = p6
  iseed = p7
  if p8 == -1 then
    seed 0
    iw random 0,8.999
  else
    iw = p8
  endif
  if p9 == -1 then
    irt random 0,8.999
  else 
    irt = p9
  endif
 imode = -1 
  if iw > 5 then
     imode = givcomode[iw - 6]
  else
     iwave = giwave[iw]
  endif 

  iamp = p5
  if irt < 3 then
     iamp /=2
     ibasefrq/=2
  endif
  isize = 7

  iratio =  giratio[irt]
  ilen = 32
  if iseed == 0 then
     seed(0)
     iseed random 1000,100000000
  endif
  if ipathseed == 0 then
     seed(0)
     ipathseed random 0,1
  endif
  print iseed,ipathseed,irt,iw,iamp,ibasefrq

  karr[] init 8
  karr yvals karr,0.03125
  imult = 2
  if p11 = 2 then
    imult = 4
  endif
  isnap gensnap 32,isize,p11,p12,50,iseed
  ;isnap gensnap 32,isize,4,0.05,50,iseed
  ipos  ftgen 0,0,1025,-7,0,1025,1024
  iamps  ftgen 0,0,ilen,-2, 0    ; THE AMPLITUDES FOR ADSYNTH
  iamps2  ftgen 0,0,ilen,-2, 0    ; THE AMPLITUDES FOR ADSYNTH

  idivs  ftgen 0,0,ilen,7,1,ilen,0.01
  kl1=randh(0.5,0.0625,ipathseed,1)/2+0.75
  kl2=randh(0.4,0.0625,ipathseed,1)/2+0.2
  kx loopseg 4,0,0,kl1,0.5,kl1-0.1,0,kl2,0.5,kl2+0.1,0
  if p10 == 0 then
    ky loopseg 0.25,0,0,0,0.25,0.6,0,0.27,0.25,0.8,0,0.4,0.25,1,0,0.17,0.25,0.6,0
  elseif p10 == 1 then
    ky loopseg 0.25,0,0,0.6,0.25,0,0,0.8,0.25,0.27,0,1,0.25,0.4,0,0.6,0.25,0.17,0
  else
    ky loopseg 0.25,0,0,karr[0],0.25,karr[1],0,karr[2],0.25,karr[3],0,karr[4],0.25,karr[5],0,karr[6],0.25,karr[7],0
  endif
  kz0 randh 1,0.125,ipathseed,1
  kz scale kz0,1,0,1,-1
  ;printks "%f %f %f\n",0.125,kx,ky,kz
  hvs3 kx,ky,kz,ilen,isize,isize,isize,iamps,ipos,isnap
  vmultv iamps, idivs, ilen 
  hvs3 ky,kx,kz,ilen,isize,isize,isize,iamps2,ipos,isnap
  vmultv iamps2, idivs, ilen
  
  if imode >= 0 then
    kpw randh 0.3,1,ipathseed,1
    aoscL vcosynt 0.08, ibasefrq, iratio, iamps,imode,0.5+kpw,0.5
    aoscR vcosynt 0.08, ibasefrq, iratio, iamps2,imode,0.5+kpw,0.5
  else
    aoscL adsynt  0.1, ibasefrq, iwave, iratio, iamps, ilen, 0
    aoscR adsynt  0.1, ibasefrq, iwave, iratio, iamps2, ilen, 0
  endif
  kfilt chnget "hvsfilt"
  aoscL moogvcf aoscL,kfilt,0.1
  aoscR moogvcf aoscR,kfilt,0.1
  aoscL = tanh(aoscL * iamp) 
  aoscR = tanh(aoscR * iamp)
  kamp chnget "hvsamp"
  krev chnget "hvsrev"
  kdel chnget "hvsdel"
  master aoscL,aoscR,kamp*imult,krev,kdel
endin

instr trigDrums
  kmetro metro 8
  kpos init 0
  if kmetro == 1 then
     event "i","drums",0,1/8,kpos
     kpos += 1
  endif
endin

/* like Steven Yi's hexbeat - the decimals are converted to velocities */
opcode decbeat, i, Si
  Spat, itick xin
  istrlen = strlen(Spat)
  iout = 0
  if (istrlen > 0) then
    ipatlen = strlen(Spat)
    itick = itick % ipatlen
    ipatidx = int(itick)
    ibeatPat = strtol(strsub(Spat, ipatidx, ipatidx + 1)) 
    iout=ibeatPat/10
  endif
  xout iout
endop

opcode decplay, 0, SiSiik
  Spat, itick, Sinstr, idur, ifreq, iamp xin
  ivel = decbeat(Spat, itick)
  if(ivel>0) then
    schedule(Sinstr, 0, idur, ifreq, iamp*ivel )
  endif
endop

instr drums
  ipos = p4
  idr xchan "drums",0.5
  decplay("0000900000009000000090000500900000009000000090000000900000039025",ipos, "snare", p3, 4000, idr*ampdbfs(-6))
  decplay("0090",ipos, "mrca", p3, 0, idr)
  decplay("90009000900090009000900090009004900090009000900090009000900090999000900090009000900090009000900390009000900090009000900090090090",ipos, "drumskick", p3, 55, idr*ampdbfs(-6))
endin

instr trigOff
  idr = nstrnum("trigDrums")
  turnoff2 idr,0,1
  ihvs = nstrnum("hvs") 
  turnoff2 ihvs,0,1
  turnoff
endin

turnon "cdelay"
instr cdelay
  aR init 0
  aL init 0
  ar1 init 0
  ar2 init 0
  abL chnget "delInL"
  abR chnget "delInR"
  kbase init 0.25
  kfb = 0.2
  aL delayr 10
  aTap1L   deltap  kbase * 2
  aTap2L   deltap  kbase * 4
  aTap3L   deltap  kbase * 6
  aTap4L   deltap  kbase * 8
          delayw   abL + (aTap4L*kfb)     ; write audio into buffer
  aR delayr 10
  aTap1R  deltap  kbase * 1
  aTap2R  deltap  kbase * 3
  aTap3R  deltap  kbase * 5
  aTap4R  deltap  kbase * 7
          delayw   abR + (aTap4R*kfb)     ; write audio into buffer
  aoutL = (aTap1L+aTap2L+aTap3L+aTap4L)*0.8
  aoutR = (aTap1R+aTap2R+aTap3R+aTap4R)*0.8
  chnmix aoutL,"mixL"
  chnmix aoutR,"mixR"
endin

turnon "creverb"
instr creverb
  krevfb=0.6
  krevfreq=1
  aR chnget "revInL"
  aL chnget "revInR"
  arevR,arevL reverbsc aR, aL, krevfb, 15000, 44100, 0.5, 1
  chnmix arevR,"mixL"
  chnmix arevL,"mixR"
endin
turnon "Mixer"
instr Mixer
  aL chnget "mixL"
  aR chnget "mixR"
  outs aL,aR
  chnclear "mixL", "mixR","delInL","delInR","revInL","revInR"
endin


</CsInstruments>

<CsScore>
;i "trigDrums" 0 3000 
;i "hvs" 0.00067 300 [220] 2 0.312
;i "hvs" 0.00067 300 [220*9/8] 2 0.312
;i "hvs" 0.00067 300 [220*7/6] 2 0.312
;i "hvs" 0.00067 300 [220*3/2] 2 0.312
;i "hvs" 0.00067 3000 [110] 8 0.322 88287413.128 1 6 0 2 0.005
;i "hvs" 0.00067 3000 [110] 4 0 6 0.193309 [821927*4294.967296] 0 0 0.04

</CsScore>
</CsoundSynthesizer>
