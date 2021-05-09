<CsoundSynthesizer>

<CsOptions>
-odac -dm0
</CsOptions>

<CsInstruments>
sr        = 48000
ksmps     = 32
nchnls    = 2
0dbfs = 1

opcode Ensemble, aa, aiiikiip
  ain, idelay, idpth, imin, imax, inumvoice, iwave, icount xin
  adel init 0
  ainl init 0
  ainr init 0
  alfo init 0
  al init 0
  ar init 0

  incr = 1/(inumvoice)

  if (icount == inumvoice) goto out
  ainl, ainr Ensemble ain, idelay, idpth, imin, imax, inumvoice, iwave, icount + 1

out:
  iratemax unirand imax

  alfo oscil idpth, iratemax + imin, iwave
  adel vdelay3 ain/(inumvoice * .5), (idelay + alfo) * 1000, 1000
  al = ainl + adel * incr * icount
  ar = ainr + adel * (1 - incr * icount)
  xout al, ar
endop

giTabEns ftgen 0,0,65536,10,1,0,0,0,0,0,0,0,0,.05,0

opcode ensemble,aa,aak
  aR, aL, kmix xin
  idelay = 0.2
  idepth = 0.3
  imin = 0.2
  imax = 0.2
  ivoices = 12
  aLL,aLR Ensemble aL, idelay/10, idepth/100,imin,imax,ivoices,giTabEns
  aRL,aRR Ensemble aR, idelay/10, idepth/100,imin,imax,ivoices,giTabEns
  aL ntrpol aL,(aLL+aRL)*6,kmix
  aR ntrpol aR,(aLR+aRR)*6,kmix
  xout aL,aR
endop


opcode master,0,aakkkk
  ainL,ainR,kamp,krev,kens,kshi xin
  aL = ainL*kamp
  aR = ainR*kamp
  kens init 0
  aL,aR ensemble,aL,aR,kens
  arL = aL*krev
  arR = aR*krev
  ashiL = aL*kshi
  ashiR = aR*kshi
  chnset arL,"revInL"
  chnset arR,"revInR"
  chnset ashiL,"shiInL"
  chnset ashiR,"shiInR"
  chnmix aL,"mixL"
  chnmix aR,"mixR"
endop

;; Channel Helper from Steven YI's livecode.orc

opcode xchnset, 0, Si
  SchanName, ival xin
  Sinit = sprintf("%s_initialized", SchanName)
  chnset(1, Sinit)
  chnset(ival, SchanName)
endop

opcode xchan, i,Si
  SchanName, initVal xin

  Sinit = sprintf("%s_initialized", SchanName)
  if(chnget:i(Sinit) == 0) then
    chnset(1, Sinit)
    chnset(initVal, SchanName)
  endif
  xout chnget:i(SchanName)
endop

opcode xchan, k,Si
  SchanName, initVal xin

  Sinit = sprintf("%s_initialized", SchanName)
  if(chnget:i(SchanName) == 0) then
    chnset(1, Sinit)
    chnset(initVal, SchanName)
  endif
  xout chnget:k(SchanName)
endop


;giratio0 ftgen 0,0,32,-2,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32
;giratio1 ftgen 0,0,32,-2,1,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50,52,54,56,58,60,64
;giratio2 ftgen 0,0,32,-2,1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55,57,59,61,63
giratio0 ftgen 0,0,32,-2,1,2,3,4,21/4,6,7,8,8,9,21/2,12,49/4,14,16,16,18,21,24,49/2,27,28,63/2,32,32,36,42,48,49,56,63,64
giratio1 ftgen 0,0,32,-2,1,3/2,2,9/4,5/2,21/8,3,7/2,4,9/2,5,21/4,6,7,8,9,10,21/2,12,14,16,18,20,21,24,28,32,36,40,48,56,64
giratio2 ftgen 0,0,32,-2,1,3/2,2,9/4,5/2,11/4,3,10/3,15/4,4,9/2,5,11/2,6,20/3,15/2,8,9,10,11,12,40/3,15,16,18,20,22,24,80/3,30,32,36
giratio3 ftgen 0,0,32,-2,1,10/9,7/6,21/16,3/2,7/4,2,20/9,7/3,21/8,3,7/2,4,40/9,14/3,21/4,6,7,8,80/9,28/3,21/2,12,14,16,160/9,56/3,21,24,28,32,320/9
giratio4 ftgen 0,0,32,-2,1,9/8,7/6,21/16,3/2,14/9,7/4,2,9/4,7/3,21/8,3,28/9,7/2,4,9/2,14/3,21/4,6,56/9,7,8,9,28/3,21/2,12,112/9,14,16,18,56/3,21
giratio5 ftgen 0,0,32,-2,1,21/20,14/11,27/20,40/27,11/7,16/9,2,21/10,28/11,27/10,80/27,22/7,32/9,4,21/5,56/11,27/5,160/27,44/7,64/9,8,42/5,112/11,54/5,320/27,88/7,128/9,16,84/5,224/11,104/5
giratio6 ftgen 0,0,32,-2,1,10/9,7/6,21/16,3/2,105/64,7/4,2,20/9,7/3,21/8,3,105/32,7/2,4,40/9,14/3,21/4,6,105/16,7,8,80/9,28/3,21/2,12,105/8,14,16,160/9,56/3,21
//giratio7 ftgen 0,0,32,-2,1,9/8,6/5,4/3,3/2,5/3,16/9,2,9/4,12/5,8/3,3,10/3,32/9,4,9/2,24/5,16/3,6,20/3,64/9,8,9,48/5,32/3,12,40/3,128/9,16,18,96/5,64/3
giratio7 ftgen 0,0,32,-2,1,3/2,2,9/4,12/5,8/3,3,10/3,32/9,4,9/2,24/5,16/3,6,20/3,64/9,8,9,48/5,32/3,12,40/3,128/9,16,18,96/5,64/3,24,80/3,256/9,32,36
giratio8 ftgen 0,0,32,-2,1,9/8,8/7,4/3,3/2,32/21,16/9,2,9/4,16/7,8/3,3,64/21,32/9,4,9/2,32/7,16/3,6,128/21,64/9,8,9,64/7,32/3,12,256/21,128/9,16,18,128/7,64/3
giratio[] fillarray giratio0,giratio1,giratio2,giratio3,giratio4,giratio5,giratio6,giratio7,giratio8
giwave0 ftgen 0,0,2^16,10,1
giwave1 ftgen 0,0,2^16,10,1,0.2,0,0.1
giwave2 ftgen 0,0,2^16,10,1,0.5,1/3,1/4,1/5,1/6,1/7,1/8,1/9
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
gisine ftgen 0,0,65536,10,1
opcode vcosynt,a,iiiiiki
  iamp,ifreq,iratio,iamps,imode,kpw,isyn xin
  asig[] init 32
  kindx = 0
  asig[kindx] vco table(kindx,iamps), ifreq*table(kindx,iratio),imode,kpw,gisine,1,0.999999,isyn,0.5
  kindx += 1
  asig[kindx] vco table(kindx,iamps), ifreq*table(kindx,iratio),imode,kpw,gisine,1,0.999999,isyn,0.5
  kindx += 1
  asig[kindx] vco table(kindx,iamps), ifreq*table(kindx,iratio),imode,kpw,gisine,1,0.999999,isyn,0.5
  kindx += 1
  asig[kindx] vco table(kindx,iamps), ifreq*table(kindx,iratio),imode,kpw,gisine,1,0.999999,isyn,0.5
  kindx += 1
  asig[kindx] vco table(kindx,iamps), ifreq*table(kindx,iratio),imode,kpw,gisine,1,0.999999,isyn,0.5
  kindx += 1
  asig[kindx] vco table(kindx,iamps), ifreq*table(kindx,iratio),imode,kpw,gisine,1,0.999999,isyn,0.5
  kindx += 1
  asig[kindx] vco table(kindx,iamps), ifreq*table(kindx,iratio),imode,kpw,gisine,1,0.999999,isyn,0.5
  kindx += 1
  asig[kindx] vco table(kindx,iamps), ifreq*table(kindx,iratio),imode,kpw,gisine,1,0.999999,isyn,0.5
  kindx += 1
  asig[kindx] vco table(kindx,iamps), ifreq*table(kindx,iratio),imode,kpw,gisine,1,0.999999,isyn,0.5
  kindx += 1
  asig[kindx] vco table(kindx,iamps), ifreq*table(kindx,iratio),imode,kpw,gisine,1,0.999999,isyn,0.5
  kindx += 1
  asig[kindx] vco table(kindx,iamps), ifreq*table(kindx,iratio),imode,kpw,gisine,1,0.999999,isyn,0.5
  kindx += 1
  asig[kindx] vco table(kindx,iamps), ifreq*table(kindx,iratio),imode,kpw,gisine,1,0.999999,isyn,0.5
  kindx += 1
  asig[kindx] vco table(kindx,iamps), ifreq*table(kindx,iratio),imode,kpw,gisine,1,0.999999,isyn,0.5
  kindx += 1
  asig[kindx] vco table(kindx,iamps), ifreq*table(kindx,iratio),imode,kpw,gisine,1,0.999999,isyn,0.5
  kindx += 1
  asig[kindx] vco table(kindx,iamps), ifreq*table(kindx,iratio),imode,kpw,gisine,1,0.999999,isyn,0.5
  kindx += 1
  asig[kindx] vco table(kindx,iamps), ifreq*table(kindx,iratio),imode,kpw,gisine,1,0.999999,isyn,0.5
  kindx += 1
  asig[kindx] vco table(kindx,iamps), ifreq*table(kindx,iratio),imode,kpw,gisine,1,0.999999,isyn,0.5
  kindx += 1
  asig[kindx] vco table(kindx,iamps), ifreq*table(kindx,iratio),imode,kpw,gisine,1,0.999999,isyn,0.5
  kindx += 1
  asig[kindx] vco table(kindx,iamps), ifreq*table(kindx,iratio),imode,kpw,gisine,1,0.999999,isyn,0.5
  kindx += 1
  asig[kindx] vco table(kindx,iamps), ifreq*table(kindx,iratio),imode,kpw,gisine,1,0.999999,isyn,0.5
  kindx += 1
  asig[kindx] vco table(kindx,iamps), ifreq*table(kindx,iratio),imode,kpw,gisine,1,0.999999,isyn,0.5
  kindx += 1
  asig[kindx] vco table(kindx,iamps), ifreq*table(kindx,iratio),imode,kpw,gisine,1,0.999999,isyn,0.5
  kindx += 1
  asig[kindx] vco table(kindx,iamps), ifreq*table(kindx,iratio),imode,kpw,gisine,1,0.999999,isyn,0.5
  kindx += 1
  asig[kindx] vco table(kindx,iamps), ifreq*table(kindx,iratio),imode,kpw,gisine,1,0.999999,isyn,0.5
  kindx += 1
  asig[kindx] vco table(kindx,iamps), ifreq*table(kindx,iratio),imode,kpw,gisine,1,0.999999,isyn,0.5
  kindx += 1
  asig[kindx] vco table(kindx,iamps), ifreq*table(kindx,iratio),imode,kpw,gisine,1,0.999999,isyn,0.5
  kindx += 1
  asig[kindx] vco table(kindx,iamps), ifreq*table(kindx,iratio),imode,kpw,gisine,1,0.999999,isyn,0.5
  kindx += 1
  asig[kindx] vco table(kindx,iamps), ifreq*table(kindx,iratio),imode,kpw,gisine,1,0.999999,isyn,0.5
  kindx += 1
  asig[kindx] vco table(kindx,iamps), ifreq*table(kindx,iratio),imode,kpw,gisine,1,0.999999,isyn,0.5
  kindx += 1
  asig[kindx] vco table(kindx,iamps), ifreq*table(kindx,iratio),imode,kpw,gisine,1,0.999999,isyn,0.5
  kindx += 1
  asig[kindx] vco table(kindx,iamps), ifreq*table(kindx,iratio),imode,kpw,gisine,1,0.999999,isyn,0.5
  kindx += 1
  asig[kindx] vco table(kindx,iamps), ifreq*table(kindx,iratio),imode,kpw,gisine,1,0.999999,isyn,0.5
  kindx += 1
  xout 0.1*iamp*sumarray(asig)
endop
gipdf ftgen 0,0,65536,-42,-0.9,-0.2,0.5,0.2,0.9,0.5
givcomode[] fillarray 0,2,4
chnset 5000,"hvsfilt"
chnset 0.6,"hvsamp"
chnset 0.6,"hvsrev"
chnset 0.5,"hvsshi"
chnset 0.1,"hvsens"
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
  if irt == 0 then
    ibasefrq/=2
  endif
  
  imode = -1 
  if iw > 5 then
     imode = givcomode[iw - 6]
  elseif iw == 5 then
    seed 0
    ip1 duserrnd gipdf
    ip2 duserrnd gipdf
    ip3 duserrnd gipdf
    ip4 duserrnd gipdf
    ip5 duserrnd gipdf
    iwave ftgen 0,0,65536,8,0,65536/6,ip1,65536/6,ip2,65536/6,ip3,65536/6,ip4,65536/6,ip5,65536/6,0
  else
     iwave = giwave[iw]
  endif 

  iamp = p5
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

  imult = 2
  if p11 = 2 then
    imult = 4
  endif

  isnap gensnap 32,isize,p11,p12,50,iseed
  ipos  ftgen 0,0,1025,-7,0,1025,1024
  iamps  ftgen 0,0,ilen,-2, 0    ; THE AMPLITUDES FOR ADSYNTH
  iamps2  ftgen 0,0,ilen,-2, 0    ; THE AMPLITUDES FOR ADSYNTH

  kz0 randc 1,0.031,ipathseed,1
  kz0 limit kz0,-1,1
  kz scale kz0,1,0,1,-1
  ky0 randc 1,0.057,ipathseed*0.9,1
  ky0 limit ky0,-1,1
  ky scale ky0,1,0,1,-1
  kx0 randc 1,0.021,ipathseed*0.8,1
  kx0 limit kx0,-1,1
  kx scale kx0,1,0,1,-1
  ;printks "%f %f %f\n",0.125,kx,ky,kz

  idmp = limit(1-p13,0.0000001,1)
  idivs ftgentmp 0,0,33,5,1,32,idmp,1
  

  hvs3 kx,ky,kz,ilen,isize,isize,isize,iamps,ipos,isnap
  vmultv iamps, idivs, ilen 
  hvs3 ky,kx,kz,ilen,isize,isize,isize,iamps2,ipos,isnap
  vmultv iamps2, idivs, ilen
  
  if imode >= 0 then
    aoscL vcosynt 0.4, ibasefrq, iratio, iamps,imode,0.5,0.141
    aoscR vcosynt 0.4, ibasefrq, iratio, iamps2,imode,0.5,0.141
  else
    aoscL adsynt  0.1, ibasefrq, iwave, iratio, iamps, ilen, 0
    aoscR adsynt  0.1, ibasefrq, iwave, iratio, iamps2, ilen, 0
  endif
  kfilt chnget "hvsfilt"
  aoscL moogvcf aoscL,kfilt,0.1
  aoscR moogvcf aoscR,kfilt,0.1
  aenv madsr 2,0,1,0.5
  aoscL = tanh(aoscL * iamp * imult* aenv) 
  aoscR = tanh(aoscR * iamp * imult* aenv)
  kamp chnget "hvsamp"
  krev chnget "hvsrev"
  kshi chnget "hvsshi"
  kens chnget "hvsens"
  master aoscL,aoscR,kamp,krev,kens,kshi/10
endin

instr trigOff
  ihvs = nstrnum("hvs") 
  turnoff2 ihvs,0,1
  turnoff
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
  //chnclear "mixL", "mixR","shiInL","shiInR","revInL","revInR","ensInR","ensInL"
  chnclear "mixL", "mixR","revInL","revInR"
endin



;Title: Shimmer Reverb
;Author: Steven Yi
;Date: 2018.08.22

;Description:

;Shimmer Reverb using reverb tail that is delayed and fed back into itself after being pitch-shifted using and FFT-based approach

turnon "shimmer"
instr shimmer
  krmsend init 0
  ar chnget "shiInR"
  al chnget "shiInL"
  krms rms ar
  if krms < 0.00001 && krmsend < 0.00001 then
    kgoto out1
  endif
  ;printk 0.5,krms
  ;printk 0.5,krmsend
  ; pre-delay
  al = vdelay3(al, 100, 100)
  ar = vdelay3(ar, 100, 100)

  afbl init 0
  afbr init 0
  ifblvl = 0.45


  al = al + (afbl * ifblvl)
  ar = ar + (afbr * ifblvl)

  ; important, or signal bias grows rapidly
  al = dcblock2(al)
  ar = dcblock2(ar)

  al = tanh(al)
  ar = tanh(ar)

  al, ar reverbsc al, ar, 0.95, 16000

  iratio = 2
  ideltime = 100

  ifftsize  = 2048
  ioverlap  = ifftsize / 4
  iwinsize  = ifftsize
  iwinshape = 1; von-Hann window

  fftin     pvsanal al, ifftsize, ioverlap, iwinsize, iwinshape
  fftscale  pvscale fftin, iratio, 0, 1
  atransL   pvsynth fftscale

  fftin2    pvsanal ar, ifftsize, ioverlap, iwinsize, iwinshape
  fftscale2 pvscale fftin2, iratio, 0, 1
  atransR   pvsynth fftscale2

  ;; delay the feedback to let it build up over time
  afbl = vdelay3(atransL, ideltime, ideltime)
  afbr = vdelay3(atransR, ideltime, ideltime)

  krmsend rms ar
  out1:
  chnmix ar,"mixL"
  chnmix al,"mixR"
endin


</CsInstruments>

<CsScore>
;i "hvs" 0.00067 300 [220] 2 0.312
;i "hvs" 0.00067 300 [220*9/8] 2 0.312
;i "hvs" 0.00067 300 [220*7/6] 2 0.312
;i "hvs" 0.00067 300 [220*3/2] 2 0.312
;i "hvs" 0.00067 3000 [110] 3 0.322 88287413.128 2 6 0 1 0.50 0.3

</CsScore>
</CsoundSynthesizer>
