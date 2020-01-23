device, decomposed=0
loadct, 4

printhead=1 ;set print file header flag
printrechead=0 ; set print record header flag

cd,  'C:\Users\pazmany\Documents\Work\X-band\UCAR\IDL\'
;cd, 'E:\xpol data\'

fpath='C:\Users\pazmany\Documents\Work\X-band\UCAR\Data\'
;fpath='E:\xpol data\'

temp=fpath
fname=dialog_pickfile(path=fpath,/read,filter="XPOL-20*",/must_exist,$
                 title="Choose data file")

;*** FILE HEADER ***

;File Header Block:

file_head = { $
		site:bytarr(1024), $		; operator notes 1024 characters										1024
	 	az_off:0D, $				; operator entered absolute azimuth offset										8
	 	rcb: 0L, $					; RCB present true/False								4
	 	cfw:0D, $					; clutter filter width m/s								8
	 	cave:0L, $					; no of groups in clutter filtering interval			4
	 	drate:0D, $					; data rate 											8
	 	fftl:0L, $					; FFT Length											4
	 	fftwindow:0L, $				; FFT window type										4
	 	res1:0L, $					; 														4
	 	nrecords:0L, $				; new file start: n records								4
	 	nscans:0L, $				; new file start: n scans								4
	 	recsize:0L, $				; new file start file size (MB)							4
	 	rectime:0L, $				; new file start time (sec)								4
	 	bitflags:0L, $				; new file start bit flags								4 ; bit 1=time, 2=size, 3=n_scans, 4, n_records
	 	res2:0L, $					;														4
	 	bw:0D, $					; dig rec filter bandwidth								8
	 	freqthres:0D, $				; Freq tracking adjust thres (% of bandwidth) 			8
	 	freqadj:0L, $				; Freq adjust  mode										4
	 	res3:0L, $					;														4
	 	pri3:0L, $					; PRI3 - group spacing (us)								4
	 	hdbz:0D, $					; H dBZ-dBm @ 1 km offset								8
	 	hnoisedBm:0D, $				; H noise power											8
	 	inttime:0D, $				; Integration time										8
	 	freqerror:0D, $				; Freq error											8
	 	freq:0D, $					; IF Freq.												8
	 	rmax:0D, $ 					; max. sampled range meters								8
	 	nrg:0L, $					; No. of Range Gates									4
	 	nps:0L, $					; No. of pulses in a group								4
	 	postdec:0L, $				; Post decimation										4
	 	postave:0L, $				; No. of clutter filtering intervals in ave. interval	4
	 	pri1:0L, $					; PRI1 (us)												4
	 	pri2:0L, $					; PRI2 (us)												4
	 	prit:0L, $					; PRIT (us)												4
	 	cicdec:0L, $				; CIC decimation (on-board)								4
	 	pl:0D, $					; Pulse Length (m)										8
	 	res5:0L, $					; 														4
	 	rgs:0D, $					; Range Gate Spacing									8
	 	res6:0L, $					;														4
	 	rres:0D, $					; Range Resolution (m/gate)								8
	 	recfftmoments:0L, $			; Rec. Moments											4
	 	recraw:0L, $				; Rec. Raw												4
	 	recenable:0L, $				; Rec. Enable											4
	 	servmode:0L, $				; Serv. Mode (PP, DPP, FFT, FFT2, FFT2I)				4
	 	res7:0L, $					;														4
	 	servstate:0L, $				; Serv. State (Idle, Run, Record)						4
	 	res8:0L, $					;														4
	 	softdec:0L, $				; Post Decimation										4
	 	sumpower:0L, $				; Sum Powers in PP or DPP mode (yes/no)					4
	 	ave:0L, $ 					; Total number of groups averaged						4
	 	txdel:0L, $					; TX Delay (ns)											4
	 	txdelpwmult:0L, $			; TX del PW mult										4
	 	txcenter:0L, $				; TX center (ns)										4
	 	txcenteroffset:0L, $		; TX center offset (ns)									4
	 	txswdel:0L, $				; TX SW Del. (ns)										4
	 	txswholdoff:0L, $			; TX SW Hold Off (ns)									4
	 	cfon:0L, $					; Clutter Flter (on/off)								4
	 	vdbz:0D, $					; V dBZ-dBm @ 1 km										8
	 	vnoisedbm:0D, $				; V noise dBm											8
	 	zrg:0D, $					; Zero Range Gate Index									8
	 	scan_type:0L, $  ; scan type 3=Point, 5=PPI, 6=RHI, 7=Az Ras, 8=El Ras, 9=Vol. 4 bytes
	 	host_ts:lonarr(2), $		; Time stamp sec										2*4
	 	hostname:bytarr(8), $
	 	spare:lonarr(2) $   ; spare
		}



file_head_size=n_tags(file_head, /data_length) ; BYTES
; The file_head main header structure totals 56*4+20*8=384 bytes
; 0.0=4 bytes,  0.0D=8 bytes, dblarr(1)=8 bytes, 0L=4 bytes, lonarr(1)=4 bytes, strarr(1)=1 bite





openr, fd, fname, /GET_LUN


readu,fd,file_head ; Read the file header

IF printhead EQ 1 THEN BEGIN
		print, 'site: ', string(file_head.site)
	 	print, 'az_off: ', file_head.az_off
	 	print, 'rcb: ', file_head.rcb
	 	print, 'cfw: ', file_head.cfw
	 	print, 'cave: ', file_head.cave
	 	print, 'drate: ', file_head.drate
	 	print, 'fftl: ', file_head.fftl
	 	print, 'fftwindow: ', file_head.fftwindow
	 	print, ' : ', file_head.res1
	 	print, 'nrecords: ', file_head.nrecords
	 	print, 'nscans: ', file_head.nscans
	 	print, 'recsize: ', file_head.recsize
	 	print, 'rectime: ', file_head.rectime
	 	print, 'bitflags: ', file_head.bitflags
	 	print, ' : ', file_head.res2
	 	print, 'bw: ', file_head.bw
	 	print, 'freqthres: ', file_head.freqthres
	 	print, 'freqadj: ', file_head.freqadj
	 	print, ' : ', file_head.res3
	 	print, 'pri3: ', file_head.pri3
	 	print, 'hdbz: ', file_head.hdbz
	 	print, 'hnoisedBm: ', file_head.hnoisedBm
	 	print, 'inttime: ', file_head.Inttime
	 	print, 'freqerror: ', file_head.freqerror
	 	print, 'freq: ', file_head.freq
	 	print, 'rmax: ', file_head.rmax
	 	print, 'nrg: ', file_head.nrg
	 	print, 'nps: ', file_head.nps
	 	print, 'postdec: ', file_head.postdec
	 	print, 'postave: ', file_head.postave
	 	print, 'pri1: ', file_head.pri1
	 	print, 'pri2: ', file_head.pri2
	 	print, 'prit: ', file_head.prit
	 	print, 'cicdec: ', file_head.cicdec
	 	print, 'pl: ', file_head.pl
	 	print, ' : ', file_head.res5
	 	print, 'rgs: ', file_head.rgs
	 	print, ' : ', file_head.res6
	 	print, 'rres: ', file_head.rres
	 	print, 'recfftmoments: ', file_head.recfftmoments
	 	print, 'recraw: ', file_head.recraw
	 	print, 'recenable: ', file_head.recenable
	 	print, 'servmode: ', file_head.servmode
	 	print, ' : ', file_head.res7
	 	print, 'servstate: ', file_head.servstate
	 	print, ' : ', file_head.res8
	 	print, 'softdec: ', file_head.softdec
	 	print, 'sumpower: ', file_head.sumpower
	 	print, 'ave: ', file_head.ave
	 	print, 'txdel: ', file_head.txdel
	 	print, 'txdelpwmult: ', file_head.txdelpwmult
	 	print, 'txcenter: ', file_head.txcenter
	 	print, 'txcenteroffset: ', file_head.txcenteroffset
	 	print, 'txswdel: ', file_head.txswdel
	 	print, 'txswholdoff: ', file_head.txswholdoff
	 	print, 'cfon: ', file_head.cfon
	 	print, 'vdbz: ', file_head.vdbz
	 	print, 'vnoisedBm: ', file_head.vnoisedbm
	 	print, 'zrg: ', file_head.zrg
	 	print, 'scan_type: ', file_head.scan_type
	 	print, 'host_ts: ', file_head.host_ts
	 	print, 'host_name: ', string(file_head.hostname)
	 	print, 'spare: ', file_head.spare
	 	print, '********************************************'
	 	print, ' '
ENDIF

;stop

serv_mode=['PP', 'DPP', 'FFT', 'FFT2', 'FFT2I']
rec_raw=[' ', 'Raw Mode']
sum_power=[' ', 'Sum Power']
print, serv_mode(file_head.servmode), ', ', rec_raw(file_head.recraw), ', ',  sum_power(file_head.sumpower)

;Range of each range gate (km):
r=0.001*(findgen(file_head.nrg)*file_head.rgs - file_head.zrg*3.75) ; 3.75 m is: 80 MHz Dig Rec sample rate dec by 2 to 40 MHz

;*** RECORDS ***

; * Record Header *

rec_head = { $
  host_ts: lonarr(2), $ 			; host time stamp										2*4		Xpol stat
  rcb_temp: lonarr(4), $			; temperatures											4*4
  rcb_incl: lonarr(2), $			; inclinometers											2*4
  rcb_fuel: 0L, $					; fuel level											4
  cpu_temp: 0., $					; cpu temp (not working yet)			4
  scan_type: 0L, $						; scan type 3=Point, 5=PPI, 6=RHI, 7=Az Ras, 8=El Ras, 9=Vol. 4 bytes
  tx_power_sample:0., $				; Tx pulse sample power
  spare: lonarr(5), $				; spare													7*4
  az: 0D, $							; az pos												8 		Pedestal
  el: 0D, $							; el pos												8
  az_vel: 0D, $						; az vel												8
  el_vel: 0D, $						; el vel												8
  lat_hem: 0L, $				; Lat hemisphere (ascii codes of 'N' or 'S')								4
  lat: 0D, $						; Latitude												8
  lon_hem: 0L, $				; Lon hemisphere (ascii codes of 'E' or 'W')								4
  lon: 0D, $						; Longitude												8
  heading_ref: 0L, $			; Heading reference ( ascii codes of 'T' or 'M'								4
  heading_deg: 0D, $				; Heading (deg)											8
  speed: 0D, $						; Speed (km/hr)											8
  gga:bytarr(96), $					; GPS GGA message text										96		GPS
  gps_ts: lonarr(2), $	 			; gps time stamp										2*4
  data_type: 0L, $					; Data Type												4		Data stat
  data_size: 0L $					; Data Size												4
}




rec_head_size=n_tags(rec_head, /data_length) ; BYTES
; The file_head main header structure totals 56*4+20*8=384 bytes
; 0.0=4 bytes,  0.0D=8 bytes, dblarr(1)=8 bytes, 0L=4 bytes, lonarr(1)=4 bytes, strarr(1)=1 bite


;*** Calulate the number of records in the file ***
; file_head_size - is the main file header block size


Case file_head.servmode of
	0: begin ; PP
    	record_size=long(rec_head_size+4*10*file_head.nrg); 4 power, 2 complex PP, 1 complex Cross-Correlation
    	data = { $
	        pow: fltarr(file_head.nrg, 4), $ ; nrg Vch 1st pulse, nrg Vch 2nd pulse, nrg Hch 1st pulse, nrg Hch 2nd pulse
	        pp: complexarr(file_head.nrg, 2), $ ; nrg Vch pulse pair, nrg Hch pulse pair
	        cc: complexarr(file_head.nrg) $ ; nrg V-H cross correlation (averaged 1st and 2nd pulse data)
           }
    	IF file_head.sumpower EQ 1 THEN BEGIN
    		record_size=long(rec_head_size+4*8*file_head.nrg); 2 power, 2 complex pp, 1 complex cc
			data = { $
		        pow: fltarr(file_head.nrg, 2), $
		        pp: complexarr(file_head.nrg, 2), $
		        cc: complexarr(file_head.nrg) $
           		}
		  ENDIF
		  IF file_head.recraw EQ 1 THEN BEGIN
        record_size=long(rec_head_size+4*2*file_head.cave*file_head.postave*2*file_head.nrg*2); V/H, all ave, 2 pulses, rg, I/Q
      data = { $
            V: complexarr(file_head.nrg, file_head.cave*file_head.postave*2), $
            H: complexarr(file_head.nrg, file_head.cave*file_head.postave*2) $
              }
      ENDIF
	end
	1: begin ; DPP
	    record_size=long(rec_head_size+4*16*file_head.nrg); 6 power, 4 complex PP, 1 complex Cross-Correlation
	    data = { $
	        pow: fltarr(file_head.nrg, 6), $
	        pp: complexarr(file_head.nrg, 4), $ ;(*,0) and (*,1) are V, (*,2) and (*,3) are H
	        cc: complexarr(file_head.nrg) $
           }
    	IF file_head.sumpower EQ 1 THEN BEGIN
    		record_size=long(rec_head_size+4*8*file_head.nrg); 2 power, 4 complex pp, 1 complex cc
			data = { $
		        pow: fltarr(file_head.nrg, 2), $
		        pp: complexarr(file_head.nrg, 4), $
		        cc: complexarr(file_head.nrg) $
           		}
		  ENDIF
		  IF file_head.recraw EQ 1 THEN BEGIN
        record_size=long(rec_head_size+4*2*file_head.cave*file_head.postave*3*file_head.nrg*2); V/H, all ave, 3 pulses, rg, I/Q
      data = { $
            V: complexarr(file_head.nrg, file_head.cave*file_head.postave*3), $
            H: complexarr(file_head.nrg, file_head.cave*file_head.postave*3) $
              }
      ENDIF

	end
	2: begin ; FFT
	    record_size=long(rec_head_size+4*4*file_head.nrg*file_head.fftl); 2 power spectrum, 1 complex cross-spectrum
	    data = { $
		        pow: fltarr(file_head.fftl, file_head.nrg, 2), $
		        cc: complexarr(file_head.fftl, file_head.nrg) $
           		}

      IF file_head.recraw EQ 1 THEN BEGIN
        record_size=long(rec_head_size+4*2*file_head.cave*file_head.postave*file_head.fftl*file_head.nrg*2); V/H, all ave, fftl pulses, rg, I/Q
      data = { $
            V: complexarr(file_head.nrg, file_head.cave*file_head.postave*file_head.fftl), $
            H: complexarr(file_head.nrg, file_head.cave*file_head.postave*file_head.fftl) $
              }
      ENDIF
	end
	3: begin ; FFT2
	    record_size=long(rec_head_size+4*8*file_head.nrg*file_head.fftl); 4 power spectrum, 2 complex cross-spectrum
	    data = { $
		        pow: fltarr(file_head.fftl, file_head.nrg, 4), $
		        cc: complexarr(file_head.fftl, file_head.nrg, 2) $
           		}
      IF file_head.recraw EQ 1 THEN BEGIN
        record_size=long(rec_head_size+4*2*file_head.cave*file_head.postave*2*file_head.fftl*file_head.nrg*2); V/H, all ave, 2*fftl pulses, rg, I/Q
      data = { $
            V: complexarr(file_head.nrg, file_head.cave*file_head.postave*2*file_head.fftl), $
            H: complexarr(file_head.nrg, file_head.cave*file_head.postave*2*file_head.fftl) $
              }
      ENDIF
	end
	4: begin ; FFT2I
	    record_size=long(rec_head_size+4*8*file_head.nrg*file_head.fftl); 4 power spectrum, 2 complex cross-spectrum
	    data = { $
		        pow: fltarr(file_head.fftl, file_head.nrg, 4), $
		        cc: complexarr(file_head.fftl, file_head.nrg, 2) $
           		}
      IF file_head.recraw EQ 1 THEN BEGIN
        record_size=long(rec_head_size+4*2*file_head.cave*file_head.postave*2*file_head.fftl*file_head.nrg*2); V/H, all ave, 2*fftl pulses, rg, I/Q
      data = { $
            V: complexarr(file_head.nrg, file_head.cave*file_head.postave*2*file_head.fftl), $
            H: complexarr(file_head.nrg, file_head.cave*file_head.postave*2*file_head.fftl) $
              }
      ENDIF
	end
ENDCASE


result=fstat(fd)
fsize=ulong64(strtrim(result.size,2))
;print, ' '
;print, 'File size = ', fsize

nrec=long((fsize-file_head_size)/float(record_size))

;print, ' '
print, 'Number of data records = ', nrec
print, ' '
;*** End Record Calculation ***

;*** Read Data ***
;window, 0, xsize=500, ysize=400;, Title="Power (dBm)"
;window, 1, xsize=500, ysize=400;, Title="Pulse Pair Phase (Rad)"

;FOR i=0, nrec-1 DO BEGIN
FOR i=0, 20 DO BEGIN

print, ' '
print, "Loop Count: ", i



    CASE file_head.servmode OF
    0: BEGIN ; PP

        readu, fd, rec_head  ; Read the record header
        ;print, rec_head
;stop
        readu, fd, data

        IF file_head.recraw EQ 0 THEN BEGIN
          dBm=10*alog10((data.pow)>1e-11)
          pp = data.pp
          cc = data.cc
          ;plot, dBm(0:20,0)
;stop
           window,0, title="Power: " + strmid(fname, strpos(fname, 'XPOL-20'), 20)
            plot, r, dBm(*, 0), yrange=[-90, 0] ;CH1-1 or summed (V-pol) power (dBm)
            oplot, r, dBm(*,1) ;CH1-2 (V-pol) or Ch-2 summed power (dBm)
            print, "TX Sample: ", dBm(ROUND(file_head.zrg/(file_head.rgs/3.75)), 0)

            IF file_head.sumpower EQ 0 THEN BEGIN
              oplot, r, dBm(*,2) ;CH2-1 (H-pol) power (dBm)
              oplot, r, dBm(*,3) ;CH2-2 (H-pol) power (dBm)
			ENDIF

           window,1, title="Phase: " + strmid(fname, strpos(fname, 'XPOL-20'), 20)
            plot, r, atan(pp(*,0), /phase), yrange=[-1*!pi, !pi] ;CH1 (V-pol) PP phase (rad)
            oplot, r, atan(pp(*,1), /phase) ;CH2 (H-pol) pp phase (rad)

           window,2, title="Differential Phase: " + strmid(fname, strpos(fname, 'XPOL-20'), 20)
            ;plot, r, atan(cc, /phase), yrange=[-1*!pi, !pi] ;differential phase (rad)
            plot, (180./!pi)*smooth(atan(cc, /phase), 5), yrange=[0., 90], /noerase ;differential phase (rad)
        ENDIF
        IF file_head.recraw EQ 1 THEN BEGIN ; I/Q data recorded - here converted to power
           window,0, title="Power: " && strmid(fname, strpos(fname, 'XPOL-20'), 20)
           plot, r, 10*alog10(1000.*(abs(data.V(*,0)))^2/(2*50.))
           oplot, r, 10*alog10(1000.*(abs(data.H(*,0)))^2/(2*50.))
        ENDIF

    END
    1: BEGIN ; DPP

        readu, fd, rec_head  ; Read the record header
        print, rec_head
        readu, fd, data

       IF file_head.recraw EQ 0 THEN BEGIN
        dBm=10*alog10((data.pow)>1e-11)
        pp = data.pp
        cc = data.cc

        window,0, title="Power: " + strmid(fname, strpos(fname, 'XPOL-20'), 20)
          plot, r, dBm(*, 0), yrange=[-90, 0] ;CH1 (V-pol) power (dBm) 1 st pulse if sumpower=0 or average of all three pulses
          oplot, r, dBm(*,1) ;CH1-2 (V-pol) power (dBm) if sumpower=0 or Ch2 (H-pol) power dBm average of three pulses
          IF file_head.sumpower EQ 0 THEN BEGIN
            oplot, r, dBm(*,2) ;CH1-3 (V-pol) power (dBm)
        		oplot, r, dBm(*,3) ;CH2-1 (H-pol) power (dBm)
        		oplot, r, dBm(*,4) ;CH2-2 (H-pol) power (dBm)
        		oplot, r, dBm(*,5) ;CH2-3 (H-pol) power (dBm)
          ENDIF

        window,1, title="Doppler Phase: " + strmid(fname, strpos(fname, 'XPOL-20'), 20)
          plot, r, atan(pp(*,0), /phase), yrange=[-1*!pi, !pi] ;CH1 V-pol PP phase (rad)
          oplot, r, atan(pp(*,1), /phase) ;CH2 H-pol pp phase (rad)

        window,2, title="Differential Phase: " + strmid(fname, strpos(fname, 'XPOL-20'), 20)
          plot, r, atan(cc, /phase), yrange=[-1*!pi, !pi] ;differential phase (rad)
       ENDIF
       IF file_head.recraw EQ 1 THEN BEGIN ; I/Q data recorded - here converted to power
           window,0, title="Power: " + strmid(fname, strpos(fname, 'XPOL-20'), 20)
           plot, r, 10*alog10(1000.*(abs(data.V(*,0)))^2/(2*50.))
           oplot, r, 10*alog10(1000.*(abs(data.H(*,0)))^2/(2*50.))
       ENDIF

    END
    2: BEGIN ; FFT

        readu, fd, rec_head  ; Read the record header
        readu, fd, data

        dBm=10*alog10((data.pow)>1e-11)
        cc = data.cc

        window,0, title="Power Spectrum: " + strmid(fname, strpos(fname, 'XPOL-20'), 20)

    		tvscl, bytscl(shift(dBm(*,*,0), round(0.5*file_head.fftl), 0), -90, 0), 0, 0
    		tvscl, bytscl(shift(dBm(*,*,1), round(0.5*file_head.fftl), 0), -90, 0), file_head.fftl+2, 0

    END
    3: BEGIN ; FFT2

        readu, fd, rec_head  ; Read the record header
        readu, fd, data

        dBm=10*alog10((data.pow)>1e-11)
        cc = data.cc

        window,0, title="Power Spectrum: " + strmid(fname, strpos(fname, 'XPOL-20'), 20)

    		tvscl, bytscl(shift(dBm(*,*,0), round(0.5*file_head.fftl), 0), -90, 0), 0, 0
    		tvscl, bytscl(shift(dBm(*,*,1), round(0.5*file_head.fftl), 0), -90, 0), file_head.fftl+2, 0
    		tvscl, bytscl(shift(dBm(*,*,2), round(0.5*file_head.fftl), 0), -90, 0), 2*(file_head.fftl+2), 0
    		tvscl, bytscl(shift(dBm(*,*,3), round(0.5*file_head.fftl), 0), -90, 0), 3*(file_head.fftl+2), 0

    END
    4: BEGIN ; FFT2I

        readu, fd, rec_head  ; Read the record header
        readu, fd, data

        dBm=10*alog10((data.pow)>1e-11)
        cc = data.cc

        window,0, title="Power Spectrum: " + strmid(fname, strpos(fname, 'XPOL-20'), 20)

    		tvscl, bytscl(shift(dBm(*,*,0), round(0.5*file_head.fftl), 0), -90, 0), 0, 0
    		tvscl, bytscl(shift(dBm(*,*,1), round(0.5*file_head.fftl), 0), -90, 0), file_head.fftl+2, 0
    		tvscl, bytscl(shift(dBm(*,*,2), round(0.5*file_head.fftl), 0), -90, 0), 2*(file_head.fftl+2), 0
    		tvscl, bytscl(shift(dBm(*,*,3), round(0.5*file_head.fftl), 0), -90, 0), 3*(file_head.fftl+2), 0

	END

    ENDCASE

        print, 'Host Time Stamp: ', systime(0, rec_head.host_ts(0)), rec_head.host_ts(1), ' us'   ; Host PC Time Stamp
        print, 'Pos. (az, el, az_vel, el_vel: ', rec_head.az, rec_head.el, rec_head.az_vel, rec_head.el_vel


END ; nrec loop

IF printrechead EQ 1 THEN BEGIN ; print the last record header

				print, 'Host Time: ', systime(0, rec_head.host_ts(0)), rec_head.host_ts(1), ' us'   ; Host PC Time Stamp
				print, "Temperatures RF, Outside, Pod, PC (C) ", (rec_head.rcb_temp-282)/6.214

        fuel=(rec_head.rcb_fuel-385)/(72-385) ; 385 = empty, 72 = full
        print, "Fuel (0-1): ", fuel

        roll= (rec_head.rcb_incl(0)-446.9)*0.085
        fore_aft=(rec_head.rcb_incl(1)-452.1)*0.085

        print, "Roll: ", roll, " deg,  Pitch: ", fore_aft, " deg."
        ; Roll:  negative (low) when right tire down; positive (high) when right tire up
        ; Pitch: negative (low) when tung is up; positive (high) when tung is down

			    print, 'RCB CPU Temp.: ', rec_head.cpu_temp ;0 is written here - not activated yet

			    scantype=[' ', ' ', ' ', 'Point', ' ', 'PPI', 'RHI', 'Az Ras', 'El Ras', 'Vol.']
			    print, 'Pedestal: ', scantype(rec_head.scan_type)

				print, 'Spare: ', rec_head.spare

			  	print, 'az, el (deg): ', rec_head.az, rec_head.el
				print, 'az_vel, el_vel (deg/sec): ', rec_head.az_vel, rec_head.el_vel
        print, 'GPS string: ', rec_head.gga
				print, 'GPS Time: ', systime(0, rec_head.gps_ts(0)), rec_head.gps_ts(1), ' us'   ; Host PC Time Stamp
				print, 'GPS Lat: ', STRING(BYTE(rec_head.lat_hem)), rec_head.lat
				print, 'GPS Lon: ', STRING(BYTE(rec_head.lon_hem)), rec_head.lon
				print, 'GPS Heading: ', rec_head.heading_ref, rec_head.heading_deg, rec_head.speed
				print, 'Data Type, Size: ', rec_head.data_type,  rec_head.data_size

		ENDIF


device,decomposed=0

FREE_LUN, fd

END ; Program
