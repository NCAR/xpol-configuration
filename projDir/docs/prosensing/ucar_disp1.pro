
;Version date: 12/18/2006
; dispX.pro -> widget controlled real time and file reading display program
;Functions used are:
; proclib.pro
; conf_radar_structures.pro
; read_data.pro

;to set set idl_path in
; Settings -> Control Panel -> System -> Advanced ->Environment Variables -> IDL_PATH
; check: print, !path
; C:\RSI\IDL61\lib;C:\Documents and Settings\Pazmany\My Documents\Work\IDL

;@proclib ;link processing libraries

; To generate Runtime routine:
; 1) Select "Compile" in the Run menu
; 2) Select "Resolve Dependencies" in the Run menu
;          ( or in the command line type: Resolve_all
; 3) In the command line type:
;		save, /routines, filename='C:/RaXpol_disp1.sav'
;		save, /routines, filename='H:/RaXpol_disp1.sav'

;***************************************************************************************

PRO nexrad_ct ;*** NEXRAD Color Table ***

 tvlct,tr,tg,tb,/get
 num_col= size(tr)
 num_col= floor(num_col(1))
 h=num_col-2

 per=(num_col-1)/255.
 c1=fix(per*4)
 c2=fix(per*255)
 dc=c2-c1

 r=transpose([188, 147,  99, 190, 133, 77,   0,   0,   0,   0,   0,   0, 255, 231, 255, 255, 214, 192, 255, 153, 20])
 g=transpose([158,  99,  60, 192, 136, 77, 236, 160, 120, 255, 200, 144, 255, 192, 144,   0,   0,   0,   0,  85, 20])
 b=transpose([209, 182, 128, 146,  79, 77, 236, 246, 190,   0,   0,   0,   0,   0,   0,   0,   0,   0, 255, 201, 20])


 tr=transpose(reform([r,r,r,r,r,r,r,r,r,r,r,r],1, 252))
 tg=transpose(reform([g,g,g,g,g,g,g,g,g,g,g,g],1, 252))
 tb=transpose(reform([b,b,b,b,b,b,b,b,b,b,b,b],1, 252))

 r=bytscl(findgen(256))
 g=r
 b=r

 r(1:h)=interpolate(tr,251*findgen(h)/(h-1))
 g(1:h)=interpolate(tg,251*findgen(h)/(h-1))
 b(1:h)=interpolate(tb,251*findgen(h)/(h-1))

 r(0)=255 & g(0)=255 & b(0)=255
 r(255)=0 & g(255)=0 & b(255)=0
 tvlct, r,g,b

 DEVICE, RETAIN=2, DECOMPOSED=0

END
;******************************************************************************************

PRO doppler_ct ;*** NEXRAD Color Table ***

 tvlct,tr,tg,tb,/get
 num_col= size(tr)
 num_col= floor(num_col(1))
 h=num_col-2

 per=(num_col-1)/255.
 c1=fix(per*4)
 c2=fix(per*255)
 dc=c2-c1

 r=transpose([  0,   0,   0,   0,   0,   0, 180, 200, 147, 175, 187, 215, 230, 255])
 g=transpose([255, 215, 190, 175, 155, 130, 200, 180,   0,   0,   0,   0,   0,   0])
 b=transpose([  0,  10,  37,  35,  30,  25, 150, 150,  30,  35,  37,   0,   0,   0])


 tr=transpose(reform([r,r,r,r,r,r,r,r,r,r,r,r,r,r,r,r,r,r],1, 252))
 tg=transpose(reform([g,g,g,g,g,g,g,g,g,g,g,g,g,g,g,g,g,g],1, 252))
 tb=transpose(reform([b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b],1, 252))

 r=bytscl(findgen(256))
 g=r
 b=r

 r(1:h)=interpolate(tr,251*findgen(h)/(h-1))
 g(1:h)=interpolate(tg,251*findgen(h)/(h-1))
 b(1:h)=interpolate(tb,251*findgen(h)/(h-1))

 r(0)=255 & g(0)=255 & b(0)=255
 r(255)=0 & g(255)=0 & b(255)=0
 tvlct, r,g,b

 DEVICE, RETAIN=2, DECOMPOSED=0

END
;************************************************************************

FUNCTION Ct_len

tvlct, r, g, b, /get

sz = size(r)

return, sz(1)
END
;************************************************************************

FUNCTION process_configuration, st

	head=st.head
	head2=st.head2
	stat=st.stat
	scan_type=stat.scan_type
	gps1=st.gps1
	gps2=st.gps2
	rec_head=st.rec_head

print, head.zrg


	st.head.rmax=0.001*(head.nrg*head.rgs-head.zrg*3.75)


	; Process Configuration Parameters section

							serv_mode=['PP', 'DPP', 'FFT', 'FFT2', 'FFT2I']
							rec_raw=[' ', 'Raw Mode']
							sum_power=[' ', 'Sum Power']
							fft_taper=['Bartlett', 'Blackman', 'Hamming', 'Hanning', 'Rect']
							pos_scan=[' ', ' ', ' ', 'Point', ' ', 'PPI', 'RHI', 'Az Ras', 'El Ras', 'Vol'] ; ; scan type 3=Point, 5=PPI, 6=RHI, 7=Az Ras, 8=El Ras, 9=Vol.
							pos_disp=[0, 2, 2, 2, 2, 0, 1, 0, 1, 0] ; ; scan type 3=Point, 5=PPI, 6=RHI, 7=Az Ras, 8=El Ras, 9=Vol.

							IF pos_disp(scan_type) LE 1 THEN dispdata=fltarr(head.nrg, st.display.n_beams, N_ELEMENTS(st.wi.parameter_list)) ; PPI or RHI
							IF pos_disp(scan_type) EQ 2 THEN dispdata=fltarr(head.nrg, st.display.bsize, N_ELEMENTS(st.wi.parameter_list)) ; Range/Time display

								dispdata(*,*,*)=!values.F_nan ; clear display array

							st.display.thet=!pi*(st.display.ant_bw*findgen(360/st.display.ant_bw)+0.5)/180. ; write the center of each angle bin as a start.

							r=0.001*(findgen(head.nrg)*head.rgs - head.zrg*3.75) ; 3.75 m is: 80 MHz Dig Rec sample rate dec by 2 to 40 MHz


							display={ $
									r:r, $ ; range gate range vector
									thet:st.display.thet, $ ; M element display angle (az or el)
									thet_last:st.display.thet_last, $
								 	dispdata:dispdata, $		; active data matrix on display nrg x M matrix
									fname:st.display.fname, $
									bsize:st.display.bsize, $ ; display update after reading bsize number of records
									az_old:st.display.az_old, $ ; used to display pedestal position in pos window
									el_old:st.display.el_old,  $ ; used to display pedestal position in pos window
									azvel_old:st.display.azvel_old, $ ; used to display pedestal position in pos window
									elvel_old:st.display.elvel_old,  $ ; used to display pedestal position in pos window
									ant_bw:st.display.ant_bw, $ ; radar antenna 3 dB beamwidth
									n_beams:st.display.n_beams $ ; number of azimuth bins used in display
									}


							st.wi.ctmax_val(3:4)=[  0.03/(4*head.pri1*1E-6),  0.03/(4.*head.pri1*1E-6)]
							st.wi.ctmin_val(3:4)=[ -0.03/(4*head.pri1*1E-6), -0.03/(4.*head.pri1*1E-6)]

	  						WIDGET_CONTROL, st.wi.ctmaxID, SET_VALUE=string(st.wi.ctmax_val(st.wi.parameter_index), FORMAT='(F5.1)')
	  						WIDGET_CONTROL, st.wi.ctminID, SET_VALUE=string(st.wi.ctmin_val(st.wi.parameter_index), FORMAT='(F5.1)')


							;print, serv_mode(head.servmode), ', ', rec_raw(head.recraw), ', ',  sum_power(head.sumpower), ', ', pos_scan(head2.scan_type)

							t=st.display.fname
							WIDGET_CONTROL, st.wi.rstat51ID, SET_VALUE=t


							t='Server Time: ' + systime(0, stat.host_ts(0)) + string(stat.host_ts(1)) + ' us'
							WIDGET_CONTROL, st.wi.rstat1ID, SET_VALUE=t

							t=	'RF: ' + 	strmid(strtrim(string((st.stat.rcb_temp(0)-282)/6.214),2),0, 5) + $
								',  PC: '  + strmid(strtrim(string((st.stat.rcb_temp(1)-282)/6.214),2),0, 5) + $
								',  Out: ' + strmid(strtrim(string((st.stat.rcb_temp(2)-282)/6.214),2),0, 5) + $
								',  AC: ' +  strmid(strtrim(string((st.stat.rcb_temp(3)-282)/6.214),2),0, 5) + $
													 ' C'
							WIDGET_CONTROL, st.wi.rstat2ID, SET_VALUE=t

								slope=0.085
								offset_pitch=-38.42
								offset_roll=-37.99

								pitch=st.stat.rcb_incl(1)*slope+offset_pitch
								roll=st.stat.rcb_incl(0)*slope+offset_roll

							t=  'Pitch: ' + STRMID(STRTRIM(STRING(pitch), 2), 0, 3) + $
								',  Roll: ' + STRMID(STRTRIM(STRING(roll), 2), 0, 3) + ' (deg.),'
							WIDGET_CONTROL, st.wi.rstat3ID, SET_VALUE=t

							t='GPS Time: ' +  systime(0, rec_head.gps_ts(0)) + string(rec_head.gps_ts(1)) + ' us'
							WIDGET_CONTROL, st.wi.rstat4ID, SET_VALUE=t

							t= string(gps2.head)+ '  ' + string(gps1.lat) + '  ' + string(gps1.lon)
							WIDGET_CONTROL, st.wi.rstat5ID, SET_VALUE=t


							t=  'Radar Mode: '+serv_mode(head.servmode)
							IF head.servmode LE 1 THEN t = t + ', ' + sum_power(head.sumpower) + ' ' + rec_raw(head.recraw)
							IF head.servmode GE 2 THEN t = t + ', FFT Length: ' + strtrim(head.fftl) + ', Taper: '+ fft_taper(head.fftwindow) + ' ' + rec_raw(head.recraw)
							t = t + ', Clutter Filtering: '
                            IF head.cfon EQ 0 THEN t = t + ' Off'
 							IF head.cfon EQ 1 THEN t = t + strmid(strtrim(head.cfw, 2), 0, 3) + ' m/s'
							WIDGET_CONTROL, st.wi.rstat6ID, SET_VALUE=t

							t=  'Range Gates: '+ strtrim(head.nrg,2) + ', ' + strmid(strtrim(head.rres,2), 0, 3) + ' m res., ' + strtrim(FIX(head.bw), 2) + ' MHz BW, ' + $
												strtrim(FIX(head.rgs),2) + ' m spacing, ' + strmid(strtrim(head.rmax, 2), 0, 3) + ' km range'
							WIDGET_CONTROL, st.wi.rstat7ID, SET_VALUE=t

							t=  'PRF: '+ strtrim(head.pri1,2) + ' us, '
							 IF head.servmode EQ 1 OR head.servmode GE 3 THEN t = t + strtrim(head.pri2,2) + ' us, '
							t = t + strtrim(head.ave,2) + ' ave., ' + strmid(strtrim(head.inttime,2), 0, 4) + ' s int. time'
							WIDGET_CONTROL, st.wi.rstat8ID, SET_VALUE=t

                            t=  'Pos: ' + pos_scan(head2.scan_type) ;+ ', Az Offset: ' + strmid(strtrim(st.head.az_off, 2), 0, 5)
							WIDGET_CONTROL, st.wi.rstat9ID, SET_VALUE=t

 							t= ' '
							WIDGET_CONTROL, st.wi.rstat10ID, SET_VALUE=t


							rec_head_size=st.data.rec_head_size


							Case head.servmode of
								0: begin ; PP
							    	record_size=long(rec_head_size+4*10*head.nrg); 4 power, 2 complex PP, 1 complex Cross-Correlation
							    	drec = { $
								        pow: fltarr(head.nrg, 4), $ ; nrg Vch 1st pulse, nrg Vch 2nd pulse, nrg Hch 1st pulse, nrg Hch 2nd pulse
								        pp: complexarr(head.nrg, 2), $ ; nrg Vch pulse pair, nrg Hch pulse pair
								        cc: complexarr(head.nrg) $ ; nrg V-H cross correlation (averaged 1st and 2nd pulse data)
							           }
							    	IF head.sumpower EQ 1 THEN BEGIN
							    		record_size=long(rec_head_size+4*8*head.nrg); 2 power, 2 complex pp, 1 complex cc
										drec = { $
									        pow: fltarr(head.nrg, 2), $
									        pp: complexarr(head.nrg, 2), $
									        cc: complexarr(head.nrg) $
							           		}
									ENDIF

									IF head.recraw EQ 1 THEN BEGIN
							        	record_size=long(rec_head_size+4*2*2*head.nrg*2); V/H, 2 pulses, rg, I/Q
							      		drec = { $
								            V: complexarr(head.nrg, 2), $
								            H: complexarr(head.nrg, 2) $
							              }
							      	ENDIF
								end
								1: begin ; DPP
								    record_size=long(rec_head_size+4*16*head.nrg); 6 power, 4 complex PP, 1 complex Cross-Correlation
								    drec = { $
								        pow: fltarr(head.nrg, 6), $
								        pp: complexarr(head.nrg, 4), $
								        cc: complexarr(head.nrg) $
							           }
							    	IF head.sumpower EQ 1 THEN BEGIN
							    		record_size=long(rec_head_size+4*8*head.nrg); 2 power, 2 complex pp, 1 complex cc
										drec = { $
									        pow: fltarr(head.nrg, 2), $
									        pp: complexarr(head.nrg, 2), $
									        cc: complexarr(head.nrg) $
							           		}
									ENDIF
									IF head.recraw EQ 1 THEN BEGIN
								        record_size=long(rec_head_size+4*2*3*head.nrg*2); V/H, all ave, 3 pulses, rg, I/Q
								      	drec = { $
								            V: complexarr(head.nrg, 3), $
								            H: complexarr(head.nrg, 3) $
								              }
							      	ENDIF

								end
								2: begin ; FFT
								    record_size=long(rec_head_size+4*4*head.nrg*head.fftl); 2 power spectrum, 1 complex cross-spectrum
								    drec = { $
									        pow: fltarr(head.fftl, head.nrg, 2), $
									        cc: complexarr(head.fftl, head.nrg) $
							           		}

								    IF head.recraw EQ 1 THEN BEGIN
								        record_size=long(rec_head_size+4*2*head.fftl*head.nrg*2); V/H, all ave, fftl pulses, rg, I/Q
								      	drec = { $
								            V: complexarr(head.nrg, head.fftl), $
								            H: complexarr(head.nrg, head.fftl) $
								              }
								    ENDIF
								end
								3: begin ; FFT2
								    record_size=long(rec_head_size+4*8*head.nrg*head.fftl); 4 power spectrum, 2 complex cross-spectrum
								    drec = { $
									        pow: fltarr(head.fftl, head.nrg, 4), $
									        cc: complexarr(head.fftl, head.nrg, 2) $
							           		}
							      	IF head.recraw EQ 1 THEN BEGIN
							        	record_size=long(rec_head_size+4*2*2*head.fftl*head.nrg*2); V/H, all ave, 2*fftl pulses, rg, I/Q
							      		drec = { $
								            V: complexarr(head.nrg, 2*head.fftl), $
								            H: complexarr(head.nrg, 2*head.fftl) $
								              }
							      	ENDIF
								end
								4: begin ; FFT2I
								    record_size=long(rec_head_size+4*8*head.nrg*head.fftl); 4 power spectrum, 2 complex cross-spectrum
								    drec = { $
									        pow: fltarr(head.fftl, head.nrg, 4), $
									        cc: complexarr(head.fftl, head.nrg, 2) $
							           		}
							      	IF head.recraw EQ 1 THEN BEGIN
							        	record_size=long(rec_head_size+4*2*2*head.fftl*head.nrg*2); V/H, all ave, 2*fftl pulses, rg, I/Q
							      		drec = { $
								            V: complexarr(head.nrg, 2*head.fftl), $
								            H: complexarr(head.nrg, 2*head.fftl) $
								              }
							      	ENDIF
								end
							ENDCASE

		data={ $	; data block
			file_head_size:st.data.file_head_size , $ ; file header size (fixed)
			rec_head_size:st.data.rec_head_size, $ ; record header size (fixed)
			record_size:record_size, $ ; total record size (depends on mode)
			nrec:st.data.nrec, $ ; number of records in file
			crec:st.data.crec, $ ; displayed record count
			v_noise:0., $ ; V channel noise floor
			h_noise:0., $ ; H channel noise floor
			drec:drec $ ; data record structure (depends on mode)
			}

		st.wi.display_index=pos_disp(scan_type); 'PPI', 'RHI', 'Range/Time', 'El/Az', 'Scope'

		WIDGET_CONTROL, st.wi.displayID, SET_DROPLIST_SELECT=st.wi.display_index
	st={wi:st.wi, head:st.head, head2:st.head2,  stat:st.stat, ped:st.ped, ped_current:st.ped_current, $
		 gps1:st.gps1, gps2:st.gps2, rec_head:st.rec_head, display:display, data_head:st.data_head, data:data} ;Stash
	return, st
END ; end process_configuration
;*****************************************************************************************************

FUNCTION get_socket_configuration, st ; This is via socket data header

	st.data.crec=0 ;initialize record counter
	st.data.nrec=st.display.bsize ; initialize display block size


	temp=0L
	counter=0 ;
	WHILE temp NE 66L and counter LT 10 DO BEGIN ; Try to get a network response from the server 10 times, then give up
		writeu, st.wi.source, 8L		; request (2L) configuration; (8L) configuration with status
		readu, st.wi.source, temp
			st.wi.netres=temp
			counter=counter+1
			wait, 0.2
	END

print, counter, st.wi.netres 	; read NETRES_OK (66 - a long integer) reply

	IF st.wi.netres EQ 66L THEN BEGIN
		readu, st.wi.source, temp ; read the total conf size (number of bytes) - should be 1324+8
			;print, 'Conf. Total Size: ', temp
		readu, st.wi.source, temp ; read the configuration archive index
			;print, 'Conf. Archive Index: ', temp
			st.wi.archive_index=temp   ;keep the archive index to read data records
		readu, st.wi.source, temp ; read conf size (number of bytes) - should be 1324
			;print, 'Conf. Size: ', temp

		readu, st.wi.source, temp ; read conf size (number of bytes) - should be 1324
			;print, 'Stat. Size: ', temp

		head=st.head
		readu, st.wi.source, head ; read the configuration structure
			;print, 'Head', head

		stat=st.stat
		readu, st.wi.source, stat
			;print, stat

		readu, st.wi.source, temp ; read NETRES_OK (66 - a long integer) reply

		st.wi.netres=temp
		IF temp EQ 66 THEN 	st={wi:st.wi, 	head:head, 	head2:st.head2, stat:stat, ped:st.ped, ped_current:st.ped_current, gps1:st.gps1, gps2:st.gps2, rec_head:st.rec_head, 	display:st.display, 	data_head:st.data_head, data:st.data} ;Stash

	ENDIF

 return, st
END
;************************************************************************

FUNCTION request_socket_data, st ; This is via socket data header

    writeu, st.wi.source, 7L  ; request data
    writeu, st.wi.source, 60L ; send the request structure size
    writeu, st.wi.source, 44L ; send the requestd data type
    writeu, st.wi.source, -1L ; get oldest available data block that we haven't yet seen
    writeu, st.wi.source, 1L  ; don't use 'block step length' feature
    writeu, st.wi.source, 1L  ; don't use 'block seen step length' feature
    writeu, st.wi.source, 1L  ; request 1 data block
    writeu, st.wi.source, long([0, 0, 0, 0])  ; request entire FFT matrix (meaning in FFT mode only)
    writeu, st.wi.source, long([-1, -1, -1, -1])  ; request entire FFT matrix (meaning in FFT mode only)
    writeu, st.wi.source, st.wi.archive_index ; send the presumed configuration archive index
    writeu, st.wi.source, 1L  ; 0=double precision data; 1=float data

 	return, st
END
;************************************************************************


FUNCTION get_stat, st ; This is via socket status block


	temp=0L
	writeu, st.wi.source, 4L  ; request status

	readu, st.wi.source, temp ; read initial response: NETRES_OK (66 - a long integer) reply
		;print, '1st netres in stat:  ', temp

	IF temp EQ	66 THEN BEGIN
			readu, st.wi.source, temp ; read total response size
				;print, 'Resp. Size:  ', temp ; Stat size is 72

			readu, st.wi.source, temp ; read status structure size
				;print, 'Size:  ', temp ; Stat size is 68
	 		stat=st.stat ;header structure block
	 		readu, st.wi.source, stat
	 		st={wi:st.wi, 	head:st.head, head2:st.head2,   stat:stat, ped:st.ped, ped_current:st.ped_current, gps1:st.gps1, gps2:st.gps2,	rec_head:st.rec_head, 	display:st.display, 	data_head:st.data_head, data:st.data} ;Stash
			readu, st.wi.source, temp ; read final status code (66 is OK)
	ENDIF

	st.wi.netres=temp


 return, st
END
;************************************************************************



; Event-handler routine
PRO disp_event, ev



	  ; The stash block of information is passed to the UVALUE of the base widget to carry along from
	  ;  the main program and from event to event.
	  WIDGET_CONTROL, ev.TOP, GET_UVALUE=st ;read in the Base widget UVALUE structure data in to stash.

	  ; Get the enevt ID to figure out which widget generated the event interupt.
	  WIDGET_CONTROL, ev.ID, GET_UVALUE=ev_uvalue
		close_answer = '' ; this is the answer of the end program dialog messagebox


	  CASE ev_uvalue OF
	  'draw_ev':BEGIN 	; draw area event (draw timer, mouse movement, click or keyboard)
		             	; if ev.type eq 0 then it was a mouse button press
		              	; if ev.type eq 1 then it was a mouse button release
		              	; if ev.type eq 2 then it was a mouse motion (not activated in the draw_widget command)
		              	; if ev.type eq 5 then it was a ASCII keyboard key press or release
		              	; IF ev.type eq 5 and ev.Press eq 1 then print, ev_uvalue, ev.CH ;Keyboard press

				IF (TAG_NAMES(ev, /STRUCTURE_NAME) EQ 'WIDGET_TIMER') THEN BEGIN ; Draw timer event

						data=st.data.drec
						crec=st.data.crec ; display record count
						nrec=st.data.nrec

						IF st.wi.ip_list(st.wi.ip_index) EQ 'File' THEN BEGIN  ; File Readback Mode - not socket
							;print, 'Reading data from ', st.display.fname

							; find out how many record are in the file
							result=fstat(st.wi.source)
							fsize=ulong64(strtrim(result.size,2))
							;nrec=long((fsize-st.data.file_head_size)/float(st.data.record_size))
							nrec=long(((fsize/1.024)-st.data.file_head_size)/float(st.data.record_size)) ; file size is 1.024 times larger on a Linux machine

							st.data.nrec=nrec

						ENDIF

						IF st.data.crec LT st.data.nrec-1 THEN BEGIN
							i=1

							WHILE i LE st.display.bsize<(st.data.nrec-st.data.crec) DO BEGIN

								IF st.wi.ip_list(st.wi.ip_index) EQ 'File' THEN BEGIN ; File Readback Mode - not socket
									rec_head=st.rec_head
									stat=st.stat
									ped=st.ped
									gps1=st.gps1
									gps2=st.gps2


									;sweep_count=0L
									;volume_count=0L
									;flags=0L 			; 2^0 bit is the antenna in transition flag

									readu, st.wi.source, stat ; read the status structure
										st.stat=stat
									readu, st.wi.source, ped ; read the pedestal structure
										st.ped=ped
									;readu, st.wi.source, sweep_count
									;readu, st.wi.source, volume_count
									;readu, st.wi.source, flags

									readu, st.wi.source, gps1 ; read the gps structure
										st.gps1=gps1
									readu, st.wi.source, gps2 ; read the gps structure
										st.gps2=gps2

									readu, st.wi.source, rec_head ; read the few extra parameters of a recorded record header
										;st.rec_head=rec_head

									readu, st.wi.source, data	; read the the data record



								ENDIF ELSE BEGIN ; Socket


								st=request_socket_data(st) ;

								temp=0L
								readu, st.wi.source, temp ; read initial response: NETRES_OK (66 - a long integer) reply
									st.wi.netres=temp

								CASE st.wi.netres OF
									66L: BEGIN ; OK
										readu, st.wi.source, temp ; read data structure size (240 + blocks*block size)

								 			data_head=st.data_head ;header structure block
										readu, st.wi.source, data_head

											ped=st.ped
										readu, st.wi.source, ped

											ped_current=st.ped_current ;this info is only in the socket data stream - not recorded in file
										readu, st.wi.source, ped_current

											gps1=st.gps1
										readu, st.wi.source, gps1

	  										alt=0D	; Altitude in meters
										readu, st.wi.source, alt ;altitude is only in the socket data stream

										gps2=st.gps2
										readu, st.wi.source, gps2


										st={wi:st.wi, 	head:st.head, head2:st.head2,   stat:st.stat, ped:ped, ped_current:ped_current, gps1:gps1, gps2:gps2,	rec_head:st.rec_head, 	display:st.display, 	data_head:data_head, data:st.data} ;Stash

	 									; read data
										data=st.data.drec
									 	readu, st.wi.source, data	; read the the data record

										readu, st.wi.source, temp ; read initial response: NETRES_OK (66 - a long integer) reply
										st.wi.netres=temp


										END
									67L: BEGIN ;
										WIDGET_CONTROL, st.wi.drawID, /CLEAR_EVENTS ; clear any existing timer events to the draw widget
										WIDGET_CONTROL, st.wi.displayID, /CLEAR_EVENTS ; clear any existing timer events to the draw widget
										WIDGET_CONTROL, st.wi.check_socket_data_readyID, /CLEAR_EVENTS ; clear any existing timer events to the draw widget

										WIDGET_CONTROL, st.wi.connectID, TIMER=1. ; read the server configuration again in 1 sec
										tv, intarr(228,13)
										xyouts, 0., 0. , "CHANGING CONFIGURATION", ALIGNMENT=0., COLOR=200, charsiz=1.4, charthick=1.5, /NORMAL
										i = (st.display.bsize<(st.data.nrec-st.data.crec)) + 1 ; to get out of this WHILE loop
										END

									70L: BEGIN ;
										print, 'Unknown data type!'
										END
									72L: BEGIN ;
										;print, 'GET DATA: Requested data type is not available!'
										END
									73L: BEGIN ;
										WIDGET_CONTROL, st.wi.drawID, /CLEAR_EVENTS ; clear any existing timer events to the draw widget
										WIDGET_CONTROL, st.wi.displayID, /CLEAR_EVENTS ; clear any existing timer events to the draw widget
										WIDGET_CONTROL, st.wi.check_socket_data_readyID, /CLEAR_EVENTS ; clear any existing timer events to the draw widget
										tv, intarr(228,13)
										xyouts, 0., 0. , "NEW CONFIGURATION", ALIGNMENT=0., COLOR=200, charsiz=1.4, charthick=1.5, /NORMAL
										WIDGET_CONTROL, st.wi.connectID, TIMER=1. ; read the server configuration again in 1 sec

										i = (st.display.bsize<(st.data.nrec-st.data.crec)) + 1 ; to get out of this WHILE loop

										END
									ENDCASE


								ENDELSE

								IF st.wi.ip_list(st.wi.ip_index) EQ 'File' OR st.wi.netres EQ 66 THEN BEGIN


									nweight=0.1 ; noise running average weight
									i=i+1


									;print, i, st.data.crec, ped.el, ped.az, (stat.rcb_incl-446.9)*0.085

									;***********************
									st.head.hdBz=67.7
									st.head.vdBz=67.7
									n_noise_gates = 2
									;***********************



										CASE st.head.servmode OF
										    0: BEGIN ; PP
										          dBm=data.pow
										          pp = data.pp
										          cc = data.cc

														Velv=0.03*atan(pp(*,0), /phase)/(4.E-6*st.head.pri1*!pi) ;
														Velh=0.03*atan(pp(*,1), /phase)/(4.E-6*st.head.pri1*!pi) ;
														Phi=atan(cc, /phase)

													; Zv
														IF st.data.v_noise EQ 0. THEN st.data.v_noise = mean(dBm(0:n_noise_gates,0))


														v_noise=(1.-nweight)*st.data.v_noise+nweight*mean(dBm(0:n_noise_gates,0))

														st.data.v_noise=v_noise

															IF st.head.sumpower EQ 0 THEN thresv=v_noise*(10^(0.1*st.wi.thres_val))/SQRT(st.head.cave*st.head.postave)
															IF st.head.sumpower EQ 1 THEN thresv=v_noise*(10^(0.1*st.wi.thres_val))/SQRT(2*st.head.cave*st.head.postave)

										          		Zv=(dBm(*,0)-v_noise)>thresv

										          		Sv=0.03*SQRT((1-(ABS(pp(*,0))/Zv))>0.)/(2*!pi*SQRT(2)*1E-6*st.head.pri1)
										          		Sv(where(Zv EQ thresv)>0)=!values.F_nan

													; Zh
													IF st.head.sumpower EQ 1 THEN BEGIN
															IF st.data.h_noise EQ 0. THEN st.data.h_noise = mean(dBm(0:n_noise_gates,1))

															h_noise=(1.-nweight)*st.data.h_noise+nweight*mean(dBm(0:n_noise_gates,1))
															;h_noise=(1.-nweight)*st.data.h_noise+nweight*mean(dBm((st.head.nrg-5):(st.head.nrg-1),1))

														st.data.h_noise=h_noise
															thres=h_noise*(10^(0.1*st.wi.thres_val))/SQRT(2*st.head.cave*st.head.postave)
										          		Zh=(dBm(*,1)-h_noise)>thres

													ENDIF

										            IF st.head.sumpower EQ 0 THEN BEGIN
															IF st.data.h_noise EQ 0. THEN st.data.h_noise = mean(dBm(0:n_noise_gates,2))

										            		h_noise=(1.-nweight)*st.data.h_noise+nweight*mean(dBm(0:n_noise_gates,2))
										            		;h_noise=(1.-nweight)*st.data.h_noise+nweight*mean(dBm((st.head.nrg-5):(st.head.nrg-1),2))

														st.data.h_noise=h_noise
										            		thres=h_noise*(10^(0.1*st.wi.thres_val))/SQRT(st.head.cave*st.head.postave)
										            	Zh=(dBm(*,2)-h_noise)>thres
													 ENDIF


										            	Sh=0.03*SQRT((1-(ABS(pp(*,1))/Zh))>0.)/(2*!pi*SQRT(2)*1E-6*st.head.pri1)
										          		Sh(where(Zh EQ thres)>0)=!values.F_nan

										          		Rvh=abs(cc)/SQRT(Zh*Zv)
														Rvh(where(Zh EQ thres OR Zv EQ thresv)>0)=!values.F_nan

														Zv(where(Zv EQ thresv)>0)=!values.F_nan
										          		dBZv=10*alog10(Zv)+20*alog10(st.display.r>1e-1)-10*alog10(st.head.rres)+st.head.vdBz;st.head.vdbz-(v_noise-st.head.hnoisedbm)
														;dBZv=10*alog10(Zv)-10*alog10(st.head.rres)+st.head.vdBz;st.head.vdbz-(v_noise-st.head.hnoisedbm)

										          		Zh(where(Zh EQ thres)>0)=!values.F_nan
										          		dBZh=10*alog10(Zh)+20*alog10(st.display.r>1e-1)-10*alog10(st.head.rres)+st.head.hdBz;st.head.hdbz-(h_noise-st.head.hnoisedbm)



										    END
										    1: BEGIN ; DPP
													dBm=data.pow
										          	pp = data.pp
										          	cc = data.cc

													Velv=0.03*atan(pp(*,0), /phase)/(4.E-6*st.head.pri1*!pi) ;first pulse pair velocity
													Velv2=0.03*atan(pp(*,1), /phase)/(4.E-6*st.head.pri2*!pi) ;second pulse pair velocity

													Velh=0.03*atan(pp(*,2), /phase)/(4.E-6*st.head.pri1*!pi) ;
													Velh2=0.03*atan(pp(*,3), /phase)/(4.E-6*st.head.pri2*!pi) ;

													Phi=atan(cc, /phase)

													; Zv
														v_noise=(1.-nweight)*st.data.v_noise+nweight*mean(dBm(0:n_noise_gates,0))
														st.data.v_noise=v_noise
															IF st.head.sumpower EQ 0 THEN thresv=v_noise*(10^(0.1*st.wi.thres_val))/SQRT(st.head.cave*st.head.postave)
															IF st.head.sumpower EQ 1 THEN thresv=v_noise*(10^(0.1*st.wi.thres_val))/SQRT(3*st.head.cave*st.head.postave)

										          		Zv=(dBm(*,0)-v_noise)>thresv

														Sv=0.03*SQRT((1-(ABS(pp(*,0))/Zv))>0.)/(2*!pi*SQRT(2)*1E-6*st.head.pri1)
														Sv(where(Zv EQ thresv)>0)=!values.F_nan


													; Zh
													IF st.head.sumpower EQ 1 THEN BEGIN
															h_noise=(1.-nweight)*st.data.h_noise+nweight*mean(dBm(0:n_noise_gates,1))
														st.data.h_noise=h_noise
															thres=h_noise*(10^(0.1*st.wi.thres_val))/SQRT(3*st.head.cave*st.head.postave)
										          		Zh=(dBm(*,1)-h_noise)>thres

														Sh=0.03*SQRT((1-(ABS(pp(*,2))/Zh))>0.)/(2*!pi*SQRT(2)*1E-6*st.head.pri1)
										          		Sh(where(Zh EQ thres)>0)=!values.F_nan

										          		Rvh=abs(cc)/SQRT(Zh*Zv)
														Rvh(where(Zh EQ thres OR Zv EQ thresv)>0)=!values.F_nan

										          		Zv(where(Zv EQ thresv)>0)=!values.F_nan
										          		dBZv=10*alog10(Zv)+20*alog10(st.display.r>1e-20)-10*alog10(st.head.rres)+st.head.vdBz;st.head.vdbz-(v_noise-st.head.hnoisedbm)

										          		Zh(where(Zh EQ thres)>0)=!values.F_nan
										          		dBZh=10*alog10(Zh)+20*alog10(st.display.r>1e-20)-10*alog10(st.head.rres)+st.head.hdBz; st.head.hdbz-(h_noise-st.head.hnoisedbm)
										       		ENDIF
										            IF st.head.sumpower EQ 0 THEN BEGIN
										            		h_noise=(1.-nweight)*st.data.h_noise+nweight*mean(dBm(0:n_noise_gates,3))
														st.data.h_noise=h_noise
															thres=h_noise*(10^(0.1*st.wi.thres_val))/SQRT(st.head.cave*st.head.postave)
										            	Zh=(dBm(*,3)-h_noise)>thres

										            	Sh=0.03*SQRT((1-(ABS(pp(*,2))/Zh))>0.)/(2*!pi*SQRT(2)*1E-6*st.head.pri1)
										          		Sh(where(Zh EQ thres)>0)=!values.F_nan

										          		Rvh=abs(cc)/SQRT(Zh*Zv)
														Rvh(where(Zh EQ thres OR Zv EQ thres)>0)=!values.F_nan

										          		Zv(where(Zv EQ thres)>0)=!values.F_nan
										          		dBZv=10*alog10(Zv)+20*alog10(st.display.r>1e-20)-10*alog10(st.head.rres)+st.head.vdBz;st.head.vdbz-(v_noise-st.head.hnoisedbm)

										          		Zh(where(Zh EQ thres)>0)=!values.F_nan
										          		dBZh=10*alog10(Zh)+20*alog10(st.display.r>1e-20)-10*alog10(st.head.rres)+st.head.hdBz; st.head.hdbz-(h_noise-st.head.hnoisedbm)

										            ENDIF
										    END
										    2: BEGIN ; FFT



										    END
										    3: BEGIN ; FFT2



										    END
										    4: BEGIN ; FFT2I



											END

										    ENDCASE

											thet=ped.az ; PPI

											;thet=(ped.az+180.+gps2.head) MOD 360. 	; PPI with GPS correction:
																				; 180 pedestal zero is towards trear of truck
																				; GPS.head truck front direction relative to True North.



											IF st.wi.display_index EQ 1 THEN thet=ped.el+90 ; RHI display


												st.display.thet_last=thet
												h_index=floor(thet/st.display.ant_bw)	; index
												st.display.thet(h_index)=!pi*thet/180. 	; the actual angle of the data in this index


											IF st.wi.display_index EQ 2 THEN 	h_index=i-1 ;Range vs Time display



											; parameter list: ['dBZh', 'dBZv', 'Zdr', 'VelH', 'VelV', 'PHIvh', , 'STDh', 'STDv']
											st.display.dispdata(*, h_index, 0)= dBZh
											st.display.dispdata(*,h_index, 1)= dBZv

											st.display.dispdata(*,h_index, 2)= dBZh-dBZv ; Zdr

											st.display.dispdata(*,h_index, 3)= Velh ; Mean Doppler vel.
											st.display.dispdata(*,h_index, 4)= Velv ; Mean Doppler vel.

											st.display.dispdata(*,h_index, 5)= Phi ; Differential phase
											st.display.dispdata(*,h_index, 6)= Rvh ; VH correlation coefficient


											st.display.dispdata(*,h_index, 7)= Sh ; STD. Doppler vel.
											st.display.dispdata(*,h_index, 8)= Sv ; STD. Doppler vel.


								ENDIF ; file or netres=66 IF block
							END ; end while loop

							IF st.wi.ip_list(st.wi.ip_index) EQ 'File' OR st.wi.netres EQ 66 THEN BEGIN ; valid data

									IF st.wi.ip_list(st.wi.ip_index) NE 'File' THEN st=get_stat(st) ; get socket status message
									st.data.crec=st.data.crec+i-1

									; Write Info. in Status window:
									t=  'Server: ' + STRING(systime(0, st.stat.host_ts(0)));+'  '+STRING(stat.host_ts(1))+ ' us'
									WIDGET_CONTROL, st.wi.rstat1ID, SET_VALUE=t


										t=	'RF: ' + 	strmid(strtrim(string((st.stat.rcb_temp(0)-282)/6.214),2),0, 5) + $
											',  PC: '  + strmid(strtrim(string((st.stat.rcb_temp(1)-282)/6.214),2),0, 5) + $
											',  Out: ' + strmid(strtrim(string((st.stat.rcb_temp(2)-282)/6.214),2),0, 5) + $
											',  AC: ' +  strmid(strtrim(string((st.stat.rcb_temp(3)-282)/6.214),2),0, 5) + $
													 ' C'
										WIDGET_CONTROL, st.wi.rstat2ID, SET_VALUE=t


										slope=0.085
										offset_pitch=-38.42
										offset_roll=-37.99

										pitch=st.stat.rcb_incl(1)*slope+offset_pitch
										roll=st.stat.rcb_incl(0)*slope+offset_roll

									t=  'Pitch: ' + STRMID(STRTRIM(STRING(pitch), 2), 0, 3) + $
									        	',  Roll: ' + STRMID(STRTRIM(STRING(roll), 2), 0, 3) + ' (deg.),'
									WIDGET_CONTROL, st.wi.rstat3ID, SET_VALUE=t


										t='GPS Time: N/A over the socket'
									IF st.wi.ip_list(st.wi.ip_index) EQ 'File' THEN $
										t='GPS Time: ' +  systime(0, st.rec_head.gps_ts(0)) + string(st.rec_head.gps_ts(1)) + ' us'

									WIDGET_CONTROL, st.wi.rstat4ID, SET_VALUE=t

									t= 	 'Heading: ' + STRMID(STRTRIM(STRING(st.gps2.head), 2), 0, 5) + ' deg,  Pos.: '+ STRMID(STRTRIM(STRING(st.gps1.lat), 2),0,6) + '  ' + STRING(BYTE(st.gps1.lath)) + ',   ' + $
												 STRMID(STRTRIM(STRING(st.gps1.lon), 2),0, 6) + ' ' + STRING(BYTE(st.gps1.lonh))

									WIDGET_CONTROL, st.wi.rstat5ID, SET_VALUE=t

									pos_scan=[' ', ' ', ' ', 'Point,', ' ', 'PPI,', 'RHI,', 'Az Ras,', 'El Ras,', 'Vol,'] ; ; scan type 3=Point, 5=PPI, 6=RHI, 7=Az Ras, 8=El Ras, 9=Vol.
									t=  'Pos: ' + pos_scan(st.stat.scan_type) + ', Az: ' +  $
									strmid(strtrim(st.ped.az, 2), 0, 3) + ', ' + strmid(strtrim(st.ped.az_vel, 2), 0, 4) + ' deg/s' + $
									', El: ' + strmid(strtrim(st.ped.el, 2), 0, 3) + ', ' + strmid(strtrim(st.ped.el_vel, 2), 0, 4) + ' deg/s'


									WIDGET_CONTROL, st.wi.rstat9ID, SET_VALUE=t

									t=' '
									IF st.wi.ip_list(st.wi.ip_index) EQ 'File' THEN BEGIN
										t= 'File Size: ' + strtrim(FIX(st.data.nrec), 2) + ' records, Rec. Count: ' + strtrim(FIX(st.data.crec), 2)
									ENDIF
									WIDGET_CONTROL, st.wi.rstat10ID, SET_VALUE=t

								; Write pedestal info in pedestal window
									wset, st.wi.posdrawwinID

									plot, [-1,1], [-1,1], /nodata, /noerase, ystyle=4, xstyle=4, xmargin=[2,2], ymargin=[2,2] ;Establish scale in the pos window

									az_old=st.display.az_old
									el_old=st.display.el_old

									azvel_old=st.display.azvel_old
									elvel_old=st.display.elvel_old

									az=st.ped.az
									el=st.ped.el

									azvel=st.ped.az_vel
									elvel=st.ped.el_vel


									arrow, 0, 0, 0.95*sin(!pi*az_old/180.), 0.95*cos(!pi*az_old/180.), /DATA, thick=1, color=0
									arrow, 0, 0, 0.95*sin(!pi*az/180.), 0.95*cos(!pi*az/180.), /DATA, thick=1, color=250

									arrow, 0, 0, 0.95*cos(!pi*el_old/180.)*sin(!pi*az_old/180.), 0.95*cos(!pi*el_old/180.)*cos(!pi*az_old/180.), /DATA, COLOR=0, thick=3
									arrow, 0, 0, 0.95*cos(!pi*el/180.)*sin(!pi*az/180.), 0.95*cos(!pi*el/180.)*cos(!pi*az/180.), /DATA, COLOR=200, thick=3



									xyouts, -1.1, -1.2,"Az:"+  strmid(string(0.1*round(az_old*10.       )),2+(az_old LT 0)+(abs(az_old       ) GE 1)+(abs(az_old       ) GE 10)+(abs(az_old    ) GE 100), 6-(az_old LT 0)), ALIGNMENT=0.0, COLOR=0
									xyouts, 1.1, -1.2, "El:"+  strmid(string(0.1*round(el_old*10.       )),2+(el_old LT 0)+(abs(el_old       ) GE 1)+(abs(el_old       ) GE 10)+(abs(el_old    ) GE 100), 6-(el_old LT 0)), ALIGNMENT=1.0, COLOR=0
									xyouts, -1.1, 1.2, "Vel:"+ strmid(string(0.1*round(azvel_old*10.    )),2+(azvel_old LT 0)+(abs(azvel_old       ) GE 1)+(abs(azvel_old       ) GE 10), 6-(azvel_old LT 0)), ALIGNMENT=0.0, COLOR=0
									xyouts, 1.1, 1.2,          strmid(string(0.1*round(elvel_old*10.    )),2+(elvel_old LT 0)+(abs(elvel_old       ) GE 1)+(abs(elvel_old       ) GE 10), 6-(elvel_old LT 0)), ALIGNMENT=1.0, COLOR=0


									xyouts, -1.1, -1.2,"Az:"+  strmid(string(0.1*round(az*10.       )),2+(az LT 0)+(abs(az       ) GE 1)+(abs(az       ) GE 10)+(abs(az    ) GE 100), 6-(az LT 0)), ALIGNMENT=0.0, COLOR=255
									xyouts, 1.1, -1.2, "El:"+  strmid(string(0.1*round(el*10.       )),2+(el LT 0)+(abs(el       ) GE 1)+(abs(el       ) GE 10)+(abs(el    ) GE 100), 6-(el LT 0)), ALIGNMENT=1.0, COLOR=255
									xyouts, -1.1, 1.2, "Vel:"+ strmid(string(0.1*round(azvel*10.    )),2+(azvel LT 0)+(abs(azvel       ) GE 1)+(abs(azvel       ) GE 10), 6-(azvel LT 0)), ALIGNMENT=0.0, COLOR=255
									xyouts, 1.1, 1.2,          strmid(string(0.1*round(elvel*10.    )),2+(elvel LT 0)+(abs(elvel       ) GE 1)+(abs(elvel       ) GE 10), 6-(elvel LT 0)), ALIGNMENT=1.0, COLOR=255


									st.display.az_old=az
									st.display.el_old=el

									st.display.azvel_old=azvel
									st.display.elvel_old=elvel

									wset, st.wi.drawwinID


									WIDGET_CONTROL, st.wi.displayID, TIMER=0.0 ; display current data

									IF st.wi.ip_list(st.wi.ip_index) NE 'File' THEN BEGIN ; socket

											st.display.bsize=st.wi.newbsize_val

											st.data.crec=0
											st.data.nrec=st.display.bsize


											IF st.wi.pause_val EQ 0 THEN WIDGET_CONTROL, st.wi.drawID, TIMER=1.  ; read data

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


									ENDIF ; socket if block

									IF st.wi.ip_list(st.wi.ip_index) EQ 'File' THEN BEGIN ; file

										IF st.wi.pause_val EQ 0 THEN WIDGET_CONTROL, st.wi.drawID, TIMER=2 ; read another data block
										st.display.bsize=st.wi.newbsize_val


									ENDIF ; file if block

							ENDIF ; process data
						ENDIF ; of record count is less than record size
				ENDIF ; Timer event

			;********************************************************************************************************************************
	              	IF (TAG_NAMES(ev, /STRUCTURE_NAME) eq 'WIDGET_DRAW') THEN BEGIN ; Mouse or keyboard generated DRAW event
	                  ;print, ev.ID, ev.TOP, ev.HANDLER, ev.TYPE, ev.X, ev.Y, ev.PRESS, ev.RELEASE, ev.CLICKS, ev.MODIFIERS, ev.CH, ev.KEY
				  		IF ev.type eq 5 then WIDGET_CONTROL, st.wi.drawinfoID, SET_VALUE='' ; Keyboard press: clear data value and range text

	                 	IF ev.type eq 0 then begin ;Mouse press: print data value and range text
							drmax=st.head.rmax/st.wi.zoom_val
							N_range=drmax*(ev.Y-320.)/320. ; range in North direction
							E_range=drmax*(ev.X-300.)/300. ; range in East direction
							range= sqrt((E_range)^2+(N_range)^2)
							direction=180.*atan(E_range, N_range, /PHASE)/!pi  ; relative to North towards East
							IF direction LT 0 THEN direction=360+direction

			                t=STRING(st.wi.drawim(ev.X, ev.Y), FORMAT='(F9.3)') + ',   ' +$
			                       STRING(range, FORMAT='(F5.1)')+ ' km   ' +$
			                       STRING(direction, FORMAT='(F6.1)') + ' deg.'

							WIDGET_CONTROL, st.wi.drawinfoID, SET_VALUE=t
	                	ENDIF

	            	ENDIF

	   			END

	  'zoom_ev':BEGIN
	  				st.wi.zoom_val=ev.value
	  				WIDGET_CONTROL, st.wi.displayID, TIMER=0.0
	  			END

	  'hshift_ev':BEGIN
	  				st.wi.hshift_val=ev.value
	  				WIDGET_CONTROL, st.wi.displayID, TIMER=0.0
	  			END

	  'vshift_ev':BEGIN
	  				st.wi.vshift_val=ev.value
	  				WIDGET_CONTROL, st.wi.displayID, TIMER=0.0
	  			END

	  'axes_ev':BEGIN
	                st.wi.axes_val=ev.select
	  				WIDGET_CONTROL, st.wi.displayID, TIMER=0.0
	            END
	  'grid_ev':BEGIN
	                st.wi.grid_val=ev.select
	  				WIDGET_CONTROL, st.wi.displayID, TIMER=0.0
	            END

	  'units_ev':BEGIN
	  			END

	  'ctmax_ev':BEGIN
	  				WIDGET_CONTROL, st.wi.ctmaxID, GET_VALUE=temp
	  				st.wi.ctmax_val(st.wi.parameter_index)=float(temp)
	  				WIDGET_CONTROL, st.wi.ctmaxID, SET_VALUE=string(st.wi.ctmax_val(st.wi.parameter_index), FORMAT='(F5.1)')
	  				WIDGET_CONTROL, st.wi.displayID, TIMER=0.0
	            END
	  'ctmin_ev':BEGIN
	                WIDGET_CONTROL, st.wi.ctminID, GET_VALUE=temp
	  				st.wi.ctmin_val(st.wi.parameter_index)=float(temp)
	  				WIDGET_CONTROL, st.wi.ctminID, SET_VALUE=string(st.wi.ctmin_val(st.wi.parameter_index), FORMAT='(F5.1)')
	  				WIDGET_CONTROL, st.wi.displayID, TIMER=0.0
	            END
	  'display_ev':BEGIN

	  				IF TAG_NAMES(ev, /STRUCTURE_NAME) NE 'WIDGET_TIMER' THEN st.wi.display_index=ev.index

						IF st.wi.display_index LE 1 THEN BEGIN ; Polar Display: PPI or RHI
							drmax=float(st.head.rmax)/(0.9+0.1*st.wi.zoom_val) ; max displayed range w. gradual zoom
							; drmax=st.head.rmax/st.wi.zoom_val ; max displayed range

							; PPI Default:
								x0=0.01*st.wi.hshift_val*float(st.head.rmax)-drmax
								y0=0.01*st.wi.vshift_val*float(st.head.rmax)-drmax
								IF st.wi.display_index EQ 1 THEN y0=0.01*st.wi.vshift_val*st.head.rmax

								x1=x0+2.*drmax
								y1=y0+2.*drmax

								temp=float(st.display.dispdata(*,*,st.wi.parameter_index))

								;temp=100.*findgen(480,360)/(480.*360.) ; dummy image

								r_pos_ind=where(st.display.r GE 0)>0



                   			IF st.wi.display_index EQ 0 THEN $ ; PPI display

									;Change angles to Double precision if polar_surface does not execute properly!!!
								image=polar_surface(temp(where(st.display.r GE 0), *), float(st.display.r(where(st.display.r GE 0))), 2.5*!pi-float(st.display.thet), /GRID, $
								MISSING=!values.F_nan, spacing=[drmax/(300.), drmax/(300.)], bounds=[x0, y0, x1, y1]  )
								;MISSING=!values.F_nan, spacing=[drmax/(299.625), drmax/(319.75)], bounds=[x0, y0, x1, y1]  )

							IF st.wi.display_index EQ 1 THEN $ ; RHI display
								image=polar_surface(temp(where(st.display.r GE 0), *), float(st.display.r(where(st.display.r GE 0))), float(st.display.thet)-0.5*!pi, /GRID, $
								MISSING=!values.F_nan, spacing=[drmax/(300.), drmax/(300.)], bounds=[x0, y0, x1, y1]  )
								;MISSING=!values.F_nan, spacing=[drmax/(299.625), drmax/(319.75)], bounds=[x0, y0, x1, y1]  )


							st.wi.drawim=image ; save to use with mouse click identification of a pixel (to print value, range and direction)

							plot, [x0,x1], [y0,y1],  /nodata, xmargin=[0, 0], ymargin=[0, 0], color=255, xstyle=4, ystyle=4, $
								xtitle='South', ytitle='West', title='North', /noerase

								temp=FINITE(image)
								image(where(temp EQ 0)>0)=-1000. ; eliminate !values.NAN	otherwise 'Program caused arithmetic error: Floating illegal operand' is printed in bytscl()
							tv, bytscl(image, $
							        min=st.wi.ctmin_val(st.wi.parameter_index), max=st.wi.ctmax_val(st.wi.parameter_index))


							IF st.wi.axes_val EQ 1 THEN BEGIN
								axis, 0, xax=1, xrange=[x0, x1], xstyle=1, color=255, /data
								axis, 0, yax=1, yrange=[y0, y1], ystyle=1, color=255, /data
							ENDIF

							grid_spacing=10. ; km
							IF st.wi.display_index EQ 1 THEN grid_spacing=1. ; km
							IF st.wi.grid_val EQ 1 THEN BEGIN
								FOR i=1, 10 DO plot, xrange=[x0,x1], yrange=[y0,y1], xmargin=[0, 0], ymargin=[0, 0],  $
							            i*grid_spacing+findgen(100)*0., 2*!pi*findgen(100)/99., /polar,$
							            /noerase, color=255, ystyle=5, xstyle=5
							ENDIF

						IF st.wi.ip_index GT 0 AND st.head.servstate EQ 1 AND st.head.recenable EQ 0 THEN BEGIN
							tv, intarr(228,13)
							;print, 'SERVER STATE: ', st.head.servstate
							xyouts, 0., 0. , '!!! NOT RECORDING !!!', ALIGNMENT=0., COLOR=200, charsiz=1.4, charthick=1.5, /NORMAL
						ENDIF

						temperatures=[	(st.stat.rcb_temp(0)-282)/6.214, $
										(st.stat.rcb_temp(1)-282)/6.214]
										;(st.stat.rcb_temp(2)-282)/6.214]
										;(st.stat.rcb_temp(3)-282)/6.214]
						tv, intarr(228,13), 0.67, 0., /NORMAL
						IF MAX(temperatures) GT 45 THEN 	BEGIN
							temp=STRMID(STRTRIM(STRING(ROUND(MAX(temperatures))),2),0,3)
							xyouts, 1., 0. , 'TEMPERATURE WARNING: ' + temp, ALIGNMENT=1., COLOR=200, charsiz=1.4, charthick=1.5, /NORMAL
						ENDIF





						; This is a dummy plot to establish correct data axis range for arrows (does not look right without this)
						plot, xrange=[x0,x1], yrange=[y0,y1], xmargin=[0, 0], ymargin=[0, 0],  $
						1+findgen(10)*0., 2*!pi*findgen(10)/99., /polar,$
						/noerase, color=0, ystyle=5, xstyle=5



						IF st.wi.display_index EQ 0 THEN $ ; PPI display
						arrow, 0., 0., drmax*sin(!pi*st.display.thet_last/180.), drmax*cos(!pi*st.display.thet_last/180.), /DATA, thick=1, color=200, Hsize=0.

						IF st.wi.display_index EQ 1 THEN $ ; RHI display
						arrow, 0., 0., drmax*sin(!pi*(180.-st.display.thet_last)/180.), drmax*cos(!pi*(180.-st.display.thet_last)/180.), /DATA, thick=1, color=200, Hsize=0.


					ENDIF ; END Polar Display

					IF st.wi.display_index EQ 2 THEN BEGIN ; Range/Time display
						tv, bytscl(TRANSPOSE(st.display.dispdata(*,*,st.wi.parameter_index)), $
							min=st.wi.ctmin_val(st.wi.parameter_index), max=st.wi.ctmax_val(st.wi.parameter_index))
					ENDIF ; END Range/Time display



				END



	  'parameter_ev':BEGIN
	                ;print, ev_uvalue, ev.index

	                st.wi.parameter_index=ev.index

					; Color Table
						nexrad_ct 									;dBZ and power color table
						IF st.wi.parameter_index GE 2 AND st.wi.parameter_index LE 5 $
						    THEN doppler_ct 	;Doppler velocity and phase color table

						n_colors=ct_len()
						ct = bytarr(15, 165)
						FOR i = 0, 14 DO BEGIN
							ct(i, *) = bindgen(165)
						ENDFOR
						wset, st.wi.ctdrawwinID
						tv, bytscl(ct+1, min=0, max=165, TOP=n_colors-3), 0, 0
						wset, st.wi.drawwinID

					WIDGET_CONTROL, st.wi.drawtitleID, SET_VALUE=st.wi.parameter_list(st.wi.parameter_index)
					WIDGET_CONTROL, st.wi.unitID, SET_VALUE=st.wi.unit_list(st.wi.parameter_index)
		            WIDGET_CONTROL, st.wi.ctmaxID, SET_VALUE=STRING(st.wi.ctmax_val(st.wi.parameter_index), FORMAT='(F5.1)')
	  				WIDGET_CONTROL, st.wi.ctminID, SET_VALUE=STRING(st.wi.ctmin_val(st.wi.parameter_index), FORMAT='(F5.1)')

	                IF N_ELEMENTS(st.display.r) GT 2 THEN WIDGET_CONTROL, st.wi.displayID, TIMER=0.0

					CASE st.wi.parameter_index OF ;display parameter
					    0: st.wi.dtype=11 ; total summed H power
					    1: st.wi.dtype=10 ; total summed V power
					    2: st.wi.dtype=0  ; Zdr
					    3: st.wi.dtype=14 ; Vel H
					    4: st.wi.dtype=12 ; Vel V
					    5: st.wi.dtype=16 ; Phi VH
					    6: st.wi.dtype=16 ; Rho VH
					    7: st.wi.dtype=14	; Sh
					    8: st.wi.dtype=12	; Sv

					ENDCASE

	            END

	  'pause_ev':BEGIN
	                ;print, ev_uvalue, ev.select
	                WIDGET_CONTROL, st.wi.drawID, /CLEAR_EVENTS
	                WIDGET_CONTROL, st.wi.displayID, /CLEAR_EVENTS
	                ;if ev.select eq 1 then WIDGET_CONTROL, st.wi.drawID, TIMER=30.0
					st.wi.pause_val=1

	                IF (st.wi.ip_index EQ 0  OR st.wi.connect_val EQ 'Disconnect') AND ev.select EQ 0 THEN BEGIN
		                	WIDGET_CONTROL, st.wi.drawID, TIMER=0
		                	st.wi.pause_val=0
					ENDIF

	            END

	  'reset_ev':BEGIN
	                ;print, ev_uvalue, ev.select
	            END

	  'autoscale_ev':BEGIN

						temp=st.display.dispdata(*,*,st.wi.parameter_index)
	                st.wi.ctmax_val(st.wi.parameter_index)=max(temp(where(temp GT -100.)))
	                st.wi.ctmin_val(st.wi.parameter_index)=min(temp(where(temp GT -100.)))
	                WIDGET_CONTROL, st.wi.ctmaxID, SET_VALUE=string(st.wi.ctmax_val(st.wi.parameter_index), FORMAT='(F5.1)')
	           		WIDGET_CONTROL, st.wi.ctminID, SET_VALUE=string(st.wi.ctmin_val(st.wi.parameter_index), FORMAT='(F5.1)')

	                WIDGET_CONTROL, st.wi.displayID, TIMER=0.0
	            END

	  'thres_ev':BEGIN
					WIDGET_CONTROL, st.wi.thresID, GET_VALUE=temp
	                st.wi.thres_val=float(temp)
	                WIDGET_CONTROL, st.wi.thresID, SET_VALUE=string(st.wi.thres_val, FORMAT='(F4.1)')
				END

	  'newbsize_ev':BEGIN
					WIDGET_CONTROL, st.wi.newbsizeID, GET_VALUE=temp
	                st.wi.newbsize_val=Long(temp>1)
	                WIDGET_CONTROL, st.wi.newbsizeID, SET_VALUE=string(st.wi.newbsize_val, FORMAT='(I3)')
				END

	  'ip_ev':BEGIN
	                ;print, ev_uvalue, ev.index

					IF ev.index GE 0 THEN BEGIN ; File or an IP address was selected from the pull down menu
						IF ev.index NE st.wi.ip_index THEN BEGIN ; if the selction is new then ...
							close, st.wi.source ; close whatever was open (works even if nothing was open)
							st.display.fname='' ; clear the file name
							WIDGET_CONTROL, st.wi.drawID, /CLEAR_EVENTS ; clear any existing timer events to the draw widget

		                	st.wi.ip_index=ev.index ; remember the new selection index
		                	st.wi.connect_val='OPEN' ; change the connect button lable
							IF st.wi.ip_index GT 0 THEN st.wi.connect_val='Connect'
							WIDGET_CONTROL, st.wi.connectID, SET_VALUE=st.wi.connect_val ; change the connect button lable
						ENDIF
					ENDIF ELSE BEGIN ; a new IP address was entered
						IF ev.index GT 0 THEN BEGIN ; One of the IP addresses were changed
							st.wi.ip_list(st.wi.ip_index)=ev.STR
							WIDGET_CONTROL, st.wi.ipID, SET_VALUE = st.wi.ip_list
							WIDGET_CONTROL, st.wi.ipID, SET_COMBOBOX_SELECT = st.wi.ip_index
						ENDIF
					ENDELSE
	            END

	  'posstatus_ev':BEGIN

	                IF ev.select EQ 0 THEN WIDGET_CONTROL, st.wi.pos_baseID,MAP=0

	                IF ev.select EQ 1 THEN BEGIN
	                	WIDGET_CONTROL, st.wi.pos_baseID,MAP=1
		                wset, st.wi.posdrawwinID
						plot, [0,0], [0,0], /nodata, ystyle=4, xstyle=4 ;Clear "white" the draw area
						a=2.*!pi*findgen(361)/360.
						x=sin(a)
						y=cos(a)

						plot, x,y, xstyle=4, ystyle=4, xmargin=[2,2], ymargin=[2,2], COLOR=110

						xyouts, 0, 1.06, "R", ALIGNMENT=0.5, COLOR=200
						xyouts, 1.1, -0.06, "D", ALIGNMENT=0.5, COLOR=200
						xyouts, 0, -1.2, "F", ALIGNMENT=0.5, COLOR=200
						xyouts, -1.1, -0.06, "P", ALIGNMENT=0.5, COLOR=200

						az=st.display.az_old
						el=st.display.el_old

						azvel=st.display.azvel_old
						elvel=st.display.elvel_old

						arrow, 0, 0, 0.95*sin(!pi*az/180.), 0.95*cos(!pi*az/180.), /DATA, thick=1, color=250
						arrow, 0, 0, 0.95*cos(!pi*el/180.)*sin(!pi*az/180.), 0.95*cos(!pi*el/180.)*cos(!pi*az/180.), /DATA, COLOR=200, thick=3

						xyouts, -1.1, -1.2,"Az:"+  strmid(string(0.1*round(az*10.       )),2+(az LT 0)+(abs(az       ) GE 1)+(abs(az       ) GE 10)+(abs(az    ) GE 100), 6-(az LT 0)), ALIGNMENT=0.0, COLOR=255
						xyouts, 1.1, -1.2, "El:"+  strmid(string(0.1*round(el*10.       )),2+(el LT 0)+(abs(el       ) GE 1)+(abs(el       ) GE 10)+(abs(el    ) GE 100), 6-(el LT 0)), ALIGNMENT=1.0, COLOR=255
						xyouts, -1.1, 1.2, "Vel:"+ strmid(string(0.1*round(azvel*10.    )),2+(azvel LT 0)+(abs(azvel       ) GE 1)+(abs(azvel       ) GE 10), 6-(azvel LT 0)), ALIGNMENT=0.0, COLOR=255
						xyouts, 1.1, 1.2,          strmid(string(0.1*round(elvel*10.    )),2+(elvel LT 0)+(abs(elvel       ) GE 1)+(abs(elvel       ) GE 10), 6-(elvel LT 0)), ALIGNMENT=1.0, COLOR=255

						wset, st.wi.drawwinID
					ENDIF
	            END

	  'stat_ev':BEGIN ;
					IF ev.select EQ 0 THEN WIDGET_CONTROL, st.wi.stat_baseID,MAP=0
	                IF ev.select EQ 1 THEN WIDGET_CONTROL, st.wi.stat_baseID,MAP=1

	            END

	  'check_socket_data_ready_ev' : BEGIN ; This is via socket data header

				END

	  'connect_ev':BEGIN
	                ;print, ev_uvalue, ev.select, st.wi.ip_index, st.wi.connect_val
					fname='' ; reset filename
					st.wi.netres=0 ; reset socket configuration response status massage

					; Socket
	                IF st.wi.ip_index GT 0  THEN BEGIN ; socket (connect to or disconnect from a server)

						;Button Disconnect from Socket
	                	IF TAG_NAMES(ev, /STRUCTURE_NAME) EQ 'WIDGET_BUTTON' AND st.wi.connect_val EQ 'Disconnect' THEN BEGIN ; Disconnect from Server if the button was pushed (not timed attempt to connect)
	                		st.wi.connect_val='Connect' ; change button label_string to "Connect"
	                		WIDGET_CONTROL, st.wi.connectID, SET_VALUE=st.wi.connect_val ; apply write button label_string on button
	                		;WIDGET_CONTROL, st.ID.servmessID, SET_VALUE='Not Connected'
	                		close, st.wi.source

							FREE_LUN, st.wi.source

							WIDGET_CONTROL, st.wi.drawID, /CLEAR_EVENTS ; clear any existing timer events to the draw widget
							WIDGET_CONTROL, st.wi.displayID, /CLEAR_EVENTS ; clear any existing timer events to the display

							wait, 1.5*st.head.inttime*st.display.bsize	; this is needed with a delayed interrupt call
	                		WIDGET_CONTROL, st.wi.check_socket_data_readyID, /CLEAR_EVENTS ; clear any existing timer events to the check_socket_data_ready


						; Button or Timed Connect to Socket
						ENDIF ELSE BEGIN ; Connect to Server


							IF TAG_NAMES(ev, /STRUCTURE_NAME) EQ 'WIDGET_BUTTON' THEN BEGIN
								close, st.wi.source ; close whatever was open (file or socket)

								FREE_LUN, st.wi.source

								st.wi.connect_val='Disconnect'
								WIDGET_CONTROL, st.wi.connectID, SET_VALUE=st.wi.connect_val

								socket, source, st.wi.ip_list(st.wi.ip_index), 3000, /GET_LUN, READ_TIMEOUT=5 ; Open a socket to an IP address and Port
								st.wi.source=source

							ENDIF

							st=get_socket_configuration(st) ; Get configuration structure from server


							CASE st.wi.netres OF
								66: BEGIN

									st.head2.scan_type=st.stat.scan_type

									st.data.crec=0
									st.data.nrec=st.data.crec+st.display.bsize ; use nrec to space the polar plot of the data

									st={wi:st.wi, head:st.head, head2:st.head2,  stat:st.stat, ped:st.ped, ped_current:st.ped_current, gps1:st.gps1, gps2:st.gps2, rec_head:st.rec_head, display:st.display, data_head:st.data_head, data:st.data} ;Stash

									st=process_configuration(st)

		 							IF st.head.servstate NE 0 THEN BEGIN
		 								WIDGET_CONTROL, st.wi.drawID, TIMER=0  ; read data
									ENDIF

									IF st.head.servstate EQ 0 THEN BEGIN
										WIDGET_CONTROL, st.wi.connectID, TIMER=1. ; read the server configuration again in 1 sec
										tv, intarr(228,13)
										xyouts, 0., 0. , "IDLE", ALIGNMENT=0., COLOR=200, charsiz=1.4, charthick=1.5, /NORMAL
									ENDIF
								END
								67: BEGIN
									WIDGET_CONTROL, st.wi.connectID, TIMER=1. ; read the server configuration again in 1 sec
									tv, intarr(228,13)
									xyouts, 0., 0. , "CHANGING CONFIGURATION", ALIGNMENT=0., COLOR=200, charsiz=1.4, charthick=1.5, /NORMAL
								END
							ENDCASE

						ENDELSE
					ENDIF ELSE BEGIN ; Open a file
						WIDGET_CONTROL, st.wi.drawID, /CLEAR_EVENTS ; clear any existing timer events to the draw widget
						close, st.wi.source ; close whatever was open (file or socket)

						;***********************************************************
						; Set Path Here!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

								; Use this with a Windows machnine
									;cd, 'E:\RaXpol data\'
									;fpath='E:\RaXpol data\'

									;cd,  'C:\Documents and Settings\Andrew L. Pazmany\My Documents\Work\X-band\RaXpol\OU\IDL'
									;fpath='C:\Documents and Settings\Andrew L. Pazmany\My Documents\Work\X-band\RaXpol\OU\IDL\'



								; Use this with a Linux machine
									cd, st.wi.fpath
									fpath=st.wi.fpath





						;***********************************************************

						temp=fpath

						fname=dialog_pickfile(path=fpath,/read,filter="XPOL-20*.dat",/must_exist,$
                 			title="Choose data file")

						st.wi.fpath=strmid(fname, 0, strpos(fname, 'XPOL-20'))
                 		cd, st.wi.fpath


                 		IF STRLEN(fname) GT 0 THEN BEGIN
							st.display.fname=fname ; save file name in stash
							st.data.crec=0 ; reset record counter

								head=st.head
								head2=st.head2
								stat=st.stat



							openr, source, fname, /GET_LUN
							st.wi.source=source ; save source number in stash to close it later

							readu,source, head ; read the file header

							readu,source, head2 ; read the file header
								scan_type=head2.scan_type
								stat.scan_type=scan_type

							st={wi:st.wi, head:head, head2:head2,  stat:stat, ped:st.ped, ped_current:st.ped_current, gps1:st.gps1, gps2:st.gps2, rec_head:st.rec_head, display:st.display, data_head:st.data_head, data:st.data} ;Stash
							st=process_configuration(st)

							st.display.bsize=st.wi.newbsize_val

 							WIDGET_CONTROL, st.wi.drawID, TIMER=0. ; start getting data in draw_ev section

						ENDIF ; filename is not a null string
					ENDELSE ; end file section
	            END

	  'close_ev':BEGIN
					close_answer=DIALOG_MESSAGE('Are you sure you want to end the diplay program?', /QUESTION)
				END
	  ENDCASE

	; Write the stash structure into the Base widget UVALUE to cary it to the next event call.
	  WIDGET_CONTROL, ev.TOP, SET_UVALUE=st

	  IF ev_uvalue EQ 'close_ev' AND close_answer EQ 'Yes' THEN WIDGET_CONTROL, ev.TOP, /DESTROY ; this line must be the last!!!

END


PRO ucar_disp1

	; Define the values for the droplist widget and define the
	; initially selected index to show a shaded surface.

	;fpath='.'
	fpath='C:\Documents and Settings\Andrew L. Pazmany\My Documents\Work\X-band\UCAR\Data'
		cd, fpath

	drawim=fltarr(601,641)
	;display_list = ['PPI', 'RHI', 'Range/Time'] ;View list
	display_list = ['PPI', 'RHI'] ;View list

	display_index = 0 ;select the index of the view_list to start with

	parameter_list =['Zh', 'Zv', 'Zdr', 'Dh', 'Dv', 'Pvh', 'Rvh', 'Sh', 'Sv'] ; Group list - this will change depending on the radar operating mode (SPD, SPP ...)
	parameter_index=1

	unit_list= 		['dBZ',  'dBZ',  'dB',  'm/s',  'm/s',  'rad', ' ', 'm/s',  'm/s']

	hshift_val=0
	vshift_val=0
	zoom_val=1

	axes_val=0
	grid_val=0

	newbsize_val=20
	ctmax_val=[ 75.,  75.,  5.,  10.,  10.,  !pi,  1.0, 2, 2]
	ctmin_val=[-30., -30., -5., -10., -10., -1*!pi, 0.9, 0, 0]
	pause_val=0


	; UCAR Xpol = 192.168.85.78    -  port 3000

	;					RaXpol
	ip_list=['File', '10.1.1.2', '192.168.85.78', ' ', ' ', ' ', ' ']
	ip_index=0 ; Start-up data source
	source=1


	mind_val= -80  ;minimum value startup
	maxd_val= 0    ;maximum value startup

	mind_min=-100   ; min value minimum
	mind_max= 100    ; min value maximum

	maxd_min=-100    ; max_range minimum
	maxd_max= 100     ; max_range maximum

	thres_val=0.0    ; threshold startup

	minr_val=0.     ; min range startup
	maxr_val=60.    ; max range startup

	minr_min=0.      ; min_range minimum
	minr_max=100.    ; min_range maximum

	maxr_min=1.      ; max_range minimum
	maxr_max=300.    ; max_range maximum


	max_group=3


	  ; Create a base widget containing a draw widget and a sub-base
	  ; containing a droplist menu and a label widget
	  base = WIDGET_BASE(/COLUMN, Title='Xpol Display') ; Main base window
		base0 = WIDGET_BASE(base, /ROW)  ; Top row - with main image area and right sidebar
	    	draw = WIDGET_DRAW(base0, XSIZE=600, YSIZE=600, $
	             /BUTTON_EVENTS, /ALIGN_TOP, KEYBOARD_EVENTS=1, uvalue='draw_ev') ; draw area (center, below draw label)

			base0R = WIDGET_BASE(base0, /COLUMN, /ALIGN_LEFT)  ; Side bar
				drawtitle = WIDGET_LABEL(base0R, VALUE=parameter_list(parameter_index), /DYNAMIC_RESIZE, /ALIGN_CENTER, xsize=20) ;draw area label (top center)
           		unit = WIDGET_LABEL(base0R, VALUE=unit_list(parameter_index), UVALUE='unit_ev')


				ctmax = WIDGET_TEXT(base0R, VALUE=string(ctmax_val(parameter_index), FORMAT='(F5.1)'), uvalue='ctmax_ev', /ALIGN_CENTER, /EDITABLE, XSIZE=4)
				ctdraw = WIDGET_DRAW(base0R, XSIZE=15, YSIZE=145, /ALIGN_CENTER)
				ctmin = WIDGET_TEXT(base0R, VALUE=string(ctmin_val(parameter_index), FORMAT='(F5.1)'), uvalue='ctmin_ev', /ALIGN_CENTER, /EDITABLE, XSIZE=4)

				autoscale = WIDGET_BUTTON(base0R, XSIZE=40, /ALIGN_CENTER, VALUE='Auto', uvalue='autoscale_ev')

				dummy = WIDGET_LABEL(base0R, VALUE='')

				base_thres = WIDGET_BASE(base0R, /COLUMN)
						dummy = WIDGET_LABEL(base_thres, VALUE='Thres')
						thres = WIDGET_TEXT(base_thres, VALUE=string(thres_val, FORMAT='(F5.1)'), uvalue='thres_ev', /ALIGN_CENTER, /EDITABLE, XSIZE=4)

				dummy = WIDGET_LABEL(base0R, VALUE='')

				base_zoom = WIDGET_BASE(base0R, /COLUMN)
					dummy = WIDGET_LABEL(base_zoom, VALUE='+  ', /ALIGN_RIGHT)
					zoom=WIDGET_SLIDER(base_zoom, /VERTICAL, TITLE='', /ALIGN_LEFT, UVALUE='zoom_ev', MINIMUM=1, MAXIMUM=99, VALUE=1)
					dummy = WIDGET_LABEL(base_zoom, VALUE='_  ', /ALIGN_RIGHT)

				dummy = WIDGET_LABEL(base0R, VALUE='')

				base_vshift = WIDGET_BASE(base0R, /COLUMN)
					dummy = WIDGET_LABEL(base_vshift, VALUE='^  ', /ALIGN_RIGHT)
					vshift=WIDGET_SLIDER(base_vshift, /VERTICAL, /ALIGN_LEFT, TITLE='', UVALUE='vshift_ev', MINIMUM=-99, MAXIMUM=99, VALUE=0.)
					dummy = WIDGET_LABEL(base_vshift, VALUE='v  ', /ALIGN_RIGHT)

				base_pause = WIDGET_BASE(base0R, /COLUMN)
						dummy = WIDGET_LABEL(base_pause, VALUE='Pause', /ALIGN_CENTER)
			    		pause_base = Widget_Base(base_pause, UNAME='cf_ev',/NONEXCLUSIVE, /ALIGN_CENTER, /ROW)
			    		pause = Widget_Button(pause_base, value='', uvalue='pause_ev', /ALIGN_CENTER)

				base1 = WIDGET_BASE(base, /ROW)  ; Secord row - Info.
					drawinfo = WIDGET_LABEL(base1, VALUE='', /DYNAMIC_RESIZE)

	    		base2 = WIDGET_BASE(base, /ROW)  ; Third row - bottom row of control widgets

					base_connect = WIDGET_BASE(base2, /COLUMN)  ; start new area below draw area
						check_socket_data_ready = WIDGET_LABEL(base_connect, VALUE=' ', uvalue='check_socket_data_ready_ev')
							;I assigned a uvalue to this space so I can call the "check_socket_data_ready" code segment with a timer function.
							; first a block of data records is requested via socket using "request_socket_data" subroutine,
							; then all the data is acquired in the server, then the server sends a netres (66L) response when it is ready to send over the socket
	     				connect = WIDGET_BUTTON(base_connect, XSIZE=60, VALUE='Disconnect', uvalue='connect_ev')

					base1115 = WIDGET_BASE(base2, /COLUMN)  ; start new area below draw area
	     					dummy = WIDGET_LABEL(base1115, VALUE='Data Source')
	     				ip=WIDGET_COMBOBOX(base1115, VALUE=ip_list, uvalue='ip_ev', /DYNAMIC_RESIZE, /EDITABLE)


					base_para = WIDGET_BASE(base2, /COLUMN)  ; start new area below draw area
						dummy = WIDGET_LABEL(base_para, VALUE='Parameter')
						parameter = WIDGET_DROPLIST(base_para, VALUE=parameter_list, uvalue='parameter_ev', /DYNAMIC_RESIZE)

					base_disp = WIDGET_BASE(base2, /COLUMN)  ; start new area below draw area
		      			dummy = WIDGET_LABEL(base_disp, VALUE='Display')
						display = WIDGET_DROPLIST(base_disp, VALUE=display_list, uvalue='display_ev', /DYNAMIC_RESIZE)


					base_axes = WIDGET_BASE(base2, /COLUMN)  ; start new area below draw area
						dummy = WIDGET_LABEL(base_axes, VALUE='Axes', /ALIGN_LEFT)
		    			axes_base = Widget_Base(base_axes,/NONEXCLUSIVE, /COLUMN, /ALIGN_RIGHT)
		    			axes = Widget_Button(axes_base, value='', uvalue='axes_ev', /ALIGN_RIGHT)

					base_grid = WIDGET_BASE(base2, /COLUMN)  ; start new area below draw area
						dummy = WIDGET_LABEL(base_grid, VALUE='Grid', /ALIGN_LEFT)
		    			grid_base = Widget_Base(base_grid,/NONEXCLUSIVE, /COLUMN, /ALIGN_RIGHT)
		    			grid = Widget_Button(grid_base, value='', uvalue='grid_ev')

					base_pos = WIDGET_BASE(base2, /COLUMN)  ; start new area below draw area
						dummy = WIDGET_LABEL(base_pos, VALUE='Pos.', /ALIGN_LEFT)
		    			pos_base = Widget_Base(base_pos,/NONEXCLUSIVE, /COLUMN, /ALIGN_RIGHT)
		    			posstatus = Widget_Button(pos_base, value='', uvalue='posstatus_ev', /ALIGN_RIGHT)

					base_stat = WIDGET_BASE(base2, /COLUMN)  ; start new area below draw area
						dummy = WIDGET_LABEL(base_stat, VALUE='Stat.', /ALIGN_LEFT)
		    			stat_base = Widget_Base(base_stat,/NONEXCLUSIVE, /COLUMN, /ALIGN_RIGHT)
		    			statstatus = Widget_Button(stat_base, value='', uvalue='stat_ev', /ALIGN_RIGHT)

					base_newbsize = WIDGET_BASE(base2, /COLUMN)
						dummy = WIDGET_LABEL(base_newbsize, VALUE='NBeams')
						newbsize = WIDGET_TEXT(base_newbsize, VALUE=string(newbsize_val, FORMAT='(I3)'), uvalue='newbsize_ev', /ALIGN_CENTER, /EDITABLE, XSIZE=4)


					base0114 = WIDGET_BASE(base2, /COLUMN, /ALIGN_TOP)  ; start new area below draw area

						base_hshift = WIDGET_BASE(base0114, /ROW, /ALIGN_TOP)  ; start new area below draw area
							dummy = WIDGET_LABEL(base_hshift, VALUE='<')
							hshift=WIDGET_SLIDER(base_hshift, TITLE='', UVALUE='hshift_ev', MINIMUM=-100., MAXIMUM=100., VALUE=0.)
							dummy = WIDGET_LABEL(base_hshift, VALUE='>')

					;base_pslogo = WIDGET_BASE(base111, /ALIGN_RIGHT, /COLUMN)  ; Pedestal position area
						;dummy = WIDGET_LABEL(base_pslogo, VALUE='')
						;button_close = WIDGET_BUTTON(base_pslogo, VALUE='PSlogo2.bmp', uvalue='close_ev', /BITMAP, /ALIGN_RIGHT);PS image file has to be 16 bit bmp



			pos_base = WIDGET_BASE(/COLUMN, TITLE='Antenna', MAP=0, XOFFSET=670)  ; pos base window

				posdraw = WIDGET_DRAW(pos_base, XSIZE=150, YSIZE=150)


			stat_base = WIDGET_BASE(/COLUMN, TITLE='System Status', MAP=0, XOFFSET=670, YOFFSET=200)  ; Stat base window

				base_stat1 = WIDGET_BASE(stat_base, /COLUMN)  ; Area
		    			rstat1 = WIDGET_LABEL(base_stat1, VALUE='Server Time', /DYNAMIC_RESIZE, /ALIGN_LEFT)


				base_stat2 = WIDGET_BASE(stat_base, /COLUMN)  ; Area
						rstat2 = WIDGET_LABEL(base_stat2, VALUE='Temperatures', /DYNAMIC_RESIZE, /ALIGN_LEFT)
						rstat21 = WIDGET_LABEL(base_stat2, VALUE=' ', /DYNAMIC_RESIZE, /ALIGN_LEFT)
				    	rstat3 = WIDGET_LABEL(base_stat2, VALUE='Inclinometers', /DYNAMIC_RESIZE, /ALIGN_LEFT)

				base_stat3 = WIDGET_BASE(stat_base, /COLUMN)  ; Area
						rstat4 = WIDGET_LABEL(base_stat3, VALUE='GPS Time', /DYNAMIC_RESIZE, /ALIGN_LEFT)
				    	rstat5 = WIDGET_LABEL(base_stat3, VALUE='Lat & Long', /DYNAMIC_RESIZE, /ALIGN_LEFT)

				base_stat4 = WIDGET_BASE(stat_base, /COLUMN)  ; Area
						rstat51 = WIDGET_LABEL(base_stat4, VALUE='Filename', /DYNAMIC_RESIZE, /ALIGN_LEFT)
				    	rstat6 = WIDGET_LABEL(base_stat4, VALUE='Radar: Mode, (sum powers), (fft_length), (tapering), Clutter Filtering', /DYNAMIC_RESIZE, /ALIGN_LEFT)
						rstat7 = WIDGET_LABEL(base_stat4, VALUE='Range Gates: N Gates, Resolution, BW, Spacing, Max. Range', /DYNAMIC_RESIZE, /ALIGN_LEFT)
						rstat8 = WIDGET_LABEL(base_stat4, VALUE='PRF: PRI1, (PRI2), Ave, Integration time', /DYNAMIC_RESIZE, /ALIGN_LEFT)

				base_stat5 = WIDGET_BASE(stat_base, /COLUMN)  ; Area
						rstat9 = WIDGET_LABEL(base_stat5, VALUE='POS Scan type', /DYNAMIC_RESIZE, /ALIGN_LEFT)
						rstat10 = WIDGET_LABEL(base_stat5, VALUE='Record Count', /DYNAMIC_RESIZE, /ALIGN_LEFT)


	  ; Realize the widget hierarchy, then retrieve the widget ID of
	  ; the draw widget.
	  	WIDGET_CONTROL, base, /REALIZE ; Draw all the widgets
	  	WIDGET_CONTROL, pos_base, /REALIZE ; Draw all the widgets
		WIDGET_CONTROL, stat_base, /REALIZE ; Draw all the widgets


		; color table
			nexrad_ct 									;dBZ and power color table
			IF parameter_index GE 2 AND parameter_index LE 5 $
			   THEN doppler_ct 	;Doppler velocity and phase color table

		WIDGET_CONTROL, drawinfo, SET_VALUE=''
	  	WIDGET_CONTROL, draw, GET_VALUE=drawwinID ; Get the window number (to be used with WSET command later)
			wset, drawwinID
			plot, [0,0], [0,0], /nodata, ystyle=4, xstyle=4 ;Clear "white" the draw area

		WIDGET_CONTROL, ctdraw, GET_VALUE=ctdrawwinID ; Get the window number (to be used with WSET command later)
			wset, ctdrawwinID
			plot, [0,0], [0,0], /nodata, ystyle=4, xstyle=4 ;Clear "white" the draw area

		WIDGET_CONTROL, posdraw, GET_VALUE=posdrawwinID ; Get the window number (to be used with WSET command later)
			wset, posdrawwinID
			plot, [0,0], [0,0], /nodata, ystyle=4, xstyle=4 ;Clear "white" the draw area
			a=2.*!pi*findgen(361)/360.
			x=sin(a)
			y=cos(a)

			plot, x,y, xstyle=4, ystyle=4, xmargin=[2,2], ymargin=[2,2], COLOR=110

			xyouts, 0, 1.06, "N", ALIGNMENT=0.5, COLOR=200
			xyouts, 1.1, -0.06, "E", ALIGNMENT=0.5, COLOR=200
			xyouts, 0, -1.2, "S", ALIGNMENT=0.5, COLOR=200
			xyouts, -1.1, -0.06, "W", ALIGNMENT=0.5, COLOR=200

			az=0.
			az_old=az
			el=120.
			el_old=el

			oplot, [0,0.95*sin(!pi*az_old/180.)], [0, 0.95*cos(!pi*az_old/180.)], thick=1, color=0
			oplot, [0,0.95*sin(!pi*az/180.)], [0, 0.95*cos(!pi*az/180.)], thick=1, color=255

			arrow, 0, 0, 0.95*cos(!pi*el_old/180.)*sin(!pi*az_old/180.), 0.95*cos(!pi*el_old/180.)*cos(!pi*az_old/180.), /DATA, COLOR=0, thick=3
			arrow, 0, 0, 0.95*cos(!pi*el/180.)*sin(!pi*az/180.), 0.95*cos(!pi*el/180.)*cos(!pi*az/180.), /DATA, COLOR=200, thick=3


			xyouts, -.98, -1.2, "Az:"+strmid(strtrim(string(float(round(az_old))),0),3+(abs(az_old) GE 10)+(az_old GE 100),4), ALIGNMENT=0.0, COLOR=0
			xyouts, -.98, -1.2, "Az:"+strmid(strtrim(string(float(round(az))),0),3+(abs(az) GE 10)+(az GE 100),4), ALIGNMENT=0.0, COLOR=255

			xyouts, 0.98, -1.2, "El:"+strmid(strtrim(string(float(round(el_old))),0),3+(abs(el_old) GE 10)+(abs(el_old) GE 100),4), ALIGNMENT=1.0, COLOR=0
			xyouts, 0.98, -1.2, "El:"+strmid(strtrim(string(float(round(el))),0),3+(abs(el) GE 10)+(abs(el) GE 100),4), ALIGNMENT=1.0, COLOR=255

			wset, drawwinID

	  ; Set the droplist to display the proper selection index.
	  WIDGET_CONTROL, display, SET_DROPLIST_SELECT=display_index
	  WIDGET_CONTROL, parameter, SET_DROPLIST_SELECT=parameter_index
	  WIDGET_CONTROL, ctmax, SET_VALUE=STRING(ctmax_val(parameter_index), FORMAT='(F5.1)')
	  WIDGET_CONTROL, ctmin, SET_VALUE=STRING(ctmin_val(parameter_index), FORMAT='(F5.1)')
	  WIDGET_CONTROL, ip, SET_COMBOBOX_SELECT=ip_index ; Select the startup IP address

			; Color Scale
			n_colors=ct_len()
			ct = bytarr(15, 145)
			FOR i = 0, 14 DO BEGIN
				ct(i, *) = bindgen(145)
			ENDFOR
			wset, ctdrawwinID
			tv, bytscl(ct+1, min=0, max=145, TOP=n_colors-3), 0, 0
			wset, drawwinID

		;connect_val='Disconnect'
		connect_val='Connect'
	  IF ip_index EQ 0 THEN connect_val='Open'
	  WIDGET_CONTROL, connect, SET_VALUE=connect_val ; Select the second IP address

time=systime(1, /SECONDS)

	; Widget structure list:

	 wi = { $
	 fpath : fpath, $
	 drawtitleID:drawtitle, $				; ID of data display title
	 drawID:draw, $             			; ID of data display widget area
	 drawwinID:drawwinID, $             	; ID of data display window
	 drawinfoID:drawinfo, $
	 drawim:drawim, $						; active polar data matrix image on display
	 ctdrawID:ctdraw, $             			; ID of the color table widget area
	 ctdrawwinID:ctdrawwinID, $             	; ID of the color table display window
	 hshiftID:hshift, $
	 hshift_val:hshift_val, $
	 vshiftID:vshift, $
	 vshift_val:vshift_val, $
	 zoomID:zoom, $
	 zoom_val:zoom_val, $
	 axesID:axes, $
	 axes_val:axes_val, $
	 gridID:grid, $
	 grid_val:grid_val, $
	 newbsizeID:newbsize, $
	 newbsize_val:newbsize_val, $
	 ctmaxID:ctmax, $
	 ctmax_val:ctmax_val, $
	 ctminID:ctmin, $
	 ctmin_val:ctmin_val, $
	 displayID:display, $					; display widget ID
	 display_index:display_index, $      	; display index (0, 1, 2 ...)
	 display_list:display_list, $        	; display list (['PPI', 'RHI', 'Range/Time', 'El/Az', 'Scope'])
	 dtype:25L, $							; data type code (from table C.1)
	 parameterID:parameter, $          		; ID of parameter list menu ['dBZh', 'dBZv', 'Zdr', 'VelH', 'VelV', 'STDh', 'STDv', 'PHIvh', ]
	 parameter_index:parameter_index, $     ; Val of parameter_index (0, 1, ...)
	 parameter_list:parameter_list, $      	; Val of product list
	 pauseID:pause, $						; ID of pause widget
	 pause_val:pause_val, $					; pause status (1=checked or 0=unchecked)
	 unit_list:unit_list, $					; radar parameter units (dBZ, m/s, rad, ...)
	 unitID:unit, $
	 thresID:thres, $						; threshold widget ID
	 thres_val:thres_val, $					; threshold value
	 connectID:connect, $
	 connect_val:connect_val, $
	 check_socket_data_readyID:check_socket_data_ready, $
	 ipID:ip, $								; data source widget IP
	 ip_list:ip_list, $						; data source list
	 ip_index:ip_index, $					; data source index
	 source:source, $						; socket or file source number
	 rstat1ID:rstat1, $						; radar status label widgets
	 rstat2ID:rstat2, $						; radar status label widgets
	 rstat21ID:rstat21, $					; radar status label widgets
	 rstat3ID:rstat3, $						; radar status label widgets
	 rstat4ID:rstat4, $						; radar status label widgets
	 rstat5ID:rstat5, $						; radar status label widgets
	 rstat51ID:rstat51, $					; radar status label widgets
	 rstat6ID:rstat6, $						; radar status label widgets
	 rstat7ID:rstat7, $						; radar status label widgets
	 rstat8ID:rstat8, $						; radar status label widgets
	 rstat9ID:rstat9, $						; radar status label widgets
	 rstat10ID:rstat10, $					; radar status label widgets
	 pos_baseID:pos_base, $					; ID of pos stat window
	 posdrawID:posdraw, $					; pos status display area ID
	 posdrawwinID:posdrawwinID, $			; pos status display window ID
	 stat_baseID:stat_base, $				; ID of stat window
	 ctable:0, $           					; currect color table
	 archive_index:0L, $					; configuration archive index
	 netres:0L, $							; network server response 42L=OK
	 time:time $
	}

	ant_bw=360./round(360./1.4) ; Xpol 6' diameter antenna 3 dB beamwidth (deg) -
								; 	the round is to adjust the beamwidth to generate
								; 	an integer number of beams over 360 deg.

	;ant_bw=360./round(360./1.) ; RaXpol 8' diameter antenna 3 dB beamwidth (deg) -
								; 	the round is to adjust the beamwidth to generate
								; 	an integer number of beams over 360 deg.

	n_beams=360./Ant_BW	; number of azimuth bins over 360 deg



; *** Main Header (this is the file header when reading from a file or the main configuration structure when getting data via the network ***
; 0.0=4 bytes,  0.0D=8 bytes, dblarr(1)=8 bytes, 0L=4 bytes, lonarr(1)=4 bytes, strarr(1)=1 bite

	head={ $
		site:bytarr(1024), $		; operator notes 1024 characters										1024
	 	az_off:0D, $				; operator entered absolute azimuth offset										8
	 	rcb: 0L, $					; RCB present true/False								4
	 	cfw:0D, $					; clutter filter width m/s								8
	 	cave:0L, $					; no of groups in clutter filtering interval			4
	 	drate:0D, $					; data rate 											8
	 	fftl:0L, $					; FFT Length											4
	 	fftwindow:0L, $				; FFT window type (0=Bartlett, 1=Blackman, 2=Hamming, 3=Hanning, 4=Rect.)										4
	 	res1:0L, $					; 														4
	 	nrecords:0L, $				; new file start: n records								4
	 	nscans:0L, $				; new file start: n scans								4
	 	recsize:0L, $				; new file start file size (MB)							4
	 	rectime:0L, $				; new file start time (sec)								4
	 	bitflags:0L, $				; new file start bit flags								4
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
	 	zrg:0D $					; Zero Range Gate Index									8
		}


	head2={ $ 				; these few parameters are at the end of the file header, but not when getting data over the network
		scan_type:0L, $  ; scan type 3=Point, 5=PPI, 6=RHI, 7=Az Ras, 8=El Ras, 9=Vol. 4 bytes
	 	host_ts:lonarr(2), $		; Time stamp sec										2*4
		hostname:bytarr(8), $
	 	spare:lonarr(2) $   ; spare
		}


	stat={ $
		host_ts: lonarr(2), $ 			; host time stamp										2*4		Xpol stat
	  	rcb_temp: lonarr(4), $			; temperatures											4*4
	  	rcb_incl: lonarr(2), $			; inclinometers											2*4
	  	rcb_fuel: 0L, $					; fuel level											4
	  	cpu_temp: 0., $					; cpu temp (not working yet)			4
		scan_type: 0L, $				; scan type 3=Point, 5=PPI, 6=RHI, 7=Az Ras, 8=El Ras, 9=Vol. 4 bytes
		tx_power_sample:0., $			; Tx pulse sample power
		spare: lonarr(5) $				; spare													7*4
	  	}

	ped={ $
		az:0D, $
	  	el:0D, $
	  	az_vel:0D, $
	  	el_vel:0D $
	  	}

	ped_current={ $
	  	motor_current_az: 0D, $
	  	motor_current_el: 0D $
	  	}

	gps1={ $
		lath:0L, $ ; 0=undefined, ascii 78 = 'N' north, ascii 83 = 'S' south
	  	lat:0D, $
	  	lonh:0L, $ ; 0=undefined, ascii 69 = 'E' east, ascii 87= 'W' west
	  	lon:0D $ 	; deg
		}
	gps2={ $
	  	headr: 0L, $ ; 0= undefined, acsii 84 = 'T' True north, ascii 77 = 'M' magnetic
	  	head:0D, $ ; deg
	  	speed:0D $ ; km/hr
	  	}


	rec_head = { $				; format changed on June 15, 2009
	  gga:bytarr(96), $					; GPS GGA message text									96		GPS
	  gps_ts: lonarr(2), $	 			; gps time stamp										2*4
	  data_type: 0L, $					; Data Type												4		Data stat
	  data_size: 0L $					; Data Size												4
	}

	display={ $
		r:fltarr(1), $ ; range gate range vector
		thet:fltarr(n_beams), $ ; M element display angle (az or el)
		thet_last:0., $
	 	dispdata:0., $		; active data matrix on display nrg x M matrix
		fname:'', $
		bsize:LONG(newbsize_val), $ ; display update after reading bsize number of records. Also set in the 'Connect' interrupt code segment, to 10 for socket and to 180 for file.
		az_old:0D, $ ; used to display pedestal position in pos window
		el_old:0D,  $ ; used to display pedestal position in pos window
		azvel_old:0D, $ ; used to display pedestal position in pos window
		elvel_old:0D,  $ ; used to display pedestal position in pos window
		ant_bw:ant_bw, $ ; radar antenna 3 dB beamwidth
		n_beams:n_beams $ ; number of azimuth bins used in display
		}


	drec={ $
		mode:0L, $
		tsource:0L, $
		pcifreq:0L, $
		adclockfreq:0L, $
		ndmadesc:0L, $
		ndmapack:0L, $
		dmasize:0L, $
		blocksize:0L, $
		res1:0L, $
		pci_inter: 0D,$
		res2:0L, $
		res3:0L, $
		state:lonarr(2), $
		source:lonarr(2), $
		skipcount:lonarr(2), $
		cicdec:lonarr(2), $
		firdec:lonarr(2), $
		postdec:lonarr(2), $
		ncofreq:lonarr(2) $
		}

	data_head={ $ 							;data header block
		type:0L, $
		tsec:0L, $
		tusec:0L, $
	 	drec:drec, $			; drec structure from above
		digrec_fgain:0D, $
		ave_int:0L, $
		archive_index:0L, $
		block_index:lonarr(2), $
		nblocks:0L, $
		blocks:0L, $
		blockdim:0L, $				; data block size
		blockdimsize:lonarr(4) $
		}

file_head_size=LONG(n_tags(head, /data_length) + n_tags(head2, /data_length))
rec_head_size=LONG(3*4+n_tags(stat, /data_length) + n_tags(ped, /data_length) + n_tags(gps1, /data_length) + n_tags(gps2, /data_length) + n_tags(rec_head, /data_length))

	data={ $	; data block
		file_head_size:file_head_size, $ ; file header size (fixed)
		rec_head_size:rec_head_size, $ ; record header size (fixed)
		record_size:0L, $ ; total record size (depends on mode)
		nrec:0L, $ ; number of records in file
		crec:0L, $ ; displayed record count
		drec:0D, $ ; data record structure (depends on mode)
		v_noise:0., $ ; V channel noise floor
		h_noise:0. $ ; H channel noise floor
		}


	st={wi:wi, head:head, head2:head2, stat:stat, ped:ped, ped_current:ped_current, gps1:gps1, gps2:gps2, rec_head:rec_head, display:display, data_head:data_head, data:data} ;Stash

	  ; Write the stash structure into the Base widget UVALUE to cary it to the next event call.
	  WIDGET_CONTROL, base, SET_UVALUE=st
		WIDGET_CONTROL, pos_base, SET_UVALUE=st
		WIDGET_CONTROL, stat_base, SET_UVALUE=st



	  ; Register the widget with the XMANAGER.
	  XMANAGER, 'disp', base, /NO_BLOCK
		XMANAGER, 'disp', pos_base, /NO_BLOCK
		XMANAGER, 'disp', stat_base, /NO_BLOCK



	;widget_event


END