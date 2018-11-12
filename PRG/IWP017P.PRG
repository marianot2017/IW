*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : IWP017p.PRG -> Emision de informe definido
*)-H Release		 : 03
*)-H Fecha de inicio :
*)-H Fecha de fin    :
*)-H
*)-H Updates
*)-H
*)-H Fecha       Programmer      Comentarios
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS w_pref,w_call

PRIVATE ALL LIKE w_*
if empty(parameters())
	w_pref=''
	w_call=.f.
endif

*) Salvado de estados anteriores-----------------------------------------------

push key clear		  &&(Salva estados de teclas)
push menu _msysmenu &&(Salva estado de menu)
set skip of menu _msysmenu .t.

*) Variables internas----------------------------------------------------------

w_pfile='17p'
w_prog =upper(prompt())
w_title=upper('Emisi๓n de informes definidos')
w_alta =.f.
w_alias='f17'

*) Apertura de archivos--------------------------------------------------------

fieldvec[1]='F00'
fieldvec[2]='F01'
fieldvec[3]='F02'
fieldvec[4]='F03'
fieldvec[5]='F04'
fieldvec[6]='F05'
fieldvec[7]='F15'
fieldvec[8]='F16'
fieldvec[9]='F17'
fieldvec[10]='F24'

use (d0f00) in 0 alias 'f00'						&&(Parametros)
use (d0f01) in 0 alias 'f01' order 'd1tip'   		&&(Tablas)
use (d0f02) in 0 alias 'f02' order 'd2clie'			&&(Clientes)
use (d0f03) in 0 alias 'f03' order 'd3prov'			&&(Proveedores)
use (d0f04) in 0 alias 'f04' order 'd4peri'			&&(Periodos)
use (d0f05) in 0 alias 'f05' order 'd5peri'			&&(Comprobantes)
use (d0f15) in 0 alias 'f15' order 'd15peri'		&&(Mov.Complementarios)
use (d0f16) in 0 alias 'f16' order 'd16defi' EXCL	&&(Def.Cabecera)
use (d0f17) in 0 alias 'f17' order 'd17defi' EXCL	&&(Def.Renglones)
use (d0f24) in 0 alias 'f24' order 'd24codi' 		&&(Percep./Retenc./Otros)
use (d0f23) in 0 alias 'f23' order 'd23codi' 		&&(Alicuotas de IVA)   && R.01 Jonatan

if neterr()
	pop key
	pop menu _msysmenu
	fieldvec=''
	=closall()
	return
endif

=creatipos('V')

fieldvec=''

sele (w_alias)

*) Variables-------------------------------------------------------------------

wstatus=15
go top in f16
wdefi=f16.d16defi
wdeta='' && R.01 Jonatan
aux1='000' && R.01 Jonatan
go bottom in f04
wdperi=f04.d4peri
whperi=f04.d4peri
*)-----------------------------------------------------------------------------

do ssm998.mpr
do iws017p.spr


pop key
pop menu _msysmenu
=closall()

dele file &d0f99.b.dbf

=borratipos()

rele v_calcT,v_calcP,v_indi,v_calcC

return

***vfp***
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons17p
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	on key label f9 do cons17p0
	keyboard '{f9}'
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons17p0
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
on key label f9
***vfp***

if .not. valid17p(.t.)
	wait wind 'Verifique los datos ingresados...'
	return
endif

w_dperi=ptoi(wdperi)
w_hperi=ptoi(whperi)

do list17p

if !p_cancela
	return
endif

t_titu =f16->d16nomb

t_leye ='Informe: '+wDeFi+'        '

* Definiciขn de Meses
t_leye =t_leye+'Desde: '+wdPeri+'  Hasta: '+whPeri


* ~ Fin Inicializacion de T_leye

* Inicializacion de T_deta's

t_deta = f16->d16ley1
t_deta1= f16->d16ley2

* Fin Inicializacion de T_deta's

do repdest &&(Setea variables internas del reporte)

sele f99b

go top
if !empty(p_repclaus) &&(debe ser preview o to printer)
	p_report='iwr017p'
	=rep_form()
	p_report=''
endif

do represet &&(Resetea estado)


return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC when17p
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
w_retu=.t.
w_vari=varread()
p_curobj=_curobj

return(w_retu)

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC valid17p
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS w_show

if empty(parameters())
	w_show=.f.
endif

w_retu=.t.
w_vari=varread()
w_vread='m.'+w_vari

if w_vari='WDEFI' or w_show
	do fhelp with 'WDEFI','','iwp016','f16','d16defi','d16nomb','Definici๓n inexistente...',(p_strc),'w_retu'
endif

&& R.01B Jonatan
if w_vari='WDEFI'
	if aux1 <> WDEFI
		wdeta=f16.d16deta
		SHOW GET wdeta
		aux1=wdefi
	endif
endif
&& R.01e Jonatan

if w_vari='WDPERI' or w_show
	if lastkey()=p_f4
		do fhelp with 'WDPERI','','iwp004','f04','d4peri','d4peri','',(p_char),'w_retu'
	endif

&& R.03b Mariano
	w_fech	= CTOD('01/' + wdperi)
	IF EMPTY(w_fech)
		RETURN	.F. 
	ENDIF
	IF TRDATE('w_fech')
		wdperi	= RIGHT(DTOC(w_fech),7)
		SHOW GET wdperi
&& R.02b Mariano
*			wkperi = PtoI(wdperi)
*			if ! seek(wkperi,'f04')
*				if !w_show
*						wait window 'Perํodo Inexistente'
*				endif
*				w_retu=.f.
*				_curobj	= objnum(wdperi)
*				RETURN .F.
*			endif
&& R.02e Mariano
	ELSE
		IF !w_show
				WAIT WIND "El perํodo no es correcto"
		ENDIF
		w_retu	= .F.
		_curobj	= objnum(wdperi)
		RETURN .F.
	ENDIF
&& R.03e Mariano
&& R.02b Mariano
	if !seek(ptoi(wdperi),'f04')
		if !w_show
			wait window 'Perํodo Inexistente'
		endif
		w_retu=.f.
	endif
&& R.02e Mariano

endif

if w_vari='WHPERI' or w_show
	if lastkey()=p_f4
		do fhelp with 'WHPERI','','iwp004','f04','d4peri','d4peri','',(p_char),'w_retu'
	endif

&& R.03b Mariano
	w_fech	= CTOD('01/' + whperi)
	IF EMPTY(w_fech)
		RETURN	.F. 
	ENDIF
	IF TRDATE('w_fech')
		whperi	= RIGHT(DTOC(w_fech),7)
		SHOW GET whperi
&& R.02b Mariano
*			wkperi = PtoI(whperi)
*			if ! seek(wkperi,'f04')
*				if !w_show
*						wait window 'Perํodo Inexistente'
*				endif
*				w_retu=.f.
*				RETURN .F.
*			endif
&& R.02e Mariano
	ELSE
		IF !w_show
				WAIT WIND "El perํodo no es correcto"
		ENDIF
		w_retu	= .F.
		_curobj	= objnum(whperi)
		RETURN .F.
	ENDIF
&& R.03e Mariano
&& R.02b Mariano
	if !seek(ptoi(whperi),'f04')
		if !w_show
			wait window 'Perํodo Inexistente'
		endif
		w_retu=.f.
	endif
&& R.02e Mariano
*===>

	if ptoi(whperi)<ptoi(wdperi)
		if !w_show
			wait wind 'Hasta debe ser mayor o igual que desde...'
		endif
		w_retu=.f.
	endif
endif

return (w_retu)

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC list17p
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

if used('f99b')
	sele f99b
	zap
else
	sele 0
	create dbf &d0f99.b (d5titu c(100),d5impu n(15,2),d5vaim l(1),d5deim c(5), d5imp2 n(15,2)) && R.01 Jonatan (agrego d5imp2)
	use &d0f99.b alias f99b exclusive
endif

wCare=0
wCard=0	&& R.02 Mariano
=seek(wDeFi,'f17')
do while f17.d17defi=wDeFi and !eof('f17')
	wCare=wCare+1
	&& R.02b Mariano
	if f17.d17deta='S'
		wCard=wCard+1
	endif
	&& R.02e Mariano
	skip in f17
enddo
if empty(wCaRe)
	return
endif

rele v_calcT,v_calcP,v_indi,v_calcC
*PUBLIC array v_calcT[wCare*(reccount('f23')+1)), wCare,2],v_calcP[wCaRe*(reccount('f23')+1),2],v_indi[wCaRe*(reccount('f23')+1)] && ,v_calcC[wCaRe] && R.01 Jonatan (Agregue ',2' y agrego '(reccount('f23')+1)') && R.02 Mariano
PUBLIC array v_calcT[iif(wdeta='S',(wCare-wCard)+(wCaRD*(reccount('f23')+1)), wCare),2],v_calcP[iif(wdeta='S',(wCare-wCard)+(wCaRD*(reccount('f23')+1)), wCare),2],v_indi[iif(wdeta='S',(wCare-wCard)+(wCaRD*(reccount('f23')+1)), wCare)]	&& R.02 Mariano

store 0 to v_calcT,v_calcP,v_indi

pedes=wdperi
pehas=whperi

&& R.01b Jonatan
wind=0
=seek(wDeFi,'f17')
*do while f17.d17defi=wDeFi and !eof('f17')
*	wInd=wInd+1
*	v_indi[wInd]=f17.d17orde
*	skip in f17
*enddo
do while f17.d17defi=wDeFi and !eof('f17')
		**// SI ES DETALLADO POR PORCENTAJE AGREGA UN REGISTRO POR CADA ALICUOTA Y UNA PARA EL TOTAL //**
		if wdeta='S' and f17.d17deta='S'	&& R.02 Mariano (agregue d17deta)
			**// VALIDA QUE TENGA PORCENTAJES EN F17.D17ALIC SINO TOMA LO QUE HAY EN F23 //**
			if !empty(f17.d17alic) and f17.d17alic<>'  .  /  .  /  .  /  .  /  .  /  .  /  .  '
				for w_cont=1 to 7
					wtiva=substr(f17.d17alic,(w_cont-1)*6+1,5)
					if wtiva<>'  .  ' and wtiva<>'    ' and !empty(wtiva)
						wInd=wInd+1
						v_indi[wInd]=f17.d17orde+wtiva && Un registro por cada % seleccionado en el renglon.
					endif
				next
			else
				calic=0
				alics=''
				sele f23
				go top
				do while !eof('f23')
					if .not. strtran(str(f23.d23alic,5,2),' ','0') $ alics
						calic=calic+1
						alics=alics+strtran(str(f23.d23alic,5,2),' ','0')+'/'
						wInd=wInd+1
						v_indi[wInd]=f17.d17orde+strtran(str(f23.d23alic,5,2),' ','0') && Un registro por cada % en f23.
					endif
					skip in f23
				enddo
			endif
			wInd=wInd+1
			v_indi[wInd]=f17.d17orde+'zz' && Un registro para grabar el total
		else
			wInd=wInd+1
			v_indi[wInd]=f17.d17orde && Registro si no es detallado
		endif
	skip in f17
enddo
&& R.01e Jonatan

if start(w_Dperi,'f05')
	sele f05
	wFlag=.f.
	wCtota=0
	WCparc=0
	do while between(PtoI(d5peri),w_Dperi,w_Hperi) and !eof('f05')
		if lastkey()=p_esc
			wflag=.f.
			exit
		endif

		w5comp=TcAbre(d5tico)

		perio=f05.d5peri
		rubro=f05.d5rubr
		clipr=f05.d5clie
		juris=f05.d5juri
		formu=f05.d5tifo

		netgr=f05.d5imp1
		nogra=f05.d5imp2
		ivain=f05.d5imp4
		ivani=f05.d5imp5
		ivapr=f05.d5imp3
		otco1=f05.d5imp6
		otco2=f05.d5imp7
		otco3=f05.d5imp8

*===> Para las retenciones ( RT )
		impre	=	f05.d5tota

		wCtota = wCtota + 1

		wait window d5peri+' '+d5tipo+' '+w5comp+' '+d5nume+spac(5)+str(wCtota,7) nowait

		if f05.d5tipo='V'
			=seek('A','f03')
			=seek(f05.d5clie,'f02')
		else
			=seek('A','f02')
			=seek(f05.d5clie,'f03')
		endif

		* Recorrida de C/u de los datos del informe

		v_calcP=0.00
		wInd=0
		=seek(wDefi,'f17')
		do while f17.d17defi=wDeFi .and. !eof('f17')
		
			&& R.01b Jonatan
			if wdeta='S' and f17.d17deta='S'	&& R.02 Mariano (d17deta)
				wInd=ascan(v_indi,f17.d17orde+strtran(str(f05.d5por1,5,2),' ','0')) && R.01 Jonatan
			else
				wInd=ascan(v_indi,f17.d17orde)
			endif
			&& R.01e Jonatan
			
			* Filtros Fijos
			* -------------
*===> En este CASE se agrega  ' AND w5comp <> "RT" ' para que no valide si son RETENCIONES.

			do case
			case 'TMC'$upper(f17.d17form) or;
					'REN'$upper(f17.d17form) or;
					'SRE'$upper(f17.d17form) or;
					'TMC'$upper(f17.d17for2) or;
					'REN'$upper(f17.d17for2) or;
					'SRE'$upper(f17.d17for2) && R.01 Jonatan (agrego para que valide d17for2)
				* Si existen estas funciones los deja para la pasada
				* final
				skip in f17
				loop
			case ! d5tipo$f17.d17veco 
				* Ventas o Compras
				skip in f17
				loop
			case ! w5comp$f17.d17comp
				* Comprobante
				skip in f17
				loop
*			case ! d5tiim$f17.d17coim
			case ! d5tiim$f17.d17coim AND w5comp <> "RT" 
				* Condicion Impositiva
				skip in f17
				loop
*			case ! d5cond$f17.d17copa
			case ! d5cond$f17.d17copa	AND w5comp <> "RT"
				* Condicion de pago
				skip in f17
				loop
*			case ! d5ttip$f17.d17tipo
			case ! d5ttip$f17.d17tipo	AND w5comp <> "RT"
				* Grupo Tabla Grupos
				skip in f17
				loop
*			case ! between(d5pere,f17.d17dpro,f17.d17hpro)
			case ! between(d5pere,f17.d17dpro,f17.d17hpro)	AND w5comp <> "RT"
				* percep./retenc./otros
				skip in f17
				loop
			endcase

			* Filtro Armado
			* -------------

*===> En este armado se agrega  ' AND w5comp <> "RT" ' para que no valide si son RETENCIONES.

********************************************************************************
			=seek(d5pere,'f24')
			if !f24.d24refe$f17.d17prot
*			if !f24.d24refe$f17.d17prot	AND w5comp <> "RT"
				* percep./retenc.
				skip in f17
				loop
			endif
*********************************************************************************
			
			=seek(iif(d5tipo='V','RV','RC')+d5rubr,'f01')
*			if !f01.d1clas$f17.d17clas
			if !f01.d1clas$f17.d17clas	AND w5comp <> "RT"
				* clase de operacion
				skip in f17
				loop
			endif
			
*			if !between(d5alic,f17.d17dali,f17.d17hali)
			if !between(d5alic,f17.d17dali,f17.d17hali)	AND w5comp <> "RT"
				* alicuotas IVA
				skip in f17
				loop
			endif

			&& R.01b Jonatan
			if !empty(f17.d17alic) and f17.d17alic<>'  .  /  .  /  .  /  .  /  .  /  .  /  .  '
				if .not. strtran(str(f05.d5por1,5,2),' ','0') $ f17.d17alic AND w5comp <> "RT"
					* porcentajes de alicuotas IVA
					skip in f17
					loop
				endif
			endif
			&& R.01e Jonatan

			wFilt=alltrim(f17.d17filt)
			if ! empty(wFilt)
				wRFilt=&wFilt
			else
				wRFilt=.t.
			endif
			if type('wRFilt')<>'L'
				skip in f17
				loop
			endif
			if ! wRFilt
				skip in f17
				loop
			endif


			* Resolucion de formula
			* ---------------------
			wForm=alltrim(f17.d17form)
			wResul=0
			if ! empty(wForm)
				wResul=&wForm
			endif
			if type('wResul')='N'
				wCparc=wCparc+1

				wait window d5peri+' '+d5tipo+' '+w5comp+' '+d5nume+'  '+f17.d17orde+str(wCparc,7) nowait

				v_calcT[wInd,1] = v_calcT[wInd,1] + wResul
				v_calcP[wInd,1] = wResul
				wFlag=.t.
			endif
			
&& R.01b Jonatan
			* Resolucion de formula 2
			* ---------------------
			wFor2=alltrim(f17.d17for2)
			wResu2=0
			if ! empty(wFor2)
				wResu2=&wFor2
			endif
			if type('wResu2')='N'
				wCparc=wCparc+1

				wait window d5peri+' '+d5tipo+' '+w5comp+' '+d5nume+'  '+f17.d17orde+str(wCparc,7) nowait

				v_calcT[wInd,2] = v_calcT[wInd,2] + wResu2
				v_calcP[wInd,2] = wResu2
				wFlag=.t.
			endif
&& R.01e Jonatan

			skip in f17
		enddo
		skip in f05
	enddo

	=seek(wDefi,'f17')
	do while f17.d17defi=wDeFi and !eof('f17')

		if 'TMC'$upper(f17.d17form) or;
				'REN'$upper(f17.d17form) or;
				'SRE'$upper(f17.d17form)
			* Si existen estas funciones totaliza

			* Resolucion de formula
			* ---------------------

			wInd=ascan(v_indi,f17.d17orde)
			wForm=alltrim(f17.d17form)
			wResul=0
			if ! empty(wForm)
				wResul=&wForm
			endif
			if type('wResul')='N'
				v_calcT[wInd,1] = v_calcT[wInd,1] + wResul
				wFlag=.t.
			endif
		endif
		
&& R.01b Jonatan
		if 'TMC'$upper(f17.d17for2) or;
				'REN'$upper(f17.d17for2) or;
				'SRE'$upper(f17.d17for2)
			* Si existen estas funciones totaliza

			* Resolucion de formula
			* ---------------------

			wInd=ascan(v_indi,f17.d17orde)
			wFor2=alltrim(f17.d17for2)
			wResu2=0
			if ! empty(wFor2)
				wResu2=&wFor2
			endif
			if type('wResu2')='N'
				v_calcT[wInd,2] = v_calcT[wInd,2] + wResu2
				wFlag=.t.
			endif
		endif
&& R.01e Jonatan

		skip in f17
	enddo
	
	wInd=0
	=seek(wDefi,'f17')
	
	&& R.01b Jonatan
	if wdeta='S'
		do while f17.d17defi=wDeFi and !eof('f17') and wFlag
			alics=''
			store 0 to w_tot1, w_tot2, calic
			if f17.d17deta='S'	&& R.02 Mariano
				if !empty(f17.d17alic) and f17.d17alic<>'  .  /  .  /  .  /  .  /  .  /  .  /  .  '
					for w_cont=1 to 7
						wtiva=substr(f17.d17alic,(w_cont-1)*6+1,5)
						if wtiva<>'  .  '
							calic=calic+1
							alics=alics+wtiva+'/'
						endif
					next
				else
					sele f23
					go top
					do while !eof('f23')
						if .not. strtran(str(f23.d23alic,5,2),' ','0') $ alics
							calic=calic+1
							alics=alics+strtran(str(f23.d23alic,5,2),' ','0')+'/'
						endif
						skip in f23
					enddo
				endif
			endif	&& R.02 Mariano
			if f17.d17impr='N'
				skip in f17
				loop
			endif
*			if empty(f17.d17form)	&& R.02 Mariano
			if empty(f17.d17form) and empty(f17.d17for2) and !empty(f17.d17titu)	&& R.02 Mariano
				insert into f99b values;
					('',0,.f.,'',0)
				wSan=4
			else
				wSan=7
			endif
			if !empty(f17.d17form) or !empty(f17.d17for2)
				if f17.d17deta='S'	&& R.02 Mariano
					for w_aux=1 to calic
						walic=substr(alics,(w_aux-1)*6+1,5)
						if walic<>'  .  ' and walic<>'    ' and !empty(walic)
							_elem=ascan(v_indi,f17.d17orde+walic)
							if !empty(v_calcT[_elem,1]) or !empty(v_calcT[_elem,2])
								insert into f99b values;
									(left(spac(wSan+2) + trim(f17->d17titu)+ repl('.',89),81)+'(%'+walic+')',v_calcT[_elem,1],.t.,'('+f17.d17orde+')',v_calcT[_elem,2]) && R.01 Jonatan (Agrego ',1' y ',2') && R.02 Mariano ( cambie spac(wSan) x spac(wSan+2) )
								w_tot1 = w_tot1 + v_calcT[_elem,1]
								w_tot2 = w_tot2 + v_calcT[_elem,2]
							endif
						endif
					next
					if w_tot1+w_tot2<>0
						wtitu='TOTAL: '
					else
						wtitu=''
					endif
					insert into f99b values;
								(left(spac(wSan) + trim(f17->d17titu)+ repl('.',89),83)+wtitu,w_tot1,.t.,'('+f17.d17orde+')',w_tot2)
				&& R.02b Mariano
				else
					_elem=ascan(v_indi,f17.d17orde)

					insert into f99b values;
						(left(spac(wSan) + trim(f17->d17titu)+ repl('.',89),89),v_calcT[_elem,1],.t.,'('+f17.d17orde+')',v_calcT[_elem,2]) && R.02 Jonatan (Agrego ',1' y ',2')
				endif
				&& R.02e Mariano
			else
				insert into f99b values;
					(left(spac(wSan) + trim(f17->d17titu),89),0,.f.,'',0)
*				insert into f99b values;
					('',0,.f.,'',0)	&& R.02 Mariano
			endif
			skip in f17
		enddo
	else
	&& R.01e Jonatan
		do while f17.d17defi=wDeFi and !eof('f17') and wFlag

			if f17.d17impr='N'
				skip in f17
				loop
			endif

*			if empty(f17.d17form)	&& R.02 Mariano
			if empty(f17.d17form) and empty(f17.d17for2) and !empty(f17.d17titu)	&& R.02 Mariano
				insert into f99b values;
					('',0,.f.,'',0) && R.01 Jonatan (Agrego ',0' al final)
				wSan=4
			else
				wSan=7
			endif

*			if ! empty(f17.d17form)	&& R.02 Mariano
			if !empty(f17.d17form) or !empty(f17.d17for2)	&& R.02 Mariano
				_elem=ascan(v_indi,f17.d17orde)

				insert into f99b values;
					(left(spac(wSan) + trim(f17->d17titu)+ repl('.',89),89),v_calcT[_elem,1],.t.,'('+f17.d17orde+')',v_calcT[_elem,2]) && R.02 Jonatan (Agrego ',1' y ',2')
			else
				insert into f99b values;
					(left(spac(wSan) + trim(f17->d17titu),89),0,.f.,'',0) && R.01 Jonatan (Agrego ',0' al final)
*				insert into f99b values;
					('',0,.f.,'',0) && R.01 Jonatan (Agrego ',0' al final)	&& R.02 Mariano
			endif

			skip in f17
		enddo
	endif

endif
return

***puesto adrede para que incluya el report al proyecto**
report form iwr017p
