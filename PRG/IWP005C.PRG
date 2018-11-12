*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : IWP005c.PRG -> Comprobante de Compras
*)-H Release		 : 29
*)-H Fecha de inicio :
*)-H Fecha de fin    :
*)-H
*)-H Actualizaciones
*)-H
*)-H Fecha       Programmer      Comentarios
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
parameters w_pref,w_call

private all like w_*

if empty(parameters())
	w_call=.f.
	w_pref=''
endif

*===> Para el total de facturas duplicadas
x_texto		= ""		&& R.13 Mariano (w_texto)
w_d5imp1a	= 0
w_d5imp2a	= 0
w_d5imp3a	= 0
w_d5imp4a	= 0
w_d5imp5a	= 0
w_d5imp6a	= 0
w_d5imp7a	= 0
w_d5imp8a	= 0
x_d5totaa	= 0		&& R.13 Mariano (w_d5totaa)
w_n			= 0


*) Salvado de estados anteriores-----------------------------------------------

push key clear		  &&(Salva estados de teclas)
push menu _msysmenu &&(Salva estado de menu)

w_curobj=_curobj	  &&(Salva numero de objeto en el read activo)
w_alan=alias()		  &&(Salva alias vigente al llamado del programa)
w_wind=wontop()	  &&(Salva ventana vigente al llamado del programa)
w_rdlevel=rdlevel() &&(Salva Nro. de read activo)

*) Variables internas----------------------------------------------------------

w_prog=upper(prompt())

w_alta=.f.
w_agre=.not. mprot(p_user,w_prog,'Registro','Agregar')
w_modi=.not. mprot(p_user,w_prog,'Registro','Modificar')
w_recn=0
w_modicampo=.f.

w_imco		= imco()
w_opci		= ""
w_cori		= SPACE(20)

w_vari		= ""

cafip		= ''
w_impo		= 0
w_5imre		= 0
w_impoib	= ''
w_porcib	= 0
w_ibant		= 0
w_5ib		= SPACE(3)

&& R.02b Ra๚l
*IF p_popup = "SUFRIDAS"
IF p_popup = "COMSUF"					&& X el cambio de los nombres de los niveles de men๚
&& R.02e Ra๚l
&& R.13b Mariano (modifique w_vec por x_vec)
				DIME x_vec(1)
				x_vec(1)	= "reteNciones"	
				w_title		= "RETENCIONES SUFRIDAS"
ELSE
	DIME x_vec(10)	&& R.14 Mariano
	x_vec(1)	= "Facturas"
	x_vec(2)	= "rEcibo-factura"
	x_vec(3)	= "notas de Cr้dito"
	x_vec(4)	= "notas de D้bito"
	x_vec(5)	= "Tickets"	
	x_vec(6)	= "ticKet-factura"
	x_vec(7)	= "ticKet-NC"	&& R.14 Mariano
	x_vec(8)	= "ticKet-ND"	&& R.14 Mariano
	x_vec(9)	= "Recibos"
	x_vec(10)	= "Liq. Varias"
	w_title		= "COMPROBANTES DE COMPRAS"
ENDIF

wopci		= x_vec(1)

&& R.13e Mariano

*) Datos del archivo a mantener------------------------------------------------

w_pfile='05'
*w_clave='ptoi(m.d5peri)+m.d5tipo+m.d5sucu+m.d5tifo+m.d5tico+m.d5nume'
*w_clave='ptoi(m.d5peri)+m.d5tipo+m.d5sucu+m.d5tico+m.d5tifo+m.d5nume'	&& R.19 Mariano
*w_clave='ptoi(m.d5peri)+m.d5tipo+m.d5sucu+m.d5tico+m.d5tifo+m.d5nume+space(4)+m.d5clie'	&& R.19 Mariano	&& R.28 Mariano
w_clave='ptoi(m.d5peri)+m.d5tipo+m.d5sucu+m.d5tico+m.d5tifo+m.d5nume+space(3)+m.d5clie'		&& R.28 Mariano
w_alias='F'+w_pfile
w_title=db_name(w_alias)
w_titu=iif(w_call,upper('Alta'),upper('Modificaci๓n/Baja'))

*) Apertura de archivos--------------------------------------------------------

if w_imco
	ncf01=w0fipre+'01'
	fieldvec[1]='F01c'
	use (ncf01) in 0 alias 'f01c' order tag 'd1cuen'

	if neterr()
		pop key
		pop menu _msysmenu
		fieldvec=''
		=closall()
		return
	endif
	fieldvec=''
endif

n0ffo='\IW\FFO'

fieldvec[1] ='f00'
fieldvec[2] ='f01'
fieldvec[3] ='f02'
fieldvec[4] ='f03'
fieldvec[5] ='f04'
fieldvec[6] ='f05'
fieldvec[7] ='f06'
fieldvec[8] ='f07'
fieldvec[9] ='f20'
fieldvec[10] ='f21'
fieldvec[11] ='f23'
fieldvec[12] ='f24'
fieldvec[13] ='f25'

use (d0f00) in 0 alias 'f00'					&& Parametros
use (d0f01) in 0 alias 'f01' order 'd1tip'		&& Tablas
use (d0f02) in 0 alias 'f02' order 'd2clie'		&& Clientes		(PARA RETENCIONES)
use (d0f03) in 0 alias 'f03' order 'd3prov'		&& Proveedores
use (d0f04) in 0 alias 'f04' order 'd4peri'		&& Periodos
use (d0f05) in 0 alias 'f05' order 'd5comp'		&& Comprobantes
use (d0f06) in 0 alias 'f06' order 'd6inte'		&& Nombres alternativos
use (d0f07) in 0 alias 'f07' order 'd7inte'		&& Memos y Comprobantes Originales xa Retenciones
use (d0f12) in 0 alias 'f12' 					&& Parametros del sistema
use (d0f20) in 0 alias 'f20' order 'd20tipo'	&& Cond Impositiva
use (d0f21) in 0 alias 'f21' order 'd21tip'		&& Tablas Definicion
use (d0f23) in 0 alias 'f23' order 'd23codi'	&&(Alicuotas IVA)
use (d0f24) in 0 alias 'f24' order 'd24codi'	&&(Per/Ret/Otr)
use (d0f25) in 0 alias 'f25' order 'd25codi' &&(Per/Ret IB)	&& R.01 Mariano

if neterr()
	pop key
	pop menu _msysmenu
	fieldvec=''
	=closall()
	return
endif
fieldvec=''

=creatipos('C')

sele (w_alias)

if !autor()
	pop key
	pop menu _msysmenu

	if wexist(w_wind) and !empty(w_wind)
		acti wind (w_wind)
	endif

	=closall()

	return
endif

select f06
scatte memvar blank
w_d6tiim=''
w_tiim=''
w_tido=''
d0ivan=0
d0ivaa=0
m.d5vcai=date()

select (w_alias)
scatter memvar blank

* x: Peri+Tipo+Sucu+TiFo+TiCo+Nume
* w: Tipo+Sucu+TiFo+TiCo+Nume
w20tipo='C'

m.d5tipo='C'
w_5tipo=m.d5tipo

m.d5empr=d0coem
w_5empr=m.d5empr

go bottom in f04
m.d5peri=f04.d4peri
w_5peri=m.d5peri	&& variable para el titulo del screen

wwtifo=''			&& variable para el campo form del screen

m.d5tico=' '
w_5tico=m.d5tico

m.d5tifo='A'
w_5tifo=m.d5tifo
w_5nume=m.d5nume

m.d5sucu='     '	&& R.27 Mariano (amplie a 5 espacios)
w_5sucu=m.d5sucu

m.d5titr='N'
w_5titr=m.d5titr

m.d5por1=0
w_5por1=m.d5por1

w_p0occ1=f00.p0occ1
w_p0occ2=f00.p0occ2
w_p0occ3=f00.p0occ3

*===> Para pantalla totales factura duplicada
w_p0oc1	= w_p0occ1 
w_p0oc2	= w_p0occ2
w_p0oc3	= w_p0occ3

w_dupl=.f.
w_copi=.f.	&& R.23 Mariano

store .f. to hclev

w_5fech={}
w_5rubr=space(6)
w_5clie=space(5)
w_5juri=' '
w_5ttip=' '
w_5cond=' '
w_5pere=space(3)
w_5cpga=space(3)
w_5cpib=space(3)
w_5cpot=space(3)

w_5alic	= SPACE(3)

*)-----------------------------------------------------------------------------

set skip of menu _msysmenu .t.

do ssm998.mpr &&(menu registro)

=skipbar('registro','confirmar',.t.)

do iws005c.spr

pop key
pop menu _msysmenu

if .not. empty(w_wind)
	acti wind (w_wind)
endif

=closall()

=borratipos()

release window iws005c

return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc alta05
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
if w_call
	w_alta=.t.
	deac wind (w_title)
endif
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc baja05
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
if iif(seek(ptoi(m.d5peri),'f04'), f04->d4esco='C', .t.)
	wait window 'Perํodo Cerrado'
	return
endif

*do clav05	&& por los duplicados sino elimino siempre el primero
if eof(w_alias) .or. .not. st_rlock(w_alias)
	return
endif

sele (w_alias)

if sino('Acepta eliminaci๓n')='S'

	if begintran()
*		=rlock('f04')	&& R.11 Mariano
		=db_rlock('f04')	&& R.11 Mariano

		wait wind ' Eliminando... ' time 1

		sele f04

		IF m.d5tico	= "R"
				DO bajaretcv
				REPLACE &pc_imp 	WITH &pc_imp - f05.d5tota
				REPLACE &pc_tota	WITH &pc_tota - f05.d5tota
		ELSE				
			replace d4ctot with d4ctot-m.d5tota;
				,d4cim1 with d4cim1-m.d5imp1;
				,d4cim2 with d4cim2-m.d5imp2;
				,d4cim3 with d4cim3-m.d5imp3;
				,d4cim4 with d4cim4-m.d5imp4;
				,d4cim5 with d4cim5-m.d5imp5;
				,d4cim6 with d4cim6-m.d5imp6;
				,d4cim7 with d4cim7-m.d5imp7;
				,d4cim8 with d4cim8-m.d5imp8
		ENDIF
		
		go recno() in f04

		if seek(m.d5inte,'f06')
			=dele_rec('f06')
		endif

		if seek(m.d5inte,'f07')
			=dele_rec('f07')
		endif

		=dele_rec(w_alias)

		auleyd='Cpr.'+m.d5inte+' '+tcabre(m.d5tico)+' '+m.d5tifo
		auleyh=m.d5nume

		=audi('BAJ',auleyd,auleyh)

		=endtran()

		unlock in f04
		unlock in f06
		unlock in f05

		select f06
		scatte memvar blank
		w_d6tiim=''
		w_tiim=''
		w_tido=''

		UNLOCK IN f07
		SELE f07
		SCATTER MEMVAR BLANK
		
		select (w_alias)
		scatter memvar blank

		store '' to w_coco, w_rubr, w_clie, w_juri, w_ttip, m.d6nocl,			;
					w_pere,w_cpib,w_cpga,w_cpot,w_per1,w_per2,w_per3

		m.d5empr=w_5empr
		m.d5peri=w_5peri
		m.d5tipo=w_5tipo
		m.d5sucu=w_5sucu
		m.d5tico=w_5tico
		m.d5tifo=w_5tifo
		m.d5titr=w_5titr
		m.d5por1=w_5por1

		d0ivan=0
		d0ivaa=0

		w_modicampo=.f.
		_curobj=1
		
	endif
endif

show gets

return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc clav05
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
if w_recn<>0
	w_recn=recno(w_alias)
endif

w_orde=order(w_alias)
*set orde to 'd5peri' in (w_alias)

set orde to 'd5comp' in (w_alias)

*if seek(w_pref+&w_clave,w_alias)   && R.19 Mariano
if &w_clave.=ptoi(f05.d5peri)+f05.d5tipo+f05.d5sucu+f05.d5tico+f05.d5tifo+f05.d5nume+f05.d5clie or seek(w_pref+&w_clave,w_alias)   && R.19 Mariano
	if !w_call	&& Modif/Baja
		if w_recn<>recno(w_alias) .or. .not. st_rlock(w_alias)
			do work05
			w_recn=recno(w_alias)
		endif
	else	&& Alta
		if !w_dupl
			*			show get m.d5nume
			*			wait wind 'Ya se encuentra el comprobante'
			*			_curobj=1
		endif
	endif

else

	if !w_call	&& Modif/Baja
		if w_recn<>recno(w_alias)
			do work05
			w_recn=recno(w_alias)
		endif

		if wontop('iws005c4')
			w_nuagperc=''
			w_dvagperc=''
			w_clie=''
			show get w_nuagperc
			show get w_dvagperc
			show get w_clie
		endif
		
		wait wind 'No se encuentra el comprobante'
		_curobj=1
	endif

endif

set orde to (w_orde) in (w_alias)
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc work05
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
private w_aux

select (w_alias)

if !(d5peri=w_5peri and d5tipo=w_5tipo and d5tico=w_5tico) && se fue de periodo o cambio el tipo o tico
	if (lastkey()=p_f8 and !(d5peri=w_5peri and d5tipo=w_5tipo and d5tico=w_5tico)) or lastkey()=31	&& ctrl+pgup
		*		=start(ptoi(w_5peri)+w_5tipo+w_5sucu+' '+w_5tico, w_alias)
		=start(ptoi(w_5peri)+w_5tipo+w_5sucu+w_5tico+' ', w_alias)
	endif
	if lastkey()=p_f8	or lastkey()=31	&& me pase para arriba
		do while d5peri<=w_5peri and !(d5peri=w_5peri and d5tipo=w_5tipo and d5tico=w_5tico) and !eof(w_alias)
			skip
		enddo
	endif
	if (lastkey()=p_f7 and !(d5peri=w_5peri and d5tipo=w_5tipo and d5tico=w_5tico)) or lastkey()=30	&& ctrl+pgdn
		w_aux=str(val(ptoi(w_5peri))+1,6)	&& periodo siguiente
		*		=start(w_aux+w_5tipo+w_5sucu+' '+w_5tico, w_alias)
		=start(w_aux+w_5tipo+w_5sucu+w_5tico+' ', w_alias)
	endif
	if lastkey()=p_f7 or lastkey()=30	&& me pase para abajo
		do while (d5peri>=w_5peri or eof(w_alias)) and !(d5peri=w_5peri and d5tipo=w_5tipo and d5tico=w_5tico) and !bof(w_alias)
			skip -1
		enddo
	endif
endif

** si no encontre el comienzo o el fin me voy al eof() y limpia variables
if bof(w_alias) or !(d5peri=w_5peri and d5tipo=w_5tipo and d5tico=w_5tico)
	go bottom in (w_alias)
	if !eof(w_alias)	&& R.05 Mariano
		skip in (w_alias)
	endif	&& R.05 Mariano
endif

if !eof(w_alias)
	** libero registros tomado antes ***
	unlock in f05
	unlock in f06
	unlock in f07
	***********************************

	*	=rlock('f05')
	=rlock(w_alias)
	if .not. st_rlock(w_alias,.t.)

		wait window 'Este comprobante esta tomado por otra terminal' timeout 1

		do while !st_rlock(w_alias,.t.)
			** recorro dentro del periodo **
			do while (d5peri=w_5peri and d5tipo=w_5tipo and d5tico=w_5tico) and !eof(w_alias)
				=rlock(w_alias)
				if st_rlock(w_alias,.t.)
					exit
				endif
				skip
			enddo
			if !(d5peri=w_5peri and d5tipo=w_5tipo and d5tico=w_5tico) or eof(w_alias)
				** analogo al go top para el periodo actual **
				=start(ptoi(w_5peri)+w_5tipo+w_5sucu+w_5tico+' ', w_alias)
				do while !(d5peri=w_5peri and d5tipo=w_5tipo and d5tico=w_5tico) and !eof(w_alias)
					skip
				enddo
			else
				exit
			endif
		enddo

		_curobj=objnum(d5tifo)

	endif

	scatter memvar

	if seek(m.d5alic,'f23')
		d0ivan	= f23.d23alic
		d0ivaa	= f23.d23acre
		SHOW GETS
	endif


	if seek(m.d5inte,'f06')
		sele f06
		=rlock('f06')
		scatter memvar
		=seek(m.d5clie,'f03')
		w_d6tiim=f03.d3tipo
		sele (w_alias)
	else
		sele f06
		scatter memvar blank
		=seek(m.d5clie,'f03')
		w_d6tiim=f03.d3tipo
		m.d6tido=f03.d3tido
		m.d6cuit=f03.d3cuit
		if wontop('iws005c4')  
			w_nuagperc=f03.d3nagp
			w_dvagperc=f03.d3dagp 
		endif
		sele (w_alias)
	endif

	if seek('TI'+w_d6tiim,'f01t')
		w_tiim=f01t.desc
	else
		w_tiim=''
	endif
	if seek('TD'+m.d6tido,'f01t')
		w_tido=f01t.desc
		if m.d6tido$'80.86.87' and empty(strtran(m.d6cuit,'-',''))		&& R.03 Ra๚l valida CDI
			m.d6cuit='  -        - '
		endif
	else
		w_tido=''
	endif

else
	scatter memvar blank fields except d5empr,d5peri,d5tipo,d5sucu,d5tifo,d5tico,d5nume,d5titr
	sele f06
	scatter memvar blank
	w_d6tiim=''
	w_tiim=''
	w_tido=''
	sele (w_alias)
endif

if seek(m.d5inte,'f07')
		sele f07
		=rlock('f07')
		scatter memvar
		sele (w_alias)
else
	sele f07
	scatter memvar blank
	sele (w_alias)
endif

SHOW GETS

if w_rdlevel<>rdlevel()
	=valid05(.t.)
	show gets
endif

if .not. eof(w_alias)
	=rlock(w_alias)
	if .not. st_rlock(w_alias,.t.)
		_curobj=1
	endif
endif

set message to csalida


return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc when05
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
w_retu=.t.
w_vari=varread()
p_curobj=_curobj

&& R.17b Mariano
IF w_dupl
	SHOW GET m.d5nume DISABLE
	SHOW GET m.d5clie DISABLE
*	SHOW GET m.d5juri DISABLE	&& R.22 Mariano
	SHOW GET m.d5fech DISABLE
	show get m.d5tifo disable
	show get m.d5coco disable
	show get m.d5leye disable
	show get m.d5ttip disable
	show get m.d5cond disable
	show get m.d5tiop disable
ELSE
	SHOW GET m.d5nume ENABLE
	SHOW GET m.d5clie ENABLE
	SHOW GET m.d5juri ENABLE
	SHOW GET m.d5fech ENABLE
	show get m.d5tifo enable
	show get m.d5coco enable
	show get m.d5leye enable
	show get m.d5ttip enable
	show get m.d5cond enable
	show get m.d5tiop enable
ENDIF
&& R.17e Mariano

*===> Posiciona el cursor en el bot๓n invisible de la pantalla informativa de facturas duplicadas
IF wontop("iws005vc")
		curobj = objnum(w_n)
		SHOW GETS
endif

if wontop('iws005c1') and wexist('calculator')
	do borracalcu
endif

*===> VALIDA QUE ESTEN DEFINIDOS IB y GANANCIAS
IF !WONTOP('iws005c')
	IF f00.p0prc1 <> 1 AND f00.p0prc2 <> 1 AND f00.p0prc3 <> 1
			w_cpib = "INDEFINIDO ING. BRUTOS"
			SHOW GETS
			SHOW GET m.d5cpib	DISABLE
	ENDIF			
	IF f00.p0prc1 <> 2 AND f00.p0prc2 <> 2 AND f00.p0prc3 <> 2
			w_cpga = "INDEFINIDO GANANCIAS"
			SHOW GETS
			SHOW GET m.d5cpga	DISABLE
	ENDIF			
ENDIF

if wontop('iws005c')
	=irclave('D5PERI','wopci')
endif

if wontop('iws005c1')
	IF w_dupl
			=skipbar('Registro','Totales Factura Duplicada',.F.)
			=skipbar('Registro','Duplicar',.T.)
	ELSE
		=skipbar('Registro','Totales Factura Duplicada',.T.)
		=skipbar('Registro','Duplicar',.F.)
	ENDIF
	
	DO sumatoria
	
	DO alta

	if w_vari='W_D6TIIM'
		w_retu=.f.
	endif

	if w_vari='D6NOCL'
		w_retu=hclev
	endif

	if w_vari='D6TIDO'
		&& R.27b Mariano
		IF VERSION()='Visual FoxPro'
			IF m.d6dire='_' AND !m.d6tido$'80.86.87'
				m.d6dire=SPACE(LEN(f06.d6dire))
			ENDIF
			show get m.d6dire
		ENDIF 
		&& R.27e Mariano
		w_retu=hclev
	endif

	if w_vari='D6CUIT'
		&& R.27b Mariano
		IF VERSION()='Visual FoxPro'
			IF EMPTY(m.d6cuit) AND !m.d6tido$'80.86.87'
				m.d6cuit='_            '
			ENDIF
			IF EMPTY(m.d6dire) AND !m.d6tido$'80.86.87'
				m.d6dire='_'+SPACE(LEN(f06.d6dire)-1)
			ENDIF
			show get m.d6dire
		ENDIF 
		&& R.27e Mariano
		show get m.d6cuit
		w_retu=hclev
	endif

	***vfp***
	if version()='Visual FoxPro'
		if w_vari='D6CUIT' and hclev
			keyb '{end}'+'{home}'+'{shift+rightarrow}'+'{shift+end}'
		endif
	endif
	***vfp***

	if w_vari='D6DIRE'
		&& R.27b Mariano
		IF VERSION()='Visual FoxPro'
			IF m.d6cuit='_' AND !m.d6tido$'80.86.87'
				m.d6cuit='             '
			ENDIF
			show get m.d6cuit
		ENDIF 
*		w_retu=hclev and d0coim$'12'
		w_retu=hclev
		&& R.27e Mariano
	endif

	if w_vari='D6LOCA'
		&& R.27b Mariano
		IF VERSION()='Visual FoxPro'
			IF VERSION()='Visual FoxPro'
				IF m.d6cuit='_' AND !m.d6tido$'80.86.87'
					m.d6cuit='             '
				ENDIF
				IF m.d6dire='_' AND !m.d6tido$'80.86.87'
					m.d6dire=SPACE(LEN(f06.d6dire))
				ENDIF
				show get m.d6dire
			ENDIF 
		endif
*		w_retu=hclev and d0coim$'12'
		w_retu=hclev
		&& R.27e Mariano
	endif

	if w_vari='D6COPO'
*		w_retu=hclev and d0coim$'12'	&& R.27 Mariano
		w_retu=hclev	&& R.27 Mariano
	endif

	if w_vari='D5POR1' and !(m.d5titr ='E')
		w_retu=.f.
	endif

*===*****************************************************************
	if w_vari='D5TOTA' and !(m.d5titr ='E')
		w_retu=.f.
		_curobj=objnum(m.d5imp8)
	else
		if lastkey()=7		&& ctrl+g --> sale de calculadora
			m.d5tota=_calcvalue
			show get m.d5tota
		endif
	endif
******************************************************************

	if w_vari='D5IMP1'
		if lastkey()=7		&& ctrl+g --> sale de calculadora
			m.d5imp1=_calcvalue
			show get m.d5imp1
			m.d5tota = m.d5imp1+m.d5imp2+m.d5imp3+m.d5imp4+m.d5imp5+m.d5imp6+m.d5imp7+m.d5imp8
			show get m.d5tota
		endif
	endif

	if w_vari='D5IMP2'
		if lastkey()=7		&& ctrl+g --> sale de calculadora
			m.d5imp2=_calcvalue
			show get m.d5imp2
			m.d5tota = m.d5imp1+m.d5imp2+m.d5imp3+m.d5imp4+m.d5imp5+m.d5imp6+m.d5imp7+m.d5imp8
			show get m.d5tota
		endif
	endif

	if w_vari='D5IMP3'
		if lastkey()=7		&& ctrl+g --> sale de calculadora
			m.d5imp3=_calcvalue
			show get m.d5imp3
			m.d5tota = m.d5imp1+m.d5imp2+m.d5imp3+m.d5imp4+m.d5imp5+m.d5imp6+m.d5imp7+m.d5imp8
			show get m.d5tota
		endif
	endif

	if w_vari='D5IMP4'
		if lastkey()=7		&& ctrl+g --> sale de calculadora
			m.d5imp4=_calcvalue
			show get m.d5imp4
			m.d5tota = m.d5imp1+m.d5imp2+m.d5imp3+m.d5imp4+m.d5imp5+m.d5imp6+m.d5imp7+m.d5imp8
			show get m.d5tota
		endif
	endif

	if w_vari='D5IMP5'
		if lastkey()=7		&& ctrl+g --> sale de calculadora
			m.d5imp5=_calcvalue
			show get m.d5imp5
			m.d5tota = m.d5imp1+m.d5imp2+m.d5imp3+m.d5imp4+m.d5imp5+m.d5imp6+m.d5imp7+m.d5imp8
			show get m.d5tota
		endif
	endif

	if w_vari='D5IMP6'
		if lastkey()=7		&& ctrl+g --> sale de calculadora
			m.d5imp6=_calcvalue
			show get m.d5imp6
			m.d5tota = m.d5imp1+m.d5imp2+m.d5imp3+m.d5imp4+m.d5imp5+m.d5imp6+m.d5imp7+m.d5imp8
			show get m.d5tota
		endif
	endif

	if w_vari='D5IMP7'
		if lastkey()=7		&& ctrl+g --> sale de calculadora
			m.d5imp7=_calcvalue
			show get m.d5imp7
			m.d5tota = m.d5imp1+m.d5imp2+m.d5imp3+m.d5imp4+m.d5imp5+m.d5imp6+m.d5imp7+m.d5imp8
			show get m.d5tota
		endif
	endif

	if w_vari='D5IMP8'
		if lastkey()=7		&& ctrl+g --> sale de calculadora
			m.d5imp8=_calcvalue
			show get m.d5imp8
			m.d5tota = m.d5imp1+m.d5imp2+m.d5imp3+m.d5imp4+m.d5imp5+m.d5imp6+m.d5imp7+m.d5imp8
			show get m.d5tota
		endif
	endif

*	IF w_vari = "D5IMFI"	AND (m.d5tico = "6"		OR m.d5tico = "?")	&& R.14 Mariano
	IF w_vari = "D5IMFI"	AND (m.d5tico $ "6.E.D"		OR m.d5tico = "?")	&& R.14 Mariano
				m.d5imfi = "S"
	ENDIF

endif

if wontop('iws005c4')

	DO sumatoriart

	DO alta

*===> VALIDA QUE ESTEN DEFINIDOS OTROS
	IF f00.p0prc1 <> 3 AND f00.p0prc2 <> 3 AND f00.p0prc3 <> 3
			w_cpot = "INDEFINIDO OTROS"
			SHOW GETS
			SHOW GET m.d5cpot	DISABLE
	ENDIF			

	IF w_dupl
			=skipbar('Registro','Totales Factura Duplicada',.F.)
			=skipbar('Registro','Duplicar',.T.)
	ELSE
		=skipbar('Registro','Totales Factura Duplicada',.T.)
		=skipbar('Registro','Duplicar',.F.)
	ENDIF
	
	if w_vari='D5PERE' 
		if !empt(m.d5cpib+m.d5cpga+m.d5cpot)
			w_retu=.f.
		endif
	endif
	if w_vari='D5CPIB' 
		if !empt(m.d5pere+m.d5cpga+m.d5cpot)
			w_retu=.f.
		endif
	endif
	if w_vari='D5CPGA' 
		if !empt(m.d5pere+m.d5cpib+m.d5cpot)
			w_retu=.f.
		endif
	endif
	if w_vari='D5CPOT' 
		if !empt(m.d5pere+m.d5cpib+m.d5cpga)
			w_retu=.f.
		endif
	endif

*===> Valida que alg๚n campo tenga info
	IF w_vari = "D5IMRE"
		IF EMPTY(m.d5pere + m.d5cpib + m.d5cpga + m.d5cpot)
			_curobj	= objnum(m.d5pere)
			w_retu	= .F.
		ENDIF
	ENDIF

*	DO alta
endif
 
return(w_retu)

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc valid05
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
parameters w_show
private w_recno,w_orde

***vfp***
*if version()='Visual Fox' and (w_pfile!='05' or (!wexist('IWS005C') and wontop()!='IWS005C1')) and w_show	&& R.16 Mariano
*if version()='Visual Fox' and (w_pfile!='05' or (wontop()!='IWS005C' and wontop()!='IWS005C1'))	&& R.16 Mariano	&& R.19 Mariano
if version()='Visual Fox' and (w_pfile!='05' or (wontop()!='IWS005C' and wontop()!='IWS005C1' and wontop()!='IWS005C4'))		&& R.19 Mariano
	if lastkey()=p_esc
		return .t.
	else
		return .f.
	endif
endif
***vfp***

if empty(parameters())
	w_show=.f.
endif

w_retu=.t.
w_vari=varread()
w_vread='m.'+w_vari

if wontop('iws005c1') and wexist('calculator')
	do borracalcu
endif

if wontop('iws005c')

	if w_vari='D5PERI' or w_show

		if lastkey()=p_f4
			do fhelp with 'D5PERI','','iwp004','f04','d4peri','d4peri','',(p_char),'w_retu'
		endif
		w_fech	= CTOD('01/' + m.d5peri)
		IF EMPTY(w_fech)
					RETURN	.F. 
		ENDIF
		IF TRDATE('w_fech')
				m.d5peri	= RIGHT(DTOC(w_fech),7)
				SHOW GET m.d5peri
		ELSE
			IF !w_show
					WAIT WIND "El perํodo no es correcto"
					w_retu	= .F.
					_curobj	= objnum(m.d5peri)
					RETURN .F.
			ENDIF
		ENDIF
		wkperi = ptoi(m.d5peri)

		if !seek(wkperi,'f04')
			if !w_show
				wait window 'Perํodo Inexistente'
			endif
			w_retu=.f.
			*			hwopci=.f.
		else
			if f04->d4esco='C'
				wait window 'Perํodo Cerrado'
				if w_call
					w_retu=.f.
				endif
			endif
			*			hwopci=.t.
			*			d0coim=f04->d4coim
		endif
		w_5peri=m.d5peri	&& variable para el titulo del screen

		w_5fech		= CTOD('01/'+m.d5peri)
		m.d5fech	= w_5fech
	endif

	if w_vari='WOPCI' or w_show

		d0coim=f04->d4coim

		scatter memvar blank fields except d5tipo,d5empr,d5peri,d5inte

		do case
			case wopci='Facturas'
				m.d5tico='A'
			case wopci='rEcibo-factura'
				m.d5tico='8'
			case wopci='notas de D้bito'
				m.d5tico='B'
			case wopci='notas de Cr้dito'
				m.d5tico='C'
			case wopci='Tickets'
				m.d5tico='?'
			case wopci='ticKet-factura'
				m.d5tico='6'
			&& R.14b Mariano
			case wopci='ticKet-NC'
				m.d5tico='E'
			case wopci='ticKet-ND'
				m.d5tico='D'
			&& R.14e Mariano
			case wopci='Recibos'
				m.d5tico='@'
			case wopci='Liq. Varias'
				m.d5tico='7'
			case wopci='reteNciones'
				m.d5tico='R'
				
		endcase
		
		IF m.d5tico <> "7"
						w_ivapr	= "Per. IVA :"
						w_ibpr	= "Per. IB :"
						w_ganpr	= "Per. Gcias. :"
		ELSE
			w_ivapr	= "P/R IVA :"
			w_ibpr	= "P/R IB :"
			w_ganpr	= "P/R Gcias. :"
		ENDIF
		
		wwtifo=tiformc(m.d5tico)	&& 'ABCIMZ'
		m.d5tifo=left(wwtifo,1)

		w_5tico=m.d5tico
		w_5sucu=m.d5sucu

		m.d5titr='N'
		m.d5por1=0

		w_recn=0
		w_modicampo=.f.

		w_opci	= wopci
		wopci=upper(wopci)

		push menu _msysmenu &&(Salva estado de menu)
		do iwm005c.mpr

		if w_agre
			on key label f9 keyboard '{ctrl+f9}'
		endif

		=skipbar('Registro','Insertar',.t.)
		=skipbar('Registro','Ordenado por Descripci๓n',.t.)
		if w_call	&& Alta
			=skipbar('Registro','Consultar',.t.)
			=skipbar('Registro','Buscar',.t.)
			=skipbar('Registro','Modificar',.t.)
			=skipbar('Registro','Eliminar' ,.t.)
			=skipbar('Registro','Ir al Anterior',.t.)
			=skipbar('Registro','Ir al Siguiente',.t.)
			=skipbar('Registro','Ir al Primero',.t.)
			=skipbar('Registro','Ir al Ultimo',.t.)
		endif

		w_5fech=ctod('01/'+m.d5peri)
		if m.d5tico='8'
			w_5cond='3'	&& Recibo-Factura
		else
			w_5cond='1'	&& Cuenta Corriente
		endif

		select f06
		scatte memvar blank
		w_d6tiim=''
		w_tiim=''
		w_tido=''
		d0ivan=0
		d0ivaa=0

*===> Para el total de facturas duplicadas
			x_texto		= ""		&& R.13 Mariano (w_texto)
			w_d5imp1a	= 0
			w_d5imp2a	= 0
			w_d5imp3a	= 0
			w_d5imp4a	= 0
			w_d5imp5a	= 0
			w_d5imp6a	= 0
			w_d5imp7a	= 0
			w_d5imp8a	= 0
			x_d5totaa	= 0		&& R.13 Mariano (w_d5totaa)
			w_dupl		= .F.

			&& R.01b Mariano
			SELE f25
			IF (m.d5tico = "7"	 OR		m.d5tico = "O")  && Liquidaciones Varias
					set filter to d25tdgi = cafip
			ELSE
				IF m.d5tico = 'R'
					set filter to  d25refe = "R" AND d25tdgi = cafip
				ELSE
					set filter to  d25refe = "P" AND d25tdgi = cafip
				ENDIF
			ENDIF
			sele (w_alias)
			&& R.01e Mariano

		&& R.19b Mariano
		if version()='Visual FoxPro'
			p_curobj=0
		endif
		&& R.19e Mariano
		
		if m.d5tico='R'
			m.d5tifo=' '
			do iws005c4.spr
		else
			do iws005c1.spr		
		endif			

		** libero registros tomado antes ***
		unlock in f05
		unlock in f06
		UNLOCK IN f07


		***********************************
		wopci	= w_opci
		pop menu _msysmenu &&(recupera estado de menu)
		release window iws005c1,barraabm
	endif

	=ultdepri('D5PERI','wopci')
	=prideult('WOPCI','wopci')
	=modicampo('D5PERI','D5SUCU')

	w_modicampo=.f.	&& R.27 Mariano

endif

if wontop('iws005c1') OR wontop('iws005vc')

	if w_vari='D5TIFO' or w_show
		if m.d5tifo$wwtifo or empty(m.d5tifo+wwtifo)
			if !w_show
				*				w_5tifo=m.d5tifo
				*				if !seek(&w_clave,'f05')
				*					w_recn=0
				*				endif
			endif
		else
			if !w_show
				wait window 'Formulario Incorrecto ...'
			endif
			w_retu=.f.
		endif
		if empty(m.d5ttip)	&& R.21 Mariano
			m.d5ttip = "0"
		endif	&& R.21 Mariano
		IF !w_show
*			m.d5coco = TCOCO(m.d5tico, m.d5tifo)	&& R.06 Mariano
			if w_call or empty(m.d5coco)	&& R.16 Mariano
				m.d5coco = TCOCO(m.d5tico, m.d5tifo, m.d5tipo)	&& R.06 Mariano
			endif	&& R.16 Mariano
			&& R.13 Mariano
			if m.d5coco='066' and m.d5tiop='0'
				m.d5tiop='X'
				=SEEK("TO" + m.d5tiop,"f01t")
				w_tiop	= f01t.desc
			endif
			&& R.13e mariano			
		ENDIF
		=SEEK("CO" + m.d5coco,"f01t")
		w_coco	= f01t.desc
		SHOW GETS
	endif

	if w_vari='D5NUME' or w_show
		m.d5nume=strcomp(m.d5nume)
	endif

	if w_vari='D5TITR' or w_show
		if !m.d5titr$'NE'
			if !w_show
				wait window 'Debe ser [N->Normal,E->Especial]'
			endif
			w_retu=.f.
		endif
	endif

	if w_vari='D5COCO' or w_show
		do fhelp with 'D5COCO','CO','iwp001t','f01t','clav','desc','Tipo de Comprobante Inexistente',(p_strc),'w_retu'
	endif

	if w_vari='D5RUBR' or w_show
		&& R.26b Mariano
		if w_vari='D5RUBR' and !empty(m.d5clie) and empty(m.d5rubr) and !empty(f03.d3rubr) and w_retu
			m.d5rubr=f03.d3rubr
		endif
		&& R.26e Mariano
		
		do fhelp with 'D5RUBR','RC','iwp001','f01','d1cla','d1des','Rubro inexistente',(p_strc),'w_retu'
		if w_retu AND w_vari = "D5RUBR"
			&& R.18b Mariano
*			=seek(f01.d1alic,'f23')
			w_5alic	= m.d5alic
			if empty(m.d5alic) or w_call	&& en Alta
				=seek(f01.d1alic,'f23')
				m.d5alic=f01.d1alic
			else
				=seek(m.d5alic,'f23')
			endif
			&& R.18e Mariano
			&& R.21b Mariano
			if m.d5tifo='C'
				m.d5alic='000'
				=seek(m.d5alic,'f23')
			endif
			&& R.21e Mariano
			if !found('f23')
				wait window 'Rubro sin Alํcuota de IVA '
				w_retu=.f.
			else
				d0ivan=f23.d23alic
				d0ivaa=f23.d23acre
*				w_5alic	= m.d5alic	&& R.18 Mariano
*				m.d5alic=f01.d1alic	&& R.18 Mariano
				show get m.d5alic
			endif
			show get d0ivan
			show get d0ivaa
			w_5rubr=m.d5rubr
		endif
	endif

	if w_vari='D5ALIC' or w_show
		do fhelp with 'D5ALIC','','iwp023','f23','d23codi','d23desc','Alํcuota Inexistente',(p_strc),'w_retu'
		if w_retu AND w_vari='D5ALIC' 
			d0ivan=f23.d23alic
			d0ivaa=f23.d23acre
			show get d0ivan
			show get d0ivaa

*===>	Muestra el porc. de IVA en la parte de totales y calcula el IVA si d5imp1 <> 0
			m.d5por1 = d0ivan
			IF m.d5imp1 <> 0
						m.d5imp4 = round(m.d5imp1 * d0ivan / 100, d0deci)
						m.d5tota = m.d5imp1+m.d5imp2+m.d5imp3+m.d5imp4+m.d5imp5+m.d5imp6+m.d5imp7+m.d5imp8
			ENDIF
			SHOW GETS

		endif
	endif

	if w_vari='D5CLIE' or w_show
		do fhelp with 'D5CLIE','','iwp003','f03','d3prov','d3nomb','Proveedor inexistente',(p_strc),'w_retu'
		&& R.26b Mariano
		if !empty(m.d5clie) and (empty(m.d5rubr) or empty(m.d5alic)) and !empty(f03.d3rubr) and w_retu
			m.d5rubr=f03.d3rubr
			show get m.d5rubr
			w_rubr=''
			show get w_rubr
		endif
		if !empty(m.d5rubr) and empty(w_rubr) and w_retu
			=seek('RC'+m.d5rubr,'f01')
			w_rubr=f01.d1des
			show get w_rubr
		endif
		&& R.26e Mariano
	endif

	if w_vari='D5CLIE' OR w_show

		*** verifica que no haya cargado esta factura antes ***

		if w_vari='D5CLIE' and w_retu

			if !w_call && modificacion
				if !eof(w_alias)
					w_recno=recno(w_alias)
				else
					w_recno=0
				endif
			endif

			w_orde=order(w_alias)
			set order to 'd5tipo' in (w_alias)

			sele(w_alias)

			if seek(m.d5tipo+m.d5sucu+m.d5tifo+m.d5tico+left(m.d5nume+space(17),17)+m.d5clie,w_alias)	&& R.18 Mariano (m.d5nume)
				do while m.d5tipo+m.d5sucu+m.d5tifo+m.d5tico+left(m.d5nume+space(17),17)+m.d5clie=d5tipo+d5sucu+d5tifo+d5tico+d5nume+d5clie ;
						and !eof(w_alias)	&& R.18 Mariano (m.d5nume)
*					if (w_call and !w_dupl) or w_5peri!=d5peri && R.18 Mariano
					if (w_call and !w_dupl) && R.18 Mariano	(doy de alta sin duplicar)
						** si repite en distinto periodo va a chillar **
						if w_5peri!=d5peri && R.18 Mariano
							&& R.18b Mariano
							skip in &w_alias
							do while m.d5tipo+m.d5sucu+m.d5tifo+m.d5tico+left(m.d5nume+space(17),17)+m.d5clie=d5tipo+d5sucu+d5tifo+d5tico+d5nume+d5clie ;
								and w_5peri!=d5peri and !eof(w_alias)	&& R.18 Mariano (m.d5nume)
								skip in &w_alias
							enddo
							skip -1 in &w_alias
							&& R.18e Mariano								
							if not w_show 
  								=advsuave('El comprobante se encuentra en el perํodo '+d5peri)
*								w_retu=.f.
							endif
*							exit	&& R.18 Mariano
					&& R.18b Mariano
						else
							** si repite en mismo periodo va a chillar y no lo deja, ya que deberia duplicar**
							if !w_show 
								=advsuave('El comprobante ya se encuentra en el perํodo '+d5peri)
								w_retu=.f.
							endif
						endif
					else
						** si esta duplicando no valido, ya que quiero repetir en el mismo periodo **
						exit
					endif
					&& R.18e Mariano
					skip in alias (w_alias)
				enddo
			endif

			set orde to (w_orde) in (w_alias)

			if w_call or w_recno=0
				go bottom in alias (w_alias)
				if !eof()
					skip 	&& cuando doy de alta voy al eof()
				endif
			else	&& cuando modifico vuelvo donde estaba
				go w_recno in alias (w_alias)
			endif

		endif

		if w_vari='D5CLIE' and w_retu

			m.d5tiim = ftiimp(f03->d3prov,m.d5peri,'P')

			* Cambia jurisdiccion de acuerdo al cliente
			if empty(m.d5juri) or w_5clie<>m.d5clie
				m.d5juri=f03->d3juri
*				show get m.d5juri
			endif

			w_5juri	= m.d5juri
			=SEEK("JU" + m.d5juri, "f01")
			w_juri	= f01.d1des
			cafip	= f01.d1tdgi

			* Se Verifica que se Haya generado El Formulario Correcto
			* --------------------------------------------------------
*			wctifo=iif(m.d5tiim$'1','ABIM ','CI ')		&& R.05 Mariano
			wctifo=iif(m.d5tiim$'1','ABIM ','BCI ')		&& R.05 Mariano
			if !m.d5tifo$wctifo
				if !w_show
					wait window 'Formulario Incorrecto'
				endif
				w_retu=.f.
			endif

			if m.d5clie>'99989' and w_retu
				hclev=.t.
				if empty(m.d6nocl) or m.d5clie<>w_5clie
					select f06
					scatte memvar blank
					select (w_alias)
					m.d6nocl=w_clie
					show get m.d6nocl
					show get m.d6dire
					show get m.d6loca
					show get m.d6copo
				endif
			else
				hclev=.f.
				select f06
				scatte memvar blank
				w_d6tiim=''
				w_tiim=''
				w_tido=''
				select (w_alias)
				show get m.d6nocl
				show get m.d6dire
				show get m.d6loca
				show get m.d6copo
			endif

			if w_retu
				=seek(m.d5clie,'f03')
				w_d6tiim=f03.d3tipo
				if empty(m.d6tido) or m.d5clie<>w_5clie
					m.d6tido=f03.d3tido
					m.d6cuit=f03.d3cuit
					&& R.27b Mariano
					if m.d5clie<='99990'
						m.d6dire=f03.d3dire
						m.d6loca=f03.d3loca
						m.d6copo=f03.d3copo
						show get m.d6dire
						show get m.d6loca
						show get m.d6copo
					endif
					&& R.27e Mariano
				endif
				if seek('TI'+w_d6tiim,'f01t')
					w_tiim=f01t.desc
				else
					w_tiim=''
				endif
				if seek('TD'+m.d6tido,'f01t')
					w_tido=f01t.desc
					if m.d6tido$'80.86.87' and empty(strtran(m.d6cuit,'-',''))			&& R.03 Ra๚l valida CDI
						m.d6cuit='  -        - '
					endif
				else
					w_tido=''
				endif
				show get w_d6tiim
				show get w_tiim
				show get m.d6tido
				show get w_tido
				show get m.d6cuit
			endif

			&& R.20b Jonatan
			if w_retu and lastkey()=p_enter and  (m.d5coco='033' or m.d5coco='063' or m.d5coco='059' or m.d5coco='060')
				=skipbar('Registro','Agregar',.t.)
				=skipbar('Registro','Modificar',.t.)
				do iws005c5.spr
				if w_call
					=skipbar('Registro','Agregar',.f.)
				else
					=skipbar('Registro','Agregar',.f.)
				=skipbar('Registro','Modificar',.f.)
				endif
			endif
			&& R.20e Jonatan
			
			if w_retu and lastkey()=p_enter and f03.d3icai='S'
	
				&& R.23b Mariano
				if empty(m.d5imfi)
					m.d5imfi='N'
				endif
				&& R.23e Mariano
				
				m.d5ncai=f03.d3ncai
				m.d5vcai=f03.d3vcai
				m.d5dcai=f03.d3dcai

				show get m.d5clie
				clear typeahead
				do iws005c2.spr  	&& Ingresa CAI
				_curobj=objnum(m.d5juri)	   		&& d5juri
			endif						
			if (m.d5tifo<>w_5tifo or m.d5nume<>w_5nume or m.d5clie<>w_5clie or empty(m.d5leye)) and w_retu
					m.d5leye=readf('f03',m.d5clie,'d3nomb',10)+' '+tcabre(m.d5tico)+' '	&& R.27 Mariano (10 x 7)

*					m.d5leye=m.d5leye+m.d5tifo+m.d5sucu+'-'+m.d5nume	&& R.27 Mariano
					m.d5leye=m.d5leye+m.d5tifo+iif(empty(m.d5sucu),' ',' '+m.d5sucu+'-')+m.d5nume	&& R.27 Mariano

					&& R.19b Mariano
					if len(rtrim(m.d5leye))>30
						m.d5leye=left(m.d5leye,26)+right(m.d5leye,4)
					else
						m.d5leye=left(m.d5leye,30)
					endif
					&& R.19e Mariano
			
					show get m.d5leye

					w_5clie=m.d5clie
					w_5tifo=m.d5tifo
					w_5nume=m.d5nume

			endif

		endif
	endif


	if (w_vari='D6TIDO' or w_show) and m.d5clie>'99989'
		
		if !w_show
			do fhelp with 'D6TIDO','TD','iwp001t','f01t','clav','desc','Tipo de Documento Inexistente',(p_strc),'w_retu'
		endif
		if w_retu
			&& R.12b Mariano
			if m.d5tifo$'A.M'
				if !m.d6tido$'80.86.87'
					if !w_show
						wait window 'Deberia ser 80-CUIT o 86-CUIL o 87-CDI'
					endif
					w_retu=.f.
				endif
			endif
			&& R.12e Mariano
			if w_retu	&& R.11 Mariano
				if m.d6tido$'80.86.87' and empty(strtran(m.d6cuit,'-','')) 		&& R.03 Ra๚l valida CDI
					m.d6cuit='  -        - '
				endif
			endif	&& R.12 Mariano
		endif
	endif

	if (w_vari='D6CUIT' or w_show) and m.d5clie>'99989'
		if m.d6tido$'80.86.87'									&& R.03 Ra๚l valida CDI
			if dicuit(left(m.d6cuit,11))<>right(m.d6cuit,1) and !empty(strtran(m.d6cuit,'-',''))
				if !w_show
					wait window 'Error en CUIT o CUIL o CDI'	&& R.03 Ra๚l valida CDI
				endif
				w_retu=.f.
			endif
		endif
	endif

	if w_vari='D5JURI' or w_show
		do fhelp with 'D5JURI','JU','iwp001','f01','d1cla','d1des','Jurisdicci๓n inexistente',(p_char),'w_retu'
		if w_retu
			w_5juri=m.d5juri
		endif
		w_juri	= f01.d1des			
		cafip	= f01.d1tdgi
	endif

	if w_vari='D5TTIP' or w_show
		do fhelp with 'D5TTIP','GC','iwp021','f21','d21cla','d21des','Grupo inexistente',(p_char),'w_retu'
		if w_retu
			w_5ttip=m.d5ttip
		endif
	endif

	if w_vari='D5FECH' or w_show

		IF TRDATE('m.d5fech')
				SHOW GET m.d5fech
		ELSE
			IF !w_show
					WAIT WIND "La fecha no es correcta"
			ENDIF
		ENDIF

		if !empty(m.d5fech)
*			if right(dtoc(m.d5fech),7)<>m.d5peri	&& R.07 Mariano
			&& R.07b Mariano
			do case
				case ptoi(right(dtoc(m.d5fech),7))<ptoi(m.d5peri)
					if !w_show
						=advsuave("Esta ingresando un comprobante" + CHR(13) + CHR(13) + "MENOR AL PERIODO")
					endif
				case ptoi(right(dtoc(m.d5fech),7))>ptoi(m.d5peri)
					if !w_show
						=advsuave("No se puede ingresar un comprobante" + CHR(13) + CHR(13) + "MAYOR AL PERIODO")
					endif
					w_retu=.f.
			endcase
			&& R.07e Mariano
			w_5fech=m.d5fech
		ELSE
			return .F.
		endif

		if !empty(m.d5vcai)
			if m.d5fech > m.d5vcai
				wait wind 'Fecha del comprobante mayor al vencimiento del C.A.I.'
				w_retu=.f.
			endif
		endif
	endif

	if w_vari='D5COND' or w_show
		if !m.d5cond$'123'
			if !w_show
				wait window 'Condici๓n inexistente'
			endif
			w_retu=.f.
		else
			w_5cond=m.d5cond
		endif
	endif

*	if w_vari='D5TIOP'
	if w_vari='D5TIOP' or w_show	&& R.13 Mariano
		&& R.13b Mariano
*		if !m.d5tiop$'O.E.I'
*			if !w_show
*				wait window 'Debe ser [O,E,I]'
*			endif
*			w_retu=.f.
*		endif
		do fhelp with 'D5TIOP','TO','iwp001t','f01t','clav','desc','Tipo de Operaci๓n Inexistente',(p_char),'w_retu'
		&& R.13e Mariano

*		if m.d5tiop='I'	&& R.13 Mariano
*		if m.d5tiop$'X.Z' && R.13 Mariano	&& R.15 Mariano
		if m.d5tiop$'X.Z' and !w_show	&& R.15 Mariano
			&& R.15b Mariano
			=skipbar('Registro','Agregar',.t.)
			=skipbar('Registro','Modificar',.t.)
			&& R.15e Mariano
			do iws005c3.spr  &&Carga de despacho,aduana,destinacion por importacion
			&& R.15b Mariano
			if w_call
				=skipbar('Registro','Agregar',.f.)
			else
				=skipbar('Registro','Agregar',.f.)
				=skipbar('Registro','Modificar',.f.)
			endif
			&& R.15e Mariano
		endif

	endif

	if w_vari='D5PERE' or w_show
		if !empty(m.d5pere) or lastkey()=p_f4	
			sele f24
			IF m.d5tico <> "7"
						set filter to d24tipo='I' AND d24refe = "P"
						w_mens	= "Per. IVA"
			ELSE
				set filter to d24tipo='I'
				w_mens	= "Per./Ret. IVA"
			ENDIF
			sele f05 
			do fhelp with 'D5PERE','','iwp024','f24','d24codi','d24desc', w_mens + ' inexistente',(p_strc),'w_retu'
			sele f24
			set filter to
			sele f05
			if w_retu
				w_5pere=m.d5pere
				w_reii=f24.d24refe
				m.d5reiv=f24.d24refe
				SHOW GET m.d5imp3	ENABLE
			endif
		else
			m.d5reiv=space(1)
			w_reii=space(1)
			w_pere = SPACE(10)
*			m.d5imp3	= 0						&& R.04 Ra๚l
			SHOW GET m.d5imp3	DISABLE
		endif
		SHOW GET w_pere
		show get m.d5reiv
		show get w_reii
	endif

	if w_vari='D5CPIB' or w_show
		if !empty(m.d5cpib) or lastkey()=p_f4
			w_aux1 = alias()
			w_x		= .T.
			do fhelp with 'D5CPIB', (cafip),'iwp025','f25','d25codi','d25norm','Percepci๓n IB inexistente',(p_strc),'w_x'	&& R.01 Mariano		'P'
			sele &w_aux1
			IF w_x
				IF iif(w_vari='D5CPIB', w_porcib	!= f25.d25porc, .t.)
					w_porcib	= f25.d25porc
					m.d5regi=f25->d25refe
					w_5cpib=m.d5cpib
					for i=1 to 3
						w_perx='w_per'+(str(i,1))
						w_cprv='f00.p0prc'+(str(i,1))
						if &w_cprv=1
							&w_perx		=	m.d5regi
							w_param		= .T.
							DO importes WITH w_param
							w_impoib	= w_impo
							IF w_vari = 'D5CPIB'
										&w_impoib.	= (m.d5imp1 + m.d5imp2) * w_porcib / 100
							ENDIF
							SHOW GET &w_impoib
							show get &w_perx
						endif						
					endfor					
				endif
			else
				w_impoib	= ''
				w_porcib	= 0
				m.d5regi=space(1)
				w_cpib=''
				for i=1 to 3
					w_perx='w_per'+(str(i,1))
					w_cprv='f00.p0prc'+(str(i,1))
					if &w_cprv=1
						&w_perx=space(1)
						show get &w_perx
						w_param		= .F.
						DO importes WITH w_param
					endif						
				endfor					
				w_retu=.f.
			endif
		else
			w_impoib	= ''
			w_porcib	= 0
			m.d5regi=space(1)
			w_cpib=''
			for i=1 to 3
				w_perx='w_per'+(str(i,1))
				w_cprv='f00.p0prc'+(str(i,1))
				if &w_cprv=1
					&w_perx=space(1)
					show get &w_perx
					w_param		= .F.
					DO importes WITH w_param
				endif						
			endfor					
		endif			
		SHOW GET w_cpib
		show get m.d5regi
		show get m.d5cpib
	endif		

	if w_vari='D5CPGA' or w_show
		if !empty(m.d5cpga) or lastkey()=p_f4
			sele f24
			IF m.d5tico <> "7"
						set filter to d24tipo='G' AND d24refe = "P"
						w_mens	= "Per. Ganancias"
			ELSE
				set filter to d24tipo='G'
				w_mens	= "Per./Ret. Ganancias"
			ENDIF
			sele f05 
			do fhelp with 'D5CPGA','','iwp024','f24','d24codi','d24desc', w_mens + ' inexistente',(p_strc),'w_retu'
			sele f24
			set filter to
			sele f05
			if w_retu
				w_5cpga=m.d5cpga
				m.d5rega=f24.d24refe
				for i=1 to 3
					w_perx='w_per'+(str(i,1))
					w_cprc='f00.p0prc'+(str(i,1))
					if &w_cprc=2
						&w_perx=m.d5rega
						show get &w_perx
						DO importes WITH .T.
					endif						
				endfor					
			endif
		else
			m.d5rega=space(1)
			w_cpga = SPACE(10)
				for i=1 to 3
					w_perx='w_per'+(str(i,1))
					w_cprc='f00.p0prc'+(str(i,1))
					if &w_cprc=2
						&w_perx=space(1)
						show get &w_perx
						DO importes WITH .F.
					endif						
				endfor					
		endif
		SHOW GET w_cpga
		show get m.d5rega
		show get m.d5cpga
	endif

*	if m.d5tico='C'	&& R.14 Mariano
	if m.d5tico$'C.E'	&& R.14 Mariano
		m.d5imp1=abs(m.d5imp1)
		m.d5imp2=abs(m.d5imp2)
		m.d5imp3=abs(m.d5imp3)
		m.d5imp4=abs(m.d5imp4)
		m.d5imp5=abs(m.d5imp5)
		m.d5imp6=abs(m.d5imp6)
		m.d5imp7=abs(m.d5imp7)
		m.d5imp8=abs(m.d5imp8)
		m.d5tota=abs(m.d5tota)

		m.d5imp1=-m.d5imp1
		m.d5imp2=-m.d5imp2
		m.d5imp3=-m.d5imp3
		m.d5imp4=-m.d5imp4
		m.d5imp5=-m.d5imp5
		m.d5imp6=-m.d5imp6
		m.d5imp7=-m.d5imp7
		m.d5imp8=-m.d5imp8
		m.d5tota=-m.d5tota
	endif

	if w_vari='D5POR1' or w_show
*		if empty(m.d5tota) and m.d5titr='E'
		if m.d5titr='E'
					m.d5por1 = d0ivan
					_curobj=objnum(m.d5tota)
		endif
	endif

	if w_vari='D5TOTA' or w_show

		*unicamente cuando getea que ocurre cuando m.d5tiim$'34567'

*===>
*		if !empty(m.d5tota) and m.d5tiim='1' and m.d5titr='E' and empty(m.d5imp1+m.d5imp2+m.d5imp3+m.d5imp4+m.d5imp5+m.d5imp6+m.d5imp7+m.d5imp8)
		if !empty(m.d5tota) and m.d5titr='E' 

*===>
*  MAL HECHA LA CUENTA !!!!!!!!!!!!
*			m.d5imp4 = round(m.d5tota * m.d5por1 / 100, d0deci)
*			m.d5imp1 = round(div(m.d5imp4 * 100,m.d5por1), d0deci)
*			if (m.d5tota - m.d5imp1 - m.d5imp4) >=0
*										m.d5imp2 = m.d5tota - m.d5imp1 - m.d5imp4
*			else
*				m.d5imp1 = m.d5tota - m.d5imp4
*				m.d5imp2 = 0
*			endif
*===>

			m.d5imp1	= ROUND(m.d5tota * 100 / (100 + d0ivan), d0deci)

			m.d5imp4	= m.d5tota - m.d5imp1
			m.d5imp2	= 0
			SHOW GETS
		endif

	endif

	if w_vari='D5IMP1' and m.d5tiim$'1' and m.d5titr = 'N'
		* unicamente cuando getea y cl ri o rni

		if m.d5tiim$'1' and d0coim$'2'    && and empty(m.d5imp5)
			* Si es tipo 2 rni y esta en 0 se calcula el iva adicional
			m.d5imp5 = round(m.d5imp1 * d0ivaa / 100,d0deci)
			show get m.d5imp5
		endif

		if m.d5tiim$'1' and !m.d5tifo$'B'  && and empty(m.d5imp4)
			* Si esta en 0 se calcula el iva
			m.d5imp4 = round(m.d5imp1 * d0ivan / 100,d0deci)
			show get m.d5imp4
		endif

	endif

		IF !EMPTY(w_impoib)		AND !w_show
*					IF &w_impoib. = 0		OR  w_ibant	= &w_impoib.	
					IF w_vari = "D5IMP1"  OR w_vari = "D5IMP2"  OR w_vari = "D5CPIB" 
								&w_impoib.	= ROUND((m.d5imp1 + m.d5imp2) * w_porcib / 100, d0deci)
					ENDIF
		ENDIF
	m.d5tota = m.d5imp1+m.d5imp2+m.d5imp3+m.d5imp4+m.d5imp5+m.d5imp6+m.d5imp7+m.d5imp8

	show get m.d5tota

	if 'W_PER'$w_vari 
		if !eval(w_vari)$'PR '
			if !w_show
				wait window 'Debe ser [P->Percepciones,R->Retenciones]'
			endif
			w_retu=.f.
		else
			m.d5prco=eval(w_vari)	
		endif
	endif

*===> Pantalla informativa de importes xa facturas duplicadas y vuelve a d๓nde estaba
* Est se hizo con el bot๓n invisible xo Mariano lo cambi๓ x opci๓n en aYuda. Lo dejo x las dudas.
*	IF w_vari = "W-N"
*			DO iws005vc.spr
*			_curobj = objnum(m.d5imp1)
*			SHOW GETS
*	ENDIF

	if !(m.d5titr='E') 
*		if f00.p0prc3=1    && campo w_ret3 editable	&& R.11 Mariano
*			=ultdepri('D5TIFO','m.d5imp8')
*			=prideult('D5IMP8','m.d5imp8')
*			=ultdepri('D5TIFO','w_nada')	&& R.11 Mariano
*			=prideult('W_NADA','w_nada')	&& R.11 Mariano
*		endif								&& R.11 Mariano
		&& R.11b Mariano
		do case 
			case f00.p0prc3=6 or (f00.p0prc3=1 and !empty(m.d5cpib)) or (f00.p0prc3=2 and !empty(m.d5cpga)) or (f00.p0prc3>=3)    && campo w_ret3 editable	&& R.16 Mariano ((f00.p0prc3=3 and !empty(m.d5cpot))	&& R.29 Mariano (p0prc3=6 x p0prc3=4  y p0prc3>=3 x p0prc3=3)
				&& R.17b Mariano
*				=ultdepri('D5TIFO','m.d5imp8')
				if w_dupl
					=ultdepri('D5RUBR','m.d5imp8')
				else
					=ultdepri('D5TIFO','m.d5imp8')
				endif
				&& R.17e Mariano
				=prideult('D5IMP8','m.d5imp8')
			case f00.p0prc2=6 or (f00.p0prc2=1 and !empty(m.d5cpib)) or (f00.p0prc2=2 and !empty(m.d5cpga)) or (f00.p0prc2>=3)     && campo w_ret2 editable && R.16 Mariano (f00.p0prc2=3 and !empty(m.d5cpot))	&& R.29 Mariano (p0prc2=6 x p0prc2=4  y p0prc2>=3 x p0prc2=3)
				&& R.17b Mariano
*				=ultdepri('D5TIFO','m.d5imp7')
				if w_dupl
					=ultdepri('D5RUBR','m.d5imp7')
				else
					=ultdepri('D5TIFO','m.d5imp7')
				endif
				&& R.17e Mariano
				=prideult('D5IMP7','m.d5imp7')
			case f00.p0prc1=6 or (f00.p0prc1=1 and !empty(m.d5cpib)) or (f00.p0prc1=2 and !empty(m.d5cpga)) or (f00.p0prc1>=3)    && campo w_ret1 editable		&& R.16 Mariano (f00.p0prc1=3 and !empty(m.d5cpot))	&& R.29 Mariano (p0prc1=6 x p0prc1=4  y p0prc1>=3 x p0prc1=3)
				&& R.17b Mariano
*				=ultdepri('D5TIFO','m.d5imp6')
				if w_dupl
					=ultdepri('D5RUBR','m.d5imp6')
				else
					=ultdepri('D5TIFO','m.d5imp6')
				endif
				&& R.17e Mariano
				=prideult('D5IMP6','m.d5imp6')
			case !empty(w_reii)    && campo w_reii editable
				&& R.17b Mariano
*				=ultdepri('D5TIFO','m.d5imp3')
				if w_dupl
					=ultdepri('D5RUBR','m.d5imp3')
				else
					=ultdepri('D5TIFO','m.d5imp3')
				endif
				&& R.17e Mariano
				=prideult('D5IMP3','m.d5imp3')
			otherwise
				&& R.17b Mariano
*				=ultdepri('D5TIFO','m.d5imp5')
				if w_dupl
					=ultdepri('D5RUBR','m.d5imp5')
				else
					=ultdepri('D5TIFO','m.d5imp5')
				endif
				&& R.17e Mariano
				=prideult('D5IMP5','m.d5imp5')
		endcase
		&& R.11e Mariano
	else
		&& R.17b Mariano
*		=ultdepri('D5TIFO','m.d5tota')
		if w_dupl
			=ultdepri('D5RUBR','m.d5tota')
		else
			=ultdepri('D5TIFO','m.d5tota')
		endif
		&& R.17e Mariano
		=prideult('D5TOTA','m.d5tota')
	endif
*	=modicampo('D5TIFO','D5NUME')	&& R.18 Mariano
	=modicampo('D5TIFO','D5NUME','D5CLIE')	&& R.18 Mariano

endif

if wontop('IWS005C2')

	if w_vari='D5VCAI' or w_show
		if empty(m.d5ncai) and !empty(m.d5vcai)
			if !w_show
				wait wind 'Falta ingresar N๚mero de C.A.I.'
			endif
			w_retu=.f.
		endif
		if !empty(m.d5ncai) and empty(m.d5vcai)
			if !w_show
				wait wind 'Falta ingresar Vencimiento de C.A.I.'
			endif
			w_retu=.f.
		endif
	endif

	if w_vari='D5DCAI' or w_show
		if empty(m.d5ncai) and !empty(m.d5dcai)
			if !w_show
				wait wind 'Falta ingresar N๚mero de C.A.I.'
			endif
			w_retu=.f.
		endif
		if empty(m.d5vcai) and !empty(m.d5dcai)
			if !w_show
				wait wind 'Falta ingresar Vencimiento de C.A.I.'
			endif
			w_retu=.f.
		endif
		if !empty(m.d5dcai)
			w_dcai=vercai(m.d5ncai)
			if m.d5dcai!=w_dcai
				if !w_show
					wait wind 'Error en N๚mero de C.A.I. o digito verificador = '+w_dcai
				endif
				w_retu=.f.
			endif
		else
			if !empty(m.d5ncai)
				if !w_show
					wait wind 'Falta ingresar Digito verificador de C.A.I.'
				endif
				w_retu=.f.
			endif
		endif
	endif
endif

if wontop('IWS005C3')

	if w_vari='D5DESP'
		if empty(m.d5desp)
			if !w_show
				wait wind 'Falta ingresar N๚mero de Despacho de Importaci๓n'
			endif
			w_retu=.f.
		else
			m.d5desp=strcero(m.d5desp)
			show get m.d5desp
		endif
	endif

	if w_vari='D5VERF'
		if empty(m.d5verf)
			if !w_show
				wait wind 'Falta ingresar Digito Verificador de Despacho'
			endif
			w_retu=.f.
		endif
	endif

	if w_vari='D5ADUA'
		do fhelp with 'D5ADUA','AD','iwp001t','f01t','clav','desc','Aduana DGI Inexistente',(p_strc),'w_retu'
	endif

	if w_vari='D5DEST'
		do fhelp with 'D5DEST','DT','iwp001t','f01t','clav','desc','Destinaci๓n DGI Inexistente',(p_char),'w_retu'
	endif
endif

if wontop('iws005c4') OR wontop('iws005rt')

	m.d5ttip	= "0"
	m.d5tiop	= "E"
*	m.d5coco	= TCOCO(m.d5tico,m.d5tifo)	&& R.06 Mariano
	if w_call or empty(m.d5coco)	&& R.16 Mariano
		m.d5coco	= TCOCO(m.d5tico,m.d5tifo, m.d5tipo)	&& R.06 Mariano
	endif 	&& R.16 Mariano
	m.d5titr	= "N"
	m.d5tifo	= " "
	m.d5por1	= 0							&& R.08 Ra๚l
	if w_vari='D5NUME' or w_show
		m.d5nume=strcomp(m.d5nume)
		if empty(m.d5fech)
			m.d5fech=w_5fech
		endif
	endif

	if w_vari='D5CLIE' or w_show
		do fhelp with 'D5CLIE','','iwp002','f02','d2clie','d2nomb','Cliente inexistente',(p_strc),'w_retu'

		if w_vari='D5CLIE' and w_retu

			m.d5tiim = ftiimp(f02->d2juri,m.d5peri,'C')

			if empty(m.d5juri) or m.d5clie<>w_5clie
				m.d5juri = f02->d2juri
			endif

			w_5juri	= m.d5juri
			=SEEK("JU" + m.d5juri, "f01")
			w_juri	= f01.d1des
			cafip	= f01.d1tdgi
	
			if m.d5clie>'99989'
				wait wind "No puede ser eventual"
				w_retu=.f.
			endif		
			if !w_call && modificacion
				if !eof(w_alias)
					w_recno=recno(w_alias)
				else
					w_recno=0
				endif
			endif
			w_orde=order(w_alias)
			set order to 'd5tipo' in (w_alias)

			sele(w_alias)

			if seek(m.d5tipo+m.d5sucu+m.d5tifo+m.d5tico+left(m.d5nume+space(17),17)+m.d5clie,w_alias)	&& R.18 Mariano (m.d5nume)
				do while m.d5tipo+m.d5sucu+m.d5tifo+m.d5tico+left(m.d5nume+space(17),17)+m.d5clie=d5tipo+d5sucu+d5tifo+d5tico+d5nume+d5clie ;
						and !eof(w_alias)	&& R.18 Mariano (m.d5nume)
*					if (w_call and !w_dupl) or w_5peri!=d5peri && R.18 Mariano
					if (w_call and !w_dupl) && R.18 Mariano	(doy de alta sin duplicar)
						** si repite en distinto periodo va a chillar **
						if w_5peri!=d5peri && R.18 Mariano
							&& R.18b Mariano
							skip in &w_alias
							do while m.d5tipo+m.d5sucu+m.d5tifo+m.d5tico+left(m.d5nume+space(17),17)+m.d5clie=d5tipo+d5sucu+d5tifo+d5tico+d5nume+d5clie ;
								and w_5peri!=d5peri and !eof(w_alias)	&& R.18 Mariano (m.d5nume)
								skip in &w_alias
							enddo
							skip -1 in &w_alias
							&& R.18e Mariano								
							if not w_show 
  								=advsuave('El comprobante se encuentra en el perํodo '+d5peri)
*								w_retu=.f.
							endif
*							exit	&& R.18 Mariano
					&& R.18b Mariano
						else
							** si repite en mismo periodo va a chillar y no lo deja, ya que deberia duplicar**
							if !w_show 
								=advsuave('El comprobante ya se encuentra en el perํodo '+d5peri)
								w_retu=.f.
							endif
						endif
					else
						** si esta duplicando no valido, ya que quiero repetir en el mismo periodo **
						exit
					endif
					&& R.18e Mariano
					skip in alias (w_alias)
				enddo
			endif

			set orde to (w_orde) in (w_alias)

			if w_call or w_recno=0

				go bottom in alias (w_alias)
				if !eof()
					skip 	&& cuando doy de alta voy al eof()
				endif
			else	&& cuando modifico vuelvo donde estaba
				go w_recno in alias (w_alias)
			endif

		endif

*		if w_vari='D5CLIE' and w_retu
		if w_vari='D5CLIE' OR w_show
			m.d5tiim = ftiimp(f02->d2clie,m.d5peri,'C')

			if w_retu
				=seek(m.d5clie,'f02')
				w_d6tiim=f02.d2tipo
				if empty(m.d6tido) or m.d5clie<>w_5clie
					m.d6tido=f02.d2tido
					m.d6cuit=f02.d2cuit
				endif
				if seek('TI'+w_d6tiim,'f01t')
					w_tiim=f01t.desc
				else
					w_tiim=''
				endif
				if seek('TD'+m.d6tido,'f01t')
					w_tido=f01t.desc
					if m.d6tido$'80.86.87' and empty(strtran(m.d6cuit,'-',''))			&& R.03 Ra๚l valida CDI
						m.d6cuit='  -        - '
					endif
				else
					w_tido=''
				endif

				if empty(strtran(m.d6cuit,'-',''))
								wait window 'El cliente no tiene CUIT'
								SHOW GETS
				endif

				w_nuagperc=f03.d3nagp
				w_dvagperc=f03.d3dagp			
								
			ELSE
				w_nuagperc=''
				w_dvagperc=''
			endif
			if (m.d5tifo<>w_5tifo or m.d5nume<>w_5nume or m.d5clie<>w_5clie or empty(m.d5leye)) and w_retu
					m.d5leye=readf('f02',m.d5clie,'d2nomb',10)+' '+tcabre(m.d5tico)+' '	&& R.27 Mariano (10 x 7)

*					m.d5leye=m.d5leye+m.d5tifo+m.d5sucu+'-'+m.d5nume	&& R.27 Mariano
					m.d5leye=m.d5leye+m.d5tifo+iif(empty(m.d5sucu),' ',' '+m.d5sucu+'-')+m.d5nume	&& R.27 Mariano

					&& R.19b Mariano
					if len(rtrim(m.d5leye))>30
						m.d5leye=left(m.d5leye,26)+right(m.d5leye,4)
					else
						m.d5leye=left(m.d5leye,30)
					endif
					&& R.19e Mariano
			
					show get m.d5leye

					w_5clie=m.d5clie
					w_5tifo=m.d5tifo
					w_5nume=m.d5nume
	
			endif
			SHOW GETS
		endif
	endif

	if w_vari='D5JURI' or w_show
		do fhelp with 'D5JURI','JU','iwp001','f01','d1cla','d1des','Jurisdicci๓n inexistente',(p_char),'w_retu'
		if w_retu
			w_5juri = m.d5juri
		endif
		w_juri	= f01.d1des			
		cafip	= f01.d1tdgi
		IF EMPTY(m.d5rubr)
						=SEEK("RS" + cafip + "0001", 'f01')
						m.d5rubr	= f01.d1cla
						w_rubr		= f01.d1des
		ENDIF
	endif

*	if w_vari='D5FECH' or w_show	&& R.19 Mariano
	IF (w_vari='D5FECH' or w_show) and !empty(m.d5clie)	&& R.19 Mariano
		IF !TRDATE('m.d5fech')
			IF !w_show
					WAIT WIND "La fecha no es correcta"
			ENDIF
			w_retu	= .F.
			_curobj	= objnum(m.d5fech)
			RETURN .F.
		ELSE
			IF m.d5peri <> RIGHT(DTOC(m.d5fech),7)
						IF !w_show
								WAIT WIND "La fecha no corresponde al perํodo"
						ENDIF
			ENDIF
		ENDIF
	endif

	IF w_vari = "D7NUMORI"	OR w_show
			IF EMPTY(m.d7numori)
					IF !w_show
							WAIT WIND "DEBE INGRESAR COMPROBANTE ORIGINAL"
					ENDIF
					w_retu=.f.
			ENDIF
	ENDIF

	if w_vari='D5RUBR' or w_show
*		do fhelp with 'D5RUBR','RC','iwp001','f01','d1cla','d1des','Rubro inexistente',(p_strc),'w_retu'
		do fhelp with 'D5RUBR','RS','iwp001','f01','d1cla','d1des','Rubro inexistente',(p_strc),'w_retu'
	endif

	if w_vari='D5PERE' or w_show
		if !empt(m.d5pere) or lastkey()=p_f4
			sele f24
			set filter to d24tipo='I' and d24refe='R'
			sele f05 
			w_x	= .T.
			do fhelp with 'D5PERE','','iwp024','f24','d24codi','d24desc','Retenci๓n IVA inexistente',(p_strc),'w_x'
			sele f24
			set filter to
			sele f05
			if w_x
				w_5pere=m.d5pere
				m.d5reiv=f24.d24refe
			ELSE
				w_retu		= .F.
				w_5pere		= " "
				w_pere		= ' '
				m.d5pere	= " "
				m.d5reiv	= " "
			endif
		else
			w_5pere		= " "
			m.d5pere	= " "
			m.d5reiv	= " "
			w_pere		= ' '
		endif			
		SHOW GETS
	endif	

	if w_vari='D5CPIB' or w_show
		if !empt(m.d5cpib) or lastkey()=p_f4
			m.d5por1	= 0
			w_x		= .T.
			do fhelp with 'D5CPIB', (cafip) ,'iwp025','f25','d25codi','d25norm','Retenci๓n IB inexistente',(p_strc),'w_x'	&& R.01 Mariano		'R'
			if w_x
				w_5cpib		= m.d5cpib
				m.d5regi	= f25.d25refe
				w_porcib	= f25.d25porc
				m.d5por1	= f25.d25porc				&& R.08 Ra๚l
				IF !w_show AND m.d5cpib <> w_impoib
									m.d5tota	= m.d5imre * w_porcib / 100
				ENDIF
			ELSE
				w_retu		= .F.
				w_5cpib		= " "
				m.d5cpib	= " "
				m.d5regi	= " "
				w_porc		= 0
			endif
		else
			m.d5cpib	= " "
			m.d5regi	= " "
			w_cpib		= ' '
			w_5cpib		= " "
			w_porc		= 0
		endif			
		w_impoib	= m.d5cpib
		SHOW GETS
	endif

	if w_vari='D5CPGA' or w_show
		if !empt(m.d5cpga) or lastkey()=p_f4
			sele f24
			set filter to d24tipo='G' AND d24refe = "R"
			sele f05 
			w_x	= .T.
			do fhelp with 'D5CPGA','','iwp024','f24','d24codi','d24desc','Retenci๓n Ganancias inexistente',(p_strc),'w_x'	
			sele f24
			set filter to
			sele f05
			if w_x
				w_5cpga		= m.d5cpga
				m.d5rega	= f24.d24refe
			ELSE
				w_retu		= .F.
				w_5cpga		= " "
				m.d5cpga	= " "
				m.d5rega	= " "
			endif
		else
			w_5cpga		= " "
			m.d5cpga	= " "
			m.d5rega	= " "
			w_cpga		= ' '
		endif			
		SHOW GETS
	endif

	if w_vari='D5CPOT' or w_show
		if !empt(m.d5cpot) or lastkey()=p_f4
			sele f24
			set filter to d24tipo='O' AND d24refe = "R"
			sele f05 
			w_x	= .T.
			do fhelp with 'D5CPOT','','iwp024','f24','d24codi','d24desc','Retenci๓n Otros inexistente',(p_strc),'w_x'	
			sele f24
			set filter to
			sele f05
			if w_retu
				w_5cpot=m.d5cpot
				m.d5prco = f24.d24refe
			ELSE
				w_retu		= .F.
				w_5cpot		= " "
				m.d5cpot	= " "
				m.d5prco	= " "
				w_cpot		= " "
			endif
		else
			w_5cpot		= " "
			m.d5cpot = " "
			m.d5prco = " "
			w_cpot	 = ' '
		endif			
		SHOW GETS
	endif

*===> Valida importes nulos 
*	IF w_vari = "D5IMRE" or w_show 	&& R.19 Mariano
	IF (w_vari = "D5IMRE" or w_show) and !empty(m.d5clie)	&& R.19 Mariano
		IF m.d5imre = 0
				IF !w_show
						WAIT WIND "El MONTO IMPONIBLE no puede ser nulo"
				ENDIF
			 	w_retu	= .F.
		ELSE
			IF !w_show
					m.d5tota	= m.d5imre * w_porcib / 100
			ENDIF
		ENDIF
		w_5imre		= m.d5imre
	ENDIF

*	IF w_vari = "D5TOTA" or w_show	&& R.19 Mariano
	IF (w_vari = "D5TOTA" or w_show) and !empty(m.d5clie)	&& R.19 Mariano
		IF m.d5tota = 0
				IF !w_show
						WAIT WIND "El IMPORTE no puede ser nulo"
				ENDIF
			 	w_retu	= .F.
		ENDIF
		IF ABS(m.d5imre) < ABS(m.d5tota)
				IF !w_show
						WAIT WIND "El MONTO no puede ser menor al IMPORTE"
				ENDIF
			 	w_retu	= .F.
		ENDIF
	ENDIF

	if w_retu
		m.d5ncai=''
		m.d5dcai=''
		m.d5vcai={//}
	endif

	IF w_show
		w_cuento = 0
		IF !EMPTY(m.d5pere)
					w_cuento = w_cuento + 1
		ENDIF
		IF !EMPTY(m.d5cpib)
					w_cuento = w_cuento + 1
		ENDIF
		IF !EMPTY(m.d5cpga)
					w_cuento = w_cuento + 1
		ENDIF
		IF !EMPTY(m.d5cpot)
					w_cuento = w_cuento + 1
		ENDIF
		IF	w_cuento <> 1
			IF w_retu
					_curobj	= objnum(m.d5pere)
				 	w_retu	= .F.
			ENDIF
		ENDIF
		IF (m.d5imre < 0		AND	m.d5tota > 0)	OR (m.d5imre > 0		AND	m.d5tota < 0)	
			IF w_retu
					_curobj	= objnum(m.d5imre)
				 	w_retu	= .F.
			ENDIF
		ENDIF
	ENDIF	

	DO sumatoriart

	SHOW GET m.d5tota

	=ultdepri('D5NUME','m.d5tota')
	=prideult('D5TOTA','m.d5tota')
*	=modicampo('D5NUME')	&& R.19 Mariano
	=modicampo('D5NUME','D5CLIE')	&& R.19 Mariano
	
endif

&& R.20b Jonatan
if wontop('iws005c5') or (w_show and (m.d5coco='033' or m.d5coco='063' or m.d5coco='059' or m.d5coco='060'))	&& R.20 Mariano
	if w_vari='D5CUCO' or w_show
		if empty(strtran(m.d5cuco,'-',''))
			m.d5cuco='  -        - '
			show get m.d5cuco
		endif
		if DiCuit(left(m.d5cuco,11))<>right(m.d5cuco,1)	&& .or. empty(strtran(m.d5cuco,'-',''))		
			if !w_show
					wait window 'Error en CUIT o CUIL o CDI'
			endif
			w_retu=.f.
		ENDIF
		IF EMPTY(strtran(m.d5cuco,'-',''))
			IF !w_show
				WAIT WIND "DEBE INGRESAR CUIT"
				w_retu = .F.
			ENDIF
		ENDIF
		if m.d5cuco = f00.p0cui
			m.d5deco = f00.p0emp
			m.d5ivco = 0
			SHOW GET m.d5ivco DISABLE
		else
			SHOW GET m.d5ivco ENABLE
		endif
	endif
	if w_vari='D5DECO' or w_show
		IF EMPTY(m.d5deco) 
			IF !w_show
				WAIT WIND "DEBE INGRESAR DENOMINACIำN"
				w_retu = .F.
			ENDIF
		ENDIF
	endif
	if w_vari='D5IVCO' or w_show
		IF EMPTY(m.d5ivco)
			IF !w_show
				WAIT WIND "DEBE INGRESAR IVA DE LA COMISIำN"
				w_retu = .F.
			ENDIF
		ENDIF
	endif
endif
&& R.20e Jonatan

IF !w_retu
		SHOW GETS
ENDIF
 

return(w_retu)

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc actu05
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
private w_orde,w_recno

if iif(seek(ptoi(m.d5peri),'f04'), f04->d4esco='C', .t.)
	wait window 'Perํodo Cerrado'
	return
endif

if !w_agre .and. !w_modi
	return
endif
if !valid05(.t.)
			wait 'Debe completar datos...' wind
			RELE WIND "iws005vc"
			RELE WIND "iws005rt"
			return
endif

w_orde=order(w_alias)

*set orde to 'd5peri' in (w_alias)
set orde to 'd5comp' in (w_alias)
*ptoi(d5peri)+d5tipo+d5sucu+d5tico+d5tifo+d5nume+d5clie


if !w_call && modificacion
	if !eof(w_alias)
		w_recno=recno(w_alias)
	else
		w_recno=0
	endif
else
	=db_capt(w_alias)		&& lockeo f98 por multiusuario antes de la siguiente validacion para alta
endif

sele(w_alias)

w_orde=order(w_alias)
set order to 'd5tipo' in (w_alias)

if seek(m.d5tipo+m.d5sucu+m.d5tifo+m.d5tico+left(m.d5nume+space(17),17)+m.d5clie,w_alias)	&& R.18 Mariano (m.d5nume)
	do while m.d5tipo+m.d5sucu+m.d5tifo+m.d5tico+left(m.d5nume+space(17),17)+m.d5clie=d5tipo+d5sucu+d5tifo+d5tico+d5nume+d5clie ;
			and !eof(w_alias)	&& R.18 Mariano (m.d5nume)
*		if (w_call and !w_dupl) or w_5peri!=d5peri && R.18 Mariano
		if (w_call and !w_dupl) && R.18 Mariano	(doy de alta sin duplicar)
			** si repite en distinto periodo va a chillar **
			if w_5peri!=d5peri && R.18 Mariano
				&& R.18b Mariano
				skip in &w_alias
				do while m.d5tipo+m.d5sucu+m.d5tifo+m.d5tico+left(m.d5nume+space(17),17)+m.d5clie=d5tipo+d5sucu+d5tifo+d5tico+d5nume+d5clie ;
					and w_5peri!=d5peri and !eof(w_alias)	&& R.18 Mariano (m.d5nume)
					skip in &w_alias
				enddo
				skip -1 in &w_alias
				&& R.18e Mariano								
				=advsuave('El comprobante se encuentra en el perํodo '+d5peri)
*				exit	&& R.18 Mariano
		&& R.18b Mariano
			else
				** si repite en mismo periodo va a chillar y no lo deja, ya que deberia duplicar**
				=advsuave('El comprobante ya se encuentra en el perํodo '+d5peri)
				if used('f97')	&& R.25 Mariano
					unlock in f97
				endif	&& R.25 Mariano
				return
			endif
		else
			** si esta duplicando no valido, ya que quiero repetir en el mismo periodo **
			exit
		endif
*		if !w_dupl and iif(!w_call, !w_recno=recno(w_alias), .t.)
*			=advsuave('Esta duplicando el comprobante.')
*			exit
*		endif
		&& R.18e Mariano
		skip in alias (w_alias)
	enddo
else
	if w_dupl
		=advgrave('Esta duplicando el comprobante con otro proveedor.')
	endif
endif

set orde to (w_orde) in (w_alias)

if w_call or w_recno=0
	go bottom in alias (w_alias)
	if !eof(w_alias)
		skip in alias (w_alias)	&& cuando doy de alta voy al eof()
	endif
else
	go w_recno in alias (w_alias)
endif

if .not. eof(w_alias) .and. .not. st_rlock(w_alias)
	if used('f97')	&& R.25 Mariano
		unlock in f97
	endif	&& R.25 Mariano
	return
endif

auleyd='Cpr.'+m.d5inte+' '+tcabre(m.d5tico)+' '+m.d5tifo
auleyh=m.d5nume

*if w_call and iif(w_dupl, !seek(w_pref+&w_clave,w_alias) or sino('Desea duplicar comprobante','N')='S', !seek(w_pref+&w_clave,w_alias))

if w_call and iif(w_dupl, sino('Desea duplicar comprobante','N')='S', .t.)

	if w_agre

		if begintran()
			if !w_dupl	&& R.18 Mariano
*				=rlock('f00')	&& R.11 Mariano
				=db_rlock('f00')	&& R.11 Mariano
				d0inte=right(str(1000001+val(f00.p0inte),7),6)
				sele f00
				replace p0inte with d0inte
				unlock in f00
			endif	&& R.18 Mariano

			=lockeo05()
			
			wait 'Agregando registro...' wind time .4

			if !w_dupl	&& R.18 Mariano
				m.d5inte=d0inte
			endif	&& R.18 Mariano
			m.d5coim=d0coim
			m.d5ncai=m.d5ncai
			m.d5vcai=m.d5vcai
			m.d5dcai=m.d5dcai

			sele f04
			IF m.d5tico = "R"
					DO retencv
					REPLACE &pc_imp 	WITH &pc_imp + m.d5tota
					REPLACE &pc_tota	WITH &pc_tota + m.d5tota
			ELSE					
				replace d4ctot with d4ctot-f05.d5tota+m.d5tota;
						,d4cim1 with d4cim1-f05.d5imp1+m.d5imp1;
						,d4cim2 with d4cim2-f05.d5imp2+m.d5imp2;
						,d4cim3 with d4cim3-f05.d5imp3+m.d5imp3;
						,d4cim4 with d4cim4-f05.d5imp4+m.d5imp4;
						,d4cim5 with d4cim5-f05.d5imp5+m.d5imp5;
						,d4cim6 with d4cim6-f05.d5imp6+m.d5imp6;
						,d4cim7 with d4cim7-f05.d5imp7+m.d5imp7;
						,d4cim8 with d4cim8-f05.d5imp8+m.d5imp8

			ENDIF
			go recno() in f04
			IF m.d5tico = "R"
					IF !SEEK(m.d5inte,"f07")
							=net_appe('f07')
					ENDIF
					m.d7nume=m.d5nume
					m.d7peri=m.d5peri
					m.d7empr=m.d5empr
					m.d7tipo=m.d5tipo
					m.d7tico=m.d5tico
					m.d7tifo=m.d5tifo
					m.d7inte=m.d5inte
					m.d7sucu=m.d5sucu
					sele f07
					gather memvar
					m.d7numori	= SPACE(20)
			ENDIF
			

			if m.d5clie>'99989'
				if !seek(m.d5inte,'f06')
					=net_appe('f06')

					sele f06
					replace d6inte with m.d5inte
				endif
				if f06.d6inte=m.d5inte
					sele f06
					m.d6nume=m.d5nume
					m.d6peri=m.d5peri
					m.d6empr=m.d5empr
					m.d6tipo=m.d5tipo
					m.d6sucu=m.d5sucu
					m.d6tico=m.d5tico
					m.d6tifo=m.d5tifo
					m.d6inte=m.d5inte
					gather memvar

					go recno() in f06

				endif

			else
				if seek(m.d5inte,'f06')
					=dele_rec('f06')
				endif
			endif

			=net_appe(w_alias)
			sele (w_alias)

			gather memvar
			
			=audi('ALT',auleyd,auleyh)

			if seek(m.d5alic,'f23')
				if !f23.d23fusa	&& la marco si no ha sido usada nunca
*					=rlock('f23')	&& R.11 Mariano
					=db_rlock('f23')	&& R.11 Mariano
					=set_alia('f23')
					replace f23.d23fusa with .t.	&& flag alicuota usada
					=rest_alia()
					unlock in f23
				endif
			endif

			if expiremp('IW')	and reccount('f05')>100	and f12.p0expi>100 and f00.p0expi>100	&& cancela por expiracion de tiempo del sistema
				**inicializa contador Alta/Modificaciones generales**
*				=rlock('f12')	&& R.11 Mariano
				=db_rlock('f12')	&& R.11 Mariano
				sele f12
				replace p0expi with 0
				unlock in f12
				**inicializa contador Alta/Modificaciones por empresa**
*				=rlock('f00')	&& R.11 Mariano
				=db_rlock('f00')	&& R.11 Mariano
				sele f00
				replace p0expi with 0
				unlock in f00
				sele (w_alias)
				*saco esto porque siempre veian el mismo error
				*w_alias='IVF'+right(sys(3),5)
				*sele (w_alias)
				** el error 14 no existe en fox, en ssp090 esta el tratamiento.
				** siempre va a variar el nro de error y el nro de linea.
*				do ssp090 with 14,100,'actu05'	&& R.10 Mariano
			else
				**incrementa contador Alta/Modificaciones generales**
*				=rlock('f12')	&& R.11 Mariano
				=db_rlock('f12')	&& R.11 Mariano
				sele f12
				if p0expi<500	&& para que no de overflow
					replace p0expi with p0expi + 1
				else
					replace p0expi with 100 + 1
				endif
				unlock in f12
				**incrementa contador Alta/Modificaciones por empresa**
*				=rlock('f00')	&& R.11 Mariano
				=db_rlock('f00')	&& R.11 Mariano
				sele f00
				if p0expi<500	&& para que no de overflow
					replace p0expi with p0expi + 1
				else
					replace p0expi with 100 + 1
				endif
				unlock in f00
				sele (w_alias)
			endif

				IF m.d5tico <> "R"
							=set_alia('f03')
							=seek(m.d5clie,'f03')
							if found()
									=rlock('f03')
									repla d3ncai with m.d5ncai
									repla d3vcai with m.d5vcai
									repla d3dcai with m.d5dcai
									=rest_alia()
									unlock in f03
							endif
				ENDIF

			=endtran()

			select f06
			scatte memvar blank
			w_d6tiim=''
			w_tiim=''
			w_tido=''

			select f07
			scatte memvar memo blank

			w_5fech	= m.d5fech
			w_5rubr	= m.d5rubr
			w_5clie	= m.d5clie
						
			select (w_alias)
			SCATTER MEMVAR BLANK
			store '' to w_coco, w_rubr, w_clie, w_juri, w_ttip, m.d6nocl, 				;
						w_pere, w_cpib, w_cpga, w_cpot, w_per1, w_per2, w_per3

*			SHOW GETS 	&& R.24 Mariano
			SHOW GETS enable	&& R.24 Mariano

			m.d5empr=w_5empr
			m.d5peri=w_5peri
			m.d5tipo=w_5tipo
			m.d5sucu=w_5sucu
			m.d5tico=w_5tico
			m.d5tifo=w_5tifo
			m.d5titr=w_5titr	&& R.24 Mariano
*			m.d5nume	= w_5nume
			m.d5fech	= w_5fech
				
			d0ivan=0
			d0ivaa=0

			show gets

		endif
		w_modicampo=.f.
		_curobj=1
		
	endif

else

	if st_rlock(w_alias) and !w_dupl
		if w_modi

			if begintran()

*				=rlock('f04')	&& R.11 Mariano
				=db_rlock('f04')	&& R.11 Mariano
				wait 'Modificando registro...' wind time .4

				m.d5ncai=m.d5ncai
				m.d5vcai=m.d5vcai
				m.d5dcai=m.d5dcai

				m.d5coim=d0coim

				sele f04

				IF m.d5tico = "R"
						DO retencv
						REPLACE &pc_imp 	WITH &pc_imp + m.d5tota - f05.d5tota
						REPLACE &pc_tota	WITH &pc_tota + m.d5tota - f05.d5tota
				ELSE
					replace d4vtot with d4vtot-f05.d5tota+m.d5tota;
							,d4cim1 with d4cim1-f05.d5imp1+m.d5imp1;
							,d4cim2 with d4cim2-f05.d5imp2+m.d5imp2;
							,d4cim3 with d4cim3-f05.d5imp3+m.d5imp3;
							,d4cim4 with d4cim4-f05.d5imp4+m.d5imp4;
							,d4cim5 with d4cim5-f05.d5imp5+m.d5imp5;
							,d4cim6 with d4cim6-f05.d5imp6+m.d5imp6;
							,d4cim7 with d4cim7-f05.d5imp7+m.d5imp7;
							,d4cim8 with d4cim8-f05.d5imp8+m.d5imp8
				ENDIF
			
				go recno() in f04

				if m.d5clie>'99989'
					if !seek(m.d5inte,'f06')
						=net_appe('f06')

						sele f06
						replace d6inte with m.d5inte
					endif
					if f06.d6inte=m.d5inte
						sele f06
						m.d6nume=m.d5nume
						m.d6peri=m.d5peri
						m.d6empr=m.d5empr
						m.d6tipo=m.d5tipo
						m.d6sucu=m.d5sucu
						m.d6tico=m.d5tico
						m.d6tifo=m.d5tifo
						m.d6inte=m.d5inte
						gather memvar

						go recno() in f06

					endif
				else
					if seek(m.d5inte,'f06')
						=dele_rec('f06')
					endif
				endif

				sele (w_alias)
				gather memvar

				=audi('MOD',auleyd,auleyh)

				if seek(m.d5alic,'f23')
					if !f23.d23fusa	&& la marco si no ha sido usada nunca
*						=rlock('f23')	&& R.11 Mariano
						=db_rlock('f23')	&& R.11 Mariano
						=set_alia('f23')
						replace f23.d23fusa with .t.	&& flag alicuota usada
						=rest_alia()
						unlock in f23
					endif
				endif

				if expiremp('IW')	and reccount('f05')>100	and f12.p0expi>100 and f00.p0expi>100	&& cancela por expiracion de tiempo del sistema
					**inicializa contador Alta/Modificaciones generales**
*					=rlock('f12')	&& R.11 Mariano
					=db_rlock('f12')	&& R.11 Mariano
					sele f12
					replace p0expi with 0
					unlock in f12
					**inicializa contador Alta/Modificaciones por empresa**
					=rlock('f00')
					sele f00
					replace p0expi with 0
					unlock in f00
					sele (w_alias)
					*saco esto porque siempre veian el mismo error
					*w_alias='IVF'+right(sys(3),5)
					*sele (w_alias)
					** el error 14 no existe en fox, en ssp090 esta el tratamiento.
					** siempre va a variar el nro de error y el nro de linea.
					do ssp090 with 14,100,'actu05'
				else
					**incrementa contador Alta/Modificaciones generales**
*					=rlock('f12')	&& R.11 Mariano
					=db_rlock('f12')	&& R.11 Mariano
					sele f12
					if p0expi<500	&& para que no de overflow
						replace p0expi with p0expi + 1
					else
						replace p0expi with 100 + 1
					endif
					unlock in f12
					**incrementa contador Alta/Modificaciones por empresa**
*					=rlock('f00')	&& R.11 Mariano
					=db_rlock('f00')	&& R.11 Mariano
					sele f00
					if p0expi<500	&& para que no de overflow
						replace p0expi with p0expi + 1
					else
						replace p0expi with 100 + 1
					endif
					unlock in f00
					sele (w_alias)
				endif

				=endtran()

				select f06
				scatte memvar blank
				w_d6tiim=''
				w_tiim=''
				w_tido=''
		
				IF !SEEK(m.d5inte,"f07")
							=net_appe('f07')
							m.d7nume=m.d5nume
							m.d7peri=m.d5peri
							m.d7empr=m.d5empr
							m.d7tipo=m.d5tipo
							m.d7tico=m.d5tico
							m.d7tifo=m.d5tifo
							m.d7inte=m.d5inte
							m.d7sucu=m.d5sucu
				ENDIF
				sele f07
				GATHER MEMVAR
				scatte memvar memo blank

				if wontop('iws005c4')
					w_nuagperc=''
					w_dvagperc=''
				endif
								
				IF m.d5tico <> "R"
							=set_alia('f03')
							=seek(m.d5clie,'f03')
							if found()
*									=rlock('f03')	&& R.11 Mariano
									=db_rlock('f03')	&& R.11 Mariano
									repla d3ncai with m.d5ncai
									repla d3vcai with m.d5vcai
									repla d3dcai with m.d5dcai
									=rest_alia()
									unlock in f03
							endif
				ENDIF
				
				select (w_alias)
				GO w_recno IN f05
				SCATTER MEMVAR BLANK 
				store '' to w_coco, w_rubr, w_clie, w_juri, w_ttip, m.d6nocl, 				;
							w_pere, w_cpib, w_cpga, w_cpot, w_per1, w_per2, w_per3

				SHOW GETS
		
				m.d5empr=w_5empr
				m.d5peri=w_5peri
				m.d5tipo=w_5tipo
				m.d5sucu=w_5sucu
				m.d5tico=w_5tico
				m.d5tifo=w_5tifo
*				m.d5nume	= w_5nume
				m.d5fech	= w_5fech
				
				d0ivan=0
				d0ivaa=0

				show gets

			endif
			w_modicampo=.f.
			_curobj=1

		endif
	else
		do work05
	endif
	_curobj=1
	w_modicampo=.f.
endif

if w_dupl and w_modicampo	&& No confirmo el duplicado
	select f06
	scatte memvar blank
	w_d6tiim=''
	w_tiim=''
	w_tido=''
	SELE f07
	SCATTER MEMVAR BLANK
	select (w_alias)
	scatter memvar blank

	store '' to w_coco, w_rubr, w_clie, w_juri, w_ttip, m.d6nocl,			;
				w_pere,w_cpib,w_cpga,w_cpot,w_per1,w_per2,w_per3

	m.d5empr=w_5empr
	m.d5peri=w_5peri
	m.d5tipo=w_5tipo
	m.d5sucu=w_5sucu
	m.d5tico=w_5tico
	m.d5tifo=w_5tifo
	m.d5titr=w_5titr
	m.d5por1=w_5por1

	d0ivan=0
	d0ivaa=0

	show gets
	w_modicampo=.f.
	_curobj=1
endif

m.d5ncai=space(14)
m.d5vcai={}
m.d5dcai=' '

w_dupl=.f.

unlock in f04
unlock in f06
unlock in f05

UNLOCK IN f07


if used('f97')
	unlock in f97
endif

set order to (w_orde) in (w_alias)

=db_goeof(w_alias)	&& R.19 Mariano
w_modicampo=.f.	&& R.19 Mariano

return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc brow05
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
private w_dkey,w_hkey

push menu _msysmenu
=skipbar('Registro','Insertar' ,.t.)
=skipbar('Registro','Agregar'	 ,.t.)
=skipbar('Registro','Eliminar' ,.t.)
=skipbar('Registro','Modificar',.t.)
=skipbar('Registro','Consultar',.t.)
=skipbar('Registro','Elegir'	 ,.t.)

if w_call
	=skipbar('Registro','Ir al Anterior',.f.)
	=skipbar('Registro','Ir al Siguiente',.f.)
	=skipbar('Registro','Ir al Primero',.f.)
	=skipbar('Registro','Ir al Ultimo',.f.)
endif

w_orde=order(w_alias)
if eof(w_alias)
	*	go top in (w_alias)
	*	=start(ptoi(w_5peri)+w_5tipo+w_5sucu+' '+w_5tico, w_alias)
	=start(ptoi(w_5peri)+w_5tipo+w_5sucu+w_5tico+' ', w_alias)
endif

*w_dkey=ptoi(w_5peri)+w_5tipo+w_5sucu+' '+w_5tico
*w_hkey=ptoi(w_5peri)+w_5tipo+w_5sucu+'Z'+w_5tico
w_dkey=ptoi(w_5peri)+w_5tipo+w_5sucu+w_5tico+' '
w_hkey=ptoi(w_5peri)+w_5tipo+w_5sucu+w_5tico+'Z'

sele (w_alias)

*IF WONTOP("iws005c4")	&& R.24 Mariano
IF WEXIST("iws005c4")	&& R.24 Mariano
browse fields ;
	d5tifo :h='F' :v=fenter():f,;
	d5sucu :h='Sucursal' :r,;
	d5nume :h='N๚mero':r,;
	d5fech :h='Fecha':r,;
	d5tota :h='Total':r,;
	clie=d5clie+'-'+iif(d5clie>='99990', iif(seek(d5inte,'f06'), f06.d6nocl, 'No encontrado!!!'), iif(seek(d5clie,'f02'), f02.d2nomb, 'No encontrado!!!')) :h='Cliente':r:40,;
	tico=iif(seek('CO'+d5coco,'f01t'), f01t.desc, '') :h='Tipo Cpbte.' :15 :r ;
	nome noap noed node norm in screen ;
	title iif(w_dupl,'DUPLICANDO ',iif(w_copi,'COPIANDO ',''))+wopci+' SUFRIDAS DEL PERIODO: '+w_5peri ;
	font 'arial',10 ;
	valid :f salebrow(lastkey()) ;
	key w_dkey, w_hkey ;
	rest	&& R.23 Mariano (Agregue la leyenda Duplicando y Copiando)
ELSE
browse fields ;
	d5tifo :h='F' :v=fenter():f,;
	d5nume :h='N๚mero':r,;
	d5fech :h='Fecha':r,;
	d5tota :h='Total':r,;
	prov=d5clie+'-'+iif(d5clie>='99990', iif(seek(d5inte,'f06'), f06.d6nocl, 'No encontrado!!!'), iif(seek(d5clie,'f03'), f03.d3nomb, 'No encontrado!!!')) :h='Proveedor':r :40,;
	tico=iif(seek('CO'+d5coco,'f01t'), f01t.desc, '') :h='Tipo Cpbte.' :25:r ;
	nome noap noed node norm in screen ;
	title iif(w_dupl,'DUPLICANDO ',iif(w_copi,'COPIANDO ',''))+wopci+' DE COMPRAS DEL PERIODO: '+w_5peri;
	font 'arial',10;
	valid :f salebrow(lastkey()) ;
	key w_dkey, w_hkey ;
	rest	&& R.23 Mariano (Agregue la leyenda Duplicando y Copiando)
ENDIF
**	for d5peri=m.d5peri and d5tipo=m.d5tipo and d5tico=m.d5tico

if lastkey()=p_esc
	m.d5empr=w_5empr
	m.d5peri=w_5peri
	m.d5tipo=w_5tipo
	m.d5sucu=w_5sucu
	m.d5tico=w_5tico
	m.d5tifo=w_5tifo
	m.d5titr=w_5titr
	m.d5por1=w_5por1

	if w_dupl
		w_dupl=.f.
	endif
ELSE
	=work05()
endif

set order to (w_orde) in (w_alias)
pop menu _msysmenu

***vfp***
IF version()='Visual FoxPro'
	if w_call
		rele wind wbrow
	else
		clear read
		on key label esc do actsalir
	endif
endif
***vfp***

return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc busc05
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
w_orde=order(w_alias)
do busca
if .not. eof(w_alias)
	do work05
endif
set orde to (w_orde) in (w_alias)
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc orde05
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
do case
	case prompt()='Ordenado por C๓digo'
		*	set orde to 'd5peri' in (w_alias)
		set orde to 'd5comp' in (w_alias)
endcase
if wexist(w_title)
	show wind (w_title) refresh
endif
return

*)ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc dupl05
*)ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

w_dupl=.t.

***vfp***
*do brow05
if version()='Visual FoxPro'
	*** cambio el lastkey() ***
	keyboard '{tab}'
	=inkey()
	DO wbrow WITH 'brow05()'
else
	do brow05
endif
***vfp***

if w_dupl
	w_modicampo=.t.

	store 0 to m.d5imp1,m.d5imp2,m.d5imp3,m.d5imp4,m.d5imp5,m.d5imp6,m.d5imp7,m.d5imp8,m.d5tota,m.d5imre
	STORE " "	TO	m.d5pere, m.d5cpib, m.d5cpga, m.d5reiv, m.d5regi, m.d5rega,					;
					w_pere, w_cpib, w_cpga, w_reii, w_per1, w_per2, w_per3

*===> Suma los importes de todos los items que conforman facturas duplicadas

	IF wontop("iws005c4")
					DO sumatoriart
	ELSE
		DO sumatoria
	ENDIF
	_curobj = objnum(m.d5rubr)
	
	show gets
endif

return

&& R.23b Mariano
*)ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc Copia05
*)ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
private x_d5tifo,x_d5nume,x_d5clie,x_clie,x_d6tiim,x_d6tido,x_d6cuit,x_d6nocl,x_d6dire,x_d6loca,x_d6copo,x_d5juri,x_juri,x_d5leye

if eof(w_alias)
	x_d5tifo=m.d5tifo
	x_d5nume=m.d5nume
	x_d5clie=m.d5clie
	x_clie=w_clie
	x_d6tiim=w_d6tiim
	x_d6tido=m.d6tido
	x_d6cuit=m.d6cuit
	x_d6nocl=m.d6nocl
	x_d6dire=m.d6dire
	x_d6loca=m.d6loca
	x_d6copo=m.d6copo
	x_d5juri=m.d5juri
	x_juri=w_juri
	x_d5leye=m.d5leye
	
	w_copi=.t.
		
	***vfp***
	if version()='Visual FoxPro'
		*** cambio el lastkey() ***
		keyboard '{tab}'
		=inkey()
		DO wbrow WITH 'brow05()'
	else
		do brow05
	endif
	***vfp***
	
	w_copi=.f.
	
	m.d5tifo=x_d5tifo
	m.d5nume=x_d5nume
	m.d5clie=x_d5clie
	w_clie=x_clie
	w_d6tiim=x_d6tiim
	m.d6tido=x_d6tido
	m.d6cuit=x_d6cuit
	m.d6nocl=x_d6nocl
	m.d6dire=x_d6dire
	m.d6loca=x_d6loca
	m.d6copo=x_d6copo
	m.d5juri=x_d5juri
	w_juri=x_juri
	m.d5leye=x_d5leye
	
	show gets
	=db_goeof(w_alias)
	_curobj=objnum(m.d5clie)
endif
	
return
&& R.23e Mariano

* ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
procedure calculadora
* ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
private w_mess

w_mess='[Ctrl+G]Graba Importe'

clear typeahead	&& R.18 Mariano

do case
	*case upper(varread())='D5IMP1'	&& R.18 Mariano
	case upper(varread())='D5IMP1' or w_vari='D5IMP1'	&& R.18 Mariano

		store m.d5imp1 to _calcvalue
		_curobj=objnum(m.d5imp1)

	*case upper(varread())='D5IMP2'	&& R.18 Mariano
	case upper(varread())='D5IMP2' or w_vari='D5IMP2'	&& R.18 Mariano

		store m.d5imp2 to _calcvalue
		_curobj=objnum(m.d5imp2)

	*case upper(varread())='D5IMP3'	&& R.18 Mariano
	case upper(varread())='D5IMP3' or w_vari='D5IMP3'	&& R.18 Mariano

		store m.d5imp3 to _calcvalue
		_curobj=objnum(m.d5imp3)

	*case upper(varread())='D5IMP4'	&& R.18 Mariano
	case upper(varread())='D5IMP4' or w_vari='D5IMP4'	&& R.18 Mariano

		store m.d5imp4 to _calcvalue
		_curobj=objnum(m.d5imp4)

	*case upper(varread())='D5IMP5'	&& R.18 Mariano
	case upper(varread())='D5IMP5' or w_vari='D5IMP5'	&& R.18 Mariano

		store m.d5imp5 to _calcvalue
		_curobj=objnum(m.d5imp5)

	*case upper(varread())='D5IMP6'	&& R.18 Mariano
	case upper(varread())='D5IMP6' or w_vari='D5IMP6'	&& R.18 Mariano

		store m.d5imp6 to _calcvalue
		_curobj=objnum(m.d5imp6)

	*case upper(varread())='D5IMP7'	&& R.18 Mariano
	case upper(varread())='D5IMP7' or w_vari='D5IMP7'	&& R.18 Mariano

		store m.d5imp7 to _calcvalue
		_curobj=objnum(m.d5imp7)

	*case upper(varread())='D5IMP8'	&& R.18 Mariano
	case upper(varread())='D5IMP8' or w_vari='D5IMP8'	&& R.18 Mariano

		store m.d5imp8 to _calcvalue
		_curobj=objnum(m.d5imp8)

	*case upper(varread())='D5TOTA'	&& R.18 Mariano
	case upper(varread())='D5TOTA' or w_vari='D5TOTA'	&& R.18 Mariano

		store m.d5tota to _calcvalue
		_curobj=objnum(m.d5tota)

	otherwise
		w_mess=''
endcase

&& R.18b Mariano
***vfp***
if version()='Visual FoxPro' and (wontop()='IWS005C1' or wontop()='IWS005C4')
	p_curobj=_curobj
endif
***vfp***
&& R.18e Mariano

if !empty(w_mess)
	set message to w_mess
endif

activate window calculator
move window calculator center
zoom window calculator norm
on key label leftmouse do borracalcu

return

*อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc actcalcu05
* อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
on key label esc do borracalcu
on key label ctrl+w do borracalcu
on key label ctrl+g do borracalcu

&& R.18b Mariano
*do calculadora
if version()='Visual FoxPro'
	if wontop()!='BARRAABM'
		keyboard '{pgdn}'
		=inkey()
		w_vread='m.'+varread()
		p_curobj=objnum(&w_vread)
		_curobj=objnum(p_baropc1)
		acti wind barraabm
		=inkey(.1)
		do calculadora
	else
		keyboard '{pgdn}'
		=inkey()
		p_curobj=objnum(&w_vread)
		=inkey(.1)
		do calculadora
	endif
else
	do calculadora
endif
&& R.18e Mariano

return

*อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proce borracalcu
*อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
if !wexist('calculator')
	on key label leftmouse
	***vfp***
	if version()='Visual Fox'
		on key label esc do actsalir
	else
		on key label esc
	endif
	***vfp***
	on key label ctrl+w
	on key label ctrl+g

	if lastkey()=11	&& si el lastkey es ctrl+k antes ctrl+c -> 3
		keyboard '{esc}'
		=inkey()	&& cambio el lastkey()
	endif

	return
endif

if mdown()	&& puedo usar el mouse solamente sobre la calculadora
	if mwindow('calculator')
		return
	endif
endif


if wexist('calculator')

	move window calculator to 79,0	&& si minimizo o desactivo cambia el icono

	release window calculator

	on key label leftmouse
	&& R.18b Mariano
*	on key label esc
	***vfp***
	if version()='Visual Fox'
		if lastkey()=7	&& ctrl+g
			if m.d5tico='R'
				activate window iws005c4
			else
				activate window iws005c1
			endif
			do case
				case p_curobj=objnum(m.D5IMP1)
					m.d5imp1=_calcvalue
					show get m.d5imp1
				case p_curobj=objnum(m.D5IMP2)
					m.d5imp2=_calcvalue
					show get m.d5imp2
				case p_curobj=objnum(m.D5IMP3)
					m.d5imp3=_calcvalue
					show get m.d5imp3
				case p_curobj=objnum(m.D5IMP4)
					m.d5imp4=_calcvalue
					show get m.d5imp4
				case p_curobj=objnum(m.D5IMP5)
					m.d5imp5=_calcvalue
					show get m.d5imp5
				case p_curobj=objnum(m.D5IMP6)
					m.d5imp6=_calcvalue
					show get m.d5imp6
				case p_curobj=objnum(m.D5IMP7)
					m.d5imp7=_calcvalue
					show get m.d5imp7
				case p_curobj=objnum(m.D5IMP8)
					m.d5imp8=_calcvalue
					show get m.d5imp8
				case p_curobj=objnum(m.D5TOTA)
					m.d5tota=_calcvalue
					show get m.d5tota
			endcase
		else
			if m.d5tico='R'
				activate window iws005c4
			else
				activate window iws005c1
			endif
		endif
		on key label esc do actsalir
	else
		on key label esc
	endif
	***vfp***
	&& R.18e Mariano
	on key label ctrl+w
	on key label ctrl+g

	set message to csalida

	if lastkey()=11	&& si el lastkey es ctrl+k antes ctrl+c -> 3
		keyboard '{esc}'
		=inkey()	&& cambio el lastkey()
	endif

endif

return

* ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
function lockeo05
* ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
private w_cont
w_cont=0
do while .not. (flock('f05') .and. flock('f06') .and.;
		rlock('f04'))

	wait 'Bloqueando Archivos...' wind time 1

	unlock in f05
	unlock in f06
	unlock in f04
	UNLOCK IN f07

	w_cont=w_cont+1
	if w_cont=10
		=advsuave('Verificar otras terminales. La operaci๓n actual de alguna de ellas impide completar el alta.')
		w_cont=0
	endif
enddo
return .t.

*==========================================================
PROCEDURE sumatoria

x_texto		= ""	&& R.13 Mariano (w_texto)
x_d5totaa	= 0		&& R.13 Mariano (w_d5totaa)

alfa = objnum(w_n)
if wontop()='IWS005VC'	&& R.18 Mariano
	SHOW OBJECT alfa DISABLE PROMPT x_texto		&& R.13 Mariano (w_texto)
endif	&& R.18 Mariano

IF !w_dupl
		SHOW GETS
		RETURN
ENDIF

*===> SUMATORIA de todas las facturas duplicadas para mostrar el total acumulado

x_texto	= "   Total Acumulado :"	&& R.13 Mariano (w_texto)
*		   123456789.123456789.123456789.1234

*w_cond = " FOR d5tipo = m.d5tipo AND d5tico = m.d5tico AND d5tifo = m.d5tifo AND d5sucu = m.d5sucu AND d5nume = m.d5nume AND d5clie = m.d5clie " 						&& R.09 Ra๚l 
w_cond = " FOR d5peri = m.d5peri AND d5tipo = m.d5tipo AND d5tico = m.d5tico AND d5tifo = m.d5tifo AND d5sucu = m.d5sucu AND d5nume = m.d5nume AND d5clie = m.d5clie " 	&& R.09 Ra๚l


SUM	ALL d5tota &w_cond	 TO x_d5totaa	&& R.13 Mariano (w_d5totaa)
x_d5totaa = x_d5totaa + m.d5tota	&& R.13 Mariano (w_d5totaa)


SUM	ALL d5imp1 &w_cond	 TO w_d5imp1a
w_d5imp1a = w_d5imp1a + m.d5imp1


SUM	ALL d5imp2 &w_cond	 TO w_d5imp2a
w_d5imp2a = w_d5imp2a + m.d5imp2


SUM	ALL d5imp3 &w_cond	 TO w_d5imp3a
w_d5imp3a = w_d5imp3a + m.d5imp3


SUM	ALL d5imp4 &w_cond	 TO w_d5imp4a
w_d5imp4a = w_d5imp4a + m.d5imp4


SUM	ALL d5imp5 &w_cond	 TO w_d5imp5a
w_d5imp5a = w_d5imp5a + m.d5imp5


SUM	ALL d5imp6 &w_cond	 TO w_d5imp6a
w_d5imp6a = w_d5imp6a + m.d5imp6


SUM	ALL d5imp7 &w_cond	 TO w_d5imp7a
w_d5imp7a = w_d5imp7a + m.d5imp7


SUM	ALL d5imp8 &w_cond	 TO w_d5imp8a
w_d5imp8a = w_d5imp8a + m.d5imp8


*alfa = objnum(w_n)
*SHOW OBJECT alfa ENABLE PROMPT w_texto

SHOW GETS

RETURN

*=================================================================================
PROCEDURE totdup05

IF wontop('iws005c4')
				DO iws005rt.spr
				_curobj = objnum(m.d5rubr)
ENDIF

IF wontop('iws005c1')
				DO iws005vc.spr
				IF m.d5tifo <> "B"
							_curobj = objnum(m.d5imp1)
				ELSE
					_curobj = objnum(m.d5tota)
				ENDIF	
ENDIF

*IF LASTKEY() <> p_esc
*			_curobj = 1
*ENDIF			

SHOW GETS

RETURN

*=================================================================================
PROCEDURE sumatoriart

x_texto		= ""	&& R.13 Mariano (w_texto)
x_d5totaa	= 0		&& R.13 Mariano (w_d5totaa)

alfa = objnum(w_n)
if wontop()='IWS005VC'	&& R.18 Mariano
	SHOW OBJECT alfa DISABLE PROMPT x_texto		&& R.13 Mariano (w_texto)
endif	&& R.18 Mariano

IF !w_dupl
		SHOW GETS
		RETURN
ENDIF

*===> SUMATORIA de todas las facturas duplicadas para mostrar el total acumulado

x_texto	= "   Total Acumulado :"	&& R.13 Mariano (w_texto)
*		   123456789.123456789.123456789.1234

*w_cond = " FOR d5tipo = m.d5tipo AND d5tico = m.d5tico AND d5tifo = m.d5tifo AND d5sucu = m.d5sucu AND d5nume = m.d5nume AND d5clie = m.d5clie" 							&& R.09 Ra๚l 
w_cond = " FOR d5peri = m.d5peri AND d5tipo = m.d5tipo AND d5tico = m.d5tico AND d5tifo = m.d5tifo AND d5sucu = m.d5sucu AND d5nume = m.d5nume AND d5clie = m.d5clie" 		&& R.09 Ra๚l

SUM	ALL d5tota &w_cond AND (!EMPTY(d5pere) OR !EMPTY(d5cpga) OR	!EMPTY(d5cpib) OR !EMPTY(d5cpot)) TO x_d5totaa	&& R.13 Mariano (w_d5totaa)
IF !EMPTY(m.d5pere) OR !EMPTY(m.d5cpga) OR	!EMPTY(m.d5cpib) OR !EMPTY(m.d5cpot)
					x_d5totaa = x_d5totaa + m.d5tota	&& R.13 Mariano (w_d5totaa)
ENDIF

SUM	ALL d5imre &w_cond AND !EMPTY(d5pere)	 TO w_d5imp1a
SUM	ALL d5tota &w_cond AND !EMPTY(d5pere)	 TO w_d5imp2a
IF !EMPTY(m.d5pere)
			w_d5imp1a = w_d5imp1a + m.d5imre
			w_d5imp2a = w_d5imp2a + m.d5tota
ENDIF

SUM	ALL d5imre &w_cond AND !EMPTY(d5cpga) 	 TO w_d5imp3a
SUM	ALL d5tota &w_cond AND !EMPTY(d5cpga)	 TO w_d5imp4a
IF !EMPTY(m.d5cpga)
				w_d5imp3a = w_d5imp3a + m.d5imre
				w_d5imp4a = w_d5imp4a + m.d5tota
ENDIF

SUM	ALL d5imre &w_cond AND !EMPTY(d5cpib)	 TO w_d5imp5a
SUM	ALL d5tota &w_cond AND !EMPTY(d5cpib)	 TO w_d5imp6a
IF !EMPTY(m.d5cpib)
			w_d5imp5a = w_d5imp5a + m.d5imre
			w_d5imp6a = w_d5imp6a + m.d5tota
ENDIF

SUM	ALL d5imre &w_cond AND !EMPTY(d5cpot)	 TO w_d5imp7a
SUM	ALL d5tota &w_cond AND !EMPTY(d5cpot)	 TO w_d5imp8a
IF !EMPTY(m.d5cpot)
			w_d5imp7a = w_d5imp7a + m.d5imre
			w_d5imp8a = w_d5imp8a + m.d5tota
ENDIF


*alfa = objnum(w_n)
*SHOW OBJECT alfa ENABLE PROMPT w_texto

SHOW GETS

RETURN

*================================================================================
PROCEDURE importes
PARAMETERS w_param

w_impo	= "m.d5imp" + STR((VAL(SUBSTR(w_perx,6,1)) + 5),1)
IF w_param
			SHOW GET &w_impo ENABLE
ELSE
*	&w_impo	= 0						&& R.04 Ra๚l
	SHOW GET &w_impo DISABLE
ENDIF

RETURN

*================================================================================
