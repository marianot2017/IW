*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : IWP866.PRG -> Generacion de archivos ASCII - Mensual
*)-H Release		 : 04
*)-H Fecha de inicio :
*)-H Fecha de fin    :
*)-H
*)-H Actualizaciones
*)-H
*)-H Fecha       Programmer      Comentarios
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PRIVATE ALL LIKE w_*

*) Salvado de estados anteriores-----------------------------------------------

push key clear		  &&(Salva estados de teclas)
push menu _msysmenu &&(Salva estado de menu)
set skip of menu _msysmenu .t.

*) Variables internas----------------------------------------------------------

w_prog=upper(prompt())
w_alta=.f.
w_pfile='866'
*w_title=upper(prompt())	&& R.03 Mariano
w_title='SIAP - CITI - '+ upper(prompt())	&& R.03 Mariano

*) Apertura de archivos--------------------------------------------------------

fieldvec[1]='F01'
fieldvec[2]='F02'
fieldvec[3]='F03'
fieldvec[4]='F04'
fieldvec[5]='F05'
fieldvec[6]='F06'
fieldvec[7]='F12'
fieldvec[8]='F23'

use (d0f01) in 0 alias 'f01' order 'd1tip'  &&(Rubros)
use (d0f02) in 0 alias 'f02' order 'd2clie' &&(Clientes)
use (d0f03) in 0 alias 'f03' order 'd3prov' &&(Proveedores)
use (d0f04) in 0 alias 'f04' order 'd4peri' &&(Clientes)
use (d0f05) in 0 alias 'f05' order 'd5pefe' &&(Clientes) Peri Tipo Fech Sucu TiCo TiFo Nume
use (d0f06) in 0 alias 'f06' order 'd6inte' &&(Nombres alternativos)
*use (d0f12) in 0 alias 'f12' EXCLU  		  &&(Parametros de Sistema)	&& R.02 Mariano
use (d0f12) in 0 alias 'f12' 				&&(Parametros de Sistema)	&& R.02 Mariano
use (d0f23) in 0 alias 'f23' order 'd23codi' &&(Alicuotas IVA)

if neterr()
	pop key
	pop menu _msysmenu
	fieldvec=''
	=closall()
	return
endif
*)-----------------------------------------------------------------------------

=SetComp() &&(Setea filtros de comprobante DB CR y su signo)

go bott in f04
go top in f12

WDPERI=F04.D4PERI
WHPERI=F04.D4PERI

wtrim		= ''
wdici		= f12.p0dici
wcoto		= 'N'
wfivebi		= 'V' + d0coem + right(WDPERI,2) + LEFT(WDPERI,2) + '.TXT'
w_fivebi	= wfivebi

do ssm998.mpr
do iws866.spr

pop key
pop menu _msysmenu
=closall()
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC valid866
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS w_show

if empty(parameters())
	w_show=.f.
endif

PRIVATE w_retu,w_vari
w_retu=.t.
w_vari=varread()

if w_vari='WDPERI' or w_show
	if lastkey()=p_f4
		do fhelp with 'WDPERI','','iwp004','f04','d4peri','d4peri','',(p_char),'w_retu'
	endif
	w_fech	= CTOD('01/' + wdperi)
	IF EMPTY(w_fech)
			RETURN	.F. 
	ENDIF				
	IF TRDATE('w_fech')
			wdperi	= RIGHT(DTOC(w_fech),7)
			SHOW GET wdperi
	ELSE
	    IF !w_show
			WAIT WIND "El perํodo no es correcto"
	    ENDIF
	    w_retu	= .F.
	    _curobj	= objnum(wdperi)
	    RETURN .F.
	ENDIF
	wkperi = ptoi(ALLT(wdperi))
	if ! seek(wkperi,'f04')
			if !w_show
				wait window 'Perํodo Inexistente'
			endif
			w_retu=.f.
	endif

	IF w_retu
			wfivebi		= 'V' + d0coem + right(WDPERI,2) + LEFT(WDPERI,2) + '.TXT'
			whperi		= wdperi
	ELSE
		wfivebi	= "ERROR.TXT"
	ENDIF
	SHOW GETS
endif


if w_vari='WDICI' or w_show
	w_DiCi=alltrim(wDiCi)
	wDiCi =subs(iif(subs(w_DiCi,len(w_DiCi),1)<>'\',w_DiCi+'\',w_DiCi)+spac(20),1,20)
	w_DiCi=alltrim(wDiCi)
	if !mfile(w_DiCi+'NUL')
		if sino('Desea crear el directorio -> '+w_dici)='S'
			if mkdir(left(w_dici,len(w_dici)-1)) = 0
				=advsuave('Creo el directorio -> '+w_dici)
			else
				=advgrave('No pudo crear el directorio -> '+w_dici)
				w_retu=.f.
			endif
		else
			=advgrave('No existe el directorio -> '+w_dici)
			w_retu=.f.
		endif
	else
		w_FIvebi=w_dici+alltrim(wFIvebi)

	endif
endif

if w_vari='WCOTO' or w_show
	if !wcoto$'SN'
		if !w_show
			wait wind 'Opciones vแlidas: S,N'
		endif
		w_retu=.f.
	endif
endif
return(w_retu)

***vfp***
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons866
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	on key label f9 do cons866x
	keyboard '{f9}'
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons866x
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
on key label f9
***vfp***

if !valid866(.t.)
	wait wind 'Verifique los datos ingresados...'
	return
endif

if sino('Confirma Generaci๓n ?.')='S'
	wait wind nowait 'Creando Archivo Temporario...'
	if creamov(wDperi,wHperi,'V','1',.t.,.t.)
		wait clear
		if !eof('f9c')

			dele file &w_FIvebi

			if Sele860(wCoTo)
				sele f9c
				dele tag all
				inde on d5sele+d5tipo+d5cuit+PtoI(d5Peri)+d5tiop+d5coco tag d5sele
				wait wind nowait 'Generando Ventas...'
				=GeAr866('V',w_FIvebi)
				wait clear
				=advsuave('Fin del Proceso, Archivos generados.','Informaci๓n')
			else
				=advsuave('No Hay datos que Cumplan...','Informaci๓n')
			endif

		else
			=advsuave('No hay datos...','Informaci๓n')
		endif
		sele f9c
		use
		dele file &d0f99.c.dbf
		dele file &d0f99.c.cdx
	endif
	wait clear
endif
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC gear866
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS s_tipo,s_file

&& R.03b Mariano
IF USED('f99d')
		USE in f99d		&& R.03 Mariano (agregue in f99d)
		DELE FILE &d0f99.d.dbf
ENDIF		

SELE 0

CREATE dbf &d0f99.d (d99reng C(254), d99line L)
USE

USE &d0f99.d IN 0 ALIAS f99d EXCL
&& R.03e Mariano

set alternate to &s_file
set console off
set alternate on

sele f9c

if seek('*'+S_tipo,'f9c')


*	store 0 to w5tota,w5caal,w5otros,w5exen,w5impu,w5imre	&& R.01 Mariano
	store 0 to w5tota,w5caal,w5otros,w5exen,w5imre,w5netog	&& R.01 Mariano
	store {} to w5fpre

	do while d5sele+d5tipo='*'+S_tipo

		if d5imp1=0 and d5tota=0
			skip in f9c
			loop
		endif

		w5peri=d5peri
		w5coco=d5coco
		w5nume=d5nume

		w5alic=iif(seek(d5alic,'F23'),strcero(str(f23.d23alic*100,4)),'0000')

		dime alic(21,3)

		alic=0

		do while d5coco=w5coco and d5nume=w5nume and !eof('f9c')


			w5alic=iif(seek(d5alic,'F23'),strcero(str(f23.d23alic*100,4)),'0000')
			i=val(d5alic)+1
*			if empty(d5imp1)					&& R.01 Mariano
			if !empty(d5imp1) and empty(d5imp4)	&& R.01 Mariano
				w5alic='0000'
				i=1
			endif
			alic(i,1)=w5alic
			alic(i,2)=alic(i,2)+d5imp4
			alic(i,3)=alic(i,3)+d5imp1

			
			w5tota = w5tota + d5tota
			w5netog= w5netog + d5imp1

			&& R.01b Mariano
*			w5otros= w5otros + iif((d5imp2=d5tota),0,d5imp2)
*			w5exen= w5exen + iif((d5imp2=d5tota),d5imp2,0)
			if !empty(d5imp2)
				if d5imp2!=w5tota 
					w5otros= w5otros + d5imp2 +w5exen
					w5exen = 0
					i=1
					alic(i,1)=0
					alic(i,2)=0
					alic(i,3)=0
				else
					w5exen= w5exen + d5imp2
					w5alic='0000'
					alic(i,1)=w5alic
					alic(i,2)=0
					alic(i,3)=0
					i=1
				endif
			endif
			&& R.01e Mariano

			if d5tifo='M'
				w5imre=w5imre+d5imre
				w5fpre=max(w5fpre,d5fpre)
			endif

			skip

		enddo

		skip -1

		w5nocl=left(iif(d5clie>='99990', iif(seek(d5inte,'f06'), f06.d6nocl, 'No encontrado!!!'), iif(seek(d5clie,'f02'), f02.d2nomb, 'No encontrado!!!')),30)
		w5tido=iif(d5clie>='99990', iif(seek(d5inte,'f06'), f06.d6tido, '  '), iif(seek(d5clie,'f02'), f02.d2tido, '  '))

*		if d5tifo='B' and w5tido='99' and d5tota < 1000		&& R.01 Mariano
		if d5tifo='B' and d5tota < 1000		&& R.01 Mariano
			w5nuic='00000000000'
		else
			w5nuic=iif(!d5cuit='  -        - ',left(d5cuit,2)+substr(d5cuit,4,8)+right(d5cuit,1),'00000000000')
		endif
		
		if val(w5nuic)=0
			w5tido='99'
		endif

		for i=1 to 21
			if !empty(alic(i,1))
				w5caal=w5caal+1
			endif
		endfor


		cont_reg=1

		for i=1 to 21
			if !empty(alic(i,1))

				if cont_reg=w5caal

					?? '1'
					?? dtos(max(ctod('01/'+d5peri),d5fech))
					&& R.03b Mariano
*					?? d5coco
					if val(d5coco)<=99
						?? right(d5coco,2)
					else
						if d5coco$'110.112.113.119'
							?? '90'
						else
							?? '99'
						endif
					endif
					&& R.03e Mariano
					?? space(1)
					?? d5sucu
					?? '000000000000'+left(d5nume,8)
*					?? '000000000000'+left(d5nume,8)	&& R.01 Mariano
*					?? '000000000000'+iif(empty(right(d5nume,4)),left(d5nume,8),left(d5nume,4)+right(d5nume,4))	&& R.01 Mariano	&& R.04 Mariano
					?? '000000000000'+iif(empty(right(d5nume,8)),left(d5nume,8),left(d5nume,8-len(allt(right(d5nume,8))))+right(d5nume,8))	&& R.04 Mariano
					?? w5tido
					?? w5nuic
					?? w5nocl
					?? strcero(str((w5tota*100),15))
					?? strcero(str(((w5otros)*100),15))
*					?? strcero(str((alic(i,3)*100),15))	&& R.01 Mariano
					?? strcero(str((w5netog*100),15))	&& R.01 Mariano
					?? alic(i,1)
					?? strcero(str(((alic(i,2))*100),15))
					?? '000000000000000'
					?? strcero(str((w5exen*100),15))
					?? '000000000000000'
					?? '000000000000000'
					?? '000000000000000'
					?? '000000000000000'
					?? '00'
					?? space(3)
					?? '0000000000'
					?? str(w5caal,1)
					?? space(1)
					?? '00000000000000'
					?? '00000000'
					?? '00000000'
					?? space(75)
					?? iif(d5tifo='M' and !empty(w5fpre),dtos(w5fpre),'00000000')
					?? iif(d5tifo='M',strcero(str((w5imre*100),15)),'000000000000000')
					?

				else

					?? '1'
					?? dtos(max(ctod('01/'+d5peri),d5fech))
					&& R.03b Mariano
*					?? d5coco
					if val(d5coco)<=99
						?? right(d5coco,2)
					else
						if d5coco$'110.112.113.119'
							?? '90'
						else
							?? '99'
						endif
					endif
					&& R.03e Mariano
					?? space(1)
					?? d5sucu
					?? '000000000000'+left(d5nume,8)
*					?? '000000000000'+left(d5nume,8)	&& R.01 Mariano
					?? '000000000000'+iif(empty(right(d5nume,4)),left(d5nume,8),left(d5nume,4)+right(d5nume,4))	&& R.01 Mariano
					?? w5tido
					?? w5nuic
					?? w5nocl
					?? '000000000000000'
					?? '000000000000000'
*					?? strcero(str((alic(i,3)*100),15))	&& R.01 Mariano
					?? strcero(space(15))				&& R.01 Mariano
					?? alic(i,1)
					?? strcero(str(((alic(i,2))*100),15))
					?? '000000000000000'
					?? '000000000000000'
					?? '000000000000000'
					?? '000000000000000'
					?? '000000000000000'
					?? '000000000000000'
					?? '00'
					?? space(3)
					?? '0000000000'
					?? str(w5caal,1)
					?? space(1)
					?? '00000000000000'
					?? '00000000'
					?? '00000000'
					?? space(75)
					?? '00000000'
					?? '000000000000000'
					?

					cont_reg=cont_reg+1

				endif

				&& R.03b Mariano
				w_99reng = iif(len(allt(d5coco))=2,'0'+d5coco,d5coco) + ' ' +d5sucu+'-'+ d5nume + ' ' + DTOC(max(ctod('01/'+d5peri),d5fech)) + ' ' + w5nuic + ' ' + left(iif(w5tido='99','VENTA GLOBAL DIARIA',w5nocl)+space(30),30) + ' ' + str(abs(w5tota),12,2)
				SELE f99d
				APPE BLAN
				REPLA d99reng		WITH w_99reng
				SELE f9c
				&& R.03e Mariano
			endif
		endfor

		skip

*		stor 0 to w5tota,w5caal,w5otros,w5exen,w5impu,w5imre	&& R.01 Mariano
		store 0 to w5tota,w5caal,w5otros,w5exen,w5imre,w5netog	&& R.01 Mariano
		store {} to w5fpre

	enddo

endif

set alternate to
set console on
set alternate off

if !p_cancela
	return
endif

&& R.03b Mariano
do repdest &&(Setea variables internas del reporte)

sele f99d
go top
if !empty(p_repclaus) &&(debe ser preview o to printer)
	p_report='iwr862'
	=rep_form()
	p_report=''
endif

do represet &&(Resetea estado)
&& R.03e Mariano

return('')
