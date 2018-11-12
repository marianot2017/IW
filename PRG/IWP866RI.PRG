*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : IWP866RI.PRG -> Generacion de archivos REGIMEN INFORMACION VENTAS
*)-H
*)-H Release		 : 12
*)-H
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
w_title='SIAP - REGIMEN DE INFORMACION - '+upper(prompt())

*) Apertura de archivos--------------------------------------------------------

fieldvec[1]='F00'
fieldvec[2]='F01'
fieldvec[3]='F02'
fieldvec[4]='F03'
fieldvec[5]='F04'
fieldvec[6]='F05'
fieldvec[7]='F06'
fieldvec[8]='F12'
fieldvec[9]='F23'

use (d0f00) in 0 alias 'f00'				&&(Parametros Empresa)
use (d0f01) in 0 alias 'f01' order 'd1tip'  &&(Rubros)
use (d0f02) in 0 alias 'f02' order 'd2clie' &&(Clientes)
use (d0f03) in 0 alias 'f03' order 'd3prov' &&(Proveedores)
use (d0f04) in 0 alias 'f04' order 'd4peri' &&(Clientes)
use (d0f05) in 0 alias 'f05' order 'd5pefe' &&(Clientes) Peri Tipo Fech Sucu TiCo TiFo Nume
use (d0f06) in 0 alias 'f06' order 'd6inte' &&(Nombres alternativos)
use (d0f12) in 0 alias 'f12' 				&&(Parametros de Sistema)	
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
wdici		= f12.p0diri
wcoto		= 'N'
wfivebi		= 'V' + d0coem + right(WDPERI,2) + LEFT(WDPERI,2) + 'C.TXT'
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
			wfivebi		= 'V' + d0coem + right(WDPERI,2) + LEFT(WDPERI,2) + 'C.TXT'
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
				DELE ALL FOR d5tipo = 'V'	AND d5tico = 'R'			&& (Elimino retenciones)	&& R.10 Mariano 
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

IF USED('f99d')
		USE in f99d
		DELE FILE &d0f99.d.dbf
ENDIF		

SELE 0

CREATE dbf &d0f99.d (d99reng C(254), d99line L)
USE

USE &d0f99.d IN 0 ALIAS f99d EXCL

** Archivo de Comprobantes **

set alternate to &s_file
set console off
set alternate on

sele f9c

if seek('*'+S_tipo,'f9c')


	store 0 to w5tota,w5caal,w5otros,w5exen,w5neto,w5imp3,w5imp6,w5imp7,w5imp8,w5imp5	&& R.05 Mariano (agregue w5imp5)

	store '' to w5cpga,w5cpib	&& R.06 Mariano
	  	  
	w_toti=0	&& R.04 Mariano

	do while d5sele+d5tipo='*'+S_tipo

		if d5imp1=0 and d5tota=0
			skip in f9c
			loop
		endif

		w5peri=d5peri
		w5coco=d5coco
		w5nume=d5nume
		
		w5tiop=d5tiop

		w5alic=iif(seek(d5alic,'F23'),strcero(str(f23.d23alic*100,4)),'0000')

		dime alic(21,3)

		alic=0

		do while d5coco=w5coco and d5nume=w5nume and !eof('f9c')


			w5alic=iif(seek(d5alic,'F23'),strcero(str(f23.d23alic*100,4)),'0000')
			i=val(d5alic)+1
			if !empty(d5imp1) and empty(d5imp4)	
				w5alic='0000'
				i=1
			endif
			alic(i,1)=w5alic
			alic(i,2)=alic(i,2)+d5imp4
			alic(i,3)=alic(i,3)+d5imp1

			
			w5tota = w5tota + d5tota
			w5neto = w5neto + d5imp1

			w5imp3= w5imp3 + d5imp3
			w5imp5= w5imp5 + d5imp5	&& R.05 Mariano
			w5imp6= w5imp6 + d5imp6
			w5imp7= w5imp7 + d5imp7
			w5imp8= w5imp8 + d5imp8

			&& R.06b Mariano
			if !empty(d5cpga)
				w5cpga=d5cpga
			endif
			if !empty(d5cpib)
				w5cpib=d5cpib
			endif
			&& R.06e Mariano

			if !empty(d5imp2)
				if d5imp2=w5tota 
					w5otros= w5otros + d5imp2 +w5exen
					w5exen = 0
					i=1
					alic(i,1)=0
					alic(i,2)=0
					alic(i,3)=0
				else
					w5exen= w5exen + d5imp2
				endif
			endif
			
			if empty(w5tiop) and !empty(d5tiop)
				w5tiop=d5tiop
			endif

			skip

		enddo

		skip -1

&& R.07b Mariano
*		w5nocl=left(iif(d5clie>='99990', iif(seek(d5inte,'f06'), f06.d6nocl, 'No encontrado!!!'), iif(seek(d5clie,'f02'), f02.d2nomb, 'No encontrado!!!')),30)
*		w5tido=iif(d5clie>='99990', iif(seek(d5inte,'f06'), f06.d6tido, '  '), iif(seek(d5clie,'f02'), f02.d2tido, '  '))
		w5nocl=d5nocl
		w5tido=d5tido

*		if d5tifo='B' and d5tota < 1000	
*			w5nuic='00000000000'
*		else
*			w5nuic=iif(!d5cuit='  -        - ',left(d5cuit,2)+substr(d5cuit,4,8)+right(d5cuit,1),'00000000000')
*		endif
		w5nuic=strtran(d5cuit,'-','')
&& R.07e Mariano
		
		if val(w5nuic)=0
			w5tido='99'
			w5nuic='00000000000'	&& R.07 Mariano
		endif

		for i=1 to 21
*			if !empty(alic(i,1))	&& R.05 Mariano
			if !empty(alic(i,1)) or !empty(alic(i,3))	&& R.05 Mariano
				w5caal=w5caal+1
			endif
		endfor

		if w5caal=0
			w5caal=1
		endif

		cont_reg=1

		for i=1 to 21
*			if !empty(alic(i,1))								&& R.08 Mariano
			if !empty(alic(i,1)) or (w5caal=1 and w5neto=0)		&& R.08 Mariano

				if cont_reg=w5caal

					?? dtos(max(ctod('01/'+d5peri),d5fech))
					?? iif(len(allt(d5coco))=2,'0'+d5coco,d5coco)
*					?? '0'+d5sucu	&& R.01 Mariano
*					?? iif(val(d5coco)=99,space(5),'0'+d5sucu)	&& R.01 Mariano	&& R.02 Mariano
*					?? '0'+d5sucu	&& R.02 Mariano	&& R.11 Mariano
					?? d5sucu	&& R.11 Mariano
					?? repl('0',12)+left(d5nume,8)
*					?? repl('0',12)+iif(empty(right(d5nume,4)),left(d5nume,8),left(d5nume,8-len(allt(right(d5nume,4))))+allt(right(d5nume,4)))	&& R.08 Mariano
					?? repl('0',12)+iif(empty(right(d5nume,8)),left(d5nume,8),left(d5nume,8-len(allt(right(d5nume,8))))+allt(right(d5nume,8)))	&& R.08 Mariano
					?? w5tido
					?? padl(allt(w5nuic),20,'0')
					?? left(iif(w5tido='99','VENTA GLOBAL DIARIA',w5nocl)+space(30),30)
					?? padl(allt(str(iif(w5tota<0,(-1),1)*(w5tota*100),15)),15,'0')	&& total							&& R.05 (Reemplace abs() por iif(w5tota<0,(-1),1)* )
					&& R.09b Mariano
*					?? padl(allt(str((iif(w5tota<0,(-1),1)*(w5otros)*100),15)),15,'0')	&& no gravados					&& R.05 (Reemplace abs() por iif(w5tota<0,(-1),1)* )
					if (w5tota<0 and w5otros>0) or (w5tota>0 and w5otros<0)
						?? '-'+padl(allt(str((iif(w5tota<0,1,(-1))*(w5otros)*100),14)),14,'0')	&& no gravados
					else
						?? padl(allt(str((iif(w5tota<0,(-1),1)*(w5otros)*100),15)),15,'0')	&& no gravados
					endif
					&& R.09e Mariano
					?? '000000000000000'	&& no categorizado					
					&& R.09b Mariano
*					?? padl(allt(str(iif(w5tota<0,(-1),1)*(w5exen*100),15)),15,'0')	&& exento							&& R.05 (Reemplace abs() por iif(w5tota<0,(-1),1)* )
*					?? padl(allt(str((iif(w5tota<0,(-1),1)*(w5imp3+percga()+percot())*100),15)),15,'0')	&& percepciones nacionales (iva, ganancias, otros)	&& R.05 (Reemplace abs() por iif(w5tota<0,(-1),1)* )
*					?? padl(allt(str(iif(w5tota<0,(-1),1)*(percib()*100),15)),15,'0')	&& percepciones ing.brutos			&& R.05 (Reemplace abs() por iif(w5tota<0,(-1),1)* )
					if (w5tota<0 and w5exen>0) or (w5tota>0 and w5exen<0)
						?? '-'+padl(allt(str(iif(w5tota<0,1,(-1))*(w5exen*100),14)),14,'0')	&& exento
					else
						?? padl(allt(str(iif(w5tota<0,(-1),1)*(w5exen*100),15)),15,'0')	&& exento
					endif
					if (w5tota<0 and w5imp3+percga()+percot()>0) or (w5tota>0 and w5imp3+percga()+percot()<0)
						?? '-'+padl(allt(str((iif(w5tota<0,(-1),1)*(w5imp3+percga()+percot())*100),14)),14,'0')	&& percepciones nacionales (iva, ganancias, otros)
					else
						?? padl(allt(str((iif(w5tota<0,(-1),1)*(w5imp3+percga()+percot())*100),15)),15,'0')	&& percepciones nacionales (iva, ganancias, otros)
					endif
					if (w5tota<0 and percib()>0) or (w5tota>0 and percib()<0)
						?? '-'+padl(allt(str(iif(w5tota<0,(-1),1)*(percib()*100),14)),14,'0')	&& percepciones ing.brutos
					else
						?? padl(allt(str(iif(w5tota<0,(-1),1)*(percib()*100),15)),15,'0')	&& percepciones ing.brutos
					endif
					&& R.09e Mariano
					&& R.11b Mariano
*					?? '000000000000000'	&& percepciones municipales
					if (w5tota<0 and impmun()>0) or (w5tota>0 and impmun()<0)
						?? '-'+padl(allt(str(iif(w5tota<0,1,(-1))*(impmun()*100),14)),14,'0')	&& impuestos municipales
					else
						?? padl(allt(str(iif(w5tota<0,(-1),1)*(impmun()*100),15)),15,'0')	&& impuestos municiaples
					endif
					&& R.11e Mariano
					&& R.09b Mariano
*					?? padl(allt(str(iif(w5tota<0,(-1),1)*(impint()*100),15)),15,'0')	&& impuestos internos				&& R.05 (Reemplace abs() por iif(w5tota<0,(-1),1)* )
					if (w5tota<0 and impint()>0) or (w5tota>0 and impint()<0)
						?? '-'+padl(allt(str(iif(w5tota<0,1,(-1))*(impint()*100),14)),14,'0')	&& impuestos internos
					else
						?? padl(allt(str(iif(w5tota<0,(-1),1)*(impint()*100),15)),15,'0')	&& impuestos internos
					endif
					&& R.09e Mariano
					?? 'PES'	&& codigo de moneda
					?? '0001000000'	&& tipo de cambio
					?? str(w5caal,1)	&& cantidad de alicuotas
					?? iif(!empty(w5tiop),w5tiop, iif(empty(w5neto), iif(empty(alic(i,1)),'N','E'),''))		&& codigo operacion
					&& R.09b Mariano
*					?? padl(allt(str(iif(w5tota<0,(-1),1)*(otrib()*100),15)),15,'0')	&& otros impuestos					&& R.05 (Reemplace abs() por iif(w5tota<0,(-1),1)* )
					if (w5tota<0 and otrib()>0) or (w5tota>0 and otrib()<0)
						?? '-'+padl(allt(str(iif(w5tota<0,1,(-1))*(otrib()*100),14)),14,'0')	&& otros impuestos
					else
						?? padl(allt(str(iif(w5tota<0,(-1),1)*(otrib()*100),15)),15,'0')	&& otros impuestos
					endif
					&& R.09e Mariano
*					?? dtos(max(ctod('01/'+d5peri),d5fech))												&& R.01 Mariano
*					?? iif(betw(val(d5coco),19,21) or d5tico$'?.6.E.D',repl('0',8),dtos(max(ctod('01/'+d5peri),d5fech)))	&& R.01 Mariano && R.03 Mariano
*					?? iif(betw(val(d5coco),19,21) or d5tico$'?.6.E.D' or d5tifo='M',repl('0',8),dtos(max(ctod('01/'+d5peri),d5fech)))	&& R.01 Mariano && R.03 Mariano	&& R.05 Mariano
					?? iif(betw(val(d5coco),19,21) or betw(val(d5coco),60,61) or d5tico$'?.6.E.D' or d5tifo='M',repl('0',8),dtos(max(ctod('01/'+d5peri),d5fech)))	&& R.01 Mariano && R.03 Mariano	&& R.05 Mariano
					?
					
					w_toti=w_toti+w5tota	&& R.04 Mariano

					w_99reng = iif(len(allt(d5coco))=2,'0'+d5coco,d5coco) + ' ' +d5sucu+'-'+ repl('0',12)+left(d5nume,8) + ' ' + DTOC(max(ctod('01/'+d5peri),d5fech)) + ' ' + w5nuic + ' ' + left(iif(w5tido='99','VENTA GLOBAL DIARIA',w5nocl)+space(30),30) + ' ' + str(abs(w5tota),12,2)		&& R.11 Mariano (saque '0'+ antes de d5sucu)
					SELE f99d
					APPE BLAN
					REPLA d99reng		WITH w_99reng
					SELE f9c

					&& R.08b Mariano
					if (w5caal=1 and w5neto=0)
						exit
					endif
					&& R.08e Mariano
				else

					cont_reg=cont_reg+1

				endif
			endif
		endfor

		skip

		store 0 to w5tota,w5caal,w5otros,w5exen,w5neto,w5imp3,w5imp6,w5imp7,w5imp8,w5imp5	&& R.05 Mariano (agregue w5imp5)

		store '' to w5cpga,w5cpib	&& R.06 Mariano

	enddo
	
	&& R.04b Mariano
	if reccount('f99d')>0
		w_99reng = space(84)+repli('-',14)
		SELE f99d
		APPE BLAN
		REPLA d99reng		WITH w_99reng
		w_99reng = space(54)+'TOTAL DEL PERIODO : '+space(11)+ str(abs(w_toti),12,2)	
		SELE f99d
		APPE BLAN
		REPLA d99reng		WITH w_99reng
		SELE f9c
	endif
	&& R.04e Mariano

	go top

endif

set alternate to
set console on
set alternate off


** archivo de alicuotas **
s_file=strtran(s_file,'C.TXT','A.TXT')
set alternate to &s_file
set console off
set alternate on

sele f9c

if seek('*'+S_tipo,'f9c')


	store 0 to w5tota,w5caal,w5otros,w5exen,w5neto,w5imp3,w5imp6,w5imp7,w5imp8,w5imp5	&& R.05 Mariano (agregue w5imp5)

	store '' to w5cpga,w5cpib	&& R.06 Mariano

	do while d5sele+d5tipo='*'+S_tipo

		if d5imp1=0 and d5tota=0
			skip in f9c
			loop
		endif

		w5peri=d5peri
		w5coco=d5coco
		w5nume=d5nume
		
		w5tiop=d5tiop

		w5alic=iif(seek(d5alic,'F23'),f23.d23alic,0)

		dime alic(21,3)

		alic=0

		do while d5coco=w5coco and d5nume=w5nume and !eof('f9c')


			w5alic=iif(seek(d5alic,'F23'),f23.d23alic,0)
			i=val(d5alic)+1
			if !empty(d5imp1) and empty(d5imp4)	
				w5alic=0
				i=1
			endif
			alic(i,1)=w5alic
			alic(i,2)=alic(i,2)+d5imp4
			alic(i,3)=alic(i,3)+d5imp1

			
			w5tota = w5tota + d5tota
			w5neto = w5neto + d5imp1

			w5imp3= w5imp3 + d5imp3
			w5imp5= w5imp5 + d5imp5	&& R.05 Mariano
			w5imp6= w5imp6 + d5imp6
			w5imp7= w5imp7 + d5imp7
			w5imp8= w5imp8 + d5imp8

			&& R.06b Mariano
			if !empty(d5cpga)
				w5cpga=d5cpga
			endif
			if !empty(d5cpib)
				w5cpib=d5cpib
			endif
			&& R.06e Mariano

			if !empty(d5imp2)
				if d5imp2=w5tota 
					w5otros= w5otros + d5imp2 +w5exen
					w5exen = 0
					i=1
					alic(i,1)=0
					alic(i,2)=0
					alic(i,3)=0
				else
					w5exen= w5exen + d5imp2
				endif
			endif
			
			if empty(w5tiop) and !empty(d5tiop)
				w5tiop=d5tiop
			endif

			skip

		enddo

		skip -1

		for i=1 to 21
*			if !empty(alic(i,1)) && R.04 Mariano
			if !empty(alic(i,1)) or !empty(alic(i,3))	&& R.04 Mariano
				w5caal=w5caal+1
			endif
		endfor
		
		if w5caal=0
			w5caal=1
		endif

		cont_reg=1

		for i=1 to 21
*			if !empty(alic(i,1)) && R.04 Mariano
			if (!empty(alic(i,1)) and w5neto!=0) or !empty(alic(i,3))	&& R.04 Mariano

					?? iif(len(allt(d5coco))=2,'0'+d5coco,d5coco)
*					?? '0'+d5sucu	&& R.11 Mariano
					?? d5sucu	&& R.11 Mariano
					?? repl('0',12)+left(d5nume,8)
					?? padl(allt(str(iif(w5tota<0,(-1),1)*(alic(i,3)*100),15)),15,'0')	&& neto gravado		&& R.05 (Reemplace abs() por iif(w5tota<0,(-1),1)* )
					?? codalicIVA(alic(i,1))			&& cod. alicuota
					?? padl(allt(str(iif(w5tota<0,(-1),1)*(alic(i,2)*100),15)),15,'0')	&& iva importe		&& R.05 (Reemplace abs() por iif(w5tota<0,(-1),1)* )
					?
					
			endif
		endfor
		if w5caal=1 and w5neto=0
					?? iif(len(allt(d5coco))=2,'0'+d5coco,d5coco)
*					?? '0'+d5sucu	&& R.11 Mariano
					?? d5sucu	&& R.11 Mariano
					?? repl('0',12)+left(d5nume,8)
					?? strcero(str(abs(0),15))	&& neto gravado
					?? codalicIVA(0)			&& cod. alicuota
					?? strcero(str(abs(0),15))	&& iva importe
					?
		endif
		skip

		store 0 to w5tota,w5caal,w5otros,w5exen,w5neto,w5imp3,w5imp6,w5imp7,w5imp8,w5imp5	&& R.05 Mariano (agregue w5imp5)

		store '' to w5cpga,w5cpib	&& R.06 Mariano

	enddo
	
	go top

endif

set alternate to
set console on
set alternate off

if !p_cancela
	return
endif

do repdest &&(Setea variables internas del reporte)

sele f99d
go top
if !empty(p_repclaus) &&(debe ser preview o to printer)
	p_report='iwr862'
	=rep_form()
	p_report=''
endif

do represet &&(Resetea estado)

return('')

************************************************************
func percib
************************************************************
private w_perc

w_perc=0
*if !empty(d5cpib)	&& R.06 Mariano
if !empty(w5cpib)	&& R.06 Mariano
	if d5tico='R' and empty(d5pere) 
		do case 
			case f00.p0prv1=1
				w_perc =w5imp6
			case f00.p0prv2=1
				w_perc =w5imp7
			case f00.p0prv3=1
				w_perc =w5imp8
		endcase
	else 
		if d5tico!='R'
			do case 
				case f00.p0prv1=1
					w_perc =w5imp6
				case f00.p0prv2=1
					w_perc =w5imp7
				case f00.p0prv3=1
					w_perc =w5imp8
			endcase
		endif		
	endif
endif	

return (w_perc)

************************************************************
func percga
************************************************************
private w_perc

w_perc=0
*if !empty(d5cpga) 	&& R.06 Mariano
if !empty(w5cpga) 	&& R.06 Mariano
	if d5tico='R' and empty(d5pere) 
		do case 
			case f00.p0prv1=2
				w_perc =w5imp6
			case f00.p0prv2=2
				w_perc =w5imp7
			case f00.p0prv3=2
				w_perc =w5imp8
		endcase
	else 
		if d5tico!='R'
			do case 
				case f00.p0prv1=2
					w_perc =w5imp6
				case f00.p0prv2=2
					w_perc =w5imp7
				case f00.p0prv3=2
					w_perc =w5imp8
			endcase
		endif		
	endif
endif	

return (w_perc)

************************************************************
func percot
************************************************************
private w_perc

w_perc=0
*if !empty(d5cpot) 					&& R.05 Mariano
*if !empty(d5cpot) and d5tico!='R'	&& R.05 Mariano	&& R.06 Mariano
if !empty(d5cpot) or d5tico!='R'	&& R.05 Mariano	&& R.06 Mariano
	if d5tico='R' and empty(d5pere) 
		do case 
			case f00.p0prv1=3
				w_perc =w5imp6
			case f00.p0prv2=3
				w_perc =w5imp7
			case f00.p0prv3=3
				w_perc =w5imp8
		endcase
	else 
		if d5tico!='R'
			do case 
				case f00.p0prv1=3
					w_perc =w5imp6
				case f00.p0prv2=3
					w_perc =w5imp7
				case f00.p0prv3=3
					w_perc =w5imp8
			endcase
		endif		
	endif
endif	

return (w_perc)

************************************************************
func otrib
************************************************************
private w_perc

w_perc=0
do case 
*	case f00.p0prv1=4 and !'Imp.Int'$f00.p0ocv1	&& R.11 Mariano
*	case f00.p0prv1=4 	&& R.11 Mariano	&& R.12 Mariano
	case f00.p0prv1=3 	&& R.12 Mariano
		w_perc =w5imp6
*	case f00.p0prv2=4 and !'Imp.Int'$f00.p0ocv2	&& R.11 Mariano
*	case f00.p0prv2=4 	&& R.11 Mariano	&& R.12 Mariano
	case f00.p0prv2=3 	&& R.12 Mariano
		w_perc =w5imp7
*	case f00.p0prv3=4 and !'Imp.Int'$f00.p0ocv3	&& R.11 Mariano
*	case f00.p0prv3=4 	&& R.11 Mariano	&& R.12 Mariano
	case f00.p0prv3=3 	&& R.12 Mariano
		w_perc =w5imp8
endcase
w_perc=w_perc+w5imp5	&& R.05 Mariano

return (w_perc)

&& R.11b Mariano
************************************************************
func impmun	&& impuestos municipales
************************************************************
private w_perc

w_perc=0
do case 
*	case f00.p0prv1=5	&& R.12 Mariano
	case f00.p0prv1=4	&& R.12 Mariano
		w_perc =w5imp6
*	case f00.p0prv2=5	&& R.12 Mariano
	case f00.p0prv2=4	&& R.12 Mariano
		w_perc =w5imp7
*	case f00.p0prv3=5	&& R.12 Mariano
	case f00.p0prv3=4	&& R.12 Mariano
		w_perc =w5imp8
endcase

return (w_perc)

************************************************************
func impint
************************************************************
private w_perc

w_perc=0
do case 
*	case f00.p0prv1=4 and 'Imp.Int'$f00.p0ocv1	&& R.11 Mariano
*	case f00.p0prv1=6	&& R.11 Mariano	&& R.12 Mariano
	case f00.p0prv1=5	&& R.12 Mariano
		w_perc =w5imp6
*	case f00.p0prv2=4 and 'Imp.Int'$f00.p0ocv2	&& R.11 Mariano
*	case f00.p0prv2=6	&& R.11 Mariano	&& R.12 Mariano
	case f00.p0prv2=5	&& R.12 Mariano
		w_perc =w5imp7
*	case f00.p0prv3=4 and 'Imp.Int'$f00.p0ocv3	&& R.11 Mariano
*	case f00.p0prv3=6	&& R.11 Mariano	&& R.12 Mariano
	case f00.p0prv3=5	&& R.12 Mariano
		w_perc =w5imp8
endcase

return (w_perc)

************************************************************
func codalicIVA      && Devuelve el c๓digo de alicuota IVA
************************************************************
parameters w_piva

&& R.04b Mariano
if type('w_piva')='C'	&& Codigo de alicuota
	w_piva=iif(seek(w_piva,'F23'),f23.d23alic,0)
endif
&& R.04e Mariano
do case
	case w_piva=0
		w_retu='0003'			
	case w_piva=10.50
		w_retu='0004'			
	case w_piva=21
		w_retu='0005'			
	case w_piva=27
		w_retu='0006'			
	case w_piva=5
		w_retu='0008'			
	case w_piva=2.5
		w_retu='0009'			
	otherwise
		w_retu='0000'
endcase
return (w_retu)

