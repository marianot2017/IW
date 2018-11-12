*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : IWP005IN.PRG -> Interfase para la incorporacion de comprobantes
*)-H Release 		 : 11
*)-H Fecha de inicio :
*)-H Fecha de fin    :
*)-H
*)-H Actualizaciones
*)-H
*)-H Fecha       Programmer      Comentarios
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS w_pref,w_call

private all like w_*
if empty(parameters())
	w_pref=''
	w_call=.f.
endif

*) Salvado de estados anteriores-----------------------------------------------

push key clear      &&(Salva estados de teclas)
push menu _msysmenu &&(Salva estado de menu)

*) Variables internas----------------------------------------------------------

w_prog =upper(prompt())
w_alta =.f.
w_pfile='05in'
w_title='INCORPORACION DE COMPROBANTES MAXIREST'

*) Apertura de archivos--------------------------------------------------------

*CLOSE Data
*set procedure to \ss_fox\library

fieldvec[ 1]='F00'
fieldvec[ 2]='F01'
fieldvec[ 3]='F02'
fieldvec[ 4]='F03'
fieldvec[ 5]='F04'
fieldvec[ 6]='F05'
fieldvec[ 7]='F06'
fieldvec[ 8]='F07'
fieldvec[ 9]='F12'
fieldvec[10]='F20'
fieldvec[11]='F21'
fieldvec[12]='F23'
fieldvec[13]='F24'
fieldvec[14]='F25'

use (d0f00) in 0 alias f00					&& Parametros
use (d0f01) in 0 alias f01 orde d1tip		&& tablas
use (d0f02) in 0 alias f02 orde d2clie		&& Clientes
use (d0f03) in 0 alias f03 orde d3prov		&& Proveedores
use (d0f04) in 0 alias f04 order D4peri		&& Periodos
use (d0f05) in 0 alias f05 order d5tipo		&& Comprobantes
use (d0f06) in 0 alias f06 order d6inte		&& Nombres alternativos
use (d0f07) in 0 alias f07 order d7inte		&& Memos y Comprobantes Originales xa Retenciones
use (d0f12) in 0 alias f12					&& Parametros del sistema
use (d0f20) in 0 alias f20 order d20tipo	&& Cond Impositiva
use (d0f21) in 0 alias f21 order d21tip		&& Tablas Definicion
use (d0f23) in 0 alias f23 order d23codi	&& Alicuotas IVA
use (d0f24) in 0 alias f24 order d24codi	&& Per/Ret/Otr
use (d0f25) in 0 alias f25 order d25codi	&& Per/Ret IB

if neterr()
	pop key
	pop menu _msysmenu
	fieldvec=''
	=closall()
	return
endif

*)-----------------------------------------------------------------------------

=SetComp() &&(Setea filtros de comprobante DB CR y su signo)

if getenv('INTERIVA')!='MATO'
	=advsuave('Si desea poder incorporar comprobantes consulte con MEMOSOFT para desarrollar la Interfase a su medida', 'INCORPORACION DE COMPROBANTES DE VENTAS Y COMPRAS')
	pop key
	pop menu _msysmenu

	=closall()

	return
endif

=creatipos('C')	&& R.04 Mariano

&& R.03b Taboada
go bott in f04
go top in f12

wdici		= ALLTRIM(F12.p0diin)
wcoem		= '\'+d0coem

if !mfile(wdici+wcoem+'\NUL')
	if !mfile(wdici+'\NUL')
		=creadir(wdici)
		=creadir(wdici+wcoem)
	else
		=creadir(wdici+wcoem)
	endif
endif			
&& R.03e Taboada	

wdici=wdici+wcoem	&& R.04 Mariano

wcoto		= 'N'

wtrim		= '1'
wano		= right(f04->d4peri,4)


wdperi		= f04.d4peri

xlsvent		= 'Ventas*.xls'
xlscomp		= 'Compras*.xls'

set skip of menu _msysmenu .t.	&& R.06 Mariano

do ssm998.mpr

&& R.08b Mariano
dele file &d0coem.\inte005v.dbf
dele file &d0coem.\inte005c.dbf
dele file &d0coem.\intevtas.dbf
dele file &d0coem.\intecpra.dbf
&& R.08e Mariano


do iws005in.spr		
&& R.03e Taboada

fieldvec=''

if !autor() 
	pop key
	pop menu _msysmenu

	=closall()
	=borratipos()	&& R.04 Mariano

	return
endif

*)-----------------------------------------------------------------------------

w_impr='S'
*do ssm998.mpr	&& R.03 Taboada

*do cons05in0	&& ***vfp***	&& R.03 Taboada

pop key
pop menu _msysmenu
=closall()
=borratipos()	&& R.04 Mariano
dele file &d0coem.\inte005v.dbf
dele file &d0coem.\inte005c.dbf
dele file &d0coem.\intevtas.dbf
dele file &d0coem.\intecpra.dbf
dele file &d0f99..dbf
dele file &d0f99.e.dbf
return

***vfp***
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons05in
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	on key label f9 do cons05in0
	keyboard '{f9}'
*return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons05in0
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
on key label f9

***vfp***

if wexist('getdest')
	* Impresion del archivo de errores

	do repdest && Setea variables internas del reporte

	go top in f9e
	sele f9e

	if !empty(p_repclaus) &&(Debe ser preview o to printer)
		p_report='iwr005in'
		=rep_form()
		p_report=''
	endif

	do represet && Resetea estado
	
	clear read
	
else

	if !valid05in(.t.)
		return
	endif

	*** INTEVTAS.DBF y INTECPRA.DBF son Dbfs acordes para recibir los datos de excel en version 5.0 ***
	&& R.01b Mariano
	if !file(d0coem+'\INTEVTAS.DBF')
		sele 0	&& R.02 Mariano
		create dbf &d0coem.\intevtas.dbf (fecha c(10), desde_fact c(25), hasta_fact c(25), cliente c(30), iva c(25), cuit c(13), tasa n(6,2), exento n(20,2), neto n(20,2), iva_insc n(20,2), iva_dif1 n(20,2), iva_dif2 n(20,2), iva_dif3 n(20,2), total n(20,2), impint n(20,2))
		use		&& R.02 Mariano
	endif	
	if !file(d0coem+'\INTECPRA.DBF')
 		sele 0	&& R.02 Mariano
		create dbf &d0coem.\intecpra.dbf (fecha c(10), comprobant c(25), codprov c(10), proveedor c(30), codconc c(10), concepto c(30), sitiva c(25), cuit c(13), tasa n(6,2), exento n(20,2), neto n(20,2), iva n(20,2), iva_dif n(20,2), impint n(20,2), perciva n(20,2), perciibb n(20,2), percgana n(20,2), percotro n(20,2), total n(20,2), imputacion c(20), fechapro c(10), observac c(40))
		use		&& R.02 Mariano
	endif	
	&& R.01e Mariano
	
	if file(d0coem+'\INTEVTAS.DBF') and file(d0coem+'\INTECPRA.DBF')
		do genera05in	&& genera los inte005v y inte005c a partir de planillas excel
	endif

	if !file(d0coem+'\INTE005v.DBF') and !file(d0coem+'\INTE005c.DBF')
	   wait 'Faltan archivos de interface INTE005v,INTE005c.DBF' wind
	   return
	endif

	wait clear

	&& R.04b Mariano
	if alias()!='FIV' 
		use &d0coem.\inte005v in 0 alias fiv 	&& Interfase Ventas
		sele fiv
		if version()='Visual FoxPro'
			*** cambio el lastkey() ***
			keyboard '{tab}'
			=inkey()
			DO wbrow WITH 'brow05in()'
	    else
   			do brow05in
	    endif
	    use in fiv
	endif
	if alias()!='FIC' 
		use &d0coem.\inte005c in 0 alias fic 	&& Interfase Compras
		sele fic
		if version()='Visual FoxPro'
			*** cambio el lastkey() ***
			keyboard '{tab}'
			=inkey()
			DO wbrow WITH 'brow05in()'
	    else
   			do brow05in
	    endif
	    use in fic
	endif
	&& R.04e Mariano

	if sino('Confirma incorporaci๓n de comprobantes ?.','Si',w_title)='N'
		return	
	endif

	wait 'Procesando <No interrumpir>...' wind nowait

	dimension wwfiles(1)
	wwfiles=''
	*wwcant=a_dir('wwfiles',sys(2003)+'\'+d0coem+'\VENTAS*.XLS','','',.t.)
*	wwcant=a_dir('wwfiles',wdici+'\'+d0coem+'\VENTAS*.XLS','','',.t.)  	&& R.03 Taboada	&& R.04 Mariano
	wwcant=a_dir('wwfiles',wdici+'\VENTAS*.XLS','','',.t.)  	&& R.04 Mariano
	wfinu=0
	for wfinu=1 to wwcant
		*w_fire='"'+sys(2003)+'\'+d0coem+'\'+wwfiles[wfinu,1]+'"'
*		w_fire='"'+wdici+'\'+d0coem+'\'+wwfiles[wfinu,1]+'"'	&& R.03 Taboada	&& R.04 Mariano
		w_fire='"'+wdici+'\'+wwfiles[wfinu,1]+'"'	&& R.04 Mariano
		if version()='Visual FoxPro'
			*w_fipr='"'+sys(2003)+'\'+d0coem+'\PROCESADO_'+wwfiles[wfinu,1]+'"'
*			w_fipr='"'+wdici+'\'+d0coem+'\PROCESADO_'+wwfiles[wfinu,1]+'"'	&& R.03 Taboada	&& R.04 Mariano
			w_fipr='"'+wdici+'\PROCESADO_'+wwfiles[wfinu,1]+'"'	&& R.04 Mariano
			dele file &w_fipr.
			rename &w_fire. to &w_fipr.
		else
			*w_fipr='"'+sys(2003)+'\'+d0coem+'\PROCESADO_'+wwfiles[wfinu,1]+'"'
*			w_fipr='"'+wdici+'\'+d0coem+'\PROCESADO_'+wwfiles[wfinu,1]+'"'	&& R.03 Taboada	&& R.04 Mariano
			w_fipr='"'+wdici+'\PROCESADO_'+wwfiles[wfinu,1]+'"'	&& R.03 Taboada	&& R.04 Mariano
			private w_vfox,w_inst001,w_inst002
			w_inst001='dele file '+w_fipr
			w_inst002='rename '+w_fire+' to '+w_fipr
	
			private d0f9ve
			d0f9ve=sys(2003)+'\I'+d0coem+p_netuser+'ve'
			w_vfox=d0f9ve+'.mem'
			save to &w_vfox. all like w_inst*
			release all like w_inst*

			=visuafox('ejeproc(.f.)',w_vfox)

			=inkey(2)	&& R.06 Mariano

			if file(w_vfox)
				erase &w_vfox.
			endif
			
*			=inkey(2)	&& R.06 Mariano
		endif
	endfor
	dimension wwfiles(1)
	wwfiles=''
	*wwcant=a_dir('wwfiles',sys(2003)+'\'+d0coem+'\COMPRAS*.XLS','','',.t.)
*	wwcant=a_dir('wwfiles',wdici+'\'+d0coem+'\COMPRAS*.XLS','','',.t.)	&& R.03 Taboada	&& R.04 Mariano
	wwcant=a_dir('wwfiles',wdici+'\COMPRAS*.XLS','','',.t.)	&& R.04 Mariano
	wfinu=0
	for wfinu=1 to wwcant
		*w_fire='"'+sys(2003)+'\'+d0coem+'\'+wwfiles[wfinu,1]+'"'
*		w_fire='"'+wdici+'\'+d0coem+'\'+wwfiles[wfinu,1]+'"'	&& R.03 Taboada	&& R.04 Mariano
		w_fire='"'+wdici+'\'+wwfiles[wfinu,1]+'"'	&& R.04 Mariano
		if version()='Visual FoxPro'
			*w_fipr='"'+sys(2003)+'\'+d0coem+'\PROCESADO_'+wwfiles[wfinu,1]+'"'
*			w_fipr='"'+wdici+'\'+d0coem+'\PROCESADO_'+wwfiles[wfinu,1]+'"'	&& R.03 Taboada	&& R.04 Mariano
			w_fipr='"'+wdici+'\PROCESADO_'+wwfiles[wfinu,1]+'"'	&& R.04 Mariano
			dele file &w_fipr.
			rename &w_fire. to &w_fipr.
		else
			*w_fipr='"'+sys(2003)+'\'+d0coem+'\PROCESADO_'+wwfiles[wfinu,1]+'"'
*			w_fipr='"'+wdici+'\'+d0coem+'\PROCESADO_'+wwfiles[wfinu,1]+'"'	&& R.03 Taboada	&& R.04 Mariano
			w_fipr='"'+wdici+'\PROCESADO_'+wwfiles[wfinu,1]+'"'	&& R.04 Mariano
			private w_vfox,w_inst001,w_inst002
			w_inst001='dele file '+w_fipr
			w_inst002='rename '+w_fire+' to '+w_fipr

			private d0f9co
			d0f9co=sys(2003)+'\I'+d0coem+p_netuser+'co'
			w_vfox=d0f9co+'.mem'
			save to &w_vfox. all like w_inst*
			release all like w_inst*

			=visuafox('ejeproc(.f.)',w_vfox)

			=inkey(2)	&& R.06 Mariano

			if file(w_vfox)
				erase &w_vfox.
			endif
			
*			=inkey(2)	&& R.06 Mariano
		endif
	endfor

	sele 0
	create table (d0f99);
	(d99mens c(90),d99tipo c(1))	&& R.06 Mariano
	if version()='Visual FoxPro'
		w_type='type fox2x'
	else
		w_type=''
	endif
	copy to &d0f99.e.dbf &w_type
	use 

	fieldvec[ 1]='F9e'
	fieldvec[ 2]='Fiv'
	fieldvec[ 3]='Fic'

	use &d0f99.e in 0 alias f9e	exclusive	&& Errores de interfase
	use &d0coem.\inte005v in 0 alias fiv 	&& Interfase Ventas
	use &d0coem.\inte005c in 0 alias fic 	&& Interfase Compras

	if neterr()
		pop key
		pop menu _msysmenu
		wait clear
		=advsuave('No pudo abrir archivo de error de interface',w_title)
		=closall()
		fieldvec=''
		return
	endif

	fieldvec=''

	if empty(reccount('fiv')+reccount('fic')) 
		wait clear
		=advsuave('No hay comprobantes para procesar.',w_title)
		clear read
		return
	endif
			
	if !empty(reccount('fiv'))
		sele fiv
		go top
		do while !eof('fiv')
	
			if seek(fiv.d5tipo+fiv.d5sucu+fiv.d5tifo+fiv.d5tico+fiv.d5nume,'f05')
				&& R.05b Mariano
*				do error05in with 'Comprobante existente :'+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'
				do while fiv.d5tipo+fiv.d5sucu+fiv.d5tifo+fiv.d5tico+fiv.d5nume=f05.d5tipo+f05.d5sucu+f05.d5tifo+f05.d5tico+f05.d5nume and !eof('f05')
					if fiv.d5peri=f05.d5peri and fiv.d5inte!=f05.d5inte
						do error05in with 'Comprobante existente :'+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'
						exit
					endif
					skip in f05
				enddo
				&& R.05e Mariano
				skip in fiv
				loop
			endif

			if !seek(fiv.d5clie,'f02')
*				do error05in with 'Falta condici๓n impositiva o Cliente inexistente :'+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'	&& R.03 Mariano
				do error05in with 'Cliente inexistente :'+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'	&& R.03 Mariano
				skip in fiv
				loop
			endif
							
			&& R.07b Mariano							
			if fiv.d6tido$'80.86.87'
				if dicuit(left(fiv.d6cuit,11))<>right(fiv.d6cuit,1) and !empty(strtran(fiv.d6cuit,'-',''))
					do error05in with 'Error en CUIT o CUIL o CDI :'+fiv.d6cuit+' '+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'		&& R.08 Mariano	(error05in x error06in)
					skip in fiv
					loop
				endif
			endif
			&& R.07e Mariano
							
			if !seek(ptoi(fiv.d5peri),'F04')
				do error05in with 'Periodo inexistente :'+fiv.d5peri+' Comp:'+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'
				skip in fiv
				loop
			endif
			
			&& R.03b Taboada
			*if fiv.d5peri)!=subs(dtoc(fiv.d5fech),4,7))
			if left(dtos(fiv.d5fech),6)!=ptoi(fiv.d5peri)
				do error05in with 'La fecha no corresponde al periodo seleccionado :'+dtoc(fiv.d5fech)+' Comp:'+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'
				skip in fiv
				loop
			endif
			&& R.03e Taboada
					
			if seek(ptoi(fiv.d5peri),'f04') and f04.d4esve='C'
				do error05in with 'Periodo cerrado :'+fiv.d5peri+' Comp:'+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'
				skip in fiv
				loop
			endif
		
			if !seek(fiv.d5alic,'f23')
				do error05in with 'Alicuota de iva inexistente :'+fiv.d5alic+' Comp:'+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'
				skip in fiv
				loop
			endif

			if !seek('JU'+fiv.d5juri,'f01')
				do error05in with 'Jurisdicci๓n inexistente :'+fiv.d5juri+' Comp:'+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'
				skip in fiv
				loop
			endif

			if fiv.d5tiim != ftiimp(fiv.d5clie,fiv.d5peri,'C')
*				do error05in with 'Tipo impositivo difiere con el cliente :'+fiv.d5clie+' Comp:'+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'	&& R.03 Mariano
*				do error05in with 'La condici๓n impositiva del cliente :'+fiv.d5clie+' no coincide con la del tipo de'+' Comp:'+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'	&& R.03 Mariano	&& R.05 Mariano
				do error05in with 'La cond.impositiva cli.:'+fiv.d5clie+' difiere a la del'+' Comp:'+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'		&& R.05 Mariano
				skip in fiv
				loop
			endif

			&& R.02b Mariano
*			if !fiv.d5tifo$iif(fiv.d5tiim$'12','ACEMTZ ',iif(fiv.d5tiim='7','ABCEMTZ ','BCETZ '))	&& R.07 Mariano
			if !fiv.d5tifo$iif(fiv.d5tiim$'12','ACEMTZ ',iif(fiv.d5tiim='7','ABCEMTZ ','BCETZ ')) or !fiv.d5tifo$tiform(fiv.d5tico) && R.07 Mariano
				do error05in with 'Formulario incorrecto :'+fiv.d5tifo+' Comp:'+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'
				skip in fiv
				loop
			endif
			&& R.02e Mariano

			if !seek('RV'+fiv.d5rubr,'f01')
				do error05in with 'Rubro inexistente :'+fiv.d5rubr+' Comp:'+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'
				skip in fiv
				loop
			endif

			&& R.07b Mariano
			if fiv.d5tifo='C' and (fiv.d5imp4!=0 or fiv.d5imp5!=0)
				do error05in with 'No puede tener IVA el Formulario:'+fiv.d5tifo+' Comp:'+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'	&& R.08 Mariano	(error05in x error06in)
				skip in fiv
				loop
			endif
			&& R.07e Mariano

			if fiv.D5imp1+fiv.D5imp2+fiv.D5imp3+fiv.D5imp4+fiv.D5imp5+fiv.D5imp6+fiv.D5imp7+fiv.D5imp8!=fiv.d5tota
				do error05in with 'Diferencia entre la suma de los importes y el total'+' Comp:'+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'
				skip in fiv
				loop
			endif

			skip in fiv
		enddo
	
		if empty(reccount('f9e'))	&& Si no Hubo errores
			sele fiv
			go top
			do while !eof('fiv')
				sele fiv
				scatter memvar
				sele f05
			
				=db_rlock('f00')
				d0inte=right(str(1000001+val(f00.p0inte),7),6)
				d0imfa=f00.p0imfa
				sele f00
				replace p0inte with d0inte
				unlock in f00
	
				do lockeo05 in iwp005v
			
				wait 'Agregando registro id: '+d0inte wind nowait

				=seek(ptoi(fiv.d5peri),'F04')
				d0coim=f04->d4coim
				m.d5coim=d0coim
	
				sele f04

				replace d4vtot with d4vtot+m.d5tota;
						,d4vim1 with d4vim1+m.d5imp1;
						,d4vim2 with d4vim2+m.d5imp2;
						,d4vim3 with d4vim3+m.d5imp3;
						,d4vim4 with d4vim4+m.d5imp4;
						,d4vim5 with d4vim5+m.d5imp5;
						,d4vim6 with d4vim6+m.d5imp6;
						,d4vim7 with d4vim7+m.d5imp7;
						,d4vim8 with d4vim8+m.d5imp8

				go recno() in f04

				if m.d5clie>'99989'

					=net_appe('f06')
	
					sele f06
					replace d6inte with d0inte

					if f06.d6inte=d0inte
						sele f06
						m.d6inte=d0inte
						gather memvar
	
						go recno() in f06

					endif
				endif

				if d0imfa$'S'
					=net_appe('f07')
	
					sele f07
					replace d7inte with d0inte

					if f07.d7inte=d0inte
						sele f07
						m.d7nume=m.d5nume
						m.d7peri=m.d5peri
						m.d7empr=m.d5empr
						m.d7tipo=m.d5tipo
						m.d7sucu=m.d5sucu
						m.d7tico=m.d5tico
						m.d7tifo=m.d5tifo
						m.d7inte=d0inte
						gather memvar memo
	
						go recno() in f07

					endif
				endif

				=net_appe('f05')
				sele f05

				m.d5inte=d0inte
				gather memvar

				skip in fiv
			enddo
			wait clear
		
		else

			do while sino('Imprime Errores Ventas', 'SI', w_title)='S'	&& R.31 Mariano
				=skipbar('registro','Elegir',.t.)
				do getdest.spr
				=skipbar('registro','Elegir',.f.)
			enddo	&& R.31 Mariano (endif)

			sele f9e 
			zap

		endif
	clear read
	endif

	if !empty(reccount('fic'))
		sele fic
		go top
		do while !eof('fic')
	
			if seek(fic.d5tipo+fic.d5sucu+fic.d5tifo+fic.d5tico+fic.d5nume+fic.d5clie,'f05')
				&& R.05b Mariano
*				do error05in with 'Comprobante existente :'+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5nume,'C'
				do while fic.d5tipo+fic.d5sucu+fic.d5tifo+fic.d5tico+fic.d5nume+fic.d5clie=f05.d5tipo+f05.d5sucu+f05.d5tifo+f05.d5tico+f05.d5nume+f05.d5clie and !eof('f05')
					if fic.d5peri=f05.d5peri and fic.d5inte!=f05.d5inte
						do error05in with 'Comprobante existente :'+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5nume,'C'
						exit
					endif
					skip in f05
				enddo
				&& R.05e Mariano
				skip in fic
				loop
			endif

			if !seek(fic.d5clie,'f03')
*				do error05in with 'Proveedor inexistente :'+fic.d5clie+' Comp:'+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5nume,'C'	&& R.03 Mariano
				do error05in with 'Falta condici๓n impositiva o Proveedor inexistente :'+fic.d5clie+' Comp:'+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5nume,'C'	&& R.03 Mariano
				skip in fic
				loop
			endif
						
			&& R.07b Mariano							
			if fic.d6tido$'80.86.87'
				if dicuit(left(fic.d6cuit,11))<>right(fic.d6cuit,1) and !empty(strtran(fic.d6cuit,'-',''))
					do error05in with 'Error en CUIT o CUIL o CDI :'+fic.d6cuit+' '+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5nume,'C'		&& R.08 Mariano	(error05in x error06in)
					skip in fic
					loop
				endif
			endif
			&& R.07e Mariano
							
			if !seek(ptoi(fic.d5peri),'F04')
				do error05in with 'Periodo inexistente :'+fic.d5peri+' Comp:'+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5nume,'C'
				skip in fic
				loop
			endif
			
			&& R.03b Taboada
			*if fic.d5peri)!=subs(dtoc(fic.d5fech),4,7))
			if left(dtos(fic.d5fech),6)>ptoi(fic.d5peri)
				do error05in with 'La fecha no corresponde al periodo seleccionado :'+dtoc(fic.d5fech)+' Comp:'+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5nume,'C'
				skip in fic
				loop
			endif
			&& R.03e Taboada
			
			if seek(ptoi(fic.d5peri),'f04') and f04.d4esco='C'
				do error05in with 'Periodo cerrado :'+fic.d5peri+' Comp:'+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5nume,'C'
				skip in fic
				loop
			endif
						
			if !seek(fic.d5alic,'f23')
				do error05in with 'Alicuota de iva inexistente :'+fic.d5alic+' Comp:'+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5nume,'C'
				skip in fic
				loop
			endif
	
			if !seek('JU'+fic.d5juri,'f01')
				do error05in with 'Jurisdicci๓n inexistente :'+fic.d5juri+' Comp:'+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5nume,'C'
				skip in fic
				loop
			endif

			if fic.d5tiim != ftiimp(fic.d5clie, fic.d5peri,'P')
*				do error05in with 'La condici๓n de IVA difiere del proveedor: '+fic.d5clie+' Comp:'+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5nume,'C'	&& R.03 Mariano
*				do error05in with 'La condici๓n impositiva del proveedor: '+fic.d5clie+' no coincide con la del tipo de'+' Comp:'+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5nume,'C'	&& R.03 Mariano	&& R.05 Mariano
				do error05in with 'La cond.impositiva prov.: '+fic.d5clie+' difiere a la del'+' Comp:'+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5nume,'C'		&& R.05 Mariano
				skip in fic
				loop
			endif
			
			&& R.02b Mariano
*			if !fic.d5tifo$iif(fic.d5tiim$'1','ABIM ','BCI ') && R.07 Mariano
			if !fic.d5tifo$iif(fic.d5tiim$'1','ABIM ','BCI ') or  !fic.d5tifo$tiformC(fic.d5tico) && R.07 Mariano
				do error05in with 'Formulario incorrecto :'+fic.d5tifo+' Comp:'+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5nume,'C'
				skip in fic
				loop
			endif
			&& R.02e Mariano
			
			if !seek('RC'+fic.d5rubr,'f01')
				do error05in with 'Rubro inexistente :'+fic.d5rubr+' Comp:'+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5nume,'C'	&& R.06 Mariano (d5juri x d5rubr)
				skip in fic
				loop
			endif


			&& R.08b Mariano (Perc o Ret)
			if !empty(fic.d5pere) or fic.d5imp3!=0
				if seek(fic.d5pere+'I'+iif(fic.d5tico!='R','P','R'),'f24')
*					if f24.d24codi!='000' or f24.d24tipo!='I' or f24.d24refe!=iif(fic.d5tico!='R','P','R')	&& R.09 Mariano
					if f24.d24tipo!='I' or f24.d24refe!=iif(fic.d5tico!='R','P','R')	&& R.09 Mariano
						do error05in with 'Perc./Ret. Iva no corresponde :'+fic.d5pere+' '+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5sucu+'-'+fic.d5nume,'C'	&& R.08 Mariano	(error05in x error06in)
						skip in fic
						loop
					endif		
				else
					do error05in with 'Perc./Ret. Iva inexistente :'+fic.d5pere+' '+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5sucu+'-'+fic.d5nume,'C'	&& R.08 Mariano	(error05in x error06in)
					skip in fic
					loop
				Endif
			endif
			
			if !empty(fic.d5cpib)
				=SEEK("JU" + fic.d5juri, "f01")
				cafip	= f01.d1tdgi
				do case
					case f00.p0prc1=1 and fic.d5imp6!=0
						if !seek(cafip+fic.d5cpib+iif(fic.d5tico!='R','P','R'),'f25') and fic.d5cpib!='000'	
							do error05in with 'Perc./Ret. IB inexistente :'+fic.d5cpib+' '+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5sucu+'-'+fic.d5nume,'C'	&& R.08 Mariano	(error05in x error06in)
							skip in fic
							loop
						endif
					case f00.p0prc2=1 and fic.d5imp7!=0
						if !seek(cafip+fic.d5cpib+iif(fic.d5tico!='R','P','R'),'f25') and fic.d5cpib!='000'	
							do error05in with 'Perc./Ret. IB inexistente :'+fic.d5cpib+' '+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5sucu+'-'+fic.d5nume,'C'	&& R.08 Mariano	(error05in x error06in)
							skip in fic
							loop
						endif
					case f00.p0prc3=1 and fic.d5imp8!=0
						if !seek(cafip+fic.d5cpib+iif(fic.d5tico!='R','P','R'),'f25') and fic.d5cpib!='000'	
							do error05in with 'Perc./Ret. IB inexistente :'+fic.d5cpib+' '+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5sucu+'-'+fic.d5nume,'C'	&& R.08 Mariano	(error05in x error06in)
							skip in fic
							loop
						endif
					otherwise
						do error05in with 'Falta definir Columna para IB o no hay importe IB :'+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5sucu+'-'+fic.d5nume,'C'	&& R.08 Mariano	(error05in x error06in)
						skip in fic
						loop
				endcase
			else
				do case
					case f00.p0prc1=1 and fic.d5imp6!=0
						do error05in with 'Falta Perc./Ret. IB :'+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5sucu+'-'+fic.d5nume,'C'	&& R.08 Mariano	(error05in x error06in)
						skip in fic
						loop
					case f00.p0prc2=1 and fic.d5imp7!=0
						do error05in with 'Falta Perc./Ret. IB :'+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5sucu+'-'+fic.d5nume,'C'	&& R.08 Mariano	(error05in x error06in)
						skip in fic
						loop
					case f00.p0prc3=1 and fic.d5imp8!=0
						do error05in with 'Falta Perc./Ret. IB :'+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5sucu+'-'+fic.d5nume,'C'	&& R.08 Mariano	(error05in x error06in)
						skip in fic
						loop
				endcase
			endif
			
			if !empty(fic.d5cpga)
				do case
					case f00.p0prc1=2 and fic.d5imp6!=0
						if !seek(fic.d5cpga+'G'+iif(fic.d5tico!='R','P','R'),'f24') and fic.d5cpga!='000'
							do error05in with 'Perc./Ret. Gcias inexistente :'+fic.d5cpga+' '+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5sucu+'-'+fic.d5nume,'C'	&& R.08 Mariano	(error05in x error06in)
							skip in fic
							loop
						endif
					case f00.p0prc2=2 and fic.d5imp7!=0
						if !seek(fic.d5cpga+'G'+iif(fic.d5tico!='R','P','R'),'f24') and fic.d5cpga!='000'
							do error05in with 'Perc./Ret. Gcias inexistente :'+fic.d5cpga+' '+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5sucu+'-'+fic.d5nume,'C'	&& R.08 Mariano	(error05in x error06in)
							skip in fic
							loop
						endif
					case f00.p0prc3=2 and fic.d5imp8!=0
						if !seek(fic.d5cpga+'G'+iif(fic.d5tico!='R','P','R'),'f24') and fic.d5cpga!='000'
							do error05in with 'Perc./Ret. Gcias inexistente :'+fic.d5cpga+' '+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5sucu+'-'+fic.d5nume,'C'	&& R.08 Mariano	(error05in x error06in)
							skip in fic
							loop
						endif
					otherwise
						do error05in with 'Falta definir Columna para Gcias o no hay importe Gcias :'+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5sucu+'-'+fic.d5nume,'C'	&& R.08 Mariano	(error05in x error06in)
						skip in fic
						loop
				endcase
			else
				do case
					case f00.p0prc1=2 and fic.d5imp6!=0
						do error05in with 'Falta Perc./Ret. Gcias :'+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5sucu+'-'+fic.d5nume,'C'	&& R.08 Mariano	(error05in x error06in)
						skip in fic
						loop
					case f00.p0prc2=2 and fic.d5imp7!=0
						do error05in with 'Falta Perc./Ret. Gcias :'+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5sucu+'-'+fic.d5nume,'C'	&& R.08 Mariano	(error05in x error06in)
						skip in fic
						loop
					case f00.p0prc3=2 and fic.d5imp8!=0
						do error05in with 'Falta Perc./Ret. Gcias :'+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5sucu+'-'+fic.d5nume,'C'	&& R.08 Mariano	(error05in x error06in)
						skip in fic
						loop
				endcase
			endif
			
			if !empty(fic.d5cpot)
				do case
					case f00.p0prc1=3 and fic.d5imp6!=0
						if !seek(fic.d5cpot+'O'+iif(fic.d5tico!='R','P','R'),'f24') and fic.d5cpot!='000'
							do error05in with 'Perc./Ret. Otros inexistente :'+fic.d5cpot+' '+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5sucu+'-'+fic.d5nume,'C'	&& R.08 Mariano	(error05in x error06in)
							skip in fic
							loop
						endif
					case f00.p0prc2=3 and fic.d5imp7!=0
						if !seek(fic.d5cpot+'O'+iif(fic.d5tico!='R','P','R'),'f24') and fic.d5cpot!='000'
							do error05in with 'Perc./Ret. Otros inexistente :'+fic.d5cpot+' '+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5sucu+'-'+fic.d5nume,'C'	&& R.08 Mariano	(error05in x error06in)
							skip in fic
							loop
						endif
					case f00.p0prc3=3 and fic.d5imp8!=0
						if !seek(fic.d5cpot+'O'+iif(fic.d5tico!='R','P','R'),'f24') and fic.d5cpot!='000'
							do error05in with 'Perc./Ret. Otros inexistente :'+fic.d5cpot+' '+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5sucu+'-'+fic.d5nume,'C'	&& R.08 Mariano	(error05in x error06in)
							skip in fic
							loop
						endif
					otherwise
						do error05in with 'Falta definir Columna para Otros o no hay importe Otros :'+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5sucu+'-'+fic.d5nume,'C'	&& R.08 Mariano	(error05in x error06in)
						skip in fic
						loop
				endcase
			else
				do case
					case f00.p0prc1=3 and fic.d5imp6!=0
						do error05in with 'Falta Perc./Ret. Otros :'+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5sucu+'-'+fic.d5nume,'C'	&& R.08 Mariano	(error05in x error06in)
						skip in fic
						loop
					case f00.p0prc2=3 and fic.d5imp7!=0
						do error05in with 'Falta Perc./Ret. Otros :'+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5sucu+'-'+fic.d5nume,'C'	&& R.08 Mariano	(error05in x error06in)
						skip in fic
						loop
					case f00.p0prc3=3 and fic.d5imp8!=0
						do error05in with 'Falta Perc./Ret. Otros :'+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5sucu+'-'+fic.d5nume,'C'	&& R.08 Mariano	(error05in x error06in)
						skip in fic
						loop
				endcase
			endif
			&& R.08e Mariano

			&& R.07b Mariano
			if fic.d5tifo='C' and (fic.d5imp4!=0 or fic.d5imp5!=0)
				do error05in with 'No puede tener IVA el Formulario:'+fic.d5tifo+' Comp:'+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5nume,'C'	&& R.08 Mariano	(error05in x error06in)
				skip in fic
				loop
			endif
			&& R.07e Mariano

			if fic.D5imp1+fic.D5imp2+fic.D5imp3+fic.D5imp4+fic.D5imp5+fic.D5imp6+fic.D5imp7+fic.D5imp8!=fic.d5tota
				do error05in with 'Diferencia entre la suma de los importes y el total'+' Comp:'+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5nume,'C'
				skip in fic
				loop
			endif

			skip in fic
		enddo
	
		if empty(reccount('f9e'))	&& Si no Hubo errores
			sele fic
			go top
			do while !eof('fic')
				sele fic
				scatter memvar
				sele f05
			
				=db_rlock('f00')
				d0inte=right(str(1000001+val(f00.p0inte),7),6)
				d0imfa=f00.p0imfa
				sele f00
				replace p0inte with d0inte
				unlock in f00

				do lockeo05 in iwp005c
			
				wait 'Agregando registro id: '+d0inte wind nowait

				=seek(ptoi(fic.d5peri),'F04')
				d0coim=f04->d4coim
				m.d5coim=d0coim
	
				sele f04

				replace d4ctot with d4ctot+m.d5tota;
						,d4cim1 with d4cim1+m.d5imp1;
						,d4cim2 with d4cim2+m.d5imp2;
						,d4cim3 with d4cim3+m.d5imp3;
						,d4cim4 with d4cim4+m.d5imp4;
						,d4cim5 with d4cim5+m.d5imp5;
						,d4cim6 with d4cim6+m.d5imp6;
						,d4cim7 with d4cim7+m.d5imp7;
						,d4cim8 with d4cim8+m.d5imp8

				go recno() in f04

				if m.d5clie>'99989'
					=net_appe('f06')

					sele f06
					replace d6inte with d0inte

					if f06.d6inte=d0inte
						sele f06
						m.d6nume=m.d5nume
						m.d6peri=m.d5peri
						m.d6empr=m.d5empr
						m.d6tipo=m.d5tipo
						m.d6sucu=m.d5sucu
						m.d6tico=m.d5tico
						m.d6tifo=m.d5tifo
						m.d6inte=d0inte
						gather memvar
	
						go recno() in f06
	
					endif
				endif

				if d0imfa$'S'
					=net_appe('f07')

					sele f07
					replace d7inte with d0inte

					if f07.d7inte=d0inte
						sele f07
						m.d7nume=m.d5nume
						m.d7peri=m.d5peri
						m.d7empr=m.d5empr
						m.d7tipo=m.d5tipo
						m.d7sucu=m.d5sucu
						m.d7tico=m.d5tico
						m.d7tifo=m.d5tifo
						m.d7inte=d0inte
						gather memvar memo

						go recno() in f07

					endif
				endif

				=net_appe('f05')
				sele f05

				m.d5inte=d0inte
				
				&& R.08b Mariano
				if !empty(fic.d5pere)
					m.d5reiv=iif(fic.d5tico!='R','P','R')
				else
					m.d5reiv=''
				endif
				if !empty(fic.d5cpib)
					m.d5regi=iif(fic.d5tico!='R','P','R')
				else
					m.d5regi=''
				endif
				if !empty(fic.d5cpga)
					m.d5rega=iif(fic.d5tico!='R','P','R')
				else
					m.d5rega=''
				endif
				if !empty(fic.d5cpot)
					m.d5prco=iif(fic.d5tico!='R','P','R')
				else
					m.d5prco=''
				endif
				&& R.08e Mariano

				gather memvar

				skip in fic
			enddo
			wait clear
		else
			do while sino('Imprime Errores Compras', 'SI', w_title)='S'	&& R.31 Mariano
				=skipbar('registro','Elegir',.t.)
				do getdest.spr
				=skipbar('registro','Elegir',.f.)
			enddo	&& R.31 Mariano (endif)

			sele f9e 
			zap

		endif

	endif

	wait clear

endif

return


*-P อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
Procedure Error05in
*-P อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
Param w_text,w_tipo
=set_alia('f9e')
=net_appe('f9e')
repl f9e.d99mens with w_text, f9e.d99tipo with w_tipo
=rest_alia()
return
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC valid05in
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS w_show

***vfp***
if version()='Visual Fox' and (w_pfile!='05in' or (!wexist('IWS005IN') and wontop()!='IWS005IN')) and w_show
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

PRIVATE w_retu,w_vari
w_retu=.t.
w_vari=varread()

&& R03.b Taboada
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

	SHOW GETS
endif
		
w_dperi=ptoi(wdperi)
		
show gets

if w_vari='W_BTN1'
	keyboard '{f9}'
	_curobj=1
endif

&& R.03e Taboada

return(w_retu)
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc genera05in		&& genera los intexxx a partir de planillas Excel
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

private wwfiles,w_d18fire
dimension wwfiles(1)
wwfiles=''
*wwcant=a_dir('wwfiles',sys(2003)+'\'+d0coem+'\VENTAS*.XLS')
*wwcant=a_dir('wwfiles',wdici+'\'+d0coem+'\VENTAS*.XLS')	&& R.03 Taboada	&& R.04 Mariano
*wwcant=a_dir('wwfiles',wdici+'\VENTAS*.XLS')	&& R.04 Mariano	&& R.06 Mariano
wwcant=a_dir('wwfiles',wdici+'\VENTAS*.XLS','','',.t.)  		&& R.06 Mariano
wfinu=0
&& R.02b Mariano
if used('fivx')
	sele fivx
else
&& R.02e Mariano
	sele 0
	use &d0coem.\intevtas alias fivx exclusive
endif	&& R.02 Mariano
for wfinu=1 to wwcant
	wait wind 'Procesando Interfase Ventas...' nowait
	
*	w_fire='"'+sys(2003)+'\'+d0coem+'\'+wwfiles[wfinu,1]+'"'	&& R.03 Taboada
*	w_fire='"'+wdici+'\'+d0coem+'\'+wwfiles[wfinu,1]+'"'	&& R.03 Taboada	&& R.04 Mariano
	w_fire='"'+wdici+'\'+wwfiles[wfinu,1]+'"'	&& R.04 Mariano
	if version()='Visual FoxPro'
		append from &w_fire type xls
	else
		use in fivx
		private w_vfox,w_inst001,w_inst002,w_inst003
		w_inst001='use '+sys(2003)+'\'+d0coem+'\intevtas.dbf in 0 alias fivx exclusive'
		w_inst002='select fivx'
		w_inst003='append from '+w_fire+' type xls'

		private d0f9ve
		d0f9ve=sys(2003)+'\I'+d0coem+p_netuser+'ve'
		w_vfox=d0f9ve+'.mem'
		save to &w_vfox. all like w_inst*
		release all like w_inst*

		=visuafox('ejeproc(.f.)',w_vfox)

		=inkey(2)	&& R.06 Mariano

		if file(w_vfox)
			erase &w_vfox.
		endif
			
*		=inkey(2)	&& R.06 Mariano
		w_paso=1
		do while .t.
			fieldvec[1]='fivx'
			use &d0coem.\intevtas alias fivx exclusive
			
			if neterr()
				=advsuave('Procesando archivo Excel '+justfname(w_fire),w_title)	&& R.06 Mariano (justfname x justpath)
				fieldvec=''
				=inkey(2)	&& R.08 Mariano
			endif

			fieldvec=''
			
			if alias()='fivx' or w_paso>=5
				exit
			endif
			w_paso=w_paso+1
		enddo
	endif
endfor
*////////// VENTAS \\\\\\\\\\*


if !file(d0coem+'\INTE005v.DBF') 
	sele 0
	use fi\00\ivf005
	copy structure extended to &d0coem.\inte005x.dbf
	use &d0coem.\inte005x alias f05x exclusive	&& Interfase Ventas
	sele 0
	use fi\00\ivf006
	copy structure extended to &d0coem.\inte006x.dbf
	use
	sele f05x
	append from  &d0coem.\inte006x.dbf
	use
	create &d0coem.\inte006x.dbf from &d0coem.\inte005x.dbf
	use &d0coem.\inte006x
	
	if version()='Visual FoxPro'
		w_type='type fox2x'
	else
		w_type=''
	endif
	copy to &d0coem.\inte005v.dbf &w_type
	use
	
	dele file &d0coem.\inte005x.dbf
	dele file &d0coem.\inte006x.dbf
endif

sele 0
use &d0coem.\inte005v in 0 alias fiv exclusive	&& Interfase Ventas
zap

SELE fiv

if used('fivx')	&& R.08 Mariano

GO TOP IN fivx
DO WHILE !EOF('fivx')

	wait wind 'Procesando Interfase Ventas...' nowait

	if empty(ctod(fivx.fecha)) or empty(fivx.desde_fact) or fivx.desde_fact='desde_fact'
		skip in fivx
		loop
	endif


	GO BOTTOM
	STORE fiv.D5inte to ld5inte
	ld5inte = Val(ld5inte)+1

	append blank

*****	Alicuotas	*****

	
	GO TOP IN F23
	DO WHILE !EOF('f23')

		if f23.d23alic=fivx.tasa
	    	replace fiv.d5alic with f23.D23codi
		ENDIF
 
		SKIP IN F23
	ENDDO
	
*****	Comp.Generico	*****

	replace fiv.D5fech with CTOD(fivx.fecha),;
			fiv.D5empr with d0coem,;
			fiv.d5titr with '',;
			fiv.d5ttip with '0',;
			fiv.D5peri with wdperi,;
			fiv.D5inte with Padl(alltr(str(ld5inte,6)), 6,'0'),;
			fiv.D5sucu with '0'+subs(fivx.Desde_fact,5,4),;
			fiv.D5por1 with fivx.Tasa,;
			fiv.D5imp1 with fivx.Neto,;
			fiv.D5imp2 with fivx.Exento,;
			fiv.D5imp4 With fivx.Iva_insc,;
			fiv.D5tota with fivx.Total	&& R.03 Mariano (wdperi por subs(fivx.fecha,4,7))	&& R.10 Mariano (agregue '0'+ antes de los 4 digitos de la sucursal)
	
*****   Imp.Interno     *****

	if 	!empty(fivx.impint)
		do case
			&& R.11b Mariano
			case f00.p0prv1=5	&& Imp.Interno
				replace fiv.d5imp6 with fivx.impint
			case f00.p0prv2=5	&& Imp.Interno
				replace fiv.d5imp7 with fivx.impint
			case f00.p0prv3=5	&& Imp.Interno
				replace fiv.d5imp8 with fivx.impint
			case f00.p0prv1=6	&& Sin Asignar
				replace fiv.d5imp6 with fivx.impint
			case f00.p0prv2=6	&& Sin Asignar
				replace fiv.d5imp7 with fivx.impint
			case f00.p0prv3=6	&& Sin Asignar
				replace fiv.d5imp8 with fivx.impint
			&& R.11e Mariano
			case f00.p0prv1!=1 and f00.p0prv1!=2
				replace fiv.d5imp6 with fivx.impint
			case f00.p0prv2!=1 and f00.p0prv2!=2
				replace fiv.d5imp7 with fivx.impint
			case f00.p0prv3!=1 and f00.p0prv3!=2
				replace fiv.d5imp8 with fivx.impint
		endcase
	endif
	
	DO CASE	
*****	Comp.Tique Z	*****

		CASE upper(subs(fivx.Desde_fact,1,3))='TIC'	&& R.08 Mariano (agregue upper() y en mayusculas)
			replace fiv.D5tifo with 'Z',;
					fiv.D5tico with '?',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('?', 'Z'),;
					fiv.D5imfi with 'S',;
					fiv.D5nume with subs(fivx.Desde_fact,10,8)+'/'+subs(fivx.Hasta_fact,10,8),;
					fiv.D5clie with '99993',;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '3',;
					fiv.D5tiop with '0'
		
*****	Comp.Tique factura A	*****

		CASE upper(subs(fivx.Desde_fact,1,3))='TFA'	&& R.08 Mariano (agregue upper() y en mayusculas)
			replace fiv.D5tifo with 'A',;
					fiv.D5tico with '6',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('6', 'A'),;
					fiv.D5imfi with 'S',;
					fiv.D5nume with subs(fivx.Desde_fact,10,8)+'/'+iif(fivx.Desde_fact<fivx.Hasta_fact, subs(fivx.Hasta_fact,10,8), ''),;
					fiv.D5clie with '99991',;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '1',;
					fiv.D5tiop with '0'
		
*****	Comp.Tique NC A	*****

		CASE upper(subs(fivx.Desde_fact,1,3))='TCA'	&& R.08 Mariano (agregue upper() y en mayusculas)
			replace fiv.D5tifo with 'A',;
					fiv.D5tico with 'E',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('E', 'A'),;
					fiv.D5imfi with 'S',;
					fiv.D5nume with subs(fivx.Desde_fact,10,8)+'/'+iif(fivx.Desde_fact<fivx.Hasta_fact, subs(fivx.Hasta_fact,10,8), ''),;
					fiv.D5clie with '99991',;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '1',;
					fiv.D5tiop with '0'
		
*****	Comp.Tique ND A	*****

		CASE upper(subs(fivx.Desde_fact,1,3))='TDA'	&& R.08 Mariano (agregue upper() y en mayusculas)
			replace fiv.D5tifo with 'A',;
					fiv.D5tico with 'D',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('D', 'A'),;
					fiv.D5imfi with 'S',;
					fiv.D5nume with subs(fivx.Desde_fact,10,8)+'/'+iif(fivx.Desde_fact<fivx.Hasta_fact, subs(fivx.Hasta_fact,10,8), ''),;
					fiv.D5clie with '99991',;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '1',;
					fiv.D5tiop with '0'
		
*****	Facturas A	*****

		CASE upper(subs(fivx.Desde_fact,1,3))='FCA'	&& R.08 Mariano (agregue upper() y en mayusculas)
			replace fiv.D5tifo with 'A',;
					fiv.D5tico with 'A',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('A', 'A'),;
					fiv.D5imfi with 'N',;
					fiv.D5nume with subs(fivx.Desde_fact,10,8)+'/'+iif(fivx.Desde_fact<fivx.Hasta_fact, subs(fivx.Hasta_fact,10,8), ''),;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '1',;
		 			fiv.D5tiop with '0'

*****	NOTA DE CREDITO A	*****

		CASE upper(subs(fivx.Desde_fact,1,3))='NCA'	&& R.08 Mariano (agregue upper() y en mayusculas)
			replace fiv.D5tifo with 'A',;
					fiv.D5tico with 'C',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('C', 'A'),;
					fiv.D5imfi with 'N',;
					fiv.D5nume with subs(fivx.Desde_fact,10,8)+'/'+iif(fivx.Desde_fact<fivx.Hasta_fact, subs(fivx.Hasta_fact,10,8), ''),;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '1',;
		 			fiv.D5tiop with '0'

*****	NOTA DE DEBITO A	*****

		CASE upper(subs(fivx.Desde_fact,1,3))='NDA'	&& R.08 Mariano (agregue upper() y en mayusculas)
			replace fiv.D5tifo with 'A',;
					fiv.D5tico with 'B',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('B', 'A'),;
					fiv.D5imfi with 'N',;
					fiv.D5nume with subs(fivx.Desde_fact,10,8)+'/'+iif(fivx.Desde_fact<fivx.Hasta_fact, subs(fivx.Hasta_fact,10,8), ''),;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '1',;
		 			fiv.D5tiop with '0'

*****	Comp. Tiquet factura B	*****

		CASE upper(subs(fivx.Desde_fact,1,3))='TFB'	&& R.08 Mariano (agregue upper() y en mayusculas)
			replace fiv.D5tifo with 'B',;
					fiv.D5tico with '6',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('6', 'B'),;
					fiv.D5imfi with 'S',;
					fiv.D5nume with subs(fivx.Desde_fact,10,8)+'/'+iif(fivx.Desde_fact<fivx.Hasta_fact, subs(fivx.Hasta_fact,10,8), ''),;
					fiv.D5clie with '99993',;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '3',;
		 			fiv.D5tiop with '0'

*****	Comp. Tiquet NC B	*****

		CASE upper(subs(fivx.Desde_fact,1,3))='TCB'	&& R.08 Mariano (agregue upper() y en mayusculas)
			replace fiv.D5tifo with 'B',;
					fiv.D5tico with 'E',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('E', 'B'),;
					fiv.D5imfi with 'S',;
					fiv.D5nume with subs(fivx.Desde_fact,10,8)+'/'+iif(fivx.Desde_fact<fivx.Hasta_fact, subs(fivx.Hasta_fact,10,8), ''),;
					fiv.D5clie with '99993',;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '3',;
		 			fiv.D5tiop with '0'

*****	Comp. Tiquet ND B	*****

		CASE upper(subs(fivx.Desde_fact,1,3))='TDB'	&& R.08 Mariano (agregue upper() y en mayusculas)
			replace fiv.D5tifo with 'B',;
					fiv.D5tico with 'D',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('D', 'B'),;
					fiv.D5imfi with 'S',;
					fiv.D5nume with subs(fivx.Desde_fact,10,8)+'/'+iif(fivx.Desde_fact<fivx.Hasta_fact, subs(fivx.Hasta_fact,10,8), ''),;
					fiv.D5clie with '99993',;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '3',;
		 			fiv.D5tiop with '0'

*****	Facturas B	*****

		CASE upper(subs(fivx.Desde_fact,1,3))='FCB'	&& R.08 Mariano (agregue upper() y en mayusculas)
			replace fiv.D5tifo with 'B',;
					fiv.D5tico with 'A',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('A', 'B'),;
					fiv.D5imfi with 'N',;
					fiv.D5nume with subs(fivx.Desde_fact,10,8)+'/'+iif(fivx.Desde_fact<fivx.Hasta_fact, subs(fivx.Hasta_fact,10,8), ''),;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '3',;
		 			fiv.D5tiop with '0'

*****	NOTA DE CREDITO B	*****

		CASE upper(subs(fivx.Desde_fact,1,3))='NCB'	&& R.08 Mariano (agregue upper() y en mayusculas)
			replace fiv.D5tifo with 'B',;
					fiv.D5tico with 'C',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('C', 'B'),;
					fiv.D5imfi with 'N',;
					fiv.D5nume with subs(fivx.Desde_fact,10,8)+'/'+iif(fivx.Desde_fact<fivx.Hasta_fact, subs(fivx.Hasta_fact,10,8), ''),;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '3',;
		 			fiv.D5tiop with '0'

*****	NOTA DE DEBITO B	*****

		CASE upper(subs(fivx.Desde_fact,1,3))='NDB'	&& R.08 Mariano (agregue upper() y en mayusculas)
			replace fiv.D5tifo with 'B',;
					fiv.D5tico with 'B',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('B', 'B'),;
					fiv.D5imfi with 'N',;
					fiv.D5nume with subs(fivx.Desde_fact,10,8)+'/'+iif(fivx.Desde_fact<fivx.Hasta_fact, subs(fivx.Hasta_fact,10,8), ''),;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '3',;
		 			fiv.D5tiop with '0'

	ENDCASE	

*****	Clientes	*****
	set order to d2cuit in f02
	if !empty(fivx.Cuit) and seek(fivx.Cuit,'f02')
		replace fiv.d5clie with f02.d2clie,;
				fiv.d5juri with f02.d2juri
		&& R.02b Mariano
		if fiv.d5tiim=3	&& para el caso de las B deberia ser 3,4,5,6,7
			replace fiv.d5tiim with ftiimp(fiv.d5clie,fiv.d5peri,'C')
		endif
		&& R.02e Mariano
	else
		replace fiv.d6nume with fiv.d5nume,;
				fiv.d6peri with fiv.d5peri,;
				fiv.d6empr with d0coem,;
				fiv.d6tipo with fiv.d5tipo,;
				fiv.d6sucu with fiv.d5sucu,;
				fiv.d6tico with fiv.d5tico,;
				fiv.d6tifo with fiv.d5tifo,;
				fiv.d6inte with fiv.d5inte,;
				fiv.d6nocl with fivx.cliente,;
				fiv.d6cuit with fivx.cuit
		do case
			case upper(fivx.iva)='RI'
				replace fiv.d5clie with '99991',;
						fiv.d6tido with '80'
			case upper(fivx.iva)='RNI'
				replace fiv.d5clie with '99992',;
						fiv.d6tido with '80'
			case upper(fivx.iva)='ACF'
				replace fiv.d5clie with '99993',;
						fiv.d6tido with '96'
			case upper(fivx.iva)='EX'
				replace fiv.d5clie with '99994',;
						fiv.d6tido with '80'
				&& R.02b Mariano
				if fiv.d5tiim='3'	&& para el caso de las B deberia ser 3,4,5,6,7
					replace fiv.d5tiim with '4'
				endif
				&& R.02e Mariano
			case upper(fivx.iva)='NR'
				replace fiv.d5clie with '99995',;
						fiv.d6tido with '96'
				&& R.02b Mariano
				if fiv.d5tiim='3'	&& para el caso de las B deberia ser 3,4,5,6,7
					replace fiv.d5tiim with '5'
				endif
				&& R.02e Mariano
			case upper(fivx.iva)='RM'
				replace fiv.d5clie with '99996',;
						fiv.d6tido with '80'
				&& R.02b Mariano
				if fiv.d5tiim='3'	&& para el caso de las B deberia ser 3,4,5,6,7
					replace fiv.d5tiim with '6'
				endif
				&& R.02e Mariano
			case upper(fivx.iva)='NC'
				replace fiv.d5clie with '99997',;
						fiv.d6tido with '80'
				&& R.02b Mariano
				if fiv.d5tiim='3'	&& para el caso de las B deberia ser 3,4,5,6,7
					replace fiv.d5tiim with '7'
				endif
				&& R.02e Mariano
			&& R.02b Mariano
			otherwise	&& si viene vacio fuerzo ACF
				if fiv.d5tiim='3'	&& para el caso de las B deberia ser 3,4,5,6,7
					replace fiv.d5clie with '99993',;
							fiv.d6tido with '96'
				endif
			&& R.02e Mariano
		endcase
		set order to d2clie in f02
		if seek(fiv.d5clie,'f02')
			replace fiv.D5juri with f02.d2juri
			if empty(fiv.d6nocl)
				replace fiv.d6nocl with f02.d2nomb,;
						fiv.d6cuit with f02.d2cuit,;
						fiv.d6dire with f02.d2dire,;
						fiv.d6copo with f02.d2copo,;
						fiv.d6loca with f02.d2loca,;
						fiv.d6tido with f02.d2tido
			endif
		endif
	endif
	set order to d2clie in f02
	
	replace fiv.d5leye with readf('f02',fiv.d5clie,'d2nomb',7)+' '+tcabre(fiv.d5tico)+' '
*	replace fiv.d5leye with allt(fiv.d5leye)+' '+fiv.d5tifo+fiv.d5sucu+'-'+fiv.d5nume	&& R.05 Mariano
	replace fiv.d5leye with iif(len(rtrim(allt(fiv.d5leye)+' '+fiv.d5tifo+fiv.d5sucu+'-'+fiv.d5nume))>30, left(allt(fiv.d5leye)+' '+fiv.d5tifo+fiv.d5sucu+'-'+fiv.d5nume,26)+right(fiv.d5nume,4), allt(fiv.d5leye)+' '+fiv.d5tifo+fiv.d5sucu+'-'+fiv.d5nume)	&& R.05 Mariano

*****	Rubro	*****

	if type('fivx.rubro')='U'
		replace fiv.d5rubr with '999999' 	&& codigo arbitrario para interfase, debe existir en la empresa
	else
		replace fiv.d5rubr with fivx.rubro
	endif
	
	SKIP IN fivx
	
ENDDO

sele fivx
zap

endif 	&& R.08 Mariano

sele fiv
use in fiv

dimension wwfiles(1)
wwfiles=''
*wwcant=a_dir('wwfiles',sys(2003)+'\'+d0coem+'\COMPRA*.XLS')
*wwcant=a_dir('wwfiles',wdici+'\'+d0coem+'\COMPRA*.XLS')	&& R.03 Taboada	&& R.04 Mariano
*wwcant=a_dir('wwfiles',wdici+'\COMPRA*.XLS')	&& R.04 Mariano	&& R.06 Mariano
wwcant=a_dir('wwfiles',wdici+'\COMPRAS*.XLS','','',.t.)			&& R.06 Mariano
wfinu=0
&& R.02b Mariano
if used('ficx')
	sele ficx
else
&& R.02e Mariano
	sele 0
	use &d0coem.\intecpra alias ficx exclusive
endif && R.02 Mariano
for wfinu=1 to wwcant
	wait wind 'Procesando Interfase Compras...' nowait
	
*	w_fire='"'+sys(2003)+'\'+d0coem+'\'+wwfiles[wfinu,1]+'"'	&& R.03 Taboada
*	w_fire='"'+wdici+'\'+d0coem+'\'+wwfiles[wfinu,1]+'"'	&& R.03 Taboada	&& R.04 Mariano
	w_fire='"'+wdici+'\'+wwfiles[wfinu,1]+'"'	&& R.04 Mariano
	if version()='Visual FoxPro'
		append from &w_fire type xls
	else
		use in ficx
		private w_vfox,w_inst001,w_inst002,w_inst003
		w_inst001='use '+sys(2003)+'\'+d0coem+'\intecpra.dbf in 0 alias ficx exclusive'
		w_inst002='select ficx'
		w_inst003='append from '+w_fire+' type xls'

		private d0f9co
		d0f9co=sys(2003)+'\I'+d0coem+p_netuser+'co'
		w_vfox=d0f9co+'.mem'
		save to &w_vfox. all like w_inst*
		release all like w_inst*

		=visuafox('ejeproc(.f.)',w_vfox)

		=inkey(2)	&& R.06 Mariano

		if file(w_vfox)
			erase &w_vfox.
		endif
			
*		=inkey(2)	&& R.06 Mariano
		w_paso=1
		do while .t.
			fieldvec[1]='ficx'
			use &d0coem.\intecpra alias ficx exclusive
			
			if neterr()
				=advsuave('Procesando archivo Excel '+justfname(w_fire),w_title)	&& R.06 Mariano (justfname x justpath)
				fieldvec=''
				=inkey(2)	&& R.08 Mariano
			endif

			fieldvec=''
			
			if alias()='ficx' or w_paso>=5
				exit
			endif
			w_paso=w_paso+1
		enddo
	endif
endfor

*/////////// COMPRAS \\\\\\\\\\\\*

if !file(d0coem+'\INTE005c.DBF') 
	sele 0
	use fi\00\ivf005
	copy structure extended to &d0coem.\inte005x.dbf
	use &d0coem.\inte005x alias f05x exclusive	&& Interfase Compras
	sele 0
	use fi\00\ivf006
	copy structure extended to &d0coem.\inte006x.dbf
	use
	sele f05x
	append from  &d0coem.\inte006x.dbf
	use
	create &d0coem.\inte006x.dbf from &d0coem.\inte005x.dbf
	use &d0coem.\inte006x
	
	if version()='Visual FoxPro'
		w_type='type fox2x'
	else
		w_type=''
	endif
	copy to &d0coem.\inte005c.dbf &w_type
	use
	
	dele file &d0coem.\inte005x.dbf
	dele file &d0coem.\inte006x.dbf
	use
endif

sele 0
use &d0coem.\inte005c in 0 alias fic exclusive	&& Interfase Compras
zap

SELE Fic

if used('ficx')	&& R.08 Mariano

GO TOP IN ficx
DO WHILE !EOF('ficx')

	wait wind 'Procesando Interfase Compras...' nowait

	if empty(ctod(ficx.fecha)) or empty(ficx.comprobant) or ficx.comprobant='comprobant'
		skip in ficx
		loop
	endif

	GO BOTTOM
	STORE fic.D5inte to lcd5inte
	lcd5inte = Val(lcd5inte)+1

	append blank

*****	Alicuotas	*****

	GO TOP IN F23
	DO WHILE !EOF('f23')

		if f23.d23alic=ficx.tasa
    	replace fic.d5alic with f23.D23codi
		ENDIF
 
		SKIP IN F23
	ENDDO
	
*****	Comp.Generico	*****

	replace fic.D5fech with CTOD(ficx.fecha),;
			fic.D5empr with d0coem,;
			fic.d5titr with 'N',;
			fic.d5ttip with '0',;
			fic.D5peri with wdperi,;
			fic.D5inte with Padl(alltr(str(lcd5inte,6)), 6,'0'),;
			fic.D5por1 with ficx.Tasa,;
			fic.D5imp1 with ficx.Neto,;
			fic.d5imp2 with ficx.exento,;
			fic.D5imp4 With ficx.Iva+ficx.iva_dif,;
			fic.D5tota with ficx.Total	&& R.03 Mariano (wdperi por subs(ficx.fecha,4,7))

*****   Imp.Interno     *****

	if 	!empty(ficx.impint)
		do case
			&& R.11b Mariano
			case f00.p0prc1=5	&& Imp.Interno
				replace fic.d5imp6 with ficx.impint
			case f00.p0prc2=5	&& Imp.Interno
				replace fic.d5imp7 with ficx.impint
			case f00.p0prc3=5	&& Imp.Interno
				replace fic.d5imp8 with ficx.impint
			case f00.p0prc1=6	&& Sin Asignar
				replace fic.d5imp6 with ficx.impint
			case f00.p0prc2=6	&& Sin Asignar
				replace fic.d5imp7 with ficx.impint
			case f00.p0prc3=6	&& Sin Asignar
				replace fic.d5imp8 with ficx.impint
			&& R.11e Mariano
			case f00.p0prc1!=1 and f00.p0prc1!=2 
				replace fic.d5imp6 with ficx.impint
			case f00.p0prc2!=1 and f00.p0prc2!=2 
				replace fic.d5imp7 with ficx.impint
			case f00.p0prc3!=1 and f00.p0prc3!=2 
				replace fic.d5imp8 with ficx.impint
		endcase
	endif
	
*****	Percepciones	*****

	if !empty(ficx.perciva)
			replace fic.d5imp3 with ficx.perciva
		
			if type('ficx.Tipo_percep_iva')='U'
				replace fic.d5pere with '000' 		&& codigo arbitrario para interfase, deberan modificar el comprobante
			else
				replace fic.d5pere with ficx.Tipo_percep_iva
			endif
	endif

	if !empty(ficx.perciibb)
			do case
				case f00.p0prc1=1
					replace fic.d5imp6 with ficx.perciibb
				case f00.p0prc2=1
					replace fic.d5imp7 with ficx.perciibb	
				case f00.p0prc3=1
					replace fic.d5imp8 with ficx.perciibb
			endcase
		
			if type('ficx.Tipo_percep_ing_b')='U'
				replace fic.d5cpib with '000' 		&& codigo arbitrario para interfase, deberan modificar el comprobante
			else
				replace fic.d5cpib with ficx.Tipo_percep_ing_b
			endif
	endif

	if !empty(ficx.percgana)
			do case
				case f00.p0prc1=2
					replace fic.d5imp6 with ficx.percgana
				case f00.p0prc2=2
					replace fic.d5imp7 with ficx.percgana	
				case f00.p0prc3=2
					replace fic.d5imp8 with ficx.percgana
			endcase
			
			if type('ficx.Tipo_percep_gan')='U'
				replace fic.d5cpga with '000' 		&& codigo arbitrario para interfase, deberan modificar el comprobante
			else
				replace fic.d5cpga with ficx.Tipo_percep_gan
			endif
	endif

	if !empty(ficx.percotro)
			do case
				case f00.p0prc1=3
					replace fic.d5imp6 with fic.d5imp6+ficx.percotro
				case f00.p0prc2=3
					replace fic.d5imp7 with fic.d5imp7+ficx.percotro	
				case f00.p0prc3=3
					replace fic.d5imp8 with fic.d5imp8+ficx.percotro
			endcase
			
	endif
	
			
*****	Facturas A	*****

	DO CASE
		CASE upper(subs(ficx.comprobant,1,3))='FCA'	&& R.08 Mariano (agregue upper() y en mayusculas)
			replace fic.D5tifo with 'A',;
					fic.D5tico with 'A',;
					fic.D5tipo with 'C',;
					fic.D5coco with TCOCO('A', 'A', 'C'),;
					fic.D5sucu with '',;
					fic.D5imfi with 'N',;
					fic.D5nume with subs(ficx.comprobant,5,13),;
					fic.D5cond with '1',;
					fic.D5coim with f00.p0coim,;
					fic.D5tiim with '1',;
		 			fic.D5tiop with '0'
*****	NC A *****		
	
		CASE upper(subs(ficx.comprobant,1,3))='NC '	&& R.08 Mariano (agregue upper() y en mayusculas)
			replace fic.D5tifo with 'A',;
					fic.D5tico with 'C',;
					fic.D5tipo with 'C',;
					fic.D5coco with TCOCO('C', 'A', 'C'),;
					fic.D5sucu with '',;
					fic.D5imfi with 'N',;
					fic.D5nume with subs(ficx.comprobant,5,13),;
					fic.D5cond with '1',;
					fic.D5coim with f00.p0coim,;
					fic.D5tiim with '1',;
		 			fic.D5tiop with '0'

*****	ND A  *****		
	
		CASE upper(subs(ficx.comprobant,1,3))='ND '	&& R.08 Mariano (agregue upper() y en mayusculas)
			replace fic.D5tifo with 'A',;
					fic.D5tico with 'B',;
					fic.D5tipo with 'C',;
					fic.D5coco with TCOCO('B', 'A', 'C'),;
					fic.D5sucu with '',;
					fic.D5imfi with 'N',;
					fic.D5nume with subs(ficx.comprobant,5,13),;
					fic.D5cond with '1',;
					fic.D5coim with f00.p0coim,;
					fic.D5tiim with '1',;
		 			fic.D5tiop with '0'

*****	Ticket Facturas A	*****

		CASE upper(subs(ficx.comprobant,1,3))='TFA'	&& R.08 Mariano (agregue upper() y en mayusculas)
			replace fic.D5tifo with 'A',;
					fic.D5tico with '6',;
					fic.D5tipo with 'C',;
					fic.D5coco with TCOCO('6', 'A', 'C'),;
					fic.D5sucu with '',;
					fic.D5imfi with 'N',;
					fic.D5nume with subs(ficx.comprobant,5,13),;
					fic.D5cond with '1',;
					fic.D5coim with f00.p0coim,;
					fic.D5tiim with '1',;
		 			fic.D5tiop with '0'
*****	Ticket NC A *****		
	
		CASE upper(subs(ficx.comprobant,1,3))='TCA'	&& R.08 Mariano (agregue upper() y en mayusculas)
			replace fic.D5tifo with 'A',;
					fic.D5tico with 'E',;
					fic.D5tipo with 'C',;
					fic.D5coco with TCOCO('E', 'A', 'C'),;
					fic.D5sucu with '',;
					fic.D5imfi with 'N',;
					fic.D5nume with subs(ficx.comprobant,5,13),;
					fic.D5cond with '1',;
					fic.D5coim with f00.p0coim,;
					fic.D5tiim with '1',;
		 			fic.D5tiop with '0'

*****	Ticket ND A  *****		
	
		CASE upper(subs(ficx.comprobant,1,3))='TDA'	&& R.08 Mariano (agregue upper() y en mayusculas)
			replace fic.D5tifo with 'A',;
					fic.D5tico with 'D',;
					fic.D5tipo with 'C',;
					fic.D5coco with TCOCO('D', 'A', 'C'),;
					fic.D5sucu with '',;
					fic.D5imfi with 'N',;
					fic.D5nume with subs(ficx.comprobant,5,13),;
					fic.D5cond with '1',;
					fic.D5coim with f00.p0coim,;
					fic.D5tiim with '1',;
		 			fic.D5tiop with '0'

*****	Facturas C	*****

		CASE upper(subs(ficx.comprobant,1,3))='FcC'	&& R.08 Mariano (agregue upper() y en mayusculas)
			replace fic.D5tifo with 'C',;
					fic.D5tico with 'A',;
					fic.D5tipo with 'C',;
					fic.D5coco with TCOCO('A', 'C', 'C'),;
					fic.D5sucu with '',;
					fic.D5imfi with 'N',;
					fic.D5nume with subs(ficx.comprobant,5,13),;
					fic.D5cond with '1',;
					fic.D5coim with f00.p0coim,;
					fic.D5tiim with '6',;
		 			fic.D5tiop with '0'

*****	NC C *****		
	
		CASE upper(subs(ficx.comprobant,1,3))='NCC'	&& R.08 Mariano (agregue upper() y en mayusculas)
			replace fic.D5tifo with 'C',;
					fic.D5tico with 'C',;
					fic.D5tipo with 'C',;
					fic.D5coco with TCOCO('C', 'C', 'C'),;
					fic.D5sucu with '',;
					fic.D5imfi with 'N',;
					fic.D5nume with subs(ficx.comprobant,5,13),;
					fic.D5cond with '6',;
					fic.D5coim with f00.p0coim,;
					fic.D5tiim with '6',;
		 			fic.D5tiop with '0'

*****	ND C  *****		
	
		CASE upper(subs(ficx.comprobant,1,3))='NDC'	&& R.08 Mariano (agregue upper() y en mayusculas)
			replace fic.D5tifo with 'C',;
					fic.D5tico with 'B',;
					fic.D5tipo with 'C',;
					fic.D5coco with TCOCO('B', 'C', 'C'),;
					fic.D5sucu with '',;
					fic.D5imfi with 'N',;
					fic.D5nume with subs(ficx.comprobant,5,13),;
					fic.D5cond with '1',;
					fic.D5coim with f00.p0coim,;
					fic.D5tiim with '6',;
		 			fic.D5tiop with '0'

*****	Ticket Facturas C	*****

		CASE upper(subs(ficx.comprobant,1,3))='TFC'	&& R.08 Mariano (agregue upper() y en mayusculas)
			replace fic.D5tifo with 'C',;
					fic.D5tico with '6',;
					fic.D5tipo with 'C',;
					fic.D5coco with TCOCO('6', 'C', 'C'),;
					fic.D5sucu with '',;
					fic.D5imfi with 'N',;
					fic.D5nume with subs(ficx.comprobant,5,13),;
					fic.D5cond with '1',;
					fic.D5coim with f00.p0coim,;
					fic.D5tiim with '6',;
		 			fic.D5tiop with '0'

*****	Ticket NC C *****		
	
		CASE upper(subs(ficx.comprobant,1,3))='TCC'	&& R.08 Mariano (agregue upper() y en mayusculas)
			replace fic.D5tifo with 'C',;
					fic.D5tico with 'E',;
					fic.D5tipo with 'C',;
					fic.D5coco with TCOCO('E', 'C', 'C'),;
					fic.D5sucu with '',;
					fic.D5imfi with 'N',;
					fic.D5nume with subs(ficx.comprobant,5,13),;
					fic.D5cond with '1',;
					fic.D5coim with f00.p0coim,;
					fic.D5tiim with '6',;
		 			fic.D5tiop with '0'

*****	Ticket ND C  *****		
	
		CASE upper(subs(ficx.comprobant,1,3))='TDC'	&& R.08 Mariano (agregue upper() y en mayusculas)
			replace fic.D5tifo with 'C',;
					fic.D5tico with 'D',;
					fic.D5tipo with 'C',;
					fic.D5coco with TCOCO('D', 'C', 'C'),;
					fic.D5sucu with '',;
					fic.D5imfi with 'N',;
					fic.D5nume with subs(ficx.comprobant,5,13),;
					fic.D5cond with '1',;
					fic.D5coim with f00.p0coim,;
					fic.D5tiim with '6',;
		 			fic.D5tiop with '0'

*****	Facturas B	*****

		CASE upper(subs(ficx.comprobant,1,3))='FCB'	&& R.08 Mariano (agregue upper() y en mayusculas)
			replace fic.D5tifo with 'B',;
					fic.D5tico with 'A',;
					fic.D5tipo with 'C',;
					fic.D5coco with TCOCO('A', 'B', 'C'),;
					fic.D5sucu with '',;
					fic.D5imfi with 'N',;
					fic.D5nume with subs(ficx.comprobant,5,13),;
					fic.D5cond with '1',;
					fic.D5coim with f00.p0coim,;
					fic.D5tiim with '1',;
		 			fic.D5tiop with '0'

*****	NC B *****		
	
		CASE upper(subs(ficx.comprobant,1,3))='NCB'	&& R.08 Mariano (agregue upper() y en mayusculas)
			replace fic.D5tifo with 'B',;
					fic.D5tico with 'C',;
					fic.D5tipo with 'C',;
					fic.D5coco with TCOCO('C', 'B', 'C'),;
					fic.D5sucu with '',;
					fic.D5imfi with 'N',;
					fic.D5nume with subs(ficx.comprobant,5,13),;
					fic.D5cond with '1',;
					fic.D5coim with f00.p0coim,;
					fic.D5tiim with '1',;
		 			fic.D5tiop with '0'

*****	ND B  *****		
	
		CASE upper(subs(ficx.comprobant,1,3))='NDB'	&& R.08 Mariano (agregue upper() y en mayusculas)
			replace fic.D5tifo with 'B',;
					fic.D5tico with 'B',;
					fic.D5tipo with 'C',;
					fic.D5coco with TCOCO('B', 'B', 'C'),;
					fic.D5sucu with '',;
					fic.D5imfi with 'N',;
					fic.D5nume with subs(ficx.comprobant,5,13),;
					fic.D5cond with '1',;
					fic.D5coim with f00.p0coim,;
					fic.D5tiim with '1',;
		 			fic.D5tiop with '0'

*****	Ticket Facturas B	*****

		CASE upper(subs(ficx.comprobant,1,3))='TFB'	&& R.08 Mariano (agregue upper() y en mayusculas)
			replace fic.D5tifo with 'B',;
					fic.D5tico with '6',;
					fic.D5tipo with 'C',;
					fic.D5coco with TCOCO('6', 'B', 'C'),;
					fic.D5sucu with '',;
					fic.D5imfi with 'N',;
					fic.D5nume with subs(ficx.comprobant,5,13),;
					fic.D5cond with '1',;
					fic.D5coim with f00.p0coim,;
					fic.D5tiim with '1',;
		 			fic.D5tiop with '0'

*****	Ticket NC B *****		
	
		CASE upper(subs(ficx.comprobant,1,3))='TCB'	&& R.08 Mariano (agregue upper() y en mayusculas)
			replace fic.D5tifo with 'B',;
					fic.D5tico with 'E',;
					fic.D5tipo with 'C',;
					fic.D5coco with TCOCO('E', 'B', 'C'),;
					fic.D5sucu with '',;
					fic.D5imfi with 'N',;
					fic.D5nume with subs(ficx.comprobant,5,13),;
					fic.D5cond with '1',;
					fic.D5coim with f00.p0coim,;
					fic.D5tiim with '1',;
		 			fic.D5tiop with '0'

*****	Ticket ND B  *****		
	
		CASE upper(subs(ficx.comprobant,1,3))='TDB'	&& R.08 Mariano (agregue upper() y en mayusculas)
			replace fic.D5tifo with 'B',;
					fic.D5tico with 'D',;
					fic.D5tipo with 'C',;
					fic.D5coco with TCOCO('D', 'B', 'C'),;
					fic.D5sucu with '',;
					fic.D5imfi with 'N',;
					fic.D5nume with subs(ficx.comprobant,5,13),;
					fic.D5cond with '1',;
					fic.D5coim with f00.p0coim,;
					fic.D5tiim with '1',;
		 			fic.D5tiop with '0'

*****	Ticket 	*****

		CASE upper(subs(ficx.comprobant,1,3))='TIC'	&& R.08 Mariano (agregue upper() y en mayusculas)
			replace fic.D5tifo with ' ',;
					fic.D5tico with '6',;
					fic.D5tipo with 'C',;
					fic.D5coco with TCOCO('6', ' ', 'C'),;
					fic.D5sucu with '',;
					fic.D5imfi with 'N',;
					fic.D5nume with subs(ficx.comprobant,5,13),;
					fic.D5cond with '1',;
					fic.D5coim with f00.p0coim,;
					fic.D5tiim with '6',;
		 			fic.D5tiop with '0'
	ENDCASE	

*****	Proveedores	*****

	set order to d3cuit in f03
	if !empty(ficx.Cuit) and seek(ficx.Cuit,'f03')
		replace fic.d5clie with f03.d3prov,;
				fic.d5juri with f03.d3juri
		&& R.02b Mariano
		if fic.d5tiim='6'	&& para el caso de las C deberia ser 4,5 o 6
			replace fic.d5tiim with ftiimp(fic.d5clie, fic.d5peri,'P')
		endif
		&& R.02e Mariano
	else
		replace fic.d6nume with fic.d5nume,;
				fic.d6peri with fic.d5peri,;
				fic.d6empr with d0coem,;
				fic.d6tipo with fic.d5tipo,;
				fic.d6sucu with fic.d5sucu,;
				fic.d6tico with fic.d5tico,;
				fic.d6tifo with fic.d5tifo,;
				fic.d6inte with fic.d5inte,;
				fic.d6nocl with ficx.proveedor,;
				fic.d6cuit with ficx.cuit
		do case
			case upper(ficx.sitiva)='RI'
				replace fic.d5clie with '99991',;
						fic.d6tido with '80'
			case upper(ficx.sitiva)='RNI'
				replace fic.d5clie with '99992',;
						fic.d6tido with '80'
			case upper(ficx.sitiva)='ACF'
				replace fic.d5clie with '99993',;
						fic.d6tido with '96'
			case upper(ficx.sitiva)='EX'
				replace fic.d5clie with '99994',;
						fic.d6tido with '80'
				&& R.02b Mariano
				if fic.d5tiim='6'	&& para el caso de las C deberia ser 4,5,6,7
					replace fic.d5tiim with '4'
				endif
				&& R.02e Mariano
			case upper(ficx.sitiva)='NR'
				replace fic.d5clie with '99995',;
						fic.d6tido with '96'
				&& R.02b Mariano
				if fic.d5tiim='6'	&& para el caso de las C deberia ser 4,5,6,7
					replace fic.d5tiim with '5'
				endif
				&& R.02e Mariano
			case upper(ficx.sitiva)='RM'
				replace fic.d5clie with '99996',;
						fic.d6tido with '80'
			case upper(ficx.sitiva)='NC'
				replace fic.d5clie with '99997',;
						fic.d6tido with '96'
				&& R.02b Mariano
				if fic.d5tiim='6'	&& para el caso de las C deberia ser 4,5,6,7
					replace fic.d5tiim with '7'
				endif
				&& R.02e Mariano
		endcase
		set order to d3prov in f03
		if seek(fic.d5clie,'f03')
			replace fic.d5juri with f03.d3juri
			if empty(fic.d6nocl)
				replace fic.d6nocl with f03.d3nomb,;
						fic.d6cuit with f03.d3cuit,;
						fic.d6dire with f03.d3dire,;
						fic.d6copo with f03.d3copo,;
						fic.d6loca with f03.d3loca,;
						fic.d6tido with f03.d3tido
			endif
		endif
	endif
	set order to d3prov in f03
	
	replace fic.d5leye with readf('f03', fic.d5clie,'d3nomb',7)+' '+tcabre(fic.d5tico)+' '
	replace fic.d5leye with allt(fic.d5leye)+' '+fic.d5tifo+fic.d5sucu+'-'+fic.d5nume
	
*****	Rubro	*****

	if type('ficx.rubro')='U'
		replace fic.d5rubr with '999999' 	&& codigo arbitrario para interfase, debe existir en la empresa
	else
		replace fic.d5rubr with ficx.rubro
	endif
		
	SKIP IN ficx

ENDDO

sele ficx
zap

endif 	&& R.08 Mariano

sele fic
use in fic

RETURN

&& R.04b Mariano
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc brow05in
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
private w_dkey,w_hkey

push menu _msysmenu

go top

if alias()='FIV'
	browse fields ;
		d5tifo :h='F' :v=fenter():f,;
		d5sucu :h='Sucursal' :r,;
		d5nume :h='N๚mero':r,;
		d5fech :h='Fecha':r,;
		d5tota :h='Total':r,;
		clie=d5clie+'-'+iif(d5clie>='99990', d6nocl, iif(seek(d5clie,'f02'), f02.d2nomb, 'No encontrado!!!')) :h='Cliente':r:40,;
		tico=iif(seek('CO'+d5coco,'f01t'), f01t.desc, '') :h='Tipo Cpbte.' :15 :r ;
	nome noap noed node norm in screen ;
	title 'COMPROBANTES DE VENTAS A INCORPORAR EN PERIODO: '+wdperi+space(10)+'<ESC> -> para Continuar' ;
	font 'arial',10 ;
	valid :f salebrow(lastkey()) rest	&& R.05 Mariano (d6nocl y ESC -> para continuar)

*	clie=d5clie+'-'+iif(d5clie>='99990', iif(seek(d5inte,'f06'), f06.d6nocl, 'No encontrado!!!'), iif(seek(d5clie,'f02'), f02.d2nomb, 'No encontrado!!!')) :h='Cliente':r:40	&& R.05 Mariano

else
	browse fields ;
		d5tifo :h='F' :v=fenter():f,;
		d5nume :h='N๚mero':r,;
		d5fech :h='Fecha':r,;
		d5tota :h='Total':r,;
		prov=d5clie+'-'+iif(d5clie>='99990', d6nocl, iif(seek(d5clie,'f03'), f03.d3nomb, 'No encontrado!!!')) :h='Proveedor':r :40,;
		tico=iif(seek('CO'+d5coco,'f01t'), f01t.desc, '') :h='Tipo Cpbte.' :25:r ;
	nome noap noed node norm in screen ;
	title 'COMPROBANTES DE COMPRAS A INCORPORAR EN PERIODO: '+wdperi+space(10)+'<ESC> -> para Continuar';
	font 'arial',10;
	valid :f salebrow(lastkey()) rest	&& R.05 Mariano (d6nocl y ESC -> para continuar)

*	prov=d5clie+'-'+iif(d5clie>='99990', iif(seek(d5inte,'f06'), f06.d6nocl, 'No encontrado!!!'), iif(seek(d5clie,'f03'), f03.d3nomb, 'No encontrado!!!')) :h='Proveedor':r :40	&& R.05 Mariano

endif

pop menu _msysmenu

***vfp***
IF version()='Visual FoxPro'
	if w_call
		rele wind wbrow
	else
		clear read
	endif
endif
***vfp***

return
&& R.04e Mariano

&& Puesto adrede para que incluya el report en el proyecto
repo form iwr005in
