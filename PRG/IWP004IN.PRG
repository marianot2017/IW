*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : IWP004IN.PRG -> Interfase para la incorporacion de comprobantes en linea
*)-H Release 		 : 15
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
w_pfile='04in'
w_title='INCORPORACION DE FACTURAS EN LINEA'	&& R.01 Mariano

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

=creatipos('C') && R.03 Mariano

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

wdici=wdici+wcoem	&& R.03 Mariano

wcoto		= 'N'

wtrim		= '1'
wano		= right(f04->d4peri,4)


wdperi		= f04.d4peri

txtvent		= 'Ventas*.txt'
txtalic		= 'Alicuotas*.txt'

set skip of menu _msysmenu .t.	&& R.05 Mariano

do ssm998.mpr

&& R.11b Mariano
dele file &d0coem.\inte005v.dbf
dele file &d0coem.\intealic.cdx
dele file &d0coem.\intevtal.dbf
dele file &d0coem.\intealic.dbf
&& R.11e Mariano

do iws004in.spr		


fieldvec=''

if !autor() 
	pop key
	pop menu _msysmenu

	=closall()
	=borratipos()	&& R.03 Mariano

	return
endif

*)-----------------------------------------------------------------------------

w_impr='S'

pop key
pop menu _msysmenu
=closall()
=borratipos()	&& R.03 Mariano

dele file &d0coem.\inte005v.dbf
dele file &d0coem.\intealic.cdx
dele file &d0coem.\intevtal.dbf
dele file &d0coem.\intealic.dbf
dele file &d0f99..dbf
dele file &d0f99.e.dbf
return

***vfp***
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons04in
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	on key label f9 do cons04in0
	keyboard '{f9}'
*return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons04in0
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
on key label f9

***vfp***

if wexist('getdest')
	* Impresion del archivo de errores

	do repdest && Setea variables internas del reporte

	go top in f9e
	sele f9e

	if !empty(p_repclaus) &&(Debe ser preview o to printer)
		p_report='iwr004in'
		=rep_form()
		p_report=''
	endif

	do represet && Resetea estado
	
	clear read
	
else

	if !valid04in(.t.)
		return
	endif

	*** INTEVTAL.DBF y INTEALIC.DBF son Dbfs acordes para recibir los datos de txt ***
	
	if !file(d0coem+'\INTEVTAL.DBF')
		sele 0	
		create dbf &d0coem.\intevtal.dbf (fec_compr c(8), tip_compr c(3), pun_venta c(5), num_compr c(20), num_cphast c(20), doc_compra c(2), num_idcomp c(20), nom_compra c(30), imp_totop	c(15), imp_totsng c(15), per_nocate c(15), imp_opexen c(15), imp_perona c(15), imp_perib c(15), imp_permun c(15), imp_int c(15), cod_moneda c(3), tip_cambio c(10), can_aliiva c(1), cod_op c(1), otr_tribut c(15), fec_venpag c(8))	&& R.03 mariano (imp_peronac por imp_perona)
		use		
	endif	
	if !file(d0coem+'\INTEALIC.DBF')
 		sele 0	
		create dbf &d0coem.\intealic.dbf (tip_compr c(3), pun_venta c(5), num_compr c(20), neto_grav c(15), ali_iva c(4), imp_liq c(15))
		use		
	endif	
	
	
	if file(d0coem+'\INTEVTAL.DBF') and file(d0coem+'\INTEALIC.DBF')
		do genera04in	&& genera los inte005v a partir de planillas excel
	endif

	if !file(d0coem+'\INTE005v.DBF') 	&& R.01 Mariano
	   wait 'Falta archivo de interface INTE005v' wind	&& R.01 Mariano
	   return
	endif

	wait clear

	&& R.03b Mariano
	if alias()!='FIV'
		use &d0coem.\inte005v in 0 alias fiv 	&& Interfase Ventas
		sele fiv
		if version()='Visual FoxPro'
			*** cambio el lastkey() ***
			keyboard '{tab}'
			=inkey()
			DO wbrow WITH 'brow04in()'
	    else
   			do brow04in
	    endif
	    use in fiv
	endif
	&& R.03e Mariano
	
	if sino('Confirma incorporaci๓n de comprobantes ?.','Si',w_title)='N'
		return	
	endif

	wait 'Procesando <No interrumpir>...' wind nowait

	dimension wwfiles(1)
	wwfiles=''
*	wwcant=a_dir('wwfiles',wdici+'\'+d0coem+'\VENTAS*.TXT','','',.t.)  	&& R.03 Mariano
	wwcant=a_dir('wwfiles',wdici+'\VENTAS*.TXT','','',.t.)  	&& R.03 Mariano
	wfinu=0
	for wfinu=1 to wwcant
*		w_fire='"'+wdici+'\'+d0coem+'\'+wwfiles[wfinu,1]+'"'	&& R.03 Mariano
		w_fire='"'+wdici+'\'+wwfiles[wfinu,1]+'"'	&& R.03 Mariano
		if version()='Visual FoxPro'
*			w_fipr='"'+wdici+'\'+d0coem+'\PROCESADO_'+wwfiles[wfinu,1]+'"'	&& R.03 Mariano
			w_fipr='"'+wdici+'\PROCESADO_'+wwfiles[wfinu,1]+'"'	&& R.03 Mariano
			dele file &w_fipr.
			rename &w_fire. to &w_fipr.
		else
*			w_fipr='"'+wdici+'\'+d0coem+'\PROCESADO_'+wwfiles[wfinu,1]+'"'	&& R.03 Mariano
			w_fipr='"'+wdici+'\PROCESADO_'+wwfiles[wfinu,1]+'"'	&& R.03 Mariano
			private w_vfox,w_inst001,w_inst002
			w_inst001='dele file '+w_fipr
			w_inst002='rename '+w_fire+' to '+w_fipr
	
			private d0f9ve
			d0f9ve=sys(2003)+'\I'+d0coem+p_netuser+'ve'
			w_vfox=d0f9ve+'.mem'
			save to &w_vfox. all like w_inst*
			release all like w_inst*

			=visuafox('ejeproc(.f.)',w_vfox)

			=inkey(2)	&& R.04 Mariano

			if file(w_vfox)
				erase &w_vfox.
			endif
			
*			=inkey(2)	&& R.04 Mariano
		endif
	endfor
	dimension wwfiles(1)
	wwfiles=''
*	wwcant=a_dir('wwfiles',wdici+'\'+d0coem+'\ALICUOTAS*.TXT','','',.t.)	&& R.03 Mariano
	wwcant=a_dir('wwfiles',wdici+'\ALICUOTAS*.TXT','','',.t.)	&& R.03 Mariano
	wfinu=0
	for wfinu=1 to wwcant
*		w_fire='"'+wdici+'\'+d0coem+'\'+wwfiles[wfinu,1]+'"'	&& R.03 Mariano
		w_fire='"'+wdici+'\'+wwfiles[wfinu,1]+'"'	&& R.03 Mariano
		if version()='Visual FoxPro'
*			w_fipr='"'+wdici+'\'+d0coem+'\PROCESADO_'+wwfiles[wfinu,1]+'"'	&& R.03 Mariano
			w_fipr='"'+wdici+'\PROCESADO_'+wwfiles[wfinu,1]+'"'	&& R.03 Mariano
			dele file &w_fipr.
			rename &w_fire. to &w_fipr.
		else
*			w_fipr='"'+wdici+'\'+d0coem+'\PROCESADO_'+wwfiles[wfinu,1]+'"'	&& R.03 Mariano
			w_fipr='"'+wdici+'\PROCESADO_'+wwfiles[wfinu,1]+'"'	&& R.03 Mariano
			private w_vfox,w_inst001,w_inst002
			w_inst001='dele file '+w_fipr
			w_inst002='rename '+w_fire+' to '+w_fipr

			private d0f9co
			d0f9co=sys(2003)+'\I'+d0coem+p_netuser+'co'
			w_vfox=d0f9co+'.mem'
			save to &w_vfox. all like w_inst*
			release all like w_inst*

			=visuafox('ejeproc(.f.)',w_vfox)

			=inkey(2)	&& R.04 Mariano

			if file(w_vfox)
				erase &w_vfox.
			endif
			
*			=inkey(2)	&& R.04 Mariano
		endif
	endfor

	sele 0
	create table (d0f99);
	(d99mens c(90),d99tipo c(1))	&& R.05 Mariano
	if version()='Visual FoxPro'
		w_type='type fox2x'
	else
		w_type=''
	endif
	copy to &d0f99.e.dbf &w_type
	use 

	fieldvec[ 1]='F9e'
	fieldvec[ 2]='Fiv'

	use &d0f99.e in 0 alias f9e	exclusive	&& Errores de interfase
	use &d0coem.\inte005v in 0 alias fiv 	&& Interfase Ventas

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

	if empty(reccount('fiv'))	&& R.01 Mariano	+reccount('fic'))
		wait clear
		=advsuave('No hay comprobantes para procesar.',w_title)
		clear read
		return
	endif
			
	if !empty(reccount('fiv'))
		sele fiv
		go top
		do while !eof('fiv')
	
			if seek(fiv.d5tipo+fiv.d5sucu+fiv.d5tifo+fiv.d5tico+fiv.d5nume+fiv.d5clie,'f05')
				do error04in with 'Comprobante existente :'+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'
				skip in fiv
				loop
			endif

			if !seek(fiv.d5clie,'f02')
				do error04in with 'Cliente inexistente :'+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'
				skip in fiv
				loop
			endif
							
			&& R.06b Mariano							
			if fiv.d6tido$'80.86.87'
				if dicuit(left(fiv.d6cuit,11))<>right(fiv.d6cuit,1) and !empty(strtran(fiv.d6cuit,'-',''))
					do error04in with 'Error en CUIT o CUIL o CDI :'+fiv.d6cuit+' '+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'		&& R.08 Mariano (figuraba error06in)
					skip in fiv
					loop
				endif
			endif
			&& R.06e Mariano
							
			if !seek(ptoi(fiv.d5peri),'F04')
				do error04in with 'Periodo inexistente :'+fiv.d5peri+' Comp:'+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'
				skip in fiv
				loop
			endif
			
			&& R.03b Taboada
			if left(dtos(fiv.d5fech),6)!=ptoi(fiv.d5peri)
				do error04in with 'La fecha no corresponde al periodo seleccionado :'+dtoc(fiv.d5fech)+' Comp:'+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'
				skip in fiv
				loop
			endif
			&& R.03e Taboada
					
			if seek(ptoi(fiv.d5peri),'f04') and f04.d4esve='C'
				do error04in with 'Periodo cerrado :'+fiv.d5peri+' Comp:'+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'
				skip in fiv
				loop
			endif
		
			if !seek(fiv.d5alic,'f23')
				do error04in with 'Alicuota de iva inexistente :'+fiv.d5alic+' Comp:'+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'
				skip in fiv
				loop
			endif

			if !seek('JU'+fiv.d5juri,'f01')
				do error04in with 'Jurisdicci๓n inexistente :'+fiv.d5juri+' Comp:'+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'
				skip in fiv
				loop
			endif

			if fiv.d5tiim != ftiimp(fiv.d5clie,fiv.d5peri,'C')
				do error04in with 'Tipo impositivo difiere con el cliente :'+fiv.d5clie+' Comp:'+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'
				skip in fiv
				loop
			endif

			
*			if !fiv.d5tifo$iif(fiv.d5tiim$'12','ACEMTZ ',iif(fiv.d5tiim='7','ABCEMTZ ','BCETZ ')) && R.06 Mariano
			if !fiv.d5tifo$iif(fiv.d5tiim$'12','ACEMTZ ',iif(fiv.d5tiim='7','ABCEMTZ ','BCETZ ')) or !fiv.d5tifo$tiform(fiv.d5tico) && R.06 Mariano
				do error04in with 'Formulario incorrecto :'+fiv.d5tifo+' Comp:'+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'
				skip in fiv
				loop
			endif
			

			if !seek('RV'+fiv.d5rubr,'f01')
				do error04in with 'Rubro inexistente :'+fiv.d5rubr+' Comp:'+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'
				skip in fiv
				loop
			endif

			&& R.11b Mariano (Perc o Ret)
			if !empty(fiv.d5pere) or fiv.d5imp3!=0 
				if seek(fiv.d5pere+'I'+iif(fiv.d5tico!='R','P','R'),'f24')
*					if f24.d24codi!='000' or f24.d24tipo!='I' or f24.d24refe!=iif(fiv.d5tico!='R','P','R')	&& R.12 Mariano
					if f24.d24tipo!='I' or f24.d24refe!=iif(fiv.d5tico!='R','P','R')	&& R.12 Mariano
						do error04in with 'Perc./Ret. Iva no corresponde :'+fiv.d5pere+' '+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'
						skip in fiv
					  	loop
					endif 			
				else
					do error04in with 'Perc./Ret. Iva inexistente :'+fiv.d5pere+' '+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'
					skip in fiv
					loop
				Endif
			endif
			
			if !empty(fiv.d5cpib)
				=SEEK("JU" + fiv.d5juri, "f01")
				cafip	= f01.d1tdgi
				do case
					case f00.p0prv1=1 and fiv.d5imp6!=0
						if !seek(cafip+fiv.d5cpib+iif(fiv.d5tico!='R','P','R'),'f25') and fiv.d5cpib!='000'	
							do error04in with 'Perc./Ret. IB inexistente :'+fiv.d5cpib+' '+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'
							skip in fiv
							loop
						endif
					case f00.p0prv2=1 and fiv.d5imp7!=0
						if !seek(cafip+fiv.d5cpib+iif(fiv.d5tico!='R','P','R'),'f25') and fiv.d5cpib!='000'	
							do error04in with 'Perc./Ret. IB inexistente :'+fiv.d5cpib+' '+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'
							skip in fiv
							loop
						endif
					case f00.p0prv3=1 and fiv.d5imp8!=0
						if !seek(cafip+fiv.d5cpib+iif(fiv.d5tico!='R','P','R'),'f25') and fiv.d5cpib!='000'	
							do error04in with 'Perc./Ret. IB inexistente :'+fiv.d5cpib+' '+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'
							skip in fiv
							loop
						endif
					otherwise
						do error04in with 'Falta definir Columna para IB o no hay importe IB :'+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'
						skip in fiv
						loop
				endcase
			else
				do case
					case f00.p0prv1=1 and fiv.d5imp6!=0
						do error04in with 'Falta Perc./Ret. IB :'+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'
						skip in fiv
						loop
					case f00.p0prv2=1 and fiv.d5imp7!=0
						do error04in with 'Falta Perc./Ret. IB :'+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'
						skip in fiv
						loop
					case f00.p0prv3=1 and fiv.d5imp8!=0
						do error04in with 'Falta Perc./Ret. IB :'+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'
						skip in fiv
						loop
				endcase
			endif
			
			if !empty(fiv.d5cpga)
				do case
					case f00.p0prv1=2 and fiv.d5imp6!=0
						if !seek(fiv.d5cpga+'G'+iif(fiv.d5tico!='R','P','R'),'f24') and fiv.d5cpga!='000'	
							do error04in with 'Perc./Ret. Gcias inexistente :'+fiv.d5cpga+' '+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'
							skip in fiv
							loop
						endif
					case f00.p0prv2=2 and fiv.d5imp7!=0
						if !seek(fiv.d5cpga+'G'+iif(fiv.d5tico!='R','P','R'),'f24') and fiv.d5cpga!='000'	
							do error04in with 'Perc./Ret. Gcias inexistente :'+fiv.d5cpga+' '+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'
							skip in fiv
							loop
						endif
					case f00.p0prv3=2 and fiv.d5imp8!=0
						if !seek(fiv.d5cpga+'G'+iif(fiv.d5tico!='R','P','R'),'f24') and fiv.d5cpga!='000'	
							do error04in with 'Perc./Ret. Gcias inexistente :'+fiv.d5cpga+' '+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'
							skip in fiv
							loop
						endif
					otherwise
						do error04in with 'Falta definir Columna para Gcias o no hay importe Gcias :'+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'
						skip in fiv
						loop
				endcase
			else
				do case
					case f00.p0prv1=2 and fiv.d5imp6!=0
						do error04in with 'Falta Perc./Ret. Gcias :'+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'
						skip in fiv
						loop
					case f00.p0prv2=2 and fiv.d5imp7!=0
						do error04in with 'Falta Perc./Ret. Gcias :'+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'
						skip in fiv
						loop
					case f00.p0prv3=2 and fiv.d5imp8!=0
						do error04in with 'Falta Perc./Ret. Gcias :'+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'
						skip in fiv
						loop
				endcase
			endif
			
			if !empty(fiv.d5cpot)
				do case
					case f00.p0prv1=3 and fiv.d5imp6!=0
						if !seek(fiv.d5cpot+'O'+iif(fiv.d5tico!='R','P','R'),'f24') and fiv.d5cpot!='000'	
							do error04in with 'Perc./Ret. Otros inexistente :'+fiv.d5cpot+' '+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'
							skip in fiv
							loop
						endif
					case f00.p0prv2=3 and fiv.d5imp7!=0
						if !seek(fiv.d5cpot+'O'+iif(fiv.d5tico!='R','P','R'),'f24') and fiv.d5cpot!='000'
							do error04in with 'Perc./Ret. Otros inexistente :'+fiv.d5cpot+' '+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'
							skip in fiv
							loop
						endif
					case f00.p0prv3=3 and fiv.d5imp8!=0
						if !seek(fiv.d5cpot+'O'+iif(fiv.d5tico!='R','P','R'),'f24') and fiv.d5cpot!='000'
							do error04in with 'Perc./Ret. Otros inexistente :'+fiv.d5cpot+' '+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'
							skip in fiv
							loop
						endif
					otherwise
						do error04in with 'Falta definir Columna para Otros o no hay importe Otros:'+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'
						skip in fiv
						loop
				endcase
			else
				do case
					case f00.p0prv1=3 and fiv.d5imp6!=0
						do error04in with 'Falta Perc./Ret. Otros :'+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'
						skip in fiv
						loop
					case f00.p0prv2=3 and fiv.d5imp7!=0
						do error04in with 'Falta Perc./Ret. Otros :'+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'
						skip in fiv
						loop
					case f00.p0prv3=3 and fiv.d5imp8!=0
						do error04in with 'Falta Perc./Ret. Otros :'+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'
						skip in fiv
						loop
				endcase
			endif
			&& R.11e Mariano	

			&& R.06b Mariano
			if fiv.d5tifo='C' and (fiv.d5imp4!=0 or fiv.d5imp5!=0)
				do error04in with 'No puede tener IVA el Formulario:'+fiv.d5tifo+' Comp:'+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'	&& R.08 Mariano (figuraba error06in)
				skip in fiv
				loop
			endif
			&& R.06e Mariano

			if fiv.D5imp1+fiv.D5imp2+fiv.D5imp3+fiv.D5imp4+fiv.D5imp5+fiv.D5imp6+fiv.D5imp7+fiv.D5imp8!=fiv.d5tota
				&& R.08b Mariano
				if abs(fiv.D5imp1+fiv.D5imp2+fiv.D5imp3+fiv.D5imp4+fiv.D5imp5+fiv.D5imp6+fiv.D5imp7+fiv.D5imp8-fiv.d5tota)<0.50
					sele fiv
					replace d5imp2 with d5imp2+d5tota-(fiv.D5imp1+fiv.D5imp2+fiv.D5imp3+fiv.D5imp4+fiv.D5imp5+fiv.D5imp6+fiv.D5imp7+fiv.D5imp8)
				endif
				if fiv.D5imp1+fiv.D5imp2+fiv.D5imp3+fiv.D5imp4+fiv.D5imp5+fiv.D5imp6+fiv.D5imp7+fiv.D5imp8!=fiv.d5tota
				&& R.08e Mariano
					do error04in with 'Diferencia entre la suma de los importes y el total'+' Comp:'+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'
					skip in fiv
					loop
				endif	&& R.08 Mariano
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

				&& R.11b Mariano
				if !empty(fiv.d5pere)
					m.d5reiv=iif(fiv.d5tico!='R','P','R')
				else
					m.d5reiv=''
				endif
				if !empty(fiv.d5cpib)
					m.d5regi=iif(fiv.d5tico!='R','P','R')
				else
					m.d5regi=''
				endif
				if !empty(fiv.d5cpga)
					m.d5rega=iif(fiv.d5tico!='R','P','R')
				else
					m.d5rega=''
				endif
				if !empty(fiv.d5cpot)
					m.d5prco=iif(fiv.d5tico!='R','P','R')
				else
					m.d5prco=''
				endif
				&& R.11e Mariano

				gather memvar

				skip in fiv
			enddo
			
			wait wind 'Finalizo la incorporaci๓n.' time 1	&& R.07 Mariano
			wait clear
		
		else

			do while sino('Imprime Errores Ventas', 'SI', w_title)='S'	
				=skipbar('registro','Elegir',.t.)
				do getdest.spr
				=skipbar('registro','Elegir',.f.)
			enddo	

			sele f9e 
			zap

		endif
	clear read
	endif
	wait clear

endif

return


*-P อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
Procedure Error04in
*-P อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
Param w_text,w_tipo
=set_alia('f9e')
=net_appe('f9e')
repl f9e.d99mens with w_text, f9e.d99tipo with w_tipo
=rest_alia()
return
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC valid04in
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS w_show

***vfp***
if version()='Visual Fox' and (w_pfile!='04in' or (!wexist('IWS004IN') and wontop()!='IWS004IN')) and w_show	&& R.03 Mariano (IWS005IN por IWS004IN)
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



return(w_retu)
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc genera04in		&& genera los intexxx a partir de planillas Excel
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

private wwfiles,w_d18fire
dimension wwfiles(1)
wwfiles=''
*wwcant=a_dir('wwfiles',wdici+'\'+d0coem+'\VENTAS*.TXT')	&& R.03 Mariano
*wwcant=a_dir('wwfiles',wdici+'\VENTAS*.TXT')	&& R.03 Mariano	&& R.04 Mariano
wwcant=a_dir('wwfiles',wdici+'\VENTAS*.TXT','','',.t.)  		&& R.04 Mariano
wfinu=0

if used('fivx')
	sele fivx
else

	sele 0
	use &d0coem.\intevtal alias fivx exclusive
endif	
for wfinu=1 to wwcant
	wait wind 'Procesando Interfase Ventas...' nowait
	
*	w_fire='"'+wdici+'\'+d0coem+'\'+wwfiles[wfinu,1]+'"'	&& R.03 Mariano
	w_fire='"'+wdici+'\'+wwfiles[wfinu,1]+'"'	&& R.03 Mariano
	if version()='Visual FoxPro'
		append from &w_fire type sdf
	else
		use in fivx
		private w_vfox,w_inst001,w_inst002,w_inst003
		w_inst001='use '+sys(2003)+'\'+d0coem+'\intevtal.dbf in 0 alias fivx exclusive'
		w_inst002='select fivx'
		w_inst003='append from '+w_fire+' type sdf'

		private d0f9ve
		d0f9ve=sys(2003)+'\I'+d0coem+p_netuser+'ve'
		w_vfox=d0f9ve+'.mem'
		save to &w_vfox. all like w_inst*
		release all like w_inst*

		=visuafox('ejeproc(.f.)',w_vfox)

		=inkey(2)	&& R.04 Mariano

		if file(w_vfox)
			erase &w_vfox.
		endif
			
*		=inkey(2)	&& R.04 Mariano
		w_paso=1
		do while .t.
			fieldvec[1]='fivx'
			use &d0coem.\intevtal alias fivx exclusive
			
			if neterr()
				=advsuave('Procesando archivo TXT '+justfname(w_fire),w_title)	&& R.04 Mariano (justfname x justpath)
				fieldvec=''
				=inkey(2)	&& R.11 Mariano
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
&& R.01b Mariano
dimension wwfiles(1)
wwfiles=''
*wwcant=a_dir('wwfiles',wdici+'\'+d0coem+'\ALICUOTAS*.TXT')	&& R.03 Mariano
*wwcant=a_dir('wwfiles',wdici+'\ALICUOTAS*.TXT')	&& R.03 Mariano	&& R.04 Mariano
wwcant=a_dir('wwfiles',wdici+'\ALICUOTAS*.TXT','','',.t.)  			&& R.04 Mariano
wfinu=0

if used('fiax')
	sele fiax
else

	sele 0
	use &d0coem.\intealic alias fiax exclusive
endif 
for wfinu=1 to wwcant
	wait wind 'Procesando Interfase Alicuotas...' nowait
	
*	w_fire='"'+wdici+'\'+d0coem+'\'+wwfiles[wfinu,1]+'"'	&& R.03 Mariano
	w_fire='"'+wdici+'\'+wwfiles[wfinu,1]+'"'	&& R.03 Mariano
	if version()='Visual FoxPro'
		append from &w_fire type sdf
	else
		use in fiax
		private w_vfox,w_inst001,w_inst002,w_inst003
		w_inst001='use '+sys(2003)+'\'+d0coem+'\intealic.dbf in 0 alias fiax exclusive'
		w_inst002='select fiax'
		w_inst003='append from '+w_fire+' type sdf'

		private d0f9co
		d0f9co=sys(2003)+'\I'+d0coem+p_netuser+'co'
		w_vfox=d0f9co+'.mem'
		save to &w_vfox. all like w_inst*
		release all like w_inst*

		=visuafox('ejeproc(.f.)',w_vfox)

		=inkey(2)	&& R.04 Mariano

		if file(w_vfox)
			erase &w_vfox.
		endif
			
*		=inkey(2)	&& R.04 Mariano
		w_paso=1
		do while .t.
			fieldvec[1]='fiax'
			use &d0coem.\intealic alias fiax exclusive
			
			if neterr()
				=advsuave('Procesando archivo TXT '+justfname(w_fire),w_title)	&& R.04 Mariano (justfname x justpath)
				fieldvec=''
			endif

			fieldvec=''
			
			if alias()='fiax' or w_paso>=5
				exit
			endif
			w_paso=w_paso+1
		enddo
	endif
endfor

*/////////// ALICUOTAS \\\\\\\\\\\\*
&& R.01e Mariano

if !file(d0coem+'\INTE005v.DBF') && R.01 Mariano
	sele 0
	use fi\00\ivf005	&& R.01 Mariano
	copy structure extended to &d0coem.\inte005x.dbf	&& R.01 Mariano
	use &d0coem.\inte005x alias f05x exclusive	&& Interfase Ventas	&& R.01 Mariano
	sele 0
	use fi\00\ivf006
	copy structure extended to &d0coem.\inte006x.dbf
	use
	sele f05x
	append from  &d0coem.\inte006x.dbf
	use
	create &d0coem.\inte006x.dbf from &d0coem.\inte005x.dbf	&& R.01 Mariano
	use &d0coem.\inte006x
	
	if version()='Visual FoxPro'
		w_type='type fox2x'
	else
		w_type=''
	endif
	copy to &d0coem.\inte005v.dbf &w_type	&& R.01 Mariano
	use
	
	dele file &d0coem.\inte005x.dbf	&& R.01 Mariano
	dele file &d0coem.\inte006x.dbf
endif

&& R.07b Mariano
*sele 0
*use &d0coem.\inte005v in 0 alias fiv exclusive	&& Interfase Ventas	&& R.01 Mariano
if used('fiv')
	sele fiv
	use &d0coem.\inte005v alias fiv exclusive	&& Interfase Ventas
else
	sele 0
	use &d0coem.\inte005v alias fiv exclusive	&& Interfase Ventas
endif
&& R.07e Mariano
zap

SELE fiv

if used('fivx')	&& R.11 Mariano

GO TOP IN fivx
DO WHILE !EOF('fivx')
	
	&& R.02b Taboada
	wait wind 'Procesando Interfase Ventas...' nowait
	
	GO BOTTOM
	STORE fiv.D5inte to ld5inte
	ld5inte = Val(ld5inte)+1
    
	append blank

*****	Comp.Generico	*****

	replace fiv.D5fech with STOD(fivx.fec_compr),;
			fiv.D5empr with d0coem,;
			fiv.d5titr with '',;
			fiv.d5ttip with '0',;
			fiv.D5peri with wdperi,;
			fiv.D5inte with Padl(alltr(str(ld5inte,6)), 6,'0'),;
			fiv.D5sucu with right(fivx.pun_venta,len(fivx.pun_venta))	&& R.14 Mariano (subs(fivx.pun_venta,2,4))
			
&& R.07b Mariano
*	replace fiv.D5imp2 with (val(fivx.imp_opexen)*iif(fiv.D5tico='C',-1,1)/100)+(val(fivx.imp_totsng)*iif(fiv.D5tico='C',-1,1)/100),;  
			fiv.D5tota with val(fivx.imp_totop)*iif(fiv.D5tico='C',-1,1)/100	&& R.07 Mariano (*iif(fiv.D5tico='C',-1,1))	
&& R.07e Mariano

&& R.09b Mariano
		
*****   Imp.Interno     *****

*	if 	!empty(fivx.imp_int)
*		do case
*			case f00.p0prv1!=1 and f00.p0prv1!=2 
*				replace fiv.d5imp6 with fiv.d5imp6+val(fivx.imp_int)*iif(fiv.D5tico='C',-1,1)/100	&& R.07 Mariano (*iif(fiv.D5tico='C',-1,1))
*			case f00.p0prv2!=1 and f00.p0prv2!=2 
*				replace fiv.d5imp7 with fiv.d5imp7+val(fivx.imp_int)*iif(fiv.D5tico='C',-1,1)/100	&& R.07 Mariano (*iif(fiv.D5tico='C',-1,1))
*			case f00.p0prv3!=1 and f00.p0prv3!=2 
*				replace fiv.d5imp8 with fiv.d5imp8+val(fivx.imp_int)*iif(fiv.D5tico='C',-1,1)/100	&& R.07 Mariano (*iif(fiv.D5tico='C',-1,1))
*		endcase
*	endif
	
	&& R.07b Mariano
	
*****   Percep. no categorizado     *****

*	if 	!empty(fivx.per_nocate)
*		do case
*			case f00.p0prv1!=1 and f00.p0prv1!=2 
*				replace fiv.d5imp6 with fiv.d5imp6+val(fivx.per_nocate)*iif(fiv.D5tico='C',-1,1)/100	&& R.07 Mariano (*iif(fiv.D5tico='C',-1,1))
*			case f00.p0prv2!=1 and f00.p0prv2!=2 
*				replace fiv.d5imp7 with fiv.d5imp7+val(fivx.per_nocate)*iif(fiv.D5tico='C',-1,1)/100	&& R.07 Mariano (*iif(fiv.D5tico='C',-1,1))
*			case f00.p0prv3!=1 and f00.p0prv3!=2 
*				replace fiv.d5imp8 with fiv.d5imp8+val(fivx.per_nocate)*iif(fiv.D5tico='C',-1,1)/100	&& R.07 Mariano (*iif(fiv.D5tico='C',-1,1))
*		endcase
*	endif

*****   Percep. municipal     *****

*	if 	!empty(fivx.imp_permun)
*		do case
*			case f00.p0prv1!=1 and f00.p0prv1!=2 
*				replace fiv.d5imp6 with fiv.d5imp6+val(fivx.imp_permun)*iif(fiv.D5tico='C',-1,1)/100	&& R.07 Mariano (*iif(fiv.D5tico='C',-1,1))
*			case f00.p0prv2!=1 and f00.p0prv2!=2 
*				replace fiv.d5imp7 with fiv.d5imp7+val(fivx.imp_permun)*iif(fiv.D5tico='C',-1,1)/100	&& R.07 Mariano (*iif(fiv.D5tico='C',-1,1))
*			case f00.p0prv3!=1 and f00.p0prv3!=2 
*				replace fiv.d5imp8 with fiv.d5imp8+val(fivx.imp_permun)*iif(fiv.D5tico='C',-1,1)/100	&& R.07 Mariano (*iif(fiv.D5tico='C',-1,1))
*		endcase
*	endif

*****   Percep. Otros Tributos     *****

*	if 	!empty(fivx.otr_tribut)
*		do case
*			case f00.p0prv1!=1 and f00.p0prv1!=2 
*				replace fiv.d5imp6 with fiv.d5imp6+val(fivx.otr_tribut)*iif(fiv.D5tico='C',-1,1)/100	&& R.07 Mariano (*iif(fiv.D5tico='C',-1,1))
*			case f00.p0prv2!=1 and f00.p0prv2!=2 
*				replace fiv.d5imp7 with fiv.d5imp7+val(fivx.otr_tribut)*iif(fiv.D5tico='C',-1,1)/100	&& R.07 Mariano (*iif(fiv.D5tico='C',-1,1))
*			case f00.p0prv3!=1 and f00.p0prv3!=2 
*				replace fiv.d5imp8 with fiv.d5imp8+val(fivx.otr_tribut)*iif(fiv.D5tico='C',-1,1)/100	&& R.07 Mariano (*iif(fiv.D5tico='C',-1,1))
*		endcase
*	endif
	&& R.07e Mariano
	
&& R.09e Mariano
	
	DO CASE	
*****	Facturas A	*****

		CASE fivx.tip_compr='001'
			replace fiv.D5tifo with 'A',;
					fiv.D5tico with 'A',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('A', 'A'),;
					fiv.D5imfi with 'N',;
					fiv.D5nume with right(fivx.num_compr,8)+'/'+iif(fivx.num_compr<fivx.num_cphast, right(fivx.num_cphast,8), ''),;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '1',;
		 			fiv.D5tiop with fivx.cod_op    && fiv.D5nume with fivx.num_compr+'/',;  R.02 Taboada

*****	NOTA DE DEBITO A	*****

		CASE fivx.tip_compr='002'
			replace fiv.D5tifo with 'A',;
					fiv.D5tico with 'B',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('B', 'A'),;
					fiv.D5imfi with 'N',;
					fiv.D5nume with right(fivx.num_compr,8)+'/'+iif(fivx.num_compr<fivx.num_cphast, right(fivx.num_cphast,8), ''),;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '1',;
		 			fiv.D5tiop with fivx.cod_op		&& fiv.D5nume with fivx.num_compr+'/',;  R.02 Taboada

*****	NOTA DE CREDITO A	*****

		CASE fivx.tip_compr='003'
			replace fiv.D5tifo with 'A',;
					fiv.D5tico with 'C',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('C', 'A'),;
					fiv.D5imfi with 'N',;
					fiv.D5nume with right(fivx.num_compr,8)+'/'+iif(fivx.num_compr<fivx.num_cphast, right(fivx.num_cphast,8), ''),;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '1',;
		 			fiv.D5tiop with fivx.cod_op		&& fiv.D5nume with fivx.num_compr+'/',;  R.02 Taboada

&& R.08b Mariano
*****	Recibos Facturas A	*****

		CASE fivx.tip_compr='004'
			replace fiv.D5tifo with 'A',;
					fiv.D5tico with '8',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('8', 'A'),;
					fiv.D5imfi with 'N',;
					fiv.D5nume with right(fivx.num_compr,8)+'/'+iif(fivx.num_compr<fivx.num_cphast, right(fivx.num_cphast,8), ''),;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '1',;
		 			fiv.D5tiop with fivx.cod_op  
&& R.08e Mariano

*****	Facturas B	*****

		CASE fivx.tip_compr='006'
			replace fiv.D5tifo with 'B',;
					fiv.D5tico with 'A',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('A', 'B'),;
					fiv.D5imfi with 'N',;
					fiv.D5nume with right(fivx.num_compr,8)+'/'+iif(fivx.num_compr<fivx.num_cphast, right(fivx.num_cphast,8), ''),;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '3',;
		 			fiv.D5tiop with fivx.cod_op		&& fiv.D5nume with fivx.num_compr+'/',;  R.02 Taboada

*****	NOTA DE DEBITO B	*****

		CASE fivx.tip_compr='007'
			replace fiv.D5tifo with 'B',;
					fiv.D5tico with 'B',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('B', 'B'),;
					fiv.D5imfi with 'N',;
					fiv.D5nume with right(fivx.num_compr,8)+'/'+iif(fivx.num_compr<fivx.num_cphast, right(fivx.num_cphast,8), ''),;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '3',;
		 			fiv.D5tiop with fivx.cod_op		&& fiv.D5nume with fivx.num_compr+'/',;  R.02 Taboada
		 			
		 			
*****	NOTA DE CREDITO B	*****

		CASE fivx.tip_compr='008'
			replace fiv.D5tifo with 'B',;
					fiv.D5tico with 'C',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('C', 'B'),;
					fiv.D5imfi with 'N',;
					fiv.D5nume with right(fivx.num_compr,8)+'/'+iif(fivx.num_compr<fivx.num_cphast, right(fivx.num_cphast,8), ''),;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '3',;
		 			fiv.D5tiop with fivx.cod_op		&& fiv.D5nume with fivx.num_compr+'/',;  R.02 Taboada

&& R.08b Mariano
*****	Recibos Facturas B	*****

		CASE fivx.tip_compr='009'
			replace fiv.D5tifo with 'B',;
					fiv.D5tico with '8',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('8', 'B'),;
					fiv.D5imfi with 'N',;
					fiv.D5nume with right(fivx.num_compr,8)+'/'+iif(fivx.num_compr<fivx.num_cphast, right(fivx.num_cphast,8), ''),;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '3',;
		 			fiv.D5tiop with fivx.cod_op  
&& R.08e Mariano

		 			
*****	Facturas C	*****

		CASE fivx.tip_compr='011'
			replace fiv.D5tifo with 'C',;
					fiv.D5tico with 'A',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('A', 'C'),;
					fiv.D5imfi with 'N',;
					fiv.D5nume with right(fivx.num_compr,8)+'/'+iif(fivx.num_compr<fivx.num_cphast, right(fivx.num_cphast,8), ''),;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '3',;
		 			fiv.D5tiop with fivx.cod_op    && fiv.D5nume with fivx.num_compr+'/',;  R.02 Taboada && R.06 Mariano (d5tiim -> 1 x 3)
		 			
		 			
*****	NOTA DE DEBITO C	*****

		CASE fivx.tip_compr='012'
			replace fiv.D5tifo with 'C',;
					fiv.D5tico with 'B',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('B', 'C'),;
					fiv.D5imfi with 'N',;
					fiv.D5nume with right(fivx.num_compr,8)+'/'+iif(fivx.num_compr<fivx.num_cphast, right(fivx.num_cphast,8), ''),;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '3',;
		 			fiv.D5tiop with fivx.cod_op		&& fiv.D5nume with fivx.num_compr+'/',;  R.02 Taboada

*****	NOTA DE CREDITO C	*****

		CASE fivx.tip_compr='013'
			replace fiv.D5tifo with 'C',;
					fiv.D5tico with 'C',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('C', 'C'),;
					fiv.D5imfi with 'N',;
					fiv.D5nume with right(fivx.num_compr,8)+'/'+iif(fivx.num_compr<fivx.num_cphast, right(fivx.num_cphast,8), ''),;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '3',;
		 			fiv.D5tiop with fivx.cod_op		&& fiv.D5nume with fivx.num_compr+'/',;  R.02 Taboada
		 			
&& R.08b Mariano
*****	Recibos Facturas C	*****

		CASE fivx.tip_compr='015'
			replace fiv.D5tifo with 'C',;
					fiv.D5tico with '8',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('8', 'C'),;
					fiv.D5imfi with 'N',;
					fiv.D5nume with right(fivx.num_compr,8)+'/'+iif(fivx.num_compr<fivx.num_cphast, right(fivx.num_cphast,8), ''),;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '3',;
		 			fiv.D5tiop with fivx.cod_op  

*****	Facturas M	*****

		CASE fivx.tip_compr='051'
			replace fiv.D5tifo with 'M',;
					fiv.D5tico with 'A',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('A', 'M'),;
					fiv.D5imfi with 'N',;
					fiv.D5nume with right(fivx.num_compr,8)+'/'+iif(fivx.num_compr<fivx.num_cphast, right(fivx.num_cphast,8), ''),;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '1',;
		 			fiv.D5tiop with fivx.cod_op    && fiv.D5nume with fivx.num_compr+'/',;  R.02 Taboada

*****	NOTA DE DEBITO M	*****

		CASE fivx.tip_compr='052'
			replace fiv.D5tifo with 'M',;
					fiv.D5tico with 'B',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('B', 'M'),;
					fiv.D5imfi with 'N',;
					fiv.D5nume with right(fivx.num_compr,8)+'/'+iif(fivx.num_compr<fivx.num_cphast, right(fivx.num_cphast,8), ''),;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '1',;
		 			fiv.D5tiop with fivx.cod_op		&& fiv.D5nume with fivx.num_compr+'/',;  R.02 Taboada

*****	NOTA DE CREDITO M	*****

		CASE fivx.tip_compr='053'
			replace fiv.D5tifo with 'M',;
					fiv.D5tico with 'C',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('C', 'M'),;
					fiv.D5imfi with 'N',;
					fiv.D5nume with right(fivx.num_compr,8)+'/'+iif(fivx.num_compr<fivx.num_cphast, right(fivx.num_cphast,8), ''),;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '1',;
		 			fiv.D5tiop with fivx.cod_op		&& fiv.D5nume with fivx.num_compr+'/',;  R.02 Taboada

*****	Recibos Facturas M	*****

		CASE fivx.tip_compr='054'
			replace fiv.D5tifo with 'M',;
					fiv.D5tico with '8',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('8', 'M'),;
					fiv.D5imfi with 'N',;
					fiv.D5nume with right(fivx.num_compr,8)+'/'+iif(fivx.num_compr<fivx.num_cphast, right(fivx.num_cphast,8), ''),;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '1',;
		 			fiv.D5tiop with fivx.cod_op  

*****	Ticket-Facturas A	*****

		CASE fivx.tip_compr='081'
			replace fiv.D5tifo with 'A',;
					fiv.D5tico with '6',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('6', 'A'),;
					fiv.D5imfi with 'N',;
					fiv.D5nume with right(fivx.num_compr,8)+'/'+iif(fivx.num_compr<fivx.num_cphast, right(fivx.num_cphast,8), ''),;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '1',;
		 			fiv.D5tiop with fivx.cod_op    && fiv.D5nume with fivx.num_compr+'/',;  R.02 Taboada

*****	Ticket-NOTA DE DEBITO A	*****

		CASE fivx.tip_compr='115'
			replace fiv.D5tifo with 'A',;
					fiv.D5tico with 'D',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('D', 'A'),;
					fiv.D5imfi with 'N',;
					fiv.D5nume with right(fivx.num_compr,8)+'/'+iif(fivx.num_compr<fivx.num_cphast, right(fivx.num_cphast,8), ''),;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '1',;
		 			fiv.D5tiop with fivx.cod_op		&& fiv.D5nume with fivx.num_compr+'/',;  R.02 Taboada

*****	Ticket-NOTA DE CREDITO A	*****

		CASE fivx.tip_compr='112'
			replace fiv.D5tifo with 'A',;
					fiv.D5tico with 'E',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('E', 'A'),;
					fiv.D5imfi with 'N',;
					fiv.D5nume with right(fivx.num_compr,8)+'/'+iif(fivx.num_compr<fivx.num_cphast, right(fivx.num_cphast,8), ''),;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '1',;
		 			fiv.D5tiop with fivx.cod_op		&& fiv.D5nume with fivx.num_compr+'/',;  R.02 Taboada

*****	Ticket-Facturas B	*****

		CASE fivx.tip_compr='082'
			replace fiv.D5tifo with 'B',;
					fiv.D5tico with '6',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('6', 'B'),;
					fiv.D5imfi with 'N',;
					fiv.D5nume with right(fivx.num_compr,8)+'/'+iif(fivx.num_compr<fivx.num_cphast, right(fivx.num_cphast,8), ''),;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '3',;
		 			fiv.D5tiop with fivx.cod_op    && fiv.D5nume with fivx.num_compr+'/',;  R.02 Taboada

*****	Ticket-NOTA DE DEBITO B	*****

		CASE fivx.tip_compr='116'
			replace fiv.D5tifo with 'B',;
					fiv.D5tico with 'D',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('D', 'B'),;
					fiv.D5imfi with 'N',;
					fiv.D5nume with right(fivx.num_compr,8)+'/'+iif(fivx.num_compr<fivx.num_cphast, right(fivx.num_cphast,8), ''),;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '3',;
		 			fiv.D5tiop with fivx.cod_op		&& fiv.D5nume with fivx.num_compr+'/',;  R.02 Taboada

*****	Ticket-NOTA DE CREDITO B	*****

		CASE fivx.tip_compr='113'
			replace fiv.D5tifo with 'B',;
					fiv.D5tico with 'E',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('E', 'B'),;
					fiv.D5imfi with 'N',;
					fiv.D5nume with right(fivx.num_compr,8)+'/'+iif(fivx.num_compr<fivx.num_cphast, right(fivx.num_cphast,8), ''),;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '3',;
		 			fiv.D5tiop with fivx.cod_op		&& fiv.D5nume with fivx.num_compr+'/',;  R.02 Taboada

*****	Ticket-Facturas M	*****

		CASE fivx.tip_compr='118'
			replace fiv.D5tifo with 'M',;
					fiv.D5tico with '6',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('6', 'M'),;
					fiv.D5imfi with 'N',;
					fiv.D5nume with right(fivx.num_compr,8)+'/'+iif(fivx.num_compr<fivx.num_cphast, right(fivx.num_cphast,8), ''),;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '1',;
		 			fiv.D5tiop with fivx.cod_op    && fiv.D5nume with fivx.num_compr+'/',;  R.02 Taboada

*****	Ticket-NOTA DE DEBITO M	*****

		CASE fivx.tip_compr='120'
			replace fiv.D5tifo with 'M',;
					fiv.D5tico with 'D',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('D', 'M'),;
					fiv.D5imfi with 'N',;
					fiv.D5nume with right(fivx.num_compr,8)+'/'+iif(fivx.num_compr<fivx.num_cphast, right(fivx.num_cphast,8), ''),;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '1',;
		 			fiv.D5tiop with fivx.cod_op		&& fiv.D5nume with fivx.num_compr+'/',;  R.02 Taboada

*****	Ticket-NOTA DE CREDITO M	*****

		CASE fivx.tip_compr='119'
			replace fiv.D5tifo with 'M',;
					fiv.D5tico with 'E',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('E', 'M'),;
					fiv.D5imfi with 'N',;
					fiv.D5nume with right(fivx.num_compr,8)+'/'+iif(fivx.num_compr<fivx.num_cphast, right(fivx.num_cphast,8), ''),;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '1',;
		 			fiv.D5tiop with fivx.cod_op		&& fiv.D5nume with fivx.num_compr+'/',;  R.02 Taboada
&& R.08e Mariano

		 			
	ENDCASE

&& R.07b Mariano
	replace fiv.D5imp2 with (val(fivx.imp_opexen)*iif(fiv.d5tico$'C.E',-1,1)/100)+(val(fivx.imp_totsng)*iif(fiv.d5tico$'C.E',-1,1)/100),;
			fiv.D5tota with val(fivx.imp_totop)*iif(fiv.d5tico$'C.E',-1,1)/100	&& R.07 Mariano (*iif(fiv.D5tico='C',-1,1))	&& R.10 Mariano (faltaba la opcion E-ticket-NC)
		
*****   Imp.Interno     *****

	if 	!empty(fivx.imp_int)
		do case
			&& R.14b Mariano
			case f00.p0prv1=5 
				replace fiv.d5imp6 with fiv.d5imp6+val(fivx.imp_int)*iif(fiv.d5tico$'C.E',-1,1)/100	&& R.07 Mariano (*iif(fiv.D5tico='C',-1,1))
			case f00.p0prv2=5 
				replace fiv.d5imp7 with fiv.d5imp7+val(fivx.imp_int)*iif(fiv.d5tico$'C.E',-1,1)/100	&& R.07 Mariano (*iif(fiv.D5tico='C',-1,1))
			case f00.p0prv3=5 
				replace fiv.d5imp8 with fiv.d5imp8+val(fivx.imp_int)*iif(fiv.d5tico$'C.E',-1,1)/100	&& R.07 Mariano (*iif(fiv.D5tico='C',-1,1))
			&& R.14e Mariano
			&& R.15b Mariano
			case f00.p0prv1=6
				replace fiv.d5imp6 with fiv.d5imp6+val(fivx.imp_int)*iif(fiv.d5tico$'C.E',-1,1)/100	&& R.07 Mariano (*iif(fiv.D5tico='C',-1,1))
			case f00.p0prv2=6 
				replace fiv.d5imp7 with fiv.d5imp7+val(fivx.imp_int)*iif(fiv.d5tico$'C.E',-1,1)/100	&& R.07 Mariano (*iif(fiv.D5tico='C',-1,1))
			case f00.p0prv3=6 
				replace fiv.d5imp8 with fiv.d5imp8+val(fivx.imp_int)*iif(fiv.d5tico$'C.E',-1,1)/100	&& R.07 Mariano (*iif(fiv.D5tico='C',-1,1))
			&& R.15e Mariano
			case f00.p0prv1!=1 and f00.p0prv1!=2 
				replace fiv.d5imp6 with fiv.d5imp6+val(fivx.imp_int)*iif(fiv.d5tico$'C.E',-1,1)/100	&& R.07 Mariano (*iif(fiv.D5tico='C',-1,1))	&& R.10 Mariano (faltaba la opcion E-ticket-NC)
			case f00.p0prv2!=1 and f00.p0prv2!=2 
				replace fiv.d5imp7 with fiv.d5imp7+val(fivx.imp_int)*iif(fiv.d5tico$'C.E',-1,1)/100	&& R.07 Mariano (*iif(fiv.D5tico='C',-1,1))	&& R.10 Mariano (faltaba la opcion E-ticket-NC)
			case f00.p0prv3!=1 and f00.p0prv3!=2 
				replace fiv.d5imp8 with fiv.d5imp8+val(fivx.imp_int)*iif(fiv.d5tico$'C.E',-1,1)/100	&& R.07 Mariano (*iif(fiv.D5tico='C',-1,1))	&& R.10 Mariano (faltaba la opcion E-ticket-NC)
		endcase
	endif
&& R.07e Mariano
	
&& R.07b Mariano
	
*****   Percep. no categorizado     *****

	if 	!empty(fivx.per_nocate)
		do case
			case f00.p0prv1!=1 and f00.p0prv1!=2 
				replace fiv.d5imp6 with fiv.d5imp6+val(fivx.per_nocate)*iif(fiv.d5tico$'C.E',-1,1)/100	&& R.07 Mariano (*iif(fiv.D5tico='C',-1,1))	&& R.10 Mariano (faltaba la opcion E-ticket-NC)
			case f00.p0prv2!=1 and f00.p0prv2!=2 
				replace fiv.d5imp7 with fiv.d5imp7+val(fivx.per_nocate)*iif(fiv.d5tico$'C.E',-1,1)/100	&& R.07 Mariano (*iif(fiv.D5tico='C',-1,1))	&& R.10 Mariano (faltaba la opcion E-ticket-NC)
			case f00.p0prv3!=1 and f00.p0prv3!=2 
				replace fiv.d5imp8 with fiv.d5imp8+val(fivx.per_nocate)*iif(fiv.d5tico$'C.E',-1,1)/100	&& R.07 Mariano (*iif(fiv.D5tico='C',-1,1))	&& R.10 Mariano (faltaba la opcion E-ticket-NC)
		endcase
	endif

*****   Percep. municipal     *****

	if 	!empty(fivx.imp_permun)
		do case
			&& R.14b Mariano
			case f00.p0prv1=4
				replace fiv.d5imp6 with fiv.d5imp6+val(fivx.imp_permun)*iif(fiv.d5tico$'C.E',-1,1)/100	&& R.07 Mariano (*iif(fiv.D5tico='C',-1,1))
			case f00.p0prv2=4 
				replace fiv.d5imp7 with fiv.d5imp7+val(fivx.imp_permun)*iif(fiv.d5tico$'C.E',-1,1)/100	&& R.07 Mariano (*iif(fiv.D5tico='C',-1,1))
			case f00.p0prv3=4 
				replace fiv.d5imp8 with fiv.d5imp8+val(fivx.imp_permun)*iif(fiv.d5tico$'C.E',-1,1)/100	&& R.07 Mariano (*iif(fiv.D5tico='C',-1,1))
			&& R.14e Mariano
			&& R.15b Mariano
			case f00.p0prv1=6
				replace fiv.d5imp6 with fiv.d5imp6+val(fivx.imp_permun)*iif(fiv.d5tico$'C.E',-1,1)/100	&& R.07 Mariano (*iif(fiv.D5tico='C',-1,1))
			case f00.p0prv2=6 
				replace fiv.d5imp7 with fiv.d5imp7+val(fivx.imp_permun)*iif(fiv.d5tico$'C.E',-1,1)/100	&& R.07 Mariano (*iif(fiv.D5tico='C',-1,1))
			case f00.p0prv3=6 
				replace fiv.d5imp8 with fiv.d5imp8+val(fivx.imp_permun)*iif(fiv.d5tico$'C.E',-1,1)/100	&& R.07 Mariano (*iif(fiv.D5tico='C',-1,1))
			&& R.15e Mariano
			case f00.p0prv1!=1 and f00.p0prv1!=2 
				replace fiv.d5imp6 with fiv.d5imp6+val(fivx.imp_permun)*iif(fiv.d5tico$'C.E',-1,1)/100	&& R.07 Mariano (*iif(fiv.D5tico='C',-1,1))	&& R.10 Mariano (faltaba la opcion E-ticket-NC)
			case f00.p0prv2!=1 and f00.p0prv2!=2 
				replace fiv.d5imp7 with fiv.d5imp7+val(fivx.imp_permun)*iif(fiv.d5tico$'C.E',-1,1)/100	&& R.07 Mariano (*iif(fiv.D5tico='C',-1,1))	&& R.10 Mariano (faltaba la opcion E-ticket-NC)
			case f00.p0prv3!=1 and f00.p0prv3!=2 
				replace fiv.d5imp8 with fiv.d5imp8+val(fivx.imp_permun)*iif(fiv.d5tico$'C.E',-1,1)/100	&& R.07 Mariano (*iif(fiv.D5tico='C',-1,1))	&& R.10 Mariano (faltaba la opcion E-ticket-NC)
		endcase
	endif

*****   Percep. Otros Tributos     *****

	if 	!empty(fivx.otr_tribut)
		do case
			&& R.14b Mariano
			case f00.p0prv1=3 
				replace fiv.d5imp6 with fiv.d5imp6+val(fivx.otr_tribut)*iif(fiv.d5tico$'C.E',-1,1)/100	&& R.07 Mariano (*iif(fiv.D5tico='C',-1,1))
			case f00.p0prv2=3 
				replace fiv.d5imp7 with fiv.d5imp7+val(fivx.otr_tribut)*iif(fiv.d5tico$'C.E',-1,1)/100	&& R.07 Mariano (*iif(fiv.D5tico='C',-1,1))
			case f00.p0prv3=3 
				replace fiv.d5imp8 with fiv.d5imp8+val(fivx.otr_tribut)*iif(fiv.d5tico$'C.E',-1,1)/100	&& R.07 Mariano (*iif(fiv.D5tico='C',-1,1))
			&& R.14e Mariano
			&& R.15b Mariano
			case f00.p0prv1=6 
				replace fiv.d5imp6 with fiv.d5imp6+val(fivx.otr_tribut)*iif(fiv.d5tico$'C.E',-1,1)/100	&& R.07 Mariano (*iif(fiv.D5tico='C',-1,1))
			case f00.p0prv2=6 
				replace fiv.d5imp7 with fiv.d5imp7+val(fivx.otr_tribut)*iif(fiv.d5tico$'C.E',-1,1)/100	&& R.07 Mariano (*iif(fiv.D5tico='C',-1,1))
			case f00.p0prv3=6 
				replace fiv.d5imp8 with fiv.d5imp8+val(fivx.otr_tribut)*iif(fiv.d5tico$'C.E',-1,1)/100	&& R.07 Mariano (*iif(fiv.D5tico='C',-1,1))
			&& R.15e Mariano
			case f00.p0prv1!=1 and f00.p0prv1!=2 
				replace fiv.d5imp6 with fiv.d5imp6+val(fivx.otr_tribut)*iif(fiv.d5tico$'C.E',-1,1)/100	&& R.07 Mariano (*iif(fiv.D5tico='C',-1,1))	&& R.10 Mariano (faltaba la opcion E-ticket-NC)
			case f00.p0prv2!=1 and f00.p0prv2!=2 
				replace fiv.d5imp7 with fiv.d5imp7+val(fivx.otr_tribut)*iif(fiv.d5tico$'C.E',-1,1)/100	&& R.07 Mariano (*iif(fiv.D5tico='C',-1,1))	&& R.10 Mariano (faltaba la opcion E-ticket-NC)
			case f00.p0prv3!=1 and f00.p0prv3!=2 
				replace fiv.d5imp8 with fiv.d5imp8+val(fivx.otr_tribut)*iif(fiv.d5tico$'C.E',-1,1)/100	&& R.07 Mariano (*iif(fiv.D5tico='C',-1,1))	&& R.10 Mariano (faltaba la opcion E-ticket-NC)
		endcase
	endif
&& R.07e Mariano
	
	*****	Percepciones	*****

	if !empty(val(fivx.imp_perona))		&& R.03 mariano (imp_peronac por imp_perona)
			replace fiv.d5imp3 with val(fivx.imp_perona)*iif(fiv.d5tico$'C.E',-1,1)/100	&& R.03 mariano (imp_peronac por imp_perona)	&& R.07 Mariano (*iif(fiv.D5tico='C',-1,1))	&& R.09 Mariano (falto /100)	&& R.10 Mariano (faltaba la opcion E-ticket-NC)
		
			if type('fivx.Tipo_percep_iva')='U'		&& R.07 Mariano (type('ficx.Tipo_percep_iva')='U')
				replace fiv.d5pere with '000' 		&& codigo arbitrario para interfase, deberan modificar el comprobante	&& R.07 Mariano (fic.d5pere)
			else
				replace fiv.d5pere with fivx.Tipo_percep_iva	&& R.07 Mariano (fic.d5pere)
			endif
	endif

	if !empty(val(fivx.imp_perib))
			do case
				case f00.p0prv1=1	&& R.07 Mariano (f00.p0prc1)
					replace fiv.d5imp6 with val(fivx.imp_perib)*iif(fiv.d5tico$'C.E',-1,1)/100	&& R.07 Mariano (fic.d5imp6 y *iif(fiv.D5tico='C',-1,1))	&& R.08 Mariano (Falto /100)	&& R.10 Mariano (faltaba la opcion E-ticket-NC)
				case f00.p0prv2=1	&& R.07 Mariano (f00.p0prc2)
					replace fiv.d5imp7 with val(fivx.imp_perib)*iif(fiv.d5tico$'C.E',-1,1)/100	&& R.07 Mariano (fic.d5imp7 y *iif(fiv.D5tico='C',-1,1))	&& R.08 Mariano (Falto /100)	&& R.10 Mariano (faltaba la opcion E-ticket-NC)
				case f00.p0prv3=1	&& R.07 Mariano (f00.p0prc3)
					replace fiv.d5imp8 with val(fivx.imp_perib)*iif(fiv.d5tico$'C.E',-1,1)/100	&& R.07 Mariano (fic.d5imp8 y *iif(fiv.D5tico='C',-1,1))	&& R.08 Mariano (Falto /100)	&& R.10 Mariano (faltaba la opcion E-ticket-NC)
			endcase
		
			if type('fivx.Tipo_percep_ing_b')='U'	&& R.07 Mariano (type('ficx.Tipo_percep_ing_b')
				replace fiv.d5cpib with '000' 		&& codigo arbitrario para interfase, deberan modificar el comprobante	&& R.07 Mariano (fic.d5cpib)
			else
				replace fiv.d5cpib with fivx.Tipo_percep_ing_b	&& R.07 Mariano (fic.d5cpib)
			endif
	endif


*****	Clientes	*****
	set order to d2cuit in f02
	m.d5cuit=allt(str(val(fivx.num_idcomp),20))
	&& R.07b Mariano
*	m.d5cuit=left(m.d5cuit,2)+'-'+substr(m.d5cuit,3,8)+'-'+right(m.d5cuit,1)
	if fivx.doc_compra$'80.86.87'
		m.d5cuit=left(m.d5cuit,2)+'-'+substr(m.d5cuit,3,8)+'-'+right(m.d5cuit,1)
	endif
	&& R.07e Mariano
*	if !empty(fivx.num_idcomp) and seek(m.d5cuit,'f02')	&& R.09 Mariano
	if !empty(val(fivx.num_idcomp)) and seek(m.d5cuit,'f02')	&& R.09 Mariano
		replace fiv.d5clie with f02.d2clie,;
				fiv.d5juri with f02.d2juri
*		if fiv.d5tiim=3	&& para el caso de las B deberia ser 3,4,5,6,7	&& R.03 Mariano
		if fiv.d5tiim='3'	&& para el caso de las B deberia ser 3,4,5,6,7	&& R.03 Mariano
			replace fiv.d5tiim with ftiimp(fiv.d5clie,fiv.d5peri,'C')
		endif
		&& R.10b Mariano
		do case
			case fivx.tip_compr$'011.012.013.015' and !fivx.doc_compra$'80.86.87'
				if fiv.d5imp1=0 and fiv.d5imp2=0 and fiv.d5imp4=0
					replace fiv.d5imp2 with fiv.d5tota
				endif
			case fivx.tip_compr$'011.012.013.015' and fivx.doc_compra$'80.86.87'
				if fiv.d5imp1=0 and fiv.d5imp2=0 and fiv.d5imp4=0
					replace fiv.d5imp2 with fiv.d5tota
				endif
		endcase		
		&& R.10e Mariano
	else
		replace fiv.d6nume with fiv.d5nume,;
				fiv.d6peri with fiv.d5peri,;
				fiv.d6empr with d0coem,;
				fiv.d6tipo with fiv.d5tipo,;
				fiv.d6sucu with fiv.d5sucu,;
				fiv.d6tico with fiv.d5tico,;
				fiv.d6tifo with fiv.d5tifo,;
				fiv.d6inte with fiv.d5inte,;
				fiv.d6nocl with fivx.nom_compra,;
				fiv.d6cuit with m.d5cuit
		&& R.09b Mariano
		if fivx.doc_compra='99' and empty(fiv.d6nocl)
*			replace fiv.d6nocl with 'VENTA GLOBAL DIARIA'
		endif
		&& R.09e Mariano
			do case
				case fivx.tip_compr$'001.002.003.004.051.052.053.054.081.115.112.118.120.119'	&& R.08 Mariano (agregue a partir de 004)
					replace fiv.d5clie with '99991',;
							fiv.d6tido with fivx.doc_compra		&& R.07 Mariano ('80')
				case fivx.tip_compr$'006.007.008.009.082.116.113' and !fivx.doc_compra$'80.86.87'	&& R.07 Mariano (fivx.doc_compra!='80')	&& R.08 Mariano (Agregue a partir de 009)
					replace fiv.d5clie with '99993',;
							fiv.d6tido with fivx.doc_compra		&& R.07 Mariano ('96')
				case fivx.tip_compr$'006.007.008.009.082.116.113' and fivx.doc_compra$'80.86.87'	&& R.07 Mariano (='80')	&& R.08 Mariano (Agregue a partir de 009)
					replace fiv.d5clie with '99996',;
							fiv.d6tido with fivx.doc_compra		&& R.07 Mariano ('99994' y '80')
					if fiv.d5tiim='3'	&& para el caso de las B deberia ser 3,4,5,6,7
*						replace fiv.d5tiim with '4'	&& R.07 Mariano
						replace fiv.d5tiim with '6'	&& R.07 Mariano
					endif
				&& R.06b Mariano
				case fivx.tip_compr$'011.012.013.015' and !fivx.doc_compra$'80.86.87'	&& R.07 Mariano (fivx.doc_compra!='80')	&& R.08 Mariano (Agregue 015)
					replace fiv.d5clie with '99993',;
							fiv.d6tido with fivx.doc_compra		&& R.07 Mariano ('96')
					if fiv.d5imp1=0 and fiv.d5imp2=0 and fiv.d5imp4=0
						replace fiv.d5imp2 with fiv.d5tota
					endif
				case fivx.tip_compr$'011.012.013.015' and fivx.doc_compra$'80.86.87'	&& R.07 Mariano (='80')	&& R.08 Mariano (Agregue 015)
					replace fiv.d5clie with '99996',;
							fiv.d6tido with fivx.doc_compra		&& R.07 Mariano ('99994' y '80')
					if fiv.d5tiim='3'	&& para el caso de las C deberia ser 3,4,5,6,7
*						replace fiv.d5tiim with '4'	&& R.07 Mariano
						replace fiv.d5tiim with '6'	&& R.07 Mariano
					endif
					if fiv.d5imp1=0 and fiv.d5imp2=0 and fiv.d5imp4=0
						replace fiv.d5imp2 with fiv.d5tota
					endif
				&& R.06e Mariano
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
	replace fiv.d5leye with allt(fiv.d5leye)+' '+fiv.d5tifo+fiv.d5sucu+'-'+fiv.d5nume

*****	Rubro	*****

	if type('fivx.rubro')='U'
		replace fiv.d5rubr with '999999' 	&& codigo arbitrario para interfase, debe existir en la empresa
	else
		replace fiv.d5rubr with fivx.rubro
	endif

*///////// ALICUOTAS \\\\\\\\\\\\*

	&& R.02b Taboada
	SELE FIAX
	INDEX ON tip_compr+pun_venta+num_compr TAG Tipo_compr ADDITIVE
	SELE FIV
	SCATTER MEMVAR
	
	regfiv=recno('fiv')
	IF seek(fivx.tip_compr+fivx.pun_venta+fivx.num_compr,'fiax')
		totaor = fiv.d5tota
		totdes = 0
		alic = 1
		do while (fivx.tip_compr+fivx.pun_venta+fivx.num_compr=fiax.tip_compr+fiax.pun_venta+fiax.num_compr and alic <= val(fivx.can_aliiva))	
				m.d5imp1 = val(fiax.neto_grav)*iif(m.d5tico$'C.E',-1,1) / 100	&& R.07 Mariano (*iif(m.D5tico='C',-1,1))	&& R.10 Mariano (faltaba la opcion E-ticket-NC)
				m.d5imp4 = val(fiax.imp_liq)*iif(m.d5tico$'C.E',-1,1) / 100	&& R.07 Mariano (*iif(m.D5tico='C',-1,1))	&& R.10 Mariano (faltaba la opcion E-ticket-NC)
					
	
				do case
					case (fiax.ali_iva='0003')
						m.d5alic = '000'
						m.d5por1 = 0.00
					case (fiax.ali_iva='0004')
						m.d5alic = '001'
						m.d5por1 = 10.50
					case (fiax.ali_iva='0005')
						m.d5alic = '011'
						m.d5por1 = 21.00
					case (fiax.ali_iva='0006')
						m.d5alic = '004'
						m.d5por1 = 27.00
					case (fiax.ali_iva='0008')
						m.d5alic = '010'
						m.d5por1 = 5.00
					case (fiax.ali_iva='0009')
						m.d5alic = '009'
						m.d5por1 = 2.50
				endcase
			
					
			*/////////// Duplicados \\\\\\\\\\\\*	
				
				sele fiv													
				if alic=1
					GATHER MEMVAR	
				else
					if fivx.tip_compr+fivx.pun_venta+fivx.num_compr=fiax.tip_compr+fiax.pun_venta+fiax.num_compr
						sele fiv
						
																																				
						append blank
			
						m.d5tota=m.d5imp1+m.d5imp4		
						
						totdes=totdes+m.d5tota
						
						&& R.13b Mariano	&& saco todos los demas importes, ya que sino quedarian duplicados
						store 0 to m.d5imp2,m.d5imp3,m.d5imp5,m.d5imp6,m.d5imp7,m.d5imp8
						store '' to m.d5pere,m.d5cpib,m.d5cpga,m.d5cpot
						store '' to m.d5reiv,m.d5regi,m.d5rega,m.d5prco
						&& R.13e Mariano
										
						GATHER MEMVAR
					endif
				endif
				skip in fiax	

				alic = alic + 1
													
		enddo
		
		*** vuekvo al primer registro para grabar el total - los subtotales por alicuotas ****
		sele fiv
		go regfiv
		replace d5tota with totaor-totdes	
	endif			
	&& R.02e Taboada
	SKIP IN fivx

ENDDO

sele fivx
zap

endif 	&& R.11 Mariano

sele fiv
use in fiv

return

&& R.02e Taboada


&& R.03b Mariano
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc brow04in
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
private w_dkey,w_hkey

push menu _msysmenu

go top
browse fields ;
	d5tifo :h='F' :v=fenter():f,;
	d5sucu :h='Sucursal' :r,;
	d5nume :h='N๚mero':r,;
	d5fech :h='Fecha':r,;
	d5tota :h='Total':r,;
	clie=d5clie+'-'+iif(d5clie>='99990', d6nocl, iif(seek(d5clie,'f02'), f02.d2nomb, 'No encontrado!!!')) :h='Cliente':r:40,;
	tico=iif(seek('CO'+d5coco,'f01t'), f01t.desc, '') :h='Tipo Cpbte.' :15 :r ;
	nome noap noed node norm in screen ;
	title 'COMPROBANTES DE VENTAS A INCORPORAR EN PERIODO: '+wdperi+'   <ESC> -> para Continuar' ;
	font 'arial',10 ;
	valid :f salebrow(lastkey()) rest	&& R.06 Mariano (Cambie iif(d5clie>='99990', iif(seek(d5inte,'f06'), f06.d6nocl, 'No encontrado!!!'), iif(seek(d5clie,'f02'), f02.d2nomb, 'No encontrado!!!')) )

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
&& R.03e Mariano




&& Puesto adrede para que incluya el report en el proyecto
repo form iwr004in


			&& fec_compr c(8); && Fecha de comprobante
			&&,tip_compr c(3);	&& Tipo de comprobante
			&&,pun_venta c(5);	&& Punto de venta
			&&,num_compr c(20);	&& N๚mero de comprobante
			&&,num_cphast c(20);	&& N๚mero de comprobante hasta
			&&,doc_compra c(2);	&& C๓digo de documento del comprador
			&&,num_idcomp c(20);	&& N๚mero de identificaci๓n del comprador
			&&,nom_compra c(30);	&& Apellido y nombre o denominaci๓n del comprador
			&&,imp_totop	c(15);	&& Importe total de la operaci๓n
			&&,imp_totsng c(15);	&& Importe total de conceptos que no integran el precio neto gravado
			&&,per_nocate c(15); 	&& Percepci๓n a no categorizados
			&&,imp_opexen c(15);	&& Importe de operaciones exentas
			&&,imp_perona c(15); && Importe de percepciones o pagos a cuenta de impuestos Nacionales
			&&,imp_perib c(15);	&& Importe de percepciones de Ingresos Brutos
			&&,imp_permun c(15);	&& Importe de percepciones impuestos Municipales
			&&,imp_int c(15);	&& Importe impuestos internos
			&&,cod_moneda c(3);	&& C๓digo de moneda
			&&,tip_cambio c(10);	&& Tipo de cambio
			&&,can_alํiva c(1);	&& Cantidad de alํcuotas de IVA
			&&,cod_op c(1);		&& C๓digo de operaci๓n
			&&,otr_tribut c(15);	&& Otros Tributos
			&&,fec_venpag c(8))	&& Fecha de Vencimiento de Pago