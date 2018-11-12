*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : IWP006IN.PRG -> Interfase para la incorporacion de comprobantes desde Excel estandard
*)-H Release 		 : 10
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
w_pfile='06in'
w_title='INCORPORACION DE COMPROBANTES DE EXCEL ESTANDAR'

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

=creatipos('C')

*)-----------------------------------------------------------------------------

=SetComp() &&(Setea filtros de comprobante DB CR y su signo)


go bott in f04
go top in f12

wdici		= sys(5)+ALLTRIM(F12.p0diin)	&& R.01 Mariano (Agregue sys(5), para la intiwxls)
wcoem		= '\'+d0coem

if !mfile(wdici+wcoem+'\NUL')
	if !mfile(wdici+'\NUL')
		=creadir(wdici)
		=creadir(wdici+wcoem)
	else
		=creadir(wdici+wcoem)
	endif
endif			
	
wdici=wdici+wcoem	&& R.01 Mariano

wcoto		= 'N'

wtrim		= '1'
wano		= right(f04->d4peri,4)


wdperi		= f04.d4peri

xlsvent		= 'Ventas*.xls?'	&& R.01 Mariano (Agregue ?)
xlscomp		= 'Compras*.xls?'	&& R.01 Mariano (Agregue ?)

&& R.01b Mariano
w_move=.f.
w_prve=.f.	
w_moco=.f.
w_prco=.f.
&& R.01e Mariano

set skip of menu _msysmenu .t.	&& R.01 Mariano

do ssm998.mpr

&& R.08b Mariano
dele file &d0coem.\inte005v.dbf
dele file &d0coem.\inte005c.dbf
dele file &d0coem.\intevtas.dbf
dele file &d0coem.\intecpra.dbf
&& R.08e Mariano

do iws006in.spr		

fieldvec=''

if !autor() 
	pop key
	pop menu _msysmenu

	=closall()
	
	=borratipos()
	
	return
endif

*)-----------------------------------------------------------------------------

w_impr='S'
*do ssm998.mpr	

*do cons06in0	&& ***vfp***	

pop key
pop menu _msysmenu
=closall()
=borratipos()	&& R.01 Mariano
dele file &d0coem.\inte005v.dbf
dele file &d0coem.\inte005c.dbf
dele file &d0coem.\intevtas.dbf
dele file &d0coem.\intecpra.dbf
dele file &d0f99..dbf
dele file &d0f99.e.dbf
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC valid06in
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS w_show

***vfp***
if version()='Visual Fox' and (w_pfile!='06in' or (!wexist('IWS006IN') and wontop()!='IWS006IN')) and w_show
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

&& R.01b Mariano
if w_vari='W_BTN3'
	w_move=.t.
	keyboard '{f9}'
	_curobj=1
endif
if w_vari='W_BTN4'
	w_moco=.t.
	keyboard '{f9}'
	_curobj=1
endif
&& R.01e Mariano

if w_vari='W_BTN1'
	w_prve=.t.	&& R.01 Mariano
	keyboard '{f9}'
	_curobj=1
endif

if w_vari='W_BTN2'
	w_prco=.t.	&& R.01 Mariano
	keyboard '{f9}'
	_curobj=1
endif

return(w_retu)

***vfp***
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons06in
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	on key label f9 do cons06in0
	keyboard '{f9}'
*return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons06in0
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
on key label f9

***vfp***

&& R.01b Mariano
if !w_move and !w_moco and !w_prve and !w_prco	&& Si puso F9 o confirmar del menu procesa todo junto
	w_prve=.t.
	w_prco=.t.
endif

if w_move or w_moco
	if w_move
		dimension wwfiles(1)
		wwfiles=''
		wdici= sys(5)+ALLTRIM(F12.p0diin)
		wwcant=a_dir('wwfiles',wdici+'\MODELO_PARA_INCORPORAR_VENTAS.XLS?','','',.t.)
		wfinu=0
		for wfinu=1 to wwcant	
			wait wind 'Generando el Excel a partir del '+wwfiles[wfinu,1] time 1
			wait wind 'Generando el Excel a partir del '+wwfiles[wfinu,1] nowait
			w_fire='"'+wdici+'\'+wwfiles[wfinu,1]+'"'	
			w_fipr='"'+wdici+wcoem+'\VENTAS_'+d0coem+'_'+right(wdperi,4)+'_'+left(wdperi,2)+right(wwfiles[wfinu,1],len(wwfiles[wfinu,1])-at('.',wwfiles[wfinu,1])+1)+'"'
			if mfile(&w_fipr)
				wait wind 'Ya existe el archivo de VENTAS en '+wdici+wcoem time 2
			else
				if version()='Visual FoxPro'
					copy file &w_fire. to &w_fipr.
				else
					private w_vfox,w_inst001	&& w_inst002
					w_inst001='copy file '+w_fire+' to '+w_fipr
*					w_inst002='=DelFxls('+w_fipr+',"5")'	&& Deletea fila 5 del Excel nuevo
					
					private d0f9ve
					d0f9ve=sys(2003)+'\I'+d0coem+p_netuser+'ve'
					w_vfox=d0f9ve+'.mem'
					save to &w_vfox. all like w_inst*
					release all like w_inst*

					=visuafox('ejeproc(.f.)',w_vfox)
	
					=inkey(2)
	
					if file(w_vfox)
						erase &w_vfox.
					endif
				endif
			endif
			=winexcel(w_fipr)
		endfor
		wdici=wdici+wcoem
		wait clear
		w_move=.f.
	endif
	if w_moco
		dimension wwfiles(1)
		wwfiles=''
		wdici= sys(5)+ALLTRIM(F12.p0diin)
		wwcant=a_dir('wwfiles',wdici+'\MODELO_PARA_INCORPORAR_COMPRAS.XLS?','','',.t.)
		wfinu=0
		for wfinu=1 to wwcant	
			wait wind 'Generendo el Excel a partir del '+wwfiles[wfinu,1] time 1
			wait wind 'Generando el Excel a partir del '+wwfiles[wfinu,1] nowait
			w_fire='"'+wdici+'\'+wwfiles[wfinu,1]+'"'	
			w_fipr='"'+wdici+wcoem+'\COMPRAS_'+d0coem+'_'+right(wdperi,4)+'_'+left(wdperi,2)+right(wwfiles[wfinu,1],len(wwfiles[wfinu,1])-at('.',wwfiles[wfinu,1])+1)+'"'
			if mfile(&w_fipr)
				wait wind 'Ya existe el archivo de COMPRAS en '+wdici+wcoem time 2
			else
				if version()='Visual FoxPro'
					copy file &w_fire. to &w_fipr.
				else
					private w_vfox,w_inst001
					w_inst001='copy file '+w_fire+' to '+w_fipr
	
					private d0f9co
					d0f9co=sys(2003)+'\I'+d0coem+p_netuser+'co'
					w_vfox=d0f9co+'.mem'
					save to &w_vfox. all like w_inst*
					release all like w_inst*

					=visuafox('ejeproc(.f.)',w_vfox)

					=inkey(2)

					if file(w_vfox)
						erase &w_vfox.
					endif
				endif
			endif
			=winexcel(w_fipr)
		endfor
		wdici=wdici+wcoem
		wait clear
		w_moco=.f.
	endif
	return
endif
&& R.01e Mariano

if wexist('getdest')
	* Impresion del archivo de errores

	do repdest && Setea variables internas del reporte

	go top in f9e
	sele f9e

	if !empty(p_repclaus) &&(Debe ser preview o to printer)
		p_report='iwr006in'
		=rep_form()
		p_report=''
	endif

	do represet 
	
	clear read
	
else

	if !valid06in(.t.)
		return
	endif

	*** INTEVTAS.DBF y INTECPRA.DBF son Dbfs acordes para recibir los datos de excel en version 5.0 ***
	
	if w_prve and !file(d0coem+'\INTEVTAS.DBF')	&& R.01 Mariano
		sele 0	
		&& R.00b Taboada
*		create dbf &d0coem.\intevtas.dbf (d5fech c(10), d5tico c(3), d5tifo c(1), d5sucu c(4), d5numd c(8), d5numh c(8), d5clie c(5), d6nocl c(20), d6tido c(2), d6cuit c(13), d5tiim c(20), d5imp1 n(20,2), d5imp2 n(20,2), d5por1 n(5,2), d5imp4 n(20,2), d5imp3 n(20,2), d5imp6 n(20,2), d5imp7 n(20,2), d5imp8 n(20,2), d5tota n(20,2), d5juri c(20), d5rubr c(6), d5tiop c(25), d5pere C(3), d5cpib c(3), d5cpga c(3), d5cpot c(3), d5cond c(1), d5imre n(20,2), d5adua c(3), d5dest c(4), d5desp c(6), d5verf c(1))	&& R.01 Mariano
*		create dbf &d0coem.\intevtas.dbf (d5fech c(10), d5tico c(3), d5tifo c(1), d5sucu c(4), d5numd c(8), d5numh c(8), d5clie c(5), d6nocl c(35), d6tido c(2), d6cuit c(13), d5tiim c(20), d5imp1 n(20,2), d5imp2 n(20,2), d5por1 n(5,2), d5imp4 n(20,2), d5imp3 n(20,2), d5imp6 n(20,2), d5imp7 n(20,2), d5imp8 n(20,2), d5tota n(20,2), d5juri c(20), d5rubr c(6), d5tiop c(25), d5cond c(10), d5pere c(3), d5cpib c(3), d5cpga c(3), d5cpot c(3), d5imre n(20,2), d7numori c(20))	&& R.01 Mariano	&& R.04 Mariano
		create dbf &d0coem.\intevtas.dbf (d5fech c(10), d5tico c(3), d5tifo c(1), d5sucu c(5), d5numd c(8), d5numh c(8), d5clie c(5), d6nocl c(35), d6tido c(4), d6cuit c(13), d5tiim c(20), d5imp1 n(20,2), d5imp2 n(20,2), d5por1 n(5,2), d5imp4 n(20,2), d5imp3 n(20,2), d5imp6 n(20,2), d5imp7 n(20,2), d5imp8 n(20,2), d5tota n(20,2), d5juri c(20), d5rubr c(6), d5tiop c(25), d5cond c(10), d5pere c(3), d5cpib c(3), d5cpga c(3), d5cpot c(3), d5imre n(20,2), d7numori c(20))	&& R.04 Mariano	&& R.10 Mariano (d5sucu con 5)
		&& R.00e Taboada
		use		
	endif	
	if w_prco and !file(d0coem+'\INTECPRA.DBF')	&& R.01 Mariano
 		sele 0	
*		create dbf &d0coem.\intecpra.dbf (d5fech c(10), d5tico c(3), d5tifo c(1), d5sucu c(4), d5numd c(8), d5numh c(8), d5clie c(5), d6nocl c(20), d6tido c(2), d6cuit c(13), d5tiim c(20), d5imp1 n(20,2), d5imp2 n(20,2), d5por1 n(5,2), d5imp4 n(20,2), d5imp3 n(20,2), d5imp6 n(20,2), d5imp7 n(20,2), d5imp8 n(20,2), d5tota n(20,2), d5juri c(20), d5rubr c(6), d5tiop c(25), d5pere C(3), d5cpib c(3), d5cpga c(3), d5cpot c(3), d5cond c(1), d5imre n(20,2), d5adua c(3), d5dest c(4), d5desp c(6), d5verf c(1))	&& R.01 Mariano
*		create dbf &d0coem.\intecpra.dbf (d5fech c(10), d5tico c(3), d5tifo c(1), d5sucu c(4), d5nume c(8), d5clie c(5), d6nocl c(20), d6tido c(2), d6cuit c(13), d5tiim c(20), d5imp1 n(20,2), d5imp2 n(20,2), d5por1 n(5,2), d5imp4 n(20,2), d5imp3 n(20,2), d5imp6 n(20,2), d5imp7 n(20,2), d5imp8 n(20,2), d5tota n(20,2), d5juri c(20), d5rubr c(6), d5tiop c(25), d5cond c(10), d5pere C(3), d5cpib c(3), d5cpga c(3), d5cpot c(3), d5imre n(20,2), d7numori c(20), d5cuco c(13), d5deco c(35), d5ivco n(20,2), d5adua c(3), d5dest c(4), d5desp c(6), d5verf c(1))	&& R.01 Mariano	&& R.04 Mariano
		create dbf &d0coem.\intecpra.dbf (d5fech c(10), d5tico c(3), d5tifo c(1), d5sucu c(5), d5nume c(8), d5clie c(5), d6nocl c(20), d6tido c(4), d6cuit c(13), d5tiim c(20), d5imp1 n(20,2), d5imp2 n(20,2), d5por1 n(5,2), d5imp4 n(20,2), d5imp3 n(20,2), d5imp6 n(20,2), d5imp7 n(20,2), d5imp8 n(20,2), d5tota n(20,2), d5juri c(20), d5rubr c(6), d5tiop c(25), d5cond c(10), d5pere C(3), d5cpib c(3), d5cpga c(3), d5cpot c(3), d5imre n(20,2), d7numori c(20), d5cuco c(13), d5deco c(35), d5ivco n(20,2), d5adua c(3), d5dest c(4), d5desp c(6), d5verf c(1))	&& R.04 Mariano	&& R.10 Mariano (d5sucu con 5)
		use		
	endif	
	
	
	if (w_prve and file(d0coem+'\INTEVTAS.DBF')) or (w_prco and file(d0coem+'\INTECPRA.DBF'))	&& R.01 Mariano
		do genera06in	&& genera los inte005v y inte005c a partir de planillas excel
	endif

	if (w_prve and !file(d0coem+'\INTE005v.DBF')) or (w_prco and !file(d0coem+'\INTE005c.DBF'))	&& R.01 Mariano
	   wait 'Faltan archivos de interface INTE005v,INTE005c.DBF' wind
	   return
	endif

	wait clear

	&& R.01b Mariano
	if w_prve and alias()!='FIV' 	&& R.01 Mariano
		use &d0coem.\inte005v in 0 alias fiv 	&& Interfase Ventas
		sele fiv
		if version()='Visual FoxPro'
			*** cambio el lastkey() ***
			keyboard '{tab}'
			=inkey()
			DO wbrow WITH 'brow06in()'
	    else
   			do brow06in
	    endif
	    use in fiv
	endif
	if w_prco and alias()!='FIC' 	&& R.01 Mariano
		use &d0coem.\inte005c in 0 alias fic 	&& Interfase Compras
		sele fic
		if version()='Visual FoxPro'
			*** cambio el lastkey() ***
			keyboard '{tab}'
			=inkey()
			DO wbrow WITH 'brow06in()'
	    else
   			do brow06in
	    endif
	    use in fic
	endif
	&& R.01e Mariano

	if sino('Confirma incorporaci๓n de comprobantes '+iif(w_prve and !w_prco,'de Ventas',iif(!w_prve and w_prco,'de Compras',''))+'?.','Si',w_title)='N'
		return	
	endif

	wait 'Procesando <No interrumpir>...' wind nowait

	if w_prve	&& R.01 Mariano
		dimension wwfiles(1)
		wwfiles=''
*		wwcant=a_dir('wwfiles',wdici+'\'+d0coem+'\VENTAS*.XLS','','',.t.)  	&& R.01 Mariano
		wwcant=a_dir('wwfiles',wdici+'\VENTAS*.XLS?','','',.t.)  	&& R.01 Mariano
		wfinu=0
		for wfinu=1 to wwcant	
*			w_fire='"'+wdici+'\'+d0coem+'\'+wwfiles[wfinu,1]+'"'	&& R.01 Mariano
			w_fire='"'+wdici+'\'+wwfiles[wfinu,1]+'"'	&& R.01 Mariano
			if version()='Visual FoxPro'
*				w_fipr='"'+wdici+'\'+d0coem+'\PROCESADO_'+wwfiles[wfinu,1]+'"'		&& R.01 Mariano
				w_fipr='"'+wdici+'\PROCESADO_'+wwfiles[wfinu,1]+'"'		&& R.01 Mariano
				dele file &w_fipr.
				rename &w_fire. to &w_fipr.
			else
*				w_fipr='"'+wdici+'\'+d0coem+'\PROCESADO_'+wwfiles[wfinu,1]+'"'	&& R.01 Mariano
				w_fipr='"'+wdici+'\PROCESADO_'+wwfiles[wfinu,1]+'"'		&& R.01 Mariano
				private w_vfox,w_inst001,w_inst002
				w_inst001='dele file '+w_fipr
				w_inst002='rename '+w_fire+' to '+w_fipr
	
				private d0f9ve
				d0f9ve=sys(2003)+'\I'+d0coem+p_netuser+'ve'
				w_vfox=d0f9ve+'.mem'
				save to &w_vfox. all like w_inst*
				release all like w_inst*

				=visuafox('ejeproc(.f.)',w_vfox)

				=inkey(2)	&& R.01 Mariano

				if file(w_vfox)
					erase &w_vfox.
				endif
			
*				=inkey(2)	&& R.01 Mariano
			endif
		endfor
	endif
	if w_prco	&& R.01 Mariano
		dimension wwfiles(1)
		wwfiles=''
*		wwcant=a_dir('wwfiles',wdici+'\'+d0coem+'\COMPRAS*.XLS','','',.t.)	&& R.01 Mariano
		wwcant=a_dir('wwfiles',wdici+'\COMPRAS*.XLS?','','',.t.)	&& R.01 Mariano
		wfinu=0
		for wfinu=1 to wwcant
*			w_fire='"'+wdici+'\'+d0coem+'\'+wwfiles[wfinu,1]+'"'	&& R.01 Mariano
			w_fire='"'+wdici+'\'+wwfiles[wfinu,1]+'"'	&& R.01 Mariano
			if version()='Visual FoxPro'
*				w_fipr='"'+sys(2003)+'\'+d0coem+'\PROCESADO_'+wwfiles[wfinu,1]+'"'
*				w_fipr='"'+wdici+'\'+d0coem+'\PROCESADO_'+wwfiles[wfinu,1]+'"'	&& R.01 Mariano
				w_fipr='"'+wdici+'\PROCESADO_'+wwfiles[wfinu,1]+'"'	&& R.01 Mariano
				dele file &w_fipr.
				rename &w_fire. to &w_fipr.
			else
*				w_fipr='"'+wdici+'\'+d0coem+'\PROCESADO_'+wwfiles[wfinu,1]+'"'	&& R.01 Mariano
				w_fipr='"'+wdici+'\PROCESADO_'+wwfiles[wfinu,1]+'"'		&& R.01 Mariano
				private w_vfox,w_inst001,w_inst002
				w_inst001='dele file '+w_fipr
				w_inst002='rename '+w_fire+' to '+w_fipr

				private d0f9co
				d0f9co=sys(2003)+'\I'+d0coem+p_netuser+'co'
				w_vfox=d0f9co+'.mem'
				save to &w_vfox. all like w_inst*
				release all like w_inst*

				=visuafox('ejeproc(.f.)',w_vfox)

				=inkey(2)	&& R.01 Mariano

				if file(w_vfox)
					erase &w_vfox.
				endif
			
*				=inkey(2)	&& R.01 Mariano
			endif
		endfor
	endif

	if used('f9e')
		use in f9e
	else
		sele 0
		create table (d0f99);
		(d99mens c(90),d99tipo c(1))
		if version()='Visual FoxPro'
			w_type='type fox2x'
		else
			w_type=''
		endif
		copy to &d0f99.e.dbf &w_type
		use 
	endif

	fieldvec[ 1]='F9e'

	use &d0f99.e in 0 alias f9e	exclusive	&& Errores de interfase
	if w_prve	&& R.01 Mariano
		fieldvec[ 2]='Fiv'
		use &d0coem.\inte005v in 0 alias fiv 	&& Interfase Ventas
	endif	&& R.01 Mariano
	if w_prco	&& R.01b Mariano
		if !w_prve
			fieldvec[ 2]='Fic'
		else
			fieldvec[ 3]='Fic'
		endif	&& R.01e Mariano
		use &d0coem.\inte005c in 0 alias fic 	&& Interfase Compras
	endif	&& R.01 Mariano
	
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

	if (w_prve and empty(reccount('fiv'))) or (w_prco and empty(reccount('fic')))	&& R.01 Mariano
		wait clear
		=advsuave('No hay comprobantes para procesar.',w_title)
		clear read
		return
	endif
			
	if w_prve and !empty(reccount('fiv'))	&& R.01 Mariano
		sele fiv
		go top
		do while !eof('fiv')
	
			if seek(fiv.d5tipo+fiv.d5sucu+fiv.d5tifo+fiv.d5tico+fiv.d5nume,'f05')
				
				do while fiv.d5tipo+fiv.d5sucu+fiv.d5tifo+fiv.d5tico+fiv.d5nume=f05.d5tipo+f05.d5sucu+f05.d5tifo+f05.d5tico+f05.d5nume and !eof('f05')
					if fiv.d5peri=f05.d5peri and fiv.d5inte!=f05.d5inte
						do error06in with 'Comprobante existente :'+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'
						exit
					endif
					skip in f05
				enddo
				
				skip in fiv
				loop
			endif

			if !seek(fiv.d5clie,'f02')
				do error06in with 'Cliente inexistente :'+fiv.d5clie+' '+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'	
				skip in fiv
				loop
			endif
		
			&& R.03b Mariano							
			if fiv.d6tido$'80.86.87'
				if dicuit(left(fiv.d6cuit,11))<>right(fiv.d6cuit,1) and !empty(strtran(fiv.d6cuit,'-',''))
					do error06in with 'Error en CUIT o CUIL o CDI :'+fiv.d6cuit+' '+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'	
					skip in fiv
					loop
				endif
			endif
			&& R.03e Mariano
							
			if !seek(ptoi(fiv.d5peri),'F04')
				do error06in with 'Periodo inexistente :'+fiv.d5peri+' Comp:'+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'
				skip in fiv
				loop
			endif
									
			if left(dtos(fiv.d5fech),6)!=ptoi(fiv.d5peri)
				do error06in with 'La fecha no corresponde al periodo :'+dtoc(fiv.d5fech)+' Comp:'+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'
				skip in fiv
				loop
			endif
							
			if seek(ptoi(fiv.d5peri),'f04') and f04.d4esve='C'
				do error06in with 'Periodo cerrado :'+fiv.d5peri+' Comp:'+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'
				skip in fiv
				loop
			endif
		
			if !seek(fiv.d5alic,'f23')
				do error06in with 'Alicuota de iva inexistente :'+fiv.d5alic+' Comp:'+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'
				skip in fiv
				loop
			endif

			if !seek('JU'+fiv.d5juri,'f01')
				do error06in with 'Jurisdicci๓n inexistente :'+fiv.d5juri+' Comp:'+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'
				skip in fiv
				loop
			endif
			
			if fiv.d5tiim!=ftiimp(fiv.d5clie,fiv.d5peri,'C')
				do error06in with 'La cond. imp. del cliente :'+fiv.d5clie+' no corresponde'+' Comp:'+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'	
				skip in fiv
				loop
			endif
			
*			if !fiv.d5tifo$iif(fiv.d5tiim$'12','ACEMTZ ',iif(fiv.d5tiim='7','ABCEMTZ ','BCETZ '))	&& R.03 Mariano
			if !fiv.d5tifo$iif(fiv.d5tiim$'12','ACEMTZ ',iif(fiv.d5tiim='7','ABCEMTZ ','BCETZ ')) or !fiv.d5tifo$tiform(fiv.d5tico) && R.03 Mariano
				do error06in with 'Formulario incorrecto :'+fiv.d5tifo+' Comp:'+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'
				skip in fiv
				loop
			endif

			if !seek('RV'+fiv.d5rubr,'f01')
				do error06in with 'Rubro inexistente :'+fiv.d5rubr+' Comp:'+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'
				skip in fiv
				loop
			endif

			&& R.00b Taboada (Perc o Ret)
			if !empty(fiv.d5pere) or fiv.d5imp3!=0 
*				if seek(fiv.d5pere,'f24')	&& R.01 Mariano
*				if seek(fiv.d5pere+'I'+iif(empty(fiv.d5pere),'P','R'),'f24')	&& R.01 Mariano	&& R.02 Mariano
*				if seek(fiv.d5pere+'I'+iif(empty(fiv.d5imre),'P','R'),'f24')	&& R.02 Mariano	&& R.07 Mariano
				if seek(fiv.d5pere+'I'+iif(fiv.d5tico!='R','P','R'),'f24')		&& R.07 Mariano
*					if f24.d24codi!='000' and f24.d24tipo!='I' or f24.d24refe!='P'	&& R.01 Mariano
*					if f24.d24codi!='000' or f24.d24tipo!='I' or f24.d24refe!=iif(empty(fiv.d5pere),'P','R')	&& R.01 Mariano && R.02 Mariano
*					if f24.d24codi!='000' or f24.d24tipo!='I' or f24.d24refe!=iif(empty(fiv.d5imre),'P','R')	&& R.02 Mariano	&& R.07 Mariano
*					if f24.d24codi!='000' or f24.d24tipo!='I' or f24.d24refe!=iif(fiv.d5tico!='R','P','R')		&& R.07 Mariano	&& R.09 Mariano
					if f24.d24tipo!='I' or f24.d24refe!=iif(fiv.d5tico!='R','P','R')		&& R.09 Mariano
						do error06in with 'Perc./Ret. Iva no corresponde :'+fiv.d5pere+' '+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'
						skip in fiv
					  	loop
					endif 			
				else
					do error06in with 'Perc./Ret. Iva inexistente :'+fiv.d5pere+' '+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'
					skip in fiv
					loop
				Endif
			endif
			
			if !empty(fiv.d5cpib)
				=SEEK("JU" + fiv.d5juri, "f01")
				cafip	= f01.d1tdgi
				do case
					case f00.p0prv1=1 and fiv.d5imp6!=0
*						if !seek(cafip+fiv.d5cpib+fiv.d5regi,'f25') and fiv.d5cpib!='000'	&& R.01 Mariano
*						if !seek(cafip+fiv.d5cpib+iif(empty(fiv.d5imre),'P','R'),'f25') and fiv.d5cpib!='000'	&& R.01 Mariano	&& R.07 Mariano
						if !seek(cafip+fiv.d5cpib+iif(fiv.d5tico!='R','P','R'),'f25') and fiv.d5cpib!='000'		&& R.07 Mariano
							do error06in with 'Perc./Ret. IB inexistente :'+fiv.d5cpib+' '+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'
							skip in fiv
							loop
						endif
					case f00.p0prv2=1 and fiv.d5imp7!=0
*						if !seek(cafip+fiv.d5cpib+fiv.d5regi,'f25') and fiv.d5cpib!='000'	&& R.01 Mariano
*						if !seek(cafip+fiv.d5cpib+iif(empty(fiv.d5imre),'P','R'),'f25') and fiv.d5cpib!='000'	&& R.01 Mariano	&& R.07 Mariano
						if !seek(cafip+fiv.d5cpib+iif(fiv.d5tico!='R','P','R'),'f25') and fiv.d5cpib!='000'		&& R.07 Mariano
							do error06in with 'Perc./Ret. IB inexistente :'+fiv.d5cpib+' '+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'
							skip in fiv
							loop
						endif
					case f00.p0prv3=1 and fiv.d5imp8!=0
*						if !seek(cafip+fiv.d5cpib+fiv.d5regi,'f25') and fiv.d5cpib!='000'	&& R.01 Mariano
*						if !seek(cafip+fiv.d5cpib+iif(empty(fiv.d5imre),'P','R'),'f25') and fiv.d5cpib!='000'	&& R.01 Mariano	&& R.07 Mariano
						if !seek(cafip+fiv.d5cpib+iif(fiv.d5tico!='R','P','R'),'f25') and fiv.d5cpib!='000'		&& R.07 Mariano
							do error06in with 'Perc./Ret. IB inexistente :'+fiv.d5cpib+' '+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'
							skip in fiv
							loop
						endif
					otherwise
						do error06in with 'Falta definir Columna para IB o no hay importe IB :'+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'
						skip in fiv
						loop
				endcase
			else
				do case
					case f00.p0prv1=1 and fiv.d5imp6!=0
						do error06in with 'Falta Perc./Ret. IB :'+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'
						skip in fiv
						loop
					case f00.p0prv2=1 and fiv.d5imp7!=0
						do error06in with 'Falta Perc./Ret. IB :'+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'
						skip in fiv
						loop
					case f00.p0prv3=1 and fiv.d5imp8!=0
						do error06in with 'Falta Perc./Ret. IB :'+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'
						skip in fiv
						loop
				endcase
			endif
			
			if !empty(fiv.d5cpga)
*				=SEEK("JU" + fiv.d5juri, "f01")	&& R.02 Mariano
*				cafip	= f01.d1tdgi	&& R.02 Mariano
				do case
					case f00.p0prv1=2 and fiv.d5imp6!=0
*						if !seek(cafip+fiv.d5cpga+fiv.d5rega,'f25') and fiv.d5cpga!='000'	&& R.01 Mariano
*						if !seek(cafip+fiv.d5cpga+iif(empty(fiv.d5imre),'P','R'),'f25') and fiv.d5cpga!='000'	&& R.01 Mariano	&& R.02 Mariano
*						if !seek(fiv.d5cpga+'G'+iif(empty(fiv.d5imre),'P','R'),'f24') and fiv.d5cpga!='000'		&& R.02 Mariano	&& R.07 Mariano
						if !seek(fiv.d5cpga+'G'+iif(fiv.d5tico!='R','P','R'),'f24') and fiv.d5cpga!='000'		&& R.07 Mariano
							do error06in with 'Perc./Ret. Gcias inexistente :'+fiv.d5cpga+' '+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'
							skip in fiv
							loop
						endif
					case f00.p0prv2=2 and fiv.d5imp7!=0
*						if !seek(cafip+fiv.d5cpga+fiv.d5rega,'f25') and fiv.d5cpga!='000'	&& R.01 Mariano
*						if !seek(cafip+fiv.d5cpga+iif(empty(fiv.d5imre),'P','R'),'f25') and fiv.d5cpga!='000'	&& R.01 Mariano	&& R.02 Mariano
*						if !seek(fiv.d5cpga+'G'+iif(empty(fiv.d5imre),'P','R'),'f24') and fiv.d5cpga!='000'		&& R.02 Mariano	&& R.07 Mariano
						if !seek(fiv.d5cpga+'G'+iif(fiv.d5tico!='R','P','R'),'f24') and fiv.d5cpga!='000'		&& R.07 Mariano
							do error06in with 'Perc./Ret. Gcias inexistente :'+fiv.d5cpga+' '+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'
							skip in fiv
							loop
						endif
					case f00.p0prv3=2 and fiv.d5imp8!=0
*						if !seek(cafip+fiv.d5cpga+fiv.d5rega,'f25') and fiv.d5cpga!='000'	&& R.01 Mariano
*						if !seek(cafip+fiv.d5cpga+iif(empty(fiv.d5imre),'P','R'),'f25') and fiv.d5cpga!='000'	&& R.01 Mariano	&& R.02 Mariano
*						if !seek(fiv.d5cpga+'G'+iif(empty(fiv.d5imre),'P','R'),'f24') and fiv.d5cpga!='000'		&& R.02 Mariano	&& R.07 Mariano
						if !seek(fiv.d5cpga+'G'+iif(fiv.d5tico!='R','P','R'),'f24') and fiv.d5cpga!='000'		&& R.07 Mariano
							do error06in with 'Perc./Ret. Gcias inexistente :'+fiv.d5cpga+' '+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'
							skip in fiv
							loop
						endif
					otherwise
						do error06in with 'Falta definir Columna para Gcias o no hay importe Gcias :'+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'
						skip in fiv
						loop
				endcase
			else
				do case
					case f00.p0prv1=2 and fiv.d5imp6!=0
						do error06in with 'Falta Perc./Ret. Gcias :'+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'
						skip in fiv
						loop
					case f00.p0prv2=2 and fiv.d5imp7!=0
						do error06in with 'Falta Perc./Ret. Gcias :'+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'
						skip in fiv
						loop
					case f00.p0prv3=2 and fiv.d5imp8!=0
						do error06in with 'Falta Perc./Ret. Gcias :'+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'
						skip in fiv
						loop
				endcase
			endif
			
			if !empty(fiv.d5cpot)
*				=SEEK("JU" + fiv.d5juri, "f01")	&& R.02 Mariano
*				cafip	= f01.d1tdgi	&& R.02 Mariano
				do case
					case f00.p0prv1=3 and fiv.d5imp6!=0
*						if !seek(cafip+fiv.d5cpot+fiv.d5prco,'f25') and fiv.d5cpot!='000'	&& R.01 Mariano
*						if !seek(cafip+fiv.d5cpot+iif(empty(fiv.d5imre),'P','R'),'f25') and fiv.d5cpot!='000'	&& R.01 Mariano	&& R.02 Mariano
*						if !seek(fiv.d5cpot+'O'+iif(empty(fiv.d5imre),'P','R'),'f24') and fiv.d5cpot!='000'		&& R.02 Mariano	&& R.07 Mariano
						if !seek(fiv.d5cpot+'O'+iif(fiv.d5tico!='R','P','R'),'f24') and fiv.d5cpot!='000'		&& R.07 Mariano
							do error06in with 'Perc./Ret. Otros inexistente :'+fiv.d5cpot+' '+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'
							skip in fiv
							loop
						endif
					case f00.p0prv2=3 and fiv.d5imp7!=0
*						if !seek(cafip+fiv.d5cpot+fiv.d5prco,'f25') and fiv.d5cpot!='000'	&& R.01 Mariano
*						if !seek(cafip+fiv.d5cpot+iif(empty(fiv.d5imre),'P','R'),'f25') and fiv.d5cpot!='000'	&& R.01 Mariano	&& R.02 Mariano
*						if !seek(fiv.d5cpot+'O'+iif(empty(fiv.d5imre),'P','R'),'f24') and fiv.d5cpot!='000'		&& R.02 Mariano	&& R.07 Mariano
						if !seek(fiv.d5cpot+'O'+iif(fiv.d5tico!='R','P','R'),'f24') and fiv.d5cpot!='000'		&& R.07 Mariano
							do error06in with 'Perc./Ret. Otros inexistente :'+fiv.d5cpot+' '+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'
							skip in fiv
							loop
						endif
					case f00.p0prv3=3 and fiv.d5imp8!=0
*						if !seek(cafip+fiv.d5cpot+fiv.d5prco,'f25') and fiv.d5cpot!='000'	&& R.01 Mariano
*						if !seek(cafip+fiv.d5cpot+iif(empty(fiv.d5imre),'P','R'),'f25') and fiv.d5cpot!='000'	&& R.01 Mariano	&& R.02 Mariano
*						if !seek(fiv.d5cpot+'O'+iif(empty(fiv.d5imre),'P','R'),'f24') and fiv.d5cpot!='000'		&& R.02 Mariano	&& R.07 Mariano
						if !seek(fiv.d5cpot+'O'+iif(fiv.d5tico!='R','P','R'),'f24') and fiv.d5cpot!='000'		&& R.07 Mariano
							do error06in with 'Perc./Ret. Otros inexistente :'+fiv.d5cpot+' '+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'
							skip in fiv
							loop
						endif
					otherwise
						do error06in with 'Falta definir Columna para Otros o no hay importe Otros:'+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'
						skip in fiv
						loop
				endcase
			else
				do case
					case f00.p0prv1=3 and fiv.d5imp6!=0
						do error06in with 'Falta Perc./Ret. Otros :'+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'
						skip in fiv
						loop
					case f00.p0prv2=3 and fiv.d5imp7!=0
						do error06in with 'Falta Perc./Ret. Otros :'+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'
						skip in fiv
						loop
					case f00.p0prv3=3 and fiv.d5imp8!=0
						do error06in with 'Falta Perc./Ret. Otros :'+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'
						skip in fiv
						loop
				endcase
			endif
			&& R.00 Taboada	

			&& R.03b Mariano
			if fiv.d5tifo='C' and (fiv.d5imp4!=0 or fiv.d5imp5!=0)
				do error06in with 'No puede tener IVA el Formulario:'+fiv.d5tifo+' Comp:'+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'
				skip in fiv
				loop
			endif
			&& R.03e Mariano

			if fiv.D5imp1+fiv.D5imp2+fiv.D5imp3+fiv.D5imp4+fiv.D5imp5+fiv.D5imp6+fiv.D5imp7+fiv.D5imp8!=fiv.d5tota
				do error06in with 'Diferencia entre la suma de los importes y el total'+' Comp:'+tcabre(fiv.d5tico)+' '+fiv.d5tifo+' '+fiv.d5sucu+'-'+fiv.d5nume,'V'
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

				if d0imfa$'S' or !empty(m.d7numori)		&& R.01 Mariano (agregue d7numori)
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
				
				&& R.02b Mariano
				if fiv.d5tico='R'
					store 0 to m.d5imp3,m.d5imp6,m.d5imp7,m.d5imp8
				endif
				&& R.02e Mariano

				&& R.08b Mariano
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
				&& R.08e Mariano

				gather memvar

				skip in fiv
			enddo

			wait wind 'Finalizo la incorporaci๓n.' time 1	&& R.04 Mariano
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
*		clear read
		w_prve=.f.	&& R.01 Mariano
	endif	&& R.01 Mariano
	
	if w_prco and !empty(reccount('fic'))	&& R.01 Mariano
		sele fic
		go top
		do while !eof('fic')
	
			if seek(fic.d5tipo+fic.d5sucu+fic.d5tifo+fic.d5tico+fic.d5nume+fic.d5clie,'f05')
				
				do while fic.d5tipo+fic.d5sucu+fic.d5tifo+fic.d5tico+fic.d5nume+fic.d5clie=f05.d5tipo+f05.d5sucu+f05.d5tifo+f05.d5tico+f05.d5nume+f05.d5clie and !eof('f05')
					if fic.d5peri=f05.d5peri and fic.d5inte!=f05.d5inte
						do error06in with 'Comprobante existente :'+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5nume,'C'
						exit
					endif
					skip in f05
				enddo
				
				skip in fic
				loop
			endif

			if !seek(fic.d5clie,'f03')
				do error06in with 'Falta condici๓n impositiva o Proveedor inexistente :'+fic.d5clie+' Comp:'+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5nume,'C'	
				skip in fic
				loop
			endif
						
			&& R.03b Mariano							
			if fic.d6tido$'80.86.87'
				if dicuit(left(fic.d6cuit,11))<>right(fic.d6cuit,1) and !empty(strtran(fic.d6cuit,'-',''))
					do error06in with 'Error en CUIT o CUIL o CDI :'+fic.d6cuit+' '+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5nume,'C'	
					skip in fic
					loop
				endif
			endif
			&& R.03e Mariano
							
			if !seek(ptoi(fic.d5peri),'F04')
				do error06in with 'Periodo inexistente :'+fic.d5peri+' Comp:'+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5nume,'C'
				skip in fic
				loop
			endif
			
			*if fic.d5peri)!=subs(dtoc(fic.d5fech),4,7))
			if left(dtos(fic.d5fech),6)>ptoi(fic.d5peri)
				do error06in with 'La fecha no corresponde al periodo :'+dtoc(fic.d5fech)+' Comp:'+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5nume,'C'
				skip in fic
				loop
			endif
			
			
			if seek(ptoi(fic.d5peri),'f04') and f04.d4esco='C'
				do error06in with 'Periodo cerrado :'+fic.d5peri+' Comp:'+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5nume,'C'
				skip in fic
				loop
			endif
						
			if !seek(fic.d5alic,'f23')
				do error06in with 'Alicuota de iva inexistente :'+fic.d5alic+' Comp:'+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5nume,'C'
				skip in fic
				loop
			endif
	
			if !seek('JU'+fic.d5juri,'f01')
				do error06in with 'Jurisdicci๓n inexistente :'+fic.d5juri+' Comp:'+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5nume,'C'
				skip in fic
				loop
			endif
									
			if fic.d5tiim != ftiimp(fic.d5clie, fic.d5peri,'P')

				*do error06in with 'La cond imp del prov: '+fic.d5clie+' no coincide con la del tipo de'+' Comp:'+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5nume,'C'	
				do error06in with 'La cond. imp. del prov: '+fic.d5clie+' no corresponde'+' Comp:'+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5sucu+'-'+fic.d5nume,'C'	
				skip in fic
				loop
			endif
			
			
*			if !fic.d5tifo$iif(fic.d5tiim$'1','ABICM ','BCI ')	&& R.03 Mariano
			if !fic.d5tifo$iif(fic.d5tiim$'1','ABICM ','BCI ') or  !fic.d5tifo$tiformC(fic.d5tico) && R.03 Mariano
				do error06in with 'Formulario incorrecto :'+fic.d5tifo+' Comp:'+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5nume,'C'
				skip in fic
				loop
			endif
			
			
			if !seek('RC'+fic.d5rubr,'f01')
				do error06in with 'Rubro inexistente :'+fic.d5rubr+' Comp:'+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5nume,'C'
				skip in fic
				loop
			endif

			&& R.00b Taboada (Perc o Ret)
			if !empty(fic.d5pere) or fic.d5imp3!=0
*				if seek(fic.d5pere,'f24')	&& R.01 Mariano
*				if seek(fic.d5pere+'I'+iif(empty(fic.d5pere),'P','R'),'f24')	&& R.01 Mariano	&& R.02 Mariano
*				if seek(fic.d5pere+'I'+iif(empty(fic.d5imre),'P','R'),'f24')	&& R.02 Mariano	&& R.07 Mariano
				if seek(fic.d5pere+'I'+iif(fic.d5tico!='R','P','R'),'f24')		&& R.07 Mariano
*					if f24.d24codi!='000' and f24.d24tipo!='I' or f24.d24refe!='P'	&& R.01 Mariano
*					if f24.d24codi!='000' or f24.d24tipo!='I' or f24.d24refe!=iif(empty(fic.d5pere),'P','R')	&& R.01 Mariano	&& R.02 Mariano
*					if f24.d24codi!='000' or f24.d24tipo!='I' or f24.d24refe!=iif(empty(fic.d5imre),'P','R')	&& R.02 Mariano	&& R.07 Mariano
*					if f24.d24codi!='000' or f24.d24tipo!='I' or f24.d24refe!=iif(fic.d5tico!='R','P','R')		&& R.07 Mariano	&& R.09 Mariano
					if f24.d24tipo!='I' or f24.d24refe!=iif(fic.d5tico!='R','P','R')		&& R.07 Mariano	&& R.09 Mariano
						do error06in with 'Perc./Ret. Iva no corresponde :'+fic.d5pere+' '+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5sucu+'-'+fic.d5nume,'C'
						skip in fic
						loop
					endif		
				else
					do error06in with 'Perc./Ret. Iva inexistente :'+fic.d5pere+' '+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5sucu+'-'+fic.d5nume,'C'
					skip in fic
					loop
				Endif
			endif
			
			if !empty(fic.d5cpib)
				=SEEK("JU" + fic.d5juri, "f01")
				cafip	= f01.d1tdgi
				do case
					case f00.p0prc1=1 and fic.d5imp6!=0
*						if !seek(cafip+fic.d5cpib+fic.d5regi,'f25') and fic.d5cpib!='000'	&& R.01 Mariano
*						if !seek(cafip+fic.d5cpib+iif(empty(fic.d5cpib),'P','R'),'f25') and fic.d5cpib!='000'	&& R.01 Mariano	&& R.02 Mariano
*						if !seek(cafip+fic.d5cpib+iif(empty(fic.d5imre),'P','R'),'f25') and fic.d5cpib!='000'	&& R.02 Mariano	&& R.07 Mariano
						if !seek(cafip+fic.d5cpib+iif(fic.d5tico!='R','P','R'),'f25') and fic.d5cpib!='000'		&& R.07 Mariano
							do error06in with 'Perc./Ret. IB inexistente :'+fic.d5cpib+' '+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5sucu+'-'+fic.d5nume,'C'
							skip in fic
							loop
						endif
					case f00.p0prc2=1 and fic.d5imp7!=0
*						if !seek(cafip+fic.d5cpib+fic.d5regi,'f25') and fic.d5cpib!='000'	&& R.01 Mariano
*						if !seek(cafip+fic.d5cpib+iif(empty(fic.d5cpib),'P','R'),'f25') and fic.d5cpib!='000'	&& R.01 Mariano	&& R.02 Mariano
*						if !seek(cafip+fic.d5cpib+iif(empty(fic.d5imre),'P','R'),'f25') and fic.d5cpib!='000'	&& R.02 Mariano	&& R.07 Mariano
						if !seek(cafip+fic.d5cpib+iif(fic.d5tico!='R','P','R'),'f25') and fic.d5cpib!='000'		&& R.07 Mariano
							do error06in with 'Perc./Ret. IB inexistente :'+fic.d5cpib+' '+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5sucu+'-'+fic.d5nume,'C'
							skip in fic
							loop
						endif
					case f00.p0prc3=1 and fic.d5imp8!=0
*						if !seek(cafip+fic.d5cpib+fic.d5regi,'f25') and fic.d5cpib!='000'	&& R.01 Mariano
*						if !seek(cafip+fic.d5cpib+iif(empty(fic.d5cpib),'P','R'),'f25') and fic.d5cpib!='000'	&& R.01 Mariano	&& R.02 Mariano
*						if !seek(cafip+fic.d5cpib+iif(empty(fic.d5imre),'P','R'),'f25') and fic.d5cpib!='000'	&& R.02 Mariano	&& R.07 Mariano
						if !seek(cafip+fic.d5cpib+iif(fic.d5tico!='R','P','R'),'f25') and fic.d5cpib!='000'		&& R.07 Mariano
							do error06in with 'Perc./Ret. IB inexistente :'+fic.d5cpib+' '+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5sucu+'-'+fic.d5nume,'C'
							skip in fic
							loop
						endif
					otherwise
						do error06in with 'Falta definir Columna para IB o no hay importe IB :'+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5sucu+'-'+fic.d5nume,'C'
						skip in fic
						loop
				endcase
			else
				do case
					case f00.p0prc1=1 and fic.d5imp6!=0
						do error06in with 'Falta Perc./Ret. IB :'+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5sucu+'-'+fic.d5nume,'C'
						skip in fic
						loop
					case f00.p0prc2=1 and fic.d5imp7!=0
						do error06in with 'Falta Perc./Ret. IB :'+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5sucu+'-'+fic.d5nume,'C'
						skip in fic
						loop
					case f00.p0prc3=1 and fic.d5imp8!=0
						do error06in with 'Falta Perc./Ret. IB :'+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5sucu+'-'+fic.d5nume,'C'
						skip in fic
						loop
				endcase
			endif
			
			if !empty(fic.d5cpga)
*				=SEEK("JU" + fic.d5juri, "f01")	&& R.02 Mariano
*				cafip	= f01.d1tdgi	&& R.02 Mariano
				do case
					case f00.p0prc1=2 and fic.d5imp6!=0
*						if !seek(cafip+fic.d5cpga+fic.d5rega,'f25') and fic.d5cpga!='000'	&& R.01 Mariano
*						if !seek(cafip+fic.d5cpga+iif(empty(fic.d5cpag),'P','R'),'f25') and fic.d5cpga!='000'	&& R.01 Mariano	&& R.02 Mariano
*						if !seek(fic.d5cpga+'G'+iif(empty(fic.d5imre),'P','R'),'f24') and fic.d5cpga!='000'		&& R.02 Mariano	&& R.07 Mariano
						if !seek(fic.d5cpga+'G'+iif(fic.d5tico!='R','P','R'),'f24') and fic.d5cpga!='000'		&& R.07 Mariano
							do error06in with 'Perc./Ret. Gcias inexistente :'+fic.d5cpga+' '+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5sucu+'-'+fic.d5nume,'C'
							skip in fic
							loop
						endif
					case f00.p0prc2=2 and fic.d5imp7!=0
*						if !seek(cafip+fic.d5cpga+fic.d5rega,'f25') and fic.d5cpga!='000'	&& R.01 Mariano
*						if !seek(cafip+fic.d5cpga+iif(empty(fic.d5cpag),'P','R'),'f25') and fic.d5cpga!='000'	&& R.01 Mariano	&& R.02 Mariano
*						if !seek(fic.d5cpga+'G'+iif(empty(fic.d5imre),'P','R'),'f24') and fic.d5cpga!='000'		&& R.02 Mariano	&& R.07 Mariano
						if !seek(fic.d5cpga+'G'+iif(fic.d5tico!='R','P','R'),'f24') and fic.d5cpga!='000'		&& R.07 Mariano
							do error06in with 'Perc./Ret. Gcias inexistente :'+fic.d5cpga+' '+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5sucu+'-'+fic.d5nume,'C'
							skip in fic
							loop
						endif
					case f00.p0prc3=2 and fic.d5imp8!=0
*						if !seek(cafip+fic.d5cpga+fic.d5rega,'f25') and fic.d5cpga!='000'	&& R.01 Mariano
*						if !seek(cafip+fic.d5cpga+iif(empty(fic.d5cpag),'P','R'),'f25') and fic.d5cpga!='000'	&& R.01 Mariano	&& R.02 Mariano
*						if !seek(fic.d5cpga+'G'+iif(empty(fic.d5imre),'P','R'),'f24') and fic.d5cpga!='000'		&& R.02 Mariano	&& R.07 Mariano
						if !seek(fic.d5cpga+'G'+iif(fic.d5tico!='R','P','R'),'f24') and fic.d5cpga!='000'		&& R.07 Mariano
							do error06in with 'Perc./Ret. Gcias inexistente :'+fic.d5cpga+' '+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5sucu+'-'+fic.d5nume,'C'
							skip in fic
							loop
						endif
					otherwise
						do error06in with 'Falta definir Columna para Gcias o no hay importe Gcias :'+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5sucu+'-'+fic.d5nume,'C'
						skip in fic
						loop
				endcase
			else
				do case
					case f00.p0prc1=2 and fic.d5imp6!=0
						do error06in with 'Falta Perc./Ret. Gcias :'+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5sucu+'-'+fic.d5nume,'C'
						skip in fic
						loop
					case f00.p0prc2=2 and fic.d5imp7!=0
						do error06in with 'Falta Perc./Ret. Gcias :'+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5sucu+'-'+fic.d5nume,'C'
						skip in fic
						loop
					case f00.p0prc3=2 and fic.d5imp8!=0
						do error06in with 'Falta Perc./Ret. Gcias :'+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5sucu+'-'+fic.d5nume,'C'
						skip in fic
						loop
				endcase
			endif
			
			if !empty(fic.d5cpot)
*				=SEEK("JU" + fic.d5juri, "f01")	&& R.02 Mariano
*				cafip	= f01.d1tdgi	&& R.02 Mariano
				do case
					case f00.p0prc1=3 and fic.d5imp6!=0
*						if !seek(cafip+fic.d5cpot+fic.d5prco,'f25') and fic.d5cpot!='000'	&& R.01 Mariano
*						if !seek(cafip+fic.d5cpot+iif(empty(fic.d5cpot),'P','R'),'f25') and fic.d5cpot!='000'	&& R.01 Mariano	&& R.02 Mariano
*						if !seek(fic.d5cpot+'O'+iif(empty(fic.d5imre),'P','R'),'f24') and fic.d5cpot!='000'	&& R.02 Mariano	&& R.07 Mariano
						if !seek(fic.d5cpot+'O'+iif(fic.d5tico!='R','P','R'),'f24') and fic.d5cpot!='000'	&& R.07 Mariano
							do error06in with 'Perc./Ret. Otros inexistente :'+fic.d5cpot+' '+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5sucu+'-'+fic.d5nume,'C'
							skip in fic
							loop
						endif
					case f00.p0prc2=3 and fic.d5imp7!=0
*						if !seek(cafip+fic.d5cpot+fic.d5prco,'f25') and fic.d5cpot!='000'	&& R.01 Mariano
*						if !seek(cafip+fic.d5cpot+iif(empty(fic.d5cpot),'P','R'),'f25') and fic.d5cpot!='000'	&& R.01 Mariano	&& R.02 Mariano
*						if !seek(fic.d5cpot+'O'+iif(empty(fic.d5imre),'P','R'),'f24') and fic.d5cpot!='000'	&& R.02 Mariano	&& R.07 Mariano
						if !seek(fic.d5cpot+'O'+iif(fic.d5tico!='R','P','R'),'f24') and fic.d5cpot!='000'	&& R.07 Mariano
							do error06in with 'Perc./Ret. Otros inexistente :'+fic.d5cpot+' '+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5sucu+'-'+fic.d5nume,'C'
							skip in fic
							loop
						endif
					case f00.p0prc3=3 and fic.d5imp8!=0
*						if !seek(cafip+fic.d5cpot+fic.d5prco,'f25') and fic.d5cpot!='000'	&& R.01 Mariano
*						if !seek(cafip+fic.d5cpot+iif(empty(fic.d5cpot),'P','R'),'f25') and fic.d5cpot!='000'	&& R.01 Mariano	&& R.02 Mariano
*						if !seek(fic.d5cpot+'O'+iif(empty(fic.d5imre),'P','R'),'f24') and fic.d5cpot!='000'		&& R.02 Mariano	&& R.07 Mariano
						if !seek(fic.d5cpot+'O'+iif(fic.d5tico!='R','P','R'),'f24') and fic.d5cpot!='000'		&& R.07 Mariano
							do error06in with 'Perc./Ret. Otros inexistente :'+fic.d5cpot+' '+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5sucu+'-'+fic.d5nume,'C'
							skip in fic
							loop
						endif
					otherwise
						do error06in with 'Falta definir Columna para Otros o no hay importe Otros :'+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5sucu+'-'+fic.d5nume,'C'
						skip in fic
						loop
				endcase
			else
				do case
					case f00.p0prc1=3 and fic.d5imp6!=0
						do error06in with 'Falta Perc./Ret. Otros :'+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5sucu+'-'+fic.d5nume,'C'
						skip in fic
						loop
					case f00.p0prc2=3 and fic.d5imp7!=0
						do error06in with 'Falta Perc./Ret. Otros :'+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5sucu+'-'+fic.d5nume,'C'
						skip in fic
						loop
					case f00.p0prc3=3 and fic.d5imp8!=0
						do error06in with 'Falta Perc./Ret. Otros :'+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5sucu+'-'+fic.d5nume,'C'
						skip in fic
						loop
				endcase
			endif
			
			if !empty(fic.d5adua)
				if !seek('AD'+fic.d5adua,'f01t') and !empty(fic.d5adua)	&& R.01 Mariano (fix.d5adua!='000')
					do error06in with 'Codigo de Aduana inexistente :'+fic.d5adua+' Comp:'+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5nume,'C'
					skip in fic
					loop
				endif
			endif
			
			if !empty(fic.d5dest)
				if !seek('DT'+fic.d5dest,'f01t') and !empty(fic.d5dest)	&& R.01 Mariano (fic.d5dest!='0000')
					do error06in with 'Codigo de Dest de Importacion inexistente :'+fic.d5dest+' Comp:'+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5nume,'C'
					skip in fic
					loop
				endif
			endif
			
			&& R.00 Taboada

			&& R.03b Mariano
			if fic.d5tifo='C' and (fic.d5imp4!=0 or fic.d5imp5!=0)
				do error06in with 'No puede tener IVA el Formulario:'+fic.d5tifo+' Comp:'+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5nume,'C'
				skip in fic
				loop
			endif
			&& R.03e Mariano

			if fic.D5imp1+fic.D5imp2+fic.D5imp3+fic.D5imp4+fic.D5imp5+fic.D5imp6+fic.D5imp7+fic.D5imp8!=fic.d5tota
				do error06in with 'Diferencia entre la suma de los importes y el total'+' Comp:'+tcabre(fic.d5tico)+' '+fic.d5tifo+' '+fic.d5nume,'C'
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

				if d0imfa$'S' or !empty(m.d7numori)		&& R.01 Mariano	(agregue d7numori)
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

				&& R.02b Mariano
				if fic.d5tico='R'
					store 0 to m.d5imp3,m.d5imp6,m.d5imp7,m.d5imp8
				endif
				&& R.02e Mariano

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

			wait wind 'Finalizo la incorporaci๓n.' time 1	&& R.04 Mariano
			wait clear
		else
			do while sino('Imprime Errores Compras', 'SI', w_title)='S'	
				=skipbar('registro','Elegir',.t.)
				do getdest.spr
				=skipbar('registro','Elegir',.f.)
			enddo	

			sele f9e 
			zap

		endif
*		clear read
		w_prco=.f.	&& R.01 Mariano
	endif

	wait clear

endif

return


*-P อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
Procedure Error06in
*-P อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
Param w_text,w_tipo
=set_alia('f9e')
=net_appe('f9e')
repl f9e.d99mens with w_text, f9e.d99tipo with w_tipo
=rest_alia()
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc genera06in		&& genera los intexxx a partir de planillas Excel
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

private wwfiles


if w_prve	&& R.01 Mariano

dimension wwfiles(1)
wwfiles=''
*wwcant=a_dir('wwfiles',wdici+'\'+d0coem+'\VENTAS*.XLS')		&& R.01 Mariano
wwcant=a_dir('wwfiles',wdici+'\VENTAS*.XLS?','','',.t.)  	&& R.01 Mariano
wfinu=0

if used('fivx')
	sele fivx
else
	sele 0
	use &d0coem.\intevtas alias fivx exclusive
endif	
for wfinu=1 to wwcant
	wait wind 'Procesando Interfase Ventas...' nowait

*	w_fire='"'+wdici+'\'+d0coem+'\'+wwfiles[wfinu,1]+'"'	&& R.01 Mariano
	w_fire=wdici+'\'+wwfiles[wfinu,1]	&& R.01 Mariano
	&& R.01b Mariano
*	if version()='Visual FoxPro'
*		append from &w_fire type xls
*	else
	&& R.01e Mariano
		use in fivx
		&& R.01b Mariano
*		private w_vfox,w_inst001,w_inst002,w_inst003
*		w_inst001='use '+sys(2003)+'\'+d0coem+'\intecpra.dbf in 0 alias ficx exclusive'
*		w_inst002='select ficx'
*		w_inst003='append from '+w_fire+' type xls'
		=seterr('OFF')
		private w_vfox
		&& R.01e Mariano

		private d0f9ve
		d0f9ve=sys(2003)+'\I'+d0coem+p_netuser+'ve'
		w_vfox=d0f9ve+'.mem'
		save to &w_vfox. all like w_inst*
		release all like w_inst*
	
		&& R.01b Mariano
*		=visuafox('ejeproc(.f.)',w_vfox)
		=visuafox("intiwxls('"+w_fire+"','"+"V"+"')",w_vfox, '', 5)	
		=seterr('ON')
	
		=inkey(2)
*		endif
		&& R.01e Mariano

		if file(w_vfox)
			erase &w_vfox.
		endif

*		=inkey(2)	&& R.01 Mariano
		w_paso=1
		do while .t.
			fieldvec[1]='fivx'
			use &d0coem.\intevtas alias fivx exclusive
			
			if neterr()
				=advsuave('Procesando archivo Excel '+justfname(w_fire),w_title)
				fieldvec=''
				=inkey(2)	&& R.08 Mariano
			endif
			fieldvec=''
			
			if alias()='fivx' or w_paso>=5
				exit
			endif
			w_paso=w_paso+1
		enddo
*	endif	&& R.01 Mariano
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
	&& R.01b Mariano
	sele 0
	use fi\00\ivf007
	copy structure extended to &d0coem.\inte007x.dbf fields d7numori
	use
	&& R.01e Mariano
	sele f05x
	append from  &d0coem.\inte006x.dbf
	append from  &d0coem.\inte007x.dbf	&& R.01 Mariano
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
	dele file &d0coem.\inte007x.dbf	&& R.01 Mariano
endif

&& R.04b Mariano
*sele 0
*use &d0coem.\inte005v in 0 alias fiv exclusive	&& Interfase Ventas
if used('fiv')
	sele fiv
	use &d0coem.\inte005v alias fiv exclusive	&& Interfase Ventas
else
	sele 0
	use &d0coem.\inte005v alias fiv exclusive	&& Interfase Ventas
endif
&& R.04e Mariano
zap

SELE fiv

if used('fivx')	&& R.08 Mariano

GO TOP IN fivx
DO WHILE !EOF('fivx')

	wait wind 'Procesando Interfase Ventas...' nowait

	if empty(ctod(fivx.d5fech)) or empty(fivx.d5numd) or fivx.d5numd='N๚mero d'
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

		if f23.d23alic=fivx.d5por1
	    	replace fiv.d5alic with f23.D23codi
		ENDIF
 
		SKIP IN F23
	ENDDO

&& R.00b Taboada
*****	Jurisdiccion	*****

	
*	GO TOP IN F01	&& R.03 Mariano
&& R.07b Mariano
*	=seek('JU','f01')	&& R.03 Mariano
*	DO WHILE !EOF('f01') and f01.d1tip='JU'	&& R.03 Mariano (agregue d1tip='JU')

*		if f01.d1des=fivx.d5juri
*	    	replace fiv.d5juri with f01.D1cla
*	    	exit 	&& R.03 Mariano
*		ENDIF
 
*		SKIP IN F01
*	ENDDO
&& R.07e Mariano
&& R.00e Taboada		
*****	Comp.Generico	*****

	replace fiv.D5fech with CTOD(fivx.d5fech),;
			fiv.D5empr with d0coem,;
			fiv.d5titr with '',;
			fiv.d5ttip with '0',;
			fiv.D5peri with wdperi,;
			fiv.D5inte with Padl(alltr(str(ld5inte,6)), 6,'0'),;
			fiv.D5nume with subs(fivx.d5numd,1,8)+'/'+subs(fivx.d5numh,1,8),;
			fiv.D5tifo with Fivx.d5tifo,;
			fiv.D5sucu with padl(allt(fivx.d5sucu),5,'0'),;
			fiv.d5clie with fivx.d5clie,;
			fiv.D5por1 with fivx.d5por1,;
			fiv.D5imp1 with fivx.d5imp1,;
			fiv.D5imp2 with fivx.d5imp2,;
			fiv.D5imp3 With fivx.d5imp3,;
			fiv.D5imp4 With fivx.d5imp4,;
			fiv.D5imp6 With fivx.d5imp6,;
			fiv.D5imp7 With fivx.d5imp7,;
			fiv.D5imp8 With fivx.d5imp8,;
			fiv.D5tota with fivx.d5Tota,;
			fiv.D5imre with	fivx.d5imre,;
			fiv.d7numori with fivx.d7numori,;
			fiv.D5pere with fivx.d5pere,;
			fiv.D5cpib with fivx.d5cpib,;
			fiv.d5cpga with fivx.d5cpga,;
			fiv.d5cpot with fivx.d5cpot		&& R.00 Taboada (se agregaron campos)	&& R.01 Mariano (d7numori)	&& R.10 Mariano (ajute d5sucu por 5 digitos)
	
	
	DO CASE	
*****	Comp.Tique Z	*****

		CASE fivx.d5tico='TIQ' and fivx.d5tifo='Z'
			replace fiv.D5tifo with 'Z',;
					fiv.D5tico with '?',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('?', 'Z'),;
					fiv.D5imfi with 'S',;
					fiv.D5nume with subs(fivx.d5numd,1,8)+'/'+subs(fivx.d5numh,1,8),;
					fiv.D5clie with '99993',;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '3',;
					fiv.D5tiop with '0'

*****	Comp.Tique T	*****

		CASE fivx.d5tico='TIQ' and fivx.d5tifo='T'
			replace fiv.D5tifo with 'T',;
					fiv.D5tico with '?',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('?', 'T'),;
					fiv.D5imfi with 'S',;
					fiv.D5nume with subs(fivx.d5numd,1,8)+'/'+subs(fivx.d5numh,1,8),;
					fiv.D5clie with '99993',;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '3',;
					fiv.D5tiop with '0'
							
*****	Comp.Tique factura A	*****

		CASE fivx.d5tico='TIF' and fivx.d5tifo='A'
			replace fiv.D5tifo with 'A',;
					fiv.D5tico with '6',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('6', 'A'),;
					fiv.D5imfi with 'S',;
					fiv.D5nume with subs(fivx.d5numd,1,8)+'/'+iif(fivx.d5numd<fivx.d5numh, subs(fivx.d5nume,1,8), ''),;
					fiv.D5clie with '99991',;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '1',;
					fiv.D5tiop with '0'

*****	Comp. Tiquet factura B	*****

		CASE fivx.d5tico='TIF' and fivx.d5tifo='B'
			replace fiv.D5tifo with 'B',;
					fiv.D5tico with '6',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('6', 'B'),;
					fiv.D5imfi with 'S',;
					fiv.D5nume with subs(fivx.d5numd,1,8)+'/'+iif(fivx.d5numd<fivx.d5numh, subs(fivx.d5numh,1,8), ''),;
					fiv.D5clie with '99993',;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '1',;
		 			fiv.D5tiop with '0'

*****	Comp. Tiquet factura C	*****

		CASE fivx.d5tico='TIF' and fivx.d5tifo='C'
			replace fiv.D5tifo with 'C',;
					fiv.D5tico with '6',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('6', 'C'),;
					fiv.D5imfi with 'S',;
					fiv.D5nume with subs(fivx.d5numd,1,8)+'/'+iif(fivx.d5numd<fivx.d5numh, subs(fivx.d5numh,1,8), ''),;
					fiv.D5clie with '99991',;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '1',;
		 			fiv.D5tiop with '0'

*****	Comp. Tiquet factura M	*****

		CASE fivx.d5tico='TIF' and fivx.d5tifo='M'
			replace fiv.D5tifo with 'M',;
					fiv.D5tico with '6',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('6', 'M'),;
					fiv.D5imfi with 'S',;
					fiv.D5nume with subs(fivx.d5numd,1,8)+'/'+iif(fivx.d5numd<fivx.d5numh, subs(fivx.d5numh,1,8), ''),;
					fiv.D5clie with '99991',;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '1',;
		 			fiv.D5tiop with '0'
		
*****	Comp.Tique NC A	*****

		CASE fivx.d5tico='TIC' and fivx.d5tifo='A'
			replace fiv.D5tifo with 'A',;
					fiv.D5tico with 'E',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('E', 'A'),;
					fiv.D5imfi with 'S',;
					fiv.D5nume with subs(fivx.d5numd,1,8)+'/'+iif(fivx.d5numd<fivx.d5numh, subs(fivx.d5numh,1,8), ''),;
					fiv.D5clie with '99991',;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '1',;
					fiv.D5tiop with '0'

*****	Comp. Tiquet NC B	*****

		CASE fivx.d5tico='TIC' and fivx.d5tifo='B'
			replace fiv.D5tifo with 'B',;
					fiv.D5tico with 'E',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('E', 'B'),;
					fiv.D5imfi with 'S',;
					fiv.D5nume with subs(fivx.d5numd,1,8)+'/'+iif(fivx.d5numd<fivx.d5numh, subs(fivx.d5numh,1,8), ''),;
					fiv.D5clie with '99993',;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '3',;
		 			fiv.D5tiop with '0'

*****	Comp. Tiquet NC C	*****

		CASE fivx.d5tico='TIC' and fivx.d5tifo='C'
			replace fiv.D5tifo with 'C',;
					fiv.D5tico with 'E',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('E', 'C'),;
					fiv.D5imfi with 'S',;
					fiv.D5nume with subs(fivx.d5numd,1,8)+'/'+iif(fivx.d5numd<fivx.d5numh, subs(fivx.d5numh,1,8), ''),;
					fiv.D5clie with '99991',;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '1',;
		 			fiv.D5tiop with '0'
		
*****	Comp. Tiquet NC M	*****

		CASE fivx.d5tico='TIC' and fivx.d5tifo='M'
			replace fiv.D5tifo with 'M',;
					fiv.D5tico with 'E',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('E', 'M'),;
					fiv.D5imfi with 'S',;
					fiv.D5nume with subs(fivx.d5numd,1,8)+'/'+iif(fivx.d5numd<fivx.d5numh, subs(fivx.d5numh,1,8), ''),;
					fiv.D5clie with '99991',;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '1',;
		 			fiv.D5tiop with '0'

*****	Comp.Tique ND A	*****

		CASE fivx.d5tico='TID' and fivx.d5tifo='A'
			replace fiv.D5tifo with 'A',;
					fiv.D5tico with 'D',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('D', 'A'),;
					fiv.D5imfi with 'S',;
					fiv.D5nume with subs(fivx.d5numd,1,8)+'/'+iif(fivx.d5numd<fivx.d5numh, subs(fivx.d5numh,1,8), ''),;
					fiv.D5clie with '99991',;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '1',;
					fiv.D5tiop with '0'

*****	Comp. Tiquet ND B	*****

		CASE fivx.d5tico='TID' and fivx.d5tifo='B'
			replace fiv.D5tifo with 'B',;
					fiv.D5tico with 'D',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('D', 'B'),;
					fiv.D5imfi with 'S',;
					fiv.D5nume with subs(fivx.d5numd,1,8)+'/'+iif(fivx.d5numd<fivx.d5numh, subs(fivx.d5numh,1,8), ''),;
					fiv.D5clie with '99993',;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '3',;
		 			fiv.D5tiop with '0'
		
*****	Comp.Tique ND C	*****

		CASE fivx.d5tico='TID' and fivx.d5tifo='C'
			replace fiv.D5tifo with 'C',;
					fiv.D5tico with 'D',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('D', 'C'),;
					fiv.D5imfi with 'S',;
					fiv.D5nume with subs(fivx.d5numd,1,8)+'/'+iif(fivx.d5numd<fivx.d5numh, subs(fivx.d5numh,1,8), ''),;
					fiv.D5clie with '99991',;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '1',;
					fiv.D5tiop with '0'	
					
*****	Comp.Tique ND M	*****

		CASE fivx.d5tico='TID' and fivx.d5tifo='M'
			replace fiv.D5tifo with 'M',;
					fiv.D5tico with 'D',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('D', 'M'),;
					fiv.D5imfi with 'S',;
					fiv.D5nume with subs(fivx.d5numd,1,8)+'/'+iif(fivx.d5numd<fivx.d5numh, subs(fivx.d5numh,1,8), ''),;
					fiv.D5clie with '99991',;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '1',;
					fiv.D5tiop with '0'		
									
*****	Facturas A	*****

		CASE fivx.d5tico='FAC' and fivx.d5tifo='A'
			replace fiv.D5tifo with 'A',;
					fiv.D5tico with 'A',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('A', 'A'),;
					fiv.D5imfi with 'N',;
					fiv.D5nume with subs(fivx.d5numd,1,8)+'/'+iif(fivx.d5numd<fivx.d5numh, subs(fivx.d5numh,1,8), ''),;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '1',;
		 			fiv.D5tiop with '0'

*****	Facturas B	*****

		CASE fivx.d5tico='FAC' and fivx.d5tifo='B'
			replace fiv.D5tifo with 'B',;
					fiv.D5tico with 'A',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('A', 'B'),;
					fiv.D5imfi with 'N',;
					fiv.D5nume with subs(fivx.d5numd,1,8)+'/'+iif(fivx.d5numd<fivx.d5numh, subs(fivx.d5numh,1,8), ''),;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '3',;
		 			fiv.D5tiop with '0'

*****	Facturas E	*****

		CASE fivx.d5tico='FAC' and fivx.d5tifo='E'
			replace fiv.D5tifo with 'E',;
					fiv.D5tico with 'A',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('A', 'E'),;
					fiv.D5imfi with 'N',;
					fiv.D5nume with subs(fivx.d5numd,1,8)+'/'+iif(fivx.d5numd<fivx.d5numh, subs(fivx.d5numh,1,8), ''),;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '1',;
		 			fiv.D5tiop with '0'

&& R.03b Mariano
*****	Factura C	*****

		CASE fivx.d5tico='FAC' and fivx.d5tifo='C'
			replace fiv.D5tifo with 'C',;
					fiv.D5tico with 'A',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('A', 'C'),;
					fiv.D5imfi with 'S',;
					fiv.D5nume with subs(fivx.d5numd,1,8)+'/'+iif(fivx.d5numd<fivx.d5numh, subs(fivx.d5numh,1,8), ''),;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '1',;
		 			fiv.D5tiop with '0'
&& R.03e Mariano

*****	Facturas M	*****

		CASE fivx.d5tico='FAC' and fivx.d5tifo='M'
			replace fiv.D5tifo with 'M',;
					fiv.D5tico with 'A',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('A', 'M'),;
					fiv.D5imfi with 'N',;
					fiv.D5nume with subs(fivx.d5numd,1,8)+'/'+iif(fivx.d5numd<fivx.d5numh, subs(fivx.d5numh,1,8), ''),;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '1',;
		 			fiv.D5tiop with '0'


*****	NOTA DE CREDITO A	*****

		CASE fivx.d5tico='CRE' and fivx.d5tifo='A'
			replace fiv.D5tifo with 'A',;
					fiv.D5tico with 'C',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('C', 'A'),;
					fiv.D5imfi with 'N',;
					fiv.D5nume with subs(fivx.d5numd,1,8)+'/'+iif(fivx.d5numd<fivx.d5numh, subs(fivx.d5numh,1,8), ''),;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '1',;
		 			fiv.D5tiop with '0'


*****	NOTA DE CREDITO B	*****

		CASE fivx.d5tico='CRE' and fivx.d5tifo='B'
			replace fiv.D5tifo with 'B',;
					fiv.D5tico with 'C',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('C', 'B'),;
					fiv.D5imfi with 'N',;
					fiv.D5nume with subs(fivx.d5numd,1,8)+'/'+iif(fivx.d5numd<fivx.d5numh, subs(fivx.d5numh,1,8), ''),;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '3',;
		 			fiv.D5tiop with '0'

*****	NOTA DE CREDITO E	*****

		CASE fivx.d5tico='CRE' and fivx.d5tifo='E'
			replace fiv.D5tifo with 'E',;
					fiv.D5tico with 'C',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('C', 'E'),;
					fiv.D5imfi with 'N',;
					fiv.D5nume with subs(fivx.d5numd,1,8)+'/'+iif(fivx.d5numd<fivx.d5numh, subs(fivx.d5numh,1,8), ''),;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '1',;
		 			fiv.D5tiop with '0'

&& R.03b Mariano
*****	NOTA DE CREDITO C	*****

		CASE fivx.d5tico='CRE' and fivx.d5tifo='C'
			replace fiv.D5tifo with 'C',;
					fiv.D5tico with 'C',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('C', 'C'),;
					fiv.D5imfi with 'N',;
					fiv.D5nume with subs(fivx.d5numd,1,8)+'/'+iif(fivx.d5numd<fivx.d5numh, subs(fivx.d5numh,1,8), ''),;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '1',;
		 			fiv.D5tiop with '0'
&& R.03e Mariano

*****	NOTA DE CREDITO M	*****

		CASE fivx.d5tico='CRE' and fivx.d5tifo='M'
			replace fiv.D5tifo with 'M',;
					fiv.D5tico with 'C',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('C', 'M'),;
					fiv.D5imfi with 'N',;
					fiv.D5nume with subs(fivx.d5numd,1,8)+'/'+iif(fivx.d5numd<fivx.d5numh, subs(fivx.d5numh,1,8), ''),;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '1',;
		 			fiv.D5tiop with '0'

*****	NOTA DE DEBITO A	*****

		CASE fivx.d5tico='DEB' and fivx.d5tifo='A'
			replace fiv.D5tifo with 'A',;
					fiv.D5tico with 'B',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('B', 'A'),;
					fiv.D5imfi with 'N',;
					fiv.D5nume with subs(fivx.d5numd,1,8)+'/'+iif(fivx.d5numd<fivx.d5numh, subs(fivx.d5numh,1,8), ''),;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '1',;
		 			fiv.D5tiop with '0'

*****	NOTA DE DEBITO B	*****

		CASE fivx.d5tico='DEB' and fivx.d5tifo='B'
			replace fiv.D5tifo with 'B',;
					fiv.D5tico with 'B',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('B', 'B'),;
					fiv.D5imfi with 'N',;
					fiv.D5nume with subs(fivx.d5numd,1,8)+'/'+iif(fivx.d5numd<fivx.d5numh, subs(fivx.d5numh,1,8), ''),;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '3',;
		 			fiv.D5tiop with '0'

*****	NOTA DE DEBITO E	*****

		CASE fivx.d5tico='DEB' and fivx.d5tifo='E'
			replace fiv.D5tifo with 'E',;
					fiv.D5tico with 'B',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('B', 'E'),;
					fiv.D5imfi with 'N',;
					fiv.D5nume with subs(fivx.d5numd,1,8)+'/'+iif(fivx.d5numd<fivx.d5numh, subs(fivx.d5numh,1,8), ''),;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '1',;
		 			fiv.D5tiop with '0'

&& R.03b Mariano
*****	NOTA DE DEBITO C	*****

		CASE fivx.d5tico='DEB' and fivx.d5tifo='C'
			replace fiv.D5tifo with 'C',;
					fiv.D5tico with 'B',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('B', 'C'),;
					fiv.D5imfi with 'N',;
					fiv.D5nume with subs(fivx.d5numd,1,8)+'/'+iif(fivx.d5numd<fivx.d5numh, subs(fivx.d5numh,1,8), ''),;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '1',;
		 			fiv.D5tiop with '0'
&& R.03e Mariano

*****	NOTA DE DEBITO M	*****

		CASE fivx.d5tico='DEB' and fivx.d5tifo='M'
			replace fiv.D5tifo with 'M',;
					fiv.D5tico with 'B',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('B', 'M'),;
					fiv.D5imfi with 'N',;
					fiv.D5nume with subs(fivx.d5numd,1,8)+'/'+iif(fivx.d5numd<fivx.d5numh, subs(fivx.d5numh,1,8), ''),;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '1',;
		 			fiv.D5tiop with '0'

*****	Recibo Factura A	*****

		CASE fivx.d5tico='REF' and fivx.d5tifo='A'
			replace fiv.D5tifo with 'A',;
					fiv.D5tico with '8',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('8', 'A'),;
					fiv.D5imfi with 'N',;
					fiv.D5nume with subs(fivx.d5numd,1,8)+'/'+iif(fivx.d5numd<fivx.d5numh, subs(fivx.d5numh,1,8), ''),;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '1',;
		 			fiv.D5tiop with '0'

*****	Recibo Factura B	*****

		CASE fivx.d5tico='REF' and fivx.d5tifo='B'
			replace fiv.D5tifo with 'B',;
					fiv.D5tico with '8',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('8', 'B'),;
					fiv.D5imfi with 'N',;
					fiv.D5nume with subs(fivx.d5numd,1,8)+'/'+iif(fivx.d5numd<fivx.d5numh, subs(fivx.d5numh,1,8), ''),;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '3',;
		 			fiv.D5tiop with '0'

*****	Recibo Factura E	*****

		CASE fivx.d5tico='REF' and fivx.d5tifo='E'
			replace fiv.D5tifo with 'E',;
					fiv.D5tico with '8',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('8', 'E'),;
					fiv.D5imfi with 'N',;
					fiv.D5nume with subs(fivx.d5numd,1,8)+'/'+iif(fivx.d5numd<fivx.d5numh, subs(fivx.d5numh,1,8), ''),;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '1',;
		 			fiv.D5tiop with '0'

&& R.03b Mariano
*****	Recibo Factura C	*****

		CASE fivx.d5tico='REF' and fivx.d5tifo='C'
			replace fiv.D5tifo with 'C',;
					fiv.D5tico with '8',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('8', 'C'),;
					fiv.D5imfi with 'N',;
					fiv.D5nume with subs(fivx.d5numd,1,8)+'/'+iif(fivx.d5numd<fivx.d5numh, subs(fivx.d5numh,1,8), ''),;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '1',;
		 			fiv.D5tiop with '0'
&& R.03e Mariano
		 			
*****	Recibo Factura M	*****

		CASE fivx.d5tico='REF' and fivx.d5tifo='M'
			replace fiv.D5tifo with 'M',;
					fiv.D5tico with '8',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('8', 'M'),;
					fiv.D5imfi with 'N',;
					fiv.D5nume with subs(fivx.d5numd,1,8)+'/'+iif(fivx.d5numd<fivx.d5numh, subs(fivx.d5numh,1,8), ''),;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '1',;
		 			fiv.D5tiop with '0'
		 			

*****	Recibo A	*****

		CASE fivx.d5tico='REC' and fivx.d5tifo='A'
			replace fiv.D5tifo with 'A',;
					fiv.D5tico with '@',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('@', 'A'),;
					fiv.D5imfi with 'N',;
					fiv.D5nume with subs(fivx.d5numd,1,8)+'/'+iif(fivx.d5numd<fivx.d5numh, subs(fivx.d5numh,1,8), ''),;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '1',;
		 			fiv.D5tiop with '0'
		 			

*****	Recibo B	*****

		CASE fivx.d5tico='REC' and fivx.d5tifo='B'
			replace fiv.D5tifo with 'B',;
					fiv.D5tico with '@',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('@', 'B'),;
					fiv.D5imfi with 'N',;
					fiv.D5nume with subs(fivx.d5numd,1,8)+'/'+iif(fivx.d5numd<fivx.d5numh, subs(fivx.d5numh,1,8), ''),;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '3',;
		 			fiv.D5tiop with '0'
		 			

*****	Recibo C	*****

		CASE fivx.d5tico='REC' and fivx.d5tifo='C'
			replace fiv.D5tifo with 'C',;
					fiv.D5tico with '@',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('@', 'C'),;
					fiv.D5imfi with 'N',;
					fiv.D5nume with subs(fivx.d5numd,1,8)+'/'+iif(fivx.d5numd<fivx.d5numh, subs(fivx.d5numh,1,8), ''),;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '1',;
		 			fiv.D5tiop with '0'
		 			

*****	Recibo M	*****

		CASE fivx.d5tico='REC' and fivx.d5tifo='M'
			replace fiv.D5tifo with 'M',;
					fiv.D5tico with '@',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('@', 'M'),;
					fiv.D5imfi with 'N',;
					fiv.D5nume with subs(fivx.d5numd,1,8)+'/'+iif(fivx.d5numd<fivx.d5numh, subs(fivx.d5numh,1,8), ''),;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '1',;
		 			fiv.D5tiop with '0'

*****	LIQ Varias A	*****

		CASE fivx.d5tico='LIQ' and fivx.d5tifo='A'
			replace fiv.D5tifo with 'A',;
					fiv.D5tico with '7',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('7', 'A'),;
					fiv.D5imfi with 'N',;
					fiv.D5nume with subs(fivx.d5numd,1,8)+'/'+iif(fivx.d5numd<fivx.d5numh, subs(fivx.d5numh,1,8), ''),;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '1',;
		 			fiv.D5tiop with '0'
		 			
*****	LIQ Varias B	*****

		CASE fivx.d5tico='LIQ' and fivx.d5tifo='B'
			replace fiv.D5tifo with 'B',;
					fiv.D5tico with '7',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('7', 'B'),;
					fiv.D5imfi with 'N',;
					fiv.D5nume with subs(fivx.d5numd,1,8)+'/'+iif(fivx.d5numd<fivx.d5numh, subs(fivx.d5numh,1,8), ''),;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '3',;
		 			fiv.D5tiop with '0'

*****	LIQ Varias C	*****

		CASE fivx.d5tico='LIQ' and fivx.d5tifo='C'
			replace fiv.D5tifo with 'C',;
					fiv.D5tico with '7',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('7', 'C'),;
					fiv.D5imfi with 'N',;
					fiv.D5nume with subs(fivx.d5numd,1,8)+'/'+iif(fivx.d5numd<fivx.d5numh, subs(fivx.d5numh,1,8), ''),;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '1',;
		 			fiv.D5tiop with '0'

*****	Ret Practicadas	*****

		CASE fivx.d5tico='RET' and fivx.d5tifo=' '
			replace fiv.D5tifo with ' ',;
					fiv.D5tico with 'R',;
					fiv.D5tipo with 'V',;
					fiv.D5coco with TCOCO('R', ' '),;
					fiv.D5imfi with 'N',;
					fiv.D5nume with subs(fivx.d5numd,1,8)+'/'+iif(fivx.d5numd<fivx.d5numh, subs(fivx.d5numh,1,8), ''),;
					fiv.D5cond with '1',;
					fiv.D5coim with f00.p0coim,;
					fiv.D5tiim with '3',;
		 			fiv.D5tiop with '0'
		 			
		 	&& R.02b Mariano
		 	if empty(fivx.d5imre)
		 		replace fiv.d5imre with fiv.d5tota
		 	endif
		 	if empty(fivx.d7numori)
		 		replace fiv.d7numori with '0'
		 	endif
		 	&& R.02e Mariano

	ENDCASE	

*****	Clientes	*****
	set order to d2cuit in f02
	&& R.04b Mariano
	if fivx.d6tido$'CUIT.CUIL.CDI '
		if at('-',fivx.d6cuit)=0
			m.d6cuit=left(allt(fivx.d6cuit),2)+'-'+substr(allt(fivx.d6cuit),3,8)+'-'+right(allt(fivx.d6cuit),1)
		else
			m.d6cuit=fivx.d6cuit
		endif
	else
		m.d6cuit=fivx.d6cuit
	endif
	do case
		case fivx.d6tido='CUIT'
			m.d6tido='80'
		case fivx.d6tido='CUIL'
			m.d6tido='86'
		case fivx.d6tido='CDI '
			m.d6tido='87'
		case fivx.d6tido='DNI '
			m.d6tido='96'
		case fivx.d6tido='CI  '
			m.d6tido='00'
		case fivx.d6tido='LC  '
			m.d6tido='90'
		case fivx.d6tido='LE  '
			m.d6tido='89'
		case fivx.d6tido='DTE '
			m.d6tido='83'
		case fivx.d6tido='DOE '
			m.d6tido='84'
		case fivx.d6tido='ET  '
			m.d6tido='92'
		case fivx.d6tido='PAS '
			m.d6tido='94'
		otherwise
			m.d6tido='  '
	endcase
	&& R.04e Mariano
*	if !empty(fivx.d6cuit) and seek(fivx.d6cuit,'f02')	&& R.04 Mariano
	if !empty(m.d6cuit) and seek(m.d6cuit,'f02')	&& R.04 Mariano
		replace fiv.d5clie with f02.d2clie,;
				fiv.d5juri with f02.d2juri

		replace fiv.d5tiim with ftiimp(fiv.d5clie,fiv.d5peri,'C')
		
	else
		replace fiv.d6nume with fiv.d5nume,;
				fiv.d6peri with fiv.d5peri,;
				fiv.d6empr with d0coem,;
				fiv.d6tipo with fiv.d5tipo,;
				fiv.d6sucu with fiv.d5sucu,;
				fiv.d6tico with fiv.d5tico,;
				fiv.d6tifo with fiv.d5tifo,;
				fiv.d6inte with fiv.d5inte,;
				fiv.d6nocl with fivx.d6nocl
				
*		replace fiv.d6cuit with fivx.d6cuit	&& R.04 Mariano
		replace fiv.d6tido with m.d6tido,;
				fiv.d6cuit with m.d6cuit	&& R.04 Mariano
		
		do case
			case fivx.d5tiim='Inscripto'
				replace fiv.d5clie with '99991',;
						fiv.d5tiim with '1'	

*						fiv.d6tido with '80'	&& R.04 Mariano
				replace	fiv.d6tido with iif(empty(m.d6tido),'80',m.d6tido)	&& R.04 Mariano

			case fivx.d5tiim='No Inscr.'
				replace fiv.d5clie with '99992',;
						fiv.d5tiim with '2'
						
*						fiv.d6tido with '80'	&& R.04 Mariano
				replace	fiv.d6tido with iif(empty(m.d6tido),'80',m.d6tido)	&& R.04 Mariano
						
			case fivx.d5tiim='Cons. Final'
				replace fiv.d5clie with '99993',;
						fiv.d5tiim with '3'	

*						fiv.d6tido with '96'	&& R.04 Mariano
				replace	fiv.d6tido with iif(empty(m.d6tido),'96',m.d6tido)	&& R.04 Mariano

			case fivx.d5tiim='Exento'
				replace fiv.d5clie with '99994',;
						fiv.d5tiim with '4'	

*						fiv.d6tido with '80'	&& R.04 Mariano
				replace	fiv.d6tido with iif(empty(m.d6tido),'80',m.d6tido)	&& R.04 Mariano

			case fivx.d5tiim='No Resp.'
				replace fiv.d5clie with '99995',;
						fiv.d5tiim with '5'

*						fiv.d6tido with '96'	&& R.04 Mariano
				replace	fiv.d6tido with iif(empty(m.d6tido),'96',m.d6tido)	&& R.04 Mariano

			case fivx.d5tiim='Monotributo'
				replace fiv.d5clie with '99996',;
						fiv.d5tiim with '6'

*						fiv.d6tido with '80'	&& R.04 Mariano
				replace	fiv.d6tido with iif(empty(m.d6tido),'80',m.d6tido)	&& R.04 Mariano

			case fivx.d5tiim='Suj. No Categ.'
				replace fiv.d5clie with '99997',;
						fiv.d5tiim with '7'

*						fiv.d6tido with '96'	&& R.04 Mariano
				replace	fiv.d6tido with iif(empty(m.d6tido),'96',m.d6tido)	&& R.04 Mariano
						
		endcase		
		set order to d2clie in f02
		if seek(fiv.d5clie,'f02')
			replace fiv.D5juri with f02.d2juri

			replace fiv.d5tiim with ftiimp(fiv.d5clie,fiv.d5peri,'C')	&& R.02 Mariano

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

	if empty(fivx.d5rubr)
		replace fiv.d5rubr with '999999' 	&& codigo arbitrario para interfase, debe existir en la empresa
	else
		replace fiv.d5rubr with fivx.d5rubr
	endif
	

*****	Condicion de Venta	*****
	
	Do case
		case fivx.d5cond='Contado'
			replace fiv.d5cond with '2'
		case fivx.d5cond='Cta Cte'
			replace fiv.d5cond with '1'
		case fivx.d5cond='Otra'
			replace fiv.d5cond with '3'
	Endcase	

***** 	Tipo de Operacion *****

	Do case
			case fivx.d5tiop='No Corresponde'
				replace fiv.d5tiop with '0'
			case fivx.d5tiop='No Alcanzado'
				replace fiv.d5tiop with 'A'
*			case fivx.d5tiim='Operaci๓n de Canje'	&& R.02 Mariano
			case fivx.d5tiop='Operaci๓n de Canje'	&& R.02 Mariano
				replace fiv.d5tiop with 'C'
*			case fivx.d5tiim='Operaci๓n Exenta'	&& R.02 Mariano
			case fivx.d5tiop='Operaci๓n Exenta'	&& R.02 Mariano
				replace fiv.d5tiop with 'E'
*			case fivx.d5tiim='No Gravado'	&& R.02 Mariano
			case fivx.d5tiop='No Gravado'	&& R.02 Mariano
				replace fiv.d5tiop with 'N'	
*			case fivx.d5tiim='Imp./Exp. Del Exterior'	&& R.02 Mariano
			case fivx.d5tiop='Imp./Exp. Del Exterior'	&& R.02 Mariano
				replace fiv.d5tiop with 'X'
*			case fivx.d5tiim='Imp./Exp. Zona Franca'	&& R.02 Mariano
			case fivx.d5tiop='Imp./Exp. Zona Franca'	&& R.02 Mariano
				replace fiv.d5tiop with 'Z'		
	Endcase

****** Percepciones ********
&& R.07b Mariano
	** Si viene la jurisdiccion en el comprobante la tomo de ahi **
	=seek('JU','f01')
	DO WHILE !EOF('f01') and f01.d1tip='JU'	

		if upper(f01.d1des)=upper(fivx.d5juri)
	    	replace fiv.d5juri with f01.D1cla
	    	exit 	
		ENDIF
 
		SKIP IN F01
	ENDDO
&& R.07e Mariano


	if empty(fivx.D5pere) and fivx.D5imp3!=0
		&& R.01b Mariano
*		replace fiv.d5pere with '000' 		&& codigo arbitrario para interfase, deberan modificar el comprobante
		go top in f24
*		do while (f24.d24tipo!='I' or f24.d24refe!=iif(empty(fivx.d5imre),'P','R')) and !eof('f24')	&& R.02 Mariano
		do while (f24.d24tipo!='I' or f24.d24refe!=iif(fivx.d5tico!='RET','P','R')) and !eof('f24')	&& R.02 Mariano
			skip in f24
		enddo
*		if f24.d24tipo='I' and f24.d24refe=iif(empty(fivx.d5imre),'P','R') and f24.d24codi='000'	&& R.02 Mariano
		if f24.d24tipo='I' and f24.d24refe=iif(fivx.d5tico!='RET','P','R') and f24.d24codi='000'	&& R.02 Mariano
			replace fiv.d5pere with f24.d24codi 		&& codigo arbitrario para interfase, deberan modificar el comprobante
		else
			sele f24
			append blank
			=db_rlock('f24')
*			replace d24codi with '000', d24tipo with 'I', d24refe with iif(empty(fivx.d5imre),'P','R'), d24desc with 'INDEFINIDO'	&& R.02 Mariano
			replace d24codi with '000', d24tipo with 'I', d24refe with iif(fivx.d5tico!='RET','P','R'), d24desc with 'INDEFINIDO'	&& R.02 Mariano
			unlock in f24
			sele fiv
			replace fiv.d5pere with '000' 		&& codigo arbitrario para interfase, deberan modificar el comprobante
		endif
		&& R.01e Mariano
	endif				

	if empty(fivx.D5cpib)
		if (f00.p0prv1=1 and fiv.d5imp6!=0) or (f00.p0prv2=1 and fiv.d5imp7!=0) or (f00.p0prv3=1 and fiv.d5imp8!=0)
			&& R.01b Mariano
*			replace fiv.d5cpib with '000' 		&& codigo arbitrario para interfase, deberan modificar el comprobante
			=SEEK("JU" + fiv.d5juri, "f01")
			cafip	= f01.d1tdgi
			if seek(cafip,'f25')
*				do while (f25.d25tdgi!=cafip or f25.d25refe!=iif(empty(fivx.d5imre),'P','R')) and !eof('f25')	&& R.02 Mariano
				do while (f25.d25tdgi!=cafip or f25.d25refe!=iif(fivx.d5tico!='RET','P','R')) and !eof('f25')	&& R.02 Mariano
					skip in f25
				enddo
*				if f25.d25tdgi=cafip and f25.d25refe=iif(empty(fivx.d5imre),'P','R') and f25.d25codi='000'	&& R.02 Mariano
				if f25.d25tdgi=cafip and f25.d25refe=iif(fivx.d5tico!='RET','P','R') and f25.d25codi='000'	&& R.02 Mariano
					replace fiv.d5cpib with f25.d25codi 		&& codigo arbitrario para interfase, deberan modificar el comprobante
				else
					sele f25
					append blank
					=db_rlock('f25')
*					replace d25tdgi with cafip, d25codi with '000', d25refe with iif(empty(fivx.d5imre),'P','R'), d25cafi with '000', d25desc with 'INDEFINIDO'	&& R.02 Mariano
					replace d25tdgi with cafip, d25codi with '000', d25refe with iif(fivx.d5tico!='RET','P','R'), d25cafi with '000', d25desc with 'INDEFINIDO'	&& R.02 Mariano
					unlock in f25
					sele fiv
					replace fiv.d5cpib with '000' 		&& codigo arbitrario para interfase, deberan modificar el comprobante
				endif
			endif
			&& R.01e Mariano
		endif
	endif				
	
	if empty(fivx.d5cpga)
		if (f00.p0prv1=2 and fiv.d5imp6!=0) or (f00.p0prv2=1 and fiv.d5imp7!=0) or (f00.p0prv3=2 and fiv.d5imp8!=0)
			&& R.01b Mariano
*			replace fiv.d5cpga with '000' 		&& codigo arbitrario para interfase, deberan modificar el comprobante
			go top in f24
*			do while (f24.d24tipo!='G' or f24.d24refe!=iif(empty(fivx.d5imre),'P','R')) and !eof('f24')	&& R.02 Mariano
			do while (f24.d24tipo!='G' or f24.d24refe!=iif(fivx.d5tico!='RET','P','R')) and !eof('f24')	&& R.02 Mariano
				skip in f24
			enddo
*			if f24.d24tipo='G' and f24.d24refe=iif(empty(fivx.d5imre),'P','R') and f24.d24codi='000'	&& R.02 Mariano
			if f24.d24tipo='G' and f24.d24refe=iif(fivx.d5tico!='RET','P','R') and f24.d24codi='000'	&& R.02 Mariano
				replace fiv.d5cpga with f24.d24codi 		&& codigo arbitrario para interfase, deberan modificar el comprobante
			else
				sele f24
				append blank
				=db_rlock('f24')
*				replace d24codi with '000', d24tipo with 'G', d24refe with iif(empty(fivx.d5imre),'P','R'), d24desc with 'INDEFINIDO'	&& R.02 Mariano
				replace d24codi with '000', d24tipo with 'G', d24refe with iif(fivx.d5tico!='RET','P','R'), d24desc with 'INDEFINIDO'	&& R.02 Mariano
				unlock in f24
				sele fiv
				replace fiv.d5cpga with '000' 		&& codigo arbitrario para interfase, deberan modificar el comprobante
			endif
			&& R.01e Mariano
		endif
	endif
	
	if empty(fivx.D5cpot)
		if (f00.p0prv1=3 and fiv.d5imp6!=0) or (f00.p0prv2=3 and fiv.d5imp7!=0) or (f00.p0prv3=3 and fiv.d5imp8!=0)
			&& R.01b Mariano
*			replace fiv.d5cpot with '000' 		&& codigo arbitrario para interfase, deberan modificar el comprobante
			go top in f24
*			do while (f24.d24tipo!='O' or f24.d24refe!=iif(empty(fivx.d5imre),'P','R')) and !eof('f24')	&& R.02 Mariano
			do while (f24.d24tipo!='O' or f24.d24refe!=iif(fivx.d5tico!='RET','P','R')) and !eof('f24')	&& R.02 Mariano
				skip in f24
			enddo
*			if f24.d24tipo='O' and f24.d24refe=iif(empty(fivx.d5imre),'P','R') and f24.d24codi='000'	&& R.02 Mariano
			if f24.d24tipo='O' and f24.d24refe=iif(fivx.d5tico!='RET','P','R') and f24.d24codi='000'	&& R.02 Mariano
				replace fiv.d5cpot with f24.d24codi 		&& codigo arbitrario para interfase, deberan modificar el comprobante
			else
				sele f24
				append blank
				=db_rlock('f24')
*				replace d24codi with '000', d24tipo with 'O', d24refe with iif(empty(fivx.d5imre),'P','R'), d24desc with 'INDEFINIDO'	&& R.02 Mariano
				replace d24codi with '000', d24tipo with 'O', d24refe with iif(fivx.d5tico!='RET','P','R'), d24desc with 'INDEFINIDO'	&& R.02 Mariano
				unlock in f24
				sele fiv
				replace fiv.d5cpot with '000' 		&& codigo arbitrario para interfase, deberan modificar el comprobante
			endif
			&& R.01e Mariano
		endif
	endif
	
	&& R.07b Mariano
	if fiv.d5tico!='R' and !empty(fiv.d5imre)
		replace fiv.d5imre with 0
	endif
	&& R.07e Mariano
	
	SKIP IN fivx
	
ENDDO

sele fivx
zap

endif 	&& R.08 Mariano

sele fiv
use in fiv

endif	&& R.01 Mariano

if w_prco	&& R.01 Mariano

dimension wwfiles(1)
wwfiles=''
*wwcant=a_dir('wwfiles',wdici+'\'+d0coem+'\COMPRA*.XLS')		&& R.01 Mariano
wwcant=a_dir('wwfiles',wdici+'\COMPRAS*.XLS?','','',.t.)  	&& R.01 Mariano
wfinu=0

if used('ficx')
	sele ficx
else

	sele 0
	use &d0coem.\intecpra alias ficx exclusive
endif 
for wfinu=1 to wwcant
	wait wind 'Procesando Interfase Compras...' nowait
	
*	w_fire='"'+wdici+'\'+d0coem+'\'+wwfiles[wfinu,1]+'"'	&& R.01 Mariano
	w_fire=wdici+'\'+wwfiles[wfinu,1]	&& R.01 Mariano
	&& R.01b Mariano
*	if version()='Visual FoxPro'
*		append from &w_fire type xls
*	else
	&& R.01e Mariano
		use in ficx
		&& R.01b Mariano
*		private w_vfox,w_inst001,w_inst002,w_inst003
*		w_inst001='use '+sys(2003)+'\'+d0coem+'\intecpra.dbf in 0 alias ficx exclusive'
*		w_inst002='select ficx'
*		w_inst003='append from '+w_fire+' type xls'
		=seterr('OFF')
		private w_vfox
		&& R.01e Mariano

		private d0f9co
		d0f9co=sys(2003)+'\I'+d0coem+p_netuser+'co'
		w_vfox=d0f9co+'.mem'
		save to &w_vfox. all like w_inst*
		release all like w_inst*
	
		&& R.01b Mariano
*		=visuafox('ejeproc(.f.)',w_vfox)
		=visuafox("intiwxls('"+w_fire+"','"+"C"+"')",w_vfox, '', 5)	
		=seterr('ON')
	
		=inkey(2)
		&& R.01e Mariano

		if file(w_vfox)
			erase &w_vfox.
		endif
			
*		=inkey(2)	&& R.01 Mariano
		w_paso=1
		do while .t.
			fieldvec[1]='ficx'
			use &d0coem.\intecpra alias ficx exclusive
			
			if neterr()
				=advsuave('Procesando archivo Excel '+justfname(w_fire),w_title)
				fieldvec=''
				=inkey(2)	&& R.08 Mariano
			endif

			fieldvec=''
			
			if alias()='ficx' or w_paso>=5
				exit
			endif
			w_paso=w_paso+1
		enddo
*	endif	&& R.01 Mariano
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
	&& R.01b Mariano
	sele 0
	use fi\00\ivf007
	copy structure extended to &d0coem.\inte007x.dbf fields d7numori
	use
	&& R.01e Mariano
	sele f05x
	append from  &d0coem.\inte006x.dbf
	append from  &d0coem.\inte007x.dbf	&& R.01 Mariano
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
	dele file &d0coem.\inte007x.dbf	&& R.01 Mariano
	use
endif

&& R.04b Mariano
*sele 0
*use &d0coem.\inte005c in 0 alias fic exclusive	&& Interfase Compras
if used('fic')
	sele fic
	use &d0coem.\inte005c alias fic exclusive	&& Interfase Compras
else
	sele 0
	use &d0coem.\inte005c alias fic exclusive	&& Interfase Compras
endif
&& R.04e Mariano
zap

SELE Fic

if used('ficx')	&& R.08 Mariano

GO TOP IN ficx
DO WHILE !EOF('ficx')

	wait wind 'Procesando Interfase Compras...' nowait

	if empty(ctod(ficx.d5fech)) or empty(ficx.d5nume)
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

		if f23.d23alic=ficx.d5por1
	    	replace fic.d5alic with f23.D23codi
		ENDIF
 
		SKIP IN F23
	ENDDO

&& R.00b Taboada
*****	Jurisdiccion	*****

	
&& R.07b Mariano
*	GO TOP IN F01
*	DO WHILE !EOF('f01')

*		if f01.d1des=ficx.d5juri
*	    	replace fic.d5juri with f01.D1cla
*		ENDIF
 
*		SKIP IN F01
*	ENDDO
&& R.07e Mariano

&& R.00e Taboada		
*****	Comp.Generico	*****

	replace fic.D5fech with CTOD(ficx.d5fech),;
			fic.D5empr with d0coem,;
			fic.d5titr with '',;
			fic.d5ttip with '0',;
			fic.D5peri with wdperi,;
			fic.D5inte with Padl(alltr(str(lcd5inte,6)), 6,'0'),;
			fic.D5nume with iif(!empty(ficx.d5sucu),ficx.d5sucu+'-','00000')+subs(ficx.d5nume,1,8),;
			fic.D5tifo with ficx.d5tifo,;
			fic.D5sucu with ' ',;
			fic.d5clie with ficx.d5clie,;
			fic.D5por1 with ficx.d5por1,;
			fic.D5imp1 with ficx.d5imp1,;
			fic.D5imp2 with ficx.d5imp2,;
			fic.D5imp3 With ficx.d5imp3,;
			fic.D5imp4 With ficx.d5imp4,;
			fic.D5imp6 With ficx.d5imp6,;
			fic.D5imp7 With ficx.d5imp7,;
			fic.D5imp8 With ficx.d5imp8,;
			fic.D5tota with ficx.d5Tota,;
			fic.D5imre with	ficx.d5imre,;
			fic.d7numori with ficx.d7numori,;
			fic.d5cuco with ficx.d5cuco,;
			fic.d5deco with ficx.d5deco,;
			fic.d5ivco with ficx.d5ivco,;
			fic.d5pere with ficx.d5pere,;
			fic.d5cpib with ficx.d5cpib,;
			fic.d5cpga with ficx.d5cpga,;
			fic.d5cpot with ficx.d5cpot,;
			fic.d5adua with ficx.d5adua,;
			fic.d5dest with ficx.d5dest,;
			fic.d5desp with ficx.d5desp,;
			fic.d5verf with ficx.d5verf		&& R.00 Taboada (se agregaron campos)	&& R.01 Mariano (Agregue d7numori,d5cuco,d5deco,d5ivco)

******   Imp.Interno     *****
*
*	if 	!empty(ficx.impint)
*		do case
*			case f00.p0prc1!=1 and f00.p0prc1!=2 
*				replace fic.d5imp6 with ficx.impint
*			case f00.p0prc2!=1 and f00.p0prc2!=2 
*				replace fic.d5imp7 with ficx.impint
*			case f00.p0prc3!=1 and f00.p0prc3!=2 
*				replace fic.d5imp8 with ficx.impint
*		endcase
*	endif

DO CASE	
*****	Comp.Tique 	*****

		CASE ficx.d5tico='TIQ'
			replace fic.D5tifo with ' ',;
					fic.D5tico with '?',;
					fic.D5tipo with 'C',;
					fic.D5coco with TCOCO('?', ' ', 'C'),;
					fic.D5imfi with 'S',;
					fic.D5cond with '1',;
					fic.D5coim with f00.p0coim,;
					fic.D5tiim with '3',;
					fic.D5tiop with '0'

*****	Comp.Tique factura A	*****

		CASE ficx.d5tico='TIF' and ficx.d5tifo='A'
			replace fic.D5tifo with 'A',;
					fic.D5tico with '6',;
					fic.D5tipo with 'C',;
					fic.D5coco with TCOCO('6', 'A', 'C'),;
					fic.D5imfi with 'S',;
					fic.D5cond with '1',;
					fic.D5coim with f00.p0coim,;
					fic.D5tiim with '1',;
					fic.D5tiop with '0'

*****	Comp. Tiquet factura B	*****

		CASE ficx.d5tico='TIF' and ficx.d5tifo='B'
			replace fic.D5tifo with 'B',;
					fic.D5tico with '6',;
					fic.D5tipo with 'C',;
					fic.D5coco with TCOCO('6', 'B', 'C'),;
					fic.D5imfi with 'S',;
					fic.D5clie with '99993',;
					fic.D5cond with '1',;
					fic.D5coim with f00.p0coim,;
					fic.D5tiim with '3',;
					fic.D5tiop with '0'

*****	Comp. Tiquet factura C	*****

		CASE ficx.d5tico='TIF' and ficx.d5tifo='C'
			replace fic.D5tifo with 'C',;
					fic.D5tico with '6',;
					fic.D5tipo with 'C',;
					fic.D5coco with TCOCO('6', 'C', 'C'),;
					fic.D5imfi with 'S',;
					fic.D5clie with '99991',;
					fic.D5cond with '1',;
					fic.D5coim with f00.p0coim,;
					fic.D5tiim with '1',;
					fic.D5tiop with '0'


*****	Comp. Tiquet factura M	*****

		CASE ficx.d5tico='TIF' and ficx.d5tifo='M'
			replace fic.D5tifo with 'M',;
					fic.D5tico with '6',;
					fic.D5tipo with 'C',;
					fic.D5coco with TCOCO('6', 'M', 'C'),;
					fic.D5imfi with 'S',;
					fic.D5clie with '99991',;
					fic.D5cond with '1',;
					fic.D5coim with f00.p0coim,;
					fic.D5tiim with '1',;
					fic.D5tiop with '0'
		
*****	Comp.Tique NC A	*****

		CASE ficx.d5tico='TIC' and ficx.d5tifo='A'
			replace fic.D5tifo with 'A',;
					fic.D5tico with 'E',;
					fic.D5tipo with 'C',;
					fic.D5coco with TCOCO('E', 'A', 'C'),;
					fic.D5imfi with 'S',;
					fic.D5clie with '99991',;
					fic.D5cond with '1',;
					fic.D5coim with f00.p0coim,;
					fic.D5tiim with '1',;
					fic.D5tiop with '0'

*****	Comp. Tiquet NC B	*****

		CASE ficx.d5tico='TIC' and ficx.d5tifo='B'
			replace fic.D5tifo with 'B',;
					fic.D5tico with 'E',;
					fic.D5tipo with 'C',;
					fic.D5coco with TCOCO('E', 'B', 'C'),;
					fic.D5imfi with 'S',;
					fic.D5clie with '99991',;
					fic.D5cond with '1',;
					fic.D5coim with f00.p0coim,;
					fic.D5tiim with '1',;
					fic.D5tiop with '0'

*****	Comp. Tiquet NC C	*****

		CASE ficx.d5tico='TIC' and ficx.d5tifo='C'
			replace fic.D5tifo with 'C',;
					fic.D5tico with 'E',;
					fic.D5tipo with 'C',;
					fic.D5coco with TCOCO('E', 'C', 'C'),;
					fic.D5imfi with 'S',;
					fic.D5clie with '99991',;
					fic.D5cond with '1',;
					fic.D5coim with f00.p0coim,;
					fic.D5tiim with '1',;
					fic.D5tiop with '0'
		
*****	Comp. Tiquet NC M	*****

		CASE ficx.d5tico='TIC' and ficx.d5tifo='M'
			replace fic.D5tifo with 'M',;
					fic.D5tico with 'E',;
					fic.D5tipo with 'C',;
					fic.D5coco with TCOCO('E', 'M', 'C'),;
					fic.D5imfi with 'S',;
					fic.D5clie with '99991',;
					fic.D5cond with '1',;
					fic.D5coim with f00.p0coim,;
					fic.D5tiim with '1',;
					fic.D5tiop with '0'

*****	Comp.Tique ND A	*****

		CASE ficx.d5tico='TID' and ficx.d5tifo='A'
			replace fic.D5tifo with 'A',;
					fic.D5tico with 'D',;
					fic.D5tipo with 'C',;
					fic.D5coco with TCOCO('D', 'A', 'C'),;
					fic.D5imfi with 'S',;
					fic.D5clie with '99991',;
					fic.D5cond with '1',;
					fic.D5coim with f00.p0coim,;
					fic.D5tiim with '1',;
					fic.D5tiop with '0'

*****	Comp. Tiquet ND B	*****

		CASE ficx.d5tico='TID' and ficx.d5tifo='B'
			replace fic.D5tifo with 'B',;
					fic.D5tico with 'D',;
					fic.D5tipo with 'C',;
					fic.D5coco with TCOCO('D', 'B', 'C'),;
					fic.D5imfi with 'S',;
					fic.D5clie with '99991',;
					fic.D5cond with '1',;
					fic.D5coim with f00.p0coim,;
					fic.D5tiim with '1',;
					fic.D5tiop with '0'
		
*****	Comp.Tique ND C	*****

		CASE ficx.d5tico='TID' and ficx.d5tifo='C'
			replace fic.D5tifo with 'C',;
					fic.D5tico with 'D',;
					fic.D5tipo with 'C',;
					fic.D5coco with TCOCO('D', 'C', 'C'),;
					fic.D5imfi with 'S',;
					fic.D5clie with '99991',;
					fic.D5cond with '1',;
					fic.D5coim with f00.p0coim,;
					fic.D5tiim with '1',;
					fic.D5tiop with '0'
					
*****	Comp.Tique ND M	*****

		CASE ficx.d5tico='TID' and ficx.d5tifo='M'
			replace fic.D5tifo with 'M',;
					fic.D5tico with 'D',;
					fic.D5tipo with 'C',;
					fic.D5coco with TCOCO('D', 'M', 'C'),;
					fic.D5imfi with 'S',;
					fic.D5clie with '99991',;
					fic.D5cond with '1',;
					fic.D5coim with f00.p0coim,;
					fic.D5tiim with '1',;
					fic.D5tiop with '0'

*****	Facturas A	*****

		CASE ficx.d5tico='FAC' and ficx.d5tifo='A'
			replace fic.D5tifo with 'A',;
					fic.D5tico with 'A',;
					fic.D5tipo with 'C',;
					fic.D5coco with TCOCO('A', 'A', 'C'),;
					fic.D5imfi with 'N',;
					fic.D5cond with '1',;
					fic.D5coim with f00.p0coim,;
					fic.D5tiim with '1',;
		 			fic.D5tiop with '0'

*****	Facturas B	*****

		CASE ficx.d5tico='FAC' and ficx.d5tifo='B'
			replace fic.D5tifo with 'B',;
					fic.D5tico with 'A',;
					fic.D5tipo with 'C',;
					fic.D5coco with TCOCO('A', 'B', 'C'),;
					fic.D5imfi with 'N',;
					fic.D5cond with '1',;
					fic.D5coim with f00.p0coim,;
					fic.D5tiim with '3',;
		 			fic.D5tiop with '0'
		 			
*****	Facturas I	*****

		CASE ficx.d5tico='FAC' and ficx.d5tifo='I'
			replace fic.D5tifo with 'I',;
					fic.D5tico with 'A',;
					fic.D5tipo with 'C',;
					fic.D5coco with TCOCO('A', 'I', 'C'),;
					fic.D5imfi with 'N',;
					fic.D5cond with '1',;
					fic.D5coim with f00.p0coim,;
					fic.D5tiim with '1',;
		 			fic.D5tiop with 'X'

*****	Facturas M	*****

		CASE ficx.d5tico='FAC' and ficx.d5tifo='M'
			replace fic.D5tifo with 'M',;
					fic.D5tico with 'A',;
					fic.D5tipo with 'C',;
					fic.D5coco with TCOCO('A', 'M', 'C'),;
					fic.D5imfi with 'N',;
					fic.D5cond with '1',;
					fic.D5coim with f00.p0coim,;
					fic.D5tiim with '1',;
		 			fic.D5tiop with '0'

*****	NOTA DE CREDITO A	*****

		CASE ficx.d5tico='CRE' and ficx.d5tifo='A'
			replace fic.D5tifo with 'A',;
					fic.D5tico with 'C',;
					fic.D5tipo with 'C',;
					fic.D5coco with TCOCO('C', 'A', 'C'),;
					fic.D5imfi with 'N',;
					fic.D5cond with '1',;
					fic.D5coim with f00.p0coim,;
					fic.D5tiim with '1',;
		 			fic.D5tiop with '0'


*****	NOTA DE CREDITO B	*****

		CASE ficx.d5tico='CRE' and ficx.d5tifo='B'
			replace fic.D5tifo with 'B',;
					fic.D5tico with 'C',;
					fic.D5tipo with 'C',;
					fic.D5coco with TCOCO('C', 'B', 'C'),;
					fic.D5imfi with 'N',;
					fic.D5cond with '1',;
					fic.D5coim with f00.p0coim,;
					fic.D5tiim with '3',;
		 			fic.D5tiop with '0'
		 			
*****	NOTA DE CREDITO I	*****

		CASE ficx.d5tico='CRE' and ficx.d5tifo='I'
			replace fic.D5tifo with 'I',;
					fic.D5tico with 'C',;
					fic.D5tipo with 'C',;
					fic.D5coco with TCOCO('C', 'I', 'C'),;
					fic.D5imfi with 'N',;
					fic.D5cond with '1',;
					fic.D5coim with f00.p0coim,;
					fic.D5tiim with '1',;
		 			fic.D5tiop with 'X'
		 			
*****	NOTA DE CREDITO M	*****

		CASE ficx.d5tico='CRE' and ficx.d5tifo='M'
			replace fic.D5tifo with 'M',;
					fic.D5tico with 'C',;
					fic.D5tipo with 'C',;
					fic.D5coco with TCOCO('C', 'M', 'C'),;
					fic.D5imfi with 'N',;
					fic.D5cond with '1',;
					fic.D5coim with f00.p0coim,;
					fic.D5tiim with '1',;
		 			fic.D5tiop with '0'

*****	NOTA DE DEBITO A	*****

		CASE ficx.d5tico='DEB' and ficx.d5tifo='A'
			replace fic.D5tifo with 'A',;
					fic.D5tico with 'B',;
					fic.D5tipo with 'C',;
					fic.D5coco with TCOCO('B', 'A', 'C'),;
					fic.D5imfi with 'N',;
					fic.D5cond with '1',;
					fic.D5coim with f00.p0coim,;
					fic.D5tiim with '1',;
		 			fic.D5tiop with '0'

*****	NOTA DE DEBITO B	*****

		CASE ficx.d5tico='DEB' and ficx.d5tifo='B'
			replace fic.D5tifo with 'B',;
					fic.D5tico with 'B',;
					fic.D5tipo with 'C',;
					fic.D5coco with TCOCO('B', 'B', 'C'),;
					fic.D5imfi with 'N',;
					fic.D5cond with '1',;
					fic.D5coim with f00.p0coim,;
					fic.D5tiim with '3',;
		 			fic.D5tiop with '0'

*****	NOTA DE DEBITO I	*****

		CASE ficx.d5tico='DEB' and ficx.d5tifo='I'
			replace fic.D5tifo with 'I',;
					fic.D5tico with 'B',;
					fic.D5tipo with 'C',;
					fic.D5coco with TCOCO('B', 'I', 'C'),;
					fic.D5imfi with 'N',;
					fic.D5cond with '1',;
					fic.D5coim with f00.p0coim,;
					fic.D5tiim with '1',;
		 			fic.D5tiop with 'X'

*****	NOTA DE DEBITO M	*****

		CASE ficx.d5tico='DEB' and ficx.d5tifo='M'
			replace fic.D5tifo with 'M',;
					fic.D5tico with 'B',;
					fic.D5tipo with 'C',;
					fic.D5coco with TCOCO('B', 'M', 'C'),;
					fic.D5imfi with 'N',;
					fic.D5cond with '1',;
					fic.D5coim with f00.p0coim,;
					fic.D5tiim with '1',;
		 			fic.D5tiop with '0'

*****	Recibo Factura A	*****

		CASE ficx.d5tico='REF' and ficx.d5tifo='A'
			replace fic.D5tifo with 'A',;
					fic.D5tico with '8',;
					fic.D5tipo with 'C',;
					fic.D5coco with TCOCO('8', 'A', 'C'),;
					fic.D5imfi with 'N',;
					fic.D5cond with '1',;
					fic.D5coim with f00.p0coim,;
					fic.D5tiim with '1',;
		 			fic.D5tiop with '0'

*****	Recibo Factura B	*****

		CASE ficx.d5tico='REF' and ficx.d5tifo='B'
			replace fic.D5tifo with 'B',;
					fic.D5tico with '8',;
					fic.D5tipo with 'C',;
					fic.D5coco with TCOCO('8', 'B', 'C'),;
					fic.D5imfi with 'N',;
					fic.D5cond with '1',;
					fic.D5coim with f00.p0coim,;
					fic.D5tiim with '3',;
		 			fic.D5tiop with '0'
		 			
*****	Recibo Factura C	*****

		CASE ficx.d5tico='REF' and ficx.d5tifo='C'
			replace fic.D5tifo with 'C',;
					fic.D5tico with '8',;
					fic.D5tipo with 'C',;
					fic.D5coco with TCOCO('8', 'C', 'C'),;
					fic.D5imfi with 'N',;
					fic.D5cond with '1',;
					fic.D5coim with f00.p0coim,;
					fic.D5tiim with '1',;
		 			fic.D5tiop with '0'

*****	Recibo Factura I	*****

		CASE ficx.d5tico='REF' and ficx.d5tifo='I'
			replace fic.D5tifo with 'I',;
					fic.D5tico with '8',;
					fic.D5tipo with 'C',;
					fic.D5coco with TCOCO('8', 'I', 'C'),;
					fic.D5imfi with 'N',;
					fic.D5cond with '1',;
					fic.D5coim with f00.p0coim,;
					fic.D5tiim with '1',;
		 			fic.D5tiop with '0'

*****	Recibo Factura M	*****

		CASE ficx.d5tico='REF' and ficx.d5tifo='M'
			replace fic.D5tifo with 'M',;
					fic.D5tico with '8',;
					fic.D5tipo with 'C',;
					fic.D5coco with TCOCO('8', 'M', 'C'),;
					fic.D5imfi with 'N',;
					fic.D5cond with '1',;
					fic.D5coim with f00.p0coim,;
					fic.D5tiim with '1',;
		 			fic.D5tiop with '0'		 			

*****	Recibo A	*****

		CASE ficx.d5tico='REC' and ficx.d5tifo='A'
			replace fic.D5tifo with 'A',;
					fic.D5tico with '@',;
					fic.D5tipo with 'C',;
					fic.D5coco with TCOCO('@', 'A', 'C'),;
					fic.D5imfi with 'N',;
					fic.D5cond with '1',;
					fic.D5coim with f00.p0coim,;
					fic.D5tiim with '1',;
		 			fic.D5tiop with '0'
		 			

*****	Recibo B	*****

		CASE ficx.d5tico='REC' and ficx.d5tifo='B'
			replace fic.D5tifo with 'B',;
					fic.D5tico with '@',;
					fic.D5tipo with 'C',;
					fic.D5coco with TCOCO('@', 'B', 'C'),;
					fic.D5imfi with 'N',;
					fic.D5cond with '1',;
					fic.D5coim with f00.p0coim,;
					fic.D5tiim with '3',;
		 			fic.D5tiop with '0'
		 			
*****	Recibo C	*****

		CASE ficx.d5tico='REC' and ficx.d5tifo='C'
			replace fic.D5tifo with 'C',;
					fic.D5tico with '@',;
					fic.D5tipo with 'C',;
					fic.D5coco with TCOCO('@', 'C', 'C'),;
					fic.D5imfi with 'N',;
					fic.D5cond with '1',;
					fic.D5coim with f00.p0coim,;
					fic.D5tiim with '1',;
		 			fic.D5tiop with '0'
		 					 			
*****	Recibo M	*****

		CASE ficx.d5tico='REC' and ficx.d5tifo='M'
			replace fic.D5tifo with 'M',;
					fic.D5tico with '@',;
					fic.D5tipo with 'C',;
					fic.D5coco with TCOCO('@', 'M', 'C'),;
					fic.D5imfi with 'N',;
					fic.D5cond with '1',;
					fic.D5coim with f00.p0coim,;
					fic.D5tiim with '1',;
		 			fic.D5tiop with '0'
	
*****	LIQ Varias A	*****

		CASE ficx.d5tico='LIQ' and ficx.d5tifo='A'
			replace fic.D5tifo with 'A',;
					fic.D5tico with '7',;
					fic.D5tipo with 'C',;
					fic.D5coco with TCOCO('7', 'A', 'C'),;
					fic.D5imfi with 'N',;
					fic.D5cond with '1',;
					fic.D5coim with f00.p0coim,;
					fic.D5tiim with '1',;
		 			fic.D5tiop with '0'

		 			
*****	LIQ Varias B	*****

		CASE ficx.d5tico='LIQ' and ficx.d5tifo='B'
			replace fic.D5tifo with 'B',;
					fic.D5tico with '7',;
					fic.D5tipo with 'C',;
					fic.D5coco with TCOCO('7', 'B', 'C'),;
					fic.D5imfi with 'N',;
					fic.D5cond with '1',;
					fic.D5coim with f00.p0coim,;
					fic.D5tiim with '3',;
		 			fic.D5tiop with '0'
		 			
*****	LIQ Varias C	*****

		CASE ficx.d5tico='LIQ' and ficx.d5tifo='C'
			replace fic.D5tifo with 'C',;
					fic.D5tico with '7',;
					fic.D5tipo with 'C',;
					fic.D5coco with TCOCO('7', 'C', 'C'),;
					fic.D5imfi with 'N',;
					fic.D5cond with '1',;
					fic.D5coim with f00.p0coim,;
					fic.D5tiim with '1',;
		 			fic.D5tiop with '0'
	
*****	Ret Sufridas	*****

		CASE ficx.d5tico='RET' and ficx.d5tifo=' '
			replace fic.D5tifo with ' ',;
					fic.D5tico with 'R',;
					fic.D5tipo with 'C',;
					fic.D5coco with TCOCO('R', ' ', 'C'),;
					fic.D5imfi with 'N',;
					fic.D5cond with '1',;
					fic.D5coim with f00.p0coim,;
					fic.D5tiim with '3',;
		 			fic.D5tiop with '0'
	
		 	&& R.02b Mariano
		 	if empty(ficx.d5imre)
		 		replace fic.d5imre with fic.d5tota
		 	endif
		 	if empty(ficx.d7numori)
		 		replace fic.d7numori with '0'
		 	endif
		 	&& R.02e Mariano

	ENDCASE	

*****	Proveedores	*****

	set order to d3cuit in f03

	&& R.04b Mariano
	if ficx.d6tido$'CUIT.CUIL.CDI '
		if at('-',ficx.d6cuit)=0
			m.d6cuit=left(allt(ficx.d6cuit),2)+'-'+substr(allt(ficx.d6cuit),3,8)+'-'+right(allt(ficx.d6cuit),1)
		else
			m.d6cuit=ficx.d6cuit
		endif
	else
		m.d6cuit=ficx.d6cuit
	endif
	do case
		case ficx.d6tido='CUIT'
			m.d6tido='80'
		case ficx.d6tido='CUIL'
			m.d6tido='86'
		case ficx.d6tido='CDI '
			m.d6tido='87'
		case ficx.d6tido='DNI '
			m.d6tido='96'
		case ficx.d6tido='CI  '
			m.d6tido='00'
		case ficx.d6tido='LC  '
			m.d6tido='90'
		case ficx.d6tido='LE  '
			m.d6tido='89'
		case ficx.d6tido='DTE '
			m.d6tido='83'
		case ficx.d6tido='DOE '
			m.d6tido='84'
		case ficx.d6tido='ET  '
			m.d6tido='92'
		case ficx.d6tido='PAS '
			m.d6tido='94'
		otherwise
			m.d6tido='  '
	endcase
	&& R.04e Mariano

*	if !empty(ficx.D6Cuit) and seek(ficx.D6cuit,'f03')	&& R.04 Mariano
	if !empty(m.D6Cuit) and seek(m.D6cuit,'f03')	&& R.04 Mariano
		replace fic.d5clie with f03.d3prov,;
				fic.d5juri with f03.d3juri
		
		replace fic.d5tiim with ftiimp(fic.d5clie, fic.d5peri,'P')
	
	else
		
		replace fic.d6nume with fic.d5nume,;
				fic.d6peri with fic.d5peri,;
				fic.d6empr with d0coem,;
				fic.d6tipo with fic.d5tipo,;
				fic.d6sucu with fic.d5sucu,;
				fic.d6tico with fic.d5tico,;
				fic.d6tifo with fic.d5tifo,;
				fic.d6inte with fic.d5inte,;
				fic.d6nocl with ficx.d6nocl
		
*		replace fic.d6cuit with ficx.d6cuit	&& R.04 Mariano
		replace fic.d6tido with m.d6tido,;
				fic.d6cuit with m.d6cuit	&& R.04 Mariano
		
		do case
			
			case ficx.d5tiim='Inscripto'
				replace fic.d5clie with '99991',;
						fic.d5tiim with '1'

*						fic.d6tido with '80'	&& R.04 Mariano
				replace	fic.d6tido with iif(empty(m.d6tido),'80',m.d6tido)	&& R.04 Mariano

			case ficx.d5tiim='No Inscr.'
				replace fic.d5clie with '99992',;
						fic.d5tiim with '2'

*						fic.d6tido with '80'	&& R.04 Mariano
				replace	fic.d6tido with iif(empty(m.d6tido),'80',m.d6tido)	&& R.04 Mariano

			case ficx.d5tiim='Cons. Final'
				replace fic.d5clie with '99993',;
						fic.d5tiim with '3'	

*						fic.d6tido with '96'	&& R.04 Mariano
				replace	fic.d6tido with iif(empty(m.d6tido),'96',m.d6tido)	&& R.04 Mariano

			case ficx.d5tiim='Exento'
				replace fic.d5clie with '99994',;
						fic.d5tiim with '4'	

*						fic.d6tido with '80'	&& R.04 Mariano
				replace	fic.d6tido with iif(empty(m.d6tido),'80',m.d6tido)	&& R.04 Mariano

			case ficx.d5tiim='No Resp.'
				replace fic.d5clie with '99995',;
						fic.d5tiim with '5'

*						fic.d6tido with '96'	&& R.04 Mariano
				replace	fic.d6tido with iif(empty(m.d6tido),'96',m.d6tido)	&& R.04 Mariano

			case ficx.d5tiim='Monotributo'
				replace fic.d5clie with '99996',;
						fic.d5tiim with '6'

*						fic.d6tido with '80'	&& R.04 Mariano
				replace	fic.d6tido with iif(empty(m.d6tido),'80',m.d6tido)	&& R.04 Mariano

			case ficx.d5tiim='Suj. No Categ.'
				replace fic.d5clie with '99997',;
						fic.d5tiim with '7'

*						fic.d6tido with '96'	&& R.04 Mariano
				replace	fic.d6tido with iif(empty(m.d6tido),'96',m.d6tido)	&& R.04 Mariano

		endcase
		set order to d3prov in f03
		if seek(fic.d5clie,'f03')
			replace fic.d5juri with f03.d3juri

			replace fic.d5tiim with ftiimp(fic.d5clie, fic.d5peri,'P')	&& R.02 Mariano

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
	replace fic.d5leye with allt(fic.d5leye)+' '+fic.d5tifo+' '+fic.d5nume
	
*****	Adua / Dest	*****

	if empty(ficx.d5adua) and fic.d5tifo='I' and !empty(ficx.d5desp)
			replace fic.d5adua with '000' 	&& codigo arbitrario para interfase, debe existir en la empresa
	else
		replace fic.d5adua with ficx.d5adua
	endif

	
	if empty(ficx.d5dest) and fic.d5tifo='I' and !empty(ficx.d5desp)
			replace fic.d5dest with 'IC01' 	&& codigo arbitrario para interfase, debe existir en la empresa
	else
		replace fic.d5dest with ficx.d5dest
	endif

*****	Rubro	*****

	if empty(ficx.d5rubr)
			replace fic.d5rubr with '999999' 	&& codigo arbitrario para interfase, debe existir en la empresa
	else
		replace fic.d5rubr with ficx.d5rubr
	endif
	
*****	Condicion de Venta	*****
	
	Do case
		case ficx.d5cond='Contado'
			replace fic.d5cond with '2'
		case ficx.d5cond='Cta Cte'
			replace fic.d5cond with '1'
		case ficx.d5cond='Otra'
			replace fic.d5cond with '3'
	Endcase	
	
***** 	Tipo de Operacion *****

	do case
			case ficx.d5tiop='No Corresponde'
				replace fic.d5tiop with '0'
			case ficx.d5tiop='No Alcanzado'
				replace fic.d5tiop with 'A'
*			case ficx.d5tiim='Operaci๓n de Canje'	&& R.02 Mariano
			case ficx.d5tiop='Operaci๓n de Canje'	&& R.02 Mariano
				replace fic.d5tiop with 'C'
*			case ficx.d5tiim='Operaci๓n Exenta'	&& R.02 Mariano
			case ficx.d5tiop='Operaci๓n Exenta'	&& R.02 Mariano
				replace fic.d5tiop with 'E'
*			case ficx.d5tiim='No Gravado'	&& R.02 Mariano
			case ficx.d5tiop='No Gravado'	&& R.02 Mariano
				replace fic.d5tiop with 'N'	
*			case ficx.d5tiim='Imp./Exp. Del Exterior'	&& R.02 Mariano
			case ficx.d5tiop='Imp./Exp. Del Exterior'	&& R.02 Mariano
				replace fic.d5tiop with 'X'
*			case ficx.d5tiim='Imp./Exp. Zona Franca'	&& R.02 Mariano
			case ficx.d5tiop='Imp./Exp. Zona Franca'	&& R.02 Mariano
				replace fic.d5tiop with 'Z'		
	Endcase
	
****** 	Percepciones ********

&& R.07b Mariano
	** Si viene la jurisdiccion en el comprobante la tomo de ahi **
	=seek('JU','f01')
	DO WHILE !EOF('f01') and f01.d1tip='JU'

		if upper(f01.d1des)=upper(ficx.d5juri)
	    	replace fic.d5juri with f01.D1cla
	    	exit
		ENDIF
 
		SKIP IN F01
	ENDDO
&& R.07e Mariano

	if empty(ficx.D5pere) and ficx.D5imp3!=0
		&& R.01b Mariano
*		replace fiv.d5pere with '000' 		&& codigo arbitrario para interfase, deberan modificar el comprobante
		go top in f24
*		do while f24.d24tipo!='I' or f24.d24refe!=iif(empty(ficx.d5imre),'P','R') and !eof('f24')	&& R.02 Mariano
		do while f24.d24tipo!='I' or f24.d24refe!=iif(ficx.d5tico!='RET','P','R') and !eof('f24')	&& R.02 Mariano
			skip in f24
		enddo
*		if f24.d24tipo='I' and f24.d24refe=iif(empty(ficx.d5imre),'P','R') and f24.d24codi='000'	&& R.02 Mariano
		if f24.d24tipo='I' and f24.d24refe=iif(ficx.d5tico!='RET','P','R') and f24.d24codi='000'	&& R.02 Mariano
			replace fic.d5pere with f24.d24codi 		&& codigo arbitrario para interfase, deberan modificar el comprobante
		else
			sele f24
			append blank
			=db_rlock('f24')
*			replace d24codi with '000', d24tipo with 'I', d24refe with iif(empty(ficx.d5imre),'P','R'), d24desc with 'INDEFINIDO'	&& R.02 Mariano
			replace d24codi with '000', d24tipo with 'I', d24refe with iif(ficx.d5tico!='RET','P','R'), d24desc with 'INDEFINIDO'	&& R.02 Mariano
			unlock in f24
			sele fic
			replace fic.d5pere with '000' 		&& codigo arbitrario para interfase, deberan modificar el comprobante
		endif
		&& R.01e Mariano
	endif				

	if empty(ficx.D5cpib)
		if (f00.p0prc1=1 and fic.d5imp6!=0) or (f00.p0prc2=1 and fic.d5imp7!=0) or (f00.p0prc3=1 and fic.d5imp8!=0)
			&& R.01b Mariano
*			replace fic.d5cpib with '000' 		&& codigo arbitrario para interfase, deberan modificar el comprobante
			=SEEK("JU" + fic.d5juri, "f01")
			cafip	= f01.d1tdgi
			if seek(cafip,'f25')
*				do while (f25.d25tdgi!=cafip or f25.d25refe!=iif(empty(ficx.d5imre),'P','R')) and !eof('f25')	&& R.02 Mariano
				do while (f25.d25tdgi!=cafip or f25.d25refe!=iif(ficx.d5tico!='RET','P','R')) and !eof('f25')	&& R.02 Mariano
					skip in f25
				enddo
*				if f25.d25tdgi=cafip and f25.d25refe=iif(empty(ficx.d5imre),'P','R') and f25.d25codi='000'	&& R.02 Mariano
*				if f25.d25tdgi=cafip and f25.d25refe=iif(ficx.d5imre!='RET','P','R') and f25.d25codi='000'	&& R.02 Mariano	&& R.06 Mariano
				if f25.d25tdgi=cafip and f25.d25refe=iif(ficx.d5tico!='RET','P','R') and f25.d25codi='000'	&& R.06 Mariano
					replace fic.d5cpib with f25.d25codi 		&& codigo arbitrario para interfase, deberan modificar el comprobante
				else
					sele f25
					append blank
					=db_rlock('f25')
*					replace d25tdgi with cafip, d25codi with '000', d25refe with iif(empty(ficx.d5imre),'P','R'), d25cafi with '000', d25desc with 'INDEFINIDO'	&& R.02 Mariano
					replace d25tdgi with cafip, d25codi with '000', d25refe with iif(ficx.d5tico!='RET','P','R'), d25cafi with '000', d25desc with 'INDEFINIDO'	&& R.02 Mariano
					unlock in f25
					sele fic
					replace fic.d5cpib with '000' 		&& codigo arbitrario para interfase, deberan modificar el comprobante
				endif
			endif
			&& R.01e Mariano
		endif
	endif				
	
	if empty(ficx.D5cpga)
		if (f00.p0prc1=2 and fic.d5imp6!=0) or (f00.p0prc2=2 and fic.d5imp7!=0) or (f00.p0prc3=2 and fic.d5imp8!=0)
			&& R.01b Mariano
*			replace fic.d5cpga with '000' 		&& codigo arbitrario para interfase, deberan modificar el comprobante
			go top in f24
*			do while f24.d24tipo!='G' or f24.d24refe!=iif(empty(ficx.d5imre),'P','R') and !eof('f24')	&& R.02 Mariano
			do while f24.d24tipo!='G' or f24.d24refe!=iif(ficx.d5tico!='RET','P','R') and !eof('f24')	&& R.02 Mariano
				skip in f24
			enddo
*			if f24.d24tipo='G' and f24.d24refe=iif(empty(ficx.d5imre),'P','R') and f24.d24codi='000'	&& R.02 Mariano
*			if f24.d24tipo='G' and f24.d24refe=iif(ficx.d5imre!='RET','P','R') and f24.d24codi='000'	&& R.02 Mariano	&& R.06 Mariano
			if f24.d24tipo='G' and f24.d24refe=iif(ficx.d5tico!='RET','P','R') and f24.d24codi='000'	&& R.06 Mariano
				replace fic.d5cpga with f24.d24codi 		&& codigo arbitrario para interfase, deberan modificar el comprobante
			else
				sele f24
				append blank
				=db_rlock('f24')
*				replace d24codi with '000', d24tipo with 'G', d24refe with iif(empty(ficx.d5imre),'P','R'), d24desc with 'INDEFINIDO'	&& R.02 Mariano
*				replace d24codi with '000', d24tipo with 'G', d24refe with iif(ficx.d5imre!='RET','P','R'), d24desc with 'INDEFINIDO'	&& R.02 Mariano	&& R.06 Mariano
				replace d24codi with '000', d24tipo with 'G', d24refe with iif(ficx.d5tico!='RET','P','R'), d24desc with 'INDEFINIDO'	&& R.06 Mariano
				unlock in f24
				sele fic
				replace fic.d5cpga with '000' 		&& codigo arbitrario para interfase, deberan modificar el comprobante
			endif
			&& R.01e Mariano
		endif
	endif
	
	if empty(ficx.D5cpot)
		if (f00.p0prc1=3 and fic.d5imp6!=0) or (f00.p0prc2=3 and fic.d5imp7!=0) or (f00.p0prc3=3 and fic.d5imp8!=0)
			&& R.01b Mariano
*			replace fic.d5cpot with '000' 		&& codigo arbitrario para interfase, deberan modificar el comprobante
			go top in f24
*			do while f24.d24tipo!='O' or f24.d24refe!=iif(empty(ficx.d5imre),'P','R') and !eof('f24')	&& R.02 Mariano
*			do while f24.d24tipo!='O' or f24.d24refe!=iif(ficx.d5imre!='RET','P','R') and !eof('f24')	&& R.02 Mariano	&& R.06 Mariano
			do while f24.d24tipo!='O' or f24.d24refe!=iif(ficx.d5tico!='RET','P','R') and !eof('f24')	&& R.06 Mariano
				skip in f24
			enddo
*			if f24.d24tipo='O' and f24.d24refe=iif(empty(ficx.d5imre),'P','R') and f24.d24codi='000'	&& R.02 Mariano
*			if f24.d24tipo='O' and f24.d24refe=iif(ficx.d5imre!='RET','P','R') and f24.d24codi='000'	&& R.02 Mariano	&& R.06 Mariano
			if f24.d24tipo='O' and f24.d24refe=iif(ficx.d5tico!='RET','P','R') and f24.d24codi='000'	&& R.06 Mariano
				replace fic.d5cpot with f24.d24codi 		&& codigo arbitrario para interfase, deberan modificar el comprobante
			else
				sele f24
				append blank
				=db_rlock('f24')
*				replace d24codi with '000', d24tipo with 'O', d24refe with iif(empty(ficx.d5imre),'P','R'), d24desc with 'INDEFINIDO'	&& R.02 Mariano
*				replace d24codi with '000', d24tipo with 'O', d24refe with iif(ficx.d5imre!='RET','P','R'), d24desc with 'INDEFINIDO'	&& R.02 Mariano	&& R.06 Mariano
				replace d24codi with '000', d24tipo with 'O', d24refe with iif(ficx.d5tico!='RET','P','R'), d24desc with 'INDEFINIDO'	&& R.06 Mariano
				unlock in f24
				sele fic
				replace fic.d5cpot with '000' 		&& codigo arbitrario para interfase, deberan modificar el comprobante
			endif
			&& R.01e Mariano
		endif
	endif

	&& R.07b Mariano
	if fic.d5tico!='R' and !empty(fic.d5imre)
		replace fic.d5imre with 0
	endif
	&& R.07e Mariano

	SKIP IN ficx

ENDDO

sele ficx
zap

endif 	&& R.08 Mariano

sele fic
use in fic

endif

RETURN

&& R.01b Mariano
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc brow06in
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

endif	&& R.01 Mariano

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
&& R.01e Mariano


&& Puesto adrede para que incluya el report en el proyecto
repo form iwr006in
