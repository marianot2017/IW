*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : IWPSIPOT.PRG -> Exportar Percep./Retenc. AFIP Ing. Brutos
*)-H									COMPRAS ( SIPOT SALTA )			
*)-H RELEASE		 : 01
*)-H
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

* Si AFIP hace cambios, buscar letcomprob y tipcomprob


if empty(parameters())
	w_pref=''
	w_call=.f.
endif

*) Salvado de estados anteriores-----------------------------------------------

push key clear		  &&(Salva estados de teclas)
push menu _msysmenu &&(Salva estado de menu)
set skip of menu _msysmenu .t.

*) Variables internas----------------------------------------------------------

w_pfile='962'
w_prog =upper(prompt())
if w_pref='P'
	w_title=upper('SIAP - SIPOT - Exportar Per/Ret practicadas I.B. Salta')
else
	w_title=upper('SIAP - SIPOT - Exportar Per/Ret sufridas I.B. Salta')
endif
w_alta =.f.
w_alias='f05'

*) Apertura de archivos--------------------------------------------------------

fieldvec[1]='F00'
fieldvec[2]='F01'
fieldvec[3]='F02'
fieldvec[4]='F03'
fieldvec[5]='F04'
fieldvec[6]='F05'
fieldvec[6]='F06'						

use (d0f00) in 0 alias 'f00'						&&(Parametros empresa)
use (d0f01) in 0 alias 'f01'						&&(Tablas)
use (d0f02) in 0 alias 'f02' order 'd2clie'  		&&(Clientes)
use (d0f03) in 0 alias 'f03' order 'd3prov'  		&&(Proveedores)
use (d0f04) in 0 alias 'f04' order 'd4peri'			&&(Periodos)
use (d0f05) in 0 alias 'f05' order 'd5peri'			&&(Comprobantes)
use (d0f06) in 0 alias 'f06' order 'd6inte'			&&(Clientes Eventuales)		
*use (d0f24) in 0 alias 'f24' order 'd24codi' 		&&(Percep./Retenc./Otros)

if neterr()
	pop key
	pop menu _msysmenu
	fieldvec=''
	=closall()
	return
endif

fieldvec=''

sele (w_alias)

*) Variables-------------------------------------------------------------------

go bottom in f04
wdperi=f04.d4peri


*===> SIPOT es solo COMPRAS

w_texto	= "COMPRAS"
w_texto1	= 'Perํodo : '

w_cpra	= .T.
w_perc	= .T.
w_rete	= .T.

w_dire	= " "
w_dipe	= " "

w_repclaus = " "

DO dirs

*===>									SALTA
SELE f01
LOCATE FOR d1tip = "JU"		AND d1tdgi = "09"

w_prov	= ALLT(d1cla)
w_cond	= " "
IF !EMPTY(d1cla)
			do ssm998.mpr
			do iwscpa2.spr
ELSE
	=advgrave("NO ESTA DEFINIDA LA JURISDICCION CORRESPONDIENTE A SALTA")
ENDIF			

*)-----------------------------------------------------------------------------

pop key
pop menu _msysmenu
=closall()

dele file &d0f99.b.dbf
dele file &d0f99.c.dbf
DELE FILE ivf099b.idx

return

***vfp***
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons962
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	on key label f9 do cons962x
	keyboard '{f9}'
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons962x
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
on key label f9
***vfp***

if .not. valid962(.t.)
	wait wind 'Verifique los datos ingresados...'
	return
endif

if !w_perc and !w_rete 
	wait window 'Debe elegir Percepciones y/o Retenciones'
	IF !w_perc 
			_curobj = objnum(w_perc)
	ELSE
		_curobj = objnum(w_rete)
	ENDIF
	w_retu = .F.		
	SHOW GETS
	return
endif

	=gentmp()

	if used('f99b')
		sele f99b
		use
	endif
	if used('f99c')
		sele f99c
		use
	endif
	dele file f99b.dbf
	dele file f99b.idx	
	dele file f99c.dbf
	dele file f99c.idx	

	w_print	= .F.

	if w_pref='P'
		IF d0comu	= "N"	
			SELE 0
			SELECT *														;		
					FROM f05												;
						WHERE d5peri = wdperi								;
							AND	 d5tipo = 'V' AND !EMPTY(d5regi)			;
							AND d5juri <> w_prov							;
						INTO CURSOR f99b
			IF RECCOUNT() <> 0 and w_perc
							=advgrave("EXISTEN VENTAS FUERA DE LA JURISDICCION DE SALTA")
							w_cond = " d5juri <> w_prov "
							DO genera
							p_report='iwripotp'
							w_tit1	= "SIPOT - PER. / RET. PRACTICADAS - INGRESOS BRUTOS - FUERA DE SALTA"
							INDEX ON signo + fecha + numcomprob		TO f99b.idx
							DO listalib
							USE
							DELE FILE F99b.IDX
			ENDIF									
		ENDIF

		SELE 0
	
		SELECT *													;		
			FROM f05												;
				WHERE d5peri = wdperi							;
					AND	 d5tipo = 'V' AND !EMPTY(d5regi)		;
					AND d5juri = w_prov							;
				INTO CURSOR f99b

		set relation to d5clie into f03
		SET RELA TO d5clie		INTO f02	ADDITIVE
	else
		IF d0comu	= "N"	
			SELE 0
			SELECT *														;		
					FROM f05												;
						WHERE d5peri = wdperi								;
							AND	 d5tipo = 'C' AND !EMPTY(d5regi)			;
							AND d5juri <> w_prov							;
						INTO CURSOR f99b
			IF RECCOUNT() <> 0 and w_perc
							=advgrave("EXISTEN COMPRAS FUERA DE LA JURISDICCION DE SALTA")
							w_cond = " d5juri <> w_prov "
							DO genera
							p_report='iwripotp'
							w_tit1	= "SIPOT - PER. / RET. SUFRIDAS - INGRESOS BRUTOS - FUERA DE SALTA"
							INDEX ON signo + fecha + numcomprob		TO f99b.idx
							DO listalib
							USE
							DELE FILE F99b.IDX
			ENDIF									
		ENDIF

		SELE 0
	
		SELECT *													;		
			FROM f05												;
				WHERE d5peri = wdperi							;
					AND	 d5tipo = 'C' AND !EMPTY(d5regi)		;
					AND d5juri = w_prov							;
				INTO CURSOR f99b

		set relation to d5clie into f03
		SET RELA TO d5clie		INTO f02	ADDITIVE
	endif
 	go top
	if eof()
		wait window 'Sin movimientos para exportar'
		IF w_print
				do represet &&(Resetea estado)
		ENDIF
		return
	endif
*endif

if w_perc	

	w_cond = " f99b.d5regi='P' "
	DO genera
	
	do agrucprape
	go top
	
	if eof('f99c')
	
		wait window 'PERCEPCIONES '+iif(w_pref='P','PRACTICADAS ','SUFRIDAS ')+'- Sin movimientos para exportar'
	else
		p_report='iwripotp'
		w_tit1	= 'SIAP - SIPOT - EXPORTACION PERCEPCIONES '+iif(w_pref='P','PRACTICADAS ','SUFRIDAS ')+'- INGRESOS BRUTOS - Salta'
		INDEX ON signo + fecha + numcomprob		TO f99c.idx
		DO listalib
		w_retu = creadir(curdir()+'\AFIP\INGBRU\SALTA')
		if w_retu = 0 or w_retu = 6
			if w_pref='P'
				COPY TO &w_dipe FIELDS EXCEPT signo, dia, domicilio, importe, montobase			TYPE SDF
			else
				COPY TO &w_dipe FIELDS EXCEPT signo, dia, domicilio, importe, montobase, mbase	TYPE SDF
			endif
		endif
	endif

endif

if w_rete

	w_cond = " f99b.d5regi='R' "
	DO genera

	do agrucprare
	go top

	if eof('f99c')
		wait window 'RETENCIONES '+iif(w_pref='P','PRACTICADAS ','SUFRIDAS ')+'- Sin movimientos para informar'
	else
		p_report='iwripotr'
		w_tit1	= 'SIAP - SIPOT - EXPORTACION RETENCIONES '+iif(w_pref='P','PRACTICADAS ','SUFRIDAS ')+'- INGRESOS BRUTOS - Salta'

		INDEX ON signo + fecha + numcomprob		TO f99c.idx
		DO listalib
		w_retu = creadir(curdir()+'\AFIP\INGBRU\SALTA')
		if w_retu = 0 or w_retu = 6
			if w_pref='P'
				COPY TO &w_dire FIELDS EXCEPT signo, fecha, tipcomprob, importe, montobase				   TYPE SDF 
			else
				COPY TO &w_dire FIELDS EXCEPT signo, dia, tipcomprob, domicilio, importe, montobase, mbase TYPE SDF 
			endif
		endif
	endif

endif

*if !empty(p_repclaus) &&(debe ser preview o to printer)
IF w_print
			do represet &&(Resetea estado)
endif

return


*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC when962
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
w_retu=.t.
w_vari=varread()
p_curobj=_curobj

return(w_retu)

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC valid962
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS w_show

if empty(parameters())
	w_show=.f.
endif

w_retu=.t.
w_vari=varread()
w_vread='m.'+w_vari

if w_vari='WDPERI' or w_show
	if lastkey()=p_f4
		do fhelp with 'WDPERI','','iwp004','f04','d4peri','d4peri','',(p_char),'w_retu'
	endif

*===>
	w_fech	= CTOD('01/' + wdperi)
	IF EMPTY(w_fech)
					RETURN	.F. 
	ENDIF
	IF trdate('w_fech')
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
*===>
	IF w_retu
			wkperi = PtoI(wdperi)

			if ! seek(wkperi,'f04')
					if !w_show
							wait window 'Perํodo Inexistente'
					endif
					w_retu=.f.
					_curobj	= objnum(wdperi)
					RETURN .F.
			endif
	ENDIF
endif

if w_vari='W_PERC'
	=mchkbox(@w_perc)
endif

if w_vari='W_RETE'
	=mchkbox(@w_rete)
endif

if w_vari='W_RETE' or w_show
			if (!w_perc and !w_rete)
						wait window 'Debe elegir Percepciones y/o Retenciones de IB'
						IF !w_perc 
								_curobj = objnum(w_perc)
						ELSE
							_curobj = objnum(w_rete)
						ENDIF
						w_retu = .F.		
						SHOW GETS
			endif
endif

DO dirs
		
return (w_retu)

************************************************************
proc agrucprape	 && Agrupa comprobantes de perc. de compras iguales
************************************************************

private w_cant, w_recno

index on fecha+tipcomprob+proveedor+numcomprob tag agrupa
go top
do while !eof('f99c') 
	scatter memvar
	w_recno=recno('f99c')

	** acumulo el comprobante

	sum importe, montobase to m.importe, m.montobase ;
		while m.fecha+m.tipcomprob+m.proveedor+m.numcomprob ;
		=fecha+tipcomprob+proveedor+numcomprob
			
	go w_recno in f99c
	gather memvar
	
	skip in f99c

	do while m.fecha+m.tipcomprob+m.proveedor+m.numcomprob ;
	=fecha+tipcomprob+proveedor+numcomprob and !eof('f99c')
		delete		
		skip in f99c
	enddo

enddo

return

************************************************************
proc agrucprare	 && Agrupa comprobantes de ret. de compras iguales
************************************************************

private w_cant, w_recno

index on fecha+proveedor+numcomprob tag agrupa
go top
do while !eof('f99c') 
	scatter memvar
	w_recno=recno('f99c')

	** acumulo el comprobante

	sum importe, montobase to m.importe, m.montobase ;
		while m.fecha+m.proveedor+m.numcomprob=fecha+proveedor+numcomprob
			
	go w_recno in f99c
	gather memvar
	
	skip in f99c

	do while m.fecha+m.proveedor+m.numcomprob=fecha+proveedor+numcomprob and !eof('f99c')
		delete		
		skip in f99c
	enddo

enddo

return


************************************************************
function tipcomprob
************************************************************
parameter w_ttab
private w_tafi
do case
	case w_ttab$'001-005-063-081'	&& R.01 Mariano (agregue 0)
		w_tafi='FA_A'
	case w_ttab$'006-010-064-082'	&& R.01 Mariano (agregue 0)
		w_tafi='FA_B'
*	case w_ttab$'11-16-65-83'		&& R.01 Mariano 
	case w_ttab$'011-016-068-083'	&& R.01 Mariano (agregue 0)
		w_tafi='FA_C'
	case w_ttab$'084'	&& R.01 Mariano (agregue 0)
		w_tafi='FA_P'
	case w_ttab$'003-112'	&& R.01 Mariano (agregue 0)
		w_tafi='NC_A'
	case w_ttab$'008-113'	&& R.01 Mariano (agregue 0)
		w_tafi='NC_B'
	case w_ttab$'013-110'	&& R.01 Mariano (agregue 0)
		w_tafi='NC_C'
	case w_ttab$'085'	&& R.01 Mariano (agregue 0)
		w_tafi='NC_P'
	case w_ttab$'002-115'	&& R.01 Mariano (agregue 0)
		w_tafi='ND_A'
	case w_ttab$'007-116'	&& R.01 Mariano (agregue 0)
		w_tafi='ND_B'
	case w_ttab$'012'	&& R.01 Mariano (agregue 0)
		w_tafi='ND_C'
	case w_ttab$'086'	&& R.01 Mariano (agregue 0)
		w_tafi='ND_P'
	&& R.01b Mariano
	case w_ttab$'051-118'
		w_tafi='FA_M'
	case w_ttab$'053-119'
		w_tafi='NC_M'
	case w_ttab$'052-120'
		w_tafi='ND_M'
	&& R.01e Mariano
	otherwise
		w_tafi='OTRO'						
endcase
return (w_tafi)

*===============================================================================
PROCEDURE dirs

IF w_perc
		w_dipe=curdir()+'AFIP\INGBRU\SALTA\P'+iif(w_pref='P','P','S')+ f77.d77empr + right(ptoi(wdperi),4)+'.TXT'
ELSE
	w_dipe=''
ENDIF

IF w_rete
		w_dire=curdir()+'AFIP\INGBRU\SALTA\R'+iif(w_pref='P','P','S')+ f77.d77empr + right(ptoi(wdperi),4)+'.TXT'
ELSE
	w_dire=''
ENDIF

SHOW GET w_dire
SHOW GET w_dipe

RETURN

*================================================================================
PROCEDURE genera

SELE 0
CREATE CURSOR f99c (signo c(1), dia c(2), fecha c(8), tipcomprob c(4), numcomprob c(iif(w_cond = " f99b.d5regi='R' ",15,20)), proveedor c(60), domicilio c(60), cuit c(11), mbase c(16), montobase n(16,2), impte c(16), importe n(16,2))

SELE f99b
go top in f99b
do while !eof('f99b')
		if &w_cond    

			SELE f99b

			w_cuit=''
			w_domi=''

			m.proveedor	= RETNOMC(f99b.d5clie,f99b.d5inte)

			m.cuit	= left(alltrim(strtran(w_cuit,'-',''))+space(11),11)
			
			m.domicilio= w_domi

			m.fecha=strtran(dtoc(f99b.d5fech),'/','')
			
			m.dia=left(m.fecha,2)
			
			m.tipcomprob=tipcomprob(f99b.d5coco)
			
			m.numcomprob=padl(allt(strtran(strtran(f99b.d5nume,'-',''),'/','')),iif(f99b.d5tico='R',15,20),'0')
			
			m.montobase = iif(f99b.d5tico='R',f99b.d5imre,f99b.d5imp1 + f99b.d5imp2)
			
			w_campo=''
			for i=1 to 3
				w_cprc='f00.p0prc'+(str(i,1))
				if &w_cprc=1   && IB
					w_campo=w_cprc
					exit
				endif						
			endfor					
			w_campo=iif(!empty(w_campo),str(val(right(w_campo,1))+5,1),'0')
			w_campo=iif(w_campo>'0','f99b.d5imp'+w_campo,'0')
*===>
*			m.importe=iif(f99b.d5tico='R',abs(f99b.d5imp3),abs(eval(w_campo)))
			m.importe = iif(f99b.d5tico='R',f99b.d5tota,eval(w_campo))

			m.impte=strtran(str(m.importe,16,2),' ','0')

			m.mbase=strtran(str(m.montobase,16,2),' ','0')

			IF m.importe >= 0
				m.signo = "+"
			ELSE
				m.signo = "-"
				m.impte = '-'+right(m.impte,15)
				m.mbase = '-'+right(m.mbase,15)
			ENDIF										

			SELE f99c
			append blank
			gather memvar
		endif			
		SELE f99b
		skip in f99b
enddo

SELE f99c	

RETURN

*================================================================================
***puesto adrede para que incluya el report al proyecto**
report form iwripotp
report form iwripotr
