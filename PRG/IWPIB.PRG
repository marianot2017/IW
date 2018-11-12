*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : IWPIB.PRG -> Exportar Percep./Retenc. AFIP Ing. Brutos
*										COMPRAS ( IB / IB2 ) - Buenos Aires		
*									Arba WEB
*	 RELEASE		 : 09
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

&& R.02b Ra๚l
*w_title=upper('SIAP - IB - Exportar Per/Ret I.B. Buenos Aires')
&& R.06b Mariano
*IF w_pref = '1'
do case
	case w_pref = '1'
		w_title=upper('SIAP - IB - Exportar Per/Ret I.B. Buenos Aires')
	case w_pref = '2'
*ELSE
		w_title=upper('SIAP - IB2 - Exportar Per/Ret I.B. Buenos Aires')
*ENDIF
	otherwise
		w_title=upper('ARBA WEB - Exportar Per/Ret I.B. Buenos Aires')
endcase
w_quin='1ra.'
&& R.06e Mariano

&& R.02e Ra๚l

w_alta =.f.
w_alias='f05'

*) Apertura de archivos--------------------------------------------------------

fieldvec[1]='F00'
fieldvec[2]='F01'
fieldvec[3]='F02'
fieldvec[4]='F03'
fieldvec[5]='F04'
fieldvec[6]='F05'
fieldvec[7]='F06'				&& R.01 Ra๚l

use (d0f00) in 0 alias 'f00'						&&(Parametros empresa)
use (d0f01) in 0 alias 'f01'						&&(Tablas)
use (d0f02) in 0 alias 'f02' order 'd2clie'  		&&(Clientes)
use (d0f03) in 0 alias 'f03' order 'd3prov'  		&&(Proveedores)
use (d0f04) in 0 alias 'f04' order 'd4peri'			&&(Periodos)
use (d0f05) in 0 alias 'f05' order 'd5peri'			&&(Comprobantes)
use (d0f06) in 0 alias 'f06' order 'd6inte'			&&(Clientes Eventuales)				&& R.01 Ra๚l
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

&& R.05b Ra๚l
*wdperi=f04.d4peri
IF w_pref = '1'	or w_pref = '3'	&& R.06 Mariano (agregue = 3)
	wdperi=f04.d4peri
ELSE
	wdperi = ' '			
ENDIF	
&& R.05e Ra๚l

*===> IB es solo COMPRAS

w_texto	= "COMPRAS"

&& R.02b Ra๚l
*IF w_pref	= '1'	&& R.06 Mariano
IF w_pref	= '1' or w_pref = '3'	&& R.06 Mariano
			w_texto1	= 'Perํodo : '
ELSE
	w_texto1	= 'Bimestre : '
ENDIF
&& R.02e Ra๚l

w_cpra	= .T.
w_perc	= .T.
w_rete	= .T.

w_dire	= " "
w_dipe	= " "

w_repclaus = " "

DO dirs

*===>									Buenos Aires
SELE f01
LOCATE FOR d1tip = "JU"		AND d1tdgi = "01"

w_prov	= ALLT(d1cla)
w_cond	= " "
IF !EMPTY(d1cla)
			do ssm998.mpr
			&& R.06b Mariano
			if w_pref='3'
				do iwscpa3.spr
			else
			&& R.06e Mariano
				do iwscpa2.spr
			endif	&& R.06 Mariano
ELSE
	=advgrave("NO ESTA DEFINIDA LA JURISDICCION CORRESPONDIENTE A BUENOS AIRES")
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


&& R.06b Mariano
*IF w_pref = '1'
do case
	case w_pref = '1'
			w_cond1 = ' d5peri = wdperi '
			whperi = wdperi		&& R.04 Mariano
*ELSE
	case w_pref = '2'
	DO CASE
		CASE LEFT(wdperi,2) = '01'
							wdperi	= '01' + RIGHT(wdperi,5)
							whperi	= '02' + RIGHT(wdperi,5)
		CASE LEFT(wdperi,2) = '02'
							wdperi	= '03' + RIGHT(wdperi,5)
							whperi	= '04' + RIGHT(wdperi,5)
		CASE LEFT(wdperi,2) = '03'
							wdperi	= '05' + RIGHT(wdperi,5)
							whperi	= '06' + RIGHT(wdperi,5)
		CASE LEFT(wdperi,2) = '04'
							wdperi	= '07' + RIGHT(wdperi,5)
							whperi	= '08' + RIGHT(wdperi,5)
		CASE LEFT(wdperi,2) = '05'
							wdperi	= '09' + RIGHT(wdperi,5)
							whperi	= '10' + RIGHT(wdperi,5)
		CASE LEFT(wdperi,2) = '06'
							wdperi	= '11' + RIGHT(wdperi,5)
							whperi	= '12' + RIGHT(wdperi,5)
	ENDCASE
	w_cond1 = ' BETWEEN(d5peri, wdperi, whperi) '
*ENDIF			  
	otherwise
			w_cond1 = ' d5peri = wdperi and iif(w_quin="1ra.", dtos(d5fech)<dtos(ctod("16/"+wdperi)), iif(w_quin="2da.", dtos(d5fech)>dtos(ctod("15/"+wdperi)), .t.)) '
			whperi = wdperi
endcase
&& R.06e Mariano

	IF d0comu	= "N"
			SELE 0
&& R.03b Mariano
*			SELECT *														;		
*					FROM f05												;
*						WHERE &w_cond										;
*							AND	 d5tipo = 'C' AND !EMPTY(d5regi)			;
*							AND d5juri <> w_prov							;
*						INTO CURSOR f99b		
			SELECT *														;		
					FROM f05,f01											;
						WHERE &w_cond1.										;
							AND	 d5tipo = 'C' AND !EMPTY(d5regi)			;
							AND 'JU'+d5juri = d1tip+allt(d1cla)				;
							AND d1tdgi <> '01'								;
						INTO CURSOR f99b		&& R.06 Mariano (modifique &w_cond por &w_cond1.)	
&& R.03e Mariano
			IF RECCOUNT() <> 0
							=advgrave("EXISTEN COMPRAS FUERA DE LA JURISDICCION DE BUENOS AIRES")
*							w_cond = " d5juri <> w_prov "	&& R.03 Mariano
							w_cond = " d1tdgi <> '01' "		&& R.03 Mariano
							DO genera
							p_report='iwribp'
							&& R.06b Mariano
*							IF w_pref	= '1'
							do case
								case w_pref = '1'
									w_tit1 = "IB - PER. / RET. INGRESOS BRUTOS FUERA DE BUENOS AIRES"
*							ELSE
								case w_pref = '2'
									w_tit1 = "IB 2 - PER. / RET. INGRESOS BRUTOS FUERA DE BUENOS AIRES"
*							ENDIF
								otherwise
									w_tit1 = "ARBA WEB - PER. / RET. INGRESOS BRUTOS FUERA DE BUENOS AIRES"
							endcase
							&& R.06e Mariano
							INDEX ON signo + fecha + numcomprob		TO f99b.idx
							DO listalib
							USE
							DELE FILE f99b.idx
			ENDIF									
	ENDIF

	SELE 0
	
&& R.03b Mariano
*	SELECT *													;		
*		FROM f05												;
*				WHERE &w_cond									;
*					AND	 d5tipo = 'C' AND !EMPTY(d5regi)		;
*					AND d5juri = w_prov							;
*				INTO CURSOR f99b

	SELECT *													;		
		FROM f05,f01											;
				WHERE &w_cond1.									;
					AND	 d5tipo = 'C' AND !EMPTY(d5regi)		;
					AND 'JU'+d5juri = d1tip+allt(d1cla) 		;
					AND d1tdgi = '01'							;
				INTO CURSOR f99b		&& R.06 Mariano (modifique &w_cond por &w_cond1.)
&& R.03e Mariano

	set relation to d5clie	into f03
	SET RELA TO d5clie		INTO f02	ADDITIVE
 	go top
	if eof()
		wait window 'Sin movimientos para exportar'
		IF w_print
				do represet &&(Resetea estado)
		ENDIF
		return
	endif

if w_perc	
	w_cond = " f99b.d5regi='P' "

	DO genera

	w_cond	= " "

	do agrucprape
	go top
	
	if eof('f99c')
		wait window 'PERCEPCIONES -Sin movimientos para exportar'
	else
		p_report='iwribp'
		&& R.06b Mariano
*		IF w_pref	= '1'
		do case
			case w_pref = '1'
				w_tit1 = "SIAP - IB - EXPORTACION PERCEPCIONES INGRESOS BRUTOS - Buenos Aires"
*		ELSE
			case w_pref = '2'
				w_tit1 = "SIAP - IB 2 - EXPORTACION PERCEPCIONES INGRESOS BRUTOS - Buenos Aires"
*		ENDIF
			otherwise
				w_tit1 = "ARBA WEB - EXPORTACION PERCEPCIONES INGRESOS BRUTOS - Buenos Aires"
		endcase
		&& R.06e Mariano
		
		INDEX ON signo + fecha + sucursal + numcomprob		TO f99c.idx
		DO listalib
		w_retu = creadir(curdir()+'\AFIP\INGBRU\BAIRES')
		if w_retu = 0 or w_retu = 6
			&& R.06b Mariano
			if w_pref='3'
				COPY TO &w_dipe FIELDS EXCEPT signo, proveedor		TYPE SDF
				=gene01g9(w_dipe,'7')
			else
*				COPY TO &w_dipe FIELDS EXCEPT signo, proveedor		TYPE SDF
				COPY TO &w_dipe FIELDS EXCEPT signo, proveedor, imponible, toperacion		TYPE SDF
			endif
			&& R.06e Mariano
		endif
	endif
	USE
	dele file f99c.dbf
	dele file f99c.idx
endif

if w_rete
	w_cond	= " f99b.d5regi='R' "

	DO genera

	w_cond	= " "

	do agrucprare
	go top

	if eof('f99c')
		wait window 'RETENCIONES - Sin movimientos para informar'
	else
		p_report='iwribr'
		&& R.06b Mariano
*		IF w_pref	= '1'
		do case
			case w_pref = '1'
				w_tit1 = "SIAP - IB - EXPORTACION RETENCIONES INGRESOS BRUTOS - Buenos Aires"
*		ELSE
			case w_pref = '2'
				w_tit1 = "SIAP - IB 2 - EXPORTACION RETENCIONES INGRESOS BRUTOS - Buenos Aires"
*		ENDIF
			otherwise
				w_tit1 = "ARBA WEB - EXPORTACION RETENCIONES INGRESOS BRUTOS - Buenos Aires"
		endcase
		&& R.06e Mariano
		
		INDEX ON signo + fecha + numcomprob		TO f99c.idx
		DO listalib
		w_retu = creadir(curdir()+'\AFIP\INGBRU\BAIRES')
		if w_retu = 0 or w_retu = 6
			&& R.06b Mariano
			if w_pref='3'
				COPY TO &w_dire FIELDS EXCEPT signo, proveedor, tipcomprob, letcomprob ;
						TYPE SDF 
				=gene01g9(w_dire,'6')
			else
*				COPY TO &w_dire FIELDS EXCEPT signo, proveedor, tipcomprob, letcomprob ;
						TYPE SDF 
				COPY TO &w_dire FIELDS EXCEPT signo, proveedor, tipcomprob, letcomprob, toperacion ;
						TYPE SDF 
			endif
			&& R.06e Mariano
		endif
	endif
	USE
	dele file f99c.dbf
	dele file f99c.idx
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
*  IF w_pref = '1'							&& R.02 Ra๚l	&& R.06 Mariano
  IF w_pref = '1' or w_pref = '3'			&& R.06 Mariano
	if lastkey()=p_f4
		do fhelp with 'WDPERI','','iwp004','f04','d4peri','d4peri','',(p_char),'w_retu'
	endif
&& R.02b Ra๚l
  ELSE
	  IF !LEFT(wdperi,2) $ '01.02.03.04.05.06'	
			IF !w_show
					WAIT WIND "El bimestre no es correcto"
					w_retu = .F.
			ENDIF
	  ENDIF
  ENDIF					
&& R.02e Ra๚l


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
*	IF w_retu							&& R.03 Mariano
	IF w_retu  and w_pref = '1'			&& R.03 Mariano

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

index on fecha+tipcomprob+letcomprob+sucursal+proveedor+numcomprob tag agrupa
go top
do while !eof('f99c') 
	scatter memvar
	w_recno=recno('f99c')

	** acumulo el comprobante

	sum importe to m.importe		;
		while m.fecha+m.tipcomprob+m.letcomprob+m.sucursal+m.proveedor+m.numcomprob ;
		=fecha+tipcomprob+letcomprob+sucursal+proveedor+numcomprob
			
	go w_recno in f99c
	gather memvar
	
	skip in f99c

	do while m.fecha+m.tipcomprob+m.letcomprob+m.sucursal+m.proveedor+m.numcomprob ;
	=fecha+tipcomprob+letcomprob+sucursal+proveedor+numcomprob and !eof('f99c')
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

	sum importe to m.importe ;
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
	case w_ttab$'001-005-006-010-011-014-016-019-022-030-034-035-036-039-040-041-051-092-093-087' 				+;
			'060-061-062-063-064-065-080-081-082-083-084-087-088-096-097-055-056-057-058-059-099' +;
			'111-118'	&& R.07 Mariano (agregue 0 y >99)
		w_tafi='F'

	case w_ttab$'004-009-015-070-054'	&& R.07 Mariano (agregue 0)
		w_tafi='R'

	case w_ttab$'002-007-012-020-037-086-094-052-089-115-116-117-120'	&& R.07 Mariano (agregue 0 y >99)
		w_tafi='D'

	case w_ttab$'003-008-013-021-038-085-095-090-053-110-112-113-114-119'	&& R.07 Mariano (agregue 0 y >99)
		w_tafi='C'

	otherwise
		w_tafi=' '						
endcase
return w_tafi

*===============================================================================
PROCEDURE dirs

IF w_perc
		w_dipe=curdir()+'AFIP\INGBRU\BAIRES\PC'+ f77.d77empr + right(ptoi(wdperi),4)+'.TXT'
ELSE
	w_dipe=''
ENDIF

IF w_rete
		w_dire=curdir()+'AFIP\INGBRU\BAIRES\RC'+ f77.d77empr + right(ptoi(wdperi),4)+'.TXT'
ELSE
	w_dire=''
ENDIF

&& R.02b Ra๚l
IF w_pref = '2'
			w_dipe = STRTRAN(w_dipe, '\PC', '\P2')
			w_dire = STRTRAN(w_dire, '\RC', '\R2')
ENDIF
&& R.02e Ra๚l

SHOW GET w_dire
SHOW GET w_dipe

RETURN

*================================================================================
PROCEDURE genera

SELE 0

*IF 	w_cond = " f99b.d5regi='P' "	OR		w_cond = " d5juri <> w_prov "	&& R.03 Mariano
IF 	w_cond = " f99b.d5regi='P' "	OR		w_cond = " d1tdgi <> '01' "			&& R.03 Mariano
				CREATE CURSOR f99c (signo C(1), cuit c(13), fecha c(10), ;
									tipcomprob c(1), letcomprob c(1), sucursal c(4), ;
									numcomprob c(8), imponible n(12,2), importe n(11,2), proveedor c(5), Toperacion c(1) )	&& R.06 Mariano (agregue imponible y Toperacion)
ENDIF


IF 	w_cond = " f99b.d5regi='R' "

				CREATE CURSOR f99c (signo C(1), cuit c(13), fecha c(10), ;
									tipcomprob c(1), letcomprob c(1), sucursal c(4), ;
									numcomprob c(8), importe n(iif(w_pref='3',11,10),2), proveedor c(5), Toperacion c(1) )	&& R.06 Mariano (modifique tama๑o importe y agregue Toperacion)
ENDIF


SELE f99b
go top in f99b
do while !eof('f99b')
		if &w_cond    

			SELE f99b
			w_cuit	= SPACE(13)
			w_x		= RETNOMC(f99b.d5clie,f99b.d5inte)
			m.cuit	= w_cuit

			m.fecha=dtoc(f99b.d5fech)
			
			m.tipcomprob=tipcomprob(f99b.d5coco)

			m.letcomprob	=	f99b.d5tifo
			IF !m.letcomprob$"ABCM"
						m.letcomprob = " "
			ENDIF						

*			m.sucursal=left(f99b.d5nume,4)	&& R.09 Mariano
			m.sucursal=right(left(f99b.d5nume,5),4)	&& R.09 Mariano
*			m.numcomprob=right(f99b.d5nume,8)	&& R.08 Mariano
*			m.numcomprob=right(left(f99b.d5nume,13),8)	&& R.08 Mariano	&& R.09 Mariano
			m.numcomprob=right(left(f99b.d5nume,14),8)	&& R.09 Mariano
			
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

			m.imponible = iif(f99b.d5tico='R',0,f99b.d5imp1 + f99b.d5imp2)	&& R.06 Mariano
			
			m.toperacion = 'A'	&& R.06 Mariano

			IF m.importe >= 0
						m.signo = "+"
			ELSE
				m.signo = "-"
			ENDIF										
			
			m.proveedor=f99b.d5clie
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
report form iwribp
report form iwribr

&& R.06b Mariano
***********************************************************************
FUNC gene01g9  && compacta el archivo, genera el hash y renombra
***********************************************************************
parameters w_fige,w_rop

private w_fnew,w_fzip,wdidj
wdidj=justpath(w_fige)+'\'
if file(w_fige)
	if file(w_fige)
		w_fnew=allt(wdidj)+'AR-'+strtran(f00.p0cui,'-','')+'-'+right(wdperi,4)+left(wdperi,2)+iif(w_quin='1ra.','1', iif(w_quin='2da.','2','0'))+'-'+w_rop+'-LOTE1.TXT'
		w_fzip=strtran(w_fnew,'.TXT','.ZIP')
		private w_vfox,w_inst001,w_inst002,w_inst003,w_inst004,w_inst005,w_inst006,w_inst007
		w_inst001='copy file '+w_fige+' to '+w_fnew
		w_inst002='erase &w_fzip.'
		w_inst003='=pkzip("'+w_fzip+'","'+w_fnew+'",,,,,,'+')'
		w_inst004='w_hash=hash("'+w_fzip+'")'
		w_inst005='w_file=upper("'+strtran(w_fnew,'.TXT','')+'_'+'&w_hash.'+'.ZIP'+'")'
		w_inst006='erase &w_file.'
		w_inst007='=pkzip("'+'&w_file.'+'","'+w_fnew+'",,,,,,'+')'
		private d0f9fe
		d0f9fe=getenv('temp')+'\'+sys(3)
		w_vfox=d0f9fe+'.mem'
		save to &w_vfox. all like w_inst0*
		release all like w_inst0*
		=visuafox('ejeproc(.f.)',w_vfox)
	endif
endif
return
&& R.06e Mariano


