*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : IWPSICOL.PRG -> Exportar Percep./Retenc. AFIP Ing. Brutos
*										COMPRAS ( SICOL )			
*	 RELEASE		 : 08
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
w_title=upper('SIAP - SICOL - Exportar Per/Ret I.B. Capital')
w_alta =.f.
w_alias='f05'

*) Apertura de archivos--------------------------------------------------------

fieldvec[1]='F00'
fieldvec[2]='F01'
fieldvec[3]='F02'
fieldvec[4]='F03'
fieldvec[5]='F04'
fieldvec[6]='F05'
fieldvec[6]='F06'						&& R.01 Ra๚l

use (d0f00) in 0 alias 'f00'						&&(Parametros empresa)
use (d0f01) in 0 alias 'f01'						&&(Tablas)
use (d0f02) in 0 alias 'f02' order 'd2clie'  		&&(Clientes)
use (d0f03) in 0 alias 'f03' order 'd3prov'  		&&(Proveedores)
use (d0f04) in 0 alias 'f04' order 'd4peri'			&&(Periodos)
use (d0f05) in 0 alias 'f05' order 'd5peri'			&&(Comprobantes)
use (d0f06) in 0 alias 'f06' order 'd6inte'			&&(Clientes Eventuales)			&& R.01 Ra๚l
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


*===> SICOL es solo COMPRAS

w_texto	= "COMPRAS"
w_texto1	= 'Perํodo : '

w_cpra	= .T.
w_perc	= .T.
w_rete	= .T.

w_dire	= " "
w_dipe	= " "

w_repclaus = " "

DO dirs

*===>									CAPITAL
SELE f01
LOCATE FOR d1tip = "JU"		AND d1tdgi = "00"

w_prov	= ALLT(d1cla)
w_cond	= " "
IF !EMPTY(d1cla)
			do ssm998.mpr
			do iwscpa2.spr
ELSE
	=advgrave("NO ESTA DEFINIDA LA JURISDICCION CORRESPONDIENTE A CAPITAL FEDERAL")
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

	IF d0comu	= "N"
			SELE 0
			SELECT *														;		
					FROM f05												;
						WHERE d5peri = wdperi								;
							AND	 d5tipo = 'C' AND !EMPTY(d5regi)			;
							AND d5juri <> w_prov							;
						INTO CURSOR f99b
			IF RECCOUNT() <> 0 and w_perc	&& R.03 Mariano ( agregue and w_perc)
							=advgrave("EXISTEN COMPRAS FUERA DE LA JURISDICCION DE CAPITAL")
							w_cond = " d5juri <> w_prov "
							DO genera
							p_report='iwricolp'
							w_tit1	= "SICOL - PER. / RET. INGRESOS BRUTOS FUERA DE CAPITAL"
							INDEX ON signo + fecha + numcomprob		TO f99b.idx
							DO listalib
							USE
							DELE FILE F99b.IDX
							&& R.06b Mariano
							IF w_print
								do represet &&(Resetea estado)
							ENDIF
							&& R.06e Mariano
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
		wait window 'PERCEPCIONES -Sin movimientos para exportar'
	else
		p_report='iwricolp'
		w_tit1	= "SIAP - SICOL - EXPORTACION PERCEPCIONES INGRESOS BRUTOS - Capital"
		INDEX ON signo + fecha + sucursal + numcomprob		TO f99c.idx
		DO listalib
		&& R.06b Mariano
		IF w_print
			do represet &&(Resetea estado)
		ENDIF
		&& R.06e Mariano
		w_retu = creadir(curdir()+'\AFIP\INGBRU\CAPITAL')
		if w_retu = 0 or w_retu = 6
*			COPY TO &w_dipe FIELDS EXCEPT signo, proveedor		FOR signo = "+" TYPE SDF	&& R.04 Mariano
			COPY TO &w_dipe FIELDS EXCEPT signo, proveedor		TYPE SDF	&& R.04 Mariano
		endif
	endif

&& R.02b Ra๚l
*	USE
*	dele file f99c.dbf
*	dele file f99c.idx
&& R.02e Ra๚l

endif

if w_rete

&& R.02b Ra๚l
	if used('f99c')
		sele f99c
		use
	endif
&& R.02e Ra๚l

	w_cond = " f99b.d5regi='R' "
	DO genera

	do agrucprare
	go top

	if eof('f99c')
		wait window 'RETENCIONES - Sin movimientos para informar'
	else
		p_report='iwricolr'
		w_tit1	= "SIAP - SICOL - EXPORTACION RETENCIONES INGRESOS BRUTOS - Capital"

		INDEX ON signo + fecha + numcomprob		TO f99c.idx
		DO listalib
		&& R.06b Mariano
		IF w_print
			do represet &&(Resetea estado)
		ENDIF
		&& R.06e Mariano
		w_retu = creadir(curdir()+'\AFIP\INGBRU\CAPITAL')
		if w_retu = 0 or w_retu = 6
*					COPY TO &w_dire FIELDS EXCEPT signo, proveedor, tipcomprob, letcomprob, sucursal ;
						FOR signo = "+" TYPE SDF 	&& R.04 Mariano
			COPY TO &w_dire FIELDS EXCEPT signo, proveedor, tipcomprob, letcomprob, sucursal ;
						TYPE SDF 	&& R.04 Mariano
		endif
	endif

&& R.02b Ra๚l
*	USE
*	dele file f99c.dbf
*	dele file f99c.idx
&& R.02e Ra๚l

endif

*if !empty(p_repclaus) &&(debe ser preview o to printer)
IF w_print
			do represet &&(Resetea estado)
endif

&& R.02b Ra๚l
if used('f99c')
	sele f99c
	use
	dele file f99c.dbf
	dele file f99c.idx
endif
&& R.02e Ra๚l

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

index on fecha+tipcomprob+letcomprob+sucursal+proveedor+numcomprob tag agrupa
go top
do while !eof('f99c') 
	scatter memvar
	w_recno=recno('f99c')

	** acumulo el comprobante

	sum importe, montobase to m.importe, m.montobase ;
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
	case w_ttab$'001-005-006-010-011-014-016-019-022-030-034-035-036-039-040-041-051-092-093-087' 				+;
			'060-061-062-063-064-065-080-081-082-083-084-087-088-096-097-055-056-057-058-059-099' +;
			'111-118'	&& R.05 Mariano (agregue 0 y >99)
		w_tafi='F'

	case w_ttab$'004-009-015-070-054'	&& R.05 Mariano (agregue 0)
		w_tafi='R'

	case w_ttab$'002-007-012-020-037-086-094-052-089-115-116-117-120'	&& R.05 Mariano (agregue 0 y >99)
		w_tafi='D'

*	case w_ttab$'03-08-13-21-38-42-85-95-90-53'	&& R.04 Mariano (agregue 42-) 	&& R.05 Mariano
	case w_ttab$'003-008-013-021-038-042-085-095-090-053-110-112-113-114-119'	&& R.05 Mariano (agregue 0 y >99)
		w_tafi='C'

	otherwise
		w_tafi=' '						
endcase
return w_tafi

*===============================================================================
PROCEDURE dirs

IF w_perc
		w_dipe=curdir()+'AFIP\INGBRU\CAPITAL\PC'+ f77.d77empr + right(ptoi(wdperi),4)+'.TXT'
ELSE
	w_dipe=''
ENDIF

IF w_rete
		w_dire=curdir()+'AFIP\INGBRU\CAPITAL\RC'+ f77.d77empr + right(ptoi(wdperi),4)+'.TXT'
ELSE
	w_dire=''
ENDIF

SHOW GET w_dire
SHOW GET w_dipe

RETURN

*================================================================================
PROCEDURE genera

SELE 0
CREATE CURSOR f99c (signo C(1), cuit c(13), nagperc n(5), dagperc n(1), fecha c(10), ;
		tipcomprob c(1), letcomprob c(1), sucursal c(4), ;
		numcomprob c(8), importe n(16,2), montobase n(16,2), proveedor c(5) )

SELE f99b
go top in f99b
do while !eof('f99b')
		if &w_cond    

*			m.cuit=f03.d3cuit 
			SELE f99b
			w_cuit	= SPACE(13)
			w_x		= RETNOMC(f99b.d5clie,f99b.d5inte)
			m.cuit	= w_cuit

*			m.nagperc=f03.d3nagp	&& R.03 Mariano
			m.nagperc=left(allt(f03.d3nagp),5)	&& R.03 Mariano
			m.dagperc=f03.d3dagp

			m.fecha=dtoc(f99b.d5fech)
			m.tipcomprob=tipcomprob(f99b.d5coco)

			m.letcomprob	=	f99b.d5tifo
			IF !m.letcomprob$"ABCM"	&& R.04 Mariano (agregue M)
						m.letcomprob = " "
			ENDIF						

*			m.sucursal=left(f99b.d5nume,4)	&& R.08 Mariano
			m.sucursal=right(left(f99b.d5nume,5),4)	&& R.08 Mariano
*			m.numcomprob=right(f99b.d5nume,8)	&& R.07 Mariano
*			m.numcomprob=right(left(f99b.d5nume,13),8)	&& R.07 Mariano	&& R.08 Mariano
			m.numcomprob=right(left(f99b.d5nume,14),8)	&& R.08 Mariano
*===>
*			m.montobase=abs(f99b.d5imp1)+abs(f99b.d5imp2)
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
report form iwricolp
report form iwricolr
