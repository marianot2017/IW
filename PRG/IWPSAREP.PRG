*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : IWPSAREP.PRG -> Exportar Percep./Retenc. AFIP Ing. Brutos
*										VENTAS ( IB )	- MENDOZA		
*	 RELEASE		 : 03
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
w_title=upper('SIAP - SAREPE - Exportar Per/Ret I.B. MENDOZA')
w_alta =.f.
w_alias='f05'

*) Apertura de archivos--------------------------------------------------------

fieldvec[1]='F00'
fieldvec[2]='F01'
fieldvec[3]='F02'
fieldvec[4]='F03'
fieldvec[5]='F04'
fieldvec[6]='F05'
fieldvec[7]='F06'
fieldvec[8]='F25'

use (d0f00) in 0 alias 'f00'						&&(Parametros empresa)
use (d0f01) in 0 alias 'f01'						&&(Tablas)
use (d0f02) in 0 alias 'f02' order 'd2clie'  		&&(Clientes)
use (d0f03) in 0 alias 'f03' order 'd3prov'  		&&(Proveedores)
use (d0f04) in 0 alias 'f04' order 'd4peri'			&&(Periodos)
use (d0f05) in 0 alias 'f05' order 'd5peri'			&&(Comprobantes)
use (d0f06) in 0 alias 'f06' order 'd6inte'			&&(Clientes Eventuales)	
use (d0f25) in 0 alias 'f25' order 'd25afip' 		&&(Percep./Retenc./Otros)

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


*===> SAREPE ws solo VENTAS

w_texto	= "VENTAS"

&& R.02b Mariano
IF w_pref	= ''
	w_texto1	= 'Perํodo : '
ELSE
	w_texto1	= 'Bimestre : '
ENDIF
&& R.02e Mariano

w_vta	= .T.
w_perc	= .T.
w_rete	= .T.

w_dire	= " "
w_dipe	= " "

w_repclaus = " "

DO dirs

*===>									MENDOZA
SELE f01
LOCATE FOR d1tip = "JU"		AND d1tdgi = "07"

w_prov	= ALLT(d1cla)
w_cond	= " "
IF !EMPTY(d1cla)
			do ssm998.mpr
			do iwscpa2.spr
ELSE
	=advgrave("NO ESTA DEFINIDA LA JURISDICCION CORRESPONDIENTE A MENDOZA")
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
							AND	 d5tipo = 'V' AND !EMPTY(d5regi)			;
							AND d5juri <> w_prov							;
						INTO CURSOR f99b
			IF RECCOUNT() <> 0
							=advgrave("EXISTEN VENTAS FUERA DE LA JURISDICCION DE MENDOZA")
							w_cond = " d5juri <> w_prov "
							DO genera
							p_report='iwrsarep'
							w_tit1	= "IB - PER. / RET. INGRESOS BRUTOS FUERA DE MENDOZA"
							INDEX ON signo + fecha + numcomprob		TO f99b.idx
							DO listalib
							USE
							DELE FILE f99b.idx
			ENDIF									
	ENDIF

	SELE 0
	
	SELECT *													;		
		FROM f05												;
				WHERE d5peri = wdperi							;
					AND	 d5tipo = 'V' AND !EMPTY(d5regi)		;
					AND d5juri = w_prov							;
				INTO CURSOR f99b

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
		p_report='iwrsarep'
		w_tit1	= "SIAP - SAREPE - EXPORTACION PERCEPCIONES INGRESOS BRUTOS - MENDOZA"
		INDEX ON signo + fecha + sucursal + numcomprob		TO f99c.idx
		DO listalib
		w_retu = creadir(curdir()+'\AFIP\INGBRU\MENDOZA')
		if w_retu = 0 or w_retu = 6
			COPY TO &w_dipe FIELDS EXCEPT signo, tipcomprob, letcomprob, sucursal, proveedor 		TYPE SDF
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
		p_report='iwrsarep'
		w_tit1	= "SIAP - SAREPE - EXPORTACION RETENCIONES INGRESOS BRUTOS - MENDOZA"

		INDEX ON signo + fecha + numcomprob		TO f99c.idx
		DO listalib
		w_retu = creadir(curdir()+'\AFIP\INGBRU\MENDOZA')
		if w_retu = 0 or w_retu = 6
				COPY TO &w_dire FIELDS EXCEPT signo, tipcomprob, letcomprob, sucursal, proveedor ;
						TYPE SDF 
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
proc agrucprape	 && Agrupa comprobantes de perc. de ventas iguales
************************************************************

private w_cant, w_recno

index on fecha+tipcomprob+letcomprob+sucursal+proveedor+numcomprob tag agrupa
go top
do while !eof('f99c') 
	scatter memvar
	w_recno=recno('f99c')

	** acumulo el comprobante

	sum importe	to m.importe	;
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
proc agrucprare	 && Agrupa comprobantes de ret. de ventas iguales
************************************************************

private w_cant, w_recno

index on fecha+proveedor+numcomprob tag agrupa
go top
do while !eof('f99c') 
	scatter memvar
	w_recno=recno('f99c')

	** acumulo el comprobante

	sum importe			to m.importe ;
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
			'111-118'	&& R.01 Mariano (agregue 0 y >99)
		w_tafi='F'

	case w_ttab$'004-009-015-070-054'	&& R.01 Mariano (agregue 0)
		w_tafi='R'

	case w_ttab$'002-007-012-020-037-086-094-052-089-115-116-117-120'	&& R.01 Mariano (agregue 0 y >99)
		w_tafi='D'

	case w_ttab$'003-008-013-021-038-085-095-090-053-110-112-113-114-119'	&& R.01 Mariano (agregue 0 y >99)
		w_tafi='C'

	otherwise
		w_tafi=' '						
endcase
return w_tafi

*===============================================================================
PROCEDURE dirs

IF w_perc
		w_dipe=curdir()+'AFIP\INGBRU\MENDOZA\PV'+ f77.d77empr + right(ptoi(wdperi),4)+'.TXT'
ELSE
	w_dipe=''
ENDIF

IF w_rete
		w_dire=curdir()+'AFIP\INGBRU\MENDOZA\RV'+ f77.d77empr + right(ptoi(wdperi),4)+'.TXT'
ELSE
	w_dire=''
ENDIF

SHOW GET w_dire
SHOW GET w_dipe

RETURN

*================================================================================
PROCEDURE genera

SELE 0

IF 	w_cond = " f99b.d5regi='P' "	OR		w_cond = " d5juri <> w_prov "
				CREATE CURSOR f99c (signo C(1), cuit C(13), nombre C(80), fecha C(8), ;
									numcomprob C(12), fecret C(8), montobase N(15,2), alic n(5,2), importe N(15,2),	;
									tipcomprob C(1), letcomprob C(1), sucursal C(5), proveedor c(5) )	&& R.03 Mariano (5 x 4)
ENDIF

IF 	w_cond = " f99b.d5regi='R' "
				CREATE CURSOR f99c (signo C(1), cuit C(13), nombre C(80), fecha C(8), ;
									numcomprob C(12), fecret C(8), montobase N(15,2), alic n(5,2), importe N(15,2),	;
									tipcomprob C(1), letcomprob C(1), sucursal C(5), proveedor c(5) )	&& R.03 Mariano (5 x 4)
ENDIF


w_campo=''
for i=1 to 3
	w_cprv='f00.p0prv'+(str(i,1))
	if &w_cprv=1   && IB
		w_campo=w_cprv
		exit
	endif						
endfor					
w_campo=iif(!empty(w_campo),str(val(right(w_campo,1))+5,1),'0')
w_campo=iif(w_campo>'0','f99b.d5imp'+w_campo,'0')

SELE f99b
go top in f99b
do while !eof('f99b')
		if &w_cond    

			SELE f99b

			w_cuit		= SPACE(13)
			m.nombre	= RETNOMC(f99b.d5clie,f99b.d5inte)
			m.cuit		= w_cuit

			=SEEK("07" + f99b.d5regi + f99b.d5cpib, 'f25')

			m.alic		= f25.d25porc
			
			m.fecret	= strtran(DTOC(f99b.d5fech),'/','')

			m.fecha		= strtran(dtoc(f99b.d5fech),'/','')
*			m.numcomprob= f99b.d5sucu+left(f99b.d5nume,8)	&& R.03 Mariano
			m.numcomprob= right(f99b.d5sucu,4)+left(f99b.d5nume,8)	&& R.03 Mariano

			IF f99b.d5tico = "R"
				m.tipcomprob	= f99b.d5tico
				m.montobase		= iif(empty(f99b.d5imre),DIV( 100 * f99b.d5tota, f25.d25porc, 2), f99b.d5imre) 
				m.importe		= f99b.d5tota
			ELSE
				m.tipcomprob	= tipcomprob(f99b.d5coco)
				m.montobase		= iif(empty(f99b.d5imp1),DIV( 100 * &w_campo, f25.d25porc, 2), f99b.d5imp1) 
				m.importe		= &w_campo
			ENDIF

			IF m.importe < 0
				m.signo = "1"
			ELSE
				m.signo = "0"
			ENDIF										

			m.letcomprob	=	f99b.d5tifo
			IF !m.letcomprob$"ABCM"
				m.letcomprob = " "
			ENDIF						

			m.sucursal	= f99b.d5sucu
			
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
report form iwrsarep
