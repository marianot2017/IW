*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : IWPADIB.PRG -> Exportar Percep./Retenc. AFIP Ing. Brutos
*										COMPRAS ( IB )	- MENDOZA		
*	 RELEASE		 : 04
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
w_title=upper('SIAP - ADIB - Exportar Per/Ret I.B. MENDOZA')
w_alta =.f.
w_alias='f05'

*) Apertura de archivos--------------------------------------------------------

fieldvec[1]='F00'
fieldvec[2]='F01'
fieldvec[3]='F02'
fieldvec[4]='F03'
fieldvec[5]='F04'
fieldvec[6]='F05'
fieldvec[7]='F06'		&& R.01 Ra๚l

use (d0f00) in 0 alias 'f00'						&&(Parametros empresa)
use (d0f01) in 0 alias 'f01'						&&(Tablas)
use (d0f02) in 0 alias 'f02' order 'd2clie'  		&&(Clientes)
use (d0f03) in 0 alias 'f03' order 'd3prov'  		&&(Proveedores)
use (d0f04) in 0 alias 'f04' order 'd4peri'			&&(Periodos)
use (d0f05) in 0 alias 'f05' order 'd5peri'			&&(Comprobantes)
use (d0f06) in 0 alias 'f06' order 'd6inte'			&&(Clientes Eventuales)		&& R.01 Ra๚l
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


*===> ADIB es solo COMPRAS

w_texto	= "COMPRAS"

&& R.02b Mariano
IF w_pref	= ''	&& R.01 Mariano (='1')
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
							AND	 d5tipo = 'C' AND !EMPTY(d5regi)			;
							AND d5juri <> w_prov							;
						INTO CURSOR f99b
			IF RECCOUNT() <> 0
							=advgrave("EXISTEN VENTAS FUERA DE LA JURISDICCION DE MENDOZA")
							w_cond = " d5juri <> w_prov "
							DO genera
							p_report='iwradibp'
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
					AND	 d5tipo = 'C' AND !EMPTY(d5regi)		;
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
		p_report='iwradibp'
		w_tit1	= "SIAP - ADIB - EXPORTACION PERCEPCIONES INGRESOS BRUTOS - MENDOZA"
		INDEX ON signo + fecha + sucursal + numcomprob		TO f99c.idx
		DO listalib
		w_retu = creadir(curdir()+'\AFIP\INGBRU\MENDOZA')
		if w_retu = 0 or w_retu = 6
			REPLA ALL importe1 WITH PADL(ALLT(STR(importe, 15,2)), 15, '0')
			COPY TO &w_dipe FIELDS EXCEPT importe, tipcomprob, letcomprob, sucursal, proveedor, montobase 		TYPE SDF
*FOR signo = "+" 
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
		p_report='iwradibr'
		w_tit1	= "SIAP - ADIB - EXPORTACION RETENCIONES INGRESOS BRUTOS - MENDOZA"

		INDEX ON signo + fecha + numcomprob		TO f99c.idx
		DO listalib
		w_retu = creadir(curdir()+'\AFIP\INGBRU\MENDOZA')
		if w_retu = 0 or w_retu = 6
			REPLA ALL importe1 WITH PADL(ALLT(STR(importe, 15,2)), 15, '0')
			COPY TO &w_dire FIELDS EXCEPT signo, importe, tipcomprob, letcomprob,	sucursal, numcomprob, proveedor, montobase ;
						TYPE SDF 
*FOR signo = "+" 
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
proc agrucprape	 && Agrupa comprobantes de perc. de compras iguales
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
proc agrucprare	 && Agrupa comprobantes de ret. de compras iguales
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
	case w_ttab$'01-05-06-10-11-14-16-19-22-30-34-35-36-39-40-41-51-92-93-87' 				+;
			'60-61-62-63-64-65-80-81-82-83-84-87-88-96-97-55-56-57-58-59-99'
		w_tafi='F'

	case w_ttab$'04-09-15-70-54'
		w_tafi='R'

	case w_ttab$'02-07-12-20-37-86-94-52-89'
		w_tafi='D'

	case w_ttab$'03-08-13-21-38-85-95-90-53'
		w_tafi='C'

	otherwise
		w_tafi=' '						
endcase
return w_tafi

*===============================================================================
PROCEDURE dirs

IF w_perc
		w_dipe=curdir()+'AFIP\INGBRU\MENDOZA\PC'+ f77.d77empr + right(ptoi(wdperi),4)+'.TXT'
ELSE
	w_dipe=''
ENDIF

IF w_rete
		w_dire=curdir()+'AFIP\INGBRU\MENDOZA\RC'+ f77.d77empr + right(ptoi(wdperi),4)+'.TXT'
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
				CREATE CURSOR f99c (signo C(1), nroagte C(7), cuit C(13), nombre C(50), fecha C(10), ;
									numcomprob C(50), nroconst C(15), importe1 C(15), importe N(15,2),	;
									tipcomprob C(1), letcomprob C(1), sucursal C(4),	;
									proveedor c(5), montobase C(12) )
ENDIF

IF 	w_cond = " f99b.d5regi='R' "
				CREATE CURSOR f99c (signo C(1), nroagte C(7), cuit C(13), nombre C(50),	; 
									fecha C(10), alic C(15), nroconst C(10),			;
									importe1 C(15), importe N(15,2), tipcomprob C(1), letcomprob C(1),	;
									sucursal C(4), numcomprob C(50), proveedor C(5), montobase C(12)  )
ENDIF

SELE f99b
go top in f99b
do while !eof('f99b')
		if &w_cond    

			SELE f99b
			w_cuit		= SPACE(13)
			m.nombre	= RETNOMC(f99b.d5clie,f99b.d5inte)
			m.cuit		= w_cuit

			IF f99b.d5regi = 'R'
							m.nroagte	= PADL(ALLT(f02.d2nagr)+f02.d2dagr, 7, '0')
*							m.nroconst	= RIGHT(f99b.d5nume, 10)	&& R.03 Mariano
*							m.nroconst	= RIGHT(left(f99b.d5nume,13), 10)	&& R.03 Mariano	&& R.04 Mariano
							m.nroconst	= RIGHT(left(f99b.d5nume,14), 10)	&& R.04 Mariano
							m.alic		= PADL(ALLT(STR(f99b.d5por1,15,2)), 15, '0')
			ELSE
				m.nroagte	= PADL(ALLT(f03.d3nagp)+f03.d3dagp, 7, '0')
				w_x			= STRTRAN(f99b.d5nume, '-', '')
				m.nroconst	= PADL(w_x, 15, '0')
			ENDIF											

			m.fecha		= dtoc(f99b.d5fech)
			w_x			= STRTRAN(f99b.d5nume, '-', '')
			m.numcomprob= PADL(w_x, 50, '0')

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
			IF m.importe < 0
						m.signo = "1"
			ELSE
				m.signo = "0"
			ENDIF										
			m.importe = PADL(ALLT(STR(iif(f99b.d5tico='R',f99b.d5tota,eval(w_campo)), 15,2)), 15, '0')
			
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
report form iwradibp
report form iwradibr
