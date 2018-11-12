*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : IWPIVA.PRG -> Exportar Percep./Retenc. de IVA SIAP-IVA
*									SOLO COMPRAS SIAP - IVA v 5.0 
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
if empty(parameters())
	w_pref=''
	w_call=.f.
endif

*) Salvado de estados anteriores-----------------------------------------------

push key clear		  &&(Salva estados de teclas)
push menu _msysmenu &&(Salva estado de menu)
set skip of menu _msysmenu .t.

*) Variables internas----------------------------------------------------------

w_pfile='961i'
w_prog =upper(prompt())
w_title=upper('SIAP IVA - Exportar Per./Ret. de I.V.A.')
w_alta =.f.
w_alias='f05'

*) Apertura de archivos--------------------------------------------------------

fieldvec[1]='F00'
fieldvec[2]='F02'
fieldvec[3]='F03'
fieldvec[4]='F04'
fieldvec[5]='F05'
fieldvec[6]='F06'			&& R.01 Ra๚l
*fieldvec[6]='F24'

use (d0f00) in 0 alias 'f00'								&&(Parametros)
use (d0f02) in 0 alias 'f02' order 'd2clie'  		&&(Proveedores)
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

*===> SOLO COMPRAS SIAP - IVA v 5.0 

w_texto	= "COMPRAS"
w_texto1	= 'Perํodo : '		&& R.01 Mariano

w_vta	= .F.
w_cpra	= .T.

w_perc	= .T.
w_rete	= .T.

w_dipe	= " "
w_dire	= " "

DO dirs

*)-----------------------------------------------------------------------------

do ssm998.mpr
do iwscpa2.spr


pop key
pop menu _msysmenu
=closall()

dele file &d0f99.b.dbf
dele file &d0f99.c.dbf
DELE FILE ivf099b.idx
return

***vfp***
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons961i
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	on key label f9 do cons961i0
	keyboard '{f9}'
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons961i0
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
on key label f9
***vfp***

if .not. valid961i(.t.)
	wait wind 'Verifique los datos ingresados...'
	return
endif

if !w_perc and !w_rete
	wait window 'Debe elegir Percepciones y/o Retenciones de IVA'
	IF !w_perc 
			_curobj = objnum(w_perc)
	ELSE
		_curobj = objnum(w_rete)
	ENDIF
	w_retu = .F.		
	SHOW GETS
	return
endif

	if used('f99b')
		sele f99b
		use
	endif
	dele file f99b.dbf
	dele file f99b.idx

*if w_perc or w_rete
	=gentmp()

	sele 0

	select *													;
		from f05												;
		where	d5peri=wdperi									;
			AND	 d5tipo = 'C' AND !EMPTY(d5reiv)				;
		into dbf f99b

	use f99b alias f99b exclusive

	set relation to d5clie into f03
	set relation to d5clie into f02	ADDITIVE
	set relation to d5inte into f06 additive
*	set relation to d5pere into f24 additive

 	go top
	if eof()
		wait window 'No hay movimientos para exportar'
		return
	endif
*endif

w_print	= .F.

if w_perc							&& PERCEPCIONES
	sele 0
	create CURSOR f99c (signo C(1), sucu C(5), comprob C(13), reten C(3),cuit c(13),fecha c(10),factup1 C(8),factup2 C(8),monto n(16,2))	&& R.03 Mariano (5 x 4 y 13 x 12)

	go top in f99b
	do while !eof('f99b')
		if f99b.d5reiv='P' 

w_aux	= ""
FOR n = 1 TO LEN(f99b.d5nume)
			IF "0" <= SUBSTR(f99b.d5nume,n,1) AND SUBSTR(f99b.d5nume,n,1) <= "9"
						w_aux	= w_aux + SUBSTR(f99b.d5nume,n,1)
			ENDIF						
ENDFOR
			m.sucu		= LEFT(w_aux,5)	&& R.03 Mariano (5 x 4)
			m.comprob	= PADL(RIGHT(w_aux,8), 13, "0")	&& R.03 Mariano (13 x 12)
			
			m.reten		= f99b.d5pere

*			m.cuit=iif(f99b.d5tipo='V',f00.p0cui, iif(!empty(f06.d6cuit),f06.d6cuit,f03.d3cuit))
			SELE f99b
			w_cuit	= SPACE(13)
			w_x		= RETNOMC(f99b.d5clie,f99b.d5inte)
			m.cuit	= w_cuit

			m.fecha=dtoc(f99b.d5fech)
			
*			m.factup1=iif(f99b.d5tipo='V',f99b.d5sucu, left(f99b.d5nume,4))
			m.factup1	= left(f99b.d5nume,5)	&& R.03 Mariano (5 x 4)

*			m.factup2=iif(f99b.d5tipo='V',left(f99b.d5nume,8),right(f99b.d5nume,8))
*			m.factup2=right(f99b.d5nume,8)	&& R.02 Mariano
			m.factup2=right(left(f99b.d5nume,13),8)	&& R.02 Mariano

*===>
*			m.monto=abs(f99b.d5imp3)
			m.monto = f99b.d5imp3
			IF m.monto >= 0
						m.signo = "+"
			ELSE
				m.signo = "-"
			ENDIF										
			SELE f99c
			append blank
			gather memvar		
		endif
		skip in f99b
	enddo
	if eof('f99c')
		wait window 'PERCEPCIONES - SIn movimientos para exportar'
	else
		w_retu=creadir(curdir()+'AFIP\IVA')
		if w_retu=0 or w_retu=6
			INDEX ON signo + fecha + sucu + comprob + reten		TO f99c.idx
			p_report='iwrivap'
			DO listalib
			COPY TO &w_dipe  FIELDS EXCEPT signo, sucu, comprob FOR signo = "+" TYPE SDF
		endif
	endif
	USE
	dele file f99c.idx
endif

if w_rete							&& RETENCIONES
	sele 0
	create CURSOR f99c (signo C(1), sucu C(5), comprob C(13), reten C(3),cuit c(13),fecha c(10),certif c(16),monto n(16,2))	&& R.03 Mariano (5 x 4 y 13 x 12)
	go top in f99b
	do while !eof('f99b')
		if f99b.d5reiv='R'

w_aux	= ""
FOR n = 1 TO LEN(f99b.d5nume)
			IF "0" <= SUBSTR(f99b.d5nume,n,1) AND SUBSTR(f99b.d5nume,n,1) <= "9"
						w_aux	= w_aux + SUBSTR(f99b.d5nume,n,1)
			ENDIF						
ENDFOR
			m.sucu		= LEFT(w_aux,5)	&& R.03 Mariano (5 x 4)
			m.comprob	= PADL(RIGHT(w_aux,8), 13, "0")	&& R.03 Mariano (13 x 12)
			
			m.reten		= f99b.d5pere

*			m.cuit=iif(f99b.d5tipo='V',f00.p0cui, iif(!empty(f06.d6cuit),f06.d6cuit,f03.d3cuit))
			SELE f99b
			w_cuit	= SPACE(13)
			w_x		= RETNOMC(f99b.d5clie,f99b.d5inte)
			m.cuit	= w_cuit

			m.fecha=dtoc(f99b.d5fech)
			m.certif=PADL(w_aux, 16, "0")
*===>
*			m.monto=abs(iif(f99b.d5tico='R',f99b.d5tota,f99b.d5imp3))
			m.monto = iif(f99b.d5tico='R',f99b.d5tota,f99b.d5imp3)
			IF m.monto >= 0
						m.signo = "+"
			ELSE
				m.signo = "-"
			ENDIF										
			SELE f99c
			append blank
			gather memvar		
		endif
		skip in f99b
	enddo
	if eof('f99c')
		wait window 'RETENCIONES - No hay movimientos para informar'
	else
		w_retu=creadir(curdir()+'AFIP\IVA')
		if w_retu=0 or w_retu=6
			INDEX ON signo + fecha + sucu + comprob + reten		TO f99c.idx
			p_report='iwrivar'
			DO listalib
			COPY TO &w_dire  FIELDS EXCEPT signo, sucu, comprob FOR signo = "+" TYPE SDF
		endif
	endif
	USE
	dele file f99c.idx
endif

*if !empty(p_repclaus) &&(debe ser preview o to printer)

IF w_print
			do represet &&(Resetea estado)
endif


return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC when961i
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
w_retu=.t.
w_vari=varread()
p_curobj=_curobj

return(w_retu)

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC valid961i
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS w_show

if empty(parameters())
	w_show=.f.
endif

w_retu=.t.
w_vari=varread()
w_vread='m.'+w_vari

if w_vari='W_VTA'
	=mchkbox(@w_vta)
endif

if w_vari='W_CPRA'
	=mchkbox(@w_CPRA)
endif

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
	DO dirs
endif

if w_vari='W_PERC'
	=mchkbox(@w_perc)
endif

if w_vari='W_PERC' or w_show
				DO dirs
endif
		
if w_vari='W_RETE'
	=mchkbox(@w_rete)
endif

if w_vari='W_RETE' or w_show
				DO dirs
			if (!w_perc and !w_rete)
						wait window 'Debe elegir Percepciones y/o Retenciones de IVA'
						IF !w_perc 
								_curobj = objnum(w_perc)
						ELSE
							_curobj = objnum(w_rete)
						ENDIF
						w_retu = .F.		
						SHOW GETS
			endif
endif

return (w_retu)

*===============================================================================
PROCEDURE dirs

IF w_perc
		w_dipe=curdir()+'AFIP\IVA\PE' + f77.d77empr + SUBSTR(PTOI(wdperi),3,4) + '.TXT'
ELSE
	w_dipe=''
ENDIF
IF w_rete
		w_dire=curdir()+'AFIP\IVA\RE' + f77.d77empr + SUBSTR(PTOI(wdperi),3,4) + '.TXT'
ELSE
	w_dire=''
ENDIF

SHOW GET w_dire
SHOW GET w_dipe

RETURN

***puesto adrede para que incluya el report al proyecto**
report form iwrivap
report form iwrivar
