*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : IWPIFERE.PRG -> Exportar Percep./Retenc. AFIP Ing. Brutos
*									 COMPRAS CONVENIO MULTILATERAL ( SIFERE )			
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
if empty(parameters())
	w_pref=''
	w_call=.f.
endif

*) Salvado de estados anteriores-----------------------------------------------

push key clear		  &&(Salva estados de teclas)
push menu _msysmenu &&(Salva estado de menu)
set skip of menu _msysmenu .t.

*) Variables internas----------------------------------------------------------

w_pfile='963'
w_prog =upper(prompt())
w_title = upper('SIAp - SIFERE - Exportar Per/Ret I.B. Convenio Multilateral')
w_alta =.f.
w_alias='f05'

*) Apertura de archivos--------------------------------------------------------

fieldvec[1]='F00'
fieldvec[2]='F01'
fieldvec[3]='F03'
fieldvec[4]='F04'
fieldvec[5]='F05'
fieldvec[6]='F07'
fieldvec[7]='F06'			&& R.01 Ra๚l

*fieldvec[6]='F24'

use (d0f00) in 0 alias 'f00'								&&(Parametros)
use (d0f01) in 0 alias 'f01' order 'd1tip'  		&&(Tablas)
use (d0f02) in 0 alias 'f02' order 'd2clie'  		&&(Clientes)
use (d0f03) in 0 alias 'f03' order 'd3prov'  		&&(Proveedores)
use (d0f04) in 0 alias 'f04' order 'd4peri'			&&(Periodos)
use (d0f05) in 0 alias 'f05' order 'd5peri'			&&(Comprobantes)
use (d0f07) in 0 alias 'f07' order 'd7inte'			&&(Comprobantes Originales)
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
wdperi=f04.d4peri


*===> SIFERE es solo COMPRAS CONVENIO MULTILATERAL

w_texto	= "COMPRAS"
w_texto1	= 'Perํodo : '

w_cpra	= .T.
w_perc	= .T.
w_rete	= .T.

w_dire	= " "
w_dipe	= " "

=creatipos()

DO dirs

*)-----------------------------------------------------------------------------

do ssm998.mpr
do iwscpa2.spr


pop key
pop menu _msysmenu
=closall()

dele file &d0f99.a.dbf
dele file &d0f99.c.dbf
DELE FILE ivf099a.idx

return

***vfp***
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons963
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	on key label f9 do cons963x
	keyboard '{f9}'
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons963x
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
on key label f9
***vfp***

if .not. valid963(.t.)
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

*if w_perc or w_rete
	=gentmp()

	if used('f99a')
		sele f99a
		use
	endif
	dele file &d0f99.a.dbf

	sele 0

	select *											;		
		from f05										;
		where d5peri=wdperi								;
			AND	 d5tipo = 'C' AND !EMPTY(d5regi)		;
		into dbf &d0f99.a

	use &d0f99.a alias f99a exclusive

	SET RELA TO
	SET RELA TO d5clie 							INTO f03	ADDITIVE
	SET RELA TO d5clie 							INTO f02	ADDITIVE
	SET RELA TO "JU" + d5juri + SPACE(5)		INTO f01	ADDITIVE

 	go top

*endif

w_print	= .F.

if w_perc							&& PERCEPCIONES	
	if used('f99c')
		sele f99c
		use
	endif
	dele file &d0f99.c.dbf

	sele 0
	
	create dbf &d0f99.c (signo C(1), provincia C(20), juri C(3), cuit c(13), fecha c(10), sucursal c(4), ;
						 numcomprob c(8), tipcomprob c(1),  letcomprob c(1), compori C (20), proveedor c(5), importe n(11,2), importe1 C(11) )

	use &d0f99.c alias f99c

	SCATTER MEMVAR
	
	go top in f99a
	do while !eof('f99a')
		if f99a.d5regi='P'

			DO jurisdiccion
			
			DO codcomprob
			
			DO letracomp

*			m.cuit=f03.d3cuit 
			SELE f99a
			w_cuit	= SPACE(13)
			w_x		= RETNOMC(f99a.d5clie,f99a.d5inte)
			m.cuit	= w_cuit

			m.fecha=dtoc(f99a.d5fech)
*			m.sucursal=left(f99a.d5nume,4)	&& R.04 Mariano
			m.sucursal=right(left(f99a.d5nume,5),4)	&& R.04 Mariano
*			m.numcomprob=right(f99a.d5nume,8)	&& R.02 Mariano
			m.numcomprob=right(left(f99a.d5nume,13),8)	&& R.02 Mariano	&& R.04 Mariano
			m.numcomprob=right(left(f99a.d5nume,14),8)	&& R.04 Mariano

			w_campo=''
			for i=1 to 3
				w_cprc='f00.p0prc'+(str(i,1))
				if &w_cprc=1   && IB
					w_campo=w_cprc
					exit
				endif						
			endfor					
			w_campo=iif(!empty(w_campo),str(val(right(w_campo,1))+5,1),'0')
			w_campo=iif(w_campo>'0','f99a.d5imp'+w_campo,'0')

			m.importe = iif(f99a.d5tico='R',f99a.d5tota,eval(w_campo))
*===>
*			m.importe1 = iif(f99a.d5tico='R',STR(abs(f99a.d5tota),11,2),STR(abs(eval(w_campo)),11,2))
			m.importe1 = iif(f99a.d5tico='R',STR(f99a.d5tota,11,2),STR(eval(w_campo),11,2))
*===> OJO !!!!!!!!!!  EL SIFERE ES CON "," NO "."
			m.importe1 = STRTRAN(m.importe1, ".", ",")
			
			m.proveedor=f99a.d5clie

			IF m.importe >= 0
						m.signo = "+"
			ELSE
				m.signo = "-"
			ENDIF										
			
			SELE f99c
			append blank
			gather memvar
		endif			
		skip in f99a
	enddo

	do agrucprape
	go top
	
	if eof('f99c')
		wait window 'PERCEPCIONES - Sin movimientos para informar'
	else
		w_retu=creadir(curdir()+'AFIP')
		if w_retu=0 or w_retu=6
			INDEX on juri + signo + fecha + sucursal + numcomprob		TO ivf099a.idx
			GO TOP
			p_report='iwrifere'
			w_var = " "
			w_var1	= "PERCEPCIONES"
			DO listalib
			COPY TO &w_dipe FIELDS EXCEPT signo, provincia, proveedor, importe, compori TYPE SDF
		endif
	endif
	USE
	dele file &d0f99.c.dbf
	DELE FILE IVF099a.IDX
endif
	
if w_rete							&& RETENCIONES
	if used('f99c')
		sele f99c
		use
	endif
	dele file &d0f99.c.dbf

	sele 0

*	create dbf &d0f99.c (signo C(1), cuit c(13), nagperc n(5), dagperc n(1), fecha c(10), ;
*		numcomprob c(8), importe n(16,2), montobase n(16,2), proveedor c(5) )

	create dbf &d0f99.c (signo C(1), provincia C(20), juri C(3), cuit c(13), fecha c(10), sucursal c(4), ;
						 numcomprob c(16), tipcomprob c(1),  letcomprob c(1), compori C (20), proveedor c(5), importe n(11,2), importe1 C(11) )
	use &d0f99.c alias f99c
	SCATTER MEMVAR
	
	go top in f99a
	do while !eof('f99a')
		if f99a.d5regi = "R"

			DO jurisdiccion
			
			DO codcomprob
			
			DO letracomp

*			m.cuit			= f02.d2cuit 
			SELE f99a
			w_cuit	= SPACE(13)
			w_x		= RETNOMC(f99a.d5clie,f99a.d5inte)
			m.cuit	= w_cuit

			m.fecha			= dtoc(f99a.d5fech)
*			m.sucursal		= left(f99a.d5nume,4)	&& R.04 Mariano
			m.sucursal		= right(left(f99a.d5nume,5),4)	&& R.04 Mariano

*			m.numcomprob	= m.sucursal + RIGHT(m.fecha,4) + RIGHT(f99a.d5nume,8)	&& R.03 Mariano
			m.numcomprob	= m.sucursal + RIGHT(m.fecha,4) + RIGHT(rtrim(f99a.d5nume),8)	&& R.03 Mariano

*===> COMPROBANTE ORIGINAL
			IF !seek(f99a.d5inte,'f07')		OR										;
				(seek(f99a.d5inte,'f07')	AND EMPTY(f07.d7numori) )
								m.compori = SPACE(20)
			ELSE
				SELE f07
				SCATTER MEMVAR
				w_aux	= ""
				FOR n = 1 TO LEN(m.d7numori)
						IF SUBSTR(m.d7numori,n,1) $ "0123456789"
										w_aux	= w_aux + SUBSTR(m.d7numori,n,1)
						ENDIF						
				ENDFOR
				w_aux = PADL(w_aux, 20, "0")
				m.compori = w_aux
			ENDIF

			SELE F99c			
			w_campo=''
			for i=1 to 3
					w_cprc='f00.p0prc'+(str(i,1))
					if &w_cprc=1   && IB
								w_campo=w_cprc
								exit
					endif						
			endfor					
			w_campo=iif(!empty(w_campo),str(val(right(w_campo,1))+5,1),'0')
			w_campo=iif(w_campo>'0','f99a.d5imp'+w_campo,'0')

			m.importe  = iif(f99a.d5tico='R',f99a.d5tota,eval(w_campo))

*			m.importe1 = iif(f99a.d5tico='R',STR(abs(f99a.d5tota),11,2),STR(abs(eval(w_campo)),11,2))
			m.importe1 = iif(f99a.d5tico='R',STR(f99a.d5tota,11,2),STR(eval(w_campo),11,2))
*===> OJO !!!!!!!!!!  EL SIFERE ES CON "," NO "."
			m.importe1 = STRTRAN(m.importe1, ".", ",")
			
			m.proveedor=f99a.d5clie

			IF m.importe >= 0
						m.signo = "+"
			ELSE
				m.signo = "-"
			ENDIF										
		
			SELE f99c
			append blank
			gather memvar		
		endif			
		skip in f99a
	enddo
	do agrucprare
	go top
	if eof('f99c')
		wait window 'RETENCIONES - Sin movimientos para informar'
	else
		w_retu=creadir(curdir()+'AFIP')
		if w_retu=0 or w_retu=6
			INDEX on juri + signo + fecha + sucursal + numcomprob		TO ivf099a.idx
			p_report='iwrifere'
			w_var = "Comp. Original"
			w_var1	= "RETENCIONES"
			DO listalib
			COPY TO &w_dire FIELDS EXCEPT signo, provincia, proveedor, importe TYPE SDF
		ENDIF
	endif
	USE
	dele file &d0f99.c.dbf
	DELE FILE IVF099A.IDX
endif

SELE f99a
USE
dele file &d0f99.a.dbf

		
*if !empty(p_repclaus) &&(debe ser preview o to printer)
IF w_print
			do represet &&(Resetea estado)
endif

return


*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC when963
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
w_retu=.t.
w_vari=varread()
p_curobj=_curobj

return(w_retu)

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC valid963
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

index on provincia+fecha+tipcomprob+letcomprob+sucursal+proveedor+numcomprob tag agrupa
go top
do while !eof('f99c') 
	scatter memvar
	w_recno=recno('f99c')

	** acumulo el comprobante

*	sum importe, montobase to m.importe, m.montobase 
	sum importe to m.importe ;
			while m.provincia+m.fecha+m.tipcomprob+m.letcomprob+m.sucursal+m.proveedor+m.numcomprob ;
		=provincia+fecha+tipcomprob+letcomprob+sucursal+proveedor+numcomprob
			
	go w_recno in f99c
	gather memvar
	
	skip in f99c

	do while m.provincia+m.fecha+m.tipcomprob+m.letcomprob+m.sucursal+m.proveedor+m.numcomprob ;
	=provincia+fecha+tipcomprob+letcomprob+sucursal+proveedor+numcomprob and !eof('f99c')
		delete		
		skip in f99c
	enddo

enddo

return

************************************************************
proc agrucprare	 && Agrupa comprobantes de ret. de compras iguales
************************************************************

private w_cant, w_recno

index on provincia+fecha+proveedor+numcomprob tag agrupa
go top
do while !eof('f99c') 
	scatter memvar
	w_recno=recno('f99c')

	** acumulo el comprobante

*	sum importe, montobase to m.importe, m.montobase 
	sum importe to m.importe ;
			while m.provincia+m.fecha+m.proveedor+m.numcomprob=provincia+fecha+proveedor+numcomprob
			
	go w_recno in f99c
	gather memvar
	
	skip in f99c

	do while m.provincia+m.fecha+m.proveedor+m.numcomprob=provincia+fecha+proveedor+numcomprob and !eof('f99c')
		delete		
		skip in f99c
	enddo

enddo

return

*===============================================================================
PROCEDURE dirs

IF w_perc
		w_dipe=curdir()+'AFIP\SIFERE\PC'+ f77.d77empr + right(ptoi(wdperi),4)+'.TXT'
ELSE
	w_dipe=''
ENDIF

IF w_rete
		w_dire=curdir()+'AFIP\SIFERE\RC'+ f77.d77empr + right(ptoi(wdperi),4)+'.TXT'
ELSE
	w_dire=''
ENDIF

SHOW GETS

RETURN


*================================================================================
PROCEDURE jurisdiccion
			
m.juri		= " "
m.provincia = " "
SELE f01t
SEEK "JD" + f01.d1tdgi
IF FOUND()
		m.juri		= f01t.jurafip
		m.provincia	= f01t.desc
ENDIF									

RETURN

*================================================================================
PROCEDURE codcomprob
			
m.tipcomprob = "O"

IF f99a.d5tico = "A"	OR f99a.d5tico = "6"	&& Factura - TicketFactura
				m.tipcomprob = "F"
ENDIF
IF f99a.d5tico = "8"	OR f99a.d5tico = "@"	&& Recibo-Factura o Recibo
				m.tipcomprob = "R"
ENDIF
IF f99a.d5tico = "B"							&& Nota DB
				m.tipcomprob = "D"
ENDIF
IF f99a.d5tico = "C"							&& Nota CR
				m.tipcomprob = "C"
ENDIF

RETURN

*================================================================================
PROCEDURE letracomp

m.letcomprob = f99a.d5tifo

IF	m.letcomprob <> "A" AND m.letcomprob <> "B" AND 	;
	m.letcomprob <> "M" AND m.letcomprob <> "E" 
										m.letcomprob = " "
ENDIF							
IF f99a.d5tico = "8"	OR f99a.d5tico = "@"	&& Recibo-Factura o Recibo
				m.letcomprob = "C"
ENDIF							

RETURN

*================================================================================

***puesto adrede para que incluya el report al proyecto**
report form iwrifere
