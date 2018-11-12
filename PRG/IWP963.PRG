*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : IWP962.PRG -> Exportar Percep./Retenc. AFIP Ing. Brutos
*									 COMPRAS CONVENIO MULTILATERAL ( SIFERE )			
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
w_title = upper('SIFERE - Exportar Per/Ret I.B. Convenio Multilateral')
w_alta =.f.
w_alias='f05'

*) Apertura de archivos--------------------------------------------------------

fieldvec[1]='F00'
fieldvec[2]='F01'
fieldvec[3]='F03'
fieldvec[4]='F04'
fieldvec[5]='F05'

*fieldvec[5]='F06'
*fieldvec[6]='F24'

use (d0f00) in 0 alias 'f00'								&&(Parametros)
use (d0f01) in 0 alias 'f01' order 'd1tip'  		&&(Tablas)
use (d0f02) in 0 alias 'f02' order 'd2clie'  		&&(Clientes)
use (d0f03) in 0 alias 'f03' order 'd3prov'  		&&(Proveedores)
use (d0f04) in 0 alias 'f04' order 'd4peri'			&&(Periodos)
use (d0f05) in 0 alias 'f05' order 'd5peri'			&&(Comprobantes)
*use (d0f06) in 0 alias 'f06' order 'd6inte'			&&(Clientes Eventuales)
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

w_cpra	= .T.
w_pecp	= .T.
w_recp	= .T.

w_vta	= .F.
w_pevt	= .f.
w_revt	= .f.

w_drcp	= " "
w_dpcp	= " "

=creatipos()

DO dirs

*)-----------------------------------------------------------------------------

do ssm998.mpr
do iws962.spr


pop key
pop menu _msysmenu
=closall()

dele file &d0f99.a.dbf
dele file &d0f99.c.dbf
DELE FILE ivf099a.idx

return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC Cons963
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

if .not. valid963(.t.)
	wait wind 'Verifique los datos ingresados...'
	return
endif

if !w_pecp and !w_recp 
	wait window 'Debe elegir Percepciones y/o Retenciones'
	IF !w_pecp 
			_curobj = objnum(w_pecp)
	ELSE
		_curobj = objnum(w_recp)
	ENDIF
	w_retu = .F.		
	SHOW GETS
	return
endif

if w_pecp or w_recp
	=gentmp()

	if used('f99a')
		sele f99a
		use
		dele file &d0f99.a.dbf
	endif

	sele 0
	
	SELE f03
	SET RELA TO
	SET RELA TO "JU" + d3juri + SPACE(5)		INTO f01	ADDITIVE

	select *;		
		from f05;
		where d5peri=wdperi ;
		and d5tipo='C'		;
		and !empty(d5cpib)  ;
		into dbf &d0f99.a

	use &d0f99.a alias f99a exclusive

	set relation to d5clie into f03	ADDITIVE
 	go top
	if eof()
		wait window 'Sin movimientos para informar' time 1
		return
	endif
endif

if w_pecp							&& PERCEPCIONES	
	if used('f99c')
		sele f99c
		use
		dele file &d0f99.c.dbf
	endif

	sele 0
	
*	create dbf &d0f99.c (signo C(1), cuit c(13), nagperc n(5), dagperc n(1), fecha c(10), ;
*		tipcomprob c(1), letcomprob c(1), sucursal c(4), ;
*		numcomprob c(8), importe n(16,2), montobase n(16,2), proveedor c(5) )

	create dbf &d0f99.c (signo C(1), provincia C(20), juri C(3), cuit c(13), fecha c(10), sucursal c(4), ;
						 numcomprob c(8), tipcomprob c(1),  letcomprob c(1), compori C (20), proveedor c(5), importe n(11,2), importe1 C(11) )

	use &d0f99.c alias f99c

	SCATTER MEMVAR
	
	go top in f99a

*===> OJO !!!!!!!!!!  EL SIFERE ES CON "," NO "."
SET POINT TO ","			

	do while !eof('f99a')
		if f99a.d5tipo='C' AND f99a.d5regi='P'

			DO jurisdiccion
			
			DO codcomprob
			
			DO letracomp

			m.cuit=f03.d3cuit 
			m.fecha=dtoc(f99a.d5fech)
			m.sucursal=left(f99a.d5nume,4)
			m.numcomprob=right(f99a.d5nume,8)

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
*===>
*			m.importe1 = iif(f99a.d5tico='R',STR(abs(f99a.d5tota),11,2),STR(abs(eval(w_campo)),11,2))
			m.importe1 = iif(f99a.d5tico='R',STR(f99a.d5tota,11,2),STR(eval(w_campo),11,2))
			m.importe = iif(f99a.d5tico='R',f99a.d5tota,eval(w_campo))

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
		wait window 'Sin movimientos para informar' time 1
	else
		w_retu=creadir(curdir()+'AFIP')
		if w_retu=0 or w_retu=6
*===>
			INDEX on juri + signo + fecha + sucursal + numcomprob		TO ivf099a.idx

			do repdest &&(Setea variables internas del reporte)

			go top
			if !empty(p_repclaus) &&(debe ser preview o to printer)
				p_report='iwr963'

				w_var = " "
				w_var1	= "PERCEPCIONES"
				
				=rep_form()

				p_report=''
			endif

			do represet &&(Resetea estado)
			
			COPY TO &w_dpcp FIELDS EXCEPT signo, provincia, proveedor, importe, compori TYPE SDF

		endif
	endif

*===> OJO !!!!!!!!!!  EL SIFERE ES CON "," NO "."
	SET POINT TO

endif

if w_recp							&& RETENCIONES
	if used('f99c')
		sele f99c
		use
		dele file &d0f99.c.dbf
	endif

	sele 0

*	create dbf &d0f99.c (signo C(1), cuit c(13), nagperc n(5), dagperc n(1), fecha c(10), ;
*		numcomprob c(8), importe n(16,2), montobase n(16,2), proveedor c(5) )

	create dbf &d0f99.c (signo C(1), provincia C(20), juri C(3), cuit c(13), fecha c(10), sucursal c(4), ;
						 numcomprob c(16), tipcomprob c(1),  letcomprob c(1), compori C (20), proveedor c(5), importe n(11,2), importe1 C(11) )
	use &d0f99.c alias f99c
	SCATTER MEMVAR
	
	go top in f99a

*===> OJO !!!!!!!!!!  EL SIFERE ES CON "," NO "."
SET POINT TO ","			

	do while !eof('f99a')
		if f99a.d5tipo='C' and (f99a.d5tico='R' or f99a.d5regi='R')

			DO jurisdiccion
			
			DO codcomprob
			
			DO letracomp

			m.cuit			= f03.d3cuit 
			m.fecha			= dtoc(f99a.d5fech)
			m.sucursal		= left(f99a.d5nume,4)

			m.numcomprob	= m.sucursal + RIGHT(m.fecha,4) + RIGHT(f99a.d5nume,8)


*===> ??????????????????????????   COMPROBANTE ORIGINAL
			m.compori		= m.sucursal + m.numcomprob
			
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
*===>
*			m.importe1 = iif(f99a.d5tico='R',STR(abs(f99a.d5tota),11,2),STR(abs(eval(w_campo)),11,2))
			m.importe1 = iif(f99a.d5tico='R',STR(f99a.d5tota,11,2),STR(eval(w_campo),11,2))
			m.importe  = iif(f99a.d5tico='R',f99a.d5tota,eval(w_campo))

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
		wait window 'Sin movimientos para informar' time 1
	else

		w_retu=creadir(curdir()+'AFIP')
		if w_retu=0 or w_retu=6

			INDEX on juri + signo + fecha + sucursal + numcomprob		TO ivf099a.idx

			do repdest &&(Setea variables internas del reporte)

			go top
			if !empty(p_repclaus) &&(debe ser preview o to printer)

				p_report='iwr963'

				w_var = "Comp. Original"
				w_var1	= "RETENCIONES"
				
				=rep_form()
				p_report=''
			endif

			do represet &&(Resetea estado)

			COPY TO &w_drcp FIELDS EXCEPT signo, provincia, proveedor, importe TYPE SDF

		endif
	endif

*===> OJO !!!!!!!!!!  EL SIFERE ES CON "," NO "."
	SET POINT TO

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
	IF trdate('w_fech')
			wdperi	= RIGHT(DTOC(w_fech),7)
			SHOW GET wdperi
	ELSE
		IF !w_show
				WAIT WIND "El perํodo no es correcto"
		ENDIF
		w_retu	= .F.
		_curobj	= objnum(wdperi)
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
			endif
	ENDIF
	DO dirs
endif

if w_vari='W_PECP'
	=mchkbox(@w_pecp)
endif

if w_vari='W_RECP'
	=mchkbox(@w_recp)
endif


if w_vari='W_PECP' or w_show
			DO dirs
endif
		
if w_vari='W_RECP'
	=mchkbox(@w_recp)
endif

if w_vari='W_RECP' or w_show
			DO dirs
			if (!w_pecp and !w_recp)
						wait window 'Debe elegir Percepciones y/o Retenciones de IB'
						IF !w_pecp 
								_curobj = objnum(w_pecp)
						ELSE
							_curobj = objnum(w_recp)
						ENDIF
						w_retu = .F.		
						SHOW GETS
			endif
endif

*if w_vari='W_PEVT'
*	=mchkbox(@w_pevt)
*endif

*if w_vari='W_REVT'
*	=mchkbox(@w_revt)
*endif

*if w_vari='W_PEVT' or w_vari='W_REVT' or w_show
*	if w_pevt or w_revt
*		w_dprv=curdir()+'AFIP\SIFERE\AG'+ f77.d77empr + right(ptoi(wdperi),4)+'.TXT'
*	else
*		w_dprv=''
*	endif
*	show get w_dprv
*endif

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

*	sum importe, montobase to m.importe, m.montobase ;
	sum importe to m.importe ;
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

*	sum importe, montobase to m.importe, m.montobase ;
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
proc agrupavta	 && Agrupa comprobantes de ventas iguales
************************************************************

private w_cant, w_recno

index on a๑o+mes+dia+regimen+codigo+isib+cuit tag agrupa
go top
do while !eof('f99c') 
	scatter memvar
	w_recno=recno('f99c')

	** acumulo el comprobante

	sum importe to m.importe ;
		while m.a๑o+m.mes+m.dia+m.regimen+m.codigo+m.isib+m.cuit=a๑o+mes+dia+regimen+codigo+isib+cuit
			
	go w_recno in f99c
	gather memvar
	
	skip in f99c

	do 	while m.a๑o+m.mes+m.dia+m.regimen+m.codigo+m.isib+m.cuit=a๑o+mes+dia+regimen+codigo+isib+cuit and !eof('f99c')
		delete		
		skip in f99c
	enddo

enddo

return

************************************************************
*function tipcomprob
************************************************************
*parameter w_ttab
private w_tafi
do case
	case w_ttab$'01-05-06-10-11-14-16-19-22-30-34-35-36-39-40-41-51'+ ;
			'60-61-62-63-64-65-80-81-82-83-84-87'
		w_tafi='F'
	case w_ttab$'04-09-15-70'
		w_tafi='R'
	case w_ttab$'02-07-12-20-37-86-94'
		w_tafi='D'
	case w_ttab$'03-08-13-21-38-85-95'
		w_tafi='C'
	case w_ttab$'92-93-50-51-52-53-54-55-56-57-58-59'
		w_tafi=' '
	otherwise
		w_tafi='F'						
endcase
return w_tafi

************************************************************
function letcomprob
************************************************************
parameter w_ltab
private w_lafi
do case
	case w_ltab$'A'
		w_lafi='A'
	case w_ltab$'B'
		w_lafi='B'
	case w_ltab$'C'
		w_lafi='C'
	otherwise
		w_lafi=' '						
endcase
return w_lafi

*===============================================================================
PROCEDURE dirs

IF w_pecp
		w_dpcp=curdir()+'AFIP\SIFERE\PC'+ f77.d77empr + right(ptoi(wdperi),4)+'.TXT'
ELSE
	w_dpcp=''
ENDIF

IF w_recp
		w_drcp=curdir()+'AFIP\SIFERE\RC'+ f77.d77empr + right(ptoi(wdperi),4)+'.TXT'
ELSE
	w_drcp=''
ENDIF

SHOW GET w_drcp
SHOW GET w_dpcp

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

IF f99a.d5tico = "A"							&& Factura
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

m.letcomprob = letcomprob(f99a.d5tifo)

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
report form iwr963
