*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : IWP962.PRG -> Exportar Percep./Retenc. AFIP Ing. Brutos
*										COMPRAS ( SICOL )			
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

w_pfile='962'
w_prog =upper(prompt())
w_title=upper('SICOL - Exportar Per/Ret I.B. Capital')
w_alta =.f.
w_alias='f05'

*) Apertura de archivos--------------------------------------------------------

fieldvec[1]='F00'
fieldvec[2]='F01'
fieldvec[3]='F02'
fieldvec[4]='F03'
fieldvec[5]='F04'
fieldvec[6]='F05'

use (d0f00) in 0 alias 'f00'						&&(Parametros empresa)
use (d0f01) in 0 alias 'f01'						&&(Tablas)
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


*===> SICOL es solo COMPRAS

w_cpra	= .T.
w_pecp	= .T.
w_recp	= .T.

w_vta	= .F.
w_pevt	= .f.
w_revt	= .f.

w_drcp	= " "
w_dpcp	= " "

DO dirs

*===>									CAPITAL
SELE f01
LOCATE FOR d1tip = "JU"		AND d1tdgi = "00"

w_prov	= ALLT(d1cla)
w_cond	= " "
IF !EMPTY(d1cla)
			do ssm998.mpr
			do iws962.spr
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

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC Cons962
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
if .not. valid962(.t.)
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

	if used('f99b')
		sele f99b
		use
		dele file &d0f99.b.dbf
	endif

	IF d0comu	= "N"
			SELE 0
			SELECT *;		
					FROM f05							;
						WHERE d5peri = wdperi			;
								AND d5tipo = 'C'		;
								AND !EMPTY(d5cpib)		;
								AND d5juri <> w_prov	;
						INTO CURSOR f99b
			IF RECCOUNT() <> 0
							=advgrave("EXISTEN COMPRAS FUERA DE LA JURISDICCION DE CAPITAL")
							DO genera
							p_report='iwr962p'
							w_tit1	= "SICOL - PERCEPCIONES Y RETENCIONES DE INGRESOS BRUTOS FUERA DE JURISDICCION"
							INDEX ON signo + fecha + numcomprob		TO ivf099b.idx
							DO listar
							USE
			ENDIF									
	ENDIF

	SELE 0
	
	SELECT *;		
		FROM f05								;
				WHERE d5peri = wdperi			;
						AND d5tipo = 'C'		;
						AND !EMPTY(d5cpib)		;
						AND d5juri = w_prov		;
				INTO CURSOR f99b

	set relation to d5clie into f03

 	go top
	if eof()
		wait window 'Sin movimientos para informar' time 1
		return
	endif
endif

if w_pecp	
	if used('f99c')
		sele f99c
		use
		dele file &d0f99.c.dbf
	endif
	
	w_cond = " AND f99b.d5regi='P' "
	DO genera
	
	do agrucprape
	go top
	
	if eof('f99c')
		wait window 'Sin movimientos para informar' time 1
	else
		p_report='iwr962p'
		w_tit1	= "SICOL - EXPORTACION DE PERCEPCIONES DE INGRESOS BRUTOS - Capital"
		INDEX ON signo + fecha + sucursal + numcomprob		TO ivf099b.idx
		DO listar

		w_retu = creadir(curdir()+'\AFIP\INGBRU\CAPITAL')
		if w_retu = 0 or w_retu = 6
			COPY TO &w_dpcp FIELDS EXCEPT signo, proveedor		FOR signo = "+" TYPE SDF
		endif
	endif
	USE
endif

if w_recp
	if used('f99c')
		sele f99c
		use
		dele file &d0f99.c.dbf
	endif

	w_cond = " AND (f99b.d5tico='R' OR f99b.d5regi='R') "
	DO genera

	do agrucprare
	go top

	if eof('f99c')
		wait window 'Sin movimientos para informar' time 1
	else
		p_report='iwr962r'
		w_tit1	= "SICOL - EXPORTACION DE RETENCIONES DE INGRESOS BRUTOS - Capital"
		DO listar
		INDEX ON signo + fecha + numcomprob		TO ivf099b.idx

		w_retu = creadir(curdir()+'\AFIP\INGBRU\CAPITAL')
		if w_retu = 0 or w_retu = 6
					COPY TO &w_drcp FIELDS EXCEPT signo, proveedor, tipcomprob, letcomprob, sucursal ;
						FOR signo = "+" TYPE SDF 
		endif
	endif
	USE
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
		w_dpcp=curdir()+'AFIP\INGBRU\CAPITAL\PC'+ f77.d77empr + right(ptoi(wdperi),4)+'.TXT'
ELSE
	w_dpcp=''
ENDIF

IF w_recp
		w_drcp=curdir()+'AFIP\INGBRU\CAPITAL\RC'+ f77.d77empr + right(ptoi(wdperi),4)+'.TXT'
ELSE
	w_drcp=''
ENDIF

SHOW GET w_drcp
SHOW GET w_dpcp

RETURN

*================================================================================
PROCEDURE genera

SELE 0
CREATE CURSOR f99c (signo C(1), cuit c(13), nagperc n(5), dagperc n(1), fecha c(10), ;
		tipcomprob c(1), letcomprob c(1), sucursal c(4), ;
		numcomprob c(8), importe n(16,2), montobase n(16,2), proveedor c(5) )
go top in f99b
do while !eof('f99b')
		if f99b.d5tipo='C' &w_cond    
			m.cuit=f03.d3cuit 
			m.nagperc=f03.d3nagp
			m.dagperc=f03.d3dagp
			m.fecha=dtoc(f99b.d5fech)
			m.tipcomprob=tipcomprob(f99b.d5coco)
			m.letcomprob=letcomprob(f99b.d5tifo)
			m.sucursal=left(f99b.d5nume,4)
			m.numcomprob=right(f99b.d5nume,8)
*===>
*			m.montobase=abs(f99b.d5imp1)+abs(f99b.d5imp2)
			m.montobase=f99b.d5imp1 + f99b.d5imp2

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
			m.importe=iif(f99b.d5tico='R',f99b.d5tota,eval(w_campo))

			IF m.importe >= 0
						m.signo = "+"
			ELSE
				m.signo = "-"
			ENDIF										
			
			m.proveedor=f99b.d5clie
			append blank
			gather memvar
		endif			
		skip in f99b
enddo
	

RETURN

*================================================================================
PROCEDURE listar

do repdest &&(Setea variables internas del reporte)

go top
if !empty(p_repclaus) &&(debe ser preview o to printer)
			=rep_form()
			p_report=''
endif

do represet &&(Resetea estado)

RETURN

*================================================================================


***puesto adrede para que incluya el report al proyecto**
report form iwr962p
report form iwr962r

