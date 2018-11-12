*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : IWP961.PRG -> Exportar Percep./Retenc. de IVA SIAP-IVA
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
fieldvec[2]='F03'
fieldvec[3]='F04'
fieldvec[4]='F05'
fieldvec[5]='F06'
*fieldvec[6]='F24'

use (d0f00) in 0 alias 'f00'								&&(Parametros)
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

*===> SOLO COMPRAS SIAP - IVA v 5.0 
w_vta	= .F.
w_cpra	= .T.

w_perc	= .T.
w_rete	= .T.

w_dipe	= " "
w_dire	= " "

DO dirs

*)-----------------------------------------------------------------------------

do ssm998.mpr
do iws961i.spr


pop key
pop menu _msysmenu
=closall()

dele file &d0f99.b.dbf
dele file &d0f99.c.dbf
DELE FILE ivf099b.idx
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC Cons961i
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
if .not. valid961i(.t.)
	wait wind 'Verifique los datos ingresados...'
	return
endif

if !w_vta and !w_cpra
	wait window 'Debe elegir Ventas y/o Compras'
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

if w_perc or w_rete
	=gentmp()

	if used('f99b')
		sele f99b
		use
		dele file &d0f99.b.dbf
	endif

	sele 0

	select *;
		from f05;
		where d5peri=wdperi and (iif(w_vta, d5tipo='V', .f.) or iif(w_cpra, d5tipo='C', .f.)) ;
		and (iif(w_cpra, d5tipo='C', .f.) or iif(w_vta, d5tipo='V', .f.)) ;
		and !empty(d5pere)  ;
		into dbf &d0f99.b

	use &d0f99.b alias f99b exclusive

	set relation to d5clie into f03
	set relation to d5inte into f06 additive
*	set relation to d5pere into f24 additive
 	go top
	if eof()
		wait window 'No hay movimientos para informar' time 1
		return
	endif
endif

if w_perc	
	if used('f99c')
		sele f99c
		use
		dele file &d0f99.c.dbf
	endif

	sele 0
	create dbf &d0f99.c (signo C(1), sucu C(4), comprob C(13), reten C(3),cuit c(13),fecha c(10),factup1 C(8),factup2 C(8),monto n(16,2))
	use &d0f99.c alias f99c
	go top in f99b
	do while !eof('f99b')
		if f99b.d5reiv='P' 
			m.sucu		= f99b.d5sucu
			m.comprob	= f99b.d5nume
			
			m.reten		= f99b.d5pere
			m.cuit=iif(f99b.d5tipo='V',f00.p0cui, iif(!empty(f06.d6cuit),f06.d6cuit,f03.d3cuit))
			m.fecha=dtoc(f99b.d5fech)
			m.factup1=iif(f99b.d5tipo='V',f99b.d5sucu, left(f99b.d5nume,4))
			m.factup2=iif(f99b.d5tipo='V',left(f99b.d5nume,8),right(f99b.d5nume,8))
*===>
*			m.monto=abs(f99b.d5imp3)
			m.monto = f99b.d5imp3
			IF m.monto >= 0
						m.signo = "+"
			ELSE
				m.signo = "-"
			ENDIF										
			append blank
			gather memvar		
		endif
		skip in f99b
	enddo
	if eof('f99c')
		wait window 'No hay movimientos para informar' time 1
	else
		w_retu=creadir(curdir()+'AFIP\IVA')
		if w_retu=0 or w_retu=6

			INDEX ON signo + fecha + sucu + comprob + reten		TO ivf099b.idx

			COPY TO &w_dipe  FIELDS EXCEPT signo, sucu, comprob FOR signo = "+" TYPE SDF

			do repdest &&(Setea variables internas del reporte)

			go top
			if !empty(p_repclaus) &&(debe ser preview o to printer)
				p_report='iwr961ip'
				=rep_form()
				p_report=''
			endif

			do represet &&(Resetea estado)
		endif
	endif
endif

if w_rete
	if used('f99c')
		sele f99c
		use
		dele file &d0f99.c.dbf
	endif

	sele 0
	create dbf &d0f99.c (signo C(1), sucu C(4), comprob C(13), reten C(3),cuit c(13),fecha c(10),certif c(16),monto n(16,2))
	use &d0f99.c alias f99c
	go top in f99b
	do while !eof('f99b')
		if f99b.d5reiv='R' or f99b.d5tico='R'

			m.sucu		= f99b.d5sucu
			m.comprob	= f99b.d5nume
			
			m.reten = f99b.d5pere
			m.cuit=iif(f99b.d5tipo='V',f00.p0cui, iif(!empty(f06.d6cuit),f06.d6cuit,f03.d3cuit))
			m.fecha=dtoc(f99b.d5fech)
			m.certif=iif(f99b.d5tipo='V','0000'+f99b.d5sucu+left(f99b.d5nume,8),'000'+f99b.d5nume)
*===>
*			m.monto=abs(iif(f99b.d5tico='R',f99b.d5tota,f99b.d5imp3))
			m.monto = iif(f99b.d5tico='R',f99b.d5tota,f99b.d5imp3)
			IF m.monto >= 0
						m.signo = "+"
			ELSE
				m.signo = "-"
			ENDIF										

			append blank
			gather memvar		
		endif
		skip in f99b
	enddo
	if eof('f99c')
		wait window 'No hay movimientos para informar' time 1
	else
		w_retu=creadir(curdir()+'AFIP\IVA')
		if w_retu=0 or w_retu=6

			INDEX ON signo + fecha + sucu + comprob + reten		TO ivf099b.idx

			COPY TO &w_dire  FIELDS EXCEPT signo, sucu, comprob FOR signo = "+" TYPE SDF

			do repdest &&(Setea variables internas del reporte)

			go top
			if !empty(p_repclaus) &&(debe ser preview o to printer)
				p_report='iwr961ir'
				=rep_form()
				p_report=''
			endif

			do represet &&(Resetea estado)
		endif
	endif
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
report form iwr961ip
report form iwr961ir
