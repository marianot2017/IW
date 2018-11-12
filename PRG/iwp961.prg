*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : IWP961.PRG -> Exportar Percep./Retenc. de IVA/Ganancias
*									 para AFIP (SICORE) 
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

w_pfile='961'
w_prog =upper(prompt())

*===>
w_title=upper('SICORE - Exportar Per./Ret. de ') + w_prog

w_alta =.f.
w_alias='f05'

*) Apertura de archivos--------------------------------------------------------

fieldvec[1]='F00'
fieldvec[2]='F01'
fieldvec[3]='F01'
fieldvec[4]='F02'
fieldvec[5]='F03'
fieldvec[6]='F04'
fieldvec[7]='F05'
fieldvec[8]='F07'

*fieldvec[6]='F24'

use (d0f00) in 0		alias 'f00'							&&(Parametros)
use (d0f01) in 0 		alias 'f01'		order 'd1tip'  		&&(Tablas)
use (d0f01) in 0 AGAIN	alias 'f01b'	order 'd1tip'  		&&(Tablas)
use (d0f02) in 0		alias 'f02'		order 'd2clie'  	&&(Clientes)
use (d0f03) in 0		alias 'f03'		order 'd3prov'  	&&(Proveedores)
use (d0f04) in 0		alias 'f04'		order 'd4peri'		&&(Periodos)
use (d0f05) in 0		alias 'f05'		order 'd5peri'		&&(Comprobantes)
use (d0f07) in 0		alias 'f07'		order 'd7inte'		&&(Clientes Eventuales)
*use (d0f24) in 0 alias 'f24' order 'd24codi' 		&&(Percep./Retenc./Otros)

if neterr()
	pop key
	pop menu _msysmenu
	fieldvec=''
	=closall()
	return
endif

fieldvec=''

DO creatipos

sele (w_alias)
scatter memvar

*) Variables-------------------------------------------------------------------

w_aux		= ""
w_pvta		= 4
w_pcpra		= 4
w_conterr	= 0

go bottom in f04
wdperi=f04.d4peri

*===>
*		SICORE ES SOLO PARA VENTAS

w_vta	= .T.
w_cpra	= .F.

w_perc	= .T.
w_rete	= .T.

w_dipe	= " "
w_dire	= " "

DO dirs

*===> SETEO VARIABLES PARA IVA O GANANCIAS
IF w_prog	= "I.V.A."
			w_codimp	= "767"
ELSE
	w_codimp	= "217"
ENDIF

w_titulo	= "SICORE - EXPORTACION DE "
w_titulo1	= "SICORE - SUJETOS RETENIDOS/PERCIBIDOS POR CONCEPTO DE "

*)-----------------------------------------------------------------------------

do ssm998.mpr
do iws961.spr


pop key
pop menu _msysmenu
=closall()

dele file &d0f99.b.dbf
dele file &d0f99.c.dbf
DELE FILE ivf099b.idx

return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC Cons961
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
if .not. valid961(.t.)
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


	=gentmp()

	if used('f99c')
		sele f99c
		use
		dele file &d0f99.c.dbf
	endif

	sele 0

	w_cond1 = ""
	w_cond2	= ""
	w_cond3	= ""
	w_cond4 = ""
	w_cond	= ""

	IF w_prog = "I.V.A."
				IF w_vta	AND w_rete
							w_cond3 = "  ( d5tipo = 'V' AND d5reiv = 'R' ) "
*							w_cond3 = "  ( d5tipo = 'V' AND d5tico = 'RT' AND d5reiv = 'R' ) "
*                                   Deberํa ser como lo anterior pero..........................
				ENDIF
				IF w_vta	AND w_perc
							w_cond4 = "  ( d5tipo = 'V' AND d5tico <> 'RT' AND d5reiv = 'P' ) "
*									d5tico <> "RT" es redundante pues en RT es d5reiv = "R"
*									pero a Seguro le dieron perpetua.
				ENDIF
	ELSE
		IF w_vta	AND w_rete
					w_cond3 = "  ( d5tipo = 'V' AND d5rega = 'R' ) "
*					w_cond3 = "  ( d5tipo = 'V' AND d5tico = 'RT' AND d5rega = 'R' ) "
*                           Deberํa ser como lo anterior pero..........................
		ENDIF
		IF w_vta	AND w_perc
					w_cond4 = "  ( d5tipo = 'V' AND d5tico <> 'RT' AND d5rega = 'P' ) "
*							d5tico <> "RT" es redundante pues en RT es d5rega = "R"
*							pero a Seguro le dieron perpetua.
		ENDIF
	ENDIF


	IF !EMPTY(w_cond1)
				w_cond = " AND ( " + w_cond1
	ENDIF					
	IF !EMPTY(w_cond3)
				IF !EMPTY(w_cond)
							w_cond = w_cond + " OR " + w_cond3
				ELSE
					w_cond = " AND ( " + w_cond3
				ENDIF
	ENDIF				
	IF !EMPTY(w_cond4)
				IF !EMPTY(w_cond)
							w_cond = w_cond + " OR " + w_cond4
				ELSE
					w_cond = " AND ( " + w_cond4
				ENDIF
	ENDIF				
	IF !EMPTY(w_cond)
				w_cond = w_cond + " )"
	ENDIF				

	select *;
		from	f05;
		where	d5peri=wdperi  &w_cond		;
		into	dbf &d0f99.c

	use &d0f99.c alias f99c exclusive

 	go top
	if eof()
		wait window 'No hay movimientos para informar' time 1
		return
	endif

*===>
	SCATTER MEMVAR
	IF w_prog = "I.V.A."
				INDEX ON d5tipo + DTOC(d5fech) + d5sucu + d5nume + d5pere		TO ivf099b.idx
	ELSE
		INDEX ON d5tipo + DTOC(d5fech) + d5sucu + d5nume + d5cpga		TO ivf099b.idx
	ENDIF

	SET RELA TO
	SET RELA TO d5clie							INTO f02	ADDITIVE
	SET RELA TO d5clie							INTO f03	ADDITIVE

*===> Generaci๓n archivo AFIP seg๚n lo seleccionado
DO retafip
SELE f99c
USE

*===> 	ACTUALIZO LOS TOTALES DE LOS COMPROBANTES
SELE afip
GO TOP

DO WHILE !EOF()
		SCATTER MEMVAR MEMO
		SELE f05
		IF m.d5tico <> "R"
					SUM d5tota											;
						FOR d5empr = afip.d5empr AND d5peri = afip.d5peri AND d5tipo = afip.d5tipo AND d5sucu = afip.d5sucu AND d5nume = afip.d5nume	;				
						TO	w_total
		ELSE
			SUM d5imre											;
				FOR d5empr = afip.d5empr AND d5peri = afip.d5peri AND d5tipo = afip.d5tipo AND d5sucu = afip.d5sucu AND d5nume = afip.d5nume	;				
				TO	w_total
		ENDIF

*		m.registro = SUBSTR(m.registro,1,28) + STR(ABS(w_total),16,2) + SUBSTR(m.registro,45,99)
		m.registro = SUBSTR(m.registro,1,28) + STR(w_total,16,2) + SUBSTR(m.registro,45,99)
		SELE afip

		GATHER MEMVAR
		SKIP
ENDDO

*===> Generaci๓n de archivo de RETENIDOS

INDEX ON d5clie UNIQUE TO iv099b.idx

SELE f02
SET RELA TO "JU" + d2juri + SPACE(5)		INTO f01	ADDITIVE
SELE f03
SET RELA TO "JU" + d3juri + SPACE(5)		INTO f01b	ADDITIVE

SELE afip
SET RELA TO d5clie							INTO f02	ADDITIVE
SET RELA TO d5clie							INTO f03	ADDITIVE

SELE afip
GO TOP

IF EOF()
		WAIT WIND "NO HAY INFORMACION PARA EXPORTAR"
		SET RELA TO
		USE
		SELE f02
		SET RELA TO
		SELE f03
		SET RELA TO
		RETURN
ENDIF

DO WHILE !EOF()
		SCATTER MEMVAR
		IF d5tipo = "C"
				w_pref	= "D3"
				w_x		= "f01b"
				SELE "f03"
		ELSE				
			w_pref	= "D2"
			w_x		= "f01"
			SELE "f02"
		ENDIF

		m.registro1 = ALLT(STRTRAN(&w_pref.cuit,"-",""))	+ SPACE(11 - LEN(ALLT(STRTRAN(&w_pref.cuit,"-",""))))				&& LARGO 11
		m.registro1 = m.registro1 + SUBSTR(&w_pref.nomb,1,20)
		m.registro1 = m.registro1 + SUBSTR(&w_pref.dire,1,20)
		m.registro1 = m.registro1 + &w_pref.loca

*===> QUILOMBO DE LA PROVINCIA, sale de la relaci๓n de CLI/PROV con el 01 x el cpo JURISDICCION q es obligatorio.
		m.registro1 = m.registro1 + &w_x..d1tdgi
		
		m.registro1 = m.registro1 + &w_pref.copo

		m.registro1 = m.registro1 + &w_pref.tido				

		IF !(&w_pref.tido = "80" OR &w_pref.tido = "83" OR &w_pref.tido = "84" OR &w_pref.tido = "86" OR &w_pref.tido = "87")
				w_conterr	= w_conterr + 1
				m.error		= "*"
		ENDIF
		IF EMPTY(ALLT(STRTRAN(&w_pref.cuit,"-","")))
					w_conterr	= w_conterr + 1
					m.error		= "*"
		ENDIF

		SELE f01t
		SEEK "JD" + &w_x..d1tdgi
		IF FOUND()
				m.provincia	= f01t.desc
		ELSE
			m.error = "*"
			w_conterr	= w_conterr + 1
		ENDIF									
		SELE afip
		GATHER MEMVAR
		SKIP
ENDDO

DO listar

SET RELA TO
USE
SELE f02
SET RELA TO
SELE f03
SET RELA TO

return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC when961
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
w_retu=.t.
w_vari=varread()
p_curobj=_curobj

return(w_retu)

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC valid961
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS w_show

if empty(parameters())
	w_show=.f.
endif

w_retu=.t.
w_vari=varread()
w_vread='m.'+w_vari

if w_vari='W_VTA'
		IF w_prog <> "I.V.A."
				w_pvta	= 4
				DO valgan
				IF !w_retu
						=advgrave("GANANCIAS NO ESTA DEFINIDO")
						w_vta = .F.
						_curobj	= objnum(w_perc)
						SHOW GETS
				ENDIF
		ENDIF				
		IF w_retu
				=mchkbox(@w_vta)
		ENDIF
endif

if w_vari='W_CPRA'
		IF w_prog <> "I.V.A."
				w_pcpra	= 4
				DO valgan
				IF !w_retu
						=advgrave("GANANCIAS NO ESTA DEFINIDO")
						w_cpra = .F.
						_curobj	= objnum(w_vta)
						SHOW GETS
				ENDIF
		ENDIF				
		IF w_retu
			=mchkbox(@w_cpra)
		ENDIF
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

if w_vari = 'W_PERC' or w_show
			DO dirs
endif
		
if w_vari='W_RETE'
	=mchkbox(@w_rete)
endif

if w_vari = 'W_RETE' or w_show
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

*===================================================================================
PROCEDURE retafip

SELE 0
CREATE CURSOR afip (error C(1), d5tipo C(1), d5tico C(1), d5tiim C(2), d5clie C(5), d5sucu C(4), d5nume C(13),registro C(143), registro1 C(83), d5empr C(2), d5peri C(7), provincia C(20))

SCATTER MEMVAR 

SELE f99c

w_cond	= ""
IF w_perc
	IF w_prog	= "I.V.A."
				w_cond = "d5reiv = 'P'"
	ELSE
		w_cond = "d5rega = 'P'"
	ENDIF
ENDIF

IF w_rete
	IF !EMPTY(w_cond)
			IF w_prog	= "I.V.A."
						w_cond = w_cond + " OR d5reiv = 'R'"
			ELSE
				w_cond = w_cond +" OR d5rega = 'R'"
			ENDIF
	ELSE
		IF w_prog	= "I.V.A."
					w_cond = "d5reiv = 'R'"
		ELSE
			w_cond = "d5rega = 'R'"
		ENDIF
	ENDIF
ENDIF

SET FILTER TO &w_cond

GO TOP
SCATTER MEMVAR

w_tipant	= m.d5tipo
w_sucant	= m.d5sucu
w_numant	= m.d5nume

IF w_prog	= "I.V.A."
			w_codant	= m.d5pere
			w_retant	= m.d5reiv
ELSE
	w_codant	= m.d5cpga
	w_retant	= m.d5rega
ENDIF			

w_total		= 0
w_base		= 0
w_reten		= 0

DO WHILE !EOF()
		IF w_tipant <> d5tipo
				DO genreg
				LOOP
		ENDIF
		IF w_sucant <> d5sucu
				DO genreg
				LOOP
		ENDIF
		IF w_numant <> d5nume
				DO genreg
				LOOP
		ENDIF

		IF w_prog	= "I.V.A."
					IF w_retant <> d5reiv OR w_codant <> d5pere
								DO genreg
								LOOP
					ENDIF
		ELSE
			IF w_retant <> d5rega OR w_codant <> d5cpga
						DO genreg
						LOOP
			ENDIF
		ENDIF

		SCATTER MEMVAR
		w_total		= 0		&& Como es el total del comprobante, se calcula despu้s de terminar
							&& xq acแ SOLO estแn los movs. de retenciones
					
		IF m.d5tico = "R"
					w_base		= w_base	+ m.d5imre
					w_reten		= w_reten	+ m.d5tota
		ELSE
			w_base		= w_base	+ m.d5imp1	+ m.d5imp2  && VER seg๚n GABY
			IF w_prog	= "I.V.A."
						w_reten		= w_reten	+ m.d5imp3
			ELSE
				IF d5tipo = "C"
						w_var	= "f99c.d5imp" + STR(w_pcpra,1)
						w_reten = &w_var
				ELSE 
					w_var	= "f99c.d5imp" + STR(w_pvta,1)
					w_reten	= &w_var
				ENDIF
			ENDIF
		ENDIF
		SKIP
ENDDO

IF w_total <> 0 OR w_base <> 0 OR w_reten <> 0
		SKIP -1
		DO genreg
ENDIF		

RETURN

*===================================================================================
PROCEDURE genreg

SELE afip
APPE BLAN

*===> CODIGO DE COMPROBANTE
IF m.d5tico = "A" OR m.d5tico = "8"  OR m.d5tico = "6"  && Factura, Recibo-Factura, Ticket-Factura
		m.registro = "01"	
ENDIF		
IF m.d5tico = "@"										&& Recibo
		m.registro = "02"
ENDIF		
IF m.d5tico = "C"										&& Nota CR
		m.registro = "03"
ENDIF		
IF m.d5tico = "B"										&& Nota DB
		m.registro = "04"
ENDIF		
IF m.d5tico = "?" OR m.d5tico = "7"  OR m.d5tico = "R"  && Tickets, Liq. Varias, Retenciones
		m.registro = "05"	
ENDIF		


*===>          FECHA EMISION COMPROBANTE 		
m.registro = m.registro + DTOC(m.d5fech)

*===>          NRO DEL COMPROBANTE, tiene que ser num้rico
w_aux	= ""
FOR n = 1 TO LEN(m.d5nume)
			IF "0" <= SUBSTR(m.d5nume,n,1) AND SUBSTR(m.d5nume,n,1) <= "9"
						w_aux	= w_aux + SUBSTR(m.d5nume,n,1)
			ENDIF						
ENDFOR

IF m.d5tipo = "C"
			m.registro = m.registro + w_aux + SPACE(16 - LEN(w_aux))
ELSE
	w_aux	= m.d5sucu	+ w_aux

	IF LEN(w_aux) > 12
					m.registro = m.registro + SUBSTR(w_aux,1,12) + SPACE(4)
	ELSE
		m.registro = m.registro + w_aux + SPACE(16 - LEN(w_aux))
	ENDIF
ENDIF

*===>          IMPORTE COMPROBANTE 
m.registro = m.registro + STR(w_total,16,2)

*===>                      IVA/GAN
m.registro = m.registro + w_codimp

*===> CODIGO DE REGIMEN
IF w_prog	= "I.V.A."
			m.registro = m.registro + m.d5pere
ELSE
	m.registro = m.registro + m.d5cpga
ENDIF


*===> C๓digo de Percepci๓n o Retenci๓n
IF w_prog	= "I.V.A."
			IF m.d5reiv = "R"
					m.registro = m.registro + "1"
			ELSE			
				m.registro = m.registro + "2"
			ENDIF	
ELSE
	IF m.d5rega = "R"
				m.registro = m.registro + "1"
	ELSE			
		m.registro = m.registro + "2"
	ENDIF	
ENDIF

*===>                     BASE DE CALCULO
*m.registro = m.registro + STR(ABS(w_base),14,2)
m.registro = m.registro + STR(w_base,14,2)

*===>                     FECHA EMISION RETENCION		
m.registro = m.registro + DTOC(m.d5fech)


*===> CODIGO DE CONDICION
IF m.d5tiim = "3" OR m.d5tiim = "4"  OR m.d5tiim = "5"  && Cons. Final, Exento, No Responsable
		m.registro = m.registro + "00"	
ENDIF		
IF m.d5tiim = "1"										&& Resp. Insc.
		m.registro = m.registro + "01"	
ENDIF		
IF m.d5tiim = "2"										&& Resp. No Insc.
		m.registro = m.registro + "02"	
ENDIF		
IF m.d5tiim = "7"										&& No Nategorizado
		m.registro = m.registro + "03"	
ENDIF		
IF m.d5tiim = "6"										&& Resp. Monotributo
		m.registro = m.registro + "10"	
ENDIF		


*===>                     IMPORTE RETENCION  + %JE EXCLUSION + FECHA EMISION BOLETIN
*m.registro = m.registro + STR(ABS(w_reten),14,2)  + SPACE(6)      + SPACE(10)
m.registro = m.registro + STR(w_reten,14,2)  + SPACE(6)      + SPACE(10)


*===> Tipo y nro. de documento del Retenido

IF m.d5tipo = "C"
			w_pref = "D3"
			SELE "f03"
ELSE				
	w_pref = "D2"
	SELE "f02"
ENDIF

SEEK m.d5clie

m.registro = m.registro + &w_pref.tido				

m.registro = m.registro + ALLT(STRTRAN(&w_pref.cuit,"-","")) + SPACE(20 - LEN(ALLT(STRTRAN(&w_pref.cuit,"-",""))))  && LARGO 20

IF !(&w_pref.tido = "80" OR &w_pref.tido = "83" OR &w_pref.tido = "84" OR &w_pref.tido = "86" OR &w_pref.tido = "87")
				m.error		= "*"
ENDIF
IF EMPTY(ALLT(STRTRAN(&w_pref.cuit,"-","")))
					m.error		= "*"
ENDIF


*===> QUILOMBO del documento original  x NC ( NO SE CARGA ACTUALMENTE, NO ESTA PREVISTO )
IF !seek(m.d5inte,'f07')		OR										;
	(seek(m.d5inte,'f07')	AND EMPTY(f07.d7numori) )
				m.registro = m.registro + "              "
*										   123456789.1234
ELSE
	SELE f07
	SCATTER MEMVAR
	w_aux	= ""
	FOR n = 1 TO LEN(m.d7numori)
				IF "0" <= SUBSTR(m.d7numori,n,1) AND SUBSTR(m.d7numori,n,1) <= "9"
								w_aux	= w_aux + SUBSTR(m.d7numori,n,1)
				ENDIF						
	ENDFOR
	w_aux	= m.d7sucori	+ w_aux

	IF LEN(w_aux) > 12
					m.registro = m.registro + SUBSTR(w_aux,1,12) + SPACE(2)
	ELSE
		m.registro = m.registro + w_aux + SPACE(14 - LEN(w_aux))
	ENDIF
ENDIF

SELE afip
GATHER MEMVAR

SELE f99c
SCATTER MEMVAR

w_tipant	= m.d5tipo
w_sucant	= m.d5sucu
w_numant	= m.d5nume
m.error		= ""

IF w_prog	= "I.V.A."
			w_codant	= m.d5pere
			w_retant	= m.d5reiv
ELSE
	w_codant	= m.d5cpga
	w_retant	= m.d5rega
ENDIF			

w_total		= 0
w_base		= 0
w_reten		= 0
RETURN

*===================================================================================
PROCEDURE valgan

w_retu	= .F.

IF w_vari = "W_CPRA"
		FOR w_pcpra = 1 TO 3
				w_pc_pr	= "f00.p0prc" + STR(w_pcpra,1)
				IF &w_pc_pr = 2
							w_pcpra = w_pcpra + 4
							w_retu	= .T.
				ENDIF
		ENDFOR

ENDIF

IF w_vari = "W_VTA"
		FOR w_pvta = 1 TO 3
				w_pc_pr	= "f00.p0prv" + STR(w_pvta,1)
				IF &w_pc_pr = 2
							w_pvta = w_pvta + 4
							w_retu	= .T.
				ENDIF
		ENDFOR

ENDIF

RETURN

*==============================================================================
PROCEDURE listar

IF w_conterr <> 0
		IF w_conterr = 1
					=advgrave("HUBO 1 ERROR." + CHR(13) + CHR(13) + "LOS ARCHIVOS SERAN RECHAZADOS POR AFIP")
		ELSE
			=advgrave("HUBIERON "+ ALLT(STR(w_conterr,5,0)) + " ERRORES." + CHR(13) + CHR(13) + "LOS ARCHIVOS SERAN RECHAZADOS POR AFIP")
		ENDIF
		w_conterr = 0
ENDIF

GO TOP

DO repdest &&(Setea variables internas del reporte)

IF !EMPTY(p_repclaus)					&&(debe ser preview o to printer)
		p_report	= 'iwr961s'
		=rep_form()
		p_report=''
		
		COPY TO &w_dire  FIELDS registro1 TYPE SDF


		INDEX ON d5tipo + SUBSTR(registro,51,1) + SUBSTR(registro,3,10) + d5sucu + d5nume 	TO ivf099b.idx
		GO TOP
		p_report	= 'iwr961'
		=rep_form()
		p_report=''

*===>  REEMPLAZAR LOS -
		GO TOP
		DO WHILE !EOF()
				SCATTER MEMVAR
				m.registro = SUBSTR(m.registro,1,28) + STRTRAN(SUBSTR(m.registro,29,16), "-", " ") + SUBSTR(m.registro,45,99)
				m.registro = SUBSTR(m.registro,1,51) + STRTRAN(SUBSTR(m.registro,52,14), "-", " ") + SUBSTR(m.registro,66,78)
				m.registro = SUBSTR(m.registro,1,77) + STRTRAN(SUBSTR(m.registro,78,14), "-", " ") + SUBSTR(m.registro,92,52)
				GATHER MEMVAR
				SKIP
		ENDDO

		COPY TO &w_dipe  FIELDS registro TYPE SDF

ENDIF

DO represet								&&(Resetea estado)

RETURN

*==============================================================================
PROCEDURE dirs

IF w_perc OR w_rete
			IF w_prog	= "I.V.A."
						w_dipe	= CURDIR()+'AFIP\SICORE\IR'+ f77.d77empr + SUBSTR(PTOI(wdperi),3,4) + '.TXT'
						w_dire	= CURDIR()+'AFIP\SICORE\IS'+ f77.d77empr + SUBSTR(PTOI(wdperi),3,4) + '.TXT'
			ELSE
				w_dipe	= CURDIR()+'AFIP\SICORE\GR'+ f77.d77empr + SUBSTR(PTOI(wdperi),3,4) + '.TXT'
				w_dire	= CURDIR()+'AFIP\SICORE\GS'+ f77.d77empr + SUBSTR(PTOI(wdperi),3,4) + '.TXT'
			ENDIF
ELSE
	w_dipe = " "
	w_dire = " "
ENDIF

SHOW GET w_dipe
SHOW GET w_dire

RETURN

*==================================================================================
***puesto adrede para que incluya el report al proyecto**
report form iwr961
report form iwr961s