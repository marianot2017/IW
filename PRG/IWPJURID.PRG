*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : IWPJURID.PRG -> Exportar Percep./Retenc. de Ganancias
*									 para AFIP (Personas Jurํdicas-Sociedades) 
*									 ES SOLO PARA COMPRAS
*)-H Release		 : 02
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

w_d0pic		= d0pic

*) Variables internas----------------------------------------------------------

w_pfile='961'

w_prog =upper(prompt())
IF BAR()	= 3
			w_prog	= "Personas Jurํdicas v 9.0"
			w_bar	= 4
ENDIF			
IF BAR()	= 4
			w_prog = "Personas Jurํdicas v 10.0"
			w_bar	= 5
ENDIF			

w_title=upper('SIAP - Exportar Per./Ret. de ') + w_prog

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
fieldvec[9]='F24'

use (d0f00) in 0		alias 'f00'							&&(Parametros)
use (d0f01) in 0 		alias 'f01'		order 'd1tip'  		&&(Tablas)
use (d0f01) in 0 AGAIN	alias 'f01b'	order 'd1tip'  		&&(Tablas)
use (d0f02) in 0		alias 'f02'		order 'd2clie'  	&&(Clientes)
use (d0f03) in 0		alias 'f03'		order 'd3prov'  	&&(Proveedores)
use (d0f04) in 0		alias 'f04'		order 'd4peri'		&&(Periodos)
use (d0f05) in 0		alias 'f05'		order 'd5peri'		&&(Comprobantes)
use (d0f07) in 0		alias 'f07'		order 'd7inte'		&&(Clientes Eventuales)
use (d0f24) in 0 		alias 'f24'		order 'd24codi'		&&(Percep./Retenc./Otros)

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
wdperi=RIGHT(f04.d4peri,4)

				w_retu	= .F.
				w_pcpra	= 4
				DO valgan
				IF !w_retu
						=advgrave("GANANCIAS NO ESTA DEFINIDO")

						&& R.01b Mariano
						pop key
						pop menu _msysmenu
						=closall()

						dele file &d0f99.b.dbf
						dele file &d0f99.c.dbf
						DELE FILE ivf099b.idx
						&& R.01e Mariano
						
						RETURN
				ENDIF

d0pic		= '9999'

w_vta	= .F.
w_cpra	= .T.
w_texto = 'COMPRAS'
w_texto1= 'A๑o :'
w_perc	= .T.
w_rete	= .T.

w_dipe	= " "
w_dire	= " "

DO dirs

w_titulo	= "SIAP - Personas Jurํdicas-Sociedades - EXPORTACION DE "

*)-----------------------------------------------------------------------------

do ssm998.mpr
do iwscpa2.spr


pop key
pop menu _msysmenu
=closall()

dele file &d0f99.b.dbf
dele file &d0f99.c.dbf
DELE FILE ivf099b.idx

d0pic		= w_d0pic

return

***vfp***
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons961
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	on key label f9 do cons961x
	keyboard '{f9}'
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons961x
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
on key label f9
***vfp***

if .not. valid961(.t.)
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

	if used('f99c')
		sele f99c
		use
	endif
	dele file f99c.dbf

	sele 0

	w_cond1 = ""
	w_cond2	= ""
	w_cond3	= ""
	w_cond4 = ""
	w_cond	= ""

		IF w_rete
					w_cond3 = "  ( d5tipo = 'C' AND d5rega = 'R' ) "
		ENDIF
		IF w_perc
					w_cond4 = "  ( d5tipo = 'C' AND d5rega = 'P' ) "
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
		where	RIGHT(d5peri, 4) = RIGHT(wdperi,4)  &w_cond		;
		into	dbf f99c


*		where	RIGHT(d5peri, 4) = RIGHT(wdperi,4)  &w_cond		

 	go top

	if eof()
		wait window 'No hay movimientos para exportar'
		return
	endif

*===>
	SCATTER MEMVAR
	INDEX ON d5tico + d5tifo + DTOS(d5fech) + d5nume + d5inte 	TO f99c.idx

	SET RELA TO
	SET RELA TO d5clie							INTO f02	ADDITIVE
	SET RELA TO d5clie							INTO f03	ADDITIVE

*===> Generaci๓n archivo AFIP seg๚n lo seleccionado

DO retafip

SELE f99c
USE

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

w_print =.F.

DO listar

if w_print
			do represet &&(Resetea estado)
endif

*===> OJO !!!!!!!!!!  Ganancias Personas Juridicas ES CON "," NO "."
GO TOP
DO WHILE !EOF()
			SCATTER MEMVAR
			m.registro = STRTRAN(m.registro, ".", ",")
			GATHER MEMVAR
			SKIP
ENDDO

IF !EMPTY(w_dire)
	COPY TO &w_dire  FIELDS registro TYPE SDF
ELSE
	COPY TO &w_dipe  FIELDS registro TYPE SDF
ENDIF

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
						wait window 'Debe elegir Percepciones y/o Retenciones'
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
*==============================================================================

SELE 0

IF w_bar = 5
CREATE CURSOR afip (error C(1), d5tipo C(1), d5tico C(1), d5tifo C(1), d5inte C(6), d5clie C(5), d5nume C(14),registro C(70), d5empr C(2), d5peri C(7), provincia C(20))	&& R.02 Mariano (14 x 13)
ENDIF

IF w_bar = 4
CREATE CURSOR afip (error C(1), d5tipo C(1), d5tico C(1), d5tifo C(1), d5inte C(6), d5clie C(5), d5nume C(14),registro C(56), d5empr C(2), d5peri C(7), provincia C(20))	&& R.02 Mariano (14 x 13)
ENDIF

SCATTER MEMVAR 

SELE f99c

GO TOP
SCATTER MEMVAR

w_tipant	= m.d5tipo
w_sucant	= m.d5sucu
w_numant	= m.d5nume

	w_codant	= m.d5cpga
	w_retant	= m.d5rega

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

			IF w_retant <> d5rega OR w_codant <> d5cpga
						DO genreg
						LOOP
			ENDIF

		SCATTER MEMVAR
		w_total		= 0		&& Como es el total del comprobante, se calcula despu้s de terminar
							&& xq acแ SOLO estแn los movs. de retenciones
					
		IF m.d5tico = "R"
					w_base		= w_base	+ m.d5imre
					w_reten		= w_reten	+ m.d5tota
		ELSE
			w_base		= w_base	+ m.d5imp1	+ m.d5imp2  && VER seg๚n GABY
			w_var	= "f99c.d5imp" + STR(w_pcpra,1)
			w_reten = &w_var
		ENDIF
		SKIP
ENDDO

IF w_total <> 0 OR w_base <> 0 OR w_reten <> 0
		DO genreg
ENDIF		

RETURN

*===================================================================================
PROCEDURE genreg
*==============================================================================

SKIP -1

SELE afip
APPE BLAN

SELE f99c

IF w_bar	= 4					&& V 9
			DO perjurv9
ENDIF

IF w_bar	= 5					&& V 10
			DO perjurv10
ENDIF


SELE afip
GATHER MEMVAR

SELE f99c
SKIP

SCATTER MEMVAR


w_tipant	= m.d5tipo
w_sucant	= m.d5sucu
w_numant	= m.d5nume

	w_codant	= m.d5cpga
	w_retant	= m.d5rega

w_total		= 0
w_base		= 0
w_reten		= 0
RETURN

*===================================================================================
PROCEDURE valgan
*==============================================================================

w_retu	= .F.

		FOR w_pcpra = 1 TO 3
				w_pc_pr	= "f00.p0prc" + STR(w_pcpra,1)
				IF &w_pc_pr = 2
							w_pcpra = w_pcpra + 4
							w_retu	= .T.
				ENDIF
		ENDFOR

RETURN

*==============================================================================
PROCEDURE listar
*==============================================================================


COUNT FOR !EMPTY(error) TO w_conterr

IF w_conterr <> 0
		IF w_conterr = 1
					=advgrave("HUBO 1 ERROR." + CHR(13) + CHR(13) + "LOS ARCHIVOS SERAN RECHAZADOS POR AFIP")
		ELSE
			=advgrave("HUBIERON "+ ALLT(STR(w_conterr,5,0)) + " ERRORES." + CHR(13) + CHR(13) + "LOS ARCHIVOS SERAN RECHAZADOS POR AFIP")
		ENDIF
		w_conterr = 0
ENDIF

GO TOP

p_report	= 'iwrjurid'

DO listalib

RETURN

*==============================================================================
PROCEDURE dirs
*==============================================================================

w_root	= CURDIR()+'AFIP\GANANCIAS\'

IF w_bar = 4
			w_root		= w_root + '9'
			w_titulo	= "SIAP - Ganancias Personas Jurํdicas v 9.0 - EXPORTACION DE "
ENDIF
IF w_bar = 5
			w_root		= w_root + '10'
			w_titulo	= "SIAP - Ganancias Personas Jurํdicas v 10.0 - EXPORTACION DE "
ENDIF


IF w_perc AND w_rete
				w_dipe		= w_root + 'J' + f77.d77empr + wdperi + '.TXT'
				w_dire		= w_dipe
				w_titulo	= w_titulo + 'PERCEPCIONES/RETENCIONES'
ELSE
	IF w_perc
				w_dipe	= w_root + 'P' + f77.d77empr + wdperi + '.TXT'
				w_dire	= ''
				w_titulo	= w_titulo + 'PERCEPCIONES'
	ELSE
		IF w_rete
				w_dire	= w_root + 'R' + f77.d77empr + wdperi + '.TXT'
				w_dipe	= ''
				w_titulo	= w_titulo + 'RETENCIONES'
		ELSE
			w_dipe = " "
			w_dire = " "
		ENDIF
	ENDIF
ENDIF

SHOW GET w_dipe
SHOW GET w_dire

RETURN

*==================================================================================
PROCEDURE perjurv9
*==============================================================================

*===> C๓digo de Percepci๓n o Retenci๓n
	IF m.d5rega = "R"
				m.registro = "1"
	ELSE			
		m.registro = "2"
	ENDIF	

*===>		CUIT
		IF m.d5tico = 'R'							&& en RETENCIONES es CLIENTE
					w_pref = "f02.D2"
		ELSE
			w_pref = "f03.D3"
		ENDIF
		m.registro = m.registro + ALLT(STRTRAN(&w_pref.cuit,"-",""))

*===>       NRO DEL COMPROBANTE, tiene que ser num้rico
		w_aux	= ""
		FOR n = 1 TO LEN(m.d5nume)
					IF "0" <= SUBSTR(m.d5nume,n,1) AND SUBSTR(m.d5nume,n,1) <= "9"
								w_aux	= w_aux + SUBSTR(m.d5nume,n,1)
					ENDIF						
		ENDFOR

		IF LEN(w_aux) > 13	&& R.02 Mariano (13 x 12)
			m.registro = m.registro + SPACE(3) + SUBSTR(w_aux,1,13)	&& R.02 Mariano (3 x 4 y 13 x 12)
		ELSE
			m.registro = m.registro + SPACE(16 - LEN(w_aux)) + w_aux
		ENDIF

*===>       FECHA EMISION COMPROBANTE 		
		m.registro = m.registro + DTOC(m.d5fech) 

*===>		CODIGO DE REGIMEN
*		=SEEK(m.d5cpga + 'G','f24')
*		IF EMPTY(f24.d24gpf8)
*				m.error = '*'
*		ENDIF				
*		m.registro = m.registro + f24.d24gpf8
		m.registro = m.registro + m.d5cpga

*===>       IMPORTE COMPROBANTE 
		m.registro = m.registro + STR(w_reten,15,2)

RETURN

*==========================================================================================
PROCEDURE perjurv10
*==============================================================================

*===> C๓digo de Percepci๓n o Retenci๓n
	IF m.d5rega = "R"
				m.registro = "1"
	ELSE			
		m.registro = "2"
	ENDIF	

*===>		CUIT
		IF m.d5tico = 'R'							&& en RETENCIONES es CLIENTE
					w_pref = "D2"
					SELE "f02"
		ELSE
			w_pref = "D3"
			SELE "f03"
		ENDIF
		m.registro = m.registro + ALLT(STRTRAN(&w_pref.cuit,"-",""))

*===>       FECHA EMISION COMPROBANTE 		
		m.registro = m.registro + DTOC(m.d5fech)

*===>		CODIGO DE REGIMEN
*		=SEEK(m.d5cpga + 'G','f24')
*		IF EMPTY(f24.d24gpf8)
*				m.error = '*'
*		ENDIF				
*		m.registro = m.registro + f24.d24gpf8

		m.registro = m.registro + m.d5cpga

*===>       IMPORTE COMPROBANTE 
		m.registro = m.registro + STR(w_reten,15,2)

*===>       NRO DEL COMPROBANTE, tiene que ser num้rico
		w_aux	= ""
		FOR n = 1 TO LEN(m.d5nume)
					IF "0" <= SUBSTR(m.d5nume,n,1) AND SUBSTR(m.d5nume,n,1) <= "9"
								w_aux	= w_aux + SUBSTR(m.d5nume,n,1)
					ENDIF						
		ENDFOR

		IF LEN(w_aux) > 13	&& R.02 Mariano (13 x 12)
			m.registro = m.registro + RIGHT(w_aux,13) + SPACE(17)		&& R.02 Mariano (13 x 12 y 17 x 18)
		ELSE
			m.registro = m.registro + w_aux + SPACE(30 - LEN(w_aux))
		ENDIF

RETURN

*==========================================================================================

***puesto adrede para que incluya el report al proyecto**
report form iwrjurid