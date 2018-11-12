*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : IWP900C.PRG -> Reconstructor de saldos
*)-H
*)-H Fecha de inicio : 22/12/1998.-
*)-H Fecha de fin    : 29/12/1998.-
*)-H
*)-H Actualizaciones
*)-H
*)-H Fecha       Programmer      Comentarios
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PRIVATE ALL LIKE w_*
*===>
PUBLIC filtrado, mensaje

*) Salvado de estados anteriores-----------------------------------------------

push key clear		  &&(Salva estados de teclas)
push menu _msysmenu &&(Salva estado de menu)

if sino('Confirma Reconstrucci๓n de saldos de comprobantes ?.','N','Sistema')='N'
	return
endif

on key label f9 DO cons900r
w_prog = "900r"

wait wind nowait 'Procesando...'

fieldvec[1]='F00'
fieldvec[2]='F04'
fieldvec[3]='F05'

use (d0f00) in 0 alias 'f00'				EXCLUSIVE &&(Parแmetros)
use (d0f01) in 0 alias 'f01' order 'd1tip'	EXCLUSIVE &&(Tablas)
use (d0f02) in 0 alias 'f02' order 'd2clie'	EXCLUSIVE &&(Tablas)
use (d0f03) in 0 alias 'f03' order 'd3prov'	EXCLUSIVE &&(Tablas)
use (d0f04) in 0 alias 'f04' order 'd4peri' EXCLUSIVE &&(Periodos)
use (d0f05) in 0 alias 'f05' order 'd5peri' EXCLUSIVE &&(Comprobantes)

if neterr()
	fieldvec=''
	=closall()
	return
endif

fieldvec=''

set skip of menu _msysmenu .t.
do ssm998.mpr	&&(menu registro)

if !lockemp(nf_coem)

	wait clear

	=errornet(050)

else
*===>
	SELE f05
	w_cantv	= 0
	w_cantc	= 0

	DO corrige


	IF w_cantv + w_cantc <> 0  OR !EMPTY(mensaje)
					=CLOSALL()
					pop key
					pop menu _msysmenu
					CLEAR READ ALL
					RETURN
	ENDIF

	SELE F05
	SET FILTER TO

	if begintran()

		sele f04
		repl all;
			d4vtot with 0,;
			d4vim1 with 0,;
			d4vim2 with 0,;
			d4vim3 with 0,;
			d4vim4 with 0,;
			d4vim5 with 0,;
			d4vim6 with 0,;
			d4vim7 with 0,;
			d4vim8 with 0,;
			d4ctot with 0,;
			d4cim1 with 0,;
			d4cim2 with 0,;
			d4cim3 with 0,;
			d4cim4 with 0,;
			d4cim5 with 0,;
			d4cim6 with 0,;
			d4cim7 with 0,;
			d4cim8 with 0

		sele f05
		go top
*===>
		SCATTER MEMVAR

		do while !eof('f05')
			if seek(ptoi(f05->d5peri),'f04')

				do case
*===> 
					CASE (m.d5tipo = "V" OR m.d5tipo = "C") AND m.d5tico	= "R"
						DO reten						&& Retenciones simples

					CASE (m.d5tipo = 'V' OR m.d5tipo = 'C')
						DO comprobante 

					otherwise

						=dele_rec('f05')

				endcase

			else

				=dele_rec('f05')

			endif
			SELE f05
			SKIP
			SCATTER MEMVAR

		enddo

		=endtran()

	endif


	=unlockemp(nf_coem)

	wait clear

	=advsuave('Reconstrucci๓n de saldos de comprobantes finalizada.','Informaci๓n')

endif

=closall()
pop key
pop menu _msysmenu

return

*========================================================================================
*===>
PROCEDURE compret

*===>  DESACTIVADO - NO SE EJECUTA

RETURN

SELE f04

IF f05.d5tipo	= "V"
				w_imp	= "d4vim"
				w_pr	= "f00.p0prv"
ELSE
	w_imp	= "d4cim"
	w_pr	= "f00.p0prc"
ENDIF

w_f05imp	= "f05.d5imp"

IF f05.d5tipo	= "V"
				REPLACE d4vtot WITH d4vtot  + f05.d5tota,		;
						d4vim1 WITH d4vim1 + f05.d5imp1,		;
						d4vim2 WITH d4vim2 + f05.d5imp2,		;
						d4vim4 WITH d4vim4 + f05.d5imp4,		;
						d4vim5 WITH d4vim5 + f05.d5imp5
ELSE
	REPLACE d4ctot WITH d4ctot  + f05.d5tota,		;
			d4cim1 WITH d4cim1 + f05.d5imp1,		;
			d4cim2 WITH d4cim2 + f05.d5imp2,		;
			d4cim4 WITH d4cim4 + f05.d5imp4,		;
			d4cim5 WITH d4cim5 + f05.d5imp5
ENDIF

*f00.p0pr{c/v}[n]							n = 1, 2, 3
*					= 1 va en {c/v}im6
*					= 2 va en {c/v}im7
*					= 3 va en {c/v}im8					

IF !EMPTY(f05.d5pere)		&& IVA 
		IF f05.d5tipo	= "V"
					REPLACE d4vim3 WITH d4vim3 + f05.d5imp3
		ELSE
			REPLACE d4cim3 WITH d4cim3 + f05.d5imp3
		ENDIF
ENDIF

FOR n = 1 TO 3			
*					1 = Ingresos Brutos
*					2 = Ganancias
*					3 = Otros

			w_pr		= SUBSTR(w_pr,1,9) + STR(n,1)

			IF &w_pr <> 4
						w_imp		= SUBSTR(w_imp,1,5) + STR( 5 + &w_pr.,1)
						
						w_f05imp	= SUBSTR(w_f05imp,1,9) + STR( 5 + &w_pr.,1)

						REPLACE &w_imp WITH &w_imp + &w_f05imp
			ENDIF
ENDFOR

RETURN

*=====================================================================================
*===>
PROCEDURE corrige

SELE f05
mensaje = " "

*===========================
* VENTAS
pc_pr	= "f00.p0prv"

filtro1 = " "

COUNT FOR !EMPTY(d5cpib) AND d5tipo = "V"			TO a
FOR pn_n = 1 TO 3
		pc_pr	= SUBSTR(pc_pr,1,9) + STR(pn_n,1)
		IF &pc_pr = 1
				pn_n = pn_n + 4
		ENDIF
ENDFOR
IF a <> 0	AND pn_n = 4
			mensaje = "En Ventas existen Percepciones/Retenciones e INGRESOS BRUTOS NO ESTA DEFINIDO"
			filtro1 = " (!EMPTY(d5cpib) AND d5tipo = 'V') "
ENDIF

COUNT FOR !EMPTY(d5cpga) AND d5tipo = "V"			TO a
FOR pn_n = 1 TO 3
		pc_pr	= SUBSTR(pc_pr,1,9) + STR(pn_n,1)
		IF &pc_pr = 2
				pn_n = pn_n + 4
		ENDIF
ENDFOR
IF a <> 0	AND pn_n = 4
			IF EMPTY(mensaje)
						mensaje = "En Ventas existen Percepciones/Retenciones y GANANCIAS NO ESTA DEFINIDO"
						filtro1 = " (!EMPTY(d5cpga) AND d5tipo = 'V') "
			ELSE
				mensaje = mensaje + CHR(13) + CHR(13) + "En Ventas existen Percepciones/Retenciones y GANANCIAS NO ESTA DEFINIDO"
				filtro1 = filtro1 + " OR (!EMPTY(d5cpga) AND d5tipo = 'V') "
			ENDIF
ENDIF

COUNT FOR !EMPTY(d5cpot) AND d5tipo = "V"			TO a
FOR pn_n = 1 TO 3
		pc_pr	= SUBSTR(pc_pr,1,9) + STR(pn_n,1)
		IF &pc_pr = 3
				pn_n = pn_n + 4
		ENDIF
ENDFOR
IF a <> 0	AND pn_n = 4
			IF EMPTY(mensaje)
						mensaje = "En Ventas existen Percepciones/Retenciones y OTROS NO ESTA DEFINIDO"
						filtro1 = " (!EMPTY(d5cpot) AND d5tipo = 'V') "
			ELSE
				mensaje = mensaje + CHR(13) + CHR(13) + "En Ventas existen Percepciones/Retenciones y OTROS NO ESTA DEFINIDO"
				filtro1 = filtro1 + " OR (!EMPTY(d5cpot) AND d5tipo = 'V') "
			ENDIF
ENDIF

*======================================
* COMPRAS
pc_pr	= "f00.p0prc"

COUNT FOR !EMPTY(d5cpib) AND d5tipo = "C"			TO a
FOR pn_n = 1 TO 3
		pc_pr	= SUBSTR(pc_pr,1,9) + STR(pn_n,1)
		IF &pc_pr = 1
				pn_n = pn_n + 4
		ENDIF
ENDFOR
IF a <> 0	AND pn_n = 4
			IF EMPTY(mensaje)
						mensaje = "En Compras existen Percepciones/Retenciones e INGRESOS BRUTOS NO ESTA DEFINIDO"
						filtro1 = " (!EMPTY(d5cpib) AND d5tipo = 'C') "
			ELSE
				mensaje = mensaje + CHR(13) + CHR(13) + "En Compras existen Percepciones/Retenciones e INGRESOS BRUTOS NO ESTA DEFINIDO"
				filtro1 = filtro1 + " OR (!EMPTY(d5cpib) AND d5tipo = 'C') "
			ENDIF
ENDIF

COUNT FOR !EMPTY(d5cpga) AND d5tipo = "C"			TO a
FOR pn_n = 1 TO 3
		pc_pr	= SUBSTR(pc_pr,1,9) + STR(pn_n,1)
		IF &pc_pr = 2
				pn_n = pn_n + 4
		ENDIF
ENDFOR
IF a <> 0	AND pn_n = 4
			IF EMPTY(mensaje)
						mensaje = "En Compras existen Percepciones/Retenciones y GANANCIAS NO ESTA DEFINIDO"
						filtro1 = " (!EMPTY(d5cpga) AND d5tipo = 'C') "
			ELSE
				mensaje = mensaje + CHR(13) + CHR(13) + "En Compras existen Percepciones/Retenciones y GANANCIAS NO ESTA DEFINIDO"
				filtro1 = filtro1 + " OR (!EMPTY(d5cpga) AND d5tipo = 'C') "
			ENDIF
ENDIF

COUNT FOR !EMPTY(d5cpot) AND d5tipo = "C"			TO a
FOR pn_n = 1 TO 3
		pc_pr	= SUBSTR(pc_pr,1,9) + STR(pn_n,1)
		IF &pc_pr = 3
				pn_n = pn_n + 4
		ENDIF
ENDFOR
IF a <> 0	AND pn_n = 4
			IF EMPTY(mensaje)
						mensaje = "En Compras existen Percepciones/Retenciones y OTROS NO ESTA DEFINIDO"
						filtro1 = " (!EMPTY(d5cpot) AND d5tipo = 'C') "
			ELSE
				mensaje = mensaje + CHR(13) + CHR(13) + "En Compras existen Percepciones/Retenciones y OTROS NO ESTA DEFINIDO"
				filtro1 = filtro1 + " OR (!EMPTY(d5cpot) AND d5tipo = 'C') "
			ENDIF
ENDIF

IF !EMPTY(mensaje)
*			w_resul	=	sino(mensaje + CHR(13) + CHR(13) + "Desea seguir con la reconstrucci๓n de saldos ?","NO", "Reconstrucci๓n de saldos")
*			IF w_resul = "N"
						mensaje = mensaje + CHR(13) + CHR(13) + "RECONSTRUCCION DE SALDOS CANCELADA"
							=ADVGRAVE(mensaje)
							filtrado = filtro1
							DO listado
*						=CLOSALL()
*						RETURN
*			ENDIF
ENDIF

*============================================
* INCONSISTENCIAS DE DATOS

filtro1		=	"EMPTY(d5cond) OR EMPTY(d5coco) OR "

filtro2		=	"(ABS(d5imre) < ABS(d5tota)) OR d5tota = 0 OR "	

filtro3		=	"( EMPTY(d5pere) AND EMPTY(d5cpib) AND EMPTY(d5cpga) AND EMPTY(d5cpot) ) OR "

filtro4		=	"( EMPTY(d5juri) OR !SEEK('JU' + d5juri,'f01') ) "

filtro5		=	")"
			  
*============================================
filtro		=	"(d5tipo = 'V' AND d5tico = 'R') AND ( ( EMPTY(d5rubr) OR !SEEK('RP' + d5rubr,'f01') ) OR "
filtrado	=	filtro + filtro1 + filtro2 + filtro3 + filtro4 + filtro5

COUNT	TO w_cantv	FOR &filtrado

IF w_cantv <> 0
		=ADVGRAVE("EXISTEN RETENCIONES DE VENTAS INCONSISTENTES, POR FAVOR VERIFIQUE y CORRIJA";
						+CHR(13) + CHR(13) + "RECONSTRUCCION DE SALDOS CANCELADA")
		DO listado
ENDIF

*==============================================================================================
* COMPRAS
filtro		=	"(d5tipo = 'C' AND d5tico = 'R') AND ( ( EMPTY(d5rubr) OR !SEEK('RS' + d5rubr,'f01') ) OR "
filtrado	=	filtro + filtro1 + filtro2 + filtro3 + filtro4 + filtro5

COUNT	TO w_cantc	FOR &filtrado

IF w_cantc <> 0
		=ADVGRAVE("EXISTEN RETENCIONES DE COMPRAS INCONSISTENTES, POR FAVOR VERIFIQUE y CORRIJA";
						+CHR(13) + CHR(13) + "RECONSTRUCCION DE SALDOS CANCELADA")
		DO listado
ENDIF

RETURN

*=====================================================================================
PROCEDURE comprobante

SELE f04

IF f05.d5tipo	= "V"
				w_imp	= "d4vim"
ELSE
	w_imp	= "d4cim"
ENDIF

w_f05imp	= "f05.d5imp"

IF f05.d5tipo	= "V"
				REPLACE d4vtot WITH d4vtot + f05.d5tota
ELSE
	REPLACE d4ctot WITH d4ctot + f05.d5tota
ENDIF

FOR n = 1 TO 8
			w_imp		= SUBSTR(w_imp,1,5) + STR(n,1)
			w_f05imp	= SUBSTR(w_f05imp,1,9) + STR(n,1)
			REPLACE &w_imp WITH &w_imp + &w_f05imp
ENDFOR

RETURN

*=====================================================================================
PROCEDURE listado

_pfile='900r'

INDEX ON d5tipo + DTOS(d5fech) + d5nume	TO ivf099b.idx
GO TOP

DO getdest.spr


RETURN

***vfp***
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons900r
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	on key label f9 do cons900r0
	keyboard '{f9}'
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons900r0
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
on key label f9
***vfp***


do repdest &&(Setea variables internas del reporte)

if !empty(p_repclaus) &&(debe ser preview o to printer)
			p_report	= 'iwr900rl'
			p_repfor	= filtrado
			=rep_form()
			p_report	= ''
			p_repfor	= ''
endif

do represet &&(Resetea estado)

KEYB CHR(27)

RETURN

*===============================================================================

*m.d5leye = m.d5clie + " " + readf('f02',m.d5clie,'d2nomb',20) + ' ' + tcabre(m.d5tico) + ' '

*m.d5leye = m.d5leye + m.d5tifo + m.d5sucu + ' - ' + m.d5nume

REPORT FORM iwr900rl