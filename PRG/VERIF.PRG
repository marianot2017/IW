push key clear		  &&(Salva estados de teclas)
push menu _msysmenu &&(Salva estado de menu)
set skip of menu _msysmenu .t.

w_pfile	= '999'
w_prog	= 'VERIF'
p_user	= ' '
w_title	= 'Actualización Versión 412.25.35 - Inconsistencias en PARAMETROS DE EMPRESA'
w_time	= TIME()
w_mensa	= ''

DO ortiva	WITH 'VERIFICACION'



IF !EMPTY(w_mensa)
			w_resul	=	SINO("Desea ver el resultado de la verificación de datos de empresas  ?","SI", "Actualización de versión")
			IF w_resul = "S"
						IF !USED('SSF090')
										USE ssf090 IN 0 ALIAS ssf090
						ENDIF
						DO ssm998.mpr 
						DO verif2.spr
			ENDIF
ELSE
	WAIT WIND 'PARAMETROS EMPRESA VERIFICADOS CORRECTAMENTE'
ENDIF

pop key
pop menu _msysmenu

RETURN

*======================================================================
PROCEDURE cons999

SELE ssf090
SET FILTER TO errdate = DATE()	AND errtime => w_time AND UPPER(ALLT(errdata)) = 'VERIFICACION'

p_report	= 'verif'

w_print		= .F.

DO listalib

DO represet

RETURN

