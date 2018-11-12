*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : IWP005J.PRG -> Listador subdiarios por Jurisdiccion
*)-H Release		 : 04
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

w_prog =upper(prompt())
w_alta =.f.

*w_pfile='05j'

w_alias='f05'
w_title='SUBDIARIO POR JURISDICCION'

*) Apertura de archivos--------------------------------------------------------

fieldvec[1]='F00'
fieldvec[2]='F01'
fieldvec[3]='F02'
fieldvec[4]='F03'
fieldvec[5]='F04'
fieldvec[6]='F05'
fieldvec[7]='F06'

use (d0f00) in 0 alias 'f00' 				     		 &&(Parametros)
use (d0f01) in 0 alias 'f01' order 'd1tip'  		 &&(Tablas)
use (d0f02) in 0 alias 'f02' order 'd2clie' 		 &&(Clientes)
use (d0f03) in 0 alias 'f03' order 'd3prov' 		 &&(Proveedores)
use (d0f04) in 0 alias 'f04' order 'd4peri'		 &&(Periodos)
use (d0f05) in 0 alias 'f05' order 'd5pefe' excl &&(Comprobantes)
use (d0f06) in 0 alias 'f06' order 'd6inte'		 &&(Nombres alternativos)

if neterr()
	pop key
	pop menu _msysmenu
	fieldvec=''
	=closall()
	return
endif

fieldvec=''

*) Variables-------------------------------------------------------------------

warch	= " "
w_dir	= " "
w_name	= " "
w_cuit	= SPACE(13)

w_pfile	= "subd"
w_while = " "

go bottom in f04
wdperi	=	f04.d4peri
whperi	=	f04.d4peri

w_vta	= .F.
w_cpra	= .F.

wdfech	= {}

w_orde	= 1					&& R.02 Ra๚l

DO CASE
		CASE !EMPTY(CTOD('31/'+whperi))
						whFech	= CTOD('31/'+whperi)
		CASE !EMPTY(CTOD('30/'+whperi))
						whFech	= CTOD('30/'+whperi)
		CASE !EMPTY(CTOD('29/'+whperi))
						whFech	= CTOD('29/'+whperi)
		OTHERWISE
				whFech	= CTOD('28/'+whperi)
ENDCASE

wlfech	= whfech
wxfech	= whfech

wdclie	= SPACE(5)
whclie	= "99999"

w_dcli	= SPACE(35)
w_hcli	= REPL('Z',35)

wdrubr	= "      "
whrubr	= "999999"

wdsucu	= "     "	&& R.04 Mariano (Agrande 1 digito)
whsucu	= "99999"	&& R.04 Mariano (Agrande 1 digito)

wdjuri	= " "
whjuri	= "Z"

store 0 to w4hove,w4ulve,w4hoco,w4ulco

w_imre	= "N"

wtalic='N'

t_leye=''

w_leot=f00.p0leot

w_iva	= 0
w_ib	= 0
w_gan	= 0
w_ot	= 0
w_imp	= " "

w_resu=.t.

w_icai	= 'N'

w_paso=.f.

*)-----------------------------------------------------------------------------

do ssm998.mpr
do iws005j.spr

pop key
pop menu _msysmenu
=closall()

*dele file f99c.dbf		&& R.01 Mariano
*dele file f99c.idx		&& R.01 Mariano
dele file &d0f99.c.dbf	&& R.01 Mariano
dele file &d0f99.c.cdx	&& R.01 Mariano

RELE ALL LIKE w*

return

***vfp***
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc conssubd
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	on key label f9 do conssubd0
	keyboard '{f9}'
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc conssubd0
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
on key label f9
***vfp***

if .not. validsubd(.t.)
	wait wind 'Verifique los datos ingresados...'
	return
endif

if !w_vta and !w_cpra
	wait window 'Debe elegir Ventas y/o Compras'
	return
endif

=gentmp()

if used('f99c')
	sele f99c
	use
*	dele file f99c.dbf	&& R.01 Mariano
	dele file &d0f99.c.dbf	&& R.01 Mariano
endif

sele 0

select * ;
	from f05 ;
		where	(d5tipo = "V" OR d5tipo = "C")										AND	;
				BETWEEN (PTOI(d5peri), PTOI(wdperi), PTOI(whperi))					AND ;	
				between(d5rubr,wdrubr,whrubr)							   			AND	;
				between(d5juri,wdjuri,whjuri)										AND ;
				between(d5sucu,wdsucu,whsucu)											;
		into dbf &d0f99.c	&& R.01 Mariano
use &d0f99.c alias f99c exclusive	&& R.01 Mariano
*		into DBF f99c	&& R.01 Mariano
*		ORDER BY d5juri, PTOI(d5peri), d5fech, d5clie, d5sucu, d5tico, d5tifo, d5nume

SELE f99c

REPLA ALL d5tipo	WITH "Y"		FOR d5tipo = "V"	AND d5tico = "R"
REPLA ALL d5tipo	WITH "X"		FOR d5tipo = "C"	AND d5tico = "R"

DO agruasto

&& R.03b Ra๚l
IF w_orde = 1
			INDEX ON d5juri + d5tipo + d5peri   		tag f99c	

ELSE
	INDEX ON d5juri + d5tipo + PTOI(d5peri) +  DTOS(d5fech) +								;
			d5sucu + d5tico + d5tifo + d5nume	tag f99c	&& R.01 Mariano	(TO f99c)
ENDIF
&& R.03e Ra๚l

SET RELA TO
SET RELATION TO d5clie INTO f02 ADDITIVE
SET RELATION TO d5clie INTO f03 ADDITIVE

p_repdos=56

w_print = .F.

SELE f99c

t_leye='Jurisdicci๓n: De '+wdjuri+' a '+whjuri+;
	   ', Rubro: De '+wdrubr+' a '+whrubr+' Perํodo: De '+wdperi+' a '+whperi+' Sucursal: De '+wdsucu+' a '+whsucu

IF w_vta
		w_reptit='SUBDIARIO DE VENTAS POR JURISDICCION'
		if !p_cancela
					return
		endif

		w_oc1 = f00->p0ocv1
		w_oc2 = f00->p0ocv2
		w_oc3 = f00->p0ocv3

		IF w_imre = "S"
					SET FILTER TO	d5tipo = "V"	OR		d5tipo = "Y"	
					w_a		= 1
		ELSE
			SET FILTER TO d5tipo = "V"
			w_a		= 0
		ENDIF
		
		GO TOP
		IF EOF()
				WAIT WIND "VENTAS - Sin movimientos para informar"
		ELSE				
			w_paso=.f.
*			p_report='iwr005jv'
&& R.02b Ra๚l
*			p_report='iwr005j'
			IF w_orde = 1
						p_report='iwr005jr'				&& Resumido
			ELSE
				p_report='iwr005j'
			ENDIF
&& R.02e Ra๚l
		
			set rela to 'JU'+d5juri into f01
		
			DO resacta								&& R.02 Ra๚l
		
			DO listalib WITH "JV", .T.
			w_print	= .T.

			&& R.01b Mariano
			w_prepout=p_repout
			w_preptype=p_reptype	
			IF w_print
				do represet &&(Resetea estado)
			ENDIF
			** Para que pida el nombre del archivo de compras
			if w_cpra
				w_print=.f.
				p_repout=w_prepout
				p_reptype=w_preptype
			endif
			&& R.01e Mariano
		ENDIF
ENDIF

if w_cpra
		w_reptit='SUBDIARIO DE COMPRAS POR JURISDICCION'
		if !p_cancela
					return
		endif
		SELE f99c

		w_oc1=f00->p0occ1
		w_oc2=f00->p0occ2
		w_oc3=f00->p0occ3

		w_a		= 0
		IF w_imre = "S"
*					SET FILTER TO	d5tipo = "C"	&& R.03 Mariano
					SET FILTER TO	d5tipo = "C"	OR		d5tipo = "X"	&& R.03 Mariano
					w_a	= 1
		ELSE
*			SET FILTER TO d5tipo = "C" AND d5tico <> "R"	&& R.03 Mariano
			SET FILTER TO d5tipo = "C" 	&& R.03 Mariano
			w_a	= 0
		ENDIF
		
		GO TOP

		IF EOF()
				WAIT WIND "COMPRAS - Sin movimientos para informar"
		ELSE				
			w_paso=.f.
*			p_report='iwr005jc'
&& R.02b Ra๚l
*			p_report='iwr005j'
			IF w_orde = 1
						p_report='iwr005jr'				&& Resumido
			ELSE
				p_report='iwr005j'
			ENDIF
&& R.02e Ra๚l

			set rela to 'JU'+d5juri into f01
		
			DO resacta								&& R.02 Ra๚l
		
			DO listalib WITH "JC", .T.
			w_print	= .T.

			&& R.01b Mariano
			IF w_print
				do represet &&(Resetea estado)
			ENDIF
			&& R.01e Mariano
		ENDIF
ENDIF

&& R.01b Mariano
*IF w_print
*		do represet &&(Resetea estado)
*ENDIF
&& R.01e Mariano

return

*=========================================================================================

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC pasa
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
w_paso=.f.

return ''

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC paso
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
w_paso=.t.

return ''


*================================================================================
&& R.02b Ra๚l

PROCEDURE resacta
PRIVATE w_base

IF LEFT(p_repout,1)		<> '*'
					RETURN
ENDIF

w_base='report\'+p_report+'.frx'

use &w_base in 0 alias freport         && Reporte

set safety off

sele freport
copy to &d0f99..frx
use 

use &d0f99..frx in 0 alias frpt EXCL

sele frpt

*============================================================================================
*===> RESUMIDO / SIN RETENCIONES
IF w_orde = 1		AND w_imre = 'N'												
		DELE ALL FOR LEFT(uniqueid, 6) = "_1VX0P"							&& d5tipo GROUP HEADER Y FOOTER

		DELE ALL FOR LEFT(uniqueid, 6) = "_1WI0X"							&& campos del FOOTER
				                          
		DELE ALL FOR "w_imre = 'S'" $ supexpr	AND platform = 'WINDOWS'	&& BORRO valores de d5tipo

		REPLA height 		WITH 3438				FOR uniqueid = '_S9Y0T6EIK'
		REPLA ALL supexpr	WITH ''					FOR '<>0'	$ supexpr
		REPLA ALL vpos 		WITH vpos - 3645.833	FOR 35937.500 <= vpos	AND vpos < 47395.833
		REPLA ALL vpos 		WITH vpos - 9583.333	FOR 47395.833 <= vpos	
		REPLA resettotal 	WITH 7 	FOR resettotal = 8

ENDIF

*============================================================================================
*===> RESUMIDO / CON RETENCIONES
IF w_orde = 1		AND w_imre = 'S'												

		REPLA height 			WITH 3438				FOR uniqueid = '_S9Y0T6EIK'
		REPLA ALL supexpr		WITH ''					FOR '<>0'		$ supexpr
		REPLA ALL supvalchng	WITH .F.				FOR 'w_a <> 0'	$ supexpr
		REPLA ALL norepeat		WITH .F.				FOR 'w_a <> 0'	$ supexpr
ENDIF

*============================================================================================
*===> DETALLADO / SIN RETENCIONES
IF w_orde = 2		AND w_imre = 'N'												

		DELE ALL FOR LEFT(uniqueid, 6) = "_1VX0P"							&& d5tipo GROUP HEADER Y FOOTER
																			&& idem prefijo en compras y ventas

		DELE ALL FOR LEFT(uniqueid, 6) = "_1WI0X"							&& campos del FOOTER

				
		DELE ALL FOR "w_imre = 'S'" $ supexpr								&& BORRO valores de d5tipo

		REPLA height 		WITH 3438				FOR uniqueid = '_S9Y0T6EIK'
		REPLA ALL supexpr	WITH ''					FOR '<>0'	$ supexpr	
		REPLA resettotal 	WITH 7				 	FOR resettotal = 8

		REPLA ALL vpos 		WITH vpos - 3645.833	FOR 31666.667 <= vpos	AND vpos < 52083.333		&& 60208.833 es la diferencia pero pincha
		REPLA ALL vpos 		WITH vpos - 9583.333	FOR 52083.333 <= vpos	

ENDIF

*============================================================================================
*===> DETALLADO / CON RETENCIONES
IF w_orde = 2		AND w_imre = 'S'												

		REPLA height 			WITH 3438				FOR uniqueid = '_S9Y0T6EIK'
		REPLA ALL supexpr		WITH ''					FOR '<>0'		$ supexpr
		REPLA ALL supvalchng	WITH .F.				FOR 'w_a <> 0'	$ supexpr
		REPLA ALL norepeat		WITH .F.				FOR 'w_a <> 0'	$ supexpr
ENDIF

*============================================================================================

PACK
use
p_report =	d0f99 

SELE f99c

return

&& R.02e Ra๚l

*================================================================================
* Puesto adrede para que incluya el report al proyecto

*report form iwr005jv
*report form iwr005jc

report form iwr005j
report form iwr005jr