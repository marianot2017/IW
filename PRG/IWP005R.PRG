*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : IWP005R.PRG -> Listador subdiarios por Rubro
*)-H Release		 : 08
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

*w_pfile='05r'
	
w_alias='f05'
w_title='IMPRESION DE SUBDIARIOS POR RUBRO'
w_imco =imco()
w_orde	= 1						&& R.03 Ra๚l

*) Apertura de archivos--------------------------------------------------------

fieldvec[1]='F00'
fieldvec[2]='F01'
fieldvec[3]='F02'
fieldvec[4]='F03'
fieldvec[5]='F04'
fieldvec[6]='F05'
fieldvec[7]='F06'

use (d0f00) in 0 alias 'f00' 				     &&(Parametros)
use (d0f01) in 0 alias 'f01' order 'd1tip'  &&(Rv,Rc)
use (d0f01) in 0 AGAIN alias 'f01v' order 'd1tip'  &&(Rv,Rc)   && RUBROS VENTAS

&& R.02b Ra๚l
use (d0f01) in 0 AGAIN alias 'f01p' order 'd1tip'  &&	RP - RUBROS RT Practicadas
use (d0f01) in 0 AGAIN alias 'f01s' order 'd1tip'  &&	RS - RUBROS RT Sufridas
&& R.02e Ra๚l

use (d0f02) in 0 alias 'f02' order 'd2clie' &&(Clientes)
use (d0f03) in 0 alias 'f03' order 'd3prov' &&(Proveedores)
use (d0f04) in 0 alias 'f04' order 'd4peri' &&(Periodos)
use (d0f05) in 0 alias 'f05' order 'd5pefe' &&(Comprobantes)
use (d0f06) in 0 alias 'f06' order 'd6inte' &&(Nombres alternativos)

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

wdsucu	= "     "	&& R.08 Mariano (agrande 1 digito)
whsucu	= "99999"	&& R.08 Mariano (agrande 1 digito)

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

w_icai	= 'S'

w_resu=.t.
w_icai	= "N"

t_leye1	= 'Tipo Comprobantes:FA=Fact,CR=Cr้d,DB=D้bito,TI=Tick,RE=Rec,FC=Fact.Cr้d,RF=Rec-Fact,TF=Tick-Fact,TE=Tick-NC,TD=Tick-ND,LV=Liq-Vs.,OT='+allt(w_leot)+",RT=Ret."	&& R.06 Mariano (agregue ticket-NC y Ticket-ND)	&& R.07 Mariano (agregue liq.varias)
*)-----------------------------------------------------------------------------

do ssm998.mpr
do iws005r.spr

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
	dele file &d0f99.c.dbf	&& R.01 Mariano
endif
*dele file f99c.dbf		&& R.01 Mariano
*dele file f99c.idx		&& R.01 Mariano

sele 0

select * ;
	from f05 ;
		where	(d5tipo = "V" OR d5tipo = "C")										AND	;
				BETWEEN (PTOI(d5peri), PTOI(wdperi), PTOI(whperi))					AND ;	
				between(d5rubr,wdrubr,whrubr)										AND	;
				between(d5fech,wdfech,whfech)										AND ;
				between(d5sucu,wdsucu,whsucu)											;
		into dbf &d0f99.c	&& R.01 Mariano
use &d0f99.c alias f99c exclusive	&& R.01 Mariano
*		into DBF f99c	&& R.01 Mariano
*		ORDER BY PTOI(d5peri), d5rubr, d5fech, d5clie, d5sucu, d5tico, d5tifo, d5nume

SELE f99c

REPLA ALL d5tipo	WITH "Y"		FOR d5tipo = "V"	AND d5tico = "R"
REPLA ALL d5tipo	WITH "X"		FOR d5tipo = "C"	AND d5tico = "R"

DO agruasto

&&b R.05 Ra๚l
*INDEX ON   d5rubr + PTOI(d5peri) + d5tipo + DTOS(d5fech) +					
*			d5sucu + d5tico + d5tifo + d5nume		tag f99c	&& R.01 Mariano	(TO f99c)
		
INDEX ON   d5tipo + d5rubr + PTOI(d5peri) + d5tipo + DTOS(d5fech) +	;		
			d5sucu + d5tico + d5tifo + d5nume		tag f99c	&& R.01 Mariano	(TO f99c)
&&e R.05 Ra๚l

set rela to 'RC'+ d5rubr into f01
set rela to 'RV'+ d5rubr into f01v		ADDITIVE

&& R.02b Ra๚l
set rela to 'RP'+ d5rubr into f01p		ADDITIVE
set rela to 'RS'+ d5rubr into f01s		ADDITIVE
&& R.02e Ra๚l

p_repdos=67

w_print = .F.

t_leye = 'Rubro : De ' + wdrubr + ' a ' + whrubr + ', Perํodo : De ' + wdperi + ' a ' + whperi +	;
		 ', Desde el : ' + dtoc(wdfech) + ' al : ' + dtoc(whfech) + ', Sucursal : De ' + wdsucu + ' a ' + whsucu

IF w_vta
		w_reptit='SUBDIARIO DE VENTAS POR RUBRO'
		if !p_cancela
					return
		endif
		w_oc1=f00->p0ocv1
		w_oc2=f00->p0ocv2
		w_oc3=f00->p0ocv3

		SELE f99c
		IF w_imre = "S"
					SET FILTER TO d5tipo = "V"		OR d5tipo = "Y"
					w_a	= 1
		ELSE
			SET FILTER TO d5tipo = "V"
			w_a	= 0
		ENDIF

		GO TOP
		IF EOF()
				WAIT WIND "VENTAS - Sin movimientos para informar"
		ELSE				
*			p_report='iwr005rv'
&& R.03b Ra๚l
*			p_report='iwr005r'
			IF w_orde = 1
						p_report='iwr005rr'				&& Resumido
			ELSE
				p_report='iwr005r'
			ENDIF
&& R.03e Ra๚l
		
			DO resacta			&& R.04 Ra๚l
		
			DO listalib WITH "RV", .T.
			w_print	= .T.

			&& R.05b Mariano
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
			&& R.05e Mariano
		ENDIF
ENDIF

if w_cpra
		w_reptit='SUBDIARIO DE COMPRAS POR RUBRO'
		if !p_cancela
					return
		endif
		w_oc1=f00->p0occ1
		w_oc2=f00->p0occ2
		w_oc3=f00->p0occ3

		SELE f99c

		IF w_imre = "S"
					SET FILTER TO d5tipo = "C"		OR d5tipo = "X"
					w_a	= 1
		ELSE
			SET FILTER TO d5tipo = "C"
			w_a	= 0
		ENDIF

		GO TOP
		IF EOF()
				WAIT WIND "COMPRAS - Sin movimientos para informar"
		ELSE				
*			p_report='iwr005rc'
&& R.03b Ra๚l
*			p_report='iwr005r'
			IF w_orde = 1
						p_report='iwr005rr'				&& Resumido
			ELSE
				p_report='iwr005r'
			ENDIF
&& R.03e Ra๚l
		
			DO resacta			&& R.04 Ra๚l
		
			DO listalib WITH "RC", .T.
			w_print	= .T.

			&& R.05b Mariano
			IF w_print
				do represet &&(Resetea estado)
			ENDIF
			&& R.05e Mariano
		ENDIF
ENDIF

&& R.05b Mariano
*IF w_print
*		do represet &&(Resetea estado)
*ENDIF
&& R.09e Mariano

SELE f99c
USE
*dele file f99c.dbf		&& R.01 Mariano
*dele file f99c.idx		&& R.01 Mariano
dele file &d0f99.c.dbf	&& R.01 Mariano
dele file &d0f99.c.cdx	&& R.01 Mariano

return

*================================================================================
&& R.04b Ra๚l

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
*===> RESUMIDO / SIN RETENCIONES / SIN CONTABILIDAD
IF w_orde = 1		AND !w_imco		AND w_imre = 'N'												
		DELE ALL FOR LEFT(uniqueid, 6) = "_1VG0Y"							&& d5tipo GROUP HEADER Y FOOTER
																			&& idem prefijo en compras y ventas
		DELE ALL FOR LEFT(uniqueid, 6) = "_1WJ0W"							&& campos del FOOTER
		DELE ALL FOR LEFT(uniqueid, 6) = "_1WJ0TYQJQ"
				
		DELE ALL FOR "w_imre = 'S'" $ supexpr	AND platform = 'WINDOWS'	&& BORRO valores de d5tipo
		DELE ALL FOR 'w_imco' $ supexpr	AND platform = 'WINDOWS'			&& BORRO Imputaci๓n Contable

		REPLA height 		WITH 3334				FOR uniqueid = '_S9Y117GHX'
		REPLA ALL supexpr	WITH ''					FOR '<>0'	$ supexpr
		REPLA resettotal 	WITH resettotal - 1 	FOR resettotal > 6

		REPLA ALL vpos 		WITH vpos - 3645.833	FOR 25625.000 <= vpos	AND vpos < 55520.833
		REPLA ALL vpos 		WITH vpos - 12604.166	FOR 55520.833 <= vpos	
ENDIF

*============================================================================================
*===> RESUMIDO / CON RETENCIONES / SIN CONTABILIDAD
IF w_orde = 1		AND !w_imco		AND w_imre = 'S'												

		DELE ALL FOR 'w_imco' $ supexpr	AND platform = 'WINDOWS'			&& BORRO Imputaci๓n Contable

		REPLA height 			WITH 3334				FOR uniqueid = '_S9Y117GHX'
		REPLA ALL supexpr		WITH ''					FOR '<>0'	$ supexpr
		REPLA ALL supvalchng	WITH .F.				FOR 'w_a <> 0'	$ supexpr
		REPLA ALL norepeat		WITH .F.				FOR 'w_a <> 0'	$ supexpr

		REPLA ALL vpos 			WITH vpos - 3541.667	FOR 47916.667 <= vpos	
ENDIF

*============================================================================================
*===> RESUMIDO / SIN RETENCIONES / CON CONTABILIDAD
IF w_orde = 1		AND w_imco		AND w_imre = 'N'												
		DELE ALL FOR LEFT(uniqueid, 6) = "_1VG0Y"							&& d5tipo GROUP HEADER Y FOOTER
		DELE ALL FOR LEFT(uniqueid, 6) = "_1WJ0W"							&& campos del FOOTER
		DELE ALL FOR LEFT(uniqueid, 6) = "_1WJ0TYQJQ"						&& 'Comprobantes : ' / 'Retenciones :'
				
		DELE ALL FOR "w_imre = 'S'" $ supexpr	AND platform = 'WINDOWS'	&& BORRO valores de d5tipo

		REPLA ALL supexpr	WITH ''					FOR '<>0'	$ supexpr
		REPLA resettotal 	WITH resettotal - 1 	FOR resettotal > 6

		REPLA ALL vpos 		WITH vpos - 3645.833	FOR 25625.000 <= vpos	AND vpos < 55520.833
		REPLA ALL vpos 		WITH vpos - 9062.500	FOR 55520.833 <= vpos	
ENDIF

*============================================================================================
*===> RESUMIDO / CON RETENCIONES / CON CONTABILIDAD
IF w_orde = 1		AND w_imco		AND w_imre = 'S'												
		REPLA ALL supexpr		WITH ''					FOR '<>0'	$ supexpr
		REPLA ALL supvalchng	WITH .F.				FOR 'w_a <> 0'	$ supexpr
		REPLA ALL norepeat		WITH .F.				FOR 'w_a <> 0'	$ supexpr
ENDIF

*============================================================================================
*===> DETALLADO / SIN RETENCIONES / SIN CONTABILIDAD
IF w_orde = 2		AND !w_imco		AND w_imre = 'N'												
		DELE ALL FOR LEFT(uniqueid, 6) = "_1VG0Y"							&& d5tipo GROUP HEADER Y FOOTER
																			&& idem prefijo en compras y ventas
		DELE ALL FOR LEFT(uniqueid, 6) = "_1WJ0W"							&& campos del FOOTER
		DELE ALL FOR LEFT(uniqueid, 6) = "_1WJ0TYQJQ"
				
		DELE ALL FOR "w_imre = 'S'" $ supexpr	AND platform = 'WINDOWS'	&& BORRO valores de d5tipo
		DELE ALL FOR 'w_imco' $ supexpr	AND platform = 'WINDOWS'			&& BORRO Imputaci๓n Contable

		REPLA height 		WITH 3334				FOR uniqueid = '_S9Y117GHX'
		REPLA ALL supexpr	WITH ''					FOR '<>0'	$ supexpr	AND  vpos <> 34479.167
		REPLA resettotal 	WITH resettotal - 1 	FOR resettotal > 6

		REPLA ALL vpos 		WITH vpos - 3645.833	FOR 25625.000 <= vpos	AND vpos < 60208		&& 60208.833 es la diferencia pero pincha
		REPLA ALL vpos 		WITH vpos - 12604.166	FOR 60208 <= vpos	
ENDIF

*============================================================================================
*===> DETALLADO / CON RETENCIONES / SIN CONTABILIDAD
IF w_orde = 2		AND !w_imco		AND w_imre = 'S'												

		DELE ALL FOR 'w_imco' $ supexpr	AND platform = 'WINDOWS'			&& BORRO Imputaci๓n Contable

		REPLA height 			WITH 3334				FOR uniqueid = '_S9Y117GHX'
		REPLA ALL supexpr		WITH ''					FOR '<>0'	$ supexpr	AND  vpos <> 34479.167
		REPLA ALL supvalchng	WITH .F.				FOR 'w_a <> 0'	$ supexpr
		REPLA ALL norepeat		WITH .F.				FOR 'w_a <> 0'	$ supexpr

		REPLA ALL vpos 			WITH vpos - 3541.667	FOR 47916.667 <= vpos	
ENDIF

*============================================================================================
*===> DETALLADO / SIN RETENCIONES / CON CONTABILIDAD
IF w_orde = 2		AND w_imco		AND w_imre = 'N'												
		DELE ALL FOR LEFT(uniqueid, 6) = "_1VG0Y"							&& d5tipo GROUP HEADER Y FOOTER
		DELE ALL FOR LEFT(uniqueid, 6) = "_1WJ0W"							&& campos del FOOTER
		DELE ALL FOR LEFT(uniqueid, 6) = "_1WJ0TYQJQ"						&& 'Comprobantes : ' / 'Retenciones :'
				
		DELE ALL FOR "w_imre = 'S'" $ supexpr	AND platform = 'WINDOWS'	&& BORRO valores de d5tipo

		REPLA ALL supexpr	WITH ''					FOR '<>0'	$ supexpr	AND  vpos <> 34479.167
		REPLA resettotal 	WITH resettotal - 1 	FOR resettotal > 6

		REPLA ALL vpos 		WITH vpos - 3645.833	FOR 25625.000 <= vpos	AND vpos < 55520.833
		REPLA ALL vpos 		WITH vpos - 9062.500	FOR 55520.833 <= vpos	
ENDIF

*============================================================================================
*===> DETALLADO / CON RETENCIONES / CON CONTABILIDAD
IF w_orde = 2		AND w_imco		AND w_imre = 'S'												
		REPLA ALL supexpr		WITH ''					FOR '<>0'	$ supexpr	AND  vpos <> 34479.167
		REPLA ALL supvalchng	WITH .F.				FOR 'w_a <> 0'	$ supexpr
		REPLA ALL norepeat		WITH .F.				FOR 'w_a <> 0'	$ supexpr
ENDIF



PACK
use
p_report =	d0f99 

SELE f99c

return

&& R.04e Ra๚l

*================================================================================

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*) Puesto adrede para que incluya el report al proyecto
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*report form iwr005rv
*report form iwr005rc

report form iwr005r
report form iwr005rr