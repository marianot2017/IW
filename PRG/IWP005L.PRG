*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : IWP005L.PRG -> Listador subdiarios por cliente
*)-H Release		 : 03
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

*w_pfile='05l'

w_alias='f05'
w_title='SUBDIARIOS DE VENTAS POR CLIENTE'
w_imco=imco()

*) Apertura de archivos--------------------------------------------------------

fieldvec[1]='F00'
fieldvec[2]='F01'
fieldvec[3]='F02'
fieldvec[4]='F03'
fieldvec[5]='F04'
fieldvec[6]='F05'
fieldvec[7]='F06'
fieldvec[8]='F20'
fieldvec[9]='F01c'

use (d0f00) in 0 alias 'f00' 				      &&(Parametros)
use (d0f01) in 0 alias 'f01' order 'd1tip'   &&(Tablas)
use (d0f02) in 0 alias 'f02' order 'd2clie'  &&(Clientes)
use (d0f03) in 0 alias 'f03' order 'd3prov'  &&(Clientes)
use (d0f04) in 0 alias 'f04' order 'd4peri'  &&(Periodos)
use (d0f05) in 0 alias 'f05' order 'd5peri'  &&(Comprobantes)
use (d0f06) in 0 alias 'f06' order 'd6inte'  &&(Nombres alternativos)
use (d0f20) in 0 alias 'f20' order 'd20tipo' &&(Tipos impos. x periodos)
if w_imco		
	ncf01=w0fipre+'01'
	use (ncf01) in 0 alias f01c order tag 'd1cuen'
endif

if neterr()
	pop key
	pop menu _msysmenu
	fieldvec=''
	=closall()
	return
endif

fieldvec=''

=creatipos('C')

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

w_vta	= .T.
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

wdsucu	= "    "
whsucu	= "9999"

wdjuri	= " "
whjuri	= "Z"

store 0 to w4hove,w4ulve,w4hoco,w4ulco

w_imre	= "N"

wtalic='N'

t_leye=''

w_ocv1=f00->p0ocv1
w_ocv2=f00->p0ocv2
w_ocv3=f00->p0ocv3

w_occ1=f00->p0occ1
w_occ2=f00->p0occ2
w_occ3=f00->p0occ3

w_leot=f00.p0leot

w_iva	= 0
w_ib	= 0
w_gan	= 0
w_ot	= 0
w_imp	= " "

w_resu=.t.

w_icai	= 'N'


*)-----------------------------------------------------------------------------

do ssm998.mpr
do iws005l.spr

pop key
pop menu _msysmenu
=closall()

=borratipos()

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

*===> Imprime RETENCIONES ??
IF w_imre	= "S"
		w_cond = '( d5tipo = "V" )	AND '
		w_a		=	1
ELSE
	w_cond	=	'(d5tipo = "V" AND d5tico <> "R") AND '
	w_a		=	0
ENDIF	

select *;
	FROM f05;
		where	&w_cond																	;
			between(retnomC(d5clie,d5inte), UPPER(w_dcli), UPPER(w_hcli))			and	;
			between(d5clie,wdclie,whclie)											and ;
			between(d5fech,wdfech,whfech)												;
		into dbf &d0f99.c	&& R.01 Mariano
use &d0f99.c alias f99c exclusive	&& R.01 Mariano
*		into DBF f99c	&& R.01 Mariano
	
SELE f99c

REPLA ALL d5tipo	WITH "Y"		FOR d5tipo = "V"	AND d5tico = "R"

DO agruasto

INDEX ON  retnomc(d5clie, d5inte) + PTOI(d5peri) + DTOS(d5fech) + d5sucu + d5tico + d5tifo + d5nume + d5clie tag f99c	&& R.01 Mariano	(TO f99c)

p_repdos=68

*t_leye='Desde el:'+dtoc(wdfech)+' al:'+dtoc(whfech)+;
		space(1)+'Tipo de Comprobantes:FA=Fact,CR=Cr้d,DB=D้bito,TI=Tick,RE=Rec,FC=Fact.Cr้d,RF=Rec-Fact,TF=Tick-Fact,OT='+allt(w_leot)+",RT=Ret."	&& R.03 Mariano
t_leye='Tipo de Comprobantes:FA=Fact,CR=Cr้d,DB=D้bito,TI=Tick,RE=Rec,FC=Fact.Cr้d,RF=Rec-Fact,TF=Tick-Fact,TE=Tick-NC,TD=Tick-ND,OT='+allt(w_leot)+",RT=Ret."	&& R.03 Mariano
		  
w_print	= .F.

if !p_cancela
			return
endif

sele f99c
GO TOP

IF EOF()
		WAIT WIND "CLIENTES - Sin movimientos para informar"
ELSE				
	p_report='iwr005l'

	DO resacta								&& R.02 Ra๚l

	DO listalib WITH "C_", .T.
	do represet &&(Resetea estado)
ENDIF

return

*=========================================================================================

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
*===> SIN RETENCIONES
IF w_imre = 'N'												
	DELE ALL FOR LEFT(uniqueid, 6) = "_1VW19"						&& d5tipo GROUP HEADER Y FOOTER

	DELE ALL FOR LEFT(uniqueid, 7) = "_1WI184"						&& d5tipo variables GROUP FOOTER

	DELE ALL FOR 'Comprobantes' $ expr								&& BORRO Leyendas de d5tipo
	
	DELE ALL FOR "w_imre = 'S'" $ supexpr							&& BORRO valores de d5tipo

	REPLA ALL supexpr	WITH ''					FOR '<>0'	$ supexpr
	REPLA ALL vpos 		WITH vpos - 3541.667	FOR 27812.500 <= vpos	AND vpos < 47708.333
	REPLA ALL vpos 		WITH vpos - 8333.333	FOR 47708.333 <= vpos	
	REPLA resettotal 	WITH 7 	FOR resettotal = 8

ENDIF

PACK
use
p_report =	d0f99 

SELE f99c

return

&& R.02e Ra๚l

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*) Puesto adrede para que incluya el report al proyecto
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
report form iwr005l



