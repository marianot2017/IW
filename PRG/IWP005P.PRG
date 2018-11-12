*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : IWP005P.PRG -> Listador subdiarios por periodo
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
*w_pfile='05p'
w_alias='f05'
w_title='IMPRESION DE SUBDIARIOS POR PERIODO'
w_imco =imco()

*) Apertura de archivos--------------------------------------------------------

fieldvec[1]='F00'
fieldvec[2]='F01'
fieldvec[3]='F02'
fieldvec[4]='F03'
fieldvec[5]='F04'
fieldvec[6]='F05'
fieldvec[7]='F06'

use (d0f00) in 0 alias 'f00' 				     	 &&(Parametros)
use (d0f01) in 0 alias 'f01' order 'd1tip'  	 &&(Rv,Rc)
use (d0f01) in 0 AGAIN alias 'f01v' order 'd1tip'  	 &&(Rv,Rc)				&& Para RUBRO VTAS.

&& R.02b Ra๚l
use (d0f01) in 0 AGAIN alias 'f01p' order 'd1tip'  	 && RP - RUBRO Practicadas
use (d0f01) in 0 AGAIN alias 'f01s' order 'd1tip'  	 && RS - RUBRO Sufridas
&& R.02e Ra๚l

use (d0f02) in 0 alias 'f02' order 'd2clie' 	 &&(Clientes)
use (d0f03) in 0 alias 'f03' order 'd3prov' 	 &&(Proveedores)
use (d0f04) in 0 alias 'f04' order 'd4peri' 	 &&(Periodos)
use (d0f05) in 0 alias 'f05' order 'd5pefe' 	 &&(Comprobantes)
use (d0f06) in 0 alias 'f06' order 'd6inte' 	 &&(Nombres alternativos)

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

w_orde	= 1					&& R.03 Ra๚l

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

w_icai	= 'N'

*)-----------------------------------------------------------------------------

do ssm998.mpr
do iws005p.spr

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

&& R.01b Mariano
if used('f99c')
	sele f99c
	use
	dele file &d0f99.c.dbf
endif
&& R.01e Mariano

select * ;
	from f05 ;
		where	(d5tipo = "V" OR d5tipo = "C")										AND	;
				BETWEEN (PTOI(d5peri), PTOI(wdperi), PTOI(whperi))					AND ;	
				between(d5rubr,wdrubr,whrubr)							   			AND	;
				between(d5sucu,wdsucu,whsucu)											;
		into dbf &d0f99.c	&& R.01 Mariano
use &d0f99.c alias f99c exclusive	&& R.01 Mariano
*		into DBF f99c	&& R.01 Mariano
*		ORDER BY PTOI(d5peri), d5rubr, d5fech, d5clie, d5sucu, d5tico, d5tifo, d5nume

SELE f99c

* COMPRAS TODO mezclado  /  VENTAS separado

REPLA ALL d5tipo	WITH "Y"		FOR d5tipo = "V"	AND d5tico = "R"
REPLA ALL d5tipo	WITH "X"		FOR d5tipo = "C"	AND d5tico = "R"

DO agruasto


&& R.03b Ra๚l
IF w_orde = 1
			INDEX ON d5tipo + d5rubr   		tag f99c	

ELSE
	INDEX ON PTOI(d5peri) + d5tipo + d5rubr +  DTOS(d5fech) +	;
				d5sucu + d5tico + d5tifo + d5nume + d5clie		tag f99c	&& R.01 Mariano	(TO f99c)
ENDIF
&& R.03e Ra๚l

set rela to 'RV'	+ d5rubr		into f01v	
SET RELA TO "RC"	+ d5rubr		INTO f01	ADDITIVE

&& R.02b Ra๚l
SET RELA TO "RP"	+ d5rubr		INTO f01p	ADDITIVE
SET RELA TO "RS"	+ d5rubr		INTO f01s	ADDITIVE
&& R.02e Ra๚l

p_repdos=67

w_print = .F.

t_leye = ' Perํodo : De ' + wdperi + ' a ' + whperi + ', Rubro : De ' + wdrubr + ' a ' + whrubr + 	;
		 ', Sucursal : De ' + wdsucu + ' a ' + whsucu

IF w_vta
		w_reptit='SUBDIARIO DE VENTAS POR PERIODO Y RUBRO'
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
*			p_report='iwr005pv'
&& R.03b Ra๚l
*			p_report='iwr005p'
			IF w_orde = 1
						p_report='iwr005pr'				&& Resumido
			ELSE
				p_report='iwr005p'
			ENDIF
&& R.03e Ra๚l
			DO listalib WITH "PV", .T.
			w_print	= .T.

			&& R.03b Mariano
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
			&& R.03e Mariano
		ENDIF
ENDIF

if w_cpra
		w_reptit='SUBDIARIO DE COMPRAS POR PERIODO Y RUBRO'
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
*			p_report='iwr005pc'
&& R.03b Ra๚l
*			p_report='iwr005p'
			IF w_orde = 1
						p_report='iwr005pr'				&& Resumido
			ELSE
				p_report='iwr005p'
			ENDIF
&& R.03e Ra๚l
			DO listalib WITH "PC", .T.
			w_print	= .T.

			&& R.03b Mariano
			IF w_print
				do represet &&(Resetea estado)
			ENDIF
			&& R.03e Mariano
		ENDIF
ENDIF

&& R.03b Mariano
*IF w_print
*		do represet &&(Resetea estado)
*ENDIF
&& R.03e Mariano

SELE f99c
USE
*dele file f99c.dbf		&& R.01 Mariano
*dele file f99c.idx		&& R.01 Mariano
dele file &d0f99.c.dbf	&& R.01 Mariano
dele file &d0f99.c.cdx	&& R.01 Mariano

return

*=========================================================================================

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*) Puesto adrede para que incluya el report al proyecto
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*report form iwr005pv
*report form iwr005pc
report form iwr005p
report form iwr005pr
