*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : IWP005ER.PRG -> ESTADISTICAS ANUAL POR RUBRO
*)-H Release		 : 06
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

*w_pfile='05er'
	
w_alias='f05'
w_title='ESTADISTICAS ANUAL POR RUBRO'
w_imco =imco()
w_orde	= 1						
w_impt  = 1						&& R.00 Taboada Variable para totales y netos	

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

use (d0f01) in 0 AGAIN alias 'f01p' order 'd1tip'  &&	RP - RUBROS RT Practicadas
use (d0f01) in 0 AGAIN alias 'f01s' order 'd1tip'  &&	RS - RUBROS RT Sufridas

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

&& R.03b Jonatan
w_grav = .F.
w_exen = .F.
w_ivai = .F.
w_ivan = .F.
w_priv = .F.
w_prib = .F.
w_prga = .F.
w_prot = .F.
&& R.03e Jonatan

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

wdsucu	= "     "	&& R.06 Mariano (Agrande 1 digito)
whsucu	= "99999"	&& R.06 Mariano (Agrande 1 digito)

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

t_leye1	= 'Tipo Comprobantes:FA=Fact,CR=Cr้d,DB=D้bito,TI=Tick,RE=Rec,FC=Fact.Cr้d,RF=Rec-Fact,TF=Tick-Fact,TE=Tick-NC,TD=Tick-ND,LV=Liq-Vs.,OT='+allt(w_leot)+",RT=Ret."	&& R.01 Mariano

*)-----------------------------------------------------------------------------

do ssm998.mpr
do iws005er.spr

pop key
pop menu _msysmenu
=closall()

dele file &d0f99.c.dbf	
dele file &d0f99.c.cdx	
dele file &d0f99.e.dbf	&& R.00 Taboada
dele file &d0f99.e.cdx	&& R.00 Taboada

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

if .not. validsubd(.t.) or .not. valid05er(.t.)
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
	dele file &d0f99.c.dbf	
endif

&& R.02b Mariano
if ptoi(whperi)>ptoi(wdperi)
	if seek(ptoi(wdperi),'f04')
		for i=1 to 12
			skip in f04
			if eof('f04') 
				skip -1 in f04
				exit
			endif
		endfor
		if ptoi(whperi)>ptoi(f04.d4peri)
			whperi=f04.d4peri
		endif
	endif
	show get whperi
endif
&& R.02e Mariano

				

sele 0

select * ;
	from f05 ;
		where	(d5tipo = "V" OR d5tipo = "C")										AND	;
				BETWEEN (PTOI(d5peri), PTOI(wdperi), PTOI(whperi))					AND ;	
				between(d5rubr,wdrubr,whrubr)										AND	;
				between(d5fech,wdfech,whfech)										AND ;
				between(d5sucu,wdsucu,whsucu)											;
			into dbf &d0f99.c	
use &d0f99.c alias f99c exclusive	

SELE f99c

REPLA ALL d5tipo	WITH "Y"		FOR d5tipo = "V"	AND d5tico = "R"
REPLA ALL d5tipo	WITH "X"		FOR d5tipo = "C"	AND d5tico = "R"

INDEX ON   d5tipo + d5rubr + PTOI(d5peri) + d5tipo + DTOS(d5fech) +	;		
			d5sucu + d5tico + d5tifo + d5nume		tag f99c	

set rela to 'RC'+ d5rubr into f01
set rela to 'RV'+ d5rubr into f01v		ADDITIVE

set rela to 'RP'+ d5rubr into f01p		ADDITIVE
set rela to 'RS'+ d5rubr into f01s		ADDITIVE

p_repdos=67

w_print = .F.

t_leye = 'Rubro : De ' + wdrubr + ' a ' + whrubr + ', Perํodo : De ' + wdperi + ' a ' + whperi +	;
		 ', Desde el : ' + dtoc(wdfech) + ' al : ' + dtoc(whfech) + ', Sucursal : De ' + wdsucu + ' a ' + whsucu

&& R.00b Taboada 
**///////// Estadisticas de Ventas \\\\\\\\\\\\\**
IF w_vta
	w_reptit='ESTADISTICAS ANUAL POR RUBRO DE VENTAS'
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
		return		&& R.00 Taboada
	ELSE				
		IF w_orde = 1
			p_report='iwr005er'				&& Resumido
		ELSE
			p_report='iwr005dt'
		ENDIF
	ENDIF	

	create table &d0f99.e;
	(d99rubr c(6), d99desc c(40), d99des2 c(100), d99me01 n(18,5),;
	d99me02 n(18,5), d99me03 n(18,5), d99me04 n(18,5), d99me05 n(18,5),;
	d99me06 n(18,5), d99me07 n(18,5), d99me08 n(18,5), d99me09 n(18,5),;
	d99me10 n(18,5), d99me11 n(18,5), d99me12 n(18,5))	&& R.05 Mariano (d99des2 c(60))
	if used('f99e')
		use in f99e
	endif

	use &d0f99.e alias f99e exclusive
	sele f99e
	index on d99rubr tag d99rubr

	*wdfec1=ctod(wdfech)   && transformo a fecha lo ingresado
	*whfec1=ctod('01/'+whfech)   && transformo a fecha lo ingresado
	&& lo hago con el dia 1 igual no lo tendre
	&& en cuenta, solo me interesa el mes/ano.

	*) Realizo un start a la primera fecha de mes / ano ingresado y
	*) el while lo hago por el ano/ mes ingresado (No toma en cuenta los dias)
	
	if w_impt = 1  	
		if w_orde = 1
			w_reptit=w_reptit+' '+'RESUMIDO'
		else
			w_reptit=w_reptit+' '+'DETALLADO'
		endif
		w_reptit=w_reptit+' '+'DE IMPORTES TOTALES'
	endif
	if w_impt = 2
		if w_orde = 1
			w_reptit=w_reptit+' '+'RESUMIDO'
		else
			w_reptit=w_reptit+' '+'DETALLADO'
		endif
		w_reptit=w_reptit+' '+'DE IMPORTES NETOS'
	endif
	
	&& R.03b Jonatan
	if w_impt = 3
		if w_orde = 1
			w_reptit=w_reptit+' '+'RESUMIDO'
		else
			w_reptit=w_reptit+' '+'DETALLADO'
		endif
		w_reptit=w_reptit+' '+'DE IMPORTES ['	&& R.04 Mariano (agregue [)
		&& R.04b Mariano
		if w_grav = .T.
			w_reptit=w_reptit+'GRAV.,'
		endif
		if w_exen = .T.
			w_reptit=w_reptit+'EXEN.,'
		endif
		if w_ivai = .T.
			w_reptit=w_reptit+'IVA.,'
		endif
		if w_ivan = .T.
			w_reptit=w_reptit+'IVA N.I.,'
		endif
		if w_priv = .T.
			w_reptit=w_reptit+'P/R IVA.,'
		endif
		if w_prib = .T.
			w_reptit=w_reptit+'P/R IIBB,'
		endif
		if w_prga = .T.
			w_reptit=w_reptit+'P/R GAN.,'
		endif
		if w_prot = .T.
			w_reptit=w_reptit+'P/R OTR.,'
		endif
		w_reptit=left(w_reptit,len(w_reptit)-1)+']'
		&& R.04e Mariano
	endif
	&& R.03e Jonatan	
	
	sele f99c
	&& R.02b Mariano
*	=start(dtos(wdfech),'f99c')	
*	do while substr(dtos(f99c->d5fech),1,6) <= substr(dtos(whfech),1,6) .and. ;
		.not. eof('f99c') .and. p_cancela
	go top in f99c
	do while !eof('f99c') and p_cancela
	&& R.02e Mariano
		if !empty(f99c.d5rubr) and !between(f99c->d5rubr,wdrubr,whrubr)
			skip in f99c
			loop
		endif
		
		IF w_orde = 1
			if .not. seek(f99c->d5RUBR,'f99e')
				=net_appe('f99e')
			endif
		ELSE
			=net_appe('f99e')	
		ENDIF
		
*		w_mes=strcero(str(month(f99c->d5fech),2))	&& R.02 Mariano
		w_mes=left(f99c->d5peri,2)	&& R.02 Mariano

		=set_alia('f99e')
		if w_impt = 1			
			if w_orde = 1
				replace f99e->d99rubr with f99c->d5rubr,;
						f99e->d99des2 with dtoc(f99c->d5fech)+' '+tcabre(f99c.d5tico)+' '+f99c.d5tifo+' '+allt(f99c->d5sucu)+'-'+allt(f99c->d5nume),;
						f99e->d99me&w_mes with f99e->d99me&w_mes + f99c.d5tota && R.00 Taboada				
			else
				replace f99e->d99rubr with f99c->d5rubr,;
						f99e->d99des2 with dtoc(f99c->d5fech)+' '+tcabre(f99c.d5tico)+' '+f99c.d5tifo+' '+allt(f99c->d5sucu)+'-'+allt(f99c->d5nume),;
						f99e->d99me&w_mes with f99c.d5tota && R.00 Taboada

				&& R.05b Mariano
				if f99c.d5tipo='V'
					replace f99e.d99des2 with rtrim(f99e.d99des2)+' Cliente : '+f99c.d5clie+'-'+iif(f99c.d5clie>='99990', iif(seek(f99c.d5inte,'f06'), f06.d6nocl, 'No encontrado!!!'), iif(seek(f99c.d5clie,'f02'), f02.d2nomb, 'No encontrado!!!'))
				else
					replace f99e.d99des2 with rtrim(f99e.d99des2)+' Proveed.: '+f99c.d5clie+'-'+iif(f99c.d5clie>='99990', iif(seek(f99c.d5inte,'f06'), f06.d6nocl, 'No encontrado!!!'), iif(seek(f99c.d5clie,'f03'), f03.d3nomb, 'No encontrado!!!'))
				endif
				&& R.05e Mariano
			endif
		endif
		if w_impt = 2
			if w_orde = 1
				replace f99e->d99rubr with f99c->d5rubr,;
						f99e->d99des2 with dtoc(f99c->d5fech)+' '+tcabre(f99c.d5tico)+' '+f99c.d5tifo+' '+allt(f99c->d5sucu)+'-'+allt(f99c->d5nume),;
						f99e->d99me&w_mes with f99e->d99me&w_mes + f99c.d5imp1 + f99c.d5imp2 && R.00 Taboada	&& R.02 Mariano (agregue d5imp2)
			else
				replace f99e->d99rubr with f99c->d5rubr,;
						f99e->d99des2 with dtoc(f99c->d5fech)+' '+tcabre(f99c.d5tico)+' '+f99c.d5tifo+' '+allt(f99c->d5sucu)+'-'+allt(f99c->d5nume),;
						f99e->d99me&w_mes with f99c.d5imp1 + f99c.d5imp2 && R.00 Taboada	&& R.02 Mariano (agregue d5imp2)
				&& R.05b Mariano
				if f99c.d5tipo='V'
					replace f99e.d99des2 with rtrim(f99e.d99des2)+' Cliente : '+f99c.d5clie+'-'+iif(f99c.d5clie>='99990', iif(seek(f99c.d5inte,'f06'), f06.d6nocl, 'No encontrado!!!'), iif(seek(f99c.d5clie,'f02'), f02.d2nomb, 'No encontrado!!!'))
				else
					replace f99e.d99des2 with rtrim(f99e.d99des2)+' Proveed.: '+f99c.d5clie+'-'+iif(f99c.d5clie>='99990', iif(seek(f99c.d5inte,'f06'), f06.d6nocl, 'No encontrado!!!'), iif(seek(f99c.d5clie,'f03'), f03.d3nomb, 'No encontrado!!!'))
				endif
				&& R.05e Mariano
			endif
		endif
		
		&& R.03b Jonatan
		if w_impt = 3
			w_impo = 0
			if w_grav = .T.
				w_impo = w_impo + f99c.d5imp1
			endif
			if w_exen = .T.
				w_impo = w_impo + f99c.d5imp2
			endif
			if w_ivai = .T.
				w_impo = w_impo + f99c.d5imp4
			endif
			if w_ivan = .T.
				w_impo = w_impo + f99c.d5imp5
			endif
			if w_priv = .T.
				if f99c.d5tipo = "V"
					w_impo = w_impo + f99c.d5imp3
				else
					if !empty(f99c.d5pere)
						w_impo = w_impo + f99c.d5tota
					endif
				endif
			endif
			if w_prib = .T.
				if f99c.d5tipo = "V"
					IF f00.p0prv1 = 1 or f00.p0prv2 = 1 or f00.p0prv3 = 1
						if f00.p0prv1 = 1
							w_impo = w_impo + f99c.d5imp6
						endif
						if f00.p0prv2 = 1
							w_impo = w_impo + f99c.d5imp7
						endif
						if f00.p0prv3 = 1
							w_impo = w_impo + f99c.d5imp8
						endif
					ENDIF
				else
					if !empty(f99c.d5cpib)
						w_impo = w_impo + f99c.d5tota
					endif
				endif
			endif
			if w_prga = .T.
				if f99c.d5tipo = "V"
					IF f00.p0prv1 = 2 or f00.p0prv2 = 2 or f00.p0prv3 = 2
						if f00.p0prv1 = 2
							w_impo = w_impo + f99c.d5imp6
						endif
						if f00.p0prv2 = 2
							w_impo = w_impo + f99c.d5imp7
						endif
						if f00.p0prv3 = 2
							w_impo = w_impo + f99c.d5imp8
						endif
					ENDIF
				else
					if !empty(f99c.d5cpga)
						w_impo = w_impo + f99c.d5tota
					endif
				endif
			endif
			if w_prot = .T.
				if f99c.d5tipo = "V"
					IF f00.p0prv1 = 3 or f00.p0prv2 = 3 or f00.p0prv3 = 3
						if f00.p0prv1 = 3
							w_impo = w_impo + f99c.d5imp6
						endif
						if f00.p0prv2 = 3
							w_impo = w_impo + f99c.d5imp7
						endif
						if f00.p0prv3 = 3
							w_impo = w_impo + f99c.d5imp8
						endif
					ENDIF
				else
					if !empty(f99c.d5cpot)
						w_impo = w_impo + f99c.d5tota
					endif
				endif			
			endif
			if w_orde = 1
				replace f99e->d99rubr with f99c->d5rubr,;
						f99e->d99des2 with dtoc(f99c->d5fech)+' '+tcabre(f99c.d5tico)+' '+f99c.d5tifo+' '+allt(f99c->d5sucu)+'-'+allt(f99c->d5nume),;
						f99e->d99me&w_mes with f99e->d99me&w_mes + w_impo 
			else
				replace f99e->d99rubr with f99c->d5rubr,;
						f99e->d99des2 with dtoc(f99c->d5fech)+' '+tcabre(f99c.d5tico)+' '+f99c.d5tifo+' '+allt(f99c->d5sucu)+'-'+allt(f99c->d5nume),;
						f99e->d99me&w_mes with w_impo
				&& R.05b Mariano
				if f99c.d5tipo='V'
					replace f99e.d99des2 with rtrim(f99e.d99des2)+' Cliente : '+f99c.d5clie+'-'+iif(f99c.d5clie>='99990', iif(seek(f99c.d5inte,'f06'), f06.d6nocl, 'No encontrado!!!'), iif(seek(f99c.d5clie,'f02'), f02.d2nomb, 'No encontrado!!!'))
				else
					replace f99e.d99des2 with rtrim(f99e.d99des2)+' Proveed.: '+f99c.d5clie+'-'+iif(f99c.d5clie>='99990', iif(seek(f99c.d5inte,'f06'), f06.d6nocl, 'No encontrado!!!'), iif(seek(f99c.d5clie,'f03'), f03.d3nomb, 'No encontrado!!!'))
				endif
				&& R.05e Mariano
			endif	
		endif	
		&& R.03e Jonatan

		if empty(f99e->d99desc)
			Do case
				Case f99c.d5tipo ='V'
					if seek('RV'+f99c->d5rubr,'f01')	
						replace f99e->d99desc with f01->d1des	&& R.00 Taboada
					endif
				case f99c.d5tipo ='C'				 
					if seek('RC'+f99c->d5rubr,'f01')	
						replace f99e->d99desc with f01->d1des	&& R.00 Taboada
					endif
			Endcase		
		endif					
					
		=rest_alia()

		skip in f99c
			
	Enddo
	
	if !p_cancela
		return
	endif

	sele f99e

	go top

	&& R.01b Mariano

*	do repdest &&(Setea variables internas del reporte)

*	if !empty(p_repclaus) &&(debe ser preview o to printer)
*		=rep_form()
*		p_report=''
*	endif
			
*	do represet &&(Resetea estado)

	DO listalib WITH "EV", .T.
	w_print	= .T.

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

	use in f99e
	dele file &d0f99.e.dbf
	dele file &d0f99.e.cdx
	
ENDIF

**///////// Estadisticas de Compra \\\\\\\\\**
IF w_cpra
	w_reptit='ESTADISTICAS ANUAL POR RUBRO DE COMPRAS'
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
		return	
	ELSE				
		IF w_orde = 1
			p_report='iwr005er'				&& Resumido
		ELSE
			p_report='iwr005dt'
		ENDIF
	ENDIF	

	create table &d0f99.e;
	(d99rubr c(6), d99desc c(40), d99des2 c(100), d99me01 n(18,5),;
	d99me02 n(18,5), d99me03 n(18,5), d99me04 n(18,5), d99me05 n(18,5),;
	d99me06 n(18,5), d99me07 n(18,5), d99me08 n(18,5), d99me09 n(18,5),;
	d99me10 n(18,5), d99me11 n(18,5), d99me12 n(18,5))	&& R.05 Mariano (d99des2(60))
	if used('f99e')
	use in f99e
	endif

	use &d0f99.e alias f99e exclusive
	sele f99e
	index on d99rubr tag d99rubr

	*wdfec1=ctod(wdfech)   && transformo a fecha lo ingresado
	*whfec1=ctod('01/'+whfech)   && transformo a fecha lo ingresado
	&& lo hago con el dia 1 igual no lo tendre
	&& en cuenta, solo me interesa el mes/ano.

	*) Realizo un start a la primera fecha de mes / ano ingresado y
	*) el while lo hago por el ano/ mes ingresado (No toma en cuenta los dias)
	&& R.03b Jonatan
	if w_impt = 1 	
		if w_orde = 1
			w_reptit=w_reptit+' '+'RESUMIDO'
		else
			w_reptit=w_reptit+' '+'DETALLADO'
		endif
		w_reptit=w_reptit+' '+'DE IMPORTES TOTALES'
	endif
	if w_impt = 2	
		if w_orde = 1
			w_reptit=w_reptit+' '+'RESUMIDO'
		else
			w_reptit=w_reptit+' '+'DETALLADO'
		endif
		w_reptit=w_reptit+' '+'DE IMPORTES NETOS'
	endif
	if w_impt = 3	
		if w_orde = 1
			w_reptit=w_reptit+' '+'RESUMIDO'
		else
			w_reptit=w_reptit+' '+'DETALLADO'
		endif
		w_reptit=w_reptit+' '+'DE IMPORTES A ELECCIำN'
	endif
	&& R.03e Jonatan
	
	sele f99c
	=start(dtos(wdfech),'f99c')
	do while substr(dtos(f99c->d5fech),1,6) <= substr(dtos(whfech),1,6) .and. ;
		.not. eof('f99c') .and. p_cancela

		if !empty(f99c.d5rubr) and !between(f99c->d5rubr,wdrubr,whrubr)
			skip in f99c
			loop
		endif
		
		IF w_orde = 1
			if .not. seek(f99c->d5RUBR,'f99e')
				=net_appe('f99e')
			endif
		ELSE
			=net_appe('f99e')	
		ENDIF
		
*		w_mes=strcero(str(month(f99c->d5fech),2))	&& R.02 Mariano
		w_mes=left(f99c->d5peri,2)	&& R.02 Mariano

		=set_alia('f99e')
		
		if w_impt = 1 	
			if w_orde = 1
				replace f99e->d99rubr with f99c->d5rubr,;
						f99e->d99des2 with dtoc(f99c->d5fech)+' '+tcabre(f99c.d5tico)+' '+f99c.d5tifo+' '+allt(f99c->d5nume),;
						f99e->d99me&w_mes with f99e->d99me&w_mes + f99c.d5tota && R.00 Taboada				
			else
				replace f99e->d99rubr with f99c->d5rubr,;
						f99e->d99des2 with dtoc(f99c->d5fech)+' '+tcabre(f99c.d5tico)+' '+f99c.d5tifo+' '+allt(f99c->d5nume),;
						f99e->d99me&w_mes with f99c.d5tota && R.00 Taboada				
				&& R.05b Mariano
				if f99c.d5tipo='C'
					replace f99e.d99des2 with rtrim(f99e.d99des2)+' Proveed.: '+f99c.d5clie+'-'+iif(f99c.d5clie>='99990', iif(seek(f99c.d5inte,'f06'), f06.d6nocl, 'No encontrado!!!'), iif(seek(f99c.d5clie,'f03'), f03.d3nomb, 'No encontrado!!!'))
				else
					replace f99e.d99des2 with rtrim(f99e.d99des2)+' Cliente : '+f99c.d5clie+'-'+iif(f99c.d5clie>='99990', iif(seek(f99c.d5inte,'f06'), f06.d6nocl, 'No encontrado!!!'), iif(seek(f99c.d5clie,'f02'), f02.d2nomb, 'No encontrado!!!'))
				endif
				&& R.05e Mariano
			endif
		endif
		if w_impt = 2
			if w_orde = 1
				replace f99e->d99rubr with f99c->d5rubr,;
						f99e->d99des2 with dtoc(f99c->d5fech)+' '+tcabre(f99c.d5tico)+' '+f99c.d5tifo+' '+allt(f99c->d5nume),;
						f99e->d99me&w_mes with f99e->d99me&w_mes + f99c.d5imp1 + f99c.d5imp2 && R.00 Taboada	&& R.02 Mariano (agregue d5imp2)
			else
				replace f99e->d99rubr with f99c->d5rubr,;
						f99e->d99des2 with dtoc(f99c->d5fech)+' '+tcabre(f99c.d5tico)+' '+f99c.d5tifo+' '+allt(f99c->d5nume),;
						f99e->d99me&w_mes with f99c.d5imp1 + f99c.d5imp2 && R.00 Taboada	&& R.02 Mariano (agregue d5imp2)
				&& R.05b Mariano
				if f99c.d5tipo='C'
					replace f99e.d99des2 with rtrim(f99e.d99des2)+' Proveed.: '+f99c.d5clie+'-'+iif(f99c.d5clie>='99990', iif(seek(f99c.d5inte,'f06'), f06.d6nocl, 'No encontrado!!!'), iif(seek(f99c.d5clie,'f03'), f03.d3nomb, 'No encontrado!!!'))
				else
					replace f99e.d99des2 with rtrim(f99e.d99des2)+' Cliente : '+f99c.d5clie+'-'+iif(f99c.d5clie>='99990', iif(seek(f99c.d5inte,'f06'), f06.d6nocl, 'No encontrado!!!'), iif(seek(f99c.d5clie,'f02'), f02.d2nomb, 'No encontrado!!!'))
				endif
				&& R.05e Mariano
			endif
		endif
		
		&& R.03b Jonatan
		if w_impt = 3
			w_impo = 0
			if w_grav = .T.
				w_impo = w_impo + f99c.d5imp1
			endif
			if w_exen = .T.
				w_impo = w_impo + f99c.d5imp2
			endif
			if w_ivai = .T.
				w_impo = w_impo + f99c.d5imp4
			endif
			if w_ivan = .T.
				w_impo = w_impo + f99c.d5imp5
			endif
			if w_priv = .T.
				if f99c.d5tipo = "C"
					w_impo = w_impo + f99c.d5imp3
				else
					w_impo = w_impo + f99c.d5tota	
				endif
			endif
			if w_prib = .T.
				if f99c.d5tipo = "C"
					IF f00.p0prc1 = 1 or f00.p0prc2 = 1 or f00.p0prc3 = 1
						if f00.p0prc1 = 1
							w_impo = w_impo + f99c.d5imp6
						endif
						if f00.p0prc2 = 1
							w_impo = w_impo + f99c.d5imp7
						endif
						if f00.p0prc3 = 1
							w_impo = w_impo + f99c.d5imp8
						endif
					ENDIF
				else
					if !empty(f99c.d5cpib)
						w_impo = w_impo + f99c.d5tota
					endif
				endif
			endif
			if w_prga = .T.
				if f99c.d5tipo = "C"
					IF f00.p0prc1 = 2 or f00.p0prc2 = 2 or f00.p0prc3 = 2
						if f00.p0prc1 = 2
							w_impo = w_impo + f99c.d5imp6
						endif
						if f00.p0prc2 = 2
							w_impo = w_impo + f99c.d5imp7
						endif
						if f00.p0prc3 = 2
							w_impo = w_impo + f99c.d5imp8
						endif
					ENDIF
				else
					if !empty(f99c.d5cpga)
						w_impo = w_impo + f99c.d5tota
					endif
				endif
			endif
			if w_prot = .T.
				if f99c.d5tipo = "C"
					IF f00.p0prc1 = 3 or f00.p0prc2 = 3 or f00.p0prc3 = 3
						if f00.p0prc1 = 3
							w_impo = w_impo + f99c.d5imp6
						endif
						if f00.p0prc2 = 3
							w_impo = w_impo + f99c.d5imp7
						endif
						if f00.p0prc3 = 3
							w_impo = w_impo + f99c.d5imp8
						endif
					ENDIF
				else
					if !empty(f99c.d5cpot)
						w_impo = w_impo + f99c.d5tota
					endif
				endif			
			endif
			if w_orde = 1
				replace f99e->d99rubr with f99c->d5rubr,;
						f99e->d99des2 with dtoc(f99c->d5fech)+' '+tcabre(f99c.d5tico)+' '+f99c.d5tifo+' '+allt(f99c->d5nume),;
						f99e->d99me&w_mes with f99e->d99me&w_mes + w_impo
			else
				replace f99e->d99rubr with f99c->d5rubr,;
						f99e->d99des2 with dtoc(f99c->d5fech)+' '+tcabre(f99c.d5tico)+' '+f99c.d5tifo+' '+allt(f99c->d5nume),;
						f99e->d99me&w_mes with w_impo 
				&& R.05b Mariano
				if f99c.d5tipo='C'
					replace f99e.d99des2 with rtrim(f99e.d99des2)+' Proveed.: '+f99c.d5clie+'-'+iif(f99c.d5clie>='99990', iif(seek(f99c.d5inte,'f06'), f06.d6nocl, 'No encontrado!!!'), iif(seek(f99c.d5clie,'f03'), f03.d3nomb, 'No encontrado!!!'))
				else
					replace f99e.d99des2 with rtrim(f99e.d99des2)+' Cliente : '+f99c.d5clie+'-'+iif(f99c.d5clie>='99990', iif(seek(f99c.d5inte,'f06'), f06.d6nocl, 'No encontrado!!!'), iif(seek(f99c.d5clie,'f02'), f02.d2nomb, 'No encontrado!!!'))
				endif
				&& R.05e Mariano
			endif
		endif
		&&R.03e Jonatan
				
			if empty(f99e->d99desc)
				Do case
					Case f99c.d5tipo ='V'
						if seek('RV'+f99c->d5rubr,'f01')	
							replace f99e->d99desc with f01->d1des	&& R.00 Taboada
						endif
					case f99c.d5tipo ='C'				 
						if seek('RC'+f99c->d5rubr,'f01')	
							replace f99e->d99desc with f01->d1des	&& R.00 Taboada
						endif
				Endcase		
			endif					
					
		=rest_alia()

		skip in f99c
			
	enddo
&& R.00e Taboada 	
	
	if !p_cancela
		return
	endif

	sele f99e

	go top

	&& R.01b Mariano
	
*	do repdest &&(Setea variables internas del reporte)

*	if !empty(p_repclaus) &&(debe ser preview o to printer)
*		=rep_form()
*		p_report=''
*	endif
	
*	do represet

	DO listalib WITH "EC", .T.
	w_print	= .T.

	IF w_print
		do represet &&(Resetea estado)
	ENDIF
	&& R.01e Mariano

	use in f99e
	dele file &d0f99.e.dbf
	dele file &d0f99.e.cdx
	
ENDIF

SELE f99c
USE
dele file &d0f99.c.dbf	
dele file &d0f99.c.cdx	

return
&& R.00b Taboada 
*-F อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
function mesr17a
*-F อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
parameter wdesp

priv w_camp,wmes

wmes=int(val(left(wdperi,2)))+wdesp
if wmes>12
	wmes=wmes-12
endif
w_camp='d99me'+strt(str(wmes,2),' ','0')

return eval(w_camp)


*-F อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
function anno17a
*-F อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
parameter wdesp

priv wmes,wanno

wmes=int(val(left(wdperi,2)))+wdesp
if wmes>12
	wanno=right(whperi,2)
else
	wanno=right(wdperi,2)
endif

return wanno

*-F อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
function prwh17a
*-F อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
parameter wdesp

w_cond=.f.

if int((ctod('01/'+whperi)-ctod('01/'+wdperi))/28)+1>wdesp
	w_cond=.t.
endif

return w_cond
&& R.00e Taboada 

*================================================================================

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*) Puesto adrede para que incluya el report al proyecto
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
report form iwr005er
report form iwr005dt

&& R.03b Jonatan
*===========================================================================================
PROC valid05er
*===========================================================================================

PARAMETERS w_show

w_retu=.t.
w_vari=varread()
p_curobj=_curobj

&& R.04b Mariano
if w_vari='W_GRAV' 
	=mchkbox(@w_grav)
endif
if w_vari='W_EXEN' 
	=mchkbox(@w_exen)
endif
if w_vari='W_IVAI' 
	=mchkbox(@w_ivai)
endif
if w_vari='W_IVAN' 
	=mchkbox(@w_ivan)
endif
if w_vari='W_PRIV' 
	=mchkbox(@w_priv)
endif
if w_vari='W_PRIB' 
	=mchkbox(@w_prib)
endif
if w_vari='W_PRGA' 
	=mchkbox(@w_prga)
endif
if w_vari='W_PROT' 
	=mchkbox(@w_prot)
endif
&& R.04e Mariano

w_dir 	= CURDIR() + "\ARCHIVOS\SDIARIOS\"
w_name	= d0coem + RIGHT(wdperi,2) + LEFT(wdperi,2)

if w_vari='W_IMPT' or w_show
	if w_impt=3
		show get w_grav enable
		show get w_exen enable
		show get w_ivai enable
		show get w_ivan enable
		show get w_priv enable
		show get w_prib enable
		show get w_prga enable
		show get w_prot enable
	else
		w_grav = .F.
		w_exen = .F.
		w_ivai = .F.
		w_ivan = .F.
		w_priv = .F.
		w_prib = .F.
		w_prga = .F.
		w_prot = .F.
		show get w_grav disable
		show get w_exen disable
		show get w_ivai disable
		show get w_ivan disable
		show get w_priv disable
		show get w_prib disable
		show get w_prga disable
		show get w_prot disable
	endif
endif

if w_impt=3 and w_show and empty(w_grav) and empty(w_exen) and empty(w_ivai) and empty(w_ivan) and empty(w_priv) and empty(w_prib) and empty(w_prga) and empty(w_prot)
	wait window 'Debe elegir algun tipo de importe'
	w_retu = .f.
endif

return(w_retu)

&& R.03e Jonatan