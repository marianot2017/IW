*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : IWP005S.PRG -> Listador subdiarios por fecha
*)-H Release		 : 17
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

*w_pfile='05s'

w_prog =upper(prompt())
w_title='IMPRESION DE SUBDIARIOS POR FECHA'
w_alta =.f.
w_alias='f05'

*) Apertura de archivos--------------------------------------------------------

fieldvec[1]='F00'
fieldvec[2]='F01'
fieldvec[3]='F02'
fieldvec[4]='F03'
fieldvec[5]='F04'
fieldvec[6]='F05'
fieldvec[7]='F06'
fieldvec[8]='F23'

use (d0f00) in 0 alias 'f00' 						&&(Parametros)
use (d0f01) in 0 alias 'f01' order 'd1tip'   &&(Tablas)
use (d0f02) in 0 alias 'f02' order 'd2clie' &&(Clientes)
use (d0f03) in 0 alias 'f03' order 'd3prov' &&(Proveedores)
use (d0f04) in 0 alias 'f04' order 'd4peri' &&(Periodos)
use (d0f05) in 0 alias 'f05' order 'd5pefe' &&(Comprobantes)
use (d0f06) in 0 alias 'f06' order 'd6inte' &&(Nombres alternativos)
use (d0f23) in 0 alias 'f23' order 'd23codi' &&(Alicuotas IVA)

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

w_subt		= SPACE(95)						&& R.08 Ra๚l
w_reptit	= ''							&& R.08 Ra๚l

warch	= " "
w_dir	= " "
w_name	= " "
w_cuit	= SPACE(13)


w_pfile	= "subd"
w_while = " "

GO BOTTOM IN f04
wdperi	=	f04.d4peri
whperi	=	f04.d4peri
w4hove	=	f04.d4hove
w4ulve	=	f04.d4hove 
w4hoco	=	f04.d4hoco
w4ulco	=	f04.d4hoco

*IF w4hove = 0
*			SKIP -1 IN f04
*			w4hove	=	f04.d4hove + 1
*			w4ulve	=	f04.d4hove + 1
*			SKIP IN f04
*ENDIF			

*IF w4hoco = 0
*			SKIP -1 IN f04
*			w4hoco	=	f04.d4hoco + 1
*			w4ulco	=	f04.d4hoco + 1
*			SKIP IN f04
*ENDIF			

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
w_lfech = {} 	&& R.07 Mariano
wxfech	= whfech

wdclie	= SPACE(5)
whclie	= "99999"

w_dcli	= SPACE(35)
w_hcli	= REPL('Z',35)

wdrubr	= "      "
whrubr	= "999999"

wdsucu	= "     "	&& R.17 Mariano (agregue 1 digito)
whsucu	= "99999"	&& R.17 Mariano (agregue 1 digito)

wdjuri	= " "
whjuri	= "Z"

w_imre	= "N"

wtalic='N'

w_leot=f00.p0leot
t_leye	= 'Correspondiente a '
t_leye1	= 'Tipo Comprobantes:FA=Fact,CR=Cr้d,DB=D้bito,TI=Tick,RE=Rec,FC=Fact.Cr้d,RF=Rec-Fact,TF=Tick-Fact,TE=Tick-NC,TD=Tick-ND,LV=Liq-Vs.,OT='+allt(w_leot)+",RT=Ret."	&& R.12 Mariano (agregue ticket-NC y Ticket-ND)	&& R.13 Mariano (agregue liq.varias)

w_iva	= 0
w_ib	= 0
w_gan	= 0
w_ot	= 0
w_imp	= " "

w_resu=.t.

w_icai	= 'N'

w_apai = 'N'	&& R.16 Mariano

w_paso=.f.
w_pline=0

public array tota[7],imp1[7],imp2[7],imp4[7],imp5[7],imp6[7],imp7[7],imp8[7],imp3[7],ret3[7],ret6[7],ret7[7],ret8[7]

store 0 to tota, imp1, imp2, imp4, imp5, imp6, imp7, imp8, imp3, ret3, ret6, ret7, ret8
store 0 to watota, watotv, waimp1, waimp2, waimp4, waimp5, waimp6, waimp7, waimp8, waimp3, wareib, waribv,wareiv, warivv, warega, wargav, wareot, warotv

w_i=reccount('f23')
*?public array aalic[w_i],atota[w_i],aimp1[w_i],aimp2[w_i],aimp4[w_i],aimp5[w_i],aimp6[w_i],aimp7[w_i],aimp8[w_i],aimp3[w_i]
public array balic[w_i],btota[w_i],bimp1[w_i],bimp2[w_i],bimp4[w_i],bimp5[w_i],bimp6[w_i],bimp7[w_i],bimp8[w_i],bimp3[w_i]

*?store 0 to aalic, atota, aimp1, aimp2, aimp4, aimp5, aimp6, aimp7, aimp8, aimp3
&& R.10b Mariano
*store 0 to balic, btota, bimp1, bimp2, bimp4, bimp5, bimp6, bimp7, bimp8, bimp3
store 0 to btota, bimp1, bimp2, bimp4, bimp5, bimp6, bimp7, bimp8, bimp3
store '***' to balic
&& R.10e Mariano

*)-----------------------------------------------------------------------------

do ssm998.mpr
do iws005s.spr

release tota,imp1,imp2,imp4,imp5,imp6,imp7,imp8,imp3,ret3,ret6,ret7,ret8

pop key
pop menu _msysmenu
=closall()

*dele file f99c.dbf		&& R.01 Mariano
*dele file f99c.idx		&& R.01 Mariano
dele file &d0f99.c.dbf	&& R.01 Mariano
dele file &d0f99.c.cdx	&& R.01 Mariano
dele file &d0f99.d.dbf	&& R.14 Mariano
dele file &d0f99.d.cdx	&& R.14 Mariano

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

IF !validsubd(.T.)
	wait wind 'Verifique los datos ingresados...'
	return
endif

if !w_vta and !w_cpra
	wait window 'Debe elegir Ventas y/o Compras'
	return
endif


w_lfech		= wlfech

=gentmp()

&& R.14b Mariano
if used('f99d')
	sele f99d
	use
	dele file &d0f99.d.dbf
	dele file &d0f99.d.cdx
endif
&& R.14e Mariano
if used('f99c')
	sele f99c
	use
	dele file &d0f99.c.dbf	&& R.01 Mariano
	dele file &d0f99.c.cdx	&& R.14 Mariano
endif
*dele file f99c.dbf		&& R.01 Mariano
*dele file f99c.idx		&& R.01 Mariano

&& R.13b Mariano
store 0 to tota, imp1, imp2, imp4, imp5, imp6, imp7, imp8, imp3, ret3, ret6, ret7, ret8	
store 0 to watota, watotv, waimp1, waimp2, waimp4, waimp5, waimp6, waimp7, waimp8, waimp3, wareib, waribv,wareiv, warivv, warega, wargav, wareot, warotv
&& R.13e Mariano

for w_j=1 to reccount('f23')	&& total de alicuotas
		balic[w_j]='***'
ENDFOR

sele 0

	select *;
		from f05;
		where	(d5tipo = "V" OR d5tipo = "C")								AND	;
				BETWEEN (PTOI(d5peri), PTOI(wdperi), PTOI(whperi))			AND ;	
				d5fech <= whfech											AND ;
				between(d5sucu,wdsucu,whsucu)									;
		into dbf &d0f99.c	&& R.01 Mariano
use &d0f99.c alias f99c exclusive	&& R.01 Mariano
*		into DBF f99c	&& R.01 Mariano
*				between(d5fech,wdfech,whfech)								AND ;
*		ORDER BY PTOI(d5peri), d5fech, d5clie, d5sucu, d5tico, d5tifo, d5nume
		
SELE f99c

*=====================================================================================
* Genero registros vacํos para perํodos inexistentes o sin informaci๓n

w_peri		= wdperi

DO WHILE BETWEEN (PTOI(w_peri), PTOI(wdperi), PTOI(whperi))	
		IF SEEK (PTOI(w_peri), 'f04')
				IF w_vta
						COUNT FOR d5peri = w_peri	AND d5tipo = "V"		TO w_count
						IF w_count = 0
									APPE BLAN
									REPLA d5peri	WITH w_peri, d5tipo WITH "V", d5empr WITH "ZX"
						ENDIF
				ENDIF
				IF w_cpra
						COUNT FOR d5peri = w_peri	AND d5tipo = "C"		TO w_count
						IF w_count = 0
									APPE BLAN
									REPLA d5peri	WITH w_peri, d5tipo WITH "C", d5empr WITH "ZX"
						ENDIF
				ENDIF
		ELSE
			IF w_cpra
					APPE BLAN
					REPLA d5peri	WITH w_peri, d5tipo WITH "C", d5empr WITH "ZZ"
			ENDIF			
			IF w_vta
					APPE BLAN
					REPLA d5peri	WITH w_peri, d5tipo WITH "V", d5empr WITH "ZZ"
			ENDIF			
		ENDIF
		w_peri	= RIGHT(DTOC((CTOD("01/" + w_peri) + 31)),7)
ENDDO

* COMPRAS TODO mezclado  /  VENTAS separado

REPLA ALL d5tipo	WITH "Y"		FOR d5tipo = "V"	AND d5tico = "R"

&& R.13b Mariano
w_peri=wdperi
DO agruasto		
&& R.13e Mariano

INDEX ON PTOI(d5peri) + d5tipo  +  DTOS(d5fech) +	;
						  + d5sucu + d5tico + d5tifo + d5nume + d5clie		tag f99c	&& R.04 Ra๚l	(TO f99c)
*						  + d5sucu + d5tico + d5tifo + d5nume		tag f99c	&& R.01 Mariano	(TO f99c)	

*=========================================================================================
*p_repdos=57	&& R.13 Mariano
p_repdos=62		&& R.13 Mariano

w_print		= .F.
w_trans		= .F.
w_count		= 0
w_leye		= " "
w_peri		= wdperi
w_fecha		= wdfech

=SEEK (PTOI(wdperi), 'f04')

if w_vta
	w_trans		= .F.
	w_oc1=f00->p0ocv1
	w_oc2=f00->p0ocv2
	w_oc3=f00->p0ocv3

	IF w4hove = 0
				w4hove = 1
	ENDIF		

&& R.08b Ra๚l
*	w_title='SUBDIARIO DE VENTAS'
	w_reptit	= ALLT(f00.p0tit1)
	DO titsubd
&& R.08e Ra๚l

	if !p_cancela
		return
	endif

	w_paso=.f.

	CREATE CURSOR hojas ( peri C(7), hojas N(4,0) )

	&& R.14b Mariano
	SELE f99d
	IF w_imre = "S"
		SET FILTER TO d5tipo = "V" OR d5tipo = "Y"
	ELSE
		SET FILTER TO d5tipo = "V"
	ENDIF
	&& R.14e Mariano
	SELE f99c
	IF w_imre = "S"
				SET FILTER TO d5tipo = "V" OR d5tipo = "Y"
				w_a	= 1
	ELSE
		SET FILTER TO d5tipo = "V"
		w_a = 0
	ENDIF

	SCATTER MEMVAR
	GO TOP
*???????????????????????????????????????????????????????
	IF !EOF()
			w_recno		= RECNO()
			DO totales
			GO w_recno
	ENDIF

	p_repfor=''	&& anulo el rep_for
&& R.02b Ra๚l
*	p_report='iwr005sv'	&& R.16 Mariano
	p_report=iif(w_apai='S','iwr005sw','iwr005sv')	&& R.16 Mariano
	DO resacta
&& R.02e Ra๚l	

	DO listalib WITH "FV", .T.

*	if p_repout="Impresora" and w4ulve>0										&& R.03 Ra๚l
*	if (p_repout="Impresora" OR LEFT(p_repout,1)	= '*') and w4ulve>0			&& R.03 Ra๚l		&& R.09 Ra๚l
	if (p_repout="Impresora" OR LEFT(p_repout,1)	= '*') and w4ulve>0	 AND p_preview = 0			&& R.09 Ra๚l
			if 'S'=sino(' Guarda Ultima Hoja y Cierra el Perํodo de VENTAS ? ')
				=db_rlock('f04')
&& R.05b Ra๚l
				SELE hojas
				GO TOP
				DO WHILE !EOF()
							IF SEEK(PTOI(peri), 'f04')
&& R.05e Ra๚l
									sele f04
									repl d4hove with hojas.hojas, d4esve with 'C'
&& R.05b Ra๚l
							ENDIF
							SELE hojas
							SKIP
				ENDDO
&& R.05e Ra๚l
				unlock in f04
			endif
	endif
	SELE hojas
	USE
	wdfech	= w_fecha
	DELE FILE &d0f99..frx				&& R.02 Ra๚l

	&& R.09b Mariano
	w_prepout=p_repout
	w_preptype=p_reptype	
	w_preptyp2=p_reptype2	&& R.13 Mariano
	IF w_print
			sele f99c	&& R.13 Mariano
			go top		&& R.13 Mariano
			do represet &&(Resetea estado)
	ENDIF
	** Para que pida el nombre del archivo de compras
	if w_cpra
		w_print=.f.
		p_repout=w_prepout
		p_reptype=w_preptype
		p_reptype2=w_preptyp2	&& R.13 Mariano
	endif
	&& R.09e Mariano
endif

w_trans		= .F.
w_count		= 0
w_leye		= " "
w_peri		= wdperi
w_fecha		= wdfech


if w_cpra
	w_trans		= .F.
	w_oc1=f00->p0occ1
	w_oc2=f00->p0occ2
	w_oc3=f00->p0occ3

	IF w4hoco = 0
				w4hoco = 1
	ENDIF		

&& R.08b Ra๚l
*	w_title='SUBDIARIO DE COMPRAS'
	w_reptit	= ALLT(f00.p0tit2)
	DO titsubd
&& R.08e Ra๚l

	if !p_cancela
		return
	endif

	w_paso=.f.
	
	CREATE CURSOR hojas ( peri C(7), hojas N(4,0) )

	&& R.14b Mariano
	SELE f99d
	IF w_imre = "S"
		SET FILTER TO d5tipo = "C"
	ELSE
		SET FILTER TO d5tipo = "C" AND d5tico <> "R" 
	ENDIF
	&& R.14e Mariano
	SELE f99c
	IF w_imre = "S"
				SET FILTER TO d5tipo = "C"
				w_a		= 1
	ELSE
		SET FILTER TO d5tipo = "C" AND d5tico <> "R" 
		w_a	= 0
	ENDIF

	SCATTER MEMVAR
	GO TOP

	&& R.13b Mariano
	store 0 to tota, imp1, imp2, imp4, imp5, imp6, imp7, imp8, imp3, ret3, ret6, ret7, ret8	
	store 0 to watota, watotv, waimp1, waimp2, waimp4, waimp5, waimp6, waimp7, waimp8, waimp3, wareib, waribv,wareiv, warivv, warega, wargav, wareot, warotv
	&& R.13e Mariano
	
	IF !EOF()
			w_recno		= RECNO()

			DO totales
			GO w_recno
	ENDIF
	p_repfor=''	&& anulo el rep_for

&& R.02b Ra๚l
*	p_report='iwr005sc'	&& R.16 Mariano
	p_report=iif(w_apai='S','iwr005sd','iwr005sc')	&& R.16 Mariano
	
	DO resacta
&& R.02e Ra๚l	

	DO listalib WITH "FC", .T.

*	if p_repout="Impresora" and w4ulco>0										&& R.03 Ra๚l
*	if (p_repout="Impresora" OR LEFT(p_repout,1)	= '*') and w4ulco>0			&& R.03 Ra๚l		&& R.09 Ra๚l
	if (p_repout="Impresora" OR LEFT(p_repout,1)	= '*') and w4ulco>0	 AND p_preview = 0			&& R.09 Ra๚l
			if 'S'=sino(' Guarda Ultima Hoja y Cierra el Perํodo de COMPRAS ? ')
				=db_rlock('f04')
				SELE hojas
				GO TOP
				DO WHILE !EOF()
							IF SEEK(PTOI(peri), 'f04')
									sele f04
									repl d4hoco with hojas.hojas, d4esco with 'C'
							ENDIF
							SELE hojas
							SKIP
				ENDDO
				unlock in f04
			endif
	endif
	SELE hojas
	USE
	wdfech	= w_fecha
*?????????	DELE FILE &d0f99..frx				&& R.02 Ra๚l

	&& R.09b Mariano
	IF w_print
			sele f99c
			go top
			do represet &&(Resetea estado)
	ENDIF
	&& R.09e Mariano
endif

&& R.09b Mariano
*IF w_print
*		sele f99c
*		go top
*		do represet &&(Resetea estado)
*ENDIF
&& R.09e Mariano

sele f00
go top

return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC FTiIm         && Tipo Impositivo
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
para wCodi,wAction,wTipo

priv F_ret
priv wCant

if wTipo='C'
	* Cliente
	wcant=7
	decl VTiIm[wCant]
	VTiIm[01] ='1 ณResponsable Inscripto   '
	VTiIm[02] ='2 ณResponsable No Inscripto'
	VTiIm[03] ='3 ณConsumidor Final        '
	VTiIm[04] ='4 ณExento                  '
	VTiIm[05] ='5 ณNo Responsable          '
	VTiIm[06] ='6 ณResponsable Monotributo '
	VTiIm[07] ='7 ณNo Categorizado         '
else
	* Proveedor
	wcant=5
	decl VTiIm[wCant]
	VTiIm[01] ='1 ณResponsable Inscripto   '
	VTiIm[02] ='2 ณResponsable No Inscripto'
	VTiIm[03] ='4 ณExento                  '
	VTiIm[04] ='5 ณNo Responsable          '
	VTiIm[05] ='6 ณResponsable Monotributo '
endif

F_ret=spac(len(VTiIm[1])-3)

priv v_ret
if wAction='H'
	*   _w=setcolor(hi)

	*   @ 10,25 clear to 22,78
	*   @ 10,25 to 12,78
	*   @ 11,26 say 'C Descripciขn   '
	*   @ 12,25,22,78 box chr(195)+chr(196)+chr(180)+chr(179)+chr(217)+chr(196)+chr(192)+chr(179)
	*   v_ret=achoice(13,26,21,77,VTiIm,.t.,'')
	*   setcolor(_w)

	*   if v_ret>0 .and. v_ret<wCant+1
	*      F_ret=VTiIm[v_ret]
	*   endif
	*   F_ret=left(F_ret,1)  && Aqui debe retornar el codigo
endif

if wAction='V'
	v_ret=ascan(VTiIm,wCodi)
	if !empty(v_ret)
		F_ret=subs(VTiIm[v_ret],4,len(f_ret))  && Aqui debe retornar la descripcion
	endif
endif

return F_ret

proc impresu
w_resu=.f.
return

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

*-F ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
Function titu0
*-F ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

*return 'Transporte .......................'	&& R.13 Mariano
return ('Saldo al '+dtoc(wdfech-1)+'...............')	&& R.13 Mariano

*-F ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
Function titu1
*-F ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		w_paso=.t.

w_lfech	= {}

return iif(watotv=0,'Totales ..........................','Total Listado ....................')

*-F ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
Function tit1c
*-F ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

w_paso	= .t.
w_trans	= .F.
w_lfech	= {}
w_leye	= " "

if !'*'$p_repout	&& R.06 Mariano
*	watota	= 0												&& R.13 Mariano
*	w_peri	= RIGHT(DTOC((CTOD("01/" + w_peri) + 31)),7)	&& R.13 Mariano
endif	&& R.06 Mariano
*wdfech	= CTOD("01/" + w_peri)							&& R.02 Ra๚l

*???return iif(watota=0,'Totales ..........................','Total Listado ....................')

return ('Totales ..........................')

*-F ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
Function titu2
*-F ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
&& R.13b Mariano
if !eof('f99c')
	skip in f99c
	if eof('f99c') or d5peri!=w_peri
		w_paso=.t.
		skip -1 in f99c
*		return 'Totales ..........................'			&& R.09 Mariano
	else
		skip -1 in f99c
**		return 'Transporte .......................'			&& R.09 Mariano
	endif
else
	w_paso=.t.
endif
**return 'Totales ..........................'				&& R.09 Mariano
&& R.13e Mariano
return ''													&& R.09 Mariano

*-F ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
Function titu3
*-F ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
return iif(w_paso,'Totales ..........................','Transporte .......................')

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC fincpra
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

if wtalic='N'	&& R.13 Mariano
	SELE hojas
	APPE BLAN
	REPLA peri WITH f99c.d5peri

	IF w_vta
		w4ulve = w4hove + _pageno - 1
*		w4ulve = w4hove + _pageno
		REPLA hojas WITH w4ulve
	ENDIF

	IF w_cpra
		w4ulco = w4hoco + _pageno - 1
*		w4ulco = w4hoco + _pageno
		REPLA hojas WITH w4ulco
	ENDIF

	SELE f99c

	&& R.13 Mariano
	&& R.06b Mariano
*	if '*'$p_repout
*		private w_aux
*		w_aux=w_peri
*		w_peri	= RIGHT(DTOC((CTOD("01/" + w_peri) + 31)),7)
*		skip
*		if eof() or d5peri!=w_peri
*			w_peri=w_aux
*			skip -1
*		endif
*	endif
	&& R.06e Mariano
	&& R.13 Mariano
endif	&& R.13 Mariano

return ''

*==============================================================================================

*-F ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
func impv05s
*-F ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
parameter f_para

private w_camp,w_retu, w_cprv

w_retu=.f.
w_camp=0
do case
*	case !empty(f99c.d5cpib)
	case f99c.d5regi = "R"
		w_camp =1
*	case !empty(f99c.d5cpga)
	case f99c.d5rega = "R"
		w_camp =2
*	case !empty(f99c.d5cpot)
	case f99c.d5prco = "R"
		w_camp =3
endcase
if w_camp>0
	w_cprv='f00.p0prv'+f_para
	if &w_cprv=w_camp
		w_retu=.t.
	endif						
endif		
return w_retu

*-F ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
func impc05s1
*-F ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
parameter f_para

private w_camp,w_retu, w_cprc

w_retu=.f.
w_camp=0
do case
	case !empty(f99c.d5cpib)
*	case f99c.d5regi = "R"
		w_camp =1
	case !empty(f99c.d5cpga)
*	case f99c.d5rega = "R"
		w_camp =2
	case !empty(f99c.d5cpot)
*	case f99c.d5prco = "R"
		w_camp =3
endcase
if w_camp>0
	w_cprc='f00.p0prc'+f_para
	if &w_cprc=w_camp
		w_retu=.t.
	endif						
endif		
return w_retu

*========================================================================================
FUNCTION fechalis

IF !EMPTY(w_lfech)
			RETURN (w_lfech)
ENDIF

DO CASE
		CASE !EMPTY(CTOD('31/'+d5peri))
						w_lfech	= CTOD('31/'+d5peri)
		CASE !EMPTY(CTOD('30/'+d5peri))
						w_lfech	= CTOD('30/'+d5peri)
		CASE !EMPTY(CTOD('29/'+d5peri))
						w_lfech	= CTOD('29/'+d5peri)
		OTHERWISE
				w_lfech	= CTOD('28/'+d5peri)
ENDCASE

RETURN (w_lfech)
	
*========================================================================================
PROCEDURE totiva
*========================================================================================
*PRIVATE w_recno

&& R.02b Ra๚l
* ESTO SOLO PARA IMPRESION DOS !!!!!!!!!!!!!!!!!!!!
*IF LEFT(p_repout,1)		= '*'
*
*	IF !w_paso
*			w_recno		= recno()
*			w_trans		= .T.
*			DO agruasto
*			GO w_recno
*			RETURN ''
*	ENDIF
*ENDIF
&& R.02e Ra๚l
		
&& R.10b Mariano
*store 0 to balic, btota, bimp1, bimp2, bimp4, bimp5, bimp6, bimp7, bimp8, bimp3
*store 0 to watota, watotv, waimp1, waimp2, waimp4, waimp5, waimp6, waimp7, waimp8, waimp3, wareib, waribv,wareiv, warivv, warega, wargav, wareot, warotv
&& R.10e Mariano
*store 0 to tota, imp1, imp2, imp4, imp5, imp6, imp7, imp8, imp3, ret3, ret6, ret7, ret8	&& R.13 Mariano

SELE f99c
SCATTER MEMVAR
w_recno		= recno()

*w_trans		= .T.	&& R.13 Mariano
DO totales	&& R.13 Mariano

*DO totales	&& R.10 Mariano
&& R.11b Mariano
IF w_vta
*		DO catimp	&& R.13 Mariano
ENDIF		
&& R.11e Mariano

GO w_recno

&& R.13b Mariano
*DO agruasto	

*SET ORDER TO TAG f99c

*if !'*'$p_repout		&& R.06 Mariano
*	GO w_recno in f99c
*else	&& R.06b Mariano
*	if between(w_recno,1,reccount('f99c'))
*		GO w_recno in f99c
*	endif
*endif	&& R.06e Mariano
&& R.13e Mariano

RETURN ""
	
*========================================================================================
PROCEDURE totales
*========================================================================================

	&& R.13b Mariano
	w_recno		= RECNO()
	w_peri=d5peri
	&& R.13e Mariano

	&& R.10b Mariano
	store 0 to btota, bimp1, bimp2, bimp4, bimp5, bimp6, bimp7, bimp8, bimp3
	store '***' to balic
	&& R.10e Mariano

	** totales por alicuotas de iva por perํodo **
	sele f99d	&& R.14 Mariano
	w_i=1

	for w_j=1 to reccount('f23')	&& total de alicuotas
		balic[w_j]='***'

		sum	d5tota,   d5imp1,   d5imp2,   d5imp4,   d5imp5,   d5imp6,   d5imp7,   d5imp8,   d5imp3 ;
			to btota[w_i],bimp1[w_i],bimp2[w_i],bimp4[w_i],bimp5[w_i],bimp6[w_i],bimp7[w_i],bimp8[w_i],bimp3[w_i] ;
			for val(d5alic)=w_j-1		AND d5tico <> "R"		AND d5peri = w_peri	&& R.15 Mariano (w_j-1 x w_j)
		
		if btota[w_i]+bimp1[w_i]+bimp2[w_i]+bimp4[w_i]+bimp5[w_i]+bimp6[w_i]+bimp7[w_i]+bimp8[w_i]+bimp3[w_i]<>0
			balic[w_i]=strcero(str(w_j-1,3))	&& R.15 Mariano (w_j-1 x w_j)	
			w_i=w_i+1
		endif
	endfor
	sele f99c	&& R.14 Mariano

	if empty(watota)	&& R.13 Mariano
		sum d5imp1,d5imp2,d5imp4,d5imp5,d5imp6,d5imp7,d5imp8,d5imp3 ;
			to waimp1,waimp2,waimp4,waimp5,waimp6,waimp7,waimp8,waimp3 ;
			for d5fech<wdfech															AND d5peri = w_peri 


		IF w_imre	= 'S' 
			sum d5tota to watota for d5fech<wdfech										AND d5peri = w_peri 
			sum d5tota to waretc for d5fech<wdfech and d5tico='R'						AND d5peri = w_peri 
			sum d5tota to wareiv for d5fech<wdfech and d5tico='R' and !empty(d5pere)	AND d5peri = w_peri 
			sum d5tota to wareib for d5fech<wdfech and d5tico='R' and !empty(d5cpib)	AND d5peri = w_peri 
			sum d5tota to warega for d5fech<wdfech and d5tico='R' and !empty(d5cpga)	AND d5peri = w_peri 
			sum d5tota to wareot for d5fech<wdfech and d5tico='R' and !empty(d5cpot)	AND d5peri = w_peri 
		else
			sum d5tota to watota for d5tico!='R' and d5fech<wdfech						AND d5peri = w_peri 
		endif		
	endif	&& R.13 Mariano

*IF w_vta && R.13 Mariano
IF !w_paso	&& R.13 Mariano
	DO catimp
ENDIF		

GO w_recno

RETURN 	

*========================================================================================
PROCEDURE catimp

&& R.13b Mariano
if !empty(d5peri)
	w_peri=d5peri
endif
&& R.13e Mariano

	for w_i=1 to 7		&& totales por categoria impositiva
			
		sum	d5tota,   d5imp1,   d5imp2,   d5imp4,   d5imp5,   d5imp6,   d5imp7,   d5imp8,   d5imp3 ;
			to tota[w_i],imp1[w_i],imp2[w_i],imp4[w_i],imp5[w_i],imp6[w_i],imp7[w_i],imp8[w_i],imp3[w_i] ;
			for val(d5tiim)=w_i and d5tico!='R'									AND d5peri = w_peri 

		sum d5tota to ret3[w_i] ;
			for val(d5tiim)=w_i and d5tico='R' and d5reiv = "R"				AND d5peri = w_peri 
			
		sum d5tota to ret6[w_i] ;
			for val(d5tiim)=w_i and d5tico='R' and impv05s('1')				AND d5peri = w_peri 

		sum d5tota to ret7[w_i] ;
			for val(d5tiim)=w_i and d5tico='R' and impv05s('2')				AND d5peri = w_peri 

		sum d5tota to ret8[w_i] ;
			for val(d5tiim)=w_i and d5tico='R' and impv05s('3')				AND d5peri = w_peri 
		
	endfor

RETURN	

*========================================================================================
FUNCTION w_leyen

w_leye		= " "
IF f99c.d5empr = "ZX"
			w_leye	= "PERIODO SIN MOVIMIENTOS"
w_lfech	= {}
ENDIF
IF f99c.d5empr = "ZZ"
			w_leye	= "PERIODO INEXISTENTE"
w_lfech	= {}
ENDIF

RETURN w_leye

*================================================================================
&& R.02b Ra๚l

* Copiado de CG
* Para la impresi๓n en DOS, va a pelar del report los :
*							wtalic="S"			totales por alํcuota
*							w_imre='S'			imprime retenciones
PROCEDURE resacta
*================================================================================

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
&& R.16b Mariano
*===> Borro todo lo relativo a TOTALES ALICUOTAS
*IF wtalic = 'N'
*		DELE ALL FOR 'wtalic = "S"' $ supexpr	AND platform = 'WINDOWS'
*ENDIF			

*============================================================================================
*===> Borro todo lo relativo a RETENCIONES
*IF w_imre = 'N'
*		DELE ALL FOR "w_imre = 'S'" $ supexpr	AND platform = 'WINDOWS'
*ENDIF			
do case
	case w_imre='N' and wtalic='N'
		DELE ALL FOR "w_imre = 'S'"$supexpr or 'wtalic = "S"'$supexpr			&& d5tipo GROUP HEADER Y FOOTER
	case w_imre='N' and wtalic='S'
		DELE ALL FOR "w_imre = 'S'"$supexpr and !'wtalic = "S"'$supexpr			&& d5tipo GROUP HEADER Y FOOTER
	case w_imre='S' and wtalic='N'
		DELE ALL FOR !"w_imre = 'S'"$supexpr and 'wtalic = "S"'$supexpr			&& d5tipo GROUP HEADER Y FOOTER
endcase
&& R.16e Mariano

*============================================================================================
*===> C O M P R A S
*IF w_cpra 	&& R.13 Mariano
IF w_cpra and 'iwr005sc'$p_report	&& R.13 Mariano

*===> Elimino el grupo d5tipo
	DELE ALL FOR LEFT(uniqueid, 6) = "_1VY1A"			&& d5tipo GROUP HEADER Y FOOTER
														&& idem prefijo en compras y ventas

*REPLA height  WITH 3750		FOR w_imre = 'N' AND wtalic = 'N' AND LEFT(uniqueid, 6) = '_S9Y11' AND objcode = 5		
REPLA height  WITH 30209	FOR w_imre = 'N' AND wtalic = 'S' AND LEFT(uniqueid, 6) = '_S9Y11' AND objcode = 5	
*REPLA height  WITH 6771		FOR w_imre = 'S' AND wtalic = 'N' AND LEFT(uniqueid, 6) = '_S9Y11' AND objcode = 5		
*REPLA height  WITH 32292	FOR w_imre = 'S' AND wtalic = 'S' AND LEFT(uniqueid, 6) = '_S9Y11' AND objcode = 5		


*===> Ajusto las posiciones de los campos desde el detalle hasta el final
	REPLA ALL vpos WITH vpos - 3750 		FOR 30625 < vpos	AND vpos <= 40000

	IF w_imre = 'S' OR (w_imre = 'N'	AND wtalic = 'N')
		REPLA ALL vpos WITH vpos - 9062.5		FOR 40000 < vpos
	ELSE
		&& R.13b Mariano
*		REPLA ALL vpos WITH vpos - 9062.5		FOR 40000 < vpos	AND vpos < 52604.167
*		REPLA ALL vpos WITH vpos - 11145.834	FOR 52604.167 <= vpos	
		REPLA ALL vpos WITH vpos - 9062.5		FOR 40000 < vpos	AND vpos < 50729.000
		REPLA ALL vpos WITH vpos - 11945.834	FOR 50729.000 <= vpos	
		&& R.13e Mariano
	ENDIF
		
*===> Ajusto posici๓n de FINCPRA al inicio del FOOTER d5peri
	REPLA vpos WITH 36979.167		FOR	'fincpra' $ expr

ENDIF

&& R.16b Mariano
IF w_cpra and 'iwr005sd'$p_report

*===> Elimino el grupo d5tipo
  do case
	case w_imre='N' and wtalic='N'
*===> Ajusto las posiciones de los campos desde el detalle hasta el final
		REPLA ALL vpos WITH vpos - 17292		FOR 43000 < vpos
		REPLA height  WITH height - 17292	FOR LEFT(uniqueid, 6) = '_S9Y11' and objcode = 5	
	
	case w_imre='N' and wtalic='S'
*===> Ajusto las posiciones de los campos desde el detalle hasta el final
		REPLA ALL vpos WITH vpos - 1563 		FOR 43050 < vpos 
		REPLA height  WITH height - 1563	FOR LEFT(uniqueid, 6) = '_S9Y11' and objcode = 5	
	
	case w_imre='S' and wtalic='N'
*===> Ajusto las posiciones de los campos desde el detalle hasta el final
		REPLA ALL vpos WITH vpos - 14167		FOR 44000 < vpos 
		REPLA height  WITH height - 14167	FOR LEFT(uniqueid, 6) = '_S9Y11' and objcode = 5	

  endcase
endif

IF w_vta and 'iwr005sw'$p_report

*===> Elimino el grupo d5tipo
  do case
	case w_imre='N' and wtalic='N'
*===> Ajusto las posiciones de los campos desde el detalle hasta el final
		REPLA ALL vpos WITH vpos - 16146		FOR 70000 < vpos
		REPLA height  WITH height - 16146	FOR LEFT(uniqueid, 6) = '_S9Y11' and objcode = 5	
	
	case w_imre='N' and wtalic='S'
*===> Ajusto las posiciones de los campos desde el detalle hasta el final
		REPLA ALL vpos WITH vpos - 1458 		FOR 60000 < vpos 
		REPLA height  WITH height - 1458	FOR LEFT(uniqueid, 6) = '_S9Y11' and objcode = 5	
	
	case w_imre='S' and wtalic='N'
*===> Ajusto las posiciones de los campos desde el detalle hasta el final
		REPLA ALL vpos WITH vpos - 13229		FOR 70000 < vpos 
		REPLA height  WITH height - 13229	FOR LEFT(uniqueid, 6) = '_S9Y11' and objcode = 5	

  endcase
endif

&& R.16e Mariano

*============================================================================================
*===> V E N T A S 
*IF w_vta	&& R.13 Mariano
IF w_vta and 'iwr005sv'$p_report	&& R.13 Mariano

	IF  w_imre = 'N'				&& Si no hay RETENCIONES tengo que eliminar el grupo d5tipo
*===> Elimino el grupo d5tipo
				DELE ALL FOR LEFT(uniqueid, 6) = "_1VY1A"			&& d5tipo GROUP HEADER Y FOOTER
																	&& idem prefijo en compras y ventas
*===> Ajusto las posiciones de los campos desde el detalle hasta el final
				&& R.13b Mariano
*				REPLA ALL vpos WITH vpos - 3750 		FOR 30625 <= vpos	AND vpos < 45520.833
*				REPLA ALL vpos WITH vpos - 9062.5		FOR 45520.833 <= vpos	
				REPLA ALL vpos WITH vpos - 3950 		FOR 30625 <= vpos	AND vpos < 38000.000
				REPLA ALL vpos WITH vpos - 9500.5		FOR 38000.000 <= vpos	
				&& R.13e Mariano
	ENDIF

*===> Ajusto posici๓n de FINCPRA al inicio del FOOTER d5peri
*	REPLA vpos WITH 36458.333		FOR	'fincpra' $ expr	&& R.13 Mariano
	REPLA vpos WITH 45458.333		FOR	'fincpra' $ expr	&& R.13 Mariano

ENDIF
*============================================================================================

PACK
use
p_report =	d0f99 

SELE f99c

return

&& R.02e Ra๚l

*================================================================================

**puesto adrede para que incluya el report al proyecto**
report form iwr005sc
report form iwr005sv
report form iwr005sd	&& R.16 Mariano
report form iwr005sw	&& R.16 Mariano



